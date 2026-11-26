(ns gesso.assurance.reference-worker-replay-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-replay-input-test :as fixture]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant ZoneId ZonedDateTime)))

(defn- canonical-op [op]
  (if (string? (first op))
    (mapv #(if (instance? ZonedDateTime %) (.toInstant ^ZonedDateTime %) %) op)
    (mapv (fn [x] (if (and (map? x) (instance? Instant (:gesso.live.outbox/lease-until x)))
                    (update x :gesso.live.outbox/lease-until #(.atZone ^Instant % (ZoneId/of "UTC"))) x)) op)))

(deftest strict-retained-worker-assertions
  (let [root (Files/createTempDirectory "gesso-retained-worker-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        at #(.plusSeconds t %)
        utc #(.atZone ^Instant (at %) (ZoneId/of "UTC"))
        id (java.util.UUID. 0 1)
        pending (first (:intents (outbox/prepare! {:intent-ids [id] :source-id "worker-source"
                                                 :operation :request/claim :purpose :gesso.live/refresh}
                                                [{:topic :request :id "r"}])))
        claim (fn [n deadline] {:intent-id id :lease-id (java.util.UUID. 0 n)
                               :worker-id (str "worker-" n) :lease-until (at deadline)})
        acquire (#'acquisition/acquire-ops (assoc pending :xt/system-from (utc 0)) (claim 2 10))
        leased (peek (peek acquire))
        reclaim (fn [old version deadline n next-deadline]
                  (let [{:keys [lease-id worker-id lease-until]} (claim n next-deadline)]
                    [[@#'acquisition/guard-expired-system-from-sql id (utc version) (utc deadline)]
                     [@#'acquisition/guard-new-deadline-sql lease-until]
                     [@#'acquisition/guard-max-new-deadline-sql lease-until]
                     [:put-docs :gesso_live_outbox (assoc old :gesso.live.outbox/lease-id lease-id
                                                        :gesso.live.outbox/lease-owner worker-id
                                                        :gesso.live.outbox/lease-until lease-until)]]))
        reclaimed (reclaim leased 1 10 3 312)
        second-lease (peek (peek reclaimed))
        recovered (reclaim second-lease 12 312 4 320)
        third-lease (peek (peek recovered))
        ack (fn [old version deadline]
              [[@#'acknowledgment/active-lease-guard-sql id (utc version) (utc deadline)]
               [:put-docs :gesso_live_outbox (assoc old :gesso.live.outbox/status :dispatch-accepted
                                                  :gesso.live.outbox/dispatch-receipt-id (java.util.UUID. 0 5)
                                                  :gesso.live.outbox/dispatch-adapter :test/adapter)]])
        txs (mapv (fn [n ops] {:system-time (at n) :ops ops})
                  [0 1 2 3 10 11 12 13 312 313 314]
                  [[[:put-docs :gesso_live_outbox pending]] acquire acquire
                   (reclaim leased 1 10 3 20) (reclaim leased 1 10 3 10)
                   reclaimed reclaimed (ack leased 1 10) (ack second-lease 12 312)
                   recovered (ack third-lease 313 320)])
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
                       report (reference/compare-local-processed-retained-replay-execution! node #{:gesso_live_outbox})
                       tx-id (:tx-id (second expected)) body (get (#'fixture/read-bytes node) tx-id)
                       arrow! arrow/decode-bounded-operations]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true true false false false false true false false true true] (mapv :committed? (:history prediction))))
                   (is (= [0 0 1 2 0 0] (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= 11 (:transaction-count report)))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :not-established (:provider-refinement report)))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (is (= :unsupported (:status (reference/compare-local-processed-retained-replay-execution! node #{:probe}))))
                   (doseq [[i mutate] [[0 #(update % :query str/replace "$2" "$1")]
                                      [0 #(update % :query str/replace "$2" "$02")]
                                      [0 #(assoc-in % [:args 0] (str id))]
                                      [0 #(assoc-in % [:args 1] "2026-01-01")]
                                      [0 #(update % :args pop)]
                                      [1 #(assoc % :args [id])]
                                      [1 #(assoc-in % [:args 0] (.plusNanos (utc 10) 1))]
                                      [1 #(assoc-in % [:args 0] (.withZoneSameInstant (utc 10) (ZoneId/of "Europe/Paris")))]
                                      [1 #(update % :query str/replace "<" "<=")]
                                      [2 #(update % :query str/replace "'5'" "'6'")]
                                      [2 #(update % :query str " -- comment")]]]
                     (with-redefs [arrow/decode-bounded-operations #(update-in (arrow! %) [:operations i] mutate)]
                       (let [r (input/decode-replay-input tx-id body)]
                         (is (= :unsupported (:status r)))
                         (is (not (contains? r :transaction))))))))]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [{:keys [system-time ops] :as tx}]
                                              (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                                                (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                                (assoc tx :tx-id id :ops (mapv canonical-op ops)))) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
