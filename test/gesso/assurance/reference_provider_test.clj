(ns gesso.assurance.reference-provider-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-temporal-fixtures :refer [sys provider-rectangles]]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.reference-lifecycle :as lifecycle]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.io RandomAccessFile)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration ZonedDateTime)
           (xtdb.api.log Log Log$Record SourceMessage$FlushBlock)))

(defn- id [n] (java.util.UUID. 0 n))
(defn- time-at [n] (.plusSeconds (sys 1) n))
(def ^:private steps
  [{:kind :publish :system-time (time-at 0)
    :spec {:source-id "provider-reference" :intent-ids [(id 1)]
           :operation :request/claim :purpose :gesso.live/refresh}
    :changes [{:topic :request :id "a"}]
    :writes [[:put-docs :providerprobe {:xt/id "a" :value 1}]]}
   {:kind :acquire :system-time (time-at 1) :observed-system (time-at 0) :observed-valid (time-at 0)
    :claim {:intent-id (id 1) :lease-id (id 2) :worker-id "w" :source-id "provider-reference"
            :now (time-at 0) :lease-until (time-at 10)}}
   {:kind :acknowledge :system-time (time-at 10) :observed-system (time-at 1) :observed-valid (time-at 10)
    :submission {:intent-id (id 1) :lease-id (id 2) :worker-id "w"
                 :receipt {:receipt-id (id 3) :adapter :test/adapter :result :accepted}}}])

(def ^:private tables #{:providerprobe :gesso_live_outbox})

(defn- transactions []
  (let [p (lifecycle/predict-lifecycle steps)]
    (is (= :predicted (:status p)))
    (into (:transactions p)
          [{:system-time (time-at 11) :ops [["ASSERT TRUE"]]}
           {:system-time (time-at 12) :ops [[:put-docs :providerprobe {:xt/id "a" :value 2}]
                                          ["ASSERT FALSE"]]}])))

(defn- observe! [node txs]
  (let [predicted (temporal/replay txs)
        ids (mapv (fn [{:keys [system-time ops]}]
                    (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                      (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                      id)) txs)
        ledger (mapv (fn [r] {:tx-id (:xt/id r) :committed? (:committed r)
                             :system-time (.toInstant ^ZonedDateTime (:system-time r))})
                     (xt/q node ["SELECT _id, committed, system_time FROM xt.txs ORDER BY _id"]))
        envelopes (mapv (fn [i tx entry]
                          ((if (#{1 2} i) envelope/check-worker-attempt-envelope! envelope/check-envelope!)
                           node (nth ids i) (:ops tx) (:committed? entry)))
                        (range) txs (:history predicted))]
    {:ledger ledger :envelopes envelopes
     :rectangles (vec (mapcat #(provider-rectangles node %) tables))}))

(deftest selected-provider-execution-comparison
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      (let [observed (observe! node txs)
            report (reference/compare-execution txs tables observed)]
        (is (= [:matched :matched :matched :matched :matched] (mapv :status (:envelopes observed))) (pr-str (:envelopes observed)))
        (is (= [true true false true false] (mapv :committed? (:ledger observed))))
        (is (= :matched (:status report)) (pr-str report))
        (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks report)))
        (is (= :not-established (:provider-refinement report)))
        (is (= :none (:authority report)))
        (is (= :not-issued (:certificate report)))
        (let [ops (:ops (nth txs 1)) id (get-in observed [:ledger 1 :tx-id])
              path [(dec (count ops)) 2 :gesso.live.outbox/lease-until]]
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope!
                                     node id (update-in ops path #(.plusSeconds % 1)) true))))
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope!
                                     node id (update-in ops path #(.atZone % (java.time.ZoneId/of "Europe/Paris"))) true))))
          (is (= :mismatch (:status (envelope/check-worker-attempt-envelope! node id ops false)))))
        (doseq [[changed expected] [[(assoc-in observed [:ledger 2 :committed?] true) :mismatch]
                                    [(update-in observed [:ledger 0 :system-time] #(.plusSeconds % 1)) :mismatch]
                                    [(assoc-in observed [:envelopes 0 :tx-id] -1) :mismatch]
                                    [(assoc-in observed [:envelopes 0 :status] :unavailable) :unavailable]
                                    [(assoc-in observed [:envelopes 0 :status] :unsupported) :unsupported]
                                    [(assoc-in observed [:envelopes 0 :status] :mismatch) :mismatch]
                                    [(update observed :rectangles pop) :mismatch]
                                    [(update observed :rectangles conj (first (:rectangles observed))) :mismatch]
                                    [(update observed :ledger pop) :unsupported]
                                    [(assoc-in observed [:envelopes 0 :profile] :foreign) :unsupported]]]
          (is (= expected (:status (reference/compare-execution txs tables changed)))))
        (is (= :unsupported (:status (reference/compare-execution txs #{:providerprobe} observed))))
        (is (= :unsupported (:status (reference/compare-execution [] tables observed))))))))

(deftest direct-quiescent-provider-comparison
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      (observe! node txs)
      (let [r (reference/compare-quiescent-execution! node txs tables)]
        (is (= :matched (:status r)) (pr-str r))
        (is (= reference/acquisition-profile (:acquisition-profile r)))
        (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks r)))
        (is (= :not-established (:provider-refinement r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r))))
      ;; Same abort and history, but different retained attempted write.
      (let [changed (assoc-in txs [4 :ops 0 2 :value] 3)]
        (is (= :mismatch (:status (reference/compare-quiescent-execution! node changed tables)))))
      (is (= :ledger-sequence-mismatch (:reason (reference/compare-quiescent-execution! node (pop txs) tables))))
      (is (= :ledger-sequence-mismatch (:reason (reference/compare-quiescent-execution!
                                                node (conj txs {:system-time (time-at 13) :ops [["ASSERT TRUE"]]}) tables))))
      (let [q xt/q calls (atom 0)]
        (with-redefs [xt/q (fn [node query & opts]
                            (let [rows (q node query (or (first opts) {}))]
                              (if (= ["SELECT _id, committed, system_time FROM xt.txs ORDER BY _id LIMIT 257"] query)
                                (if (= 2 (swap! calls inc)) (pop rows) rows)
                                rows)))]
          (is (= :ledger-changed-during-observation
                 (:reason (reference/compare-quiescent-execution! node txs tables))))
          (is (= 2 @calls))))
      (with-redefs [envelope/check-worker-attempt-envelope!
                    (fn [& _] {:status :unavailable :profile envelope/profile})]
        (is (= :unavailable (:status (reference/compare-quiescent-execution! node txs tables)))))
      (with-redefs [xt/q (fn [& _] (throw (AssertionError. "invalid expectation must not query")))]
        (doseq [bad [#{:providerprobe} (conj tables (keyword "x;drop")) (conj tables :xt_txs)]]
          (is (= :unsupported (:status (reference/compare-quiescent-execution! node txs bad))))))
      (with-redefs [xt/q (fn [& _] (throw (ex-info "read failed" {})))]
        (is (= :unavailable (:status (reference/compare-quiescent-execution! node txs tables)))))
      (with-redefs [xt/q (fn [& _] (vec (repeat 257 {})))]
        (is (= :ledger-limit (:reason (reference/compare-quiescent-execution! node txs tables))))))))

(deftest explicit-source-identity-binding
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      ;; Capture submission identities, not IDs learned from the audit ledger.
      (let [bound (mapv (fn [{:keys [system-time ops] :as tx}]
                          (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                            (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                            (assoc tx :tx-id id))) txs)
            r (reference/compare-source-bound-execution! node bound tables)]
        (is (= :matched (:status r)) (pr-str r))
        (is (= reference/source-binding-profile (:source-binding-profile r)))
        (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks r)))
        (is (= :not-established (:provider-refinement r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r)))
        ;; Every unbound physical expectation still matches. Wrong identities
        ;; must fail before any retained envelope or historical table query.
        (is (= :matched (:status (reference/compare-quiescent-execution! node txs tables))))
        (with-redefs [envelope/check-envelope! (fn [& _] (throw (AssertionError. "unexpected envelope read")))
                      envelope/check-worker-attempt-envelope! (fn [& _] (throw (AssertionError. "unexpected worker read")))]
          (doseq [wrong [(mapv #(update % :tx-id + 100) bound)
                         (update-in bound [(dec (count bound)) :tx-id] inc)
                         (pop bound)]]
            (let [r (reference/compare-source-bound-execution! node wrong tables)]
              (is (= :mismatch (:status r)))
              (is (= :source-identity-sequence-mismatch (:reason r))))))
        (let [wrong (assoc-in bound [4 :ops 0 2 :value] 3)]
          (is (= :mismatch (:status (reference/compare-source-bound-execution! node wrong tables)))))
        (with-redefs [xt/q (fn [& _] (throw (AssertionError. "invalid binding must not query")))]
          (doseq [bad [[] (reverse bound) (mapv #(dissoc % :tx-id) bound)
                       (assoc-in bound [1 :tx-id] (:tx-id (first bound)))
                       (assoc bound 0 (nth bound 1))
                       (assoc-in bound [0 :tx-id] -1)
                       (assoc-in bound [0 :tx-id] Long/MAX_VALUE)
                       (assoc-in bound [0 :tx-id] "0")
                       (assoc-in bound [0 :extra] true)
                       (with-meta bound {:foreign true})]]
            (let [r (reference/compare-source-bound-execution! node bad tables)]
              (is (= :unsupported (:status r)))
              (is (= :unsupported-source-expectation (:reason r)))
              (is (not (contains? r :checks))))))))))

(deftest bounded-source-coverage-distinguishes-maintenance-from-transactions
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      (let [database (db/primary-db node)
            ^Log log (.getSourceLog database)
            flush! (fn []
                     ;; A deliberately nonmatching expected block index is a
                     ;; maintenance message, not a request to commit a tx.
                     (let [id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
                       (.awaitSourceBlocking database id (Duration/ofSeconds 10))
                       id))
            first-maintenance (flush!)
            bound (mapv (fn [{:keys [system-time ops] :as tx}]
                          (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                            (.awaitTxBlocking database id (Duration/ofSeconds 10))
                            (flush!)
                            (assoc tx :tx-id id))) txs)
            observed (coverage/observe-coverage! node)
            report (reference/compare-source-covered-execution! node bound tables)]
        (is (= 0 first-maintenance))
        (is (= :observed (:status observed)) (pr-str observed))
        (is (= (mapv :tx-id bound) (:transaction-ids observed)))
        (is (= 6 (:maintenance-count observed)))
        (is (= 10 (:last-message-id observed)))
        (is (= :matched (:status report)) (pr-str report))
        (is (= :matched (:source-coverage report)))
        (is (= :none (:authority report)))
        (is (= :not-issued (:certificate report)))
        (is (= :not-established (:provider-refinement report)))
        (is (= :source-transaction-coverage-mismatch
               (:reason (reference/compare-source-covered-execution! node (pop bound) tables))))
        (is (= :source-transaction-coverage-mismatch
               (:reason (reference/compare-source-covered-execution! node (update-in bound [0 :tx-id] dec) tables))))
        (let [compare! reference/compare-source-bound-execution!]
          (with-redefs [reference/compare-source-bound-execution!
                        (fn [& args] (let [r (apply compare! args)] (flush!) r))]
            (is (= :source-log-changed-during-comparison
                   (:reason (reference/compare-source-covered-execution! node bound tables))))))
        (with-redefs [coverage/observe-coverage! (fn [_] (throw (AssertionError. "invalid binding must not read")))]
          (is (= :unsupported-source-expectation
                 (:reason (reference/compare-source-covered-execution! node [] tables)))))))))

(deftest bounded-source-coverage-rejects-gaps-and-unknown-records
  (let [record #(Log$Record. 0 % (time-at 0) (SourceMessage$FlushBlock. Long/MAX_VALUE))
        classify #'coverage/classify-records]
    (is (= :source-record-coverage-gap (:reason (classify [(record 1)] 1))))
    (is (= :source-record-coverage-gap (:reason (classify [(record 0) (record 0)] 1))))
    (is (= :source-record-coverage-gap (:reason (classify [(record 0)] 1))))
    (is (= :unsupported-source-message-kind
           (:reason (classify [(Log$Record. 0 0 (time-at 0) nil)] 0))))
    (is (= :embedded-primary-log-required (:reason (coverage/observe-coverage! nil))))))

(deftest selected-processed-source-frontier-correspondence
  (let [txs (transactions)]
    (with-open [node (xtn/start-node {})]
      (let [database (db/primary-db node)
            ^Log log (.getSourceLog database)
            bound (mapv (fn [{:keys [system-time ops] :as tx}]
                          (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                            (.awaitTxBlocking database id (Duration/ofSeconds 10))
                            (assoc tx :tx-id id))) txs)
            flush! (fn []
                     (let [id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
                       (.awaitSourceBlocking database id (Duration/ofSeconds 10)) id))
            end (flush!)
            actual (coverage/observe-processed-coverage! node)
            r (reference/compare-processed-source-execution! node bound tables)]
        (is (> end (:tx-id (peek bound))))
        (is (= :observed (:status actual)) (pr-str actual))
        (is (= end (:last-message-id actual) (:processed-message-id actual)))
        (is (= :matched (:status r)) (pr-str r))
        (is (= :matched (:processed-frontier r)))
        (is (= coverage/processed-profile (:processed-profile r)))
        (is (= :not-established (:provider-refinement r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r)))
        ;; Simulate a delayed or failed watcher without racing actual indexing.
        (let [sample {:submitted end :processed end :ingestion-failed? false}]
          (doseq [[a b reason]
                  [[(update sample :processed dec) (update sample :processed dec) :source-processing-incomplete]
                   [(update sample :processed inc) (update sample :processed inc) :source-frontier-contradiction]
                   [(assoc sample :ingestion-failed? true) sample :source-ingestion-failed]
                   [sample (assoc sample :ingestion-failed? true) :source-ingestion-failed]
                   [(update sample :processed dec) sample :source-frontier-changed-during-observation]
                   [(update sample :submitted inc) (update sample :submitted inc) :source-frontier-coverage-mismatch]]]
            (let [samples (atom [a b])]
              (with-redefs-fn {#'coverage/read-frontier! (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))
                                #'reference/compare-source-bound-execution! (fn [& _] (throw (AssertionError. "unavailable frontier must not compare")))}
                (fn []
                  (let [r (reference/compare-processed-source-execution! node bound tables)]
                    (is (= :unavailable (:status r)))
                    (is (= reason (:reason r)))
                    (is (not (contains? r :processed-frontier)))))))))
        (let [compare! reference/compare-source-bound-execution!]
          (with-redefs [reference/compare-source-bound-execution!
                        (fn [& args] (let [r (apply compare! args)] (flush!) r))]
            (let [r (reference/compare-processed-source-execution! node bound tables)]
              (is (= :unavailable (:status r)))
              (is (= :source-log-changed-during-comparison (:reason r)))
              (is (not (contains? r :processed-frontier))))))))))

(deftest payload-fingerprint-binds-framing-and-content
  (let [fingerprint #'coverage/payload-fingerprint
        frames [[0 100 2] [23 101 1]]
        bodies [(byte-array [1 2]) (byte-array [3])]
        original (fingerprint frames bodies)]
    (is (re-matches #"[0-9a-f]{64}" original))
    (is (= original (fingerprint frames (mapv #(aclone ^bytes %) bodies))))
    (is (not= original (fingerprint frames [(byte-array [1 3]) (second bodies)])))
    (is (not= original (fingerprint frames [(first bodies) (byte-array [4])])))
    (is (not= original (fingerprint (assoc-in frames [0 1] 99) bodies)))
    (is (not= original (fingerprint (assoc-in frames [1 0] 24) bodies)))
    (is (not= original (fingerprint [[0 100 1] [22 101 2]] [(byte-array [1]) (byte-array [2 3])])))
    (is (not= original (fingerprint (vec (reverse frames)) (vec (reverse bodies)))))
    (is (not= original (fingerprint [] [])))))

(defn- assert-bound-payload-comparison! [node bound]
  (let [sample (coverage/observe-local-payload-bound-coverage! node)
        plain (coverage/observe-local-payload-coverage! node)
        r (reference/compare-local-payload-source-execution! node bound tables)]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :payload-fingerprint)))
    (is (= plain (dissoc sample :payload-binding-profile :payload-fingerprint)))
    (is (re-matches #"[0-9a-f]{64}" (:payload-fingerprint sample)))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:source-coverage r)))
    (is (= :matched (:payload-correspondence r)))
    (is (= coverage/payload-binding-profile (:payload-binding-profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:provider-refinement r)))
    (is (not (contains? r :payload-fingerprint)))
    (is (not (contains? r :processed-frontier)))
    (is (= :source-transaction-coverage-mismatch
           (:reason (reference/compare-local-payload-source-execution! node (pop bound) tables))))
    (doseq [[before after reason]
            [[sample (update sample :payload-fingerprint #(str (if (= \0 (first %)) "1" "0") (subs % 1)))
              :source-log-changed-during-comparison]
             [(dissoc sample :payload-fingerprint) sample :invalid-source-payload-binding]
             [sample (assoc sample :payload-fingerprint "bad") :invalid-source-payload-binding]
             [(dissoc sample :payload-correspondence) sample :invalid-source-payload-binding]
             [sample (dissoc sample :payload-binding-profile) :invalid-source-payload-binding]
             [sample {:status :unsupported :reason :unsupported-source-payload} :unsupported-source-payload]]]
      (let [samples (atom [before after])]
        (with-redefs [coverage/observe-local-payload-bound-coverage!
                      (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))]
          (let [r (reference/compare-local-payload-source-execution! node bound tables)]
            (is (= (if (= reason :unsupported-source-payload) :unsupported :unavailable) (:status r)))
            (is (= reason (:reason r)))
            (is (not (contains? r :payload-correspondence)))
            (is (not (contains? r :source-coverage)))
            (is (not (contains? r :payload-fingerprint)))))))
    (:payload-fingerprint sample)))

(deftest local-source-coverage-across-node-reopen
  (let [root (Files/createTempDirectory "gesso-reference-local-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        txs (transactions)]
    (try
      (let [[bound initial initial-fingerprint]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    ^Log log (.getSourceLog database)
                    flush! (fn []
                             (let [id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
                               (.awaitSourceBlocking database id (Duration/ofSeconds 10)) id))
                    _ (flush!)
                    bound (mapv (fn [{:keys [system-time ops] :as tx}]
                                  (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                                    (.awaitTxBlocking database id (Duration/ofSeconds 10))
                                    (flush!)
                                    (assoc tx :tx-id id))) txs)
                    observed (coverage/observe-local-coverage! node)
                    r (reference/compare-local-source-covered-execution! node bound tables)]
                (is (= :observed (:status observed)) (pr-str observed))
                (is (= (mapv :tx-id bound) (:transaction-ids observed)))
                (is (= 6 (:maintenance-count observed)))
                (is (= 11 (count (:record-offsets observed))))
                (is (= 0 (first (:record-offsets observed))))
                (is (> (second (:record-offsets observed)) 1))
                (is (> (:file-bytes observed) (:last-message-id observed)))
                (is (= :matched (:status r)) (pr-str r))
                (is (= :matched (:source-coverage r)))
                (is (= coverage/local-profile (:source-coverage-profile r)))
                (is (= :none (:authority r)))
                (is (= :not-issued (:certificate r)))
                (is (= :not-established (:provider-refinement r)))
                (is (= :unsupported (:status (coverage/observe-coverage! node))))
                (is (= :source-transaction-coverage-mismatch
                       (:reason (reference/compare-local-source-covered-execution! node (pop bound) tables))))
                (let [processed (coverage/observe-local-processed-coverage! node)
                      r (reference/compare-local-processed-source-execution! node bound tables)]
                  (is (= :observed (:status processed)))
                  (is (= (:last-message-id observed) (:processed-message-id processed)))
                  (is (> (:processed-message-id processed) (:tx-id (peek bound))))
                  (is (= :matched (:status r)))
                  (is (= :matched (:processed-frontier r)))
                  (is (= coverage/local-processed-profile (:processed-profile r)))
                  (is (= :not-established (:provider-refinement r)))
                  (is (= :none (:authority r)))
                  (is (= :not-issued (:certificate r))))
                [bound observed (assert-bound-payload-comparison! node bound)]))]
        (with-open [node (xtn/start-node config)]
          (let [database (db/primary-db node)]
            (.awaitSourceBlocking database (:last-message-id initial) (Duration/ofSeconds 10))
            (is (= initial (coverage/observe-local-coverage! node)))
            (is (= initial-fingerprint (assert-bound-payload-comparison! node bound)))
            (is (= :matched (:status (reference/compare-local-source-covered-execution! node bound tables))))
            (let [processed (coverage/observe-local-processed-coverage! node)
                  end (:last-message-id initial)
                  sample {:submitted end :processed end :ingestion-failed? false}]
              (is (= initial (dissoc processed :processed-profile :processed-message-id)))
              (is (= end (:processed-message-id processed)))
              (is (= :matched (:status (reference/compare-local-processed-source-execution! node bound tables))))
              (doseq [[a b reason]
                      (let [lag (assoc sample :processed (:tx-id (peek bound)))
                            interior (assoc sample :processed (dec end))
                            eof (assoc sample :processed (:file-bytes initial))]
                        [[lag lag :source-processing-incomplete]
                         [interior interior :source-processed-offset-not-framed]
                         [eof eof :source-frontier-contradiction]
                         [(assoc sample :processed -1) (assoc sample :processed -1) :source-processing-incomplete]
                         [(assoc sample :processed nil) (assoc sample :processed nil) :source-frontier-contradiction]
                         [(assoc sample :ingestion-failed? true) sample :source-ingestion-failed]
                         [sample (assoc sample :ingestion-failed? true) :source-ingestion-failed]
                         [lag sample :source-frontier-changed-during-observation]
                         [(update sample :submitted inc) (update sample :submitted inc) :source-frontier-coverage-mismatch]])]
                (let [samples (atom [a b])]
                  (with-redefs-fn {#'coverage/read-local-frontier! (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))
                                    #'reference/compare-source-bound-execution! (fn [& _] (throw (AssertionError. "unavailable frontier must not compare")))}
                    (fn []
                      (let [r (reference/compare-local-processed-source-execution! node bound tables)]
                        (is (= :unavailable (:status r)))
                        (is (= reason (:reason r)))
                        (is (not (contains? r :processed-frontier)))))))))
            (let [compare! reference/compare-source-bound-execution!]
              (with-redefs [reference/compare-source-bound-execution!
                            (fn [& args]
                              (let [r (apply compare! args)
                                    ^Log log (.getSourceLog database)
                                    id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
                                (.awaitSourceBlocking database id (Duration/ofSeconds 10)) r))]
                (is (= :source-log-changed-during-comparison
                       (:reason (reference/compare-local-source-covered-execution! node bound tables))))
                (let [r (reference/compare-local-processed-source-execution! node bound tables)]
                  (is (= :unavailable (:status r)))
                  (is (= :source-log-changed-during-comparison (:reason r)))
                  (is (not (contains? r :processed-frontier)))))))))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))

(deftest local-source-framing-rejects-incomplete-and-contradictory-records
  (let [path (Files/createTempFile "gesso-reference-frame-" ".log" (make-array FileAttribute 0))
        scan #'coverage/local-frames
        classify #'coverage/classify-local-records
        timestamp (java.time.Instant/EPOCH)
        record #(Log$Record. 0 % timestamp (SourceMessage$FlushBlock. Long/MAX_VALUE))]
    (try
      (with-open [file (RandomAccessFile. (.toFile path) "rw")]
        (let [write! (fn [offset n]
                       (.seek file offset) (.writeByte file 30) (.writeInt file n)
                       (.writeLong file 0) (.write file (byte-array n)) (.writeLong file offset))]
          (is (= {:status :framed :size 0 :frames []} (scan file)))
          (write! 0 3) (write! 24 5)
          (let [framing (scan file)]
            (is (= {:status :framed :size 50 :frames [[0 0 3] [24 0 5]]} framing))
            (is (= :observed (:status (classify framing [(record 0) (record 24)] 24))))
            (is (= :source-record-coverage-gap (:reason (classify framing [(record 24)] 24))))
            (is (= :source-record-coverage-gap (:reason (classify framing [(record 0) (record 0)] 24))))
            (is (= :source-record-coverage-gap (:reason (classify framing [(record 0) (Log$Record. 0 24 (.plusSeconds timestamp 1) (SourceMessage$FlushBlock. 1))] 24))))
            (is (= :source-frame-end-mismatch (:reason (classify framing [(record 0) (record 24)] 0))))
            (is (= :unsupported-source-message-kind (:reason (classify framing [(record 0) (Log$Record. 0 24 timestamp nil)] 24)))))
          (.seek file 42) (.writeLong file 0)
          (is (= :invalid-source-frame (:reason (scan file))))
          (write! 24 5) (.setLength file 49)
          (is (= :invalid-source-frame (:reason (scan file))))
          (write! 24 5) (.seek file 24) (.writeByte file 0)
          (is (= :invalid-source-frame (:reason (scan file))))
          (write! 24 5) (.seek file 25) (.writeInt file -1)
          (is (= :invalid-source-frame (:reason (scan file))))
          (.setLength file (inc (* 64 1024 1024)))
          (is (= :source-byte-limit (:reason (scan file))))))
      (finally (Files/deleteIfExists path)))))

(deftest local-processed-coverage-requires-local-backend
  (is (= :embedded-primary-log-required (:reason (coverage/observe-local-processed-coverage! nil))))
  (with-open [node (xtn/start-node {})]
    (is (= :unsupported-source-log (:reason (coverage/observe-local-processed-coverage! node))))))
