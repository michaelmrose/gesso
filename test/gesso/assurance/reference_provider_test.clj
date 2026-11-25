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
                              (if (= ["SELECT _id, committed, system_time FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL ORDER BY _id LIMIT 257"] query)
                                (if (= 2 (swap! calls inc)) (pop rows) rows)
                                rows)))]
          (is (= :ledger-changed-during-observation
                 (:reason (reference/compare-quiescent-execution! node txs tables))))
          (is (= 2 @calls))))
      (with-redefs [envelope/check-diagnostic-envelope!
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
        (with-redefs [envelope/check-diagnostic-envelope! (fn [& _] (throw (AssertionError. "unexpected diagnostic envelope read")))]
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

(defn- assert-processed-payload-comparison! [node bound]
  (let [observe! coverage/observe-local-processed-payload-bound-coverage!
        compare! reference/compare-local-processed-payload-source-execution!
        sample (observe! node)
        end (:last-message-id sample)
        frontier {:submitted end :processed end :ingestion-failed? false}
        r (compare! node bound tables)]
    (is (= :observed (:status sample)))
    (is (= end (:processed-message-id sample)))
    (is (= (coverage/observe-local-payload-bound-coverage! node)
           (dissoc sample :processed-profile :processed-payload-profile :processed-message-id)))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:source-coverage r) (:payload-correspondence r) (:processed-frontier r)))
    (is (= coverage/local-processed-payload-profile (:processed-payload-profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:provider-refinement r)))
    (is (not (contains? r :payload-fingerprint)))
    (doseq [[a b reason]
            [[(assoc frontier :processed (:tx-id (peek bound)))
              (assoc frontier :processed (:tx-id (peek bound))) :source-processing-incomplete]
             [(assoc frontier :processed (dec end)) (assoc frontier :processed (dec end))
              :source-processed-offset-not-framed]
             [frontier (assoc frontier :ingestion-failed? true) :source-ingestion-failed]
             [frontier (update frontier :submitted inc) :source-frontier-changed-during-observation]]]
      (let [samples (atom [a b])]
        (with-redefs-fn {#'coverage/read-local-frontier! (fn [_] (let [v (first @samples)] (swap! samples subvec 1) v))
                          #'reference/compare-source-bound-execution! (fn [& _] (throw (AssertionError. "invalid frontier compared")))}
          (fn []
            (let [r (compare! node bound tables)]
              (is (= :unavailable (:status r)))
              (is (= reason (:reason r)))
              (is (not (contains? r :processed-frontier)))
              (is (not (contains? r :payload-correspondence))))))))
    (doseq [[a b reason]
            [[(dissoc sample :processed-message-id) sample :invalid-processed-source-payload-binding]
             [(dissoc sample :payload-fingerprint) sample :invalid-processed-source-payload-binding]
             [sample (assoc sample :processed-message-id (dec end)) :invalid-processed-source-payload-binding]
             [sample (dissoc sample :processed-payload-profile) :invalid-processed-source-payload-binding]
             [sample (update sample :payload-fingerprint #(str (if (= \0 (first %)) "1" "0") (subs % 1)))
              :source-log-changed-during-comparison]
             [sample {:status :unsupported :reason :unsupported-source-payload} :unsupported-source-payload]]]
      (let [samples (atom [a b])]
        (with-redefs [coverage/observe-local-processed-payload-bound-coverage!
                      (fn [_] (let [v (first @samples)] (swap! samples subvec 1) v))
                      reference/compare-source-bound-execution! (fn [& _] {:status :matched})]
          (let [r (compare! node bound tables)]
            (is (= (if (= reason :unsupported-source-payload) :unsupported :unavailable) (:status r)))
            (is (= reason (:reason r)))
            (is (not (contains? r :source-coverage)))
            (is (not (contains? r :processed-frontier)))
            (is (not (contains? r :payload-correspondence)))
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
                (assert-processed-payload-comparison! node bound)
                [bound observed (assert-bound-payload-comparison! node bound)]))]
        (with-open [node (xtn/start-node config)]
          (let [database (db/primary-db node)]
            (.awaitSourceBlocking database (:last-message-id initial) (Duration/ofSeconds 10))
            (is (= initial (coverage/observe-local-coverage! node)))
            (is (= initial-fingerprint (assert-bound-payload-comparison! node bound)))
            (is (= initial-fingerprint (assert-processed-payload-comparison! node bound)))
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
                  (is (not (contains? r :processed-frontier))))
                (let [r (reference/compare-local-processed-payload-source-execution! node bound tables)]
                  (is (= :unavailable (:status r)))
                  (is (= :source-log-changed-during-comparison (:reason r)))
                  (is (not (contains? r :processed-frontier)))
                  (is (not (contains? r :payload-correspondence)))))))))
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
  (is (= :embedded-primary-log-required (:reason (coverage/observe-local-processed-payload-bound-coverage! nil))))
  (with-open [node (xtn/start-node {})]
    (is (= :unsupported-source-log (:reason (coverage/observe-local-processed-coverage! node))))
    (is (= :unsupported-source-log (:reason (coverage/observe-local-processed-payload-bound-coverage! node))))))

(defn- check-retained-execution! [node bound]
  (let [compare! reference/compare-local-processed-retained-execution!
        observe! coverage/observe-local-processed-retained-history-bound!
        sample (observe! node)
        table-set #{:providerprobe}
        report (compare! node bound table-set)]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :payload-fingerprint)))
    (is (= :matched (:status report)) (pr-str report))
    (is (= :matched (:source-coverage report) (:operation-correspondence report)
           (:outcome-correspondence report) (:processed-frontier report) (:payload-correspondence report)))
    (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:provider-refinement report)))
    (is (not (contains? report :payload-fingerprint)))
    (let [bad (assoc-in bound [1 :ops 1] ["ASSERT TRUE"])
          r (compare! node bad table-set)]
      (is (= :mismatch (:status r)))
      (is (= :ledger-sequence-mismatch (:reason r)))
      (is (not (contains? r :outcome-correspondence))))
    (is (= :source-transaction-coverage-mismatch (:reason (compare! node (pop bound) table-set))))
    (doseq [[a b reason] [[(dissoc sample :operation-correspondence) sample :invalid-processed-retained-history-binding]
                         [(assoc sample :operation-message-ids []) sample :invalid-processed-retained-history-binding]
                         [sample (update sample :payload-fingerprint #(str (if (= \0 (first %)) "1" "0") (subs % 1))) :source-log-changed-during-comparison]
                         [sample {:status :unsupported :reason :retained-operation-not-matched} :retained-operation-not-matched]]]
      (let [samples (atom [a b])]
        (with-redefs [coverage/observe-local-processed-retained-history-bound!
                      (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))]
          (let [r (compare! node bound table-set)]
            (is (= reason (:reason r)) (pr-str r))
            (is (not-any? #(contains? r %) [:source-coverage :operation-correspondence :outcome-correspondence :payload-fingerprint]))))))
    (with-redefs [reference/compare-source-bound-execution! (constantly {:status :matched})]
      (is (= :invalid-retained-execution-comparison (:reason (compare! node bound table-set)))))
    (is (= report (compare! node bound table-set)))))

(defn- check-direct-retained-replay! [node bound]
  (let [compare! reference/compare-local-processed-retained-replay-execution!
        observe! coverage/observe-local-processed-retained-replay-inputs!
        read! reference/compare-source-bound-execution!
        tables #{:providerprobe}
        sample (observe! node)
        report (compare! node tables)
        no-match! (fn [r]
                    (is (not-any? #(contains? r %) [:source-coverage :operation-correspondence
                                                   :outcome-correspondence :processed-frontier
                                                   :payload-correspondence :replay-inputs :payload-fingerprint
                                                   :checks :operations :transaction-count]))
                    (is (= :none (:authority r)))
                    (is (= :not-issued (:certificate r))))]
    (is (= bound (:replay-inputs sample)))
    (is (= :matched (:status report)) (pr-str report))
    (is (= reference/retained-replay-execution-profile (:retained-replay-execution-profile report)))
    (is (= :matched (:source-coverage report) (:operation-correspondence report)
           (:outcome-correspondence report) (:processed-frontier report) (:payload-correspondence report)))
    (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks report)))
    (is (= 3 (:transaction-count report)))
    (is (= :not-established (:provider-refinement report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (not-any? #(contains? report %) [:replay-inputs :payload-fingerprint :operations :transaction-ids]))
    (with-redefs [reference/compare-source-bound-execution!
                  (fn [node inputs tables] (is (= bound inputs)) (read! node inputs tables))]
      (is (= report (compare! node tables))))
    (doseq [bad [(dissoc sample :replay-input-profile)
                 (assoc sample :replay-inputs [])
                 (update-in sample [:replay-inputs 0 :tx-id] inc)
                 (assoc-in sample [:replay-inputs 1 :system-time] (:system-time (first bound)))]]
      (with-redefs [coverage/observe-local-processed-retained-replay-inputs! (constantly bad)
                    reference/compare-source-bound-execution! (fn [& _] (throw (AssertionError. "must not compare unqualified sample")))]
        (let [r (compare! node tables)]
          (is (= :unavailable (:status r)))
          (is (= :invalid-processed-retained-replay-binding (:reason r)))
          (no-match! r))))
    (doseq [[after reason] [[(update sample :payload-fingerprint #(str (if (= \0 (first %)) "1" "0") (subs % 1))) :source-log-changed-during-comparison]
                            [(assoc-in sample [:replay-inputs 0 :ops 0 2 :value] 99) :source-log-changed-during-comparison]
                            [{:status :unsupported :reason :retained-replay-input-not-qualified} :retained-replay-input-not-qualified]]]
      (let [samples (atom [sample after])]
        (with-redefs [coverage/observe-local-processed-retained-replay-inputs!
                      (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))]
          (let [r (compare! node tables)]
            (is (= reason (:reason r)))
            (is (empty? @samples))
            (no-match! r)))))
    (with-redefs [reference/compare-source-bound-execution! (constantly {:status :matched :replay-inputs bound})]
      (let [r (compare! node tables)]
        (is (= :invalid-retained-replay-execution-comparison (:reason r)))
        (no-match! r)))
    (let [ledger! @#'reference/read-ledger!]
      (with-redefs-fn {#'reference/read-ledger! (fn [node] (update-in (ledger! node) [1 :committed?] not))}
        (fn [] (let [r (compare! node tables)]
                 (is (= :mismatch (:status r)))
                 (is (= :ledger-sequence-mismatch (:reason r)))
                 (no-match! r)))))
    (with-redefs-fn {#'reference/read-rectangles! (constantly [])}
      (fn [] (let [r (compare! node tables)]
               (is (= :mismatch (:status r)))
               (no-match! r))))
    (let [r (compare! node #{:other})]
      (is (= :unsupported (:status r)))
      (no-match! r))
    (is (= report (compare! node tables)))))

(deftest retained-operation-history-bound-to-independent-outcomes
  (let [root (Files/createTempDirectory "gesso-retained-execution-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        txs [{:system-time (time-at 0) :ops [[:put-docs :providerprobe {:xt/id "a" :value 1}]]}
             {:system-time (time-at 1) :ops [[:put-docs :providerprobe {:xt/id "a" :value 2}] ["ASSERT FALSE"]]}
             {:system-time (time-at 2) :ops [["ASSERT ? = ?" 3 3] ["ASSERT TRUE"]]}]]
    (try
      (let [bound (with-open [node (xtn/start-node config)]
                    (let [database (db/primary-db node)
                          bound (mapv (fn [{:keys [system-time ops] :as tx}]
                                        (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                                          (.awaitTxBlocking database id (Duration/ofSeconds 10))
                                          (assoc tx :tx-id id))) txs)]
                      (.awaitSourceBlocking database (:tx-id (peek bound)) (Duration/ofSeconds 10))
                      (check-retained-execution! node bound)
                      (check-direct-retained-replay! node bound)
                      bound))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek bound)) (Duration/ofSeconds 10))
          (check-retained-execution! node bound)
          (check-direct-retained-replay! node bound)
          (let [bad-id (:tx-id (xt/submit-tx node [["ASSERT ? IS NOT NULL" 1]]
                                            {:system-time (time-at 3)}))]
            (.awaitSourceBlocking (db/primary-db node) bad-id (Duration/ofSeconds 10))
            (let [r (reference/compare-local-processed-retained-replay-execution! node #{:providerprobe})]
              (is (= :unsupported (:status r)))
              (is (= :retained-replay-input-not-qualified (:reason r)))
              (is (not-any? #(contains? r %) [:outcome-correspondence :replay-inputs :payload-fingerprint]))))))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))

(deftest direct-replay-requires-nonempty-qualified-local-history
  (let [compare! reference/compare-local-processed-retained-replay-execution!
        root (Files/createTempDirectory "gesso-empty-replay-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (is (= :embedded-primary-log-required (:reason (compare! nil #{:probe}))))
      (with-open [node (xtn/start-node config)]
        (let [database (db/primary-db node)
              log (.getSourceLog database)
              id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
          (.awaitSourceBlocking database id (Duration/ofSeconds 15))
          (let [r (compare! node #{:probe})]
            (is (= :unsupported (:status r)))
            (is (= :empty-retained-replay-history (:reason r)))
            (is (not (contains? r :outcome-correspondence))))))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))

(defn- check-temporal-put-execution! [node bound]
  (let [compare! reference/compare-local-processed-retained-replay-execution!
        tables #{:providerprobe}
        txs (mapv #(dissoc % :tx-id) bound)
        prediction (temporal/replay txs)
        ledger (#'reference/read-ledger! node)
        envelopes (mapv (fn [tx row]
                          (envelope/check-temporal-envelope! node (:tx-id row) (:ops tx) (:committed? row)))
                        txs ledger)
        observed {:ledger ledger :envelopes envelopes :rectangles (#'reference/read-rectangles! node tables)}
        r (compare! node tables)
        last-tx (peek bound)
        check #(envelope/check-temporal-envelope! node (:tx-id last-tx) % false)
        ops (:ops last-tx)]
    (is (= [true true true false] (mapv :committed? (:history prediction)) (mapv :committed? ledger)))
    (is (= [:matched :matched :matched :matched] (mapv :status envelopes)) (pr-str envelopes))
    (is (every? #(= envelope/temporal-profile (:profile %)) envelopes))
    (is (= :matched (:status (reference/compare-execution txs tables observed))))
    (is (= :unsupported (:status (reference/compare-execution txs tables
                                    (assoc-in observed [:envelopes 0 :profile] envelope/profile)))))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:outcome-correspondence r) (:operation-correspondence r)))
    (is (= {:ledger true :retained-envelopes true :selected-history true} (:checks r)))
    (is (= 4 (:transaction-count r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:provider-refinement r)))
    (is (not-any? #(contains? r %) [:replay-inputs :payload-fingerprint :operations]))
    (is (= :unsupported-put-shape (:reason (envelope/check-envelope! node (:tx-id last-tx) ops false))))
    ;; Even a rolled-back write must match the exact retained temporal bounds.
    (doseq [changed [(update-in ops [0 1 :valid-from] #(.plusSeconds % 1))
                     (update-in ops [0 1] dissoc :valid-to)
                     (assoc-in ops [0 2 :value] 99)]]
      (is (= :operation-sequence-mismatch (:reason (check changed)))))
    (doseq [opts [(assoc (get-in ops [0 1]) :valid-from nil)
                  (assoc (get-in ops [0 1]) :unknown true)
                  (assoc (get-in ops [0 1]) :valid-to (get-in ops [0 1 :valid-from]))
                  (update (get-in ops [0 1]) :valid-from #(.plusNanos % 1))]]
      (is (= :unsupported-temporal-put-options (:reason (check (assoc-in ops [0 1] opts))))))
    (is (= :unsupported-document-shape (:reason (check (assoc-in ops [0 2 :xt/valid-from] (time-at 0))))))
    (is (= :transaction-outcome-mismatch (:reason (envelope/check-temporal-envelope! node (:tx-id last-tx) ops true))))
    (is (= :mismatch (:status (reference/compare-source-bound-execution! node
                                (update-in bound [3 :ops 0 1 :valid-to] #(.plusSeconds % 1)) tables))))
    (is (= r (compare! node tables)))))

(deftest direct-retained-execution-with-uniform-valid-time-bounds
  (let [root (Files/createTempDirectory "gesso-temporal-envelope-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        from (java.time.Instant/parse "1969-12-31T23:59:59.999999Z")
        to (time-at 100)
        txs [[[:put-docs {:into :providerprobe :valid-from from :valid-to to}
               {:xt/id "a" :value 1} {:xt/id "b" :value 1}]]
             [[:put-docs {:into :providerprobe :valid-from (time-at 1)} {:xt/id "a" :value 2}]]
             [[:put-docs {:into :providerprobe :valid-to to} {:xt/id "b" :value 3}]]
             [[:put-docs {:into :providerprobe :valid-from from :valid-to to} {:xt/id "a" :value 4}]
              ["ASSERT FALSE"]]]]
    (try
      (let [bound (with-open [node (xtn/start-node config)]
                    (let [database (db/primary-db node)
                          bound (mapv (fn [i ops]
                                        (let [time (time-at i)
                                              id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                          (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                          {:tx-id id :system-time time :ops ops}))
                                      (range) txs)]
                      (.awaitSourceBlocking database (:tx-id (peek bound)) (Duration/ofSeconds 15))
                      (check-temporal-put-execution! node bound)
                      bound))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek bound)) (Duration/ofSeconds 15))
          (check-temporal-put-execution! node bound)))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))

(defn- check-row-temporal-execution! [node expected]
  (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
        compare! #(reference/compare-local-processed-retained-replay-execution! node #{:providerprobe})
        prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
        tx (peek expected)
        ops (:ops tx)
        check #(envelope/check-temporal-envelope! node (:tx-id tx) % false)
        r (compare!)]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
    (is (= expected (:replay-inputs sample)))
    (is (= [true true false] (mapv :committed? (:history prediction))))
    (is (= 1 (:failed-assertion-index (peek (:history prediction)))))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:outcome-correspondence r) (:operation-correspondence r)))
    (is (= 3 (:transaction-count r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:status (check ops))))
    (is (= :unsupported-put-shape (:reason (envelope/check-envelope! node (:tx-id tx) ops false))))
    (doseq [changed [(update-in ops [0 2 :xt/valid-from] #(.plusSeconds % 1))
                     (update-in ops [0 3] dissoc :xt/valid-to)
                     (update ops 0 #(assoc % 2 (nth % 3) 3 (nth % 2)))]]
      (is (= :operation-sequence-mismatch (:reason (check changed)))))
    (doseq [bad [(assoc-in ops [0 1 :row-valid-time?] false)
                 (assoc-in ops [0 1 :valid-from] (time-at 0))
                 (assoc-in ops [0 2 :xt/valid-from] nil)
                 (update-in ops [0 2 :xt/valid-from] #(.plusNanos % 1))
                 (assoc-in ops [0 3 :xt/valid-from] (get-in ops [0 3 :xt/valid-to]))]]
      (is (= :unsupported (:status (check bad))))
      (is (= :unsupported (:status (temporal/replay [{:system-time (:system-time tx) :ops bad}])))))
    (let [bad (assoc-in ops [0 3 :xt/id] (get-in ops [0 2 :xt/id]))]
      (is (= :unsupported (:status (temporal/replay [{:system-time (:system-time tx) :ops bad}])))))
    (is (= r (compare!)))))

(deftest direct-retained-execution-with-heterogeneous-row-bounds
  (let [root (Files/createTempDirectory "gesso-row-temporal-envelope-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        from (java.time.Instant/parse "1969-12-31T23:59:59.999999Z")
        to (time-at 100)
        rows [[{:xt/id "a" :value 1 :xt/valid-from from :xt/valid-to to}
               {:xt/id "b" :value 2 :xt/valid-to to}
               {:xt/id "c" :value 3}]
              [{:xt/id "a" :value 4 :xt/valid-from (time-at 1)}
               {:xt/id "b" :value 5 :xt/valid-from from :xt/valid-to (time-at 10)}]
              [{:xt/id "a" :value 6 :xt/valid-from from}
               {:xt/id "b" :value 7 :xt/valid-to to}]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [i docs]
                                              (let [time (time-at i)
                                                    ops (cond-> [(into [:put-docs :providerprobe] docs)] (= i 2) (conj ["ASSERT FALSE"]))
                                                    id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                                (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                                {:tx-id id :system-time time
                                                 :ops (assoc-in ops [0 1] {:into :providerprobe :row-valid-time? true})}))
                                            (range) rows)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-row-temporal-execution! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check-row-temporal-execution! node expected)))
      (finally (doseq [file (reverse (file-seq (.toFile root)))] (io/delete-file file true))))))

(defn- check-future-ledger! [node bound]
  (let [ids (mapv :tx-id bound)
        ledger (#'reference/read-ledger! node)
        compare! #(reference/compare-local-processed-retained-replay-execution! node #{:providerprobe})
        r (compare!)
        q xt/q
        outcome-query "SELECT committed FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2"
        check! (fn [i committed? kind]
                 (let [tx (nth bound i)]
                   (envelope/check-diagnostic-envelope! node (:tx-id tx) (:ops tx) committed? kind)))]
    (is (= ids (mapv :tx-id ledger)))
    (is (= (mapv :system-time bound) (mapv :system-time ledger)))
    (is (= [true true false] (mapv :committed? ledger)))
    (is (= [(:tx-id (first bound))]
           (mapv :xt/id (xt/q node ["SELECT _id FROM xt.txs ORDER BY _id"]))))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:outcome-correspondence r)))
    (is (= 3 (:transaction-count r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:status (reference/compare-source-bound-execution! node bound #{:providerprobe}))))
    (doseq [[i committed? kind] [[0 true :ordinary] [1 true :temporal] [2 false :ordinary]]]
      (let [e (check! i committed? kind)]
        (is (= :matched (:status e)) (pr-str e))
        (is (= envelope/diagnostic-outcome-profile (:outcome-observation-profile e)))
        (is (= :transaction-outcome-mismatch (:reason (check! i (not committed?) kind))))))
    (is (= :transaction-outcome-unavailable
           (:reason (envelope/check-envelope! node (peek ids) (:ops (peek bound)) false))))
    (doseq [[rows reason] [[[] :transaction-outcome-unavailable]
                           [[{:committed false} {:committed false}] :invalid-transaction-outcome]
                           [[{:committed nil}] :invalid-transaction-outcome]]]
      (with-redefs [xt/q (fn [node query & opts]
                          (if (= outcome-query (first query)) rows (apply q node query opts)))]
        (is (= reason (:reason (check! 2 false :ordinary))))
        (is (not= :matched (:status (compare!))))))
    (let [check envelope/check-diagnostic-envelope!]
      (with-redefs [envelope/check-diagnostic-envelope! #(dissoc (apply check %&) :outcome-observation-profile)]
        (is (= :unsupported (:status (compare!))))))
    (is (= r (compare!)))))

(deftest all-time-ledger-includes-future-dated-transactions
  (let [root (Files/createTempDirectory "gesso-all-time-ledger-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        future (java.time.Instant/parse "2100-01-01T00:00:00Z")
        txs [{:system-time (time-at 0) :ops [[:put-docs :providerprobe {:xt/id "a" :value 1}]]}
             {:system-time future :ops [[:put-docs {:into :providerprobe :valid-from future} {:xt/id "a" :value 2}]]}
             {:system-time (.plusSeconds future 1) :ops [[:put-docs :providerprobe {:xt/id "a" :value 3}] ["ASSERT FALSE"]]}]]
    (try
      (let [bound (with-open [node (xtn/start-node config)]
                    (let [database (db/primary-db node)
                          bound (mapv (fn [{:keys [ops system-time] :as tx}]
                                        (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                                          (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                          (assoc tx :tx-id id))) txs)]
                      (.awaitSourceBlocking database (:tx-id (peek bound)) (Duration/ofSeconds 15))
                      (check-future-ledger! node bound)
                      bound))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek bound)) (Duration/ofSeconds 15))
          (check-future-ledger! node bound)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
