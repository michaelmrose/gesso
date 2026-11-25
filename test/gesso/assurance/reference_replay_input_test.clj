(ns gesso.assurance.reference-replay-input-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-temporal :as temporal]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.io RandomAccessFile)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant ZoneId)))

(defn- read-bytes [node]
  (let [log (.getSourceLog (db/primary-db node))
        path (#'coverage/local-log-path log)]
    (with-open [file (RandomAccessFile. (.toFile path) "r")]
      (let [frames (:frames (#'coverage/local-frames file))]
        (zipmap (map first frames) (#'coverage/read-local-payloads! file frames))))))

(defn- check-inputs! [node expected rejected]
  (let [bytes (read-bytes node)
        decode! #(input/decode-replay-input % (get bytes %))
        reports (mapv #(decode! (:tx-id %)) expected)]
    (is (= expected (mapv :transaction reports)) (pr-str reports))
    (is (every? #(= :decoded (:status %)) reports))
    (is (every? #(= :none (:authority %)) reports))
    (is (every? #(= :not-issued (:certificate %)) reports))
    (is (= [true false true]
           (mapv :committed? (:history (temporal/replay (mapv #(dissoc (:transaction %) :tx-id) reports))))))
    (doseq [id rejected]
      (let [r (decode! id)]
        (is (= :unsupported (:status r)) (pr-str r))
        (is (not (contains? r :transaction)))))
    (let [id (:tx-id (first expected)) decode-payload payload/decode-payload]
      (doseq [[k v] [[:system-time nil] [:default-tz (ZoneId/of "Asia/Tokyo")] [:user "other"]
                     [:user-metadata (byte-array [1])]]]
        (with-redefs [payload/decode-payload #(assoc-in (decode-payload %) [:message k] v)]
          (let [r (decode! id)]
            (is (= :unsupported-replay-envelope-context (:reason r)))
            (is (not (contains? r :transaction)))))))
    (is (= :invalid-replay-transaction-id (:reason (input/decode-replay-input -1 nil))))
    (is (= :unsupported-replay-envelope (:reason (input/decode-replay-input 0 (byte-array [1])))))))

(deftest replay-input-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-replay-input-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        txs [[[:put-docs :probe {:xt/id "a" :value 1}]]
             [[:put-docs :probe {:xt/id "a" :value 2}] ["ASSERT FALSE"]]
             [["ASSERT ? = ?" 3 3] ["ASSERT TRUE"]]]]
    (try
      (let [[expected rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [i ops] (:tx-id (xt/submit-tx node ops {:system-time (.plusSeconds t i)})))
                    expected (mapv (fn [i ops] {:tx-id (submit! i ops) :system-time (.plusSeconds t i) :ops ops})
                                   (range) txs)
                    rejected [(submit! 3 [["ASSERT ? IS NOT NULL" 1]])
                              (submit! 4 [[:put-docs :probe {:xt/id 5 :xt/valid-from t}
                                                   {:xt/id "c"}]])
                              (:tx-id (xt/submit-tx node [["ASSERT TRUE"]]))]]
                (.awaitSourceBlocking database (peek rejected) (Duration/ofSeconds 15))
                (check-inputs! node expected rejected)
                [expected rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek rejected) (Duration/ofSeconds 15))
          (check-inputs! node expected rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- no-replay-sample! [r]
  (is (not-any? #(contains? r %) [:replay-inputs :payload-fingerprint :operation-correspondence
                                 :operation-message-ids :payload-correspondence]))
  (is (= :none (:authority r)))
  (is (= :not-issued (:certificate r))))

(defn- check-replay-history! [node expected]
  (let [observe! #(coverage/observe-local-processed-retained-replay-inputs! node)
        decode! input/decode-replay-input
        r (observe!)]
    (is (= :observed (:status r)) (pr-str (dissoc r :replay-inputs :payload-fingerprint)))
    (is (= coverage/processed-retained-replay-inputs-profile (:processed-retained-replay-inputs-profile r)))
    (is (= input/profile (:replay-input-profile r)))
    (is (= expected (:replay-inputs r)))
    (is (= (mapv :tx-id expected) (:transaction-ids r) (:operation-message-ids r)))
    (is (= (if (seq expected) :matched :not-applicable) (:operation-correspondence r)))
    (is (= :matched (:payload-correspondence r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= (coverage/observe-local-processed-retained-history-bound! node)
           (dissoc r :processed-retained-replay-inputs-profile :replay-input-profile :replay-inputs)))
    (when (seq expected)
      (let [last-id (:tx-id (peek expected))
            first-time (:system-time (first expected))]
        (doseq [change [(fn [r] (assoc r :profile :wrong))
                        (fn [r] (update-in r [:transaction :tx-id] inc))
                        (fn [r] (dissoc r :transaction))]]
          (with-redefs [input/decode-replay-input
                        (fn [id bytes & context] (cond-> (apply decode! id bytes context) (= id last-id) change))]
            (let [r (observe!)]
              (is (= :unsupported (:status r)))
              (is (= :retained-replay-input-not-qualified (:reason r)))
              (is (= last-id (:message-id r)))
              (no-replay-sample! r))))
        (with-redefs [input/decode-replay-input
                      (fn [id bytes & context] (cond-> (apply decode! id bytes context)
                                       (= id last-id) (assoc-in [:transaction :system-time] first-time)))]
          (let [r (observe!)]
            (is (= :unsupported (:status r)))
            (is (= :retained-replay-system-time-order (:reason r)))
            (no-replay-sample! r)))
        (with-redefs-fn {#'coverage/max-replay-transactions 2}
          (fn [] (let [r (observe!)]
                   (is (= :retained-replay-transaction-limit (:reason r)))
                   (no-replay-sample! r))))
        (let [read! @#'coverage/read-local-payloads!
              seen (atom 0)]
          (with-redefs-fn {#'coverage/read-local-payloads!
                          (fn [file frames]
                            (let [sample (read! file frames)]
                              (if (= 2 (swap! seen inc)) (pop sample) sample)))}
            (fn [] (let [r (observe!)]
                     (is (= :source-payload-changed-during-observation (:reason r)))
                     (no-replay-sample! r)))))
        (let [frontier {:submitted (:last-message-id r) :processed -1 :ingestion-failed? false}]
          (with-redefs-fn {#'coverage/read-local-frontier! (constantly frontier)}
            (fn [] (let [r (observe!)]
                     (is (= :source-processing-incomplete (:reason r)))
                     (no-replay-sample! r)))))
        (is (= [true false true]
               (mapv :committed? (:history (temporal/replay (mapv #(dissoc % :tx-id) (:replay-inputs r)))))))))
    (is (= r (observe!)))))

(deftest complete-retained-replay-input-history
  (let [root (Files/createTempDirectory "gesso-replay-history-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        txs [[[:put-docs :probe {:xt/id "a" :value 1}]]
             [[:put-docs :probe {:xt/id "a" :value 2}] ["ASSERT FALSE"]]
             [["ASSERT ? = ?" 3 3] ["ASSERT TRUE"]]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             log (.getSourceLog database)
                             flush-id (.getMsgId (.appendMessageBlocking log (xtdb.api.log.SourceMessage$FlushBlock. Long/MAX_VALUE)))
                             _ (.awaitSourceBlocking database flush-id (Duration/ofSeconds 15))
                             _ (check-replay-history! node [])
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)]
                                                {:tx-id (:tx-id (xt/submit-tx node ops {:system-time time}))
                                                 :system-time time :ops ops}))
                                            (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-replay-history! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (let [database (db/primary-db node)]
            (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
            (check-replay-history! node expected)
            (let [bad-id (:tx-id (xt/submit-tx node [["ASSERT ? IS NOT NULL" 1]] {:system-time (.plusSeconds t 3)}))]
              (.awaitSourceBlocking database bad-id (Duration/ofSeconds 15))
              (let [r (coverage/observe-local-processed-retained-replay-inputs! node)]
                (is (= :unsupported (:status r)))
                (is (= bad-id (:message-id r)))
                (is (= :retained-replay-input-not-qualified (:reason r)))
                (no-replay-sample! r))
              (is (= :observed (:status (coverage/observe-local-processed-retained-history! node))))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-valid-time-inputs! [node expected]
  (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
        bytes (read-bytes node)
        id (:tx-id (first expected))
        body (get bytes id)
        decode! #(input/decode-replay-input id body)
        arrow! arrow/decode-bounded-operations
        prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
    (is (= expected (:replay-inputs sample)))
    (is (= :matched (:operation-correspondence sample)))
    (is (= :predicted (:status prediction)))
    (is (= [true true true false] (mapv :committed? (:history prediction))))
    (doseq [tx expected]
      (let [r (input/decode-replay-input (:tx-id tx) (get bytes (:tx-id tx)))]
        (is (= :decoded (:status r)))
        (is (= input/profile (:profile r)))
        (is (= tx (:transaction r)))))
    ;; Invalid retained facts are not silently coalesced.
    (doseq [alter [(fn [r] (assoc-in r [:operations 0 :documents 0 :xt/valid-from] nil))
                   (fn [r] (assoc-in r [:operations 0 :bounds] {:xt/valid-from Instant/EPOCH}))
                   (fn [r] (update-in r [:operations 0 :documents]
                                     #(mapv (fn [d] (assoc d :xt/valid-to (:xt/valid-from d))) %)))
                   (fn [r] (update-in r [:operations 0 :documents]
                                     #(mapv (fn [d] (update d :xt/valid-from
                                                          (fn [v] (.withZoneSameInstant v (ZoneId/of "Asia/Tokyo"))))) %)))]]
      (with-redefs [arrow/decode-bounded-operations (comp alter arrow!)]
        (let [r (decode!)]
          (is (= :unsupported (:status r)))
          (is (= :unsupported-replay-operation (:reason r)))
          (is (not (contains? r :transaction))))))))

(deftest uniform-valid-time-replay-from-retained-history
  (let [root (Files/createTempDirectory "gesso-replay-valid-time-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        from (Instant/parse "1969-12-31T23:59:59.999999Z")
        to (.plusSeconds t 100)
        txs [[[:put-docs {:into :probe :valid-from from :valid-to to}
               {:xt/id "a" :value 1} {:xt/id "b" :value 1}]]
             [[:put-docs {:into :probe :valid-from t} {:xt/id "a" :value 2}]]
             [[:put-docs {:into :probe :valid-to to} {:xt/id "b" :value 3}]]
             [[:put-docs {:into :probe :valid-from from :valid-to to} {:xt/id "a" :value 4}]
              ["ASSERT FALSE"]]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)]
                                                {:tx-id (:tx-id (xt/submit-tx node ops {:system-time time}))
                                                 :system-time time :ops ops}))
                                            (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-valid-time-inputs! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check-valid-time-inputs! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-implicit-history! [node ids future txs]
  (let [log (.getSourceLog (db/primary-db node))
        frames (with-open [file (RandomAccessFile. (.toFile (#'coverage/local-log-path log)) "r")]
                 (:frames (#'coverage/local-frames file)))
        micros (second (first (filter #(= (first ids) (first %)) frames)))
        stamp (Instant/ofEpochSecond (Math/floorDiv (long micros) 1000000)
                                     (* 1000 (Math/floorMod (long micros) 1000000)))
        times [stamp future (.plusNanos future 1000) (.plusNanos future 2000)]
        expected (mapv (fn [id time ops] {:tx-id id :system-time time :ops ops}) ids times txs)
        sample (coverage/observe-local-processed-retained-replay-inputs! node)
        r (reference/compare-local-processed-retained-replay-execution! node #{:probe})
        ledger (#'reference/read-ledger! node)
        body (get (read-bytes node) (nth ids 2))]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
    (is (= expected (:replay-inputs sample)))
    (is (= times (mapv :system-time ledger)))
    (is (= [true true false true] (mapv :committed? ledger)))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:outcome-correspondence r)))
    (is (= :unsupported (:status (input/decode-replay-input (nth ids 2) body))))
    (doseq [[m prior expected-time] [[-1 nil (Instant/parse "1969-12-31T23:59:59.999999Z")]
                                    [0 Instant/EPOCH (.plusNanos Instant/EPOCH 1000)]
                                    [2 Instant/EPOCH (.plusNanos Instant/EPOCH 2000)]]]
      (is (= expected-time (get-in (input/decode-replay-input (nth ids 2) body m prior) [:transaction :system-time]))))
    (doseq [[m prior] [[nil nil] [(inc (bigint Long/MAX_VALUE)) nil] [0 "invalid"]
                       [0 (Instant/ofEpochSecond 9223372036854 775807000)]]]
      (is (= :unsupported (:status (input/decode-replay-input (nth ids 2) body m prior)))))
    (let [decode! input/decode-replay-input]
      (with-redefs [input/decode-replay-input
                    (fn [id body micros prior]
                      (cond-> (decode! id body micros prior)
                        (= id (peek ids)) (assoc-in [:transaction :system-time] future)))]
        (let [r (coverage/observe-local-processed-retained-replay-inputs! node)]
          (is (= :retained-replay-system-time-order (:reason r)))
          (no-replay-sample! r))))))

(deftest implicit-system-time-from-retained-frames
  (let [root (Files/createTempDirectory "gesso-implicit-replay-" (make-array FileAttribute 0))
        clock (java.time.Clock/fixed (Instant/parse "2026-01-01T00:00:00Z") (ZoneId/of "UTC"))
        config {:log [:local {:path (str root "/log") :instant-src clock}]
                :storage [:local {:path (str root "/storage")}]}
        future (Instant/parse "2026-01-02T00:00:00Z")
        txs [[[:put-docs :probe {:xt/id "a" :value 1}]]
             [[:put-docs :probe {:xt/id "a" :value 2}]]
             [[:put-docs :probe {:xt/id "a" :value 3}] ["ASSERT FALSE"]]
             [[:put-docs :probe {:xt/id "a" :value 4}]]]]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        a (:tx-id (xt/submit-tx node (nth txs 0)))
                        b (:tx-id (xt/submit-tx node (nth txs 1) {:system-time future}))
                        c (:tx-id (xt/submit-tx node (nth txs 2)))
                        _ (.awaitSourceBlocking database c (Duration/ofSeconds 15))
                        log (.getSourceLog database)
                        _ (.appendMessageBlocking log (xtdb.api.log.SourceMessage$FlushBlock. Long/MAX_VALUE))
                        d (:tx-id (xt/submit-tx node (nth txs 3)))
                        ids [a b c d]]
                    (.awaitSourceBlocking database d (Duration/ofSeconds 15))
                    (check-implicit-history! node ids future txs)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
          (check-implicit-history! node ids future txs)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-clocked-history! [node expected]
  (let [observe! #(coverage/observe-local-processed-retained-clocked-inputs! node)
        decode! input/decode-clocked-replay-input
        sample (observe!)
        ids (mapv #(get-in % [:transaction :tx-id]) expected)
        fail! (fn [r reason]
                (is (not= :observed (:status r)))
                (is (= reason (:reason r)))
                (is (not (contains? r :clocked-replay-inputs)))
                (no-replay-sample! r))]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :clocked-replay-inputs :payload-fingerprint)))
    (is (= coverage/processed-retained-clocked-inputs-profile (:processed-retained-clocked-inputs-profile sample)))
    (is (= input/clocked-input-profile (:clocked-replay-input-profile sample)))
    (is (= expected (:clocked-replay-inputs sample)))
    (is (= ids (:transaction-ids sample) (:operation-message-ids sample)))
    (is (= (coverage/observe-local-processed-retained-history-bound! node)
           (dissoc sample :processed-retained-clocked-inputs-profile :clocked-replay-input-profile :clocked-replay-inputs)))
    (when (seq expected)
      (doseq [change [#(assoc % :profile :wrong)
                      #(update % :frame-micros inc)
                      #(assoc % :requested-system-time Instant/EPOCH)
                      #(assoc-in % [:clock :decision] :reject-before-execution)
                      #(update-in % [:transaction :system-time] (fn [t] (.plusNanos t 1000)))
                      #(update-in % [:transaction :tx-id] inc)
                      #(assoc % :status :unsupported)]]
        (with-redefs [input/decode-clocked-replay-input
                      (fn [id bytes micros prior]
                        (cond-> (decode! id bytes micros prior) (= id (peek ids)) change))]
          (let [r (observe!)]
            (is (= (peek ids) (:message-id r)))
            (fail! r :retained-clocked-input-not-qualified))))
      (with-redefs-fn {#'coverage/max-replay-transactions 2}
        #(fail! (observe!) :retained-replay-transaction-limit))
      (let [read! @#'coverage/read-local-payloads!
            seen (atom 0)]
        (with-redefs-fn {#'coverage/read-local-payloads!
                         (fn [file frames]
                           (let [b (read! file frames)]
                             (if (= 2 (swap! seen inc)) (pop b) b)))}
          #(fail! (observe!) :source-payload-changed-during-observation)))
      (let [read! @#'coverage/read-local-frontier!
            seen (atom 0)]
        (with-redefs-fn {#'coverage/read-local-frontier!
                         (fn [node]
                           (cond-> (read! node) (= 2 (swap! seen inc)) (update :processed dec)))}
          #(fail! (observe!) :source-frontier-changed-during-observation))))
    (is (= sample (observe!)))))

(defn- check-clocked-execution! [node]
  (let [compare! #(reference/compare-local-processed-retained-clocked-execution! node #{:probe})
        report (compare!)
        observe! coverage/observe-local-processed-retained-clocked-inputs!
        sample (observe! node)
        read! @#'reference/read-clocked-outcomes!
        fail! (fn [r status reason]
                (is (= status (:status r)) (pr-str r))
                (is (= reason (:reason r)))
                (is (not-any? #(contains? r %) [:checks :clock-correspondence :outcome-correspondence
                                                :clocked-replay-inputs :payload-fingerprint])))]
    (is (= :matched (:status report)) (pr-str report))
    (is (= reference/retained-clocked-execution-profile (:retained-clocked-execution-profile report)))
    (is (= :matched (:clock-correspondence report) (:outcome-correspondence report)))
    (is (= {:ledger true :retained-envelopes true :selected-history true :outcome-kind true} (:checks report)))
    (is (= 4 (:transaction-count report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:provider-refinement report)))
    (is (not-any? #(contains? report %) [:clocked-replay-inputs :payload-fingerprint :history :rectangles]))
    (with-redefs-fn {#'reference/read-clocked-outcomes!
                     (fn [node ids] (assoc-in (read! node ids) [2 :error-code] :xtdb/assert-failed))}
      #(fail! (compare!) :mismatch :clocked-outcome-mismatch))
    (let [seen (atom 0)]
      (with-redefs-fn {#'reference/read-clocked-outcomes!
                       (fn [node ids] (cond-> (read! node ids) (= 2 (swap! seen inc))
                                        (assoc-in [2 :error-code] :xtdb/assert-failed)))}
        #(fail! (compare!) :unavailable :ledger-changed-during-observation)))
    (let [seen (atom 0)]
      (with-redefs [coverage/observe-local-processed-retained-clocked-inputs!
                    (fn [_] (cond-> sample (= 2 (swap! seen inc))
                              (assoc-in [:clocked-replay-inputs 0 :transaction :ops 0 2 :value] 99)))]
        (fail! (compare!) :unavailable :source-log-changed-during-comparison)))
    (with-redefs [coverage/observe-local-processed-retained-clocked-inputs!
                  (constantly (assoc-in sample [:clocked-replay-inputs 2 :clock :decision] :execute))]
      (fail! (compare!) :unsupported :unsupported-clocked-expectation))
    (is (= report (compare!)))))

(defn- check-clock-decisions! [node ids t]
  (let [log (.getSourceLog (db/primary-db node))
        frames (with-open [file (RandomAccessFile. (.toFile (#'coverage/local-log-path log)) "r")]
                 (:frames (#'coverage/local-frames file)))
        micros (into {} (map (juxt first second) frames))
        bodies (read-bytes node)
        requests (mapv #(get-in (payload/decode-payload (get bodies %)) [:message :system-time]) ids)
        reports (reduce (fn [rs [id requested]]
                          (conj rs (input/resolve-retained-clock (get micros id) requested (:system-time (peek rs)))))
                        [] (map vector ids requests))
        clocked (reduce (fn [rs id]
                          (conj rs (input/decode-clocked-replay-input
                                    id (get bodies id) (get micros id)
                                    (get-in (peek rs) [:transaction :system-time])))) [] ids)
        times [t t (.plusNanos t 1000) (.plusNanos t 2000)]
        ledger (#'reference/read-ledger! node)]
    (check-clocked-history! node clocked)
    (check-clocked-execution! node)
    (let [r (temporal/replay-clocked (:clocked-replay-inputs (coverage/observe-local-processed-retained-clocked-inputs! node)))
          history (:history r)
          rejected (nth history 2)]
      (is (= :predicted (:status r)) (pr-str r))
      (is (= temporal/clocked-profile (:profile r)))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (= ledger (mapv #(select-keys % [:tx-id :system-time :committed?]) history)))
      (is (= requests (mapv :requested-system-time history)))
      (is (= (mapv :decision reports) (mapv :clock-decision history)))
      (is (= :invalid-system-time (:rejection rejected)))
      (is (not (contains? rejected :failed-assertion-index)))
      (is (= (:segments (nth history 1)) (:segments rejected)))
      (is (= (#'reference/canonical-rectangles (:rectangles r))
             (#'reference/canonical-rectangles (#'reference/read-rectangles! node #{:probe}))))
      (is (= #{"a" "b" "d"} (set (keys (:probe (temporal/visible-at r (peek times) (peek times)))))))
      ;; Count executions of the state transition, not grammar validation.
      (let [execute! @#'temporal/execute-transaction! calls (atom [])]
        (with-redefs-fn {#'temporal/execute-transaction!
                         (fn [prior time ops] (swap! calls conj time) (execute! prior time ops))}
          #(is (= r (temporal/replay-clocked clocked))))
        (is (= [(first times) (second times) (peek times)] @calls)))
      (let [attempts (update-in clocked [3 :transaction :ops] conj ["ASSERT FALSE"])
            result (temporal/replay-clocked attempts)
            abort (peek (:history result))]
        (is (= :predicted (:status result)))
        (is (= false (:committed? abort)))
        (is (= 1 (:failed-assertion-index abort)))
        (is (not (contains? abort :rejection)))
        (is (= (:segments rejected) (:segments abort))))
      (doseq [bad [(assoc clocked 1 nil)
                   (assoc clocked 1 false)
                   (assoc clocked 1 {})
                   (assoc-in clocked [1 :transaction] :invalid)
                   (assoc-in clocked [1 :transaction :tx-id] (first ids))
                   (assoc-in clocked [2 :clock :decision] :execute)
                   (assoc-in clocked [2 :requested-system-time] t)
                   (assoc-in clocked [2 :frame-micros] nil)
                   (assoc-in clocked [2 :transaction :system-time] t)
                   (assoc-in clocked [2 :transaction :ops] [[:unknown]])
                   (update-in clocked [1 :transaction :ops] conj ["ASSERT CURRENT_TIMESTAMP < ?" (.plusSeconds t 1)])
                   []]]
        (let [failure (temporal/replay-clocked bad)]
          (is (= :unsupported (:status failure)) (pr-str failure))
          (is (not-any? #(contains? failure %) [:history :rectangles])))))
    (is (every? #(= :decoded (:status %)) clocked))
    (is (every? #(= input/clocked-input-profile (:profile %)) clocked))
    (is (every? #(= :none (:authority %)) clocked))
    (is (every? #(= :not-issued (:certificate %)) clocked))
    (is (= reports (mapv :clock clocked)))
    (is (= requests (mapv :requested-system-time clocked)))
    (is (= (mapv micros ids) (mapv :frame-micros clocked)))
    (is (= (mapv (fn [id time doc-id]
                   {:tx-id id :system-time time :ops [[:put-docs :probe {:xt/id doc-id :value 1}]]})
                 ids times ["a" "b" "c" "d"])
           (mapv :transaction clocked)))
    (let [id (nth ids 2)
          body (get bodies id)
          stamp (micros id)
          decode! #(input/decode-clocked-replay-input id body %1 %2)
          arrow! arrow/decode-bounded-operations
          payload! payload/decode-payload
          fail! (fn [r reason]
                  (is (= :unsupported (:status r)))
                  (is (= reason (:reason r)))
                  (is (not-any? #(contains? r %) [:transaction :clock :requested-system-time :frame-micros])))]
      (fail! (decode! nil t) :invalid-clock-frame)
      (fail! (decode! stamp "bad") :invalid-prior-system-time)
      (with-redefs [payload/decode-payload #(assoc-in (payload! %) [:message :user] "other")]
        (fail! (decode! stamp t) :unsupported-replay-envelope-context))
      (with-redefs [arrow/decode-bounded-operations #(assoc (arrow! %) :profile :wrong)]
        (fail! (decode! stamp t) :unsupported-replay-operation-bytes))
      (with-redefs [arrow/decode-bounded-operations #(assoc-in (arrow! %) [:operations 0 :bounds] {:unknown true})]
        (fail! (decode! stamp t) :unsupported-replay-operation)))
    (is (= [t t (.minusSeconds t 1) nil] requests))
    (is (every? #(= :resolved (:status %)) reports))
    (is (every? #(= input/clock-profile (:profile %)) reports))
    (is (every? #(= :none (:authority %)) reports))
    (is (every? #(= :not-issued (:certificate %)) reports))
    (is (= [:execute :execute :reject-before-execution :execute] (mapv :decision reports)))
    (is (= [:explicit :explicit :smoothed :smoothed] (mapv :time-source reports)))
    (is (= times (mapv :system-time reports) (mapv :system-time ledger)))
    (is (= ids (mapv :tx-id ledger)))
    (is (= [true true false true] (mapv :committed? ledger)))
    (is (= :invalid-system-time (:reason (nth reports 2))))
    ;; Equal-time execution is allowed, not a guarantee of commit. A
    ;; backwards attempted write must leave no persisted document.
    (is (= #{"a" "b" "d"} (set (map :xt/id (xt/q node ["SELECT _id FROM probe FOR VALID_TIME ALL FOR SYSTEM_TIME ALL"])))))
    ;; The existing strict temporal replay profile remains unchanged.
    (is (= :retained-replay-system-time-order
           (:reason (coverage/observe-local-processed-retained-replay-inputs! node))))))

(deftest retained-clock-decisions-for-equal-and-backwards-explicit-times
  (let [root (Files/createTempDirectory "gesso-clock-decisions-" (make-array FileAttribute 0))
        clock (java.time.Clock/fixed (Instant/parse "2026-01-01T00:00:00Z") (ZoneId/of "UTC"))
        config {:log [:local {:path (str root "/log") :instant-src clock}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-02T00:00:00Z")]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        flush-id (.getMsgId (.appendMessageBlocking (.getSourceLog database) (xtdb.api.log.SourceMessage$FlushBlock. Long/MAX_VALUE)))
                        _ (.awaitSourceBlocking database flush-id (Duration/ofSeconds 15))
                        _ (check-clocked-history! node [])
                        ids (mapv (fn [[id requested]]
                                    (:tx-id (xt/submit-tx node [[:put-docs :probe {:xt/id id :value 1}]]
                                                          (cond-> {} requested (assoc :system-time requested)))))
                                  [["a" t] ["b" t] ["c" (.minusSeconds t 1)] ["d" nil]])]
                    (.awaitSourceBlocking database (peek ids) (Duration/ofSeconds 15))
                    (check-clock-decisions! node ids t)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
          (check-clock-decisions! node ids t)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest retained-clock-decision-boundaries
  (let [resolve! input/resolve-retained-clock
        epoch Instant/EPOCH
        before (.minusNanos epoch 1000)
        after (.plusNanos epoch 1000)
        max-time (Instant/ofEpochSecond 9223372036854 775807000)]
    (doseq [[micros requested prior expected decision source]
            [[-1 nil nil before :execute :frame]
             [0 epoch epoch epoch :execute :explicit]
             [2 before epoch (.plusNanos epoch 2000) :reject-before-execution :frame]
             [0 before epoch after :reject-before-execution :smoothed]
             [0 nil epoch after :execute :smoothed]
             [0 max-time max-time max-time :execute :explicit]]]
      (let [r (resolve! micros requested prior)]
        (is (= :resolved (:status r)))
        (is (= [expected decision source] ((juxt :system-time :decision :time-source) r)))))
    (doseq [[micros requested prior reason]
            [[nil epoch nil :invalid-clock-frame]
             [0 (.plusNanos epoch 1) nil :invalid-requested-system-time]
             [0 nil "invalid" :invalid-prior-system-time]
             [0 nil max-time :retained-clock-overflow]
             [0 epoch max-time :retained-clock-overflow]]]
      (let [r (resolve! micros requested prior)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)))
        (is (not-any? #(contains? r %) [:system-time :decision :time-source]))))))

(deftest clocked-execution-distinguishes-rejection-from-assertion-abort
  (let [root (Files/createTempDirectory "gesso-clocked-outcomes-" (make-array FileAttribute 0))
        clock (java.time.Clock/fixed (Instant/parse "2026-01-01T00:00:00Z") (ZoneId/of "UTC"))
        config {:log [:local {:path (str root "/log") :instant-src clock}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-02T00:00:00Z")
        put (fn [v] [:put-docs :probe {:xt/id "a" :value v}])]
    (try
      (with-open [node (xtn/start-node config)]
        (let [ids (mapv (fn [[ops time]]
                          (:tx-id (xt/submit-tx node ops (cond-> {} time (assoc :system-time time)))))
                        [[[(put 1)] t]
                         [[(put 2) ["ASSERT FALSE"]] (.minusSeconds t 1)]
                         [[(put 3) ["ASSERT FALSE"]] nil]])
              _ (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
              compare! #(reference/compare-local-processed-retained-clocked-execution! node #{:probe})
              report (compare!)
              read! @#'reference/read-clocked-outcomes!]
          (is (= :matched (:status report)) (pr-str report))
          (is (= [{:committed? true} {:committed? false :error-code :invalid-system-time}
                  {:committed? false :error-code :xtdb/assert-failed :failed-assertion-index 1}]
                 (read! node ids)))
          (doseq [change [#(assoc-in % [1 :error-code] :xtdb/assert-failed)
                          #(assoc-in % [2 :error-code] :invalid-system-time)
                          #(assoc-in % [2 :failed-assertion-index] 0)]]
            (with-redefs-fn {#'reference/read-clocked-outcomes! (fn [node ids] (change (read! node ids)))}
              (fn [] (let [r (compare!)]
                       (is (= :mismatch (:status r)))
                       (is (= :clocked-outcome-mismatch (:reason r)))
                       (is (not (contains? r :clock-correspondence)))))))
          (is (= report (compare!)))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-state-assertion-inputs! [node expected]
  (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
        prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
        compare! #(reference/compare-local-processed-retained-replay-execution! node #{:probe})
        report (compare!)
        id (:tx-id (second expected))
        body (get (read-bytes node) id)
        arrow! arrow/decode-bounded-operations]
    (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs :payload-fingerprint)))
    (is (= expected (:replay-inputs sample)))
    (is (= :predicted (:status prediction)))
    (is (= [true true false true] (mapv :committed? (:history prediction))))
    (is (= 1 (:failed-assertion-index (nth (:history prediction) 2))))
    (is (= :matched (:status report)) (pr-str report))
    (is (= :matched (:outcome-correspondence report)))
    (is (= 4 (:transaction-count report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
    (doseq [bad [{:kind :sql :query "ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = $2 AND \"value\" = $1)" :args ["a" 1]}
                 {:kind :sql :query "ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = $1 AND \"value\" = $1)" :args ["a" 1]}
                 {:kind :sql :query "ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = $1 AND \"value\" = $2)" :args [1 1]}
                 {:kind :sql :query "ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = $1 AND \"value\" = $2)" :args ["a" :other]}
                 {:kind :sql :query "ASSERT $1 = (SELECT COUNT(*) FROM \"probe\" WHERE _id = $2)" :args [1]}
                 {:kind :sql :query "ASSERT $1 = (SELECT COUNT(*) FROM \"probe\")" :args [16385]}
                 {:kind :sql :query "ASSERT 0 = (SELECT COUNT(*) FROM \"probe\" WHERE _id = $01)" :args ["a"]}
                 {:kind :sql :query "ASSERT 0 = (SELECT COUNT(*) FROM \"probe\" WHERE _id = $1) -- comment" :args ["a"]}]]
      (with-redefs [arrow/decode-bounded-operations #(assoc-in (arrow! %) [:operations 0] bad)]
        (let [r (input/decode-replay-input id body)]
          (is (= :unsupported (:status r)) (pr-str bad))
          (is (not (contains? r :transaction))))))
    (is (= report (compare!)))))

(deftest strict-retained-state-dependent-assertions
  (let [root (Files/createTempDirectory "gesso-retained-state-assertions-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        put (fn [v] [:put-docs :probe {:xt/id "a" :value v}])
        guard (fn [v] ["ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = ? AND \"value\" = ?)" "a" v])
        txs [[(put 1) ["ASSERT 1 = (SELECT COUNT(*) FROM \"probe\")"]]
             [(guard 1) (put 2) (guard 2) ["ASSERT ? = (SELECT COUNT(*) FROM \"probe\" WHERE _id = ?)" 1 "a"]]
             [(put 3) (guard 2)]
             [(guard 2) ["ASSERT ? = (SELECT COUNT(*) FROM \"probe\")" 1]
              ["ASSERT 0 = (SELECT COUNT(*) FROM \"probe\" WHERE _id = ?)" "missing"]]]]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             expected (mapv (fn [i ops]
                                              (let [time (.plusSeconds t i)
                                                    id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                                (.awaitTxBlocking database id (Duration/ofSeconds 15))
                                                {:tx-id id :system-time time :ops ops})) (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 15))
                         (check-state-assertion-inputs! node expected)
                         expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 15))
          (check-state-assertion-inputs! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest strict-retained-assertion-only-table-coverage
  (let [root (Files/createTempDirectory "gesso-assertion-table-coverage-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        txs [{:system-time t :ops [[:put-docs :probe {:xt/id "a" :value 1}]
                                  ["ASSERT 0 = (SELECT COUNT(*) FROM \"unwritten\")"]]}
             {:system-time (.plusSeconds t 1) :ops [["ASSERT FALSE"]
                                  ["ASSERT 0 = (SELECT COUNT(*) FROM \"unreached\" WHERE _id = ?)" "a"]]}]
        tables #{:probe :unwritten :unreached}
        check! (fn [node bound]
                 (let [compare! #(reference/compare-local-processed-retained-replay-execution! node %)
                       r (compare! tables)
                       read! @#'reference/read-rectangles!
                       scanned (atom [])]
                   (is (= tables (temporal/required-observation-tables txs)))
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= 2 (:transaction-count r)))
                   (doseq [omitted [:unwritten :unreached :probe]]
                     (let [selected (disj tables omitted)]
                       (is (= :unsupported (:status (compare! selected))))
                       (with-redefs [xt/q (fn [& _] (throw (AssertionError. "incomplete scope must not query")))]
                         (is (= :unsupported (:status (reference/compare-quiescent-execution! node txs selected))))
                         (is (= :unsupported (:status (reference/compare-source-bound-execution! node bound selected)))))
                       (is (= :unsupported (:status (reference/compare-execution txs selected {}))))))
                   (with-redefs-fn {#'reference/read-rectangles! (fn [node selected]
                                                                 (swap! scanned conj selected)
                                                                 (read! node selected))}
                     #(is (= r (compare! tables))))
                   (is (= [tables] @scanned))
                   (with-redefs-fn {#'reference/read-rectangles! (fn [node selected]
                                                                 (let [rows (read! node selected)]
                                                                   (conj rows (assoc (first rows) :table :unwritten))))}
                     #(is (= :mismatch (:status (compare! tables)))))))]
    (try
      (let [bound (with-open [node (xtn/start-node config)]
                    (let [database (db/primary-db node)
                          bound (mapv (fn [{:keys [system-time ops] :as tx}]
                                        (assoc tx :tx-id (:tx-id (xt/submit-tx node ops {:system-time system-time})))) txs)]
                      (.awaitSourceBlocking database (:tx-id (peek bound)) (Duration/ofSeconds 15))
                      (check! node bound)
                      bound))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek bound)) (Duration/ofSeconds 15))
          (check! node bound)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
