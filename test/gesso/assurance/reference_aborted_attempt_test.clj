(ns gesso.assurance.reference-aborted-attempt-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-aborted-attempt :as attempt]
            [gesso.assurance.reference-retained-acquisition :as retained]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant ZonedDateTime]
           [xtdb.api TransactionKey]))

(defn- row-candidate [node input]
  (let [rows (xt/q node ["SELECT _id, committed, system_time, error FROM xt.txs WHERE _id = ?" (:tx-id input)])
        row (first rows)
        data (ex-data (:error row))
        key (:tx-key data)]
    (is (= 1 (count rows)))
    (is (= (:tx-id input) (:xt/id row)))
    (is (= (:system-time input) (.toInstant ^ZonedDateTime (:system-time row))))
    (assoc input :committed? (:committed row)
           :error-code (:xtdb.error/code data) :error-category (:cognitect.anomalies/category data)
           :failed-operation-index (:tx-op-idx data)
           :error-key (when (instance? TransactionKey key)
                        {:tx-id (.getTxId ^TransactionKey key) :system-time (.getSystemTime ^TransactionKey key)}))))

(deftest competing-identical-stale-claim-attempts
  (let [root (Files/createTempDirectory "gesso-competing-aborts-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        put (fn [v] [:put-docs :probe {:xt/id "r1" :value v}])
        guard ["ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = ? AND \"value\" = ?)" "r1" "open"]
        winner [guard (put "helper-1")]
        stale [guard (put "helper-2")]
        refusal [["ASSERT EXISTS (SELECT 1 FROM \"probe\" WHERE _id = ? AND \"value\" = ?)" "r1" "helper-1"]
                 [:put-docs :probe_journal {:xt/id "loser" :value "rejected"}]]]
    (try
      (with-open [node (xtn/start-node config)]
        (xt/execute-tx node [(put "open")])
        (let [capture (live/capture-tx-api-calls!
                       (fn []
                         (live/execute-tx! node winner)
                         (doseq [_ (range 2)]
                           (try (live/execute-tx! node stale)
                                (catch xtdb.error.Conflict e
                                  (is (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))))))
                         (live/execute-tx! node refusal)))
              acquisition (retained/capture! node)
              sample (retained/read-acquisition! node (:acquisition acquisition))
              candidates (mapv #(row-candidate node %) (:replay-inputs sample))
              aborted (filterv #(= :threw (:phase %)) (:events capture))
              ids (mapv :call-id aborted)
              anomalies (mapv #(get-in capture [:xtdb-anomalies %]) ids)
              rows (filterv #(false? (:committed? %)) candidates)
              match #(attempt/compare-candidates %1 %2 %3)]
          (is (= :returned (:outcome capture)))
          (is (= :issued (:status acquisition)))
          (is (= [true true false false true] (mapv :committed? candidates)))
          (is (= [stale stale] (mapv :ops rows)))
          (is (= 2 (count ids)))
          (is (apply not= ids))
          (is (= (set ids) (set (keys (:xtdb-anomalies capture)))))
          (is (= (mapv :error-key rows) (mapv :transaction-key anomalies)))
          (is (apply not= (mapv :tx-id rows)))
          (is (= (dissoc (first anomalies) :transaction-key)
                 (dissoc (second anomalies) :transaction-key)))
          (doseq [[id row other] (map vector ids rows (reverse rows))]
            (let [r (match capture id candidates)
                  no-key (update capture :xtdb-anomalies dissoc id)]
              (is (= :matched (:status r)) (pr-str r))
              (is (= (:tx-id row) (:selected-tx-id r)))
              (is (= r (match capture id (vec (reverse candidates)))))
              (is (= 2 (:candidate-count r)))
              (is (= :not-established (:aborted-receipt-identity r) (:candidate-window-completeness r)))
              (is (= :ambiguous (:status (match no-key id candidates))))
              (is (= :competing-aborted-attempts (:reason (match no-key id candidates))))
              (is (not (contains? (match no-key id candidates) :selected-tx-id)))
              (is (= :unresolved (:status (match no-key id [row]))))
              (is (= :mismatch (:status (match capture id [other]))))
              (is (= :mismatch (:status (match capture id [(assoc row :error-key (:error-key other))]))))
              (is (= :mismatch (:status (match capture id [(update row :system-time #(.plusSeconds ^Instant % 1))]))))
              (is (= :mismatch (:status (match capture id [(assoc row :committed? true)]))))
              (is (= :unsupported (:status (match capture id [row row]))))
              (is (= :unsupported (:status (match (update-in capture [:xtdb-anomalies id] dissoc :transaction-key) id candidates))))))
          (is (= [{:xt/id "r1" :value "helper-1"}] (xt/q node "SELECT _id, value FROM probe")))
          (is (= [{:xt/id "loser" :value "rejected"}] (xt/q node "SELECT _id, value FROM probe_journal")))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
