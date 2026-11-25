(ns gesso.live.consistency.xtdb-adapter-handle-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.live.outbox.read-frontier :as frontier]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest issued-adapter-handle-never-bypasses-physical-policy
  (with-open [node (xtn/start-node {})]
    (let [ops [[:put-docs :items {:xt/id "one" :value 1}]]
          bad [[:put-docs :items {:xt/id "bad" :value 2}]]
          seen (atom [])]
      (is (= :missing-adapter-policy (reason #(adapter/adapter-connectable! node))))
      (let [release (adapter/register-submission-policy!
                     node (fn [envelope]
                            (swap! seen conj envelope)
                            (and (= :execute-tx (:api envelope)) (= ops (:tx-ops envelope)))))
            handle (adapter/adapter-connectable! node)
            ctx {:xtdb/connectable handle :biff.xtdb/node handle}
            forged (adapter/->AdapterConnectable)
            deferred (adapter/plan-q handle ["SELECT * FROM items"])]
        (try
          (is (not (identical? node handle)))
          (is (not (map? handle)))
          (is (not (instance? javax.sql.DataSource handle)))
          (is (identical? handle (adapter/connectable-from ctx)))
          (is (identical? handle (adapter/node-from ctx)))
          (is (identical? handle (adapter/node-from handle)))
          (is (adapter/same-connectable? node handle))
          (is (= :invalid-adapter-connectable (reason #(adapter/q forged ["SELECT 1"]))))
          (is (thrown? Exception (xt/execute-tx handle bad)))
          (is (empty? @seen))
          (is (= :submission-policy-refused (reason #(adapter/execute-tx-from! ctx bad))))
          (is (= :submission-policy-refused (reason #(adapter/submit-tx! handle ops))))
          (is (some? (:tx-result (adapter/execute-tx-from! ctx ops))))
          (is (= 3 (count @seen)))
          (let [query ["SELECT * FROM items"]
                expected [{:xt/id "one" :value 1}]]
            (is (= expected (adapter/q-from ctx query)))
            (is (= expected (into [] (adapter/plan-q-from ctx query))))
            (is (= expected (frontier/q-from! ctx query)))
            (is (= expected (:rows (frontier/q-with-basis! ctx query))))
            (is (= expected (xt/q node query))))
          (finally (release)))
        (is (= :invalid-adapter-connectable (reason #(into [] deferred))))
        (is (= :invalid-adapter-connectable (reason #(adapter/status handle))))
        (is (= :invalid-adapter-connectable (reason #(adapter/execute-tx! handle bad))))
        (let [release-new (adapter/register-submission-policy! node (constantly true))]
          (try
            (is (= :invalid-adapter-connectable (reason #(adapter/q handle ["SELECT 1"]))))
            (is (= :invalid-adapter-connectable (reason #(adapter/execute-tx! handle bad))))
            (is (adapter/same-connectable? node (adapter/adapter-connectable! node)))
            (finally (release-new))))))))
