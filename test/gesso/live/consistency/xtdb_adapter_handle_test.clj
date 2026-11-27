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

(deftest node-registration-is-not-database-wide-writer-isolation
  ;; Deliberate negative witnesses on a disposable database. None of these
  ;; direct/alternate paths should be used by a mapped application.
  (with-open [node (xtn/start-node {})]
    (let [seen (atom [])
          release (adapter/register-submission-policy!
                   node (fn [envelope] (swap! seen conj envelope) false))
          handle (adapter/adapter-connectable! node)
          ops (fn [id] [[:put-docs :ingress_audit {:xt/id id :value 1}]])]
      (try
        (is (= :submission-policy-refused
               (reason #(adapter/execute-tx! handle (ops "refused")))))
        (is (= :submission-policy-refused
               (reason #(adapter/execute-tx! node (ops "refused")))))
        (is (= :submission-policy-refused
               (reason #(adapter/submit-tx! handle (ops "refused")))))
        (is (= 3 (count @seen)))
        (is (empty? (adapter/q handle ["SELECT * FROM ingress_audit"])))
        ;; Raw XTDB does not consult the adapter registration.
        (is (some? (xt/execute-tx node (ops "raw"))))
        ;; A second connectable reaches the SAME database, but the adapter
        ;; registry is keyed by object identity, not database identity.
        (with-open [conn (.getConnection ^javax.sql.DataSource node)]
          (is (not (identical? node conn)))
          (is (nil? (adapter/current-submission-policy-token conn)))
          (is (some? (:tx-result (adapter/execute-tx! conn (ops "connection")))))
          (is (= #{{:xt/id "raw" :value 1} {:xt/id "connection" :value 1}}
                 (set (xt/q conn ["SELECT * FROM ingress_audit"])))))
        (is (= 3 (count @seen)))
        (finally (release))))))

(deftest policy-release-is-not-an-in-flight-drain
  ;; Intentionally violate the release-after-close owner contract, on a
  ;; disposable node, to show why the independent lifecycle fence matters.
  (with-open [node (xtn/start-node {})]
    (let [entered (promise) proceed (promise)
          release (adapter/register-submission-policy!
                   node (fn [_] (deliver entered true)
                          (true? (deref proceed 10000 false))))
          handle (adapter/adapter-connectable! node)
          ops [[:put-docs :ingress_audit {:xt/id "in-flight" :value 1}]]
          call (future (adapter/execute-tx! handle ops))]
      (try
        (is (= true (deref entered 10000 ::timeout)))
        (release)
        (is (not (realized? call)))
        (is (= :invalid-adapter-connectable
               (reason #(adapter/execute-tx! handle ops))))
        (deliver proceed true)
        (is (some? (:tx-result (deref call 20000 ::timeout))))
        (is (= [{:xt/id "in-flight" :value 1}]
               (xt/q node ["SELECT * FROM ingress_audit"])))
        (finally
          (deliver proceed false)
          (deref call 20000 ::timeout)
          (release))))))

(deftest owned-policy-fences-and-drains-entered-submissions
  (let [node (xtn/start-node {})
        entered (promise) proceed (promise)
        owner (adapter/register-owned-submission-policy!
               node (fn [_] (deliver entered true) (true? (deref proceed 10000 false))))
        {:keys [status fence-and-await! release!]} owner
        handle (adapter/adapter-connectable! node)
        ops [[:put-docs :ingress_audit {:xt/id "owned" :value 1}]]
        call (future (adapter/execute-tx! handle ops))]
    (try
      (is (= true (deref entered 10000 ::timeout)))
      (is (= {:phase :open :in-flight 1} (status)))
      (is (= :owned-submission-not-drained (reason release!)))
      (is (= :owned-submission-drain-timeout (reason #(fence-and-await! 0))))
      (is (= {:phase :fenced :in-flight 1} (status)))
      (is (= :owned-submission-fenced (reason #(adapter/execute-tx! handle ops))))
      (is (= :owned-submission-fenced (reason #(adapter/submit-tx! node ops))))
      (is (= :owned-submission-not-drained (reason release!)))
      (deliver proceed true)
      (is (= {:phase :fenced :in-flight 0} (fence-and-await! 20000)))
      (is (some? (:tx-result (deref call 20000 ::timeout))))
      (is (= [{:xt/id "owned" :value 1}] (adapter/q handle ["SELECT * FROM ingress_audit"])))
      (is (= :owned-submission-fenced (reason #(adapter/execute-tx! node ops))))
      (is (= {:phase :fenced :in-flight 0} (fence-and-await! 0)))
      (finally
        (deliver proceed false)
        (fence-and-await! 20000)
        (.close ^java.lang.AutoCloseable node)
        (release!)))
    (is (= {:phase :released :in-flight 0} (status)))
    (is (= :invalid-adapter-connectable (reason #(adapter/execute-tx! handle ops))))
    (is (nil? (release!)))))

(deftest owned-policy-drains-refusals-and-exceptions
  (let [node (Object.)
        owner (adapter/register-owned-submission-policy!
               node (fn [{:keys [tx-ops]}]
                      (if (= "throw" (get-in tx-ops [0 2 :xt/id]))
                        (throw (ex-info "Admission failed" {:reason :test-exception})) false)))
        {:keys [status fence-and-await! release!]} owner
        ops (fn [id] [[:put-docs :items {:xt/id id}]])]
    (try
      (is (= :submission-policy-refused (reason #(adapter/execute-tx! node (ops "refuse")))))
      (is (= :test-exception (reason #(adapter/execute-tx! node (ops "throw")))))
      (is (= {:phase :open :in-flight 0} (status)))
      (is (= :invalid-drain-timeout (reason #(fence-and-await! -1))))
      (is (= {:phase :fenced :in-flight 0} (fence-and-await! 0)))
      (finally (fence-and-await! 0) (release!)))))
