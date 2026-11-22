(ns gesso.live.outbox.delivery-test
  "v2096: actual source acceptance, trusted lease progression and fenced
   terminal acknowledgment. No SSE/browser receipt or process-restart proof."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.core :as live]
            [gesso.live.dispatch :as dispatch]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.source :as source]
            [manifold.stream :as s]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(def ^:private source-id "gesso/test-delivery")
(def ^:private subject-id "v2096-request")

(defn- create-system
  ([] (create-system true))
  ([matching?]
   (live/create {:rules (if matching?
                          [{:when-topic :request
                            :expand (fn [_ctx change]
                                      [(select-keys change [:topic :id :change/kind])])}]
                          [{:when-topic :request :expand (fn [_ctx _change] [])}])
                 :source-options {:coalesce-window-ms 0}
                 :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}})))

(defn- seed! [node id]
  (let [plan (outbox/prepare!
              {:intent-ids [id] :source-id source-id
               :operation :request/claim :purpose :gesso.live/refresh}
              [{:topic :request :id subject-id :change/kind :updated}])]
    (xt/execute-tx node (into (vec (:guard-ops plan))
                              [[:put-docs :requests {:xt/id subject-id :status :claimed}]
                               (:tx-op plan)]))))

(defn- acquire! [node id]
  (let [now (Instant/now)]
    (acquisition/acquire-pending!
      node {:intent-id id :worker-id "worker-a" :lease-id (random-uuid)
            :now now :lease-until (.plusSeconds now 60)})))

(defn- row [node id]
  (first (xt/q node ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- rejection [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest acquired-outbox-emits-real-invalidation-with-lease-basis-and-acks-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [sse-input (source/subscribe (:source system)
                                          {:topic :request :id subject-id})
              lease (acquire! node id)
              output (delivery/dispatch-acquired! node system {} lease source-id)
              invalidation (deref (s/take! sse-input) 1500 ::timeout)
              persisted (row node id)]
          (is (= :acquired (:status lease)))
          (is (= :dispatch-accepted (:status output)))
          (is (= 1 (get-in output [:source-result :count])))
          (is (= :emitted (get-in output [:source-result :results 0 :status])))
          (is (= :acknowledged (get-in output [:acknowledgment :status])))
          (is (= {:topic :request :id subject-id :change/kind :updated}
                 (select-keys invalidation [:topic :id :change/kind])))
          (is (= (:progression lease) (:progression invalidation)))
          (is (= (:progression lease) (:progression output)))
          (is (= :dispatch-accepted (:gesso.live.outbox/status persisted)))
          (is (= (:receipt-id output)
                 (:gesso.live.outbox/dispatch-receipt-id persisted)))
          (is (= :gesso.live/synchronous-source
                 (:gesso.live.outbox/dispatch-adapter persisted)))
          (is (empty? (:candidates (discovery/discover-eligible! node {:source-id source-id}))))
          (s/close! sse-input))
        (finally (live/close! system))))))

(deftest worker-acquisition-result-can-feed-delivery-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [work (worker/acquire-sweep! node {:source-id source-id :worker-id "worker-a" :max-pages 1})
              lease (first (:results work))
              output (delivery/dispatch-acquired! node system {} lease source-id)]
          (is (= 1 (:acquired work)))
          (is (= :dispatch-accepted (:status output)))
          ;; Zero listeners does not mean a browser received the notice.
          (is (= 0 (get-in output [:source-result :results 0 :attempted])))
          (is (= :dispatch-accepted (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))

(deftest closed-source-failure-never-acks-outbox-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)]
          (source/close! (:source system))
          (is (thrown? clojure.lang.ExceptionInfo
                       (delivery/dispatch-acquired! node system {} lease source-id)))
          (is (= :leased (:gesso.live.outbox/status (row node id))))
          (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
        (finally (live/close! system))))))

(deftest unmatched-rule-does-not-silently-ack-without-notices-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system false) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)]
          (is (= :no-invalidations
                 (:reason (rejection #(delivery/dispatch-acquired!
                                       node system {} lease source-id)))))
          (is (= :leased (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))

(deftest invalid-identity-source-and-basis-reject-before-source-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              tap (source/subscribe (:source system)
                                    {:topic :request :id subject-id})]
          (doseq [[candidate expected-source reason]
                  [[(assoc lease :status :contended) source-id :not-acquired]
                   [(assoc lease :intent-id "invalid") source-id :invalid-intent-id]
                   [(assoc lease :progression nil) source-id :invalid-acquisition-basis]
                   [(assoc-in lease [:consistency :tx-id] 9999)
                    source-id :invalid-acquisition-basis]
                   [lease "other/source" :source-mismatch]]]
            (is (= reason (:reason (rejection #(delivery/dispatch-acquired!
                                               node system {} candidate
                                               expected-source))))))
          (is (= ::timeout (deref (s/take! tap) 100 ::timeout)))
          (is (= :leased (:gesso.live.outbox/status (row node id))))
          (s/close! tap))
        (finally (live/close! system))))))

(deftest stale-physical-version-refuses-a-previously-valid-claim-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              old (row node id)]
          (xt/execute-tx node [[:put-docs outbox/intent-table
                                (assoc (dissoc old :xt/system-from)
                                       :gesso.live.outbox/lease-owner "another-worker")]])
          (is (= :stale-or-unsupported-lease
                 (:reason (rejection #(delivery/dispatch-acquired!
                                       node system {} lease source-id)))))
          (is (= :leased (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))

(deftest async-queue-acceptance-is-not-acknowledged-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)]
          (with-redefs [dispatch/run-sync!
                        (fn [_job] {:status :submitted :job-id 123})]
            (is (= :unverified-dispatch-acceptance
                   (:reason (rejection #(delivery/dispatch-acquired!
                                         node system {} lease source-id))))))
          (is (= :leased (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))

(deftest acceptance-but-competing-terminal-update-is-not-reported-as-ack-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [tap (source/subscribe (:source system)
                                    {:topic :request :id subject-id})
              lease (acquire! node id)
              result (with-redefs [acknowledgment/acknowledge-dispatch!
                                   (fn [_ _] {:status :contended})]
                       (delivery/dispatch-acquired! node system {} lease source-id))]
          (is (= :dispatched-unacknowledged (:status result)))
          (is (= :contended (get-in result [:acknowledgment :status])))
          (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout))))
          (is (= :leased (:gesso.live.outbox/status (row node id))))
          (s/close! tap))
        (finally (live/close! system))))))

(deftest interrupted-post-acceptance-ack-preserves-uncertainty-and-interrupt-test
  ;; An actual Live source acceptance precedes the acknowledgement call.
  ;; An interrupted physical ack is unknowable: neither reinterpret it as a
  ;; rejected dispatch nor erase the caller's JVM cancellation signal while
  ;; wrapping the exact failure in the post-dispatch-unknown envelope.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [tap (source/subscribe (:source system)
                                    {:topic :request :id subject-id})
              lease (acquire! node id)
              original (InterruptedException. "acknowledgment interrupted")]
          (try
            (Thread/interrupted)
            (let [failure
                  (with-redefs [acknowledgment/acknowledge-dispatch!
                                (fn [_ _] (throw original))]
                    (try
                      (delivery/dispatch-acquired!
                       node system {} lease source-id)
                      (catch Throwable error error)))
                  interrupted? (.isInterrupted (Thread/currentThread))]
              (Thread/interrupted)
              (is (instance? clojure.lang.ExceptionInfo failure))
              (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                     (:error/type (ex-data failure))))
              (is (= :accepted (:dispatch/status (ex-data failure))))
              (is (= :unknown (:acknowledgment/status (ex-data failure))))
              (is (identical? original (.getCause ^Throwable failure)))
              (is interrupted?
                  "An ack interruption must remain observable after wrapping")
              (is (= subject-id (:id (deref (s/take! tap)
                                              1500 ::timeout))))
              (is (= :leased (:gesso.live.outbox/status (row node id))))
              (is (nil? (:gesso.live.outbox/dispatch-receipt-id
                         (row node id)))))
            (finally
              (Thread/interrupted)
              (s/close! tap))))
        (finally
          (Thread/interrupted)
          (live/close! system))))))

(deftest interrupted-delivery-preflight-read-keeps-original-error-and-no-emission-test
  ;; The physical current-row read precedes Live source acceptance. A failed
  ;; read is NOT proof that a lease is missing, present, or still current.
  ;; Preserve the original InterruptedException and JVM cancellation signal,
  ;; without emitting any invalidation or attempting an acknowledgment.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)]
      (try
        (doseq [phase [:current :stale]]
          (let [id (random-uuid)
                _ (seed! node id)
                lease (acquire! node id)
                _ (when (= phase :stale)
                    (xt/execute-tx node
                                   [[:put-docs outbox/intent-table
                                     (-> (row node id)
                                         (dissoc :xt/system-from)
                                         (assoc :gesso.live.outbox/lease-owner
                                                "other-worker"))]]))
                before (row node id)
                tap (source/subscribe (:source system)
                                      {:topic :request :id subject-id})
                cause (InterruptedException.
                       (str (name phase) " lease preflight interrupted"))
                reads (atom 0)
                acknowledgments (atom 0)]
            (try
              (Thread/interrupted)
              (let [failure
                    (with-redefs [xt/q (fn [& _]
                                         (swap! reads inc)
                                         (throw cause))
                                  acknowledgment/acknowledge-dispatch!
                                  (fn [& _]
                                    (swap! acknowledgments inc)
                                    (throw (AssertionError.
                                            "No ack before physical read")))]
                      (try
                        (delivery/dispatch-acquired!
                         node system {} lease source-id)
                        (catch Throwable error error)))
                    interrupted? (.isInterrupted (Thread/currentThread))]
                (Thread/interrupted)
                (is (identical? cause failure)
                    (str phase " must propagate exact physical read failure"))
                (is interrupted?
                    (str phase " must preserve JVM cancellation signal"))
                (is (= 1 @reads)
                    "A failed physical preflight must not be retried")
                (is (zero? @acknowledgments)
                    "No acknowledgement can follow indeterminate preflight")
                (is (= ::timeout (deref (s/take! tap) 100 ::timeout))
                    "No invalidation can be emitted before preflight")
                (is (= before (row node id))
                    "Delivery preflight does not change durable lease state")
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id
                           (row node id)))))
              (finally
                (Thread/interrupted)
                (s/close! tap)))))
        (finally
          (Thread/interrupted)
          (live/close! system))))))

(deftest unknown-post-acceptance-ack-outcome-has-explicit-classification-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              error (with-redefs [acknowledgment/acknowledge-dispatch!
                                  (fn [_ _] (throw (ex-info "unknown commit" {})))]
                      (rejection #(delivery/dispatch-acquired!
                                   node system {} lease source-id)))]
          (is (= :gesso.live.outbox/post-dispatch-ack-unknown (:error/type error)))
          (is (= :accepted (:dispatch/status error)))
          (is (= :unknown (:acknowledgment/status error)))
          (is (= :leased (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))
