(ns gesso.live.outbox.delivery-test
  "v2096: actual source acceptance, trusted lease progression and fenced
   terminal acknowledgment. No SSE/browser receipt or process-restart proof."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.core :as live]
            [gesso.live.dispatch :as dispatch]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.source :as source]
            [manifold.stream :as s]
            [gesso.live.outbox.read-frontier :as frontier]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
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
                                   (fn [_ submission]
                                     {:status :contended
                                      :intent-id (:intent-id submission)})]
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
                    (with-redefs [live.xtdb/q-from (fn [& _]
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

(deftest invalid-acknowledgment-result-after-source-emission-requires-reconciliation-test
  ;; A status of :acknowledged is not physical receipt evidence when the
  ;; returned intent, lease or receipt differs from the guarded submission.
  ;; The source has already emitted by this point: never classify these
  ;; contradictions as safe pre-dispatch rejection or completed delivery.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)]
      (try
        (doseq [[label distort]
                [[:wrong-intent #(assoc % :intent-id (random-uuid))]
                 [:missing-intent #(dissoc % :intent-id)]
                 [:wrong-lease #(assoc % :lease-id (random-uuid))]
                 [:missing-lease #(dissoc % :lease-id)]
                 [:wrong-receipt #(assoc % :receipt-id (random-uuid))]
                 [:missing-receipt #(dissoc % :receipt-id)]
                 [:unknown-status #(assoc % :status :surprising)]
                 [:missing-status #(dissoc % :status)]
                 [:nil-result (constantly nil)]]]
          (let [id (random-uuid)
                _ (seed! node id)
                lease (acquire! node id)
                tap (source/subscribe (:source system)
                                      {:topic :request :id subject-id})
                calls (atom [])]
            (try
              (let [outcome
                    (with-redefs
                      [acknowledgment/acknowledge-dispatch!
                       (fn [_ submission]
                         (swap! calls conj submission)
                         (distort {:status :acknowledged
                                   :intent-id (:intent-id submission)
                                   :lease-id (:lease-id submission)
                                   :receipt-id (get-in submission
                                                       [:receipt :receipt-id])}))]
                      (try
                        (delivery/dispatch-acquired! node system {} lease
                                                     source-id)
                        (catch Throwable error error)))
                    data (when (instance? clojure.lang.ExceptionInfo outcome)
                           (ex-data outcome))
                    receipt-id (get-in (first @calls) [:receipt :receipt-id])]
                (is (instance? clojure.lang.ExceptionInfo outcome)
                    (str label " must not report success"))
                (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                       (:error/type data))
                    (str label " must require post-emission reconciliation"))
                (is (= :invalid-acknowledgment-result (:reason data)))
                (is (= :accepted (:dispatch/status data)))
                (is (= :unknown (:acknowledgment/status data)))
                (is (= id (:intent-id data)))
                (is (= (:lease-id lease) (:lease-id data)))
                (is (uuid? receipt-id))
                (is (= receipt-id (:receipt-id data))
                    "Exact submitted receipt is necessary for reconciliation")
                (is (= 1 (count @calls)) "No implicit acknowledgment retry")
                (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout)))
                    "Source emission preceded acknowledgment uncertainty")
                (is (= :leased (:gesso.live.outbox/status (row node id)))
                    "Fake positive acknowledgment did not commit XTDB")
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
              (finally
                (s/close! tap)))))
        (finally
          (live/close! system))))))

(deftest post-dispatch-ack-exception-retains-submitted-receipt-test
  ;; A real acknowledgment adapter can fail after submitting its XTDB CAS.
  ;; Retain the receipt and lease identities even when the callback throws,
  ;; because physical commit is then uncertain and the source already sent.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)
          attempted (atom nil)
          cause (ex-info "XTDB acknowledgment indeterminate" {:physical :unknown})]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              tap (source/subscribe (:source system)
                                    {:topic :request :id subject-id})]
          (try
            (let [failure
                  (with-redefs
                    [acknowledgment/acknowledge-dispatch!
                     (fn [_ submission]
                       (reset! attempted submission)
                       (throw cause))]
                    (try
                      (delivery/dispatch-acquired! node system {} lease source-id)
                      (catch Throwable error error)))
                  data (ex-data failure)]
              (is (instance? clojure.lang.ExceptionInfo failure))
              (is (identical? cause (.getCause ^Throwable failure)))
              (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                     (:error/type data)))
              (is (= :accepted (:dispatch/status data)))
              (is (= :unknown (:acknowledgment/status data)))
              (is (= id (:intent-id data)))
              (is (= (:lease-id lease) (:lease-id data)))
              (is (= (get-in @attempted [:receipt :receipt-id])
                     (:receipt-id data)))
              (is (uuid? (:receipt-id data)))
              (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout))))
              (is (= :leased (:gesso.live.outbox/status (row node id)))))
            (finally
              (s/close! tap))))
        (finally
          (live/close! system))))))

(deftest malformed-delivery-current-page-never-emits-or-acks-test
  ;; This is an actual leased XTDB row. Even a non-vector *containing that
  ;; valid row* is not an XTDB q result and must not authorize emission.
  ;; Nil, empty maps/seqs and bad vector entries are not safe missing rows.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              physical (row node id)
              invalid [[:nil nil]
                       [:empty-map {}]
                       [:empty-sequence '()]
                       [:java-list (java.util.ArrayList.)]
                       [:one-row-list (list physical)]
                       [:one-row-sequence (seq [physical])]
                       [:nil-row [nil]]
                       [:false-row [false]]]
              before (row node id)]
          (doseq [[case-name provider-result] invalid]
            (let [tap (source/subscribe (:source system)
                                        {:topic :request :id subject-id})
                  reads (atom 0)
                  acks (atom 0)]
              (try
                (let [failure (with-redefs
                                [live.xtdb/q-from (fn [& _]
                                        (swap! reads inc)
                                        provider-result)
                                 acknowledgment/acknowledge-dispatch!
                                 (fn [_ submitted]
                                   (swap! acks inc)
                                   {:status :acknowledged
                                    :intent-id (:intent-id submitted)
                                    :lease-id (:lease-id submitted)
                                    :receipt-id (get-in submitted
                                                        [:receipt :receipt-id])})]
                                (try
                                  (delivery/dispatch-acquired!
                                   node system {} lease source-id)
                                  (catch Throwable e e)))]
                  (is (instance? clojure.lang.ExceptionInfo failure)
                      (str case-name " must fail with a typed physical-read error"))
                  (is (= :gesso.live.outbox/invalid-delivery
                         (:error/type (ex-data failure)))
                      (str case-name " must retain delivery's error type"))
                  (is (= :invalid-physical-current-page
                         (:reason (ex-data failure)))
                      (str case-name " must reject the provider container"))
                  (is (= 1 @reads) "Never retry a malformed physical read")
                  (is (zero? @acks) "Never ack an invalid physical query")
                  (is (= ::timeout (deref (s/take! tap) 75 ::timeout))
                      "Never emit an invalidation on a malformed preflight")
                  (is (= before (row node id))
                      "No physical lease or receipt change"))
                (finally (s/close! tap))))))
        (finally (live/close! system))))))

(deftest vector-delivery-current-page-has-explicit-missing-row-behavior-test
  ;; Only a legitimate XTDB vector can represent observed absence. An empty
  ;; vector does not authorize an emission; a genuine leased row vector does.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              physical (xt/q node
                             ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?"
                              id])
              taps (source/subscribe (:source system)
                                     {:topic :request :id subject-id})]
          (try
            (is (vector? physical))
            (is (= 1 (count physical)))
            (is (= :missing-or-ambiguous-intent
                   (:reason (with-redefs [live.xtdb/q-from (fn [& _] [])]
                              (rejection
                               #(delivery/dispatch-acquired!
                                 node system {} lease source-id))))))
            (is (= ::timeout (deref (s/take! taps) 75 ::timeout))
                "A missing physical row cannot emit an invalidation")
            (is (= :leased (:gesso.live.outbox/status (row node id))))
            (finally (s/close! taps))))
        (finally (live/close! system))))))

(deftest malformed-negative-ack-is-post-dispatch-unknown-test
  ;; :contended and :unavailable can be trusted only with their exact
  ;; returned shapes. Conflicting receipt/lease credentials or a fictional
  ;; reason cannot establish a known negative after Live already emitted.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          invalid [[:contended-with-reason
                    {:status :contended :reason :not-leased}]
                   [:contended-with-lease
                    {:status :contended :lease-id (random-uuid)}]
                   [:contended-with-receipt
                    {:status :contended :receipt-id (random-uuid)}]
                   [:unavailable-no-reason
                    {:status :unavailable}]
                   [:unavailable-unknown-reason
                    {:status :unavailable :reason :arbitrary}]
                   [:unavailable-with-lease
                    {:status :unavailable :reason :missing
                     :lease-id (random-uuid)}]
                   [:unavailable-with-receipt
                    {:status :unavailable :reason :not-leased
                     :receipt-id (random-uuid)}]
                   [:unavailable-with-worker
                    {:status :unavailable :reason :not-leased
                     :worker-id "different-worker"}]]]
      (try
        (doseq [[case-name malformed] invalid]
          (let [id (random-uuid)
                _ (seed! node id)
                lease (acquire! node id)
                tap (source/subscribe (:source system)
                                      {:topic :request :id subject-id})
                attempts (atom [])]
            (try
              (let [failure (with-redefs
                              [acknowledgment/acknowledge-dispatch!
                               (fn [_ submission]
                                 (swap! attempts conj submission)
                                 (assoc malformed :intent-id (:intent-id submission)))]
                              (try
                                (delivery/dispatch-acquired!
                                 node system {} lease source-id)
                                (catch Throwable e e)))
                    data (when (instance? clojure.lang.ExceptionInfo failure)
                           (ex-data failure))
                    submitted (first @attempts)]
                (is (instance? clojure.lang.ExceptionInfo failure)
                    (str case-name " cannot be a known negative"))
                (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                       (:error/type data)))
                (is (= :invalid-acknowledgment-result (:reason data)))
                (is (= :accepted (:dispatch/status data)))
                (is (= :unknown (:acknowledgment/status data)))
                (is (= id (:intent-id data)))
                (is (= (:lease-id lease) (:lease-id data)))
                (is (= (get-in submitted [:receipt :receipt-id])
                       (:receipt-id data))
                    "Reconciliation requires the submitted, not returned, receipt")
                (is (= 1 (count @attempts)) "No automatic ack retry")
                (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout)))
                    "Live source accepted before the malformed ack result")
                (is (= :leased (:gesso.live.outbox/status (row node id))))
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
              (finally
                (s/close! tap)))))
        (finally
          (live/close! system))))))

(deftest canonical-negative-acknowledgment-results-still-remain-known-test
  ;; Positive control: the actual adapter returns :contended with no reason,
  ;; or :unavailable with either :missing or :not-leased. These still mean
  ;; accepted source emission followed by an acknowledged rejection of the
  ;; terminal state transition, NOT a successful delivery receipt.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)]
      (try
        (doseq [[status reason] [[:contended nil]
                                 [:unavailable :missing]
                                 [:unavailable :not-leased]]]
          (let [id (random-uuid)
                _ (seed! node id)
                lease (acquire! node id)
                tap (source/subscribe (:source system)
                                      {:topic :request :id subject-id})]
            (try
              (let [outcome (with-redefs
                              [acknowledgment/acknowledge-dispatch!
                               (fn [_ submission]
                                 (cond-> {:status status
                                          :intent-id (:intent-id submission)}
                                   reason (assoc :reason reason)))]
                              (delivery/dispatch-acquired!
                               node system {} lease source-id))]
                (is (= :dispatched-unacknowledged (:status outcome)))
                (is (= status (get-in outcome [:acknowledgment :status])))
                (is (= reason (get-in outcome [:acknowledgment :reason])))
                (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout))))
                (is (= :leased (:gesso.live.outbox/status (row node id))))
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
              (finally (s/close! tap)))))
        (finally (live/close! system))))))

(deftest acknowledged-label-without-commit-proof-is-post-dispatch-unknown-test
  ;; Source acceptance occurs BEFORE acknowledgment. A matched status/intent/
  ;; lease/receipt with no committed transaction result cannot establish a
  ;; durable terminal update, even when the trusted adapter claims success.
  ;; Equally, reusing the acquisition's earlier transaction is not a new ack.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              prior {:tx-result (:tx-result lease)
                     :consistency (:consistency lease)
                     :progression (:progression lease)}
              cases [[:label-only (constantly {})]
                     [:missing-tx-result #(dissoc % :tx-result)]
                     [:missing-consistency #(dissoc % :consistency)]
                     [:missing-progression #(dissoc % :progression)]
                     [:reused-acquisition-receipt identity]
                     [:conflicting-tx-id
                      #(assoc-in % [:consistency :tx-id]
                                  (inc (get-in % [:consistency :tx-id])))]
                     [:conflicting-progression
                      #(assoc % :progression {:not-a-valid-basis true})]
                     [:unusable-tx-result
                      #(assoc % :tx-result {:tx-id :not-a-tx})]]]
          (doseq [[scenario corrupt] cases]
            (let [tap (source/subscribe (:source system)
                                        {:topic :request :id subject-id})
                  submissions (atom [])]
              (try
                (let [outcome
                      (with-redefs
                        [acknowledgment/acknowledge-dispatch!
                         (fn [_ submission]
                           (swap! submissions conj submission)
                           (merge {:status :acknowledged
                                   :intent-id (:intent-id submission)
                                   :lease-id (:lease-id submission)
                                   :receipt-id (get-in submission
                                                       [:receipt :receipt-id])}
                                  (corrupt prior)))]
                        (try
                          (delivery/dispatch-acquired!
                           node system {} lease source-id)
                          (catch Throwable e e)))
                      data (when (instance? clojure.lang.ExceptionInfo outcome)
                             (ex-data outcome))
                      issued (first @submissions)]
                  (is (instance? clojure.lang.ExceptionInfo outcome)
                      (str scenario " must not report completed delivery"))
                  (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                         (:error/type data))
                      (str scenario " must require reconciliation"))
                  (is (= :invalid-acknowledgment-result (:reason data)))
                  (is (= :accepted (:dispatch/status data)))
                  (is (= :unknown (:acknowledgment/status data)))
                  (is (= id (:intent-id data)))
                  (is (= (:lease-id lease) (:lease-id data)))
                  (is (= (get-in issued [:receipt :receipt-id])
                         (:receipt-id data)))
                  (is (= 1 (count @submissions))
                      "Uncertain acknowledgments must not retry")
                  (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout)))
                      "Live source acceptance occurred before the bad receipt")
                  (is (= :leased (:gesso.live.outbox/status (row node id)))
                      "False proof cannot terminalize the physical outbox")
                  (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id)))))
                (finally (s/close! tap))))))
        (finally (live/close! system))))))

(deftest real-ack-transaction-advances-acquisition-transaction-test
  ;; The native XTDB acknowledgment supplies complete committed coordinates,
  ;; and its transaction must be later than the physical lease acquisition.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              outcome (delivery/dispatch-acquired!
                       node system {} lease source-id)
              ack (:acknowledgment outcome)]
          (is (= :dispatch-accepted (:status outcome)))
          (is (= :acknowledged (:status ack)))
          (is (integer? (get-in ack [:consistency :tx-id])))
          (is (> (get-in ack [:consistency :tx-id])
                 (get-in lease [:consistency :tx-id])))
          (is (= (:consistency ack)
                 (live.xtdb/tx-result-consistency
                  (:tx-result ack))))
          (is (= (:progression ack)
                 (live.xtdb/tx-result-progression
                  (:tx-result ack))))
          (is (= :dispatch-accepted (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))


(deftest partial-source-emission-failure-needs-reconciliation-test
  ;; source/emit-many! is explicitly non-atomic with source shutdown. One
  ;; invalidation may have reached the source before the next emit fails. A
  ;; raw exception with no physical lease identity loses reconciliation data.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)]
      (try
        (doseq [interrupt? [false true]]
          (let [id (random-uuid)
                _ (seed! node id)
                lease (acquire! node id)
                tap (source/subscribe (:source system)
                                      {:topic :request :id subject-id})
                physical-cause (if interrupt?
                                 (InterruptedException. "second source emit interrupted")
                                 (ex-info "source stopped after first emission"
                                          {:physical :partial}))
                emits (atom 0)
                acknowledgments (atom 0)]
            (try
              (Thread/interrupted)
              (let [failure (with-redefs
                              [live/emit-many!
                               (fn [running invalidations]
                                 (swap! emits inc)
                                 (live/emit! running (first invalidations))
                                 (throw physical-cause))
                               acknowledgment/acknowledge-dispatch!
                               (fn [& _]
                                 (swap! acknowledgments inc)
                                 (throw (AssertionError.
                                         "No acknowledgment after uncertain source emission")))]
                              (try
                                (delivery/dispatch-acquired! node system {} lease
                                                             source-id)
                                (catch Throwable e e)))
                    cancelled? (.isInterrupted (Thread/currentThread))]
                (Thread/interrupted)
                (is (instance? clojure.lang.ExceptionInfo failure))
                (when (instance? clojure.lang.ExceptionInfo failure)
                  (let [details (ex-data failure)]
                    (is (identical? physical-cause (.getCause ^Throwable failure)))
                    (is (= :gesso.live.outbox/source-emission-unknown
                           (:error/type details)))
                    (is (= :source-emission-exception (:reason details)))
                    (is (= :unknown (:source-acceptance details)))
                    (is (= :unknown (:dispatch/status details)))
                    (is (= :not-attempted (:acknowledgment/status details)))
                    (is (true? (:rescan-required? details)))
                    (is (= id (:intent-id details)))
                    (is (= (:lease-id lease) (:lease-id details)))))
                (is (= interrupt? cancelled?)
                    "Physical InterruptedException must restore JVM cancellation")
                (is (= 1 @emits) "Do not repeat a possibly partial emission")
                (is (zero? @acknowledgments) "Never acknowledge a partial emission")
                (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout)))
                    "The Live source actually accepted one invalidation")
                (is (= :leased (:gesso.live.outbox/status (row node id))))
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id
                           (row node id)))))
              (finally
                (Thread/interrupted)
                (s/close! tap)))))
        (finally
          (Thread/interrupted)
          (live/close! system))))))

(deftest plausible-source-report-without-exact-callback-return-is-not-acknowledged
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              tap (source/subscribe (:source system) {:topic :request :id subject-id})
              source-id' (get-in system [:source :id])
              real-run dispatch/run-sync!]
          (try
            (doseq [[runner emitted?]
                    [[(fn [_] {:status :ran
                               :result {:status :emitted-many :source/id source-id'
                                        :count 1 :results [{:status :emitted :source/id source-id'}]}}) false]
                     [(fn [job] (update (real-run job) :result #(into {} %))) true]]]
              (let [data (with-redefs [dispatch/run-sync! runner]
                           (rejection #(delivery/dispatch-acquired! node system {} lease source-id)))]
                (is (= :gesso.live.outbox/source-emission-unknown (:error/type data)))
                (is (= :unverified-dispatch-acceptance (:reason data)))
                (is (= :not-attempted (:acknowledgment/status data)))
                (is (= :leased (:gesso.live.outbox/status (row node id))))
                (is (nil? (:gesso.live.outbox/dispatch-receipt-id (row node id))))
                (let [notice @(s/try-take! tap ::timeout (if emitted? 1500 100) ::timeout)]
                  (is (if emitted? (= subject-id (:id notice)) (= ::timeout notice))))))
            (finally (s/close! tap))))
        (finally (live/close! system))))))

(deftest inconsistent-synchronous-emission-report-is-uncertain-after-acceptance-test
  ;; A trusted dispatch adapter may report an incomplete/contradictory
  ;; :emitted-many outcome after actual physical source acceptance. It cannot
  ;; be classified as a known pre-dispatch validation error or acknowledged.
  (with-open [node (xtn/start-node {})]
    (let [system (create-system)
          id (random-uuid)
          acknowledgments (atom 0)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              tap (source/subscribe (:source system)
                                    {:topic :request :id subject-id})]
          (try
            (let [failure (with-redefs
                            [dispatch/run-sync!
                             (fn [job]
                               ((:run job))
                               {:status :ran
                                :result {:status :emitted-many
                                         :count 0 :results []}})
                             acknowledgment/acknowledge-dispatch!
                             (fn [& _]
                               (swap! acknowledgments inc)
                               (throw (AssertionError. "Never acknowledge")))]
                            (try
                              (delivery/dispatch-acquired! node system {} lease
                                                           source-id)
                              (catch Throwable e e)))
                  data (ex-data failure)]
              (is (= :gesso.live.outbox/source-emission-unknown
                     (:error/type data)))
              (is (= :unverified-dispatch-acceptance (:reason data)))
              (is (= :unknown (:source-acceptance data)))
              (is (= :not-attempted (:acknowledgment/status data)))
              (is (= id (:intent-id data)))
              (is (= (:lease-id lease) (:lease-id data)))
              (is (true? (:rescan-required? data)))
              (is (= :unverified-dispatch-acceptance
                     (:reason (ex-data (.getCause ^Throwable failure)))))
              (is (zero? @acknowledgments))
              (is (= subject-id (:id (deref (s/take! tap) 1500 ::timeout))))
              (is (= :leased (:gesso.live.outbox/status (row node id)))))
            (finally
              (s/close! tap))))
        (finally
          (live/close! system))))))

(deftest delivery-awaits-selected-node-frontier-before-emission-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id)
              selected :selected-node
              calls (atom [])]
          (with-redefs [xt/status (fn [n]
                                   (swap! calls conj [:status n])
                                   {:latest-submitted-msg-ids {"xtdb" [42]}})
                        live.xtdb/q-from (fn [n q opts]
                                           (swap! calls conj [:query n q opts]) [])
                        dispatch/run-sync! (fn [& _] (throw (AssertionError. "Unexpected emission")))
                        acknowledgment/acknowledge-dispatch! (fn [& _] (throw (AssertionError. "Unexpected ack")))]
            (is (= :missing-or-ambiguous-intent
                   (:reason (rejection #(delivery/dispatch-acquired!
                                         selected system {} lease source-id)))))
            (is (= [[:status selected]
                    [:query selected ["SELECT *, _system_from FROM gesso_live_outbox WHERE _id = ?" id]
                     {:await-token (basis/->tx-basis-str {"xtdb" [42]})}]] @calls))
            (reset! calls [])
            (with-redefs [xt/status (constantly {:latest-submitted-msg-ids {}})]
              (is (= ::frontier/invalid-frontier
                     (:error/type (rejection #(delivery/dispatch-acquired!
                                               selected system {} lease source-id)))))
              (is (empty? @calls)))))
        (finally (live/close! system))))))

(deftest delivery-observes-submitted-lease-change-before-emission-test
  (with-open [node (xtn/start-node {})]
    (let [system (create-system) id (random-uuid)]
      (try
        (seed! node id)
        (let [lease (acquire! node id) old (row node id)]
          (xt/submit-tx node [[:put-docs outbox/intent-table
                               (assoc (dissoc old :xt/system-from)
                                      :gesso.live.outbox/lease-owner "other-worker")]])
          (with-redefs [dispatch/run-sync! (fn [& _] (throw (AssertionError. "Stale lease emitted")))]
            (is (= :stale-or-unsupported-lease
                   (:reason (rejection #(delivery/dispatch-acquired!
                                         node system {} lease source-id))))))
          (is (= :leased (:gesso.live.outbox/status (row node id)))))
        (finally (live/close! system))))))
