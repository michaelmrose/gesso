(ns gesso.assurance.proof.xtdb-gateway-biff-bounded-mediation-test
  "v1566: permanent adversarial checks for opt-in, fair, bounded request admission.
   Concurrent callers are allowed into a finite queue; their guarded Ring
   executions remain exclusive over one process-local registry. No proof of
   parallel execution, deployed handler adoption or outside-writer closure."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-request-mediation :as mediation]
            [gesso.assurance.proof.xtdb-gateway-biff-request-mediation-test :as mt]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as bh]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as app]))

(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                          (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (fixture run))))

(defn- private-fn [ns sym] (var-get (ns-resolve ns sym)))
(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))
(defn- ctx [behavior policy]
  (let [ctx ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation-test 'context) behavior)
        source! (fn [_]
                  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                               'gateway-source) ctx))
        issued (mediation/issue-bounded-queued-biff-request-mediation-module!
                (:adoption ctx) (:source ctx) (:inventory ctx) (:registry ctx)
                {} {wc/op wc/path} wc/*node* source! :revision policy)]
    (assoc ctx :queue-issued issued)))
(defn- start! [ctx]
  (let [m (get-in ctx [:queue-issued :module])]
    ((:biff.core/init m) nil)
    ((:biff.core/start m) (:system ctx))))
(defn- dispatch [ctx final]
  ((get final app/biff-ring-handler-key) (:request ctx)))
(defn- revision [ctx]
  (:revision (wc/physical (:before ctx))))
(defn- policy [n wait-ms]
  {:max-pending-requests n :wait-timeout-ms wait-ms})

(deftest admission-policy-validation-is-fail-closed
  (let [validate (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                             'bounded-concurrency-policy!)]
    (doseq [bad [nil {} {:max-pending-requests 1 :wait-timeout-ms 10}
                 {:max-pending-requests 65 :wait-timeout-ms 10}
                 {:max-pending-requests 2 :wait-timeout-ms 0}
                 {:max-pending-requests 2 :wait-timeout-ms 60001}
                 {:max-pending-requests 2.0 :wait-timeout-ms 5}
                 {:max-pending-requests 2 :wait-timeout-ms 5 :extra true}]]
      (is (= :invalid-bounded-mediation-policy
             (reason #(validate bad)))))))

(deftest bounded-dispatch-rejects-recursion-before-waiting
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        self (atom nil)
        nested (atom nil)
        wrapped (wrap (fn [x]
                        (reset! nested (reason #(@self x)))
                        :completed)
                      {:capacity 2 :wait-ms 3000})]
    (reset! self wrapped)
    (is (= :completed (wrapped :request)))
    (is (= :recursive-biff-mediation-dispatch @nested))
    (is (= :completed (wrapped :next)))))

(deftest bounded-dispatch-timeout-releases-admission
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise)
        done (wrap (fn [x]
                     (when (= x :first)
                       (deliver entered true)
                       @release)
                     x)
                   {:capacity 2 :wait-ms 100})
        first-call (future (done :first))]
    (try
      (is (= true (deref entered 15000 :not-entered)))
      (is (= :bounded-mediation-wait-timeout (reason #(done :second))))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timed-out)))
    (is (= :third (done :third)))))

(deftest bounded-dispatch-concurrent-calls-are-sequential
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise)
        running (atom 0) peak (atom 0)
        wrapped (wrap (fn [x]
                        (let [n (swap! running inc)]
                          (swap! peak max n)
                          (try
                            (when (= x 1)
                              (deliver entered true)
                              @release)
                            x
                            (finally (swap! running dec)))))
                      {:capacity 2 :wait-ms 3000})
        first-call (future (wrapped 1))]
    (try
      (is (= true (deref entered 15000 :timed-out)))
      (let [second-call (future (wrapped 2))]
        (is (= :pending (deref second-call 50 :pending)))
        (deliver release true)
        (is (= 1 (deref first-call 15000 :timed-out)))
        (is (= 2 (deref second-call 15000 :timed-out))))
      (finally (deliver release true)))
    (is (= 1 @peak))))

(deftest two-overlapping-real-biff-requests-get-fresh-guarded-writers
  (let [entered (promise) release (promise)
        inside (atom 0) peak (atom 0) invoked (atom 0) callbacks (atom [])
        ctx (ctx (fn [{:keys [registry]}]
                   (let [n (swap! inside inc)]
                     (swap! peak max n)
                     (try
                       (swap! callbacks conj (get-in @registry [wc/path :writer]))
                       (when (= 1 (swap! invoked inc))
                         (deliver entered true)
                         @release)
                       ((get-in @registry [wc/path :writer]))
                       (finally (swap! inside dec)))))
                 (policy 3 30000))
        final (start! ctx)
        report (mediation/require-current-biff-request-mediation!
                (get-in ctx [:queue-issued :handle]) (:source ctx) final)
        first-call (future (dispatch ctx final))]
    (is (= :bounded-fair-serialized-dispatch (:concurrent-arrival-policy report)))
    (is (= :not-permitted (:simultaneous-guarded-handler-execution report)))
    (is (= 3 (:max-pending-requests report)))
    (try
      (is (= true (deref entered 90000 :timed-out)))
      (let [second-call (future (dispatch ctx final))]
        (is (= :pending (deref second-call 70 :pending)))
        (deliver release true)
        (is (= 200 (:status (deref first-call 90000 :timed-out))))
        (is (= 200 (:status (deref second-call 90000 :timed-out)))))
      (finally (deliver release true)))
    (is (= 1 @peak))
    (is (= 2 (revision ctx)))
    (is (= 2 (count (distinct @callbacks))))
    (is (= {} @(:registry ctx)))
    (doseq [f @callbacks]
      (is (= :writer-outside-owning-request-scope (reason f))))))

(deftest queue-cleans-up-and-remains-usable-after-handler-throw
  (let [calls (atom 0)
        ctx (ctx (fn [{:keys [registry]}]
                   ((get-in @registry [wc/path :writer]))
                   (when (= 1 (swap! calls inc))
                     (throw (ex-info "secret" {:reason :after-physical-write}))))
                 (policy 3 10000))
        final (start! ctx)]
    (is (= :after-physical-write (reason #(dispatch ctx final))))
    (is (= {} @(:registry ctx)))
    (is (= 1 (revision ctx)))
    (is (= 200 (:status (dispatch ctx final))))
    (is (= 2 (revision ctx)))
    (is (= {} @(:registry ctx)))))

(defn- await-semaphore-wait! [^Thread t]
  (loop [n 0]
    (if (= java.lang.Thread$State/TIMED_WAITING (.getState t))
      true
      (if (>= n 400)
        false
        (do (Thread/sleep 5) (recur (inc n)))))))

(deftest pending-capacity-is-strict-and-recovers
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise) result (promise)
        wrapped (wrap (fn [x]
                        (when (= x :first)
                          (deliver entered true)
                          @release)
                        x)
                      {:capacity 2 :wait-ms 5000})
        first-call (future (wrapped :first))
        waiting (Thread. (fn [] (deliver result (wrapped :second))))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (.start waiting)
      (is (true? (await-semaphore-wait! waiting)))
      (is (= :bounded-mediation-capacity-exhausted
             (reason #(wrapped :third))))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (is (= :second (deref result 15000 :timeout)))
    (.join waiting 2000)
    (is (= :fourth (wrapped :fourth)))))

(deftest interrupting-waiter-releases-capacity-and-preserves-interrupt-status
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise) result (promise)
        wrapped (wrap (fn [x]
                        (when (= x :first)
                          (deliver entered true)
                          @release)
                        x)
                      {:capacity 2 :wait-ms 5000})
        first-call (future (wrapped :first))
        waiting (Thread. (fn []
                           (let [r (reason #(wrapped :second))]
                             (deliver result [r (.isInterrupted (Thread/currentThread))]))))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (.start waiting)
      (is (true? (await-semaphore-wait! waiting)))
      (.interrupt waiting)
      (is (= [:bounded-mediation-interrupted true]
             (deref result 15000 :timeout)))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (.join waiting 2000)
    (is (= :third (wrapped :third)))))

;; Additional permanent failure-boundary and integration regressions.
;; Timed concurrency tests synchronize on promises / JVM thread state rather
;; than relying on timing alone. All blocking calls have bounded joins.

(defn- queued-report [c final]
  (mediation/require-current-biff-request-mediation!
   (get-in c [:queue-issued :handle]) (:source c) final))

(deftest bounds-accept-endpoints-without-integer-narrowing
  (let [validate (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                             'bounded-concurrency-policy!)]
    (is (= {:capacity 2 :wait-ms 1}
           (validate (policy 2 1))))
    (is (= {:capacity 64 :wait-ms 60000}
           (validate (policy 64 60000))))
    (doseq [bad [false 0 -1 1 65 100000 2.1 "2" nil]]
      (is (= :invalid-bounded-mediation-policy
             (reason #(validate (policy bad 100))))))
    (doseq [bad [false 0 -1 60001 1.1 "10" nil]]
      (is (= :invalid-bounded-mediation-policy
             (reason #(validate (policy 2 bad))))))))

(deftest bounded-policy-rejects-caller-authority-and-extra-fields
  (let [validate (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                             'bounded-concurrency-policy!)]
    (doseq [extra [{:max-pending-requests 2 :wait-timeout-ms 10 :executor identity}
                   {:max-pending-requests 2 :wait-timeout-ms 10 :callback identity}
                   {:max-pending-requests 2 :wait-timeout-ms 10 :node (Object.)}
                   {:max-pending-requests 2 :wait-timeout-ms 10 :ttl nil}]]
      (is (= :invalid-bounded-mediation-policy
             (reason #(validate extra)))))))

(deftest fair-semaphore-preserves-known-waiter-order
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise)
        order (atom [])
        dispatcher (wrap (fn [x]
                           (swap! order conj x)
                           (when (= x :first)
                             (deliver entered true)
                             @release)
                           x)
                         {:capacity 3 :wait-ms 10000})
        first-call (future (dispatcher :first))
        second-result (promise)
        third-result (promise)
        second (Thread. (fn [] (deliver second-result (dispatcher :second))))
        third (Thread. (fn [] (deliver third-result (dispatcher :third))))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (.start second)
      (is (true? (await-semaphore-wait! second)))
      (.start third)
      (is (true? (await-semaphore-wait! third)))
      (is (= [:first] @order))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (is (= :second (deref second-result 15000 :timeout)))
    (is (= :third (deref third-result 15000 :timeout)))
    (.join second 2000)
    (.join third 2000)
    (is (= [:first :second :third] @order))))

(deftest recursive-entry-does-not-consume-remaining-queue-capacity
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        own (atom nil) nested (atom nil)
        dispatcher (wrap (fn [x]
                           (reset! nested (reason #(@own :nested)))
                           x)
                         {:capacity 2 :wait-ms 100})]
    (reset! own dispatcher)
    (is (= :first (dispatcher :first)))
    (is (= :recursive-biff-mediation-dispatch @nested))
    (is (= :second (dispatcher :second)))
    (is (= :recursive-biff-mediation-dispatch @nested))))

(deftest handler-throw-releases-permit-and-admission
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        dispatcher (wrap (fn [x]
                           (if (= x :fail)
                             (throw (ex-info "private-handler-data" {:reason :handler-failed}))
                             x))
                         {:capacity 2 :wait-ms 100})]
    (is (= :handler-failed (reason #(dispatcher :fail))))
    (is (= :second (dispatcher :second)))
    (is (= :third (dispatcher :third)))))

(deftest timed-out-waiter-never-runs-submission-thunk
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise) calls (atom [])
        dispatcher (wrap (fn [x]
                           (swap! calls conj x)
                           (when (= x :first)
                             (deliver entered true)
                             @release)
                           x)
                         {:capacity 2 :wait-ms 60})
        first-call (future (dispatcher :first))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (is (= :bounded-mediation-wait-timeout
             (reason #(dispatcher :expired))))
      (is (= [:first] @calls))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (is (= :new (dispatcher :new)))
    (is (= [:first :new] @calls))))

(deftest capacity-rejection-never-enters-handler
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise) result (promise)
        calls (atom [])
        dispatcher (wrap (fn [x]
                           (swap! calls conj x)
                           (when (= x :first)
                             (deliver entered true)
                             @release)
                           x)
                         {:capacity 2 :wait-ms 5000})
        first-call (future (dispatcher :first))
        waiting (Thread. (fn [] (deliver result (dispatcher :second))))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (.start waiting)
      (is (true? (await-semaphore-wait! waiting)))
      (is (= :bounded-mediation-capacity-exhausted
             (reason #(dispatcher :overflow))))
      (is (= [:first] @calls))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (is (= :second (deref result 15000 :timeout)))
    (.join waiting 2000)
    (is (= [:first :second] @calls))))

(deftest repeated-timeouts-do-not-poison-the-queue
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise)
        dispatcher (wrap (fn [x]
                           (when (= x :first)
                             (deliver entered true)
                             @release)
                           x)
                         {:capacity 2 :wait-ms 30})
        first-call (future (dispatcher :first))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (dotimes [_ 3]
        (is (= :bounded-mediation-wait-timeout
               (reason #(dispatcher :late)))))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (is (= :new (dispatcher :new)))))

(deftest already-interrupted-waiter-preserves-flag
  (let [wrap (private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-mediation
                         'bounded-serial-dispatch)
        entered (promise) release (promise) outcome (promise)
        dispatcher (wrap (fn [x]
                           (when (= x :first)
                             (deliver entered true)
                             @release)
                           x)
                         {:capacity 2 :wait-ms 5000})
        first-call (future (dispatcher :first))
        waiting (Thread. (fn []
                           (.interrupt (Thread/currentThread))
                           (deliver outcome [(reason #(dispatcher :second))
                                             (.isInterrupted (Thread/currentThread))])))]
    (try
      (is (= true (deref entered 15000 :timeout)))
      (.start waiting)
      (is (= [:bounded-mediation-interrupted true]
             (deref outcome 15000 :timeout)))
      (finally (deliver release true)))
    (is (= :first (deref first-call 15000 :timeout)))
    (.join waiting 2000)
    (is (= :third (dispatcher :third)))))

(deftest queued-issuer-rejects-invalid-construction-policy-before-startup
  (let [c (ctx (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer])))
               (policy 2 100))]
    (is (= :invalid-bounded-mediation-policy
           (reason #(mediation/issue-bounded-queued-biff-request-mediation-module!
                     (:adoption c) (:source c) (:inventory c) (:registry c)
                     {} {wc/op wc/path} wc/*node* (fn [_] nil) :revision
                     {:max-pending-requests 1 :wait-timeout-ms 100}))))))

(deftest checked-biff-report-says-serialized-and-not-observed
  (let [c (ctx (fn [_] nil) (policy 3 5000))
        final (start! c)
        report (queued-report c final)]
    (is (= mediation/report-type (:type report)))
    (is (= :bounded-fair-serialized-dispatch (:concurrent-arrival-policy report)))
    (is (= :not-permitted (:simultaneous-guarded-handler-execution report)))
    (is (= 3 (:max-pending-requests report)))
    (is (= :not-observed-by-this-inspection (:request-invocation report)))
    (is (= :not-established (:deployed-server-use report)))
    (is (= :not-established (:complete-writer-mediation report)))
    (is (= :not-established (:physical-refinement report)))
    (is (contains? (:open-obligations report) :parallel-request-writer-execution-not-established))
    (is (contains? (:open-obligations report) :bounded-queue-timeout-and-admission-overflow))
    (is (not (contains? (:open-obligations report)
                        :concurrent-requests-through-this-controller-are-rejected)))
    (is (= {} @(:registry c)))
    (is (zero? @(:calls c)))))

(deftest queued-inspection-rejects-copied-final-system-and-foreign-handle
  (let [c (ctx (fn [_] nil) (policy 2 5000))
        final (start! c)]
    (is (= :final-biff-handler-system-identity-mismatch
           (reason #(mediation/require-current-biff-request-mediation!
                     (get-in c [:queue-issued :handle]) (:source c)
                     (into {} final)))))
    (is (= :unissued-mediation-module
           (reason #(mediation/require-current-biff-request-mediation!
                     (Object.) (:source c) final))))
    (is (= :changed-mediation-contract-source
           (reason #(mediation/require-current-biff-request-mediation!
                     (get-in c [:queue-issued :handle]) (into {} (:source c)) final))))))

(deftest queued-biff-module-init-and-start-stay-single-use
  (let [c (ctx (fn [_] nil) (policy 2 3000))
        module (get-in c [:queue-issued :module])]
    (is (= :mediation-module-not-initialized
           (reason #((:biff.core/start module) (:system c)))))
    (is (= :repeated-mediation-module-start
           (reason #((:biff.core/start module) (:system c)))))
    (is (= {} ((:biff.core/init module) nil)))
    (is (= :repeated-mediation-module-init
           (reason #((:biff.core/init module) nil))))))

(deftest sequential-queued-requests-retire-exact-distinct-callbacks
  (let [callbacks (atom [])
        c (ctx (fn [{:keys [registry]}]
                 (let [cb (get-in @registry [wc/path :writer])]
                   (swap! callbacks conj cb)
                   (cb)))
               (policy 2 3000))
        final (start! c)]
    (dotimes [n 3]
      (is (= 200 (:status (dispatch c final))))
      (is (= (inc n) (revision c)))
      (is (= {} @(:registry c))))
    (is (= 3 (count (distinct @callbacks))))
    (doseq [cb @callbacks]
      (is (= :writer-outside-owning-request-scope (reason cb))))))

(deftest queued-failure-after-physical-write-does-not-retry-or-rollback
  (let [calls (atom 0)
        c (ctx (fn [{:keys [registry]}]
                 ((get-in @registry [wc/path :writer]))
                 (when (= 1 (swap! calls inc))
                   (throw (ex-info "private-after-write" {:reason :after-write}))))
               (policy 2 3000))
        final (start! c)]
    (is (= :after-write (reason #(dispatch c final))))
    (is (= 1 (revision c)))
    (is (= {} @(:registry c)))
    (is (= 200 (:status (dispatch c final))))
    (is (= 2 (revision c)))
    (is (= {} @(:registry c)))))

(deftest queued-handler-without-callback-fails-without-writing
  (let [c (ctx (fn [_] nil) (policy 2 1000))
        final (start! c)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #(dispatch c final))))
    (is (= 0 (revision c)))
    (is (= {} @(:registry c)))))

(deftest queued-handler-direct-xtdb-write-is-still-out-of-scope
  (let [c (ctx (fn [{:keys [registry]}]
                 ((get-in @registry [wc/path :writer]))
                 ;; Registry callback visibility does not rule out other XTDB
                 ;; writes. This is a separate real physical writer.
                 (xtdb.api/execute-tx wc/*node*
                                      [[:put-docs wc/table
                                        {:xt/id (str (random-uuid))
                                         :revision 0 :created-at "raw"
                                         :updated-at "raw" :owner "raw"}]]))
               (policy 2 3000))
        final (start! c)]
    (is (= 200 (:status (dispatch c final))))
    (is (= 1 (revision c)))
    (is (= :not-established (:complete-writer-mediation (queued-report c final))))
    (is (= {} @(:registry c)))))

(deftest private-values-and-host-capabilities-are-not-exposed-by-queue-report
  (let [c (ctx (fn [_] nil) (policy 2 1000))
        final (start! c)
        report (queued-report c final)
        printed (pr-str report)]
    (is (not (str/includes? printed "private-v1549-sentinel")))
    (is (not (str/includes? printed (str (:xt/id (:before c))))))
    (is (not-any? #(contains? report %)
                  [:callback :registry :handler :document :source :node
                   :request :response :tx-key :writer :handle :system]))
    (is (= :none (:authority report)))
    (is (= :none (:transaction-authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))))
