(ns gesso.assurance.proof.xtdb-gateway-biff-bounded-parallel-test
  "v1572: permanent adversarial tests for bounded genuinely parallel Biff
   dispatch. Exercise finite active/pending admission, fair-permit boundaries,
   timeout, interruption, recursion, and independent one-shot writer scopes.
   Real certified multi-operation routes and embedded XTDB accompany local
   deterministic concurrency tests. No process-local observation proves
   deployed-server adoption, complete writer mediation, or refinement."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [xtdb.api :as xt]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test :as multi]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as host]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as app])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

;; The permanent distinct-operation fixture includes authentic application
;; induction, independently checked writer inventories, Biff startup, and XTDB.
;; Tests never replace XTDB, the checked handler, or the real gateway.
(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                          (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test))))]
      (fixture run))))

(defn- private-fn [ns-name sym]
  (let [v (ns-resolve ns-name sym)]
    (when-not v (throw (ex-info "Missing source/test fixture" {:symbol sym})))
    (var-get v)))
(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e (or (:reason (ex-data e)) :exception-info))
       (catch Throwable e [:unexpected (.getName (class e))])))
(defn- policy [active pending timeout]
  {:max-active-requests active :max-pending-requests pending :wait-timeout-ms timeout})
(defn- validate! [p]
  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
                'bounded-parallel-policy!) p))
(defn- wrap [f p]
  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
                'bounded-parallel-dispatch) f p))
(defn- latch! [^CountDownLatch l] (.countDown l))
(defn- await! [^CountDownLatch l ms]
  (.await l (long ms) TimeUnit/MILLISECONDS))
(defn- await-result [future-result]
  (deref future-result 30000 :timed-out))
(defn- single-case! [behavior p]
  (let [context ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
                             'context)
                 (fn [{:keys [request]}] (behavior request)))
        context (assoc context :second-before (wc/seeded))
        source! (fn [request]
                  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                               'gateway-source)
                   (assoc context :before (if (= :second (:request-id request))
                                            (:second-before context) (:before context)))))
        issued (parallel/issue-bounded-isolated-parallel-biff-request-module!
                (:adoption context) (:source context) wc/*source* {wc/op wc/path}
                wc/*node* source! :revision p)
        module (:module issued)]
    ((:biff.core/init module) nil)
    (assoc context :issued issued :final ((:biff.core/start module) (:system context)))))
(defn- current-handler [c] (get (:final c) app/biff-ring-handler-key))
(defn- revision [before] (:revision (wc/physical before)))
(defn- multi-case! [behavior p]
  (let [c ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
                       'build-case))
        original @(:behavior multi/*case*)
        _ (reset! (:behavior multi/*case*) behavior)
        sources {:per-operation-writer-sources
                 {multi/claim wc/*source*
                  multi/cancel (:writer-source multi/*second*)}}
        source! (fn [request]
                  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
                               'source-for-request) c request))
        issued (parallel/issue-bounded-isolated-parallel-biff-request-module!
                (:adoption c) (:source c) sources multi/writer-bindings
                wc/*node* source! :revision p)
        module (:module issued)]
    ((:biff.core/init module) nil)
    (assoc c :original-behavior original :issued issued
           :final ((:biff.core/start module) (:system c)))))
(defn- restore-behavior! [c]
  (when-let [b (:original-behavior c)]
    (reset! (:behavior multi/*case*) b)))
(defn- multi-request [which]
  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
               'req) which))

(deftest complete-minimal-policy-is-valid
  (is (= (policy 2 2 1) (validate! (policy 2 2 1)))))
(deftest complete-maximum-policy-is-valid
  (is (= (policy 64 256 60000) (validate! (policy 64 256 60000)))))
(deftest pending-capacity-may-equal-active-capacity
  (is (= (policy 4 4 100) (validate! (policy 4 4 100)))))
(deftest malformed-policy-shapes-reject
  (doseq [p [nil false [] () #{} {:max-active-requests 2}
             {:max-active-requests 2 :max-pending-requests 3}
             (assoc (policy 2 3 50) :extra true)
             (with-meta (policy 2 3 50) {:forged true})]]
    (is (= :invalid-bounded-parallel-policy (reason #(validate! p))))))
(deftest active-limit-must-be-between-two-and-sixty-four
  (doseq [n [-100 -1 0 1 65 1000000]]
    (is (= :invalid-bounded-parallel-policy
           (reason #(validate! (policy n (max n 2) 100)))))))
(deftest pending-limit-must-cover-active-and-remain-bounded
  (doseq [[active pending] [[2 0] [2 1] [3 2] [64 63]
                            [2 257] [2 1000000]]]
    (is (= :invalid-bounded-parallel-policy
           (reason #(validate! (policy active pending 100)))))))
(deftest timeout-must-be-positive-and-finite
  (doseq [timeout [-1 0 60001 1000000]]
    (is (= :invalid-bounded-parallel-policy
           (reason #(validate! (policy 2 3 timeout)))))))
(deftest policy-values-must-be-integers-without-coercion
  (doseq [p [(policy 2.0 3 100) (policy 2 3.0 100)
             (policy 2 3 100.0) (policy "2" 3 100)
             (policy 2 "3" 100) (policy 2 3 "100")
             (policy 2 3 nil) (policy true 3 100)]]
    (is (= :invalid-bounded-parallel-policy
           (reason #(validate! p))))))
(deftest request-execution-does-not-serialize-all-handlers
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        active (atom 0) peak (atom 0)
        f (wrap (fn [v]
                  (let [n (swap! active inc)]
                    (swap! peak max n)
                    (try (latch! entered)
                         (is (await! release 18000))
                         v
                         (finally (swap! active dec)))))
                (policy 2 2 1000))
        a (future (f :a)) b (future (f :b))]
    (try (is (await! entered 15000))
         (is (= 2 @peak))
         (is (= :bounded-parallel-admission-full (reason #(f :excess))))
         (finally (latch! release)))
    (is (= :a (await-result a)))
    (is (= :b (await-result b)))
    (is (zero? @active))
    (is (= :later (f :later)))))
(deftest waiting-slot-times-out-without-entering-handler
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        call-count (atom 0)
        f (wrap (fn [v]
                  (swap! call-count inc)
                  (when (#{:a :b} v)
                    (latch! entered)
                    (await! release 18000))
                  v)
                (policy 2 3 70))
        a (future (f :a)) b (future (f :b))]
    (try (is (await! entered 15000))
         (is (= :bounded-parallel-admission-timeout
                (reason #(f :wait))))
         (is (= 2 @call-count))
         (finally (latch! release)))
    (is (= :a (await-result a)))
    (is (= :b (await-result b)))
    (is (= :next (f :next)))
    (is (= 3 @call-count))))
(deftest maximum-pending-is-running-plus-waiting-not-just-waiting
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        f (wrap (fn [v]
                  (when (#{:a :b} v)
                    (latch! entered)
                    (await! release 18000))
                  v) (policy 2 2 1000))
        a (future (f :a)) b (future (f :b))]
    (try (is (await! entered 15000))
         (is (= :bounded-parallel-admission-full (reason #(f :third))))
         (finally (latch! release)))
    (is (= :a (await-result a)))
    (is (= :b (await-result b)))))
(deftest recursive-entry-rejects-before-acquiring-another-permit
  (let [self (atom nil) seen (atom nil)
        f (wrap (fn [v]
                  (when (= v :outer)
                    (reset! seen (reason #(@self :nested))))
                  v)
                (policy 2 2 100))]
    (reset! self f)
    (is (= :outer (f :outer)))
    (is (= :recursive-bounded-parallel-request-dispatch @seen))
    (is (= :fresh (f :fresh)))))
(deftest handler-exception-propagates-without-stranding-permits
  (let [f (wrap (fn [v]
                  (if (= :fail v) (throw (ex-info "handler" {:scenario :handler})) v))
                (policy 2 2 100))]
    (is (= :handler
           (try (f :fail) :not-thrown
                (catch clojure.lang.ExceptionInfo e (:scenario (ex-data e))))))
    (is (= :after (f :after)))))
(deftest interrupted-waiter-releases-its-pending-slot
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        attempting (CountDownLatch. 1) outcome (promise)
        f (wrap (fn [v]
                  (when (#{:a :b} v)
                    (latch! entered) (await! release 18000))
                  v) (policy 2 3 10000))
        a (future (f :a)) b (future (f :b))]
    (try
      (is (await! entered 15000))
      (let [^Thread thread (Thread. (fn []
                                      (latch! attempting)
                                      (let [result (reason #(f :waiting))]
                                        (deliver outcome [result (.isInterrupted (Thread/currentThread))]))))]
        (.start thread)
        (is (await! attempting 5000))
        (Thread/sleep 80)
        (.interrupt thread)
        (is (= [:bounded-parallel-admission-interrupted true]
               (deref outcome 5000 :timed-out)))
        (.join thread 5000))
      (finally (latch! release)))
    (is (= :a (await-result a)))
    (is (= :b (await-result b)))
    (is (= :after (f :after)))))
(deftest failed-nested-entry-does-not-block-an-unrelated-thread
  (let [self (atom nil) observed (atom [])
        f (wrap (fn [v]
                  (when (= v :outer)
                    (swap! observed conj (reason #(@self :inside))))
                  v) (policy 2 2 1000))]
    (reset! self f)
    (is (= :outer (f :outer)))
    (is (= :parallel (await-result (future (f :parallel)))))
    (is (= [:recursive-bounded-parallel-request-dispatch] @observed))))
(deftest bounded-handler-result-is-not-rewritten
  (let [token (Object.)
        f (wrap (fn [_] token) (policy 2 2 100))]
    (is (identical? token (f :request)))))
(deftest failed-wait-does-not-issue-a-second-handler-invocation
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        invoked (atom [])
        f (wrap (fn [v]
                  (swap! invoked conj v)
                  (when (#{:a :b} v)
                    (latch! entered) (await! release 18000))
                  v) (policy 2 3 70))
        a (future (f :a)) b (future (f :b))]
    (try (is (await! entered 15000))
         (is (= :bounded-parallel-admission-timeout (reason #(f :timed-out))))
         (finally (latch! release)))
    (is (= :a (await-result a)))
    (is (= :b (await-result b)))
    (is (= #{:a :b} (set @invoked)))))
(deftest the-admission-gate-is-per-issuer-not-global
  (let [entered (CountDownLatch. 4) release (CountDownLatch. 1)
        f1 (wrap (fn [v] (latch! entered) (await! release 18000) v)
                 (policy 2 2 100))
        f2 (wrap (fn [v] (latch! entered) (await! release 18000) v)
                 (policy 2 2 100))
        fs [(future (f1 :a)) (future (f1 :b))
            (future (f2 :c)) (future (f2 :d))]]
    (try (is (await! entered 15000))
         (is (= :bounded-parallel-admission-full (reason #(f1 :e))))
         (is (= :bounded-parallel-admission-full (reason #(f2 :f))))
         (finally (latch! release)))
    (is (= #{:a :b :c :d} (set (map await-result fs))))))

(deftest real-two-request-biff-execution-overlaps-with-independent-documents
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        running (atom 0) peak (atom 0)
        c (single-case! (fn [_]
                          (let [n (swap! running inc)]
                            (swap! peak max n)
                            (try (latch! entered)
                                 (when-not (await! release 24000)
                                   (throw (ex-info "test deadline" {:reason :test-deadline})))
                                 (parallel/invoke-current-request-writer! wc/path)
                                 (finally (swap! running dec)))))
                        (policy 2 2 2000))
        handler (current-handler c) request (:request c)
        a (future (handler request))
        b (future (handler (assoc request :request-id :second)))]
    (try (is (await! entered 90000))
         (is (= 2 @peak))
         (is (= :bounded-parallel-admission-full
                (reason #(handler (assoc request :request-id :excess)))))
         (finally (latch! release)))
    (is (= 200 (:status (deref a 90000 :timed-out))))
    (is (= 200 (:status (deref b 90000 :timed-out))))
    (is (= 1 (revision (:before c))))
    (is (= 1 (revision (:second-before c))))))
(deftest rejected-before-admission-does-not-create-a-writer-or-call-source-builder
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        c (single-case! (fn [_]
                          (latch! entered)
                          (await! release 24000)
                          (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 2000))
        handler (current-handler c) request (:request c)
        a (future (handler request))
        b (future (handler (assoc request :request-id :second)))]
    (try (is (await! entered 90000))
         (is (= :bounded-parallel-admission-full
                (reason #(handler (assoc request :request-id :excess)))))
         ;; The rejected request targets neither seeded writer; only the
         ;; accepted pair can change physical revisions.
         (is (= 0 (revision (:before c))))
         (is (= 0 (revision (:second-before c))))
         (finally (latch! release)))
    (is (= 200 (:status (deref a 90000 :timed-out))))
    (is (= 200 (:status (deref b 90000 :timed-out))))))
(deftest genuine-biff-report-limits-claims-to-process-local-installation
  (let [c (single-case! (fn [_] (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 2000))
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (get-in c [:issued :handle]) (:source c) (:final c))
        printed (pr-str report)]
    (is (= :bounded-fair-permits-with-request-local-registries
           (:parallel-admission-policy report)))
    (is (= 2 (:max-active-requests report)))
    (is (= 2 (:max-pending-requests report)))
    (is (= 2000 (:wait-timeout-ms report)))
    (is (= :independent-per-request (:registered-writer-context report)))
    (is (= :not-established (:actual-http-server-adoption report)))
    (is (= :not-established (:other-writer-closure report)))
    (is (= :not-established (:operation-refinement report)))
    (is (= :not-issued (:certificate report)))
    (is (= :none (:transaction-authority report)))
    (is (not (str/includes? printed "private-v1549-sentinel")))
    (is (not-any? #(contains? report %)
                  [:node :tx-ops :tx-key :writer :handler :source :request]))))
(deftest forged-parallel-handle-cannot-inspect-installed-handler
  (let [c (single-case! (fn [_] nil) (policy 2 2 1000))]
    (is (= :unissued-parallel-module
           (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                     (Object.) (:source c) (:final c)))))))
(deftest copied-final-system-is-not-authentic-installation
  (let [c (single-case! (fn [_] nil) (policy 2 2 1000))]
    (is (= :parallel-final-handler-identity-mismatch
           (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                     (get-in c [:issued :handle]) (:source c) (into {} (:final c))))))))
(deftest original-unbounded-mediator-remains-separate
  (let [c (single-case! (fn [_] (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 1000))]
    (is (= 200 (:status ((current-handler c) (:request c)))))))
(deftest raw-xtdb-client-can-bypass-the-bounded-mediator
  (let [c (single-case! (fn [_] (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 1000))
        before (wc/seeded)]
    (is (= 200 (:status ((current-handler c) (:request c)))))
    (xt/execute-tx wc/*node* [[:put-docs wc/table (assoc before :direct-unmediated true)]])
    (is (true? (:direct-unmediated (wc/physical before))))
    (is (= 1 (revision (:before c))))))
(deftest observed-writer-accessor-rejects-off-thread-use
  (let [seen (atom nil)
        c (single-case! (fn [_]
                          (reset! seen (await-result
                                        (future (reason #(parallel/invoke-current-request-writer! wc/path))))))
                        (policy 2 2 1000))]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #((current-handler c) (:request c)))))
    (is (= :no-matching-active-thread-bound-request-writer @seen))))
(deftest request-handler-exception-does-not-poison-later-admission
  (let [calls (atom 0)
        c (single-case! (fn [_]
                          (if (= 1 (swap! calls inc))
                            (throw (ex-info "before write" {:reason :test-before-write}))
                            (parallel/invoke-current-request-writer! wc/path)))
                        (policy 2 2 1000))]
    (is (= :test-before-write
           (reason #((current-handler c) (:request c)))))
    (is (= 200 (:status ((current-handler c) (:request c)))))
    (is (= 1 (revision (:before c))))))
(deftest unrelated-physical-writes-are-not-proved-absent-by-a-clean-report
  (let [c (single-case! (fn [_] (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 1000))]
    (is (= 200 (:status ((current-handler c) (:request c)))))
    (let [report (parallel/require-current-isolated-parallel-biff-request-module!
                  (get-in c [:issued :handle]) (:source c) (:final c))]
      (is (contains? (:open-obligations report)
                     :retained-biff-handlers-and-direct-xtdb-write-bypasses))
      (is (contains? (:open-obligations report)
                     :abstract-operation-to-command-correspondence-not-established)))))
(deftest selected-route-mismatch-does-not-acquire-request-capability
  (let [c (single-case! (fn [_] (parallel/invoke-current-request-writer! wc/path))
                        (policy 2 2 1000))]
    (is (= :unmatched-parallel-request-route
           (reason #((current-handler c)
                     (assoc (:request c) :uri "/wrong-route")))))
    (is (= 0 (revision (:before c))))))

;; Distinct-operation coverage: the two independent operation sources below
;; originate from genuinely checked induction and writer-coverage machinery.
(deftest bounded-multi-route-operation-sources-remain-independent
  (let [c (multi-case! (fn [request]
                         (parallel/invoke-current-request-writer!
                          (if (= "/operations/example/claim" (:uri request))
                            multi/claim-path multi/cancel-path)))
                       (policy 2 3 2000))]
    (try
      (is (= 2 (:certified-literal-route-count
                (parallel/require-current-isolated-parallel-biff-request-module!
                 (get-in c [:issued :handle]) (:source c) (:final c)))))
      (is (= 200 (:status ((current-handler c) (multi-request :claim)))))
      (is (= 200 (:status ((current-handler c) (multi-request :cancel)))))
      (is (= 1 (revision (:claim-before c))))
      (is (= 1 (revision (:cancel-before c))))
      (finally (restore-behavior! c)))))
(deftest two-different-certified-operations-run-simultaneously-under-two-permits
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        running (atom 0) peak (atom 0)
        c (multi-case! (fn [request]
                         (let [n (swap! running inc)]
                           (swap! peak max n)
                           (try
                             (latch! entered)
                             (when-not (await! release 24000)
                               (throw (ex-info "test deadline" {:reason :test-deadline})))
                             (parallel/invoke-current-request-writer!
                              (if (= "/operations/example/claim" (:uri request))
                                multi/claim-path multi/cancel-path))
                             (finally (swap! running dec)))))
                       (policy 2 2 2000))
        h (current-handler c)
        a (future (h (multi-request :claim)))
        b (future (h (multi-request :cancel)))]
    (try
      (is (await! entered 90000))
      (is (= 2 @peak))
      (is (= :bounded-parallel-admission-full
             (reason #(h (multi-request :claim)))))
      (finally (latch! release)
               (restore-behavior! c)))
    (is (= 200 (:status (deref a 90000 :timed-out))))
    (is (= 200 (:status (deref b 90000 :timed-out))))
    (is (= 1 (revision (:claim-before c))))
    (is (= 1 (revision (:cancel-before c))))))
(deftest bounded-multiroute-still-rejects-missing-operation-coverage
  (let [c ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
                       'build-case))
        issued (parallel/issue-bounded-isolated-parallel-biff-request-module!
                (:adoption c) (:source c)
                {:per-operation-writer-sources {multi/claim wc/*source*}}
                multi/writer-bindings wc/*node*
                (fn [request]
                  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
                               'source-for-request) c request))
                :revision (policy 2 2 1000))
        module (:module issued)]
    ((:biff.core/init module) nil)
    (is (= :requires-independent-operation-writer-coverage-sources
           (reason #((:biff.core/start module) (:system c)))))))
(deftest bounded-multiroute-selects-only-the-requested-operation
  (let [c (multi-case! (fn [request]
                         (parallel/invoke-current-request-writer!
                          (if (= "/operations/example/claim" (:uri request))
                            multi/claim-path multi/cancel-path)))
                       (policy 2 3 2000))]
    (try
      (is (= 200 (:status ((current-handler c) (multi-request :cancel)))))
      (is (= 0 (revision (:claim-before c))))
      (is (= 1 (revision (:cancel-before c))))
      (finally (restore-behavior! c)))))
(deftest operation-identity-still-does-not-prove-command-target-identity
  (let [c ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
                       'build-case))
        issued (parallel/issue-bounded-isolated-parallel-biff-request-module!
                (:adoption c) (:source c)
                {:per-operation-writer-sources
                 {multi/claim wc/*source*
                  multi/cancel (:writer-source multi/*second*)}}
                multi/writer-bindings wc/*node*
                (fn [_] (wc/source (:cancel-before c)))
                :revision (policy 2 2 1000))
        module (:module issued)]
    ((:biff.core/init module) nil)
    (let [final ((:biff.core/start module) (:system c))]
      (try
        (is (= 200 (:status ((get final app/biff-ring-handler-key)
                            (multi-request :claim)))))
        (is (= 0 (revision (:claim-before c))))
        (is (= 1 (revision (:cancel-before c))))
        (finally (restore-behavior! c))))))
