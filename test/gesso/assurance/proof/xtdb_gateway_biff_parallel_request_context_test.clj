(ns gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context-test
  "v1568: permanent real-Biff and embedded-XTDB tests for independent
   per-request guarded writer registration and simultaneous dispatch.
   Positive reports are process-local and do not attest deployment-wide
   handler adoption, exhaustive physical writers or causal refinement."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [xtdb.api :as xt]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as bh]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as app])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                          (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (fixture run))))

(defn- private-fn [namespace sym] (var-get (ns-resolve namespace sym)))
(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- source! [ctx request]
  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test 'gateway-source)
   (assoc ctx :before (if (= (:request-id request) :second)
                        (:second-before ctx)
                        (:before ctx)))))
(defn- ctx [behavior]
  (let [c ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test 'context)
           (fn [{:keys [request]}] (behavior request)))
        c (assoc c :second-before (wc/seeded))
        m (parallel/issue-isolated-parallel-biff-request-module!
           (:adoption c) (:source c) wc/*source* {wc/op wc/path}
           wc/*node* (partial source! c) :revision)]
    (assoc c :parallel m)))
(defn- start! [c]
  (let [m (get-in c [:parallel :module])]
    ((:biff.core/init m) nil)
    ((:biff.core/start m) (:system c))))
(defn- dispatch! [c system request]
  ((get system app/biff-ring-handler-key) request))

(deftest current-writer-accessor-rejects-outside-dispatch
  (is (= :no-matching-active-thread-bound-request-writer
         (reason #(parallel/invoke-current-request-writer! wc/path)))))

(deftest single-request-with-real-biff-handler-and-xtdb
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)
        r (parallel/require-current-isolated-parallel-biff-request-module!
           (get-in c [:parallel :handle]) (:source c) final)]
    (is (= parallel/report-type (:type r)))
    (is (= :independent-per-request (:registered-writer-context r)))
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= 2 (:revision (wc/physical (:before c)))))
    (is (= :not-established (:actual-http-server-adoption r)))))

(deftest missing-writer-rejected-even-when-handler-returns
  (let [c (ctx (fn [_] nil)) final (start! c)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #(dispatch! c final (:request c)))))
    (is (= 0 (:revision (wc/physical (:before c)))))))

(deftest off-thread-writer-lookup-rejected
  (let [result (atom nil)
        c (ctx (fn [_]
                 (reset! result
                         @(future (reason #(parallel/invoke-current-request-writer! wc/path))))))
        final (start! c)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #(dispatch! c final (:request c)))))
    (is (= :no-matching-active-thread-bound-request-writer @result))))

(deftest exact-final-system-and-handle-required
  (let [c (ctx (fn [_] nil)) final (start! c)]
    (is (= :unissued-parallel-module
           (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                     (Object.) (:source c) final))))
    (is (= :parallel-final-handler-identity-mismatch
           (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                     (get-in c [:parallel :handle]) (:source c) (into {} final)))))))

(deftest genuinely-simultaneous-requests-have-isolated-registrations
  (let [entered (CountDownLatch. 2)
        release (CountDownLatch. 1)
        running (atom 0) peak (atom 0)
        c (ctx (fn [_]
                 (let [n (swap! running inc)]
                   (swap! peak max n)
                   (try
                     (.countDown entered)
                     (when-not (.await release 30 TimeUnit/SECONDS)
                       (throw (ex-info "parallel fixture timeout" {:failure :timeout})))
                     (parallel/invoke-current-request-writer! wc/path)
                     (finally (swap! running dec))))))
        final (start! c)
        first-request (:request c)
        second-request (assoc first-request :request-id :second)
        a (future (dispatch! c final first-request))
        b (future (dispatch! c final second-request))]
    (try
      (is (true? (.await entered 90 TimeUnit/SECONDS)) "Both real Biff handlers overlap")
      (finally (.countDown release)))
    (is (= 200 (:status (deref a 100000 {:status :timed-out}))))
    (is (= 200 (:status (deref b 100000 {:status :timed-out}))))
    (is (= 2 @peak) "No global dispatcher mutex")
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= 1 (:revision (wc/physical (:second-before c)))))))

(deftest two-competing-requests-for-same-prestate-do-not-bypass-guards
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        c (ctx (fn [_]
                 (.countDown entered)
                 (.await release 25 TimeUnit/SECONDS)
                 (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)
        request (:request c)
        attempt! (fn [] (try
                          (dispatch! c final request)
                          :success
                          (catch Throwable t
                            (if (= "xtdb.error.Conflict" (.getName (class t)))
                              :guard-conflict
                              :unexpected-exception))))
        a (future (attempt!))
        b (future (attempt!))]
    (try
      (is (true? (.await entered 80 TimeUnit/SECONDS)))
      (finally (.countDown release)))
    (let [outcomes [(deref a 100000 :timeout) (deref b 100000 :timeout)]]
      (is (= #{:success :guard-conflict} (set outcomes)))
      (is (= 1 (:revision (wc/physical (:before c))))))))

;; These cases intentionally depend on a real certified Biff handler and a
;; local embedded XTDB node. They do not replace the original standalone smoke
;; fixture, which remains useful as a short gate.

(defn- report! [c final]
  (parallel/require-current-isolated-parallel-biff-request-module!
   (get-in c [:parallel :handle]) (:source c) final))

(defn- revision-of [b]
  (:revision (wc/physical b)))

(defn- reject-instead-of-throwing? [f]
  (not= :not-rejected (reason f)))

(deftest installation-report-has-a-narrow-scope-and-no-authority
  (let [c (ctx (fn [_] nil)) final (start! c) r (report! c final)]
    (is (= parallel/report-version (:version r)))
    (is (= :conditional-request-local-writer-mediation-installed (:status r)))
    (is (= :one-process-local-final-biff-system (:scope r)))
    (is (= :independent-per-request (:registered-writer-context r)))
    (is (= :permitted (:parallel-handler-dispatch r)))
    (is (= :fresh-per-request (:request-scoped-one-shot r)))
    (is (= :one-checked-invocation-per-request (:in-process-callback-trace r)))
    (doseq [k [:actual-http-server-adoption :other-writer-closure :operation-refinement]]
      (is (= :not-established (get r k)) (str "open claim: " k)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :none (:authority r) (:transaction-authority r)))
    (is (contains? (:open-obligations r)
                   :retained-biff-handlers-and-direct-xtdb-write-bypasses))
    (is (contains? (:open-obligations r)
                   :shared-target-concurrent-transactions-may-conflict))
    (is (zero? @(:calls c)) "Inspection must not dispatch")
    (is (= 0 (revision-of (:before c))))))

(deftest wrong-contract-source-identity-is-rejected
  (let [c (ctx (fn [_] nil)) final (start! c)]
    (is (= :changed-parallel-contract-source
           (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                     (get-in c [:parallel :handle])
                     (into {} (:source c)) final))))))

(deftest copied-or-replaced-final-system-is-rejected
  (let [c (ctx (fn [_] nil)) final (start! c)]
    (doseq [fake [(into {} final)
                  (assoc final app/biff-ring-handler-key (fn [_] {:status 200}))]]
      (is (= :parallel-final-handler-identity-mismatch
             (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                       (get-in c [:parallel :handle]) (:source c) fake)))))))

(deftest incorrect-route-method-and-uri-are-rejected-before-issuance
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (doseq [req [(assoc (:request c) :uri "/operations/other/claim")
                 (assoc (:request c) :request-method :get)
                 (assoc (:request c) :request-method "post")
                 (assoc (:request c) :uri nil)]]
      (is (= :unmatched-parallel-request-route
             (reason #(dispatch! c final req)))))
    (is (zero? @(:calls c)))
    (is (= 0 (revision-of (:before c))))))

(deftest malformed-ring-request-is-rejected-before-issuance
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (doseq [bad [nil [] {:request-method :post}
                 (with-meta (:request c) {:forged true})]]
      (is (= :unmatched-parallel-request-route
             (reason #(dispatch! c final bad)))))
    (is (zero? @(:calls c)))
    (is (= 0 (revision-of (:before c))))))

(deftest module-init-is-one-shot
  (let [c (ctx (fn [_] nil)) module (get-in c [:parallel :module])]
    ((:biff.core/init module) nil)
    (is (= :repeated-parallel-module-init
           (reason #((:biff.core/init module) nil))))))

(deftest failed-start-before-init-is-permanently-consumed
  (let [c (ctx (fn [_] nil)) module (get-in c [:parallel :module])]
    (is (= :parallel-module-not-initialized
           (reason #((:biff.core/start module) (:system c)))))
    ((:biff.core/init module) nil)
    (is (= :repeated-parallel-module-start
           (reason #((:biff.core/start module) (:system c)))))))

(deftest module-start-is-one-shot-after-success
  (let [c (ctx (fn [_] nil)) module (get-in c [:parallel :module])]
    (start! c)
    (is (= :repeated-parallel-module-start
           (reason #((:biff.core/start module) (:system c)))))))

(deftest issuance-validates-node-source-and-revision-field
  (let [c (ctx (fn [_] nil))
        base [(:adoption c) (:source c) wc/*source*
              {wc/op wc/path} wc/*node* (fn [_] nil) :revision]]
    (doseq [args [(assoc (vec base) 1 nil)
                  (assoc (vec base) 2 nil)
                  (assoc (vec base) 3 nil)
                  (assoc (vec base) 4 nil)
                  (assoc (vec base) 5 nil)
                  (assoc (vec base) 6 "revision")]]
      (is (= :invalid-parallel-mediation-issuance
             (reason #(apply parallel/issue-isolated-parallel-biff-request-module!
                             args)))))))

(deftest route-writer-binding-is-checked-before-handler-installation
  (let [c (ctx (fn [_] nil))]
    (doseq [binding [{} {wc/op :host/other :extra/route wc/path}
                     {wc/op :unqualified} {wc/op nil}]]
      (let [issued (parallel/issue-isolated-parallel-biff-request-module!
                    (:adoption c) (:source c) wc/*source* binding
                    wc/*node* (partial source! c) :revision)
            module (:module issued)]
        ((:biff.core/init module) nil)
        (is (= :requires-one-certified-literal-route-and-writer-binding
               (reason #((:biff.core/start module) (:system c)))))))))

(deftest substituted-original-issuer-is-rejected
  (let [c (ctx (fn [_] nil))]
    (with-redefs [writers/issue-request-scoped-monotonic-writer!
                  (fn [& _] (throw (ex-info "fake issuer" {})))]
      (is (= :substituted-parallel-mediation-boundary
             (reason #(dispatch! c (start! c) (:request c))))))))

(deftest substituted-host-adoption-is-rejected
  (let [c (ctx (fn [_] nil)) final (start! c)]
    (with-redefs [adoption/require-current-biff-host-adoption!
                  (fn [& _] {:type adoption/report-type})]
      (is (= :substituted-parallel-mediation-boundary
             (reason #(dispatch! c final (:request c))))))))

(deftest wrong-request-writer-path-is-rejected
  (let [access (atom nil)
        c (ctx (fn [_]
                 (reset! access (reason #(parallel/invoke-current-request-writer!
                                          :host/not-this-route)))))
        final (start! c)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #(dispatch! c final (:request c)))))
    (is (= :no-matching-active-thread-bound-request-writer @access))
    (is (= 0 (revision-of (:before c))))))

(deftest nested-dispatch-cannot-reuse-the-current-thread-scope
  (let [system (atom nil) nested (atom nil)
        c (ctx (fn [req]
                 (reset! nested
                         (reason #((get @system app/biff-ring-handler-key) req)))
                 (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (reset! system final)
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= :nested-parallel-request-dispatch @nested))
    (is (= 1 (revision-of (:before c))))))

(deftest off-thread-lookup-cannot-steal-another-requests-writer
  (let [result (atom nil)
        c (ctx (fn [_]
                 (reset! result
                         @(future (reason #(parallel/invoke-current-request-writer!
                                             wc/path))))
                 (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= :no-matching-active-thread-bound-request-writer @result))
    (is (= 1 (revision-of (:before c))))))

(deftest request-local-lookup-does-not-escape-after-handler-return
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= :no-matching-active-thread-bound-request-writer
           (reason #(parallel/invoke-current-request-writer! wc/path))))
    (is (= 1 (revision-of (:before c))))))

(deftest repeated-accessor-call-rejects-observation-with-one-physical-commit
  (let [second-call (atom nil)
        c (ctx (fn [_]
                 (parallel/invoke-current-request-writer! wc/path)
                 (reset! second-call
                         (reason #(parallel/invoke-current-request-writer! wc/path)))))
        final (start! c)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (reason #(dispatch! c final (:request c)))))
    (is (= :one-shot-already-attempted @second-call))
    (is (= 1 (revision-of (:before c))))))

(deftest exception-before-writer-does-not-submit
  (let [c (ctx (fn [_] (throw (ex-info "private" {:reason :before-submit}))))
        final (start! c)]
    (is (= :before-submit (reason #(dispatch! c final (:request c)))))
    (is (= 0 (revision-of (:before c))))))

(deftest exception-after-guarded-write-does-not-roll-back
  (let [c (ctx (fn [_]
                 (parallel/invoke-current-request-writer! wc/path)
                 (throw (ex-info "private" {:reason :after-submit}))))
        final (start! c)]
    (is (= :after-submit (reason #(dispatch! c final (:request c)))))
    (is (= 1 (revision-of (:before c))))))

(deftest invalid-request-source-fails-before-physical-submit
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption c) (:source c) wc/*source* {wc/op wc/path}
                wc/*node* (fn [_] nil) :revision)
        alternative (assoc c :parallel issued)
        final (start! alternative)]
    (is (reject-instead-of-throwing?
         #(dispatch! alternative final (:request c))))
    (is (= 0 (revision-of (:before c))))))

(deftest request-local-registration-does-not-mutate-old-global-registry
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        old-registry (:registry c)
        snapshot @old-registry
        final (start! c)]
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= snapshot @old-registry))
    (is (= 1 (revision-of (:before c))))))

(deftest direct-xtdb-writer-can-bypass-the-in-process-callback-trace
  (let [outside (atom nil)
        c (ctx (fn [_]
                 (let [rogue (assoc (wc/before) :rogue-writer true)]
                   (xt/execute-tx wc/*node* [[:put-docs wc/table rogue]])
                   (reset! outside rogue))
                 (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= 1 (revision-of (:before c))))
    (is (= true (:rogue-writer (wc/physical @outside))))
    (is (= :not-established (:other-writer-closure (report! c final))))))

(deftest retained-pre-mediation-handler-remains-an-independent-entry-point
  (let [bypass (atom nil)
        c (ctx (fn [_]
                 (reset! bypass :old-handler-ran)))
        final (start! c)
        original-handler (get (:system c) app/biff-ring-handler-key)]
    (is (fn? original-handler))
    (is (= 200 (:status (original-handler (:request c)))))
    (is (= :old-handler-ran @bypass))
    (is (= 0 (revision-of (:before c))))
    (is (= :conditional-request-local-writer-mediation-installed
           (:status (report! c final))))))

(deftest independent-request-sources-produce-independent-document-commits
  (let [c (ctx (fn [_] (parallel/invoke-current-request-writer! wc/path)))
        final (start! c)]
    (is (= 200 (:status (dispatch! c final (:request c)))))
    (is (= 200 (:status (dispatch! c final
                                 (assoc (:request c) :request-id :second)))))
    (is (= 1 (revision-of (:before c))))
    (is (= 1 (revision-of (:second-before c))))))

(deftest two-competing-same-document-requests-have-only-one-commit
  (let [entered (CountDownLatch. 2) release (CountDownLatch. 1)
        c (ctx (fn [_]
                 (.countDown entered)
                 (when-not (.await release 25 TimeUnit/SECONDS)
                   (throw (ex-info "fixture timeout" {:reason :fixture-timeout})))
                 (parallel/invoke-current-request-writer! wc/path)))
        final (start! c) request (:request c)
        outcomes (fn []
                   (try (dispatch! c final request)
                        :success
                        (catch Throwable t
                          (if (= "xtdb.error.Conflict" (.getName (class t)))
                            :guard-conflict
                            :unexpected-exception))))
        a (future (outcomes))
        b (future (outcomes))]
    (try
      (is (true? (.await entered 80 TimeUnit/SECONDS))
          "Both threads must reach their handlers before release")
      (finally (.countDown release)))
    (is (= #{:success :guard-conflict}
           (set [(deref a 90000 :timed-out)
                 (deref b 90000 :timed-out)])))
    (is (= 1 (revision-of (:before c))))))

(deftest report-does-not-leak-physical-private-fields-or-snapshot-tokens
  (let [c (ctx (fn [_] nil)) final (start! c)
        r (report! c final) printed (pr-str r)]
    (is (not (str/includes? printed "private-v1549-sentinel")))
    (is (not (str/includes? printed (:xt/id (:before c)))))
    (is (not-any? #(contains? r %)
                  [:xtdb-node :node :tx-ops :tx-key :before :after
                   :snapshot :request :writer :writer-handle :handler]))))
