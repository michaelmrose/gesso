(ns gesso.assurance.proof.xtdb-gateway-biff-request-mediation-test
  "v1564: permanent adversarial real Biff/XTDB checks for one issued
   mediated final-handler boundary. One local installed handler is not
   equivalent to deployed-server adoption or exhaustive XTDB writer closure."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [xtdb.api :as xt]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-biff-request-mediation :as mediation]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test :as lc]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as bh]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as app]))

(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                          (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (fixture run))))

(defn- private-fn [namespace symbol]
  (var-get (ns-resolve namespace symbol)))

(defn- failure [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :foreign-exception))))

(defn- context [behavior]
  (let [ctx ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
                         'context) behavior)
        registry (:registry ctx)
        _ (reset! registry {})
        source! (fn [_] ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                                    'gateway-source) ctx))
        issued (mediation/issue-checked-biff-request-mediation-module!
                (:adoption ctx) (:source ctx) (:inventory ctx) registry
                {} {wc/op wc/path} wc/*node* source! :revision)]
    (assoc ctx :issued-mediation issued)))

(defn- start! [ctx]
  (let [m (get-in ctx [:issued-mediation :module])]
    ((:biff.core/init m) nil)
    ((:biff.core/start m) (:system ctx))))

(defn- dispatch! [ctx final-system]
  ((get final-system app/biff-ring-handler-key) (:request ctx)))

(defn- writer [registry]
  (get-in @registry [wc/path :writer]))

(deftest automatic-final-handler-dispatch
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        final (start! ctx)
        report (mediation/require-current-biff-request-mediation!
                (get-in ctx [:issued-mediation :handle]) (:source ctx) final)]
    (is (not (identical? (get final app/biff-ring-handler-key)
                         (get (:system ctx) app/biff-ring-handler-key))))
    (is (= mediation/report-type (:type report)))
    (is (= :conditional-final-biff-handler-installed-from-checked-host (:status report)))
    (is (= :not-established (:deployed-server-use report)))
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 1 (:revision (wc/physical (:before ctx)))))
    (is (= {} @(:registry ctx)))
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 2 (:revision (wc/physical (:before ctx)))))
    (is (= 2 @(:calls ctx)))))

(deftest inspection-never-dispatches
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        final (start! ctx)]
    (is (= mediation/report-type
           (:type (mediation/require-current-biff-request-mediation!
                   (get-in ctx [:issued-mediation :handle]) (:source ctx) final))))
    (is (zero? @(:calls ctx)))
    (is (= {} @(:registry ctx)))))

(deftest copied-final-system-rejected
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        final (start! ctx)]
    (is (= :final-biff-handler-system-identity-mismatch
           (failure #(mediation/require-current-biff-request-mediation!
                      (get-in ctx [:issued-mediation :handle]) (:source ctx)
                      (into {} final)))))
    (is (= :final-biff-handler-system-identity-mismatch
           (failure #(mediation/require-current-biff-request-mediation!
                      (get-in ctx [:issued-mediation :handle]) (:source ctx)
                      (assoc final app/biff-ring-handler-key (fn [_] nil))))))))

(deftest forged-handle-and-original-source-rejected
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        final (start! ctx)]
    (is (= :unissued-mediation-module
           (failure #(mediation/require-current-biff-request-mediation!
                      (Object.) (:source ctx) final))))
    (is (= :changed-mediation-contract-source
           (failure #(mediation/require-current-biff-request-mediation!
                      (get-in ctx [:issued-mediation :handle])
                      (into {} (:source ctx)) final))))))

(deftest missing-writer-remains-fail-closed-and-cleans-registry
  (let [ctx (context (fn [_] nil)) final (start! ctx)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(dispatch! ctx final))))
    (is (= 0 (:revision (wc/physical (:before ctx)))))
    (is (= {} @(:registry ctx)))))

(deftest handler-exception-after-write-does-not-rollback
  (let [ctx (context (fn [{:keys [registry]}]
                       ((writer registry))
                       (throw (ex-info "private" {:reason :after-write}))))
        final (start! ctx)]
    (is (= :after-write (failure #(dispatch! ctx final))))
    (is (= 1 (:revision (wc/physical (:before ctx)))))
    (is (= {} @(:registry ctx)))))

(deftest startup-single-use
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        m (get-in ctx [:issued-mediation :module])]
    (is (= :mediation-module-not-initialized
           (failure #((:biff.core/start m) (:system ctx)))))
    (is (= :repeated-mediation-module-start
           (failure #((:biff.core/start m) (:system ctx)))))))

(deftest module-init-single-use
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        m (get-in ctx [:issued-mediation :module])]
    ((:biff.core/init m) nil)
    (is (= :repeated-mediation-module-init
           (failure #((:biff.core/init m) nil))))))

(deftest caller-cannot-use-old-handler-via-final-handler-report
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        final (start! ctx)]
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= :conditional-final-biff-handler-installed-from-checked-host
           (:status (mediation/require-current-biff-request-mediation!
                     (get-in ctx [:issued-mediation :handle]) (:source ctx) final))))
    (is (contains? (:open-obligations
                    (mediation/require-current-biff-request-mediation!
                     (get-in ctx [:issued-mediation :handle]) (:source ctx) final))
                   :direct-access-to-pre-mediation-handlers-and-other-entry-points))))

(deftest actual-biff-startup-installs-the-final-mediation-handler
  (let [ctx (context (fn [{:keys [registry]}] ((writer registry))))
        ;; This is a fresh host issuance: the context's original adoption
        ;; module already completed its independent Biff startup.
        fresh-host (gesso.assurance.proof.checked-biff-host-adoption/issue-checked-biff-host-module!
                    (:source ctx))
        source! (fn [_]
                  ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                               'gateway-source) ctx))
        fresh-mediator
        (mediation/issue-checked-biff-request-mediation-module!
         (:handle fresh-host) (:source ctx) (:inventory ctx)
         (:registry ctx) {} {wc/op wc/path} wc/*node* source! :revision)
        modules [app/application-handler-module (:module fresh-host) (:module fresh-mediator)]
        final
        (binding [bh/*biff-modules* modules]
          (app/start-biff-application!
           (:application-assembly (:source ctx)) {} #'bh/*biff-modules*
           [app/application-handler-module-id
            gesso.assurance.proof.checked-biff-host-adoption/module-id
            mediation/module-id]))]
    (is (= mediation/report-type
           (:type (mediation/require-current-biff-request-mediation!
                   (:handle fresh-mediator) (:source ctx) final))))
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 1 (:revision (wc/physical (:before ctx)))))
    (is (= {} @(:registry ctx)))))



;; Further adversarial cases: source and Biff startup integrity, exact installed
;; handler identity, failure cleanup, concurrent dispatch, and residual gaps.
;; All use the genuine checked Biff handler and embedded XTDB fixture above.

(defn- inspect! [ctx final]
  (mediation/require-current-biff-request-mediation!
   (get-in ctx [:issued-mediation :handle]) (:source ctx) final))

(defn- revision [ctx]
  (:revision (wc/physical (:before ctx))))

(defn- live-writer [ctx]
  (get-in @(:registry ctx) [wc/path :writer]))

(defn- dispatch-request! [ctx final request]
  ((get final app/biff-ring-handler-key) request))

(deftest report-preserves-scope-and-unproved-obligations
  (let [ctx (context (fn [_] nil))
        report (inspect! ctx (start! ctx))]
    (is (= mediation/report-version (:version report)))
    (is (= :one-issued-process-local-final-biff-system-map (:scope report)))
    (is (= :observed-once (:biff-module-init report)))
    (is (= :observed-once (:biff-module-start report)))
    (is (= :exact-identity-checked (:final-handler-slot report)))
    (is (= :source-checked-biff-adoption (:intermediate-handler report)))
    (is (= :at-dispatch-only (:fresh-request-scoped-writer-issuance report)))
    (is (= :not-observed-by-this-inspection (:request-invocation report)))
    (doseq [k [:deployed-server-use :complete-writer-mediation
               :physical-refinement]]
      (is (= :not-established (get report k))))
    (doseq [[k v] [[:certificate :not-issued] [:admission :not-admitted]
                  [:authority :none] [:transaction-authority :none]]]
      (is (= v (get report k))))
    (doseq [k [:server-must-consume-the-final-returned-biff-system
               :direct-access-to-pre-mediation-handlers-and-other-entry-points
               :later-biff-modules-or-server-may-replace-handler
               :concurrent-requests-through-this-controller-are-rejected
               :independent-controllers-and-host-registry-mutators
               :host-enumerator-exhaustiveness-and-external-xtdb-clients
               :request-source-builder-and-principal-authorization
               :caller-supplied-xtdb-node-deployment-identity
               :declared-operation-physical-write-correspondence
               :transaction-commit-causality-and-application-refinement]]
      (is (contains? (:open-obligations report) k)))))

(deftest inspection-cannot-expose-physical-document-or-capability
  (let [ctx (context (fn [_] nil))
        report (inspect! ctx (start! ctx))
        printable (pr-str report)]
    (is (not (str/includes? printable "private-v1549-sentinel")))
    (is (not (str/includes? printable (str (:xt/id (:before ctx))))))
    (is (not-any? #(contains? report %)
                  [:source :handler :callback :registry :node :document
                   :request :response :tx-key :writer :handle :system]))
    (is (zero? @(:calls ctx)))
    (is (= {} @(:registry ctx)))))

(deftest issued-handle-before-start-does-not-attest-installation
  (let [ctx (context (fn [_] nil))]
    (is (= :mediation-module-not-initialized
           (failure #(inspect! ctx (:system ctx)))))
    (is (zero? @(:calls ctx)))))

(deftest initialized-but-unstarted-module-does-not-attest-installation
  (let [ctx (context (fn [_] nil))
        module (get-in ctx [:issued-mediation :module])]
    ((:biff.core/init module) nil)
    (is (= :mediation-module-not-started
           (failure #(inspect! ctx (:system ctx)))))))

(deftest repeated-start-after-success-is-rejected
  (let [ctx (context (fn [_] nil))
        final (start! ctx)
        module (get-in ctx [:issued-mediation :module])]
    (is (= :repeated-mediation-module-start
           (failure #((:biff.core/start module) (:system ctx)))))
    (is (= mediation/report-type (:type (inspect! ctx final))))))

(deftest start-rejects-an-unadopted-system-and-poisons-retry
  (let [ctx (context (fn [_] nil))
        module (get-in ctx [:issued-mediation :module])]
    ((:biff.core/init module) nil)
    (is (not= :not-rejected
              (failure #((:biff.core/start module) {}))))
    (is (= :repeated-mediation-module-start
           (failure #((:biff.core/start module) (:system ctx)))))))

(deftest changing-physical-handler-slot-rejects-final-inspection
  (let [ctx (context (fn [_] nil))
        final (start! ctx)]
    (is (= :final-biff-handler-system-identity-mismatch
           (failure #(inspect! ctx
                               (assoc final app/biff-ring-handler-key
                                      (get (:system ctx) app/biff-ring-handler-key))))))
    (is (= mediation/report-type (:type (inspect! ctx final))))))

(deftest distinct-issuances-cannot-exchange-their-final-system-maps
  (let [ctx1 (context (fn [_] nil))
        ctx2 (context (fn [_] nil))
        final1 (start! ctx1)
        final2 (start! ctx2)]
    (is (= mediation/report-type (:type (inspect! ctx1 final1))))
    (is (= mediation/report-type (:type (inspect! ctx2 final2))))
    (is (= :final-biff-handler-system-identity-mismatch
           (failure #(inspect! ctx1 final2))))
    (is (= :final-biff-handler-system-identity-mismatch
           (failure #(inspect! ctx2 final1))))))

(deftest copied-source-cannot-authenticate-issued-handle
  (let [ctx (context (fn [_] nil))
        final (start! ctx)]
    (is (= :changed-mediation-contract-source
           (failure #(mediation/require-current-biff-request-mediation!
                      (get-in ctx [:issued-mediation :handle])
                      (into {} (:source ctx)) final))))))

(deftest unissued-handle-and-nil-handle-are-rejected
  (let [ctx (context (fn [_] nil))
        final (start! ctx)]
    (doseq [h [nil (Object.) (atom {}) {}]]
      (is (= :unissued-mediation-module
             (failure #(mediation/require-current-biff-request-mediation!
                        h (:source ctx) final)))))))

(deftest malformed-module-issuance-rejected-before-startup
  (let [ctx (context (fn [_] nil))
        valid [(:adoption ctx) (:source ctx) (:inventory ctx)
               (:registry ctx) {} {wc/op wc/path} wc/*node*
               (fn [_] nil) :revision]]
    (doseq [[index bad] [[2 []] [3 {}] [4 nil] [5 []]
                         [6 nil] [7 nil] [8 "revision"]]]
      (is (= :invalid-mediation-issuance
             (failure #(apply mediation/issue-checked-biff-request-mediation-module!
                              (assoc valid index bad))))))))

(deftest substituted-host-adoption-seam-is-rejected
  (let [ctx (context (fn [_] nil))]
    (is (= :substituted-biff-adoption-or-lifecycle
           (with-redefs [adoption/require-current-biff-host-adoption! (fn [& _] {})]
             (failure #(inspect! ctx (:system ctx))))))))

(deftest substituted-request-lifecycle-seam-is-rejected
  (let [ctx (context (fn [_] nil))]
    (is (= :substituted-biff-adoption-or-lifecycle
           (with-redefs [lifecycle/issue-checked-biff-request-writer-lifecycle!
                         (fn [& _] nil)]
             (failure #(inspect! ctx (:system ctx))))))))

(deftest final-handler-rechecks-original-checker-at-request-time
  (let [ctx (context (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer]))))
        final (start! ctx)]
    (is (= :substituted-biff-adoption-or-lifecycle
           (with-redefs [adoption/require-current-biff-host-adoption! (fn [& _] {})]
             (failure #(dispatch! ctx final)))))
    (is (zero? (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest invalid-method-rejected-without-capability-issuance
  (let [ctx (context (fn [_] nil)) final (start! ctx)]
    (is (= :invalid-ring-request
           (failure #(dispatch-request! ctx final
                                        (assoc (:request ctx) :request-method "post")))))
    (is (zero? @(:calls ctx)))
    (is (zero? (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest invalid-route-rejected-without-dispatch
  (let [ctx (context (fn [_] nil)) final (start! ctx)]
    (is (= :request-not-one-bound-certified-literal-route
           (failure #(dispatch-request! ctx final
                                        (assoc (:request ctx) :uri "/not-certified")))))
    (is (zero? @(:calls ctx)))
    (is (zero? (revision ctx)))))

(deftest nil-request-cannot-start-lifecycle
  (let [ctx (context (fn [_] nil)) final (start! ctx)]
    (is (= :invalid-ring-request
           (failure #(dispatch-request! ctx final nil))))
    (is (zero? @(:calls ctx)))
    (is (empty? @(:registry ctx)))))

(deftest source-builder-failure-retires-lease-and-next-request-can-succeed
  (let [fail? (atom true)
        ctx (context (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer]))))
        module (get-in ctx [:issued-mediation :module])
        ;; The issuer's stored source builder deliberately throws once.
        _ (is (map? module))]
    ;; A second module issuance owns its own source builder and handle.
    (let [issued (mediation/issue-checked-biff-request-mediation-module!
                  (:adoption ctx) (:source ctx) (:inventory ctx) (:registry ctx)
                  {} {wc/op wc/path} wc/*node*
                  (fn [_]
                    (if (compare-and-set! fail? true false)
                      (throw (ex-info "private-builder" {:reason :builder-failure}))
                      ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                                   'gateway-source) ctx)))
                  :revision)
          module (:module issued)]
      ((:biff.core/init module) nil)
      (let [final ((:biff.core/start module) (:system ctx))]
        (is (= :builder-failure
               (failure #(dispatch! ctx final))))
        (is (= {} @(:registry ctx)))
        (is (zero? (revision ctx)))
        (is (= 200 (:status (dispatch! ctx final))))
        (is (= 1 (revision ctx)))
        (is (= {} @(:registry ctx)))))))

(deftest missing-writer-first-request-does-not-poison-next-issuance
  (let [invoke? (atom false)
        ctx (context (fn [{:keys [registry]}]
                       (when @invoke? ((get-in @registry [wc/path :writer])))))
        final (start! ctx)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(dispatch! ctx final))))
    (is (zero? (revision ctx)))
    (is (= {} @(:registry ctx)))
    (reset! invoke? true)
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 1 (revision ctx)))))

(deftest repeat-callback-in-one-request-does-not-submit-twice
  (let [second-reason (atom nil)
        ctx (context (fn [{:keys [registry]}]
                       (let [callback (get-in @registry [wc/path :writer])]
                         (callback)
                         (reset! second-reason (failure callback)))))
        final (start! ctx)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(dispatch! ctx final))))
    (is (= :one-shot-already-attempted @second-reason))
    (is (= 1 (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest saved-callback-is-closed-after-dispatch
  (let [saved (atom nil)
        ctx (context (fn [{:keys [registry]}]
                       (reset! saved (get-in @registry [wc/path :writer]))
                       (@saved)))
        final (start! ctx)]
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= :writer-outside-owning-request-scope (failure @saved)))
    (is (= {} @(:registry ctx)))
    (is (= 1 (revision ctx)))))

(deftest foreign-thread-callback-cannot-submit-a-transaction
  (let [thread-reason (atom nil)
        ctx (context (fn [{:keys [registry]}]
                       (let [callback (get-in @registry [wc/path :writer])]
                         (reset! thread-reason @(future (failure callback))))))
        final (start! ctx)]
    (is (not= :not-rejected (failure #(dispatch! ctx final))))
    (is (= :writer-outside-owning-request-scope @thread-reason))
    (is (zero? (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest handler-registry-replacement-fails-cleanup-without-clobber
  (let [replacement {:kind :host-external :operation-id wc/op :writer (fn [] :external)}
        ctx (context (fn [{:keys [registry]}]
                       (let [original (get-in @registry [wc/path :writer])]
                         (original)
                         (reset! registry {wc/path replacement}))))
        final (start! ctx)]
    (is (= :host-registration-changed-cleanup-not-overwritten
           (failure #(dispatch! ctx final))))
    (is (= {wc/path replacement} @(:registry ctx)))
    (is (= 1 (revision ctx)))))

(deftest tampered-registry-before-request-rejected-and-retained
  (let [ctx (context (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer]))))
        final (start! ctx)
        injected {:kind :raw :operation-id nil :writer (fn [] :raw)}]
    (reset! (:registry ctx) {:host/rogue injected})
    (is (not= :not-rejected (failure #(dispatch! ctx final))))
    (is (= {:host/rogue injected} @(:registry ctx)))
    (is (zero? (revision ctx)))))

(deftest two-concurrent-requests-share-no-single-one-shot
  (let [entered (promise)
        release (promise)
        ctx (context (fn [{:keys [registry]}]
                       (deliver entered true)
                       @release
                       ((get-in @registry [wc/path :writer]))))
        final (start! ctx)
        pending (future (failure #(dispatch! ctx final)))]
    (try
      (is (= true (deref entered 90000 :timed-out)))
      (is (= :concurrent-host-request-lifecycle
             (failure #(dispatch! ctx final))))
      (finally (deliver release true)))
    (is (= :not-rejected (deref pending 90000 :timed-out)))
    (is (= 1 (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest nested-handler-dispatch-rejected-but-outer-request-can-complete
  (let [final-slot (atom nil)
        nested-reason (atom nil)
        ctx (context (fn [{:keys [registry request]}]
                       (reset! nested-reason
                               (failure #(dispatch-request!
                                          {:request request} @final-slot request)))
                       ((get-in @registry [wc/path :writer]))))
        final (start! ctx)]
    (reset! final-slot final)
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= :concurrent-host-request-lifecycle @nested-reason))
    (is (= 1 (revision ctx)))
    (is (= {} @(:registry ctx)))))

(deftest older-handler-can-still-be-invoked-without-mediation
  (let [ctx (context (fn [_] nil))
        final (start! ctx)
        old-handler (get (:system ctx) app/biff-ring-handler-key)]
    (is (= 200 (:status (old-handler (:request ctx)))))
    (is (= 0 (revision ctx)))
    (is (= {} @(:registry ctx)))
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(dispatch! ctx final))))
    (is (contains? (:open-obligations (inspect! ctx final))
                   :direct-access-to-pre-mediation-handlers-and-other-entry-points))))

(deftest raw-xtdb-write-in-handler-remains-invisible-to-mediator
  (let [ctx (context (fn [{:keys [registry before]}]
                       (xt/execute-tx wc/*node*
                                      [[:put-docs wc/table
                                        {:xt/id (str (random-uuid))
                                         :revision 0 :created-at "raw"
                                         :updated-at "raw" :owner "raw"}]])
                       ((get-in @registry [wc/path :writer]))))
        final (start! ctx)]
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 1 (revision ctx)))
    (is (= {} @(:registry ctx)))
    (is (= :not-established (:complete-writer-mediation (inspect! ctx final))))))

(deftest inspection-after-valid-request-does-not-claim-an-observed-request
  (let [ctx (context (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer]))))
        final (start! ctx)]
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= :not-observed-by-this-inspection
           (:request-invocation (inspect! ctx final))))
    (is (= :not-established (:deployed-server-use (inspect! ctx final))))
    (is (= {} @(:registry ctx)))))

(deftest distinct-consecutive-request-callback-identities
  (let [saved (atom [])
        ctx (context (fn [{:keys [registry]}]
                       (let [callback (get-in @registry [wc/path :writer])]
                         (swap! saved conj callback)
                         (callback))))
        final (start! ctx)]
    (dotimes [n 3]
      (is (= 200 (:status (dispatch! ctx final))))
      (is (= (inc n) (revision ctx)))
      (is (= {} @(:registry ctx))))
    (is (= 3 (count (distinct @saved))))
    (doseq [previous @saved]
      (is (= :writer-outside-owning-request-scope (failure previous))))))

(deftest native-biff-module-order-keeps-final-handler-distinct
  (let [ctx (context (fn [{:keys [registry]}] ((get-in @registry [wc/path :writer]))))
        fresh-host (adoption/issue-checked-biff-host-module! (:source ctx))
        fresh-mediator
        (mediation/issue-checked-biff-request-mediation-module!
         (:handle fresh-host) (:source ctx) (:inventory ctx) (:registry ctx)
         {} {wc/op wc/path} wc/*node*
         (fn [_] ((private-fn 'gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
                             'gateway-source) ctx)) :revision)
        mods [app/application-handler-module (:module fresh-host) (:module fresh-mediator)]
        final (binding [bh/*biff-modules* mods]
                (app/start-biff-application!
                 (:application-assembly (:source ctx)) {} #'bh/*biff-modules*
                 [app/application-handler-module-id adoption/module-id mediation/module-id]))]
    (is (= mediation/report-type
           (:type (mediation/require-current-biff-request-mediation!
                   (:handle fresh-mediator) (:source ctx) final))))
    (is (fn? (get final app/biff-ring-handler-key)))
    (is (= 200 (:status (dispatch! ctx final))))
    (is (= 1 (revision ctx)))
    (is (= {} @(:registry ctx)))))
