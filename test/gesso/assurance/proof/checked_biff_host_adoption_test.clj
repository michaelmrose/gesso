(ns gesso.assurance.proof.checked-biff-host-adoption-test
  "v1479: Adversarial checks of the v1478 Biff host-adoption boundary.

   Positive tests use a real closed ApplicationAssembly, a v1251-issued
   certified-plan executor, an issued Ring handler and Biff's actual module
   lifecycle. No source, route checker, or proof is stubbed to make a positive
   adoption succeed. Negative tests deliberately substitute boundaries.

   The recorded result authenticates only a process-local module callback and
   one returned system-map handler slot. It must never be interpreted as
   evidence that an HTTP server consumed that handler, that later modules did
   not replace it, or that all physical database writers are mediated."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.installed-route-provenance :as routes]
            [gesso.http :as http]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:dynamic *case* nil)
(def ^:dynamic *biff-test-modules* [])

(defn- plan-fixture [name]
  (let [v (ns-resolve 'gesso.model.certified-plan-test name)]
    (when-not v
      (throw (ex-info "Missing certified-plan fixture" {:symbol name})))
    (var-get v)))

(defn- request
  ([] (request :biff))
  ([id] {:request-method :post
         :uri "/operations/example/claim"
         :request-id id}))

(defn- rejection [thunk]
  (try (thunk) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (:checked-biff-host-adoption/rejection (ex-data e))
           (:reason (ex-data e))
           [:unexpected-error (ex-data e)]))))

(defn- rejects! [reason thunk]
  (is (= reason (rejection thunk))
      (str "Expected host-adoption rejection " reason)))

(use-fixtures
  :once
  (fn [run]
    (let [plan-source (:source ((plan-fixture 'environment)))
          executor (plan/build-certified-plan-operation! plan-source)
          calls (atom [])
          handler (certified/issue-certified-handler!
                   {:executor executor
                    :current-source plan-source
                    :request->input!
                    (fn [req]
                      (swap! calls conj [:input req])
                      ((plan-fixture 'invocation) :reconcile))
                    :result->response!
                    (fn [req result]
                      (swap! calls conj [:response req result])
                      {:status 200 :body result})})]
      (fixture/with-closed-application
        {:operations {operation
                      (fixture/trusted-operation
                       operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [*case* {:source {:application-assembly assembly
                                    :semantic-route-handlers {operation handler}
                                    :certified-operations
                                    {operation {:executor executor
                                                :current-source plan-source}}}
                            :handler handler
                            :executor executor
                            :plan-source plan-source
                            :calls calls}]
            (run)))))))

(defn- issue! []
  (host/issue-checked-biff-host-module! (:source *case*)))

(defn- startup!
  ([issued] (startup! issued {}))
  ([issued {:keys [other-modules order initial-system]}]
   (let [mods (into [application/application-handler-module
                     (:module issued)]
                    other-modules)
         order (or order (into [application/application-handler-module-id
                               host/module-id]
                              (map :biff.core/id other-modules)))]
     (binding [*biff-test-modules* mods]
       (application/start-biff-application!
        (get-in *case* [:source :application-assembly])
        (or initial-system {})
        #'*biff-test-modules*
        order)))))

(defn- report! [issued system]
  (host/require-current-biff-host-adoption!
   (:handle issued) (:source *case*) system))

(defn- issue-and-start! []
  (let [issued (issue!)
        system (startup! issued)]
    {:issued issued :system system :report (report! issued system)}))

(deftest real-biff-module-is-issued-with-limited-process-local-scope
  (let [{:keys [issued system report]} (issue-and-start!)
        handler (:biff.ring/handler system)]
    (is (host/checked-biff-host-adoption? (:handle issued)))
    (is (not (host/checked-biff-host-adoption? (Object.))))
    (is (= host/module-id (get-in issued [:module :biff.core/id])))
    (is (= host/report-type (:type report)))
    (is (= host/report-version (:version report)))
    (is (= :one-in-process-biff-system-handler-slot (:scope report)))
    (is (= :observed-once (:module-start report)))
    (is (= :observed-once (:biff-module-init report)))
    (is (= :identical-to-guarded-checked-router-wrapper
           (:handler-slot report)))
    (is (= :wrapped-at-module-start (:canonical-html-preflight report)))
    (is (= #{operation} (:checked-route-ids report)))
    (is (= #{operation} (:checked-certified-route-ids report)))
    (is (= :not-established (:host-deployment report)))
    (is (= :not-established (:physical-correspondence report)))
    (is (= :not-issued (:certification report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))
    (is (fn? handler))
    (is (not (identical? handler (:handler *case*))))
    (is (not (contains? system
                        :gesso.live.application-preflight/application-assembly)))
    (is (not-any? #(contains? report %)
                  [:valid? :handler :router :contract-source :system :source
                   :request :response :executor :witness]))
    (is (= report (report! issued system)))))

(deftest actual-adopted-handler-dispatches-one-certified-route-once
  (let [{:keys [issued system]} (issue-and-start!)
        calls (:calls *case*)
        before (count @calls)
        req (request :once)
        response ((:biff.ring/handler system) req)]
    (is (= 200 (:status response)))
    (is (= 2 (- (count @calls) before)))
    (is (= [:input req] (nth @calls before)))
    (is (identical? req (second (nth @calls (inc before)))))
    (is (= :response (first (nth @calls (inc before)))))
    (is (= 404 (:status ((:biff.ring/handler system)
                        {:request-method :get :uri (:uri req)}))))
    (is (= 2 (- (count @calls) before)))
    (is (= host/report-type (:type (report! issued system))))
    (is (= 2 (- (count @calls) before))
        "Report inspection must never execute the handler again")))

(deftest unissued-and-forged-handles-and-identity-copies-fail-closed
  (let [{:keys [issued system]} (issue-and-start!)
        source (:source *case*)
        forged (host/->HostAdoptionHandle)]
    (rejects! :unissued-host-adoption
              #(host/require-current-biff-host-adoption!
                (Object.) source system))
    (rejects! :unissued-host-adoption
              #(host/require-current-biff-host-adoption!
                forged source system))
    (rejects! :changed-host-adoption-source
              #(host/require-current-biff-host-adoption!
                (:handle issued) (into {} source) system))
    (rejects! :changed-host-adoption-source
              #(host/require-current-biff-host-adoption!
                (:handle issued) (assoc source :extra true) system))
    (is (= host/report-type (:type (report! issued system))))))

(deftest missing-and-substituted-host-handler-slots-are-not-adoption
  (let [{:keys [issued system]} (issue-and-start!)
        fake (fn [_] {:status 200})]
    (doseq [altered [nil [] :fake {}
                     (dissoc system application/biff-ring-handler-key)
                     (assoc system application/biff-ring-handler-key nil)
                     (assoc system application/biff-ring-handler-key fake)
                     (assoc system application/biff-ring-handler-key
                            (:handler *case*))]]
      (rejects! :host-handler-slot-mismatch
                #(report! issued altered)))
    (is (= :one-in-process-biff-system-handler-slot
           (:scope (report! issued system))))))

(deftest wrong-completed-system-cannot-be-borrowed-by-another-issuance
  (let [a (issue!) b (issue!)
        sa (startup! a)
        sb (startup! b)]
    (is (not (identical? (get sa application/biff-ring-handler-key)
                         (get sb application/biff-ring-handler-key))))
    (rejects! :host-handler-slot-mismatch #(report! a sb))
    (rejects! :host-handler-slot-mismatch #(report! b sa))
    (is (= :observed-once (:module-start (report! a sa))))
    (is (= :observed-once (:module-start (report! b sb))))))

(deftest copied-system-map-is-not-independent-startup-authentication
  (let [{:keys [issued system report]} (issue-and-start!)
        ;; Any caller with the returned handler reference can fabricate another
        ;; map carrying the same slot.  The local report must not advertise
        ;; system-map origin or actual web-server consumption as proved.
        imitation {application/biff-ring-handler-key
                   (get system application/biff-ring-handler-key)
                   :fixture/not-biff-returned true}]
    (is (= report (report! issued imitation)))
    (is (= :not-established (:host-deployment report)))
    (is (contains? (:open-obligations report)
                   :server-consumes-the-returned-biff-handler-not-established))
    (is (= :none (:authority report)))))

(deftest fake-module-with-same-biff-id-cannot-start-issued-host
  (let [issued (issue!)
        forged {:biff.core/id host/module-id
                :biff.core/start identity}
        mods [application/application-handler-module forged]
        system (binding [*biff-test-modules* mods]
                 (application/start-biff-application!
                  (get-in *case* [:source :application-assembly])
                  {application/biff-ring-handler-key
                   (:handler *case*)}
                  #'*biff-test-modules*
                  [application/application-handler-module-id host/module-id]))]
    (is (fn? (get system application/biff-ring-handler-key)))
    (rejects! :biff-module-init-not-observed #(report! issued system))))

(deftest rechecking-a-report-never-implicitly-restarts-a-module
  (let [{:keys [issued system]} (issue-and-start!)
        calls (:calls *case*)
        before (count @calls)]
    (dotimes [_ 3] (is (= host/report-type (:type (report! issued system)))))
    (is (= before (count @calls)))
    (rejects! :repeated-biff-module-init
              #((get-in issued [:module :biff.core/init]) #'*biff-test-modules*))
    (rejects! :repeated-biff-module-start
              #((get-in issued [:module :biff.core/start]) system))
    (is (= host/report-type (:type (report! issued system))))))

(deftest start-before-init-is-poisoned-not-upgraded-by-later-init
  (let [{:keys [module handle]} (issue!)
        source (:source *case*)]
    (rejects! :biff-module-init-not-observed
              #((:biff.core/start module)
                {application/biff-ring-handler-key (:handler *case*)}))
    ;; Initializing after the rejected start cannot launder its state.
    (is (map? ((:biff.core/init module) #'*biff-test-modules*)))
    (rejects! :repeated-biff-module-start
              #((:biff.core/start module)
                {application/biff-ring-handler-key (:handler *case*)}))
    (rejects! :host-adoption-start-not-completed
              #(host/require-current-biff-host-adoption!
                handle source {application/biff-ring-handler-key
                               (:handler *case*)}))))

(deftest failed-start-is-single-use-even-after-corrected-system
  (doseq [bad [nil [] {}
               {application/biff-ring-handler-key nil}
               {application/biff-ring-handler-key :fake}]]
    (let [{:keys [module handle]} (issue!)
          source (:source *case*)]
      ((:biff.core/init module) #'*biff-test-modules*)
      (rejects! :missing-or-invalid-preexisting-biff-handler
                #((:biff.core/start module) bad))
      (rejects! :repeated-biff-module-start
                #((:biff.core/start module)
                  {application/biff-ring-handler-key (:handler *case*)}))
      (rejects! :host-adoption-start-not-completed
                #(host/require-current-biff-host-adoption!
                  handle source
                  {application/biff-ring-handler-key (:handler *case*)})))))

(deftest duplicate-initialization-is-rejected-before-any-start
  (let [{:keys [module handle]} (issue!)
        source (:source *case*)]
    (is (fn? (get ((:biff.core/init module) #'*biff-test-modules*)
                  application/biff-ring-handler-key)))
    (rejects! :repeated-biff-module-init
              #((:biff.core/init module) #'*biff-test-modules*))
    (rejects! :host-adoption-start-not-completed
              #(host/require-current-biff-host-adoption!
                handle source {}))))

(deftest invalid-or-unissued-contract-source-cannot-create-a-host-module
  (let [source (:source *case*)]
    (doseq [bad [nil {} [] :source (dissoc source :application-assembly)
                 (assoc source :application-assembly {})]]
      (rejects! :invalid-application-source
                #(host/issue-checked-biff-host-module! bad)))
    (is (host/checked-biff-host-adoption? (:handle (issue!))))))

(deftest substituted-core-checkers-cannot-create-new-host-adoption
  (let [source (:source *case*)]
    (with-redefs [application/wrap-application-handler
                  (fn [& _] (constantly {:status 200}))]
      (rejects! :substituted-biff-or-route-boundary
                #(host/issue-checked-biff-host-module! source)))
    (with-redefs [application/application-assembly? (constantly true)]
      (rejects! :substituted-biff-or-route-boundary
                #(host/issue-checked-biff-host-module! source)))
    (with-redefs [routes/install-checked-ring-router!
                  (fn [_] (constantly {:status 200}))]
      (rejects! :substituted-biff-or-route-boundary
                #(host/issue-checked-biff-host-module! source)))
    (with-redefs [routes/describe-checked-ring-router
                  (fn [& _] {:route-ids #{operation}})]
      (rejects! :substituted-biff-or-route-boundary
                #(host/issue-checked-biff-host-module! source)))
    (is (host/checked-biff-host-adoption? (:handle (issue!))))))

(deftest checker-rebinding-after-start-invalidates-report
  (let [{:keys [issued system]} (issue-and-start!)]
    (with-redefs [routes/describe-checked-ring-router
                  (fn [& _] {:route-ids #{operation}})]
      (rejects! :substituted-biff-or-route-boundary
                #(report! issued system)))
    (with-redefs [application/wrap-application-handler
                  (fn [& _] (constantly {:status 200}))]
      (rejects! :substituted-biff-or-route-boundary
                #(report! issued system)))
    (is (= host/report-type (:type (report! issued system))))))

(deftest missing-start-order-id-is-rejected-by-biff-without-adoption
  (let [issued (issue!)]
    ;; Native Biff requires the start order to enumerate every module.  It
    ;; rejects before our module can even initialize: no host report exists.
    (is (thrown? AssertionError
                 (startup! issued
                           {:order [application/application-handler-module-id]})))
    (rejects! :biff-module-init-not-observed
              #(report! issued {}))))

(deftest earlier-server-capture-is-reported-as-open-not-proven-absent
  (let [issued (issue!)
        captured (atom nil)
        capture-id :fixture/capture-before-checked-host
        capture {:biff.core/id capture-id
                 :biff.core/start
                 (fn [system]
                   (reset! captured
                           (get system application/biff-ring-handler-key))
                   system)}
        system (startup!
                issued
                {:other-modules [capture]
                 :order [application/application-handler-module-id
                         capture-id
                         host/module-id]})
        report (report! issued system)]
    (is (fn? @captured))
    (is (not (identical? @captured
                         (get system application/biff-ring-handler-key))))
    (is (contains? (:open-obligations report)
                   :server-may-capture-an-earlier-handler))
    (is (= :not-established (:host-deployment report)))
    (is (= :not-admitted (:admission report)))))

(deftest later-module-replacement-invalidates-final-system-slot
  (let [issued (issue!)
        replace-id :fixture/replace-biff-handler
        replacement (fn [_] {:status 200 :body "unexamined"})
        replace-module {:biff.core/id replace-id
                        :biff.core/start
                        (fn [system]
                          (assoc system application/biff-ring-handler-key
                                 replacement))}
        system (startup!
                issued {:other-modules [replace-module]})]
    (is (identical? replacement
                    (get system application/biff-ring-handler-key)))
    (rejects! :host-handler-slot-mismatch #(report! issued system))))

(deftest unknown-independent-routes-remain-an-explicit-open-obligation
  (let [{:keys [report]} (issue-and-start!)]
    (is (contains? (:open-obligations report)
                   :no-independent-router-or-raw-writer-bypass))
    (is (contains? (:open-obligations report)
                   :complete-physical-writer-coverage))
    (is (contains? (:open-obligations report)
                   :universal-xtdb-refinement))
    (is (contains? (:open-obligations report)
                   :biff-start-order-not-independently-attested))
    (is (contains? (:open-obligations report)
                   :trusted-ring-request-method-uri-and-principal))))

(deftest preserved-html-preflight-runs-at-final-installed-handler
  (let [source (:source *case*)
        handler (certified/issue-certified-handler!
                 {:executor (:executor *case*)
                  :current-source (:plan-source *case*)
                  :request->input! (fn [_] ((plan-fixture 'invocation) :reconcile))
                  :result->response! (fn [_ _]
                                       (http/html-response
                                        [:p {:data-fixture "guarded"}
                                         "rendered"]))})
        altered-source (assoc-in source [:semantic-route-handlers operation]
                                 handler)
        issued (host/issue-checked-biff-host-module! altered-source)
        mods [application/application-handler-module (:module issued)]
        system (binding [*biff-test-modules* mods]
                 (application/start-biff-application!
                  (:application-assembly source)
                  #'*biff-test-modules*
                  [application/application-handler-module-id host/module-id]))
        seen (atom [])
        expected (ex-info "guard blocked rendering" {:test/guard :rejected})]
    (with-redefs [application/require-rendered-surface!
                  (fn [assembly surface hiccup]
                    (swap! seen conj [assembly surface hiccup])
                    (throw expected))]
      (is (identical? expected
                      (try ((get system application/biff-ring-handler-key)
                            (request :guard))
                           (catch clojure.lang.ExceptionInfo e e)))))
    (is (= 1 (count @seen)))
    (is (identical? (:application-assembly source) (ffirst @seen)))
    (is (= :gesso.live.application-preflight/html-response
           (second (first @seen))))
    (is (= [:p {:data-fixture "guarded"} "rendered"]
           (nth (first @seen) 2)))
    (is (= :wrapped-at-module-start
           (:canonical-html-preflight
            (host/require-current-biff-host-adoption!
             (:handle issued) altered-source system))))))
