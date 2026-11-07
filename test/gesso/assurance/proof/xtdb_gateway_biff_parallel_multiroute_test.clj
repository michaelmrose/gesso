(ns gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
  "v1570: real certified multi-route parallel Biff mediation, including two
   independently checked operation/writer sources and physical XTDB guards.
   Observed request-local callbacks do not establish exhaustive deployed
   writer coverage, business-rule equivalence, or physical causality."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as coverage]
            [gesso.model.inductive-plan :as inductive-plan]
            [gesso.model.certified-plan-test]
            [gesso.model.inductive-plan-test]
            [gesso.assurance.proof.abstract-execution-induction-test :as abstract-fixture]
            [gesso.assurance.proof.installed-route-provenance-test :as ring]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight :as app]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(def claim :example/claim)
(def cancel :example/cancel)
(def claim-route :route/claim)
(def cancel-route :route/cancel)
(def claim-path :host/claim)
(def cancel-path :host/cancel)
(def routes {claim-route {:operation claim :path "/operations/example/claim"}
             cancel-route {:operation cancel :path "/operations/example/cancel"}})
(def writer-bindings {claim-route claim-path cancel-route cancel-path})
(def ^:dynamic *second* nil)
(def ^:dynamic *modules* [])

(defn- original [ns-name symbol]
  (let [v (ns-resolve ns-name symbol)]
    (when-not v (throw (ex-info "Missing required original test source" {:symbol symbol})))
    (var-get v)))
(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e (or (:reason (ex-data e)) :unexpected-error))
       (catch Throwable _ :unexpected-throwable)))

(defn- second-operation! []
  (let [base (:source ((original 'gesso.model.certified-plan-test 'environment)))
        construction (-> (:construction base)
                         (assoc-in [:subject :id] cancel)
                         (assoc-in [:subject :interface :operation] cancel))
        subject (:subject construction)
        transition (-> construction (dissoc :replacements :frame)
                       (assoc :assignments (:replacements construction)))
        proof {:transition transition
               :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
               :goal [:guard]}
        graph {:roots #{cancel}
               :nodes [{:id cancel :subject subject
                        :subject-handle (admission/admit-subject! subject)
                        :depends-on []}]}
        cert (certificate/admit-certificate!
              {:proof-kind :guarded-transition :proof-source proof
               :graph-handle (dependency/check-graph! graph)
               :current-graph graph
               :expected-open-premises #{:gesso.assurance.proof.derivation/abstract-relation-match}
               :expected-open-obligations
               #{:production-transition-correspondence
                 :current-runtime-authorization
                 :atomic-transaction-enforcement
                 :checker-meta-soundness}})
        plan-source (-> base
                        (assoc :construction construction)
                        (assoc :certified-source
                               (-> (:certified-source base)
                                   (assoc :operation cancel :certificate cert
                                          :proof-source proof :current-graph graph)
                                   (assoc-in [:resolved-configuration :operation] cancel))))
        catalog (-> ((original 'gesso.model.inductive-plan-test 'catalog-for) construction)
                    (assoc :declared-operation-ids #{cancel}
                           :operations [{:id cancel :construction construction}]))
        induction-proof ((original 'gesso.model.inductive-plan-test 'proof-package) catalog)
        induction-source {:inductive-source induction-proof
                          :certified-plan-source plan-source}
        executor (inductive-plan/build-inductive-plan-operation! induction-source)
        invocation (assoc-in
                    ((original 'gesso.model.certified-plan-test 'invocation) :reconcile)
                    [:binding :operation] cancel)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source plan-source
                  :request->input! (fn [_] invocation)
                  :result->response! (fn [_ result] {:status 200 :body result})})
        declaration (-> abstract-fixture/operation
                        (assoc-in [:subject :id] cancel)
                        (assoc-in [:subject :interface :operation] cancel))
        abstract-catalog (assoc abstract-fixture/base
                                :declared-operation-ids #{cancel}
                                :operations [{:id cancel :declaration declaration}])]
    {:plan-source plan-source :executor executor :induction-source induction-source
     :handler handler :proof induction-proof :abstract-catalog abstract-catalog}))

(defn- independent-second-source [op assembly]
  (let [{:keys [plan-source executor induction-source handler proof abstract-catalog]} op
        contract {:inductive-source proof
                  :application-assembly assembly
                  :semantic-route-handlers {cancel handler}
                  :certified-operations {cancel {:executor executor :current-source plan-source}}}
        observed {:inductive-contract-source contract
                  :enumerate-entry-points! (fn [] {cancel {:kind :semantic-route :handler handler}})}]
    {:induction-catalog abstract-catalog
     :application-induction-source
     {:authoritative-coverage-source observed
      :inductive-plans {cancel {:executor executor :current-source induction-source}}}}))

(deftest independent-operation-source-is-a-real-checked-join
  (let [report (coverage/derive-observed-writer-coverage! (:writer-source *second*))]
    (is (= #{cancel} (:independent-operation-ids report)))
    (is (= #{cancel} (:issued-application-operation-ids report)))
    (is (:conditional-id-join? report))
    (is (= :none (:transaction-authority report)))))

(def ^:dynamic *case* nil)

(defn- source-for-request [c request]
  (wc/source (if (= (:uri request) "/operations/example/claim")
               (:claim-before c) (:cancel-before c))))

(defn- build-case []
  (let [claim-before (wc/seeded)
        cancel-before (wc/seeded)
        current (atom nil)
        source-calls (atom [])
        source-builder (fn [request]
                         (swap! source-calls conj (:uri request))
                         (source-for-request @current request))
        module-sources {:per-operation-writer-sources
                        {claim wc/*source* cancel (:writer-source *second*)}}
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption *case*) (:source *case*) module-sources writer-bindings
                wc/*node* source-builder :revision)
        c {:claim-before claim-before :cancel-before cancel-before
           :source-calls source-calls :issued issued
           :source (:source *case*) :system (:system *case*)
           :adoption (:adoption *case*)}]
    (reset! current c)
    c))

(defn- start-case! [c]
  (let [module (get-in c [:issued :module])]
    ((:biff.core/init module) nil)
    ((:biff.core/start module) (:system c))))

(defn- req [route]
  {:request-method :post
   :uri (case route :claim "/operations/example/claim"
                    :cancel "/operations/example/cancel")
   :request-id (random-uuid)})

(defn- call! [system route]
  ((get system app/biff-ring-handler-key) (req route)))

(defn- rev [b] (:revision (wc/physical b)))
(defn- expected-path [request]
  (if (= "/operations/example/claim" (:uri request)) claim-path cancel-path))
(defn- invoke! [request]
  (parallel/invoke-current-request-writer! (expected-path request)))

(defn- use-case [f]
  (let [c (build-case)
        final (start-case! c)]
    (f c final)))

(defn- checked-application-fixture [run]
  (let [claim-source (:plan-source ring/*case*)
        claim-executor (:executor ring/*case*)
        cancel-source (:plan-source *second*)
        cancel-executor (:executor *second*)
        behavior (atom invoke!)
        calls (atom [])
        issue-handler (fn [exec plan]
                        (certified/issue-certified-handler!
                         {:executor exec :current-source plan
                          :request->input!
                          (fn [request]
                            (swap! calls conj (:uri request))
                            (@behavior request)
                            (assoc-in
                             ((original 'gesso.model.certified-plan-test 'invocation) :reconcile)
                             [:binding :operation]
                             (if (= (:uri request) "/operations/example/claim") claim cancel)))
                          :result->response! (fn [_ result] {:status 200 :body result})}))
        h1 (issue-handler claim-executor claim-source)
        h2 (issue-handler cancel-executor cancel-source)]
    (fixture/with-surfaced-application
      {:operations {claim (fixture/trusted-operation claim {:published-change-topics #{}})
                    cancel (fixture/trusted-operation cancel {:published-change-topics #{}})}
       :route-capabilities (fixture/custom-route-capabilities routes)
       :rendered-surfaces {:proof [:div "safe"]}}
      (fn [{:keys [assembly report]}]
        (is (app/valid? report) (pr-str (:errors report)))
        (when assembly
          (let [source {:application-assembly assembly
                        :semantic-route-handlers {claim-route h1 cancel-route h2}
                        :certified-operations
                        {claim {:executor claim-executor :current-source claim-source}
                         cancel {:executor cancel-executor :current-source cancel-source}}}
                issued (adoption/issue-checked-biff-host-module! source)
                modules [app/application-handler-module (:module issued)]
                system (binding [*modules* modules]
                         (app/start-biff-application!
                          assembly {} #'*modules*
                          [app/application-handler-module-id adoption/module-id]))]
            (binding [*case* {:source source :adoption (:handle issued)
                              :system system :calls calls :behavior behavior}]
              (run))))))))

;; One expensive genuine certified assembly, independent one-operation
;; coverage sources, and actual Biff startup are shared across the permanent
;; suite. Individual scenarios seed independent XTDB documents and issue
;; entirely fresh parallel mediator modules.
(use-fixtures :once
  (fn [run]
    (let [biff-fixture
          (first (:clojure.test/once-fixtures
                  (meta (the-ns
                         'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (biff-fixture
       (fn []
         (let [op (second-operation!)]
           (fixture/with-closed-application
             {:operations {cancel (fixture/trusted-operation
                                   cancel {:published-change-topics #{}})}}
             (fn [{:keys [assembly]}]
               (let [source (independent-second-source op assembly)
                     result (coverage/derive-observed-writer-coverage! source)]
                 (is (= :conditional-observed-operation-id-join (:status result)))
                 (binding [*second* (assoc op :writer-source source)]
                   (checked-application-fixture run)))))))))))

(deftest actual-distinct-operation-route-table-is-certified
  (is (= #{claim cancel} (set (keys (:certified-operations (:source *case*))))))
  (is (= #{claim-route cancel-route}
         (set (keys (:semantic-route-handlers (:source *case*))))))
  (is (= #{claim} (:independent-operation-ids
                  (coverage/derive-observed-writer-coverage! wc/*source*))))
  (is (= #{cancel} (:independent-operation-ids
                   (coverage/derive-observed-writer-coverage!
                    (:writer-source *second*)))))
  (is (not= wc/*source* (:writer-source *second*))))

(deftest two-certified-operations-commit-with-distinct-writer-bindings
  (use-case
   (fn [c final]
     (is (= 2 (:certified-literal-route-count
               (parallel/require-current-isolated-parallel-biff-request-module!
                (get-in c [:issued :handle]) (:source c) final))))
     (is (= 200 (:status (call! final :claim))))
     (is (= 200 (:status (call! final :cancel))))
     (is (= 1 (rev (:claim-before c))))
     (is (= 1 (rev (:cancel-before c))))
     (is (= 2 (count @(:source-calls c)))))))

(deftest two-distinct-operations-truly-overlap
  (let [entered (CountDownLatch. 2)
        release (CountDownLatch. 1)
        running (atom 0)
        peak (atom 0)]
    (reset! (:behavior *case*)
            (fn [request]
              (let [n (swap! running inc)]
                (swap! peak max n)
                (try
                  (.countDown entered)
                  (when-not (.await release 25 TimeUnit/SECONDS)
                    (throw (ex-info "fixture timed out" {:reason :fixture-timeout})))
                  (invoke! request)
                  (finally (swap! running dec))))))
    (try
      (use-case
       (fn [c final]
         (let [a (future (call! final :claim))
               b (future (call! final :cancel))]
           (try
             (is (true? (.await entered 80 TimeUnit/SECONDS)))
             (finally (.countDown release)))
           (is (= 200 (:status (deref a 95000 {:status :timed-out}))))
           (is (= 200 (:status (deref b 95000 {:status :timed-out}))))
           (is (= 2 @peak))
           (is (= 1 (rev (:claim-before c))))
           (is (= 1 (rev (:cancel-before c)))))))
      (finally
        (reset! (:behavior *case*) invoke!)
        (.countDown release)))))

(deftest different-operation-source-omitted-is-rejected
  (let [c (build-case)
        base [(:adoption c) (:source c)
              {:per-operation-writer-sources {claim wc/*source*}}
              writer-bindings wc/*node* (fn [request] (source-for-request c request))
              :revision]
        m (apply parallel/issue-isolated-parallel-biff-request-module! base)]
    ((:biff.core/init (:module m)) nil)
    (is (= :requires-independent-operation-writer-coverage-sources
           (reason #((:biff.core/start (:module m)) (:system c)))))))

(deftest swapped-independent-operation-sources-fail-closed
  (let [c (build-case)
        sources {:per-operation-writer-sources
                 {claim (:writer-source *second*) cancel wc/*source*}}
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption c) (:source c) sources writer-bindings wc/*node*
                (fn [request] (source-for-request c request)) :revision)
        m (:module issued)]
    ((:biff.core/init m) nil)
    (let [final ((:biff.core/start m) (:system c))]
      (is (not= :not-rejected (reason #(call! final :claim))))
      (is (= 0 (rev (:claim-before c)))))))

(deftest route-specific-writer-path-cannot-be-confused
  (reset! (:behavior *case*) (fn [request]
                               (parallel/invoke-current-request-writer!
                                (if (= (expected-path request) claim-path)
                                  cancel-path claim-path))))
  (try
    (use-case
     (fn [c final]
       (is (= :no-matching-active-thread-bound-request-writer
              (reason #(call! final :claim))))
       (is (= 0 (rev (:claim-before c))))))
    (finally (reset! (:behavior *case*) invoke!))))

(deftest unrelated-method-and-uri-never-issue-writer
  (use-case
   (fn [c final]
     (let [handler (get final app/biff-ring-handler-key)]
       (doseq [request [{:request-method :get :uri "/operations/example/claim"}
                        {:request-method :post :uri "/operations/example/missing"}
                        {:request-method :post :uri "/operations/example/cancel/"}
                        {} nil]]
         (is (= :unmatched-parallel-request-route
                (reason #(handler request)))))
       (is (empty? @(:source-calls c)))
       (is (= 0 (rev (:claim-before c))))
       (is (= 0 (rev (:cancel-before c))))))))

(deftest duplicate-paths-and-partial-static-bindings-fail-at-start
  (let [c (build-case)]
    (doseq [bindings [{} {claim-route claim-path}
                     {claim-route claim-path cancel-route claim-path}
                     {claim-route :host/other cancel-route nil}
                     {claim-route claim-path cancel-route cancel-path :extra/route :host/extra}]]
      (let [issued (parallel/issue-isolated-parallel-biff-request-module!
                    (:adoption c) (:source c)
                    {:per-operation-writer-sources
                     {claim wc/*source* cancel (:writer-source *second*)}}
                    bindings wc/*node* (fn [request] (source-for-request c request))
                    :revision)
            module (:module issued)]
        ((:biff.core/init module) nil)
        (is (= :requires-one-certified-literal-route-and-writer-binding
               (reason #((:biff.core/start module) (:system c)))))))))

(deftest passing-local-multiroute-report-never-claims-deployment-authority
  (use-case
   (fn [c final]
     (let [report (parallel/require-current-isolated-parallel-biff-request-module!
                   (get-in c [:issued :handle]) (:source c) final)
           printed (pr-str report)]
       (is (= :independent-per-request (:registered-writer-context report)))
       (is (= 2 (:certified-literal-route-count report)))
       (is (= :not-observed (:unselected-route-invocations report)))
       (is (= :not-established (:other-writer-closure report)))
       (is (= :not-established (:actual-http-server-adoption report)))
       (is (= :none (:transaction-authority report)))
       (is (= :not-issued (:certificate report)))
       (is (not (str/includes? printed "private-v1549-sentinel")))
       (is (not-any? #(contains? report %)
                     [:node :tx-key :tx-ops :writer :writer-handle :handler :request]))))))

(deftest singleton-writer-coverage-source-cannot-cover-two-operations
  (let [c (build-case)
        issuance (parallel/issue-isolated-parallel-biff-request-module!
                  (:adoption c) (:source c) wc/*source*
                  writer-bindings wc/*node*
                  (fn [request] (source-for-request c request)) :revision)
        module (:module issuance)]
    ((:biff.core/init module) nil)
    (is (= :requires-independent-operation-writer-coverage-sources
           (reason #((:biff.core/start module) (:system c)))))))

(deftest extra-independent-source-operation-is-rejected
  (let [c (build-case)
        sources {:per-operation-writer-sources
                 {claim wc/*source* cancel (:writer-source *second*)
                  :example/extra wc/*source*}}
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption c) (:source c) sources writer-bindings wc/*node*
                (fn [request] (source-for-request c request)) :revision)
        module (:module issued)]
    ((:biff.core/init module) nil)
    (is (= :requires-independent-operation-writer-coverage-sources
           (reason #((:biff.core/start module) (:system c)))))))

(deftest malformed-per-operation-source-map-is-rejected
  (let [c (build-case)]
    (doseq [bad [nil [] {:per-operation-writer-sources []}
                 {:per-operation-writer-sources {claim wc/*source* cancel nil}}
                 {:per-operation-writer-sources {claim wc/*source*
                                                 cancel (:writer-source *second*)}
                  :extra true}]]
      (let [why (reason #(let [issued
                               (parallel/issue-isolated-parallel-biff-request-module!
                                (:adoption c) (:source c) bad writer-bindings
                                wc/*node* (fn [request] (source-for-request c request))
                                :revision)
                               module (:module issued)]
                           ((:biff.core/init module) nil)
                           ((:biff.core/start module) (:system c))))]
        (is (contains? #{:invalid-parallel-mediation-issuance
                         :requires-independent-operation-writer-coverage-sources}
                       why))))))

(deftest unrelated-operation-source-is-not-silently-promoted-to-valid-coverage
  (let [c (build-case)
        sources {:per-operation-writer-sources
                 {claim wc/*source* cancel wc/*source*}}
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption c) (:source c) sources writer-bindings wc/*node*
                (fn [request] (source-for-request c request)) :revision)
        module (:module issued)]
    ((:biff.core/init module) nil)
    (let [final ((:biff.core/start module) (:system c))]
      (is (not= :not-rejected (reason #(call! final :cancel))))
      (is (= 0 (rev (:cancel-before c)))))))

(deftest stale-second-invocation-remains-guarded
  (use-case
   (fn [c final]
     (is (= 200 (:status (call! final :claim))))
     (is (= 1 (rev (:claim-before c))))
     ;; The test source builder intentionally reuses the stale original
     ;; before-image; this is not a reusable transaction capability.
     (is (= :source-before-disagrees-with-external-read
            (reason #(call! final :claim))))
     (is (= 1 (rev (:claim-before c)))))))

(deftest independent-writer-path-is-not-callable-outside-request
  (is (= :no-matching-active-thread-bound-request-writer
         (reason #(parallel/invoke-current-request-writer! claim-path))))
  (is (= :no-matching-active-thread-bound-request-writer
         (reason #(parallel/invoke-current-request-writer! cancel-path)))))

(deftest asynchronous-cancel-writer-invocation-is-rejected
  (let [async-reason (atom nil)]
    (reset! (:behavior *case*)
            (fn [request]
              (reset! async-reason
                      @(future (reason #(parallel/invoke-current-request-writer!
                                          (expected-path request)))))))
    (try
      (use-case
       (fn [c final]
         (is (= :missing-extra-or-foreign-writer-invocation
                (reason #(call! final :cancel))))
         (is (= :no-matching-active-thread-bound-request-writer @async-reason))
         (is (= 0 (rev (:cancel-before c))))))
      (finally (reset! (:behavior *case*) invoke!)))))

(deftest omitted-cancel-writer-does-not-gain-observation-from-ring-success
  (reset! (:behavior *case*) (fn [_] nil))
  (try
    (use-case
     (fn [c final]
       (is (= :missing-extra-or-foreign-writer-invocation
              (reason #(call! final :cancel))))
       (is (= 0 (rev (:cancel-before c))))))
    (finally (reset! (:behavior *case*) invoke!))))

(deftest exception-before-request-local-write-preserves-prestate
  (reset! (:behavior *case*)
          (fn [_] (throw (ex-info "private before" {:reason :before-write}))))
  (try
    (use-case
     (fn [c final]
       (is (= :before-write (reason #(call! final :cancel))))
       (is (= 0 (rev (:cancel-before c))))))
    (finally (reset! (:behavior *case*) invoke!))))

(deftest exception-after-request-local-write-is-not-a-rollback
  (reset! (:behavior *case*)
          (fn [request]
            (invoke! request)
            (throw (ex-info "private after" {:reason :after-write}))))
  (try
    (use-case
     (fn [c final]
       (is (= :after-write (reason #(call! final :cancel))))
       (is (= 1 (rev (:cancel-before c))))))
    (finally (reset! (:behavior *case*) invoke!))))

(deftest final-handler-system-identity-cannot-be-copied
  (use-case
   (fn [c final]
     (is (= :parallel-final-handler-identity-mismatch
            (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                      (get-in c [:issued :handle]) (:source c) (into {} final)))))
     (is (= 0 (rev (:claim-before c)))))))

(deftest foreign-issued-module-handle-is-not-adopted
  (use-case
   (fn [c final]
     (is (= :unissued-parallel-module
            (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                      (Object.) (:source c) final))))
     (is (= 0 (rev (:cancel-before c)))))))

(deftest changed-final-handler-slot-fails-identity-check
  (use-case
   (fn [c final]
     (is (= :parallel-final-handler-identity-mismatch
            (reason #(parallel/require-current-isolated-parallel-biff-request-module!
                      (get-in c [:issued :handle]) (:source c)
                      (assoc final app/biff-ring-handler-key
                             (fn [_] {:status 200}))))))
     (is (= 0 (rev (:claim-before c)))))))

(deftest two-operation-module-is-single-start
  (let [c (build-case)
        module (get-in c [:issued :module])]
    ((:biff.core/init module) nil)
    (is (map? ((:biff.core/start module) (:system c))))
    (is (= :repeated-parallel-module-start
           (reason #((:biff.core/start module) (:system c)))))))

(deftest selecting-a-route-does-not-execute-other-operation
  (use-case
   (fn [c final]
     (is (= 200 (:status (call! final :cancel))))
     (is (= 0 (rev (:claim-before c))))
     (is (= 1 (rev (:cancel-before c))))
     (is (= ["/operations/example/cancel"] @(:source-calls c))))))

(deftest physical-command-semantics-are-not-proved-by-operation-label
  ;; Deliberately provide the other document's physical command while the
  ;; selected certified route advertises the claim operation. The checked
  ;; callback/route identity cannot authenticate the host's source builder.
  ;; A positive Ring response here is a documented OPEN correspondence gap.
  (let [c (build-case)
        issued (parallel/issue-isolated-parallel-biff-request-module!
                (:adoption c) (:source c)
                {:per-operation-writer-sources
                 {claim wc/*source* cancel (:writer-source *second*)}}
                writer-bindings wc/*node*
                (fn [_] (wc/source (:cancel-before c))) :revision)
        module (:module issued)]
    ((:biff.core/init module) nil)
    (let [final ((:biff.core/start module) (:system c))]
      (is (= 200 (:status (call! final :claim))))
      (is (= 0 (rev (:claim-before c))))
      (is (= 1 (rev (:cancel-before c)))))))
