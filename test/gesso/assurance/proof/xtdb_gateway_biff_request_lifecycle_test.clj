(ns gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle-test
  "v1562: permanent adversarial integration tests for v1561's opt-in,
   single-request Biff writer issuance, checked installation, observed
   dispatch, and CAS retirement. Real certified Biff/Ring and embedded XTDB
   fixtures. A clean local result neither authenticates deployed HTTP routing
   nor closes independently controlled XTDB writer paths."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as biff]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.live.application-preflight :as application]
            [gesso.model.command :as command]
            [xtdb.api :as xt]))

(use-fixtures :once
  (fn [run]
    ;; Reuse the same actual checked Biff host and embedded XTDB fixture as
    ;; the permanent v1554 tests, not a simulated handler or transaction.
    (let [f (first (:clojure.test/once-fixtures
                    (meta (the-ns
                           'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (f run))))

(defn- failure [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :untyped-ex-info))))

(defn- biff-context [behavior]
  (let [make (var-get (ns-resolve
                       'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
                       'context))
        ctx (make behavior)
        registry (:registry ctx)]
    ;; The checked host fixture begins with a legacy static callback. The
    ;; leased path is deliberately VACANT between controlled requests.
    (reset! registry {})
    (assoc ctx :controller
           (lifecycle/issue-checked-biff-request-writer-lifecycle!
            (:adoption ctx) (:source ctx) (:system ctx) (:inventory ctx)
            registry {} {wc/op wc/path}))))

(defn- gateway-source [ctx]
  (let [before (wc/physical (:before ctx))
        after (-> before
                  (assoc :revision (inc (:revision before))
                         :updated-at (str "next-" (inc (:revision before)))
                         :owner "reassigned"))]
    {:plan {:commands [(command/update-command wc/table :claim
                                               before after wc/version)]
            :emit false}
     :protected-documents [{:target [wc/table (:xt/id before)] :fields :all}]}))

(defn- execute!
  ([ctx] (execute! ctx (fn [_] (gateway-source ctx))))
  ([ctx source!]
   ((:run-request! (:controller ctx))
    (:request ctx) wc/*node* source! :revision)))

(defn- live-revision [ctx]
  (:revision (wc/physical (:before ctx))))

(defn- installed-writer [registry]
  (get-in @registry [wc/path :writer]))

(defn- require-nonclaims! [report]
  (is (= lifecycle/report-type (:type report)))
  (is (= lifecycle/report-version (:version report)))
  (is (= :conditional-one-request-issued-observed-and-retired (:status report)))
  (is (= :one-synchronous-process-local-biff-request (:scope report)))
  (doseq [k [:fresh-issued-writer? :one-shot-request-scope-closed?
             :exact-callback-installed-and-invoked?
             :registered-callback-retired?
             :host-registry-restored-by-compare-and-set?]]
    (is (true? (get report k)) (str "Required local result: " k)))
  (is (= :rejected (:concurrent-requests-through-this-controller report)))
  (is (= :this-request-only (:route-invocation-coverage report)))
  (doseq [k [:deployed-server-mediation :external-writer-closure
             :abstract-operation-correspondence
             :xtdb-commit-and-poststate-causality]]
    (is (= :not-established (get report k)) (str "Open assurance boundary: " k)))
  (doseq [[k v] [[:certificate :not-issued] [:admission :not-admitted]
                  [:transaction-authority :none] [:authority :none]]]
    (is (= v (get report k))))
  (doseq [k [:other-request-entry-points-must-use-this-controller
             :host-enumerator-and-actual-server-dispatch-exhaustiveness
             :independent-controllers-and-other-threads-can-change-registry
             :uncaught-registry-drift-must-be-reconciled-by-host
             :source-builder-operation-and-principal-authorization
             :direct-and-external-xtdb-writer-bypasses
             :callback-and-declared-operation-semantic-correspondence
             :xtdb-transaction-commit-and-physical-causality
             :application-wide-refinement-and-certification]]
    (is (contains? (:open-obligations report) k) (str "Open obligation: " k)))
  (is (not-any? #(contains? report %)
                [:source :node :before :after :document :tx-key :tx-ops
                 :registry :writer :handle :handler :request :response :system]))
  (is (not (str/includes? (pr-str report) "private-v1549-sentinel"))))

(deftest successful-first-request-retired-and-redacted
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))))
        result (execute! ctx)]
    (is (= 200 (get-in result [:response :status])))
    (require-nonclaims! (:report result))
    (is (= {} @(:registry ctx)))
    (is (false? ((:busy? (:controller ctx)))))
    (is (= 1 (live-revision ctx)))
    (is (= 1 @(:calls ctx)))))

(deftest successive-requests-reissue-not-reuse
  (let [seen (atom [])
        ctx (biff-context (fn [{:keys [registry]}]
                            (let [writer (installed-writer registry)]
                              (swap! seen conj writer)
                              (writer))))]
    (dotimes [n 3]
      (is (= 200 (get-in (execute! ctx) [:response :status])))
      (is (= (inc n) (live-revision ctx)))
      (is (empty? @(:registry ctx))))
    (is (= 3 (count (distinct @seen))))
    (doseq [old @seen]
      (is (= :writer-outside-owning-request-scope (failure old))))
    (is (= 3 @(:calls ctx)))))

(deftest missing-writer-retires-and-allows-new-request
  (let [omit? (atom true)
        ctx (biff-context (fn [{:keys [registry]}]
                            (when-not @omit? ((installed-writer registry)))))]
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(execute! ctx))))
    (is (empty? @(:registry ctx)))
    (is (= 0 (live-revision ctx)))
    (reset! omit? false)
    (is (= 200 (get-in (execute! ctx) [:response :status])))
    (is (= 1 (live-revision ctx)))))

(deftest handler-throw-before-write-cleans-up
  (let [ctx (biff-context (fn [_] (throw (ex-info "private-before" {:reason :before}))))]
    (is (= :before (failure #(execute! ctx))))
    (is (empty? @(:registry ctx)))
    (is (= 0 (live-revision ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest handler-throw-after-write-cleans-up-not-rollback
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))
                            (throw (ex-info "private-after" {:reason :after}))))]
    (is (= :after (failure #(execute! ctx))))
    (is (empty? @(:registry ctx)))
    (is (= 1 (live-revision ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest callback-called-twice-is-not-two-transactions
  (let [repeat-reason (atom nil)
        ctx (biff-context (fn [{:keys [registry]}]
                            (let [writer (installed-writer registry)]
                              (writer)
                              (reset! repeat-reason (failure writer)))))]
    (is (= :missing-extra-or-foreign-writer-invocation
           (failure #(execute! ctx))))
    (is (= :one-shot-already-attempted @repeat-reason))
    (is (= 1 (live-revision ctx)))
    (is (empty? @(:registry ctx)))))

(deftest asynchronous-callback-is-rejected-and-retired
  (let [async-reason (atom nil)
        ctx (biff-context (fn [{:keys [registry]}]
                            (reset! async-reason
                                    @(future (failure (installed-writer registry))))))]
    (is (not= :not-rejected (failure #(execute! ctx))))
    (is (= :writer-outside-owning-request-scope @async-reason))
    (is (= 0 (live-revision ctx)))
    (is (empty? @(:registry ctx)))))

(deftest captured-callback-cannot-run-after-request
  (let [captured (atom nil)
        ctx (biff-context (fn [{:keys [registry]}]
                            (reset! captured (installed-writer registry))
                            (@captured)))]
    (is (= 200 (get-in (execute! ctx) [:response :status])))
    (is (= :writer-outside-owning-request-scope (failure @captured)))
    (is (= 1 (live-revision ctx)))))

(deftest malformed-method-rejected-before-builder
  (let [ctx (biff-context (fn [_] nil))
        calls (atom 0)]
    (is (= :invalid-ring-request
           (failure #((:run-request! (:controller ctx))
                      (assoc (:request ctx) :request-method "post")
                      wc/*node* (fn [_] (swap! calls inc)) :revision))))
    (is (zero? @calls))
    (is (empty? @(:registry ctx)))))

(deftest wrong-route-rejected-before-builder
  (let [ctx (biff-context (fn [_] nil)) calls (atom 0)]
    (is (= :request-not-one-bound-certified-literal-route
           (failure #((:run-request! (:controller ctx))
                      (assoc (:request ctx) :uri "/wrong")
                      wc/*node* (fn [_] (swap! calls inc)) :revision))))
    (is (zero? @calls))
    (is (= 0 (live-revision ctx)))))

(deftest invalid-request-builder-is-rejected-before-lease
  (let [ctx (biff-context (fn [_] nil))]
    (is (= :invalid-request-source-builder-or-revision
           (failure #((:run-request! (:controller ctx))
                      (:request ctx) wc/*node* nil :revision))))
    (is (false? ((:busy? (:controller ctx)))))
    (is (empty? @(:registry ctx)))))

(deftest invalid-revision-field-is-rejected-before-lease
  (let [ctx (biff-context (fn [_] nil))]
    (is (= :invalid-request-source-builder-or-revision
           (failure #((:run-request! (:controller ctx))
                      (:request ctx) wc/*node* (fn [_] nil) "revision"))))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest source-builder-throw-ends-lease-without-install
  (let [ctx (biff-context (fn [_] nil))]
    (is (= :builder (failure #(execute! ctx
                                 (fn [_] (throw (ex-info "private-source" {:reason :builder})))))))
    (is (= 0 (live-revision ctx)))
    (is (empty? @(:registry ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest invalid-gateway-source-does-not-install
  (let [ctx (biff-context (fn [_] nil))]
    (is (not= :not-rejected (failure #(execute! ctx (fn [_] {:plan nil})))) )
    (is (= 0 (live-revision ctx)))
    (is (empty? @(:registry ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest incomplete-route-bindings-are-rejected-at-construction
  (let [ctx (biff-context (fn [_] nil))]
    (is (= :invalid-host-lifecycle-source
           (failure #(lifecycle/issue-checked-biff-request-writer-lifecycle!
                      (:adoption ctx) (:source ctx) (:system ctx)
                      (:inventory ctx) (:registry ctx) {} {}))))
    (is (= :incomplete-or-uncertified-source-route-bindings
           (failure #(lifecycle/issue-checked-biff-request-writer-lifecycle!
                      (:adoption ctx) (:source ctx) (:system ctx)
                      (:inventory ctx) (:registry ctx) {}
                      {:example/not-installed wc/path}))))))

(deftest nonatom-or-invalid-inventory-rejected-at-construction
  (let [ctx (biff-context (fn [_] nil))]
    (is (= :invalid-host-lifecycle-source
           (failure #(lifecycle/issue-checked-biff-request-writer-lifecycle!
                      (:adoption ctx) (:source ctx) (:system ctx)
                      (:inventory ctx) {} {} {wc/op wc/path}))))
    (is (= :invalid-host-lifecycle-source
           (failure #(lifecycle/issue-checked-biff-request-writer-lifecycle!
                      (:adoption ctx) (:source ctx) (:system ctx)
                      [] (:registry ctx) {} {wc/op wc/path}))))))

(deftest copied-host-inventory-rejected
  (let [ctx (biff-context (fn [_] nil))
        fake (assoc (:inventory ctx) :enumerate-write-paths!
                    (fn [] (into (sorted-map) @(:registry ctx))))]
    (is (= :enumerator-not-current-registry-object
           (failure #(lifecycle/issue-checked-biff-request-writer-lifecycle!
                      (:adoption ctx) (:source ctx) (:system ctx)
                      fake (:registry ctx) {} {wc/op wc/path}))))))

(deftest selected-path-not-vacant-rejected
  (let [ctx (biff-context (fn [_] nil))]
    (reset! (:registry ctx) {wc/path {:kind :raw-xtdb :operation-id nil
                                     :writer (fn [] nil)}})
    (is (= :selected-writer-not-vacant-or-unexpected-host-path
           (failure #(execute! ctx))))
    (is (= 0 (live-revision ctx)))))

(deftest unexpected-extra-writer-rejected
  (let [ctx (biff-context (fn [_] nil))]
    (reset! (:registry ctx) {:host/outside {:kind :raw-xtdb :operation-id nil
                                           :writer (fn [] nil)}})
    (is (= :selected-writer-not-vacant-or-unexpected-host-path
           (failure #(execute! ctx))))
    (is (= 0 (live-revision ctx)))))

(deftest registry-changes-during-source-acquisition-rejected
  (let [ctx (biff-context (fn [_] nil))
        foreign {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)}]
    (is (= :host-registry-changed-during-source-acquisition
           (failure #(execute! ctx
                              (fn [_]
                                (reset! (:registry ctx) {:host/foreign foreign})
                                (gateway-source ctx))))))
    (is (= {:host/foreign foreign} @(:registry ctx)))
    (is (= 0 (live-revision ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest mutation-during-handler-cannot-be-clobbered-on-cleanup
  (let [foreign {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)}
        ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))
                            (reset! registry {:host/foreign foreign})))]
    (is (= :host-registration-changed-cleanup-not-overwritten
           (failure #(execute! ctx))))
    (is (= {:host/foreign foreign} @(:registry ctx)))
    (is (= 1 (live-revision ctx)))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest substituted-callback-during-dispatch-rejected-and-not-overwritten
  (let [replacement {:kind :issued-operation :operation-id wc/op
                     :writer (fn [] :fake)}
        ctx (biff-context (fn [{:keys [registry]}]
                            (reset! registry {wc/path replacement})))]
    (is (= :host-registration-changed-cleanup-not-overwritten
           (failure #(execute! ctx))))
    (is (= {wc/path replacement} @(:registry ctx)))
    (is (= 0 (live-revision ctx)))))

(deftest concurrent-entry-through-one-controller-rejected
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))))
        entered (promise) release (promise)
        worker (future
                 (execute! ctx (fn [_] (deliver entered true)
                                 (deref release 30000 :timed-out)
                                 (gateway-source ctx))))]
    (try
      (is (= true (deref entered 30000 :timed-out)))
      (is (true? ((:busy? (:controller ctx)))))
      (is (= :concurrent-host-request-lifecycle (failure #(execute! ctx))))
      (is (= 0 (live-revision ctx)))
      (finally (deliver release true)))
    (is (= 200 (get-in (deref worker 45000 :timed-out) [:response :status])))
    (is (false? ((:busy? (:controller ctx)))))
    (is (= 1 (live-revision ctx)))))

(deftest two-controllers-cannot-use-same-installed-path-simultaneously
  (let [entered (promise) release (promise)
        ctx (biff-context (fn [{:keys [registry]}]
                            (deliver entered true)
                            (deref release 30000 :timed-out)
                            ((installed-writer registry))))
        other (lifecycle/issue-checked-biff-request-writer-lifecycle!
               (:adoption ctx) (:source ctx) (:system ctx)
               (:inventory ctx) (:registry ctx) {} {wc/op wc/path})
        worker (future (execute! ctx))]
    (try
      (is (= true (deref entered 30000 :timed-out)))
      (is (= :selected-writer-not-vacant-or-unexpected-host-path
             (failure #((:run-request! other)
                        (:request ctx) wc/*node* (fn [_] (gateway-source ctx))
                        :revision))))
      (finally (deliver release true)))
    (is (= 200 (get-in (deref worker 45000 :timed-out) [:response :status])))
    (is (empty? @(:registry ctx)))
    (is (= 1 (live-revision ctx)))))

(deftest stale-prestate-writer-conflict-cleans-up
  (let [ctx (biff-context (fn [{:keys [registry before]}]
                            (xt/execute-tx wc/*node*
                                           [[:put-docs wc/table (assoc before :owner "outsider")]])
                            ((installed-writer registry))))]
    ;; The actual XTDB assertion conflict is not required to be an ExceptionInfo.
    (is (not= :returned
              (try (execute! ctx) :returned (catch Throwable _ :failed))))
    (is (empty? @(:registry ctx)))
    (is (= "outsider" (:owner (wc/physical (:before ctx)))))
    (is (false? ((:busy? (:controller ctx)))))))

(deftest independent-direct-write-can-accompany-clean-observation
  (let [outsider-id (str (random-uuid))
        ctx (biff-context (fn [{:keys [registry before]}]
                            (xt/execute-tx wc/*node*
                                           [[:put-docs wc/table
                                             (assoc before :xt/id outsider-id
                                                    :owner "direct-only")]])
                            ((installed-writer registry))))
        response (execute! ctx)]
    (is (= :conditional-one-request-issued-observed-and-retired
           (get-in response [:report :status])))
    (is (= 1 (live-revision ctx)))
    (is (= "direct-only"
           (:owner (first (xt/q wc/*node*
                                ["SELECT * FROM gesso_v1549_coverage WHERE _id = ?"
                                 outsider-id])))))
    (is (= :not-established
           (get-in response [:report :external-writer-closure])))))

(deftest changing-checker-seams-rejected-without-writing
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))))]
    (with-redefs [writers/issue-request-scoped-monotonic-writer! (fn [& _] nil)]
      (is (= :substituted-host-issuer-or-observer (failure #(execute! ctx)))))
    (with-redefs [assembly/observe-current-biff-registered-writer-assembly!
                  (fn [& _] nil)]
      (is (= :substituted-host-issuer-or-observer (failure #(execute! ctx)))))
    (with-redefs [adoption/require-current-biff-host-adoption! (fn [& _] nil)]
      (is (= :substituted-host-issuer-or-observer (failure #(execute! ctx)))))
    (with-redefs [application/operation-summary (fn [& _] nil)]
      (is (= :substituted-host-issuer-or-observer (failure #(execute! ctx)))))
    (is (= 0 (live-revision ctx)))
    (is (empty? @(:registry ctx)))))

(deftest failed-concurrent-entry-does-not-exhaust-controller
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))))
        entered (promise) release (promise)
        worker (future (execute! ctx (fn [_] (deliver entered true)
                                         (deref release 30000 :timed-out)
                                         (gateway-source ctx))))]
    (try
      (is (= true (deref entered 30000 :timed-out)))
      (is (= :concurrent-host-request-lifecycle (failure #(execute! ctx))))
      (finally (deliver release true)))
    (is (= 200 (get-in (deref worker 45000 :timed-out) [:response :status])))
    (is (= 200 (get-in (execute! ctx) [:response :status])))
    (is (= 2 (live-revision ctx)))
    (is (empty? @(:registry ctx)))))

(deftest registry-after-handler-exception-can-be-used-again
  (let [fail? (atom true)
        ctx (biff-context (fn [{:keys [registry]}]
                            (when @fail? (throw (ex-info "private" {:reason :fail})))
                            ((installed-writer registry))))]
    (is (= :fail (failure #(execute! ctx))))
    (is (empty? @(:registry ctx)))
    (reset! fail? false)
    (is (= 200 (get-in (execute! ctx) [:response :status])))
    (is (= 1 (live-revision ctx)))))

(deftest no-positive-result-from-registry-cleanup-conflict
  (let [foreign {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)}
        ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))
                            (reset! registry {:host/foreign foreign})))]
    (is (not= :returned
              (try (execute! ctx) :returned (catch Throwable _ :failed))))
    (is (= 1 (live-revision ctx)))
    (is (= {:host/foreign foreign} @(:registry ctx)))))

(deftest report-contains-no-source-request-or-capability-data
  (let [ctx (biff-context (fn [{:keys [registry]}]
                            ((installed-writer registry))))
        report (:report (execute! ctx))
        printed (pr-str report)]
    (is (not (str/includes? printed (str (:xt/id (:before ctx))))))
    (is (not (str/includes? printed (str (:request-id (:request ctx))))))
    (is (not (str/includes? printed "private-v1549-sentinel")))
    (is (not (str/includes? printed "reassigned")))
    (is (not-any? #(contains? report %) [:writer :callback :source :handle
                                        :node :request :system :registry]))))
