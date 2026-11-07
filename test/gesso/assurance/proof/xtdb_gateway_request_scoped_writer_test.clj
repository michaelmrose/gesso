(ns gesso.assurance.proof.xtdb-gateway-request-scoped-writer-test
  "v1560: permanent adversarial real-XTDB and Biff tests for one-request
   scoped, locally issued monotonic guarded writer capabilities. Request
   scope and process-local callback identity do not prove deployed request
   authentication, all-writer closure, operation refinement or causality."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as biff-fixture]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution :as monotonic]
            [gesso.model.command :as command]
            [xtdb.api :as xt])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))
(use-fixtures :once
  (fn [run]
    (let [f (first (:clojure.test/once-fixtures
                    (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (f run))))
(defn- issued [b]
  (coverage/issue-request-scoped-monotonic-writer!
   wc/path wc/op wc/*node* (wc/source b) :revision))
(defn- failure [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :untyped))))
(deftest scope-protects-pre-activation
  (let [b (wc/seeded) i (issued b)]
    (is (= :available ((:request-scope-status! i))))
    (is (= :writer-outside-owning-request-scope (failure (:writer i))))
    (is (= :available ((:request-scope-status! i))))
    (is (= b (wc/physical b)))))
(deftest active-scope-executes-once-and-closes
  (let [b (wc/seeded) i (issued b)]
    (is (map? ((:with-request! i) (:writer i))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= 1 (:revision (wc/physical b))))
    (is (= :writer-outside-owning-request-scope (failure (:writer i))))))
(deftest scope-does-not-reopen
  (let [b (wc/seeded) i (issued b)]
    (is (= :ok ((:with-request! i) (fn [] :ok))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= :request-scope-already-used-or-active
           (failure #((:with-request! i) (:writer i)))))
    (is (= b (wc/physical b)))))
(deftest nested-scope-rejected-without-consuming-original
  (let [b (wc/seeded) i (issued b)]
    (is (map? ((:with-request! i)
               (fn []
                 (is (= :active ((:request-scope-status! i))))
                 (is (= :request-scope-already-used-or-active
                        (failure #((:with-request! i) (:writer i)))))
                 ((:writer i))))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= 1 (:revision (wc/physical b))))))
(deftest throw-ends-scope-without-writing
  (let [b (wc/seeded) i (issued b)]
    (is (= :expected
           (failure #((:with-request! i)
                      (fn [] (throw (ex-info "private-data" {:reason :expected})))))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= b (wc/physical b)))))
(deftest bad-dispatch-function-does-not-burn-scope
  (let [b (wc/seeded) i (issued b)]
    (is (= :invalid-request-scope-dispatch
           (failure #((:with-request! i) nil))))
    (is (= :available ((:request-scope-status! i))))
    (is (map? ((:with-request! i) (:writer i))))
    (is (= 1 (:revision (wc/physical b))))))
(deftest writer-argument-rejected-while-scope-remains-active
  (let [b (wc/seeded) i (issued b)]
    ((:with-request! i)
     (fn []
       (is (= :registered-writer-requires-zero-arguments
              (failure #((:writer i) :bad))))
       (is (map? ((:writer i))))))
    (is (= 1 (:revision (wc/physical b))))))
(deftest repeat-write-in-same-request-is-not-permitted
  (let [b (wc/seeded) i (issued b)]
    ((:with-request! i)
     (fn []
       ((:writer i))
       (is (= :one-shot-already-attempted (failure (:writer i))))))
    (is (= 1 (:revision (wc/physical b))))))
(deftest callback-on-foreign-thread-rejected
  (let [b (wc/seeded) i (issued b)]
    ((:with-request! i)
     (fn []
       (is (= :writer-outside-owning-request-scope
              @(future (failure (:writer i)))))))
    (is (= b (wc/physical b)))))
(deftest callback-after-completed-request-on-foreign-thread-rejected
  (let [b (wc/seeded) i (issued b)
        gate (promise)
        result (future @gate (failure (:writer i)))]
    (is (= :ok ((:with-request! i) (fn [] :ok))))
    (deliver gate true)
    (is (= :writer-outside-owning-request-scope @result))
    (is (= b (wc/physical b)))))
(deftest host-registered-identity-still-valid
  (let [b (wc/seeded) i (issued b)
        host (atom {wc/path (wc/registered (:writer i))})
        args [(wc/inv host) {wc/path (:handle i)}]]
    (is (:all-currently-surveyed-writers-locally-bound?
         (apply coverage/require-registered-writer-coverage! args)))
    (is (not (:all-currently-surveyed-writers-locally-bound?
              (coverage/inspect-registered-writer-coverage!
               (wc/inv (atom {wc/path
                              (wc/registered (fn [] ((:writer i))))}))
               {wc/path (:handle i)}))))
    (is (= b (wc/physical b)))
    ((:with-request! i) (:writer i))
    (is (:all-currently-surveyed-writers-locally-bound?
         (apply coverage/require-registered-writer-coverage! args)))))
(deftest request-scoped-issuances-are-distinct-and-sequential
  (let [b (wc/seeded) a (issued b)
        host (atom {wc/path (wc/registered (:writer a))})]
    ((:with-request! a) (:writer a))
    (let [updated (wc/physical b)
          next (-> updated (assoc :revision 2 :updated-at "second" :owner "reassigned"))
          source {:plan {:commands [(command/update-command wc/table :claim
                                                             updated next wc/version)]
                         :emit false}
                  :protected-documents [{:target [wc/table (:xt/id b)] :fields :all}]}
          later (coverage/issue-request-scoped-monotonic-writer!
                 wc/path wc/op wc/*node* source :revision)]
      (is (not (identical? (:writer a) (:writer later))))
      (is (not (identical? (:handle a) (:handle later))))
      (reset! host {wc/path (wc/registered (:writer later))})
      (is (:all-currently-surveyed-writers-locally-bound?
           (coverage/require-registered-writer-coverage!
            (wc/inv host) {wc/path (:handle later)})))
      (is (= :writer-outside-owning-request-scope (failure (:writer a))))
      (is (map? ((:with-request! later) (:writer later))))
      (is (= 2 (:revision (wc/physical b)))))))
(deftest conflicting-transaction-closes-scope
  (let [b (wc/seeded) i (issued b)]
    (xt/execute-tx wc/*node* [[:put-docs wc/table (assoc b :owner "outsider")]])
    (is (not= :not-rejected
              (try ((:with-request! i) (:writer i)) :not-rejected
                   (catch Throwable _ :conflict))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= :writer-outside-owning-request-scope (failure (:writer i))))))
(deftest post-write-throw-closes-scope-and-does-not-rewind-write
  (let [b (wc/seeded) i (issued b)]
    (is (= :after-write
           (failure #((:with-request! i)
                      (fn [] ((:writer i))
                        (throw (ex-info "after-write" {:reason :after-write})))))))
    (is (= :closed ((:request-scope-status! i))))
    (is (= 1 (:revision (wc/physical b))))))
(deftest malformed-path-rejected-before-issuance
  (let [b (wc/seeded)]
    (is (= :invalid-path-or-operation-id
           (failure #(coverage/issue-request-scoped-monotonic-writer!
                      :notqualified wc/op wc/*node* (wc/source b) :revision))))
    (is (= b (wc/physical b)))))
(deftest substituted-preparation-seam-fails-closed
  (let [b (wc/seeded)]
    (with-redefs [monotonic/prepare-monotonic-revision-one-shot! (fn [& _] :fake)]
      (is (= :substituted-issuer-or-inventory
             (failure #(issued b)))))
    (is (= b (wc/physical b)))))


;; Additional permanent cases: inspect callback lifetime and genuine Ring/Biff
;; adoption without asserting that test-local dispatch is a deployed request.
(defn- issue-with [b path operation]
  (coverage/issue-request-scoped-monotonic-writer!
   path operation wc/*node* (wc/source b) :revision))

(defn- biff-case [behavior]
  ;; The genuine v1554 fixture builds a checked application, starts the Biff
  ;; module and retains the actual installed HTML-wrapped handler. Only the
  ;; test's issuer helper is switched to v1559's request-scoped issuer;
  ;; neither the Biff observer nor the XTDB gateway is replaced.
  (let [issued! (atom nil)
        constructor (var-get (ns-resolve
                              'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
                              'context))
        c (with-redefs [wc/issue
                        (fn [before]
                          (let [issuance (issued before)]
                            (reset! issued! issuance)
                            issuance))]
            (constructor (fn [data] (behavior @issued! data))))]
    (assoc c :scope @issued!)))

(defn- biff-observe [c]
  (coverage/observe-checked-biff-host-registered-writer!
   (:adoption c) (:source c) (:system c) (:request c)
   (:inventory c) {wc/path (:writer-handle c)} wc/path))

(defn- biff-joined-observe [c]
  (assembly/observe-current-biff-registered-writer-assembly!
   (:adoption c) (:source c) (:system c) (:inventory c)
   {wc/path (:writer-handle c)} {wc/op wc/path} (:request c)))

(deftest scope-report-is-opaque-and-non-authoritative
  (let [b (wc/seeded) issuance (issued b)]
    (is (= #{:handle :writer :with-request! :request-scope-status!}
           (set (keys issuance))))
    (is (fn? (:writer issuance)))
    (is (fn? (:with-request! issuance)))
    (is (= :available ((:request-scope-status! issuance))))
    (is (not (str/includes? (pr-str ((:request-scope-status! issuance)))
                            "private-v1549-sentinel")))
    (is (= b (wc/physical b)))))

(deftest dispatch-return-value-is-preserved-without-writing
  (let [b (wc/seeded) issuance (issued b)
        response {:status 204 :body :no-write}]
    (is (= response ((:with-request! issuance) (fn [] response))))
    (is (= :closed ((:request-scope-status! issuance))))
    (is (= b (wc/physical b)))))

(deftest multiple-nonwriter-actions-within-one-scope-do-not-consume-submission
  (let [b (wc/seeded) issuance (issued b) calls (atom [])]
    ((:with-request! issuance)
     (fn []
       (swap! calls conj :before)
       (is (= :active ((:request-scope-status! issuance))))
       (swap! calls conj :during)
       (is (= 1 (:revision (do ((:writer issuance)) (wc/physical b)))))
       (swap! calls conj :after)))
    (is (= [:before :during :after] @calls))
    (is (= :closed ((:request-scope-status! issuance))))))

(deftest invalid-arity-does-not-permit-submission-after-close
  (let [b (wc/seeded) issuance (issued b)]
    ((:with-request! issuance)
     (fn [] (is (= :registered-writer-requires-zero-arguments
                   (failure #((:writer issuance) :unexpected))))))
    (is (= :writer-outside-owning-request-scope (failure (:writer issuance))))
    (is (= b (wc/physical b)))))

(deftest invalid-dispatch-values-do-not-spend-available-capability
  (let [b (wc/seeded) issuance (issued b)]
    (doseq [value [nil 1 :not-function {} [] "not-a-handler"]]
      (is (= :invalid-request-scope-dispatch
             (failure #((:with-request! issuance) value))))
      (is (= :available ((:request-scope-status! issuance)))))
    (is (map? ((:with-request! issuance) (:writer issuance))))
    (is (= 1 (:revision (wc/physical b))))))

(deftest exception-before-write-permanently-expires-capability
  (let [b (wc/seeded) issuance (issued b)]
    (is (= :intentional
           (failure #((:with-request! issuance)
                      (fn [] (throw (ex-info "private-v1560-exception"
                                             {:reason :intentional})))))))
    (is (= :closed ((:request-scope-status! issuance))))
    (is (= :request-scope-already-used-or-active
           (failure #((:with-request! issuance) (:writer issuance)))))
    (is (= b (wc/physical b)))))

(deftest exception-after-write-permanently-expires-without-rollback
  (let [b (wc/seeded) issuance (issued b)]
    (is (= :throw-after-commit
           (failure #((:with-request! issuance)
                      (fn [] ((:writer issuance))
                        (throw (ex-info "finished-write" {:reason :throw-after-commit})))))))
    (is (= :closed ((:request-scope-status! issuance))))
    (is (= 1 (:revision (wc/physical b))))
    (is (= :request-scope-already-used-or-active
           (failure #((:with-request! issuance) (:writer issuance)))))))

(deftest concurrent-owner-cannot-reactivate-active-scope
  (let [b (wc/seeded) issuance (issued b)
        entered (CountDownLatch. 1) release (CountDownLatch. 1)
        first (future ((:with-request! issuance)
                       (fn []
                         (.countDown entered)
                         (is (= :active ((:request-scope-status! issuance))))
                         (is (.await release 8 TimeUnit/SECONDS))
                         ((:writer issuance)))))]
    (try
      (is (.await entered 8 TimeUnit/SECONDS))
      (is (= :request-scope-already-used-or-active
             (failure #((:with-request! issuance) (fn [] :foreign-owner)))))
      (is (= :writer-outside-owning-request-scope (failure (:writer issuance))))
      (finally (.countDown release)))
    (is (map? (deref first 20000 :timed-out)))
    (is (= :closed ((:request-scope-status! issuance))))
    (is (= 1 (:revision (wc/physical b))))))

(deftest simultaneous-activation-has-exactly-one-owner
  (let [b (wc/seeded) issuance (issued b)
        start (CountDownLatch. 1)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        attempt (fn []
                  (.await start 8 TimeUnit/SECONDS)
                  (try
                    ((:with-request! issuance)
                     (fn [] (.countDown entered)
                       (.await release 8 TimeUnit/SECONDS) :won))
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e)))))
        fa (future (attempt)) fb (future (attempt))]
    (try
      (.countDown start)
      (is (.await entered 8 TimeUnit/SECONDS))
      (finally (.countDown release)))
    (is (= #{:won :request-scope-already-used-or-active}
           #{(deref fa 20000 :timed-out) (deref fb 20000 :timed-out)}))
    (is (= b (wc/physical b)))
    (is (= :closed ((:request-scope-status! issuance))))))

(deftest scope-does-not-propagate-to-futures-even-though-bindings-do
  (let [b (wc/seeded) issuance (issued b)]
    ((:with-request! issuance)
     (fn []
       (let [seen @(future
                     {:status ((:request-scope-status! issuance))
                      :reason (failure (:writer issuance))})]
         (is (= :active (:status seen)))
         (is (= :writer-outside-owning-request-scope (:reason seen))))))
    (is (= b (wc/physical b)))))

(deftest callback-delayed-until-after-closure-is-not-a-submission
  (let [b (wc/seeded) issuance (issued b)
        release (promise)
        delayed (future @release (failure (:writer issuance)))]
    (is (= :no-write ((:with-request! issuance) (fn [] :no-write))))
    (deliver release true)
    (is (= :writer-outside-owning-request-scope (deref delayed 20000 :timed-out)))
    (is (= b (wc/physical b)))))

(deftest registry-inspection-does-not-activate-a-request-scope
  (let [b (wc/seeded) issuance (issued b)
        registry (atom {wc/path (wc/registered (:writer issuance))})]
    (dotimes [_ 3]
      (is (true? (:all-currently-surveyed-writers-locally-bound?
                  (coverage/require-registered-writer-coverage!
                   (wc/inv registry) {wc/path (:handle issuance)}))))
      (is (= :available ((:request-scope-status! issuance)))))
    (is (= b (wc/physical b)))))

(deftest original-unscoped-issuer-retains-legacy-contract
  (let [b (wc/seeded) issuance (wc/issue b)]
    (is (= #{:handle :writer} (set (keys issuance))))
    (is (map? ((:writer issuance))))
    (is (= :one-shot-already-attempted (failure (:writer issuance))))
    (is (= 1 (:revision (wc/physical b))))))

(deftest distinct-issuances-are-not-cross-scoped
  (let [b (wc/seeded) a (issued b) foreign (issued b)]
    ((:with-request! a)
     (fn []
       (is (= :writer-outside-owning-request-scope (failure (:writer foreign))))
       (is (map? ((:writer a))))))
    (is (= :available ((:request-scope-status! foreign))))
    (is (= :closed ((:request-scope-status! a))))
    (is (= 1 (:revision (wc/physical b))))))

(deftest foreign-issuance-cannot-activate-expired-original
  (let [b (wc/seeded) a (issued b) foreign (issued b)]
    (is (= :ok ((:with-request! a) (fn [] :ok))))
    (is (= :ok ((:with-request! foreign) (fn [] :ok))))
    (is (= :writer-outside-owning-request-scope (failure (:writer a))))
    (is (= :request-scope-already-used-or-active
           (failure #((:with-request! a) (:writer a)))))
    (is (= b (wc/physical b)))))

(deftest invalid-qualified-paths-rejected-before-gateway-creation
  (let [b (wc/seeded)]
    (doseq [[path operation] [[nil wc/op] [wc/path nil]
                              [:plain wc/op] [wc/path :plain]
                              ["host/path" wc/op] [wc/path "example/claim"]]]
      (is (= :invalid-path-or-operation-id
             (failure #(issue-with b path operation)))))
    (is (= b (wc/physical b)))))

(deftest registered-wrapper-cannot-impersonate-exact-issued-callback
  (let [b (wc/seeded) issuance (issued b)
        registry (atom {wc/path (wc/registered (fn [] ((:writer issuance))))})
        report (coverage/inspect-registered-writer-coverage!
                (wc/inv registry) {wc/path (:handle issuance)})]
    (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))
    (is (= 0 (:matched-issued-monotonic-writer-count report)))
    (is (= :available ((:request-scope-status! issuance))))
    (is (= b (wc/physical b)))))

(deftest an-unknown-registry-handle-cannot-attest-scoped-writer
  (let [b (wc/seeded) issuance (issued b)
        registry (atom {wc/path (wc/registered (:writer issuance))})]
    (is (not (:all-currently-surveyed-writers-locally-bound?
              (coverage/inspect-registered-writer-coverage!
               (wc/inv registry) {wc/path (Object.)}))))
    (is (= :available ((:request-scope-status! issuance))))
    (is (= b (wc/physical b)))))

(deftest hidden-unregistered-direct-xtdb-writer-remains-unproven
  (let [b (wc/seeded) issuance (issued b)
        registry (atom {wc/path (wc/registered (:writer issuance))})]
    (is (:all-currently-surveyed-writers-locally-bound?
         (coverage/require-registered-writer-coverage!
          (wc/inv registry) {wc/path (:handle issuance)})))
    (xt/execute-tx wc/*node* [[:put-docs wc/table (assoc b :rogue true)]])
    (is (= :available ((:request-scope-status! issuance))))
    (is (true? (:rogue (wc/physical b))))
    (is (= :writer-outside-owning-request-scope (failure (:writer issuance))))))

(deftest biff-installed-handler-dispatches-with-owning-request-scope
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope) writer)))
        out (biff-observe c)]
    (is (= 200 (get-in out [:response :status])))
    (is (= :observed-checked-biff-handler-invoked-exact-registered-writer
           (get-in out [:report :status])))
    (is (= 1 @(:calls c)))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= :closed ((:request-scope-status! (:scope c)))))))

(deftest full-biff-assembly-joins-scoped-callback-for-one-request
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope) writer)))
        out (biff-joined-observe c)]
    (is (= :conditional-observed-route-invoked-joined-one-shot-writer
           (get-in out [:report :status])))
    (is (= :only-this-observed-request
           (get-in out [:report :invocation-coverage])))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= :closed ((:request-scope-status! (:scope c)))))))

(deftest biff-handler-outside-scope-does-not-submit
  (let [c (biff-case (fn [_ {:keys [writer]}] (writer)))]
    (is (not= :not-rejected (failure #(biff-observe c))))
    (is (= 0 (:revision (wc/physical (:before c)))))
    (is (= :available ((:request-scope-status! (:scope c)))))))

(deftest biff-handler-with-closed-empty-scope-does-not-submit
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope) (fn [] :no-write))
                       (writer)))]
    (is (not= :not-rejected (failure #(biff-observe c))))
    (is (= :closed ((:request-scope-status! (:scope c)))))
    (is (= 0 (:revision (wc/physical (:before c)))))))

(deftest biff-handler-foreign-thread-call-is-rejected-without-write
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope)
                        (fn [] @(future (failure writer))))))]
    (is (not= :not-rejected (failure #(biff-observe c))))
    (is (= :closed ((:request-scope-status! (:scope c)))))
    (is (= 0 (:revision (wc/physical (:before c)))))))

(deftest biff-handler-repeated-scoped-request-does-not-resubmit
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope) writer)))]
    (biff-observe c)
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (not= :not-rejected (failure #(biff-observe c))))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= :closed ((:request-scope-status! (:scope c)))))))

(deftest biff-handler-throws-after-scoped-write-and-does-not-roll-back
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope)
                        (fn [] (writer)
                          (throw (ex-info "private-v1560-after"
                                          {:reason :after-write}))))))]
    (is (not= :not-rejected (failure #(biff-observe c))))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= :closed ((:request-scope-status! (:scope c)))))))

(deftest biff-scoped-request-observer-cannot-detect-extra-direct-write
  (let [c (biff-case (fn [scope {:keys [writer before]}]
                       ((:with-request! scope)
                        (fn []
                          (xt/execute-tx wc/*node*
                                         [[:put-docs wc/table
                                           (assoc before :xt/id (str (random-uuid))
                                                 :unregistered true)]])
                          (writer)))))
        out (biff-joined-observe c)]
    (is (= :conditional-observed-route-invoked-joined-one-shot-writer
           (get-in out [:report :status])))
    (is (= :not-established
           (get-in out [:report :direct-and-external-xtdb-writer-closure])))
    (is (= 1 (:revision (wc/physical (:before c)))))
    (is (= :closed ((:request-scope-status! (:scope c)))))))

(deftest scoped-biff-report-remains-redacted
  (let [c (biff-case (fn [scope {:keys [writer]}]
                       ((:with-request! scope) writer)))
        out (biff-joined-observe c)
        report (:report out)]
    (is (not (str/includes? (pr-str report) "private-v1549-sentinel")))
    (is (not-any? #(contains? report %)
                  [:node :tx-key :tx-ops :before :after :handler :writer
                   :writer-handle :document-id :system :request]))
    (is (= :not-established (:abstract-operation-refinement report)))
    (is (= :not-issued (:certificate report)))
    (is (= :closed ((:request-scope-status! (:scope c)))))))
