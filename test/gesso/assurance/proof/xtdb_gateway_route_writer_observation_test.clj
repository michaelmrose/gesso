(ns gesso.assurance.proof.xtdb-gateway-route-writer-observation-test
  "v1552: permanent adversarial tests for v1551's genuine Ring route ->
   exact locally issued monotonic XTDB writer invocation observation.

   These tests use actual checked closed-application Ring installation and
   embedded XTDB. An observed synchronous callback invocation is not proof of
   exclusive writer coverage, authenticated deployment, committed transaction
   effects, or abstract operation correspondence. Rejection is POST-dispatch
   and cannot undo any direct or guarded write already performed."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.assurance.proof.installed-route-provenance-test :as rt]
            [gesso.assurance.proof.installed-route-provenance :as installed]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [xtdb.api :as xt]))

(def ^:private operation :example/claim)
(def ^:private writer-path :host/claim)
(def ^:private private-sentinel "private-v1549-sentinel")

;; Both original fixtures are real. The ring fixture supplies a closed
;; application and certified executor; the writer fixture supplies XTDB and
;; the independently derived physical-writer inventory source.
(use-fixtures :once
  (fn [run]
    (let [writer-fixture
          (first (:clojure.test/once-fixtures
                  (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-writer-coverage-test))))
          route-fixture
          (first (:clojure.test/once-fixtures
                  (meta (the-ns 'gesso.assurance.proof.installed-route-provenance-test))))]
      (writer-fixture #(route-fixture run)))))

(defn- certified-invocation []
  ((var-get (ns-resolve 'gesso.model.certified-plan-test 'invocation)) :reconcile))

(defn- failure [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (:failure (ex-data e)) :foreign-exception))))

(defn- context
  "Construct a fresh, installed checked route with a real one-shot writer.
   `behavior` is called during the actual request->input! conversion."
  [behavior]
  (let [before (wc/seeded)
        issued (wc/issue before)
        writer (:writer issued)
        host (atom {writer-path (wc/registered writer)})
        route-source (:source rt/*case*)
        executor (:executor rt/*case*)
        plan-source (:plan-source rt/*case*)
        input-calls (atom 0)
        state {:before before :writer writer :host host :input-calls input-calls}
        input! (fn [request]
                 (swap! input-calls inc)
                 (behavior (assoc state :request request))
                 (certified-invocation))
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source plan-source
                  :request->input! input!
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})
        route-source (assoc-in route-source [:semantic-route-handlers operation]
                               handler)
        router (installed/install-checked-ring-router! route-source)]
    (assoc state
           :handle (:handle issued)
           :route-source route-source :router router
           :request {:request-method :post :uri "/operations/example/claim"
                     :request-id (random-uuid)}
           :inventory (wc/inv host))))

(defn- make-context [choice]
  (context
   (case choice
     :invoke (fn [{:keys [writer]}] (writer))
     :omit (fn [_] nil)
     :direct-only (fn [{:keys [before]}]
                    (xt/execute-tx wc/*node*
                                   [[:put-docs wc/table (assoc before :rogue true)]]))
     :direct-and-invoke (fn [{:keys [writer before]}]
                          (xt/execute-tx
                           wc/*node*
                           [[:put-docs wc/table
                             (assoc before :xt/id (str (random-uuid)) :rogue true)]])
                          (writer))
     :twice (fn [{:keys [writer]}]
              (writer)
              (try (writer) (catch Exception _ nil)))
     :future (fn [{:keys [writer]}] @(future (writer)))
     :throws-before (fn [_]
                      (throw (ex-info "intentional input failure"
                                      {:failure :intentional-before})))
     :throws-after (fn [{:keys [writer]}]
                     (writer)
                     (throw (ex-info "intentional after-write failure"
                                     {:failure :intentional-after})))
     :registration-after (fn [{:keys [writer host]}]
                           (writer)
                           (reset! host {writer-path (wc/registered (fn [] nil))}))
     :registration-before (fn [{:keys [writer host]}]
                            (reset! host {writer-path (wc/registered (fn [] nil))})
                            (writer))
     (throw (ex-info "Invalid test behavior" {:behavior choice})))))

(defn- observe
  ([ctx] (observe ctx (:request ctx)))
  ([{:keys [router route-source inventory handle]} request]
   (coverage/observe-checked-route-registered-writer!
    router route-source request inventory {writer-path handle} writer-path)))

(defn- revision [ctx]
  (:revision (wc/physical (:before ctx))))

(defn- assert-observation-boundary! [report]
  (is (= coverage/route-observation-type (:type report)))
  (is (= coverage/route-observation-version (:version report)))
  (is (= :one-synchronous-in-process-route-dispatch (:scope report)))
  (is (= :observed-checked-ring-dispatch-invoked-exact-registered-writer
         (:status report)))
  (is (= operation (:observed-operation-id report) (:observed-route-id report)))
  (is (= writer-path (:observed-writer-path-id report)))
  (is (= 1 (:observed-registered-callback-invocations report)))
  (is (true? (:registered-callback-returned? report)))
  (is (true? (:post-dispatch-route-and-host-registration-rechecked? report)))
  (doseq [k [:host-deployment :other-xtdb-writer-bypasses
             :abstract-operation-correspondence :physical-commit-and-causality]]
    (is (= :not-established (get report k)) (str "Unproved: " k)))
  (doseq [[k value] [[:certificate :not-issued]
                     [:admission :not-admitted]
                     [:transaction-authority :none]
                     [:authority :none]]]
    (is (= value (get report k))))
  (doseq [obligation [:unchecked-direct-and-asynchronous-writes-during-route-dispatch
                      :host-operation-id-matches-prepared-command-semantics
                      :other-processes-and-external-xtdb-clients
                      :physical-refinement-and-certification]]
    (is (contains? (:open-obligations report) obligation)))
  (is (not-any? #(contains? report %)
                [:node :tx-key :tx-ops :transaction :plan :before :after
                 :writer :handle :request :response :secret :physical-document]))
  (is (not (str/includes? (pr-str report) private-sentinel))))

(deftest genuine-ring-route-invokes-exact-writer-and-returns-observation
  (let [ctx (make-context :invoke)
        outcome (observe ctx)]
    (is (= 200 (get-in outcome [:response :status])))
    (is (some? (:route-witness outcome)))
    (is (= 1 @(:input-calls ctx)))
    (is (= 1 (revision ctx)))
    (assert-observation-boundary! (:report outcome))))

(deftest route-observation-is-post-dispatch-not-preflight-execution
  (let [ctx (make-context :invoke)]
    (is (= 0 (revision ctx)))
    (is (= 0 @(:input-calls ctx)))
    (is (= 1 (:matched-issued-monotonic-writer-count
              (coverage/require-registered-writer-coverage!
               (:inventory ctx) {writer-path (:handle ctx)}))))
    (is (= 0 (revision ctx)))
    (is (= 0 @(:input-calls ctx)))
    (observe ctx)
    (is (= 1 (revision ctx)))
    (is (= 1 @(:input-calls ctx)))))

(deftest omitted-writer-fails-even-when-certified-ring-handler-succeeds
  (let [ctx (make-context :omit)]
    (is (= :missing-extra-or-foreign-writer-invocation (failure #(observe ctx))))
    (is (= 1 @(:input-calls ctx)))
    (is (= 0 (revision ctx)))))

(deftest raw-xtdb-write-only-is-not-mistaken-for-registered-callback
  (let [ctx (make-context :direct-only)]
    (is (= :missing-extra-or-foreign-writer-invocation (failure #(observe ctx))))
    (is (true? (:rogue (wc/physical (:before ctx)))))
    (is (= 1 @(:input-calls ctx)))
    (is (= 0 (revision ctx)))))

(deftest extra-raw-write-remains-undetected-even-with-a-positive-report
  (let [ctx (make-context :direct-and-invoke)
        outcome (observe ctx)]
    (assert-observation-boundary! (:report outcome))
    (is (= 1 (revision ctx)))
    (is (= :not-established
           (get-in outcome [:report :other-xtdb-writer-bypasses])))))

(deftest twice-invoked-callback-is-not-one-invocation
  (let [ctx (make-context :twice)]
    (is (= :missing-extra-or-foreign-writer-invocation (failure #(observe ctx))))
    (is (= 1 (revision ctx)) "Rejecting the observation cannot undo the first write")
    (is (= 1 @(:input-calls ctx)))))

(deftest future-invocation-inherits-trace-but-is-not-same-thread
  (let [ctx (make-context :future)]
    (is (= :missing-extra-or-foreign-writer-invocation (failure #(observe ctx))))
    (is (= 1 (revision ctx)) "Async callback may already have completed")
    (is (= 1 @(:input-calls ctx)))))

(deftest handler-throwing-before-writer-does-not-yield-positive-observation
  (let [ctx (make-context :throws-before)]
    (is (= :intentional-before (failure #(observe ctx))))
    (is (= 0 (revision ctx)))
    (is (= 1 @(:input-calls ctx)))))

(deftest handler-throwing-after-write-does-not-yield-positive-observation
  (let [ctx (make-context :throws-after)]
    (is (= :intentional-after (failure #(observe ctx))))
    (is (= 1 (revision ctx)) "Write remains even when route fails afterward")
    (is (= 1 @(:input-calls ctx)))))

(deftest host-replaces-callback-before-invocation-but-observer-rechecks-after
  (let [ctx (make-context :registration-before)]
    (is (= :unverified-or-unaccounted-registered-writers
           (failure #(observe ctx))))
    (is (= 1 (revision ctx)) "Post-dispatch rejection does not roll back XTDB")))

(deftest host-replaces-callback-after-invocation-but-observer-rechecks-after
  (let [ctx (make-context :registration-after)]
    (is (= :unverified-or-unaccounted-registered-writers
           (failure #(observe ctx))))
    (is (= 1 (revision ctx)))))

(deftest substituted-registration-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (swap! (:host ctx) assoc writer-path (wc/registered (fn [] :foreign)))
    (is (= :unverified-or-unaccounted-registered-writers (failure #(observe ctx))))
    (is (= 0 @(:input-calls ctx)))
    (is (= 0 (revision ctx)))))

(deftest wrong-host-path-handle-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (is (= :unknown-or-foreign-route-writer
           (failure #(coverage/observe-checked-route-registered-writer!
                      (:router ctx) (:route-source ctx) (:request ctx)
                      (:inventory ctx) {:host/other (:handle ctx)} writer-path))))
    (is (= 0 @(:input-calls ctx)))))

(deftest fabricated-writer-handle-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (doseq [handle [(Object.) (coverage/->WriterHandle) nil]]
      (is (= :unknown-or-foreign-route-writer
             (failure #(coverage/observe-checked-route-registered-writer!
                        (:router ctx) (:route-source ctx) (:request ctx)
                        (:inventory ctx) {writer-path handle} writer-path)))))
    (is (= 0 (revision ctx)))
    (is (= 0 @(:input-calls ctx)))))

(deftest foreign-issued-handle-does-not-authorize-another-callback
  (let [ctx (make-context :invoke)
        b (wc/seeded) foreign (wc/issue b)]
    (is (= :unverified-or-unaccounted-registered-writers
           (failure #(coverage/observe-checked-route-registered-writer!
                      (:router ctx) (:route-source ctx) (:request ctx)
                      (:inventory ctx) {writer-path (:handle foreign)} writer-path))))
    (is (= 0 (revision ctx)))
    (is (= 0 @(:input-calls ctx)))))

(deftest registered-delegating-wrapper-is-not-exact-callback
  (let [ctx (make-context :invoke)
        wrapped (fn [] ((:writer ctx)))]
    (reset! (:host ctx) {writer-path (wc/registered wrapped)})
    (is (= :unverified-or-unaccounted-registered-writers
           (failure #(observe ctx))))
    (is (= 0 (revision ctx)))))

(deftest operation-mismatch-in-host-table-is-open
  (let [ctx (make-context :invoke)]
    (swap! (:host ctx) assoc writer-path
           {:kind :issued-operation :operation-id :example/other
            :writer (:writer ctx)})
    (is (= :unverified-or-unaccounted-registered-writers (failure #(observe ctx))))
    (is (= 0 @(:input-calls ctx)))))

(deftest raw-extra-registered-writer-prevents-checked-route-dispatch
  (let [ctx (make-context :invoke)]
    (swap! (:host ctx) assoc :host/unverified
           {:kind :raw-xtdb :operation-id nil :writer (fn [] :raw)})
    (is (= :unverified-or-unaccounted-registered-writers (failure #(observe ctx))))
    (is (= 0 @(:input-calls ctx)))))

(deftest missing-selected-registration-prevents-checked-route-dispatch
  (let [ctx (make-context :invoke)]
    (reset! (:host ctx) {})
    (is (= :unverified-or-unaccounted-registered-writers (failure #(observe ctx))))
    (is (= 0 @(:input-calls ctx)))))

(deftest unsupported-method-does-not-match-literal-checked-route
  (let [ctx (make-context :invoke)]
    (is (= :route-not-literal-or-wrong-operation
           (failure #(observe ctx (assoc (:request ctx) :request-method :get)))))
    (is (= 0 @(:input-calls ctx)))))

(deftest unsupported-uri-does-not-match-literal-checked-route
  (let [ctx (make-context :invoke)]
    (doseq [uri ["/operations/example/claim/" "/unmatched"
                 "/operations/example/other"]]
      (is (= :route-not-literal-or-wrong-operation
             (failure #(observe ctx (assoc (:request ctx) :uri uri))))))
    (is (= 0 @(:input-calls ctx)))))

(deftest malformed-route-observation-arguments-rejected-before-handler
  (let [ctx (make-context :invoke)]
    (doseq [[req handles path]
            [[nil {writer-path (:handle ctx)} writer-path]
             [nil {} writer-path]
             [[] {writer-path (:handle ctx)} writer-path]
             [(:request ctx) {writer-path (:handle ctx)} :not-qualified]
             [(:request ctx) [] writer-path]
             [{:uri 17 :request-method :post} {writer-path (:handle ctx)} writer-path]
             [{:uri "/operations/example/claim" :request-method "post"}
              {writer-path (:handle ctx)} writer-path]]]
      (is (= :invalid-route-writer-observation-source
             (failure #(coverage/observe-checked-route-registered-writer!
                        (:router ctx) (:route-source ctx) req
                        (:inventory ctx) handles path)))))
    (is (= 0 @(:input-calls ctx)))))

(deftest foreign-ring-router-is-not-accepted
  (let [ctx (make-context :invoke)]
    (is (= :unregistered-ring-router
           (failure #(coverage/observe-checked-route-registered-writer!
                      (fn [_] {:status 200}) (:route-source ctx) (:request ctx)
                      (:inventory ctx) {writer-path (:handle ctx)} writer-path))))
    (is (= 0 @(:input-calls ctx)))))

(deftest copied-source-object-cannot-reauthenticate-router
  (let [ctx (make-context :invoke)]
    (is (= :changed-ring-router-source-object
           (failure #(coverage/observe-checked-route-registered-writer!
                      (:router ctx) (into {} (:route-source ctx)) (:request ctx)
                      (:inventory ctx) {writer-path (:handle ctx)} writer-path))))
    (is (= 0 @(:input-calls ctx)))))

(deftest substituted-ring-observer-is-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (with-redefs [installed/observe-checked-ring-route!
                  (fn [& _] {:report {:observed-route-id operation}})]
      (is (= :substituted-route-observation-boundary (failure #(observe ctx)))))
    (is (= 0 @(:input-calls ctx)))))

(deftest substituted-route-recheck-is-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (with-redefs [installed/require-current-route-dispatch!
                  (fn [& _] :forged)]
      (is (= :substituted-route-observation-boundary (failure #(observe ctx)))))
    (is (= 0 @(:input-calls ctx)))))

(deftest substituted-route-summary-is-rejected-before-handler-runs
  (let [ctx (make-context :invoke)]
    (with-redefs [application/operation-summary
                  (fn [& _] {operation {:routes []}})]
      (is (= :substituted-route-observation-boundary (failure #(observe ctx)))))
    (is (= 0 @(:input-calls ctx)))))

(deftest delegated-writer-returns-not-a-commit-certificate
  (let [ctx (make-context :invoke)
        report (:report (observe ctx))]
    (is (= :not-established (:physical-commit-and-causality report)))
    (is (contains? (:open-obligations report)
                   :callback-result-not-xtdb-commit-authentication))
    (is (contains? (:open-obligations report)
                   :transaction-internal-prestate-and-write-causality))
    (is (= :not-issued (:certificate report)))))

(deftest repeated-observation-does-not-resubmit-a-one-shot-callback
  (let [ctx (make-context :invoke)]
    (is (= 200 (get-in (observe ctx) [:response :status])))
    (is (= 1 (revision ctx)))
    (is (= :one-shot-already-attempted (failure #(observe ctx))))
    (is (= 1 (revision ctx)))
    (is (= 2 @(:input-calls ctx)))))

(deftest foreign-issued-writer-invoked-instead-of-selected-writer
  (let [extra (atom nil)
        ctx (context (fn [_] ((:writer @extra))))
        b (wc/seeded)
        foreign (wc/issue b)]
    (reset! extra foreign)
    (is (= :missing-extra-or-foreign-writer-invocation (failure #(observe ctx))))
    (is (= 0 (revision ctx)))
    (is (= 1 (:revision (wc/physical b))))
    (is (= 1 @(:input-calls ctx)))))

(deftest route-writer-trace-is-not-an-authenticated-deployment-proof
  (let [ctx (make-context :invoke)
        outcome (observe ctx)
        report (:report outcome)]
    (is (= :not-established (:host-deployment report)))
    (is (= :not-established (:other-xtdb-writer-bypasses report)))
    (is (= :not-established (:abstract-operation-correspondence report)))
    (is (= :not-established (:physical-commit-and-causality report)))
    (is (not-any? #(str/includes? (pr-str report) %)
                  [private-sentinel (str (:xt/id (:before ctx)))
                   (str (:request-id (:request ctx)))]))))
