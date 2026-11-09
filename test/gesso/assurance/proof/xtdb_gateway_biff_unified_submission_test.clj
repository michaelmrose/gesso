(ns gesso.assurance.proof.xtdb-gateway-biff-unified-submission-test
  "v1594: permanent real-Biff/XTDB adversarial regression for v1593's
   opt-in unified, plan-bound request mediators.

   These checks exercise the SAME plan-bound acquired-prestate gateway that
   submits the guarded monotonic write through an identity-registered,
   request-scoped writer. They demonstrate bounded physical observations,
   NOT target-table business correctness, authenticated command causality,
   ABA exclusion, universal refinement, or deployment-wide writer closure."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test :as previous]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test :as prestate]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers-api]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private bounded-policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 1000})
(def ^:private canary "v1587-secret-must-not-be-disclosed")
(def ^:private private-target "v1587-private-physical-target")
(def ^:dynamic *modules* [])

(defn- fixture-helper [symbol]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      symbol)]
    (when-not v
      (throw (ex-info "Missing real certified XTDB fixture" {:reason :missing-fixture})))
    (var-get v)))

(defn- with-real-biff-and-xtdb [run]
  ((var-get (ns-resolve 'gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
                        'with-real-biff-and-xtdb)) run))

(use-fixtures :once with-real-biff-and-xtdb)

(defn- before [] ((fixture-helper 'before)))
(defn- after [b] ((fixture-helper 'after) b))
(defn- candidate [b] ((fixture-helper 'candidate) b))
(defn- candidate-with-after [b a] ((fixture-helper 'candidate) b a))
(defn- put! [b] ((fixture-helper 'put!) b))
(defn- physical [] ((fixture-helper 'physical)))

(defn- observe [f]
  (try {:accepted (f)}
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e) :data (ex-data e)})))

(defn- assert-denied! [f]
  (let [{:keys [data message] :as outcome} (observe f)]
    (is (not (contains? outcome :accepted))
        (str "Unexpectedly accepted invalid request: " (pr-str outcome)))
    (is (keyword? (:reason data)) (str "Missing typed rejection: " (pr-str outcome)))
    (is (not (some #(str/includes? (str message " " (pr-str data)) %)
                   [canary private-target]))
        "Diagnostics must not leak physical document identifiers")
    (:reason data)))

(defn- issue-module! [host contract resolver source! bounded?]
  (if bounded?
    (parallel/issue-unified-plan-bound-bounded-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision bounded-policy resolver)
    (parallel/issue-unified-plan-bound-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision resolver)))

(defn- build-case!
  ([source!] (build-case! source! false))
  ([source! bounded?]
   (let [{:keys [executor source target!]} prestate/*case*
         constructed (atom 0)
         invoked (atom 0)
         handler (certified/issue-certified-handler!
                  {:executor executor
                   :current-source source
                   :request->input!
                   (fn [_]
                     (swap! invoked inc)
                     (parallel/invoke-current-request-writer! writers/path)
                     ((var-get (ns-resolve 'gesso.model.certified-plan-test
                                           'invocation)) :reconcile))
                   :result->response! (fn [_ result] {:status 200 :body result})})
         contract (assoc-in (:contract prestate/*case*)
                            [:semantic-route-handlers operation] handler)
         resolver (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                   contract {operation target!})
         host (adoption/issue-checked-biff-host-module! contract)
         modules [app/application-handler-module (:module host)]
         intermediate (binding [*modules* modules]
                        (app/start-biff-application!
                         (:application-assembly contract) {} #'*modules*
                         [app/application-handler-module-id adoption/module-id]))
         wrapped (fn [r] (swap! constructed inc) (source! r))
         issued (issue-module! host contract resolver wrapped bounded?)
         module (:module issued)
         _ ((:biff.core/init module) nil)
         final ((:biff.core/start module) intermediate)
         report (parallel/require-current-isolated-parallel-biff-request-module!
                 (:handle issued) contract final)]
     {:host host :contract contract :resolver resolver :issued issued
      :final final :report report :constructed constructed :invoked invoked})))

(defn- call!
  ([case] (call! case request))
  ([case r] ((get (:final case) app/biff-ring-handler-key) r)))

(defn- assert-unmodified! [b]
  (is (= b (physical)) "Invalid request must not alter the protected document"))

(defn- assert-report! [report bounded?]
  (is (= parallel/report-type (:type report)))
  (is (= parallel/report-version (:version report)))
  (is (= :conditional-request-local-writer-mediation-installed (:status report)))
  (is (true? (:unified-plan-bound-submission-required? report)))
  (is (true? (:plan-bound-guarded-prestate-required? report)))
  (is (true? (:source-backed-target-resolver-required? report)))
  (is (= :one-prepared-acquired-gateway-for-this-submission
         (:prestate-submission-correspondence report)))
  (is (= :issued-certified-plan-callback-identity-checked-table-not-authenticated
         (:expected-target-provenance report)))
  (is (= :required-before-each-request-writer-issuance
         (:operation-target-binding-policy report)))
  (is (= (if bounded?
           :bounded-fair-permits-with-request-local-registries
           :unbounded-per-issued-mediator)
         (:parallel-admission-policy report)))
  (is (= (when bounded? 2) (:max-active-requests report)))
  (is (not (contains? (:open-obligations report)
                      :acquired-prestate-probe-and-real-monotonic-writer-are-distinct-preparations)))
  (doseq [key [:operation-refinement :other-writer-closure :actual-http-server-adoption]]
    (is (= :not-established (get report key)) (str "Unproved: " key)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (= :none (:transaction-authority report)))
  (is (not (some #(str/includes? (pr-str report) %)
                 [canary private-target]))))

(deftest unbounded-request-uses-unified-one-shot-physical-submission
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)))]
      (assert-report! (:report c) false)
      (is (= 0 @(:constructed c) @(:invoked c)))
      (assert-unmodified! b)
      (is (= 200 (:status (call! c))))
      (is (= 1 @(:constructed c) @(:invoked c)))
      (is (= (after b) (physical)))
      (assert-denied! #(call! c))
      (is (= 2 @(:constructed c)))
      (is (= 1 @(:invoked c)) "Stale repeat must not reach registered writer")
      (is (= (after b) (physical))))))

(deftest bounded-request-preserves-the-same-submission-guarantees
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) true)]
      (assert-report! (:report c) true)
      (is (= 2 (:max-pending-requests (:report c))))
      (is (= 1000 (:wait-timeout-ms (:report c))))
      (is (= 200 (:status (call! c))))
      (is (= 1 @(:constructed c) @(:invoked c)))
      (is (= (after b) (physical)))
      (assert-denied! #(call! c))
      (is (= 1 @(:invoked c)))
      (is (= (after b) (physical))))))

(deftest wrong-document-is-rejected-before-handler-for-both-mediators
  (doseq [bounded? [false true]]
    (let [b (before)]
      (put! b)
      (let [c (build-case! (constantly (candidate (assoc b :xt/id "wrong-document")))
                           bounded?)]
        (assert-denied! #(call! c))
        (is (= 1 @(:constructed c)))
        (is (zero? @(:invoked c)))
        (assert-unmodified! b)))))

(deftest incorrect-command-operation-and-protected-target-are-rejected
  (let [b (before)]
    (put! b)
    (doseq [bad [(assoc-in (candidate b) [:plan :commands 0 :model/operation] :cancel)
                 (assoc-in (candidate b) [:protected-documents 0 :target]
                           [:requests "wrong-protected-document"])]]
      (let [c (build-case! (constantly bad))]
        (assert-denied! #(call! c))
        (is (= 1 @(:constructed c)))
        (is (zero? @(:invoked c)))
        (assert-unmodified! b)))))

(deftest forged-full-prestate-is-rejected-even-with-equal-revision
  (doseq [bounded? [false true]]
    (let [b (before)]
      (put! b)
      (let [forged (assoc b :note "forged-but-same-revision")
            c (build-case! (constantly (candidate forged)) bounded?)]
        (assert-denied! #(call! c))
        (is (= 1 @(:constructed c)))
        (is (zero? @(:invoked c)))
        (assert-unmodified! b)))))

(deftest non-successor-revision-is-rejected-before-physical-write
  (let [b (before)]
    (put! b)
    (let [after-b (assoc (after b) :revision (+ 2 (:revision b)))
          c (build-case! (fn [_] (candidate-with-after b after-b)))]
      (let [outcome (observe #(call! c))]
        (is (not (contains? outcome :accepted)))
        (is (= :gesso.model.command/invalid-update-command
               (get-in outcome [:data :error/type]))))
      (is (= 1 @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))

(deftest unmatched-route-is-denied-before-constructing-the-command
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) true)]
      (is (= :unmatched-parallel-request-route
             (assert-denied! #(call! c (assoc request :uri "/operations/not-certified")))))
      (is (zero? @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))

(deftest arbitrary-and-weaker-target-resolvers-cannot-issue-unified-mediator
  (let [b (before)]
    (put! b)
    (let [{:keys [contract issuance target!]} prestate/*case*
          host (adoption/issue-checked-biff-host-module! contract)
          weaker (provenance/issue-certified-literal-route-target-resolver!
                  contract {operation {:command-operation :claim
                                       :target [:requests (:xt/id b)]}})
          wrapped {:expected-target! (fn [r] ((:expected-target! issuance) r))}
          source! (constantly (candidate b))]
      (doseq [resolver [nil wrapped weaker]]
        (assert-denied! #(issue-module! host contract resolver source! false))
        (assert-denied! #(issue-module! host contract resolver source! true)))
      (assert-unmodified! b))))

(deftest unified-writer-constructor-substitution-fails-before-any-write
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)))]
      (with-redefs [writers-api/issue-request-scoped-plan-bound-monotonic-writer!
                    (fn [& _] (throw (ex-info "substituted" {:reason :substituted}))) ]
        (assert-denied! #(call! c)))
      (is (zero? @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))

(deftest outside-request-cannot-invoke-the-registered-writer
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)))]
      (assert-denied! #(parallel/invoke-current-request-writer! writers/path))
      (is (zero? @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))
