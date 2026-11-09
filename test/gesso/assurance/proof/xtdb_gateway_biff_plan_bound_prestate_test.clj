(ns gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
  "v1589: real-Biff/XTDB adversarial regression for the v1588 opt-in
   plan-bound guarded-prestate request mediators. Each matched request must
   independently bind its certified plan target to its physical command,
   acquire and check the actual complete XTDB prestate, and only then issue
   the existing request-local monotonic one-shot writer.

   The acquired prestate gateway is DISCARDED, not submitted. These tests do
   not assert identical submission capabilities, authenticated application
   targets, semantic operation refinement, physical causality, or closure of
   unmediated writers. Real transaction observations remain bounded tests."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate :as guarded]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test :as prestate]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private bounded-policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 1000})
(def ^:private private-token "v1587-secret-must-not-be-disclosed")
(def ^:dynamic *modules* [])

(defn- previous-helper [sym]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      sym)]
    (when-not v
      (throw (ex-info "Missing real certified-plan fixture helper"
                      {:reason :missing-real-certified-plan-helper :name sym})))
    (var-get v)))

(defn- with-real-biff-and-xtdb [run]
  (let [writers-fixture (first (:clojure.test/once-fixtures
                               (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-writer-coverage-test))))
        prestate-fixture (previous-helper 'with-certified-xtdb)]
    (when-not (fn? writers-fixture)
      (throw (ex-info "Missing real XTDB writer fixture"
                      {:reason :missing-real-writer-fixture})))
    (writers-fixture #(prestate-fixture run))))

(use-fixtures :once with-real-biff-and-xtdb)

(defn- before [] ((previous-helper 'before)))
(defn- after [b] ((previous-helper 'after) b))
(defn- candidate [b] ((previous-helper 'candidate) b))
(defn- put! [b] ((previous-helper 'put!) b))
(defn- physical [] ((previous-helper 'physical)))

(defn- observe [f]
  (try {:accepted (f)}
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e) :data (ex-data e)})))

(defn- assert-rejected! [expected f]
  (let [{:keys [data message] :as outcome} (observe f)]
    (is (not (contains? outcome :accepted))
        "A failed request must not return an accepted result")
    (is (= expected (:reason data)) (str "Unexpected failure " (pr-str outcome)))
    (is (some true? (map data
                         [:xtdb-gateway-biff-parallel-request-context/rejection
                          :xtdb-gateway-plan-bound-guarded-prestate/rejection
                          :xtdb-gateway-operation-target-binding/rejection
                          :xtdb-gateway-route-target-provenance/rejection])))
    (is (not (str/includes? (str message " " (pr-str data)) private-token)))))

(defn- issue! [host contract issuance source! policy]
  (if policy
    (parallel/issue-plan-bound-guarded-prestate-bounded-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision policy issuance)
    (parallel/issue-plan-bound-guarded-prestate-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision issuance)))

(defn- build-case!
  ([source! policy] (build-case! source! policy nil))
  ([source! policy instrumentation]
   (let [{:keys [executor source target!] :as certified-case} prestate/*case*
         invoked (atom 0)
         constructed (atom 0)
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
         contract (assoc-in (:contract certified-case)
                            [:semantic-route-handlers operation] handler)
         issuance (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                   contract {operation target!})
         host (adoption/issue-checked-biff-host-module! contract)
         modules [app/application-handler-module (:module host)]
         intermediate (binding [*modules* modules]
                        (app/start-biff-application!
                         (:application-assembly contract) {} #'*modules*
                         [app/application-handler-module-id adoption/module-id]))
         wrapped (fn [req]
                   (swap! constructed inc)
                   (source! req))
         resolver (or (:resolver-issuance instrumentation) issuance)
         issued (issue! host contract resolver wrapped policy)
         module (:module issued)
         _ ((:biff.core/init module) nil)
         final ((:biff.core/start module) intermediate)]
     {:contract contract :issuance issuance :host host :issued issued
      :final final :constructed constructed :invoked invoked
      :report (parallel/require-current-isolated-parallel-biff-request-module!
               (:handle issued) contract final)})))

(defn- call! [c]
  ((get (:final c) app/biff-ring-handler-key) request))

(defn- assert-unmodified! [b]
  (is (= b (physical)) "Rejected request must not mutate the protected XTDB document"))

(defn- assert-report-honest! [r policy]
  (is (= parallel/report-type (:type r)))
  (is (= parallel/report-version (:version r)))
  (is (true? (:source-backed-target-resolver-required? r)))
  (is (true? (:plan-bound-guarded-prestate-required? r)))
  (is (= :required-before-each-request-writer-issuance
         (:operation-target-binding-policy r)))
  (is (= :issued-certified-plan-callback-identity-checked-table-not-authenticated
         (:expected-target-provenance r)))
  (is (= :independent-preparation-before-monotonic-writer-not-same-capability
         (:prestate-submission-correspondence r)))
  (is (= (if policy :bounded-fair-permits-with-request-local-registries
                      :unbounded-per-issued-mediator)
         (:parallel-admission-policy r)))
  (doseq [k [:operation-refinement :other-writer-closure :actual-http-server-adoption]]
    (is (= :not-established (get r k)) (str "Unproved: " k)))
  (is (contains? (:open-obligations r)
                 :acquired-prestate-probe-and-real-monotonic-writer-are-distinct-preparations))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (not (str/includes? (pr-str r) private-token))))

(deftest unbounded-real-biff-commits-only-after-plan-target-and-prestate-checks
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) nil)]
      (assert-report-honest! (:report c) nil)
      (is (= 0 @(:constructed c) @(:invoked c)))
      (assert-unmodified! b)
      (is (= 200 (:status (call! c))))
      (is (= 1 @(:constructed c) @(:invoked c)))
      (is (= (after b) (physical)))
      (assert-rejected! :plan-bound-guarded-prestate-preparation-failed #(call! c))
      (is (= 2 @(:constructed c)))
      (is (= 1 @(:invoked c)) "Stale request must fail before handler/writer")
      (is (= (after b) (physical))))))

(deftest bounded-real-biff-retains-successful-monotonic-writer
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) bounded-policy)]
      (assert-report-honest! (:report c) bounded-policy)
      (is (= 2 (:max-active-requests (:report c))))
      (is (= 2 (:max-pending-requests (:report c))))
      (is (= 200 (:status (call! c))))
      (is (= 1 @(:constructed c) @(:invoked c)))
      (is (= (after b) (physical)))
      (assert-rejected! :plan-bound-guarded-prestate-preparation-failed #(call! c))
      (is (= 1 @(:invoked c)))
      (is (= (after b) (physical))))))

(deftest forged-external-before-fails-before-issuing-writer
  (let [b (before)]
    (put! b)
    (let [forged (assoc b :note "forged-prestate-but-same-revision")
          c (build-case! (constantly (candidate forged)) nil)]
      (assert-rejected! :plan-bound-guarded-prestate-preparation-failed #(call! c))
      (is (= 1 @(:constructed c)))
      (is (= 0 @(:invoked c)))
      (assert-unmodified! b))))

(deftest bounded-path-also-rejects-physical-prestate-mismatch
  (let [b (before)]
    (put! b)
    (let [forged (assoc b :note "forged-prestate")
          c (build-case! (constantly (candidate forged)) bounded-policy)]
      (assert-rejected! :plan-bound-guarded-prestate-preparation-failed #(call! c))
      (is (= 1 @(:constructed c)))
      (is (= 0 @(:invoked c)))
      (assert-unmodified! b))))

(deftest wrong-document-and-command-operation-fail-before-writer
  (let [b (before)]
    (put! b)
    (doseq [candidate-source
            [(candidate (assoc b :xt/id "v1589-unrelated-document"))
             (assoc-in (candidate b) [:plan :commands 0 :model/operation] :cancel)
             (assoc-in (candidate b) [:protected-documents 0 :target]
                       [:requests "v1589-unrelated-document"])]]
      (let [c (build-case! (constantly candidate-source) nil)]
        (let [outcome (observe #(call! c))]
          (is (not (contains? outcome :accepted)))
          (is (contains? #{:command-target-disagrees-with-expectation
                           :command-operation-disagrees-with-expectation
                           :protected-document-disagrees-with-expectation}
                         (get-in outcome [:data :reason]))
              (pr-str outcome)))
        (is (= 1 @(:constructed c)))
        (is (= 0 @(:invoked c)))
        (assert-unmodified! b)))))

(deftest unmatched-route-never-constructs-a-writer-or-reads-a-prestate
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) nil)
          invalid-request (assoc request :uri "/operations/example/not-certified")]
      (assert-rejected! :unmatched-parallel-request-route
                        #((get (:final c) app/biff-ring-handler-key) invalid-request))
      (is (zero? @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))

(deftest issuer-rejects-copied-unissued-and-weaker-resolvers
  (let [b (before)]
    (put! b)
    (let [{:keys [source target!] :as certified-case} prestate/*case*
          contract (:contract certified-case)
          host (adoption/issue-checked-biff-host-module! contract)
          plan-issuance (:issuance certified-case)
          equivalent-wrapper {:expected-target!
                              (fn [req] ((:expected-target! plan-issuance) req))}
          weaker (provenance/issue-certified-literal-route-target-resolver!
                  contract {operation {:command-operation :claim
                                       :target [:requests (:xt/id b)]}})
          issuer (fn [resolver policy]
                   (issue! host contract resolver (constantly (candidate b)) policy))]
      (doseq [[bad expected]
              [[nil :requires-exact-issued-route-target-resolver]
               [{:expected-target! identity :extra true}
                :requires-exact-issued-route-target-resolver]
               [equivalent-wrapper :unissued-or-incompatible-route-target-resolver]
               [weaker :unexpected-issued-route-target-resolver-report]]]
        (assert-rejected! expected #(issuer bad nil)))
      (assert-rejected! :unissued-or-incompatible-route-target-resolver
                        #(issuer equivalent-wrapper bounded-policy))
      (assert-unmodified! b))))

(deftest substituted-prestate-checker-fails-closed-before-physical-write
  (let [b (before)]
    (put! b)
    (let [c (build-case! (constantly (candidate b)) nil)]
      (with-redefs [guarded/prepare-plan-bound-acquired-one-shot!
                    (fn [& _] (throw (ex-info "substituted" {})))]
        (assert-rejected! :substituted-parallel-mediation-boundary
                          #(call! c)))
      (is (zero? @(:constructed c)))
      (is (zero? @(:invoked c)))
      (assert-unmodified! b))))
