(ns gesso.assurance.proof.xtdb-gateway-biff-unified-unknown-outcome-test
  "v1597: real-Biff/XTDB adversarial comparison of a KNOWN guarded-CAS
   rejection and an INDETERMINATE request result following a successful
   one-shot physical submission. The latter is deliberately simulated by
   throwing AFTER the transaction's submit! returns but BEFORE the handler
   can return an HTTP response. The caller's error is not proof of rejection.

   This checks concrete recovery behavior in a controlled single-node test,
   not general unknown-outcome reconciliation, HTTP transport guarantees,
   complete writer coverage, operation refinement or causal proof. In the
   conflict scenario a controlled external write is intentionally used to
   invalidate the acquired prestate; that write is outside Gesso mediation."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test :as fixture]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test :as prestate]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan-test]))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 30000})
(def ^:dynamic *modules* [])

(defn- fixture-helper [name]
  (or (some-> (ns-resolve
               'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
               name) var-get)
      (throw (ex-info "Missing XTDB test fixture"
                      {:reason :missing-xtdb-test-fixture}))))

(defn- with-real-biff-and-xtdb [run]
  ((var-get (ns-resolve
             'gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
             'with-real-biff-and-xtdb)) run))

(use-fixtures :once with-real-biff-and-xtdb)

(defn- before [] ((fixture-helper 'before)))
(defn- after [b] ((fixture-helper 'after) b))
(defn- candidate [b a] ((fixture-helper 'candidate) b a))
(defn- put! [b] ((fixture-helper 'put!) b))
(defn- physical [] ((fixture-helper 'physical)))

(defn- outcome [f]
  (try {:status (:status (f))}
       (catch Throwable t
         {:reason (or (:reason (ex-data t))
                      (:xtdb.error/code (ex-data t))
                      :unclassified-throwable)})))

(defn- issue! [host contract resolver source! bounded?]
  (if bounded?
    (parallel/issue-unified-plan-bound-bounded-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision policy resolver)
    (parallel/issue-unified-plan-bound-isolated-parallel-biff-request-module!
     (:handle host) contract writers/*source* {operation writers/path}
     prestate/*node* source! :revision resolver)))

(defn- build-case! [source! bounded? attempts]
  (let [{:keys [executor source target!]} prestate/*case*
        entered (atom 0)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input!
                  (fn [req]
                    (swap! entered inc)
                    (case (:scenario req)
                      :lost-response
                      (do
                        ;; The submission has actually returned, but the
                        ;; request never delivers a success response.
                        (parallel/invoke-current-request-writer! writers/path)
                        (swap! attempts assoc :physical-submit-returned true
                               :in-request-reuse
                               (:reason
                                (outcome
                                 #(parallel/invoke-current-request-writer!
                                   writers/path))))
                        (throw (ex-info "Synthetic post-submit response loss"
                                        {:reason :simulated-response-loss})))
                      :conflict
                      (do
                        ;; Deliberate bypass writer, used ONLY by this test:
                        ;; prestate acquisition happened before this handler.
                        (put! (:intervening req))
                        (parallel/invoke-current-request-writer! writers/path))
                      :fresh
                      (parallel/invoke-current-request-writer! writers/path)
                      (throw (ex-info "Unsupported test scenario"
                                      {:reason :unknown-test-scenario})))
                    ((var-get (ns-resolve 'gesso.model.certified-plan-test
                                          'invocation)) :reconcile))
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract (assoc-in (:contract prestate/*case*)
                           [:semantic-route-handlers operation] handler)
        resolver (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                  contract {operation target!})
        host (adoption/issue-checked-biff-host-module! contract)
        modules [app/application-handler-module (:module host)]
        initial (binding [*modules* modules]
                  (app/start-biff-application!
                   (:application-assembly contract) {} #'*modules*
                   [app/application-handler-module-id adoption/module-id]))
        issued (issue! host contract resolver source! bounded?)
        module (:module issued)
        _ ((:biff.core/init module) nil)
        final ((:biff.core/start module) initial)
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (:handle issued) contract final)]
    {:handler (get final app/biff-ring-handler-key)
     :entered entered :report report}))

(defn- exercise! [bounded? scenario]
  (let [initial (before)
        _ (put! initial)
        first-after (assoc (after initial) :note "first-attempt")
        source (atom (candidate initial first-after))
        attempts (atom {})
        case (build-case! (fn [_] @source) bounded? attempts)
        handler (:handler case)
        req (assoc request :scenario scenario)
        intervening (assoc initial :note "competing-external-write")
        result (outcome #(handler (cond-> req
                                    (= scenario :conflict)
                                    (assoc :intervening intervening))))]
    (is (= (if (= scenario :lost-response)
             :simulated-response-loss
             :xtdb/assert-failed)
           (:reason result))
        "A response loss cannot be treated as the same event as CAS rejection")
    (is (= 1 @(:entered case)) "Both outcomes occur after writer issuance")
    (if (= scenario :lost-response)
      (do
        (is (true? (:physical-submit-returned @attempts)))
        (is (= :one-shot-already-attempted (:in-request-reuse @attempts))
            "A lost response cannot reactivate the original one-shot writer")
        (is (= first-after (physical))
            "The caller has no success response, but XTDB physically committed"))
      (do
        (is (empty? @attempts))
        (is (= intervening (physical))
            "An independently committed competing write survives the CAS conflict")))
    ;; In either case, resubmitting the OLD source through a new Biff request
    ;; is rejected before its handler executes. This is not a client retry API.
    (let [stale (outcome #(handler (assoc request :scenario :fresh)))]
      (is (= :guarded-external-prestate-acquisition-failed (:reason stale)))
      (is (= 1 @(:entered case)))
      (is (= (if (= scenario :lost-response) first-after intervening)
             (physical))))
    ;; A controlled test observer now reads authoritative XTDB state and
    ;; constructs a DIFFERENT command with a fresh prestate. In production,
    ;; this read would require node, history and command-identity assurance.
    (let [authoritative (physical)
          fresh-after (assoc (after authoritative) :note "reconciled-new-command")]
      (reset! source (candidate authoritative fresh-after))
      (is (= {:status 200}
             (outcome #(handler (assoc request :scenario :fresh)))))
      (is (= fresh-after (physical)))
      (is (= 2 @(:entered case)))
      (is (= (if (= scenario :lost-response) 2 1)
             (:revision (physical)))
          "Lost-response commit and CAS-conflict rejection have distinct bases"))
    (is (= :one-prepared-acquired-gateway-for-this-submission
           (get-in case [:report :prestate-submission-correspondence])))
    (is (= :not-established (get-in case [:report :operation-refinement])))
    (is (= :not-established (get-in case [:report :other-writer-closure])))
    (is (= :none (get-in case [:report :transaction-authority])))))

(deftest unknown-response-after-commit-remains-distinct-from-cas-rejection
  (testing "unbounded Biff mediator distinguishes post-submit response failure"
    (exercise! false :lost-response)))

(deftest bounded-unknown-response-after-commit-remains-distinct-from-cas-rejection
  (testing "bounded Biff mediator does not infer physical rejection from failed response"
    (exercise! true :lost-response)))

(deftest known-cas-conflict-has-distinct-physical-recovery-state
  (testing "controlled intervening write creates genuine transaction-time CAS rejection"
    (exercise! false :conflict)))

(deftest bounded-known-cas-conflict-has-distinct-physical-recovery-state
  (testing "bounded admission cannot convert CAS conflict into a lost-response commit"
    (exercise! true :conflict)))
