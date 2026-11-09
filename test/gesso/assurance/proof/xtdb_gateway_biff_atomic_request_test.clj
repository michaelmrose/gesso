(ns gesso.assurance.proof.xtdb-gateway-biff-atomic-request-test
  "v1607: real-Biff/XTDB adversarial regressions for v1606's opt-in
   plan-bound atomic command-incorporation request mediators. Both bounded
   and unbounded requests use exactly one request-scoped gateway to perform
   the guarded domain UPDATE and durable command-record CREATE together.

   Observed physical outcomes and command records are NOT universal proofs
   of execution causality, command argument truth, principal authorization,
   deployment-wide writer closure, or abstract-operation refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
             :as mediator]
            [gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance
             :as provenance]
            [gesso.assurance.proof.xtdb-gateway-unknown-command-reconciliation
             :as reconciliation]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writer-api]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.certified-plan-test]
            [xtdb.api :as xt])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private writer-path :host/claim)
(def ^:private policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 30000})
(def ^:private wait-seconds 35)
(def ^:dynamic *modules* [])

(defn- helper [name]
  (or (some-> (ns-resolve
               'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
               name) var-get)
      (throw (ex-info "Missing certified XTDB fixture"
                      {:reason :missing-certified-fixture}))))

(defn- with-real-biff-and-xtdb [run]
  ((var-get (ns-resolve
             'gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
             'with-real-biff-and-xtdb)) run))

(use-fixtures :once with-real-biff-and-xtdb)

(defn- before [] ((helper 'before)))
(defn- after [b note] (assoc ((helper 'after) b) :note note))
(defn- candidate [b a] ((helper 'candidate) b a))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))

(defn- command-input
  ([] (command-input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "v1607-helper")
              :operation operation
              :arguments {:request-id "v1587-private-physical-target"}}
    :outcome {:status :claimed}
    :now "2026-11-07T00:00:00Z"}))

(defn- command-record [input]
  (first (xt/q fixture/*node*
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding input))])))

(defn- expected-record [input]
  (resolution/command-record
   (:binding input) (:outcome input) (:now input)))

(defn- outcome [f]
  (try {:status (:status (f))}
       (catch Throwable t
         (let [data (ex-data t)]
           {:reason (or (:reason data)
                        (:xtdb.error/code data)
                        :unclassified-throwable)
            :exception-class (.getName (class t))}))))

(defn- issued-case! [source! incorporation-input! bounded?]
  (let [{:keys [executor source target!]} fixture/*case*
        sources (atom 0)
        inputs (atom 0)
        entered (atom 0)
        in-scope-retries (atom [])
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input!
                  (fn [r]
                    (swap! entered inc)
                    (when-let [{:keys [^CountDownLatch arrived
                                       ^CountDownLatch release]} (:race r)]
                      (.countDown arrived)
                      (when-not (.await release wait-seconds TimeUnit/SECONDS)
                        (throw (ex-info "Competing request release timed out"
                                        {:reason :race-release-timeout}))))
                    (mediator/invoke-current-request-writer! writer-path)
                    (when (= :lost-response (:scenario r))
                      (swap! in-scope-retries conj
                             (:reason (outcome
                                       #(mediator/invoke-current-request-writer!
                                         writer-path))))
                      (throw (ex-info "Response lost after transaction return"
                                      {:reason :simulated-response-loss})))
                    ((var-get
                      (ns-resolve 'gesso.model.certified-plan-test 'invocation))
                     :reconcile))
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract (assoc-in (:contract fixture/*case*)
                           [:semantic-route-handlers operation] handler)
        resolver (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                  contract {operation target!})
        host (adoption/issue-checked-biff-host-module! contract)
        modules [app/application-handler-module (:module host)]
        initial (binding [*modules* modules]
                  (app/start-biff-application!
                   (:application-assembly contract) {} #'*modules*
                   [app/application-handler-module-id adoption/module-id]))
        wrapped-source! (fn [r] (swap! sources inc) (source! r))
        wrapped-input! (fn [r] (swap! inputs inc) (incorporation-input! r))
        issued (if bounded?
                 (mediator/issue-plan-bound-atomic-incorporation-bounded-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writer-path}
                  fixture/*node* wrapped-source! :revision policy resolver wrapped-input!)
                 (mediator/issue-plan-bound-atomic-incorporation-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writer-path}
                  fixture/*node* wrapped-source! :revision resolver wrapped-input!))
        module (:module issued)
        _ ((:biff.core/init module) nil)
        final ((:biff.core/start module) initial)]
    {:handler (get final app/biff-ring-handler-key)
     :sources sources :inputs inputs :entered entered
     :in-scope-retries in-scope-retries
     :report (mediator/require-current-isolated-parallel-biff-request-module!
              (:handle issued) contract final)}))

(defn- request-with [b a input]
  (assoc request :source-before b :source-after a :command-input input))

(defn- standard-case! [bounded?]
  (issued-case!
   (fn [r] (candidate (:source-before r) (:source-after r)))
   (fn [r] (:command-input r))
   bounded?))

(defn- assert-report-scope! [report bounded?]
  (is (true? (:atomic-command-incorporation-required? report)))
  (is (= :one-transaction-guarded-domain-update-and-command-record-create
         (:atomic-command-incorporation-envelope report)))
  (is (= :one-prepared-plan-bound-atomic-gateway-with-pinned-prestate
         (:prestate-submission-correspondence report)))
  (is (= (if bounded?
           :bounded-fair-permits-with-request-local-registries
           :unbounded-per-issued-mediator)
         (:parallel-admission-policy report)))
  (is (= :not-established (:atomic-command-commit-claim report)))
  (is (= :not-established (:operation-refinement report)))
  (is (= :not-established (:other-writer-closure report)))
  (is (= :not-established (:actual-http-server-adoption report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:transaction-authority report)))
  (is (contains? (:open-obligations report)
                 :command-arguments-and-target-identity-not-proved))
  (is (contains? (:open-obligations report)
                 :principal-authorization-and-recorded-outcome-not-proved)))

(deftest atomic-incorporation-through-unbounded-and-bounded-biff
  (doseq [bounded? [false true]]
    (testing (str "bounded? " bounded?)
      (let [b (before) a (after b "incorporated") ci (command-input)
            _ (put! b)
            c (standard-case! bounded?)
            req (request-with b a ci)]
        (assert-report-scope! (:report c) bounded?)
        (is (= b (physical)))
        (is (nil? (command-record ci)))
        (is (= {:status 200} (outcome #((:handler c) req))))
        (is (= a (physical)))
        (is (= (expected-record ci) (command-record ci)))
        (is (= 1 @(:sources c) @(:inputs c) @(:entered c)))
        (is (= :incorporation-before-disagrees-with-external-read
               (:reason (outcome #((:handler c) req)))))
        (is (= 2 @(:sources c) @(:inputs c)))
        (is (= 1 @(:entered c)))
        (is (= a (physical)))
        (is (= (expected-record ci) (command-record ci)))))))

(deftest rejected-preparation-does-not-create-command-record
  (doseq [bounded? [false true]]
    (let [b (before) ci (command-input) _ (put! b)
          c (standard-case! bounded?)
          wrong-op (assoc-in ci [:binding :operation] :example/cancel)
          wrong-target (assoc b :xt/id (str (random-uuid)))
          forged (assoc b :note "forged-prestate")]
      (is (= :incorporation-binding-operation-disagrees-with-route
             (:reason (outcome #((:handler c)
                                 (request-with b (after b "bad-op") wrong-op))))))
      (is (not= {:status 200}
                (outcome #((:handler c)
                           (request-with wrong-target (after wrong-target "bad-target") ci)))))
      (is (= :incorporation-before-disagrees-with-external-read
             (:reason (outcome #((:handler c)
                                 (request-with forged (after forged "bad-before") ci))))))
      (is (= :unmatched-parallel-request-route
             (:reason (outcome #((:handler c)
                                 (assoc (request-with b (after b "never") ci)
                                        :uri "/operations/unregistered"))))))
      (is (= b (physical)))
      (is (nil? (command-record ci)))
      (is (nil? (command-record wrong-op)))
      (is (zero? @(:entered c))))))

(defn- competing! [bounded? duplicate-command-id?]
  (let [b (before) _ (put! b)
        arrived (CountDownLatch. 2)
        release (CountDownLatch. 1)
        ci-left (command-input)
        ci-right (if duplicate-command-id?
                   ci-left
                   (command-input))
        after-left (after b "left-wins")
        after-right (after b "right-wins")
        c (standard-case! bounded?)
        r-left (assoc (request-with b after-left ci-left)
                      :race {:arrived arrived :release release})
        r-right (assoc (request-with b after-right ci-right)
                       :race {:arrived arrived :release release})
        futures [(future (outcome #((:handler c) r-left)))
                 (future (outcome #((:handler c) r-right)))]]
    (try
      (let [both? (.await arrived wait-seconds TimeUnit/SECONDS)]
        (is both? "Both checked Biff requests must hold valid writers simultaneously")
        (when both?
          (is (= 2 @(:sources c) @(:inputs c) @(:entered c)))
          (is (= b (physical)))
          (is (nil? (command-record ci-left)))
          (is (nil? (command-record ci-right)))))
      (.countDown release)
      (let [results (mapv #(deref % (* 1000 wait-seconds) ::timed-out) futures)
            actual (physical)
            winner-left? (= actual after-left)
            winning-input (if winner-left? ci-left ci-right)
            losing-input (if winner-left? ci-right ci-left)]
        (is (not-any? #{::timed-out} results))
        (is (= 1 (count (filter #(= {:status 200} %) results))))
        (is (= 1 (count (filter #(= :xtdb/assert-failed (:reason %)) results))))
        (is (contains? #{after-left after-right} actual))
        (is (= 1 (:revision actual)))
        (is (= (expected-record winning-input) (command-record winning-input)))
        (if duplicate-command-id?
          (is (= (command-record ci-left) (command-record ci-right))
              "One canonical command record exists under the shared ID")
          (is (nil? (command-record losing-input))
              "A losing, distinct command cannot leave an orphaned record"))
        (assert-report-scope! (:report c) bounded?))
      (finally
        (.countDown release)
        (doseq [f futures] (future-cancel f))))))

(deftest parallel-distinct-command-incorporations-have-one-winner
  (competing! false false)
  (competing! true false))

(deftest parallel-duplicate-command-id-incorporates-once
  (competing! false true)
  (competing! true true))

(deftest duplicate-command-id-cannot-authorize-fresh-domain-update
  (doseq [bounded? [false true]]
    (let [b (before) a (after b "first") ci (command-input)
          _ (put! b)
          c (standard-case! bounded?)]
      (is (= {:status 200}
             (outcome #((:handler c) (request-with b a ci)))))
      (let [a2 (after a "second")
            duplicate (outcome #((:handler c) (request-with a a2 ci)))]
        (is (= :xtdb/assert-failed (:reason duplicate)))
        (is (= a (physical)))
        (is (= (expected-record ci) (command-record ci)))
        (is (= 1 (:revision (physical))))))))

(deftest post-submit-response-loss-reconciles-by-command-record-not-document
  (doseq [bounded? [false true]]
    (let [b (before) a (after b "committed-but-response-lost")
          ci (command-input) fresh-id (command-input)
          _ (put! b)
          c (standard-case! bounded?)
          req (assoc (request-with b a ci) :scenario :lost-response)
          result (outcome #((:handler c) req))]
      (is (= :simulated-response-loss (:reason result)))
      (is (= a (physical)))
      (is (= (expected-record ci) (command-record ci)))
      (is (= [:one-shot-already-attempted] @(:in-scope-retries c)))
      (let [report (reconciliation/acquire-unknown-command-reconciliation!
                    fixture/*node* (:binding ci) (fn [_ _] true))]
        (is (= :compatible-durable-record-observed-attempt-unattributed
               (:status report)))
        (is (true? (:compatible-durable-record-observed? report)))
        (is (false? (:prior-attempt-commit-attributed? report)))
        (is (false? (:retry-authorized? report)))
        (is (= :none (:transaction-authority report))))
      (is (= :incorporation-before-disagrees-with-external-read
             (:reason (outcome #((:handler c) (request-with b a ci))))))
      (let [fresh-after (after a "new-command")]
        (is (= :xtdb/assert-failed
               (:reason (outcome #((:handler c)
                                   (request-with a fresh-after ci))))))
        (is (= a (physical)))
        (is (= {:status 200}
               (outcome #((:handler c)
                          (request-with a fresh-after fresh-id)))))
        (is (= fresh-after (physical)))
        (is (= (expected-record fresh-id) (command-record fresh-id)))
        (is (= (expected-record ci) (command-record ci)))))))

(deftest substituted-writer-issuer-does-not-admit-request
  (let [b (before) ci (command-input) _ (put! b)
        c (standard-case! false)]
    (with-redefs [writer-api/issue-request-scoped-plan-bound-atomic-incorporation-writer!
                  (fn [& _] (throw (ex-info "unexpected substitute" {})))]
      (is (= :substituted-atomic-incorporation-request-writer-issuer
             (:reason (outcome #((:handler c)
                                 (request-with b (after b "not-written") ci)))))))
    (is (every? zero? [@(:sources c) @(:inputs c) @(:entered c)]))
    (is (= b (physical)))
    (is (nil? (command-record ci)))))
