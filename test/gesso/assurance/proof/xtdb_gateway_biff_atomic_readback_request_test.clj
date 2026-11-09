(ns gesso.assurance.proof.xtdb-gateway-biff-atomic-readback-request-test
  "v1611: real Biff/XTDB request-scope regressions for v1610's opt-in
   atomic-incorporation postsubmission observer. The real Biff cases check
   the exact registered writer and its pinned, redacted three-part readback.
   A separately identified controlled-scope case uses the SAME genuine
   issued atomic writer with a failing observer to exercise the after-commit
   failure path without substituting the pinned production observer.

   These are finite execution tests, not causal proofs, automatic retry
   authority, application authorization, or deployment-wide writer closure."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-atomic-readback :as readback]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
             :as mediator]
            [gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance
             :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writer-api]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.certified-plan-test]
            [xtdb.api :as xt])
  (:import (java.util.concurrent CountDownLatch TimeUnit)
           (xtdb.api TransactionKey)))

(def ^:private operation :example/claim)
(def ^:private path :host/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private policy {:max-active-requests 2
                       :max-pending-requests 2
                       :wait-timeout-ms 30000})
(def ^:private wait-seconds 35)
(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-outcome "PRIVATE-v1611-outcome")
(def ^:dynamic *modules* [])

(defn- helper [sym]
  (or (some-> (ns-resolve
               'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
               sym) var-get)
      (throw (ex-info "Missing real certified XTDB fixture"
                      {:reason :missing-v1611-fixture}))))

(use-fixtures :once
  (fn [run]
    ((var-get (ns-resolve
                'gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
                'with-real-biff-and-xtdb)) run)))

(defn- before [] ((helper 'before)))
(defn- after [b note] (assoc ((helper 'after) b) :note note))
(defn- candidate [b a] ((helper 'candidate) b a))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))
(defn- command-input
  ([] (command-input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "v1611-helper")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))
(defn- expected-record [input]
  (resolution/command-record (:binding input) (:outcome input) (:now input)))
(defn- command-record [input]
  (first (xt/q fixture/*node*
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding input))])))
(defn- outcome [f]
  (try {:status (:status (f))}
       (catch Throwable e
         {:reason (or (:reason (ex-data e))
                      (:xtdb.error/code (ex-data e))
                      :unknown-throwable)
          :exception-class (.getName (class e))})))
(defn- reason [f] (or (:reason (outcome f)) :no-error))
(defn- request-with [b a input]
  (assoc request :source-before b :source-after a :command-input input))

(defn- issued-case! [bounded?]
  (let [{:keys [executor source target!]} fixture/*case*
        sources (atom 0)
        inputs (atom 0)
        entered (atom 0)
        observations (atom [])
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input!
                  (fn [req]
                    (swap! entered inc)
                    ;; Readback is not available until the writer returns.
                    (let [before-reason (reason mediator/current-request-atomic-readback-report!)]
                      (when-let [{:keys [^CountDownLatch arrived
                                         ^CountDownLatch release]} (:barrier req)]
                        (.countDown arrived)
                        (when-not (.await release wait-seconds TimeUnit/SECONDS)
                          (throw (ex-info "Test barrier timed out"
                                          {:reason :v1611-barrier-timeout}))))
                      (let [result (mediator/invoke-current-request-writer! path)
                            report (mediator/current-request-atomic-readback-report!)]
                        (swap! observations conj
                               {:tag (:tag req) :before-reason before-reason
                                :report report :tx-key (:tx-key result)
                                :owner (Thread/currentThread)
                                :foreign-thread-reason
                                @(future
                                   (reason mediator/current-request-atomic-readback-report!))})
                        (when (= :lose-response (:scenario req))
                          (throw (ex-info "Simulated response loss after readback"
                                          {:reason :simulated-response-loss-after-readback}))))
                    ((var-get (ns-resolve 'gesso.model.certified-plan-test
                                          'invocation)) :reconcile)))
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract (assoc-in (:contract fixture/*case*)
                           [:semantic-route-handlers operation] handler)
        resolver (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                  contract {operation target!})
        host (adoption/issue-checked-biff-host-module! contract)
        modules [app/application-handler-module (:module host)]
        intermediate (binding [*modules* modules]
                       (app/start-biff-application!
                        (:application-assembly contract) {} #'*modules*
                        [app/application-handler-module-id adoption/module-id]))
        source! (fn [req]
                  (swap! sources inc)
                  (candidate (:source-before req) (:source-after req)))
        input! (fn [req] (swap! inputs inc) (:command-input req))
        issued (if bounded?
                 (mediator/issue-plan-bound-atomic-readback-bounded-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation path}
                  fixture/*node* source! :revision policy resolver input!)
                 (mediator/issue-plan-bound-atomic-readback-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation path}
                  fixture/*node* source! :revision resolver input!))
        module (:module issued)
        _ ((:biff.core/init module) nil)
        final ((:biff.core/start module) intermediate)]
    {:handler (get final app/biff-ring-handler-key)
     :report (mediator/require-current-isolated-parallel-biff-request-module!
              (:handle issued) contract final)
     :sources sources :inputs inputs :entered entered
     :observations observations}))

(defn- assert-no-authority! [report]
  (is (= readback/report-type (:type report)))
  (is (= readback/report-version (:version report)))
  (is (= :bounded-postsubmission-read-only-coobservation (:proof-status report)))
  (is (true? (:three-observations-share-pinned-basis? report)))
  (is (false? (:key-attributed-to-command-creation? report)))
  (is (false? (:submitted-transaction-contents-authenticated? report)))
  (is (false? (:application-command-inputs-authenticated? report)))
  (is (false? (:retry-authorized? report)))
  (is (= :not-established (:transaction-causality report)))
  (is (= :not-established (:external-writer-closure report)))
  (is (= :not-established (:universal-operation-refinement report)))
  (is (= :none (:transaction-authority report)
         (:live-settlement-authority report) (:authority report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (not (contains? report :tx-key)))
  (is (not (str/includes? (pr-str report) private-id)))
  (is (not (str/includes? (pr-str report) private-outcome))))

(defn- assert-mediator-report! [r bounded?]
  (is (= :conditional-request-local-writer-mediation-installed (:status r)))
  (is (true? (:atomic-command-incorporation-required? r)))
  (is (true? (:postsubmission-atomic-readback-required? r)))
  (is (= :same-request-after-registered-writer-return-pinned-basis
         (:postsubmission-atomic-readback-policy r)))
  (is (= :not-established (:postsubmission-readback-causality r)))
  (is (= :none (:postsubmission-readback-retry-authority r)))
  (is (= (if bounded?
           :bounded-fair-permits-with-request-local-registries
           :unbounded-per-issued-mediator)
         (:parallel-admission-policy r)))
  (is (= :not-admitted (:admission r))))

(deftest readback-is-bound-to-actual-biff-request-and-writer
  (doseq [bounded? [false true]]
    (testing (str "bounded=" bounded?)
      (let [b (before) a (after b "committed") input (command-input)
            _ (put! b) c (issued-case! bounded?)
            req (assoc (request-with b a input) :tag :first)]
        (assert-mediator-report! (:report c) bounded?)
        (is (= :no-current-request-atomic-readback-report
               (reason mediator/current-request-atomic-readback-report!)))
        (is (= 200 (:status ((:handler c) req))))
        (is (= a (physical)))
        (is (= (expected-record input) (command-record input)))
        (is (= 1 @(:entered c) @(:sources c) @(:inputs c)))
        (is (= 1 (count @(:observations c))))
        (let [{:keys [tag before-reason report tx-key foreign-thread-reason]} (first @(:observations c))]
          (is (= :first tag))
          (is (= :no-current-request-atomic-readback-report before-reason))
          (is (= :no-current-request-atomic-readback-report foreign-thread-reason))
          (is (instance? TransactionKey tx-key))
          (is (= :bounded-poststate-command-and-ledger-coobservation (:status report)))
          (assert-no-authority! report))
        (is (= :no-current-request-atomic-readback-report
               (reason mediator/current-request-atomic-readback-report!)))
        (is (= :incorporation-before-disagrees-with-external-read
               (reason #((:handler c) req))))
        (is (= 1 (count @(:observations c))))
        (is (= a (physical)))))))

(deftest distinct-sequential-biff-requests-do-not-share-observation-state
  (doseq [bounded? [false true]]
    (let [b (before) a (after b "first") a2 (after a "second")
          first-input (command-input) second-input (command-input)
          _ (put! b) c (issued-case! bounded?)]
      (is (= 200 (:status ((:handler c) (assoc (request-with b a first-input) :tag :first)))))
      (is (= 200 (:status ((:handler c) (assoc (request-with a a2 second-input) :tag :second)))))
      (is (= a2 (physical)))
      (is (= (expected-record first-input) (command-record first-input)))
      (is (= (expected-record second-input) (command-record second-input)))
      (is (= [:first :second] (mapv :tag @(:observations c))))
      (is (= 2 (count (distinct (map :tx-key @(:observations c))))))
      (is (every? #(= :bounded-poststate-command-and-ledger-coobservation
                      (get-in % [:report :status])) @(:observations c)))
      (is (= 2 @(:entered c) @(:sources c) @(:inputs c)))
      (is (= :no-current-request-atomic-readback-report
             (reason mediator/current-request-atomic-readback-report!))))))

(deftest concurrent-requests-have-isolated-observation-and-one-cas-winner
  (doseq [bounded? [false true]]
    (let [b (before) a1 (after b "left") a2 (after b "right")
          first-input (command-input) second-input (command-input)
          arrived (CountDownLatch. 2) release (CountDownLatch. 1)
          _ (put! b) c (issued-case! bounded?)
          r1 (assoc (request-with b a1 first-input) :tag :left
                    :barrier {:arrived arrived :release release})
          r2 (assoc (request-with b a2 second-input) :tag :right
                    :barrier {:arrived arrived :release release})
          tasks [(future (outcome #((:handler c) r1)))
                 (future (outcome #((:handler c) r2)))]]
      (try
        (let [both? (.await arrived wait-seconds TimeUnit/SECONDS)]
          (is both? "Both requests must have obtained valid one-shot writers")
          (when both?
            (is (= 2 @(:entered c) @(:sources c) @(:inputs c)))
            (is (= b (physical)))
            (is (nil? (command-record first-input)))
            (is (nil? (command-record second-input)))))
        (.countDown release)
        (let [results (mapv #(deref % (* 1000 wait-seconds) ::timed-out) tasks)
              persisted (physical)
              winner (if (= persisted a1) first-input second-input)
              loser (if (= persisted a1) second-input first-input)]
          (is (not-any? #{::timed-out} results))
          (is (= 1 (count (filter #(= {:status 200} %) results))))
          (is (= 1 (count (filter #(= :xtdb/assert-failed (:reason %)) results))))
          (is (contains? #{a1 a2} persisted))
          (is (= 1 (:revision persisted)))
          (is (= (expected-record winner) (command-record winner)))
          (is (nil? (command-record loser)))
          (is (= 1 (count @(:observations c))))
          (let [{:keys [tag report tx-key foreign-thread-reason]} (first @(:observations c))]
            (is (= (if (= persisted a1) :left :right) tag))
            (is (instance? TransactionKey tx-key))
            (is (= :bounded-poststate-command-and-ledger-coobservation (:status report)))
            (is (= :no-current-request-atomic-readback-report foreign-thread-reason))
            (assert-no-authority! report))
          (is (= :no-current-request-atomic-readback-report
                 (reason mediator/current-request-atomic-readback-report!))))
        (finally
          (.countDown release)
          (doseq [f tasks] (future-cancel f)))))))

(deftest lost-response-after-successful-readback-is-still-not-rejection
  (doseq [bounded? [false true]]
    (let [b (before) a (after b "response-lost") input (command-input)
          _ (put! b) c (issued-case! bounded?)
          req (assoc (request-with b a input) :scenario :lose-response :tag :lost)]
      (is (= :simulated-response-loss-after-readback
             (reason #((:handler c) req))))
      (is (= a (physical)))
      (is (= (expected-record input) (command-record input)))
      (is (= 1 (count @(:observations c))))
      (let [report (:report (first @(:observations c)))]
        (is (= :bounded-poststate-command-and-ledger-coobservation (:status report)))
        (assert-no-authority! report))
      (is (= :no-current-request-atomic-readback-report
             (reason mediator/current-request-atomic-readback-report!)))
      (is (= :incorporation-before-disagrees-with-external-read
             (reason #((:handler c) req))))
      (is (= a (physical)))
      (is (= (expected-record input) (command-record input))))))

(deftest invalid-prestate-and-substituted-observer-grant-no-write-or-report
  (doseq [bounded? [false true]]
    (let [b (before) input (command-input) _ (put! b)
          c (issued-case! bounded?)
          forged (assoc b :note "forged")
          wrong (assoc (request-with forged (after forged "no-write") input) :tag :forged)]
      (is (= :incorporation-before-disagrees-with-external-read
             (reason #((:handler c) wrong))))
      (is (= b (physical)))
      (is (nil? (command-record input)))
      (is (zero? @(:entered c)))
      (is (empty? @(:observations c)))
      (with-redefs [readback/acquire-atomic-command-readback! (fn [& _] {})]
        (is (= :substituted-atomic-readback-observer
               (reason #((:handler c) (request-with b (after b "no-write") input))))))
      (is (= b (physical)))
      (is (nil? (command-record input)))
      (is (empty? @(:observations c))))))

;; Controlled postcommit observer-failure seam: this is NOT an alternate Biff
;; implementation or a replacement for the pinned production observer. The
;; issued writer and real XTDB transaction remain identical to v1604/v1610.
(defn- controlled-failing-observer! [failure-kind]
  (let [b (before) a (after b "committed-before-observer-error")
        input (command-input) _ (put! b)
        {:keys [contract issuance]} fixture/*case*
        issued (writer-api/issue-request-scoped-plan-bound-atomic-incorporation-writer!
                path operation fixture/*node* contract issuance request
                (candidate b a) :revision input)
        observation (atom {:state :not-observed})
        scope-var (ns-resolve
                   'gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
                   '*current-request-writer*)
        observed-error (atom nil)
        retry-reason (atom nil)
        failing-observer (case failure-kind
                           :throw (fn [_] (throw (ex-info "readback unavailable" {})))
                           :unsafe-report (fn [_] {:type readback/report-type
                                                   :version readback/report-version
                                                   :status :committed
                                                   :retry-authorized? true}))]
    ((:with-request! issued)
     (fn []
       (with-bindings {scope-var {:owner (Thread/currentThread)
                                  :path path :writer (:writer issued)
                                  :observe! failing-observer
                                  :observation observation}}
         (reset! observed-error (reason #(mediator/invoke-current-request-writer! path)))
         (reset! retry-reason (reason #(mediator/invoke-current-request-writer! path))))))
    (is (= a (physical)))
    (is (= (expected-record input) (command-record input)))
    (is (= :one-shot-already-attempted @retry-reason))
    (is (= {:state :not-observed} @observation))
    (is (= :closed ((:request-scope-status! issued))))
    (is (= :no-current-request-atomic-readback-report
           (reason mediator/current-request-atomic-readback-report!)))
    @observed-error))

(deftest postcommit-observer-exception-leaves-authoritative-commit-unknown-to-caller
  (is (= :postsubmission-observation-unavailable-outcome-unknown
         (controlled-failing-observer! :throw))))

(deftest unsafe-postcommit-observer-report-cannot-promote-to-commit-certificate
  (is (= :unexpected-postsubmission-observation-report
         (controlled-failing-observer! :unsafe-report))))
