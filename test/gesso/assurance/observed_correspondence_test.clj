(ns gesso.assurance.observed-correspondence-test
  "v1303: permanent adversarial tests for the bounded v1302 join of a live
   source-bound application and one copyable v1298 XTDB observation.

   These tests use the original certified-plan/induction fixtures, a real
   issued executor and Ring handler, a closed Gesso assembly, and committed
   embedded-XTDB transactions. Negative cases distinguish malformed sources
   (reject), valid but divergent persisted states (diagnostic mismatch), and
   plausible forged witnesses (diagnostic only; NEVER authenticated proof).
   Neither successful observations nor a closed declared catalog establish
   complete physical writer coverage, soundness, or universal refinement."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.observed-correspondence :as joined]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.inductive-plan-test]
            [gesso.model.transition-conformance :as conformance]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private operation :example/claim)
(def ^:private rejection-type
  :gesso.assurance.observed-correspondence/invalid-observed-correspondence)
(def ^:dynamic *context* nil)

(defn- fixture-helper [test-ns symbol]
  (or (some-> (ns-resolve test-ns symbol) deref)
      (throw (ex-info "Missing certified source fixture"
                      {:test-ns test-ns :name symbol}))))

(defn- fresh-context []
  (let [e ((fixture-helper 'gesso.model.certified-plan-test 'environment))
        plan-source (:source e)
        catalog ((fixture-helper 'gesso.model.inductive-plan-test 'catalog-for)
                 (:construction plan-source))
        proof ((fixture-helper 'gesso.model.inductive-plan-test 'proof-package)
               catalog)
        induction-source {:inductive-source proof
                          :certified-plan-source plan-source}
        executor (induction-plan/build-inductive-plan-operation! induction-source)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source plan-source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})]
    {:plan-source plan-source :induction-source induction-source
     :proof proof :executor executor :handler handler
     :environment e}))

(defn- capture-committed! [node plan-source environment wrong?]
  (let [before ((:current-document! plan-source) {} {})
        target ((:target! plan-source) {} {})
        inputs ((:transition-inputs! plan-source) {} {})
        domain @(get environment :proposed)
        binding {:command-id (identity/command-id (str (random-uuid)))
                 :principal (identity/principal "helper")
                 :operation operation :arguments {}}
        incorporation (resolution/incorporation-plan
                       {:binding binding :outcome {:status :claimed}
                        :now "now" :plan {:commands [domain] :emit false}})
        command-target [:gesso_commands (:xt/id (:record incorporation))]
        _ (xt/execute-tx node [[:put-docs :requests before]])
        handle (observation/begin-observation!
                {:node node :incorporation incorporation
                 :targets [target command-target]})
        submitted (get-in incorporation [:plan])
        ;; For a real committed-but-wrong result, retain only the persisted
        ;; command record. The physical domain update is absent.
        submitted (if wrong?
                    (update submitted :commands
                            (fn [commands]
                              (filterv #(= :create (:model/operation %))
                                       commands)))
                    submitted)
        tx-key (xt/execute-tx node (:tx-ops (tx/prepare {} submitted)))
        witness (observation/complete-observation! handle {:tx-key tx-key})]
    {:observation witness
     :construction (:construction plan-source)
     :field-map (:field-map plan-source)
     :inputs inputs
     :target target
     :command-operation :claim}))

(defn- full-source [transition]
  {:application-source (:application-source *context*)
   :transition-source transition})

(defn- check! [transition]
  (joined/check-observed-correspondence! (full-source transition)))

(defn- thrown [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected? [f]
  (let [e (thrown f)]
    (and (map? e) (qualified-keyword? (:error/type e)))))

(defn- rejected-as [reason f]
  (let [e (thrown f)]
    (is (= rejection-type (:error/type e)) (pr-str e))
    (is (= reason (:reason e)) (pr-str e))))

(use-fixtures
  :once
  (fn [run]
    (let [{:keys [plan-source proof executor handler environment] :as context}
          (fresh-context)]
      (fixture/with-closed-application
        {:operations {operation
                      (fixture/trusted-operation
                       operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [application-source
                {:authoritative-coverage-source
                 {:inductive-contract-source
                  {:inductive-source proof
                   :application-assembly assembly
                   :semantic-route-handlers {operation handler}
                   :certified-operations
                   {operation {:executor executor :current-source plan-source}}}
                  :enumerate-entry-points!
                  (fn [] {operation {:kind :semantic-route :handler handler}})}
                 :inductive-plans
                 {operation {:executor executor
                             :current-source (:induction-source context)}}}]
            (with-open [node (xtn/start-node {})]
              (let [conforming (capture-committed!
                                node plan-source environment false)
                    divergent (capture-committed!
                               node plan-source environment true)]
                (binding [*context*
                          (assoc context
                                 :application-source application-source
                                 :conforming conforming
                                 :divergent divergent)]
                  (run))))))))))

(defn- conforming [] (:conforming *context*))
(defn- divergent [] (:divergent *context*))

(deftest real-committed-update-matches-only-diagnostically
  (let [r (check! (conforming))]
    (is (= joined/report-type (:type r)))
    (is (= joined/report-version (:version r)))
    (is (= :current-application-and-declared-physical-step-rechecked
           (:checking r)))
    (is (= operation (:operation r)))
    (is (true? (:diagnostic-conforms? r)))
    (is (= :corresponds-to-declared-step (:diagnostic-correspondence r)))
    (is (every? true? (vals (:observed-checks r))))
    (is (= :exact-source-and-issued-executor-rechecked
           (:plan-correspondence r)))
    (is (pos? (:observed-target-count r)))
    (is (nat-int? (:physical-tx-id r)))))

(deftest committed-command-without-domain-update-diverges
  (let [r (check! (divergent))]
    (is (false? (:diagnostic-conforms? r)))
    (is (= :diverges-from-declared-step (:diagnostic-correspondence r)))
    (is (some false? (vals (:observed-checks r))))
    (is (= :none (:authority r)))
    (is (= :not-established (:authenticated-physical-correspondence r)))))

(deftest a-copyable-physical-observation-is-not-authenticated
  (let [source (conforming)
        fabricated (assoc-in source [:observation :physical-tx-id] 99999999)
        r (check! fabricated)]
    ;; A well-shaped fabricated witness can pass the v1300 comparison. This
    ;; is a documented limit, not an opportunity to issue stronger evidence.
    (is (true? (:diagnostic-conforms? r)))
    (is (= 99999999 (:physical-tx-id r)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance r)))
    (is (= :not-established (:authenticated-physical-correspondence r)))
    (is (= :none (:authority r)))))

(deftest erroneous-physical-state-is-a-mismatch-not-a-commit-proof
  (let [transition (conforming)
        target (:target transition)
        changed (assoc-in transition
                          [:observation :observed-after target :request/status]
                          :open)
        r (check! changed)]
    (is (false? (:diagnostic-conforms? r)))
    (is (= :diverges-from-declared-step (:diagnostic-correspondence r)))
    (is (false? (get-in r [:observed-checks :abstract-assignments])))
    (is (= :not-established (:application-invariant-physical-preservation r)))))

(deftest incorrect-physical-prestate-cannot-confirm-a-valid-step
  (let [transition (conforming)
        target (:target transition)
        changed (assoc-in transition
                          [:observation :observed-before target :request/status]
                          :claimed)
        r (check! changed)]
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:observed-checks :declared-prestate])))))

(deftest wrong-physical-value-type-is-nonconforming
  (let [transition (conforming)
        target (:target transition)
        changed (assoc-in transition
                          [:observation :observed-after target :request/status]
                          777)
        r (check! changed)]
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:observed-checks :abstract-assignments])))))

(deftest wrong-operation-id-cannot-select-a-different-plan
  (let [transition (assoc-in (conforming)
                             [:construction :subject :id] :example/cancel)]
    (rejected-as :operation-not-bound-to-current-application
                 #(check! transition))))

(deftest same-operation-id-with-different-guard-is-not-bound
  (let [transition (assoc-in (conforming)
                             [:construction :guard]
                             {:readers #{:request/status}
                              :expression
                              [:= [:read :request/status] [:literal :open]]})]
    (rejected-as :observation-not-bound-to-issued-plan
                 #(check! transition))))

(deftest changed-assignment-is-not-the-issued-construction
  (let [transition (assoc-in (conforming)
                             [:construction :replacements :request/status]
                             [:literal :open])]
    (rejected-as :observation-not-bound-to-issued-plan
                 #(check! transition))))

(deftest replaced-physical-mapping-is-not-the-issued-construction
  (let [transition (assoc-in (conforming)
                             [:field-map :request/note] :request/other-note)]
    (rejected-as :observation-not-bound-to-issued-plan
                 #(check! transition))))

(deftest different-command-operation-is-not-the-issued-plan
  (let [transition (assoc (conforming) :command-operation :cancel)]
    (rejected-as :observation-not-bound-to-issued-plan
                 #(check! transition))))

(deftest observation-cannot-add-a-proof-claim
  (let [transition (assoc-in (conforming)
                             [:observation :certification] :proved)]
    (is (= :invalid-observation-claims
           (:reason (thrown #(check! transition)))))))

(deftest incomplete-or-tampered-command-record-is-rejected
  (let [transition (conforming)
        changed (update-in transition [:observation :command-binding]
                           assoc :operation :example/other)]
    (is (= :inconsistent-observed-footprint-or-command
           (:reason (thrown #(check! changed)))))))

(deftest observation-shape-cannot-hide-arbitrary-proof-flags
  (let [transition (update (conforming) :observation
                           assoc :proved? true)]
    (is (= :invalid-source-shape
           (:reason (thrown #(check! transition)))))))

(deftest malformed-top-level-inputs-are-rejected-before-upstream-work
  (doseq [source [nil [] #{} {:application-source (:application-source *context*)}
                  {:transition-source (conforming)}
                  {:application-source (:application-source *context*)
                   :transition-source (conforming) :proved? true}]]
    (rejected-as :invalid-source-shape
                 #(joined/check-observed-correspondence! source))))

(deftest malformed-transition-descriptions-are-rejected
  (doseq [transition [nil {} {:construction (:construction (conforming))}
                      (assoc (conforming) :proved? true)
                      (with-meta (conforming) {:certified true})]]
    (rejected-as :invalid-source-shape
                 #(check! transition))))

(deftest missing-or-substituted-ring-handler-is-rejected
  (let [base (full-source (conforming))
        source (assoc-in base
                         [:application-source :authoritative-coverage-source
                          :inductive-contract-source :semantic-route-handlers
                          operation]
                         (fn [_] {:status 200}))]
    (is (rejected? #(joined/check-observed-correspondence! source)))))

(deftest mismatched-but-valid-issued-executor-is-not-installed-handler
  (let [different (induction-plan/build-inductive-plan-operation!
                   (:induction-source *context*))
        base (full-source (conforming))
        source (assoc-in base [:application-source :inductive-plans
                               operation :executor] different)]
    (is (not (identical? (:executor *context*) different)))
    (is (rejected? #(joined/check-observed-correspondence! source)))))

(deftest stale-plan-guard-must-not-reuse-old-induction-proof
  (let [base (full-source (conforming))
        source (assoc-in base [:application-source :inductive-plans
                               operation :current-source
                               :certified-plan-source :construction :guard]
                         {:readers #{:request/status}
                          :expression
                          [:= [:read :request/status] [:literal :claimed]]})]
    (is (rejected? #(joined/check-observed-correspondence! source)))))

(deftest missing-declared-semantic-route-fails-closed
  (let [base (full-source (conforming))
        source (assoc-in base [:application-source :authoritative-coverage-source
                               :enumerate-entry-points!]
                         (fn [] {}))]
    (is (rejected? #(joined/check-observed-correspondence! source)))))

(deftest extra-observed-writer-stays-unclassified
  (let [base (full-source (conforming))
        handler (:handler *context*)
        source (assoc-in base [:application-source :authoritative-coverage-source
                               :enumerate-entry-points!]
                         (fn [] {operation {:kind :semantic-route :handler handler}
                                 :example/direct-writer
                                 {:kind :unclassified
                                  :handler (fn [_] nil)}}))
        r (joined/check-observed-correspondence! source)]
    (is (contains? (:unclassified-entry-point-ids r)
                   :example/direct-writer))
    (is (= :not-established (:physical-write-path-completeness r)))
    (is (= :none (:authority r)))))

(deftest forged-sibling-checker-maps-never-become-authority
  (let [source (full-source (conforming))]
    (with-redefs [application/check-application-induction!
                  (fn [_] {:type application/report-type
                           :authority :none :valid? true})]
      (rejected-as :substituted-upstream-checker
                   #(joined/check-observed-correspondence! source)))
    (with-redefs [conformance/check-observed-transition!
                  (fn [_] {:type conformance/report-type
                           :corresponds? true :proved? true})]
      (rejected-as :substituted-upstream-checker
                   #(joined/check-observed-correspondence! source)))))

(deftest original-proofs-and-physical-assumptions-remain-open
  (let [r (check! (conforming))]
    (is (seq (:open-premises r)))
    (is (seq (:open-obligations r)))
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))
    (is (contains? (:declared-abstract-operation-ids r) operation))
    (is (= #{operation} (:observed-application-operation-ids r)))
    (is (= (disj (:declared-abstract-operation-ids r) operation)
           (:unobserved-declared-operation-ids r)))
    (doseq [x [:physical-observation-origin-is-unauthenticated
               :actual-submitted-inputs-and-plan-are-not-established
               :all-physical-writers-are-not-established
               :single-observation-does-not-establish-universal-refinement]]
      (is (contains? (:open-obligations r) x) (pr-str x)))
    (doseq [k [:physical-write-path-completeness
               :authenticated-physical-correspondence
               :universal-xtdb-refinement
               :application-invariant-physical-preservation
               :deployed-router-adoption
               :independent-checker-soundness
               :production-certification]]
      (is (= :not-established (get r k)) (pr-str k)))
    (is (= :none (:transaction-authority r)))
    (is (= :none (:authority r)))))

(deftest prior-diagnostic-report-is-rechecked-not-trusted
  (let [source (full-source (conforming))
        prior (joined/check-observed-correspondence! source)]
    (is (= prior (joined/require-current-observed-correspondence! prior source)))
    (doseq [fabricated [(assoc prior :diagnostic-conforms? false)
                       (assoc prior :authenticated-physical-correspondence :proved)
                       (assoc prior :authority :granted)
                       (assoc prior :valid? true)
                       (with-meta prior {:certificate true})]]
      (rejected-as :stale-or-forged-diagnostic-report
                   #(joined/require-current-observed-correspondence!
                     fabricated source)))))

(deftest changed-observation-invalidates-the-old-diagnostic-description
  (let [old-source (full-source (conforming))
        report (joined/check-observed-correspondence! old-source)
        new-source (full-source
                    (assoc-in (conforming)
                              [:observation :physical-tx-id] 314159))]
    (rejected-as :stale-or-forged-diagnostic-report
                 #(joined/require-current-observed-correspondence!
                   report new-source))))

(deftest physical-diagnostic-is-not-evidence-of-complete-production-coverage
  (let [r (check! (conforming))]
    (is (= :one-supplied-unauthenticated-physical-observation (:scope r)))
    (is (= :non-authoritative-observed-application-step-diagnostic
           (:evidence-class r)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance r)))
    (is (set? (:open-obligations r)))
    (is (map? (:original-open-obligation-ids-by-operation r)))
    (is (set? (get (:original-open-obligation-ids-by-operation r)
                   operation)))
    (is (not= :proved (:production-certification r)))
    (is (= :none (:transaction-authority r)))))
