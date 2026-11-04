(ns gesso.assurance.diagnostic-test
  "v1314: permanent first-blocker and authority-boundary diagnostics.

   Check CURRENT original sources, never a caller's copied report. Exercise
   deterministic blocker priority, source attribution, finite counterexamples,
   source-only admission, and actual embedded-XTDB observed correspondence.
   This test reuses permanent v1310/v1311/v1303 fixture builders; it does not
   depend on the temporary v1313 diagnostic smoke namespaces.

   Every positive description remains a COPYABLE diagnostic, not an admitted
   certificate, authenticated physical trace, or transaction permission."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.diagnostic :as diagnostic]
            [gesso.assurance.proof.finite-model-oracle-test]
            [gesso.assurance.proof.induction-crosscheck-test]
            [gesso.assurance.observed-correspondence-test]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.inductive-report :as inductive-report]
            [gesso.assurance.observed-correspondence :as observed]
            [gesso.assurance.proof.finite-model-oracle :as finite]
            [gesso.assurance.proof.induction :as induction]))

(defn- permanent-fixture [ns-name symbol]
  (or (some-> (ns-resolve ns-name symbol) deref)
      (throw (ex-info "Permanent diagnostic fixture missing"
                      {:ns ns-name :symbol symbol}))))

(def ^:private catalog
  (permanent-fixture 'gesso.assurance.proof.induction-crosscheck-test 'catalog))
(def ^:private finite-source
  (permanent-fixture 'gesso.assurance.proof.finite-model-oracle-test 'base-source))
(def ^:private crosscheck-source
  (permanent-fixture 'gesso.assurance.proof.induction-crosscheck-test 'source))

(defn- diagnose [kind original]
  (diagnostic/diagnose! {:kind kind :source original}))

(defn- source-only-report [c]
  {:catalog c :certificate-handle nil
   :dependency-handle nil :current-graph nil})

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))

(defn- blocker-code [report] (get-in report [:first-blocker :code]))
(defn- blocker-class [report] (get-in report [:first-blocker :class]))
(defn- blocker-path [report] (get-in report [:first-blocker :at]))

(def ^:private mandatory-open
  #{:independent-induction-checker-soundness
    :host-authoritative-entry-point-inventory-is-complete
    :physical-write-path-coverage
    :physical-prestate-and-initialization-correspondence
    :abstract-to-physical-transition-refinement
    :deployed-router-and-executors-adopt-checked-snapshot})

(defn- diagnostic-only [kind report]
  (is (= diagnostic/diagnostic-type (:type report)) (pr-str report))
  (is (= diagnostic/diagnostic-version (:version report)))
  (is (= kind (:kind report)))
  (is (= :blocked (:status report)))
  (is (= :diagnostic-only (:evidence-status report)))
  (is (= :none (:authority report)))
  (is (= :none (:transaction-authority report)))
  (is (= :not-established (:production-certification report)))
  (is (= :not-established (:independent-checker-soundness report)))
  (is (= :not-established (:physical-write-path-completeness report)))
  (is (= :not-established (:universal-xtdb-refinement report)))
  (is (set? (:open-obligations report)))
  (is (every? (:open-obligations report) mandatory-open))
  (is (map? (:first-blocker report)))
  (is (keyword? (blocker-class report)))
  (is (keyword? (blocker-code report)))
  (is (vector? (blocker-path report)))
  report)

(defn- context []
  (permanent-fixture 'gesso.assurance.observed-correspondence-test '*context*))
(defn- observed-source [transition]
  {:application-source (:application-source (context))
   :transition-source transition})
(defn- application-with-extras [entry-ids]
  (let [app (:application-source (context))
        h (get-in app [:authoritative-coverage-source
                       :inductive-contract-source :semantic-route-handlers
                       :example/claim])
        inventory (merge {:example/claim {:kind :semantic-route :handler h}}
                         (into {} (map (fn [id] [id {:kind :unclassified
                                                  :handler (fn [_] nil)}])
                                       entry-ids)))]
    (assoc-in app [:authoritative-coverage-source :enumerate-entry-points!]
              (fn [] inventory))))

;; The permanent v1303 fixture creates its original assembly, genuine issued
;; handler, and committed before/after embedded-XTDB observations exactly once.
;; Its use here is intentional: no fabricated abstract report is substituted
;; for a real source-bound runtime observation.
(use-fixtures
  :once
  (fn [run]
    (let [once-fixture
          (first (:clojure.test/once-fixtures
                  (meta (the-ns 'gesso.assurance.observed-correspondence-test))))]
      (assert (ifn? once-fixture) "The permanent XTDB fixture is required")
      (once-fixture run))))

(deftest original-induction-candidate-never-admits-itself
  (let [r (diagnostic-only :induction (diagnose :induction catalog))]
    (is (= :current-original-source-rechecked (:checking r)))
    (is (= :unadmitted-source-derived-abstract (:evidence-class r)))
    (is (= :admission (blocker-class r)))
    (is (= :conditional-induction-not-admitted (blocker-code r)))
    (is (= [:source] (blocker-path r)))
    (is (seq (:source-fingerprints r)))
    (is (seq (:original-open-obligation-ids-by-operation r)))))

(deftest weak-initializer-is-reported-before-admission
  (let [changed (assoc catalog :initial-condition
                       {:readers #{} :expression [:literal true]})
        r (diagnostic-only :induction (diagnose :induction changed))]
    (is (= :abstract-induction (blocker-class r)))
    (is (= :initial-consequence-open (blocker-code r)))
    (is (= :request/status-domain
           (get-in r [:first-blocker :invariant-id])))
    (is (= [:source :invariants :request/status-domain]
           (blocker-path r)))
    (is (= [:initial-state :request/status-domain]
           (get-in r [:first-blocker :obligation-id])))))

(deftest missing-initial-invariant-is-selected-deterministically
  (let [changed (assoc-in catalog [:invariants :request/needs-claim]
                          {:readers #{:request/status}
                           :expression [:= [:read :request/status]
                                        [:literal :claimed]]})
        a (diagnose :induction changed)
        b (diagnose :induction (update changed :invariants
                                       #(into (array-map) (reverse (seq %)))))]
    (diagnostic-only :induction a)
    (is (= (:first-blocker a) (:first-blocker b)))
    (is (= :initial-consequence-open (blocker-code a)))
    (is (= :request/needs-claim
           (get-in a [:first-blocker :invariant-id])))))

(deftest open-preservation-points-to-original-operation
  (let [changed (-> catalog
                    (assoc-in [:invariants :request/always-open]
                              {:readers #{:request/status}
                               :expression [:= [:read :request/status]
                                            [:literal :open]]})
                    (assoc :initial-condition
                           {:readers #{:request/status :request/version}
                            :expression [:and
                                         [:= [:read :request/status]
                                          [:literal :open]]
                                         [:>= [:read :request/version]
                                          [:literal 0]]]}))
        r (diagnostic-only :induction (diagnose :induction changed))]
    (is (= :abstract-induction (blocker-class r)))
    (is (= :preservation-open (blocker-code r)))
    (is (= :request/always-open (get-in r [:first-blocker :invariant-id])))
    (is (= :request/claim (get-in r [:first-blocker :operation-id])))
    (is (= [:source :operations 0 :construction] (blocker-path r)))))

(deftest source-change-produces-fresh-identity-not-reused-report
  (let [original (diagnose :induction catalog)
        changed (diagnose :induction
                          (assoc-in catalog [:operations 0 :construction :guard
                                              :expression]
                                    [:= [:read :request/status]
                                     [:literal :claimed]]))]
    (diagnostic-only :induction changed)
    (is (not= (:source-fingerprints original)
              (:source-fingerprints changed)))
    (is (not= original changed))
    (is (= :none (:authority changed)))))

(deftest malformed-catalog-rejected-before-inference
  (let [r (diagnostic-only :induction (diagnose :induction {:foo :bar}))]
    (is (= :source-validation-rejected (:checking r)))
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))
    (is (= [:source] (blocker-path r)))
    (is (keyword? (get-in r [:first-blocker :upstream-error-type])))
    (is (keyword? (get-in r [:first-blocker :reason])))
    (is (contains? (:open-obligations r)
                   :original-source-validation-failed))))

(deftest source-only-report-denies-certificate-admission
  (let [r (diagnostic-only :inductive-report
                           (diagnose :inductive-report
                                     (source-only-report catalog)))]
    (is (= :source-relative-conditional-induction (:evidence-class r)))
    (is (= :admission (blocker-class r)))
    (is (= :conditional-induction-certificate-not-admitted
           (blocker-code r)))
    (is (= [:source :certificate-handle] (blocker-path r)))
    (is (seq (:original-open-obligation-ids-by-operation r)))
    (is (seq (:source-fingerprints r)))))

(deftest source-only-report-prioritizes-open-base
  (let [changed (assoc catalog :initial-condition
                       {:readers #{} :expression [:literal true]})
        r (diagnostic-only :inductive-report
                           (diagnose :inductive-report
                                     (source-only-report changed)))]
    (is (= :abstract-induction (blocker-class r)))
    (is (= :initial-consequence-open (blocker-code r)))
    (is (= [:source :catalog :invariants :request/status-domain]
           (blocker-path r)))))

(deftest source-only-report-prioritizes-open-preservation
  (let [changed (-> catalog
                    (assoc-in [:invariants :request/always-open]
                              {:readers #{:request/status}
                               :expression [:= [:read :request/status]
                                            [:literal :open]]})
                    (assoc :initial-condition
                           {:readers #{:request/status :request/version}
                            :expression [:and
                                         [:= [:read :request/status]
                                          [:literal :open]]
                                         [:>= [:read :request/version]
                                          [:literal 0]]]}))
        r (diagnostic-only :inductive-report
                           (diagnose :inductive-report
                                     (source-only-report changed)))]
    (is (= :abstract-induction (blocker-class r)))
    (is (= :preservation-open (blocker-code r)))
    (is (= :request/claim (get-in r [:first-blocker :operation-id])))
    (is (= [:source :catalog :operations 0 :construction]
           (blocker-path r)))))

(deftest incomplete-certificate-source-cannot-be-laundered
  (let [r (diagnostic-only :inductive-report
                           (diagnose :inductive-report
                                     (assoc (source-only-report catalog)
                                            :certificate-handle {:proof :forged}))) ]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))
    (is (= :source-validation-rejected (:checking r)))))

(deftest diagnostic-request-has-closed-shape
  (doseq [request [nil [] {} {:source catalog}
                   {:kind :unknown :source catalog}
                   {:kind :induction :source catalog :authority :certified}
                   {:kind :induction :source catalog :report {:status :success}}
                   (with-meta {:kind :induction :source catalog}
                     {:trusted true})
                   (sorted-map :kind :induction :source catalog)]]
    (let [e (failure #(diagnostic/diagnose! request))]
      (is (= :gesso.assurance.diagnostic/invalid-diagnostic-input
             (:error/type e)) (pr-str request))
      (is (= :invalid-request-shape (:reason e)) (pr-str request))
      (is (= [] (:at e))))))

(deftest caller-supplied-proof-report-is-not-a-source
  (let [copied (induction/check-induction! catalog)
        r (diagnostic-only :induction
                           (diagnose :induction
                                     (assoc copied :authority :universal-proof)))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))
    (is (= :source-validation-rejected (:checking r)))))

(deftest diagnostic-report-is-copyable-and-not-a-credential
  (let [r (diagnose :induction catalog)
        forged (assoc r :authority :certified
                      :production-certification :established
                      :first-blocker {:code :none})
        again (diagnostic-only :induction (diagnose :induction catalog))]
    (is (= r again))
    (is (not= forged again))
    (is (= :none (:authority again)))
    (is (= :conditional-induction-not-admitted (blocker-code again)))))

(deftest every-kind-rejects-public-checker-substitution
  (doseq [[kind public] [[:induction #'induction/check-induction!]
                         [:inductive-report #'inductive-report/build-inductive-report!]
                         [:application-induction #'application/check-application-induction!]
                         [:observed-correspondence #'observed/check-observed-correspondence!]
                         [:finite-model #'finite/check-finite-model!]]]
    (let [e (with-redefs-fn {public (fn [_] {:type diagnostic/diagnostic-type
                                            :status :certified
                                            :authority :write})}
              #(failure (fn [] (diagnose kind nil))))]
      (is (= :gesso.assurance.diagnostic/invalid-diagnostic-input
             (:error/type e)) (str kind))
      (is (= :substituted-upstream-checker (:reason e)) (str kind))
      (is (= [:kind] (:at e))))))

(deftest source-identity-is-captured-not-invented
  (let [r (diagnostic-only :induction (diagnose :induction catalog))
        report (induction/check-induction! catalog)]
    (is (= (:catalog-fingerprint report)
           (get-in r [:source-fingerprints :catalog-fingerprint])))
    (is (= (:induction-fingerprint report)
           (get-in r [:source-fingerprints :induction-fingerprint])))))

(deftest finite-enumeration-is-bounded-even-if-exhaustive
  (let [r (diagnostic-only :finite-model
                           (diagnose :finite-model finite-source))]
    (is (= :bounded-finite-universe-analysis (:evidence-class r)))
    (is (= :finite-only (blocker-class r)))
    (is (= :finite-model-does-not-prove-universal-induction
           (blocker-code r)))
    (is (= [:source :state-domains] (blocker-path r)))
    (is (= false (get-in r [:first-blocker :all-type-domains-complete?])))
    (is (contains? (:open-obligations r)
                   :finite-model-does-not-prove-universal-induction))))

(deftest finite-base-counterexample-keeps-concrete-witness
  (let [s (-> finite-source
              (assoc-in [:catalog :invariants :request/status-domain
                         :expression]
                        [:= [:read :request/status] [:literal :open]])
              (assoc-in [:catalog :initial-condition]
                        {:readers #{} :expression [:literal true]}))
        r (diagnostic-only :finite-model (diagnose :finite-model s))
        w (get-in r [:first-blocker :counterexample])]
    (is (= :finite-counterexample (blocker-class r)))
    (is (= :initial-counterexample (blocker-code r)))
    (is (= [:source :catalog :invariants :request/status-domain]
           (blocker-path r)))
    (is (= :base (:kind w)))
    (is (= :request/status-domain (:invariant-id w)))
    (is (map? (:state w)))))

(deftest finite-step-counterexample-keeps-operation-and-poststate
  (let [s (assoc-in finite-source
                    [:catalog :invariants :request/status-domain :expression]
                    [:= [:read :request/status] [:literal :open]])
        s (assoc-in s [:catalog :initial-condition]
                    {:readers #{:request/status}
                     :expression [:= [:read :request/status] [:literal :open]]})
        r (diagnostic-only :finite-model (diagnose :finite-model s))
        w (get-in r [:first-blocker :counterexample])]
    (is (= :finite-counterexample (blocker-class r)))
    (is (= :preservation-counterexample (blocker-code r)))
    (is (= [:source :catalog :operations] (blocker-path r)))
    (is (= :step (:kind w)))
    (is (= :request/claim (:operation-id w)))
    (is (= :open (get-in w [:prestate :request/status])))
    (is (= :claimed (get-in w [:poststate :request/status])))))

(deftest finite-counterexample-is-stable-on-rechecking
  (let [s (assoc-in finite-source
                    [:catalog :invariants :request/status-domain :expression]
                    [:= [:read :request/status] [:literal :open]])
        first-run (diagnose :finite-model s)
        second-run (diagnose :finite-model s)]
    (is (= (:first-blocker first-run) (:first-blocker second-run)))
    (is (= (:open-obligations first-run)
           (:open-obligations second-run)))))

(deftest finite-budget-exhaustion-is-inconclusive
  (let [r (diagnostic-only :finite-model
                           (diagnose :finite-model
                                     (assoc finite-source :max-cases 1)))]
    (is (= :finite-inconclusive (blocker-class r)))
    (is (= :finite-enumeration-budget-exceeded (blocker-code r)))
    (is (= [:source :max-cases] (blocker-path r)))
    (is (keyword? (get-in r [:first-blocker :reason])))))

(deftest finite-unsupported-universe-is-not-a-success
  (let [s (assoc-in finite-source [:state-domains :request/status] [:open])
        r (diagnostic-only :finite-model (diagnose :finite-model s))]
    (is (= :finite-inconclusive (blocker-class r)))
    (is (= :finite-semantics-unsupported (blocker-code r)))
    (is (= :not-established (:universal-xtdb-refinement r)))
    (is (keyword? (get-in r [:first-blocker :reason])))))

(deftest finite-malformed-domains-are-located
  (let [r (diagnostic-only :finite-model
                           (diagnose :finite-model
                                     (assoc finite-source :state-domains {})))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :finite-input-invalid (blocker-code r)))
    (is (= :source-validation-rejected (:checking r)))
    (is (vector? (blocker-path r)))
    (is (= :none (:authority r)))))

(deftest finite-impossible-initial-state-is-inconclusive
  (let [s (assoc-in finite-source [:catalog :initial-condition]
                    {:readers #{}
                     :expression [:literal false]})
        r (diagnostic-only :finite-model (diagnose :finite-model s))]
    (is (= :finite-inconclusive (blocker-class r)))
    (is (= :finite-semantics-unsupported (blocker-code r)))
    (is (= :none (:transaction-authority r)))))

(deftest finite-report-cannot-be-resubmitted-as-original-source
  (let [report (finite/check-finite-model! finite-source)
        r (diagnostic-only :finite-model
                           (diagnose :finite-model
                                     (assoc report :authority :certified)))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :finite-input-invalid (blocker-code r)))))

(deftest narrowing-finite-samples-never-produces-proof-authority
  (let [s (-> crosscheck-source
              (assoc-in [:catalog :initial-condition]
                        {:readers #{} :expression [:literal true]})
              (assoc-in [:state-domains :request/version] [0 1]))
        r (diagnostic-only :finite-model (diagnose :finite-model s))]
    (is (= :finite-only (blocker-class r)))
    (is (= :finite-model-does-not-prove-universal-induction
           (blocker-code r)))
    (is (= :none (:authority r)))
    (is (= false (get-in r [:first-blocker :all-type-domains-complete?])))))

(deftest source-only-explanations-preserve-assumptions
  (let [a (diagnostic-only :induction (diagnose :induction catalog))
        b (diagnostic-only :inductive-report
                           (diagnose :inductive-report
                                     (source-only-report catalog)))]
    (is (seq (:original-open-obligation-ids-by-operation a)))
    (is (= (:original-open-obligation-ids-by-operation a)
           (:original-open-obligation-ids-by-operation b)))
    (is (every? (:open-obligations a) mandatory-open))
    (is (every? (:open-obligations b) mandatory-open))
    (is (set? (:open-premises a)))
    (is (set? (:open-premises b)))))

(deftest source-only-report-is-deterministic
  (let [s (source-only-report catalog)
        a (diagnose :inductive-report s)
        b (diagnose :inductive-report s)]
    (is (= a b))
    (is (= (:source-fingerprints a) (:source-fingerprints b)))
    (is (= :conditional-induction-certificate-not-admitted
           (blocker-code b)))))

(deftest valid-application-inventory-still-has-physical-blocker
  (let [r (diagnostic-only :application-induction
                           (diagnose :application-induction
                                     (:application-source (context))))]
    (is (= :source-relative-application-assembly (:evidence-class r)))
    (is (= :physical-correspondence (blocker-class r)))
    (is (= :physical-write-path-unproved (blocker-code r)))
    (is (= [:source :authoritative-coverage-source
            :enumerate-entry-points!] (blocker-path r)))
    (is (seq (:source-fingerprints r)))))

(deftest unclassified-host-entry-point-precedes-physical-proof-gap
  (let [r (diagnostic-only :application-induction
                           (diagnose :application-induction
                                     (application-with-extras
                                      #{:example/other}))) ]
    (is (= :entry-point-coverage (blocker-class r)))
    (is (= :unclassified-entry-point (blocker-code r)))
    (is (= :example/other (get-in r [:first-blocker :entry-point-id])))
    (is (= [:source :authoritative-coverage-source
            :enumerate-entry-points!] (blocker-path r)))))

(deftest multiple-unclassified-entries-are-ordered-by-identifier
  (let [r (diagnostic-only :application-induction
                           (diagnose :application-induction
                                     (application-with-extras
                                      #{:example/zed :example/alpha}))) ]
    (is (= :entry-point-coverage (blocker-class r)))
    (is (= :example/alpha (get-in r [:first-blocker :entry-point-id])))
    (is (contains? (:open-obligations r)
                   :host-authoritative-entry-point-inventory-is-complete))))

(deftest missing-host-route-is-invalid-source-not-unclassified
  (let [app (:application-source (context))
        bad (assoc-in app [:authoritative-coverage-source
                           :enumerate-entry-points!] (fn [] {}))
        r (diagnostic-only :application-induction
                           (diagnose :application-induction bad))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))
    (is (= :source-validation-rejected (:checking r)))
    (is (keyword? (get-in r [:first-blocker :reason])))))

(deftest deployed-handler-substitution-is-invalid-source
  (let [app (:application-source (context))
        bad (assoc-in app [:authoritative-coverage-source
                           :inductive-contract-source :semantic-route-handlers
                           :example/claim] (fn [_] nil))
        r (diagnostic-only :application-induction
                           (diagnose :application-induction bad))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-validation-rejected (:checking r)))
    (is (= :none (:authority r)))))

(deftest observed-conforming-step-is-still-unauthenticated
  (let [r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence
                                     (observed-source (:conforming (context)))))]
    (is (= :unauthenticated-observed-application-step (:evidence-class r)))
    (is (= :observation-provenance (blocker-class r)))
    (is (= :physical-observation-not-authenticated (blocker-code r)))
    (is (= [:source :transition-source :observation] (blocker-path r)))
    (is (= :example/claim (get-in r [:first-blocker :operation-id])))
    (is (= :not-established (:universal-xtdb-refinement r)))))

(deftest observed-committed-wrong-write-takes-precedence
  (let [r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence
                                     (observed-source (:divergent (context)))))]
    (is (= :observed-transition (blocker-class r)))
    (is (= :physical-transition-diverges (blocker-code r)))
    (is (= [:source :transition-source :observation] (blocker-path r)))
    (is (keyword? (get-in r [:first-blocker :failed-check])))
    (is (= :example/claim (get-in r [:first-blocker :operation-id])))))

(deftest observed-wrong-abstract-guard-is-invalid-source
  (let [transition (:conforming (context))
        bad (assoc-in transition [:construction :guard :expression]
                      [:literal false])
        r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence
                                     (observed-source bad)))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))
    (is (= :source-validation-rejected (:checking r)))))

(deftest copied-physical-report-is-not-an-observed-transition-source
  (let [source (observed-source (:conforming (context)))
        report (observed/check-observed-correspondence! source)
        r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence
                                     (assoc source :transition-source report)))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :source-check-rejected (blocker-code r)))))

(deftest matching-witness-copy-cannot-prove-physical-authenticity
  (let [source (observed-source (:conforming (context)))
        copied (assoc-in source [:transition-source :observation :authority]
                         :certified)
        baseline (diagnose :observed-correspondence source)
        r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence copied))]
    (is (= :invalid-source (blocker-class r)))
    (is (= :observation-provenance (blocker-class baseline)))
    (is (= :not-established (:universal-xtdb-refinement baseline)))))

(deftest observed-priority-does-not-hide-app-entry-points
  (let [app (application-with-extras #{:example/other})
        src {:application-source app
             :transition-source (:conforming (context))}
        r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence src))]
    (is (= :entry-point-coverage (blocker-class r)))
    (is (= :unclassified-entry-point (blocker-code r)))
    (is (= :example/other (get-in r [:first-blocker :entry-point-id])))))

(deftest observed-physical-divergence-precedes-unclassified-entrypoint
  (let [app (application-with-extras #{:example/other})
        src {:application-source app
             :transition-source (:divergent (context))}
        r (diagnostic-only :observed-correspondence
                           (diagnose :observed-correspondence src))]
    (is (= :observed-transition (blocker-class r)))
    (is (= :physical-transition-diverges (blocker-code r)))
    (is (keyword? (get-in r [:first-blocker :failed-check])))))

(deftest checked-diagnostics-never-grant-authority
  (doseq [[kind source] [[:induction catalog]
                         [:inductive-report (source-only-report catalog)]
                         [:application-induction (:application-source (context))]
                         [:observed-correspondence
                          (observed-source (:conforming (context)))]
                         [:finite-model finite-source]]]
    (let [r (diagnostic-only kind (diagnose kind source))]
      (is (= :none (:authority r)))
      (is (= :none (:transaction-authority r)))
      (is (contains? (:open-obligations r)
                     :independent-induction-checker-soundness)))))
