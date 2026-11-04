(ns gesso.assurance.proof.initial-test
  "v1271 permanent adversarial tests for the v1270 source-derived initial
   consequence checker.  These tests distinguish syntactic entailment from
   satisfiability, induction base discharge, physical initialization, and
   application certification.  Original v1241 sources are the ONLY inputs."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.initial :as initial]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private operation
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}
    :request/organization {:type :keyword}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note :request/organization}})

(def ^:private status-open
  [:= [:read :request/status] [:literal :open]])
(def ^:private version-nonnegative
  [:>= [:read :request/version] [:literal 0]])
(def ^:private organization-known
  [:not= [:read :request/organization] [:literal :organization/missing]])
(def ^:private note-present
  [:present [:read :request/note]])
(def ^:private note-nonnull
  [:not= [:read :request/note] [:literal nil]])
(def ^:private optional-safe
  [:and note-present note-nonnull])
(def ^:private status-domain
  [:in [:read :request/status] [:literal #{:open :claimed :done}]])
(def ^:private negative-version
  [:< [:read :request/version] [:literal 0]])
(def ^:private initial-expression
  [:and status-open [:and version-nonnegative optional-safe] organization-known])

(def ^:private baseline
  {:construction operation
   :initial-condition
   {:readers #{:request/status :request/version :request/note
               :request/organization}
    :expression initial-expression}
   :invariants
   {:request/status-open {:readers #{:request/status} :expression status-open}
    :request/version-safe {:readers #{:request/version}
                           :expression version-nonnegative}
    :request/optional-safe {:readers #{:request/note}
                            :expression optional-safe}
    :request/organization-known {:readers #{:request/organization}
                                 :expression organization-known}
    :request/status-domain {:readers #{:request/status}
                            :expression status-domain}
    :request/always-true {:readers #{} :expression [:literal true]}
    :request/always-false {:readers #{} :expression [:literal false]}}})

(defn- check! [source] (initial/check-initial-consequence! source))
(defn- outcome [report id]
  (first (filter #(= id (:invariant %)) (:initial-outcomes report))))
(defn- checked? [report id]
  (= :checked-conditional-inference (:status (outcome report id))))
(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- reject! [f expected-type reason]
  (let [result (failure f)]
    (is (map? result) (str "Expected failure " expected-type "/" reason))
    (is (= expected-type (:error/type result)) (pr-str result))
    (is (= reason (:reason result)) (pr-str result))
    result))
(defn- obligation-ids [source]
  (:open-obligation-ids (obligation/derive-obligations! source)))
(defn- with-initial [expression readers]
  (assoc baseline :initial-condition
         {:readers readers :expression expression}))
(defn- with-invariant [id expression readers]
  (assoc-in baseline [:invariants id]
            {:readers readers :expression expression}))

(deftest original-source-produces-complete-typed-report
  (let [r (check! baseline)
        ledger (obligation/derive-obligations! baseline)
        expected (set (map #(vector :initial-state %)
                           (keys (:invariants baseline))))
        checked (:conditionally-checked-initial-ids r)
        unsupported (:unsupported-initial-ids r)]
    (is (= initial/report-type (:type r)))
    (is (= initial/initial-version (:version r)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint r)))
    (is (= (:fingerprint ledger) (:obligation-fingerprint r)))
    (is (= (:transition-fingerprint ledger) (:transition-fingerprint r)))
    (is (= (:subject-fingerprint ledger) (:subject-fingerprint r)))
    (is (= expected (set (map :obligation-id (:initial-outcomes r)))))
    (is (= (count expected) (count (:initial-outcomes r))))
    (is (= (count expected)
           (count (distinct (map :obligation-id (:initial-outcomes r))))))
    (is (= expected (set/union checked unsupported)))
    (is (empty? (set/intersection checked unsupported)))
    (is (= #{[:initial-state :request/status-open]
             [:initial-state :request/version-safe]
             [:initial-state :request/optional-safe]
             [:initial-state :request/organization-known]
             [:initial-state :request/always-true]}
           checked))
    (is (= #{[:initial-state :request/status-domain]
             [:initial-state :request/always-false]}
           unsupported))
    (is (= (obligation-ids baseline) (:original-open-obligation-ids r)))
    (is (= :typed-structural-initial-consequence (:proof-rule r)))
    (is (= :declared-abstract-state-projection-only (:scope r)))
    (is (= :initial-condition-implies-invariant-for-all-declared-states
           (:claim r)))
    (is (= :original-source-typed-rules-and-exact-conjunct-structure
           (:checking r)))
    (is (= :restricted-conditional-initial-consequence-checked
           (:proof-status r)))
    (is (false? (:initial-condition-manifestly-false? r)))))

(deftest every-outcome-has-exact-source-bound-identity
  (let [r (check! baseline)]
    (doseq [[id rule] (:invariants baseline)]
      (let [o (outcome r id)]
        (is (= [:initial-state id] (:obligation-id o)))
        (is (= id (:invariant o)))
        (is (= (:readers rule) (:readers o)))
        (is (#{:checked-conditional-inference :open} (:status o)))
        (is (= (checked? r id)
               (contains? (:conditionally-checked-initial-ids r)
                          [:initial-state id])))))))

(deftest nested-required-conjunction-paths-are-preserved
  (let [r (check! baseline)]
    (doseq [[id expected]
            {:request/status-open [1]
             :request/version-safe [2 1]
             :request/optional-safe [2 2]
             :request/organization-known [3]}]
      (let [o (outcome r id)]
        (is (= :required-conjunct-elimination (:rule o)))
        (is (= expected (:conjunct-path o)))
        (is (checked? r id))))))

(deftest identical-whole-initial-predicate-is-checked
  (let [s (with-invariant :request/whole initial-expression
                          #{:request/status :request/version :request/note
                            :request/organization})
        r (check! s)]
    (is (= :identical-initial-rule (:rule (outcome r :request/whole))))
    (is (= [] (:conjunct-path (outcome r :request/whole))))
    (is (checked? r :request/whole))))

(deftest only-root-identical-rule-receives-identical-rule-label
  (let [s (with-initial status-open #{:request/status})
        r (check! s)]
    (is (= :identical-initial-rule (:rule (outcome r :request/status-open))))
    (is (= [] (:conjunct-path (outcome r :request/status-open))))
    (is (not (checked? r :request/version-safe)))))

(deftest closed-literal-true-is-supported-without-an-initial-reader
  (let [r (check! baseline)
        o (outcome r :request/always-true)]
    (is (= :literal-true-introduction (:rule o)))
    (is (nil? (:conjunct-path o)))
    (is (= #{} (:readers o)))
    (is (checked? r :request/always-true))))

(deftest literal-true-occurring-as-required-node-has-actual-path
  (let [s (with-initial [:and status-open [:literal true]] #{:request/status})
        r (check! s)
        o (outcome r :request/always-true)]
    (is (= :required-conjunct-elimination (:rule o)))
    (is (= [2] (:conjunct-path o)))))

(deftest literal-true-as-entire-initial-gets-root-identity
  (let [r (check! (with-initial [:literal true] #{}))]
    (is (= :identical-initial-rule
           (:rule (outcome r :request/always-true))))
    (is (= [] (:conjunct-path (outcome r :request/always-true))))
    (is (= :open (:status (outcome r :request/status-open))))))

(deftest closed-literal-false-is-never-promoted-to-true
  (let [o (outcome (check! baseline) :request/always-false)]
    (is (= :open (:status o)))
    (is (= :unsupported-initial-implication (:reason o)))
    (is (nil? (:rule o)))
    (is (nil? (:conjunct-path o)))))

(deftest matching-child-inside-or-is-not-a-required-conjunct
  (let [s (with-initial [:or status-open version-nonnegative]
                         #{:request/status :request/version})
        r (check! s)]
    (is (not (checked? r :request/status-open)))
    (is (not (checked? r :request/version-safe)))
    (is (= :unsupported-initial-implication
           (:reason (outcome r :request/status-open))))))

(deftest matching-child-inside-implies-is-not-a-required-conjunct
  (let [s (with-initial [:implies status-open version-nonnegative]
                         #{:request/status :request/version})
        r (check! s)]
    (is (not (checked? r :request/status-open)))
    (is (not (checked? r :request/version-safe)))))

(deftest matching-child-inside-not-is-not-a-required-conjunct
  (let [s (with-initial [:not status-open] #{:request/status})
        r (check! s)]
    (is (not (checked? r :request/status-open)))
    (is (= :unsupported-initial-implication
           (:reason (outcome r :request/status-open))))))

(deftest nested-nonconjunct-children-never-leak-to-required-nodes
  (let [s (with-initial [:and organization-known
                          [:or status-open version-nonnegative]]
                         #{:request/organization :request/status
                           :request/version})
        r (check! s)]
    (is (checked? r :request/organization-known))
    (is (= [1] (:conjunct-path (outcome r :request/organization-known))))
    (is (not (checked? r :request/status-open)))
    (is (not (checked? r :request/version-safe)))))

(deftest nested-implication-under-and-does-not-leak-consequent
  (let [s (with-initial [:and organization-known
                          [:implies status-open version-nonnegative]]
                         #{:request/organization :request/status
                           :request/version})
        r (check! s)]
    (is (checked? r :request/organization-known))
    (is (= :open (:status (outcome r :request/version-safe))))))

(deftest logically-equivalent-but-different-expression-is-open
  (let [s (with-invariant :request/status-open
                          [:not [:not status-open]] #{:request/status})
        r (check! s)]
    (is (= :open (:status (outcome r :request/status-open))))
    (is (= :unsupported-initial-implication
           (:reason (outcome r :request/status-open))))))

(deftest arithmetic-consequence-is-not-proved-by-structural-checker
  (let [s (with-invariant :request/version-safe
                          [:>= [:read :request/version] [:literal -1]]
                          #{:request/version})
        r (check! s)]
    (is (= :open (:status (outcome r :request/version-safe))))))

(deftest membership-consequence-is-not-proved-from-single-equality
  (let [r (check! baseline)]
    (is (= :open (:status (outcome r :request/status-domain))))
    (is (= :unsupported-initial-implication
           (:reason (outcome r :request/status-domain))))))

(deftest explicit-false-root-refuses-vacuous-base-case
  (let [r (check! (with-initial [:literal false] #{}))]
    (is (true? (:initial-condition-manifestly-false? r)))
    (is (empty? (:conditionally-checked-initial-ids r)))
    (is (= (set (map #(vector :initial-state %)
                     (keys (:invariants baseline))))
           (:unsupported-initial-ids r)))
    (doseq [o (:initial-outcomes r)]
      (is (= :open (:status o)))
      (is (= :manifestly-false-initial-condition (:reason o)))
      (is (nil? (:rule o))))))

(deftest explicit-false-required-child-refuses-all-inferences
  (let [s (with-initial [:and status-open [:literal false]
                          version-nonnegative]
                         #{:request/status :request/version})
        r (check! s)]
    (is (true? (:initial-condition-manifestly-false? r)))
    (is (empty? (:conditionally-checked-initial-ids r)))
    (is (= :manifestly-false-initial-condition
           (:reason (outcome r :request/always-true))))
    (is (= :manifestly-false-initial-condition
           (:reason (outcome r :request/status-open))))))

(deftest false-in-disjunction-is-not-mistaken-for-required-false
  (let [s (with-initial [:or status-open [:literal false]]
                         #{:request/status})
        r (check! s)]
    (is (false? (:initial-condition-manifestly-false? r)))
    (is (checked? r :request/always-true))
    (is (= :open (:status (outcome r :request/status-open))))))

(deftest false-in-implication-is-not-mistaken-for-required-false
  (let [s (with-initial [:implies status-open [:literal false]]
                         #{:request/status})
        r (check! s)]
    (is (false? (:initial-condition-manifestly-false? r)))
    (is (checked? r :request/always-true))
    (is (= :open (:status (outcome r :request/status-open))))))

(deftest nontrivial-contradiction-does-not-claim-satisfiability
  (let [s (with-initial [:and status-open
                          [:= [:read :request/status] [:literal :claimed]]]
                         #{:request/status})
        r (check! s)]
    (is (false? (:initial-condition-manifestly-false? r)))
    (is (checked? r :request/status-open))
    (is (= :not-established (:initial-state-existence r)))
    (is (= :not-established (:initial-state-coverage r)))))

(deftest optional-presence-and-value-must-appear-as-complete-conjunct
  (let [r (check! baseline)]
    (is (checked? r :request/optional-safe))
    (is (= :required-conjunct-elimination
           (:rule (outcome r :request/optional-safe))))
    (is (= [2 2] (:conjunct-path (outcome r :request/optional-safe))))))

(deftest optional-unsafe-read-rejected-before-entailment
  (let [s (with-invariant :request/optional-safe note-nonnull
                          #{:request/note})]
    (reject! #(check! s)
             :gesso.assurance.rule-type/invalid-rule-type
             :unguarded-optional-reader)))

(deftest optional-guard-order-is-not-commutative
  (let [s (with-invariant :request/optional-safe
                          [:and note-nonnull note-present]
                          #{:request/note})]
    (reject! #(check! s)
             :gesso.assurance.rule-type/invalid-rule-type
             :unguarded-optional-reader)))

(deftest optional-nil-and-absent-remain-distinct-in-concrete-evaluation
  (let [declaration {:readers #{:request/note} :expression optional-safe}
        types {:request/note {:type :string :optional? true :nullable? true}}]
    (doseq [[bindings expected]
            [[{} false]
             [{:request/note nil} false]
             [{:request/note "memo"} true]
             [{:request/note ""} true]]]
      (is (= expected (:value (rule-eval/evaluate-rule!
                              declaration types bindings)))))))

(deftest removing-conjunct-opens-its-former-invariant
  (let [s (with-initial [:and status-open organization-known]
                         #{:request/status :request/organization})
        r (check! s)]
    (is (checked? r :request/status-open))
    (is (not (checked? r :request/version-safe)))
    (is (not (checked? r :request/optional-safe)))))

(deftest changing-conjunct-order-changes-identity-and-paths
  (let [s (with-initial [:and organization-known status-open
                          [:and version-nonnegative optional-safe]]
                         #{:request/status :request/version :request/note
                           :request/organization})
        before (check! baseline)
        after (check! s)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (not= (:obligation-fingerprint before)
              (:obligation-fingerprint after)))
    (is (= [2] (:conjunct-path (outcome after :request/status-open))))
    (is (= [3 2] (:conjunct-path (outcome after :request/optional-safe))))))

(deftest adding-true-conjunct-still-invalidates-source-identity
  (let [s (update-in baseline [:initial-condition :expression]
                     #(vector :and % [:literal true]))
        a (check! baseline)
        b (check! s)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (checked? b :request/status-open))))

(deftest changed-invariant-identity-invalidates-complete-ledger
  (let [s (-> baseline
              (update :invariants dissoc :request/status-open)
              (assoc-in [:invariants :request/renamed]
                        {:readers #{:request/status} :expression status-open}))
        a (check! baseline)
        b (check! s)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (checked? b :request/renamed))
    (is (not (contains? (:conditionally-checked-initial-ids b)
                        [:initial-state :request/status-open])))))

(deftest changed-invariant-rule-removes-old-support
  (let [s (with-invariant :request/status-open status-domain
                          #{:request/status})
        a (check! baseline)
        b (check! s)]
    (is (checked? a :request/status-open))
    (is (not (checked? b :request/status-open)))
    (is (not= (:fingerprint a) (:fingerprint b)))))

(deftest changed-initial-reader-types-require-fresh-proof
  (let [s (assoc-in baseline [:construction :state-types
                              :request/version :type] :string)]
    (reject! #(check! s)
             :gesso.assurance.rule-type/invalid-rule-type
             :expected-nonnullable-integers)))

(deftest changed-operation-guard-changes-proof-identity
  (let [s (assoc-in baseline [:construction :guard]
                    {:readers #{:request/status}
                     :expression [:= [:read :request/status]
                                  [:literal :claimed]]})
        a (check! baseline)
        b (check! s)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:transition-fingerprint a)
              (:transition-fingerprint b)))
    (is (not= (:obligation-fingerprint a)
              (:obligation-fingerprint b)))))

(deftest changed-operation-assignment-changes-proof-identity
  (let [s (assoc-in baseline [:construction :replacements :request/status]
                    [:literal :done])
        a (check! baseline)
        b (check! s)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:transition-fingerprint a)
              (:transition-fingerprint b)))))

(deftest changing-subject-identity-invalidates-proof
  (let [s (assoc-in baseline [:construction :subject :id]
                    :example/claim-v2)
        a (check! baseline)
        b (check! s)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:subject-fingerprint a) (:subject-fingerprint b)))))

(deftest exact-source-repeat-deterministically-rechecks
  (let [a (check! baseline) b (check! baseline)]
    (is (= a b))
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:conditionally-checked-initial-ids a)
           (:conditionally-checked-initial-ids b)))))

(deftest cannot-submit-copied-initial-report-as-source
  (reject! #(check! (check! baseline))
           :gesso.assurance.proof.obligation/invalid-obligation-source
           :wrong-keys))

(deftest cannot-submit-copied-obligation-report-as-source
  (reject! #(check! (obligation/derive-obligations! baseline))
           :gesso.assurance.proof.obligation/invalid-obligation-source
           :wrong-keys))

(deftest invented-authorization-or-proof-flag-in-source-is-rejected
  (doseq [flag [:proved? :certificate :admission :authority
                :initial-obligation-discharged?]]
    (reject! #(check! (assoc baseline flag true))
             :gesso.assurance.proof.obligation/invalid-obligation-source
             :wrong-keys)))

(deftest missing-required-source-key-is-rejected
  (doseq [k [:construction :initial-condition :invariants]]
    (reject! #(check! (dissoc baseline k))
             :gesso.assurance.proof.obligation/invalid-obligation-source
             :wrong-keys)))

(deftest empty-invariant-set-is-rejected-not-vacuously-proved
  (reject! #(check! (assoc baseline :invariants {}))
           :gesso.assurance.proof.obligation/invalid-obligation-source
           :invariant-count))

(deftest invented-status-in-invariant-source-is-rejected
  (reject! #(check! (assoc-in baseline
                             [:invariants :request/status-open :status]
                             :proved))
           :gesso.assurance.proof.obligation/invalid-obligation-source
           :wrong-keys))

(deftest undeclared-state-reader-is-rejected
  (let [s (with-invariant :request/status-open
                          [:= [:read :request/missing] [:literal :open]]
                          #{:request/missing})]
    (reject! #(check! s)
             :gesso.assurance.proof.obligation/invalid-obligation-source
             :non-state-reader)))

(deftest input-reader-cannot-stand-in-for-an-initial-state-reader
  (let [s (with-invariant :request/status-open
                          [:= [:read :input/actor] [:literal :helper]]
                          #{:input/actor})]
    (reject! #(check! s)
             :gesso.assurance.proof.obligation/invalid-obligation-source
             :non-state-reader)))

(deftest forged-obligation-proof-status-is-rejected
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc (original s) :proof-status :proved))]
      (reject! #(check! baseline)
               :gesso.assurance.proof.initial/invalid-initial-consequence
               :obligation-checker-contract-changed))))

(deftest forged-obligation-open-id-set-is-rejected
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (update (original s) :open-obligation-ids
                                  disj [:initial-state :request/status-open]))]
      (reject! #(check! baseline)
               :gesso.assurance.proof.initial/invalid-initial-consequence
               :obligation-checker-contract-changed))))

(deftest forged-obligation-typed-invariant-is-rejected
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc-in (original s)
                                    [:invariants :request/status-open :expression]
                                    [:literal true]))]
      (reject! #(check! baseline)
               :gesso.assurance.proof.initial/invalid-initial-consequence
               :obligation-checker-contract-changed))))

(deftest forged-obligation-authority-is-rejected
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc (original s) :authority :production))]
      (reject! #(check! baseline)
               :gesso.assurance.proof.initial/invalid-initial-consequence
               :obligation-checker-contract-changed))))

(deftest initial-results-do-not-discharge-the-original-obligation-ledger
  (let [r (check! baseline)
        ledger (obligation/derive-obligations! baseline)]
    (is (= (:open-obligation-ids ledger) (:original-open-obligation-ids r)))
    (doseq [id (:conditionally-checked-initial-ids r)]
      (is (contains? (:original-open-obligation-ids r) id)))
    (doseq [id [:initial-state-existence :initial-state-coverage
                :checker-meta-soundness :declared-state-projection-coverage
                :declared-invariant-set-coverage]]
      (is (contains? (:original-open-obligation-ids r) [:assumption id])))
    (doseq [id [:production-transition-correspondence
                :current-runtime-authorization
                :atomic-transaction-enforcement
                :physical-effect-containment
                :runtime-type-correspondence
                :integer-semantics-correspondence]]
      (is (contains? (:original-open-obligation-ids r)
                     [:implementation id])))))

(deftest conditional-base-inference-has-no-authoritative-capability
  (let [r (check! baseline)]
    (is (= :not-established (:initial-state-existence r)))
    (is (= :not-established (:initial-state-coverage r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :none (:authority r)))
    (is (contains? (:open-meta-obligations r)
                   :initial-structural-inference-rule-soundness))
    (is (contains? (:open-meta-obligations r) :initial-state-existence))
    (is (contains? (:open-meta-obligations r) :initial-state-coverage))
    (is (contains? (:open-meta-obligations r) :certificate-admission))
    (is (not (contains? r :proved?)))
    (is (not (contains? r :transaction-authority)))))

(deftest finite-independent-oracle-for-nested-conjuncts
  ;; This is a bounded sanity oracle, NOT a proof of the universal implication.
  ;; All listed states satisfy the typed read footprints.  Optional absence
  ;; is intentionally excluded for this initial condition because it requires
  ;; a PRESENT non-null note; independently tested above.
  (let [r (check! baseline)
        types (select-keys (:state-types operation)
                           #{:request/status :request/version
                             :request/note :request/organization})
        init (:initial-condition baseline)]
    (doseq [status [:open :claimed :done]
            version [-50 -1 0 1 2 999999999999999999N]
            note [nil "hello" ""]
            organization [:organization/missing :organization/alpha]
            :let [bindings {:request/status status
                            :request/version version
                            :request/note note
                            :request/organization organization}
                  init-true? (:value (rule-eval/evaluate-rule! init types bindings))]]
      (when init-true?
        (doseq [[id invariant] (:invariants baseline)
                :when (checked? r id)]
          (is (= true
                 (:value (rule-eval/evaluate-rule!
                          invariant
                          (select-keys types (:readers invariant))
                          (select-keys bindings (:readers invariant)))))
              (str "Initial consequence failed for " id "/" bindings)))))))
