(ns gesso.assurance.proof.initial-constructor-test
  "v1273 permanent adversarial tests for source-derived typed literal initial
   constructors (v1272). Constructor checks are ONE concrete abstract case,
   NOT a universal Init => I derivation, complete constructor catalog, proof
   admission, authoritative initialization, or XTDB correspondence.

   Test exact field coverage, present nil versus absent optional values,
   nominal and numeric typing, original declared predicates, source freshness,
   tamper resistance, and non-authoritative evidence boundaries."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}
    :request/flag {:type :boolean :optional? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note :request/flag}})

(def ^:private initial-rule
  {:readers #{:request/status :request/owner :request/version}
   :expression [:and
                [:= [:read :request/status] [:literal :open]]
                [:= [:read :request/owner] [:literal nil]]
                [:>= [:read :request/version] [:literal 0]]]})

(def ^:private invariants
  {:request/allowed-status
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/version-safe
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/unclaimed-owner
   {:readers #{:request/owner}
    :expression [:= [:read :request/owner] [:literal nil]]}
   :request/note-absent
   {:readers #{:request/note}
    :expression [:not [:present [:read :request/note]]]}
   :request/true
   {:readers #{} :expression [:literal true]}
   :request/false
   {:readers #{} :expression [:literal false]}})

(def ^:private source
  {:construction claim
   :initial-condition initial-rule
   :invariants invariants})

(def ^:private baseline
  {:id :request/new
   :fields {:request/status [:literal :open]
            :request/owner [:literal nil]
            :request/version [:literal 0]
            :request/note [:absent]
            :request/flag [:absent]}})

(def ^:private expected-state
  {:request/status :open
   :request/owner nil
   :request/version 0})

(defn- check!
  ([ctor] (constructor/check-initial-constructor! source ctor))
  ([s ctor] (constructor/check-initial-constructor! s ctor)))

(defn- error-of [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejection!
  ([ctor error-type reason]
   (rejection! source ctor error-type reason))
  ([s ctor error-type reason]
   (let [err (error-of #(check! s ctor))]
     (is (map? err) (str "Expected typed rejection " reason))
     (is (= error-type (:error/type err)) (pr-str err))
     (is (= reason (:reason err)) (pr-str err))
     (is (vector? (:at err)) (pr-str err))
     err)))

(def ^:private constructor-error
  :gesso.assurance.proof.initial-constructor/invalid-initial-constructor)
(def ^:private evaluation-error
  :gesso.assurance.rule-eval/invalid-rule-evaluation)
(def ^:private rule-type-error
  :gesso.assurance.rule-type/invalid-rule-type)
(def ^:private obligation-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)

(defn- outcome [report id]
  (first (filter #(= id (:invariant %)) (:initial-case-outcomes report))))

(defn- with-field [id expression]
  (assoc-in baseline [:fields id] expression))

(defn- without-false-invariant []
  (update source :invariants dissoc :request/false))

(deftest exact-source-owned-report-and-constructor-witness
  (let [r (check! baseline)
        ledger (obligation/derive-obligations! source)
        ids (set (map #(vector :initial-state %) (keys invariants)))]
    (is (= constructor/report-type (:type r)))
    (is (= constructor/initial-constructor-version (:version r)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint r)))
    (is (= :request/new (:constructor-id r)))
    (is (= expected-state (:constructed-state r)))
    (is (true? (:initial-condition-holds? r)))
    (is (= (:fingerprint ledger) (:obligation-fingerprint r)))
    (is (= (:transition-fingerprint ledger) (:transition-fingerprint r)))
    (is (= (:subject-fingerprint ledger) (:subject-fingerprint r)))
    (is (= ids (set (map :initial-obligation-id (:initial-case-outcomes r)))))
    (is (= (count ids) (count (:initial-case-outcomes r))))
    (is (= ids (set/union (:checked-initial-case-ids r)
                          (:violated-initial-case-ids r))))
    (is (empty? (set/intersection (:checked-initial-case-ids r)
                                  (:violated-initial-case-ids r))))
    (is (= (:open-obligation-ids ledger) (:original-open-obligation-ids r)))))

(deftest witness-reports-honest-per-invariant-truth
  (let [r (check! baseline)]
    (doseq [id [:request/allowed-status :request/version-safe
                :request/unclaimed-owner :request/note-absent :request/true]]
      (let [o (outcome r id)]
        (is (= :checked-constructor-case (:status o)) (pr-str id))
        (is (true? (:value o)))
        (is (nil? (:reason o)))
        (is (contains? (:checked-initial-case-ids r) [:initial-state id]))))
    (is (= :violated-constructor-case
           (:status (outcome r :request/false))))
    (is (= :invariant-false-at-constructor
           (:reason (outcome r :request/false))))
    (is (false? (:value (outcome r :request/false))))
    (is (= #{[:initial-state :request/false]} (:violated-initial-case-ids r)))
    (is (false? (:all-declared-invariants-hold? r)))))

(deftest a-constructor-that-satisfies-every-invariant-is-still-only-a-case
  (let [s (without-false-invariant)
        r (check! s baseline)
        ids (set (map #(vector :initial-state %) (keys (:invariants s))))]
    (is (true? (:all-declared-invariants-hold? r)))
    (is (= ids (:checked-initial-case-ids r)))
    (is (empty? (:violated-initial-case-ids r)))
    (is (every? #(= :checked-constructor-case (:status %))
                (:initial-case-outcomes r)))
    (is (= (:open-obligation-ids (obligation/derive-obligations! s))
           (:original-open-obligation-ids r)))
    (is (contains? (:original-open-obligation-ids r)
                   [:assumption :initial-state-coverage]))
    (is (contains? (:original-open-obligation-ids r)
                   [:assumption :initial-state-existence]))))

(deftest optional-field-absent-is-not-present-nil
  (let [a (check! baseline)
        b (check! (with-field :request/note [:literal nil]))]
    (is (not (contains? (:constructed-state a) :request/note)))
    (is (contains? (:constructed-state b) :request/note))
    (is (nil? (get (:constructed-state b) :request/note)))
    (is (= :checked-constructor-case
           (:status (outcome a :request/note-absent))))
    (is (= :violated-constructor-case
           (:status (outcome b :request/note-absent))))
    (is (not= (:fingerprint a) (:fingerprint b)))))

(deftest optional-string-population-is-visible-to-predicates
  (let [r (check! (with-field :request/note [:literal "hello"]))]
    (is (= "hello" (get-in r [:constructed-state :request/note])))
    (is (= :violated-constructor-case
           (:status (outcome r :request/note-absent))))
    (is (true? (:initial-condition-holds? r)))))

(deftest optional-boolean-absence-and-present-false-are-distinct
  (let [a (check! baseline)
        b (check! (with-field :request/flag [:literal false]))
        c (check! (with-field :request/flag [:literal true]))]
    (is (not (contains? (:constructed-state a) :request/flag)))
    (is (false? (get-in b [:constructed-state :request/flag])))
    (is (true? (get-in c [:constructed-state :request/flag])))
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:fingerprint b) (:fingerprint c)))))

(deftest optional-nonnullable-boolean-refuses-nil
  (rejection! (with-field :request/flag [:literal nil])
              evaluation-error :nil-for-nonnullable-binding))

(deftest optional-notes-may-be-absent-nil-or-text
  (doseq [expression [[:absent] [:literal nil] [:literal ""]
                      [:literal "safe"]]]
    (is (true? (:initial-condition-holds?
                (check! (with-field :request/note expression))))
        (pr-str expression))))

(deftest explicit-nil-owner-is-valid-but-owner-absence-is-not
  (is (= :checked-constructor-case
         (:status (outcome (check! baseline) :request/unclaimed-owner))))
  (rejection! (with-field :request/owner [:absent])
              constructor-error :absent-required-field))

(deftest omitted-required-status-is-not-a-total-constructor
  (rejection! (update baseline :fields dissoc :request/status)
              constructor-error :wrong-keys))

(deftest omitted-required-version-is-not-a-total-constructor
  (rejection! (update baseline :fields dissoc :request/version)
              constructor-error :wrong-keys))

(deftest omitted-optional-note-still-requires-explicit-absence
  (rejection! (update baseline :fields dissoc :request/note)
              constructor-error :wrong-keys))

(deftest omitted-optional-flag-still-requires-explicit-absence
  (rejection! (update baseline :fields dissoc :request/flag)
              constructor-error :wrong-keys))

(deftest invented-state-field-is-not-accepted
  (rejection! (assoc-in baseline [:fields :request/secret] [:literal "x"])
              constructor-error :wrong-keys))

(deftest extra-undeclared-state-field-cannot-hide-behind-a-valid-constructor
  (let [err (rejection! (assoc-in baseline [:fields :request/secret] [:literal "x"])
                        constructor-error :wrong-keys)]
    (is (= #{:request/secret} (:unexpected err)))
    (is (= [:constructor :fields] (:at err)))))

(deftest constructor-must-be-exact-two-key-map
  (doseq [extra [{:certificate :issued}
                 {:authority :commit}
                 {:status :proved}
                 {:proof-status :proved}
                 {:already-checked? true}
                 {:source-fingerprint "sha256:fake"}]]
    (rejection! (merge baseline extra) constructor-error :wrong-keys)))

(deftest constructor-source-cannot-be-a-copyable-result-report
  (let [r (check! baseline)]
    (rejection! r constructor-error :wrong-keys)))

(deftest constructor-cannot-be-a-vector-or-null
  (doseq [x [nil [] [:request/new] #{} "request/new" 14]]
    (rejection! x constructor-error :expected-plain-map)))

(deftest constructor-fields-must-be-a-plain-persistent-map
  (doseq [fields [[] #{} nil (into (sorted-map) (:fields baseline))]]
    (rejection! (assoc baseline :fields fields)
                constructor-error :expected-plain-map)))

(deftest unqualified-or-nonkeyword-constructor-ids-are-invalid
  (doseq [id [:new "request/new" 42 nil true]]
    (rejection! (assoc baseline :id id)
                constructor-error :invalid-constructor-id)))

(deftest constructor-and-fields-reject-hidden-metadata
  (doseq [ctor [(with-meta baseline {:accepted? true})
                (update baseline :fields with-meta {:verified true})
                (with-field :request/version
                  (with-meta [:literal 0] {:proof :complete}))]]
    (rejection! ctor constructor-error :metadata-not-allowed)))

(deftest only-literal-and-absent-instructions-are-admitted
  (doseq [instruction [[:read :input/actor]
                       [:read :request/version]
                       [:copy :request/version]
                       [:if [:literal true] [:literal 0] [:literal 1]]
                       [:absent :extra]
                       [:literal]
                       [:literal 0 :extra]
                       [nil]]]
    (rejection! (with-field :request/version instruction)
                constructor-error :unsupported-constructor-expression)))

(deftest instructions-must-be-exact-vector-values
  (doseq [instruction [nil :absent '(:literal 0) {:literal 0} 0 false]]
    (rejection! (with-field :request/version instruction)
                constructor-error :expected-constructor-expression)))

(deftest present-nil-is-not-valid-for-required-nonnullable-fields
  (doseq [field [:request/status :request/version]]
    (rejection! (with-field field [:literal nil])
                evaluation-error :nil-for-nonnullable-binding)))

(deftest nominal-enum-membership-is-checked
  (doseq [value [:unknown :request/claimed :done-later]]
    (rejection! (with-field :request/status [:literal value])
                evaluation-error :enum-value-outside-domain)))

(deftest nominal-enum-must-be-keyword
  (doseq [value ["open" 0 true]]
    (rejection! (with-field :request/status [:literal value])
                evaluation-error :wrong-binding-type)))

(deftest numeric-state-rejects-floats-decimals-and-strings
  (doseq [[value reason]
          [[0.0 :unsupported-binding-value]
           [1.5 :unsupported-binding-value]
           ["0" :wrong-binding-type]
           [:request/zero :wrong-binding-type]
           [true :wrong-binding-type]]]
    (rejection! (with-field :request/version [:literal value])
                evaluation-error reason)))

(deftest arbitrary-clojure-objects-are-not-literal-state-values
  (doseq [value [(atom 0) (fn [] 0) {:version 0} [0] #{:open}]]
    (rejection! (with-field :request/version [:literal value])
                evaluation-error :unsupported-binding-value)))

(deftest optional-boolean-is-still-typed-when-ignored-by-all-invariants
  (doseq [value [0 :true "true" 1N]]
    (rejection! (with-field :request/flag [:literal value])
                evaluation-error :wrong-binding-type)))

(deftest optional-string-is-still-typed-when-ignored-by-initial-condition
  (doseq [value [0 :note true]]
    (rejection! (with-field :request/note [:literal value])
                evaluation-error :wrong-binding-type)))

(deftest optional-field-cannot-be-absent-when-made-required
  (let [s (assoc-in source [:construction :state-types :request/note :optional?] false)]
    (rejection! s baseline constructor-error :absent-required-field)))

(deftest required-owner-is-validated-even-if-no-rule-reads-it
  (let [s (-> source
              (assoc :initial-condition
                     {:readers #{:request/status}
                      :expression [:= [:read :request/status] [:literal :open]]})
              (update :invariants dissoc :request/unclaimed-owner))]
    (rejection! s (with-field :request/owner [:literal 42])
                evaluation-error :wrong-binding-type)))

(deftest initial-condition-not-satisfied-by-claimed-status
  (rejection! (with-field :request/status [:literal :claimed])
              constructor-error :constructor-outside-initial-condition))

(deftest initial-condition-not-satisfied-by-owner-change
  (rejection! (with-field :request/owner [:literal :user/alice])
              constructor-error :constructor-outside-initial-condition))

(deftest initial-condition-not-satisfied-by-negative-version
  (rejection! (with-field :request/version [:literal -1])
              constructor-error :constructor-outside-initial-condition))

(deftest explicitly-false-initial-condition-rejects-witness
  (let [s (assoc source :initial-condition
                 {:readers #{} :expression [:literal false]})]
    (rejection! s baseline constructor-error :constructor-outside-initial-condition)))

(deftest inconsistent-initial-clauses-reject-all-literal-witnesses
  (let [s (assoc source :initial-condition
                 {:readers #{:request/status}
                  :expression [:and
                               [:= [:read :request/status] [:literal :open]]
                               [:= [:read :request/status] [:literal :claimed]]]})]
    (doseq [value [:open :claimed :done]]
      (rejection! s (with-field :request/status [:literal value])
                  constructor-error :constructor-outside-initial-condition))))

(deftest invariant-violation-is-reported-not-constructor-rejection
  (let [s (assoc-in source [:invariants :request/version-positive]
                    {:readers #{:request/version}
                     :expression [:> [:read :request/version] [:literal 0]]})
        r (check! s baseline)]
    (is (= :violated-constructor-case
           (:status (outcome r :request/version-positive))))
    (is (= :invariant-false-at-constructor
           (:reason (outcome r :request/version-positive))))
    (is (contains? (:original-open-obligation-ids r)
                   [:initial-state :request/version-positive]))))

(deftest a-false-invariant-cannot-be-smuggled-into-a-true-overall-status
  (let [r (check! baseline)]
    (is (not (:all-declared-invariants-hold? r)))
    (is (contains? (:violated-initial-case-ids r)
                   [:initial-state :request/false]))
    (is (= :none (:authority r)))))

(deftest optional-presence-guard-prevents-absent-read-evaluation
  (let [s (assoc-in source [:invariants :request/note-nonblank]
                    {:readers #{:request/note}
                     :expression [:and
                                  [:present [:read :request/note]]
                                  [:not= [:read :request/note] [:literal ""]]]})
        absent (check! s baseline)
        populated (check! s (with-field :request/note [:literal "ok"]))]
    (is (= :violated-constructor-case
           (:status (outcome absent :request/note-nonblank))))
    (is (= :checked-constructor-case
           (:status (outcome populated :request/note-nonblank))))))

(deftest absent-optional-direct-read-does-not-become-unknown-success
  (let [s (assoc-in source [:invariants :request/note-null]
                    {:readers #{:request/note}
                     :expression [:= [:read :request/note] [:literal nil]]})]
    (rejection! s baseline rule-type-error :unguarded-optional-reader)))

(deftest entire-state-is-always-checked-even-for-literal-true-predicates
  (let [s (assoc source :initial-condition
                 {:readers #{} :expression [:literal true]})
        s (assoc s :invariants
                 {:request/true {:readers #{} :expression [:literal true]}})]
    (rejection! s (with-field :request/version [:literal "not-an-integer"])
                evaluation-error :wrong-binding-type)
    (is (true? (:all-declared-invariants-hold? (check! s baseline))))))

(deftest separate-constructor-ids-change-evidence-identity
  (let [a (check! baseline)
        b (check! (assoc baseline :id :request/other-constructor))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (= (:constructed-state a) (:constructed-state b)))
    (is (= (:obligation-fingerprint a) (:obligation-fingerprint b)))))

(deftest changed-initial-condition-changes-report-identity
  (let [a (check! baseline)
        s (assoc source :initial-condition
                 {:readers #{:request/status}
                  :expression [:= [:read :request/status] [:literal :open]]})
        b (check! s baseline)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (= (:constructed-state a) (:constructed-state b)))))

(deftest changed-invariant-set-changes-report-identity
  (let [a (check! baseline)
        b (check! (without-false-invariant) baseline)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (not= (:all-declared-invariants-hold? a)
              (:all-declared-invariants-hold? b)))))

(deftest changed-guard-or-assignment-changes-report-identity
  (let [a (check! baseline)
        guard-source (assoc-in source [:construction :guard]
                               {:readers #{:request/status}
                                :expression [:in [:read :request/status]
                                             [:literal #{:open :claimed}]]})
        assignment-source (assoc-in source [:construction :replacements
                                            :request/status] [:literal :done])
        b (check! guard-source baseline)
        c (check! assignment-source baseline)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:fingerprint a) (:fingerprint c)))
    (is (not= (:transition-fingerprint a) (:transition-fingerprint c)))))

(deftest changed-constructor-literal-changes-evidence-identity
  (let [a (check! baseline)
        b (check! (with-field :request/version [:literal 1]))
        c (check! (with-field :request/version [:literal 10N]))]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:fingerprint a) (:fingerprint c)))
    (is (not= (:fingerprint b) (:fingerprint c)))))

(deftest numeric-equality-in-constructed-state-is-exact
  (let [a (check! baseline)
        b (check! (with-field :request/version [:literal 0N]))]
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:constructed-state a) (:constructed-state b)))))

(deftest arbitrarily-large-mathematical-integers-are-supported
  (let [huge (+ (bigint Long/MAX_VALUE) 200000000000000000000N)
        r (check! (with-field :request/version [:literal huge]))]
    (is (= huge (get-in r [:constructed-state :request/version])))
    (is (= :checked-constructor-case
           (:status (outcome r :request/version-safe))))))

(deftest extreme-negative-bigint-fails-initial-condition
  (let [very-negative (- (bigint Long/MIN_VALUE) 100000000000000000000N)]
    (rejection! (with-field :request/version [:literal very-negative])
                constructor-error :constructor-outside-initial-condition)))

(deftest forged-expression-metadata-is-rejected-before-evaluation
  (rejection! (with-field :request/status
                (with-meta [:literal :open] {:trusted true}))
              constructor-error :metadata-not-allowed))

(deftest altered-obligation-summary-is-not-trusted
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc (original s) :authority :all))]
      (rejection! baseline constructor-error
                  :obligation-checker-contract-changed))))

(deftest obligation-checker-cannot-delete-invariant-obligations
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (update-in (original s) [:obligations :initial-state] pop))]
      (rejection! baseline constructor-error
                  :obligation-checker-contract-changed))))

(deftest obligation-checker-cannot-lie-about-open-obligations
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s] (assoc (original s) :open-obligation-ids #{}))]
      (rejection! baseline constructor-error
                  :obligation-checker-contract-changed))))

(deftest swapped-evaluator-cannot-return-fabricated-true
  (let [original rule-eval/evaluate-rule!]
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [declaration types bindings]
                    (assoc (original declaration types bindings) :value true))]
      ;; Baseline has a deliberately false invariant. A self-consistent
      ;; evaluator report with a false -> true flip must still be rejected.
      (rejection! baseline constructor-error
                  :rule-evaluator-contract-changed))))

(deftest swapped-evaluator-cannot-return-fabricated-false
  (let [original rule-eval/evaluate-rule!]
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [declaration types bindings]
                    (assoc (original declaration types bindings) :value false))]
      (rejection! baseline constructor-error
                  :rule-evaluator-contract-changed))))

(deftest swapped-evaluator-cannot-forge-authority
  (let [original rule-eval/evaluate-rule!]
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [declaration types bindings]
                    (assoc (original declaration types bindings)
                           :authority :authoritative))]
      (rejection! baseline constructor-error
                  :rule-evaluator-contract-changed))))

(deftest swapped-evaluator-cannot-misreport-an-unrelated-value
  (with-redefs [rule-eval/evaluate-rule! (fn [& _] {:value true})]
    (rejection! baseline constructor-error
                :rule-evaluator-contract-changed)))

(deftest forged-source-proof-flags-are-rejected
  (doseq [forgery [{:proved? true}
                   {:certificate :issued}
                   {:authority :all}
                   {:physical-state-completeness true}]]
    (rejection! (merge source forgery) baseline
                obligation-error :wrong-keys)))

(deftest constructor-source-cannot-be-a-copied-ledger
  (let [ledger (obligation/derive-obligations! source)]
    (rejection! ledger baseline obligation-error :wrong-keys)))

(deftest non-syntactic-source-replacements-are-rejected
  (doseq [bad [nil [] {:construction claim}]]
    (let [reason (if (= bad {:construction claim}) :wrong-keys :expected-plain-map)]
      (rejection! bad baseline obligation-error reason))))

(deftest finite-independent-state-oracle-for-optional-presence-and-bounds
  ;; These checks are independent simple arithmetic/presence expectations;
  ;; the tested checker is invoked with ORIGINAL declarative invariants.
  (doseq [version [0 1 13N 999999999999999999999999N]
          note [[:absent] [:literal nil] [:literal "memo"]]
          flag [[:absent] [:literal false] [:literal true]]]
    (let [ctor (-> baseline
                   (assoc-in [:fields :request/version] [:literal version])
                   (assoc-in [:fields :request/note] note)
                   (assoc-in [:fields :request/flag] flag))
          r (check! ctor)
          note-present? (= :literal (first note))]
      (is (true? (:initial-condition-holds? r)))
      (is (= (not note-present?)
             (= :checked-constructor-case
                (:status (outcome r :request/note-absent)))))
      (is (= (>= version 0)
             (= :checked-constructor-case
                (:status (outcome r :request/version-safe)))))
      (is (= (= :absent (first flag))
             (not (contains? (:constructed-state r) :request/flag))))
      (is (= #{[:initial-state :request/false]}
             (if note-present?
               (disj (:violated-initial-case-ids r)
                     [:initial-state :request/note-absent])
               (:violated-initial-case-ids r)))))))

(deftest evidence-never-becomes-a-transaction-authorization
  (let [r (check! (without-false-invariant) baseline)]
    (is (= :original-typed-literal-constructor-and-interpreted-rules
           (:checking r)))
    (is (= :total-typed-literal-initial-valuation (:proof-rule r)))
    (is (= :one-declared-abstract-state-projection-valuation (:scope r)))
    (is (= :source-derived-conditional-initial-case-check (:proof-status r)))
    (is (= :constructed-typed-literal-state (:abstract-initial-witness r)))
    (is (= :not-established (:initial-case-catalog-completeness r)))
    (is (= :not-established (:physical-initialization-coverage r)))
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= #{:initial-case-catalog-completeness
             :physical-initialization-coverage
             :initial-rule-evaluation-soundness
             :certificate-admission}
           (:open-meta-obligations r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :none (:authority r)))
    (is (not (contains? r :valid?)))
    (is (not (contains? r :transaction-authority)))
    (is (not (contains? r :all-initial-states-proved)))))

(deftest case-evidence-is-stable-under-exact-rechecking
  (let [a (check! baseline)
        b (check! baseline)]
    (is (= a b))
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:original-open-obligation-ids a)
           (:original-open-obligation-ids b)))))
