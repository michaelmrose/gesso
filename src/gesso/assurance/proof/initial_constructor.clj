(ns gesso.assurance.proof.initial-constructor
  "Typed, source-derived LITERAL initial-state constructor (v1272).

   Accepts (check-initial-constructor! obligation-source constructor), where
   obligation-source is the ORIGINAL v1241 source and constructor is exactly
     {:id :request/new
      :fields {:request/status [:literal :open]
               :request/version [:literal 0]
               :request/note [:absent]}}

   Every declared state field MUST occur in :fields exactly once. The only
   supported instructions are [:literal scalar] and [:absent] for an OPTIONAL
   state field. [:literal nil] is a PRESENT nullable value, different from
   [:absent]. There are no callbacks, hidden reads, dynamically chosen field
   names, external inputs, conditionals, or implicit defaults. Consequently
   this constructor denotes ONE total valuation of the declared projection,
   not an unbounded family of states. The v1223 evaluator performs strict
   runtime type/enum/optional/nullable validation of that valuation.

   The constructor MUST satisfy the original declared initial-condition rule.
   Each original typed invariant is separately evaluated at that state. An
   invariant false for this case is reported UNSUPPORTED, not claimed proved by
   the mere existence of the constructor. The report is COPYABLE conditional
   case evidence, NOT a general Init => I theorem, a complete initializer
   catalog, a proof certificate, runtime authorization, or physical-state
   correspondence. All v1241 obligations remain OPEN. A subsequent finite-
   case aggregator must establish coverage of EVERY declared constructor;
   physical initializer coverage remains a separate explicit assumption."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.subject :as subject]))

(def initial-constructor-version 1)
(def report-type
  :gesso.assurance.proof.initial-constructor/unadmitted-initial-case)

(def ^:private identity-id :gesso.assurance.proof/initial-constructor-v1)
(def ^:private expected-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private required-assumptions
  #{[:assumption :initial-state-existence]
    [:assumption :initial-state-coverage]
    [:assumption :checker-meta-soundness]
    [:assumption :declared-state-projection-coverage]
    [:assumption :declared-invariant-set-coverage]})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso typed initial-state constructor."
                  (merge {:error/type ::invalid-initial-constructor
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [x keys-required at]
  (when-not (plain-map? x)
    (reject! :expected-plain-map at {}))
  (metadata-free! x at)
  (let [actual (set (keys x))]
    (when-not (= actual keys-required)
      (reject! :wrong-keys at
               {:missing (set/difference keys-required actual)
                :unexpected (set/difference actual keys-required)})))
  x)

(defn- sha256? [x]
  (and (string? x) (boolean (re-matches #"sha256:[0-9a-f]{64}" x))))

;; Keep the source-owned implementations as v1269/v1270 do. Checking a
;; swapped public Var against a fresh original result detects altered report
;; providers in test/in-process contexts; it does NOT prove the implementation
;; or interpreter soundness independently.
(def ^:private source-obligation-checker obligation/derive-obligations!)
(def ^:private source-evaluator rule-eval/evaluate-rule!)

(defn- rederive-obligations! [source]
  (let [public (var-get #'obligation/derive-obligations!)]
    (if (identical? source-obligation-checker public)
      (source-obligation-checker source)
      (let [reported (public source)
            expected (source-obligation-checker source)]
        (when-not (= reported expected)
          (reject! :obligation-checker-contract-changed [] {}))
        expected))))

(defn- evaluate-source-rule! [declaration types bindings at]
  (let [public (var-get #'rule-eval/evaluate-rule!)]
    (if (identical? source-evaluator public)
      (source-evaluator declaration types bindings)
      (let [reported (public declaration types bindings)
            expected (source-evaluator declaration types bindings)]
        (when-not (= reported expected)
          (reject! :rule-evaluator-contract-changed at {}))
        expected))))

(defn- checked-ledger! [source]
  (let [ledger (rederive-obligations! source)
        categories (:obligations ledger)
        invs (:invariants ledger)
        initial-obligations (:initial-state categories)
        all-obligations (mapcat val categories)
        all-ids (map :id all-obligations)
        expected-initial (set (map #(vector :initial-state %) (keys invs)))]
    (when-not
      (and (= obligation/obligation-type (:type ledger))
           (= obligation/obligation-version (:version ledger))
           (= source (:source ledger))
           (sha256? (:fingerprint ledger))
           (sha256? (:transition-fingerprint ledger))
           (sha256? (:subject-fingerprint ledger))
           (= expected-categories (set (keys categories)))
           (map? invs) (seq invs)
           (map? (:initial-condition ledger))
           (vector? initial-obligations)
           (= expected-initial (set (map :id initial-obligations)))
           (= (count expected-initial) (count initial-obligations))
           (every? #(and (= :initial-state (:kind %))
                         (= :open (:status %))) initial-obligations)
           (= (count all-ids) (count (set all-ids)))
           (= (set all-ids) (:open-obligation-ids ledger))
           (set/subset? required-assumptions (:open-obligation-ids ledger))
           (every? #(= :open (:status %)) all-obligations)
           (= :all-obligations-open (:status ledger))
           (= :not-proved (:proof-status ledger))
           (= :not-issued (:certificate ledger))
           (= :not-admitted (:admission ledger))
           (= :not-established (:runtime-enforcement ledger))
           (= :not-established (:implementation-correspondence ledger))
           (= :none (:authority ledger)))
      (reject! :obligation-source-contract-changed [] {}))
    ledger))

(defn- checked-constructor! [constructor state-types]
  (exact-map! constructor #{:id :fields} [:constructor])
  (let [id (:id constructor)
        fields (:fields constructor)
        state-ids (set (keys state-types))]
    (when-not (and (keyword? id) (some? (namespace id)))
      (reject! :invalid-constructor-id [:constructor :id] {}))
    (exact-map! fields state-ids [:constructor :fields])
    (reduce-kv
     (fn [valuation field expression]
       (let [at [:constructor :fields field]
             descriptor (get state-types field)]
         (when-not (and (vector? expression)
                        (instance? clojure.lang.APersistentVector expression))
           (reject! :expected-constructor-expression at {}))
         (metadata-free! expression at)
         (cond
           (and (= 2 (count expression)) (= :literal (first expression)))
           (assoc valuation field (second expression))

           (and (= 1 (count expression)) (= :absent (first expression)))
           (if (true? (:optional? descriptor))
             valuation
             (reject! :absent-required-field at {:field field}))

           :else
           (reject! :unsupported-constructor-expression at
                    {:supported #{:literal :absent}}))))
     {} fields)))

(defn- presence-probe [ids]
  (let [terms (mapv (fn [id] [:present [:read id]]) (sort ids))]
    {:readers ids
     :expression (if (= 1 (count terms))
                   (first terms)
                   (into [:and] terms))}))

(defn- evaluate-declared! [declaration state-types state at]
  (let [readers (:readers declaration)
        types (select-keys state-types readers)
        bindings (select-keys state readers)
        evaluated (evaluate-source-rule! declaration types bindings at)]
    (when-not (and (= rule-eval/evaluated-type (:type evaluated))
                   (= rule-eval/evaluator-version (:version evaluated))
                   (= readers (:readers evaluated))
                   (instance? Boolean (:value evaluated))
                   (= :pure-interpreted (:evaluation evaluated))
                   (= :not-proved (:proof-status evaluated))
                   (= :not-admitted (:admission evaluated))
                   (= :none (:authority evaluated)))
      (reject! :rule-evaluator-contract-changed at {}))
    (:value evaluated)))

(defn check-initial-constructor!
  "Evaluate the ONE total valuation denoted by an original literal constructor
   against the ORIGINAL v1241 source, declared Init and every invariant.

   This checks a constructor CASE; it does not establish Init => I for all
   initial states, completeness of an initializer catalog, or physical
   initialization correctness. Missing/extra state fields or a constructor
   outside the declared initial class are rejected, not treated as proofs."
  [obligation-source constructor]
  (let [ledger (checked-ledger! obligation-source)
        state-types (get-in ledger [:source :construction :state-types])
        constructor-state (checked-constructor! constructor state-types)
        state-ids (set (keys state-types))
        ;; Force the ORIGINAL evaluator to validate every state binding,
        ;; including values not read by Init or any invariant. Its strict
        ;; reader footprint checks required fields, enum domains and nil.
        _ (evaluate-source-rule!
           (presence-probe state-ids) state-types constructor-state
           [:constructor :fields])
        initial-rule (get-in ledger [:source :initial-condition])
        initial-holds? (evaluate-declared!
                        initial-rule state-types constructor-state
                        [:initial-condition])
        _ (when-not initial-holds?
            (reject! :constructor-outside-initial-condition
                     [:constructor :fields] {}))
        outcomes (mapv
                  (fn [[id invariant]]
                    (let [source-rule (get-in ledger [:source :invariants id])
                          holds? (evaluate-declared!
                                  source-rule state-types constructor-state
                                  [:invariants id])]
                      {:invariant id
                       :initial-obligation-id [:initial-state id]
                       :status (if holds?
                                 :checked-constructor-case
                                 :violated-constructor-case)
                       :reason (when-not holds? :invariant-false-at-constructor)
                       :value holds?}))
                  (:invariants ledger))
        supported (set (map :initial-obligation-id
                            (filter #(= :checked-constructor-case (:status %))
                                    outcomes)))
        violated (set (map :initial-obligation-id
                           (filter #(= :violated-constructor-case (:status %))
                                   outcomes)))
        expected (set (map #(vector :initial-state %)
                           (keys (:invariants ledger))))
        _ (when-not (and (= expected (set/union supported violated))
                         (empty? (set/intersection supported violated))
                         (= (count outcomes) (count expected)))
            (reject! :initial-case-coverage-lost [] {}))
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:version initial-constructor-version
                               :obligation-fingerprint (:fingerprint ledger)
                               :constructor constructor
                               :constructed-state constructor-state
                               :initial-holds true
                               :outcomes outcomes}})]
    {:type report-type
     :version initial-constructor-version
     :fingerprint (:fingerprint identity)
     :constructor-id (:id constructor)
     :obligation-fingerprint (:fingerprint ledger)
     :transition-fingerprint (:transition-fingerprint ledger)
     :subject-fingerprint (:subject-fingerprint ledger)
     :constructed-state constructor-state
     :initial-condition-holds? true
     :initial-case-outcomes outcomes
     :checked-initial-case-ids supported
     :violated-initial-case-ids violated
     :all-declared-invariants-hold? (empty? violated)
     :original-open-obligation-ids (:open-obligation-ids ledger)
     :checking :original-typed-literal-constructor-and-interpreted-rules
     :proof-rule :total-typed-literal-initial-valuation
     :scope :one-declared-abstract-state-projection-valuation
     :proof-status :source-derived-conditional-initial-case-check
     :abstract-initial-witness :constructed-typed-literal-state
     :initial-case-catalog-completeness :not-established
     :physical-initialization-coverage :not-established
     :checker-soundness :not-independently-proved
     :open-meta-obligations
     #{:initial-case-catalog-completeness
       :physical-initialization-coverage
       :initial-rule-evaluation-soundness
       :certificate-admission}
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
