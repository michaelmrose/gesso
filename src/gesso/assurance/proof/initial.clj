(ns gesso.assurance.proof.initial
  "Restricted, SOURCE-DERIVED initial-state consequence checker (v1270).

   Input is ONLY the original v1241 obligation source:
     {:construction <v1239 guarded-replacement SOURCE>
      :initial-condition <typed state rule>
      :invariants {<qualified-id> <typed state rule> ...}}

   We check the implication Initial(s) => I(s) only through exact,
   normalized, typed expressions:
     * an invariant identical to the complete initial condition;
     * an invariant occurring as an obligatory :and conjunct, descending ONLY
       through :and nodes (never :or, :implies, :not, or arbitrary rewrites);
     * the independently typed closed invariant [:literal true].

   An explicit [:literal false] in a REQUIRED conjunct makes the declared
   initial class manifestly uninhabited. We conservatively leave all results
   OPEN for that case, rather than claim a useful base case through vacuity.
   Other uninhabited or contradictory conditions may remain undetected. No
   satisfiability check, arithmetic theorem, semantic-equivalence rewrite,
   optional-field substitution, or actual initializer coverage is implied.

   The v1241 source is independently rechecked. All original obligations,
   including initial-state EXISTENCE and physical COVERAGE, remain OPEN.
   This report contains COPYABLE, UNADMITTED conditional inference data;
   it is not a proof certificate, a kernel soundness theorem, a declaration
   of inhabited initial states, or a production execution authorization.
   Subsequent certificate consumers MUST rederive from the original source."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.subject :as subject]))

(def initial-version 1)
(def report-type :gesso.assurance.proof.initial/unadmitted-initial-consequence)

(def ^:private identity-id :gesso.assurance.proof/initial-consequence-v1)
(def ^:private checked-status :checked-conditional-inference)
(def ^:private open-status :open)
(def ^:private expected-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private required-assumptions
  #{[:assumption :initial-state-existence]
    [:assumption :initial-state-coverage]
    [:assumption :checker-meta-soundness]
    [:assumption :declared-state-projection-coverage]
    [:assumption :declared-invariant-set-coverage]})

(defn- fail! [reason details]
  (throw (ex-info "Invalid Gesso initial-state consequence inference."
                  (merge {:error/type ::invalid-initial-consequence
                          :reason reason}
                         details))))

(defn- sha256? [value]
  (and (string? value)
       (boolean (re-matches #"sha256:[0-9a-f]{64}" value))))

;; Keep the original source-derived obligation procedure, just as v1269
;; retains original preservation family implementations. A replaced public
;; Var is allowed only when its COMPLETE output matches freshly derived source
;; data. This is tamper resistance against substituted report providers, NOT
;; independent proof of the shared checker implementation.
(def ^:private source-obligation-checker obligation/derive-obligations!)

(defn- rederive-obligations! [source]
  (let [public (var-get #'obligation/derive-obligations!)]
    (if (identical? public source-obligation-checker)
      (source-obligation-checker source)
      (let [reported (public source)
            expected (source-obligation-checker source)]
        (when-not (= reported expected)
          (fail! :obligation-checker-contract-changed
                 {:detail :source-derived-obligations-mismatch}))
        expected))))

(defn- validate-ledger! [source ledger]
  (let [categories (:obligations ledger)
        invariants (:invariants ledger)
        initial-obligations (:initial-state categories)
        expected-ids (set (map #(vector :initial-state %) (keys invariants)))
        actual-ids (map :id initial-obligations)
        all-obligations (mapcat val categories)
        listed-ids (map :id all-obligations)
        open-ids (:open-obligation-ids ledger)]
    (when-not
      (and (= obligation/obligation-type (:type ledger))
           (= obligation/obligation-version (:version ledger))
           (= source (:source ledger))
           (sha256? (:fingerprint ledger))
           (sha256? (:transition-fingerprint ledger))
           (sha256? (:subject-fingerprint ledger))
           (= expected-categories (set (keys categories)))
           (map? invariants)
           (seq invariants)
           (map? (:initial-condition ledger))
           (vector? initial-obligations)
           (= (count expected-ids) (count actual-ids))
           (= expected-ids (set actual-ids))
           (every? (fn [{:keys [id invariant kind status quantification
                                antecedent consequent transition-fingerprint]}]
                     (and (= id [:initial-state invariant])
                          (= kind :initial-state)
                          (= status :open)
                          (= :all-declared-abstract-states quantification)
                          (= {:initial-condition-ref :declared-initial-condition}
                             antecedent)
                          (= {:state :pre :invariant-ref invariant} consequent)
                          (= (:transition-fingerprint ledger)
                             transition-fingerprint)))
                   initial-obligations)
           (= (count listed-ids) (count (set listed-ids)))
           (= (set listed-ids) open-ids)
           (set/subset? required-assumptions open-ids)
           (every? #(= :open (:status %)) all-obligations)
           (= :all-obligations-open (:status ledger))
           (= :not-proved (:proof-status ledger))
           (= :not-issued (:certificate ledger))
           (= :not-admitted (:admission ledger))
           (= :not-established (:runtime-enforcement ledger))
           (= :not-established (:implementation-correspondence ledger))
           (= :none (:authority ledger)))
      (fail! :obligation-source-contract-changed {}))
    expected-ids))

(defn- mandatory-nodes
  "Visit only nodes guaranteed true when the root :and expression is true.
   A path is made of original normalized-vector indices and is evidence of
   EXACT structural inclusion, not an evaluation witness or theorem token."
  [expression]
  (letfn [(visit [node path]
            (cons {:expression node :path path}
                  (when (= :and (first node))
                    (mapcat (fn [index]
                              (visit (nth node index) (conj path index)))
                            (range 1 (count node))))))]
    (vec (visit expression []))))

(defn- manifestly-false-initial? [nodes]
  (boolean (some #(= [:literal false] (:expression %)) nodes)))

(defn- infer-one [initial-nodes manifestly-false? invariant id]
  (let [goal (:expression invariant)
        match (first (filter #(= goal (:expression %)) initial-nodes))
        literal-true? (= goal [:literal true])
        support (cond
                  manifestly-false? nil
                  (= [] (:path match)) :identical-initial-rule
                  match :required-conjunct-elimination
                  literal-true? :literal-true-introduction
                  :else nil)]
    {:obligation-id [:initial-state id]
     :invariant id
     :readers (:readers invariant)
     :rule support
     :conjunct-path (when (and match support) (:path match))
     :status (if support checked-status open-status)
     :reason (when-not support
               (if manifestly-false?
                 :manifestly-false-initial-condition
                 :unsupported-initial-implication))}))

(defn check-initial-consequence!
  "Derive restricted, conditional initial-state implications from ORIGINAL
   typed v1241 sources. Never take a cached obligation report as evidence.

   Exact conjunction membership is sufficient for the logical implication
   assuming the declared typed Boolean interpreter and the :and rule are
   sound. This does not prove the class has any member or that a real state
   can be created within it; both remain explicit open assumptions."
  [source]
  (let [ledger (rederive-obligations! source)
        expected-ids (validate-ledger! source ledger)
        initial (:initial-condition ledger)
        invariants (:invariants ledger)
        nodes (mandatory-nodes (:expression initial))
        manifestly-false? (manifestly-false-initial? nodes)
        outcomes (mapv (fn [[id invariant]]
                         (infer-one nodes manifestly-false? invariant id))
                       invariants)
        supported (set (map :obligation-id
                            (filter #(= checked-status (:status %)) outcomes)))
        unsupported (set (map :obligation-id
                              (filter #(= open-status (:status %)) outcomes)))
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:version initial-version
                               :obligation-fingerprint (:fingerprint ledger)
                               :initial-condition initial
                               :invariants invariants
                               :outcomes outcomes}})]
    (when-not (and (= expected-ids (set/union supported unsupported))
                   (empty? (set/intersection supported unsupported))
                   (= (count outcomes) (count expected-ids)))
      (fail! :initial-obligation-coverage-lost {}))
    {:type report-type
     :version initial-version
     :fingerprint (:fingerprint identity)
     :obligation-fingerprint (:fingerprint ledger)
     :transition-fingerprint (:transition-fingerprint ledger)
     :subject-fingerprint (:subject-fingerprint ledger)
     :proof-rule :typed-structural-initial-consequence
     :scope :declared-abstract-state-projection-only
     :claim :initial-condition-implies-invariant-for-all-declared-states
     :initial-condition-manifestly-false? manifestly-false?
     :initial-outcomes outcomes
     :conditionally-checked-initial-ids supported
     :unsupported-initial-ids unsupported
     :original-open-obligation-ids (:open-obligation-ids ledger)
     :checking :original-source-typed-rules-and-exact-conjunct-structure
     :proof-status :restricted-conditional-initial-consequence-checked
     :initial-state-existence :not-established
     :initial-state-coverage :not-established
     :checker-soundness :not-independently-proved
     :open-meta-obligations
     #{:initial-structural-inference-rule-soundness
       :initial-state-existence
       :initial-state-coverage
       :certificate-admission}
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
