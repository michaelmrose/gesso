(ns gesso.assurance.proof.initial-state-coverage-soundness
  "Conditional coverage bridge between v1272-style TOTAL LITERAL initializer
   declarations and the v1362 enumerated abstract induction base (v1364).

   Input source (not a cached proof report):
     (derive-initial-coverage!
       induction-catalog
       {:id :request/initializers
        :declared-initializer-ids #{:request/new :request/import}
        :initializers [{:id :request/new
                        :fields {:request/status [:literal :open]
                                 :request/note [:absent] ...}} ...]})

   Each source constructor has exactly one instruction per projected state
   field. [:literal scalar] is PRESENT; [:absent] is allowed only for an
   optional field. Every typed constructor denotes ONE abstract state. The
   constructor image is compared with the EXACT enumerated induction base.

   If ALL declared literal constructors denote states in the declared base,
   then every invocation of one of THESE abstract constructors starts inside
   that base, assuming the constructors execute exactly as interpreted. If
   the v1362 schema also has supported base and step premises, ordinary
   mathematical induction applies to histories starting from THESE states.

   This does NOT show that the declared initializer IDs include actual app
   writers/branches; physical XTDB initialization, dynamic constructors,
   universal S1/S4 soundness, state projection completeness, and certificate
   admission remain OPEN. A matching observed state is a COPYABLE witness,
   never runtime provenance or a source of proof authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.abstract-execution-induction :as induction]
            [gesso.assurance.proof.typed-value-semantics :as values]))

(def semantics-version 1)
(def report-type
  :gesso.assurance.proof.initial-state-coverage-soundness/conditional-image)
(def witness-type
  :gesso.assurance.proof.initial-state-coverage-soundness/witness)

(def ^:private catalog-keys #{:id :declared-initializer-ids :initializers})
(def ^:private initializer-keys #{:id :fields})
(def ^:private max-initializers 32)
(def ^:private external-obligations
  #{:actual-initializer-id-completeness
    :physical-initializer-to-literal-constructor-refinement
    :all-real-initialization-paths-mediated
    :declared-state-projection-completeness
    :sound-independent-typed-state-semantics
    :sound-independent-abstract-transition-semantics
    :sound-conditional-preservation-composition
    :universal-induction-implementation-soundness
    :xtdb-initialization-correspondence
    :certificate-admission})

(def ^:private source-schema induction/derive-induction-schema!)
(def ^:private source-observe values/observe-state!)

(defn- reject! [reason at details]
  (throw (ex-info "Invalid independent abstract initializer coverage source."
                  (merge {:initial-state-coverage-soundness/rejection true
                          :reason reason :at at} details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- qualified-id? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- metadata-free! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (reject! :metadata-not-allowed at {}))
  v)

(defn- exact-map! [v required at]
  (when-not (plain-map? v)
    (reject! :expected-plain-map at {}))
  (metadata-free! v at)
  (when-not (= required (set (keys v)))
    (reject! :wrong-keys at
             {:expected required :actual (set (keys v))}))
  v)

(defn- checked-schema! [source]
  ;; Capture the original source checker, and cross-check a substituted public
  ;; Var. This detects ordinary report substitutions, NOT checker soundness.
  (let [schema (source-schema source)]
    (when-not (identical? source-schema
                          (var-get #'induction/derive-induction-schema!))
      (when-not (= schema
                   (induction/derive-induction-schema! source))
        (reject! :substituted-induction-schema [:induction-catalog] {})))
    (when-not (and (= induction/schema-type (:type schema))
                   (= induction/semantics-version (:version schema))
                   (= source (:source-catalog schema))
                   (= (:id source) (:catalog-id schema))
                   (= (:id (:invariant source)) (:invariant-id schema))
                   (= (count (:declared-initial-states source))
                      (count (:initial-cases schema)))
                   (= (:declared-operation-ids source)
                      (:declared-operation-ids schema))
                   (contains? #{:conditional-declared-induction-schema
                                :open-base-or-step} (:status schema))
                   (= :conditional-schema-only (:proof-status schema))
                   (= :not-issued (:certificate schema))
                   (= :not-admitted (:admission schema))
                   (= :none (:authority schema))
                   (= :not-established (:initializer-coverage schema)))
      (reject! :induction-schema-contract-changed [:induction-catalog] {}))
    schema))

(defn- checked-state! [types state at]
  (let [expected (source-observe types state)]
    (when-not (identical? source-observe
                          (var-get #'values/observe-state!))
      (when-not (= expected (values/observe-state! types state))
        (reject! :substituted-state-interpreter at {})))
    (when-not (and (= values/observation-type (:type expected))
                   (= values/semantics-version (:version expected))
                   (= (set (keys types))
                      (set (keys (:fields expected))))
                   (= :not-proved (:proof-status expected))
                   (= :not-admitted (:admission expected))
                   (= :none (:authority expected)))
      (reject! :state-interpreter-contract-changed at {}))
    expected))

(defn- initializer-source! [source]
  (exact-map! source catalog-keys [:initializer-catalog])
  (when-not (qualified-id? (:id source))
    (reject! :invalid-catalog-id [:initializer-catalog :id] {}))
  (let [ids (:declared-initializer-ids source)
        entries (:initializers source)]
    (when-not (and (instance? clojure.lang.PersistentHashSet ids)
                   (<= 1 (count ids) max-initializers)
                   (every? qualified-id? ids))
      (reject! :invalid-declared-initializer-set
               [:initializer-catalog :declared-initializer-ids] {}))
    (metadata-free! ids [:initializer-catalog :declared-initializer-ids])
    (when-not (and (instance? clojure.lang.APersistentVector entries)
                   (<= 1 (count entries) max-initializers))
      (reject! :invalid-initializer-vector
               [:initializer-catalog :initializers] {}))
    (metadata-free! entries [:initializer-catalog :initializers])
    (doseq [[i entry] (map-indexed vector entries)]
      (exact-map! entry initializer-keys [:initializer-catalog :initializers i])
      (when-not (qualified-id? (:id entry))
        (reject! :invalid-initializer-id
                 [:initializer-catalog :initializers i :id] {})))
    (let [actual (mapv :id entries)]
      (when-not (and (= (count actual) (count (set actual)))
                     (= ids (set actual)))
        (reject! :initializer-id-coverage-mismatch
                 [:initializer-catalog :initializers]
                 {:declared ids :actual (set actual)})))
    (vec (sort-by (comp str :id) entries))))

(defn- denote-literal-constructor! [entry state-types]
  (let [id (:id entry)
        fields (:fields entry)
        required (set (keys state-types))]
    (exact-map! fields required [:initializers id :fields])
    (let [state
          (reduce-kv
           (fn [state field expression]
             (let [at [:initializers id :fields field]]
               (when-not (instance? clojure.lang.APersistentVector expression)
                 (reject! :invalid-constructor-expression at {}))
               (metadata-free! expression at)
               (cond
                 (and (= 2 (count expression))
                      (= :literal (first expression)))
                 (assoc state field (second expression))

                 (and (= 1 (count expression))
                      (= :absent (first expression)))
                 (if (true? (get-in state-types [field :optional?]))
                   state
                   (reject! :absent-required-field at {}))

                 :else
                 (reject! :unsupported-constructor-expression at {}))))
           {} fields)]
      ;; Validates EVERY declared field, including ones the invariant does not
      ;; mention; guards nil-vs-absent, enum domain, unsupported scalar values.
      (checked-state! state-types state [:initializers id :fields])
      state)))

(defn derive-initial-coverage!
  "Recompute the exact finite image of source-declared literal initializers.
   A subset of the induction base is sufficient for the CONDITIONAL starting-
   state rule; an exact image also accounts for every enumerated base state.
   The latter is stronger bookkeeping, not proof that all physical writers or
   all states satisfying an Init predicate are represented."
  [induction-catalog initializer-catalog]
  (let [schema (checked-schema! induction-catalog)
        entries (initializer-source! initializer-catalog)
        types (:state-types induction-catalog)
        rows (mapv (fn [entry]
                     {:initializer-id (:id entry)
                      :constructed-state
                      (denote-literal-constructor! entry types)}) entries)
        enumerated (set (:declared-initial-states induction-catalog))
        derived (set (map :constructed-state rows))
        outside (set/difference derived enumerated)
        unrepresented (set/difference enumerated derived)
        subset? (empty? outside)
        exact? (and subset? (empty? unrepresented))
        inductive? (= :conditional-declared-induction-schema (:status schema))]
    {:type report-type
     :version semantics-version
     :source-induction-catalog induction-catalog
     :source-initializer-catalog initializer-catalog
     :induction-catalog-id (:catalog-id schema)
     :initializer-catalog-id (:id initializer-catalog)
     :declared-initializer-ids (:declared-initializer-ids initializer-catalog)
     :constructor-results rows
     :declared-base-states enumerated
     :constructor-image derived
     :outside-declared-base-states outside
     :unrepresented-enumerated-base-states unrepresented
     :all-declared-initializers-in-base? subset?
     :exact-declared-initializer-image? exact?
     :induction-schema-status (:status schema)
     :failing-initial-indices (:failing-initial-indices schema)
     :unsupported-operation-ids (:unsupported-operation-ids schema)
     :status (cond
               (not subset?) :open-constructor-outside-base
               exact? :conditional-exact-declared-image
               :else :conditional-constructor-image-subset)
     :conditional-coverage-claim
     (when subset? :every-declared-literal-constructor-produces-declared-base)
     :conditional-history-claim
     (when (and subset? inductive?)
       :declared-literal-initializer-and-declared-history-preserve-invariant)
     :scope :finite-source-declared-literal-initializers-only
     :external-obligations external-obligations
     :real-initializer-coverage :not-established
     :physical-initialization-refinement :not-established
     :initializer-catalog-completeness :not-established
     :universal-implementation-soundness :not-established
     :proof-status :conditional-source-schema-only
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :authority :none}))

(defn audit-initializer-witness!
  "Compare a supplied typed state with one source-derived abstract initializer.
   A match is NOT evidence that the physical application ran that initializer,
   and an open induction or incomplete declared image cannot be upgraded."
  [induction-catalog initializer-catalog initializer-id observed-state]
  (let [report (derive-initial-coverage! induction-catalog initializer-catalog)
        _ (when-not (contains? (:declared-initializer-ids report) initializer-id)
            (reject! :undeclared-initializer-id [:initializer-id] {}))
        _ (checked-state! (:state-types induction-catalog)
                            observed-state [:observed-state])
        expected (:constructed-state
                  (first (filter #(= initializer-id (:initializer-id %))
                                 (:constructor-results report))))
        match? (= expected observed-state)]
    {:type witness-type
     :version semantics-version
     :source-induction-catalog induction-catalog
     :source-initializer-catalog initializer-catalog
     :initializer-id initializer-id
     :expected-state expected
     :observed-state observed-state
     :matches-declared-constructor? match?
     :coverage-status (:status report)
     :induction-schema-status (:induction-schema-status report)
     :history-premises-supported?
     (and match?
          (:all-declared-initializers-in-base? report)
          (= :conditional-declared-induction-schema
             (:induction-schema-status report)))
     :status (cond
               (not match?) :nonmatching-declared-initializer-witness
               (not (:all-declared-initializers-in-base? report))
               :matching-witness-but-initial-coverage-open
               (not= :conditional-declared-induction-schema
                     (:induction-schema-status report))
               :matching-witness-but-induction-open
               :else :matching-declared-initializer-witness)
     :scope :one-untrusted-abstract-initializer-observation
     :physical-provenance :not-established
     :real-initializer-coverage :not-established
     :initializer-catalog-completeness :not-established
     :proof-status :finite-witness-not-universal-proof
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :authority :none}))
