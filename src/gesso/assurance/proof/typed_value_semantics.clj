(ns gesso.assurance.proof.typed-value-semantics
  "Independent denotational foundation for the admitted S1 reader-value
   language (v1341). A state is a finite partial map from qualified reader
   identifiers to scalar values. Absence (no entry), present nil, and present
   false are three different observations.

   A field descriptor uses the original v1221 reader-type vocabulary:
     {:type :integer|:boolean|:string|:keyword|:enum
      :optional? boolean? :nullable? boolean?
      :name :domain/status :values #{:status/open ...}}

   :name and :values are required only for nominal enums. Missing flags mean
   false. Integers denote mathematical integers represented by JVM integral
   values (including BigInt/BigInteger); no CLJS portability, XTDB snapshot
   provenance, expression-evaluation correspondence, or transaction authority
   is implied. This namespace deliberately does not require or invoke
   gesso.assurance.rule, rule-type, rule-eval, or another proof checker.

   Public API:
     (value-kind! value)                 ; strict admitted scalar kind
     (value-in-domain? descriptor value) ; valid present value, including nil
     (observe-state! reader-types state) ; closed, well-typed partial state
     (observe-field! reader-types state reader-id)

   Every result is ordinary, copyable diagnostic data, never a proof handle."
  (:require [clojure.set :as set]))

(def semantics-version 1)
(def observation-type :gesso.assurance.proof.typed-value-semantics/observation)

(def ^:private value-types #{:integer :boolean :string :keyword :enum})
(def ^:private descriptor-keys #{:type :optional? :nullable? :name :values})
(def ^:private max-fields 64)
(def ^:private max-enum-members 256)
(def ^:private max-scalar-text 2048)

(defn- reject!
  [reason at more]
  (throw (ex-info "Invalid independent typed-value semantics source."
                  (merge {:typed-value-semantics/rejection true
                          :reason reason :at at} more))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- metadata-free!
  [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {}))
  x)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- exact-plain-map!
  [x at]
  (when-not (plain-map? x)
    (reject! :expected-plain-map at {}))
  (metadata-free! x at))

(defn value-kind!
  "Identify a supported *present* scalar value. Nil has its own kind;
   absence is never represented by a scalar sentinel."
  [value]
  (let [kind (cond
               (nil? value) :nil
               (instance? Boolean value) :boolean
               (integer? value) :integer
               (string? value) :string
               (keyword? value) :keyword
               :else :unsupported)]
    (when (= kind :unsupported)
      (reject! :unsupported-scalar [:value] {}))
    (metadata-free! value [:value])
    (when (and (#{:integer :string :keyword} kind)
               (> (count (str value)) max-scalar-text))
      (reject! :scalar-size-limit [:value] {:maximum max-scalar-text}))
    kind))

(defn- descriptor!
  [id d]
  (let [at [:reader-types id]]
    (exact-plain-map! d at)
    (let [unknown (set/difference (set (keys d)) descriptor-keys)
          kind (:type d)]
      (when (seq unknown)
        (reject! :unknown-descriptor-keys at {:unexpected unknown}))
      (when-not (value-types kind)
        (reject! :unsupported-reader-type at {:declared kind}))
      (doseq [k [:optional? :nullable?]]
        (when (and (contains? d k) (not (instance? Boolean (get d k))))
          (reject! :nonboolean-descriptor-flag (conj at k) {})))
      (if (= kind :enum)
        (let [name (:name d)
              members (:values d)]
          (when-not (qualified-id? name)
            (reject! :invalid-nominal-enum-name (conj at :name) {}))
          (when (> (count (str name)) max-scalar-text)
            (reject! :enum-name-size-limit (conj at :name) {}))
          (when-not (and (plain-set? members)
                         (<= 1 (count members) max-enum-members))
            (reject! :invalid-enum-members (conj at :values) {}))
          (metadata-free! members (conj at :values))
          (doseq [member members]
            (when-not (keyword? member)
              (reject! :nonkeyword-enum-member (conj at :values) {}))
            (when (> (count (str member)) max-scalar-text)
              (reject! :enum-member-size-limit (conj at :values) {}))))
        (when (or (contains? d :name) (contains? d :values))
          (reject! :unexpected-nominal-enum-keys at {})))
      {:type kind
       :optional? (true? (:optional? d))
       :nullable? (true? (:nullable? d))
       :name (when (= :enum kind) (:name d))
       :values (when (= :enum kind) (:values d))})))

(defn- types!
  [reader-types]
  (exact-plain-map! reader-types [:reader-types])
  (when (> (count reader-types) max-fields)
    (reject! :reader-count-limit [:reader-types] {:maximum max-fields}))
  (into {}
        (map (fn [[id d]]
               (when-not (qualified-id? id)
                 (reject! :unqualified-reader-id [:reader-types id] {}))
               [id (descriptor! id d)]))
        reader-types))

(defn- value-in-domain*
  [descriptor value kind]
  (if (= kind :nil)
    (:nullable? descriptor)
    (case (:type descriptor)
      :enum (and (= kind :keyword) (contains? (:values descriptor) value))
      (= kind (:type descriptor)))))

(defn value-in-domain?
  "Does a *present* scalar inhabit a reader descriptor? A nil value needs
   :nullable? true; :optional? controls only whether the map key may be absent.
   Invalid descriptors and unsupported scalar representations throw."
  [descriptor value]
  (let [d (descriptor! :independent/value descriptor)
        kind (value-kind! value)]
    (boolean (value-in-domain* d value kind))))

(defn- observation
  [id descriptor state]
  (if-not (contains? state id)
    (if (:optional? descriptor)
      {:presence :absent}
      (reject! :missing-required-reader [:state id] {:reader id}))
    (let [value (get state id)
          kind (value-kind! value)]
      (when-not (value-in-domain* descriptor value kind)
        (reject! :ill-typed-present-value [:state id]
                 {:reader id :declared-type (:type descriptor)
                  :actual-kind kind}))
      {:presence :present :kind kind :value value})))

(defn observe-state!
  "Interpret an exact, well-typed partial reader map. Every declared reader
   receives one explicit observation. A present false is not absence; a present
   nil is not absence. Unknown readers, missing required readers, metadata,
   executable values, and non-enum membership fail closed.

   This is a semantic *snapshot description*, not a claim that the snapshot
   came from an authoritative source."
  [reader-types state]
  (let [types (types! reader-types)
        _ (exact-plain-map! state [:state])
        provided (set (keys state))
        unexpected (set/difference provided (set (keys types)))]
    (when (seq unexpected)
      (reject! :undeclared-readers [:state] {:unexpected unexpected}))
    (let [fields (into {}
                       (map (fn [[id descriptor]]
                              [id (observation id descriptor state)]))
                       types)]
      {:type observation-type
       :version semantics-version
       :fields fields
       :present-readers (set (filter #(= :present (:presence (get fields %)))
                                     (keys fields)))
       :absent-readers (set (filter #(= :absent (:presence (get fields %)))
                                    (keys fields)))
       :source-semantics :typed-partial-map
       :checking :independent-value-membership
       :proof-status :not-proved
       :admission :not-admitted
       :trusted-snapshot-provenance :not-established
       :jvm-cljs-equivalence :not-established
       :authority :none})))

(defn observe-field!
  "Interpret one declared reader in a fully checked partial map. Never
   bypass validation of other declared readers or the closed state footprint."
  [reader-types state reader-id]
  (let [report (observe-state! reader-types state)]
    (when-not (contains? (:fields report) reader-id)
      (reject! :unknown-reader [:reader-id] {:reader reader-id}))
    (get (:fields report) reader-id)))
