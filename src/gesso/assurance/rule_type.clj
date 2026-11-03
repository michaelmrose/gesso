(ns gesso.assurance.rule-type
  "Closed nominal type checks for Gesso assurance predicates (v1221).

   Public API: (check-rule-type! rule-declaration reader-type-map).

   A rule-declaration is the EXACT v1219 {:readers ... :expression ...} data
   syntax. A reader-type-map has exactly those reader keys; each value is a
   plain data descriptor:
     {:type :integer|:boolean|:string|:keyword|:enum
      :optional? true|false       ;; omitted means false
      :nullable? true|false       ;; omitted means false
      :name :domain/status        ;; required qualified identity for :enum
      :values #{:enum/value ...}} ;; required for :enum, forbidden otherwise

   Enums have named nominal identity and nonempty finite sets of keywords. An optional binding may
   be read only where presence has been established. A nullable binding may
   participate in equality with nil, but order/membership and other non-null
   operations need a non-nil guard. :and, :or, :implies perform conservative,
   order-sensitive refinements matching short-circuit evaluation; v1223's
   interpreter MUST implement those evaluation guarantees. This module does
   not evaluate data, prove predicates, or grant admission or runtime authority.

   Integer type is the MATHEMATICAL integer syntax, not a claim about JVM/CLJS
   overflow, external read behavior, or XTDB state/transaction correspondence."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]))

(def type-version 1)
(def checked-type :gesso.assurance.rule-type/checked-types)

(def ^:private primitive-types #{:integer :boolean :string :keyword})
(def ^:private max-enum-members 256)
(def ^:private max-text-length 2048)
(def ^:private allowed-descriptor-keys #{:type :name :values :optional? :nullable?})

(defn- reject!
  [reason at data]
  (throw (ex-info "Invalid Gesso assurance rule types."
                  (merge {:error/type ::invalid-rule-type
                          :reason reason :at at} data))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- qualified-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- validate-descriptor!
  [field descriptor]
  (let [at [:reader-types field]]
    (when-not (plain-map? descriptor)
      (reject! :invalid-type-descriptor at {}))
    (metadata-free! descriptor at)
    (let [unknown (set/difference (set (keys descriptor)) allowed-descriptor-keys)
          t (:type descriptor)]
      (when (seq unknown)
        (reject! :unknown-descriptor-keys at {:unexpected unknown}))
      (when-not (or (primitive-types t) (= :enum t))
        (reject! :unknown-reader-type at {:declared-type t}))
      (doseq [flag [:optional? :nullable?]]
        (when (and (contains? descriptor flag)
                   (not (instance? Boolean (get descriptor flag))))
          (reject! :invalid-type-flag at {:flag flag})))
      (if (= t :enum)
        (let [values (:values descriptor)]
          (when-not (qualified-keyword? (:name descriptor))
            (reject! :missing-enum-name at {}))
          (when (> (count (str (:name descriptor))) max-text-length)
            (reject! :enum-name-size-limit at {}))
          (when-not (and (plain-set? values) (seq values)
                         (<= (count values) max-enum-members))
            (reject! :invalid-enum-domain at {}))
          (metadata-free! values (conj at :values))
          (doseq [v values]
            (when-not (and (keyword? v) (<= (count (str v)) max-text-length))
              (reject! :invalid-enum-member (conj at :values) {}))))
        (do
          (when (contains? descriptor :values)
            (reject! :unexpected-enum-domain at {}))
          (when (contains? descriptor :name)
            (reject! :unexpected-enum-name at {}))))
      {:type t
       :optional? (true? (:optional? descriptor))
       :nullable? (true? (:nullable? descriptor))
       :name (when (= t :enum) (:name descriptor))
       :values (when (= t :enum) (:values descriptor))})))

(defn- validate-reader-types!
  [readers reader-types]
  (when-not (plain-map? reader-types)
    (reject! :expected-reader-type-map [:reader-types] {}))
  (metadata-free! reader-types [:reader-types])
  (let [actual (set (keys reader-types))]
    (when (not= actual readers)
      (reject! :reader-type-footprint-mismatch [:reader-types]
               {:missing (set/difference readers actual)
                :unexpected (set/difference actual readers)}))
    (doseq [id actual]
      (when-not (qualified-keyword? id)
        (reject! :invalid-reader-type-key [:reader-types] {:reader id})))
    (into {} (map (fn [[id descriptor]]
                    [id (validate-descriptor! id descriptor)]))
          reader-types)))

(defn- scalar-type [v]
  (cond
    (nil? v) :nil
    (instance? Boolean v) :boolean
    (integer? v) :integer
    (string? v) :string
    (keyword? v) :keyword
    :else nil))

(defn- value-type
  [v at]
  (if (plain-set? v)
    (let [members (set (map scalar-type v))]
      (when-not (= 1 (count members))
        (reject! :heterogeneous-set at {:member-types members}))
      {:type :set :element-type (first members) :values v :nullable? false})
    {:type (scalar-type v) :nullable? false}))

(defn- reader-info
  [id types at env]
  (let [descriptor (get types id)]
    (when (and (:optional? descriptor)
               (not (contains? (:present env) id)))
      (reject! :unguarded-optional-reader at {:reader id}))
    (assoc descriptor :nullable?
           (and (:nullable? descriptor)
                (not (contains? (:nonnull env) id))))))

(defn- boolean!
  [result at]
  (when (or (not= :boolean (:type result)) (:nullable? result))
    (reject! :expected-nonnullable-boolean at {:actual-type (:type result)}))
  result)

(defn- base-type
  [t]
  (if (= :enum (:type t)) :keyword (:type t)))

(defn- valid-equality?
  [a b]
  (let [ta (base-type a)
        tb (base-type b)]
    (or (and (= ta tb) (some? ta) (not= ta :set)
             (cond
               (and (= :enum (:type a)) (= :enum (:type b)))
               (and (= (:name a) (:name b)) (= (:values a) (:values b)))
               (= :enum (:type a)) (= :literal (:origin b))
               (= :enum (:type b)) (= :literal (:origin a))
               :else true))
        (and (= ta :nil) (:nullable? b))
        (and (= tb :nil) (:nullable? a)))))

(defn- enum-literal-compatible!
  [a b at]
  (doseq [[en other] [[a b] [b a]]]
    (when (and (= :enum (:type en))
               (= :literal (:origin other))
               (= :keyword (:type other))
               (not (contains? (:values en) (:literal other))))
      (reject! :enum-literal-outside-domain at {}))))

(defn- refine
  "Conservative facts valid IF expression evaluates to truth-value.  These
   facts are used only in branches reachable via short-circuit control flow."
  [env expression truth-value]
  (let [[op a b] expression]
    (cond
      (= op :not) (refine env a (not truth-value))
      (= op :present)
      (if truth-value
        (update env :present conj (second a))
        env)
      (and truth-value (= op :and))
      (reduce #(refine %1 %2 true) env (rest expression))
      (and (not truth-value) (= op :or))
      (reduce #(refine %1 %2 false) env (rest expression))
      (and (= op :implies) (not truth-value))
      (-> env (refine a true) (refine b false))
      (and (#{:= :not=} op)
           (= truth-value (= op :not=)))
      (let [id (cond
                 (and (= :read (first a)) (= b [:literal nil])) (second a)
                 (and (= :read (first b)) (= a [:literal nil])) (second b))]
        (if id (update env :nonnull conj id) env))
      :else env)))

(declare type-expression!)

(defn- two-types!
  [expression at types env]
  [(type-expression! (nth expression 1) (conj at 1) types env)
   (type-expression! (nth expression 2) (conj at 2) types env)])

(defn- type-expression!
  [expression at types env]
  (let [[op a] expression]
    (case op
      :literal (assoc (value-type a (conj at 1)) :origin :literal :literal a)
      :read (reader-info a types at env)
      :present {:type :boolean :nullable? false}
      :not (do
             (boolean! (type-expression! a (conj at 1) types env) (conj at 1))
             {:type :boolean :nullable? false})
      (:and :or)
      (do
        (loop [i 1, state env]
          (when (< i (count expression))
            (boolean! (type-expression! (nth expression i) (conj at i) types state)
                      (conj at i))
            (recur (inc i)
                   (refine state (nth expression i) (= op :and)))))
        {:type :boolean :nullable? false})
      :implies
      (do
        (boolean! (type-expression! a (conj at 1) types env) (conj at 1))
        (boolean! (type-expression! (nth expression 2) (conj at 2)
                                    types (refine env a true))
                  (conj at 2))
        {:type :boolean :nullable? false})
      (:= :not=)
      (let [[left right] (two-types! expression at types env)]
        (when-not (valid-equality? left right)
          (reject! :incompatible-equality-types at
                   {:left-type (:type left) :right-type (:type right)}))
        (enum-literal-compatible! left right at)
        {:type :boolean :nullable? false})
      (:< :<= :> :>=)
      (let [[left right] (two-types! expression at types env)]
        (when-not (and (= :integer (:type left))
                       (= :integer (:type right))
                       (not (:nullable? left))
                       (not (:nullable? right)))
          (reject! :expected-nonnullable-integers at
                   {:left-type (:type left) :right-type (:type right)}))
        {:type :boolean :nullable? false})
      :in
      (let [[item domain] (two-types! expression at types env)]
        (when-not (and (= :set (:type domain))
                       (= (base-type item) (:element-type domain))
                       (not (:nullable? item)))
          (reject! :incompatible-membership-types at
                   {:item-type (:type item) :domain-type (:type domain)}))
        (when (and (= :enum (:type item))
                   (not (set/subset? (:values domain) (:values item))))
          (reject! :enum-domain-not-subset at {}))
        {:type :boolean :nullable? false})
      (reject! :unrecognized-typed-expression at {:operator op}))))

(defn check-rule-type!
  "Type-check a v1219 rule declaration against an EXACT field-type footprint.
   Return a copyable, unadmitted, UNEVALUATED report, never proof evidence.

   Safe refinements are restricted to facts established by short-circuit
   evaluation. E.g. an optional integer requires [:present [:read id]] before
   any [:read id]; if also nullable, it requires [:not= [:read id]
   [:literal nil]] before an integer comparison. Types do not establish that
   future runtime values or external operations honor these declarations."
  [declaration reader-types]
  (let [syntax (rule/check-rule! declaration)
        types (validate-reader-types! (:readers syntax) reader-types)
        result (type-expression! (:expression syntax) [:expression] types
                                 {:present #{} :nonnull #{}})]
    (boolean! result [:expression])
    {:type checked-type
     :version type-version
     :rule-version rule/rule-version
     :expression (:expression syntax)
     :readers (:readers syntax)
     :reader-types reader-types
     :result-type :boolean
     :checking :syntax-and-types
     :evaluation :not-performed
     :proof-status :not-proved
     :admission :not-admitted
     :authority :none}))
