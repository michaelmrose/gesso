(ns gesso.assurance.proof.jvm-cljs-semantics
  "Portable, deliberately restricted JVM/CLJS semantic probe (v1325).

   (probe! {:rule {:readers #{:request/score}
                   :expression [:> [:read :request/score] [:literal 7]]}
            :reader-types {:request/score {:type :integer}}
            :bindings {:request/score 8}})

   The exact same source can be compiled and run under Clojure/JVM and
   ClojureScript/Node or Chromium. Cross-host agreement is established only
   by a harness that independently runs BOTH hosts and compares results;
   one invocation of probe! is never evidence of cross-host correspondence.

   In particular, CLJS numbers use JS Number. Reject integers outside
   +/- (2^53-1), including JVM BigInt and unsafe JavaScript numbers, rather
   than silently accepting rounded integers. This is a strict portable
   SUBSET, not the complete JVM rule language, a portable type checker, a
   checked physical snapshot, a theorem, or transaction authority."
  (:require [clojure.set :as set]))

(def probe-version 1)
(def report-type :gesso.assurance.proof.jvm-cljs-semantics/portable-probe)
(def max-safe-integer 9007199254740991)
(def ^:private max-text 2048)
(def ^:private max-nodes 512)
(def ^:private max-depth 32)
(def ^:private max-readers 16)
(def ^:private max-members 128)
(def ^:private allowed-types #{:boolean :integer :string :keyword :enum})
(def ^:private binary-ops #{:= :not= :< :<= :> :>= :in :implies})

(defn- fail! [status reason at]
  (throw (ex-info "Rule cannot be evaluated in the portable subset."
                  {:jvm-cljs-semantics/failure true
                   :status status :reason reason :at at})))

(defn- invalid! [reason at] (fail! :invalid-source reason at))
(defn- unsupported! [reason at] (fail! :unsupported-portable-semantics reason at))

(defn- metadata-free! [v at]
  (when (some? (meta v)) (invalid! :metadata-not-allowed at))
  v)

(defn- ordinary-map! [v at]
  (when-not (and (map? v) (not (record? v)))
    (invalid! :expected-plain-map at))
  (metadata-free! v at))

(defn- exact-map! [v expected at]
  (ordinary-map! v at)
  (when-not (= expected (set (keys v)))
    (invalid! :wrong-keys at))
  v)

(defn- qualified-reader? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- scalar-kind [v]
  (cond (nil? v) :nil
        (boolean? v) :boolean
        (integer? v) :integer
        (string? v) :string
        (keyword? v) :keyword
        :else nil))

(defn- scalar! [v at]
  (metadata-free! v at)
  (let [kind (scalar-kind v)]
    (when-not kind (unsupported! :nonportable-value at))
    (when (and (= kind :integer)
               (not (<= (- max-safe-integer) v max-safe-integer)))
      (unsupported! :unsafe-integer-precision at))
    (when (and (#{:integer :keyword :string} kind)
               (> (count (str v)) max-text))
      (unsupported! :oversize-value at))
    kind))

(defn- literal! [v at]
  (if (set? v)
    (do (metadata-free! v at)
        (when-not (<= 1 (count v) max-members)
          (unsupported! :unsupported-literal-set at))
        (let [kinds (set (map #(scalar! % (conj at :member)) v))]
          (when-not (= 1 (count kinds))
            (unsupported! :mixed-literal-set at)))
        :set)
    (scalar! v at)))

(defn- descriptor! [field d]
  (let [at [:reader-types field]]
    (ordinary-map! d at)
    (when-not (contains? allowed-types (:type d))
      (unsupported! :unsupported-reader-type at))
    (when (seq (set/difference (set (keys d))
                               #{:type :optional? :nullable? :name :values}))
      (invalid! :unknown-descriptor-keys at))
    (doseq [flag [:optional? :nullable?]]
      (when (and (contains? d flag) (not (boolean? (flag d))))
        (invalid! :nonboolean-descriptor-flag (conj at flag))))
    (if (= :enum (:type d))
      (do (when-not (qualified-reader? (:name d))
            (invalid! :invalid-enum-name (conj at :name)))
          (when-not (and (set? (:values d))
                         (<= 1 (count (:values d)) max-members))
            (invalid! :invalid-enum-values (conj at :values)))
          (metadata-free! (:values d) (conj at :values))
          (doseq [value (:values d)]
            (when-not (keyword? value)
              (invalid! :invalid-enum-value (conj at :values)))
            (scalar! value (conj at :values))))
      (when (or (contains? d :name) (contains? d :values))
        (invalid! :unexpected-enum-keys at))))
  d)

(defn- validate-readers! [{:keys [readers expression] :as rule} types bindings]
  (exact-map! rule #{:readers :expression} [:rule])
  (when-not (and (set? readers) (<= (count readers) max-readers))
    (invalid! :invalid-reader-set [:rule :readers]))
  (metadata-free! readers [:rule :readers])
  (doseq [reader readers]
    (when-not (qualified-reader? reader)
      (invalid! :unqualified-reader [:rule :readers])))
  (exact-map! types readers [:reader-types])
  (doseq [reader readers] (descriptor! reader (get types reader)))
  (ordinary-map! bindings [:bindings])
  (when (seq (set/difference (set (keys bindings)) readers))
    (invalid! :undeclared-bindings [:bindings]))
  (doseq [reader readers]
    (let [descriptor (get types reader)]
      (if-not (contains? bindings reader)
        (when-not (true? (:optional? descriptor))
          (invalid! :missing-required-binding [:bindings reader]))
        (let [value (get bindings reader)
              kind (scalar! value [:bindings reader])
              expected (:type descriptor)]
          (when (and (= :nil kind) (not (true? (:nullable? descriptor))))
            (invalid! :nil-for-nonnullable-binding [:bindings reader]))
          (when (and (not= :nil kind)
                     (not= kind (if (= expected :enum) :keyword expected)))
            (invalid! :incorrect-binding-type [:bindings reader]))
          (when (and (= expected :enum) (not= :nil kind)
                     (not (contains? (:values descriptor) value)))
            (invalid! :enum-binding-outside-domain [:bindings reader]))))))
  expression)

(defn- expression! [expression readers at depth budget]
  (when (> depth max-depth) (unsupported! :expression-depth-limit at))
  (when (> (swap! budget inc) max-nodes)
    (unsupported! :expression-node-limit at))
  (when-not (and (vector? expression) (<= 2 (count expression)))
    (invalid! :expected-expression-vector at))
  (metadata-free! expression at)
  (let [[op a] expression
        n (count expression)]
    (case op
      :literal (do (when-not (= 2 n) (invalid! :wrong-expression-arity at))
                   (literal! a (conj at 1)))
      :read (do (when-not (= 2 n) (invalid! :wrong-expression-arity at))
                (when-not (contains? readers a)
                  (invalid! :undeclared-reader (conj at 1))))
      :present (do (when-not (and (= n 2) (vector? a)
                                  (= 2 (count a)) (= :read (first a))
                                  (contains? readers (second a)))
                     (invalid! :invalid-presence-operand at))
                   (expression! a readers (conj at 1) (inc depth) budget))
      :not (do (when-not (= n 2) (invalid! :wrong-expression-arity at))
               (expression! a readers (conj at 1) (inc depth) budget))
      (:and :or) (do (when (< n 3) (invalid! :wrong-expression-arity at))
                     (doseq [i (range 1 n)]
                       (expression! (nth expression i) readers
                                    (conj at i) (inc depth) budget)))
      (if (contains? binary-ops op)
        (do (when-not (= n 3) (invalid! :wrong-expression-arity at))
            (doseq [i [1 2]]
              (expression! (nth expression i) readers
                           (conj at i) (inc depth) budget)))
        (unsupported! :unsupported-expression-operator at)))))

(defn- bool! [value at]
  (when-not (boolean? value)
    (invalid! :nonboolean-operand at))
  value)

(defn- comparable-integer! [a b at]
  (when-not (and (= :integer (scalar! a (conj at 1)))
                 (= :integer (scalar! b (conj at 2))))
    (invalid! :noninteger-comparison at))
  (compare a b))

(declare eval!)

(defn- eval! [expression bindings at]
  (let [[op a b] expression]
    (case op
      :literal a
      :read (if (contains? bindings a)
              (get bindings a)
              (invalid! :absent-reader-evaluated at))
      :present (contains? bindings (second a))
      :not (not (bool! (eval! a bindings (conj at 1)) (conj at 1)))
      :and (loop [i 1]
             (if (= i (count expression))
               true
               (if (bool! (eval! (nth expression i) bindings (conj at i))
                          (conj at i))
                 (recur (inc i)) false)))
      :or (loop [i 1]
            (if (= i (count expression))
              false
              (if (bool! (eval! (nth expression i) bindings (conj at i))
                         (conj at i))
                true (recur (inc i)))))
      :implies (if (bool! (eval! a bindings (conj at 1)) (conj at 1))
                 (bool! (eval! b bindings (conj at 2)) (conj at 2))
                 true)
      := (= (eval! a bindings (conj at 1))
            (eval! b bindings (conj at 2)))
      :not= (not= (eval! a bindings (conj at 1))
                   (eval! b bindings (conj at 2)))
      :< (neg? (comparable-integer! (eval! a bindings (conj at 1))
                                     (eval! b bindings (conj at 2)) at))
      :<= (not (pos? (comparable-integer! (eval! a bindings (conj at 1))
                                           (eval! b bindings (conj at 2)) at)))
      :> (pos? (comparable-integer! (eval! a bindings (conj at 1))
                                    (eval! b bindings (conj at 2)) at))
      :>= (not (neg? (comparable-integer! (eval! a bindings (conj at 1))
                                           (eval! b bindings (conj at 2)) at)))
      :in (let [value (eval! a bindings (conj at 1))
                domain (eval! b bindings (conj at 2))]
            (when-not (set? domain)
              (invalid! :nonset-membership-domain at))
            (contains? domain value))
      (unsupported! :unsupported-expression-operator at))))

(defn probe!
  "Evaluate one pretyped, portable Boolean rule over a supplied snapshot.
   Revalidates finite data shape but does NOT independently prove static
   typing of all branches. Use identical inputs and compare results from two
   separately executed hosts to test bounded host correspondence."
  [source]
  (exact-map! source #{:rule :reader-types :bindings} [])
  (let [{:keys [rule reader-types bindings]} source
        expression (validate-readers! rule reader-types bindings)]
    (expression! expression (:readers rule) [:rule :expression] 0 (atom 0))
    (let [result (bool! (eval! expression bindings [:rule :expression])
                        [:rule :expression])]
      {:type report-type
       :version probe-version
       :status :portable-single-host-result
       :value result
       :scope :restricted-pure-pretyped-boolean-expression
       :integer-scope :exact-js-safe-integers-only
       :snapshot-provenance :not-established
       :jvm-cljs-correspondence :not-established-by-single-host
       :independent-checker-soundness :not-established
       :authority :none
       :transaction-authority :none})))
