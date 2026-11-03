(ns gesso.assurance.rule
  "Closed, data-only rule expression syntax for the Gesso assurance path (v1219).

   Rule declarations have EXACTLY:
     {:readers #{:request/status :result/score}
      :expression [:and
                   [:in [:read :request/status]
                    [:literal #{:new :active :done}]]
                   [:> [:read :result/score] [:literal 7]]]}

   Syntax:
     [:literal scalar-or-finite-set]
     [:read qualified-keyword]
     [:present [:read qualified-keyword]]
     [:not expression]
     [:and expression expression ...]   ; at least two
     [:or expression expression ...]    ; at least two
     [:= expression expression]
     [:not= expression expression]
     [:< expression expression], [:<= ...], [:> ...], [:>= ...]
     [:in expression expression]
     [:implies expression expression]

   Literals are nil, booleans, integers (mathematical syntax only), strings,
   keywords, or finite sets of these scalars. There are NO callbacks, arbitrary
   readers, function values, maps, arbitrary collection objects, or executable
   payloads in this AST. Reader references must be explicitly declared; unused
   declarations are rejected to keep the source-owned read footprint exact.

   This namespace checks ONLY shape, closed operators, declared reads, and
   resource limits. It does not type-check operands, evaluate a rule, prove a
   proposition, admit a certificate, or assert runtime enforcement. In
   particular, mathematical integers here DO NOT establish Java/CLJS overflow
   semantics. Later rule-type, rule-eval, and proof-kernel revisions supply
   those independent obligations. External EDN should be parsed without #=
   reader evaluation, e.g. via clojure.edn/read-string, BEFORE this validator."
  (:require [clojure.set :as set]))

(def rule-version 1)
(def rule-type :gesso.assurance.rule/validated-syntax)

(def ^:private declaration-keys #{:readers :expression})
(def ^:private max-readers 256)
(def ^:private max-depth 64)
(def ^:private max-nodes 8192)
(def ^:private max-arity 256)
(def ^:private max-set-elements 256)
(def ^:private max-text-length 2048)

(def ^:private binary-operators
  #{:= :not= :< :<= :> :>= :in :implies})
(def ^:private nary-operators #{:and :or})

(defn- invalid!
  [reason data]
  (throw (ex-info "Invalid Gesso assurance rule syntax."
                  (merge {:error/type ::invalid-rule
                          :reason reason}
                         data))))

(defn- plain-map?
  [value]
  ;; Sorted maps may carry arbitrary comparator callbacks; records and custom
  ;; map implementations can also execute user code when read. Not part of the
  ;; closed, pure declaration language.
  (or (instance? clojure.lang.PersistentArrayMap value)
      (instance? clojure.lang.PersistentHashMap value)))

(defn- plain-set?
  [value]
  (instance? clojure.lang.PersistentHashSet value))

(defn- plain-vector?
  [value]
  (instance? clojure.lang.APersistentVector value))

(defn- qualified-keyword?
  [value]
  (and (keyword? value) (some? (namespace value))))

(defn- metadata-free!
  [value at]
  (when (and (instance? clojure.lang.IObj value)
             (some? (meta value)))
    (invalid! :metadata-not-allowed {:at at})))

(defn- scalar-literal?
  [value]
  (or (nil? value)
      (true? value)
      (false? value)
      (integer? value)
      (string? value)
      (keyword? value)))

(defn- bounded-scalar!
  [value at]
  (when-not (scalar-literal? value)
    (invalid! :invalid-literal
              {:at at :value-type (some-> value class str)}))
  (when (and (or (string? value) (keyword? value))
             (> (count (str value)) max-text-length))
    (invalid! :literal-size-limit {:at at :maximum max-text-length}))
  ;; A BigInt with an enormous decimal representation can exhaust resources
  ;; even though it is a mathematically valid integer literal.
  (when (and (integer? value)
             (> (count (str value)) max-text-length))
    (invalid! :literal-size-limit {:at at :maximum max-text-length}))
  value)

(defn- spend!
  [budget at]
  (when (> (swap! budget inc) max-nodes)
    (invalid! :node-limit {:at at :maximum max-nodes})))

(defn- literal!
  [value at budget]
  (cond
    (plain-set? value)
    (do
      (metadata-free! value at)
      (when (or (empty? value) (> (count value) max-set-elements))
        (invalid! :invalid-literal-set
                  {:at at :maximum max-set-elements}))
      (doseq [item value]
        (spend! budget (conj at :set-element))
        (bounded-scalar! item (conj at :set-element)))
      value)

    (set? value)
    (invalid! :unsupported-collection {:at at :collection :set})

    :else
    (bounded-scalar! value at)))

(declare check-expression!)

(defn- check-children!
  [expression offset at depth budget readers used]
  (doseq [index (range offset (count expression))]
    (check-expression! (nth expression index)
                       (conj at index) (inc depth) budget readers used)))

(defn- check-expression!
  [expression at depth budget readers used]
  (when (> depth max-depth)
    (invalid! :depth-limit {:at at :maximum max-depth}))
  (spend! budget at)
  (when-not (plain-vector? expression)
    (invalid! :expected-expression-vector {:at at}))
  (metadata-free! expression at)
  (when (or (empty? expression) (> (count expression) max-arity))
    (invalid! :invalid-expression-arity {:at at :maximum max-arity}))
  (let [op (first expression)
        length (count expression)]
    (when-not (keyword? op)
      (invalid! :invalid-operator {:at at :operator op}))
    (cond
      (= op :literal)
      (do
        (when-not (= length 2)
          (invalid! :invalid-arity {:at at :operator op :expected 1 :actual (dec length)}))
        (literal! (second expression) (conj at 1) budget))

      (= op :read)
      (let [id (second expression)]
        (when-not (= length 2)
          (invalid! :invalid-arity {:at at :operator op :expected 1 :actual (dec length)}))
        (when-not (qualified-keyword? id)
          (invalid! :invalid-reader-id {:at at :reader id}))
        (when-not (contains? readers id)
          (invalid! :undeclared-reader {:at at :reader id}))
        (swap! used conj id))

      (= op :present)
      (do
        (when-not (= length 2)
          (invalid! :invalid-arity {:at at :operator op :expected 1 :actual (dec length)}))
        ;; :present refers to *binding presence*, not a truthiness or non-nil
        ;; test. Restrict to a direct reader to keep absence semantics explicit.
        (when-not (and (plain-vector? (second expression))
                       (= :read (first (second expression))))
          (invalid! :present-requires-reader {:at at}))
        (check-children! expression 1 at depth budget readers used))

      (= op :not)
      (do
        (when-not (= length 2)
          (invalid! :invalid-arity {:at at :operator op :expected 1 :actual (dec length)}))
        (check-children! expression 1 at depth budget readers used))

      (contains? nary-operators op)
      (do
        (when (< length 3)
          (invalid! :invalid-arity {:at at :operator op :minimum 2 :actual (dec length)}))
        (check-children! expression 1 at depth budget readers used))

      (contains? binary-operators op)
      (do
        (when-not (= length 3)
          (invalid! :invalid-arity {:at at :operator op :expected 2 :actual (dec length)}))
        (check-children! expression 1 at depth budget readers used))

      :else
      (invalid! :unknown-operator {:at at :operator op})))
  expression)

(defn check-rule!
  "Check one untrusted rule DECLARATION for data-only syntax and an exact read
   footprint. Return a *copyable, unadmitted* syntax report, not a proof handle.

   {:readers #{:request/status :result/score}
    :expression [:> [:read :result/score] [:literal 7]]}

   Types, truth values, pure/effect behavior, and deployment correspondence
   remain entirely unchecked. This function does not read the named fields."
  [declaration]
  (when-not (plain-map? declaration)
    (invalid! :expected-declaration-map {:at []}))
  (metadata-free! declaration [])
  (let [actual (set (keys declaration))]
    (when-not (= declaration-keys actual)
      (invalid! :wrong-declaration-keys
                {:at []
                 :missing (set/difference declaration-keys actual)
                 :unexpected (set/difference actual declaration-keys)})))
  (let [readers (:readers declaration)]
    (when-not (plain-set? readers)
      (invalid! :invalid-reader-declarations {:at [:readers]}))
    (metadata-free! readers [:readers])
    (when (> (count readers) max-readers)
      (invalid! :reader-limit {:at [:readers] :maximum max-readers}))
    (doseq [reader readers]
      (when-not (qualified-keyword? reader)
        (invalid! :invalid-reader-id {:at [:readers] :reader reader})))
    (let [budget (atom 0)
          used (atom #{})]
      (check-expression! (:expression declaration)
                         [:expression] 0 budget readers used)
      (when-not (= @used readers)
        (invalid! :unused-reader-declaration
                  {:at [:readers]
                   :unused (set/difference readers @used)}))
      {:type rule-type
       :version rule-version
       :expression (:expression declaration)
       :readers readers
       :nodes @budget
       :checking :syntax-only
       :typing :not-performed
       :evaluation :not-performed
       :proof-status :not-proved
       :admission :not-admitted
       :authority :none})))
