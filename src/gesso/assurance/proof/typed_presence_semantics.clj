(ns gesso.assurance.proof.typed-presence-semantics
  "Independent S1 branch-sensitive semantics for :present, optional fields,
   nullable readers, and short-circuit Boolean rules (v1344).

   (evaluate-expression!
     [:and [:present [:read :request/score]]
           [:not= [:read :request/score] [:literal nil]]
           [:> [:read :request/score] [:literal 7]]]
     {:request/score {:type :integer :optional? true :nullable? true}}
     {:request/score 8})

   This namespace derives guard facts and evaluates admitted expressions
   independently of gesso.assurance.rule, rule-type, rule-eval, and all proof
   kernels. It relies only on v1341's independent closed typed-value model.
   A guard licenses reads only on the short-circuit branch where the fact
   follows; a present nil never implies nonnil, and a present false never
   implies an absent reader. The source read footprint is exact.

   Results are diagnostic, copyable descriptions, NOT certified theorems,
   proof handles, runtime authorization, or authoritative XTDB observations.
   Neither universal S1 evaluator correspondence nor JVM/CLJS portability is
   established by this module."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.typed-value-semantics :as values]))

(def semantics-version 1)
(def result-type :gesso.assurance.proof.typed-presence-semantics/expression-value)

(def ^:private comparisons #{:= :not= :< :<= :> :>= :in})
(def ^:private orderings #{:< :<= :> :>=})
(def ^:private max-nodes 8192)
(def ^:private max-depth 64)
(def ^:private max-arity 256)
(def ^:private max-members 256)

(defn- reject!
  [reason at data]
  (throw (ex-info "Invalid independent typed-presence semantics."
                  (merge {:typed-presence-semantics/rejection true
                          :reason reason :at at} data))))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {}))
  x)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- expression-shape!
  [expr at depth budget]
  (when (> depth max-depth)
    (reject! :expression-depth-limit at {:maximum max-depth}))
  (when-not (and (plain-vector? expr)
                 (<= 2 (count expr) max-arity))
    (reject! :invalid-expression-vector at {}))
  (metadata-free! expr at)
  (when (> (swap! budget inc) max-nodes)
    (reject! :expression-node-limit at {:maximum max-nodes}))
  (let [op (first expr)
        n (count expr)]
    (cond
      (#{:read :literal} op)
      (when-not (= n 2) (reject! :invalid-arity at {:operator op}))
      (= :present op)
      (when-not (and (= n 2) (plain-vector? (second expr))
                     (= 2 (count (second expr)))
                     (= :read (first (second expr))))
        (reject! :present-requires-direct-reader at {}))
      (= :not op)
      (when-not (= n 2) (reject! :invalid-arity at {:operator op}))
      (#{:and :or} op)
      (when (< n 3) (reject! :invalid-arity at {:operator op}))
      (or (= :implies op) (comparisons op))
      (when-not (= n 3) (reject! :invalid-arity at {:operator op}))
      :else (reject! :unsupported-operator at {:operator op}))))

(defn- literal-type!
  [value at]
  (if (plain-set? value)
    (do
      (metadata-free! value at)
      (when-not (<= 1 (count value) max-members)
        (reject! :invalid-literal-set at {}))
      (let [kinds (set (map values/value-kind! value))]
        (when-not (= 1 (count kinds))
          (reject! :heterogeneous-literal-set at {:kinds kinds}))
        {:type :set :element-type (first kinds)
         :literal value :origin :literal :nullable? false}))
    (if (set? value)
      (reject! :unsupported-literal-set at {})
      {:type (values/value-kind! value)
       :literal value :origin :literal :nullable? false})))

(defn- atom-base [t]
  (if (= :enum (:type t)) :keyword (:type t)))

(defn- compatible-equality? [a b]
  (let [ta (atom-base a) tb (atom-base b)]
    (or (and (= ta tb) (not= ta :set)
             (cond
               (and (= :enum (:type a)) (= :enum (:type b)))
               (and (= (:name a) (:name b))
                    (= (:values a) (:values b)))
               (= :enum (:type a)) (= :literal (:origin b))
               (= :enum (:type b)) (= :literal (:origin a))
               :else true))
        (and (= ta :nil) (:nullable? b))
        (and (= tb :nil) (:nullable? a)))))

(defn- valid-equality!
  [a b at]
  (when-not (compatible-equality? a b)
    (reject! :incompatible-equality-types at {}))
  (doseq [[e other] [[a b] [b a]]]
    (when (and (= :enum (:type e))
               (= :literal (:origin other))
               (= :keyword (:type other))
               (not (contains? (:values e) (:literal other))))
      (reject! :enum-literal-outside-domain at {}))))

(defn- guard-facts
  "Extract only facts forced by a specified Boolean outcome. Does NOT
   infer facts from :and false, :or true or :implies true."
  [env expr truth]
  (let [[op a b] expr]
    (cond
      (= op :not) (guard-facts env a (not truth))
      (= op :present)
      (if truth (update env :present conj (second a)) env)
      (and (= op :and) truth)
      (reduce #(guard-facts %1 %2 true) env (rest expr))
      (and (= op :or) (not truth))
      (reduce #(guard-facts %1 %2 false) env (rest expr))
      (and (= op :implies) (not truth))
      (-> env (guard-facts a true) (guard-facts b false))
      (and (#{:= :not=} op)
           (= truth (= op :not=)))
      (let [reader (cond
                     (and (plain-vector? a) (= :read (first a))
                          (= b [:literal nil])) (second a)
                     (and (plain-vector? b) (= :read (first b))
                          (= a [:literal nil])) (second b))]
        (if reader (update env :nonnull conj reader) env))
      :else env)))

(defn- nonnullable-boolean! [t at]
  (when-not (and (= :boolean (:type t)) (not (:nullable? t)))
    (reject! :expected-nonnullable-boolean at {:actual (:type t)}))
  t)

(declare type-expression!)

(defn- type-expression!
  [expr types env at depth budget used]
  (expression-shape! expr at depth budget)
  (let [[op a b] expr]
    (case op
      :literal (literal-type! a (conj at 1))

      :read
      (do
        (when-not (and (qualified-id? a) (contains? types a))
          (reject! :undeclared-reader at {:reader a}))
        (swap! used conj a)
        (let [descriptor (get types a)]
          (when (and (:optional? descriptor)
                     (not (contains? (:present env) a)))
            (reject! :unguarded-optional-reader at {:reader a}))
          (assoc descriptor :nullable?
                 (and (:nullable? descriptor)
                      (not (contains? (:nonnull env) a))))))

      :present
      (let [id (second a)]
        (when-not (and (qualified-id? id) (contains? types id))
          (reject! :undeclared-reader at {:reader id}))
        (metadata-free! a (conj at 1))
        (swap! used conj id)
        {:type :boolean :nullable? false})

      :not
      (do
        (nonnullable-boolean!
         (type-expression! a types env (conj at 1) (inc depth) budget used)
         (conj at 1))
        {:type :boolean :nullable? false})

      (:and :or)
      (do
        (loop [i 1 current env]
          (when (< i (count expr))
            (nonnullable-boolean!
             (type-expression! (nth expr i) types current (conj at i)
                               (inc depth) budget used)
             (conj at i))
            (recur (inc i)
                   (guard-facts current (nth expr i) (= op :and)))))
        {:type :boolean :nullable? false})

      :implies
      (do
        (nonnullable-boolean!
         (type-expression! a types env (conj at 1) (inc depth) budget used)
         (conj at 1))
        (nonnullable-boolean!
         (type-expression! b types (guard-facts env a true) (conj at 2)
                           (inc depth) budget used)
         (conj at 2))
        {:type :boolean :nullable? false})

      (:= :not= :< :<= :> :>= :in)
      (let [left (type-expression! a types env (conj at 1) (inc depth) budget used)
            right (type-expression! b types env (conj at 2) (inc depth) budget used)]
        (cond
          (#{:= :not=} op) (valid-equality! left right at)
          (orderings op)
          (when-not (and (= :integer (:type left))
                         (= :integer (:type right))
                         (not (:nullable? left)) (not (:nullable? right)))
            (reject! :expected-nonnullable-integers at {}))
          (= :in op)
          (do
            (when-not (and (= :set (:type right))
                           (= (atom-base left) (:element-type right))
                           (not (:nullable? left)))
              (reject! :incompatible-membership-types at {}))
            (when (and (= :enum (:type left))
                       (not (set/subset? (:values right) (:values left))))
              (reject! :enum-membership-outside-domain at {}))))
        {:type :boolean :nullable? false})

      (reject! :unsupported-operator at {:operator op}))))

(defn- interpretation!
  [expr fields at evaluated]
  (swap! evaluated conj at)
  (let [[op a b] expr]
    (case op
      :literal a
      :read (let [observation (get fields a)]
              (if (= :present (:presence observation))
                (:value observation)
                (reject! :read-absent-reader at {:reader a})))
      :present (= :present (:presence (get fields (second a))))
      :not (not (interpretation! a fields (conj at 1) evaluated))
      :and (loop [i 1]
             (if (= i (count expr))
               true
               (if (interpretation! (nth expr i) fields (conj at i) evaluated)
                 (recur (inc i))
                 false)))
      :or (loop [i 1]
            (if (= i (count expr))
              false
              (if (interpretation! (nth expr i) fields (conj at i) evaluated)
                true
                (recur (inc i)))))
      :implies (if (interpretation! a fields (conj at 1) evaluated)
                 (interpretation! b fields (conj at 2) evaluated)
                 true)
      := (= (interpretation! a fields (conj at 1) evaluated)
            (interpretation! b fields (conj at 2) evaluated))
      :not= (not= (interpretation! a fields (conj at 1) evaluated)
                   (interpretation! b fields (conj at 2) evaluated))
      :< (neg? (compare (interpretation! a fields (conj at 1) evaluated)
                        (interpretation! b fields (conj at 2) evaluated)))
      :<= (not (pos? (compare (interpretation! a fields (conj at 1) evaluated)
                             (interpretation! b fields (conj at 2) evaluated))))
      :> (pos? (compare (interpretation! a fields (conj at 1) evaluated)
                        (interpretation! b fields (conj at 2) evaluated)))
      :>= (not (neg? (compare (interpretation! a fields (conj at 1) evaluated)
                             (interpretation! b fields (conj at 2) evaluated))))
      :in (contains? (interpretation! b fields (conj at 2) evaluated)
                     (interpretation! a fields (conj at 1) evaluated))
      (reject! :unsupported-operator at {:operator op}))))

(defn evaluate-expression!
  "Evaluate a CLOSED, statically admitted Boolean expression in a validated
   partial state, deriving only safe branch-sensitive facts. Optional readers
   need a preceding presence guard on every executable path. Nullable values
   need a preceding nonnil guard for order/membership/Boolean positions.
   Both branches of a Boolean operator are type-checked even when one will be
   skipped at runtime. No source/certificate/proof/kernel input is trusted.

   :evaluated-paths records the subexpressions visited at runtime; the
   interpretation never evaluates a skipped branch. Caller-provided state
   provenance, cross-runtime equivalence and implementation correspondence
   remain outside this diagnostic contract."
  [expression reader-types state]
  (let [observed (values/observe-state! reader-types state)
        used (atom #{})
        budget (atom 0)
        checked-type (type-expression! expression reader-types
                                       {:present #{} :nonnull #{}}
                                       [:expression] 0 budget used)
        declared (set (keys reader-types))]
    (nonnullable-boolean! checked-type [:expression])
    (when-not (= @used declared)
      (reject! :reader-footprint-mismatch [:reader-types]
               {:unused (set/difference declared @used)}))
    (let [visited (atom [])
          evaluated (interpretation! expression (:fields observed)
                                     [:expression] visited)]
      {:type result-type
       :version semantics-version
       :expression expression
       :value evaluated
       :source-readers declared
       :evaluated-paths @visited
       :observed-presence (select-keys observed [:present-readers :absent-readers])
       :checking :independent-branch-sensitive-typing
       :evaluation :independent-short-circuit-semantics
       :proof-status :not-proved
       :admission :not-admitted
       :runtime-enforcement :not-established
       :snapshot-provenance :not-established
       :jvm-cljs-equivalence :not-established
       :authority :none})))
