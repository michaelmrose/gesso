(ns gesso.assurance.proof.frame-soundness
  "Restricted S4 frame-preservation rule schema (v1353).

   For an admitted abstract transition T, let F be its exact set of
   unwritten state fields and let R be the syntactically complete reader
   footprint of a well-typed, deterministic invariant I. Then:

     R subset F, T(s,x,s') => projection_R(s) = projection_R(s')
     projection_R(s) = projection_R(s') => I(s) = I(s')

   The first implication follows by the EXACT frame definition (including
   optional presence); the second is the extensionality premise for pure
   typed expressions. Thus I(s) and T(s,x,s') imply I(s'). A false guard
   never establishes a matching transition. Invariants reading written
   fields remain OPEN even if one observed write happened to preserve them.

   derive-frame-lemma! validates source using the v1233 production transition
   describer and typed-rule admission, but independently reconstructs the
   frame and invariant's syntactic reader footprint. It instantiates a
   CONDITIONAL rule schema,
   not a proof of the production describer's or typed evaluator's soundness.
   audit-frame-witness! separately checks one caller-supplied witness using
   v1351 independent transition semantics and v1344 independent S1 typing.

   Neither function issues certificates or admits real XTDB transactions.
   Universal S1 extensionality, implementation correspondence, writer
   completeness, and authoritative snapshot provenance remain OPEN."
  (:require [clojure.set :as set]
            [gesso.assurance.rule-normalize :as typed-rule]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.typed-presence-semantics :as presence]))

(def semantics-version 1)
(def report-type :gesso.assurance.proof.frame-soundness/conditional-lemma)
(def witness-type :gesso.assurance.proof.frame-soundness/witness-audit)

(def ^:private invariant-keys #{:id :readers :expression})
(def ^:private max-expression-depth 64)
(def ^:private max-expression-nodes 8192)
(def ^:private max-expression-arity 256)
(def ^:private atom-operators #{:= :not= :< :<= :> :>= :in :implies})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid conditional frame-soundness construction."
                  (merge {:frame-soundness/rejection true
                          :reason reason :at at} details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- plain-set? [v]
  (instance? clojure.lang.PersistentHashSet v))

(defn- plain-vector? [v]
  (instance? clojure.lang.APersistentVector v))

(defn- metadata-free! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (reject! :metadata-not-allowed at {})))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- expression-readers!
  "Extract all source reader occurrences without evaluating the expression.
   This prevents a forged :readers declaration from hiding written fields.
   The independent S1 interpreter performs the actual static type check."
  [expression at depth remaining]
  (when (> depth max-expression-depth)
    (reject! :expression-depth-limit at {}))
  (when (neg? (swap! remaining dec))
    (reject! :expression-node-limit at {}))
  (when-not (and (plain-vector? expression)
                 (<= 2 (count expression) max-expression-arity))
    (reject! :invalid-expression at {}))
  (metadata-free! expression at)
  (let [[op & args] expression
        n (count args)
        descend (fn [i child]
                  (expression-readers! child (conj at i)
                                       (inc depth) remaining))]
    (cond
      (= op :read)
      (do
        (when-not (and (= n 1) (qualified-id? (first args)))
          (reject! :invalid-read at {}))
        #{(first args)})

      (= op :literal)
      (do
        (when-not (= n 1) (reject! :invalid-literal at {}))
        ;; Full literal type and metadata validation belongs to independent S1.
        #{})

      (= op :present)
      (do
        (when-not (and (= n 1) (plain-vector? (first args))
                       (= :read (first (first args)))
                       (= 2 (count (first args))))
          (reject! :present-requires-reader at {}))
        (descend 1 (first args)))

      (= op :not)
      (do (when-not (= n 1) (reject! :wrong-arity at {:operator op}))
          (descend 1 (first args)))

      (#{:and :or} op)
      (do (when (< n 2) (reject! :wrong-arity at {:operator op}))
          (apply set/union #{}
                 (map-indexed (fn [i v] (descend (inc i) v)) args)))

      (atom-operators op)
      (do (when-not (= n 2) (reject! :wrong-arity at {:operator op}))
          (set/union (descend 1 (first args))
                     (descend 2 (second args))))

      :else (reject! :unsupported-operator at {:operator op}))))

(defn- exact-invariant! [invariant state-types]
  (when-not (plain-map? invariant)
    (reject! :expected-invariant-map [:invariant] {}))
  (metadata-free! invariant [:invariant])
  (when-not (= invariant-keys (set (keys invariant)))
    (reject! :invariant-keys [:invariant]
             {:expected invariant-keys :actual (set (keys invariant))}))
  (when-not (qualified-id? (:id invariant))
    (reject! :invalid-invariant-id [:invariant :id] {}))
  (let [declared (:readers invariant)]
    (when-not (plain-set? declared)
      (reject! :expected-reader-set [:invariant :readers] {}))
    (metadata-free! declared [:invariant :readers])
    (when-not (every? qualified-id? declared)
      (reject! :unqualified-reader [:invariant :readers] {}))
    (let [actual (expression-readers! (:expression invariant)
                                      [:invariant :expression] 0
                                      (atom max-expression-nodes))]
      (when-not (= actual declared)
        (reject! :invariant-reader-footprint-mismatch [:invariant :readers]
                 {:expected actual :actual declared}))
      (when-not (set/subset? actual (set (keys state-types)))
        (reject! :invariant-reads-outside-state [:invariant :readers]
                 {:unknown (set/difference actual (set (keys state-types)))}))
      actual)))

(defn derive-frame-lemma!
  "Derive a source-relative instance of the universal FRAME rule schema.
   Eligible means structural premises are satisfied; it does not certify
   the universal soundness of S1 typing or the production transition checker.
   A written-field dependency is OPEN, not a failed invariant proof."
  [declaration invariant]
  (let [description (transition/describe-transition! declaration)
        state-types (:state-types declaration)
        frame (set/difference (set (keys state-types)) (:writes declaration))
        _ (when-not (= frame (get-in description [:semantics :frame-fields]))
            (reject! :transition-frame-disagreement [:transition] {}))
        readers (exact-invariant! invariant state-types)
        typed-admission (typed-rule/normalize-rule!
                         (select-keys invariant [:readers :expression])
                         (select-keys state-types readers))
        _ (when-not (= readers (get-in typed-admission [:semantics :readers]))
            (reject! :typed-reader-footprint-disagreement [:invariant] {}))
        written-readers (set/difference readers frame)
        eligible? (empty? written-readers)]
    {:type report-type
     :version semantics-version
     :transition-fingerprint (:fingerprint description)
     :invariant-id (:id invariant)
     :source-invariant invariant
     :typed-invariant-fingerprint (:fingerprint typed-admission)
     :typed-invariant-admission :production-typed-source-check
     :frame-fields frame
     :readers readers
     :written-readers written-readers
     :premises #{:valid-typed-invariant
                 :exact-abstract-transition-frame
                 :pure-typed-expression-extensionality}
     :schema :equal-reader-projections-imply-equal-invariant-values
     :conditional-claim (when eligible?
                          :matching-transition-preserves-invariant-truth)
     :status (if eligible? :conditional-frame-rule-instance
                 :open-written-reader)
     :typed-expression-soundness :not-independently-proved
     :transition-implementation-soundness :not-independently-proved
     :proof-status :conditional-schema-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- reader-projection-equal?
  [before after readers]
  (every? (fn [field]
            (and (= (contains? before field) (contains? after field))
                 (or (not (contains? before field))
                     (= (get before field) (get after field)))))
          readers))

(defn audit-frame-witness!
  "Check an individual before/input/after witness against the lemma.
   Even identical pre/post invariant values do NOT justify a frame lemma
   when an invariant reads a written field. Nonmatching relations are
   inapplicable, not counterexamples to the conditional theorem."
  [declaration invariant before inputs after]
  (let [lemma (derive-frame-lemma! declaration invariant)
        observed (independent/observe-transition! declaration before inputs after)
        readers (:readers lemma)
        types (select-keys (:state-types declaration) readers)
        pre-value (:value (presence/evaluate-expression!
                           (:expression invariant) types
                           (select-keys before readers)))
        post-value (:value (presence/evaluate-expression!
                            (:expression invariant) types
                            (select-keys after readers)))
        equal-projection? (reader-projection-equal? before after readers)
        matches? (:matches? observed)
        eligible? (= :conditional-frame-rule-instance (:status lemma))]
    (when (and matches? eligible?
               (or (not equal-projection?) (not= pre-value post-value)))
      (reject! :independent-frame-extensionality-disagreement
               [:witness] {:invariant (:id invariant)}))
    {:type witness-type
     :version semantics-version
     :transition-fingerprint (:transition-fingerprint lemma)
     :invariant-id (:id invariant)
     :lemma-status (:status lemma)
     :matches-transition? matches?
     :reader-projections-equal? equal-projection?
     :before-invariant pre-value
     :after-invariant post-value
     :preservation-implication-holds? (or (not pre-value) post-value)
     :status (cond
               (not eligible?) :open-written-reader
               (not matches?) :nonmatching-transition
               :else :checked-conditional-frame-instance)
     :checking :independent-abstract-relation-and-typed-expression
     :proof-status :not-universally-proved
     :physical-observation :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
