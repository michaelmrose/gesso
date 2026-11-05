(ns gesso.assurance.proof.propositional-semantics
  "Independent truth-functional semantics for the closed Gesso proposition
   syntax (v1331). This is a REFERENCE INTERPRETATION, not an admission path.

   Formula syntax: [:true], [:false], [:atom <original rule declaration>],
   [:not P], [:and P Q ...], [:or P Q ...], [:implies P Q]. Each distinct
   *entire* atomic declaration denotes an independent Boolean variable.
   Its Clojure data is inert: this module neither interprets typed rules nor
   calls proposition normalization, the derivation kernel, or other oracles.

   Entailment means: for EVERY total Boolean valuation of the distinct atoms
   occurring in the supplied premises and goal, if all premises are true,
   the goal is true. An executable exhaustive decision is available only
   within explicit atomic/cardinality/work limits. An over-budget query is
   NOT a proof. Even a complete propositional truth-table result establishes
   no typed-atom validity, implementation refinement, or certificate authority.

   All public operations accept SOURCE DATA, not cached reports. Bounded,
   metadata-free EDN-like syntax prevents executable atoms and unbounded
   expression trees from entering the reference semantics."
  (:require [clojure.set :as set]))

(def semantics-version 1)
(def interpretation-type
  :gesso.assurance.proof.propositional-semantics/opaque-boolean-entailment)

(def ^:private max-formula-nodes 8192)
(def ^:private max-opaque-nodes 65536)
(def ^:private max-depth 64)
(def ^:private max-arity 128)
(def ^:private max-premises 128)
(def ^:private max-valuation-atoms 12)
(def ^:private max-evaluation-work 500000)

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso independent propositional semantics input."
                  (merge {:error/type ::invalid-source
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- no-meta! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- named-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- opaque-data!
  "Check inert atomic payloads without deriving their semantic truth.
   A separate limit prevents large atomic expressions from bypassing the
   connective-tree budget. The same counter spans every supplied formula."
  [value at counter]
  (loop [pending [[value at]]]
    (when (seq pending)
      (let [[node path] (peek pending)
            pending (pop pending)]
        (when (> (swap! counter inc) max-opaque-nodes)
          (reject! :opaque-node-limit path {:maximum max-opaque-nodes}))
        (no-meta! node path)
        (cond
          (or (nil? node) (boolean? node) (keyword? node)
              (string? node) (number? node))
          (recur pending)

          (plain-vector? node)
          (recur (into pending (map-indexed
                               (fn [i child] [child (conj path i)]) node)))

          (plain-map? node)
          (recur (into pending (mapcat
                               (fn [[k v]] [[k (conj path :key)]
                                            [v (conj path k)]]) node)))

          (set? node)
          (recur (into pending (map (fn [child]
                                     [child (conj path :member)]) node)))

          :else
          (reject! :executable-or-unsupported-atom path
                   {:class (str (class node))}))))))

(defn- inspect-formula!
  [form at connective-counter opaque-counter]
  (loop [pending [[form at 0]] atoms #{}]
    (if (empty? pending)
      atoms
      (let [[node path depth] (peek pending)
            pending (pop pending)]
        (when (> depth max-depth)
          (reject! :formula-depth-limit path {:maximum max-depth}))
        (when (> (swap! connective-counter inc) max-formula-nodes)
          (reject! :formula-node-limit path {:maximum max-formula-nodes}))
        (when-not (plain-vector? node)
          (reject! :expected-formula-vector path {}))
        (no-meta! node path)
        (let [operator (first node)
              arity (dec (count node))
              children
              (case operator
                (:true :false)
                (do (when-not (zero? arity)
                      (reject! :invalid-arity path {:operator operator})) [])

                :atom
                (do (when-not (= arity 1)
                      (reject! :invalid-arity path {:operator operator}))
                    (let [declaration (second node)]
                      (when-not (plain-map? declaration)
                        (reject! :invalid-atomic-declaration path {}))
                      (no-meta! declaration (conj path 1))
                      (when-not (= #{:readers :expression}
                                   (set (keys declaration)))
                        (reject! :invalid-atomic-keys path {}))
                      (when-not (and (set? (:readers declaration))
                                     (every? named-keyword?
                                             (:readers declaration)))
                        (reject! :invalid-atomic-readers path {}))
                      (when-not (plain-vector? (:expression declaration))
                        (reject! :invalid-atomic-expression path {}))
                      (opaque-data! declaration (conj path 1) opaque-counter))
                    [])

                :not
                (do (when-not (= arity 1)
                      (reject! :invalid-arity path {:operator operator}))
                    (subvec node 1))

                (:and :or)
                (do (when-not (<= 2 arity (dec max-arity))
                      (reject! :invalid-arity path {:operator operator}))
                    (subvec node 1))

                :implies
                (do (when-not (= arity 2)
                      (reject! :invalid-arity path {:operator operator}))
                    (subvec node 1))

                (reject! :unknown-connective path {:operator operator}))]
          (recur (into pending (map-indexed
                                (fn [i child]
                                  [child (conj path (inc i)) (inc depth)])
                                children))
                 (if (= operator :atom) (conj atoms (second node)) atoms)))))))

(defn- inspect!
  "Validate a collection of [source-location formula] pairs once."
  [located-formulas]
  (let [connective-counter (atom 0)
        opaque-counter (atom 0)
        atoms (reduce (fn [known [path formula]]
                        (set/union known
                                   (inspect-formula! formula path
                                                     connective-counter
                                                     opaque-counter)))
                      #{} located-formulas)]
    {:atoms atoms
     :connective-nodes @connective-counter
     :opaque-nodes @opaque-counter}))

(defn atoms!
  "Return distinct *opaque atomic declarations* of a well-formed formula.
   This does not type-check their expressions or assign truth values."
  [formula]
  (:atoms (inspect! [[[:formula] formula]])))

(defn- truth
  "Total mathematical interpretation, called ONLY on previously validated
   formulas and valuations. Connectives are truth-functional; the atomic
   declaration is an opaque key, not an executable predicate."
  [formula valuation]
  (case (first formula)
    :true true
    :false false
    :atom (get valuation (second formula))
    :not (not (truth (second formula) valuation))
    :and (every? #(truth % valuation) (subvec formula 1))
    :or (boolean (some #(truth % valuation) (subvec formula 1)))
    :implies (or (not (truth (second formula) valuation))
                 (truth (nth formula 2) valuation))))

(defn- total-valuation!
  [valuation atoms]
  (when-not (plain-map? valuation)
    (reject! :invalid-valuation [:valuation] {}))
  (no-meta! valuation [:valuation])
  (when-not (= atoms (set (keys valuation)))
    (reject! :non-total-valuation [:valuation]
             {:missing (set/difference atoms (set (keys valuation)))
              :unexpected (set/difference (set (keys valuation)) atoms)}))
  (doseq [[atom value] valuation]
    (when-not (boolean? value)
      (reject! :non-boolean-assignment [:valuation atom] {:value value})))
  valuation)

(defn truth-at!
  "Interpret a closed formula at an EXACT, total Boolean valuation.
   Missing or extra atom assignments are rejected; false is not a default.
   This makes no statement about the typed interpretation of any atom."
  [formula valuation]
  (let [atoms (:atoms (inspect! [[[:formula] formula]]))]
    (truth formula (total-valuation! valuation atoms))))

(defn- premises!
  [premises]
  (when-not (and (plain-map? premises)
                 (<= (count premises) max-premises))
    (reject! :invalid-premises [:premises] {}))
  (no-meta! premises [:premises])
  (doseq [id (keys premises)]
    (when-not (named-keyword? id)
      (reject! :invalid-premise-id [:premises id] {:value id})))
  premises)

(defn- valuation-at [ordered-atoms index]
  (into {} (map-indexed
            (fn [i atomic] [atomic (bit-test (long index) i)])
            ordered-atoms)))

(defn entailment-at!
  "For one EXACT total valuation, return whether premises semantically imply
   the goal, i.e. (not (and premises)) OR goal. This is a pointwise result,
   NEVER a universal entailment claim. Premises use qualified IDs as in the
   derivation source language."
  [premises goal valuation]
  (premises! premises)
  (let [located (cons [[:goal] goal]
                      (map (fn [[id formula]] [[:premises id] formula]) premises))
        atoms (:atoms (inspect! located))]
    (total-valuation! valuation atoms)
    (or (not (every? #(truth % valuation) (vals premises)))
        (truth goal valuation))))

(defn enumerate-entailment!
  "Decide pure propositional entailment by enumerating *all* 2^n valuations
   when the explicit budget permits. Atomic declarations are independent
   Booleans, even when their hidden typed expressions are related.

   Returns :counterexample, :entails, :vacuously-entails, or :not-checked.
   In the last case no valuation was checked and NO entailment is asserted.
   :entails is exact for the finite Boolean abstraction, NOT a proof that
   any typed atom is valid or that an actual app is certified."
  [premises goal]
  (premises! premises)
  (let [located (cons [[:goal] goal]
                      (map (fn [[id formula]] [[:premises id] formula]) premises))
        {:keys [atoms connective-nodes]} (inspect! located)
        n (count atoms)
        valuations (when (<= n max-valuation-atoms)
                     (bit-shift-left 1 n))
        work (when valuations (* valuations connective-nodes))
        base {:type interpretation-type
              :version semantics-version
              :scope :complete-opaque-boolean-valuations-only
              :typed-atomic-semantics :not-interpreted
              :proof-kernel :not-invoked
              :certificate :not-issued
              :authority :none
              :atom-count n
              :formula-nodes connective-nodes}]
    (if (or (nil? valuations) (> work max-evaluation-work))
      (assoc base
             :result :not-checked
             :reason (if (nil? valuations) :atom-budget :work-budget)
             :maximum-atoms max-valuation-atoms
             :maximum-work max-evaluation-work
             :valuations-checked 0)
      (let [ordered-atoms (vec (sort-by pr-str atoms))]
        (loop [index 0 premise-satisfying 0]
          (if (= index valuations)
            (assoc base :result (if (zero? premise-satisfying)
                                  :vacuously-entails :entails)
                   :premise-satisfying-valuations premise-satisfying
                   :valuations-checked valuations)
            (let [valuation (valuation-at ordered-atoms index)
                  holds (every? #(truth % valuation) (vals premises))]
              (if (and holds (not (truth goal valuation)))
                (assoc base :result :counterexample
                       :valuation valuation
                       :valuation-index index
                       :valuations-checked (inc index)
                       :premise-satisfying-valuations (inc premise-satisfying))
                (recur (inc index)
                       (if holds (inc premise-satisfying) premise-satisfying))))))))))
