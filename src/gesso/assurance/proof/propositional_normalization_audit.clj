(ns gesso.assurance.proof.propositional-normalization-audit
  "v1337: independent source-relative audit of propositional normalization.

   The v1229 proposition normalizer changes each typed atomic declaration to
   its canonical semantics map. This module checks the WHOLE connective tree
   against the original source, independently reconstructs the restricted
   expected atomic representation, and compares source and canonical shadows
   for every opaque Boolean valuation within explicit budgets.

   Atom truth is only transported along a checked source-to-canonical mapping;
   NO proof of typed-rule denotational equivalence is inferred. Such a claim
   still needs S1. No kernel admission or certificate is issued."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.propositional-semantics :as semantics]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.rule-eval :as rule-eval]))

(def audit-version 1)
(def report-type :gesso.assurance.proof.propositional-normalization-audit/source-relative)
(def ^:private max-valuation-atoms 8)
(def ^:private max-valuation-work 50000)
(def ^:private normalization-version 1) ; fail closed if the source language changes

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso proposition normalization correspondence."
                  (merge {:error/type ::invalid-normalization
                          :reason reason :at at}
                         details))))

(defn- no-meta! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (reject! :metadata-not-allowed at {})))

(defn- no-metadata-tree!
  "Metadata is not part of Clojure's value equality. Inspect the produced
   canonical atom recursively so metadata cannot be smuggled past = checks."
  [value at]
  (loop [todo [[value at]]]
    (when (seq todo)
      (let [[item path] (peek todo)
            rest-todo (pop todo)]
        (no-meta! item path)
        (recur
         (cond
           (or (instance? clojure.lang.PersistentArrayMap item)
               (instance? clojure.lang.PersistentHashMap item))
           (into rest-todo (mapcat (fn [[k v]] [[k (conj path :key)]
                                                   [v (conj path k)]]) item))

           (instance? clojure.lang.APersistentVector item)
           (into rest-todo (map-indexed (fn [i v] [v (conj path i)]) item))

           (instance? clojure.lang.PersistentHashSet item)
           (into rest-todo (map (fn [v] [v (conj path :member)]) item))

           :else rest-todo))))))

(defn- canonical-number [x]
  (if (integer? x) (bigint x) x))

(defn- canonical-literal [v]
  (if (set? v)
    (let [new-value (into #{} (map canonical-number) v)]
      (when-not (= (count v) (count new-value))
        (reject! :literal-collision [:expression] {}))
      new-value)
    (canonical-number v)))

(defn- expected-expression
  "A second, source-oriented spelling of the v1225 *structural* rewrites.
   Original expression syntax has already been admitted by v1229."
  [expr]
  (case (first expr)
    :literal [:literal (canonical-literal (second expr))]
    :read [:read (second expr)]
    (into [(first expr)] (map expected-expression (rest expr)))))

(defn- expected-type [descriptor]
  (cond-> {:type (:type descriptor)
           :optional? (true? (:optional? descriptor))
           :nullable? (true? (:nullable? descriptor))}
    (= :enum (:type descriptor))
    (assoc :name (:name descriptor) :values (:values descriptor))))

(defn- expected-atom [declaration types]
  (let [readers (:readers declaration)]
    {:normalization-version normalization-version
     :syntax-version rule/rule-version
     :type-version rule-type/type-version
     :evaluator-version rule-eval/evaluator-version
     :readers readers
     :reader-types (into {} (map (fn [id] [id (expected-type (get types id))]))
                         readers)
     :expression (expected-expression (:expression declaration))}))

(defn- shadow-atom [canonical]
  ;; The independent v1331 Boolean interpreter accepts original-style opaque
  ;; atoms. The canonical versions and nominal types have already been checked
  ;; against the source, but are not interpreted as predicates here.
  {:readers (:readers canonical) :expression (:expression canonical)})

(defn- aligned-shadow!
  [source normalized types at pairs]
  (no-meta! normalized at)
  (when-not (and (vector? normalized)
                 (= (count source) (count normalized))
                 (= (first source) (first normalized)))
    (reject! :connective-structure-mismatch at
             {:source-operator (first source)
              :normalized-operator (when (vector? normalized)
                                     (first normalized))}))
  (case (first source)
    (:true :false) source
    :atom
    (let [original (second source)
          expected (expected-atom original types)
          actual (second normalized)]
      (no-metadata-tree! actual (conj at 1))
      (when-not (= expected actual)
        (reject! :atomic-canonicalization-mismatch (conj at 1)
                 {:source-readers (:readers original)}))
      (swap! pairs conj {:source original :canonical (shadow-atom expected)})
      [:atom (shadow-atom expected)])
    (into [(first source)]
          (map-indexed (fn [index child]
                         (aligned-shadow! child (nth normalized (inc index))
                                          types (conj at (inc index)) pairs))
                       (rest source)))))

(defn- distinct-atomic-map!
  [pairs]
  (reduce (fn [mapping {:keys [source canonical]}]
            (when (and (contains? mapping source)
                       (not= (get mapping source) canonical))
              (reject! :inconsistent-atom-mapping [:proposition] {}))
            (assoc mapping source canonical))
          {} pairs))

(defn- valuation [ordered index]
  (into {} (map-indexed (fn [i atom] [atom (bit-test (long index) i)]) ordered)))

(defn- base-report [checked pairs atom-map]
  {:type report-type
   :version audit-version
   :scope :source-relative-propositional-normalization
   :source-fingerprint (:fingerprint checked)
   :proposition-version proposition/proposition-version
   :connective-shape :exact-match
   :atomic-structure :independently-reconstructed
   :atomic-occurrences (count pairs)
   :distinct-source-atoms (count atom-map)
   :typed-atomic-denotational-equivalence :not-established
   :universal-normalizer-implementation-soundness :not-established
   :kernel-derivation-soundness :not-established
   :assumption-discharge :not-performed
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(defn audit-proposition!
  "Recheck one ORIGINAL v1229 proposition declaration. Independently compare
   the normalizer's connective tree and expected atomic canonicalization.
   For <=8 distinct source atoms, exhaustively compare the source and canonical
   Boolean shadows on every source valuation, subject to work limits.

   This proves only source-instance *propositional shape* preservation when
   checked. It explicitly DOES NOT prove that the typed atom before and after
   canonicalization have the same value on all typed states."
  [source]
  (let [checked (proposition/describe-proposition! source)
        normalized (get-in checked [:semantics :proposition])
        pairs (atom [])
        shadow (aligned-shadow! (:proposition source) normalized
                                (:variables source) [:proposition] pairs)
        atom-map (distinct-atomic-map! @pairs)
        base (base-report checked @pairs atom-map)
        atom-count (count atom-map)
        collisions (not= atom-count (count (set (vals atom-map))))
        required (bit-shift-left 1 atom-count)
        work (* required (+ 1 (:nodes checked)))]
    (cond
      collisions
      (assoc base :status :not-established :reason :canonical-atom-collision
             :valuations-checked 0)

      (> atom-count max-valuation-atoms)
      (assoc base :status :not-checked :reason :atom-budget
             :maximum-atoms max-valuation-atoms :valuations-checked 0)

      (> work max-valuation-work)
      (assoc base :status :not-checked :reason :evaluation-work-budget
             :maximum-work max-valuation-work :valuations-checked 0)

      :else
      (let [ordered (vec (sort-by pr-str (keys atom-map)))]
        (loop [i 0]
          (if (= i required)
            (assoc base :status :preserved-for-all-opaque-valuations
                   :valuations-checked required
                   :correspondence :source-to-normalized-atomic-valuation
                   :semantic-scope :opaque-propositional-connectives)
            (let [values (valuation ordered i)
                  canonical-values (into {} (map (fn [[atom value]]
                                                   [(get atom-map atom) value]))
                                         values)
                  source-value (semantics/truth-at! (:proposition source) values)
                  normalized-value (semantics/truth-at! shadow canonical-values)]
              (if (= source-value normalized-value)
                (recur (inc i))
                (assoc base :status :counterexample :valuations-checked (inc i)
                       :valuation values)))))))))
