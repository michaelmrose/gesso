(ns gesso.assurance.proof.propositional-soundness
  "Independent, bounded truth-table audit of ORIGINAL v1231 kernel derivation
   sources (v1318). Never consumes a kernel result or trusted report.

   Every typed atomic rule is treated as an opaque Boolean proposition. For
   each valuation, audit the semantic entailment of EVERY purported step
   from its immediately referenced premises/steps, and then the complete
   premise-to-goal entailment. The evaluator here does not call the Gesso
   propositional parser, kernel, rule evaluator, finite-model oracle, or any
   certificate checker. This deliberately independent Boolean interpretation
   can expose inference errors, but does NOT establish S1 atomic semantics,
   undefinedness, soundness on unbounded formula families, or any authority.

   The audit is a diagnostic test instrument, not an admission path."
  (:require [clojure.set :as set]))

(def oracle-version 1)
(def report-type :gesso.assurance.proof.propositional-soundness/bounded-audit)

(def ^:private source-keys #{:subject :variables :premises :steps :goal})
(def ^:private step-keys
  {:premise #{:id :rule :premise :conclusion}
   :truth-introduction #{:id :rule :conclusion}
   :implication-reflexivity #{:id :rule :conclusion}
   :conjunction-introduction #{:id :rule :from :conclusion}
   :conjunction-elimination #{:id :rule :from :index :conclusion}
   :implication-elimination #{:id :rule :from :conclusion}
   :disjunction-introduction #{:id :rule :from :index :conclusion}})
(def ^:private max-nodes 65536)
(def ^:private max-steps 256)
(def ^:private max-premises 128)
(def ^:private max-atoms 20)
(def ^:private max-valuations-cap 4096)

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid source for independent propositional audit."
                  (merge {:error/type ::invalid-source
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentHashMap x)
      (instance? clojure.lang.PersistentArrayMap x)))

(defn- named-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- no-meta! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-keys! [m expected at]
  (when-not (plain-map? m)
    (reject! :expected-plain-map at {}))
  (no-meta! m at)
  (when-not (= (set (keys m)) expected)
    (reject! :wrong-keys at
             {:expected expected :actual (set (keys m))})))

(defn- id! [x at]
  (when-not (named-keyword? x)
    (reject! :invalid-id at {:value x})))

(defn- opaque-data!
  "Audit opaque atom syntax as bounded, inert EDN-like data, without
   interpreting or typing any rule operator. Reject executable objects and
   nested metadata rather than mistaking them for a checked atom."
  [value at counter]
  (loop [todo [[value at]]]
    (when (seq todo)
      (let [[node path] (peek todo)
            todo (pop todo)]
        (when (> (swap! counter inc) max-nodes)
          (reject! :formula-budget-exceeded path {:maximum max-nodes}))
        (no-meta! node path)
        (cond
          (or (nil? node) (boolean? node) (keyword? node)
              (string? node) (number? node))
          (recur todo)

          (instance? clojure.lang.APersistentVector node)
          (recur (into todo (map-indexed
                             (fn [i child] [child (conj path i)]) node)))

          (plain-map? node)
          (recur (into todo (mapcat (fn [[k v]]
                                     [[k (conj path :key)] [v (conj path k)]])
                                   node)))

          (set? node)
          (recur (into todo (map (fn [child] [child (conj path :member)])
                                 node)))

          :else (reject! :non-data-atom path {}))))))

(defn- finite-formula!
  "Validate ONLY propositional connective shape. Atomic rule semantics remain
   opaque. Return the distinct raw atomic declarations used by this formula."
  [form at counter]
  (loop [todo [[form at]] atoms #{}]
    (if (empty? todo)
      atoms
      (let [[node path] (peek todo)
            todo (pop todo)]
        (when (> (swap! counter inc) max-nodes)
          (reject! :formula-budget-exceeded path {:maximum max-nodes}))
        (when-not (instance? clojure.lang.APersistentVector node)
          (reject! :invalid-formula path {}))
        (no-meta! node path)
        (let [[operator & args] node
              argc (count args)
              children (case operator
                         (:true :false) (do (when-not (zero? argc)
                                             (reject! :invalid-arity path {})) [])
                         :atom (do
                                 (when-not (= 1 argc)
                                   (reject! :invalid-arity path {}))
                                 (let [atom (first args)]
                                   (exact-keys! atom #{:readers :expression}
                                                (conj path 1))
                                   (when-not (and (set? (:readers atom))
                                                  (every? named-keyword?
                                                          (:readers atom)))
                                     (reject! :invalid-atom-readers path {}))
                                   (no-meta! (:readers atom) (conj path 1 :readers))
                                   (when-not (instance? clojure.lang.APersistentVector
                                                        (:expression atom))
                                     (reject! :invalid-opaque-atom path {}))
                                   (opaque-data! (:expression atom)
                                                 (conj path 1 :expression)
                                                 counter))
                                 [])
                         :not (do (when-not (= argc 1)
                                    (reject! :invalid-arity path {})) args)
                         (:and :or) (do (when-not (<= 2 argc 127)
                                          (reject! :invalid-arity path {})) args)
                         :implies (do (when-not (= argc 2)
                                        (reject! :invalid-arity path {})) args)
                         (reject! :unknown-connective path {:operator operator}))]
          (recur (into todo (map-indexed
                             (fn [i child] [child (conj path (inc i))])
                             children))
                 (if (= operator :atom) (conj atoms (first args)) atoms)))))))

(defn- all-formulas!
  [source]
  (let [{:keys [variables premises steps goal]} source
        _ (when-not (plain-map? variables)
            (reject! :invalid-variables [:variables] {}))
        _ (no-meta! variables [:variables])
        _ (doseq [k (keys variables)] (id! k [:variables k]))
        _ (when-not (and (plain-map? premises)
                         (<= (count premises) max-premises))
            (reject! :invalid-premises [:premises] {}))
        _ (no-meta! premises [:premises])
        _ (doseq [k (keys premises)] (id! k [:premises k]))
        _ (when-not (and (instance? clojure.lang.APersistentVector steps)
                         (<= 1 (count steps) max-steps))
            (reject! :invalid-steps [:steps] {}))
        _ (no-meta! steps [:steps])
        counter (atom 0)
        atoms (reduce set/union
                      (finite-formula! goal [:goal] counter)
                      (concat
                       (map (fn [[id form]]
                              (finite-formula! form [:premises id] counter))
                            premises)
                       (map-indexed (fn [i step]
                                      (finite-formula! (:conclusion step)
                                                       [:steps i :conclusion] counter))
                                    steps)))
        readers (reduce set/union #{} (map :readers atoms))]
    (when-not (= readers (set (keys variables)))
      (reject! :variable-footprint-mismatch [:variables]
               {:expected readers :actual (set (keys variables))}))
    atoms))

(defn- normalized-steps!
  [source]
  (let [{:keys [premises steps goal]} source]
    (loop [remaining (seq (map-indexed vector steps)) known {} result []]
      (if-not remaining
        (do (when-not (= goal (:conclusion (peek result)))
              (reject! :goal-mismatch [:goal] {}))
            result)
        (let [[i step] (first remaining)
              at [:steps i]
              rule (:rule step)
              expected (get step-keys rule)]
          (when-not expected
            (reject! :unknown-rule (conj at :rule) {:rule rule}))
          (exact-keys! step expected at)
          (id! (:id step) (conj at :id))
          (when (contains? known (:id step))
            (reject! :duplicate-step-id (conj at :id) {}))
          (when (= rule :premise)
            (id! (:premise step) (conj at :premise))
            (when-not (contains? premises (:premise step))
              (reject! :unknown-premise (conj at :premise) {})))
          (when (contains? step :from)
            (let [references (:from step)
                  expected-count (case rule
                                   :conjunction-introduction [2 128]
                                   :conjunction-elimination [1 1]
                                   :implication-elimination [2 2]
                                   :disjunction-introduction [1 1])]
              (when-not (and (instance? clojure.lang.APersistentVector references)
                             (<= (first expected-count) (count references)
                                 (second expected-count)))
                (reject! :invalid-sources (conj at :from) {}))
              (no-meta! references (conj at :from))
              (doseq [ref references]
                (id! ref (conj at :from))
                (when-not (contains? known ref)
                  (reject! :unknown-or-forward-step (conj at :from)
                           {:reference ref})))))
          (when (contains? step :index)
            (when-not (and (integer? (:index step))
                           (<= 0 (:index step)))
              (reject! :invalid-index (conj at :index) {})))
          (recur (next remaining)
                 (assoc known (:id step) (:conclusion step))
                 (conj result
                       {:id (:id step) :rule rule :conclusion (:conclusion step)
                        :antecedents (if (= :premise rule)
                                       [(get premises (:premise step))]
                                       (mapv known (:from step)))})))))))

(defn- boolean-value
  "Independent truth-functional interpretation. Each atom is opaque and
   assigned separately, without interpreting its typed expression."
  [form valuation]
  (case (first form)
    :true true
    :false false
    :atom (get valuation (second form))
    :not (not (boolean-value (second form) valuation))
    :and (every? #(boolean-value % valuation) (subvec form 1))
    :or (boolean (some #(boolean-value % valuation) (subvec form 1)))
    :implies (or (not (boolean-value (second form) valuation))
                 (boolean-value (nth form 2) valuation))))

(defn- valuation-at [atoms n]
  (into {} (map-indexed (fn [i atom]
                          [atom (bit-test (long n) i)]) atoms)))

(defn- base-report [atoms]
  {:type report-type
   :version oracle-version
   :scope :independent-bounded-propositional-skeleton-only
   :atomic-semantics :opaque-total-booleans-assumed
   :source-type-validation :not-performed
   :assumption-discharge :not-performed
   :physical-refinement :not-established
   :checker-soundness :not-established
   :certificate :not-issued
   :runtime-enforcement :not-established
   :authority :none
   :atom-count (count atoms)
   :atoms atoms})

(defn audit-derivation!
  "Inspect original raw kernel SOURCE under every Boolean valuation of its
   opaque atoms, bounded by :max-valuations (default 4096). Validity of each
   inference is checked separately from its immediately cited sources. A
   success is a *bounded semantic audit*, not an admission or proof report.

   Returns :counterexample (with first offending valuation and step),
   :bounded-no-counterexample, :vacuous-premises, or :budget-exceeded.
   Malformed sources throw ::invalid-source with a typed :reason/:at."
  ([source] (audit-derivation! source {}))
  ([source options]
   (exact-keys! source source-keys [])
   (exact-keys! options (if (contains? options :max-valuations)
                          #{:max-valuations} #{}) [:options])
   (let [max-valuations (:max-valuations options max-valuations-cap)]
     (when-not (and (integer? max-valuations)
                    (<= 1 max-valuations max-valuations-cap))
       (reject! :invalid-valuation-limit [:options :max-valuations] {}))
     (let [atoms-set (all-formulas! source)
           steps (normalized-steps! source)
           atoms (vec (sort-by pr-str atoms-set))
           base (base-report atoms)
           n (count atoms)
           required (bit-shift-left 1 (min n 20))]
       (if (or (> n max-atoms) (> required max-valuations))
         (assoc base :result :budget-exceeded
                :required-valuations (if (> n max-atoms) :above-max-atoms required)
                :valuation-limit max-valuations
                :valuations-checked 0
                :step-implications-tested 0
                :goal-implications-tested 0)
         (loop [index 0 satisfied 0]
           (if (= index required)
             (assoc base
                    :result (if (zero? satisfied)
                              :vacuous-premises :bounded-no-counterexample)
                    :premise-satisfying-valuations satisfied
                    :valuations-checked required
                    :step-implications-tested (* required (count steps))
                    :goal-implications-tested required)
             (let [valuation (valuation-at atoms index)
                   failed (first
                           (keep-indexed
                            (fn [i {:keys [antecedents conclusion id rule]}]
                              (when (and (every? #(boolean-value % valuation)
                                                 antecedents)
                                         (not (boolean-value conclusion valuation)))
                                {:failure-kind :invalid-step-entailment
                                 :step-index i :step-id id :rule rule
                                 :premises antecedents
                                 :conclusion conclusion}))
                            steps))
                   premises-true?
                   (every? #(boolean-value % valuation)
                           (vals (:premises source)))
                   goal-failed? (and (not failed) premises-true?
                                     (not (boolean-value (:goal source)
                                                         valuation)))]
               (if (or failed goal-failed?)
                 (merge base
                        {:result :counterexample
                         :valuation-index index
                         :valuation valuation
                         :valuations-checked (inc index)
                         :step-implications-tested (+ (* index (count steps))
                                                      (if failed
                                                        (inc (:step-index failed))
                                                        (count steps)))
                         :goal-implications-tested (if failed index (inc index))
                         :premise-satisfying-valuations
                         (if premises-true? (inc satisfied) satisfied)}
                        (or failed {:failure-kind :invalid-goal-entailment
                                    :goal (:goal source)}))
                 (recur (inc index)
                        (if premises-true? (inc satisfied) satisfied)))))))))))
