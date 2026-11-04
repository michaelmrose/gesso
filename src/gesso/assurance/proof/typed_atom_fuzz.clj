(ns gesso.assurance.proof.typed-atom-fuzz
  "Deterministic, bounded generated-expression cross-check (v1322).

   Generate syntactically closed, well-typed Boolean rule templates from a
   supplied ORIGINAL typed-reader universe. Every candidate is interpreted by
   the independent v1320 typed-atom oracle and cross-checked against the real
   production evaluator. On divergence, greedily reduce the counterexample
   while retaining its precise discrepancy. These results are REPRODUCIBLE
   DIAGNOSTICS, not formal proof, runtime enforcement, or transaction authority.

   This tool samples a restricted expression grammar: even complete finite
   binding domains do NOT imply exhaustive expression-language coverage.
   It does not independently authenticate either interpreter or its inputs."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.typed-atom-oracle :as atom]))

(def fuzz-version 1)
(def report-type :gesso.assurance.proof.typed-atom-fuzz/generated-crosscheck)
(def ^:private maximum-readers 8)
(def ^:private maximum-trials 256)
(def ^:private maximum-cases 8192)
(def ^:private maximum-total-cases 32768)
(def ^:private maximum-shrink-audits 64)

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentHashMap v)
      (instance? clojure.lang.PersistentArrayMap v)))

(defn- plain-vector? [v] (instance? clojure.lang.APersistentVector v))

(defn- no-meta? [v]
  (or (not (instance? clojure.lang.IObj v)) (nil? (meta v))))

(defn- fail! [reason at]
  (throw (ex-info "Invalid generated typed-atom audit configuration."
                  {:typed-atom-fuzz/failure true :reason reason :at at})))

(defn- validate! [config]
  (when-not (and (plain-map? config) (no-meta? config))
    (fail! :expected-unannotated-map []))
  (when-not (= #{:reader-types :binding-domains :seed :max-expressions
                 :max-cases :max-total-cases} (set (keys config)))
    (fail! :wrong-keys []))
  (let [{:keys [reader-types binding-domains seed max-expressions
                max-cases max-total-cases]} config]
    (when-not (and (plain-map? reader-types) (no-meta? reader-types)
                   (<= (count reader-types) maximum-readers))
      (fail! :invalid-reader-types [:reader-types]))
    (when-not (and (plain-map? binding-domains) (no-meta? binding-domains)
                   (= (set (keys reader-types)) (set (keys binding-domains))))
      (fail! :invalid-binding-domains [:binding-domains]))
    (when-not (and (integer? seed) (<= 0 seed 2147483647))
      (fail! :invalid-seed [:seed]))
    ;; Each reader gets a mandatory presence-expression probe, irrespective of
    ;; the shuffled generated corpus. No declared reader is silently skipped.
    (when-not (and (integer? max-expressions)
                   (<= (max 1 (count reader-types)) max-expressions
                       maximum-trials))
      (fail! :invalid-trial-budget [:max-expressions]))
    (when-not (and (integer? max-cases) (<= 1 max-cases maximum-cases))
      (fail! :invalid-case-budget [:max-cases]))
    (when-not (and (integer? max-total-cases)
                   (<= max-cases max-total-cases maximum-total-cases))
      (fail! :invalid-total-budget [:max-total-cases]))
    config))

(defn- reader-expr [id] [:read id])
(defn- presence [id] [:present (reader-expr id)])

(defn- present-values [descriptor entries]
  (let [allowed (fn [v]
                  (and (not= v atom/absent) (some? v)
                       (case (:type descriptor)
                         :boolean (instance? Boolean v)
                         :integer (integer? v)
                         :string (string? v)
                         :keyword (keyword? v)
                         :enum (and (keyword? v)
                                    (contains? (:values descriptor) v))
                         false)))]
    (vec (take 3 (distinct (filter allowed entries))))))

(defn- safely-read [id descriptor expr]
  (let [r (reader-expr id)
        nonnull (if (true? (:nullable? descriptor))
                  [:or [:= r [:literal nil]] expr]
                  expr)]
    (if (true? (:optional? descriptor))
      [:or [:not (presence id)] nonnull]
      nonnull)))

(defn- atoms-for-reader [id descriptor entries]
  (let [r (reader-expr id)
        values (present-values descriptor entries)
        primary (case (:type descriptor)
                  :boolean (when (seq values) [r [:not r]])
                  :integer (mapcat (fn [v]
                                     [[:> r [:literal v]]
                                      [:<= r [:literal v]]
                                      [:= r [:literal v]]]) values)
                  (:string :keyword :enum)
                  (mapcat (fn [v] [[:= r [:literal v]]
                                   [:not= r [:literal v]]]) values)
                  nil)
        member-set (when (seq values)
                     (if (= :boolean (:type descriptor)) nil
                         [:in r [:literal (set values)]]))
        ;; The :present probe itself does not read the optional value.
        generated (concat [(presence id)]
                          (when (true? (:nullable? descriptor))
                            [[:and (presence id) [:= r [:literal nil]]]])
                          (map #(safely-read id descriptor %)
                               (cond-> (vec primary) member-set (conj member-set))))]
    (vec (distinct generated))))

(declare readers-in)

(defn- all-expressions [config]
  (let [ids (sort-by str (keys (:reader-types config)))
        per-field (mapv (fn [id]
                          (atoms-for-reader id
                                            (get-in config [:reader-types id])
                                            (get-in config [:binding-domains id])))
                        ids)
        atoms (vec (distinct (concat [[:literal true] [:literal false]]
                                     (mapcat identity per-field))))
        ;; Deliberately finite grammar; fixed ordering before seeded shuffle.
        simple (vec (take 64 atoms))
        pairs (for [a simple, b simple
                    :when (empty? (set/intersection (readers-in a)
                                                    (readers-in b)))
                    :when (not= a b)
                    op [:and :or :implies]]
                [op a b])
        negations (mapv (fn [a] [:not a]) simple)
        compound (for [a (take 12 simple)
                       b (take 12 simple)
                       c (take 12 simple)
                       :when (and (not= a b) (not= b c) (not= a c)
                                  (empty? (set/intersection (readers-in a)
                                                                  (readers-in b)))
                                  (empty? (set/intersection (readers-in b)
                                                                  (readers-in c)))
                                  (empty? (set/intersection (readers-in a)
                                                                  (readers-in c))))
                       op [:and :or]]
                   [op a b c])]
    {:mandatory (mapv presence ids)
     :candidates (vec (distinct (concat atoms negations pairs compound)))}))

(defn- shuffled [values seed]
  (let [arr (java.util.ArrayList. ^java.util.Collection values)]
    (java.util.Collections/shuffle arr (java.util.Random. (long seed)))
    (vec arr)))

(defn- readers-in [expr]
  (let [[op a] expr]
    (case op
      :literal #{}
      :read #{a}
      :present (readers-in a)
      :not (readers-in a)
      (reduce set/union #{} (map readers-in (rest expr))))))

(defn- source-for [config expression domains]
  (let [fields (readers-in expression)]
    {:rule {:readers fields :expression expression}
     :reader-types (select-keys (:reader-types config) fields)
     :binding-domains (select-keys domains fields)
     :max-cases (:max-cases config)}))

(defn- node-count [expr]
  (inc (reduce + 0 (for [sub (rest expr) :when (plain-vector? sub)
                         :when (keyword? (first sub))]
                     (node-count sub)))))

(defn- shrink-choices [expr]
  (let [[op a] expr
        children (vec (filter (fn [child]
                                (and (plain-vector? child)
                                     (keyword? (first child))))
                              (rest expr)))
        immediate (case op
                    :not [a]
                    (:and :or :implies) children
                    [])
        deletions (if (and (#{:and :or} op) (> (count children) 2))
                    (for [i (range (count children))]
                      (into [op] (concat (take i children)
                                        (drop (inc i) children))))
                    [])]
    (->> (concat [[:literal false] [:literal true]] immediate deletions)
         distinct
         (filter #(and (plain-vector? %) (< (node-count %) (node-count expr))))
         (sort-by (juxt node-count pr-str))
         vec)))

(defn- discrepancy-key [report]
  (when (= :divergence (:status report))
    [(:reason report)
     (get-in report [:counterexample :independent-value])
     (get-in report [:counterexample :production-value])
     (get-in report [:counterexample :production-error])]))

(defn- shrink-divergence [config original-source initial]
  (let [original-bindings (get-in initial [:counterexample :bindings])
        ;; A single case remains: this is a reproducible *local* reduction,
        ;; not a claim that we found the globally smallest divergent program.
        singular-domains (into {} (map (fn [[id _]]
                                         [id [(if (contains? original-bindings id)
                                                (get original-bindings id)
                                                atom/absent)]])
                                       (:reader-types config)))
        signature (discrepancy-key initial)
        probe (fn [expression]
                (let [candidate (source-for config expression singular-domains)
                      result (atom/audit-typed-atom! candidate)]
                  (when (= signature (discrepancy-key result))
                    [candidate result])))]
    (loop [current (or (probe (get-in original-source [:rule :expression]))
                       [original-source initial])
           attempted 0 accepted 0]
      (let [expr (get-in (first current) [:rule :expression])
            choices (shrink-choices expr)
            [found attempts]
            (loop [todo choices, spent 0]
              (if (or (empty? todo)
                      (>= (+ attempted spent) maximum-shrink-audits))
                [nil spent]
                (let [candidate (probe (first todo))
                      cost (inc spent)]
                  (if candidate [candidate cost]
                      (recur (rest todo) cost)))))]
        (if found
          (recur found (+ attempted attempts) (inc accepted))
          {:source (first current)
           :comparison (second current)
           :attempts (+ attempted attempts)
           :accepted accepted
           :local-minimality (if (< (+ attempted attempts)
                                    maximum-shrink-audits)
                               :no-smaller-defined-reduction-preserves-discrepancy
                               :budget-limited)})))))

(defn- base-result [config]
  {:type report-type :version fuzz-version
   :checking :generated-typed-expression-independent-crosscheck
   :scope :generated-expressions-and-supplied-binding-domains-only
   :seed (:seed config)
   :proof-status :not-proved :admission :not-admitted
   :authority :none :transaction-authority :none
   :independent-checker-soundness :not-established
   :atomic-semantics-universality :not-established
   :expression-language-exhaustiveness :not-established
   :jvm-cljs-equivalence :not-established
   :trusted-snapshot-provenance :not-established})

(defn audit-generated-atoms!
  "Generate deterministic typed Boolean formulas, cross-check original rules
   against the independent bounded v1320 oracle, and locally reduce the first
   discrepancy. `:bounded-agreement` is *only* for the selected expression
   corpus and supplied value domains, not mathematical proof.

   Required data keys: :reader-types, :binding-domains, :seed (nonnegative int),
   :max-expressions (1..256), :max-cases (1..8192), and :max-total-cases
   (max-cases..32768). Never accepts user callbacks or prior proof reports."
  [config]
  (try
    (validate! config)
    (let [base (base-result config)
          {:keys [mandatory candidates]} (all-expressions config)
          mandatory-set (set mandatory)
          shuffled-rest (shuffled (vec (remove mandatory-set candidates))
                                  (:seed config))
          pool (vec (concat mandatory shuffled-rest))
          requested (min (count pool) (:max-expressions config))]
      (loop [remaining (take requested pool)
             checked 0 cases 0 total-true 0 total-false 0
             all-finite? true]
        (if-let [expr (first remaining)]
          (let [source (source-for config expr (:binding-domains config))
                required (reduce *' 1 (map count
                                            (vals (:binding-domains source))))]
            (if (> (+ cases required) (:max-total-cases config))
              (assoc base :status :budget-exceeded
                     :reason :aggregate-enumeration-limit
                     :expressions-checked checked :cases-checked cases
                     :expression-corpus-size (count pool))
              (let [result (atom/audit-typed-atom! source)
                    next-checked (inc checked)
                    next-cases (+ cases (or (:cases-checked result) 0))]
                (case (:status result)
                  :bounded-agreement
                  (recur (next remaining) next-checked next-cases
                         (+ total-true (:true-cases result))
                         (+ total-false (:false-cases result))
                         (and all-finite?
                              (= :complete-declared-finite-values
                                 (:domain-coverage result))))
                  :divergence
                  (assoc base :status :divergence
                         :reason (:reason result)
                         :counterexample (:counterexample result)
                         :source source :shrunk (shrink-divergence config source result)
                         :expressions-checked next-checked
                         :cases-checked next-cases
                         :expression-corpus-size (count pool))
                  :budget-exceeded
                  (assoc base :status :budget-exceeded
                         :reason :per-expression-enumeration-limit
                         :oracle-result result :source source
                         :expressions-checked checked
                         :cases-checked cases
                         :expression-corpus-size (count pool))
                  ;; A generated expression is expected to be well typed by
                  ;; construction. Never silently filter an invalid formula.
                  (assoc base :status :generation-error
                         :reason :generated-source-not-evaluable
                         :oracle-result result :source source
                         :expressions-checked checked
                         :cases-checked cases
                         :expression-corpus-size (count pool))))))
          (assoc base :status :bounded-agreement
                 :expressions-checked checked :cases-checked cases
                 :true-cases total-true :false-cases total-false
                 :expression-corpus-size (count pool)
                 :domain-coverage (if all-finite?
                                    :complete-declared-finite-values
                                    :representative-values)
                 :expression-coverage :sampled-from-restricted-grammar))))
    (catch clojure.lang.ExceptionInfo e
      (if (:typed-atom-fuzz/failure (ex-data e))
        (merge (base-result (if (plain-map? config) config {}))
               {:status :invalid-config
                :reason (:reason (ex-data e)) :at (:at (ex-data e))})
        (throw e)))))
