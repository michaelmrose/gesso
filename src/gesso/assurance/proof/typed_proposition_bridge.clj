(ns gesso.assurance.proof.typed-proposition-bridge
  "Source-relative finite *semantic bridge* between v1229 propositions and
   the independently cross-checked v1320 typed atomic rules (v1329).

   (audit-typed-proposition!
     {:proposition-source {:subject <original subject>
                           :variables {:result/score {:type :integer}}
                           :proposition [:implies
                                         [:atom {:readers #{:result/score}
                                                 :expression [:> [:read :result/score]
                                                                [:literal 7]]}]
                                         [:atom {:readers #{:result/score}
                                                 :expression [:>= [:read :result/score]
                                                                [:literal 7]]}]]}
      :binding-domains {:result/score [6 7 8]}
      :max-cases 128})

   Every distinct atom is audited over its entire projected finite domain by
   the independent typed-atom oracle *before* evaluating the propositional
   connectives. All concrete valuations are derived from original bindings;
   Boolean combinations which cannot arise in this universe are not fabricated.

   Results are COPYABLE DIAGNOSTICS. Agreement for all enumerated bindings
   establishes neither universal atomic soundness nor an unconditional theorem.
   In particular, the bridge does not admit kernel derivations, discharge
   premises, authenticate bindings, or issue certificates or transaction rights.
   The production evaluator is assumed stable between per-atom audits and
   concrete connective evaluation; this is not a theorem of the bridge.
   No assumption of independent checker soundness is made."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.proof.typed-atom-oracle :as atom-oracle]
            [gesso.assurance.rule-eval :as production]))

(def bridge-version 1)
(def report-type :gesso.assurance.proof.typed-proposition-bridge/bounded-diagnostic)

(def ^:private source-keys #{:proposition-source :binding-domains :max-cases})
(def ^:private max-cases-cap 8192)
(def ^:private max-fields 12)
(def ^:private max-atom-occurrences 128)
(def ^:private max-unique-atoms 32)
(def ^:private max-domain-size 24)
(def ^:private max-depth 64)
(def ^:private max-tree-nodes 4096)

;; Capture real implementations rather than allowing a redefinition of a
;; public checker to mint a convincing positive diagnostic. This is not an
;; independent trust root or a defense against classpath compromise.
(def ^:private checked-proposition proposition/describe-proposition!)
(def ^:private checked-atom-audit atom-oracle/audit-typed-atom!)
(def ^:private checked-atom-eval production/evaluate-rule!)

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentHashMap x)
      (instance? clojure.lang.PersistentArrayMap x)))
(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))
(defn- metadata-free? [x]
  (not (and (instance? clojure.lang.IObj x) (some? (meta x)))))

(defn- reject!
  [reason at details]
  (throw (ex-info "Cannot construct typed-proposition diagnostic."
                  (merge {:typed-proposition-bridge/rejection true
                          :reason reason :at at} details))))

(defn- exact-map!
  [x expected at]
  (when-not (and (plain-map? x) (metadata-free? x))
    (reject! :expected-unannotated-map at {}))
  (when-not (= expected (set (keys x)))
    (reject! :wrong-keys at
             {:missing (set/difference expected (set (keys x)))
              :unexpected (set/difference (set (keys x)) expected)}))
  x)

(defn- source-atoms!
  "Traverse only already validated proposition shapes. Preserve first-seen
   order and occurrences, counting nodes even when formulas share an atom."
  [formula]
  (loop [pending [[formula [:proposition] 0]]
         atoms [] nodes 0]
    (if (empty? pending)
      atoms
      (let [[form path depth] (peek pending)
            pending (pop pending)
            nodes (inc nodes)]
        (when (or (> depth max-depth) (> nodes max-tree-nodes))
          (reject! :formula-budget path {}))
        (if (= :atom (first form))
          (let [atoms (conj atoms {:rule (second form) :at path})]
            (when (> (count atoms) max-atom-occurrences)
              (reject! :atom-occurrence-budget path {}))
            (recur pending atoms nodes))
          (recur (into pending
                       (map (fn [i]
                              [(nth form i) (conj path i) (inc depth)])
                            (reverse (range 1 (count form)))))
                 atoms nodes))))))

(defn- domains!
  [types domains]
  (exact-map! domains (set (keys types)) [:binding-domains])
  (when (> (count types) max-fields)
    (reject! :reader-budget [:binding-domains] {}))
  (doseq [[id entries] domains]
    (when-not (and (plain-vector? entries)
                   (metadata-free? entries)
                   (<= 1 (count entries) max-domain-size))
      (reject! :invalid-domain [:binding-domains id] {}))
    (when-not (= (count entries) (count (distinct entries)))
      (reject! :duplicate-domain-entry [:binding-domains id] {}))))

(defn- complete-domain?
  [types domains]
  (every?
   (fn [[id descriptor]]
     (let [values (case (:type descriptor)
                    :boolean #{true false}
                    :enum (:values descriptor)
                    nil)]
       (and values
            (= (set (domains id))
               (cond-> values
                 (true? (:nullable? descriptor)) (conj nil)
                 (true? (:optional? descriptor)) (conj atom-oracle/absent))))))
   types))

(defn- bindings-for
  [fields domains]
  (reduce (fn [rows id]
            (vec (for [row rows, v (domains id)]
                   (if (= v atom-oracle/absent)
                     row
                     (assoc row id v)))))
          [{}] fields))

(defn- value!
  "The connective evaluator is separate from v1231's kernel. Atomic truth
   values come from the production evaluator only after independent exhaustive
   cross-checking over every projected binding in this concrete universe."
  [form values]
  (case (first form)
    :true true
    :false false
    :atom (get values (second form))
    :not (not (value! (second form) values))
    :and (loop [i 1]
           (if (= i (count form)) true
               (if (true? (value! (nth form i) values))
                 (recur (inc i)) false)))
    :or (loop [i 1]
          (if (= i (count form)) false
              (if (true? (value! (nth form i) values))
                true (recur (inc i)))))
    :implies (or (not (value! (second form) values))
                 (value! (nth form 2) values))))

(defn- base-report
  [fingerprint]
  {:type report-type
   :version bridge-version
   :proposition-fingerprint fingerprint
   :checking :original-typed-proposition-and-per-atom-finite-cross-check
   :scope :supplied-concrete-bindings-only
   :atomic-semantics-universality :not-established
   :propositional-kernel-soundness :not-established
   :independent-checker-soundness :not-established
   :trusted-binding-provenance :not-established
   :jvm-cljs-equivalence :not-established
   :premise-discharge :not-performed
   :kernel-admission :not-performed
   :production-certification :not-established
   :transaction-authority :none
   :authority :none})

(defn audit-typed-proposition!
  "Audit original v1229 proposition source against supplied bounded domains.

   :bounded-no-counterexample means the proposition evaluated true for every
   concrete binding in the supplied universe, with all atoms independently
   cross-checked against the production evaluator over their projections.
   :counterexample includes the first false concrete binding and its *realizable*
   atom valuation. An atom divergence, bad source, or exhausted budget never
   becomes a successful report. The result is not proof/admission authority."
  [source]
  (try
    (exact-map! source source-keys [])
    (let [{:keys [proposition-source binding-domains max-cases]} source
          _ (when-not (and (integer? max-cases)
                           (<= 1 max-cases max-cases-cap))
              (reject! :invalid-budget [:max-cases] {}))
          _ (when-not (and (identical? checked-proposition proposition/describe-proposition!)
                           (identical? checked-atom-audit atom-oracle/audit-typed-atom!)
                           (identical? checked-atom-eval production/evaluate-rule!))
              (reject! :substituted-checker [] {}))
          description (try
                        (checked-proposition proposition-source)
                        (catch clojure.lang.ExceptionInfo e
                          (reject! :invalid-original-proposition
                                   [:proposition-source]
                                   {:source-reason (:reason (ex-data e))})))
          types (:variables proposition-source)
          _ (domains! types binding-domains)
          fields (vec (sort-by str (keys types)))
          required (reduce *' 1 (map #(count (binding-domains %)) fields))
          base (base-report (:fingerprint description))]
      (if (> required max-cases)
        (assoc base :status :budget-exceeded :reason :enumeration-limit
               :cases-planned required :cases-checked 0)
        (let [occurrences (source-atoms! (:proposition proposition-source))
              unique-rules (vec (distinct (map :rule occurrences)))
              _ (when (> (count unique-rules) max-unique-atoms)
                  (reject! :unique-atom-budget [:proposition] {}))
              atom-results
              (mapv
               (fn [rule]
                 (let [readers (:readers rule)
                       result (checked-atom-audit
                               {:rule rule
                                :reader-types (select-keys types readers)
                                :binding-domains (select-keys binding-domains readers)
                                :max-cases max-cases})]
                   {:rule rule :audit result}))
               unique-rules)
              failure (first (filter #(not= :bounded-agreement
                                             (get-in % [:audit :status]))
                                     atom-results))
              common (assoc base
                            :domain-coverage
                            (if (complete-domain? types binding-domains)
                              :complete-declared-finite-values
                              :representative-values)
                            :reader-ids (set fields)
                            :atom-occurrences (count occurrences)
                            :unique-atoms (count unique-rules)
                            :cases-planned required)]
          (if failure
            (assoc common
                   :status :atomic-cross-check-failed
                   :reason (get-in failure [:audit :status])
                   :atom (:rule failure)
                   :atom-reason (get-in failure [:audit :reason])
                   :atom-counterexample (get-in failure [:audit :counterexample])
                   :cases-checked 0)
            (loop [rows (seq (bindings-for fields binding-domains))
                   checked 0 true-count 0 realized #{}]
              (if-let [bindings (first rows)]
                (let [values
                      (into {}
                            (for [rule unique-rules
                                  :let [projected (select-keys bindings (:readers rule))
                                        result (checked-atom-eval
                                                rule (select-keys types (:readers rule))
                                                projected)]]
                              [rule (:value result)]))
                      valuation (mapv values unique-rules)
                      truth (value! (:proposition proposition-source) values)
                      realized (conj realized valuation)]
                  (if truth
                    (recur (next rows) (inc checked) (inc true-count) realized)
                    (assoc common
                           :status :counterexample
                           :cases-checked (inc checked)
                           :true-cases true-count
                           :realized-atom-valuations (count realized)
                           :counterexample {:bindings bindings
                                            :atom-values (mapv (fn [rule]
                                                                 {:rule rule
                                                                  :value (values rule)})
                                                               unique-rules)
                                            :proposition-value false})))
                (assoc common
                       :status :bounded-no-counterexample
                       :cases-checked checked
                       :true-cases true-count
                       :realized-atom-valuations (count realized))))))))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (if (:typed-proposition-bridge/rejection data)
          (assoc (base-report nil) :status :invalid-source
                 :reason (:reason data) :at (:at data))
          (if (= (:error/type data)
                 :gesso.assurance.proof.proposition/invalid-proposition)
            (assoc (base-report nil) :status :invalid-source
                   :reason :invalid-original-proposition
                   :source-reason (:reason data) :at (:at data))
            (throw e)))))))
