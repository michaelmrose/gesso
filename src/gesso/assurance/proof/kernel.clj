(ns gesso.assurance.proof.kernel
  "Minimal, CLOSED propositional derivation checker for Gesso (v1231).

   Input is SOURCE DATA, never an alleged prior checking report:
     {:subject <v1211 semantic subject>
      :variables {:result/score {:type :integer}}
      :premises {:premise/P <v1229 proposition formula>}
      :steps [{:id :step/one :rule :premise
               :premise :premise/P :conclusion <formula>}
              {:id :step/two :rule :implication-elimination
               :from [:step/one :step/other] :conclusion <formula>}]
      :goal <v1229 proposition formula>}

   Every proposition is independently re-parsed and typed by v1229, using
   the same EXACT subject and a subset of one declared nominal variable
   context. The checker never accepts cached normalized forms, status flags,
   reports, descriptions, evaluator observations, or purported certificates.
   Referenced steps must precede their use. All declared premises and steps
   must participate in the final checked derivation; those premises remain
   OPEN rather than silently discharged.

   Rules (the ONLY initial inference rules):
     :premise                   P in Gamma entails Gamma |- P
     :truth-introduction       Gamma |- true
     :implication-reflexivity  Gamma |- (P => P)
     :conjunction-introduction P1,...,Pn entails (and P1 ... Pn), n >= 2
     :conjunction-elimination  (and P1 ... Pn) entails Pi (zero-based index)
     :implication-elimination  (P => Q), P entails Q (in that order)
     :disjunction-introduction P entails (or ... P ...), indexed child

   This checker deliberately has NO atomic truth axioms, numerical/SMT
   reasoning, hypothetical-context discharge, rewriting, classical excluded
   middle, arbitrary function execution, transaction authority, or certificate
   issuance. A conditional proof is NOT an unconditional theorem. A checked
   report is COPYABLE DATA, and later certificate admission must independently
   rerun checking from this source input. No claim about the meta-soundness of
   this implementation or JVM/CLJS/XTDB correspondence is made."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.subject :as subject]))

(def kernel-version 1)
(def report-type :gesso.assurance.proof.kernel/unadmitted-checked-derivation)

(def ^:private root-keys #{:subject :variables :premises :steps :goal})
(def ^:private max-premises 128)
(def ^:private max-steps 256)
(def ^:private max-formula-tree-nodes 65536)
(def ^:private max-proof-references 128)
(def ^:private max-variables 256)
(def ^:private identity-id :gesso.assurance.proof/kernel-derivation-v1)

(def ^:private rule-keys
  {:premise                  #{:id :rule :premise :conclusion}
   :truth-introduction      #{:id :rule :conclusion}
   :implication-reflexivity #{:id :rule :conclusion}
   :conjunction-introduction #{:id :rule :from :conclusion}
   :conjunction-elimination #{:id :rule :from :index :conclusion}
   :implication-elimination #{:id :rule :from :conclusion}
   :disjunction-introduction #{:id :rule :from :index :conclusion}})

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso assurance propositional derivation."
                  (merge {:error/type ::invalid-derivation
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- plain-vector? [v]
  (instance? clojure.lang.APersistentVector v))

(defn- qualified-keyword? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- metadata-free! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (fail! :metadata-not-allowed at {})))

(defn- exact-keys! [m expected at]
  (when-not (plain-map? m)
    (fail! :expected-plain-map at {}))
  (metadata-free! m at)
  (let [actual (set (keys m))]
    (when-not (= actual expected)
      (fail! :wrong-keys at
             {:missing (set/difference expected actual)
              :unexpected (set/difference actual expected)}))))

(defn- named! [v at]
  (when-not (qualified-keyword? v)
    (fail! :invalid-id at {:value v})))

(defn- formula-readers!
  "Extract syntactic reader footprints, with an independent traversal cap.
   v1229 then checks all syntax, arity, types, and the EXACT footprint. This
   traversal does not confer validity on any of the encountered expressions."
  [formula at]
  (loop [todo [formula] used #{} nodes 0]
    (if (empty? todo)
      {:readers used :nodes nodes}
      (let [current (peek todo)
            todo (pop todo)
            nodes (inc nodes)]
        (when (> nodes max-formula-tree-nodes)
          (fail! :total-formula-limit at {:maximum max-formula-tree-nodes}))
        (if-not (plain-vector? current)
          (recur todo used nodes)
          (if (= :atom (first current))
            (let [decl (second current)
                  rs (when (plain-map? decl) (:readers decl))]
              (recur todo (if (set? rs) (set/union used rs) used) nodes))
            (recur (into todo (reverse (rest current))) used nodes)))))))

(defn- normalized-formula!
  [subject types formula at budget]
  (let [{:keys [readers nodes]} (formula-readers! formula at)
        _ (when (> (swap! budget + nodes) max-formula-tree-nodes)
            (fail! :total-formula-limit at {:maximum max-formula-tree-nodes}))
        missing (set/difference readers (set (keys types)))]
    (when (seq missing)
      (fail! :undeclared-variable at {:missing missing}))
    (let [desc (proposition/describe-proposition!
                {:subject subject
                 :variables (select-keys types readers)
                 :proposition formula})]
      (get-in desc [:semantics :proposition]))))

(defn- require-sources!
  [step previous count-min count-max]
  (let [ids (:from step)]
    (when-not (and (plain-vector? ids)
                   (<= count-min (count ids) count-max))
      (fail! :invalid-sources [:steps (:id step) :from]
             {:minimum count-min :maximum count-max}))
    (metadata-free! ids [:steps (:id step) :from])
    (doseq [id ids]
      (named! id [:steps (:id step) :from])
      (when-not (contains? previous id)
        (fail! :unknown-or-forward-step [:steps (:id step) :from]
               {:referenced-step id})))
    (mapv previous ids)))

(defn- zero-based-index!
  [step children]
  (let [index (:index step)]
    (when-not (and (integer? index)
                   (<= 0 index)
                   (< index (count children)))
      (fail! :invalid-index [:steps (:id step) :index]
             {:index index :children (count children)}))
    (int index)))

(defn- prove-step!
  [step previous premises]
  (let [id (:id step)
        rule (:rule step)
        result (:normalized-conclusion step)
        at [:steps id]]
    (case rule
      :premise
      (let [premise-id (:premise step)]
        (named! premise-id (conj at :premise))
        (when-not (contains? premises premise-id)
          (fail! :unknown-premise (conj at :premise)
                 {:premise premise-id}))
        (when-not (= result (get premises premise-id))
          (fail! :wrong-premise-conclusion (conj at :conclusion) {}))
        {:premises #{premise-id} :from []})

      :truth-introduction
      (do (when-not (= result [:true])
            (fail! :invalid-truth-introduction (conj at :conclusion) {}))
          {:premises #{} :from []})

      :implication-reflexivity
      (do (when-not (and (= :implies (first result))
                         (= 3 (count result))
                         (= (nth result 1) (nth result 2)))
            (fail! :invalid-implication-reflexivity (conj at :conclusion) {}))
          {:premises #{} :from []})

      :conjunction-introduction
      (let [sources (require-sources! step previous 2 max-proof-references)]
        (when-not (= result (into [:and] (map :formula sources)))
          (fail! :invalid-conjunction-introduction (conj at :conclusion) {}))
        {:premises (apply set/union #{} (map :premises sources))
         :from (:from step)})

      :conjunction-elimination
      (let [[source] (require-sources! step previous 1 1)
            conjunction (:formula source)]
        (when-not (= :and (first conjunction))
          (fail! :expected-conjunction (conj at :from) {}))
        (let [children (subvec conjunction 1)
              index (zero-based-index! step children)]
          (when-not (= result (nth children index))
            (fail! :invalid-conjunction-elimination (conj at :conclusion) {})))
        {:premises (:premises source) :from (:from step)})

      :implication-elimination
      (let [[implication antecedent] (require-sources! step previous 2 2)
            implication-formula (:formula implication)]
        (when-not (and (= :implies (first implication-formula))
                       (= (:formula antecedent) (nth implication-formula 1))
                       (= result (nth implication-formula 2)))
          (fail! :invalid-implication-elimination (conj at :conclusion) {}))
        {:premises (set/union (:premises implication) (:premises antecedent))
         :from (:from step)})

      :disjunction-introduction
      (let [[source] (require-sources! step previous 1 1)]
        (when-not (= :or (first result))
          (fail! :expected-disjunction (conj at :conclusion) {}))
        (let [children (subvec result 1)
              index (zero-based-index! step children)]
          (when-not (= (:formula source) (nth children index))
            (fail! :invalid-disjunction-introduction (conj at :conclusion) {})))
        {:premises (:premises source) :from (:from step)})

      (fail! :unknown-rule (conj at :rule) {:rule rule}))))

(defn- reachable-step-ids
  [proved last-id]
  (loop [pending [last-id] reached #{}]
    (if (empty? pending)
      reached
      (let [id (peek pending)
            pending (pop pending)]
        (if (contains? reached id)
          (recur pending reached)
          (recur (into pending (:from (get proved id)))
                 (conj reached id)))))))

(defn check-derivation!
  "Independently check a finite propositional proof from raw SOURCE DATA.

   Result is copyable descriptive evidence of checking, NOT a proof
   certificate, discharge of assumptions, runtime admission handle, or
   authorization to commit. With used premises the judgment is conditional;
   no premise may be hidden or treated as proved. A later certificate issuer
   must rerun this function and close its own evidence obligations."
  [source]
  (exact-keys! source root-keys [])
  (let [{:keys [subject variables premises steps goal]} source]
    (when-not (plain-map? variables)
      (fail! :invalid-variables [:variables] {}))
    (metadata-free! variables [:variables])
    (when (> (count variables) max-variables)
      (fail! :variable-limit [:variables] {:maximum max-variables}))
    (doseq [id (keys variables)]
      (named! id [:variables id]))
    (when-not (plain-map? premises)
      (fail! :invalid-premises [:premises] {}))
    (metadata-free! premises [:premises])
    (when (> (count premises) max-premises)
      (fail! :premise-limit [:premises] {:maximum max-premises}))
    (doseq [id (keys premises)]
      (named! id [:premises id]))
    (when-not (and (plain-vector? steps)
                   (pos? (count steps))
                   (<= (count steps) max-steps))
      (fail! :invalid-steps [:steps] {:maximum max-steps}))
    (metadata-free! steps [:steps])
    ;; Recalculate the semantic subject identity from ORIGINAL source data.
    ;; It cannot be supplied as a cached fingerprint or report.
    (let [subject-id (subject/describe-subject subject)
          budget (atom 0)
          goal* (normalized-formula! subject variables goal [:goal] budget)
          premises* (into {}
                          (map (fn [[id p]]
                                 [id (normalized-formula!
                                      subject variables p [:premises id] budget)]))
                          premises)
          used-variable-ids (atom (set/union
                                   (:readers (formula-readers! goal [:goal]))
                                   (reduce set/union #{}
                                           (map (fn [[id p]]
                                                  (:readers (formula-readers!
                                                             p [:premises id])))
                                                premises))))
          checked (reduce
                   (fn [prior [index step]]
                     (let [at [:steps index]]
                       (when-not (plain-map? step)
                         (fail! :invalid-step at {}))
                       (metadata-free! step at)
                       (let [rule (:rule step)
                             expected (get rule-keys rule)]
                         (when-not expected
                           (fail! :unknown-rule (conj at :rule) {:rule rule}))
                         (exact-keys! step expected at)
                         (named! (:id step) (conj at :id))
                         (when (contains? prior (:id step))
                           (fail! :duplicate-step-id (conj at :id) {}))
                         (let [formula (:conclusion step)
                               _ (swap! used-variable-ids set/union
                                        (:readers (formula-readers! formula
                                                                    (conj at :conclusion))))
                               normalized (normalized-formula!
                                           subject variables formula
                                           (conj at :conclusion) budget)
                               record (prove-step! (assoc step
                                                         :normalized-conclusion normalized)
                                                   prior premises*)]
                           (assoc prior (:id step)
                                  (assoc record :formula normalized
                                         :rule rule :index (:index step)))))))
                   {}
                   (map-indexed vector steps))
          last-id (:id (peek steps))
          final (get checked last-id)
          reachable (reachable-step-ids checked last-id)
          unused-steps (set/difference (set (keys checked)) reachable)
          used-premises (:premises final)
          unused-premises (set/difference (set (keys premises)) used-premises)
          unused-variables (set/difference (set (keys variables))
                                           @used-variable-ids)]
      (when-not (= goal* (:formula final))
        (fail! :wrong-final-conclusion [:goal] {}))
      (when (seq unused-steps)
        (fail! :unused-proof-steps [:steps] {:unused unused-steps}))
      (when (seq unused-premises)
        (fail! :unused-premises [:premises] {:unused unused-premises}))
      (when (seq unused-variables)
        (fail! :unused-variables [:variables] {:unused unused-variables}))
      ;; A deterministic description identifies the exact checked derivation.
      ;; It is NOT an admission capability. Replaying this data requires a
      ;; fresh check, and the separate future certificate authority must bind
      ;; its own trusted result to the subject, premises and checker version.
      (let [semantics {:kernel-version kernel-version
                       :proposition-version proposition/proposition-version
                       :subject-fingerprint (:fingerprint subject-id)
                       :variables variables
                       :goal goal*
                       :premises premises*
                       :steps (mapv
                               (fn [{:keys [id rule from premise index]}]
                                 (cond-> {:id id :rule rule
                                          :conclusion (:formula (get checked id))}
                                   (some? from) (assoc :from from)
                                   (some? premise) (assoc :premise premise)
                                   (some? index) (assoc :index index)))
                               steps)}
            identity (subject/describe-subject
                      {:kind :rule :id identity-id :interface semantics})]
        {:type report-type
         :version kernel-version
         :fingerprint (:fingerprint identity)
         :subject-fingerprint (:fingerprint subject-id)
         :goal-fingerprint (:fingerprint (proposition/describe-proposition!
                                         {:subject subject
                                          :variables (select-keys variables
                                                                  (:readers (formula-readers!
                                                                             goal [:goal])))
                                          :proposition goal}))
         :rule-count (count steps)
         :open-premises used-premises
         :checking :propositional-derivation-checked
         :conclusion-status (if (empty? used-premises)
                              :checked-without-premises
                              :conditional-on-open-premises)
         :assumption-discharge :not-performed
         :certificate :not-issued
         :admission :not-admitted
         :runtime-enforcement :not-established
         :authority :none}))))
