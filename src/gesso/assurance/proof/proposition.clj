(ns gesso.assurance.proof.proposition
  "Closed, subject-bound, typed proposition declarations for Gesso proof work (v1229).

   Source declaration (EXACT keys):
     {:subject {:kind :operation :id :example/advance
                :interface {:operation :example/advance}}
      :variables {:result/score {:type :integer}}
      :proposition
      [:implies
       [:atom {:readers #{:result/score}
               :expression [:> [:read :result/score] [:literal 7]]}]
       [:atom {:readers #{:result/score}
               :expression [:>= [:read :result/score] [:literal 7]]}]]}

   Connectives are [:true], [:false], [:atom v1219-rule-declaration],
   [:not proposition], [:and proposition proposition ...],
   [:or proposition proposition ...], [:implies antecedent consequent].
   Each atom is a *typed Boolean formula* of the existing closed v1219/1221
   language, not an arbitrary callable predicate. Variables are explicitly
   declared using v1221 nominal type descriptors; their union must equal the
   exact readers used by all atoms. An atom must be independently well typed:
   presence/non-nil facts from one atom DO NOT refine another atom.

   Propositional connectives are logical, not v1223 control-flow evaluation;
   atoms retain their own interpreter's ordering and guard semantics. No
   quantifiers, arbitrary Clojure terms, unbound variables, arithmetic axioms,
   semantic rewrites, or proof flags enter this syntax. The normalized identity
   is conservative structural identity, not logical-equivalence checking.

   This namespace constructs UNADMITTED DESCRIPTIONS. It neither checks a
   derivation, evaluates a proposition, discharges assumptions, issues proof
   authority, nor establishes a JVM/CLJS/XTDB correspondence. Subsequent
   proof-kernel revisions must independently recheck source declarations.
   Both proposition-tree nodes and the combined nested atomic-rule nodes are
   bounded to avoid unexpectedly large generated proof subjects."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-normalize :as rule-normalize]
            [gesso.assurance.subject :as subject]))

(def proposition-version 1)
(def description-type :gesso.assurance.proof.proposition/unadmitted-proposition)

(def ^:private source-keys #{:subject :variables :proposition})
(def ^:private max-variables 256)
(def ^:private max-depth 64)
(def ^:private max-nodes 4096)
(def ^:private max-atoms 512)
(def ^:private max-atomic-rule-nodes 65536)
(def ^:private max-arity 128)
(def ^:private identity-id :gesso.assurance.proof/proposition-v1)

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso assurance proof proposition."
                  (merge {:error/type ::invalid-proposition
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map?
  [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-vector?
  [x]
  (instance? clojure.lang.APersistentVector x))

(defn- qualified-keyword?
  [x]
  (and (keyword? x) (some? (namespace x))))

(defn- no-metadata!
  [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (fail! :metadata-not-allowed at {})))

(defn- validate-variables!
  [variables]
  (when-not (plain-map? variables)
    (fail! :invalid-variable-map [:variables] {}))
  (no-metadata! variables [:variables])
  (when (> (count variables) max-variables)
    (fail! :variable-limit [:variables] {:maximum max-variables}))
  ;; v1221 is the source of truth for nominal descriptors.  The dummy
  ;; :present expression is type-safe even for optional/nullable variables,
  ;; and forces the existing checker to inspect every descriptor independently.
  (into {}
        (map (fn [[id descriptor]]
               (when-not (qualified-keyword? id)
                 (fail! :invalid-variable-id [:variables id] {}))
               (let [probe {:readers #{id}
                            :expression [:present [:read id]]}
                     checked (rule-normalize/normalize-rule! probe {id descriptor})]
                 [id (get-in checked [:semantics :reader-types id])])))
        variables))

(defn- spend!
  [budget key maximum at]
  (let [n (swap! budget update key (fnil inc 0))]
    (when (> (get n key) maximum)
      (fail! (case key :nodes :node-limit :atoms :atom-limit)
             at {:maximum maximum}))))

(defn- reserve-rule-nodes!
  [budget nodes at]
  (let [total (:atomic-rule-nodes
               (swap! budget update :atomic-rule-nodes + nodes))]
    (when (> total max-atomic-rule-nodes)
      (fail! :atomic-rule-node-limit at
             {:maximum max-atomic-rule-nodes}))))

(declare check-formula!)

(defn- check-children!
  [items at depth types budget used]
  (mapv (fn [index]
          (check-formula! (nth items index) (conj at index)
                          (inc depth) types budget used))
        (range 1 (count items))))

(defn- check-formula!
  [form at depth types budget used]
  (when (> depth max-depth)
    (fail! :depth-limit at {:maximum max-depth}))
  (spend! budget :nodes max-nodes at)
  (when-not (plain-vector? form)
    (fail! :expected-proposition-vector at {}))
  (no-metadata! form at)
  (when (or (empty? form) (> (count form) max-arity))
    (fail! :invalid-proposition-arity at {:maximum max-arity}))
  (let [op (first form)
        arity (dec (count form))]
    (case op
      :true (do
              (when-not (zero? arity)
                (fail! :invalid-arity at {:operator op :expected 0 :actual arity}))
              [:true])
      :false (do
               (when-not (zero? arity)
                 (fail! :invalid-arity at {:operator op :expected 0 :actual arity}))
               [:false])
      :atom (do
              (when-not (= 1 arity)
                (fail! :invalid-arity at {:operator op :expected 1 :actual arity}))
              (spend! budget :atoms max-atoms at)
              (let [declaration (second form)
                    syntax (rule/check-rule! declaration)
                    readers (:readers syntax)
                    _ (reserve-rule-nodes! budget (:nodes syntax) at)
                    unbound (set/difference readers (set (keys types)))]
                (when (seq unbound)
                  (fail! :unbound-variable at {:unbound unbound}))
                (swap! used set/union readers)
                (let [normalized (rule-normalize/normalize-rule!
                                  declaration (select-keys types readers))]
                  [:atom (:semantics normalized)])))
      :not (do
             (when-not (= 1 arity)
               (fail! :invalid-arity at {:operator op :expected 1 :actual arity}))
             (into [:not] (check-children! form at depth types budget used)))
      :and (do
             (when (< arity 2)
               (fail! :invalid-arity at {:operator op :minimum 2 :actual arity}))
             (into [:and] (check-children! form at depth types budget used)))
      :or (do
            (when (< arity 2)
              (fail! :invalid-arity at {:operator op :minimum 2 :actual arity}))
            (into [:or] (check-children! form at depth types budget used)))
      :implies (do
                 (when-not (= 2 arity)
                   (fail! :invalid-arity at {:operator op :expected 2 :actual arity}))
                 (into [:implies] (check-children! form at depth types budget used)))
      (fail! :unknown-connective at {:operator op}))))

(defn describe-proposition!
  "Recheck an EXACT source declaration and return a *copyable, unadmitted*
   description of a typed, subject-bound proposition. No reports, checked
   fingerprints, proof results or discharge flags may be supplied as input.

   :variables must exactly equal the union of readers across all atomic rules.
   The returned identity is structural within declared normalization rules;
   it is NOT an independently checked theorem or a kernel admission handle."
  [declaration]
  (when-not (plain-map? declaration)
    (fail! :expected-declaration-map [] {}))
  (no-metadata! declaration [])
  (let [actual (set (keys declaration))]
    (when-not (= source-keys actual)
      (fail! :wrong-declaration-keys []
             {:missing (set/difference source-keys actual)
              :unexpected (set/difference actual source-keys)})))
  (let [subject-identity (subject/describe-subject (:subject declaration))
        types (validate-variables! (:variables declaration))
        budget (atom {:nodes 0 :atoms 0 :atomic-rule-nodes 0})
        used (atom #{})
        normalized (check-formula! (:proposition declaration)
                                   [:proposition] 0 types budget used)
        declared (set (keys types))]
    (when-not (= declared @used)
      (fail! :unused-variable [:variables]
             {:unused (set/difference declared @used)}))
    (let [semantics {:version proposition-version
                     :subject-fingerprint (:fingerprint subject-identity)
                     :subject-kind (get-in subject-identity [:subject :kind])
                     :subject-id (get-in subject-identity [:subject :id])
                     :variable-types types
                     :proposition normalized}
          identity (subject/describe-subject
                    {:kind :rule
                     :id identity-id
                     :interface semantics})]
      {:type description-type
       :version proposition-version
       :fingerprint (:fingerprint identity)
       :subject-fingerprint (:fingerprint subject-identity)
       :semantics semantics
       :variables declared
       :nodes (:nodes @budget)
       :atoms (:atoms @budget)
       :atomic-rule-nodes (:atomic-rule-nodes @budget)
       :checking :syntax-and-types
       :evaluation :not-performed
       :proof-status :not-proved
       :assumption-discharge :not-performed
       :admission :not-admitted
       :runtime-enforcement :not-established
       :authority :none})))
