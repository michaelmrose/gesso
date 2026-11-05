(ns gesso.assurance.proof.propositional-rule-obligations
  "Seven SOURCE-LEVEL propositional soundness obligations (v1332).

   This module specifies what must hold for each rule admitted by the
   Gesso proof kernel. It does NOT check the kernel, establish a theorem,
   interpret typed atoms, discharge open premises, or issue certificates.

   For a rule instance with cited source formulas H1 ... Hn and conclusion C,
   the local obligation is: for EVERY total opaque-Boolean valuation v,
     (H1(v) AND ... AND Hn(v)) IMPLIES C(v).
   With no cited sources the conclusion must be true for every valuation.

   A :premise step cites its selected member of the global Gamma; this
   selected premise remains OPEN. Other rules inherit the union of the open
   premises of their cited steps. Local Boolean entailment does not discharge
   any of these assumptions. It also does not establish that actual kernel
   code implements the stated structural rules; that is a separate S2 task.

   All formulas are independently shape-checked by the v1331 reference
   semantics, which treats every entire typed atomic declaration as one
   opaque Boolean variable. We do not call the production normalizer,
   proof kernel, or other soundness auditors."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.propositional-semantics :as semantics]))

(def obligations-version 1)
(def obligation-type
  :gesso.assurance.proof.propositional-rule-obligations/unchecked-rule-obligation)

(def ^:private max-connective-children 127)

;; A CLOSED inventory of rule schemas. These declarations are the independent
;; specification, not a computed list copied from the implementation. Their
;; exact-match comparison against the production rule inventory is a later
;; implementation-correspondence obligation.
(def rule-specifications
  {:premise
   {:inputs #{:rule :premise-id :formula}
    :source-count 1
    :dependency-policy :selected-open-premise
    :side-condition :selected-premise-equals-conclusion}

   :truth-introduction
   {:inputs #{:rule}
    :source-count 0
    :dependency-policy :no-new-premises
    :side-condition :conclusion-is-true}

   :implication-reflexivity
   {:inputs #{:rule :formula}
    :source-count 0
    :dependency-policy :no-new-premises
    :side-condition :antecedent-equals-consequent}

   :conjunction-introduction
   {:inputs #{:rule :sources}
    :source-count :two-to-127
    :dependency-policy :union-of-source-premises
    :side-condition :conclusion-conjoins-ordered-sources}

   :conjunction-elimination
   {:inputs #{:rule :conjunction :index}
    :source-count 1
    :dependency-policy :union-of-source-premises
    :side-condition :conclusion-is-indexed-conjunct}

   :implication-elimination
   {:inputs #{:rule :implication :antecedent}
    :source-count 2
    :dependency-policy :union-of-source-premises
    :side-condition :second-source-equals-implication-antecedent}

   :disjunction-introduction
   {:inputs #{:rule :source :disjunction :index}
    :source-count 1
    :dependency-policy :union-of-source-premises
    :side-condition :source-equals-indexed-disjunct}})

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso propositional rule-soundness obligation."
                  (merge {:error/type ::invalid-obligation
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- no-meta! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (fail! :metadata-not-allowed at {})))

(defn- validate-keys!
  [source]
  (when-not (plain-map? source)
    (fail! :expected-source-map [] {}))
  (no-meta! source [])
  (let [rule (:rule source)
        specification (get rule-specifications rule)]
    (when-not specification
      (fail! :unknown-rule [:rule] {:rule rule}))
    (let [expected (:inputs specification)
          actual (set (keys source))]
      (when-not (= expected actual)
        (fail! :wrong-source-keys []
               {:missing (set/difference expected actual)
                :unexpected (set/difference actual expected)})))
    specification))

(defn- formula!
  [form at]
  (try
    (semantics/atoms! form)
    (catch clojure.lang.ExceptionInfo e
      (fail! :invalid-formula at
             {:cause (select-keys (ex-data e) [:reason :at])})))
  form)

(defn- formula-sequence!
  [values at]
  (when-not (and (plain-vector? values)
                 (<= 2 (count values) max-connective-children))
    (fail! :invalid-formula-count at
           {:minimum 2 :maximum max-connective-children}))
  (no-meta! values at)
  (doseq [[i form] (map-indexed vector values)]
    (formula! form (conj at i)))
  values)

(defn- indexed!
  [index count-children at]
  (when-not (and (integer? index) (<= 0 index)
                 (< index count-children))
    (fail! :invalid-index at
           {:index index :children count-children}))
  index)

(defn- connective!
  [form operator at]
  (formula! form at)
  (when-not (= operator (first form))
    (fail! :expected-connective at
           {:expected operator :actual (first form)}))
  (subvec form 1))

(defn- local-arguments!
  "Return [ordered local hypotheses, conclusion]. Do not accept caller-supplied
   proofs, conclusions, premise propagation, or source-step references."
  [{:keys [rule formula premise-id sources conjunction index
           implication antecedent source disjunction]}]
  (case rule
    :premise
    (do
      (when-not (and (keyword? premise-id) (namespace premise-id))
        (fail! :invalid-premise-id [:premise-id] {}))
      (let [p (formula! formula [:formula])]
        [[p] p]))

    :truth-introduction
    [[] [:true]]

    :implication-reflexivity
    (let [p (formula! formula [:formula])]
      [[] [:implies p p]])

    :conjunction-introduction
    (let [forms (formula-sequence! sources [:sources])]
      [forms (into [:and] forms)])

    :conjunction-elimination
    (let [children (connective! conjunction :and [:conjunction])
          i (indexed! index (count children) [:index])]
      [[conjunction] (nth children i)])

    :implication-elimination
    (let [children (connective! implication :implies [:implication])
          given (formula! antecedent [:antecedent])]
      (when-not (= given (first children))
        (fail! :antecedent-mismatch [:antecedent] {}))
      [[implication given] (second children)])

    :disjunction-introduction
    (let [p (formula! source [:source])
          children (connective! disjunction :or [:disjunction])
          i (indexed! index (count children) [:index])]
      (when-not (= p (nth children i))
        (fail! :disjunct-mismatch [:source] {}))
      [[p] disjunction])))

(defn instantiate!
  "Build the EXACT local semantic obligation for one well-formed rule schema.

   :hypotheses is the map of facts that cited prior steps (or the selected
   global premise) must establish; the conclusion must hold whenever all of
   them hold. Hypothesis names are LOCALLY SYNTHETIC, not kernel step IDs.
   For :premise the provided qualified :premise-id is preserved as an OPEN
   assumption; other rules inherit but never discharge their source premises.

   Returned data is intentionally copyable, unchecked, and non-authoritative.
   v1333 may exhaustively check these obligations against opaque valuations;
   even that would not by itself establish that kernel code enforces them."
  [source]
  (let [specification (validate-keys! source)
        rule (:rule source)
        [hypotheses conclusion] (local-arguments! source)
        ids (if (= rule :premise)
              [(:premise-id source)]
              (mapv (fn [i] (keyword "hypothesis" (str "source-" i)))
                    (range (count hypotheses))))
        hypothesis-map (into {} (map vector ids hypotheses))
        _ (formula! conclusion [:conclusion])]
    {:type obligation-type
     :version obligations-version
     :rule rule
     :side-condition (:side-condition specification)
     :dependency-policy (:dependency-policy specification)
     :hypothesis-order (vec ids)
     :hypotheses hypothesis-map
     :conclusion conclusion
     :quantification :every-total-opaque-boolean-valuation
     :local-obligation :hypotheses-semantically-entail-conclusion
     :open-premises (if (= rule :premise)
                      #{(:premise-id source)}
                      :inherited-from-cited-steps)
     :semantic-status :not-checked
     :kernel-correspondence :not-established
     :typed-atomic-semantics :not-interpreted
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
