(ns gesso.assurance.proof.literal-assignment-soundness
  "Restricted, independent S4 literal-assignment preservation schema (v1355).

   Let L be fields assigned typed, source-declared literal values by the
   guarded abstract transition T. For a well-typed, deterministic invariant
   I with a NONEMPTY reader set R subset L, every matching transition gives
   the identical post-state projection on R. If independent S1 interpretation
   evaluates I on that closed literal projection to TRUE, then, conditional
   on exact transition semantics and S1 extensionality:

       T(s,x,s') => I(s'), hence I(s) AND T(s,x,s') => I(s').

   No prestate sample or assumption that the guard is satisfiable is used to
   derive this rule. If the fixed postcondition is false, the invariant reads
   a frame/copy field, or there are no readers, this narrow rule stays OPEN.
   In particular an observed successful transition cannot justify a universal
   claim about unsupported reader dependencies.

   The original source is checked by the production transition and typed-rule
   admission paths. Literal values and the full postcondition are interpreted
   independently using v1341/v1344, NOT the production rule evaluator or the
   v1261 preservation checker. audit-literal-witness! separately interprets
   caller-supplied before/input/after maps through v1351. Reports are
   copyable diagnostics, not certificates or evidence of actual XTDB writes.
   Universal S1 soundness and physical implementation correspondence remain
   open obligations."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-normalize :as typed-rule]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.proof.typed-presence-semantics :as presence]))

(def semantics-version 1)
(def report-type
  :gesso.assurance.proof.literal-assignment-soundness/conditional-lemma)
(def witness-type
  :gesso.assurance.proof.literal-assignment-soundness/witness-audit)

(def ^:private invariant-keys #{:id :readers :expression})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid conditional literal-assignment soundness construction."
                  (merge {:literal-assignment-soundness/rejection true
                          :reason reason :at at} details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- checked-invariant! [invariant state-types]
  (when-not (plain-map? invariant)
    (reject! :expected-invariant-map [:invariant] {}))
  (metadata-free! invariant [:invariant])
  (when-not (= invariant-keys (set (keys invariant)))
    (reject! :invariant-keys [:invariant]
             {:expected invariant-keys :actual (set (keys invariant))}))
  (when-not (qualified-id? (:id invariant))
    (reject! :invalid-invariant-id [:invariant :id] {}))
  ;; The syntax checker independently extracts *every* reader occurrence,
  ;; disallowing hidden readers and unused declared readers.
  (let [source (select-keys invariant [:readers :expression])
        syntax (rule/check-rule! source)
        readers (:readers syntax)]
    (when-not (set/subset? readers (set (keys state-types)))
      (reject! :invariant-reads-outside-state [:invariant :readers]
               {:unknown (set/difference readers (set (keys state-types)))}))
    (let [admission (typed-rule/normalize-rule!
                     source (select-keys state-types readers))]
      (when-not (= readers (get-in admission [:semantics :readers]))
        (reject! :typed-reader-footprint-disagreement [:invariant] {}))
      {:readers readers :admission admission})))

(defn- literal-fields!
  "Reconstruct literal writes from ORIGINAL source, rather than trusting a
   cached summary or normalized report. Check each present literal with the
   independent v1341 value domain before using it as a proof premise."
  [declaration description]
  (let [writes (:writes declaration)
        assignments (:assignments declaration)
        normalized (get-in description [:semantics :assignments])]
    (when-not (= writes (set (keys assignments)) (set (keys normalized)))
      (reject! :assignment-footprint-disagreement [:assignments] {}))
    (into (sorted-map)
          (keep (fn [[field expr]]
                  (when (= :literal (first expr))
                    (let [literal (second expr)
                          descriptor (get-in declaration [:state-types field])]
                      (when-not (values/value-in-domain? descriptor literal)
                        (reject! :independent-literal-type-disagreement
                                 [:assignments field] {}))
                      ;; Normalization may normalize type descriptors, but it
                      ;; must not change the declared literal's value.
                      (when-not (= [:literal literal] (get normalized field))
                        (reject! :normalized-literal-disagreement
                                 [:assignments field] {}))
                      [field literal]))))
          assignments)))

(defn derive-literal-lemma!
  "Instantiate the restricted S4 constant-postcondition rule from sources.
   Successful status is a CONDITIONAL SCHEMA, not a certificate.
   A false postcondition or unsupported reader footprint remains explicitly
   open. The returned map is not accepted as evidence by another boundary."
  [declaration invariant]
  (let [description (transition/describe-transition! declaration)
        state-types (:state-types declaration)
        {:keys [readers admission]} (checked-invariant! invariant state-types)
        literals (literal-fields! declaration description)
        literal-readers (set (keys literals))
        unsupported (set/difference readers literal-readers)
        eligible? (and (seq readers) (empty? unsupported))
        bindings (when eligible? (select-keys literals readers))
        fixed-result (when eligible?
                       (:value (presence/evaluate-expression!
                                (:expression invariant)
                                (select-keys state-types readers)
                                bindings)))
        status (cond
                 (empty? readers) :open-empty-reader-footprint
                 (seq unsupported) :open-nonliteral-reader
                 (not fixed-result) :open-false-postcondition
                 :else :conditional-literal-rule-instance)]
    {:type report-type
     :version semantics-version
     :transition-fingerprint (:fingerprint description)
     :invariant-id (:id invariant)
     :source-invariant invariant
     :typed-invariant-fingerprint (:fingerprint admission)
     :literal-writes literals
     :readers readers
     :nonliteral-readers unsupported
     :fixed-post-reader-projection bindings
     :fixed-post-invariant fixed-result
     :schema :literal-writes-fix-entire-invariant-reader-projection
     :premises #{:valid-typed-invariant
                 :exact-abstract-literal-assignment-relation
                 :pure-typed-expression-extensionality}
     :conditional-claim (when (= status :conditional-literal-rule-instance)
                          :matching-transition-establishes-post-invariant)
     :status status
     :typed-expression-soundness :not-independently-proved
     :transition-implementation-soundness :not-independently-proved
     :proof-status :conditional-schema-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- fixed-reader-projection?
  [after readers literals]
  (every? (fn [field]
            (and (contains? after field)
                 (= (get after field) (get literals field))))
          readers))

(defn audit-literal-witness!
  "Observe a caller-supplied abstract transition, then compare a matching
   transition's post-invariant with the independently evaluated fixed literal
   result. Nonmatching transitions do not instantiate the preservation rule;
   witnesses never prove the universal schema or authentic database effects."
  [declaration invariant before inputs after]
  (let [lemma (derive-literal-lemma! declaration invariant)
        observed (independent/observe-transition! declaration before inputs after)
        readers (:readers lemma)
        types (select-keys (:state-types declaration) readers)
        before-value (:value (presence/evaluate-expression!
                              (:expression invariant) types
                              (select-keys before readers)))
        after-value (:value (presence/evaluate-expression!
                             (:expression invariant) types
                             (select-keys after readers)))
        eligible? (= :conditional-literal-rule-instance (:status lemma))
        matches? (:matches? observed)
        fixed? (fixed-reader-projection?
                after readers (:literal-writes lemma))]
    ;; The independent observation remains copyable data. Refuse a partial,
    ;; substituted, or internally inconsistent observation before using it as
    ;; a witness. Even a well-formed observation is NOT authoritative proof.
    (when-not (and (= independent/report-type (:type observed))
                   (= independent/semantics-version (:version observed))
                   (= declaration (:source-declaration observed))
                   (= before (:before observed))
                   (= inputs (:inputs observed))
                   (= after (:after observed))
                   (= :independent-typed-guarded-relation (:checking observed))
                   (every? #(instance? Boolean (get observed %))
                           [:guard-holds? :assignments-hold? :frame-holds? :matches?])
                   (= matches? (and (:guard-holds? observed)
                                    (:assignments-hold? observed)
                                    (:frame-holds? observed)))
                   (= :not-proved (:proof-status observed))
                   (= :not-admitted (:admission observed))
                   (= :none (:authority observed)))
      (reject! :independent-observation-contract-changed [:witness] {}))
    (when (and matches? eligible?
               (or (not fixed?)
                   (not= (:fixed-post-invariant lemma) after-value)))
      (reject! :independent-literal-extensionality-disagreement [:witness]
               {:invariant (:id invariant)}))
    {:type witness-type
     :version semantics-version
     :transition-fingerprint (:transition-fingerprint lemma)
     :invariant-id (:id invariant)
     :lemma-status (:status lemma)
     :matches-transition? matches?
     :fixed-post-reader-projection? (and eligible? fixed?)
     :before-invariant before-value
     :after-invariant after-value
     :preservation-implication-holds? (or (not before-value) after-value)
     :status (cond
               (not eligible?) (:status lemma)
               (not matches?) :nonmatching-transition
               :else :checked-conditional-literal-instance)
     :checking :independent-abstract-relation-and-typed-expression
     :proof-status :not-universally-proved
     :physical-observation :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
