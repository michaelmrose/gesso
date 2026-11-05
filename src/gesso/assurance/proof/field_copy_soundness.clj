(ns gesso.assurance.proof.field-copy-soundness
  "Independent, restricted S4 same-origin field-copy soundness (v1357).

   Under the guarded relation T, each written field is assigned from one
   PRE-state or INPUT reader (or a literal), and each untouched field keeps
   its pre-state value and presence.  When two distinct POST-state fields
   have the same TOTAL pre-state/input origin, and at least one is written
   by a :read assignment, their values are equal for every matching T.

      T(s,x,s') => s'[a] = origin(s,x) = s'[b].

   This rule covers two copies of one source and a copy paired with its
   required, frame-preserved source. It does not use sampled equal values to
   infer a relationship between different origins. In particular, sources
   that are themselves written are read from BEFORE, not from the resulting
   state (simultaneous assignment semantics). Optional frame fields do not
   have a total origin and are unsupported by this schema.

   This module independently reconstructs origins from the ORIGINAL
   declaration; it does not use v1265 copy-preservation, the production
   derivation kernel, or cached origin summaries. Source declarations must
   still pass production transition and typed-rule admission. Concrete
   witnesses use v1351 independent transition semantics and v1344 typed
   expression interpretation. Results are copyable conditional diagnostics,
   NOT universal implementation proofs, certificates, physical XTDB facts,
   or authorization to execute a command. S1 correctness and the actual
   implementation's transition correspondence remain separate obligations."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-normalize :as typed-rule]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.typed-presence-semantics :as presence]))

(def semantics-version 1)
(def report-type :gesso.assurance.proof.field-copy-soundness/conditional-lemma)
(def witness-type :gesso.assurance.proof.field-copy-soundness/witness-audit)

(def ^:private invariant-keys #{:id :readers :expression})

(defn- reject! [reason at more]
  (throw (ex-info "Invalid conditional field-copy soundness construction."
                  (merge {:field-copy-soundness/rejection true
                          :reason reason :at at} more))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- checked-invariant! [invariant state-types]
  (when-not (and (plain-map? invariant)
                 (nil? (meta invariant))
                 (= invariant-keys (set (keys invariant)))
                 (qualified-id? (:id invariant)))
    (reject! :invalid-invariant-source [:invariant] {}))
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

(defn- direct-equality-fields [expression]
  (when (and (vector? expression)
             (nil? (meta expression))
             (= 3 (count expression))
             (= := (first expression)))
    (let [[_ a b] expression]
      (when (and (vector? a) (nil? (meta a))
                 (= 2 (count a)) (= :read (first a))
                 (vector? b) (nil? (meta b))
                 (= 2 (count b)) (= :read (first b))
                 (qualified-id? (second a)) (qualified-id? (second b))
                 (not= (second a) (second b)))
        [(second a) (second b)]))))

(defn- post-equality-fields
  "Support direct equality OR one exact guarded form allowing optional POST
   targets to be present before equality is evaluated. No general Boolean
   rewriting, commutation, or arbitrary guard inference is admitted."
  [expression]
  (or (direct-equality-fields expression)
      (when (and (vector? expression) (nil? (meta expression))
                 (= :and (first expression)) (= 4 (count expression)))
        (when-let [[left right] (direct-equality-fields (nth expression 3))]
          (when (and (= [:present [:read left]] (nth expression 1))
                     (= [:present [:read right]] (nth expression 2))
                     (every? nil? (map meta [(nth expression 1)
                                             (nth expression 2)
                                             (second (nth expression 1))
                                             (second (nth expression 2))])))
            [left right])))))

(defn- post-origin
  "Return a total value term, independent of the production normalizer.
   A required untouched field reads its pre-state value; a written field
   reads the pre-state or input named by its source assignment."
  [declaration field]
  (let [{:keys [state-types input-types writes assignments]} declaration]
    (if-not (contains? writes field)
      (when-not (true? (get-in state-types [field :optional?]))
        [:pre field])
      (let [[op reader] (get assignments field)]
        (when (= op :read)
          (cond
            (contains? state-types reader) [:pre reader]
            (contains? input-types reader) [:input reader]
            :else (reject! :unrecognized-copy-reader [:assignments field]
                           {:reader reader})))))))

(defn- copied-field? [declaration field]
  (and (contains? (:writes declaration) field)
       (= :read (first (get-in declaration [:assignments field])))))

(defn- checked-normalized-copy! [declaration description field]
  (when (copied-field? declaration field)
    (let [source (get-in declaration [:assignments field])
          normalized (get-in description [:semantics :assignments field])]
      (when-not (= source normalized)
        (reject! :normalized-copy-disagreement [:assignments field]
                 {:source source :normalized normalized})))))

(defn derive-copy-lemma!
  "Instantiate the restricted same-total-origin equality rule from ORIGINAL
   source, after ordinary transition and typed-invariant admission.

   An exact well-typed POST equality is in scope, optionally guarded by
   both POST-field presence checks for optional copy targets. Both origins
   must be total and identical; at least
   one field must be copy-written. Other valid expressions stay explicitly
   OPEN. The report has NO certificate or admission authority."
  [declaration invariant]
  (let [description (transition/describe-transition! declaration)
        {:keys [readers admission]}
        (checked-invariant! invariant (:state-types declaration))
        fields (post-equality-fields (:expression invariant))
        [left right] fields
        _ (doseq [field fields]
            (checked-normalized-copy! declaration description field))
        left-origin (when left (post-origin declaration left))
        right-origin (when right (post-origin declaration right))
        shared? (and (some? left-origin) (= left-origin right-origin))
        has-copy? (boolean (some #(copied-field? declaration %) fields))
        status (cond
                 (nil? fields) :open-unsupported-invariant-shape
                 (not shared?) :open-no-shared-total-origin
                 (not has-copy?) :open-no-copy-assignment
                 :else :conditional-same-origin-copy-rule)]
    {:type report-type
     :version semantics-version
     :transition-fingerprint (:fingerprint description)
     :invariant-id (:id invariant)
     :source-invariant invariant
     :typed-invariant-fingerprint (:fingerprint admission)
     :readers readers
     :post-fields fields
     :postcondition-shape (when fields
                            (if (= :and (first (:expression invariant)))
                              :presence-guarded-equality :direct-equality))
     :left-origin left-origin
     :right-origin right-origin
     :shared-total-origin (when shared? left-origin)
     :copied-fields (into #{} (filter #(copied-field? declaration %)) fields)
     :schema :same-total-prestate-or-input-origin-implies-post-equality
     :premises #{:well-typed-equality-invariant
                 :exact-guarded-simultaneous-copy-and-frame-relation
                 :total-required-prestate-or-input-origin
                 :pure-typed-equality-extensionality}
     :conditional-claim (when (= status :conditional-same-origin-copy-rule)
                          :matching-transition-establishes-post-equality)
     :status status
     :typed-expression-soundness :not-independently-proved
     :transition-implementation-soundness :not-independently-proved
     :proof-status :conditional-schema-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- origin-value [term before inputs]
  (let [[kind field] term]
    (case kind
      :pre (get before field)
      :input (get inputs field))))

(defn- consistent-observation! [observed declaration before inputs after]
  ;; Reports are copyable. Verify every contract field used to make a witness
  ;; claim rather than trusting a substituted or partially forged report.
  (let [m (:matches? observed)]
    (when-not (and (= independent/report-type (:type observed))
                   (= independent/semantics-version (:version observed))
                   (= declaration (:source-declaration observed))
                   (= before (:before observed))
                   (= inputs (:inputs observed))
                   (= after (:after observed))
                   (= :independent-typed-guarded-relation (:checking observed))
                   (every? #(instance? Boolean (get observed %))
                           [:guard-holds? :assignments-hold? :frame-holds? :matches?])
                   (= m (and (:guard-holds? observed)
                             (:assignments-hold? observed)
                             (:frame-holds? observed)))
                   (= :not-proved (:proof-status observed))
                   (= :not-admitted (:admission observed))
                   (= :none (:authority observed)))
      (reject! :independent-observation-contract-changed [:witness] {}))))

(defn audit-copy-witness!
  "Compare one caller-supplied before/input/after sample against the
   independent guarded relation and the post-equality schema. Never use an
   observed equality between DIFFERENT origins to infer a general rule."
  [declaration invariant before inputs after]
  (let [lemma (derive-copy-lemma! declaration invariant)
        observed (independent/observe-transition! declaration before inputs after)
        _ (consistent-observation! observed declaration before inputs after)
        fields (:post-fields lemma)
        origin (:shared-total-origin lemma)
        matching? (:matches? observed)
        eligible? (= :conditional-same-origin-copy-rule (:status lemma))
        readers (:readers lemma)
        types (select-keys (:state-types declaration) readers)
        before-invariant (:value (presence/evaluate-expression!
                                  (:expression invariant) types
                                  (select-keys before readers)))
        after-invariant (:value (presence/evaluate-expression!
                                 (:expression invariant) types
                                 (select-keys after readers)))
        common-value (when eligible? (origin-value origin before inputs))
        exact-post? (and eligible?
                         (every? #(and (contains? after %)
                                       (= common-value (get after %)))
                                 fields))]
    (when (and eligible? matching?
               (or (not exact-post?) (not after-invariant)))
      (reject! :independent-copy-extensionality-disagreement [:witness]
               {:invariant (:id invariant)}))
    {:type witness-type
     :version semantics-version
     :transition-fingerprint (:transition-fingerprint lemma)
     :invariant-id (:id invariant)
     :lemma-status (:status lemma)
     :matches-transition? matching?
     :common-prestate-or-input-value (when eligible? common-value)
     :post-fields-follow-common-origin? (and eligible? exact-post?)
     :before-invariant before-invariant
     :after-invariant after-invariant
     :preservation-implication-holds? (or (not before-invariant) after-invariant)
     :status (cond
               (not eligible?) (:status lemma)
               (not matching?) :nonmatching-transition
               :else :checked-conditional-copy-instance)
     :checking :independent-abstract-relation-and-typed-expression
     :proof-status :not-universally-proved
     :physical-observation :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
