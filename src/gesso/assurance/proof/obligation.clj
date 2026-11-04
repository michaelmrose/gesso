(ns gesso.assurance.proof.obligation
  "Source-derived, UNEVALUATED assurance obligations for a guarded replacement.

   A user supplies only original, typed Gesso construction and rule DATA:

     {:construction <the exact v1239 guarded-replacement SOURCE map>
      :initial-condition {:readers #{:request/status}
                          :expression [:= [:read :request/status]
                                          [:literal :open]]}
      :invariants {:request/valid-status
                   {:readers #{:request/status}
                    :expression [:in [:read :request/status]
                                 [:literal #{:open :claimed}]]}}}

   The closed source is checked afresh by the v1239 constructor (which in turn
   checks the v1233 transition) and the existing typed rule normalizer. An
   invariant is a predicate on a DECLARED state projection, not a callback or
   a proposition asserted true by its mere presence in this input.

   For EACH invariant I, this namespace emits TWO open obligations:

     initial:       forall s. Initial(s) => I(s)
     preservation:  forall s,x,s'. ALL-I(s) and T(s,x,s') => I(s')

   ALL-I is the conjunction of every declared invariant, not just the current
   one. T is the EXACT abstract guarded assignment/frame relation, including
   its original read/write footprint. No SMT solving, implication proof,
   satisfaction check or certificate issuance happens here. In particular,
   an invariant independent of the writes is NOT silently discharged: it still
   has a preservation obligation and the exact frame must be justified.

   Initial(s) only describes a proposed initial-state class. Existence of such
   states and coverage of all real initial states are separate OPEN assumptions.
   Gesso also cannot infer that the submitted invariant map contains EVERY
   intended application rule. Invariant-set completeness is an OPEN assumption
   until independently established at whole-application assembly.
   Assumptions concerning projection completeness, runtime value interpretation,
   authorization, actual physical side effects, atomic XTDB commits, and proof
   checker soundness remain open and are never silently certified. Passing an
   incomplete state projection can never prove properties of omitted fields.
   The returned structure is COPYABLE UNADMITTED DATA; it is not proof evidence,
   deployment authorization, or a durable identity token. Subsequent consumers
   MUST rederive it from the original sources and establish every premise."
  (:require [clojure.set :as set]
            [gesso.assurance.rule-normalize :as rule-normalize]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def obligation-version 1)
(def obligation-type :gesso.assurance.proof.obligation/unadmitted-obligations)
(def ^:private identity-id :gesso.assurance.proof/obligations-v1)
(def ^:private required-keys
  #{:construction :initial-condition :invariants})
(def ^:private max-invariants 64)

;; This mapping must be reviewed when the v1239 constructor changes its
;; contract. A new upstream obligation cannot be dropped by an older composer.
(def ^:private known-construction-obligations
  #{:initial-state-invariant
    :abstract-invariant-preservation
    :production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :physical-effect-containment})

(def ^:private source-assumption-specs
  {:initial-state-existence
   {:required-property :declared-initial-state-class-is-inhabited
    :source-path [:initial-condition]}
   :initial-state-coverage
   {:required-property :every-physical-initial-state-refines-declared-initial-condition
    :source-path [:initial-condition]}
   :declared-state-projection-coverage
   {:required-property :all-relevant-physical-fields-are-in-declared-projection
    :source-path [:construction :state-types]}
   :declared-invariant-set-coverage
   {:required-property :all-relevant-application-invariants-are-declared
    :source-path [:invariants]}
   :checker-meta-soundness
   {:required-property :checker-rules-preserve-their-stated-mathematical-meaning
    :source-path []}})

(def ^:private implementation-specs
  {:production-transition-correspondence
   {:required-property :each-real-accepted-step-refines-the-declared-transition
    :source-path [:construction]}
   :current-runtime-authorization
   {:required-property :authorizations-are-checked-at-the-real-execution-boundary
    :source-path [:construction :subject]}
   :atomic-transaction-enforcement
   {:required-property :authoritative-commit-is-atomic
    :source-path [:construction]}
   :physical-effect-containment
   {:required-property :physical-effects-respect-the-complete-declared-footprint
    :source-path [:construction]}
   :runtime-type-correspondence
   {:required-property :runtime-values-conform-to-the-declared-types
    :source-path [:construction]}
   :integer-semantics-correspondence
   {:required-property :runtime-integer-behavior-refines-mathematical-integers
    :source-path [:construction :state-types]}})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso source-derived proof obligations."
                  (merge {:error/type ::invalid-obligation-source
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value required at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= actual required)
      (reject! :wrong-keys at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual required)})))
  value)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- checked-state-rule!
  "A state predicate may not read an input field or an undeclared field.
   Invoke the existing syntax/type/normalization pipeline, not a second AST
   parser or a custom type checker."
  [declaration state-types at]
  (when-not (plain-map? declaration)
    (reject! :expected-predicate-source at {}))
  (metadata-free! declaration at)
  (exact-map! declaration #{:readers :expression} at)
  (let [readers (:readers declaration)]
    (when-not (instance? clojure.lang.PersistentHashSet readers)
      (reject! :expected-reader-set (conj at :readers) {}))
    (metadata-free! readers (conj at :readers))
    (let [state-fields (set (keys state-types))]
      (when-not (set/subset? readers state-fields)
        (reject! :non-state-reader (conj at :readers)
                 {:unexpected (set/difference readers state-fields)})))
    ;; This returns the FULL normalized typed semantic expression and its
    ;; normalized nominal reader types. No syntax/type semantics are duplicated.
    (:semantics (rule-normalize/normalize-rule!
                 declaration (select-keys state-types readers)))))

(defn- invariant-sources! [invariants]
  (when-not (plain-map? invariants)
    (reject! :expected-invariant-map [:invariants] {}))
  (metadata-free! invariants [:invariants])
  (when-not (<= 1 (count invariants) max-invariants)
    (reject! :invariant-count [:invariants]
             {:minimum 1 :maximum max-invariants}))
  (doseq [id (keys invariants)]
    (when-not (qualified-id? id)
      (reject! :invalid-invariant-id [:invariants id] {:id id})))
  invariants)

(defn- assumed-boundaries [transition-fingerprint]
  (mapv (fn [[id spec]]
          (merge {:id [:assumption id]
                  :kind :assumption
                  :source :gesso-assurance-abstract-contract
                  :transition-fingerprint transition-fingerprint
                  :status :open}
                 spec))
        (sort-by key source-assumption-specs)))

(defn- implementation-boundaries [transition-fingerprint]
  (mapv (fn [[id spec]]
          (merge {:id [:implementation id]
                  :kind :implementation
                  :transition-fingerprint transition-fingerprint
                  :status :open}
                 spec))
        (sort-by key implementation-specs)))

(defn- initial-obligation
  [id fingerprint]
  {:id [:initial-state id]
   :kind :initial-state
   :invariant id
   :quantification :all-declared-abstract-states
   ;; Rule meaning is owned by :initial-condition and :invariants in the
   ;; surrounding, exact-source-bound report. Avoid copying the same typed
   ;; AST into every obligation (which causes quadratic identity growth).
   :antecedent {:initial-condition-ref :declared-initial-condition}
   :consequent {:state :pre :invariant-ref id}
   :transition-fingerprint fingerprint
   :status :open})

(defn- preservation-obligation
  [id invariant invariant-ids source description]
  (let [{:keys [reads writes frame-fields guard]} (:semantics description)]
    {:id [:preservation id]
     :kind :invariant-preservation
     :invariant id
     :quantification :all-declared-abstract-pre-input-post-triples
     ;; The antecedent is ALL invariants in the pre-state, including THIS one.
     ;; The named, checked v1233 transition relation binds pre/input/post.
     :antecedent {:pre-invariant-ids invariant-ids
                  :transition-fingerprint (:fingerprint description)
                  :transition-relation :exact-typed-guard-assignments-and-frame}
     :consequent {:state :post :invariant-ref id}
     :source-read-fields reads
     :source-write-fields writes
     :source-frame-fields frame-fields
     :guard-read-fields (get guard :readers)
     :invariant-read-fields (:readers invariant)
     :invariant-written-fields (set/intersection writes (:readers invariant))
     :invariant-framed-fields (set/intersection frame-fields (:readers invariant))
     :state-projection (set (keys (:state-types source)))
     :status :open}))

(defn derive-obligations!
  "Recheck original source declarations and enumerate complete, open
   initial/preservation/assumption/implementation obligations. The ONLY input
   is {:construction <v1239 SOURCE> :initial-condition <typed state rule>
       :invariants {qualified-id <typed state rule> ...}}.

   All named invariants appear in both the initial and preservation obligation
   sets. All declared writes and frames appear in EVERY preservation statement;
   invariants on framed fields still require a proof. Returned reports/fingerprints
   supplied as input are rejected, not accepted as evidence.

   A returned :fingerprint is a semantic *change detector*, never proof or
   authenticity. The returned :source supports independent rederivation."
  [source]
  (exact-map! source required-keys [])
  (let [{:keys [construction initial-condition invariants]} source
        checked-construction (construction/build-guarded-replacement! construction)
        transition-source (:transition checked-construction)
        transition-fingerprint (:transition-fingerprint checked-construction)
        _ (when-not (= known-construction-obligations
                       (:open-obligations checked-construction))
            (reject! :construction-obligation-contract-changed
                     [:construction :open-obligations]
                     {:missing (set/difference known-construction-obligations
                                               (:open-obligations checked-construction))
                      :unexpected (set/difference (:open-obligations checked-construction)
                                                  known-construction-obligations)}))
        description (abstract-transition/describe-transition! transition-source)
        _ (when-not (= transition-fingerprint (:fingerprint description))
            (reject! :transition-fingerprint-mismatch [:construction] {}))
        state-types (:state-types transition-source)
        initial (checked-state-rule! initial-condition state-types
                                     [:initial-condition])
        invariant-rules (invariant-sources! invariants)
        invariant-semantics
        (into (sorted-map)
              (for [[id rule] invariant-rules]
                [id (checked-state-rule! rule state-types [:invariants id])]))
        ;; Recheck source on EVERY call, but compute the same v1233 semantic
        ;; description only once rather than once per invariant.
        preservation-obligations
        (mapv (fn [[id invariant]]
                (preservation-obligation id invariant
                                         (set (keys invariant-semantics))
                                         transition-source description))
              invariant-semantics)
        initial-obligations
        (mapv (fn [[id _]]
                (initial-obligation id transition-fingerprint))
              invariant-semantics)
        assumption-obligations (assumed-boundaries transition-fingerprint)
        implementation-obligations (implementation-boundaries transition-fingerprint)
        obligations {:initial-state initial-obligations
                     :preservation preservation-obligations
                     :assumption assumption-obligations
                     :implementation implementation-obligations}
        open-ids (into #{} (map :id) (mapcat val obligations))
        semantic-context
        {:transition-source transition-source
         :transition-fingerprint transition-fingerprint
         :initial-condition initial
         :invariants invariant-semantics
         :obligations obligations}
        identity (subject/describe-subject
                  {:kind :rule :id identity-id :interface semantic-context})]
    (when-not (= (set (keys invariant-semantics))
                 (set (map :invariant initial-obligations))
                 (set (map :invariant preservation-obligations)))
      (reject! :invariant-obligation-loss [] {}))
    (when-not (= (set (keys obligations))
                 #{:initial-state :preservation :assumption :implementation})
      (reject! :obligation-category-loss [] {}))
    {:type obligation-type
     :version obligation-version
     :source source
     :subject-fingerprint (:subject-fingerprint checked-construction)
     :transition-fingerprint transition-fingerprint
     :fingerprint (:fingerprint identity)
     :initial-condition initial
     :invariants invariant-semantics
     :obligations obligations
     :open-obligation-ids open-ids
     :status :all-obligations-open
     :checking :original-sources-and-typed-rules
     :proof-status :not-proved
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
