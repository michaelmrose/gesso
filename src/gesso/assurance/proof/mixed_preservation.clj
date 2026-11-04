(ns gesso.assurance.proof.mixed-preservation
  "Source-derived, restricted preservation by independent frame and literal
   assignment facts (v1263).

   In addition to the existing whole-rule frame-only (v1259) and literal-only
   (v1261) cases, this checker recognizes a limited MIXED conjunction:

     [:and FRAME-CLAUSE LITERAL-CLAUSE ...]

   Every immediate conjunct must be independently well-typed: no conjunct
   may rely on order-dependent optional/non-nil refinements from a preceding
   conjunct. A frame-only conjunct is unchanged because all its readers have
   checked :same-field derivations. A literal-only conjunct holds afterwards
   only if its complete typed expression evaluates TRUE on the exact literal
   post-bindings, supported by checked assignment-equality derivations.
   Conjunction introduction then gives the full post-invariant, conditional
   on the original pre-invariant and matching abstract transition.

   IMPORTANT: Do not flatten, reorder, distribute, or normalize a user rule's
   :and. Gesso's typed evaluator is short-circuit and order-sensitive. In
   particular optional-field refinements across conjuncts are NOT independently
   justified here, and remain open. Nor does this rule prove predicates where
   one conjunct reads both written and framed fields, copy assignments, or
   arbitrary Clojure callbacks. A typed literal is not an opaque computation.

   All returned evidence is COPYABLE, UNADMITTED data. This is an inference
   over the declared abstract relation, not an admitted certificate, a proof
   of the inference rules' meta-soundness, or a production/XTDB guarantee.
   All original v1241 obligations remain open for independent certification."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def mixed-preservation-version 1)
(def report-type
  :gesso.assurance.proof.mixed-preservation/unadmitted-mixed-preservation)

(def ^:private identity-id :gesso.assurance.proof/mixed-preservation-v1)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private checker-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})
(def ^:private obligation-categories
  #{:initial-state :preservation :assumption :implementation})

(defn- fail!
  [reason details]
  (throw (ex-info "Invalid Gesso combined preservation inference."
                  (merge {:error/type ::invalid-mixed-preservation
                          :reason reason}
                         details))))

(defn- expression-readers
  "Only called AFTER the original rule's syntax and types were checked.
   This is a syntactic footprint, not an interpretation or normalization."
  [expression]
  (case (first expression)
    :read #{(second expression)}
    :literal #{}
    (reduce set/union #{} (map expression-readers (rest expression)))))

(defn- checked-derivation!
  [transition description field kind literal]
  (let [goal (if (= kind :frame)
               [:same-field field]
               [:eq [:post field] [:literal literal]])
        proof {:transition transition
               :steps [{:id :gesso.assurance.proof.mixed-preservation/fact
                        :rule kind :field field :conclusion goal}]
               :goal goal}
        checked (derivation/check-transition-derivation! proof)]
    (when-not (and (= derivation/report-type (:type checked))
                   (= derivation/derivation-version (:version checked))
                   (= derivation/derivation-version (:checker-version checked))
                   (= :checked-abstract-transition-derivation (:evidence-class checked))
                   (= (:fingerprint description) (:transition-fingerprint checked))
                   (= (:subject-fingerprint description) (:subject-fingerprint checked))
                   (= goal (:goal checked))
                   (= 1 (:rule-count checked))
                   (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint checked))
                   (= #{relation-premise} (:open-premises checked))
                   (= checker-obligations (:missing-obligations checked))
                   (= #{} (:trusted-assumptions checked))
                   (= :matching-abstract-transition-only (:claim-scope checked))
                   (= :pure-abstract-transition-only (:implementation-boundary checked))
                   (= :abstract-transition-derivation-checked (:checking checked))
                   (= :conditional-abstract-inference (:proof-status checked))
                   (= :conditional-on-matching-abstract-relation
                      (:conclusion-status checked))
                   (= :not-performed (:assumption-discharge checked))
                   (= :not-issued (:certificate checked))
                   (= :not-admitted (:admission checked))
                   (= :not-established (:runtime-enforcement checked))
                   (= :not-established (:implementation-correspondence checked))
                   (= :none (:authority checked)))
      (fail! :derivation-checker-contract-changed
             {:field field :kind kind :transition-fingerprint
              (:fingerprint description)}))
    {:field field :claim goal :derivation-fingerprint (:fingerprint checked)
     :scope :matching-abstract-transition-only}))

(defn- standalone-typed?
  "A well-typed whole conjunction may have clauses that cannot be typed in
   isolation (notably optional presence refinements). Such clauses stay OPEN.
   A forged or changed successful type-check report, however, is a hard error."
  [expression readers state-types]
  (try
    (let [types (select-keys state-types readers)
          checked (rule-type/check-rule-type!
                   {:readers readers :expression expression} types)]
      (when-not (and (= rule-type/checked-type (:type checked))
                     (= rule-type/type-version (:version checked))
                     (= readers (:readers checked))
                     (= expression (:expression checked))
                     (= types (:reader-types checked))
                     (= :boolean (:result-type checked))
                     (= :syntax-and-types (:checking checked))
                     (= :not-performed (:evaluation checked))
                     (= :not-proved (:proof-status checked))
                     (= :not-admitted (:admission checked))
                     (= :none (:authority checked)))
        (fail! :rule-type-checker-contract-changed {:expression expression}))
      true)
    (catch clojure.lang.ExceptionInfo e
      (if (= ::invalid-mixed-preservation (:error/type (ex-data e)))
        (throw e)
        false))))

(defn- literal-true!
  "An independent, self-contained literal fragment is a CLOSED postcondition.
   Evaluating a sampled state or trusting a cached value is insufficient."
  [expression readers state-types literal-bindings]
  (let [declaration {:readers readers :expression expression}
        evaluated (rule-eval/evaluate-rule!
                   declaration (select-keys state-types readers)
                   (select-keys literal-bindings readers))]
    (when-not (and (= rule-eval/evaluated-type (:type evaluated))
                   (= rule-eval/evaluator-version (:version evaluated))
                   (= readers (:readers evaluated))
                   (= expression (:expression evaluated))
                   (instance? Boolean (:value evaluated))
                   (= :syntax-and-types (:checking evaluated))
                   (= :pure-interpreted (:evaluation evaluated))
                   (= :not-proved (:proof-status evaluated))
                   (= :not-admitted (:admission evaluated))
                   (= :not-established (:runtime-enforcement evaluated))
                   (= :none (:authority evaluated)))
      (fail! :rule-evaluator-contract-changed {:expression expression}))
    (:value evaluated)))

(defn- conjuncts
  "Do not flatten nested :and (or rearrange the original expression)."
  [expression]
  (if (= :and (first expression))
    (subvec expression 1)
    [expression]))

(defn- clause-kind
  [readers frame literal-fields]
  (cond
    (set/subset? readers frame) :frame
    (and (seq readers) (set/subset? readers literal-fields)) :literal
    :else :unsupported))

(defn- classify-invariant!
  [id source-rule state-types frame literal-bindings]
  (let [expression (:expression source-rule)
        clauses (conjuncts expression)
        literal-fields (set (keys literal-bindings))
        judgments
        (mapv (fn [index clause]
                (let [readers (expression-readers clause)
                      kind (clause-kind readers frame literal-fields)
                      standalone? (and (not= :unsupported kind)
                                       (standalone-typed? clause readers state-types))]
                  {:index index :readers readers
                   :kind (if standalone? kind :unsupported)
                   :reason (when-not standalone?
                             (if (= :unsupported kind)
                               :mixed-or-copy-assigned-clause
                               :not-independently-well-typed))}))
              (range) clauses)
        kinds (set (map :kind judgments))
        separated? (not (contains? kinds :unsupported))
        pure-frame? (= kinds #{:frame})
        pure-literal? (= kinds #{:literal})
        mixed? (and (= :and (first expression))
                    (= kinds #{:frame :literal}))
        candidate? (and separated? (or pure-frame? pure-literal? mixed?))
        literals-true?
        (and candidate?
             (every? true?
                     (for [[clause judgment] (map vector clauses judgments)
                           :when (= :literal (:kind judgment))]
                       (literal-true! clause (:readers judgment)
                                      state-types literal-bindings))))
        checked? (and candidate? literals-true?)
        frame-readers (if checked?
                        (into #{} (mapcat :readers)
                              (filter #(= :frame (:kind %)) judgments))
                        #{})
        literal-readers (if checked?
                          (into #{} (mapcat :readers)
                                (filter #(= :literal (:kind %)) judgments))
                          #{})]
    {:obligation-id [:preservation id]
     :invariant id
     :readers (:readers source-rule)
     :clause-judgments judgments
     :inference (cond
                  (not checked?) :not-established-by-separable-conjunction
                  mixed? :frame-and-literal-conjunction
                  pure-frame? :pure-typed-rule-frame-extensionality
                  :else :closed-typed-literal-postcondition)
     :frame-readers frame-readers
     :literal-readers literal-readers
     :literal-postcondition (cond
                              (not candidate?) :not-checked
                              pure-frame? :not-applicable
                              literals-true? :true
                              :else :false)
     :status (if checked? :checked-conditional-inference :open)}))

(defn check-mixed-preservation!
  "Recheck original v1241 source, independently derive primitive field facts,
   and classify EACH preservation obligation. The new inference handles flat
   conjunctions composed exclusively of self-contained frame-only and typed
   literal-only clauses. Original obligations remain OPEN and unadmitted."
  [source]
  (let [obligations (obligation/derive-obligations! source)
        transition (:transition (construction/build-guarded-replacement!
                                 (:construction source)))
        description (abstract-transition/describe-transition! transition)
        {:keys [state-types writes assignments frame-fields]} (:semantics description)
        declared (:invariants obligations)
        preservation (get-in obligations [:obligations :preservation])]
    (when-not (and (= obligation-categories (set (keys (:obligations obligations))))
                   (= (:fingerprint description) (:transition-fingerprint obligations))
                   (= writes (set (keys assignments)))
                   (= (set (keys state-types)) (set/union writes frame-fields))
                   (= (set (keys declared)) (set (map :invariant preservation)))
                   (= (count declared) (count preservation))
                   (every? #(= :open (:status %)) preservation))
      (fail! :obligation-source-contract-changed {}))
    (let [literal-bindings
          (into (sorted-map)
                (keep (fn [[field assignment]]
                        (when (= :literal (first assignment))
                          [field (second assignment)])))
                assignments)
          outcomes (mapv (fn [[id _normalized]]
                           (classify-invariant! id (get-in source [:invariants id])
                                                state-types frame-fields literal-bindings))
                         declared)
          frame-fields-needed (into (sorted-set) (mapcat :frame-readers) outcomes)
          assigned-fields-needed (into (sorted-set) (mapcat :literal-readers) outcomes)
          frame-proofs (into (sorted-map)
                             (map (fn [field]
                                    [field (checked-derivation! transition description
                                                                field :frame nil)]))
                             frame-fields-needed)
          assignment-proofs
          (into (sorted-map)
                (map (fn [field]
                       [field (checked-derivation! transition description field
                                                   :assignment (get literal-bindings field))]))
                assigned-fields-needed)
          established (into #{} (map :obligation-id)
                            (filter #(= :checked-conditional-inference (:status %)) outcomes))
          open (into #{} (map :obligation-id)
                     (filter #(= :open (:status %)) outcomes))
          expected (set (map :id preservation))]
      (when-not (and (= expected (set/union established open))
                     (empty? (set/intersection established open)))
        (fail! :preservation-obligation-coverage-loss {}))
      (let [identity
            (subject/describe-subject
             {:kind :rule :id identity-id
              :interface
              {:version mixed-preservation-version
               :obligation-fingerprint (:fingerprint obligations)
               :transition-fingerprint (:fingerprint description)
               :outcomes outcomes
               :frame-derivations (into (sorted-map)
                                        (map (fn [[field x]]
                                               [field (:derivation-fingerprint x)]))
                                        frame-proofs)
               :assignment-derivations (into (sorted-map)
                                             (map (fn [[field x]]
                                                    [field (:derivation-fingerprint x)]))
                                             assignment-proofs)}})]
        {:type report-type
         :version mixed-preservation-version
         :fingerprint (:fingerprint identity)
         :obligation-fingerprint (:fingerprint obligations)
         :transition-fingerprint (:fingerprint description)
         :subject-fingerprint (:subject-fingerprint description)
         :proof-rule :separable-frame-and-literal-conjunction
         :claim :all-pre-invariants-and-matching-transition-imply-post-invariant
         :scope :declared-abstract-state-projection-only
         :preservation-outcomes outcomes
         :frame-derivations frame-proofs
         :assignment-derivations assignment-proofs
         :conditionally-checked-preservation-ids established
         :unsupported-preservation-ids open
         :original-open-obligation-ids (:open-obligation-ids obligations)
         :checking :original-sources-independent-field-derivations-and-typed-evaluation
         :proof-status :restricted-conditional-preservation-checked
         :checker-soundness :not-independently-proved
         :open-meta-obligations
         #{:proof-checker-frame-and-literal-conjunction-rule-soundness
           :certificate-admission}
         :certificate :not-issued
         :admission :not-admitted
         :runtime-enforcement :not-established
         :implementation-correspondence :not-established
         :authority :none}))))
