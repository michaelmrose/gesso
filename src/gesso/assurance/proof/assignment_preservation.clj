(ns gesso.assurance.proof.assignment-preservation
  "Source-derived, RESTRICTED preservation for constant written fields (v1261).

   Input is the original v1241 obligation source, NOT a cached obligation or
   transition report. For an invariant I whose (nonempty) reader set consists
   entirely of fields written with typed literal assignments, every matching
   abstract transition gives I exactly the same closed, post-state inputs.

   For each such field, this module independently checks the exact assignment
   equality through the v1235 derivation checker. It then rechecks and evaluates
   the ORIGINAL well-typed invariant against the literal bindings using v1223.
   When that evaluation is true, the following conditional abstract inference
   is justified under the declared semantics:

     ALL-I(pre) AND T(pre,input,post) => I(post)

   This is NOT an evaluation of an arbitrary sampled post-state: all readers
   are fixed by checked literal assignments, so the interpreter sees the same
   bindings in EVERY matching transition. Required and optional fields both
   become PRESENT when assigned, including optional nullable fields assigned
   nil. Invariants reading framed fields, copy-assigned fields, or a mixture of
   those readers are deliberately left open here; v1259 separately handles
   the frame-only case. There is no arithmetic assignment or general solver.

   Checked derivations and a pure interpreter are the semantic premises of this
   rule; their soundness has not been independently proved. The reports are
   COPYABLE, UNADMITTED observations, NOT certificates, invariant-set coverage,
   proofs of physical XTDB effects, or transaction authorization. Recheck the
   original sources at every subsequent assurance boundary."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def assignment-preservation-version 1)
(def report-type
  :gesso.assurance.proof.assignment-preservation/unadmitted-literal-preservation)

(def ^:private identity-id
  :gesso.assurance.proof/literal-assignment-preservation-v1)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private expected-checker-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})
(def ^:private expected-obligation-categories
  #{:initial-state :preservation :assumption :implementation})

(defn- fail!
  [reason details]
  (throw (ex-info "Invalid Gesso literal-assignment preservation inference."
                  (merge {:error/type ::invalid-assignment-preservation
                          :reason reason}
                         details))))

(defn- assignment-proof-source
  [transition field literal]
  {:transition transition
   :steps [{:id :gesso.assurance.proof.assignment-preservation/assignment
            :rule :assignment
            :field field
            :conclusion [:eq [:post field] [:literal literal]]}]
   :goal [:eq [:post field] [:literal literal]]})

(defn- check-assignment!
  "Require the independent v1235 checker to prove the exact literal write.
   Reject unexpected changes to its proof and authority contracts."
  [transition fingerprint subject-fingerprint field literal]
  (let [proof (assignment-proof-source transition field literal)
        checked (derivation/check-transition-derivation! proof)]
    (when-not (and (= derivation/report-type (:type checked))
                   (= derivation/derivation-version (:version checked))
                   (= derivation/derivation-version (:checker-version checked))
                   (= :checked-abstract-transition-derivation
                      (:evidence-class checked))
                   (= fingerprint (:transition-fingerprint checked))
                   (= subject-fingerprint (:subject-fingerprint checked))
                   (= :abstract-transition-derivation-checked (:checking checked))
                   (= :pure-abstract-transition-only (:implementation-boundary checked))
                   (= (:goal proof) (:goal checked))
                   (= 1 (:rule-count checked))
                   (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint checked))
                   (= #{relation-premise} (:open-premises checked))
                   (= expected-checker-obligations (:missing-obligations checked))
                   (= #{} (:trusted-assumptions checked))
                   (= :matching-abstract-transition-only (:claim-scope checked))
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
             {:field field :transition-fingerprint fingerprint}))
    {:field field
     :conclusion (:goal checked)
     :derivation-fingerprint (:fingerprint checked)
     :scope :matching-abstract-transition-only}))

(defn- check-evaluation!
  "Evaluate the source invariant ONLY on complete, fixed literal bindings.
   This is a closed syntactic instance, not a sampled arbitrary pre/post state."
  [id source-rule normalized-rule state-types bindings]
  (let [readers (:readers normalized-rule)
        typed (select-keys state-types readers)
        result (rule-eval/evaluate-rule! source-rule typed bindings)]
    (when-not (and (= rule-eval/evaluated-type (:type result))
                   (= rule-eval/evaluator-version (:version result))
                   (= readers (:readers result))
                   (= (:expression source-rule) (:expression result))
                   (instance? Boolean (:value result))
                   (= :syntax-and-types (:checking result))
                   (= :pure-interpreted (:evaluation result))
                   (= :not-proved (:proof-status result))
                   (= :not-admitted (:admission result))
                   (= :not-established (:runtime-enforcement result))
                   (= :none (:authority result)))
      (fail! :rule-evaluator-contract-changed {:invariant id}))
    (:value result)))

(defn check-literal-assignment-preservation!
  "From ORIGINAL obligation source, check conditional preservation of each
   invariant that reads one or more exclusively literal-assigned state fields.

   A false postcondition, any frame reader, or any copy assignment yields an
   OPEN outcome, not a purported proof. No original v1241 obligation is
   discharged; this supplies separately checkable candidate inference evidence.
   An unsupported invariant remains eligible for future stronger rule systems."
  [source]
  (let [obligations (obligation/derive-obligations! source)
        transition-source
        (:transition (construction/build-guarded-replacement!
                      (:construction source)))
        description (abstract-transition/describe-transition! transition-source)
        fingerprint (:fingerprint description)
        {:keys [state-types writes assignments]} (:semantics description)
        declared-invariants (:invariants obligations)
        preservation-obligations (get-in obligations [:obligations :preservation])]
    (when-not (and (= fingerprint (:transition-fingerprint obligations))
                   (= expected-obligation-categories
                      (set (keys (:obligations obligations))))
                   (= (set (keys declared-invariants))
                      (set (map :invariant preservation-obligations)))
                   (= (count declared-invariants) (count preservation-obligations))
                   (every? #(= :open (:status %)) preservation-obligations)
                   (= writes (set (keys assignments)))
                   (= (set (keys state-types))
                      (set/union writes (:frame-fields (:semantics description)))))
      (fail! :obligation-source-contract-changed {}))
    (let [literal-writes
          (into (sorted-map)
                (keep (fn [[field expression]]
                        (when (= :literal (first expression))
                          [field (second expression)])))
                assignments)
          literal-fields (set (keys literal-writes))
          invariant-readers
          (into (sorted-map)
                (map (fn [[id rule]] [id (:readers rule)]))
                declared-invariants)
          candidate-ids
          (into (sorted-set)
                (keep (fn [[id readers]]
                        (when (and (seq readers)
                                   (set/subset? readers literal-fields))
                          id)))
                invariant-readers)
          needed-fields
          (into (sorted-set) (mapcat invariant-readers) candidate-ids)
          proofs
          (into (sorted-map)
                (map (fn [field]
                       [field (check-assignment! transition-source fingerprint
                                                 (:subject-fingerprint description)
                                                 field (get literal-writes field))]))
                needed-fields)
          outcomes
          (mapv (fn [[id readers]]
                  (let [candidate? (contains? candidate-ids id)
                        bindings (select-keys literal-writes readers)
                        true-for-all?
                        (and candidate?
                             (check-evaluation! id
                                                (get-in source [:invariants id])
                                                (get declared-invariants id)
                                                state-types bindings))
                        unsupported-readers (set/difference readers literal-fields)]
                    {:obligation-id [:preservation id]
                     :invariant id
                     :readers readers
                     :unsupported-readers unsupported-readers
                     :literal-postcondition (cond
                                              (not candidate?) :not-checked
                                              true-for-all? :true
                                              :else :false)
                     :inference (if true-for-all?
                                  :closed-typed-literal-postcondition
                                  :not-established-by-literal-assignment)
                     :assignment-derivations
                     (if true-for-all?
                       (select-keys proofs readers)
                       {})
                     :status (if true-for-all?
                               :checked-conditional-inference
                               :open)}))
                invariant-readers)
          established-ids
          (into #{} (map :obligation-id)
                (filter #(= :checked-conditional-inference (:status %)) outcomes))
          open-ids
          (into #{} (map :obligation-id)
                (filter #(= :open (:status %)) outcomes))
          expected-ids (into #{} (map :id) preservation-obligations)]
      (when-not (and (= expected-ids (set/union established-ids open-ids))
                     (empty? (set/intersection established-ids open-ids)))
        (fail! :preservation-obligation-coverage-loss {}))
      (let [identity
            (subject/describe-subject
             {:kind :rule :id identity-id
              :interface
              {:version assignment-preservation-version
               :obligation-fingerprint (:fingerprint obligations)
               :transition-fingerprint fingerprint
               :inference :closed-typed-literal-postcondition
               :outcomes (mapv #(select-keys % [:obligation-id :readers
                                                :unsupported-readers
                                                :literal-postcondition :status])
                               outcomes)
               :derivation-fingerprints
               (into (sorted-map)
                     (map (fn [[id proof]]
                            [id (:derivation-fingerprint proof)]))
                     proofs)}})]
        {:type report-type
         :version assignment-preservation-version
         :fingerprint (:fingerprint identity)
         :obligation-fingerprint (:fingerprint obligations)
         :transition-fingerprint fingerprint
         :subject-fingerprint (:subject-fingerprint description)
         :proof-rule :closed-typed-literal-postcondition
         :scope :declared-abstract-state-projection-only
         :claim :all-pre-invariants-and-matching-transition-imply-post-invariant
         :assignment-derivations proofs
         :preservation-outcomes outcomes
         :conditionally-checked-preservation-ids established-ids
         :unsupported-preservation-ids open-ids
         :original-open-obligation-ids (:open-obligation-ids obligations)
         :checking :original-sources-independent-assignment-derivations-and-typed-evaluation
         :proof-status :restricted-conditional-preservation-checked
         :checker-soundness :not-independently-proved
         :open-meta-obligations
         #{:proof-checker-and-literal-substitution-rule-soundness
           :certificate-admission}
         :certificate :not-issued
         :admission :not-admitted
         :runtime-enforcement :not-established
         :implementation-correspondence :not-established
         :authority :none}))))
