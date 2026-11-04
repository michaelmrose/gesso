(ns gesso.assurance.proof.preservation
  "A narrow, source-checked invariant-preservation inference (v1259).

   Input is ONLY the original v1241 obligation source:
     {:construction <v1239 source>
      :initial-condition <typed state predicate>
      :invariants {qualified-id <typed state predicate> ...}}

   The inference is FRAME EXTENSIONALITY. For a pure, deterministic v1219
   predicate I whose entire reader set is unchanged in both value AND presence
   across an abstract v1233 transition, I(before) = I(after). Therefore
      ALL-I(before) AND T(before,input,after) => I(after)
   for each invariant whose readers are a subset of T's exact frame fields.

   The v1241 generator rechecks each source invariant and the v1239/v1233
   transition. For every participating frame field we additionally generate
   an exact :same-field claim and run it through the independent v1235
   derivation checker. This rule also applies to predicates with no readers,
   because their closed expressions are constant across the two states.

   This is the first deliberately RESTRICTED universal abstract preservation
   inference, NOT a solver for invariants touching written fields, a general
   predicate-congruence proof kernel, a proof of the checker rules' soundness,
   or production correspondence. Source completeness, initial states, runtime
   effects, and certification all remain separate OPEN obligations. All data
   returned here are copyable, unadmitted observations; downstream callers
   MUST recheck original sources, not trust this report or its fingerprint."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def preservation-version 1)
(def report-type :gesso.assurance.proof.preservation/unadmitted-frame-preservation)

(def ^:private identity-id :gesso.assurance.proof/frame-preservation-v1)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private known-checker-obligations
  #{:certificate-admission
    :production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement})
(def ^:private expected-obligation-categories
  #{:initial-state :preservation :assumption :implementation})

(defn- fail!
  [reason details]
  (throw (ex-info "Invalid Gesso abstract preservation inference."
                  (merge {:error/type ::invalid-preservation
                          :reason reason}
                         details))))

(defn- frame-source
  [transition field]
  (let [claim [:same-field field]]
    {:transition transition
     :steps [{:id :gesso.assurance.proof.preservation/frame
              :rule :frame
              :field field
              :conclusion claim}]
     :goal claim}))

(defn- check-frame!
  "Recheck the original transition and one proposed frame fact through v1235.
   Exact checker response requirements fail closed on upstream contract drift."
  [transition fingerprint field]
  (let [proof (frame-source transition field)
        checked (derivation/check-transition-derivation! proof)]
    (when-not (and (= derivation/report-type (:type checked))
                   (= fingerprint (:transition-fingerprint checked))
                   (= (:goal proof) (:goal checked))
                   (= 1 (:rule-count checked))
                   (re-matches #"sha256:[0-9a-f]{64}"
                               (:fingerprint checked))
                   (= #{relation-premise} (:open-premises checked))
                   (= known-checker-obligations (:missing-obligations checked))
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
     :claim (:goal proof)
     :derivation-fingerprint (:fingerprint checked)
     :scope :matching-abstract-transition-only}))

(defn check-frame-preservation!
  "Check a SOURCE-DERIVED, conditional frame-preservation inference.

   An invariant is supported iff all its normalized typed readers are exact
   frame fields of the original guarded replacement. Unsupported invariants
   are explicitly left open, with their written readers named. This checker
   does not try to infer that a write preserves a value, or appeal to typed
   arithmetic, guard implication, an arbitrary predicate, or an external
   certificate. All v1241 obligations remain present and OPEN for certification
   purposes; this report supplies candidate checked inference evidence only."
  [source]
  (let [obligations (obligation/derive-obligations! source)
        transition-source
        (:transition (construction/build-guarded-replacement!
                      (:construction source)))
        description (abstract-transition/describe-transition! transition-source)
        fingerprint (:fingerprint description)
        semantics (:semantics description)
        frame (:frame-fields semantics)
        declared-invariants (:invariants obligations)
        preservation-obligations (get-in obligations [:obligations :preservation])]
    (when-not (and (= fingerprint (:transition-fingerprint obligations))
                   (= expected-obligation-categories
                      (set (keys (:obligations obligations))))
                   (= (set (keys declared-invariants))
                      (set (map :invariant preservation-obligations)))
                   (= (count declared-invariants) (count preservation-obligations))
                   (every? #(= :open (:status %)) preservation-obligations))
      (fail! :obligation-source-contract-changed {}))
    (let [invariant-readers
          (into (sorted-map)
                (map (fn [[id typed-rule]]
                       [id (:readers typed-rule)]))
                declared-invariants)
          eligible (into (sorted-set)
                         (keep (fn [[id readers]]
                                 (when (set/subset? readers frame) id)))
                         invariant-readers)
          needed-fields
          (into (sorted-set)
                (mapcat #(get invariant-readers %))
                eligible)
          checked-fields
          (into (sorted-map)
                (map (fn [field]
                       [field (check-frame! transition-source fingerprint field)]))
                needed-fields)
          outcomes
          (mapv (fn [[id readers]]
                  (let [affected (set/difference readers frame)
                        checked? (contains? eligible id)
                        own-id [:preservation id]]
                    {:obligation-id own-id
                     :invariant id
                     :readers readers
                     :written-readers affected
                     :inference (if checked?
                                  :pure-typed-rule-frame-extensionality
                                  :unsupported-written-field-preservation)
                     :field-derivations
                     (if checked?
                       (select-keys checked-fields readers)
                       {})
                     :status (if checked?
                               :checked-conditional-inference
                               :open)}))
                invariant-readers)
          supported-ids (into #{} (map :obligation-id)
                              (filter #(= :checked-conditional-inference
                                          (:status %)) outcomes))
          unsupported-ids (into #{} (map :obligation-id)
                                (filter #(= :open (:status %)) outcomes))
          all-ids (into #{} (map :id) preservation-obligations)
          _ (when-not (= all-ids (set/union supported-ids unsupported-ids))
              (fail! :preservation-obligation-coverage-loss {}))
          identity (subject/describe-subject
                    {:kind :rule :id identity-id
                     :interface {:version preservation-version
                                 :obligation-fingerprint (:fingerprint obligations)
                                 :transition-fingerprint fingerprint
                                 :inference :pure-typed-rule-frame-extensionality
                                 :outcomes
                                 (mapv #(select-keys % [:obligation-id :readers
                                                        :written-readers :status])
                                       outcomes)
                                 :field-proof-fingerprints
                                 (into (sorted-map)
                                       (map (fn [[id proof]]
                                              [id (:derivation-fingerprint proof)]))
                                       checked-fields)}})]
      {:type report-type
       :version preservation-version
       :fingerprint (:fingerprint identity)
       :obligation-fingerprint (:fingerprint obligations)
       :transition-fingerprint fingerprint
       :subject-fingerprint (:subject-fingerprint description)
       :proof-rule :pure-typed-rule-frame-extensionality
       :scope :declared-abstract-state-projection-only
       :claim :all-pre-invariants-and-matching-transition-imply-post-invariant
       :frame-derivations checked-fields
       :preservation-outcomes outcomes
       :conditionally-checked-preservation-ids supported-ids
       :unsupported-preservation-ids unsupported-ids
       :original-open-obligation-ids (:open-obligation-ids obligations)
       :checking :original-sources-and-independent-frame-derivations
       :proof-status :restricted-conditional-preservation-checked
       :checker-soundness :not-independently-proved
       :open-meta-obligations
       #{:proof-checker-and-extensionality-rule-soundness
         :certificate-admission}
       :certificate :not-issued
       :admission :not-admitted
       :runtime-enforcement :not-established
       :implementation-correspondence :not-established
       :authority :none})))
