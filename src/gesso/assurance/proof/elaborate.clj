(ns gesso.assurance.proof.elaborate
  "Source-derived, proof-producing elaboration for the FIRST closed fragment of
   Gesso's abstract guarded replacements (v1243).

   Input is ONLY the exact v1241 source map:
     {:construction <original v1239 source>
      :initial-condition <original typed predicate>
      :invariants {qualified-id <original typed predicate> ...}}

   The v1241 obligation generator and v1239/v1233 transition constructors
   recheck these original sources. For EVERY declared state field this compiler
   generates the matching assignment or frame inference, plus one guard
   inference, and submits each derivation to the independent v1235 checker.
   Small batches also receive a checked conjunction derivation. Batching is
   bounded by the checker's maximum conjunction arity, and is never used to
   assert that the whole state projection was proved correct.

   :same-field for a frame means BOTH presence and value are preserved, even
   when that field is optional. An assignment establishes the exact abstract
   post-state equality, not an effect performed against XTDB. The guard fact
   is conditional on a MATCHING abstract transition; it is NOT a proof that
   an arbitrary attempted command passes its guard.

   THIS IS NOT AN INVARIANT-PRESERVATION PROVER. The v1241 initial-state,
   preservation, assumption, and physical-implementation obligations are all
   retained OPEN. The existing v1235 derivation language has no general
   implication, arithmetic, or predicate-rewriting rules. Fabricating such
   steps here would be unsound; later revisions must introduce checked rules
   and independently establish the appropriate premises.

   Generated proof sources and checked reports are COPYABLE DATA, not
   certificates, durable credentials, proof of kernel meta-soundness,
   authorization, or production correspondence. Every downstream consumer
   must recheck ORIGINAL sources and derivations at its own trust boundary."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def elaboration-version 1)
(def report-type :gesso.assurance.proof.elaborate/unadmitted-conditional-elaboration)
(def ^:private identity-id :gesso.assurance.proof/transition-elaboration-v1)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private known-derivation-obligations
  #{:certificate-admission
    :production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement})
(def ^:private known-obligation-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private max-conjuncts 128)

(defn- reject!
  [reason details]
  (throw (ex-info "Invalid Gesso proof elaboration."
                  (merge {:error/type ::invalid-elaboration
                          :reason reason} details))))

(defn- step-id [i]
  (keyword "gesso.assurance.proof.elaborate" (str "fact-" i)))

(defn- conjunction-id []
  :gesso.assurance.proof.elaborate/conjunction)

(defn- assignment-claim
  "v1233 has already checked/normalized the replacement. v1235 remains
   authoritative about whether our proposed claim follows from it."
  [semantics field]
  (let [[operation value] (get (:assignments semantics) field)
        rhs (case operation
              :literal [:literal value]
              :read (cond
                      (contains? (:state-types semantics) value) [:pre value]
                      (contains? (:input-types semantics) value) [:input value]
                      :else (reject! :unknown-checked-assignment-reader
                                     {:field field :reader value}))
              (reject! :unknown-checked-assignment-kind
                       {:field field :operation operation}))]
    [:eq [:post field] rhs]))

(defn- proposed-facts
  "Deterministic, COMPLETE enumeration of the primitives supported by the
   current small checker. A framed optional field uses :same-field, not a
   partially defined [:pre field] equality."
  [semantics]
  (let [write-fields (sort (:writes semantics))
        frame-fields (sort (:frame-fields semantics))
        candidates (concat
                    [{:kind :guard :goal [:guard]}]
                    (map (fn [field]
                           {:kind :assignment :field field
                            :goal (assignment-claim semantics field)})
                         write-fields)
                    (map (fn [field]
                           {:kind :frame :field field
                            :goal [:same-field field]})
                         frame-fields))]
    (mapv (fn [i {:keys [kind field goal] :as candidate}]
            (assoc candidate :step
                   (cond-> {:id (step-id i) :rule kind :conclusion goal}
                     field (assoc :field field))))
          (range) candidates)))

(defn- check-proof!
  "The generator never issues its own positive verdict: all proofs MUST
   independently pass the existing small transition-derivation checker.
   Fail closed if that checker's open-premise contract changes."
  [transition-fingerprint proof-source]
  (let [checked (derivation/check-transition-derivation! proof-source)]
    (when-not (and (= (:transition-fingerprint checked) transition-fingerprint)
                   (= (:goal checked) (:goal proof-source))
                   (= #{relation-premise} (:open-premises checked))
                   (= known-derivation-obligations
                      (:missing-obligations checked))
                   (= :matching-abstract-transition-only (:claim-scope checked))
                   (= :conditional-abstract-inference (:proof-status checked))
                   (= :not-admitted (:admission checked))
                   (= :none (:authority checked)))
      (reject! :derivation-contract-changed
               {:expected-transition transition-fingerprint
                :report checked}))
    checked))

(defn- checked-fact
  [transition transition-fingerprint candidate]
  (let [source {:transition transition
                :steps [(:step candidate)]
                :goal (:goal candidate)}
        checked (check-proof! transition-fingerprint source)]
    {:kind (:kind candidate)
     :field (:field candidate)
     :goal (:goal candidate)
     :proof-source source
     :checked-derivation checked
     :scope :matching-abstract-transition-only}))

(defn- checked-batch
  "Emit only conjuncts and a conjunction rule already supported by v1235.
   A singleton still receives its ordinary checked direct proof."
  [transition transition-fingerprint facts]
  (let [steps (mapv :step facts)
        goal (if (= 1 (count facts))
               (:goal (first facts))
               (into [:and] (map :goal facts)))
        proof-source {:transition transition
                      :steps (if (= 1 (count steps))
                               steps
                               (conj steps
                                     {:id (conjunction-id)
                                      :rule :conjunction-introduction
                                      :from (mapv :id steps)
                                      :conclusion goal}))
                      :goal goal}]
    {:fact-ids (mapv (comp :id :step) facts)
     :goal goal
     :proof-source proof-source
     :checked-derivation
     (check-proof! transition-fingerprint proof-source)
     :scope :matching-abstract-transition-only}))

(defn- validate-footprint!
  [checked-construction semantics facts obligation-report]
  (let [writes (set (keep :field (filter #(= :assignment (:kind %)) facts)))
        frames (set (keep :field (filter #(= :frame (:kind %)) facts)))
        guards (count (filter #(= :guard (:kind %)) facts))
        obligation-classes (set (keys (:obligations obligation-report)))
        listed-ids (set (map :id (mapcat val (:obligations obligation-report))))]
    (when-not (and (= 1 guards)
                   (= writes (:writes semantics) (:writes checked-construction))
                   (= frames (:frame-fields semantics) (:frame checked-construction))
                   (= (set/union writes frames)
                      (set (keys (:state-types semantics))))
                   (empty? (set/intersection writes frames)))
      (reject! :incomplete-primitive-coverage
               {:expected-writes (:writes semantics)
                :actual-writes writes
                :expected-frame (:frame-fields semantics)
                :actual-frame frames}))
    (when-not (and (= obligation-classes known-obligation-categories)
                   (= listed-ids (:open-obligation-ids obligation-report))
                   (every? (fn [item] (= :open (:status item)))
                           (mapcat val (:obligations obligation-report))))
      (reject! :obligation-contract-changed
               {:categories obligation-classes}))
    {:guard-count guards
     :assignment-fields writes
     :frame-fields frames
     :state-fields (set (keys (:state-types semantics)))}))

(defn elaborate-transition!
  "Generate and CHECK proof terms for every primitive fact of an admitted-
   SYNTAX-ONLY guarded replacement. The input is the full original v1241
   obligation SOURCE; a derived report, a proof flag, or a caller-provided
   certificate is not accepted.

   Returned fact and batch derivations are conditional on the exact abstract
   transition relation. The entire original v1241 obligation set remains
   open. This function neither proves inductive invariants nor issues
   certified evidence of any physical operation."
  [source]
  (let [obligation-report (obligation/derive-obligations! source)
        built (construction/build-guarded-replacement! (:construction source))
        transition (:transition built)
        description (abstract-transition/describe-transition! transition)
        fingerprint (:fingerprint description)
        _ (when-not (and (= fingerprint (:transition-fingerprint built))
                         (= fingerprint (:transition-fingerprint obligation-report))
                         (= (:subject-fingerprint description)
                            (:subject-fingerprint built)
                            (:subject-fingerprint obligation-report)))
            (reject! :source-transition-identity-mismatch {}))
        candidates (proposed-facts (:semantics description))
        coverage (validate-footprint! built (:semantics description)
                                      candidates obligation-report)
        facts (mapv #(checked-fact transition fingerprint %) candidates)
        batches (mapv #(checked-batch transition fingerprint %)
                      (partition-all max-conjuncts candidates))
        checked-batch-ids (into #{} (mapcat :fact-ids batches))
        candidate-ids (set (map (comp :id :step) candidates))
        _ (when-not (= checked-batch-ids candidate-ids)
            (reject! :missing-checked-batch-facts
                     {:missing (set/difference candidate-ids checked-batch-ids)}))
        all-open (:open-obligation-ids obligation-report)
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:version elaboration-version
                               :obligation-fingerprint (:fingerprint obligation-report)
                               :transition-fingerprint fingerprint
                               ;; Bind compact proof STEPS to the checked
                               ;; source identity. Do not duplicate the entire
                               ;; transition in every fact: doing so makes
                               ;; identity size quadratic in state fields.
                               :fact-steps (mapv :step candidates)
                               :batch-steps (mapv #(get-in % [:proof-source :steps]) batches)
                               :batch-goals (mapv :goal batches)
                               :open-obligation-ids all-open}})]
    {:type report-type
     :version elaboration-version
     :source source
     :fingerprint (:fingerprint identity)
     :obligation-fingerprint (:fingerprint obligation-report)
     :subject-fingerprint (:subject-fingerprint built)
     :transition-fingerprint fingerprint
     :primitive-coverage coverage
     :fact-proofs facts
     :batch-proofs batches
     :open-obligation-ids all-open
     :open-obligations (:obligations obligation-report)
     :checked-fact-count (count facts)
     :checking :independently-checked-abstract-derivations
     :claim-scope :matching-abstract-transition-only
     :proof-status :conditional-abstract-inference-only
     :invariant-preservation :not-established
     :initial-state-invariant :not-established
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
