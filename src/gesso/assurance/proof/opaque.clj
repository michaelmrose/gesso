(ns gesso.assurance.proof.opaque
  "Source-bound opaque-result gating for abstract guarded replacements (v1247).

   Original source (DATA only):
     {:opaque-contract <original v1227 opaque contract>
      :construction   <original v1239 guarded replacement>
      :result-writes  {:entity/status :result/status
                       :entity/score  :result/score}}

   The opaque contract's OUTPUT readers are EXACTLY the abstract transition's
   input readers, and its normalized output predicate is EXACTLY the transition
   guard. Every write must be a direct, typed read of one output reader. Every
   output reader must contribute to a write. The v1239 constructor enforces the
   entire explicit frame; v1233 remains the authority for transition meaning.

   For a matching abstract relation, v1235 independently checks the guard,
   every assignment equality, and the complete frame. These are CONDITIONAL
   facts, NOT a universal invariant-preservation theorem. Neither a callback
   nor a service is executed; even a well-typed, predicate-satisfying output
   could be the WRONG business answer. We cannot prove that a real callback is
   pure, terminating, correctly implemented, or coupled to an atomic commit.

   check-opaque-witness! checks caller-supplied input, output, pre-state and
   post-state maps. It rejects invalid outputs BEFORE accepting the abstract
   witness. Passing is a local, interpreted observation: NOT a certificate,
   transaction admission, production guard, or authorization to commit.

   The stronger theorem that transfers arbitrary output predicates through
   assignment equalities into post-state invariants is NOT a rule in v1235;
   it remains an explicit open obligation rather than a fabricated proof."
  (:require [clojure.set :as set]
            [gesso.assurance.opaque :as opaque-contract]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-normalize :as normalize]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def opaque-proof-version 1)
(def description-type :gesso.assurance.proof.opaque/unadmitted-guarded-opaque-result)
(def observation-type :gesso.assurance.proof.opaque/checked-abstract-opaque-witness)
(def ^:private identity-id :gesso.assurance.proof/guarded-opaque-v1)
(def ^:private source-keys #{:opaque-contract :construction :result-writes})
(def ^:private witness-keys #{:opaque-input :result :before :after})
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private derivation-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})
(def ^:private extra-obligations
  #{:opaque/declared-output-contract-completeness
    :opaque/output-to-postcondition-transfer
    :opaque/actual-result-provenance
    :opaque/production-guard-enforcement
    :opaque/actual-effect-containment
    :opaque/physical-transaction-correspondence})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid guarded opaque-result construction or witness."
                  (merge {:error/type ::invalid-opaque-proof
                          :reason reason :at at} details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value expected at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= expected actual)
      (reject! :wrong-keys at {:missing (set/difference expected actual)
                               :unexpected (set/difference actual expected)})))
  value)

(defn- qualified-id? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- validate-result-writes! [result-writes result-fields state-fields writes]
  (when-not (plain-map? result-writes)
    (reject! :expected-result-writes-map [:result-writes] {}))
  (metadata-free! result-writes [:result-writes])
  (when-not (and (seq result-writes)
                 (every? qualified-id? (keys result-writes))
                 (every? qualified-id? (vals result-writes)))
    (reject! :invalid-result-writes [:result-writes] {}))
  (let [targets (set (keys result-writes))
        sources (set (vals result-writes))]
    (when-not (= targets writes)
      (reject! :result-write-footprint-mismatch [:result-writes]
               {:missing (set/difference writes targets)
                :unexpected (set/difference targets writes)}))
    (when-not (set/subset? targets state-fields)
      (reject! :result-write-outside-state [:result-writes]
               {:unexpected (set/difference targets state-fields)}))
    (when-not (= sources result-fields)
      (reject! :result-reader-coverage-mismatch [:result-writes]
               {:missing (set/difference result-fields sources)
                :unexpected (set/difference sources result-fields)})))
  result-writes)

(defn- checked-fact! [transition fingerprint rule field goal]
  (let [step (cond-> {:id :gesso.assurance.proof.opaque/step
                      :rule rule :conclusion goal}
               field (assoc :field field))
        proof-source {:transition transition :steps [step] :goal goal}
        checked (derivation/check-transition-derivation! proof-source)]
    (when-not (and (= derivation/report-type (:type checked))
                   (= derivation/derivation-version (:version checked))
                   (= fingerprint (:transition-fingerprint checked))
                   (= goal (:goal checked))
                   (= :checked-abstract-transition-derivation
                      (:evidence-class checked))
                   (= :abstract-transition-derivation-checked
                      (:checking checked))
                   (= :conditional-on-matching-abstract-relation
                      (:conclusion-status checked))
                   (= #{relation-premise} (:open-premises checked))
                   (= derivation-obligations (:missing-obligations checked))
                   (= :matching-abstract-transition-only (:claim-scope checked))
                   (= :conditional-abstract-inference (:proof-status checked))
                   (= :not-performed (:assumption-discharge checked))
                   (= :not-issued (:certificate checked))
                   (= :not-admitted (:admission checked))
                   (= :not-established (:runtime-enforcement checked))
                   (= :not-established (:implementation-correspondence checked))
                   (not (contains? checked :valid?))
                   (not (contains? checked :proved?))
                   (= :none (:authority checked)))
      (reject! :derivation-checker-contract-changed [:proof rule field] {}))
    ;; Only retain a compact, COPYABLE description. The checker must be run
    ;; again from source at certificate/admission time.
    {:rule rule :field field :conclusion goal
     :derivation-fingerprint (:fingerprint checked)
     :scope :matching-abstract-transition-only}))

(defn- assignment-goal [semantics state-field result-field]
  ;; The original v1233 semantics MUST contain the exact direct read. This
  ;; separate construction check is not an alternative assignment interpreter.
  (when-not (= [:read result-field]
               (get (:assignments semantics) state-field))
    (reject! :result-assignment-mismatch [:construction :replacements state-field]
             {:state-field state-field :result-field result-field}))
  [:eq [:post state-field] [:input result-field]])

(defn- checked-source! [source]
  (exact-map! source source-keys [])
  (let [{:keys [opaque-contract construction result-writes]} source
        opaque-description (opaque-contract/describe-contract opaque-contract)
        built (construction/build-guarded-replacement! construction)
        transition (:transition built)
        described (abstract/describe-transition! transition)
        semantics (:semantics described)
        normalized-output
        (normalize/normalize-rule! (get-in opaque-contract [:output :rule])
                                   (get-in opaque-contract [:output :reader-types]))
        result-types (get-in normalized-output [:semantics :reader-types])
        result-fields (set (keys result-types))
        state-fields (set (keys (:state-types semantics)))]
    ;; Keep callback identity tied to the exact operation subject. This is a
    ;; naming/structural equality, NOT evidence about deployed code provenance.
    (when-not (and (= :operation (get-in construction [:subject :kind]))
                   (= (:id opaque-contract)
                      (get-in construction [:subject :id])))
      (reject! :operation-subject-mismatch [:construction :subject] {}))
    (when-not (= result-types (:input-types semantics))
      (reject! :result-input-type-mismatch [:construction :input-types] {}))
    (when-not (= (:semantics normalized-output) (:guard semantics))
      (reject! :opaque-output-guard-mismatch [:construction :guard] {}))
    (validate-result-writes! result-writes result-fields state-fields
                             (:writes semantics))
    (doseq [[state-field result-field] result-writes]
      (assignment-goal semantics state-field result-field))
    (let [facts (into [(checked-fact! transition (:fingerprint described)
                                     :guard nil [:guard])]
                      (concat
                       (map (fn [[target reader]]
                              (checked-fact! transition (:fingerprint described)
                                             :assignment target
                                             (assignment-goal semantics target reader)))
                            (sort-by key result-writes))
                       (map (fn [field]
                              (checked-fact! transition (:fingerprint described)
                                             :frame field [:same-field field]))
                            (sort (:frame-fields semantics)))))
          all-fields (set/union (:writes semantics) (:frame-fields semantics))]
      (when-not (= all-fields state-fields)
        (reject! :incomplete-frame-coverage [:construction] {}))
      {:opaque-description opaque-description
       :transition-description described
       :checked-construction built
       :facts facts
       :output-normalization normalized-output})))

(defn describe-opaque-transition!
  "Recheck ORIGINAL opaque contract and guarded replacement source. Derive
   only conditional guard/assignment/frame facts through v1235, and enumerate
   every open promise about opaque correctness, effect isolation, predicate
   transfer and physical enforcement. The returned map is NOT an admitted
   certificate or executable capability."
  [source]
  (let [{:keys [opaque-description transition-description
                checked-construction facts output-normalization]}
        (checked-source! source)
        obligations (set/union (:open-obligations opaque-description)
                               (:open-obligations checked-construction)
                               derivation-obligations extra-obligations)
        identity (subject/describe-subject
                  {:kind :rule :id identity-id
                   :interface {:version opaque-proof-version
                               :opaque-fingerprint (:fingerprint opaque-description)
                               :output-rule-fingerprint (:fingerprint output-normalization)
                               :transition-fingerprint (:fingerprint transition-description)
                               :result-writes (:result-writes source)
                               :checked-fact-identities
                               (mapv :derivation-fingerprint facts)
                               :open-obligations obligations}})]
    {:type description-type
     :version opaque-proof-version
     :source source
     :fingerprint (:fingerprint identity)
     :opaque-fingerprint (:fingerprint opaque-description)
     :transition-fingerprint (:fingerprint transition-description)
     :subject-fingerprint (:subject-fingerprint transition-description)
     :result-writes (:result-writes source)
     :checked-abstract-facts facts
     :open-premises #{relation-premise}
     :open-obligations obligations
     :checking :independently-checked-conditional-abstract-facts
     :opaque-output-semantics :exact-transition-guard
     :result-admission :requires-fresh-supplied-witness-check
     :postcondition-transfer :not-proved
     :invariant-preservation :not-proved
     :opaque-business-correctness :not-proved
     :effect-assurance :not-established
     :termination-assurance :not-established
     :proof-status :conditional-abstract-inference-only
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))

(defn check-opaque-witness!
  "Check one externally supplied opaque input/result and abstract pre/post
   witness. Bad results or nonmatching guarded replacements are rejected;
   acceptable-but-business-wrong outputs may pass, and no opaque computation
   is executed. No XTDB, callback, source-provenance, or commit claim follows."
  [source witness]
  (exact-map! witness witness-keys [:witness])
  ;; Always rederive against SOURCE. A cached description, fabricated proof,
  ;; or stale identity is not accepted as a witness or authorization.
  (let [description (describe-opaque-transition! source)
        contract (:opaque-contract source)
        _ (opaque-contract/validate-input! contract (:opaque-input witness))
        _ (opaque-contract/validate-result! contract (:result witness))
        checked (abstract/observe-transition!
                 (:transition (construction/build-guarded-replacement!
                               (:construction source)))
                 (:before witness) (:result witness) (:after witness))]
    (when-not (= (:transition-fingerprint description)
                 (:transition-fingerprint checked))
      (reject! :stale-transition-witness [:witness] {}))
    (when-not (true? (:matches? checked))
      (reject! :nonmatching-abstract-transition [:witness]
               {:guard-holds? (:guard-holds? checked)
                :assignments-hold? (:assignments-hold? checked)
                :frame-holds? (:frame-holds? checked)}))
    {:type observation-type
     :version opaque-proof-version
     :construction-fingerprint (:fingerprint description)
     :opaque-fingerprint (:opaque-fingerprint description)
     :transition-fingerprint (:transition-fingerprint description)
     :subject-fingerprint (:subject-fingerprint description)
     :observation :opaque-predicates-and-abstract-relation-matched
     :guard-gate :passed-for-supplied-witness
     :open-premises (:open-premises description)
     :open-obligations (:open-obligations description)
     :invariant-preservation :not-proved
     :opaque-business-correctness :not-proved
     :proof-status :interpreted-witness-only
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))
