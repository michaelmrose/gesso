(ns gesso.assurance.proof.compose
  "Conditional, source-derived composition of TWO abstract Gesso operations.

   A composition is original Gesso data, not a certificate or transaction:

     {:mode :sequential ;; or :atomic (a REQUEST, not established atomicity)
      :subject {:kind :choreography :id :example/claim-then-complete
                :interface {:resource :request/entity
                            :steps [:example/claim :example/complete]}}
      :resource :request/entity
      :assumptions #{:example/shared-resource-binding}
      :steps [{:resource :request/entity
               :assumptions #{:example/shared-resource-binding}
               :source <original v1241 obligation source for claim>}
              {:resource :request/entity
               :assumptions #{:example/shared-resource-binding}
               :source <original v1241 obligation source for complete>}]}

   Each component is re-elaborated through v1243, and each generated primitive
   and batch proof is rechecked against the ORIGINAL v1233 transition by the
   independent v1235 derivation checker. The two state projections must have
   exactly the SAME normalized types and the SAME declared invariant meanings.
   This checks structural compatibility; it does not show that the first step
   enables the second guard, nor that the second step's declared initial rule
   applies to intermediate states.

   The abstract composition RELATION is DEFINED by an existential intermediate
   state m with T1(s0,x1,m) and T2(m,x2,s2). No new inference rule or universal
   theorem is introduced here. check-composed-witness! observes one pair of
   matching abstract transitions, with EXACT intermediate state equality,
   including optional presence. An observed witness is not proof that EVERY
   first step may be followed by a second, nor a proof of any invariant.

   Both modes leave resource aliasing, assumptions, real execution order,
   runtime authorization, physical effect containment and XTDB correspondence
   OPEN. In :atomic mode, single-commit enforcement and intermediate-state
   invisibility are also OPEN, never derived from the word ':atomic'. A
   structural digest is only a source-change detector. This namespace issues
   no certificate, admission, or transaction authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.elaborate :as elaborate]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def composition-version 1)
(def report-type :gesso.assurance.proof.compose/unadmitted-conditional-composition)
(def witness-type :gesso.assurance.proof.compose/abstract-composition-witness)
(def ^:private identity-id :gesso.assurance.proof/transition-composition-v1)
(def ^:private input-keys #{:mode :subject :resource :assumptions :steps})
(def ^:private step-keys #{:resource :assumptions :source})
(def ^:private witness-keys #{:first :second})
(def ^:private observation-keys #{:before :inputs :after})
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private derivation-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})
(def ^:private composition-open
  #{[:composition :resource-aliasing]
    [:composition :assumption-validity]
    [:composition :intermediate-state-refinement]
    [:composition :second-guard-enablement]
    [:composition :composed-invariant-preservation]
    [:composition :production-order-correspondence]})
(def ^:private atomic-open
  #{[:composition :single-atomic-commit]
    [:composition :intermediate-state-invisibility]})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso abstract transition composition."
                  (merge {:error/type ::invalid-composition
                          :reason reason :at at} details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [x required at]
  (when-not (plain-map? x)
    (reject! :expected-plain-map at {}))
  (metadata-free! x at)
  (let [actual (set (keys x))]
    (when-not (= required actual)
      (reject! :wrong-keys at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual required)})))
  x)

(defn- assumptions! [x at]
  (when-not (and (plain-set? x) (<= (count x) 64))
    (reject! :invalid-assumption-set at {}))
  (metadata-free! x at)
  (when-not (every? qualified-id? x)
    (reject! :invalid-assumption-id at {}))
  x)

(defn- checked-proof!
  "Do not trust copied v1243 reports. Rerun the independent derivation checker
   on EACH generated proof source and require its precise conditional verdict."
  [proof transition-fingerprint at]
  (let [checked (derivation/check-transition-derivation! (:proof-source proof))]
    (when-not (and (= checked (:checked-derivation proof))
                   (= transition-fingerprint (:transition-fingerprint checked))
                   (= (:goal proof) (:goal checked))
                   (= #{relation-premise} (:open-premises checked))
                   (= derivation-obligations (:missing-obligations checked))
                   (= :conditional-abstract-inference (:proof-status checked))
                   (= :matching-abstract-transition-only (:claim-scope checked))
                   (= :not-admitted (:admission checked))
                   (= :none (:authority checked)))
      (reject! :independent-proof-contract-changed at {})))
  true)

(defn- checked-component!
  [index step]
  (exact-map! step step-keys [:steps index])
  (when-not (qualified-id? (:resource step))
    (reject! :invalid-step-resource [:steps index :resource] {}))
  (assumptions! (:assumptions step) [:steps index :assumptions])
  (let [source (:source step)
        elaborated (elaborate/elaborate-transition! source)
        obligations (obligation/derive-obligations! source)
        built (construction/build-guarded-replacement! (:construction source))
        transition (:transition built)
        description (abstract/describe-transition! transition)
        transition-fingerprint (:fingerprint description)
        fact-proofs (:fact-proofs elaborated)
        batch-proofs (:batch-proofs elaborated)]
    (when-not (and (= transition-fingerprint
                      (:transition-fingerprint obligations)
                      (:transition-fingerprint elaborated))
                   (= (:subject-fingerprint description)
                      (:subject-fingerprint obligations)
                      (:subject-fingerprint elaborated))
                   (= (:open-obligation-ids obligations)
                      (:open-obligation-ids elaborated))
                   (= :conditional-abstract-inference-only
                      (:proof-status elaborated))
                   (= :not-admitted (:admission elaborated))
                   (= :none (:authority elaborated))
                   (= (count fact-proofs) (:checked-fact-count elaborated))
                   (seq batch-proofs))
      (reject! :component-contract-changed [:steps index :source] {}))
    (doseq [[proof-index proof] (map-indexed vector fact-proofs)]
      (checked-proof! proof transition-fingerprint
                      [:steps index :fact-proofs proof-index]))
    (doseq [[proof-index proof] (map-indexed vector batch-proofs)]
      (checked-proof! proof transition-fingerprint
                      [:steps index :batch-proofs proof-index]))
    {:index index
     :resource (:resource step)
     :assumptions (:assumptions step)
     :operation-subject (get-in transition [:subject])
     :subject-fingerprint (:subject-fingerprint description)
     :transition-fingerprint transition-fingerprint
     :obligation-fingerprint (:fingerprint obligations)
     :elaboration-fingerprint (:fingerprint elaborated)
     :state-types (get-in description [:semantics :state-types])
     :invariants (:invariants obligations)
     :reads (get-in description [:semantics :reads])
     :writes (get-in description [:semantics :writes])
     :frame (get-in description [:semantics :frame-fields])
     :checked-facts (count fact-proofs)
     :open-obligation-ids (:open-obligation-ids obligations)}))

(defn- validate-composition!
  [source components]
  (exact-map! source input-keys [])
  (let [{:keys [mode subject resource assumptions steps]} source
        [first-component second-component] components]
    (when-not (#{:sequential :atomic} mode)
      (reject! :invalid-mode [:mode] {:mode mode}))
    (when-not (qualified-id? resource)
      (reject! :invalid-resource [:resource] {}))
    (assumptions! assumptions [:assumptions])
    (exact-map! subject #{:kind :id :interface} [:subject])
    (when-not (= :choreography (:kind subject))
      (reject! :invalid-composition-subject-kind [:subject :kind] {}))
    (exact-map! (:interface subject) #{:resource :steps}
                [:subject :interface])
    (let [operation-ids (mapv (comp :id :operation-subject) components)]
      (when-not (and (= resource (get-in subject [:interface :resource]))
                     (= operation-ids (get-in subject [:interface :steps])))
        (reject! :composition-subject-mismatch [:subject :interface]
                 {:expected {:resource resource :steps operation-ids}})))
    (subject/describe-subject subject)
    (doseq [component components]
      (let [i (:index component)]
        (when-not (= :operation (get-in component [:operation-subject :kind]))
          (reject! :invalid-step-subject-kind [:steps i :source :construction :subject] {}))
        (when-not (= resource (:resource component))
          (reject! :resource-mismatch [:steps i :resource] {}))
        (when-not (= assumptions (:assumptions component))
          (reject! :assumptions-mismatch [:steps i :assumptions] {}))))
    (when-not (= (:state-types first-component)
                 (:state-types second-component))
      (reject! :intermediate-state-type-mismatch [:steps]
               {:first-fields (set (keys (:state-types first-component)))
                :second-fields (set (keys (:state-types second-component)))}))
    (when-not (= (:invariants first-component)
                 (:invariants second-component))
      (reject! :invariant-meaning-mismatch [:steps] {}))
    ;; The second source's initial-condition is NOT used as an inferred
    ;; intermediate-state predicate. That implication is an OPEN obligation.
    true))

(defn compose-transitions!
  "Recheck TWO original v1241 sources and derive a structurally compatible,
   unadmitted conditional composition. A result describes the relation
   EXISTS m: T1(before, input1, m) AND T2(m, input2, after), NOT a proof of
   guard enablement, invariant preservation, atomicity, or physical execution.

   :mode :atomic requests grouping; it adds rather than closes atomicity
   obligations. The :resource and :assumptions entries are declarative labels,
   not verification of real resource identity or truth of assumptions."
  [source]
  ;; Validate the entire outer shape BEFORE invoking any nested checker.
  (exact-map! source input-keys [])
  (let [steps (:steps source)]
    (when-not (and (plain-vector? steps) (= 2 (count steps)))
      (reject! :expected-two-steps [:steps] {}))
    (metadata-free! steps [:steps])
    (let [components (mapv checked-component! (range 2) steps)
          _ (validate-composition! source components)
          mode (:mode source)
          subject-id (subject/describe-subject (:subject source))
          step-obligations
          (into #{} (mapcat (fn [{:keys [index open-obligation-ids]}]
                             (map (fn [id] [:step index id]) open-obligation-ids)))
                components)
          declared-assumptions
          (into #{} (map (fn [id] [:declared-assumption id]))
                (:assumptions source))
          additional (if (= :atomic mode) atomic-open #{})
          all-open (set/union step-obligations declared-assumptions
                              composition-open additional)
          summaries (mapv #(dissoc % :state-types :invariants
                                    :operation-subject :open-obligation-ids)
                          components)
          fingerprint
          (:fingerprint
           (subject/describe-subject
            {:kind :rule :id identity-id
             :interface {:version composition-version
                         :mode mode
                         :composition-subject (:fingerprint subject-id)
                         :resource (:resource source)
                         :assumptions (:assumptions source)
                         :component-transitions (mapv :transition-fingerprint components)
                         :component-obligations (mapv :obligation-fingerprint components)
                         :component-elaborations (mapv :elaboration-fingerprint components)
                         :open-obligation-ids all-open}}))]
      {:type report-type
       :version composition-version
       :source source
       :mode mode
       :fingerprint fingerprint
       :subject-fingerprint (:fingerprint subject-id)
       :resource (:resource source)
       :declared-assumptions (:assumptions source)
       :components summaries
       :state-projection (set (keys (:state-types (first components))))
       :intermediate-contract
       {:first-post :intermediate-state
        :second-pre :intermediate-state
        :equality :complete-typed-state-map-including-presence
        :type-compatibility :checked
        :guard-enablement :not-established
        :second-initial-condition :not-inferred}
       :relation :exists-intermediate-with-two-matching-original-relations
       :open-obligation-ids all-open
       :open-obligation-count (count all-open)
       :checking :independent-component-derivations-and-structural-compatibility
       :composition-status :structurally-compatible-unadmitted
       :proof-status :conditional-abstract-composition-only
       :invariant-preservation :not-established
       :certificate :not-issued
       :admission :not-admitted
       :atomicity (if (= :atomic mode) :requested-not-established :not-claimed)
       :runtime-enforcement :not-established
       :implementation-correspondence :not-established
       :authority :none})))

(defn check-composed-witness!
  "Check ONE concrete abstract witness of the source-derived two-step
   composition. Inputs are {:first {:before ... :inputs ... :after ...}
   :second {:before ... :inputs ... :after ...}}. The two intermediate maps
   MUST be EXACTLY equal; unequal optional presence is not ignored. Each step
   must independently satisfy the existing v1233 relation. This is bounded
   observation, not an inductive proof, runtime commit, or certificate."
  [source witness]
  (let [composition (compose-transitions! source)]
    (exact-map! witness witness-keys [:witness])
    (doseq [[index label] (map-indexed vector [:first :second])]
      (exact-map! (get witness label) observation-keys [:witness label]))
    ;; Type-check all exposed values through the original abstract interpreter
    ;; BEFORE comparing maps: otherwise caller-defined Java objects could run
    ;; arbitrary .equals methods during the intermediate-state comparison.
    (let [observations
          (mapv (fn [step observation]
                  (let [built (construction/build-guarded-replacement!
                               (get-in step [:source :construction]))]
                    (abstract/observe-transition!
                     (:transition built) (:before observation)
                     (:inputs observation) (:after observation))))
                (:steps source) [(:first witness) (:second witness)])]
      (when-not (= (get-in witness [:first :after])
                   (get-in witness [:second :before]))
        (reject! :intermediate-state-mismatch [:witness]
                 {:first-after (get-in witness [:first :after])
                  :second-before (get-in witness [:second :before])}))
      (doseq [[index observed] (map-indexed vector observations)]
        (when-not (:matches? observed)
          (reject! :nonmatching-abstract-component [:witness index]
                   {:index index
                    :guard-holds? (:guard-holds? observed)
                    :assignments-hold? (:assignments-hold? observed)
                    :frame-holds? (:frame-holds? observed)})))
      {:type witness-type
       :version composition-version
       :composition-fingerprint (:fingerprint composition)
       :component-transition-fingerprints (mapv :transition-fingerprint observations)
       :checking :both-original-abstract-relations-observed
       :intermediate-equality :observed-exact
       :matches? true
       :claim-scope :one-abstract-two-step-witness-only
       :proof-status :not-proved
       :invariant-preservation :not-established
       :certificate :not-issued
       :admission :not-admitted
       :atomicity :not-established
       :runtime-enforcement :not-established
       :implementation-correspondence :not-established
       :authority :none})))
