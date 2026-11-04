(ns gesso.assurance.proof.derivation
  "A SMALL, CLOSED checker for abstract transition inferences (v1235).

   Source input, NOT an alleged checking report:
     {:transition <v1233 original guarded-transition declaration>
      :steps [{:id :step/guard :rule :guard :conclusion [:guard]}
              {:id :step/write :rule :assignment :field :request/status
               :conclusion [:eq [:post :request/status] [:literal :claimed]]}
              {:id :step/both :rule :conjunction-introduction
               :from [:step/guard :step/write]
               :conclusion [:and [:guard]
                            [:eq [:post :request/status] [:literal :claimed]]]}]
      :goal [:and [:guard]
             [:eq [:post :request/status] [:literal :claimed]]]}

   Every conclusion is conditional on the SAME abstract relation matching:
     T(before, input, after) => conclusion(before, input, after).
   :guard means the declaration's exact typed guard holds on the pre-state;
   :same-field means both presence and value are preserved on a frame field.
   Equality terms are [:pre field], [:input field], [:post field], or a typed
   scalar [:literal value]. Optional pre/input fields have no total value term;
   optional post-fields have a total value term ONLY when written (because a
   matching assignment makes them present). No arbitrary numeric or Boolean
   facts are introduced by these term forms.

   The ONLY rules are guard, frame (presence-preserving or required-field
   equality), assignment, conjunction introduction/elimination, and exact
   equality substitution. Substitution replaces ONE WHOLE equality operand
   using an equality proved earlier; it is not a rewrite of opaque predicates,
   a typed-arithmetic theorem, or an implication-introduction rule.

   This module independently rechecks original v1233 source declarations on
   EVERY invocation, verifies a closed acyclic derivation, and reports only
   COPYABLE, UNADMITTED conditional evidence. It does NOT issue certificates,
   discharge the abstract-relation premise, establish an inductive invariant,
   prove callback/XTDB/CLJS correspondence, or authorize a transaction. A later
   certificate boundary must rerun this checker from source and independently
   close its remaining obligations."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.subject :as subject]))

(def derivation-version 1)
(def report-type :gesso.assurance.proof.derivation/unadmitted-checked-transition-derivation)

(def ^:private root-keys #{:transition :steps :goal})
(def ^:private rule-keys
  {:guard #{:id :rule :conclusion}
   :frame #{:id :rule :field :conclusion}
   :assignment #{:id :rule :field :conclusion}
   :conjunction-introduction #{:id :rule :from :conclusion}
   :conjunction-elimination #{:id :rule :from :index :conclusion}
   :substitution #{:id :rule :from :side :direction :conclusion}})
(def ^:private max-steps 256)
(def ^:private max-sources 128)
(def ^:private max-claim-depth 48)
(def ^:private max-total-claim-nodes 32768)
(def ^:private identity-id :gesso.assurance.proof/transition-derivation-v1)
(def ^:private relation-premise :gesso.assurance.proof.derivation/abstract-relation-match)

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso abstract transition derivation."
                  (merge {:error/type ::invalid-derivation
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- plain-vector? [v]
  (instance? clojure.lang.APersistentVector v))

(defn- qualified-keyword? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- metadata-free! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (fail! :metadata-not-allowed at {})))

(defn- exact-keys! [v expected at]
  (when-not (plain-map? v)
    (fail! :expected-plain-map at {}))
  (metadata-free! v at)
  (let [actual (set (keys v))]
    (when-not (= actual expected)
      (fail! :wrong-keys at {:missing (set/difference expected actual)
                             :unexpected (set/difference actual expected)}))))

(defn- scalar? [v]
  (or (nil? v) (boolean? v) (integer? v) (keyword? v)
      (and (string? v) (<= (count v) 2048))))

(defn- total-term!
  "Only total terms may occur in an equality. An optional field is not a
   total term unless the transition writes it into the post-state."
  [term semantics at]
  (when-not (and (plain-vector? term) (= 2 (count term)))
    (fail! :invalid-equality-term at {}))
  (metadata-free! term at)
  (let [[kind operand] term
        {:keys [state-types input-types writes]} semantics
        desc (case kind
               :pre (get state-types operand)
               :post (get state-types operand)
               :input (get input-types operand)
               nil)]
    (if (= kind :literal)
      (when-not (scalar? operand)
        (fail! :invalid-literal at {}))
      (do
        (when-not (and (qualified-keyword? operand) desc)
          (fail! :unknown-equality-reader at {:term term}))
        (when (and (true? (:optional? desc))
                   (not (and (= kind :post) (contains? writes operand))))
          (fail! :partial-equality-reader at {:term term}))))
    term))

(defn- checked-claim!
  [claim semantics at budget depth]
  (when (> depth max-claim-depth)
    (fail! :claim-depth-limit at {:maximum max-claim-depth}))
  (when (> (swap! budget inc) max-total-claim-nodes)
    (fail! :claim-node-limit at {:maximum max-total-claim-nodes}))
  (when-not (and (plain-vector? claim) (seq claim))
    (fail! :expected-claim-vector at {}))
  (metadata-free! claim at)
  (let [kind (first claim)]
    (case kind
      :guard
      (when-not (= claim [:guard])
        (fail! :invalid-guard-claim at {}))

      :same-field
      (do
        (when-not (and (= 2 (count claim))
                       (qualified-keyword? (second claim))
                       (contains? (:state-types semantics) (second claim)))
          (fail! :invalid-frame-claim at {})))

      :eq
      (do
        (when-not (= 3 (count claim))
          (fail! :invalid-equality-arity at {}))
        (total-term! (second claim) semantics (conj at 1))
        (total-term! (nth claim 2) semantics (conj at 2)))

      :and
      (do
        (when-not (<= 3 (count claim) (inc max-sources))
          (fail! :invalid-conjunction-arity at {}))
        (doseq [index (range 1 (count claim))]
          (checked-claim! (nth claim index) semantics (conj at index)
                          budget (inc depth))))

      ;; No arbitrary atoms, implication, negation, arithmetic, predicates,
      ;; assertion flags, or unchecked user-supplied premises exist here.
      (fail! :unknown-claim-kind at {:kind kind})))
  claim)

(defn- source-claims!
  [step prior minimum maximum]
  (let [from (:from step)]
    (when-not (and (plain-vector? from)
                   (<= minimum (count from) maximum))
      (fail! :invalid-sources [:steps (:id step) :from]
             {:minimum minimum :maximum maximum}))
    (metadata-free! from [:steps (:id step) :from])
    (doseq [id from]
      (when-not (qualified-keyword? id)
        (fail! :invalid-source-id [:steps (:id step) :from] {:id id}))
      (when-not (contains? prior id)
        (fail! :unknown-or-forward-step [:steps (:id step) :from] {:id id})))
    (mapv (comp :claim prior) from)))

(defn- expected-assignment-claim
  [field expression semantics]
  (let [[op operand] expression
        rhs (case op
              :literal [:literal operand]
              :read (if (contains? (:state-types semantics) operand)
                      [:pre operand]
                      [:input operand]))]
    [:eq [:post field] rhs]))

(defn- exact-rule!
  [step previous semantics]
  (let [{:keys [id rule conclusion field]} step
        at [:steps id]
        frame-fields (:frame-fields semantics)
        state-types (:state-types semantics)
        writes (:writes semantics)]
    (case rule
      :guard
      (when-not (= [:guard] conclusion)
        (fail! :wrong-guard-conclusion (conj at :conclusion) {}))

      :frame
      (do
        (when-not (and (qualified-keyword? field)
                       (contains? frame-fields field))
          (fail! :not-a-frame-field (conj at :field) {:field field}))
        (let [required? (not (true? (:optional? (get state-types field))))
              structural [:same-field field]
              equality [:eq [:post field] [:pre field]]]
          (when-not (or (= conclusion structural)
                        (and required? (= conclusion equality)))
            (fail! :wrong-frame-conclusion (conj at :conclusion) {}))))

      :assignment
      (do
        (when-not (and (qualified-keyword? field)
                       (contains? writes field))
          (fail! :not-an-assigned-field (conj at :field) {:field field}))
        (when-not (= conclusion
                     (expected-assignment-claim
                      field (get (:assignments semantics) field) semantics))
          (fail! :wrong-assignment-conclusion (conj at :conclusion) {})))

      :conjunction-introduction
      (let [sources (source-claims! step previous 2 max-sources)]
        (when-not (= conclusion (into [:and] sources))
          (fail! :wrong-conjunction-introduction (conj at :conclusion) {})))

      :conjunction-elimination
      (let [[source] (source-claims! step previous 1 1)
            index (:index step)]
        (when-not (= :and (first source))
          (fail! :source-not-conjunction (conj at :from) {}))
        (when-not (and (integer? index) (<= 0 index)
                       (< index (dec (count source))))
          (fail! :invalid-conjunction-index (conj at :index) {:index index}))
        (when-not (= conclusion (nth source (inc index)))
          (fail! :wrong-conjunction-elimination (conj at :conclusion) {})))

      :substitution
      (let [[evidence target] (source-claims! step previous 2 2)
            side (:side step)
            direction (:direction step)]
        (when-not (and (= :eq (first evidence))
                       (= :eq (first target)))
          (fail! :substitution-requires-equalities (conj at :from) {}))
        (when-not (#{:left :right} side)
          (fail! :invalid-substitution-side (conj at :side) {}))
        (when-not (#{:forward :reverse} direction)
          (fail! :invalid-substitution-direction (conj at :direction) {}))
        (let [[from-term to-term] (if (= direction :forward)
                                    [(second evidence) (nth evidence 2)]
                                    [(nth evidence 2) (second evidence)])
              index (if (= side :left) 1 2)]
          (when-not (= from-term (nth target index))
            (fail! :substitution-source-mismatch (conj at :from) {}))
          (when-not (= conclusion (assoc target index to-term))
            (fail! :wrong-substitution-conclusion (conj at :conclusion) {}))))

      (fail! :unknown-rule (conj at :rule) {:rule rule}))))

(defn- reachable-steps
  [records last-id]
  (loop [pending [last-id] visited #{}]
    (if (empty? pending)
      visited
      (let [id (peek pending)
            pending (pop pending)]
        (if (contains? visited id)
          (recur pending visited)
          (recur (into pending (:from (get records id)))
                 (conj visited id)))))))

(defn check-transition-derivation!
  "Recheck original v1233 transition SOURCE and a CLOSED derivation of facts
   true of EVERY matching abstract pre/input/post triple. No premises are
   discharged: the report's claims remain conditional on the exact abstract
   transition relation, not on any implementation's correspondence to it.
   The source must contain all and only :transition, :steps and :goal."
  [source]
  (exact-keys! source root-keys [])
  (let [{:keys [transition steps goal]} source
        description (transition/describe-transition! transition)
        semantics (:semantics description)
        budget (atom 0)]
    (when-not (and (plain-vector? steps)
                   (pos? (count steps))
                   (<= (count steps) max-steps))
      (fail! :invalid-steps [:steps] {:maximum max-steps}))
    (metadata-free! steps [:steps])
    (checked-claim! goal semantics [:goal] budget 0)
    (let [records
          (reduce
           (fn [prior [index step]]
             (let [at [:steps index]]
               (when-not (plain-map? step)
                 (fail! :invalid-step at {}))
               (metadata-free! step at)
               (let [rule (:rule step)
                     expected (get rule-keys rule)]
                 (when-not expected
                   (fail! :unknown-rule (conj at :rule) {:rule rule}))
                 (exact-keys! step expected at)
                 (let [id (:id step)]
                   (when-not (qualified-keyword? id)
                     (fail! :invalid-step-id (conj at :id) {:id id}))
                   (when (contains? prior id)
                     (fail! :duplicate-step-id (conj at :id) {:id id}))
                   (checked-claim! (:conclusion step) semantics
                                   (conj at :conclusion) budget 0)
                   (exact-rule! step prior semantics)
                   (assoc prior id {:claim (:conclusion step)
                                    :from (or (:from step) [])})))))
           {} (map-indexed vector steps))
          last-id (:id (peek steps))
          unused (set/difference (set (keys records))
                                 (reachable-steps records last-id))]
      (when-not (= goal (:claim (get records last-id)))
        (fail! :wrong-final-conclusion [:goal] {}))
      (when (seq unused)
        (fail! :unused-proof-steps [:steps] {:unused unused}))
      (let [proof-semantics
            {:derivation-version derivation-version
             :transition-version transition/transition-version
             :subject-fingerprint (:subject-fingerprint description)
             :transition-fingerprint (:fingerprint description)
             :claim-scope :matching-abstract-transition-only
             :goal goal
             :steps steps}
            identity (subject/describe-subject
                      {:kind :rule :id identity-id :interface proof-semantics})]
        {:type report-type
         :version derivation-version
         :checker-version derivation-version
         :evidence-class :checked-abstract-transition-derivation
         :fingerprint (:fingerprint identity)
         :subject-fingerprint (:subject-fingerprint description)
         :transition-fingerprint (:fingerprint description)
         :goal goal
         :rule-count (count steps)
         :open-premises #{relation-premise}
         :trusted-assumptions #{}
         :missing-obligations
         #{:certificate-admission
           :production-transition-correspondence
           :current-runtime-authorization
           :atomic-transaction-enforcement}
         :implementation-boundary :pure-abstract-transition-only
         :claim-scope :matching-abstract-transition-only
         :checking :abstract-transition-derivation-checked
         :conclusion-status :conditional-on-matching-abstract-relation
         :assumption-discharge :not-performed
         :proof-status :conditional-abstract-inference
         :certificate :not-issued
         :admission :not-admitted
         :runtime-enforcement :not-established
         :implementation-correspondence :not-established
         :authority :none}))))
