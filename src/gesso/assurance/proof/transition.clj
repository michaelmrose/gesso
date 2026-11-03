(ns gesso.assurance.proof.transition
  "A closed, purely ABSTRACT guarded assignment relation for Gesso (v1233).

   Source declaration (all keys required, no additional keys):
     {:subject {:kind :operation :id :example/claim
                :interface {:operation :example/claim}}
      :state-types {:request/status {:type :enum
                                     :name :request/status
                                     :values #{:open :claimed}}
                    :request/owner {:type :keyword :nullable? true}}
      :input-types {:input/actor {:type :keyword}}
      :guard {:readers #{:request/status}
              :expression [:= [:read :request/status] [:literal :open]]}
      :reads #{:request/status :input/actor}
      :writes #{:request/status :request/owner}
      :assignments {:request/status [:literal :claimed]
                    :request/owner [:read :input/actor]}}

   State field names and input field names are disjoint, qualified keywords.
   The state is an EXACTLY described projection of the abstract machine; this
   file makes NO claim about omitted application or database fields. The guard
   uses v1219 typed Boolean rules over the PRE-state and input. Each assigned
   field receives a scalar literal or the value of one required typed pre-state
   or input field. All other declared state fields are unchanged, including
   whether optional fields are present. There are no arithmetic assignments,
   calls, effects, deletion, sequencing, or runtime database operations here.

   describe-transition! validates and canonicalizes source syntax and typing.
   observe-transition! checks whether supplied pre/input/post data satisfy
   the abstract guarded relation. Both return COPYABLE, UNADMITTED DATA. Even
   an observed matching transition is not an inductive invariant proof, an
   admitted mathematical theorem, a certificate, or authority to commit. No
   arbitrary Clojure callbacks, XTDB transactions, or handlers are executed.
   Implementation correspondence remains a separate obligation."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-normalize :as rule-normalize]
            [gesso.assurance.subject :as subject]))

(def transition-version 1)
(def description-type :gesso.assurance.proof.transition/unadmitted-transition)
(def observation-type :gesso.assurance.proof.transition/abstract-observation)

(def ^:private declaration-keys
  #{:subject :state-types :input-types :guard :reads :writes :assignments})
(def ^:private max-state-fields 128)
(def ^:private max-input-fields 128)
(def ^:private max-total-fields 128)
(def ^:private identity-id :gesso.assurance.proof/transition-v1)
;; A fresh synthetic target is needed for type-checking assignments via the
;; existing v1221 type-checker. It is reserved to prevent a name collision.
(def ^:private target-reader :gesso.assurance.proof.transition/target)

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid abstract Gesso guarded transition."
                  (merge {:error/type ::invalid-transition
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- qualified-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- metadata-free! [value at]
  (when (and (instance? clojure.lang.IObj value)
             (some? (meta value)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value expected at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= actual expected)
      (reject! :wrong-keys at
               {:missing (set/difference expected actual)
                :unexpected (set/difference actual expected)}))))

(defn- keyword-set! [value at]
  (when-not (plain-set? value)
    (reject! :expected-plain-keyword-set at {}))
  (metadata-free! value at)
  (when (> (count value) max-total-fields)
    (reject! :set-size-limit at {:maximum max-total-fields}))
  (doseq [field value]
    (when-not (qualified-keyword? field)
      (reject! :unqualified-field at {:field field}))))

(defn- type-declarations! [state-types input-types]
  (doseq [[at declarations maximum] [[:state-types state-types max-state-fields]
                                     [:input-types input-types max-input-fields]]]
    (when-not (plain-map? declarations)
      (reject! :expected-type-map [at] {}))
    (metadata-free! declarations [at])
    (when (> (count declarations) maximum)
      (reject! :type-map-size-limit [at] {:maximum maximum}))
    (doseq [field (keys declarations)]
      (when-not (qualified-keyword? field)
        (reject! :unqualified-field [at field] {}))))
  (when (empty? state-types)
    (reject! :empty-state-projection [:state-types] {}))
  (let [state-ids (set (keys state-types))
        input-ids (set (keys input-types))
        all-ids (set/union state-ids input-ids)]
    (when (seq (set/intersection state-ids input-ids))
      (reject! :overlapping-state-and-input [:input-types] {}))
    (when (contains? all-ids target-reader)
      (reject! :reserved-reader [:state-types :input-types] {}))
    (when (> (count all-ids) max-total-fields)
      (reject! :total-type-limit [:state-types :input-types]
               {:maximum max-total-fields}))
    ;; Reuse v1221 type checking for every nominal descriptor. :present
    ;; checks an OPTIONAL binding without evaluating or reading its value.
    ;; The probe is a well-typed Boolean involving ALL declared fields.
    (let [readers (mapv (fn [id] [:present [:read id]])
                        (sort all-ids))
          expression (if (= 1 (count readers))
                       (first readers)
                       (into [:and] readers))
          checked (rule-normalize/normalize-rule!
                   {:readers all-ids :expression expression}
                   (merge state-types input-types))]
      {:state (select-keys (get-in checked [:semantics :reader-types]) state-ids)
       :input (select-keys (get-in checked [:semantics :reader-types]) input-ids)})))

(defn- assignment-reads [assignments]
  (into #{} (keep (fn [[_ expression]]
                    (when (= :read (first expression)) (second expression))))
        assignments))

(defn- normalized-assignment!
  [field expression types all-types]
  (let [at [:assignments field]
        target (get types field)]
    (when-not (plain-vector? expression)
      (reject! :expected-assignment-vector at {}))
    (metadata-free! expression at)
    (when-not (and (= 2 (count expression))
                   (#{:literal :read} (first expression)))
      (reject! :unsupported-assignment at
               {:allowed #{:literal :read}}))
    (let [[op operand] expression
          reads (if (= :read op) #{operand} #{})]
      (when (and (= :read op)
                 (not (contains? all-types operand)))
        (reject! :undeclared-assignment-reader at {:reader operand}))
      (when (and (= :read op) (true? (:optional? (get all-types operand))))
        (reject! :optional-assignment-source at {:reader operand}))
      (when (and (= :read op)
                 (true? (:nullable? (get all-types operand)))
                 (not (:nullable? target)))
        (reject! :nullable-assignment-source at {:reader operand}))
      ;; Check the ENTIRE assignment using the existing typed rule language:
      ;; a synthetic, REQUIRED target read must be type-compatible with the
      ;; closed literal/read RHS. An optional target can be assigned, but it
      ;; is always present in the resulting state after assignment.
      (let [target-type (assoc target :optional? false)
            probe {:readers (conj reads target-reader)
                   :expression [:= [:read target-reader] expression]}
            checked (rule-normalize/normalize-rule!
                     probe
                     (assoc (select-keys all-types reads)
                            target-reader target-type))]
        ;; The typed, canonical RHS is the third operand of normalized equality.
        (get-in checked [:semantics :expression 2])))))

(defn describe-transition!
  "Validate an EXACT, data-only, guarded assignment declaration and produce a
   stable, unadmitted semantic description. The read footprint is exactly the
   guard reads plus RHS source reads, and the write footprint is exactly the
   assignment keys. Optional source fields cannot be assigned without a later
   explicit refinement/assignment semantics extension."
  [declaration]
  (exact-map! declaration declaration-keys [])
  (let [{:keys [subject state-types input-types guard reads writes assignments]}
        declaration
        subject-id (subject/describe-subject subject)
        types (type-declarations! state-types input-types)
        state-fields (set (keys (:state types)))
        input-fields (set (keys (:input types)))
        all-types (merge (:state types) (:input types))]
    (keyword-set! reads [:reads])
    (keyword-set! writes [:writes])
    (when-not (plain-map? assignments)
      (reject! :expected-assignment-map [:assignments] {}))
    (metadata-free! assignments [:assignments])
    (when (> (count assignments) max-state-fields)
      (reject! :assignment-count-limit [:assignments] {}))
    (let [assigned (set (keys assignments))]
      (when-not (= writes assigned)
        (reject! :write-footprint-mismatch [:writes]
                 {:missing (set/difference assigned writes)
                  :unexpected (set/difference writes assigned)}))
      (when-not (set/subset? assigned state-fields)
        (reject! :writes-outside-state [:assignments]
                 {:unexpected (set/difference assigned state-fields)}))
      (let [normalized-assignments
            (into {} (map (fn [[field expression]]
                            [field (normalized-assignment!
                                    field expression (:state types) all-types)]))
                  assignments)
            assignment-readers (assignment-reads assignments)
            guard-syntax (rule/check-rule! guard)
            guard-readers (:readers guard-syntax)
            _ (when-not (set/subset? guard-readers (set (keys all-types)))
                (reject! :unknown-guard-reader [:guard]
                         {:unexpected (set/difference guard-readers (set (keys all-types)))}))
            guard-normalized
            (rule-normalize/normalize-rule!
             guard (select-keys all-types guard-readers))
            effective-readers (set/union guard-readers assignment-readers)]
        (when-not (= reads effective-readers)
          (reject! :read-footprint-mismatch [:reads]
                   {:missing (set/difference effective-readers reads)
                    :unexpected (set/difference reads effective-readers)}))
        (when-not (= input-fields (set/intersection reads input-fields))
          (reject! :unused-input-declarations [:input-types]
                   {:unused (set/difference input-fields reads)}))
        (let [semantics
              {:version transition-version
               :subject-fingerprint (:fingerprint subject-id)
               :state-types (:state types)
               :input-types (:input types)
               :guard (:semantics guard-normalized)
               :reads reads
               :writes writes
               :assignments normalized-assignments
               :frame-fields (set/difference state-fields writes)}
              identity (subject/describe-subject
                        {:kind :rule
                         :id identity-id
                         :interface semantics})]
          {:type description-type
           :version transition-version
           :fingerprint (:fingerprint identity)
           :subject-fingerprint (:fingerprint subject-id)
           :semantics semantics
           :checking :syntax-and-types
           :interpretation :guarded-assignments-and-frame
           :proof-status :not-proved
           :admission :not-admitted
           :runtime-enforcement :not-established
           :authority :none})))))

(defn- presence-probe [ids]
  (let [terms (mapv (fn [id] [:present [:read id]]) (sort ids))]
    {:readers ids
     :expression (cond
                   (empty? terms) [:literal true]
                   (= 1 (count terms)) (first terms)
                   :else (into [:and] terms))}))

(defn- valid-bindings!
  [types bindings]
  ;; The result of :present is irrelevant; its evaluation forces v1223's
  ;; strict required/optional/type/enum/extra-key checks for every field.
  ;; This is not validation of arbitrary opaque effects or persisted state.
  (rule-eval/evaluate-rule! (presence-probe (set (keys types)))
                            types bindings)
  bindings)

(defn- same-field?
  [before after field]
  (and (= (contains? before field) (contains? after field))
       (if (contains? before field)
         (= (get before field) (get after field))
         true)))

(defn- expected-assignment
  [expression pre-and-input]
  (let [[op operand] expression]
    (case op
      :literal operand
      :read (get pre-and-input operand))))

(defn observe-transition!
  "Interpret the guarded abstract relation over caller-supplied pre/input/post
   maps. A matching observation has guard=true, every declared write equal to
   its assignment's value, and all remaining declared state fields unchanged,
   including presence. Invalid observations throw; valid but nonmatching ones
   return :matches? false with separate guard/write/frame components.

   This is NOT a commit, verified invariant, proof certificate, or assertion
   that an actual handler/database realizes this relation. All three maps are
   checked against their exact declared type footprints on every call."
  [declaration before inputs after]
  (let [description (describe-transition! declaration)
        {:keys [state-types input-types writes assignments frame-fields]}
        (:semantics description)
        all-types (merge state-types input-types)]
    (when-not (and (plain-map? before) (plain-map? inputs) (plain-map? after))
      (reject! :expected-state-input-maps [] {}))
    (metadata-free! before [:before])
    (metadata-free! inputs [:inputs])
    (metadata-free! after [:after])
    (valid-bindings! state-types before)
    (valid-bindings! input-types inputs)
    (valid-bindings! state-types after)
    (let [pre-and-input (merge before inputs)
          guard-value (:value
                       (rule-eval/evaluate-rule!
                        (:guard declaration)
                        (select-keys all-types (get-in declaration [:guard :readers]))
                        (select-keys pre-and-input (get-in declaration [:guard :readers]))))
          assignments-ok?
          (every? (fn [field]
                    (and (contains? after field)
                         (= (get after field)
                            (expected-assignment (get assignments field)
                                                 pre-and-input))))
                  writes)
          frame-ok?
          (every? #(same-field? before after %) frame-fields)]
      {:type observation-type
       :version transition-version
       :subject-fingerprint (:subject-fingerprint description)
       :transition-fingerprint (:fingerprint description)
       :guard-holds? guard-value
       :assignments-hold? (boolean assignments-ok?)
       :frame-holds? (boolean frame-ok?)
       :matches? (boolean (and guard-value assignments-ok? frame-ok?))
       :checking :abstract-relation-observed
       :proof-status :not-proved
       :admission :not-admitted
       :runtime-enforcement :not-established
       :authority :none})))
