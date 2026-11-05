(ns gesso.assurance.proof.abstract-execution-induction
  "Independent S1/S4 finite-initial-set, declared-operation induction schema (v1362).

   Given an exact source-owned set of initial states and a closed DECLARED
   operation set, check the invariant on every enumerated initial state and
   derive a v1359 conditional preservation instance for EVERY operation.
   Composition lemmas retain their production typed-admission dependencies;
   this module does not prove the source admission checker sound.
   Conditional schema: Init_subset(s0) and all declared matching abstract
   transitions imply I(sn) for every finite history, IF the constituent
   typed/transition/lemma semantics are sound. This is structural induction;
   it does NOT establish that the enumerated initial states or operations
   cover actual initialization and physical writers.

   audit-finite-history! independently checks one supplied sequence of
   abstract before/input/after transitions. A witness cannot discharge
   universal induction premises, prove production correspondence, or issue
   certificates. All reports are ordinary, copyable diagnostic data."
  (:require [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.independent-transition-semantics :as transition]
            [gesso.assurance.proof.preservation-composition-soundness :as compose]))

(def semantics-version 1)
(def schema-type :gesso.assurance.proof.abstract-execution-induction/conditional-schema)
(def history-type :gesso.assurance.proof.abstract-execution-induction/finite-history)

(def ^:private catalog-keys
  #{:id :state-types :invariant :declared-initial-states
    :declared-operation-ids :operations})
(def ^:private operation-keys #{:id :declaration})
(def ^:private step-keys #{:operation-id :inputs :after})
(def ^:private max-operations 32)
(def ^:private max-initial-states 32)
(def ^:private max-history-steps 128)

(def ^:private initial-and-step-assumptions
  #{:sound-independent-typed-expression-semantics
    :sound-independent-abstract-transition-semantics
    :sound-conditional-frame-literal-and-copy-rules
    :sound-conjunctive-rule-composition
    :pure-invariant-extensionality
    :finite-induction-over-declared-abstract-transitions})
(def ^:private open-external-obligations
  #{:actual-initializer-set-coverage
    :actual-operation-set-completeness
    :physical-write-path-coverage
    :declared-state-projection-completeness
    :universal-s1-s4-implementation-soundness
    :xtdb-execution-correspondence
    :certificate-admission})

(def ^:private original-derive compose/derive-composed-lemma!)
(def ^:private original-evaluate presence/evaluate-expression!)
(def ^:private original-observe transition/observe-transition!)

(defn- reject! [reason at details]
  (throw (ex-info "Invalid source-derived abstract execution induction."
                  (merge {:abstract-execution-induction/rejection true
                          :reason reason :at at} details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- meta-free! [value at]
  (when (and (instance? clojure.lang.IObj value)
             (some? (meta value)))
    (reject! :metadata-not-allowed at {}))
  value)

(defn- exact-map! [value required at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (meta-free! value at)
  (when-not (= required (set (keys value)))
    (reject! :wrong-keys at
             {:expected required :actual (set (keys value))}))
  value)

(defn- qualified-id? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- bounded-vector! [value lower upper at]
  (when-not (and (instance? clojure.lang.APersistentVector value)
                 (<= lower (count value) upper))
    (reject! :invalid-bounded-vector at
             {:minimum lower :maximum upper}))
  (meta-free! value at))

(defn- checked-evaluation! [expression types state]
  (let [result (original-evaluate expression types state)]
    (when-not (identical? (var-get #'presence/evaluate-expression!)
                           original-evaluate)
      (when-not (= result (presence/evaluate-expression!
                           expression types state))
        (reject! :substituted-typed-interpreter [:invariant] {})))
    (when-not (and (= presence/result-type (:type result))
                   (= presence/semantics-version (:version result))
                   (= expression (:expression result))
                   (= (set (keys types)) (:source-readers result))
                   (instance? Boolean (:value result))
                   (= :not-proved (:proof-status result))
                   (= :not-admitted (:admission result))
                   (= :none (:authority result)))
      (reject! :typed-evaluation-contract-changed [:invariant] {}))
    (:value result)))

(defn- invariant-truth! [invariant state-types state]
  ;; Closed, typed states are checked against the FULL shared projection,
  ;; not merely the invariant's readers.
  (values/observe-state! state-types state)
  (let [readers (:readers invariant)]
    (checked-evaluation! (:expression invariant)
                         (select-keys state-types readers)
                         (select-keys state readers))))

(defn- checked-lemma! [declaration invariant operation-id]
  (let [report (original-derive declaration invariant)]
    (when-not (identical? (var-get #'compose/derive-composed-lemma!)
                           original-derive)
      (when-not (= report (compose/derive-composed-lemma!
                           declaration invariant))
        (reject! :substituted-composition-lemma
                 [:operations operation-id] {})))
    (when-not (and (= compose/report-type (:type report))
                   (= compose/semantics-version (:version report))
                   (= declaration (:source-declaration report))
                   (= invariant (:source-invariant report))
                   (= (:id invariant) (:invariant-id report))
                   (= :conditional-schema-only (:proof-status report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report))
                   (if (= :conditional-composed-rule-instance (:status report))
                     (and (empty? (:open-clause-indices report))
                          (= :invariant-and-matching-transition-imply-post-invariant
                             (:conditional-claim report)))
                     (and (= :open-unproved-clause (:status report))
                          (seq (:open-clause-indices report))
                          (nil? (:conditional-claim report)))))
      (reject! :composition-lemma-contract-changed
               [:operations operation-id] {}))
    report))

(defn- checked-observation! [declaration before inputs after]
  (let [report (original-observe declaration before inputs after)]
    (when-not (identical? (var-get #'transition/observe-transition!)
                           original-observe)
      (when-not (= report (transition/observe-transition!
                           declaration before inputs after))
        (reject! :substituted-transition-observer [:history] {})))
    (when-not (and (= transition/report-type (:type report))
                   (= transition/semantics-version (:version report))
                   (= declaration (:source-declaration report))
                   (= before (:before report))
                   (= inputs (:inputs report))
                   (= after (:after report))
                   (every? #(instance? Boolean (get report %))
                           [:guard-holds? :assignments-hold?
                            :frame-holds? :matches?])
                   (= (:matches? report)
                      (and (:guard-holds? report)
                           (:assignments-hold? report)
                           (:frame-holds? report)))
                   (= :not-proved (:proof-status report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report)))
      (reject! :transition-observation-contract-changed [:history] {}))
    report))

(defn- exact-operations! [catalog]
  (let [declared (:declared-operation-ids catalog)
        entries (:operations catalog)]
    (when-not (and (instance? clojure.lang.PersistentHashSet declared)
                   (<= 1 (count declared) max-operations)
                   (every? qualified-id? declared))
      (reject! :invalid-declared-operation-set [:declared-operation-ids] {}))
    (meta-free! declared [:declared-operation-ids])
    (bounded-vector! entries 1 max-operations [:operations])
    (doseq [[index entry] (map-indexed vector entries)]
      (exact-map! entry operation-keys [:operations index])
      (when-not (and (qualified-id? (:id entry))
                     (= (:id entry)
                        (get-in entry [:declaration :subject :id]))
                     (= (:state-types catalog)
                        (get-in entry [:declaration :state-types])))
        (reject! :operation-source-mismatch [:operations index] {})))
    (let [ids (mapv :id entries)]
      (when-not (and (= (count ids) (count (set ids)))
                     (= declared (set ids)))
        (reject! :operation-set-coverage-mismatch [:operations]
                 {:declared declared :actual (set ids)})))
    (vec (sort-by (comp str :id) entries))))

(defn derive-induction-schema!
  "Induction schema over EXACTLY the enumerated initial states and declared
   abstract operations. The initial set is finite by DEFINITION, not a claim
   about real-world initializer completeness. Every operation must have a
   supported v1359 composed lemma; one open step leaves induction open."
  [catalog]
  (exact-map! catalog catalog-keys [])
  (when-not (qualified-id? (:id catalog))
    (reject! :invalid-catalog-id [:id] {}))
  (bounded-vector! (:declared-initial-states catalog)
                   1 max-initial-states [:declared-initial-states])
  (let [states (:declared-initial-states catalog)
        invariant (:invariant catalog)
        types (:state-types catalog)
        operations (exact-operations! catalog)]
    (when-not (= (count states) (count (distinct states)))
      (reject! :duplicate-declared-initial-state
               [:declared-initial-states] {}))
    ;; Full source typing and per-operation invariant admission are checked
    ;; by the composition checker, independently of supplied cached reports.
    (let [operation-rows
          (mapv (fn [{:keys [id declaration]}]
                  (let [lemma (checked-lemma! declaration invariant id)]
                    {:operation-id id
                     :status (:status lemma)
                     :transition-fingerprint (:transition-fingerprint lemma)
                     :typed-invariant-fingerprint
                     (:typed-invariant-fingerprint lemma)
                     :supported-clause-indices
                     (:conditionally-supported-clause-indices lemma)
                     :open-clause-indices (:open-clause-indices lemma)}))
                operations)
          base-rows
          (mapv (fn [i state]
                  {:index i
                   :state state
                   :invariant-holds?
                   (invariant-truth! invariant types state)})
                (range (count states)) states)
          failing-bases (into #{} (map :index)
                              (remove :invariant-holds? base-rows))
          unsupported-ops (into #{} (map :operation-id)
                                (remove #(= :conditional-composed-rule-instance
                                            (:status %)) operation-rows))
          supported? (and (empty? failing-bases)
                          (empty? unsupported-ops))]
      {:type schema-type
       :version semantics-version
       :source-catalog catalog
       :catalog-id (:id catalog)
       :invariant-id (:id invariant)
       :declared-operation-ids (:declared-operation-ids catalog)
       :initial-cases base-rows
       :failing-initial-indices failing-bases
       :operation-cases operation-rows
       :unsupported-operation-ids unsupported-ops
       :status (if supported? :conditional-declared-induction-schema
                   :open-base-or-step)
       :conditional-claim
       (when supported?
         :every-finite-matching-declared-history-preserves-invariant)
       :scope :finite-enumerated-initial-states-and-closed-declared-operations
       :induction-premises initial-and-step-assumptions
       :open-external-obligations open-external-obligations
       :initializer-coverage :not-established
       :physical-operation-completeness :not-established
       :universal-implementation-soundness :not-established
       :xtdb-correspondence :not-established
       :proof-status :conditional-schema-only
       :certificate :not-issued
       :admission :not-admitted
       :runtime-enforcement :not-established
       :authority :none})))

(defn audit-finite-history!
  "Re-derive the complete induction schema, then check a supplied finite
   history starting from one of its enumerated initial states. Each step is
   {:operation-id ... :inputs ... :after ...}. The first undeclared or
   nonmatching step stops the audit; no later states are counted. This is
   only a finite, nonauthoritative witness of the declared abstract machine."
  [catalog initial-state steps]
  (bounded-vector! steps 0 max-history-steps [:history])
  (let [schema (derive-induction-schema! catalog)
        initial-set (set (:declared-initial-states catalog))
        initial? (contains? initial-set initial-state)
        operations (into {} (map (juxt :id :declaration)
                                 (:operations catalog)))
        invariant (:invariant catalog)
        types (:state-types catalog)]
    (values/observe-state! types initial-state)
    (let [initial-holds? (invariant-truth! invariant types initial-state)
          observation
          (if-not initial?
            {:status :outside-declared-initial-set
             :state initial-state :rows []
             :all-invariant? initial-holds?}
            (loop [index 0 state initial-state remaining steps
                   rows [] all-invariant? initial-holds?]
              (if (empty? remaining)
                {:status (if all-invariant?
                           :checked-finite-declared-history
                           :finite-history-invariant-counterexample)
                 :state state :rows rows
                 :all-invariant? all-invariant?}
                (let [step (first remaining)
                      _ (exact-map! step step-keys [:history index])
                      id (:operation-id step)
                      _ (when-not (qualified-id? id)
                          (reject! :invalid-step-operation-id
                                   [:history index :operation-id] {}))
                      declaration (get operations id)]
                  (if-not declaration
                    {:status :undeclared-operation
                     :state state :rows (conj rows
                                             {:index index :operation-id id
                                              :status :undeclared-operation})
                     :all-invariant? all-invariant?}
                    (let [post (:after step)
                          inputs (:inputs step)
                          observed (checked-observation!
                                    declaration state inputs post)
                          matches? (:matches? observed)
                          after-holds? (invariant-truth!
                                        invariant types post)
                          row {:index index :operation-id id
                               :status (if matches? :matching-abstract-step
                                           :nonmatching-abstract-step)
                               :guard-holds? (:guard-holds? observed)
                               :assignments-hold? (:assignments-hold? observed)
                               :frame-holds? (:frame-holds? observed)
                               :before-invariant
                               (invariant-truth! invariant types state)
                               :after-invariant after-holds?}
                          new-rows (conj rows row)]
                      (if-not matches?
                        {:status :nonmatching-abstract-transition
                         :state state :rows new-rows
                         :all-invariant? all-invariant?}
                        (do
                          (when (and (= :conditional-declared-induction-schema
                                        (:status schema))
                                     all-invariant? (not after-holds?))
                            (reject! :conditional-induction-disagreement
                                     [:history index] {:operation-id id}))
                          (recur (inc index) post (rest remaining)
                                 new-rows (and all-invariant? after-holds?))))))))))]
      {:type history-type
       :version semantics-version
       :source-catalog catalog
       :schema-status (:status schema)
       :initial-state initial-state
       :initial-in-declared-set? initial?
       :initial-invariant initial-holds?
       :steps-requested (count steps)
       :steps-inspected (count (:rows observation))
       :steps-matching (count (filter #(= :matching-abstract-step (:status %))
                                      (:rows observation)))
       :step-observations (:rows observation)
       :last-matched-state (:state observation)
       :all-matched-states-satisfy-invariant?
       (:all-invariant? observation)
       :status (:status observation)
       :checking :independent-typed-abstract-history
       :proof-status :finite-witness-not-universal-proof
       :physical-observation :not-established
       :certificate :not-issued
       :admission :not-admitted
       :authority :none})))
