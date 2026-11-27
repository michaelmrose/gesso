(ns gesso.assurance.reference-recovery-invariant
  "Finite selected replay checks, not a universal physical invariant theorem."
  (:require [gesso.assurance.proof.independent-transition-semantics :as transition]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-recovery-expression :as expression]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.rule-eval :as production]
            [gesso.assurance.transition :as construction]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.inductive-plan :as induction]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant ZonedDateTime]))

(def profile :selected-retained-invariant-boundaries-v1)
(def ^:private original-project poststate/project-visible!)
(def ^:private original-replay temporal/replay)
(def ^:private original-visible temporal/visible-at)
(def ^:private original-observe-state values/observe-state!)
(def ^:private original-evaluate presence/evaluate-expression!)
(def ^:private original-production-evaluate production/evaluate-rule!)
(def ^:private original-construction construction/build-guarded-replacement!)
(def ^:private original-transition transition/observe-transition!)
(def ^:private original-cases outcome/derive-cases!)
(def ^:private original-reader acquisition/read-acquisition!)
(def ^:private original-observation publication/read-framed-submission-observation!)
(def ^:private original-initialization induction/check-mapped-initialization!)
(def ^:private original-query xt/q)
(def ^:private original-decode-basis basis/<-time-basis-str)
(def ^:private original-expression-catalog expression/check-catalog!)
(def ^:private original-expression-boundaries expression/boundaries-current?)
(def ^:private original-independent-project projection/project!)
(def ^:private original-projection-boundaries projection/boundaries-current?)
(def ^:private original-projection-catalog projection/check-catalog!)
(def ^:private original-selected-transition refinement/check-selected!)

(defn boundaries-current? []
  (and (identical? original-project poststate/project-visible!)
       (identical? original-replay temporal/replay)
       (identical? original-visible temporal/visible-at)
       (identical? original-observe-state values/observe-state!)
       (identical? original-evaluate presence/evaluate-expression!)
       (identical? original-production-evaluate production/evaluate-rule!)
       (identical? original-construction construction/build-guarded-replacement!)
       (identical? original-transition transition/observe-transition!)
       (identical? original-cases outcome/derive-cases!)
       (identical? original-reader acquisition/read-acquisition!)
       (identical? original-observation publication/read-framed-submission-observation!)
       (identical? original-initialization induction/check-mapped-initialization!)
       (identical? original-query xt/q)
       (identical? original-decode-basis basis/<-time-basis-str)
       (identical? original-expression-catalog expression/check-catalog!)
       (identical? original-expression-boundaries expression/boundaries-current?)
       (original-expression-boundaries)
       (identical? original-independent-project projection/project!)
       (identical? original-projection-boundaries projection/boundaries-current?)
       (original-projection-boundaries)
       (identical? original-projection-catalog projection/check-catalog!)
       (identical? original-selected-transition refinement/check-selected!)))

(defn- report [status reason]
  (cond-> {:profile profile :status status
           :scope :original-selected-transaction-boundaries-only
           :physical-invariant-theorem :not-established
           :universal-refinement :not-established
           :proof-discharge :not-performed :authority :none :certificate :not-issued}
    reason (assoc :reason reason)))

(def expression-profile :selected-invariant-expression-correspondence-v1)

(defn- corresponding-value! [types state {:keys [readers expression] :as declaration}]
  (let [reader-types (select-keys types readers)
        bindings (select-keys state readers)
        independent (original-evaluate expression reader-types bindings)
        evaluated (original-production-evaluate declaration reader-types bindings)]
    (when-not (and (= readers (set (keys reader-types)))
                   (= expression (:expression independent) (:expression evaluated))
                   (= readers (:source-readers independent) (:readers evaluated))
                   (= presence/result-type (:type independent))
                   (= presence/semantics-version (:version independent))
                   (= production/evaluated-type (:type evaluated))
                   (= production/evaluator-version (:version evaluated))
                   (= :independent-short-circuit-semantics (:evaluation independent))
                   (= :pure-interpreted (:evaluation evaluated))
                   (= :not-proved (:proof-status independent) (:proof-status evaluated))
                   (= :not-admitted (:admission independent) (:admission evaluated))
                   (= :none (:authority independent) (:authority evaluated))
                   (instance? Boolean (:value independent))
                   (instance? Boolean (:value evaluated))
                   (= (:value independent) (:value evaluated)))
      (throw (ex-info "Original invariant expression evaluators disagree"
                      {:reason :invariant-expression-correspondence-mismatch})))
    (:value independent)))

(defn- holds? [types state declaration]
  (true? (corresponding-value! types state declaration)))

(defn- expression-correspondence [invariants checked]
  {:profile expression-profile
   :status :matched
   :scope :selected-replay-projections-only
   :checking :original-declarations-production-vs-independent
   :initial-condition :matched-true-at-initial-projection
   :invariant-ids (set (keys invariants))
   :checked-points (into [{:tx-id (:tx-id (first checked)) :side :initial}]
                         (mapcat (fn [{:keys [tx-id]}]
                                   [{:tx-id tx-id :side :prestate}
                                    {:tx-id tx-id :side :poststate}]))
                         (rest checked))
   :universal-evaluator-correspondence :not-established
   :proof-discharge :not-performed :authority :none :certificate :not-issued})

(defn- check-state? [types state invariants]
  (and (= (set (keys types)) (set (keys state)))
       (= (set (keys types)) (:present-readers (original-observe-state types state)))
       (every? #(holds? types state %) (vals invariants))))

(defn check-projected-invariants!
  "Check every original invariant on a supplied complete typed projection,
   using both expression evaluators. This is finite evaluation, not a
   physical observation or an invariant-preservation theorem."
  [{:keys [operations invariants]} state]
  (let [types (get-in operations [0 :construction :state-types])]
    (when-not (and (boundaries-current?) (map? types) (seq types)
                   (map? invariants) (seq invariants)
                   (every? #(= types (get-in % [:construction :state-types])) operations)
                   (check-state? types state invariants)
                   (boundaries-current?))
      (throw (ex-info "Unqualified projected invariant state"
                      {:reason :projected-invariant-state-mismatch})))
    (set (keys invariants))))

(defn- audit-replay!
  [catalog coordinates cases seed-state inputs comparison steps]
  (let [{:keys [initial-condition invariants operations]} catalog
        types (get-in operations [0 :construction :state-types])
        [winner abort recovery :as ids] (:selected-tx-ids comparison)
        manifest (:manifest-tx-id comparison)
        initial (:initialization-tx-id comparison)
        maintenance (mapv :tx-id (:intervening-maintenance comparison))
        expected (when (and (vector? ids) (every? integer? (concat ids maintenance)))
                   (into [manifest initial] (sort (into ids maintenance))))
        selected (when (and (integer? recovery) (vector? inputs)
                            (every? #(integer? (:tx-id %)) inputs))
                   (vec (take-while #(<= (:tx-id %) recovery) inputs)))
        steps-by-id (zipmap ids steps)
        by-operation (into {} (map (juxt :id :construction)) operations)
        project (fn [replay system valid]
                  (original-project coordinates (original-visible replay system valid)
                                    {:outcome-cases cases}))]
    (if-not (and (vector? ids) (= 3 (count ids))
                 (seq expected) (every? integer? expected) (apply < manifest initial ids)
                 (= [:committed :aborted :committed] (:selected-commitments comparison))
                 (= expected (mapv :tx-id selected))
                 (= (count expected) (count (distinct expected)))
                 (= 3 (count steps))
                 (= ids (mapv #(get-in % [:expectation :tx-id]) steps))
                 (every? #(and (< winner % recovery) (not= abort %)) maintenance)
                 (seq invariants) (map? types)
                 (every? #(= types (:state-types %)) (vals by-operation)))
      (report :unsupported :invalid-selected-invariant-sequence)
      (let [replay (original-replay (mapv #(dissoc % :tx-id) selected))
            history (:history replay)
            initial-time (:system-time (second selected))]
        (cond
          (not= :predicted (:status replay)) (report :unsupported :unsupported-invariant-replay)
          (not (every? true? (map :committed? (take 2 history))))
          (report :mismatch :uncommitted-invariant-initialization)
          :else
          (let [initial-state (project replay initial-time initial-time)]
            (if-not (and (= seed-state initial-state)
                         (check-state? types initial-state invariants)
                         (holds? types initial-state initial-condition))
              (report :mismatch :initial-projection-invariant-mismatch)
              (loop [i 2 prior initial-state
                     checked [{:tx-id initial :relation :checked-initial-projection}]]
                (if (= i (count selected))
                  (assoc (report :checked nil)
                         :initial-condition :checked-on-projected-initial-state
                         :projection-continuity :checked-at-selected-transaction-boundaries
                         :invariant-checking :direct-on-selected-replay-projections
                         :derivation :finite-direct-evaluation-not-inductive-theorem
                         :poststate-coordinate :transaction-system-and-valid-time
                         :prestate-coordinate :preceding-system-current-transaction-valid-time
                         :conclusion :declared-invariants-hold-on-selected-replay-projections
                         :invariant-ids (set (keys invariants))
                         :expression-correspondence (expression-correspondence invariants checked)
                         :selected-tx-ids ids :checked-boundaries checked
                         :unestablished-implications #{:projection-to-authoritative-physical-state
                                                      :coverage-of-unselected-valid-times
                                                      :coverage-of-unselected-transactions
                                                      :universal-physical-refinement})
                  (let [tx (nth selected i) id (:tx-id tx) t (:system-time tx)
                        prior-t (:system-time (nth selected (dec i)))
                        before (project replay prior-t t)
                        after (project replay t t)
                        committed? (:committed? (nth history i))
                        step (get steps-by-id id)
                        operation-id (get-in step [:spec :alternative])
                        original (get by-operation operation-id)
                        relation (cond (= id abort) :aborted-stutter
                                       (#{winner recovery} id) :committed-abstract-transition
                                       :else :maintenance-stutter)]
                    (cond
                      (not= prior before) (report :mismatch :selected-projection-continuity-mismatch)
                      (not (case relation
                             :aborted-stutter (and (false? committed?) (= before after))
                             :maintenance-stutter (and (true? committed?) (= before after))
                             :committed-abstract-transition
                             (and (true? committed?) original (= original (:original step))
                                  (true? (:matches? (original-transition
                                                    (:transition (original-construction original))
                                                    before (get-in step [:spec :inputs]) after))))))
                      (report :mismatch :selected-invariant-transition-mismatch)
                      (not (and (check-state? types before invariants)
                                (check-state? types after invariants)))
                      (report :mismatch :selected-projection-invariant-violation)
                      :else (recur (inc i) after
                                   (conj checked (cond-> {:tx-id id :relation relation}
                                                   (= :committed-abstract-transition relation)
                                                   (assoc :alternative-id operation-id)))))))))))))))

(def physical-profile :selected-pinned-invariant-readbacks-v3)
(def snapshot-profile :selected-pinned-snapshot-continuity-v1)

(defn- snapshot-token! [rows]
  (let [token (when (and (sequential? rows) (= 1 (count rows))
                         (= #{:snapshot-token} (set (keys (first rows)))))
                (:snapshot-token (first rows)))]
    (when-not (and (string? token) (seq token))
      (throw (ex-info "Invalid selected snapshot token" {:reason :invalid-selected-snapshot-token})))
    token))

(defn- snapshot-point! [token selected]
  (let [decoded (try (original-decode-basis token)
                     (catch Exception _ nil))
        times (when (and (map? decoded) (= #{"xtdb"} (set (keys decoded)))) (get decoded "xtdb"))
        point (when (and (sequential? times) (= 1 (count times))) (first times))]
    (when-not (and (instance? Instant point) (seq selected)
                   (every? #(instance? Instant (:system-time %)) selected)
                   (every? (fn [[a b]] (.isBefore ^Instant (:system-time a) ^Instant (:system-time b)))
                           (partition 2 1 selected))
                   (not (.isBefore ^Instant point ^Instant (:system-time (peek selected)))))
      (throw (ex-info "Unqualified selected snapshot point" {:reason :selected-snapshot-does-not-cover-cut})))
    point))

(defn- confirm-basis! [node basis]
  (when-not (= (:snapshot-token basis)
               (snapshot-token! (original-query node "SHOW SNAPSHOT_TOKEN" basis)))
    (throw (ex-info "Pinned snapshot token changed" {:reason :selected-pinned-token-mismatch}))))

(defn- check-selected-ledger! [node basis selected history]
  (when-not (and (= (count selected) (count history))
                 (= (mapv :system-time selected) (mapv :system-time history)))
    (throw (ex-info "Unqualified retained replay history" {:reason :selected-ledger-history-mismatch})))
  (mapv (fn [{:keys [tx-id system-time]} {:keys [committed?]}]
          (let [rows (original-query node ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?" tx-id] basis)
                row (when (and (sequential? rows) (= 1 (count rows))) (first rows))]
            (when-not (and (map? row) (= #{:xt/id :committed :system-time} (set (keys row)))
                           (= tx-id (:xt/id row)) (instance? Boolean committed?)
                           (= committed? (:committed row)) (instance? ZonedDateTime (:system-time row))
                           (= system-time (.toInstant ^ZonedDateTime (:system-time row))))
              (throw (ex-info "Selected pinned ledger disagrees with replay" {:reason :selected-pinned-ledger-mismatch})))
            {:tx-id tx-id :system-time system-time :committed? committed?}))
        selected history))

(defn- read-physical-visible! [node basis tables system valid]
  (into {} (map (fn [table]
                  (let [rows (original-query
                              node [(str "SELECT * FROM \"public\".\"" (name table)
                                         "\" FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? LIMIT 65537")
                                    valid system] basis)]
                    (when-not (and (sequential? rows) (<= (count rows) 65536)
                                   (every? #(and (map? %) (some? (:xt/id %))) rows)
                                   (= (count rows) (count (set (map :xt/id rows)))))
                      (throw (ex-info "Invalid historical physical rows" {:reason :invalid-physical-invariant-rows})))
                    [table (into {} (map (juxt :xt/id identity)) rows)]))) tables))

(defn- audit-physical!
  ([node coordinates cases inputs checked]
   (audit-physical! node coordinates cases inputs checked nil))
  ([node coordinates cases inputs checked transition-context]
  (let [base {:profile physical-profile :scope :original-selected-invariant-coordinates-only
              :provider-refinement :not-established :physical-invariant-theorem :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [ids (mapv :tx-id (:checked-boundaries checked))
            selected (vec (take-while #(<= (:tx-id %) (peek ids)) inputs))
            replay (original-replay (mapv #(dissoc % :tx-id) selected))
            tables (set (map (fn [[kind table]] (if (= :journal-slot kind) :gesso_commands table))
                             (vals coordinates)))
            token (snapshot-token! (original-query node "SHOW SNAPSHOT_TOKEN"))]
        (if-not (and (= :checked (:status checked)) (seq ids)
                     (= ids (mapv :tx-id (rest selected)))
                     (= :predicted (:status replay)) (seq tables)
                     (every? #(and (simple-keyword? %) (re-matches #"[a-z][a-z0-9_]*" (name %))) tables)
                     (string? token) (seq token))
          (fail :unsupported :unqualified-physical-invariant-readback)
          (let [basis {:snapshot-token token :current-time (Instant/now)}
                snapshot-time (snapshot-point! token selected)
                _ (confirm-basis! node basis)
                ledger (check-selected-ledger! node basis selected (:history replay))
                initial (second selected)
                points (into [{:tx-id (:tx-id initial) :side :initial
                               :system-time (:system-time initial) :valid-time (:system-time initial)}]
                             (mapcat (fn [[prior tx]]
                                       [{:tx-id (:tx-id tx) :side :prestate
                                         :system-time (:system-time prior) :valid-time (:system-time tx)}
                                        {:tx-id (:tx-id tx) :side :poststate
                                         :system-time (:system-time tx) :valid-time (:system-time tx)}])
                                     (partition 2 1 (rest selected))))
                project #(original-project coordinates % {:outcome-cases cases})]
            (loop [[point & remaining] points prior nil images {}]
              (if-not point
                (do (confirm-basis! node basis)
                    (cond-> (assoc base :status :matched :correspondence :matched-selected-physical-projections
                       :checked-coordinates points :invariant-ids (:invariant-ids checked)
                       :snapshot-correspondence
                       {:profile snapshot-profile :status :matched
                        :scope :selected-pinned-provider-observations-only
                        :snapshot-system-time snapshot-time :database "xtdb"
                        :token-echo :matched-before-and-after
                        :ledger ledger :checked-coordinates points
                        :valid-time-continuity :matched-prior-poststate-to-next-prestate
                        :provider-refinement :not-established :proof-discharge :not-performed
                        :authority :none :certificate :not-issued}
                       :projection-correspondence
                       {:profile projection/profile :status :matched
                        :scope :selected-supplied-visible-rows-only
                        :checking :independent-coordinate-and-closed-outcome-decoding
                        :checked-coordinates points :field-coordinates coordinates
                        :closed-outcome-case-count (count cases) :closed-outcome-cases cases
                        :record-validation :shared-canonical-record-validator
                        :provider-refinement :not-established :universal-projection :not-established
                        :proof-discharge :not-performed :authority :none :certificate :not-issued})
                      transition-context
                      (assoc :transition-correspondence
                             (original-selected-transition
                              (assoc transition-context :coordinates coordinates :cases cases
                                     :inputs selected :checked checked :ledger ledger :points points :images images)))))
                (let [{:keys [system-time valid-time side]} point
                      retained-rows (original-visible replay system-time valid-time)
                      physical-rows (read-physical-visible! node basis tables system-time valid-time)
                      retained-state (project retained-rows)
                      physical-state (project physical-rows)]
                  (cond
                    (not= retained-state (original-independent-project coordinates retained-rows cases))
                    (fail :mismatch :independent-retained-projection-mismatch)
                    (not= physical-state (original-independent-project coordinates physical-rows cases))
                    (fail :mismatch :independent-physical-projection-mismatch)
                    (not= retained-state physical-state)
                    (fail :mismatch :physical-invariant-projection-mismatch)
                    (and (= :prestate side) (not= prior physical-state))
                    (fail :mismatch :selected-physical-valid-time-continuity-mismatch)
                    :else (recur remaining physical-state
                                 (cond-> images transition-context
                                   (assoc point {:retained retained-rows :physical physical-rows}))))))))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :unsupported-physical-invariant-projection)))
      (catch Exception _ (fail :unavailable :physical-invariant-readback-failed))))))

(def conditional-physical-profile :selected-conditional-physical-invariants-v1)

(defn- derive-physical-invariants [checked physical inputs]
  (let [base {:profile conditional-physical-profile
              :scope :original-selected-invariant-coordinates-only
              :physical-invariant-theorem :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :premise-acceptance :not-performed :authority :none :certificate :not-issued}
        ids (mapv :tx-id (:checked-boundaries checked))
        selected (when (and (seq ids) (every? integer? ids) (vector? inputs)
                            (every? #(integer? (:tx-id %)) inputs))
                   (vec (take-while #(<= (:tx-id %) (peek ids)) inputs)))
        initial (second selected)
        expected (when (and (seq ids) (= ids (mapv :tx-id (rest selected)))
                            (apply < (map :tx-id selected))
                            (every? #(instance? Instant (:system-time %)) selected))
                   (into [{:tx-id (:tx-id initial) :side :initial
                           :system-time (:system-time initial) :valid-time (:system-time initial)}]
                         (mapcat (fn [[prior tx]]
                                   [{:tx-id (:tx-id tx) :side :prestate
                                     :system-time (:system-time prior) :valid-time (:system-time tx)}
                                    {:tx-id (:tx-id tx) :side :poststate
                                     :system-time (:system-time tx) :valid-time (:system-time tx)}])
                                 (partition 2 1 (rest selected)))))
        invariant-ids (:invariant-ids checked)]
    (if (and (= profile (:profile checked)) (= :checked (:status checked))
             (= :original-selected-transaction-boundaries-only (:scope checked))
             (= :declared-invariants-hold-on-selected-replay-projections (:conclusion checked))
             (= :direct-on-selected-replay-projections (:invariant-checking checked))
             (= :checked-at-selected-transaction-boundaries (:projection-continuity checked))
             (= :finite-direct-evaluation-not-inductive-theorem (:derivation checked))
             (= physical-profile (:profile physical)) (= :matched (:status physical))
             (= :original-selected-invariant-coordinates-only (:scope physical))
             (= :matched-selected-physical-projections (:correspondence physical))
             (set? invariant-ids) (seq invariant-ids) (= invariant-ids (:invariant-ids physical))
             (= snapshot-profile (get-in physical [:snapshot-correspondence :profile]))
             (= :matched (get-in physical [:snapshot-correspondence :status]))
             (= :matched-before-and-after (get-in physical [:snapshot-correspondence :token-echo]))
             (= :matched-prior-poststate-to-next-prestate (get-in physical [:snapshot-correspondence :valid-time-continuity]))
             (= "xtdb" (get-in physical [:snapshot-correspondence :database]))
             (instance? Instant (get-in physical [:snapshot-correspondence :snapshot-system-time]))
             (seq expected)
             (not (.isBefore ^Instant (get-in physical [:snapshot-correspondence :snapshot-system-time])
                             ^Instant (:system-time (peek selected))))
             (= (mapv #(select-keys % [:tx-id :system-time]) selected)
                (mapv #(select-keys % [:tx-id :system-time]) (get-in physical [:snapshot-correspondence :ledger])))
             (= (into [true] (map #(not= :aborted-stutter (:relation %)) (:checked-boundaries checked)))
                (mapv :committed? (get-in physical [:snapshot-correspondence :ledger])))
             (= projection/profile (get-in physical [:projection-correspondence :profile]))
             (= :matched (get-in physical [:projection-correspondence :status]))
             (seq expected) (= expected (:checked-coordinates physical)
                               (get-in physical [:projection-correspondence :checked-coordinates])
                               (get-in physical [:snapshot-correspondence :checked-coordinates])))
      (assoc base :status :conditional
             :conclusion :declared-invariants-hold-on-selected-physical-projections
             :derivation :finite-invariant-evaluation-and-projection-equality
             :conditional-on #{:faithful-pinned-provider-readbacks
                               :faithful-authoritative-state-projection
                               :faithful-invariant-expression-semantics}
             :invariant-ids invariant-ids :checked-coordinates expected
             :unestablished-implications #{:coverage-of-unselected-valid-times
                                           :coverage-of-unselected-transactions
                                           :universal-writer-coverage
                                           :universal-physical-refinement})
      (assoc base :status :not-established :reason :unqualified-physical-invariant-evidence))))

(defn audit-issued-selection!
  "Check finite projected states from the exact issued sample, not caller
   supplied valuations. The caller must have independently matched the
   activation root, issued catalog/decoder and selected physical trajectory.
   Only that caller may retain this descriptive report in an issued cut.
   Check Init, continuity, committed abstract transitions, abort/maintenance
   stuttering and each declared invariant. No assertion about later suffix,
   arbitrary valid instants, physical provider truth or all executions."
  [node retained admission source declarations steps observation comparison]
  (if-not (boundaries-current?)
    (report :unsupported :invariant-boundary-replaced)
    (try
      (let [sample (original-reader node retained)
            root (:activation-root (original-observation admission observation))
            initializer (get-in root [:root :initializer])
            catalog (get-in source [:inductive-source :catalog])]
        (if-not (and sample (vector? initializer)
                     (= :matched (:status comparison))
                     (= :matched-issued-declarations (:issued-admission-binding comparison))
                     (= :matched-issued-closed-construction (:closed-decoder-binding comparison))
                     (= :matched-local-activated-retained-root (:activation-root-binding comparison)))
          (report :unsupported :unqualified-invariant-source)
          (let [checked (original-initialization admission initializer)
                cases (:cases (original-cases source declarations))
                r (audit-replay! catalog (:target-map source) cases (:state checked)
                                 (:replay-inputs sample) comparison steps)
                physical (when (= :checked (:status r))
                           (audit-physical! node (:target-map source) cases (:replay-inputs sample) r
                                            {:catalog catalog :steps steps :comparison comparison}))
                catalog-refinement (when (= :checked (:status r))
                                     (original-expression-catalog catalog (:catalog-fingerprint checked)))
                catalog-projection (when (= :checked (:status r))
                                     (original-projection-catalog source declarations (:catalog-fingerprint checked) cases))]
            (cond
              (not (boundaries-current?)) (report :unsupported :invariant-boundary-replaced)
              (not= :checked (:status r)) r
              :else (assoc r :retained-payload-fingerprint (:payload-fingerprint sample)
                           :catalog-fingerprint (:catalog-fingerprint checked)
                           :physical-readbacks physical
                           :catalog-expression-refinement catalog-refinement
                           :catalog-projection-correspondence catalog-projection
                           :conditional-physical-invariant
                           (derive-physical-invariants r physical (:replay-inputs sample)))))))
      (catch clojure.lang.ExceptionInfo e
        (report :unsupported (or (:reason (ex-data e)) :unsupported-invariant-projection))))))
