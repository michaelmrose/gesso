(ns gesso.assurance.reference-recovery-invariant
  "Finite selected replay checks, not a universal physical invariant theorem."
  (:require [gesso.assurance.proof.independent-transition-semantics :as transition]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-recovery-expression :as expression]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.rule-eval :as production]
            [gesso.assurance.transition :as construction]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.inductive-plan :as induction]
            [xtdb.api :as xt])
  (:import [java.time Instant]))

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
(def ^:private original-expression-catalog expression/check-catalog!)
(def ^:private original-expression-boundaries expression/boundaries-current?)

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
       (identical? original-expression-catalog expression/check-catalog!)
       (identical? original-expression-boundaries expression/boundaries-current?)
       (original-expression-boundaries)))

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

(def physical-profile :selected-pinned-invariant-readbacks-v1)

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

(defn- audit-physical! [node coordinates cases inputs checked]
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
            tokens (original-query node "SHOW SNAPSHOT_TOKEN")
            token (when (and (sequential? tokens) (= 1 (count tokens))
                             (= #{:snapshot-token} (set (keys (first tokens)))))
                    (:snapshot-token (first tokens)))]
        (if-not (and (= :checked (:status checked)) (seq ids)
                     (= ids (mapv :tx-id (rest selected)))
                     (= :predicted (:status replay)) (seq tables)
                     (every? #(and (simple-keyword? %) (re-matches #"[a-z][a-z0-9_]*" (name %))) tables)
                     (string? token) (seq token))
          (fail :unsupported :unqualified-physical-invariant-readback)
          (let [basis {:snapshot-token token :current-time (Instant/now)}
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
            (if (every? (fn [{:keys [system-time valid-time]}]
                          (= (project (original-visible replay system-time valid-time))
                             (project (read-physical-visible! node basis tables system-time valid-time))))
                        points)
              (assoc base :status :matched :correspondence :matched-selected-physical-projections
                     :checked-coordinates points :invariant-ids (:invariant-ids checked))
              (fail :mismatch :physical-invariant-projection-mismatch)))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :unsupported-physical-invariant-projection)))
      (catch Exception _ (fail :unavailable :physical-invariant-readback-failed)))))

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
             (seq expected) (= expected (:checked-coordinates physical)))
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
                           (audit-physical! node (:target-map source) cases (:replay-inputs sample) r))
                catalog-refinement (when (= :checked (:status r))
                                     (original-expression-catalog catalog (:catalog-fingerprint checked)))]
            (cond
              (not (boundaries-current?)) (report :unsupported :invariant-boundary-replaced)
              (not= :checked (:status r)) r
              :else (assoc r :retained-payload-fingerprint (:payload-fingerprint sample)
                           :catalog-fingerprint (:catalog-fingerprint checked)
                           :physical-readbacks physical
                           :catalog-expression-refinement catalog-refinement
                           :conditional-physical-invariant
                           (derive-physical-invariants r physical (:replay-inputs sample)))))))
      (catch clojure.lang.ExceptionInfo e
        (report :unsupported (or (:reason (ex-data e)) :unsupported-invariant-projection))))))
