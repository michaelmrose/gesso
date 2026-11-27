(ns gesso.assurance.reference-transition-refinement
  "Conditional local simulation for the closed mapped command fragment.
   Supplied row images are not authenticated provider observations."
  (:require [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.proof.mapped-prestate-semantics :as prestate]
            [gesso.assurance.proof.independent-transition-semantics :as transition]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.transition :as construction]))

(def profile :conditional-closed-mapped-transition-refinement-v1)
(def ^:private original-writes writes/derive-transition!)
(def ^:private original-frequencies writes/put-row-frequencies!)
(def ^:private original-prestate prestate/describe!)
(def ^:private original-projection projection/project!)
(def ^:private original-observe values/observe-state!)
(def ^:private original-build construction/build-guarded-replacement!)
(def ^:private original-transition transition/observe-transition!)

(defn- boundaries-current? []
  (and (identical? original-writes writes/derive-transition!)
       (identical? original-frequencies writes/put-row-frequencies!)
       (identical? original-prestate prestate/describe!)
       (identical? original-projection projection/project!)
       (identical? original-observe values/observe-state!)
       (identical? original-build construction/build-guarded-replacement!)
       (identical? original-transition transition/observe-transition!)
       (projection/boundaries-current?)))

(defn- reject! [reason]
  (throw (ex-info "Unqualified closed mapped transition" {:reason reason})))

(defn- row-image! [original coordinates docs journal]
  ;; The existing closed-row predicate checker rejects unrelated targets,
  ;; duplicate slots and missing required documents before indexing.
  (original-prestate original coordinates docs journal)
  (into {:gesso_commands (into {} (map (juxt :xt/id identity)) journal)}
        (map (fn [[table rows]] [table (into {} (map (juxt :xt/id identity)) rows)])) docs))

(defn- supplied-image! [original coordinates image]
  (when-not (and (map? image) (contains? image :gesso_commands)
                 (every? (fn [[_ rows]]
                           (and (map? rows) (every? (fn [[id row]] (and (map? row) (= id (:xt/id row)))) rows))) image))
    (reject! :invalid-supplied-row-image))
  (let [docs (into {} (map (fn [[table rows]] [table (vec (vals rows))])) (dissoc image :gesso_commands))
        journal (vec (vals (:gesso_commands image)))]
    (when-not (= image (row-image! original coordinates docs journal))
      (reject! :invalid-supplied-row-image)))
  image)

(defn- put-image! [before suffix]
  ;; The closed suffix is ordinary default-valid-time puts, one row per
  ;; physical identity. No deletes, explicit valid ranges or repeated targets.
  (when-not (and (vector? suffix) (seq suffix)) (reject! :invalid-mapped-put-suffix))
  (let [rows (mapcat (fn [[op table & rows :as put]]
                      (when-not (and (vector? put) (= :put-docs op)
                                     (contains? before table) (seq rows)
                                     (every? #(and (map? %) (string? (:xt/id %))) rows))
                        (reject! :invalid-mapped-put-suffix))
                      (map (fn [row] [[table (:xt/id row)] row]) rows)) suffix)]
    (when-not (= (count rows) (count (set (map first rows))))
      (reject! :duplicate-physical-put-target))
    (reduce (fn [image [address row]] (assoc-in image address row)) before rows)))

(defn- typed-projection! [original coordinates image cases]
  (let [state (original-projection coordinates image cases)
        types (:state-types original)]
    (when-not (and (= (set (keys types)) (set (keys state)))
                   (= (set (keys types)) (:present-readers (original-observe types state))))
      (reject! :incomplete-typed-projection))
    state))

(defn check-step!
  "Check one source-derived mapped put suffix and supplied actual row images.
   For a commit the successful full prestate guard is modeled by equality
   with the prepared image; independent exact puts must produce the entire
   post image and its projection must satisfy the original abstract relation.
   For an abort the attempted suffix still qualifies against the prepared
   image, but actual images must stutter. An abort need not have a false
   abstract guard: the provider may abort for another reason.

   This is a source-relative instance of a conditional local simulation,
   not proof of SQL guard semantics, atomic provider execution, attribution,
   valid-time continuity or complete admitted-writer coverage."
  [original coordinates docs journal spec suffix commitment before after]
  (let [base {:profile profile :scope :supplied-closed-mapped-command-rows-only
              :provider-refinement :not-established :writer-coverage :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (when-not (and (boundaries-current?) (#{:committed :aborted} commitment)
                     (map? (:outcome-cases spec)) (seq (:outcome-cases spec)))
        (reject! :unqualified-transition-boundary))
      (let [prepared (row-image! original coordinates docs journal)
            before (supplied-image! original coordinates before)
            after (supplied-image! original coordinates after)
            expected (original-writes original coordinates docs journal spec)
            prepared-post (put-image! prepared suffix)
            expected-post (put-image! prepared (:puts expected))
            cases (:outcome-cases spec)
            pre (typed-projection! original coordinates before cases)
            post (typed-projection! original coordinates after cases)]
        (when-not (and (= (original-frequencies suffix)
                          (original-frequencies (:puts expected)))
                       (= prepared-post expected-post)
                       (= (:before expected) (typed-projection! original coordinates prepared cases))
                       (= (:after expected) (typed-projection! original coordinates prepared-post cases)))
          (reject! :source-derived-put-mismatch))
        (case commitment
          :committed
          (do (when-not (= prepared before) (reject! :committed-prestate-guard-mismatch))
              (when-not (= prepared-post after) (reject! :committed-row-effect-mismatch))
              (when-not (and (= pre (:before expected)) (= post (:after expected))
                             (true? (:matches? (original-transition
                                                (:transition (original-build original))
                                                pre (:inputs spec) post))))
                (reject! :abstract-transition-mismatch)))
          :aborted
          (when-not (= before after) (reject! :aborted-row-effect-mismatch)))
        (when-not (boundaries-current?) (reject! :transition-boundary-replaced))
        (assoc base :status :conditional-step-correspondence
               :alternative-id (get-in original [:subject :id]) :commitment commitment
               :relation (if (= commitment :committed) :committed-abstract-transition :aborted-stutter)
               :checked-row-effect (if (= commitment :committed) :exact-source-derived-puts :exact-row-stutter)
               :before pre :after post
               :conditions #{:faithful-closed-prestate-guard-semantics
                             :faithful-ordered-atomic-default-valid-time-puts
                             :faithful-abort-atomicity
                             :faithful-command-and-envelope-attribution
                             :faithful-authoritative-projection-and-expression-semantics
                             :complete-admitted-writer-and-temporal-boundary-coverage}))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-transition))))))

(def selected-profile :selected-retained-mapped-transition-refinement-v1)
(def ^:private original-step check-step!)

(defn- mapped-tables [coordinates]
  (set (map (fn [[kind table]] (if (= :journal-slot kind) :gesso_commands table))
            (vals coordinates))))

(defn- mapped-image [tables rows]
  ;; Replay omits empty tables; provider queries include them. Normalize
  ;; only that distinction, never missing fields or supplemental values.
  (when-not (map? rows) (reject! :invalid-selected-row-image))
  (into {} (map (fn [table] [table (get rows table {})])) tables))

(defn- selected-points [ledger]
  (let [{:keys [tx-id system-time]} (second ledger)]
    (into [{:tx-id tx-id :side :initial :system-time system-time :valid-time system-time}]
          (mapcat (fn [[prior tx]]
                    [{:tx-id (:tx-id tx) :side :prestate :system-time (:system-time prior)
                      :valid-time (:system-time tx)}
                     {:tx-id (:tx-id tx) :side :poststate :system-time (:system-time tx)
                      :valid-time (:system-time tx)}]))
          (partition 2 1 (rest ledger)))))

(defn check-selected!
  "Check the original selected mapped transaction sequence over independently
   acquired row images. The caller must already have qualified the entire
   issued envelope, selected maintenance linkage, and pinned ledger. Exact
   equality with the retained expectation binds the mapped suffix; this
   function does not itself validate publication or residual assertions.
   Row observations and comparison reports are premises, not authenticated
   capabilities. Only the issued-cut capture may retain the result."
  [{:keys [catalog coordinates cases inputs checked comparison steps ledger points images]}]
  (let [base {:profile selected-profile :scope :original-selected-mapped-row-boundaries-only
              :provider-refinement :not-established :writer-coverage :not-established
              :universal-refinement :not-established :proof-discharge :not-performed
              :authority :none :certificate :not-issued}]
    (try
      (when-not (and (boundaries-current?) (identical? original-step check-step!)
                     (= :checked (:status checked)) (= :matched (:status comparison))
                     (= :matched-issued-declarations (:issued-admission-binding comparison))
                     (= :matched-issued-closed-construction (:closed-decoder-binding comparison))
                     (= :matched-local-activated-retained-root (:activation-root-binding comparison))
                     (= :matched-local-observed-retained-steps (:submission-recovery-linkage comparison))
                     (= [:matched-source-relative-record :matched-uncommitted-record :matched-source-relative-record]
                        (:executor-envelopes comparison))
                     (map? cases) (seq cases) (vector? steps) (= 3 (count steps))
                     (vector? ledger) (<= 5 (count ledger)) (vector? inputs)
                     (= (mapv #(select-keys % [:tx-id :system-time]) inputs)
                        (mapv #(select-keys % [:tx-id :system-time]) ledger))
                     (= (mapv :tx-id (rest ledger)) (mapv :tx-id (:checked-boundaries checked)))
                     (every? integer? (map :tx-id ledger))
                     (apply < (map :tx-id ledger))
                     (= [(:manifest-tx-id comparison) (:initialization-tx-id comparison)]
                        (mapv :tx-id (take 2 ledger)))
                     (every? true? (map :committed? (take 2 ledger)))
                     (= :checked-initial-projection (get-in checked [:checked-boundaries 0 :relation]))
                     (= [true false true]
                        (mapv (into {} (map (juxt :tx-id :committed?)) ledger) (:selected-tx-ids comparison)))
                     (= (:selected-tx-ids comparison) (:selected-tx-ids checked)
                        (mapv #(get-in % [:expectation :tx-id]) steps))
                     (= [:committed :aborted :committed] (:selected-commitments comparison))
                     (= (mapv :tx-id (:intervening-maintenance comparison))
                        (mapv :tx-id (filter #(= :maintenance-stutter (:relation %)) (:checked-boundaries checked))))
                     (= points (selected-points ledger))
                     (map? images) (= (count points) (count images)) (= (set points) (set (keys images))))
        (reject! :unqualified-selected-transition-sequence))
      (let [tables (mapped-tables coordinates)
            by-id (into {} (map (juxt :tx-id identity)) inputs)
            by-step (into {} (map (juxt #(get-in % [:expectation :tx-id]) identity)) steps)
            by-operation (into {} (map (juxt :id :construction)) (:operations catalog))
            types (get-in catalog [:operations 0 :construction :state-types])
            original (get-in catalog [:operations 0 :construction])
            row-images (mapv (fn [point]
                               (let [{:keys [retained physical]} (images point)
                                     retained (mapped-image tables retained)
                                     physical (mapped-image tables physical)]
                                 (when-not (= retained physical)
                                   (reject! :selected-complete-mapped-row-mismatch))
                                 (supplied-image! original coordinates physical))) points)
            reports
            (loop [prior (first row-images) i 0 reports []]
              (if (= i (- (count ledger) 2)) reports
                  (let [{:keys [tx-id relation alternative-id]} (nth (:checked-boundaries checked) (inc i))
                        tx (by-id tx-id)
                        entry (nth ledger (+ i 2))
                        before (nth row-images (inc (* 2 i)))
                        after (nth row-images (+ 2 (* 2 i)))
                        step (by-step tx-id)
                        committed? (:committed? entry)]
                    (when-not (and (= prior before) (= tx-id (:tx-id entry))
                                   (instance? Boolean committed?))
                      (reject! :selected-complete-row-continuity-mismatch))
                    (let [r (if (= :maintenance-stutter relation)
                              (do (when-not (and (nil? step) committed? (= before after))
                                    (reject! :selected-maintenance-row-stutter-mismatch))
                                  {:relation relation :checked-row-effect :exact-mapped-row-stutter})
                              (let [{:keys [original targets docs journal spec expectation]} step
                                    id (:alternative spec)
                                    ops (:ops tx)
                                    suffix (filterv #(and (vector? %) (= :put-docs (first %))
                                                          (contains? tables (second %))) ops)]
                                (when-not (and step (= tx expectation) (= coordinates targets)
                                               (= cases (:outcome-cases spec)) (= original (by-operation id))
                                               (= types (:state-types original))
                                               (vector? ops) (every? vector? ops)
                                               (every? #(or (not= :put-docs (first %)) (simple-keyword? (second %))) ops)
                                               (= relation (if committed? :committed-abstract-transition :aborted-stutter))
                                               (or (not committed?) (= alternative-id id)))
                                  (reject! :selected-retained-command-envelope-mismatch))
                                (let [r (original-step original coordinates docs journal spec suffix
                                                       (if committed? :committed :aborted) before after)]
                                  (when-not (and (= profile (:profile r)) (= :conditional-step-correspondence (:status r))
                                                 (= id (:alternative-id r)) (= relation (:relation r)))
                                    (reject! (or (:reason r) :selected-command-simulation-mismatch)))
                                  (select-keys r [:relation :alternative-id :checked-row-effect :conditions]))))]
                      (recur after (inc i) (conj reports (assoc r :tx-id tx-id)))))))]
        (when-not (and (boundaries-current?) (identical? original-step check-step!))
          (reject! :transition-boundary-replaced))
        (assoc base :status :conditional-selected-transition-correspondence
               :selected-tx-ids (:selected-tx-ids checked) :checked-coordinates points
               :steps reports :field-coordinates coordinates
               :row-correspondence :exact-complete-mapped-tables
               :envelope-binding :exact-retained-expectation-with-qualified-issued-envelope-premise
               :maintenance-scope :mapped-tables-only
               :conditions (into #{:faithful-pinned-provider-readbacks
                                   :faithful-issued-envelope-and-maintenance-correspondence}
                                 (mapcat :conditions) reports)))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-selected-transition)))
      (catch Exception _
        (assoc base :status :not-established :reason :invalid-selected-transition-input)))))
