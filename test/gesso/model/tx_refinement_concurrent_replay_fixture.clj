(ns gesso.model.tx-refinement-concurrent-replay-fixture
  "Bounded, reusable single-node transaction contention fixtures.

   A schedule is a vector of *waves*. Each wave releases its independently
   prepared plans through one CountDownLatch; the next wave is not released
   until all calls in the preceding wave finish. Singleton waves give a
   controlled program order between calls. Multiple calls in one wave are
   deliberately concurrent and may execute in any physical order.

   Every candidate plan is checked with the restricted serial reference model
   and lowered by the real Gesso transaction planner before submissions. The
   resulting observations are compared with serial paths compatible with
   wave barriers, including the final whole-document projection supplied by
   the caller. Caller-provided readback and submission results are NOT
   independently authenticated physical evidence. Neither event recording
   nor wave release establishes an actual XTDB serialization order.

   No proof, certificate, retries, settlement authority, or universal physical
   refinement follows from a matching bounded run. This is test infrastructure,
   not a new production transaction interface."
  (:require
   [clojure.set :as set]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt])
  (:import
   [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private max-plans 6)
(def ^:private max-runs 16)
(def ^:private default-timeout-ms 60000)

(defn- invalid! [reason message & [details]]
  (throw (ex-info message (merge {:error/kind ::invalid-replay
                                  :reason reason}
                                 details))))

(defn- valid-target? [target]
  (and (vector? target)
       (= 2 (count target))
       (keyword? (first target))
       (some? (second target))))

(defn- checked-schedule! [schedule n]
  (when-not (and (vector? schedule)
                 (seq schedule)
                 (every? (fn [wave]
                           (and (vector? wave) (seq wave)
                                (every? #(and (integer? %)
                                              (<= 0 %)
                                              (< % n)) wave)))
                         schedule)
                 (= (vec (range n))
                    (sort (mapcat identity schedule))))
    (invalid! :invalid-schedule
              "Every plan index must occur once in nonempty, ordered release waves"
              {:plan-count n}))
  schedule)

(defn- required-targets [initial-state plans]
  (into (set (keys initial-state))
        (mapcat (fn [plan]
                  (concat (map command/target (:commands plan))
                          (map command/guard-target (:guards plan))))
                plans)))

(defn- checked-scenario! [scenario schedule]
  (when-not (map? scenario)
    (invalid! :invalid-scenario "A replay scenario must be a map"))
  (let [{:keys [initial-state plans targets submit! readback!]} scenario]
    (model/require-state initial-state)
    (when-not (and (vector? plans)
                   (<= 1 (count plans) max-plans))
      (invalid! :invalid-plans "A scenario needs one to six plans"))
    (checked-schedule! schedule (count plans))
    ;; This fixture intentionally excludes non-canonical SQL assertions. The
    ;; serial oracle admits only exact generated field-count assertions, but
    ;; this fixture does not independently parse their target dependencies.
    (let [admitted (mapv model/admitted-plan plans)]
      (when (some (comp seq :assertions) admitted)
        (invalid! :unsupported-assertions
                  "Replay fixture cannot establish assertion target coverage"))
      (when-not (and (vector? targets)
                     (= (count targets) (count (distinct targets)))
                     (every? valid-target? targets)
                     (set/subset? (required-targets initial-state admitted)
                                  (set targets)))
        (invalid! :incomplete-targets
                  "Readback targets must uniquely cover initial, guarded and written documents"))
      (when-not (and (fn? submit!) (fn? readback!))
        (invalid! :missing-io "Replay requires callable submit! and readback! functions"))
      {:initial-state initial-state
       :plans admitted
       :targets targets
       :submit! submit!
       :readback! readback! 
       :operations (mapv (fn [plan] (:tx-ops (tx/prepare {} plan))) admitted)})))

(defn xtdb-submitter
  "Create a strict, test-only executor for an embedded XTDB node.
   Only the concrete ASSERT rejection is interpreted as :rejected; every
   other exception propagates and fails the fixture instead of becoming an
   abstract noncommit. The public TransactionKey is not a payload receipt."
  [node]
  (when-not node
    (invalid! :missing-node "An XTDB test node is required"))
  (fn [_plan-index operations]
    (try
      {:status :committed :tx-key (xt/execute-tx node operations)}
      (catch Throwable error
        (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
          {:status :rejected :error/code :xtdb/assert-failed}
          (throw error))))))

(defn- checked-result! [result]
  (when-not (or (and (map? result)
                     (= #{:status :tx-key} (set (keys result)))
                     (= :committed (:status result))
                     (some? (:tx-key result)))
                (and (map? result)
                     (= #{:status :error/code} (set (keys result)))
                     (= :rejected (:status result))
                     (= :xtdb/assert-failed (:error/code result))))
    (invalid! :unexpected-submission-result
              "A submission must return a concrete committed key or typed ASSERT rejection"))
  result)

(defn- run-wave! [wave-index indices operations submit! timeout-ms]
  (let [ready (CountDownLatch. (count indices))
        release (CountDownLatch. 1)
        workers (mapv (fn [index]
                        (future
                          (.countDown ready)
                          (when-not (.await release timeout-ms TimeUnit/MILLISECONDS)
                            (invalid! :release-timeout "Submission release gate expired"))
                          (checked-result! (submit! index (nth operations index)))))
                      indices)]
    (try
      (when-not (.await ready timeout-ms TimeUnit/MILLISECONDS)
        (invalid! :ready-timeout "Not all workers reached the scheduled release gate"))
      (.countDown release)
      (let [deadline (+ (System/nanoTime) (* 1000000 timeout-ms))
            results (mapv (fn [worker]
                            (let [remaining (max 1 (quot (- deadline (System/nanoTime))
                                                         1000000))
                                  result (try
                                           (deref worker remaining ::timeout)
                                           (catch java.util.concurrent.ExecutionException error
                                             (throw (or (.getCause error) error))))]
                              (when (= ::timeout result)
                                (invalid! :submission-timeout
                                          "A submission failed to complete within the wave deadline"))
                              result))
                          workers)]
        (mapv (fn [index result]
                {:plan-index index :wave-index wave-index :result result})
              indices results))
      (finally
        (.countDown release)
        (doseq [worker workers]
          (when-not (realized? worker) (future-cancel worker)))))))

(defn- model-path-allowed? [schedule path]
  (let [wave-by-index (into {}
                            (mapcat (fn [wave-index indices]
                                      (map (fn [index] [index wave-index]) indices))
                                    (range) schedule))
        order (:order path)]
    (apply <= (mapv wave-by-index order))))

(defn- state-from-readback! [targets state]
  (model/require-state state)
  (when-not (set/subset? (set (keys state)) (set targets))
    (invalid! :out-of-scope-readback
              "The observed state includes an undeclared target"))
  state)

(defn replay!
  "Execute one bounded wave schedule against externally prepared scenario.

   Scenario keys: :initial-state, :plans, :targets (including absent creates),
   :submit! [plan-index tx-ops], and :readback! [targets] -> state map.
   Callers are responsible for installing an isolated fresh initial database
   state before calling this function. Only valid serial paths that respect
   completed-wave barriers are eligible for comparison. Results are retained
   in original plan order, not worker completion order.

   A negative :matches-serial-reference? remains observable for diagnostics;
   use require-correspondence! when a mismatch must fail the calling test."
  ([scenario schedule]
   (replay! scenario schedule {}))
  ([scenario schedule {:keys [timeout-ms]
                       :or {timeout-ms default-timeout-ms}}]
   (when-not (and (integer? timeout-ms) (<= 1 timeout-ms 120000))
     (invalid! :invalid-timeout "A bounded positive timeout is required"))
   (let [{:keys [initial-state plans targets submit! readback! operations]}
         (checked-scenario! scenario schedule)
         actual-entries (mapcat (fn [[wave-index indices]]
                                  (run-wave! wave-index indices operations
                                             submit! timeout-ms))
                                (map-indexed vector schedule))
         by-index (into {} (map (juxt :plan-index identity) actual-entries))
         results (mapv (fn [index] (:result (get by-index index)))
                       (range (count plans)))
         observed-state (state-from-readback! targets (readback! targets))
         possible (->> (:paths (model/explore-orders initial-state plans))
                       (filter (partial model-path-allowed? schedule))
                       (map (fn [path]
                              {:statuses (reduce (fn [statuses step]
                                                   (assoc statuses (:plan-index step)
                                                          (:status step)))
                                                 (vec (repeat (count plans) nil))
                                                 (:trace path))
                               :state (:final-state path)}))
                       set)
         actual {:statuses (mapv :status results)
                 :state observed-state}]
     {:type ::replay-report
      :version 1
      :release-waves schedule
      :observed-per-plan results
      :observed-state observed-state
      :matching-serial-path? (contains? possible actual)
      :eligible-serial-outcomes (count possible)
      :plan-count (count plans)
      :scope :bounded-single-node-reference-comparison
      :readback-origin :caller-supplied
      :actual-xtdb-order :not-established
      :transaction-causality :not-established
      :complete-write-coverage :not-established
      :proof-status :diagnostic-only
      :certificate :not-issued
      :authority :none})))

(defn require-correspondence!
  "Fail a test when no admitted, wave-compatible serial path explains a run."
  [report]
  (when-not (and (= ::replay-report (:type report))
                 (= 1 (:version report))
                 (true? (:matching-serial-path? report)))
    (invalid! :no-admitted-serial-path
              "The observed outcomes and state have no eligible serial explanation"
              {:plan-count (:plan-count report)
               :eligible-serial-outcomes (:eligible-serial-outcomes report)}))
  report)

(defn replay-matrix!
  "Replay at most sixteen schedules, creating a FRESH scenario for each run.

   scenario! takes [run-index schedule] and must seed new isolated targets;
   this fixture cannot establish that the caller truly isolated those runs."
  [scenario! schedules opts]
  (when-not (and (fn? scenario!)
                 (vector? schedules)
                 (<= 1 (count schedules) max-runs))
    (invalid! :invalid-matrix "A bounded vector of schedules and factory is required"))
  (mapv (fn [[run-index schedule]]
          (require-correspondence!
           (replay! (scenario! run-index schedule) schedule opts)))
        (map-indexed vector schedules)))
