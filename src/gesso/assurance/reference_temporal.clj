(ns gesso.assurance.reference-temporal
  "Independent finite bitemporal replay of selected document puts.
   System times are supplied as inputs, never inferred from provider results."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.reference-clock :as clock])
  (:import (java.time Instant ZonedDateTime)))

(def profile :selected-bitemporal-transaction-v12)

(defn- instant? [x]
  (and (instance? Instant x) (zero? (mod (.getNano ^Instant x) 1000))))

(defn- before? [^Instant a ^Instant b] (.isBefore a b))

(defn- reject! [reason]
  (throw (ex-info "Unsupported temporal reference input" {::unsupported true :reason reason})))

(defn- before-end? [instant end]
  ;; Internal nil represents an unbounded exclusive end, not a SQL null.
  (or (nil? end) (before? instant end)))

;; Exact transaction-clock guards emitted by the outbox lease worker.
(def ^:private clock-guards
  {"ASSERT CURRENT_TIMESTAMP < ?" :future
   "ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" :within-five-minutes})

(defn- clock-instant? [x]
  ;; Conservative portable timestamp domain; exclude precision loss and
  ;; extreme JVM instants before doing arithmetic or comparing deadlines.
  (and (instant? x)
       (not (before? x (Instant/parse "0001-01-01T00:00:00Z")))
       (before? x (Instant/parse "9999-01-01T00:00:00Z"))))

(defn- check-clock-guard! [system-time op]
  (when-not (and (= 2 (count op)) (clock-instant? system-time)
                 (clock-instant? (second op)))
    (reject! :unsupported-temporal-clock-guard)))

(defn- clock-matches? [system-time [sql deadline]]
  (case (get clock-guards sql)
    :future (before? system-time deadline)
    :within-five-minutes (not (before? (.plusSeconds ^Instant system-time 300) deadline))))

(def ^:private version-guards
  {"ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" :version
   "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)" :expired
   "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)" :active})

(defn- version-instant [x]
  (cond
    (instance? Instant x) x
    (instance? ZonedDateTime x) (.toInstant ^ZonedDateTime x)))

(defn- check-version-guard! [system-time op]
  (let [kind (get version-guards (first op))]
    (when-not (and (= (if (= :version kind) 3 4) (count op))
                   (uuid? (second op)) (clock-instant? system-time)
                   (clock-instant? (version-instant (nth op 2)))
                   (or (= :version kind)
                       (clock-instant? (version-instant (nth op 3)))))
      (reject! :unsupported-temporal-version-guard))))

(defn- version-matches! [working system-time [sql id version deadline]]
  (let [rows (get-in working [:segments :gesso_live_outbox])
        visible (fn [intervals]
                  (first (filter (fn [[a b _ _]]
                                   (and (not (before? system-time a))
                                        (before-end? system-time b))) intervals)))]
    (when (some (fn [[k intervals]] (and (not (uuid? k)) (visible intervals))) rows)
      (reject! :unsupported-temporal-version-id-domain))
    (and (= (version-instant version) (nth (visible (get rows id)) 3 nil))
         ;; The physical predicate binds the observed deadline; it does
         ;; not itself read the stored lease-until column.
         (case (get version-guards sql)
           :version true
           :expired (not (before? system-time (version-instant deadline)))
           :active (before? system-time (version-instant deadline))))))

(defn- normalize-row-put! [system-time opts docs]
  (let [table (:into opts)
        bound-keys [:xt/valid-from :xt/valid-to]]
    (when-not (and (= #{:into :row-valid-time?} (set (keys opts)))
                   (true? (:row-valid-time? opts)) (nil? (meta opts))
                   (<= 1 (count docs) 16384)
                   (every? #(and (map? %) (nil? (meta %))) docs))
      (reject! :unsupported-temporal-put))
    (let [plain (mapv #(apply dissoc % bound-keys) docs)
          decoded (current/execute {} (vector (into [:put-docs table] plain)))
          intervals (mapv (fn [doc]
                            (let [from (get doc :xt/valid-from system-time)
                                  to (:xt/valid-to doc)]
                              (when-not (and (instant? from)
                                             (or (not (contains? doc :xt/valid-to)) (instant? to))
                                             (before-end? from to))
                                (reject! :unsupported-temporal-put))
                              [from to])) docs)]
      (when-not (= :predicted (:status decoded)) (reject! :unsupported-temporal-put))
      (into [:put-docs {:into table :row-intervals intervals}]
            (map #(get-in decoded [:state table (:xt/id %)]) plain)))))

(defn- normalize-op! [system-time op]
  (when-not (and (vector? op) (nil? (meta op)))
    (reject! :unsupported-temporal-operation))
  (if (string? (first op))
    (do (cond
          (contains? clock-guards (first op)) (check-clock-guard! system-time op)
          (contains? version-guards (first op)) (check-version-guard! system-time op)
          :else nil)
        op)
    (let [[kind raw-opts & docs] op
          opts (if (keyword? raw-opts) {:into raw-opts} raw-opts)]
      (if (and (= :put-docs kind) (map? opts) (contains? opts :row-valid-time?))
        (normalize-row-put! system-time opts docs)
        (do
          (when-not (and (= :put-docs kind) (map? opts) (nil? (meta opts))
                         (contains? opts :into)
                         (every? #{:into :valid-from :valid-to} (keys opts)))
            (reject! :unsupported-temporal-put))
          (let [table (:into opts)
                from (get opts :valid-from system-time)
                to (:valid-to opts)
                decoded (current/execute {} [(into [:put-docs table] docs)])]
            (when-not (and (instant? from)
                           (or (not (contains? opts :valid-to)) (instant? to))
                           (before-end? from to)
                           (= :predicted (:status decoded)))
              (reject! :unsupported-temporal-put))
            (into [:put-docs {:into table :valid-from from :valid-to to}]
                  (map #(get-in decoded [:state table (:xt/id %)]) docs))))))))

(defn- current-assertion? [op]
  (and (string? (first op))
       (not (contains? clock-guards (first op)))
       (not (contains? version-guards (first op)))))

(defn- operation-groups [ops]
  ;; Consecutive ordinary assertions share the same tentative read basis.
  ;; Puts and temporal-only guards are barriers, never moved or combined.
  (into [] (mapcat (fn [run]
                     (if (current-assertion? (first run))
                       [(vec run)] (mapv vector run))))
        (partition-by current-assertion? ops)))

(defn- group-tables! [group]
  (let [op (first group)
        deps (cond
               (current-assertion? op) (current/operation-sequence-tables group)
               (= :put-docs (first op)) #{(:into (second op))}
               (contains? clock-guards (first op)) #{}
               (contains? version-guards (first op)) #{:gesso_live_outbox})]
    (when-not (set? deps) (reject! :unsupported-temporal-assertion))
    deps))

(defn- normalize-transaction! [{:keys [system-time ops] :as tx}]
  (let [normalized (mapv #(normalize-op! system-time %) ops)
        groups (operation-groups normalized)
        tables (reduce into #{} (map group-tables! groups))]
    (assoc tx :ops normalized :groups groups :tables tables)))

(defn required-observation-tables
  "Derive all attempted read/write table dependencies from a qualified
   temporal transaction sequence (not from reached reads or resulting
   rectangles). Return nil on unsupported operation shapes. This is a
   conservative observation scope, not a proof of history completeness."
  [transactions]
  (try
    (when (and (vector? transactions) (nil? (meta transactions))
               (<= 1 (count transactions) 256)
               (every? #(and (map? %) (nil? (meta %)) (instant? (:system-time %))
                             (vector? (:ops %)) (nil? (meta (:ops %)))
                             (<= 1 (count (:ops %)) 256)) transactions))
      (reduce into #{} (map (comp :tables normalize-transaction!) transactions)))
    (catch clojure.lang.ExceptionInfo e
      (if (::unsupported (ex-data e)) nil (throw e)))))

(defn- rectangle [table [from to doc origin] end]
  {:table table :document doc :valid-from from :valid-to to
   :system-from origin :system-to end})

(defn- earlier-end [a b]
  (cond (nil? a) b (nil? b) a (before? a b) a :else b))

(defn- overlay [segments table from to doc system-time]
  ;; Preserve issuing system time on unaffected pieces. Close only the
  ;; intersection that is overwritten, and never emit a zero-system-time
  ;; rectangle for an earlier put at the same system time.
  (let [{:keys [active closed]}
        (reduce (fn [acc [a b old origin :as segment]]
                  (if (or (not (before-end? a to)) (not (before-end? from b)))
                    (update acc :active conj segment)
                    (cond-> acc
                      (before? a from) (update :active conj [a from old origin])
                      (and to (before-end? to b)) (update :active conj [to b old origin])
                      (before? origin system-time)
                      (update :closed conj
                              (rectangle table [(if (before? a from) from a)
                                                (earlier-end b to) old origin]
                                         system-time)))))
                {:active [] :closed []} segments)]
    {:active (vec (sort-by first (conj active [from to doc system-time])))
     :closed closed}))

(defn- apply-put [state system-time [_ opts & docs]]
  (reduce (fn [s [i doc]]
            (let [table (:into opts)
                  [from to] (if-let [intervals (:row-intervals opts)]
                              (nth intervals i) [(:valid-from opts) (:valid-to opts)])
                  path [:segments table (:xt/id doc)]
                  {:keys [active closed]} (overlay (get-in s path) table from to doc system-time)]
              (-> s (assoc-in path active) (update :closed into closed))))
          state (map-indexed vector docs)))

(defn- active-rectangles [segments]
  (vec (for [[table rows] segments [_ intervals] rows segment intervals]
         (rectangle table segment nil))))

(defn- project-visible [segments valid-time]
  (into {} (keep (fn [[table rows]]
                   (let [visible (into {} (keep (fn [[id intervals]]
                                                 (when-let [[_ _ doc]
                                                            (first (filter (fn [[a b _]]
                                                                             (and (not (before? valid-time a))
                                                                                  (before-end? valid-time b))) intervals))]
                                                   [id doc]))) rows)]
                     (when (seq visible) [table visible])))) segments))

(defn- execute-transaction! [prior system-time groups]
  ;; Assertions without explicit temporal clauses read tentative documents
  ;; at the supplied transaction time, not all active intervals. Execute
  ;; each ordinary assertion run at that unchanged basis and translate its
  ;; local failure index back to the original physical transaction index.
  (loop [working prior i 0 remaining (seq groups)]
    (if-not remaining
      {:state working :committed? true}
      (let [group (first remaining)
            op (first group)
            advance (+ i (count group))]
        (if (= :put-docs (first op))
          (recur (apply-put working system-time op) advance (next remaining))
          (let [failed (cond
                         (contains? clock-guards (first op))
                         (when-not (clock-matches? system-time op) i)
                         (contains? version-guards (first op))
                         (when-not (version-matches! working system-time op) i)
                         :else
                         (let [result (current/execute (project-visible (:segments working) system-time) group)]
                           (when-not (= :predicted (:status result))
                             (reject! :unsupported-temporal-assertion-domain))
                           (when-not (:committed? result)
                             (+ i (:failed-assertion-index result)))))]
            (if (nil? failed)
              (recur working advance (next remaining))
              {:state prior :committed? false :failed-assertion-index failed})))))))

(def equal-time-profile :selected-bitemporal-equal-time-transaction-v1)

(def clocked-profile :selected-bitemporal-clocked-transaction-v1)

(defn- clocked-transactions! [inputs]
  (when-not (and (vector? inputs) (nil? (meta inputs)) (<= 1 (count inputs) 256))
    (reject! :unsupported-clocked-sequence))
  (loop [remaining (seq inputs) prior nil prior-id -1 txs []]
    (if (seq remaining)
      (let [r (first remaining)
            _ (when-not (map? r) (reject! :unsupported-clocked-transaction))
            {:keys [transaction frame-micros requested-system-time]} r
            _ (when-not (map? transaction) (reject! :unsupported-clocked-transaction))
            {:keys [tx-id system-time ops]} transaction
            decision (clock/resolve-retained-clock frame-micros requested-system-time prior)]
        (when-not (and (map? r) (nil? (meta r))
                       (= #{:status :profile :authority :certificate :transaction :clock
                            :requested-system-time :frame-micros} (set (keys r)))
                       (= :decoded (:status r)) (= clock/clocked-input-profile (:profile r))
                       (= :none (:authority r)) (= :not-issued (:certificate r))
                       (map? transaction) (nil? (meta transaction))
                       (= #{:tx-id :system-time :ops} (set (keys transaction)))
                       (integer? tx-id) (< prior-id tx-id Long/MAX_VALUE)
                       (= :resolved (:status decision)) (= decision (:clock r))
                       (= system-time (:system-time decision)))
          (reject! :unsupported-clocked-transaction))
        (recur (next remaining) system-time tx-id
               (conj txs {:system-time system-time :ops ops :tx-id tx-id
                          :requested-system-time requested-system-time
                          :clock-decision (:decision decision)})))
      txs)))

(defn- replay*
  "Replay from empty state. Each transaction is {:system-time Instant :ops
   vector}. System times increase, with ties admitted only by the
   equal-time and clocked profiles. Valid intervals are
   half-open and microsecond-aligned, with omitted starts at the supplied
   system time and omitted ends unbounded. Abort retains the prior snapshot.
   Selected assertions read tentative documents at the transaction system
   time (no current-time override), including selected worker clock, row-version and lease-expiry guards.
   Validate all operation shapes, including consecutive wide journal groups,
   before replay. Rectangles retain the issuing system time and exclusive supersession
   time. No provider observations or authority are admitted."
  [inputs mode]
  (let [clocked? (= :clocked mode)
        equal-time? (not= :strict mode)
        report {:profile (case mode :clocked clocked-profile :equal equal-time-profile profile)
                :authority :none :certificate :not-issued}]
    (try
      (let [transactions (if clocked? (clocked-transactions! inputs) inputs)]
        (when-not (and (vector? transactions) (nil? (meta transactions))
                       (<= 1 (count transactions) 256))
          (reject! :unsupported-temporal-sequence))
        (doseq [[i {:keys [system-time ops] :as tx}] (map-indexed vector transactions)]
          (when-not (and (map? tx) (nil? (meta tx))
                         (= (if clocked? #{:system-time :ops :tx-id :requested-system-time :clock-decision}
                              #{:system-time :ops}) (set (keys tx)))
                         (instant? system-time)
                         (or (zero? i)
                             (let [prior (:system-time (nth transactions (dec i)))]
                               (or (before? prior system-time) (and equal-time? (= prior system-time)))))
                         (vector? ops) (nil? (meta ops)) (<= 1 (count ops) 256))
            (reject! :unsupported-temporal-transaction)))
        (let [normalized (mapv normalize-transaction! transactions)]
          (when (and equal-time?
                     (some (fn [op] (and (string? (first op))
                                         (not (#{"ASSERT TRUE" "ASSERT FALSE" "ASSERT ? = ?"} (first op)))))
                           (mapcat :ops normalized)))
            (reject! :unsupported-equal-time-assertion))
          (let [{:keys [history segments closed]}
                (reduce (fn [{:keys [history] :as prior} {:keys [system-time groups clock-decision] :as tx}]
                          (let [clock-rejected? (= :reject-before-execution clock-decision)
                                {:keys [state committed? failed-assertion-index]}
                                (if clock-rejected? {:state prior :committed? false}
                                    (execute-transaction! prior system-time groups))]
                            (assoc state :history
                                   (conj history (cond-> {:system-time system-time
                                                          :committed? committed?
                                                          :segments (:segments state)}
                                                   clocked?
                                                   (merge (select-keys tx [:tx-id :requested-system-time :clock-decision]))
                                                   clock-rejected?
                                                   (assoc :rejection :invalid-system-time)
                                                   (and (not committed?) (not clock-rejected?))
                                                   (assoc :failed-assertion-index failed-assertion-index))))))
                        {:history [] :segments {} :closed []} normalized)]
            (assoc report :status :predicted :history history
                   :rectangles (into closed (active-rectangles segments))))))
      (catch clojure.lang.ExceptionInfo e
        (if (::unsupported (ex-data e))
          (assoc report :status :unsupported :reason (:reason (ex-data e)))
          (throw e))))))

(defn replay
  "Replay the selected strictly increasing system-time domain."
  [transactions]
  (replay* transactions :strict))

(defn replay-equal-time
  "Replay ordered transactions with nondecreasing system times. The last
   committed state at a shared time determines the visible snapshot;
   superseded same-time segments have no positive system interval. Abort
   preserves the preceding state, including earlier commits at that time.
   Only puts and literal/equality assertions are admitted; state-dependent
   and clock guards are unqualified. Separate diagnostic, not clock admission."
  [transactions]
  (replay* transactions :equal))

(defn replay-clocked
  "Replay selected clock-qualified attempted operations from empty state.
   Recompute the complete clock chain; reject malformed or unordered inputs.
   A clock-rejected attempt advances history time but never applies its
   operations or evaluates assertions against the running state. Validate
   its operation grammar too. Equal-time state-dependent assertions remain
   outside this profile. Input provenance is not established by this API."
  [inputs]
  (replay* inputs :clocked))

(defn visible-at
  "Project complete visible document maps at a system and valid instant."
  [report system-time valid-time]
  (when (and (#{profile equal-time-profile clocked-profile} (:profile report)) (= :predicted (:status report))
             (instant? system-time) (instant? valid-time))
    (project-visible (:segments (last (take-while #(not (before? system-time (:system-time %)))
                                                (:history report))))
                     valid-time)))

(def observation-profile :selected-outbox-observation-v1)

(defn observe-outbox-at
  "Predict the decoded worker row at a pinned system/valid point.
   No live frontier or authority is established."
  [report system-time valid-time id]
  (let [result {:profile observation-profile :authority :none :certificate :not-issued}]
    (if-not (and (= profile (:profile report)) (= :predicted (:status report))
                 (seq (:history report)) (uuid? id)
                 (clock-instant? system-time) (clock-instant? valid-time)
                 (not (before? (:system-time (peek (:history report))) system-time)))
      (assoc result :status :unsupported)
      (let [rows (get-in (last (take-while #(not (before? system-time (:system-time %)))
                                          (:history report))) [:segments :gesso_live_outbox])
            visible (fn [intervals]
                      (first (filter (fn [[a b _ _]]
                                       (and (not (before? valid-time a))
                                            (before-end? valid-time b))) intervals)))]
        (if (some (fn [[k intervals]] (and (not (uuid? k)) (visible intervals))) rows)
          (assoc result :status :unsupported)
          (assoc result :status :predicted
                 :row (when-let [[_ _ doc origin] (visible (get rows id))]
                        (assoc doc :xt/system-from (.atZone ^Instant origin (java.time.ZoneId/of "UTC"))))))))))

(defn lease-fence-correspondence
  "Check exact decoded observation operands for an active/expired guard.
   Not later guard execution, lease ownership, or read freshness."
  [report system-time valid-time guard]
  (let [result {:profile observation-profile :authority :none :certificate :not-issued}]
    (if-not (and (vector? guard) (nil? (meta guard)) (= 4 (count guard))
                 (#{:active :expired} (get version-guards (first guard)))
                 (uuid? (second guard))
                 (every? #(and (instance? ZonedDateTime %)
                               (clock-instant? (version-instant %))) (drop 2 guard)))
      (assoc result :status :unsupported)
      (let [{:keys [status row]} (observe-outbox-at report system-time valid-time (second guard))]
        (cond
          (not= :predicted status) (assoc result :status :unsupported)
          (or (nil? row) (not= :leased (:gesso.live.outbox/status row)))
          (assoc result :status :mismatch :reason :not-leased)
          (not (and (= :gesso.live.outbox/publication-intent (:gesso.live.outbox/type row))
                    (= 1 (:gesso.live.outbox/version row))
                    (uuid? (:gesso.live.outbox/lease-id row))
                    (string? (:gesso.live.outbox/lease-owner row))
                    (not (str/blank? (:gesso.live.outbox/lease-owner row)))
                    (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))))
          (assoc result :status :unsupported)
          (not= (:xt/system-from row) (nth guard 2))
          (assoc result :status :mismatch :reason :row-version)
          (not= (:gesso.live.outbox/lease-until row) (nth guard 3))
          (assoc result :status :mismatch :reason :lease-deadline)
          :else (assoc result :status :matched))))))

(def worker-profile :selected-worker-acquisition-v4)

(defn- supported-claim? [claim]
  (and (map? claim) (nil? (meta claim))
       (every? #{:intent-id :lease-id :worker-id :source-id :now :lease-until} (keys claim))
       (uuid? (:intent-id claim)) (uuid? (:lease-id claim))
       (string? (:worker-id claim)) (not (str/blank? (:worker-id claim)))
       (or (not (contains? claim :source-id))
           (and (string? (:source-id claim)) (not (str/blank? (:source-id claim)))))
       (clock-instant? (:now claim)) (clock-instant? (:lease-until claim))
       (<= 1 (.toMillis (java.time.Duration/between (:now claim) (:lease-until claim))) 300000)))

(defn- predict-acquisition
  [mode prefix observed-system observed-valid transaction-time claim]
  (let [result {:profile worker-profile :authority :none :certificate :not-issued}
        prior (replay prefix)]
    (if-not (and (supported-claim? claim) (= :predicted (:status prior))
                 (clock-instant? transaction-time)
                 (before? (:system-time (peek (:history prior))) transaction-time))
      (assoc result :status :unsupported)
      (let [{:keys [status row]} (observe-outbox-at prior observed-system observed-valid (:intent-id claim))
            reclaim? (= :reclaim mode)
            no-submission (fn [outcome reason]
                            (assoc result :status :predicted :outcome outcome
                                   :reason reason :ops [] :replay prior))
            unavailable (partial no-submission :unavailable)]
        (cond
          (not= :predicted status) (assoc result :status :unsupported)
          (nil? row) (unavailable :missing)
          (not (and (= :gesso.live.outbox/publication-intent (:gesso.live.outbox/type row))
                    (= 1 (:gesso.live.outbox/version row))
                    (string? (:gesso.live.outbox/source-id row))
                    (qualified-keyword? (:gesso.live.outbox/operation row))
                    (qualified-keyword? (:gesso.live.outbox/purpose row))))
          (assoc result :status :unsupported)
          (and (contains? claim :source-id) (not= (:source-id claim) (:gesso.live.outbox/source-id row)))
          (unavailable :source-mismatch)
          (not= (if reclaim? :leased :pending) (:gesso.live.outbox/status row))
          (unavailable (if reclaim? :not-leased :not-pending))
          (and reclaim?
               (not (and (uuid? (:gesso.live.outbox/lease-id row))
                         (string? (:gesso.live.outbox/lease-owner row))
                         (not (str/blank? (:gesso.live.outbox/lease-owner row)))
                         (instance? ZonedDateTime (:gesso.live.outbox/lease-until row)))))
          (assoc result :status :unsupported)
          (and reclaim? (= (:lease-id claim) (:gesso.live.outbox/lease-id row)))
          (no-submission :rejected :reused-lease-id)
          :else
          (let [deadline (:lease-until claim)
                doc (assoc (dissoc row :xt/system-from)
                           :gesso.live.outbox/status :leased
                           :gesso.live.outbox/lease-id (:lease-id claim)
                           :gesso.live.outbox/lease-owner (:worker-id claim)
                           :gesso.live.outbox/lease-until deadline)
                guard (if reclaim?
                        ["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)"
                         (:intent-id claim) (:xt/system-from row) (:gesso.live.outbox/lease-until row)]
                        ["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)"
                         (:intent-id claim) (:xt/system-from row)])
                ops [guard
                     ["ASSERT CURRENT_TIMESTAMP < ?" deadline]
                     ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]
                     [:put-docs :gesso_live_outbox doc]]
                after (replay (conj prefix {:system-time transaction-time :ops ops}))]
            (if (= :predicted (:status after))
              (assoc result :status :predicted :ops ops :replay after
                     :outcome (if (:committed? (peek (:history after)))
                                (if reclaim? :reclaimed :acquired) :contended))
              (assoc result :status :unsupported))))))))

(defn predict-pending-acquisition
  "Compose pinned pending eligibility, independent operation construction
   and later temporal replay. The observation may be stale. No live
   frontier, trusted caller, arbitrary writer closure or delivery is proven."
  [prefix observed-system observed-valid transaction-time claim]
  (predict-acquisition :pending prefix observed-system observed-valid transaction-time claim))

(defn predict-expired-reclamation
  "Compose pinned leased-row eligibility and a fresh lease token with
   provider-time expiry and new-deadline guards. Reused tokens are rejected
   before submission; an unexpired or stale observed lease is contended."
  [prefix observed-system observed-valid transaction-time claim]
  (predict-acquisition :reclaim prefix observed-system observed-valid transaction-time claim))

(defn predict-active-fence
  "Derive the selected physical active guard from a pinned observed row.
   A matching owner at observation is not a successful later fence."
  [prior observed-system observed-valid fence]
  (let [result {:profile worker-profile :authority :none :certificate :not-issued}
        rejected (fn [reason] (assoc result :status :predicted :outcome :rejected
                                     :reason reason :ops []))]
    (if-not (and (map? fence) (nil? (meta fence))
                 (= #{:intent-id :lease-id :worker-id} (set (keys fence)))
                 (uuid? (:intent-id fence)) (uuid? (:lease-id fence))
                 (string? (:worker-id fence)) (not (str/blank? (:worker-id fence))))
      (assoc result :status :unsupported)
      (let [{:keys [status row]} (observe-outbox-at prior observed-system observed-valid (:intent-id fence))]
        (cond
          (not= :predicted status) (assoc result :status :unsupported)
          (nil? row) (rejected :missing-lease)
          (not (and (= :gesso.live.outbox/publication-intent (:gesso.live.outbox/type row))
                    (= 1 (:gesso.live.outbox/version row))
                    (string? (:gesso.live.outbox/source-id row))
                    (qualified-keyword? (:gesso.live.outbox/operation row))
                    (qualified-keyword? (:gesso.live.outbox/purpose row))))
          (assoc result :status :unsupported)
          (not= :leased (:gesso.live.outbox/status row)) (rejected :not-leased)
          (not (and (uuid? (:gesso.live.outbox/lease-id row))
                    (string? (:gesso.live.outbox/lease-owner row))
                    (not (str/blank? (:gesso.live.outbox/lease-owner row)))
                    (instance? ZonedDateTime (:gesso.live.outbox/lease-until row))))
          (assoc result :status :unsupported)
          (not (and (= (:lease-id fence) (:gesso.live.outbox/lease-id row))
                    (= (:worker-id fence) (:gesso.live.outbox/lease-owner row))))
          (rejected :stale-lease)
          :else (assoc result :status :predicted :outcome :guarded
                       :ops [["ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"
                              (:intent-id fence) (:xt/system-from row) (:gesso.live.outbox/lease-until row)]]))))))

(defn predict-active-fenced-transaction
  "Compose a selected active fence with supplied physical puts in ONE
   transaction. This predicts execution, not authorization of the puts,
   acknowledgment semantics, or permission to perform external effects."
  [prefix observed-system observed-valid transaction-time fence writes]
  (let [prior (replay prefix)
        result (predict-active-fence prior observed-system observed-valid fence)]
    (cond
      (not= :predicted (:status result)) result
      (not (and (clock-instant? transaction-time)
                (before? (:system-time (peek (:history prior))) transaction-time)
                (vector? writes) (nil? (meta writes)) (<= 1 (count writes) 255)
                (every? #(and (vector? %) (= :put-docs (first %))) writes)))
      (select-keys (assoc result :status :unsupported) [:profile :authority :certificate :status])
      (= :rejected (:outcome result)) (assoc result :replay prior)
      :else (let [ops (into (:ops result) writes)
                  after (replay (conj prefix {:system-time transaction-time :ops ops}))]
              (if (= :predicted (:status after))
                (assoc result :ops ops :replay after
                       :outcome (if (:committed? (peek (:history after))) :committed :contended))
                (select-keys (assoc result :status :unsupported)
                             [:profile :authority :certificate :status]))))))

(defn predict-acknowledgment
  "Predict selected trusted receipt bookkeeping, not adapter authenticity
   or external delivery. Invalid submissions remain outside the fragment."
  [prefix observed-system observed-valid transaction-time submission]
  (let [result {:profile worker-profile :authority :none :certificate :not-issued
                :status :unsupported}
        receipt (:receipt submission)
        prior (replay prefix)]
    (if-not (and (map? submission) (nil? (meta submission))
                 (= #{:intent-id :lease-id :worker-id :receipt} (set (keys submission)))
                 (map? receipt) (nil? (meta receipt))
                 (= #{:receipt-id :adapter :result} (set (keys receipt)))
                 (uuid? (:receipt-id receipt)) (qualified-keyword? (:adapter receipt))
                 (= :accepted (:result receipt))
                 (= :predicted (:status prior)) (clock-instant? transaction-time)
                 (before? (:system-time (peek (:history prior))) transaction-time))
      result
      (let [fence (dissoc submission :receipt)
            guard (predict-active-fence prior observed-system observed-valid fence)
            row (:row (observe-outbox-at prior observed-system observed-valid (:intent-id fence)))]
        (cond
          (not= :predicted (:status guard)) result
          (and row (str/blank? (:gesso.live.outbox/source-id row))) result
          (= :rejected (:outcome guard))
          (let [reason (:reason guard)]
            (assoc guard :replay prior
                   :outcome (if (= :stale-lease reason) :rejected :unavailable)
                   :reason (if (= :missing-lease reason) :missing reason)))
          :else
          (let [updated (assoc (dissoc row :xt/system-from)
                               :gesso.live.outbox/status :dispatch-accepted
                               :gesso.live.outbox/dispatch-receipt-id (:receipt-id receipt)
                               :gesso.live.outbox/dispatch-adapter (:adapter receipt))
                ops (conj (:ops guard) [:put-docs :gesso_live_outbox updated])
                after (replay (conj prefix {:system-time transaction-time :ops ops}))]
            (if (= :predicted (:status after))
              (assoc result :status :predicted :ops ops :replay after
                     :outcome (if (:committed? (peek (:history after))) :acknowledged :contended))
              result)))))))
