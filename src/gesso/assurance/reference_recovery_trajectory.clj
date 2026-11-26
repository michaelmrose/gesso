(ns gesso.assurance.reference-recovery-trajectory
  "Bounded linkage of a winning commit, competing abort and recovery commit."
  (:require [clojure.string :as str]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.live.outbox.maintenance-policy :as maintenance]
            [gesso.model.command-resolution :as resolution])
  (:import [java.time Instant ZoneId ZonedDateTime]))

(def profile :selected-retained-recovery-linkage-v1)
(def interleaved-profile :selected-worker-interleaved-recovery-linkage-v1)

(defn- candidate-record! [tx]
  (let [puts (filter #(and (vector? %) (= [:put-docs :gesso_commands]
                                          (vec (take 2 %)))) (:ops tx))]
    (when (and (= 1 (count puts)) (= 3 (count (first puts))))
      (resolution/require-record (nth (first puts) 2)))))

(defn- selection-indices [inputs selected]
  (mapv (fn [step] (first (keep-indexed #(when (= step %2) %1) inputs))) selected))

(defn- instant [x]
  (if (instance? ZonedDateTime x) (.toInstant ^ZonedDateTime x) x))

(defn- worker-ops [ops]
  ;; Only the fixed worker footprint is compared, after replay validated its
  ;; grammar. SQL timestamp operands have instant semantics; the supported
  ;; stored lease deadline is represented in UTC by the temporal model.
  (mapv (fn [op]
          (cond
            (string? (first op)) (mapv instant op)
            (and (= 3 (count op)) (= [:put-docs :gesso_live_outbox] (subvec op 0 2))
                 (map? (peek op)) (instance? Instant (:gesso.live.outbox/lease-until (peek op))))
            (update-in op [2 :gesso.live.outbox/lease-until] #(.atZone ^Instant % (ZoneId/of "UTC")))
            :else op)) ops))

(defn- boundary-between? [segments from to]
  (boolean (some (fn [t] (and t (neg? (compare from t)) (not (pos? (compare t to)))))
                 (for [rows (vals segments), intervals (vals rows), [a b] intervals, t [a b]] t))))

(defn- intervening-worker [inputs replay source-id i]
  (let [tx (nth inputs i) time (:system-time tx) prior-time (:system-time (nth inputs (dec i)))
        prior (nth (:history replay) (dec i)) after (nth (:history replay) i)
        protected (dissoc (:segments prior) :gesso_live_outbox)
        ops (:ops tx) put (peek ops)]
    (when (and (:committed? after)
               (= protected (dissoc (:segments after) :gesso_live_outbox))
               (not (boundary-between? protected prior-time time))
               (vector? put) (= 3 (count put)) (= [:put-docs :gesso_live_outbox] (subvec put 0 2))
               (map? (peek put)))
      (let [id (:xt/id (peek put))
            before-observation (temporal/observe-outbox-at replay prior-time time id)
            after-observation (temporal/observe-outbox-at replay time time id)
            old (:row before-observation) new (:row after-observation)
            journal (get (temporal/visible-at replay prior-time time) :gesso_commands {})]
        (when (and (= :predicted (:status before-observation) (:status after-observation))
                   old new (contains? journal (:gesso.live.outbox/command-id old)))
          (when-let [expected (maintenance/historical-worker-envelope source-id time old new)]
            (when (= (worker-ops (:ops expected)) (worker-ops ops))
              {:tx-id (:tx-id tx) :maintenance (:maintenance expected) :intent-id id})))))))

(defn- compare* [inputs selected expected interleaved? source-id]
  (let [base {:profile (if interleaved? interleaved-profile profile) :authority :none :certificate :not-issued
              :provider-refinement :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (if-not (and (vector? inputs) (nil? (meta inputs)) (<= 4 (count inputs) 256)
                   (every? #(and (map? %) (nil? (meta %))
                                 (= #{:tx-id :system-time :ops} (set (keys %)))
                                 (integer? (:tx-id %)) (<= 0 (:tx-id %) (dec Long/MAX_VALUE))) inputs)
                   (apply < (map :tx-id inputs))
                   (vector? selected) (nil? (meta selected)) (= 3 (count selected))
                   (map? expected) (= #{:winner :abort :recovery} (set (keys expected)))
                   (or (not interleaved?) (and (string? source-id) (not (str/blank? source-id)))))
        (fail :unsupported :invalid-recovery-linkage-source)
        (let [[start middle end :as indices] (selection-indices inputs selected)]
          (if-not (and (every? some? indices) (pos? start) (< start middle end)
                       (or interleaved? (= [start (inc start) (+ start 2)] indices)))
            (fail :mismatch (if interleaved? :invalid-interleaved-recovery-selection :nonadjacent-recovery-selection))
            (let [[winning abort recovery] selected
                  records (mapv candidate-record! selected)
                  [w a r] records
                  wb (:gesso.command/binding w) ab (:gesso.command/binding a)
                  rb (:gesso.command/binding r)
                  w-id (:xt/id w) a-id (:xt/id a)
                  replay (temporal/replay (mapv #(dissoc % :tx-id)
                                                (if interleaved? inputs (subvec inputs 0 (inc end)))))
                  history (:history replay)]
              (cond
                (not (and (every? some? records) (= records (mapv expected [:winner :abort :recovery]))
                          (= ab rb) (not= w-id a-id) (not= (:principal wb) (:principal ab))
                          (= (select-keys wb [:operation :arguments]) (select-keys ab [:operation :arguments]))
                          (not= (:gesso.command/outcome a) (:gesso.command/outcome r))))
                (fail :mismatch :recovery-command-linkage-mismatch)
                (not= :predicted (:status replay))
                (fail :unsupported :unsupported-recovery-linkage-replay)
                (not= [true false true] (mapv #(get-in history [% :committed?]) indices))
                (fail :mismatch :recovery-commitment-sequence-mismatch)
                (not= (:segments (nth history (dec middle))) (:segments (nth history middle)))
                (fail :mismatch :recovery-abort-effects-mismatch)
                :else
                (let [gaps (concat (range (inc start) middle) (range (inc middle) end))
                      workers (mapv #(intervening-worker inputs replay source-id %) gaps)
                      journal (fn [system valid] (get (temporal/visible-at replay system valid) :gesso_commands {}))
                      wt (:system-time winning) at (:system-time abort) rt (:system-time recovery)
                      initial (journal (:system-time (nth inputs (dec start))) wt)
                      won (journal wt wt) aborted (journal at at)
                      recovered (journal rt rt)]
                  (cond
                    (not (every? some? workers)) (fail :mismatch :unqualified-intervening-worker)
                    (not (and (not-any? #(contains? initial %) [w-id a-id])
                              (= won (assoc initial w-id w))
                              (= aborted won)
                              (= recovered (assoc won a-id r))))
                    (fail :mismatch :recovery-journal-trajectory-mismatch)
                    :else (cond-> (assoc base :status :matched :recovery-linkage :matched-retained-journal-trajectory
                                         :selected-tx-ids (mapv :tx-id selected))
                            interleaved? (assoc :intervening-maintenance workers
                                                :replayed-transaction-count (count inputs))))))))))
      (catch clojure.lang.ExceptionInfo _
        (fail :unsupported :invalid-recovery-command-record)))))

(defn compare-selected!
  "Check adjacent selected retained steps and command-journal linkage.
   Expected bindings and outcomes are supplied premises. This is a pure
   replay diagnostic, not a provider bracket or abstract relation check."
  [inputs selected expected]
  (compare* inputs selected expected false nil))

(defn compare-worker-interleaved!
  "Replay every supplied transaction, including the suffix. Between the
   three selected steps require exact committed worker envelopes for the
   supplied source, a linked existing journal record, and unchanged
   non-outbox temporal segments with no intervening valid-time boundary.
   Prefix and suffix are replayed, not classified as workers. This is a
   conditional pure relation, not acquisition, writer authentication,
   provider correspondence or a delivery proof."
  [inputs selected expected source-id]
  (compare* inputs selected expected true source-id))
