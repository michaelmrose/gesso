(ns gesso.assurance.reference-physical-envelope-argument
  "Join issued envelope correspondence to pinned selected physical rows."
  (:require [gesso.assurance.reference-issued-envelope-argument :as issued]
            [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import (java.time Instant ZonedDateTime)))

(def profile :conditional-physical-source-envelope-recovery-v1)
(def ^:private boundaries
  {#'issued/compare-recovery! issued/compare-recovery!
   #'recovery/compare-selected! recovery/compare-selected!
   #'coverage/observe-local-processed-retained-replay-inputs! coverage/observe-local-processed-retained-replay-inputs!
   #'temporal/replay temporal/replay
   #'temporal/visible-at temporal/visible-at
   #'xt/q xt/q
   #'basis/<-time-basis-str basis/<-time-basis-str})
(def ^:private base
  {:profile profile :authority :none :certificate :not-issued
   :provider-refinement :not-established :universal-refinement :not-established
   :writer-coverage :not-established :proof-discharge :not-performed
   :physical-preparation-provenance :not-established :aborted-receipt-identity :not-established})

(defn- require! [condition reason]
  (when-not condition (throw (ex-info "Unqualified physical envelope recovery" {:reason reason}))))

(defn- boundaries! []
  (require! (every? (fn [[v f]] (identical? @v f)) boundaries)
            :physical-envelope-boundary-replaced))

(defn- rows! [rows]
  (require! (and (sequential? rows) (<= (count rows) 65536)
                 (every? #(and (map? %) (some? (:xt/id %))) rows)
                 (= (count rows) (count (set (map :xt/id rows)))))
            :invalid-physical-envelope-rows)
  (into {} (map (juxt :xt/id identity)) rows))

(defn- token! [rows]
  (let [token (when (and (sequential? rows) (= 1 (count rows))
                         (= #{:snapshot-token} (set (keys (first rows)))))
                (:snapshot-token (first rows)))]
    (require! (and (string? token) (seq token)) :invalid-physical-envelope-token)
    token))

(defn- prepared! [step tables]
  (require! (= (disj tables :gesso_commands) (set (keys (:docs step))))
            :physical-envelope-preparation-scope)
  (assoc (into {} (map (fn [[k rows]] [k (rows! rows)])) (:docs step))
         :gesso_commands (rows! (:journal step))))

(defn- read-point! [node opts replay tables system valid]
  (let [expected (temporal/visible-at replay system valid)
        physical (into {} (map (fn [table]
                                 [table (rows! (xt/q node [(str "SELECT * FROM \"public\".\"" (name table)
                                                                  "\" FOR VALID_TIME AS OF ? FOR SYSTEM_TIME AS OF ? LIMIT 65537")
                                                             valid system] opts))])) tables)]
    (require! (map? expected) :unsupported-physical-envelope-replay)
    (require! (= physical (into {} (map (fn [t] [t (get expected t {})])) tables))
              :physical-envelope-readback-mismatch)
    physical))

(defn compare-recovery!
  "Selected local three-attempt recovery only. Match issued envelopes and
   retained history, then pin historical reads of mapped documents and journal.
   Match stale preparation to an earlier image without claiming acquisition
   provenance. No receipt for the throw, proof discharge or cross-restart authority."
  [node tables admission source declarations steps handle]
  (try
    (boundaries!)
    (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
          inputs (:replay-inputs sample)
          ids (mapv #(get-in % [:expectation :tx-id]) steps)
          positions (mapv (fn [id] (first (keep-indexed #(when (= id (:tx-id %2)) %1) inputs))) ids)
          mapped-tables (conj (into #{} (keep (fn [[kind table]]
                                                (when (#{:document :nullable-document} kind) table)))
                                          (vals (:target-map source))) :gesso_commands)]
      (require! (and (= :observed (:status sample)) (vector? inputs)
                     (= (:transaction-ids sample) (mapv :tx-id inputs))
                     (= 3 (count positions)) (every? pos-int? positions)
                     (= positions (vec (range (first positions) (+ 3 (first positions)))))
                     (= (mapv :expectation steps) (mapv #(nth inputs %) positions))
                     (set? tables) (every? tables mapped-tables)
                     (every? #(and (simple-keyword? %) (re-matches #"[a-z][a-z0-9_]*" (name %))) mapped-tables))
                :unqualified-physical-envelope-history)
      (let [local (issued/compare-recovery! node admission source declarations steps handle)
            retained (recovery/compare-selected! node tables admission source declarations steps handle)
            replay (temporal/replay (mapv #(dissoc % :tx-id) inputs))
            selected (mapv #(nth inputs %) positions)
            priors (mapv #(nth inputs (dec %)) positions)
            token (token! (xt/q node "SHOW SNAPSHOT_TOKEN"))
            decoded (basis/<-time-basis-str token)
            times (get decoded "xtdb")
            point (first times)
            opts {:snapshot-token token :current-time (Instant/now)}]
        (require! (and (= :conditional-issued-source-envelope-recovery (:status local))
                       (= :matched (:status retained))
                       (= :matched-local-observed-retained-steps (:submission-recovery-linkage retained))
                       (= ids (:selected-tx-ids retained))
                       (= [:committed :aborted :committed] (:selected-commitments retained))
                       (= :predicted (:status replay))
                       (= #{"xtdb"} (set (keys decoded))) (= 1 (count times))
                       (instance? Instant point) (every? #(instance? Instant (:system-time %)) inputs)
                       (not (.isBefore ^Instant point ^Instant (:system-time (peek inputs)))))
                  :unqualified-physical-envelope-composition)
        (require! (= token (token! (xt/q node "SHOW SNAPSHOT_TOKEN" opts))) :physical-envelope-token-mismatch)
        (let [ledger (mapv (fn [tx expected]
                             (let [rows (xt/q node ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?" (:tx-id tx)] opts)
                                   row (first rows)]
                               (require! (and (= 1 (count rows)) (= #{:xt/id :committed :system-time} (set (keys row)))
                                              (= (:tx-id tx) (:xt/id row)) (= expected (:committed row))
                                              (instance? ZonedDateTime (:system-time row))
                                              (= (:system-time tx) (.toInstant ^ZonedDateTime (:system-time row))))
                                         :physical-envelope-ledger-mismatch)
                               {:tx-id (:tx-id tx) :system-time (:system-time tx) :committed? expected}))
                           selected [true false true])
              images (mapv (fn [prior tx]
                             (let [system (:system-time prior) valid (:system-time tx)]
                               {:pre (read-point! node opts replay mapped-tables system valid)
                                :post (read-point! node opts replay mapped-tables valid valid)})) priors selected)
              [winner abort refusal] images
              preparations (mapv #(prepared! % mapped-tables) steps)]
          (require! (= preparations [(:pre winner) (:pre winner) (:pre refusal)])
                    :physical-envelope-preparation-image-mismatch)
          (require! (and (= (:post winner) (:pre abort)) (= (:pre abort) (:post abort))
                         (= (:post abort) (:pre refusal)) (not= (:pre winner) (:pre abort)))
                    :physical-envelope-recovery-continuity-mismatch)
          (require! (= token (token! (xt/q node "SHOW SNAPSHOT_TOKEN" opts))) :physical-envelope-token-mismatch)
          (require! (= sample (coverage/observe-local-processed-retained-replay-inputs! node))
                    :physical-envelope-history-changed)
          (boundaries!)
          (assoc base :status :conditional-physical-source-envelope-recovery
                 :induction-identity (:induction-identity local) :selected-tx-ids ids :ledger ledger
                 :snapshot-system-time point :token-echo :matched-before-and-after
                 :physical-rows :matched-independent-retained-replay
                 :preparation-images [:matched-predecessor :matched-earlier-selected-image :matched-predecessor]
                 :aborted-prestate :distinct-from-stale-preparation
                 :aborted-effect :matched-physical-stutter
                 :remaining-conditions (into (:remaining-conditions local)
                                             #{:faithful-pinned-provider-readbacks
                                               :faithful-retained-source-log-decoding-and-retention
                                               :faithful-provider-ledger-identity-and-outcome
                                               :faithful-original-preparation-acquisition
                                               :faithful-selected-aborted-attempt-attribution})))))
    (catch InterruptedException e (.interrupt (Thread/currentThread)) (throw e))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-physical-envelope)))
    (catch Exception _ (assoc base :status :not-established :reason :physical-envelope-readback-failed))))
