(ns gesso.assurance.proof.committed-incorporation-progression
  "v1408: Correlate a reported committed Gesso resolved operation with one
   separately supplied durable-command readback and Live progression.

   A transaction acknowledgement, durable command record, and read basis are
   different evidence. A readback matching the command binding/outcome does
   not prove that the observed attempt wrote it; a reported progression basis
   does not authenticate the read or authorize optimistic settlement. Replay
   cannot be promoted to a new commit, and unresolved/rejected/unknown paths
   cannot be promoted merely because a record is later visible.

   Input contains PRIVATE execution and persisted command values. Never expose
   or log input maps. The returned report contains only statuses and counts.
   Neither this diagnostic nor the v1396 report establishes physical
   refinement, writer completeness, backend serializability, or permission to
   retry or settle. All nine physical-refinement obligations remain open."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.transaction-outcome-progression-correspondence
             :as outcome]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.progression :as progression]
            [gesso.model.command-resolution :as resolution]))

(def report-type
  :gesso.assurance.proof.committed-incorporation-progression/report)
(def report-version 1)

(def ^:private original-classifier outcome/classify-observation)
(def ^:private original-record-validator resolution/require-record)
(def ^:private original-binding-validator resolution/require-binding)
(def ^:private open-obligations
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})
(def ^:private source-keys #{:observation :binding :post-readback})
(def ^:private readback-keys #{:record :progression})

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentHashMap x)
           (instance? clojure.lang.PersistentArrayMap x))
       (nil? (meta x))))

(defn- closed-map? [x expected]
  (and (plain-map? x) (= expected (set (keys x)))))

(defn- reject! [reason]
  (throw (ex-info "Invalid committed-incorporation progression diagnostic."
                  {:committed-incorporation-progression/rejection true
                   :reason reason})))

(defn- validated-record-status [record]
  (cond
    (nil? record) :not-visible
    (not (plain-map? record)) :malformed
    :else (try
            (original-record-validator record)
            :valid
            (catch Throwable _ :malformed))))

(defn- resolution-from [observation]
  (when (= :returned (get-in observation [:execution :outcome]))
    (get-in observation [:execution :result :controlled-result :resolution])))

(defn- report-progression [transaction]
  ;; The receipt and context are data attached by the reported transaction,
  ;; not authenticated execution evidence. Inspect both independently.
  (let [reported (:progression transaction)
        context (get-in transaction [:ctx :gesso.live/progression])
        consistency (:consistency transaction)
        tx-result (:tx-result transaction)]
    (cond
      (not (progression/requirement? reported)) :no-valid-reported-transaction-basis
      (not (map? (:ctx transaction))) :no-reported-read-context
      (not (progression/requirement? context)) :no-valid-reported-context-basis
      (not (set/subset? (:bases reported) (:bases context)))
      :reported-context-omits-transaction-basis
      (not (map? consistency)) :no-valid-reported-consistency
      :else
      (try
        (let [public (xtdb/tx-result-consistency tx-result)
              bs (:bases reported)]
          (if (and (= (:tx-id public) (:tx-id consistency))
                   (= (:system-time public) (:system-time consistency))
                   (some? (:tx-id public))
                   (some? (:system-time public))
                   (= 1 (count bs))
                   (let [b (first bs)]
                     (and (xtdb/xtdb-basis? b)
                          (= (:tx-id public) (xtdb/basis-tx-id b))
                          (= (:system-time public) (xtdb/basis-system-time b)))))
            :reported-key-consistency-and-context-agree
            :reported-key-consistency-or-context-diverges))
        (catch Throwable _ :uncheckable-reported-consistency)))))

(defn- readback-progression [reported post]
  (cond
    (nil? post) :not-reported
    (not (progression/requirement? post)) :invalid-readback-progression
    (not (progression/requirement? reported)) :no-transaction-requirement
    (set/subset? (:bases reported) (:bases post))
    :reported-readback-includes-transaction-basis
    :else :reported-readback-omits-transaction-basis))

(defn compare-incorporation!
  "Pure, redacted comparison of ONE rechecked v1394 observation with a separately
   supplied command record and progression requirement. Exact request shape:

     {:observation {:execution ... :diagnostic ...} ; v1394, private
      :binding <typed command binding>             ; caller-supplied, private
      :post-readback {:record <command record or nil>
                      :progression <requirement or nil>}}

   This does NOT read XTDB. The caller must independently arrange read-after-
   write consistency; supplying a progression requirement doesn't attest that
   the read actually used it. A matching report never authorizes a browser
   transition, retry, or settlement."
  [source]
  (when-not (closed-map? source source-keys)
    (reject! :invalid-source-shape))
  (let [{:keys [observation binding post-readback]} source]
    (when-not (and (closed-map? observation #{:execution :diagnostic})
                   (closed-map? post-readback readback-keys))
      (reject! :invalid-observation-or-readback-shape))
    ;; Guard against replacing the entrusted checkers with application fakes.
    ;; This is in-process tamper detection, NOT a security boundary.
    (when-not (and (identical? original-classifier
                               (var-get #'outcome/classify-observation))
                   (identical? original-record-validator
                               (var-get #'resolution/require-record))
                   (identical? original-binding-validator
                               (var-get #'resolution/require-binding)))
      (reject! :substituted-upstream-checker))
    (try (original-binding-validator binding)
         (catch Throwable _ (reject! :invalid-command-binding)))
    (let [prior (try (original-classifier observation)
                     (catch Throwable _ (reject! :invalid-outcome-observation)))
          resolution (resolution-from observation)
          resolution-status (:durable-command-resolution-status prior)
          record (:record post-readback)
          record-status (validated-record-status record)
          binding-match? (and (= :valid record-status)
                              (= binding (:gesso.command/binding record)))
          outcome-match? (and binding-match?
                              (= :incorporated resolution-status)
                              (map? resolution)
                              (= (:outcome resolution)
                                 (:gesso.command/outcome record)))
          transaction (:transaction resolution)
          tx-progression-status
          (if (= :incorporated resolution-status)
            (report-progression transaction)
            :not-applicable)
          rb-progression-status
          (readback-progression (:progression transaction)
                                (:progression post-readback))
          acknowledged? (and (= :incorporated resolution-status)
                             (= :bounded-incorporation-and-ledger-coobservation
                                (:status prior))
                             (= :reported-basis-matches-public-transaction-key
                                (:reported-authoritative-progression-status prior)))
          basis-match? (and (= :reported-key-consistency-and-context-agree
                               tx-progression-status)
                            (= :reported-readback-includes-transaction-basis
                               rb-progression-status))
          status (cond
                   (not= :incorporated resolution-status)
                   (case resolution-status
                     :replay :previous-incorporation-not-attributed-to-attempt
                     :unresolved :unresolved-command-no-commit-established
                     :incompatible :incompatible-command-binding
                     :unavailable :command-disclosure-unavailable
                     :not-established :no-resolved-command-outcome
                     :nonincorporating-or-uncertain-outcome)
                   (not acknowledged?) :commit-or-ledger-correlation-incomplete
                   (= :not-visible record-status) :durable-record-not-visible
                   (not= :valid record-status) :invalid-durable-record
                   (not binding-match?) :durable-record-binding-diverged
                   (not outcome-match?) :durable-record-outcome-diverged
                   (not basis-match?) :progression-or-readback-diverged
                   :else :bounded-incorporation-and-progression-coobservation)]
      {:type report-type
       :version report-version
       :status status
       :reported-resolution-status resolution-status
       :reported-transaction-status (:status prior)
       :durable-record-status record-status
       :durable-command-binding-agrees? (boolean binding-match?)
       :durable-outcome-agrees? (boolean outcome-match?)
       :transaction-context-progression-status tx-progression-status
       :post-readback-progression-status rb-progression-status
       :bounded-three-way-agreement? (= :bounded-incorporation-and-progression-coobservation
                                       status)
       :observation-origins
       {:executor-and-ledger :process-local-v1394-copied-evidence
        :durable-record :caller-supplied-readback-unattested
        :read-progression :caller-supplied-read-requirement-unattested}
       :record-read-authenticated? false
       :transaction-causality-authenticated? false
       :physical-write-footprint-complete? false
       :prior-attempt-commit-attributed? false
       :business-rejection-established? false
       :safe-to-retry? false
       :safe-to-settle? false
       :authority :none
       :transaction-authority :none
       :live-settlement-authority :none
       :certificate :not-issued
       :admission :not-admitted
       :proof-status :bounded-incorporation-progression-diagnostic-only
       :physical-refinement-obligations open-obligations
       :physical-refinement-obligation-statuses :open})))
