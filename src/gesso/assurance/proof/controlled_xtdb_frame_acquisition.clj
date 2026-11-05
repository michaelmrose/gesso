(ns gesso.assurance.proof.controlled-xtdb-frame-acquisition
  "v1417/v1419: Acquire bounded frame-only pre/post rows by issuing XTDB q
   calls on one caller-supplied node around ONE captured adapter invocation.

   The helper controls the order of its *own* calls, not the database's internal
   transaction execution or any concurrent writers. v1419 pins the queries
   *within each phase* to an XTDB SHOW SNAPSHOT_TOKEN basis and one fixed
   current-time. The two phases have separate bases, and pinning them does
   not authenticate the transaction-internal prestate. The submission
   callback is untrusted and may ignore the supplied node; the adapter capture
   is process-local and may miss other writer paths. Therefore even a matching
   pre/post comparison is diagnostic only, never proof of a physical frame,
   actual tx causality, writer closure, retry or browser settlement authority.

   Detailed results include private document values and transaction arguments.
   Never log them; use redacted-summary instead. Failures after the submission
   may mean that a transaction committed even though no report was returned.
   The helper NEVER retries or makes a second submission."
  (:require [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.assurance.proof.captured-write-domain-correspondence :as captured]
            [gesso.assurance.proof.finite-physical-frame-observation :as frame]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.basis :as xt.basis])
  (:import [java.time Instant]))

(def acquisition-version 2)
(def acquisition-type :gesso.assurance.proof.controlled-xtdb-frame-acquisition/diagnostic)
(def ^:private max-frame-targets 256)
(def ^:private original-domain domain/derive-protected-write-domain!)
(def ^:private original-domain-check domain/require-current-protected-write-domain!)
(def ^:private original-captured captured/compare-captured-write-domain!)
(def ^:private original-captured-check captured/require-current-captured-write-domain!)
(def ^:private original-frame frame/compare-finite-physical-frame!)
(def ^:private original-frame-check frame/require-current-finite-physical-frame!)
(def ^:private original-capture live/capture-tx-api-calls!)
(def ^:private original-query xt/q)
(def ^:private original-decode xt.basis/<-time-basis-str)
(def ^:private original-tx-consistency live/tx-result-consistency)

(defn- reject! [reason at]
  (throw (ex-info "Invalid controlled XTDB finite-frame acquisition."
                  {:controlled-xtdb-frame-acquisition/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x required at]
  (when-not (and (plain-map? x)
                 (nil? (meta x))
                 (= required (set (keys x))))
    (reject! :invalid-map-shape at))
  x)

(defn- check-seams! []
  ;; Protect this small diagnostic against casual var replacement. This does
  ;; not authenticate the node, the running application, or the JVM itself.
  (when-not (and (identical? original-domain
                             (var-get #'domain/derive-protected-write-domain!))
                 (identical? original-domain-check
                             (var-get #'domain/require-current-protected-write-domain!))
                 (identical? original-captured
                             (var-get #'captured/compare-captured-write-domain!))
                 (identical? original-captured-check
                             (var-get #'captured/require-current-captured-write-domain!))
                 (identical? original-frame
                             (var-get #'frame/compare-finite-physical-frame!))
                 (identical? original-frame-check
                             (var-get #'frame/require-current-finite-physical-frame!))
                 (identical? original-capture
                             (var-get #'live/capture-tx-api-calls!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'xt.basis/<-time-basis-str))
                 (identical? original-tx-consistency
                             (var-get #'live/tx-result-consistency)))
    (reject! :substituted-acquisition-or-checker [])))

(defn- frame-entries! [domain-report]
  (let [entries (:frame-only-protected-documents domain-report)]
    (when-not (and (vector? entries)
                   (<= 1 (count entries) max-frame-targets))
      (reject! :no-supported-finite-frame-targets [:domain-source]))
    (doseq [[i entry] (map-indexed vector entries)]
      (let [[table id] (:target entry)]
        (when-not (and (keyword? table)
                       (nil? (namespace table))
                       (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                       (some? id) (not (coll? id)))
          (reject! :unsupported-sql-table-or-target [:frame-targets i]))))
    entries))

(defn- phase-token-time!
  "Interpret a locally obtained XTDB token for an ordering *diagnostic*.
   A valid multi-database/empty token can be pinned, but has no single
   comparable system time. No transaction-internal basis is inferred."
  [token phase]
  (when-not (and (string? token) (seq token))
    (reject! :missing-or-invalid-phase-snapshot-token [phase]))
  (let [decoded (try (original-decode token)
                     (catch Throwable _
                       (reject! :unreadable-phase-snapshot-token [phase])))]
    (when-not (map? decoded)
      (reject! :unreadable-phase-snapshot-token [phase]))
    (when (and (= 1 (count decoded))
               (= 1 (count (val (first decoded))))
               (instance? Instant (first (val (first decoded)))))
      (first (val (first decoded))))))

(defn- read-phase! [node entries phase]
  ;; A single token and valid-time clock govern all SELECTs in this phase.
  ;; Neither basis is asserted to be the transaction's internal snapshot.
  (let [token-rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                        (catch Throwable _
                          (reject! :physical-read-failed [phase :snapshot-token])))
        token (when (and (sequential? token-rows)
                         (= 1 (count token-rows))
                         (plain-map? (first token-rows)))
                (:snapshot-token (first token-rows)))
        _ (phase-token-time! token phase)
        basis {:snapshot-token token :current-time (Instant/now)}
        rows (mapv (fn [i {:keys [target]}]
                     (let [[table id] target
                           docs (try
                                  (original-query node
                                                  [(str "SELECT * FROM " (name table)
                                                        " WHERE _id = ? LIMIT 2") id]
                                                  basis)
                                  (catch Throwable _
                                    (reject! :physical-read-failed [phase i])))]
                       (when-not (and (sequential? docs) (<= (count docs) 1))
                         (reject! :ambiguous-or-invalid-physical-read [phase i]))
                       (let [doc (first docs)]
                         (when-not (or (nil? doc)
                                       (and (plain-map? doc) (nil? (meta doc))
                                            (every? keyword? (keys doc))
                                            (or (not (contains? doc :xt/id))
                                                (= id (:xt/id doc)))))
                           (reject! :invalid-or-contradictory-physical-row [phase i]))
                         {:target target :document doc})))
                   (range (count entries)) entries)]
    {:rows rows :token token}))

(defn- time-order [pre post]
  (if (and (instance? Instant pre) (instance? Instant post))
    (if (pos? (compare pre post)) :pre-after-post :pre-at-or-before-post)
    :not-comparable))

(defn- reported-transaction-time-order [capture pre post]
  ;; An execute-tx result may provide a complete *public* tx system time.
  ;; This does not authenticate its origin or prove that a frame read used it.
  (let [events (:events capture)
        [started ended] (when (= 2 (count events)) events)
        tx-time (when (and (= :execute-tx (:api started))
                           (= :returned (:phase ended)))
                  (:system-time (original-tx-consistency (:tx-result ended))))]
    (cond
      (not (instance? Instant tx-time)) :no-complete-reported-transaction-time
      (not (and (instance? Instant pre) (instance? Instant post)))
      :phase-time-not-comparable
      (pos? (compare pre tx-time)) :pre-phase-after-reported-transaction-time
      (neg? (compare post tx-time)) :post-phase-before-reported-transaction-time
      :else :reported-transaction-time-between-phase-bases)))

(defn- assemble! [source]
  (let [frame-input (:frame-input source)
        captured-source (:captured-source frame-input)
        domain-source (:domain-source captured-source)
        domain-report (:domain-report captured-source)
        capture-report (:capture-report captured-source)
        observation (get-in frame-input [:frame-observation])
        before-time (phase-token-time! (:reported-pre-read-basis observation) :pre-read)
        after-time (phase-token-time! (:reported-post-read-basis observation) :post-read)
        phase-order (time-order before-time after-time)
        tx-time-order (reported-transaction-time-order capture-report
                                                       before-time after-time)
        _ (original-domain-check domain-source domain-report)
        captured-report (original-captured captured-source)
        frame-report (original-frame frame-input)
        status (cond
                 (= :threw (:outcome capture-report))
                 :submission-threw-physical-outcome-unknown
                 (= :pre-after-post phase-order)
                 :phase-snapshot-time-regressed
                 (#{:pre-phase-after-reported-transaction-time
                    :post-phase-before-reported-transaction-time} tx-time-order)
                 :phase-snapshot-time-contradicts-reported-transaction
                 (= :bounded-supplied-protected-frame-diverges
                    (:status frame-report))
                 :bounded-queried-protected-frame-diverges
                 (= :bounded-supplied-protected-frame-agrees
                    (:status frame-report))
                 :bounded-queried-protected-frame-agrees
                 :else :adapter-or-frame-comparison-unavailable)]
    ;; An altered upstream report supplied in source cannot be silently
    ;; accepted when this diagnostic is rechecked.
    (original-captured-check captured-source (:captured-report source))
    (original-frame-check frame-input (:frame-report source))
    (when-not (= captured-report (:captured-report source))
      (reject! :stale-or-forged-captured-report [:captured-report]))
    (when-not (= frame-report (:frame-report source))
      (reject! :stale-or-forged-frame-report [:frame-report]))
    {:type acquisition-type
     :version acquisition-version
     :checking :bounded-xtdb-queries-around-one-process-local-adapter-capture
     :scope :only-enumerated-frame-only-documents-on-caller-node
     :source source
     :status status
     :frame-only-target-count (:frame-only-target-count frame-report)
     :changed-frame-target-indices (:changed-frame-target-indices frame-report)
     :protected-field-difference-count (:different-protected-field-count frame-report)
     :captured-transaction-call-count (:captured-adapter-call-count frame-report)
     :capture-status (:status captured-report)
     :frame-status (:status frame-report)
     :submission-threw? (= :threw (:outcome capture-report))
     :read-origin :gesso-issued-xtdb-query-on-caller-node
     :read-sequence :pre-reads-then-captured-invocation-then-post-reads
     :phase-snapshot-token-query-count 2
     :read-query-count (* 2 (:frame-only-target-count frame-report))
     :per-phase-document-queries-use-pinned-token? true
     :per-phase-fixed-valid-time? true
     :phase-snapshot-time-order phase-order
     :reported-transaction-time-order tx-time-order
     :phase-token-time-comparable? (not= :not-comparable phase-order)
     :public-tx-time-is-causal-proof? false
     :same-node-used-for-read-arguments? true
     :node-identity-authenticated? false
     :submission-node-use-authenticated? false
     :each-phase-shared-snapshot? :not-established
     :read-basis-authenticated? false
     :transaction-internal-prestate :not-established
     :transaction-causality :not-established
     :transaction-outcome :not-established
     :complete-writer-coverage :not-established
     :physical-frame-for-protected-documents :not-established
     :physical-frame-outside-enumeration :not-established
     :proof-status :bounded-process-local-xtdb-frame-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :settlement-authority :none
     :retry-authority :none
     :authority :none}))

(defn acquire-controlled-xtdb-frame!
  "Input: {:node <caller-owned XTDB connectable>
          :domain-source <v1412 domain source>
          :submit! (fn [same-node-argument] ...)}.

   Phase 1: get a SHOW SNAPSHOT_TOKEN and pin SELECT * by _id for all
            frame-only targets to one token and fixed valid-time clock.
   Phase 2: execute submit! exactly once inside the v1390 adapter capture.
   Phase 3: get a NEW token and pin the post-reads to that phase's basis.
   Phase 4: recheck v1412, v1413 and v1415 reports.

   A thrown callback is captured, not retried; the post-read is still issued.
   If a post-read fails, no report is returned and the attempt may have
   committed. Do not infer noncommit or repeat the submission.

   The query basis, node identity, callback behavior and third-party writers
   are not authenticated. Even a matching result is only local diagnostic
   agreement, not a sound physical refinement certificate."
  [input]
  (exact! input #{:node :domain-source :submit!} [])
  (check-seams!)
  (let [{:keys [node domain-source submit!]} input]
    (when-not (and (some? node) (fn? submit!))
      (reject! :missing-node-or-submit-function []))
    (let [declared (original-domain domain-source)
          entries (frame-entries! declared)
          before (read-phase! node entries :pre-read)
          capture (original-capture #(submit! node))
          after (read-phase! node entries :post-read)
          captured-source {:domain-source domain-source
                           :domain-report declared
                           :capture-report capture}
          captured-report (original-captured captured-source)
          call-id (get-in capture [:events 0 :call-id])]
      ;; No useful frame comparison exists if no single valid adapter call is
      ;; recorded. In particular do not manufacture a UUID for a missing call.
      (when-not (and (= 2 (count (:events capture)))
                     (uuid? call-id))
        (reject! :not-exactly-one-observed-adapter-call [:capture-report]))
      (let [frame-input {:captured-source captured-source
                         :captured-report captured-report
                         :frame-observation
                         {:observation-origin :caller-supplied-unattested
                          :captured-call-id call-id
                          :pre-read-phase :before-attempt
                          :post-read-phase :after-attempt
                          :reported-pre-read-basis (:token before)
                          :reported-post-read-basis (:token after)
                          :pre-rows (:rows before)
                          :post-rows (:rows after)}}
            frame-report (original-frame frame-input)
            source {:frame-input frame-input
                    :captured-report captured-report
                    :frame-report frame-report}]
        (assemble! source)))))

(defn require-current-controlled-xtdb-frame! [source report]
  "Recompute the source-relative results; cannot replay or authenticate XTDB
   queries or a prior physical submission. Reject altered/copy-modified maps."
  (check-seams!)
  (exact! source #{:frame-input :captured-report :frame-report} [:source])
  (let [current (assemble! source)]
    (when-not (and (plain-map? report) (nil? (meta report))
                   (= report current))
      (reject! :stale-or-forged-acquisition-report [:report]))
    current))

(defn redacted-summary [report]
  "Return ONLY categories/counts/indices; never source, SQL, read documents,
   captured operations, transaction receipts, or callback return values."
  (dissoc (require-current-controlled-xtdb-frame! (:source report) report)
          :source))
