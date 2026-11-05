(ns gesso.assurance.proof.controlled-xtdb-frame-acquisition
  "v1417: Acquire bounded frame-only pre/post rows by issuing real XTDB q
   calls on one caller-supplied node around ONE captured adapter invocation.

   The helper controls the order of its *own* calls, not the database's internal
   transaction execution or any concurrent writers. Each SELECT is a separate
   read, without an authenticated or shared snapshot basis. The submission
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
            [xtdb.api :as xt]))

(def acquisition-version 1)
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
                 (identical? original-query (var-get #'xt/q)))
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

(defn- read-rows! [node entries phase]
  (mapv (fn [i {:keys [target]}]
          (let [[table id] target
                rows (try
                       (original-query node
                                       [(str "SELECT * FROM " (name table)
                                             " WHERE _id = ?") id])
                       (catch Throwable _
                         (reject! :physical-read-failed [phase i])))]
            (when-not (and (sequential? rows) (<= (count rows) 1))
              (reject! :ambiguous-or-invalid-physical-read [phase i]))
            (let [doc (first rows)]
              (when-not (or (nil? doc)
                            (and (plain-map? doc) (nil? (meta doc))
                                 (every? keyword? (keys doc))
                                 (or (not (contains? doc :xt/id))
                                     (= id (:xt/id doc)))))
                (reject! :invalid-or-contradictory-physical-row [phase i]))
              {:target target :document doc})))
        (range (count entries)) entries))

(defn- assemble! [source]
  (let [frame-input (:frame-input source)
        captured-source (:captured-source frame-input)
        domain-source (:domain-source captured-source)
        domain-report (:domain-report captured-source)
        capture-report (:capture-report captured-source)
        _ (original-domain-check domain-source domain-report)
        captured-report (original-captured captured-source)
        frame-report (original-frame frame-input)
        status (cond
                 (= :threw (:outcome capture-report))
                 :submission-threw-physical-outcome-unknown
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
     :read-query-count (* 2 (:frame-only-target-count frame-report))
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

   Phase 1: SELECT * by _id for every declared frame-only target, in order.
   Phase 2: execute submit! exactly once inside the v1390 adapter capture.
   Phase 3: SELECT * for the SAME finite targets, in order.
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
          before (read-rows! node entries :pre-read)
          capture (original-capture #(submit! node))
          after (read-rows! node entries :post-read)
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
                          :reported-pre-read-basis nil
                          :reported-post-read-basis nil
                          :pre-rows before
                          :post-rows after}}
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
