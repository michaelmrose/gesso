(ns gesso.assurance.proof.controlled-xtdb-write-accounting
  "v1421: Bounded controlled XTDB readback for declared write targets AND
   frame-only protected documents around one process-local captured attempt.

   Each read phase uses ONE XTDB snapshot token and fixed valid time for all
   enumerated documents. This composes v1412 declared protection, v1413 exact
   adapter-plan comparison and v1415 finite frame comparison. The post-reads
   of commanded documents are compared to the prepared :put-docs payloads;
   pre-reads are also compared to the admitted abstract before-documents.

   Query equality is finite and subject to XTDB type/projection normalization.
   It is not an authenticated tx-internal prestate, evidence of which attempt
   caused a matching poststate, ledger-confirmed commit, or a global frame.
   A callback may use another node or perform untracked writes. NEVER retry.
   Detailed reports retain private documents and tx args: use redacted-summary."
  (:require [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.assurance.proof.captured-write-domain-correspondence :as captured]
            [gesso.assurance.proof.finite-physical-frame-observation :as frame]
            [gesso.live.consistency.xtdb :as live]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant ZonedDateTime]))

(def report-type :gesso.assurance.proof.controlled-xtdb-write-accounting/diagnostic)
(def report-version 1)
(def ^:private max-targets 256)
(def ^:private original-domain domain/derive-protected-write-domain!)
(def ^:private original-domain-check domain/require-current-protected-write-domain!)
(def ^:private original-captured captured/compare-captured-write-domain!)
(def ^:private original-captured-check captured/require-current-captured-write-domain!)
(def ^:private original-frame frame/compare-finite-physical-frame!)
(def ^:private original-frame-check frame/require-current-finite-physical-frame!)
(def ^:private original-capture live/capture-tx-api-calls!)
(def ^:private original-prepare tx/prepare)
(def ^:private original-before command/before)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)

(defn- reject! [reason at]
  (throw (ex-info "Invalid controlled XTDB write-accounting observation."
                  {:controlled-xtdb-write-accounting/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x ks at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= ks (set (keys x))))
    (reject! :invalid-map-shape at))
  x)

(defn- seams! []
  (when-not (and (identical? original-domain (var-get #'domain/derive-protected-write-domain!))
                 (identical? original-domain-check (var-get #'domain/require-current-protected-write-domain!))
                 (identical? original-captured (var-get #'captured/compare-captured-write-domain!))
                 (identical? original-captured-check (var-get #'captured/require-current-captured-write-domain!))
                 (identical? original-frame (var-get #'frame/compare-finite-physical-frame!))
                 (identical? original-frame-check (var-get #'frame/require-current-finite-physical-frame!))
                 (identical? original-capture (var-get #'live/capture-tx-api-calls!))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-before (var-get #'command/before))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-checker-or-acquisition-seam [])))

(defn- token-time! [token phase]
  (when-not (and (string? token) (seq token))
    (reject! :invalid-phase-token [phase]))
  (let [decoded (try (original-decode token)
                     (catch Throwable _ (reject! :invalid-phase-token [phase])))]
    (when-not (map? decoded)
      (reject! :invalid-phase-token [phase]))
    (when (and (= 1 (count decoded))
               (= 1 (count (val (first decoded))))
               (instance? Instant (first (val (first decoded)))))
      (first (val (first decoded))))))

(defn- entries! [domain-report]
  (let [entries (:protected-documents domain-report)]
    (when-not (and (vector? entries)
                   (<= 2 (count entries) max-targets)
                   (seq (:declared-command-domains domain-report))
                   (seq (:frame-only-protected-documents domain-report))
                   (:declared-replacement-domain-covered? domain-report))
      (reject! :incomplete-declared-write-or-frame-domain [:domain-source]))
    (doseq [[i {:keys [target]}] (map-indexed vector entries)]
      (let [[table id] target]
        (when-not (and (keyword? table) (nil? (namespace table))
                       (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                       (some? id) (not (coll? id)))
          (reject! :unsupported-physical-query-target [:protected-documents i]))))
    entries))

(defn- valid-row! [row entry phase i]
  (exact! row #{:target :document} [phase i])
  (when-not (= (:target row) (:target entry))
    (reject! :missing-reordered-or-unexpected-row [phase i]))
  (let [doc (:document row)
        id (second (:target entry))]
    (when-not (or (nil? doc)
                  (and (plain-map? doc) (nil? (meta doc))
                       (every? keyword? (keys doc))
                       (or (not (contains? doc :xt/id))
                           (= id (:xt/id doc)))))
      (reject! :invalid-observed-document [phase i])))
  row)

(defn- phase! [node entries phase]
  (let [token-rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                        (catch Throwable _ (reject! :physical-read-failed [phase :token])))
        token (when (and (sequential? token-rows) (= 1 (count token-rows))
                         (plain-map? (first token-rows)))
                (:snapshot-token (first token-rows)))
        _ (token-time! token phase)
        options {:snapshot-token token :current-time (Instant/now)}
        rows (mapv (fn [i {:keys [target] :as entry}]
                     (let [[table id] target
                           docs (try (original-query node
                                                     [(str "SELECT * FROM " (name table)
                                                           " WHERE _id = ? LIMIT 2") id]
                                                     options)
                                     (catch Throwable _
                                       (reject! :physical-read-failed [phase i])))]
                       (when-not (and (sequential? docs) (<= (count docs) 1))
                         (reject! :invalid-query-result [phase i]))
                       (valid-row! {:target target :document (first docs)} entry phase i)))
                   (range (count entries)) entries)]
    {:token token :rows rows}))

(defn- validate-phase! [phase-data entries phase]
  (exact! phase-data #{:token :rows} [phase])
  (token-time! (:token phase-data) phase)
  (let [rows (:rows phase-data)]
    (when-not (and (vector? rows) (nil? (meta rows))
                   (= (count entries) (count rows)))
      (reject! :incomplete-phase-readback [phase]))
    (doseq [[i [entry row]] (map-indexed vector (map vector entries rows))]
      (valid-row! row entry phase i)))
  phase-data)

(defn- normalized-time-equal? [a b]
  (cond
    (and (instance? Instant a) (instance? ZonedDateTime b))
    (= a (.toInstant ^ZonedDateTime b))
    (and (instance? ZonedDateTime a) (instance? Instant b))
    (= (.toInstant ^ZonedDateTime a) b)
    :else (= a b)))

(defn- doc-equal? [target expected observed]
  ;; SELECT * may omit :xt/id. Do not ignore any OTHER added or missing field.
  ;; Normalization is limited to Instant/ZonedDateTime, never coercive guessing.
  (let [normalize (fn [doc]
                    (when doc
                      (if (= (second target) (:xt/id doc))
                        (dissoc doc :xt/id)
                        doc)))
        expected (normalize expected)
        observed (normalize observed)]
    (or (and (nil? expected) (nil? observed))
        (and (plain-map? expected) (plain-map? observed)
             (= (set (keys expected)) (set (keys observed)))
             (every? (fn [[k v]] (normalized-time-equal? v (get observed k)))
                     expected)))))

(defn- assemble! [source]
  (exact! source #{:domain-source :domain-report :capture-report
                   :pre-phase :post-phase} [:source])
  (let [{:keys [domain-source domain-report capture-report pre-phase post-phase]} source
        _ (original-domain-check domain-source domain-report)
        entries (entries! domain-report)
        _ (validate-phase! pre-phase entries :pre-read)
        _ (validate-phase! post-phase entries :post-read)
        captured-source {:domain-source domain-source
                         :domain-report domain-report
                         :capture-report capture-report}
        captured-report (original-captured captured-source)
        _ (original-captured-check captured-source captured-report)
        events (:events capture-report)
        _ (when-not (and (= 2 (count events)) (uuid? (get-in events [0 :call-id])))
            (reject! :not-exactly-one-adapter-call [:capture-report]))
        targets (mapv :target entries)
        pre-by-target (zipmap targets (map :document (:rows pre-phase)))
        post-by-target (zipmap targets (map :document (:rows post-phase)))
        frame-entries (:frame-only-protected-documents domain-report)
        frame-row (fn [phase] (mapv (fn [entry]
                                      {:target (:target entry)
                                       :document (get (if (= phase :pre) pre-by-target post-by-target)
                                                      (:target entry))})
                                    frame-entries))
        frame-input {:captured-source captured-source
                     :captured-report captured-report
                     :frame-observation
                     {:observation-origin :caller-supplied-unattested
                      :captured-call-id (get-in events [0 :call-id])
                      :pre-read-phase :before-attempt
                      :post-read-phase :after-attempt
                      :reported-pre-read-basis (:token pre-phase)
                      :reported-post-read-basis (:token post-phase)
                      :pre-rows (frame-row :pre)
                      :post-rows (frame-row :post)}}
        frame-report (original-frame frame-input)
        _ (original-frame-check frame-input frame-report)
        prepared (try (original-prepare nil (:plan domain-source))
                      (catch Throwable _ (reject! :unsupported-canonical-preparation [:plan])))
        writes (filterv #(and (vector? %) (= :put-docs (first %))) (:tx-ops prepared))
        command-targets (mapv :target (:declared-command-domains domain-report))
        _ (when-not (= command-targets
                       (mapv (fn [op] [(second op) (:xt/id (nth op 2))]) writes))
            (reject! :prepared-command-write-target-mismatch [:plan]))
        command-befores (mapv original-before (get-in domain-source [:plan :commands]))
        pre-mismatches (->> (map vector command-targets command-befores)
                            (keep-indexed (fn [i [target expected]]
                                            (when-not (doc-equal? target expected
                                                                  (get pre-by-target target)) i))) vec)
        post-mismatches (->> (map vector command-targets writes)
                             (keep-indexed (fn [i [target op]]
                                             (when-not (doc-equal? target (nth op 2)
                                                                   (get post-by-target target)) i))) vec)
        pre-time (token-time! (:token pre-phase) :pre-read)
        post-time (token-time! (:token post-phase) :post-read)
        time-order (if (and pre-time post-time)
                     (if (pos? (compare pre-time post-time))
                       :pre-after-post :pre-at-or-before-post)
                     :not-comparable)
        submitted? (= :returned (:outcome capture-report))
        capture-match? (= :bounded-captured-adapter-operations-agree
                          (:status captured-report))
        frame-match? (= :bounded-supplied-protected-frame-agrees
                        (:status frame-report))
        status (cond
                 (not submitted?) :submission-outcome-unknown
                 (not capture-match?) :adapter-plan-correspondence-unavailable
                 (= :pre-after-post time-order) :read-basis-time-regressed
                 (seq pre-mismatches) :queried-prestate-diverges-from-admitted-before
                 (seq post-mismatches) :queried-poststate-diverges-from-prepared-writes
                 (not frame-match?) :queried-protected-frame-diverges
                 :else :bounded-queried-write-and-frame-agreement)]
    {:type report-type
     :version report-version
     :source source
     :status status
     :checking :controlled-pinned-pre-and-post-reads-of-finite-declared-write-and-frame-domains
     :write-target-count (count command-targets)
     :frame-target-count (count frame-entries)
     :protected-target-count (count entries)
     :read-query-count (* 2 (count entries))
     :phase-snapshot-token-query-count 2
     :pre-read-token-time-order time-order
     :prestate-mismatch-command-indices pre-mismatches
     :poststate-mismatch-command-indices post-mismatches
     :changed-protected-frame-indices (:changed-frame-target-indices frame-report)
     :captured-write-status (:status captured-report)
     :finite-frame-status (:status frame-report)
     :submission-threw? (not submitted?)
     :phase-queries-use-shared-pinned-token? true
     :physical-read-origin :xtdb-query-on-caller-supplied-node
     :read-basis-authenticated? false
     :node-identity-authenticated? false
     :submission-node-use-authenticated? false
     :actual-submitted-ops-authenticated? false
     :actual-commit-causality :not-established
     :transaction-internal-prestate :not-established
     :all-writer-paths-covered? :not-established
     :complete-physical-write-footprint :not-established
     :global-physical-frame :not-established
     :proof-status :bounded-local-write-and-frame-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :retry-authority :none
     :settlement-authority :none
     :authority :none}))

(defn acquire-controlled-xtdb-write-accounting!
  "Read every enumerated protected document at a pinned pre-basis, invoke
   submit! EXACTLY ONCE inside one process-local adapter capture, then read
   all documents at a separately pinned post-basis. Missing/failed postreads
   after submission may be an unknown commit: NEVER retry. The callback may
   ignore the supplied node, and other writers are unobserved."
  [input]
  (exact! input #{:node :domain-source :submit!} [])
  (seams!)
  (let [{:keys [node domain-source submit!]} input]
    (when-not (and (some? node) (fn? submit!))
      (reject! :missing-node-or-submit-function []))
    (let [declared (original-domain domain-source)
          entries (entries! declared)
          before (phase! node entries :pre-read)
          capture (original-capture #(submit! node))
          after (phase! node entries :post-read)]
      (assemble! {:domain-source domain-source
                  :domain-report declared
                  :capture-report capture
                  :pre-phase before
                  :post-phase after}))))

(defn require-current-controlled-xtdb-write-accounting! [source report]
  "Recheck supplied evidence and reject modified reports; cannot authenticate
   prior physical reads by replaying an observation map."
  (seams!)
  (let [current (assemble! source)]
    (when-not (and (plain-map? report) (nil? (meta report))
                   (= current report))
      (reject! :stale-or-forged-accounting-report [:report]))
    current))

(defn redacted-summary [report]
  "Positional indices and statuses only; no targets, values, tokens, SQL or
   captured submissions."
  (dissoc (require-current-controlled-xtdb-write-accounting!
           (:source report) report) :source))
