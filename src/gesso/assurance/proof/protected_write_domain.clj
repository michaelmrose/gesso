(ns gesso.assurance.proof.protected-write-domain
  "v1412: Explicit finite document/field protection domains for admitted
   gesso.model.tx command plans. This is a *declaration* layer for later
   physical write/frame accounting, not a physical observer or an assertion
   that the enumerated documents exhaust the deployed database.

   A canonical :put-docs command replaces a WHOLE persisted document; the
   document's current keys, or a syntactic before/after delta, are not its
   complete possible physical field-write set. A :fields #{...} protection
   declaration therefore cannot cover an entire command replacement, even
   when it names every currently known field. Only :fields :all declares
   whole-document protection for that *particular* target. Absence and
   deletion are included in :all.

   The supplied target set is finite and caller-enumerated. No wildcard for
   other documents, closed-world claim, provenance, authenticated XTDB
   payload, transaction-internal prestate, or certificate is implied.
   The detailed output retains target IDs and field names for subsequent
   checking; callers MUST treat it as sensitive. redacted-summary does not
   expose those values. Diagnostic exceptions also omit them."
  (:require [clojure.set :as set]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as tx-model]))

(def domain-version 1)
(def domain-type :gesso.assurance.proof.protected-write-domain/finite-declaration)

(def ^:private input-keys #{:plan :protected-documents})
(def ^:private entry-keys #{:target :fields})
(def ^:private max-commands 64)
(def ^:private max-protected-documents 256)
(def ^:private max-fields 256)
(def ^:private original-admitted tx-model/admitted-plan)
(def ^:private original-target command/target)
(def ^:private original-before command/before)
(def ^:private original-after command/after)

(defn- reject! [reason at]
  (throw (ex-info "Invalid finite protected-write domain declaration."
                  {:protected-write-domain/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- metadata-free? [x]
  (or (not (instance? clojure.lang.IObj x))
      (nil? (meta x))))

(defn- exact-map! [value fields at]
  (when-not (and (plain-map? value)
                 (metadata-free? value)
                 (= fields (set (keys value))))
    (reject! :invalid-map-shape at))
  value)

(defn- current-checkers! []
  (when-not (and (identical? original-admitted
                             (var-get #'tx-model/admitted-plan))
                 (identical? original-target (var-get #'command/target))
                 (identical? original-before (var-get #'command/before))
                 (identical? original-after (var-get #'command/after)))
    (reject! :substituted-upstream-checker [])))

(defn- target! [value at]
  ;; No wildcard/table-only identifier can stand for an entire physical
  ;; collection. It must designate exactly ONE caller-named document.
  (when-not (and (plain-vector? value) (metadata-free? value)
                 (= 2 (count value))
                 (keyword? (first value))
                 (some? (second value))
                 (not (coll? (second value)))
                 (not (fn? (second value))))
    (reject! :invalid-document-target at))
  value)

(defn- protected-entry! [entry i]
  (exact-map! entry entry-keys [:protected-documents i])
  (target! (:target entry) [:protected-documents i :target])
  (let [fields (:fields entry)]
    (when-not (or (= :all fields)
                  (and (plain-set? fields)
                       (metadata-free? fields)
                       (<= 1 (count fields) max-fields)
                       (every? keyword? fields)))
      (reject! :invalid-protected-field-domain
               [:protected-documents i :fields])))
  entry)

(defn- protected! [entries]
  (when-not (and (plain-vector? entries)
                 (metadata-free? entries)
                 (<= 1 (count entries) max-protected-documents))
    (reject! :invalid-protected-document-vector [:protected-documents]))
  (doseq [[i entry] (map-indexed vector entries)]
    (protected-entry! entry i))
  (when-not (= (count entries) (count (set (map :target entries))))
    (reject! :duplicate-protected-target [:protected-documents]))
  entries)

(defn- admitted! [raw-plan]
  ;; tx-model can reject unsupported SQL, custom execution hooks, and
  ;; noncanonical commands. Its error data may contain sensitive values;
  ;; expose only our own rejection reason and stable position.
  (try
    (let [plan (original-admitted raw-plan)
          commands (:commands plan)]
      (when-not (and (vector? commands)
                     (<= 1 (count commands) max-commands))
        (reject! :unsupported-command-count [:plan :commands]))
      (when-not (= (count commands)
                   (count (set (map original-target commands))))
        (reject! :duplicate-command-target [:plan :commands]))
      (doseq [[i c] (map-indexed vector commands)]
        (target! (original-target c) [:plan :commands i])
        (let [before (original-before c)
              after (original-after c)]
          (when-not (and (or (nil? before) (plain-map? before))
                         (plain-map? after)
                         (metadata-free? before)
                         (metadata-free? after)
                         (every? keyword? (keys after))
                         (or (nil? before)
                             (every? keyword? (keys before))))
            (reject! :invalid-command-document [:plan :commands i]))))
      plan)
    (catch clojure.lang.ExceptionInfo ex
      (if (:protected-write-domain/rejection (ex-data ex))
        (throw ex)
        (reject! :unsupported-or-invalid-admitted-plan [:plan])))
    (catch Exception _
      (reject! :unsupported-or-invalid-admitted-plan [:plan]))))

(defn- top-level-declared-delta [before after]
  ;; This is a syntactic Clojure map delta. XTDB may omit nil-valued entries
  ;; or normalize values. DO NOT infer an exact physical column/field delta.
  ;; Distinguish a missing key from a present key with a nil value.
  (let [prior (or before {})
        names (set/union (set (keys prior)) (set (keys after)))]
    (into #{}
          (filter (fn [field]
                    (or (not= (contains? prior field)
                              (contains? after field))
                        (not= (get prior field) (get after field)))))
          names)))

(defn derive-protected-write-domain!
  "Input: {:plan <ordinary Gesso model transaction plan>
          :protected-documents [{:target [entity-type id],
                                 :fields :all | #{field-keywords}} ...]}.

   Return the precise *declared* finite frame/write domain. :fields :all
   means all possible top-level and nested fields of this document, including
   absent fields and the ability to delete fields via replacement; a finite
   set protects ONLY those named top-level fields and cannot cover a whole
   :put-docs command. Unlisted targets are NOT implicitly protected.

   :declared-command-domains deliberately describes full-document effects,
   even when the returned :syntactic-delta-fields is empty. The latter is a
   diagnostic, not permission to narrow a write footprint.

   This is a SOURCE-RELATIVE result: no physical write observation occurs."
  [input]
  (exact-map! input input-keys [])
  (current-checkers!)
  (let [entries (protected! (:protected-documents input))
        plan (admitted! (:plan input))
        commands (:commands plan)
        by-target (into {} (map (juxt :target identity) entries))
        targets (mapv original-target commands)
        written-targets (set targets)
        details (mapv (fn [i c]
                        (let [target (nth targets i)
                              before (original-before c)
                              after (original-after c)
                              protection (:fields (get by-target target))]
                          {:index i
                           :target target
                           :operation (:model/operation c)
                           :effect :whole-document-replacement
                           :includes-field-deletion? true
                           :before-field-names (set (keys before))
                           :after-field-names (set (keys after))
                           :syntactic-delta-fields
                           (top-level-declared-delta before after)
                           :protection (cond
                                         (nil? protection) :absent
                                         (= :all protection) :whole-document
                                         :else :partial-fields)
                           :declared-full-document-covered?
                           (= :all protection)}))
                      (range (count commands)) commands)
        uncovered (into [] (comp (filter #(= :absent (:protection %)))
                               (map :index)) details)
        partial (into [] (comp (filter #(= :partial-fields (:protection %)))
                             (map :index)) details)
        frame-entries (filterv #(not (contains? written-targets (:target %)))
                               entries)
        full-covered? (and (empty? uncovered) (empty? partial))
        status (cond
                 (seq uncovered) :declared-command-target-outside-protection
                 (seq partial) :whole-document-replacement-partially-protected
                 :else :declared-command-targets-covered-in-finite-domain)]
    {:type domain-type
     :version domain-version
     :checking :admitted-abstract-command-and-caller-enumerated-protection
     :scope :caller-enumerated-finite-document-domain-only
     :source input
     :declared-command-domains details
     :protected-documents entries
     :frame-only-protected-documents frame-entries
     :declared-command-target-count (count commands)
     :protected-document-count (count entries)
     :protected-but-not-commanded-count (count frame-entries)
     :uncovered-command-indices uncovered
     :partially-protected-command-indices partial
     :declared-replacement-domain-covered? full-covered?
     :status status
     :physical-submission-writes-observed? false
     :physical-write-footprint-complete? :not-established
     :physical-frame-for-protected-documents :not-established
     :physical-frame-outside-enumeration :not-established
     :writer-path-closure :not-established
     :physical-snapshot-origin :not-established
     :proof-status :declared-finite-domain-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn require-current-protected-write-domain!
  "Rebuild the source-relative declaration and reject altered/stale output.
   It does not authenticate the caller's plan, enumerator or deployment."
  [input report]
  (let [current (derive-protected-write-domain! input)]
    (when-not (and (plain-map? report)
                   (metadata-free? report)
                   (= report current))
      (reject! :stale-or-forged-domain-report [:report]))
    current))

(defn redacted-summary
  "Counts, categorical statuses, and explicit open obligations only. Does
   not return source, document IDs, field names, or command payloads."
  [report]
  (let [current (require-current-protected-write-domain!
                 (:source report) report)]
    (select-keys current
                 [:type :version :checking :scope
                  :declared-command-target-count :protected-document-count
                  :protected-but-not-commanded-count
                  :uncovered-command-indices
                  :partially-protected-command-indices
                  :declared-replacement-domain-covered? :status
                  :physical-submission-writes-observed?
                  :physical-write-footprint-complete?
                  :physical-frame-for-protected-documents
                  :physical-frame-outside-enumeration
                  :writer-path-closure :physical-snapshot-origin
                  :proof-status :certificate :admission
                  :transaction-authority :authority])))
