(ns gesso.assurance.proof.xtdb-gateway-atomic-write-intent
  "v1887: Opt-in SAME-TRANSACTION physical write-intent production.

   Derive one ordinary Gesso CREATE command for a restricted, generated
   write-intent document and append it to the caller's admitted protected
   application plan BEFORE the original one-shot XTDB gateway re-admits and
   executes its entire vector. Thus a successful atomic XTDB transaction
   contains both the protected application PUT and the intent PUT; an XTDB
   assertion failure cannot commit only the generated intent.

   This is not an erasure/DELETE writer. The generated record is a scoped
   statement of intended non-ERASE work, NOT an authenticated audit event,
   independent observation of execute-tx, durable/complete producer log,
   proof of commit, evidence of transaction causality, writer coverage,
   history retention, or a certificate. The caller supplies the XTDB node.
   The published gateway is the unchanged original one-shot descriptor.
   Its per-call local evidence remains subject to all v1886 restrictions."
  (:require [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as tx-model]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant ZonedDateTime]
           [java.time.temporal ChronoUnit]
           [java.util UUID WeakHashMap]
           [xtdb.api TransactionKey]))

(def intent-table :gesso_guarded_write_intents)
(def intent-kind "owned-guarded-nonerase-write-intent")
(def readback-type :gesso.assurance.proof.xtdb-gateway-atomic-write-intent/readback)
(def readback-version 1)

;; Weakly keyed by the ORIGINAL one-shot submission closure; no caller can
;; replace the private generated document, observed node, or source identity.
;; Not a durable log or authenticated process trust root.
(defonce ^:private ^WeakHashMap private-intents (WeakHashMap.))
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)
(def ^:private original-local-return gateway/require-local-return!)
(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- supported-protected-command [commands]
  ;; An additional read of a caller-named SQL table must never turn an
  ;; admitted model keyword into SQL text unchecked. Other canonical plan
  ;; shapes remain admissible but have NO protected-poststate correlation.
  (when (= 1 (count commands))
    (let [c (first commands)
          [table id] (command/target c)
          after (command/after c)]
      (when (and (keyword? table)
                 (nil? (namespace table))
                 (re-matches #"[a-z][a-z0-9_]{0,127}" (name table))
                 (or (and (string? id) (<= 1 (count id) 128))
                     (instance? UUID id))
                 (map? after)
                 (= id (:xt/id after)))
        {:table-name (name table)
         :row-id id
         :expected-before (command/before c)
         :expected-after after}))))

(defn- reject! [reason]
  (throw (ex-info "Unsupported atomic guarded-write intent."
                  {:xtdb-gateway-atomic-write-intent/rejection true
                   :reason reason})))

(defn prepare-one-shot!
  "Make the ORIGINAL gateway prepare a single vector containing both the
   caller's complete admitted application plan and one GENERATED CREATE for
   a distinct write-intent document. No separate audit writer is used.

   Input is exactly the same source shape as the restricted gateway:
   {:plan <existing Gesso plan with :emit false>
    :protected-documents <finite full-document declarations>}.

   Generated fields are a fresh identifier, version metadata, an explicit
   intent-only kind and count of original application commands. The source
   and target identities, principal, credentials, and returned transaction
   key are never written to the intent record. The generated table is
   reserved here: original commands/protection cannot write it.

   The returned descriptor is exactly the original gateway, including its
   one-shot submit! and summary!. It grants no new ERASE, DELETE or retry."
  [node input]
  (when-not (and (map? input)
                 (= #{:plan :protected-documents} (set (keys input)))
                 (vector? (:protected-documents input)))
    (reject! :invalid-original-source))
  (let [admitted (try
                   (tx-model/admitted-plan (:plan input))
                   (catch Exception _
                     (reject! :unadmitted-original-plan)))
        commands (:commands admitted)
        protections (:protected-documents input)]
    (when-not (and (= false (:emit admitted))
                   (vector? commands)
                   (<= 1 (count commands) 63)
                   (not-any? #(= intent-table (first (command/target %)))
                             commands)
                   (not-any? #(= intent-table (first (:target %)))
                             protections))
      (reject! :reserved-intent-table-or-unsupported-plan))
    (let [stamp (Instant/now)
          record {:xt/id (str (UUID/randomUUID))
                  :revision 0
                  :created-at stamp
                  :updated-at stamp
                  :intent-kind intent-kind
                  :application-command-count (count commands)
                  :intent-scope "one-admitted-xtdb-transaction-vector-only"}
          audit-command (command/create intent-table record version-keys)
          complete-source
          {:plan (update admitted :commands conj audit-command)
           :protected-documents
           (conj protections {:target [intent-table (:xt/id record)]
                              :fields :all})}]
      ;; The ORIGINAL gateway re-admits the entire enlarged plan, including
      ;; the generated intent PUT. It alone owns the physical execute-tx.
      ;; No gateway callback or second transaction may produce this record.
      (let [g (gateway/prepare-one-shot! node complete-source)]
        (locking private-intents
          (.put private-intents (:submit! g)
                {:summary! (:summary! g) :node node :record record
                 :original-protected (supported-protected-command commands)}))
        g))))

(defn- observed-time [v]
  (cond
    (instance? Instant v) v
    (instance? ZonedDateTime v) (.toInstant ^ZonedDateTime v)
    :else nil))

(defn- private-record-matches? [record found]
  (and (map? found)
       (= (set (keys record)) (set (keys found)))
       (= (dissoc record :created-at :updated-at)
          (dissoc found :created-at :updated-at))
       ;; XTDB's TIMESTAMP preserves microseconds, not Instant nanos.
       (= (.truncatedTo ^Instant (:created-at record) ChronoUnit/MICROS)
          (observed-time (:created-at found)))
       (= (.truncatedTo ^Instant (:updated-at record) ChronoUnit/MICROS)
          (observed-time (:updated-at found)))))

(defn- private-document-matches? [expected observed]
  ;; XTDB converts Instant nanoseconds to microseconds, and may return
  ;; ZonedDateTime values. Compare ALL fields under only this representation
  ;; normalization, never accepting missing or additional document keys.
  (let [normalized (fn [v]
                     (cond
                       (instance? Instant v)
                       (.truncatedTo ^Instant v ChronoUnit/MICROS)
                       (instance? ZonedDateTime v)
                       (.toInstant ^ZonedDateTime v)
                       :else v))]
    (and (map? observed)
         (= (set (keys expected)) (set (keys observed)))
         (every? (fn [[k v]]
                   (= (normalized v) (normalized (get observed k))))
                 expected))))

(defn- historical-after-correspondence
  "Compare one already-bounded historical row with the privately retained
   admitted AFTER and returned transaction time. Does not query, create
   audit records, certify causality, or infer history completeness."
  [expected-after top returned-time]
  (let [historical-document
        (apply dissoc top [:xt/system-from :xt/system-to
                           :xt/valid-from :xt/valid-to])
        valid-from (observed-time (:xt/valid-from top))]
    (cond
      (not= returned-time valid-from)
      :latest-historical-valid-start-differs-from-returned-tx
      (not (private-document-matches? expected-after historical-document))
      :latest-historical-after-diverges-from-private-after
      :else
      :private-after-matches-latest-historical-version)))

(def ^:private diagnostic-premise-definitions
  ;; An ordered, finite vocabulary of RAW redacted findings from the one
  ;; original private readback. Never trust a caller-supplied claim or an
  ;; already-composed aggregate as a premise. These equality checks state
  ;; observational agreement, NOT an independently justified proof rule.
  [[:exact-original-return :returned-key-origin
    :exact-one-shot-gateway-return]
   [:private-intent-origin :expected-intent-origin
    :private-original-prepared-gateway]
   [:one-pinned-snapshot :both-observations-share-pinned-basis? true]
   [:basis-after-return :read-basis-order
    :pinned-basis-not-before-original-return]
   [:committed-ledger :returned-ledger-status
    :matches-original-returned-committed-row]
   [:generated-intent :intent-record-status
    :matches-private-generated-intent]
   [:generated-intent-history :intent-history-origin-status
    :one-open-visible-history-version-starts-at-returned-tx]
   [:protected-current-after :protected-original-poststate-status
    :matches-private-original-protected-after]
   [:protected-latest-origin :protected-original-history-origin-status
    :latest-open-protected-version-starts-at-returned-tx]
   [:protected-historical-after :protected-original-after-history-status
    :private-after-matches-latest-historical-version]
   [:protected-historical-before :protected-original-before-history-status
    :private-original-before-matches-immediate-predecessor]])

(def ^:private diagnostic-claim-dependencies
  ;; Explicit monotonic dependency DAG, from the least constrained to the
  ;; most constrained observation. Only SAME-BASIS comparison findings are
  ;; eligible; no dependent claim can bypass a missing earlier premise.
  (array-map
   :intent-ledger-coobservation
   [:exact-original-return :private-intent-origin :one-pinned-snapshot
    :basis-after-return :committed-ledger :generated-intent]
   :intent-temporal-correlation
   [:exact-original-return :private-intent-origin :one-pinned-snapshot
    :basis-after-return :committed-ledger :generated-intent
    :generated-intent-history]
   :protected-current-coobservation
   [:exact-original-return :private-intent-origin :one-pinned-snapshot
    :basis-after-return :committed-ledger :generated-intent
    :generated-intent-history :protected-current-after]
   :protected-dual-history-correlation
   [:exact-original-return :private-intent-origin :one-pinned-snapshot
    :basis-after-return :committed-ledger :generated-intent
    :generated-intent-history :protected-current-after
    :protected-latest-origin]
   :protected-before-after-history-correlation
   [:exact-original-return :private-intent-origin :one-pinned-snapshot
    :basis-after-return :committed-ledger :generated-intent
    :generated-intent-history :protected-current-after
    :protected-latest-origin :protected-historical-after
    :protected-historical-before]))

(def ^:private diagnostic-open-obligations
  ;; No ordering of bounded observations can close these by itself.
  [:authenticated-node-and-execution-identity
   :exclusive-transaction-causality
   :durable-authenticated-audit-event-producer
   :complete-audit-event-ingestion
   :complete-deployment-writer-coverage
   :retention-and-physical-erasure-completeness
   :independent-executable-refinement-proof])

(defn- compose-diagnostic-correspondence
  "Compose ONLY redacted observations from this same private pinned read.
   This internal, pure function also permits adversarial unit tests; neither
   a caller-constructed map nor this returned plain-data structure is a
   trusted proof object, cryptographic attestation, admission or certificate."
  [observed]
  (let [premises
        (into (sorted-map)
              (for [[name field required] diagnostic-premise-definitions
                    :let [found (get observed field ::unavailable)]]
                [name {:status (if (= required found)
                                 :observational-match :unestablished)
                       :source-field field
                       :expected required
                       :observed (if (= ::unavailable found)
                                   :missing-observation found)}]))
        nodes
        (into (sorted-map)
              (for [[name deps] diagnostic-claim-dependencies
                    :let [blocked (->> deps
                                       (remove #(= :observational-match
                                                   (get-in premises [% :status])))
                                       vec)]]
                [name {:depends-on deps
                       :status (if (empty? blocked)
                                 :bounded-premises-present :blocked)
                       :blocked-by blocked}]))
        strongest (->> diagnostic-claim-dependencies
                       keys
                       (filter #(= :bounded-premises-present
                                   (get-in nodes [% :status])))
                       last)]
    {:type :gesso.assurance.proof.xtdb-gateway-atomic-write-intent/diagnostic-premises
     :version 1
     :scope :one-original-gateway-return-and-one-independent-pinned-read
     :premises premises
     :dependency-nodes nodes
     :strongest-bounded-node (or strongest :none)
     :open-obligations diagnostic-open-obligations
     :open-obligation-count (count diagnostic-open-obligations)
     :closed-obligations []
     :evidence-authenticity :not-established
     :record-execution-causality :not-established
     :history-completeness :not-established
     :audit-producer-completeness :not-established
     :proof-status :diagnostic-plain-data-not-a-proof-object
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :retry-authority :none
     :authority :none}))

(defn inspect-pinned-intent!
  "Read back the PRIVATE v1887 generated intent and original returned-key
   ledger row at ONE independent, pinned XTDB basis. Accept ONLY the original
   gateway descriptor and the exact object returned by its one-shot submit!;
   no caller supplies an expected intent, ID, ledger key, basis or node.

   A matching generated document and committed xt.txs row at a later basis
   corroborate physical co-visibility. They cannot prove which transaction
   created the record, prevent later alteration, authenticate the supplied
   node, establish complete writer/audit capture, or authorize any effect.
   No write, retry, independent audit discharge or certificate occurs.

   v1894: Compose the already-observed redacted premises into an explicit
   closed dependency DAG of BOUNDED diagnostics and a separate invariant
   list of still-open physical and audit obligations. Every node fails
   closed if any supporting field is absent or disagrees; even fully matched
   nodes never become proofs, write authority or certificates.

   v1889 additionally reads the generated row's bounded SYSTEM and VALID
   time history on the SAME pinned snapshot. One open version beginning at
   the returned transaction's system time adds temporal correspondence;
   two or more visible version records (including a same-value overwrite)
   are explicitly insufficient for a unique origin claim. These are row
   observations, not independent transaction causality or writer closure.

   v1890: A third co-observation compares the one original canonical
   protected command's expected after-document to the current document at
   that SAME pinned basis, with a strict private table/ID binding. Later
   unauthorized rewrites must not leave a falsely positive three-way match.
   Only one supported original command is checked: other plan shapes are
   explicitly not established rather than silently certified.

   v1891: A separate bounded, newest-first historical observation of the
   original protected row checks that the latest open physical version began
   at the exact returned transaction system time. Unlike a count of all
   versions, this allows the legitimate pre-update history of an UPDATE.
   An unregistered same-value PUT creates a newer system version and revokes
   the stronger dual-row temporal correlation. Bitemporal ties and missing
   history remain explicitly inconclusive. Neither a timestamp match nor
   absence of a visible newer version establishes transaction causality.

   v1892: For a supported UPDATE, also compare the complete predecessor
   document visible in this SAME bounded pinned history with the privately
   retained admitted command/before. Require the preceding valid-time
   interval to close at the returned transaction time. This checks a
   declared BEFORE/AFTER pair, not authenticated earlier execution,
   historic completeness, or exclusive physical transaction causality.

   v1893: Check the complete latest historical AFTER document and its
   valid-time start against the privately admitted command/after, rather
   than relying on a current-value read and historical timestamps alone.
   Keep all historical data private and restrict the stronger before/after
   correlation to an exact same-pinned historical post-image match."
  [g result]
  (let [local (original-local-return g result)
        private (locking private-intents
                  (.get private-intents (:submit! g)))
        {:keys [summary! node record original-protected]} private]
    (when-not (and private
                   (identical? summary! (:summary! g))
                   (= :public-transaction-key (:observed-result-kind local))
                   (= 1 (:owned-execute-tx-call-entries local)))
      (reject! :not-original-generated-intent-descriptor))
    (when-not (and (identical? original-query (var-get #'xt/q))
                   (identical? original-decode
                               (var-get #'basis/<-time-basis-str))
                   (identical? original-local-return
                               (var-get #'gateway/require-local-return!)))
      (reject! :substituted-readback-seam))
    (let [^TransactionKey key (:tx-key result)
          tx-id (.getTxId key)
          tx-time (.getSystemTime key)
          token-rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                          (catch Throwable _ (reject! :snapshot-unavailable)))
          token (when (and (sequential? token-rows)
                           (= 1 (count token-rows))
                           (= #{:snapshot-token} (set (keys (first token-rows)))))
                  (:snapshot-token (first token-rows)))
          decoded (try (when (and (string? token) (seq token))
                         (original-decode token))
                       (catch Throwable _ nil))]
      (when-not (and (map? decoded) (= 1 (count decoded))
                     (= 1 (count (val (first decoded))))
                     (instance? Instant (first (val (first decoded)))))
        (reject! :invalid-private-intent-snapshot))
      (let [point (first (val (first decoded)))
            read-basis {:snapshot-token token :current-time (Instant/now)}
            query-one (fn [sql param]
                        (let [rows (try (original-query node [sql param] read-basis)
                                        (catch Throwable _
                                          (reject! :pinned-intent-read-unavailable)))]
                          (when-not (and (sequential? rows) (<= (count rows) 1)
                                         (every? map? rows))
                            (reject! :ambiguous-private-intent-read))
                          (first rows)))
            ;; Preserve the existing single pinned basis. A bounded history
            ;; query cannot certify complete retention or immutable audit;
            ;; two returned records already prevent unique visible-version
            ;; correspondence (and valid-time splitting can yield several
            ;; records for one physical transaction).
            intent-history-sql
            (str "SELECT _system_from, _system_to"
                 " FROM gesso_guarded_write_intents"
                 " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL"
                 " WHERE _id = ? LIMIT 2")
            history-rows (try (original-query node
                                              [intent-history-sql
                                               (:xt/id record)]
                                              read-basis)
                              (catch Throwable _
                                (reject! :pinned-intent-history-unavailable)))
            _ (when-not (and (sequential? history-rows)
                             (<= 0 (count history-rows) 2)
                             (every? (fn [row]
                                       (and (map? row)
                                            (contains? row :xt/system-from)
                                            (instance? ZonedDateTime
                                                       (:xt/system-from row))
                                            (every? #{:xt/system-from
                                                      :xt/system-to}
                                                    (keys row))
                                            (or (not (contains? row
                                                                :xt/system-to))
                                                (instance? ZonedDateTime
                                                           (:xt/system-to row)))))
                                     history-rows))
                (reject! :invalid-pinned-intent-history))
            history-status
            (cond
              (empty? history-rows) :no-visible-pinned-intent-history
              (= 2 (count history-rows))
              :multiple-visible-pinned-intent-history-records-at-bound
              :else
              (let [row (first history-rows)
                    from (observed-time (:xt/system-from row))]
                (cond
                  (not= tx-time from)
                  :one-visible-history-version-start-differs-from-returned-tx
                  (contains? row :xt/system-to)
                  :one-visible-history-version-start-matches-but-is-closed
                  :else
                  :one-open-visible-history-version-starts-at-returned-tx)))
            ;; These are independent reads but all use the SAME immutable
            ;; pin. Restrict SQL identifiers to our checked original source;
            ;; use a parameter for the private original row ID.
            protected-row (when original-protected
                            (query-one (str "SELECT * FROM "
                                            (:table-name original-protected)
                                            " WHERE _id = ? LIMIT 2")
                                       (:row-id original-protected)))
            protected-status (cond
                               (nil? original-protected)
                               :not-checked-unsupported-original-plan-shape
                               (nil? protected-row) :not-visible
                               (private-document-matches?
                                (:expected-after original-protected)
                                protected-row)
                               :matches-private-original-protected-after
                               :else :diverges-from-private-original-after)
            ;; The original protected row normally has BEFORE history.
            ;; Sorting newest-first (and LIMIT 2 for ties) tests ONLY the
            ;; most recent version; requiring one total version would reject
            ;; every ordinary UPDATE. Both reads share the same pinned token.
            protected-history-rows
            (when original-protected
              (try
                (original-query
                 node
                 [(str "SELECT *, _system_from, _system_to, _valid_from, _valid_to"
                       " FROM " (:table-name original-protected)
                       " FOR VALID_TIME ALL FOR SYSTEM_TIME ALL"
                       " WHERE _id = ? ORDER BY _system_from DESC LIMIT 2")
                  (:row-id original-protected)]
                 read-basis)
                (catch Throwable _
                  (reject! :pinned-protected-history-unavailable))))
            protected-history-valid?
            (or (nil? original-protected)
                (and (sequential? protected-history-rows)
                     (<= (count protected-history-rows) 2)
                     (every? (fn [row]
                               (and (map? row)
                                    (= (:row-id original-protected) (:xt/id row))
                                    (instance? ZonedDateTime
                                               (:xt/system-from row))
                                    (every? (fn [k]
                                              (or (not (contains? row k))
                                                  (instance? ZonedDateTime
                                                             (get row k))))
                                            [:xt/system-to :xt/valid-from
                                             :xt/valid-to])))
                             protected-history-rows)))
            _ (when-not protected-history-valid?
                (reject! :invalid-pinned-protected-history))
            protected-history-status
            (if-not original-protected
              :not-checked-unsupported-original-plan-shape
              (if (empty? protected-history-rows)
                :no-visible-pinned-protected-history
                (let [top (first protected-history-rows)
                      top-from (observed-time (:xt/system-from top))
                      next-from (some-> protected-history-rows second
                                        :xt/system-from observed-time)]
                  (cond
                    (> (compare top-from tx-time) 0)
                    :newer-protected-system-version-than-returned-tx
                    (< (compare top-from tx-time) 0)
                    :latest-protected-system-version-precedes-returned-tx
                    (= top-from next-from)
                    :ambiguous-latest-protected-system-time-tie
                    (or (contains? top :xt/system-to)
                        (contains? top :xt/valid-to))
                    :latest-protected-version-not-open
                    :else
                    :latest-open-protected-version-starts-at-returned-tx))))
            ;; Unlike the current-valid row read, this inspects the full
            ;; physical version whose SYSTEM start matches the returned tx.
            ;; Compare the complete historical post-image and the valid-time
            ;; start. Do not promote a timestamp-only match into a claim
            ;; about the admitted AFTER value.
            protected-after-history-status
            (cond
              (nil? original-protected)
              :not-checked-unsupported-original-plan-shape
              (not= :latest-open-protected-version-starts-at-returned-tx
                    protected-history-status)
              :latest-protected-version-not-origin-anchored
              :else
              (historical-after-correspondence
               (:expected-after original-protected)
               (first protected-history-rows)
               tx-time))
            ;; For UPDATEs, retain the ORIGINAL admitted before-document
            ;; privately and compare its complete persisted fields with
            ;; the immediate preceding historical version on the SAME pin.
            ;; The v1891 newest-first LIMIT 2 and time-tie refusal remain
            ;; mandatory: this is not a scan or proof of complete history.
            protected-before-status
            (cond
              (nil? original-protected)
              :not-checked-unsupported-original-plan-shape
              (not (map? (:expected-before original-protected)))
              :not-checked-original-command-has-no-before
              (not= :latest-open-protected-version-starts-at-returned-tx
                    protected-history-status)
              :latest-protected-version-not-origin-anchored
              (< (count protected-history-rows) 2)
              :no-visible-immediate-protected-predecessor
              :else
              (let [predecessor (second protected-history-rows)
                    predecessor-document
                    (apply dissoc predecessor
                           [:xt/system-from :xt/system-to
                            :xt/valid-from :xt/valid-to])
                    predecessor-from (observed-time
                                      (:xt/system-from predecessor))
                    predecessor-valid-to (observed-time
                                          (:xt/valid-to predecessor))]
                (cond
                  (not (and predecessor-from
                            (neg? (compare predecessor-from tx-time))
                            (= tx-time predecessor-valid-to)))
                  :immediate-predecessor-not-valid-time-adjacent
                  (private-document-matches?
                   (:expected-before original-protected)
                   predecessor-document)
                  :private-original-before-matches-immediate-predecessor
                  :else
                  :immediate-predecessor-diverges-from-private-before)))
            intent-row (query-one
                        "SELECT * FROM gesso_guarded_write_intents WHERE _id = ? LIMIT 2"
                        (:xt/id record))
            ledger-row (query-one
                        "SELECT _id, committed, system_time FROM xt.txs WHERE _id = ? LIMIT 2"
                        tx-id)
            intent-status (cond
                            (nil? intent-row) :not-visible
                            (private-record-matches? record intent-row)
                            :matches-private-generated-intent
                            :else :diverges-from-private-generated-intent)
            ledger-status (cond
                            (nil? ledger-row) :not-visible
                            (not= #{:xt/id :committed :system-time}
                                  (set (keys ledger-row))) :invalid-ledger-row
                            (and (= tx-id (:xt/id ledger-row))
                                 (true? (:committed ledger-row))
                                 (= tx-time (observed-time
                                              (:system-time ledger-row))))
                            :matches-original-returned-committed-row
                            :else :diverges-from-original-return)
            ordering (if (neg? (compare point tx-time))
                       :pinned-basis-precedes-original-return
                       :pinned-basis-not-before-original-return)
            positive? (and (= :matches-private-generated-intent intent-status)
                           (= :matches-original-returned-committed-row
                              ledger-status)
                           (= :pinned-basis-not-before-original-return ordering))]
        ;; Protect the read-only seam from mid-observation replacement, too.
        (when-not (and (identical? original-query (var-get #'xt/q))
                       (identical? original-decode
                                   (var-get #'basis/<-time-basis-str)))
          (reject! :substituted-readback-seam))
        (let [report {:type readback-type
         :version readback-version
         :status (if positive?
                   :bounded-private-intent-and-ledger-coobservation
                   :intent-or-ledger-correspondence-not-established)
         :intent-record-status intent-status
         :returned-ledger-status ledger-status
         :read-basis-order ordering
         :both-observations-share-pinned-basis? true
         :protected-original-poststate-status protected-status
         :protected-original-poststate-scope
         :one-original-canonical-command-at-same-pinned-basis
         :protected-intent-ledger-coobservation
         (if (and positive?
                  (= :matches-private-original-protected-after
                     protected-status)
                  (= :one-open-visible-history-version-starts-at-returned-tx
                     history-status))
           :bounded-three-way-physical-coobservation
           :not-established)
         :protected-original-poststate-proves-transaction-causality? false
         :protected-original-history-origin-status protected-history-status
         :protected-original-history-observation-limit 2
         :protected-original-history-completeness :not-established
         :protected-intent-ledger-temporal-correlation
         (if (and positive?
                  (= :matches-private-original-protected-after
                     protected-status)
                  (= :one-open-visible-history-version-starts-at-returned-tx
                     history-status)
                  (= :latest-open-protected-version-starts-at-returned-tx
                     protected-history-status))
           :bounded-dual-row-history-and-ledger-coobservation
           :not-established)
         :protected-temporal-correlation-proves-transaction-causality? false
         :protected-original-after-history-status protected-after-history-status
         :protected-after-history-scope
         :private-original-after-compared-with-pinned-latest-historical-version
         :protected-historical-after-completeness :not-established
         :protected-original-before-history-status protected-before-status
         :protected-before-history-scope
         :private-original-before-compared-with-pinned-immediate-predecessor
         :protected-before-after-intent-ledger-correlation
         (if (and positive?
                  (= :matches-private-original-protected-after
                     protected-status)
                  (= :one-open-visible-history-version-starts-at-returned-tx
                     history-status)
                  (= :latest-open-protected-version-starts-at-returned-tx
                     protected-history-status)
                  (= :private-original-before-matches-immediate-predecessor
                     protected-before-status)
                  (= :private-after-matches-latest-historical-version
                     protected-after-history-status))
           :bounded-private-before-after-intent-ledger-coobservation
           :not-established)
         :protected-before-history-completeness :not-established
         :protected-before-after-correlation-proves-causality? false
         :intent-history-origin-status history-status
         :returned-tx-time-intent-history-correlation
         (if (and positive?
                  (= :one-open-visible-history-version-starts-at-returned-tx
                     history-status))
           :bounded-same-pinned-basis-history-start-matches-returned-tx
           :not-established)
         :intent-history-observation-limit 2
         :intent-history-completeness :not-established
         :expected-intent-origin :private-original-prepared-gateway
         :returned-key-origin :exact-one-shot-gateway-return
         :original-local-attempt-id (:process-local-attempt-id local)
         :transaction-causality :not-established
         :audit-event-producer :not-established
         :caller-node-authenticity :not-established
         :outside-gateway-writer-coverage :not-established
         :complete-history :not-established
         :record-immutability :not-established
         :physical-erasure-proof :none
         :retry-authority :none
         :transaction-authority :none
         :admission :not-admitted
         :certificate :not-issued
         :authority :none}]
          (assoc report :correspondence-premises
                 (compose-diagnostic-correspondence report)))))))
