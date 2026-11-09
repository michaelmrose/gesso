(ns gesso.assurance.proof.xtdb-gateway-unknown-command-reconciliation
  "v1598: Read-only, independently pinned XTDB command-record observation
   after an indeterminate Biff/XTDB request outcome.

   The v1597 recovery tests show that the domain document may change even
   when the caller receives an exception. Inspecting that document alone
   cannot establish durable command identity or distinguish a retry from a
   different successful operation. This module reads the canonical
   :gesso_commands record BY the exact typed command ID, using a pinned
   external XTDB snapshot, then applies the existing command-resolution
   binding/disclosure rules. It never infers command incorporation from a
   matching domain document, performs no writes, and issues no retry permit.

   A compatible record, even from a genuine XTDB read, does not show WHICH
   attempt incorporated it. A missing record at one external read does not
   establish noncommit. In particular, the v1593 single-UPDATE gateway does
   NOT atomically persist a gesso_commands record with its domain write:
   those writes cannot be reported as durable command incorporation by this
   observer. Atomic command-record creation, node authenticity, full writer
   coverage, transaction causality and recovery liveness remain premises.

   The returned report is deliberately redacted: it contains no record,
   command ID, principal, arguments, stored outcome, snapshot token or SQL.
   It must not be exposed to an untrusted client: :unresolved could otherwise
   function as a command-record existence oracle."
  (:require [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-unknown-command-reconciliation/report)
(def report-version 1)

(def ^:private original-require-binding resolution/require-binding)
(def ^:private original-record-id resolution/record-id)
(def ^:private original-reconcile resolution/reconcile)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)

(defn- reject! [reason]
  (throw (ex-info "Invalid XTDB unknown-command reconciliation."
                  {:xtdb-gateway-unknown-command-reconciliation/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- seams! []
  ;; This is ordinary in-process Var substitution detection, not an
  ;; authenticated library build or protection against hostile instrumentation.
  (when-not (and (identical? original-require-binding
                             (var-get #'resolution/require-binding))
                 (identical? original-record-id
                             (var-get #'resolution/record-id))
                 (identical? original-reconcile
                             (var-get #'resolution/reconcile))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode
                             (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-command-reconciliation-seam)))

(defn- pinned-snapshot! [node]
  (let [result (try (original-query node "SHOW SNAPSHOT_TOKEN")
                    (catch Throwable _ (reject! :snapshot-unavailable)))
        token (when (and (sequential? result) (= 1 (count result))
                         (plain-map? (first result))
                         (= #{:snapshot-token} (set (keys (first result)))))
                (:snapshot-token (first result)))]
    (when-not (and (string? token) (seq token)
                   (try (map? (original-decode token))
                        (catch Throwable _ false)))
      (reject! :invalid-snapshot-token))
    {:snapshot-token token :current-time (java.time.Instant/now)}))

(defn- read-command-record! [node row-id snapshot]
  ;; Fixed canonical table and fixed parameterized predicate; the command ID
  ;; is NEVER concatenated into SQL and no candidate domain state is read.
  (let [rows (try (original-query node
                                  ["SELECT * FROM gesso_commands WHERE _id = ? LIMIT 2"
                                   row-id]
                                  snapshot)
                  (catch Throwable _ (reject! :command-record-read-unavailable)))]
    (when-not (and (sequential? rows)
                   (<= (count rows) 1)
                   (every? plain-map? rows))
      (reject! :invalid-or-ambiguous-command-record-read))
    (first rows)))

(defn acquire-unknown-command-reconciliation!
  "Observe a canonical command record after an unknown outcome.

   [node binding may-disclose?] requires a caller-supplied XTDB node, an
   exact typed gesso.model.command-resolution binding, and a TRUSTED
   application disclosure predicate (binding, stored-record) -> boolean.
   Acquires one pinned read of :gesso_commands, then invokes the canonical
   pure reconciliation helper. A compatible record is reported WITHOUT its
   outcome: a separate authorized application layer must handle actual
   disclosure and any replay behavior. No domain document is inspected.

   A :replay classification means only that a compatible record was visible
   at this external snapshot and disclosure was permitted. It neither
   attributes the record to the previous failed attempt nor establishes
   that v1593's single-UPDATE gateway atomically incorporated the command.
   :unresolved is never permission to retry. Returns NO write capability."
  [node binding may-disclose?]
  (when (or (nil? node) (not (fn? may-disclose?)))
    (reject! :missing-node-or-disclosure-predicate))
  (seams!)
  (let [row-id (try (original-record-id (original-require-binding binding))
                    (catch Throwable _ (reject! :invalid-command-binding)))
        snapshot (pinned-snapshot! node)
        record (read-command-record! node row-id snapshot)
        result (try (original-reconcile binding record may-disclose?)
                    (catch Throwable _ (reject! :invalid-record-or-disclosure-result)))
        status (:status result)]
    (when-not (and (= resolution/assurance-scope (:scope result))
                   (#{:replay :unresolved :unavailable :incompatible} status))
      (reject! :unexpected-reconciliation-result))
    (seams!)
    {:type report-type
     :version report-version
     :status (case status
               :replay :compatible-durable-record-observed-attempt-unattributed
               :unresolved :no-command-record-visible-noncommit-unproved
               :unavailable :record-disclosure-unavailable
               :incompatible :record-binding-incompatible)
     :canonical-resolution-status status
     :command-record-read :same-node-pinned-external-read-on-caller-node
     :compatible-durable-record-observed? (= :replay status)
     :prior-attempt-commit-attributed? false
     :no-record-proves-noncommit? false
     :domain-document-match-proves-command-incorporation? false
     :unified-single-update-record-atomicity :not-established
     :command-binding-source-authenticity :not-established
     :submitted-payload-attribution :not-established
     :command-record-immutability :not-established
     :snapshot-and-node-authenticity :not-established
     :external-writer-closure :not-established
     :transaction-causality :not-established
     :retry-authorized? false
     :automatic-retry :forbidden-by-this-observation
     :live-settlement-authority :none
     :transaction-authority :none
     :proof-status :bounded-read-only-command-record-observation
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))
