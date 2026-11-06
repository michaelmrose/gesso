(ns gesso.assurance.proof.xtdb-poststate-basis-acquisition
  "v1521: Acquire the ORIGINAL v1492 declared physical domain on one pinned,
   external XTDB read basis, then inspect xt.txs at that SAME basis for a
   caller-supplied public TransactionKey. Only a matching committed ledger
   row and a comparable basis not earlier than the key's system time can
   produce bounded poststate/ledger agreement.

   No transaction is executed, replayed, or retried. The key, node, and
   physical origin are caller supplied; a foreign writer's real key may
   yield the same bounded agreement. A token/ledger join is NOT proof of
   transaction causality, complete writer coverage, internal prestate,
   execution of an issued plan, physical frame preservation or refinement.
   This is a diagnostic restricted to the pinned XTDB 2.2.0-beta1 surface.

   Reports contain private source, target documents, keys and tokens. Only
   redacted-summary is suitable for logs. The opaque handle binds this
   report in this JVM process; it does not attest deployment or the node."
  (:require [gesso.assurance.proof.physical-domain-acquisition :as acquisition]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as capability]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant ZonedDateTime]
           [java.util WeakHashMap]
           [xtdb.api TransactionKey]))

(def report-type
  :gesso.assurance.proof.xtdb-poststate-basis-acquisition/ledger-aligned)
(def report-version 1)

(deftype PoststateHandle []
  Object
  (toString [_] "#<gesso.assurance/poststate-basis-observation>"))

(def ^:private ^WeakHashMap live-handles (WeakHashMap.))
(def ^:private original-acquire acquisition/acquire-physical-domain!)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)
(def ^:private original-capability capability/capability-report!)

(defn- reject! [reason]
  ;; Do not reveal documents, transaction keys, tokens or exception text.
  (throw (ex-info "Unsupported XTDB poststate basis acquisition."
                  {:xtdb-poststate-basis-acquisition/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- check-seams! []
  (when-not (and (identical? original-acquire
                             (var-get #'acquisition/acquire-physical-domain!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode
                             (var-get #'basis/<-time-basis-str))
                 (identical? original-capability
                             (var-get #'capability/capability-report!)))
    (reject! :substituted-acquisition-or-xtdb-interface))
  (when-not (= :pinned-api-and-gesso-option-surface
               (:compatibility (original-capability)))
    (reject! :unsupported-xtdb-api-version)))

(defn- checked-key! [key]
  (when-not (and (instance? TransactionKey key)
                 (integer? (.getTxId ^TransactionKey key))
                 (<= 0 (.getTxId ^TransactionKey key))
                 (instance? Instant (.getSystemTime ^TransactionKey key)))
    (reject! :no-complete-public-transaction-key))
  key)

(defn- token-point! [token]
  (let [decoded (try (original-decode token)
                     (catch Throwable _ (reject! :invalid-observation-basis)))]
    (when-not (map? decoded)
      (reject! :invalid-observation-basis))
    ;; Multi-database tokens, empty tokens, and tokens with multiple points
    ;; are still useful read bases, but are NOT sufficient for an ordering
    ;; claim against a single public transaction's system time.
    (when (= 1 (count decoded))
      (let [times (val (first decoded))]
        (when (and (= 1 (count times))
                   (instance? Instant (first times)))
          (first times))))))

(defn- matching-ledger! [node basis ^TransactionKey key]
  ;; Reuse the very same snapshot token and valid-time clock as the domain
  ;; acquisition. A second SHOW SNAPSHOT_TOKEN would destroy that identity.
  (let [rows (try (original-query node
                                  ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                                   (.getTxId key)]
                                  basis)
                  (catch Throwable _ (reject! :ledger-read-unavailable)))
        row (when (and (sequential? rows) (= 1 (count rows))
                       (plain-map? (first rows)))
              (first rows))
        checks {:one-ledger-row (boolean row)
                :matching-transaction-id
                (boolean (and row (= (.getTxId key) (:xt/id row))))
                :committed (boolean (and row (true? (:committed row))))
                :matching-transaction-time
                (boolean (and row (instance? ZonedDateTime (:system-time row))
                              (= (.getSystemTime key)
                                 (.toInstant ^ZonedDateTime (:system-time row)))))}]
    (when-not (every? true? (vals checks))
      (reject! :no-matching-committed-ledger-row))
    checks))

(defn acquire-poststate-basis!
  "Read the declared v1491 domain and XTDB ledger on ONE fresh, pinned
   external read basis, AFTER receiving a public transaction key.

   Only reads occur. An observed foreign transaction can pass this join:
   its causality and the caller-supplied node remain unverified. Returns a
   report with sensitive acquisition data and a process-local opaque handle.
   Never publish the report; use redacted-summary with the handle instead."
  [node projection-source transaction-key]
  (when-not (some? node)
    (reject! :missing-observation-node))
  (check-seams!)
  (let [^TransactionKey key (checked-key! transaction-key)
        acquisition (original-acquire node projection-source)
        read-basis (:external-read-basis acquisition)
        _ (when-not (and (= acquisition/acquisition-type (:type acquisition))
                         (= acquisition/acquisition-version (:version acquisition))
                         (= :external-read-diagnostic-only
                            (:proof-status acquisition))
                         (plain-map? read-basis)
                         (= #{:snapshot-token :current-time}
                            (set (keys read-basis)))
                         (string? (:snapshot-token read-basis))
                         (instance? Instant (:current-time read-basis)))
            (reject! :unexpected-physical-acquisition-shape))
        token-time (token-point! (:snapshot-token read-basis))
        ledger-checks (matching-ledger! node read-basis key)
        ordering (cond
                   (nil? token-time) :snapshot-system-time-not-comparable
                   (neg? (compare token-time (.getSystemTime key)))
                   :snapshot-earlier-than-transaction
                   :else :snapshot-not-earlier-than-transaction)
        positive? (= :snapshot-not-earlier-than-transaction ordering)
        report {:type report-type
                :version report-version
                :checking :one-source-derived-snapshot-shared-with-transaction-ledger
                :scope :only-declared-physical-targets-and-public-tx-ledger
                :projection-source projection-source
                :transaction-key key
                :node node
                :physical-acquisition acquisition
                :ledger-checks ledger-checks
                :snapshot-system-time token-time
                :basis-ordering ordering
                :status (if positive?
                          :bounded-poststate-and-committed-ledger-agreement
                          :poststate-order-not-established)
                :poststate-read-basis-joined-to-ledger? true
                :poststate-at-or-after-public-transaction-time? positive?
                :node-and-key-origin :caller-supplied-unattested
                :physical-poststate-transaction-causality :not-established
                :transaction-internal-prestate :not-authenticated
                :executed-plan-binding :not-established
                :complete-writer-coverage :not-established
                :physical-frame-preservation :not-established
                :universal-physical-refinement :not-established
                :proof-status :bounded-external-ledger-diagnostic-only
                :certificate :not-issued
                :admission :not-admitted
                :transaction-authority :none
                :retry-authority :none
                :settlement-authority :none
                :authority :none}
        handle (PoststateHandle.)]
    (locking live-handles
      (.put live-handles handle report))
    {:report report :handle handle}))

(defn require-live-poststate-basis!
  "Check an issued process-local handle and exact report identity without
   reading XTDB again. This is NOT a physical readback authentication." 
  [report handle]
  (when-not (instance? PoststateHandle handle)
    (reject! :invalid-process-local-handle))
  (let [registered (locking live-handles (.get live-handles handle))]
    (when-not (and registered (identical? registered report)
                   (= report-type (:type report))
                   (= report-version (:version report))
                   (= :bounded-external-ledger-diagnostic-only
                      (:proof-status report))
                   (= :not-issued (:certificate report))
                   (= :none (:authority report)))
      (reject! :stale-or-substituted-poststate-report))
    report))

(defn redacted-summary
  "Safe diagnostic fields only. In particular NO source, document values,
   node, snapshot token, public transaction key or physical row is returned."
  [report handle]
  (select-keys (require-live-poststate-basis! report handle)
               [:type :version :checking :scope :ledger-checks :basis-ordering
                :status :poststate-read-basis-joined-to-ledger?
                :poststate-at-or-after-public-transaction-time?
                :node-and-key-origin :physical-poststate-transaction-causality
                :transaction-internal-prestate :executed-plan-binding
                :complete-writer-coverage :physical-frame-preservation
                :universal-physical-refinement :proof-status :certificate
                :admission :transaction-authority :retry-authority
                :settlement-authority :authority]))
