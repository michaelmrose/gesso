(ns gesso.assurance.proof.xtdb-gateway-atomic-readback
  "v1608: Bounded, read-only post-submission co-observation for the v1606
   atomic domain UPDATE + canonical command-record CREATE path.

   An independent, single pinned XTDB read basis is used for the expected
   domain document, the exact canonical persisted command record, and the
   public transaction-key ledger row. The expected documents are DERIVED
   from the caller-supplied source and incorporation input; no arbitrary
   expected record, SQL, table, snapshot, or ledger observation is accepted.

   This is *not* a causal proof. A real transaction key from an unrelated
   writer can match the ledger while an independently written command record
   and domain document match the expected values. The caller-supplied node,
   source, command input and key are not authenticated. An observation of a
   missing record does not prove noncommit, and an observation of a matching
   one does not authorize retry, disclose its outcome, or settle Live.

   The fixed redacted report is an INTERNAL diagnostic: record-presence
   distinctions must not be exposed directly to untrusted callers. No
   command, document, transaction key, basis token or value is returned."
  (:require [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import (java.time Instant ZonedDateTime)
           (xtdb.api TransactionKey)))

(def report-type :gesso.assurance.proof.xtdb-gateway-atomic-readback/observation)
(def report-version 1)

(def ^:private original-update? command/update?)
(def ^:private original-require-binding resolution/require-binding)
(def ^:private original-command-record resolution/command-record)
(def ^:private original-require-record resolution/require-record)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB atomic command readback."
                  {:xtdb-gateway-atomic-readback/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- seams! []
  ;; Ordinary Var-substitution defense, NOT runtime/process authenticity.
  (when-not (and (identical? original-update? (var-get #'command/update?))
                 (identical? original-require-binding
                             (var-get #'resolution/require-binding))
                 (identical? original-command-record
                             (var-get #'resolution/command-record))
                 (identical? original-require-record
                             (var-get #'resolution/require-record))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode
                             (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-observation-seam)))

(defn- expected-documents! [source incorporation]
  (when-not (and (plain-map? source)
                 (= #{:plan :protected-documents} (set (keys source)))
                 (plain-map? (:plan source))
                 (= #{:commands :emit} (set (keys (:plan source))))
                 (= false (get-in source [:plan :emit]))
                 (plain-vector? (get-in source [:plan :commands]))
                 (= 1 (count (get-in source [:plan :commands])))
                 (plain-vector? (:protected-documents source))
                 (= 1 (count (:protected-documents source)))
                 (plain-map? incorporation)
                 (= #{:binding :outcome :now} (set (keys incorporation))))
    (reject! :unsupported-domain-or-command-source))
  (let [c (first (get-in source [:plan :commands]))
        before (:model/before c)
        after (:model/after c)
        table (:model/entity-type c)
        id (:model/id c)
        protected (first (:protected-documents source))
        binding (:binding incorporation)
        expected-record (try
                          (original-require-binding binding)
                          (original-command-record binding
                                                   (:outcome incorporation)
                                                   (:now incorporation))
                          (catch Throwable _
                            (reject! :invalid-canonical-command-record)))]
    (when-not (and (plain-map? c) (original-update? c)
                   (plain-map? before) (plain-map? after)
                   (not= table resolution/command-entity-type)
                   (keyword? table) (nil? (namespace table))
                   (re-matches #"[a-z][a-z0-9_]*" (name table))
                   (or (string? id) (uuid? id))
                   (not (and (string? id) (empty? id)))
                   (= (:xt/id before) id (:xt/id after))
                   (= (set (keys before)) (set (keys after)))
                   (plain-map? protected)
                   (= #{:target :fields} (set (keys protected)))
                   (= [table id] (:target protected))
                   (= :all (:fields protected))
                   (plain-map? expected-record)
                   (not= id (:xt/id expected-record)))
      (reject! :unsupported-domain-or-command-source))
    {:domain-table (name table)
     :domain-id id
     :domain-after after
     :command-id (:xt/id expected-record)
     :command-record expected-record}))

(defn- transaction-key! [key]
  (when-not (and (instance? TransactionKey key)
                 (integer? (.getTxId ^TransactionKey key))
                 (<= 0 (.getTxId ^TransactionKey key))
                 (instance? Instant (.getSystemTime ^TransactionKey key)))
    (reject! :invalid-public-transaction-key))
  key)

(defn- snapshot! [node]
  (let [rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                  (catch Throwable _ (reject! :snapshot-unavailable)))
        token (when (and (sequential? rows) (= 1 (count rows))
                         (plain-map? (first rows))
                         (= #{:snapshot-token} (set (keys (first rows)))))
                (:snapshot-token (first rows)))
        decoded (try (when (and (string? token) (seq token))
                       (original-decode token))
                     (catch Throwable _ nil))]
    (when-not (map? decoded)
      (reject! :invalid-snapshot-token))
    {:basis {:snapshot-token token :current-time (Instant/now)}
     :system-time (when (= 1 (count decoded))
                    (let [times (val (first decoded))]
                      (when (and (= 1 (count times))
                                 (instance? Instant (first times)))
                        (first times))))}))

(defn- read-one! [node sql param read-basis]
  (let [rows (try (original-query node [sql param] read-basis)
                  (catch Throwable _ (reject! :pinned-read-unavailable)))]
    (when-not (and (sequential? rows)
                   (<= (count rows) 1)
                   (every? plain-map? rows))
      (reject! :invalid-or-ambiguous-pinned-read))
    (first rows)))

(defn- ledger-status [row ^TransactionKey key]
  (cond
    (nil? row) :not-visible
    (not= #{:xt/id :committed :system-time} (set (keys row))) :invalid
    (not= (.getTxId key) (:xt/id row)) :mismatched-id
    (not (true? (:committed row))) :not-committed
    (not (instance? ZonedDateTime (:system-time row))) :invalid
    (not= (.getSystemTime key)
          (.toInstant ^ZonedDateTime (:system-time row))) :mismatched-time
    :else :matching-committed-ledger-row))

(defn acquire-atomic-command-readback!
  "Observe the expected domain poststate, complete canonical command record,
   and committed ledger row on ONE pinned basis after a physical attempt.

   Arguments: [node original-single-update-source incorporation-input
               public-transaction-key]. The source and input are the exact
   caller-side inputs used to prepare the atomic gateway; this function
   does not establish that provenance. The public key may be from ANY writer.
   No additional write, retry, settlement or certificate is issued."
  [node domain-source incorporation-input transaction-key]
  (when (nil? node) (reject! :missing-observation-node))
  (seams!)
  (let [^TransactionKey key (transaction-key! transaction-key)
        {:keys [domain-table domain-id domain-after command-id command-record]}
        (expected-documents! domain-source incorporation-input)
        {:keys [basis system-time]} (snapshot! node)
        domain-row (read-one! node
                              (str "SELECT * FROM " domain-table " WHERE _id = ? LIMIT 2")
                              domain-id basis)
        command-row (read-one! node
                               "SELECT * FROM gesso_commands WHERE _id = ? LIMIT 2"
                               command-id basis)
        ledger-row (read-one! node
                              "SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                              (.getTxId key) basis)
        domain-status (cond (nil? domain-row) :not-visible
                            (= domain-after domain-row) :matches-source-after
                            :else :diverges)
        record-status (cond
                        (nil? command-row) :not-visible
                        (try (= command-record
                                (original-require-record command-row))
                             (catch Throwable _ false))
                        :matches-canonical-record
                        :else :diverges-or-invalid)
        ledger (ledger-status ledger-row key)
        ordering (cond
                   (nil? system-time) :basis-not-comparable
                   (neg? (compare system-time (.getSystemTime key)))
                   :basis-before-reported-transaction
                   :else :basis-not-before-reported-transaction)
        matches? (and (= :matches-source-after domain-status)
                      (= :matches-canonical-record record-status)
                      (= :matching-committed-ledger-row ledger)
                      (= :basis-not-before-reported-transaction ordering))]
    (seams!)
    {:type report-type
     :version report-version
     :status (if matches?
               :bounded-poststate-command-and-ledger-coobservation
               :postsubmission-correspondence-not-established)
     :domain-poststate-status domain-status
     :command-record-status record-status
     :ledger-status ledger
     :basis-order-status ordering
     :three-observations-share-pinned-basis? true
     :observed-record-exactly-canonical? (= :matches-canonical-record record-status)
     :observed-domain-exactly-source-after? (= :matches-source-after domain-status)
     :observed-ledger-matches-public-key? (= :matching-committed-ledger-row ledger)
     :key-attributed-to-command-creation? false
     :submitted-transaction-contents-authenticated? false
     :application-command-inputs-authenticated? false
     :command-record-immutability :not-established
     :node-and-transaction-key-origin :caller-supplied-unattested
     :transaction-causality :not-established
     :external-writer-closure :not-established
     :universal-operation-refinement :not-established
     :retry-authorized? false
     :automatic-retry :forbidden-by-this-observation
     :transaction-authority :none
     :live-settlement-authority :none
     :certificate :not-issued
     :admission :not-admitted
     :authority :none
     :proof-status :bounded-postsubmission-read-only-coobservation}))
