(ns gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution
  "v1539: Bind one opt-in complete-top-level-guard submission to a preceding,
   same-node, PINNED EXTERNAL XTDB document read, rather than accepting an
   entirely unexamined caller-supplied :model/before as the only prestate.

   Re-admit the ordinary one-document source through v1537's strict gateway,
   acquire the whole document using XTDB's pinned snapshot option, and demand
   an EXACT match with the source's :model/before. Only after that match is
   the existing one-shot submission capability exposed. The underlying
   transaction still has canonical per-field and field-count ASSERTs, which
   reject changes between acquisition and the transaction's assertion point.

   The node is caller supplied. The read is EXTERNAL to the transaction; it
   cannot establish a transaction-internal basis, an uninterrupted history,
   principal authentication, other-writer exclusion, or physical causality.
   An ABA restoration remains possible. No arbitrary SQL, update operation,
   raw transaction vector, read row, target identifier, or snapshot token is
   returned by this module. There is no certificate or admission authority."
  (:require [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as strict]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import [java.time Instant]
           [java.util UUID]))

(def acquired-type
  :gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution/pinned-before)
(def acquired-version 1)

(def ^:private original-strict strict/prepare-complete-guard-one-shot!)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)

(defn- reject! [reason]
  ;; Never echo a source, ID, SQL statement, row, exception or snapshot token.
  (throw (ex-info "Unsupported acquired XTDB complete-guard submission."
                  {:xtdb-gateway-acquired-complete-guard-execution/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- seams! []
  ;; Detect ordinary Var replacement, not hostile JVM or node instrumentation.
  (when-not (and (identical? original-strict
                             (var-get #'strict/prepare-complete-guard-one-shot!))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode
                             (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-acquisition-or-execution-seam)))

(defn- table-sql! [table]
  (when-not (and (keyword? table)
                 (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                 (or (nil? (namespace table))
                     (re-matches #"[A-Za-z_][A-Za-z0-9_]*"
                                 (namespace table))))
    (reject! :unsupported-table-identifier))
  (if-let [schema (namespace table)]
    (str schema "." (name table))
    (name table)))

(defn- snapshot! [node]
  (let [rows (try (original-query node "SHOW SNAPSHOT_TOKEN")
                  (catch Throwable _
                    (reject! :external-snapshot-unavailable)))
        token (when (and (sequential? rows) (= 1 (count rows))
                         (plain-map? (first rows))
                         (= #{:snapshot-token} (set (keys (first rows)))))
                (:snapshot-token (first rows)))]
    (when-not (and (string? token) (seq token)
                   (try (map? (original-decode token))
                        (catch Throwable _ false)))
      (reject! :invalid-external-snapshot))
    {:snapshot-token token :current-time (Instant/now)}))

(defn- read-matching-before! [node source]
  ;; v1537 already validated the source shape, command and complete protection
  ;; before this function is called. Still reject any surprising read target;
  ;; never construct SQL with an unchecked table identifier or concatenate ID.
  (let [command (first (get-in source [:plan :commands]))
        table (:model/entity-type command)
        id (:model/id command)
        expected (:model/before command)
        _ (when-not (and (plain-map? expected)
                         (or (string? id) (instance? UUID id)
                             (integer? id))
                         (= id (:xt/id expected)))
            (reject! :unsupported-external-read-target))
        sql (str "SELECT * FROM " (table-sql! table)
                 " WHERE _id = ? LIMIT 2")
        snapshot (snapshot! node)
        rows (try (original-query node [sql id] snapshot)
                  (catch Throwable _
                    (reject! :external-document-read-unavailable)))]
    (when-not (and (sequential? rows) (= 1 (count rows))
                   (plain-map? (first rows)))
      (reject! :missing-or-ambiguous-external-document))
    (when-not (= expected (first rows))
      (reject! :source-before-disagrees-with-external-read))
    true))

(defn prepare-acquired-complete-guard-one-shot!
  "Prepare a v1537 complete-guard gateway, requiring an exact, read-only,
   same-node external prestate observation before exposing :submit!.

   Input is exactly the ordinary source accepted by v1537 (one UPDATE with
   :model/before and :model/after and one :all protected-document entry).
   Return {:type :version :gateway :submit! :summary!}. The original strict
   gateway's return, attempt, ledger and poststate APIs remain available via
   :gateway. Submission uses only the original one-use capability.

   The pre-read is pinned on the caller-supplied node; the transaction asserts
   the source-derived full top-level prestate at its own execution point.
   This does NOT authenticate the node, prove those two reads use one basis,
   prevent ABA, prove commit causality or control other XTDB writers."
  [node source]
  (when (nil? node)
    (reject! :missing-xtdb-node))
  (seams!)
  ;; Validation, including the stronger whole-vector rederivation, must happen
  ;; before the physical read. No :submit! escapes if acquisition fails.
  (let [guarded (original-strict node source)
        _ (when-not (and (= strict/guarded-type (:type guarded))
                         (= strict/guarded-version (:version guarded))
                         (= :prepared (:status ((:summary! guarded))))
                         (fn? (:submit! guarded))
                         (fn? (:summary! guarded)))
            (reject! :unexpected-strict-gateway-contract))
        _ (seams!)
        _ (read-matching-before! node source)
        summary! (fn []
                   (merge
                    {:type acquired-type
                     :version acquired-version
                     :scope :one-update-acquired-external-before-and-complete-cas
                     :external-before :same-node-pinned-read-matched-at-preparation
                     :source-before-matched-external-read? true
                     :external-basis-authenticity :caller-node-unattested
                     :transaction-time-preconditions :canonical-complete-top-level-cas
                     :transaction-internal-read-basis :not-authenticated
                     :aba-history-detection :not-established
                     :backend-assert-semantics :trusted-bounded-premise
                     :physical-poststate-causality :not-established
                     :complete-deployment-writer-coverage :not-established
                     :principal-authorization :not-established
                     :universal-physical-refinement :not-established
                     :proof-status :externally-acquired-conditional-guard-only
                     :certificate :not-issued
                     :admission :not-admitted
                     :authority :none}
                    (select-keys ((:summary! guarded))
                                 [:status :owned-execute-tx-call-entries
                                  :compared-top-level-field-count
                                  :added-snapshot-comparison-count])))]
    {:type acquired-type
     :version acquired-version
     :gateway (:gateway guarded)
     :submit! (:submit! guarded)
     :summary! summary!}))
