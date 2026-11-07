(ns gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence
  "v1541: Opt-in, ONE-document source-bound execution/poststate diagnostic.

   Compose v1539's same-node pinned external-before acquisition, v1537's
   complete top-level transaction-time ASSERTs, the v1525+ restricted one-shot
   gateway with its re-admitted whole-vector footprint and exact local return,
   and v1521's later pinned XTDB poststate/ledger read. Compare the entire
   observed protected document with the declared post-image, not merely the
   typed subset mapped by the physical projection.

   This does NOT prove physical causality. An external writer can intervene
   after submission and before the later read, and a later whole-document
   replacement can restore matching bytes. The acquired prestate is EXTERNAL
   to the transaction, and XTDB ASSERT semantics are a trusted backend premise.
   Nor does this authenticate a principal, the caller-supplied node, complete
   writer coverage, application installation, or a universal refinement.
   Source, target IDs, rows, SQL, snapshot tokens and transaction keys remain
   inside closures. Diagnostics are redacted, not certificates or authority."
  (:require [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution :as acquired]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate])
  (:import [xtdb.api TransactionKey]))

(def correspondence-type
  :gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence/bounded-observation)
(def correspondence-version 1)

(def ^:private original-project projection/describe-physical-domain-projection!)
(def ^:private original-prepare acquired/prepare-acquired-complete-guard-one-shot!)
(def ^:private original-return gateway/require-local-return!)
(def ^:private original-attempt gateway/require-local-attempt!)
(def ^:private original-poststate poststate/acquire-poststate-basis!)
(def ^:private original-poststate-check poststate/require-live-poststate-basis!)
(def ^:private original-poststate-summary poststate/redacted-summary)

(defn- reject! [reason]
  (throw (ex-info "Unsupported guarded XTDB poststate correspondence."
                  {:xtdb-gateway-acquired-poststate-correspondence/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- seams! []
  ;; Ordinary Var-replacement detection, NOT an authenticated JVM boundary.
  (when-not (and (identical? original-project
                             (var-get #'projection/describe-physical-domain-projection!))
                 (identical? original-prepare
                             (var-get #'acquired/prepare-acquired-complete-guard-one-shot!))
                 (identical? original-return
                             (var-get #'gateway/require-local-return!))
                 (identical? original-attempt
                             (var-get #'gateway/require-local-attempt!))
                 (identical? original-poststate
                             (var-get #'poststate/acquire-poststate-basis!))
                 (identical? original-poststate-check
                             (var-get #'poststate/require-live-poststate-basis!))
                 (identical? original-poststate-summary
                             (var-get #'poststate/redacted-summary)))
    (reject! :substituted-correspondence-seam)))

(defn- source! [projection-source]
  (when-not (and (plain-map? projection-source)
                 (= #{:obligation-source :protected-domain-source
                      :field-locations}
                    (set (keys projection-source))))
    (reject! :invalid-projection-source))
  (let [p (try (original-project projection-source)
               (catch Exception _ (reject! :unadmitted-physical-projection)))
        original (:protected-domain-source projection-source)
        commands (get-in original [:plan :commands])
        c (when (and (vector? commands) (= 1 (count commands)))
            (first commands))
        protected (:protected-documents original)
        only (when (and (vector? protected) (= 1 (count protected)))
               (first protected))
        expected-after (:model/after c)
        expected-before (:model/before c)]
    (when-not (and (= projection/projection-type (:type p))
                   (= projection/projection-version (:version p))
                   (= :declarations-only (:proof-status p))
                   (= :not-issued (:certificate p))
                   (= :none (:authority p))
                   (plain-map? expected-before)
                   (plain-map? expected-after)
                   (= :all (:fields only))
                   (= [(:model/entity-type c) (:model/id c)] (:target only))
                   (= (:xt/id expected-before) (:xt/id expected-after)
                      (:model/id c)))
      (reject! :unsupported-one-document-projection))
    {:gateway-source original
     :target (:target only)
     :expected-after expected-after}))

(defn- checked-return! [gate response]
  (let [returned (original-return gate response)
        attempt (original-attempt gate)]
    (when-not (and (= :public-transaction-key (:observed-result-kind returned))
                   (instance? TransactionKey (:tx-key response))
                   (= :returned-transaction-key (:status attempt))
                   (= 1 (:owned-execute-tx-call-entries attempt))
                   (= 3 (:candidate-operation-count returned))
                   (= 2 (:prewrite-assertion-count returned))
                   (= 1 (:whole-document-replacement-count returned))
                   (= (:process-local-attempt-id returned)
                      (:process-local-attempt-id attempt))
                   (= (:process-local-source-binding returned)
                      (:process-local-source-binding attempt)))
      (reject! :incoherent-complete-guard-submission))
    returned))

(defn prepare-acquired-poststate-one-shot!
  "Prepare a source-bound, one-document guarded gateway and read-only observer.

   Input: (node, original v1491 physical-domain projection source). The
   projection's :protected-domain-source must be the ORIGINAL one-update
   v1539 gateway plan with :all protection; its typed obligation and field
   mapping are independently checked by v1491. The v1539 gateway acquires
   and matches the whole external pre-image before returning :submit!.

   Return exactly {:type :version :submit! :observe! :summary!}. After an
   EXACT gateway return, :observe! compares the declared entire post-image
   against a later read whose XTDB ledger shares the same pinned basis.
   Multiple read-only observations are allowed and can disagree. A divergent
   observation is never itself evidence of a failed or retried submission.

   Failure to read the later state is returned as an unavailable diagnostic;
   failures in source validation or identity checking are never downgraded.
   No assertion of transaction-internal prestate or physical causality follows."
  [node projection-source]
  (when (nil? node)
    (reject! :missing-xtdb-node))
  (seams!)
  (let [{:keys [gateway-source target expected-after]} (source! projection-source)
        prepared (original-prepare node gateway-source)
        gate (:gateway prepared)
        before-summary ((:summary! prepared))
        _ (when-not (and (= acquired/acquired-type (:type prepared))
                         (= acquired/acquired-version (:version prepared))
                         (true? (:source-before-matched-external-read?
                                 before-summary))
                         (= :prepared (:status before-summary))
                         (fn? (:submit! prepared))
                         (fn? (:summary! prepared)))
            (reject! :unusable-acquired-before-gateway))
        summary! (fn []
                   (merge
                    {:type correspondence-type
                     :version correspondence-version
                     :scope :one-acquired-complete-guard-and-later-whole-document
                     :external-before :pinned-same-node-source-matched
                     :transaction-time-preconditions :complete-top-level-cas
                     :admitted-write-footprint :one-whole-document-replacement
                     :transaction-internal-prestate :not-authenticated
                     :backend-assert-semantics :trusted-bounded-premise
                     :aba-history-detection :not-established
                     :physical-causality :not-established
                     :complete-deployment-writer-coverage :not-established
                     :caller-node-authenticity :not-established
                     :principal-authorization :not-established
                     :universal-physical-refinement :not-established
                     :proof-status :source-bound-conditional-observation-only
                     :certificate :not-issued
                     :admission :not-admitted
                     :authority :none}
                    (select-keys ((:summary! prepared))
                                 [:status :owned-execute-tx-call-entries
                                  :compared-top-level-field-count
                                  :added-snapshot-comparison-count])))
        observe! (fn [response]
                   (seams!)
                   (let [returned (checked-return! gate response)
                         post (try (original-poststate node projection-source
                                                       (:tx-key response))
                                   (catch Exception _ nil))
                         ;; If a read fails, do not invent a physical row,
                         ;; ledger record, transaction result or frame proof.
                         checked (when post
                                   (original-poststate-check
                                    (:report post) (:handle post)))
                         redacted (when post
                                    (original-poststate-summary
                                     (:report post) (:handle post)))
                         physical (when checked
                                    (:physical-acquisition checked))
                         rows (when physical (:observed-rows physical))
                         row (when (and (vector? rows) (= 1 (count rows)))
                               (first rows))
                         joined? (and (= :bounded-poststate-and-committed-ledger-agreement
                                         (:status redacted))
                                      (true? (:poststate-read-basis-joined-to-ledger?
                                              redacted))
                                      (= :snapshot-not-earlier-than-transaction
                                         (:basis-ordering redacted)))
                         complete? (and row
                                        (= target (:target row))
                                        (= :whole-document (:protection-mode row))
                                        (= :present (:observed-document-status row))
                                        (plain-map? (:document row)))
                         equal? (boolean (and complete?
                                              (= expected-after (:document row))))
                         status (cond
                                  (nil? post) :poststate-or-ledger-unavailable
                                  (not complete?) :protected-poststate-not-observable
                                  (not joined?) :ledger-aligned-poststate-not-established
                                  equal? :bounded-later-poststate-matches-after
                                  :else :bounded-later-poststate-diverges)]
                     {:type correspondence-type
                      :version correspondence-version
                      :status status
                      :process-local-attempt-id
                      (:process-local-attempt-id returned)
                      :process-local-source-binding
                      (:process-local-source-binding returned)
                      :owned-execute-tx-call-entries 1
                      :return-linkage :exact-gateway-owned-transaction-key-object
                      :prestate-origin :source-matched-pinned-external-read
                      :transaction-time-preconditions :complete-top-level-cas
                      :submitted-footprint :one-rechecked-whole-document-replacement
                      :prewrite-assertion-count 2
                      :observed-ledger-joined-poststate? (boolean joined?)
                      :observed-complete-protected-document? (boolean complete?)
                      :observed-whole-document-equals-source-after? equal?
                      :poststate-basis-ordering
                      (or (:basis-ordering redacted) :not-established)
                      :physical-poststate-origin :later-pinned-external-read
                      :transaction-internal-prestate :not-authenticated
                      :backend-assert-semantics :trusted-bounded-premise
                      :aba-history-detection :not-established
                      :actual-transaction-caused-poststate :not-established
                      :physical-frame-preservation :not-established
                      :outside-gateway-writer-closure :not-established
                      :complete-deployment-writer-coverage :not-established
                      :caller-node-authenticity :not-established
                      :principal-authorization :not-established
                      :universal-physical-refinement :not-established
                      :proof-status :source-bound-conditional-observation-only
                      :certificate :not-issued
                      :admission :not-admitted
                      :authority :none}))]
    {:type correspondence-type
     :version correspondence-version
     :submit! (:submit! prepared)
     :observe! observe!
     :summary! summary!}))
