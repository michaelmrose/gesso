(ns gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence
  "v1543: Opt-in, ONE-document source-bound execution/poststate diagnostic.

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
   v1543 adds an explicitly CONDITIONAL transaction-local write-effect
   inference. If the source-owned transaction returned its exact public key,
   the later pinned ledger confirms that key was committed, and we TRUST
   XTDB's atomic transaction and :put-docs semantics, then that transaction's
   admitted single whole-document replacement wrote its declared post-image
   AT EXECUTION. If the physical poststate is unavailable, the separate
   exact-return-bound ledger observer may corroborate the transaction without
   inventing a physical poststate. This does not imply anything about a LATER
   read, nor certify backend semantics, writer closure, transaction history
   or causality of an observed poststate. Missing ledger evidence NEVER
   yields this inference.

   v1545 fails closed unless the currently loaded XTDB API artifact is the
   exact beta1 interface under which the bounded ASSERT, atomic PUT, and
   ledger probes were run, and Gesso's option filters still match their
   reviewed contract. This checks an API VERSION/OPTION premise, NOT the
   correctness of XTDB internals, the deployed node or its writer history.
   Preparation, each submission attempt and each observation recheck that
   premise; a failed pre-submit check consumes this wrapper's one-shot permit
   without invoking XTDB. Reports do not turn compatibility into a proof.

   Source, target IDs, rows, SQL, snapshot tokens and transaction keys remain
   inside closures. Diagnostics are redacted, not certificates or authority."
  (:require [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as backend]
            [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution :as acquired]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate])
  (:import [xtdb.api TransactionKey]))

(def correspondence-type
  :gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence/bounded-observation)
(def correspondence-version 1)

(def ^:private original-backend-report backend/capability-report!)
(def ^:private original-project projection/describe-physical-domain-projection!)
(def ^:private original-prepare acquired/prepare-acquired-complete-guard-one-shot!)
(def ^:private original-return gateway/require-local-return!)
(def ^:private original-attempt gateway/require-local-attempt!)
(def ^:private original-ledger gateway/require-local-ledger-observation!)
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
  (when-not (and (identical? original-backend-report
                             (var-get #'backend/capability-report!))
                 (identical? original-project
                             (var-get #'projection/describe-physical-domain-projection!))
                 (identical? original-prepare
                             (var-get #'acquired/prepare-acquired-complete-guard-one-shot!))
                 (identical? original-return
                             (var-get #'gateway/require-local-return!))
                 (identical? original-attempt
                             (var-get #'gateway/require-local-attempt!))
                 (identical? original-ledger
                             (var-get #'gateway/require-local-ledger-observation!))
                 (identical? original-poststate
                             (var-get #'poststate/acquire-poststate-basis!))
                 (identical? original-poststate-check
                             (var-get #'poststate/require-live-poststate-basis!))
                 (identical? original-poststate-summary
                             (var-get #'poststate/redacted-summary)))
    (reject! :substituted-correspondence-seam)))

(def ^:private expected-backend-checks
  #{:pinned-runtime-api-version
    :gesso-query-basis-options-present
    :gesso-write-options-present
    :gesso-does-not-forward-query-basis-as-write-options})

(defn- require-reviewed-backend! []
  ;; This is an exact loaded-API/option-surface compatibility check, NOT an
  ;; internal isolation proof. Never promote historical beta1 probes to a
  ;; positive physical-effect conclusion on an unrecognized backend.
  (seams!)
  (let [r (try (original-backend-report)
               (catch Throwable _ (reject! :backend-capability-unavailable)))
        checks (:checks r)]
    (when-not (and (= backend/report-type (:type r))
                   (= backend/report-version (:version r))
                   (= :pinned-api-and-gesso-option-surface (:compatibility r))
                   (= backend/pinned-backend-version
                      (get-in r [:backend :expected-version]))
                   (= backend/pinned-backend-version
                      (get-in r [:backend :loaded-api-version]))
                   (= expected-backend-checks (set (keys checks)))
                   (every? true? (vals checks))
                   (false? (:prior-experiments-reexecuted? r))
                   (= :not-established (:proof-status r))
                   (= :not-issued (:certificate r))
                   (= :none (:authority r)))
      (reject! :unreviewed-backend-api-or-option-surface))
    true))

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

   Failure to read the later physical state is reported as unavailable. An
   independently pinned, gateway-bound ledger observation can still support
   a CONDITIONAL statement about the transaction's own single PUT under
   trusted XTDB semantics, but never about the later physical document.
   Failures in source validation or identity checking are never downgraded.
   No assertion of transaction-internal prestate or observed-state causality
   follows; this is not an independently checked backend execution proof."
  [node projection-source]
  (when (nil? node)
    (reject! :missing-xtdb-node))
  (require-reviewed-backend!)
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
        ;; This wrapper is itself one-shot even if the runtime premise fails
        ;; before the underlying restricted gateway has entered execute-tx.
        permit (atom :prepared)
        submit! (fn []
                  (when-not (compare-and-set! permit :prepared :checking-backend)
                    (reject! :one-shot-already-attempted))
                  (try
                    (require-reviewed-backend!)
                    (catch Throwable failure
                      (reset! permit :backend-preflight-rejected)
                      (throw failure)))
                  ;; Delegation is also one-shot. A subsequent XTDB exception
                  ;; remains the underlying gateway's unknown outcome.
                  (reset! permit :delegated)
                  ((:submit! prepared)))
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
                     :backend-api-check-at-preparation :pinned-2_2_0-beta1-interface-only
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
                    (let [inner ((:summary! prepared))]
                      (assoc (select-keys inner
                                          [:status :owned-execute-tx-call-entries
                                           :compared-top-level-field-count
                                           :added-snapshot-comparison-count])
                             :status (case @permit
                                       :checking-backend :checking-backend
                                       :backend-preflight-rejected
                                       :backend-preflight-rejected
                                       (:status inner))))))
        observe! (fn [response]
                   (require-reviewed-backend!)
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
                         ;; A poststate read can be unavailable (e.g. a later
                         ;; DELETE). A separate exact-return-bound ledger
                         ;; observation can still corroborate that transaction
                         ;; without yielding ANY poststate evidence. Only make
                         ;; that second read when the shared-basis join failed.
                         ledger (when-not joined?
                                  (try (original-ledger gate response)
                                       (catch Exception _ nil)))
                         ledger-only?
                         (boolean (and ledger
                                       (= (:process-local-attempt-id returned)
                                          (:process-local-attempt-id ledger))
                                       (= (:process-local-source-binding returned)
                                          (:process-local-source-binding ledger))
                                       (= 1 (:owned-execute-tx-call-entries ledger))
                                       (= :reported-key-matches-committed-ledger-row
                                          (:ledger-status ledger))
                                       (= {:tx-id true :committed true :system-time true}
                                          (:ledger-checks ledger))))
                         ledger-corroborated? (boolean (or joined? ledger-only?))
                         ;; A narrowly derived statement about the admitted
                         ;; transaction's OWN write, not the origin of a later
                         ;; physical snapshot. These premises are independent
                         ;; of whether the later document still matches.
                         conditional-effect?
                         (boolean (and ledger-corroborated?
                                       (= 1 (:owned-execute-tx-call-entries returned))
                                       (= 3 (:candidate-operation-count returned))
                                       (= 2 (:prewrite-assertion-count returned))
                                       (= 1 (:whole-document-replacement-count returned))))
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
                      :backend-api-check-at-observation :pinned-2_2_0-beta1-interface-only
                      :submitted-footprint :one-rechecked-whole-document-replacement
                      :prewrite-assertion-count 2
                      :observed-ledger-joined-poststate? (boolean joined?)
                      :observed-complete-protected-document? (boolean complete?)
                      :observed-whole-document-equals-source-after? equal?
                      ;; This inference is conditional on the actual XTDB
                      ;; backend semantics, not an independently verified
                      ;; effect or an assertion about current database state.
                      :transaction-local-write-effect
                      (if conditional-effect?
                        :declared-postimage-under-trusted-xtdb-semantics
                        :not-established)
                      :transaction-local-write-effect-premises
                      {:source-vector :rederived-and-one-shot-owned
                       :whole-document-put :exactly-one-target
                       :transaction-ledger
                       (cond joined? :same-basis-matching-committed-key
                             ledger-only? :separately-pinned-matching-committed-key
                             :else :not-established)
                       :backend-atomic-put-semantics :trusted-not-proved}
                      :transaction-local-effect-time-scope :execution-only
                      :transaction-local-effect-physical-verification
                      :not-independently-established
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
     :submit! submit!
     :observe! observe!
     :summary! summary!}))
