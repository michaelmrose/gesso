(ns gesso.assurance.proof.xtdb-gateway-frame-observation
  "v1535: Opt-in, bounded, same-node before/after frame observations for ONE
   restricted XTDB submission gateway.

   Prepare the original one-shot gateway and acquire a pinned physical-domain
   snapshot BEFORE returning its submission capability. After that exact
   capability returns a genuine public transaction key, re-read the domain and
   xt.txs on a shared poststate basis, then apply the existing v1519 source-
   derived frame checker to the two observed acquisitions.

   This is evidence about two EXTERNAL snapshots, not a transaction-internal
   prestate or a proof that the submitted transaction caused their difference.
   Other writers can intervene before submission, between submission and
   observation, or within the interval between external snapshots. A guarded
   plan, a returned TransactionKey, and a bounded frame agreement do not
   establish exclusive writer access or universal physical refinement.

   All source, node, keys, rows, and basis tokens remain closed over. The
   returned read-only diagnostic contains only counts, positional differences
   and explicit open assumptions. No retry, Live publication, certification,
   transaction authority, or principal authentication is provided."
  (:require [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.physical-domain-acquisition :as acquisition]
            [gesso.assurance.proof.xtdb-physical-frame-preservation :as frame]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]))

(def observation-type
  :gesso.assurance.proof.xtdb-gateway-frame-observation/one-shot-external-pair)
(def observation-version 1)

(def ^:private original-project projection/describe-physical-domain-projection!)
(def ^:private original-acquire acquisition/acquire-physical-domain!)
(def ^:private original-prepare gateway/prepare-one-shot!)
(def ^:private original-return gateway/require-local-return!)
(def ^:private original-attempt gateway/require-local-attempt!)
(def ^:private original-poststate poststate/acquire-poststate-basis!)
(def ^:private original-poststate-summary poststate/redacted-summary)
(def ^:private original-frame frame/compare-external-physical-frame!)
(def ^:private original-frame-summary frame/redacted-summary)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB gateway frame observation."
                  {:xtdb-gateway-frame-observation/rejection true
                   :reason reason})))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentHashMap v))
       (nil? (meta v))))

(defn- seams! []
  ;; Ordinary process-local Var-substitution detection, not an authenticated
  ;; deployment boundary and not a protection against JVM instrumentation.
  (when-not (and (identical? original-project
                             (var-get #'projection/describe-physical-domain-projection!))
                 (identical? original-acquire
                             (var-get #'acquisition/acquire-physical-domain!))
                 (identical? original-prepare
                             (var-get #'gateway/prepare-one-shot!))
                 (identical? original-return
                             (var-get #'gateway/require-local-return!))
                 (identical? original-attempt
                             (var-get #'gateway/require-local-attempt!))
                 (identical? original-poststate
                             (var-get #'poststate/acquire-poststate-basis!))
                 (identical? original-poststate-summary
                             (var-get #'poststate/redacted-summary))
                 (identical? original-frame
                             (var-get #'frame/compare-external-physical-frame!))
                 (identical? original-frame-summary
                             (var-get #'frame/redacted-summary)))
    (reject! :substituted-physical-observation-seam)))

(defn- valid-projection! [source]
  (when-not (and (plain-map? source)
                 (= #{:obligation-source :protected-domain-source
                      :field-locations}
                    (set (keys source))))
    (reject! :invalid-physical-projection-source))
  (let [p (try (original-project source)
               (catch Exception _
                 (reject! :invalid-physical-projection-source)))]
    (when-not (and (= projection/projection-type (:type p))
                   (= projection/projection-version (:version p))
                   (= :declarations-only (:proof-status p))
                   (= :not-issued (:certificate p))
                   (= :none (:authority p)))
      (reject! :unexpected-projection-contract)))
  source)

(defn prepare-observed-one-shot!
  "Prepare a one-shot source-admitted gateway AND a pinned EXTERNAL pre-read.

   Input: an XTDB node and an original v1491 physical projection source whose
   :protected-domain-source is a complete restricted gateway source.
   A readable mapped prestate is required; unlike a gateway without this
   optional observer, creating an absent mapped document is unsupported.

   Returns exactly {:type :version :submit! :observe! :summary!}. :submit!
   is the gateway's original one-use submission function. :observe! accepts
   ONLY its exact returned response and never issues another transaction.
   Multiple observations are allowed; each is a distinct, later external read
   and may differ if another writer intervenes. No observation establishes
   transaction-internal prestate or the writer responsible for its poststate."
  [node projection-source]
  (when-not (some? node)
    (reject! :missing-observation-node))
  (seams!)
  (valid-projection! projection-source)
  ;; The gateway closes over the original plan and rechecks its full write
  ;; vector at the only submission boundary. No caller-provided tx vector.
  (let [gate (original-prepare node (:protected-domain-source projection-source))
        ;; Captured before :submit! is exposed. A competing writer can still
        ;; change the database after this read and before gateway submission.
        pre (try (original-acquire node projection-source)
                 (catch Exception _
                   (reject! :external-prestate-unavailable)))
        _ (when-not (and (= acquisition/acquisition-type (:type pre))
                         (= acquisition/acquisition-version (:version pre))
                         (= projection-source (:source pre))
                         (= :external-read-diagnostic-only (:proof-status pre))
                         (= :not-issued (:certificate pre))
                         (= :none (:authority pre)))
            (reject! :unexpected-external-prestate-contract))
        summary! (fn []
                   (merge {:type observation-type
                           :version observation-version
                           :prestate-origin :external-pinned-before-capability-exposed
                           :prestate-basis-authenticity :not-established
                           :transaction-internal-prestate :not-authenticated
                           :physical-frame-preservation :not-established
                           :outside-gateway-writer-closure :not-established
                           :proof-status :bounded-external-frame-observation-only
                           :certificate :not-issued
                           :admission :not-admitted
                           :authority :none}
                          (select-keys ((:summary! gate))
                                       [:status :owned-execute-tx-call-entries
                                        :candidate-operation-count
                                        :whole-document-replacement-count])))
        observe! (fn [result]
                   (seams!)
                   (let [returned (original-return gate result)
                         attempt (original-attempt gate)]
                     (when-not (and (= :returned-transaction-key (:status attempt))
                                    (= 1 (:owned-execute-tx-call-entries attempt))
                                    (= (:process-local-attempt-id returned)
                                       (:process-local-attempt-id attempt))
                                    (= (:process-local-source-binding returned)
                                       (:process-local-source-binding attempt)))
                       (reject! :incoherent-gateway-attempt))
                     ;; A valid local return is not necessarily a committed
                     ;; transaction. Any failed ledger join yields a diagnostic
                     ;; without inventing an execution result or retrying.
                     (let [;; An unavailable XTDB read is a diagnostic, not
                           ;; a commit result. Conversely, failures in the
                           ;; frame checker or redaction MUST propagate rather
                           ;; than silently become a positive observation.
                           post (try
                                  (original-poststate
                                   node projection-source (:tx-key result))
                                  (catch Exception _ nil))
                           observed (when post
                                      (let [{:keys [report handle]} post
                                            post-summary
                                            (original-poststate-summary report handle)
                                            frame-report
                                            (original-frame
                                             {:projection-source projection-source
                                              :pre-acquisition pre
                                              :post-acquisition
                                              (:physical-acquisition report)})
                                            frame-summary
                                            (original-frame-summary frame-report)]
                                        {:poststate post-summary
                                         :frame frame-summary}))
                           post-summary (:poststate observed)
                           frame-summary (:frame observed)
                           joined? (and (= :bounded-poststate-and-committed-ledger-agreement
                                           (:status post-summary))
                                        (true? (:poststate-read-basis-joined-to-ledger?
                                                post-summary)))
                           status (cond
                                    (nil? observed) :external-comparison-unavailable
                                    (not joined?) :poststate-basis-not-established
                                    :else (:status frame-summary))]
                       {:type observation-type
                        :version observation-version
                        :status status
                        :process-local-attempt-id
                        (:process-local-attempt-id returned)
                        :process-local-source-binding
                        (:process-local-source-binding returned)
                        :owned-execute-tx-call-entries 1
                        :return-linkage :exact-object-from-gateway-owned-call-site
                        :source-relative-transaction-vector
                        :readmitted-and-one-shot-submitted
                        :declared-candidate-operation-count
                        (:candidate-operation-count returned)
                        :declared-write-count
                        (:whole-document-replacement-count returned)
                        :observed-ledger-joined-poststate? (boolean joined?)
                        :poststate-status (or (:status post-summary)
                                              :poststate-acquisition-unavailable)
                        :poststate-basis-ordering
                        (or (:basis-ordering post-summary) :not-established)
                        :declared-typed-frame-field-count
                        (or (:declared-typed-frame-field-count frame-summary) 0)
                        :declared-frame-only-document-count
                        (or (:declared-frame-only-document-count frame-summary) 0)
                        :changed-typed-frame-field-indices
                        (vec (or (:changed-typed-frame-field-indices frame-summary) []))
                        :changed-frame-only-document-indices
                        (vec (or (:changed-frame-only-document-indices frame-summary) []))
                        :finite-observed-frame-agrees?
                        (boolean (and joined?
                                      (:finite-supplied-frame-agrees? frame-summary)))
                        :prestate-origin :external-pinned-before-capability-exposed
                        :poststate-origin :external-pinned-ledger-aligned-read
                        :external-observation-order :not-authenticated
                        :transaction-internal-prestate :not-authenticated
                        :actual-transaction-caused-observed-difference :not-established
                        :physical-frame-preservation :not-established
                        :frame-outside-enumerated-domain :not-established
                        :physical-value-type-correspondence :not-established
                        :physical-poststate-causality :not-established
                        :caller-node-authenticity :not-established
                        :installed-host-adoption :not-established
                        :outside-gateway-writer-closure :not-established
                        :complete-deployment-writer-coverage :not-established
                        :principal-authorization :not-established
                        :universal-physical-refinement :not-established
                        :proof-status :bounded-external-frame-observation-only
                        :certificate :not-issued
                        :admission :not-admitted
                        :authority :none}))) ]
    {:type observation-type
     :version observation-version
     :submit! (:submit! gate)
     :observe! observe!
     :summary! summary!}))
