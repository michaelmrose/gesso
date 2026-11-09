(ns gesso.assurance.proof.xtdb-gateway-complete-guard-execution
  "v1537: Opt-in top-level compare-and-set strengthening for ONE Gesso
   whole-document UPDATE before an existing restricted gateway is issued.

   The ordinary Gesso command guards only its declared version keys. That
   permits a concurrent writer to change an unguarded field which a later
   whole-document PUT silently restores. For a strictly bounded subset,
   strengthen the original command's canonical expected-version check to
   include ALL non-nil top-level fields in its caller-provided :model/before
   document, and add the canonical document-field-count ASSERT. The existing
   gateway then admits and owns EXACTLY the strengthened transaction vector.

   This does not authenticate the supplied prestate, provide the transaction's
   internal read basis, prevent ABA writes, protect nested attributes or
   other documents, or establish a universal XTDB isolation/serialization
   theorem. SQL ASSERT behavior remains a backend premise. No certificate,
   physical causality proof, writer closure, or principal authority follows.
   Reports do not contain identifiers, values, SQL text or transaction keys.

   v1600 adds an independent OPT-IN two-write constructor: strengthen the
   domain UPDATE as above, append the canonical durable command outcome CREATE
   with an absence ASSERT, then delegate the whole five-operation vector to
   ONE restricted XTDB submission. v1612 additionally requires the exact
   restricted-gateway return object and its source-bound one-attempt journal
   before that public key can propagate to Biff readback. This is process-
   local provenance, not authenticated physical transaction causality."
  (:require [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def guarded-type
  :gesso.assurance.proof.xtdb-gateway-complete-guard-execution/one-document)
(def guarded-version 1)

(def ^:private original-update? command/update?)
(def ^:private original-count tx/assert-document-field-count)
(def ^:private original-prepare tx/prepare)
(def ^:private original-gateway gateway/prepare-one-shot!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB complete-guard gateway construction."
                  {:xtdb-gateway-complete-guard-execution/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- scalar? [x]
  ;; Intentionally much narrower than XTDB's full value language. A nil
  ;; (including a nil field in :before) is not SQL-equality-comparable here.
  (or (string? x) (boolean? x)
      (and (integer? x) (not (instance? clojure.lang.BigInt x)))
      (instance? java.math.BigDecimal x)
      (instance? java.util.UUID x)
      (instance? java.time.Instant x)))

(defn- check-seams! []
  (when-not (and (identical? original-update? (var-get #'command/update?))
                 (identical? original-count
                             (var-get #'tx/assert-document-field-count))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-gateway
                             (var-get #'gateway/prepare-one-shot!)))
    (reject! :substituted-preparation-seam)))

(defn- strengthened-source! [source]
  (when-not (and (plain-map? source)
                 (= #{:plan :protected-documents} (set (keys source))))
    (reject! :invalid-gateway-source))
  (let [{:keys [plan protected-documents]} source]
    ;; Refuse a broader plan instead of silently dropping its guards,
    ;; assertions, changes, entry, callbacks, or transaction options.
    (when-not (and (plain-map? plan)
                   (= #{:commands :emit} (set (keys plan)))
                   (= false (:emit plan))
                   (plain-vector? (:commands plan))
                   (= 1 (count (:commands plan)))
                   (plain-vector? protected-documents)
                   (= 1 (count protected-documents)))
      (reject! :unsupported-plan-scope))
    (let [c (first (:commands plan))
          entry (first protected-documents)
          before (:model/before c)
          after (:model/after c)
          original-checks (get-in c [:model/expected :model/checks])
          target [(:model/entity-type c) (:model/id c)]]
      (when-not (and (original-update? c)
                     (plain-map? c) (plain-map? before) (plain-map? after)
                     (<= 2 (count before) 64)
                     (= (set (keys before)) (set (keys after)))
                     (= (:xt/id before) (:xt/id after) (:model/id c))
                     (every? keyword? (keys before))
                     (every? scalar? (vals before))
                     (every? scalar? (vals after))
                     (plain-map? entry)
                     (= #{:target :fields} (set (keys entry)))
                     (= target (:target entry))
                     (= :all (:fields entry)))
        (reject! :unsupported-single-document-update))
      (let [all-fields (disj (set (keys before)) :xt/id)
            checked-fields (set (map first original-checks))]
        (when-not (and (seq original-checks)
                       (every? (fn [[field value]]
                                 (and (contains? all-fields field)
                                      (= (get before field) value)))
                               original-checks))
          (reject! :version-check-not-bound-to-before))
        (let [additional-fields (sort-by str
                                         (remove checked-fields all-fields))
              stronger-checks (into (vec original-checks)
                                   (map (fn [field]
                                          [field (get before field)]))
                                   additional-fields)
              stronger (assoc-in c [:model/expected :model/checks]
                                 stronger-checks)
              count-assert (original-count (:model/entity-type c)
                                           (:model/id c) (count before))
              enriched {:plan {:commands [stronger]
                               :assertions [count-assert]
                               :emit false}
                        :protected-documents protected-documents}
              prepared (try (original-prepare nil (:plan enriched))
                            (catch Exception _
                              (reject! :strengthened-preparation-unsupported)))
              tx-ops (:tx-ops prepared)]
          (when-not (and (plain-vector? tx-ops)
                         (= 3 (count tx-ops))
                         (every? (fn [op]
                                   (and (plain-vector? op)
                                        (string? (first op))
                                        (re-find #"(?i)^ASSERT\b" (first op))))
                                 (take 2 tx-ops))
                         (= :put-docs (first (last tx-ops)))
                         (= (:model/after c) (last (last tx-ops))))
            (reject! :noncanonical-strengthened-vector))
          {:source enriched
           :field-count (count all-fields)
           :added-check-count (count additional-fields)})))))

(defn prepare-complete-guard-one-shot!
  "Prepare a strictly guarded ONE-document update using the existing
   v1525+ restricted one-shot gateway. Input is exactly the original ordinary
   one-command Gesso plan plus its :all protected-document declaration.

   Returns {:type :version :gateway :submit! :summary!}. The :gateway member
   is the original restricted gateway descriptor and supports its exact-return,
   attempt and ledger readback APIs. The submission closure sends only the
   source-rederived strengthened vector. Its result is unchanged.

   Every observed non-id top-level field must match its caller-supplied
   :model/before value at the transaction's ASSERT stage, and the canonical
   count ASSERT detects added or removed fields, subject to the tested XTDB
   beta1 semantics and accepted SQL scalar types. These are transaction
   PRECONDITIONS, not independent attestation of an internal prestate basis.
   There is no application admission, authenticated writer identity, or proof
   that unrelated writes cannot occur before or after the transaction."
  [node source]
  (check-seams!)
  (let [{:keys [source field-count added-check-count]}
        (strengthened-source! source)
        gate (original-gateway node source)
        summary! (fn []
                   (merge {:type guarded-type
                           :version guarded-version
                           :scope :one-update-complete-top-level-cas
                           :compared-top-level-field-count field-count
                           :added-snapshot-comparison-count added-check-count
                           :field-count-assertion :canonical-prewrite
                           :all-source-observed-fields-guarded? true
                           :source-prestate-authenticity :caller-supplied-unattested
                           :transaction-internal-read-basis :not-authenticated
                           :backend-assert-semantics :trusted-bounded-premise
                           :nested-and-external-fields :not-established
                           :aba-history-detection :not-established
                           :physical-poststate-causality :not-established
                           :complete-deployment-writer-coverage :not-established
                           :principal-authorization :not-established
                           :universal-physical-refinement :not-established
                           :proof-status :guarded-submission-only
                           :certificate :not-issued
                           :admission :not-admitted
                           :authority :none}
                          (select-keys ((:summary! gate))
                                       [:status :owned-execute-tx-call-entries])))]
    {:type guarded-type
     :version guarded-version
     :gateway gate
     :submit! (:submit! gate)
     :summary! summary!}))


;; v1600: The first opt-in TWO-write restricted submission boundary. The
;; original one-document constructor above remains unchanged. The command
;; record is constructed from the canonical command-resolution semantics,
;; not from an unvalidated caller-provided XTDB operation or record map.
(def incorporation-type
  :gesso.assurance.proof.xtdb-gateway-complete-guard-execution/atomic-command-incorporation)
(def incorporation-version 1)
(def ^:private original-incorporation resolution/incorporation-plan)
(def ^:private original-require-record resolution/require-record)
(def ^:private original-local-return gateway/require-local-return!)
(def ^:private original-local-attempt gateway/require-local-attempt!)

(defn- incorporation-seams! []
  (check-seams!)
  (when-not (and (identical? original-incorporation
                             (var-get #'resolution/incorporation-plan))
                 (identical? original-require-record
                             (var-get #'resolution/require-record))
                 (identical? original-local-return
                             (var-get #'gateway/require-local-return!))
                 (identical? original-local-attempt
                             (var-get #'gateway/require-local-attempt!)))
    (reject! :substituted-command-incorporation-seam)))

(defn- require-atomic-local-return!
  "v1612: Check only the SAME registered restricted gateway and the exact
   in-process response object from its ONE two-write execute-tx call.
   This is an execution-origin check, NOT a durable or causal proof.
   Failed checks occur after a potentially committed submission and MUST be
   treated by callers as an UNKNOWN outcome, never as safe retry authority."
  [prepared response]
  (incorporation-seams!)
  (let [returned (try (original-local-return prepared response)
                      (catch Throwable _
                        (reject! :atomic-return-origin-not-established)))
        attempt (try (original-local-attempt prepared)
                     (catch Throwable _
                       (reject! :atomic-attempt-origin-not-established)))]
    (incorporation-seams!)
    (when-not
      (and (= gateway/local-return-type (:type returned))
           (= gateway/local-return-version (:version returned))
           (= gateway/local-attempt-type (:type attempt))
           (= gateway/local-attempt-version (:version attempt))
           (= gateway/gateway-version (:gateway-version returned)
              (:gateway-version attempt))
           (= :exact-object-from-owned-call-site
              (:local-return-linkage returned))
           (= :readmitted-source-derived-finite-vector
              (:operation-origin returned))
           (= :process-local-nonauthenticating
              (:source-binding-scope returned)
              (:source-binding-scope attempt))
           (string? (:process-local-source-binding returned))
           (seq (:process-local-source-binding returned))
           (= (:process-local-source-binding returned)
              (:process-local-source-binding attempt))
           (= (:process-local-attempt-id returned)
              (:process-local-attempt-id attempt))
           (= 1 (:owned-execute-tx-call-entries returned)
              (:owned-execute-tx-call-entries attempt))
           (= 5 (:candidate-operation-count returned)
              (:candidate-operation-count attempt))
           (= 3 (:prewrite-assertion-count returned)
              (:prewrite-assertion-count attempt))
           (= 2 (:whole-document-replacement-count returned)
              (:whole-document-replacement-count attempt))
           (= :public-transaction-key
              (:observed-result-kind returned)
              (:observed-result-kind attempt))
           (= :returned-transaction-key (:status attempt))
           (= [:prepared :checking :submitting :returned-transaction-key]
              (:observed-local-events attempt))
           (= :process-local-result-correspondence-only
              (:proof-status returned))
           (= :not-established (:committed-transaction returned)
              (:committed-transaction attempt))
           (= :not-checked (:public-key-to-ledger-join returned)
              (:public-key-to-ledger-join attempt))
           (= :not-established (:physical-poststate-causality returned)
              (:physical-poststate-causality attempt))
           (= :not-issued (:certificate returned))
           (= :not-admitted (:admission returned))
           (= :none (:authority returned)))
      (reject! :atomic-return-attempt-linkage-not-established))
    ;; Return the original object unchanged; Biff's later readback uses its
    ;; public :tx-key. No new token, transaction authority, or receipt is made.
    response))

(defn prepare-complete-guard-command-incorporation-one-shot!
  "v1600: Prepare a SINGLE restricted XTDB submission with TWO writes:
   an existing whole-document domain UPDATE, covered by the original v1537
   complete-field and field-count CAS, and a canonical immutable command
   outcome CREATE, covered by an XTDB absence assertion. Both writes and
   all their preconditions are lowered from the SAME admitted Gesso model
   plan and passed to ONE v1525 restricted execute-tx gateway.

   Inputs: [node domain-source {:binding ... :outcome ... :now ...}].
   The domain source is exactly the v1537 one-update source, with :emit false;
   the command input never accepts a prebuilt record, plan, SQL or tx vector.
   The caller owns principal authenticity, outcome correctness, and timestamp
   provenance. The command record is source-derived, not a replay permit.

   Returns only {:type :version :submit! :summary!}. Nothing here performs
   a pre-submission physical read, checks an issued certified plan callback,
   proves that the domain revision is monotonic or registers a Biff writer.
   Those v1590-v1593 checks MUST be joined to this exact two-write gateway
   before it can replace the current unified Biff execution path. This
   constructor is deliberately opt-in and does not weaken that path.

   Under XTDB's assumed same-transaction ASSERT/PUT semantics, an accepted
   transaction cannot apply just one of these two writes. A returned public
   key is not on its own proof of commit, causality, immutable persistence,
   successful reconciliation, or protection from out-of-gateway writers."
  [node domain-source incorporation-input]
  (incorporation-seams!)
  (when-not (and (some? node)
                 (plain-map? incorporation-input)
                 (= #{:binding :outcome :now}
                    (set (keys incorporation-input))))
    (reject! :invalid-command-incorporation-input))
  ;; Reuse the precise one-document complete-CAS construction. It preserves
  ;; the original UPDATE and adds *all* supported top-level before guards.
  (let [{guarded :source} (strengthened-source! domain-source)
        incorporation
        (try
          (original-incorporation
           (assoc incorporation-input :plan (:plan guarded)))
          (catch Throwable _
            (reject! :invalid-canonical-command-incorporation)))
        record (:record incorporation)
        _ (try (original-require-record record)
               (catch Throwable _
                 (reject! :invalid-command-record)))
        domain-command (first (get-in guarded [:plan :commands]))
        commands (get-in incorporation [:plan :commands])
        command-target [resolution/command-entity-type (:xt/id record)]
        _ (when-not (and (= 2 (count commands))
                         (= domain-command (first commands))
                         (original-update? (first commands))
                         (command/create? (second commands))
                         (= resolution/command-entity-type
                            (:model/entity-type (second commands)))
                         (= record (:model/after (second commands)))
                         (= (:xt/id record) (:model/id (second commands)))
                         (= (command/target domain-command)
                            (:target (first (:protected-documents guarded))))
                         (not= (command/target domain-command) command-target))
            (reject! :unexpected-two-command-incorporation))
        closed-source {:plan (:plan incorporation)
                       :protected-documents
                       (conj (:protected-documents guarded)
                             {:target command-target :fields :all})}
        ;; Independently lower the ENTIRE plan and check precisely three
        ;; canonical preconditions + two complete-document PUTs. The restricted
        ;; gateway will independently lower/re-admit the same plan again.
        operations (try (:tx-ops (original-prepare nil (:plan closed-source)))
                        (catch Throwable _
                          (reject! :atomic-transaction-lowering-failed)))
        _ (when-not (and (vector? operations)
                         (= 5 (count operations))
                         (every? (fn [op]
                                   (and (vector? op)
                                        (string? (first op))
                                        (re-find #"(?i)^ASSERT\b" (first op))))
                                 (take 3 operations))
                         (= [:put-docs (:model/entity-type domain-command)
                             (:model/after domain-command)]
                            (nth operations 3))
                         (= [:put-docs resolution/command-entity-type record]
                            (nth operations 4)))
            (reject! :unexpected-atomic-transaction-vector))
        prepared (try (original-gateway node closed-source)
                      (catch Throwable _
                        (reject! :atomic-restricted-gateway-admission-failed)))
        initial (try ((:summary! prepared))
                     (catch Throwable _
                       (reject! :invalid-restricted-gateway-report)))
        _ (when-not (and (= gateway/gateway-type (:type prepared))
                         (= gateway/gateway-version (:version prepared))
                         (= :prepared (:status initial))
                         (= 0 (:owned-execute-tx-call-entries initial))
                         (= 2 (:whole-document-replacement-count initial))
                         (= 2 (:declared-command-count initial))
                         (= 5 (:candidate-operation-count initial))
                         (fn? (:submit! prepared))
                         (fn? (:summary! prepared)))
            (reject! :unexpected-atomic-gateway-contract))
        attempted (AtomicBoolean. false)
        submit! (fn []
                  (when-not (.compareAndSet attempted false true)
                    (reject! :one-shot-already-attempted))
                  (incorporation-seams!)
                  ;; The actual gateway independently re-admits and re-lowers
                  ;; its closed source immediately before the ONLY submission.
                  ;; Check its exact locally registered result and attempt
                  ;; BEFORE the response reaches the issued Biff writer.
                  ;; An after-submit check failure is outcome-unknown.
                  (let [response ((:submit! prepared))]
                    (require-atomic-local-return! prepared response)))
        summary! (fn []
                   (let [s ((:summary! prepared))]
                     {:type incorporation-type
                      :version incorporation-version
                      :status (if (.get attempted) :permit-consumed :prepared)
                      :underlying-gateway-status (:status s)
                      :owned-execute-tx-call-entries
                      (:owned-execute-tx-call-entries s)
                      :admitted-transaction-write-count 2
                      :single-restricted-submission :one-derived-xtdb-vector
                      :domain-update :complete-top-level-cas
                      :command-record-create :canonical-absence-precondition-prepared
                      :atomicity :conditional-on-xtdb-assert-and-put-semantics
                      :durable-record-readback :not-observed
                      :physical-commit-observation :not-observed
                      :command-binding-authenticity :application-premise
                      :outcome-and-time-authenticity :application-premise
                      :physical-prestate-acquisition :not-performed
                      :certified-plan-target-binding :not-established
                      :monotonic-revision-policy :not-established
                      :biff-registered-writer-adoption :not-established
                      :record-immutability-after-commit :not-established
                      :indeterminate-outcome-resolution :not-established
                      :physical-causality :not-established
                      :universal-refinement :not-established
                      :proof-status :conditional-two-write-atomic-plan-submission
                      :certificate :not-issued
                      :admission :not-admitted
                      :authority :one-shot-physical-submission-only
                      :open-obligations
                      #{:join-certified-plan-target-and-external-prestate
                        :join-exact-successor-monotonic-revision
                        :join-registered-biff-request-writer
                        :authenticate-principal-outcome-and-timestamp
                        :verify-command-record-readback-and-retention
                        :assume-backend-atomic-assert-and-put-semantics
                        :establish-deployment-wide-writer-closure}}))]
    (incorporation-seams!)
    {:type incorporation-type
     :version incorporation-version
     :submit! submit!
     :summary! summary!}))
