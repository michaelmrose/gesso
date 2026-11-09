(ns gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
  "v1590: Opt-in monotonic-revision admission for ONE acquired, complete-CAS
   XTDB update. The existing model command constructor checks revision bumps,
   but a structurally valid hand-authored command can bypass that constructor:
   a generic :model/expected guard need not include the intended revision
   field, and :model/after need not advance it. This module fails closed on
   that gap BEFORE acquiring an execution capability.

   Require the named, non-nil top-level revision to be an exact natural JVM
   integer below Long/MAX_VALUE in :model/before; require :model/after to hold
   exactly its successor; and require the ORIGINAL optimistic version checks
   to compare that same old value exactly once. v1539 then matches the entire
   supplied prestate against a pinned same-node XTDB read, and v1537 adds
   transaction-time top-level CAS and field-count assertions. The restricted
   gateway owns the rederived one-shot operation vector.

   With the EXTRA DEPLOYMENT PREMISE that every relevant writer preserves a
   strictly increasing, non-wrapping revision, an old revision cannot reappear
   and stale-version ABA cannot pass the compare-and-set guard. That premise
   is NOT enforced against direct XTDB writers. Physical-history authenticity,
   other-writer closure, internal prestate, node identity, authorization,
   poststate causality and universal refinement remain UNESTABLISHED.
   v1590 adds a distinct opt-in plan-bound constructor that REUSES this exact
   monotonic revision policy and the v1586 certified-plan-bound acquired
   complete-CAS submission, not two separately prepared gateways. It owns
   only ONE underlying actual XTDB submission capability, with its existing
   plan-target recheck and an additional one-shot revision recheck. This is
   a gateway-building block, NOT yet a registered Biff writer: the earlier
   v1588 Biff mediator continues to prepare two independent gateways until
   the writer-identity integration is explicitly changed and tested.

   v1602 additionally exposes an opt-in two-write incorporation bridge:
   the same exact-successor policy and certified-plan target checks are
   combined with a pinned read-only prestate observation, followed by ONE
   atomic domain UPDATE / durable command-record CREATE gateway. This does
   not switch the registered Biff writer and does not authenticate command
   arguments, principals, outcomes or external writer behavior.

   Results are redacted and do not confer certificates or admission authority."
  (:require [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution
             :as acquired]
            [gesso.model.command :as command]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate
             :as plan-bound]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as complete]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as target-binding]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import (java.util.concurrent.atomic AtomicBoolean)
           (java.time Instant)
           (java.util UUID)))

(def monotonic-type
  :gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution/one-document)
(def monotonic-version 1)
(def plan-bound-monotonic-type
  :gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution/plan-bound)
(def plan-bound-monotonic-version 1)

(def ^:private original-acquired acquired/prepare-acquired-complete-guard-one-shot!)
(def ^:private original-update? command/update?)
(def ^:private original-plan-bound
  plan-bound/prepare-plan-bound-acquired-one-shot!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported XTDB monotonic revision gateway."
                  {:xtdb-gateway-monotonic-revision-execution/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- seams! []
  ;; Detect ordinary Var replacement; not signed bytecode or node provenance.
  (when-not (and (identical? original-acquired
                             (var-get #'acquired/prepare-acquired-complete-guard-one-shot!))
                 (identical? original-update? (var-get #'command/update?)))
    (reject! :substituted-revision-execution-seam)))

(defn- revision-value? [x]
  ;; Deliberately narrow: no arbitrary precision, negative values, fractional
  ;; values, booleans, or wrap-around. This is a model policy, not an XTDB
  ;; numeric coercion rule or a proof of external writer behavior.
  (and (integer? x)
       (not (instance? clojure.lang.BigInt x))
       (not (instance? java.math.BigInteger x))
       (<= 0 x)
       (< x Long/MAX_VALUE)))

(defn- require-source-revision! [source revision-field]
  (when-not (and (keyword? revision-field)
                 (nil? (namespace revision-field))
                 (not= revision-field :xt/id)
                 (<= 1 (count (name revision-field)) 64)
                 (re-matches #"[a-z][a-z0-9-]*" (name revision-field)))
    (reject! :unsupported-revision-field))
  (when-not (and (plain-map? source)
                 (= #{:plan :protected-documents} (set (keys source)))
                 (plain-map? (:plan source))
                 (= #{:commands :emit} (set (keys (:plan source))))
                 (false? (get-in source [:plan :emit]))
                 (plain-vector? (get-in source [:plan :commands]))
                 (= 1 (count (get-in source [:plan :commands]))))
    (reject! :unsupported-revision-plan))
  (let [c (first (get-in source [:plan :commands]))
        before (:model/before c)
        after (:model/after c)
        checks (get-in c [:model/expected :model/checks])
        old (get before revision-field)
        new (get after revision-field)]
    (when-not (and (plain-map? c) (original-update? c)
                   (plain-map? before) (plain-map? after)
                   (contains? before revision-field)
                   (contains? after revision-field)
                   (revision-value? old)
                   (and (integer? new)
                        (not (instance? clojure.lang.BigInt new))
                        (not (instance? java.math.BigInteger new)))
                   (= (inc old) new))
      (reject! :revision-not-exact-successor))
    (when-not (and (plain-vector? checks)
                   (= 1 (count (filter #(and (plain-vector? %)
                                             (= 2 (count %))
                                             (= revision-field (first %))
                                             (= old (second %))) checks)))
                   (= 1 (count (filter #(and (sequential? %)
                                             (= revision-field (first %))) checks))))
      (reject! :revision-not-originally-version-guarded))
    true))

(defn prepare-monotonic-revision-one-shot!
  "Prepare an acquired, one-use complete-field-CAS gateway for exactly one
   update with a source-bound monotonic revision.

   Accept [node source revision-field], where source is the normal Gesso
   one-command, one-:all-protected-document gateway source. Return
   {:type :version :gateway :submit! :summary!}; :gateway is the original
   restricted submission capability, so its exact-return/ledger observers
   remain available. No raw rows, revision values or IDs are returned.

   This checks a policy on the admitted before/after documents and requires
   revision in the ORIGINAL version guard. Transaction-time enforcement is
   inherited from v1537's full CAS, conditional on XTDB ASSERT semantics.
   Only an application-wide monotonic-writer discipline can rule out ABA
   restoration of the revision: that discipline is NOT attested here."
  [node source revision-field]
  (seams!)
  (require-source-revision! source revision-field)
  (let [g (original-acquired node source)]
    (seams!)
    (when-not (and (= acquired/acquired-type (:type g))
                   (= acquired/acquired-version (:version g))
                   (fn? (:submit! g))
                   (fn? (:summary! g))
                   (= :prepared (:status ((:summary! g)))))
      (reject! :unexpected-acquired-gateway-contract))
    {:type monotonic-type
     :version monotonic-version
     :gateway (:gateway g)
     :submit! (:submit! g)
     :summary!
     (fn []
       (merge
        {:type monotonic-type
         :version monotonic-version
         :revision-policy :source-checked-exactly-one-nonwrapping-successor
         :original-revision-version-guard :required-and-source-matched
         :external-prestate :same-node-pinned-read-matched-at-preparation
         :transaction-time-revision-guard :complete-top-level-cas
         :cooperating-writer-revision-aba
         :excluded-only-under-monotonic-complete-writer-premise
         :monotonic-complete-writer-premise :not-established
         :external-writer-aba :not-excluded
         :aba-history-detection :not-established
         :complete-deployment-writer-coverage :not-established
         :transaction-internal-prestate :not-authenticated
         :backend-assert-semantics :trusted-bounded-premise
         :caller-node-authenticity :not-established
         :physical-poststate-causality :not-established
         :principal-authorization :not-established
         :universal-physical-refinement :not-established
         :proof-status :conditional-revision-guard-only
         :certificate :not-issued
         :admission :not-admitted
         :authority :none}
        (select-keys ((:summary! g))
                     [:status :owned-execute-tx-call-entries
                      :compared-top-level-field-count
                      :added-snapshot-comparison-count])))}))


(defn- plan-bound-seams! []
  ;; Preserve the original monotonic gateway's independent source check and
  ;; the plan-bound gateway's current identity. Ordinary Var replacement is
  ;; rejected; hostile bytecode/JVM modification is outside this boundary.
  (seams!)
  (when-not (identical? original-plan-bound
                        (var-get #'plan-bound/prepare-plan-bound-acquired-one-shot!))
    (reject! :substituted-plan-bound-prestate-gateway)))

(defn prepare-plan-bound-monotonic-revision-one-shot!
  "v1590: Prepare one AND ONLY ONE XTDB submission capability satisfying
   both the v1547 exact-successor/source-version-guard policy and the v1586
   issued-certified-plan-target/external-prestate/complete-CAS policy.

   Inputs: [node contract-source resolver-issuance selected-operation request
            candidate-source revision-field]. The strict revision check is
   run BEFORE any XTDB prestate acquisition. The plan-bound gateway performs
   plan-target and command/protected-document checks and makes the only
   underlying acquired gateway, with its own revalidation on submit.

   Return only {:type :version :submit! :summary!}. No raw XTDB client,
   prepared SQL, gateway descriptor, document ID, or protected prestate is
   exposed. A submission attempt consumes the outer permit even if its final
   revision check or the underlying plan-target recheck fails. The original
   v1547 and v1586 public constructors remain unchanged.

   This is NOT a Biff writer issuance. It does not grant route authority,
   prove application target-table truth, close direct writers, establish
   actual transaction-internal prestate/physical causality, or rule out ABA
   without the stated complete-writer monotonicity premise."
  [node contract-source resolver-issuance selected-operation request
   candidate-source revision-field]
  (plan-bound-seams!)
  (require-source-revision! candidate-source revision-field)
  (let [prepared (original-plan-bound node contract-source resolver-issuance
                                      selected-operation request candidate-source)
        initial (try ((:summary! prepared))
                     (catch Throwable _
                       (reject! :invalid-plan-bound-prestate-summary)))]
    (when-not (and (= plan-bound/report-type (:type prepared))
                   (= plan-bound/report-version (:version prepared))
                   (fn? (:submit! prepared))
                   (fn? (:summary! prepared))
                   (= :prepared (:status initial))
                   (= :prepared (:underlying-gateway-status initial))
                   (= :same-node-pinned-read-matched-at-preparation
                      (:external-before initial))
                   (= :canonical-complete-top-level-cas
                      (:transaction-preconditions initial))
                   (= :conditional-plan-bound-acquired-prestate
                      (:proof-status initial))
                   (= :one-shot-submission-capability
                      (:transaction-authority initial))
                   (= :not-issued (:certificate initial))
                   (= :not-admitted (:admission initial)))
      (reject! :unexpected-plan-bound-prestate-contract))
    (plan-bound-seams!)
    (let [attempted (AtomicBoolean. false)
          submit! (fn []
                    (when-not (.compareAndSet attempted false true)
                      (reject! :one-shot-already-attempted))
                    (plan-bound-seams!)
                    (require-source-revision! candidate-source revision-field)
                    ;; This is the ONLY physical submit. The inner wrapper
                    ;; independently revalidates current plan-target identity.
                    ((:submit! prepared)))
          summary! (fn []
                     (let [underlying ((:summary! prepared))]
                       {:type plan-bound-monotonic-type
                        :version plan-bound-monotonic-version
                        :status (if (.get attempted) :permit-consumed :prepared)
                        :underlying-gateway-status
                        (:underlying-gateway-status underlying)
                        :physical-submission-gateways-prepared 1
                        :revision-policy
                        :source-checked-exactly-one-nonwrapping-successor
                        :original-revision-version-guard
                        :required-and-source-matched
                        :pre-submit-revision-recheck :required
                        :plan-target-correspondence
                        :exact-current-issued-callback-checked-at-preparation
                        :pre-submit-plan-target-recheck :required
                        :external-prestate
                        :same-node-pinned-read-matched-at-preparation
                        :transaction-time-revision-guard
                        :canonical-complete-top-level-cas
                        :prestate-submission-correspondence
                        :one-prepared-acquired-gateway-for-this-submission
                        :monotonic-complete-writer-premise :not-established
                        :external-writer-aba :not-excluded
                        :aba-history-detection :not-established
                        :caller-node-authenticity :not-established
                        :transaction-internal-prestate :not-authenticated
                        :backend-assert-semantics :trusted-bounded-premise
                        :physical-poststate-causality :not-established
                        :complete-deployment-writer-coverage :not-established
                        :principal-authorization :not-established
                        :abstract-operation-refinement :not-established
                        :target-table-business-truth :application-premise
                        :registered-biff-writer-adoption :not-established
                        :proof-status :conditional-plan-bound-monotonic-prestate
                        :certificate :not-issued
                        :admission :not-admitted
                        :transaction-authority :one-shot-submission-capability
                        :open-obligations
                        (conj (:open-obligations underlying)
                              :complete-deployment-monotonic-writer-discipline
                              :biff-writer-identity-not-yet-integrated)}))]
      (plan-bound-seams!)
      {:type plan-bound-monotonic-type
       :version plan-bound-monotonic-version
       :submit! submit!
       :summary! summary!})))


;; v1602: Opt-in single-capability combination of the exact v1590 monotonic
;; revision policy, the v1584 issued certified-plan target source, an
;; independent same-node XTDB snapshot read, and the v1600 TWO-write atomic
;; domain/command incorporation. Do not build a disposable v1586/v1590
;; submission capability just to observe its read: only the TWO-write
;; incorporation constructor below may prepare a physical submission.
(def plan-bound-incorporation-type
  :gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution/plan-bound-incorporation)
(def plan-bound-incorporation-version 1)

(def ^:private original-incorporation-gateway
  complete/prepare-complete-guard-command-incorporation-one-shot!)
(def ^:private original-target-resolver-check
  provenance/require-current-certified-literal-route-target-resolver!)
(def ^:private original-target-binding-check
  target-binding/require-declared-operation-target!)
(def ^:private original-xtdb-query xt/q)
(def ^:private original-basis-decode basis/<-time-basis-str)

(defn- incorporation-seams! []
  (plan-bound-seams!)
  (when-not (and (identical? original-incorporation-gateway
                             (var-get #'complete/prepare-complete-guard-command-incorporation-one-shot!))
                 (identical? original-target-resolver-check
                             (var-get #'provenance/require-current-certified-literal-route-target-resolver!))
                 (identical? original-target-binding-check
                             (var-get #'target-binding/require-declared-operation-target!))
                 (identical? original-xtdb-query (var-get #'xt/q))
                 (identical? original-basis-decode
                             (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-incorporation-correspondence-seam)))

(defn- checked-plan-incorporation-target!
  [contract-source resolver-issuance selected-operation request candidate]
  (incorporation-seams!)
  (when-not (and (plain-map? resolver-issuance)
                 (= #{:expected-target!} (set (keys resolver-issuance)))
                 (fn? (:expected-target! resolver-issuance))
                 (plain-map? request)
                 (keyword? selected-operation)
                 (some? (namespace selected-operation)))
    (reject! :invalid-incorporation-target-input))
  (let [resolver (:expected-target! resolver-issuance)
        report (try (original-target-resolver-check resolver contract-source)
                    (catch Throwable _
                      (reject! :unissued-or-changed-target-resolver)))]
    (when-not (and (= provenance/report-type (:type report))
                   (= provenance/report-version (:version report))
                   (= :closed-issued-certified-plan-target-callback
                      (:target-identity report))
                   (= :exact-issued-source-callback-checked
                      (:certified-plan-target-callback-identity report))
                   (= :none (:transaction-authority report))
                   (= :not-issued (:certificate report)))
      (reject! :requires-exact-issued-plan-bound-resolver))
    (let [expected (try (resolver request)
                        (catch Throwable _
                          (reject! :plan-bound-target-resolution-failed)))
          checked (try (original-target-binding-check
                       selected-operation expected candidate)
                       (catch Throwable _
                         (reject! :command-or-protection-disagrees-with-plan-target)))]
      (when-not (and (= target-binding/report-type (:type checked))
                     (= target-binding/report-version (:version checked))
                     (= :conditional-declared-command-target-match
                        (:status checked))
                     (true? (:selected-route-operation-checked? checked))
                     (true? (:canonical-command-operation-checked? checked))
                     (true? (:declared-command-and-protected-document-match-expected-target?
                             checked)))
        (reject! :unexpected-plan-bound-command-target-report))))
  (incorporation-seams!)
  true)

(defn- require-pinned-external-before!
  [node candidate]
  ;; This performs only a read. It intentionally parallels the narrow v1539
  ;; snapshot protocol, but DOES NOT prepare its one-UPDATE submit gateway.
  ;; The sole gateway is instead v1600's two-write incorporation gateway.
  (when-not node (reject! :missing-xtdb-node))
  (incorporation-seams!)
  (let [c (first (get-in candidate [:plan :commands]))
        table (:model/entity-type c)
        id (:model/id c)
        before (:model/before c)]
    (when-not (and (keyword? table)
                   (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                   (or (nil? (namespace table))
                       (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (namespace table)))
                   (plain-map? before)
                   (= id (:xt/id before))
                   (or (string? id) (instance? UUID id) (integer? id)))
      (reject! :unsupported-incorporation-prestate-read))
    (let [snapshot-rows (try (original-xtdb-query node "SHOW SNAPSHOT_TOKEN")
                             (catch Throwable _
                               (reject! :incorporation-snapshot-unavailable)))
          token (when (and (sequential? snapshot-rows)
                           (= 1 (count snapshot-rows))
                           (plain-map? (first snapshot-rows))
                           (= #{:snapshot-token} (set (keys (first snapshot-rows)))))
                  (:snapshot-token (first snapshot-rows)))
          _ (when-not (and (string? token) (seq token)
                           (try (map? (original-basis-decode token))
                                (catch Throwable _ false)))
              (reject! :invalid-incorporation-snapshot))
          table-name (if-let [schema (namespace table)]
                       (str schema "." (name table)) (name table))
          sql (str "SELECT * FROM " table-name " WHERE _id = ? LIMIT 2")
          rows (try (original-xtdb-query
                     node [sql id]
                     {:snapshot-token token :current-time (Instant/now)})
                    (catch Throwable _
                      (reject! :incorporation-prestate-read-failed)))]
      (when-not (and (sequential? rows) (= 1 (count rows))
                     (plain-map? (first rows))
                     (= before (first rows)))
        (reject! :incorporation-before-disagrees-with-external-read))))
  (incorporation-seams!)
  true)

(defn prepare-plan-bound-monotonic-command-incorporation-one-shot!
  "v1602: Build one guarded XTDB transaction incorporating BOTH a domain
   UPDATE and a canonical durable command record, binding its domain target
   to the Gesso-issued certified-plan :target! callback, acquiring its
   external prestate from the same supplied XTDB node, and enforcing the
   existing exact-successor revision policy. No second submit capability
   is prepared. One consumed submit! revalidates the plan and revision.

   Inputs: [node contract-source resolver-issuance selected-operation request
            candidate-source revision-field incorporation-input]. The
   incorporation input's :binding/:operation must match selected-operation;
   its principal, arguments, outcome and timestamp remain application-owned.

   This is not yet an identity-registered Biff writer, an authorization proof,
   transaction-internal read attestation, physical causality proof, a proof
   of the target table's business meaning, or a deployment-wide writer audit.
   Backend transaction assertions/atomicity remain explicit assumptions."
  [node contract-source resolver-issuance selected-operation request
   candidate-source revision-field incorporation-input]
  (incorporation-seams!)
  (require-source-revision! candidate-source revision-field)
  (when-not (and (plain-map? incorporation-input)
                 (= #{:binding :outcome :now} (set (keys incorporation-input)))
                 (plain-map? (:binding incorporation-input))
                 (= selected-operation
                    (get-in incorporation-input [:binding :operation])))
    (reject! :incorporation-binding-operation-disagrees-with-route))
  (checked-plan-incorporation-target!
   contract-source resolver-issuance selected-operation request candidate-source)
  (require-pinned-external-before! node candidate-source)
  ;; The one and only gateway re-derives the full two-write transaction:
  ;; v1600's canonical domain-field+count CAS and command-ID absence ASSERT.
  (let [prepared (original-incorporation-gateway
                  node candidate-source incorporation-input)
        initial (try ((:summary! prepared))
                     (catch Throwable _
                       (reject! :invalid-atomic-incorporation-report)))]
    (when-not (and (= complete/incorporation-type (:type prepared))
                   (= complete/incorporation-version (:version prepared))
                   (fn? (:submit! prepared))
                   (fn? (:summary! prepared))
                   (= :prepared (:status initial))
                   (= :prepared (:underlying-gateway-status initial))
                   (= 2 (:admitted-transaction-write-count initial))
                   (= :one-derived-xtdb-vector
                      (:single-restricted-submission initial))
                   (= :complete-top-level-cas (:domain-update initial))
                   (= :canonical-absence-precondition-prepared
                      (:command-record-create initial))
                   (= :not-issued (:certificate initial))
                   (= :not-admitted (:admission initial)))
      (reject! :unexpected-atomic-incorporation-contract))
    (incorporation-seams!)
    (let [attempted (AtomicBoolean. false)
          submit! (fn []
                    (when-not (.compareAndSet attempted false true)
                      (reject! :one-shot-already-attempted))
                    (incorporation-seams!)
                    (require-source-revision! candidate-source revision-field)
                    (checked-plan-incorporation-target!
                     contract-source resolver-issuance selected-operation
                     request candidate-source)
                    ;; Rechecks are read-only. The *only* physical submission
                    ;; is through the original v1600 two-write capability.
                    ((:submit! prepared)))
          summary! (fn []
                     (let [underlying ((:summary! prepared))]
                       {:type plan-bound-incorporation-type
                        :version plan-bound-incorporation-version
                        :status (if (.get attempted) :permit-consumed :prepared)
                        :underlying-gateway-status
                        (:underlying-gateway-status underlying)
                        :owned-execute-tx-call-entries
                        (:owned-execute-tx-call-entries underlying)
                        :physical-submission-gateways-prepared 1
                        :admitted-transaction-write-count 2
                        :domain-update :complete-top-level-cas
                        :command-record-create
                        :canonical-absence-precondition-prepared
                        :atomicity :conditional-on-xtdb-assert-and-put-semantics
                        :revision-policy
                        :source-checked-exactly-one-nonwrapping-successor
                        :original-revision-version-guard :required-and-source-matched
                        :plan-target-correspondence
                        :exact-current-issued-callback-checked-at-preparation
                        :pre-submit-plan-target-recheck :required
                        :external-prestate
                        :same-node-pinned-read-matched-at-preparation
                        :pre-submit-revision-recheck :required
                        :command-record-operation :bound-to-selected-route
                        :command-arguments-target-correspondence :not-established
                        :application-target-table-truth :not-established
                        :principal-authorization :not-established
                        :outcome-and-time-authenticity :application-premise
                        :physical-commit-causality :not-established
                        :durable-record-readback :not-observed
                        :record-immutability-after-commit :not-established
                        :external-writer-aba :not-excluded
                        :transaction-internal-prestate :not-authenticated
                        :complete-deployment-writer-coverage :not-established
                        :registered-biff-writer-adoption :not-established
                        :abstract-operation-refinement :not-established
                        :proof-status :conditional-plan-bound-atomic-incorporation
                        :certificate :not-issued
                        :admission :not-admitted
                        :transaction-authority :one-shot-submission-capability
                        :open-obligations
                        #{:authenticate-principal-outcome-and-timestamp
                          :check-command-arguments-against-selected-target
                          :prove-static-target-table-business-correctness
                          :verify-command-record-readback-and-retention
                          :establish-deployment-wide-writer-closure
                          :integrate-identity-registered-biff-request-writer
                          :assume-xtdb-atomic-assert-and-put-semantics
                          :establish-physical-commit-causality}}))]
      (incorporation-seams!)
      {:type plan-bound-incorporation-type
       :version plan-bound-incorporation-version
       :submit! submit!
       :summary! summary!})))
