(ns gesso.model.tx-refinement-evidence
  "Fail-closed *candidate evidence* review for the open XTDB refinement premises.

   A proof artifact's title, hash, or claimed issuer is not its proof. In
   particular, a caller cannot discharge an XTDB premise by constructing a
   map with plausible fields, attaching a successful test log, or asserting
   that a backend contract has been verified. This module identifies the
   *kind* of independent evidence required for each open premise and rejects
   malformed, incompatible, duplicated, or cross-subject proposals.

   Even structurally reviewable proposals have status :requires-independent-
   verification. There is deliberately no discharge API, verifier registry,
   certificate constructor, or production admission authority here. Genuine
   proof checking and explicitly trusted, version-pinned backend contracts
   must be established by a separate trusted mechanism before any premise
   status could change."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gesso.model.tx-refinement-premises :as premises]
            [gesso.model.tx-refinement-subject :as instance-subject]))

;; A route specifies ALTERNATIVE sets of *required* evidence kinds. A route
;; containing a two-element set requires BOTH pieces; having either alone is
;; insufficient even for a complete review package. This is only a shape
;; classification, never an independent validation of the alleged artifact.
(def ^:private routes
  {:versioned-backend-contract-or-proof
   [#{:versioned-backend-contract} #{:independent-formal-proof}]

   :predicate-equivalence-proof
   [#{:independent-predicate-proof}]

   :versioned-backend-contract-and-representation-proof
   [#{:versioned-backend-contract :representation-refinement-proof}]

   :internal-read-state-refinement-proof
   [#{:internal-read-state-proof}]

   :closed-world-and-dependency-proof
   [#{:application-dependency-closure-proof}]

   :independent-prestate-provenance-proof
   [#{:abstract-prestate-provenance-proof}]})

;; An independently fixed inventory: a new upstream premise cannot replace an
;; existing premise while preserving the count and plausible route metadata.
(def ^:private expected-premise-routes
  {:xtdb-dml-is-serialized-in-transaction-log-order
   :versioned-backend-contract-or-proof
   :xtdb-asserts-evaluate-against-current-transaction-state
   :versioned-backend-contract-or-proof
   :xtdb-assert-failure-rolls-back-all-transaction-writes
   :versioned-backend-contract-or-proof
   :xtdb-put-docs-atomically-realizes-command-after-documents
   :versioned-backend-contract-or-proof
   :xtdb-sql-predicates-refine-finite-guard-semantics
   :predicate-equivalence-proof
   :transaction-valid-time-and-value-representation-is-admitted
   :versioned-backend-contract-and-representation-proof
   :concrete-transaction-internal-prestate-relates-to-abstract-prestate
   :internal-read-state-refinement-proof
   :relevant-external-writers-and-policy-dependencies-are-closed
   :closed-world-and-dependency-proof
   :supplied-abstract-prestate-is-complete-and-genuine
   :independent-prestate-provenance-proof})

(def ^:private owners-by-route
  {:versioned-backend-contract-or-proof :xtdb-backend-semantics
   :predicate-equivalence-proof :xtdb-backend-semantics
   :versioned-backend-contract-and-representation-proof :xtdb-backend-semantics
   :internal-read-state-refinement-proof :concrete-abstract-correspondence
   :closed-world-and-dependency-proof :application-admission
   :independent-prestate-provenance-proof :concrete-abstract-correspondence})

(def ^:private proposal-keys
  #{:premise-id :evidence-kind :subject :artifact-sha256
    :claimed-issuer :artifact-version})

(def ^:private instance-proposal-keys
  (conj proposal-keys :instance-subject-sha256))

(def evidence-scope
  {:obligation :gesso.model.tx/refinement-evidence-review-v1
   :premise-source :gesso.model.tx/serial-refinement-premise-ledger-v1
   :checked #{:open-premise-catalog-and-dependency-graph
              :independent-evidence-route-classification
              :proposal-shape-and-subject-binding
              :duplicate-and-unknown-proposal-rejection
              :no-caller-supplied-discharge}
   :not-established #{:artifact-existence
                      :artifact-content-authenticity
                      :application-plan-specific-subject-binding
                      :issuer-authentication
                      :independent-proof-validation
                      :backend-contract-admission
                      :physical-execution-attestation
                      :transaction-internal-read-state-refinement
                      :any-external-premise-discharged
                      :general-xtdb-refinement
                      :application-certification}})

(def instance-evidence-scope
  {:obligation :gesso.model.tx/instance-refinement-evidence-review-v1
   :premise-source (:premise-source evidence-scope)
   :subject-source (:identity instance-subject/subject-scope)
   :checked #{:exact-instance-proposal-shape
              :freshly-derived-plan-and-prestate-subject
              :declared-backend-source-and-footprint-binding
              :cross-instance-proposal-rejection
              :generic-premise-route-and-completeness-checks
              :no-caller-supplied-discharge}
   :not-established #{:source-or-binary-authentication
                      :application-dependency-or-writer-completeness
                      :selected-physical-prestate-authenticity
                      :artifact-existence-or-authenticity
                      :issuer-authentication
                      :independent-proof-validation
                      :backend-contract-admission
                      :physical-execution-attestation
                      :transaction-internal-read-state-refinement
                      :any-external-premise-discharged
                      :general-xtdb-refinement
                      :application-certification
                      :confidentiality-of-low-entropy-digest-input}})

(defn- contract-error! [reason]
  (throw (ex-info "XTDB refinement evidence source contract changed."
                  {:error/type ::source-contract-drift
                   :error/reason reason})))

(defn- require-ledger!
  [ledger]
  (let [ps (:open-premises ledger)
        ids (mapv :id ps)
        goal (:refinement-goal ledger)
        checks (get-in ledger [:local-argument :checks])
        local-passed? (and (seq checks) (every? #(= :checked (:status %)) checks))]
    (when-not (and (= premises/obligation-scope (:scope ledger))
                   (= (:premise-source evidence-scope) (:obligation ledger))
                   (vector? ps)
                   (= (set (keys expected-premise-routes)) (set ids))
                   (= (count ids) (count (set ids)))
                   (every? keyword? ids)
                   (every? (fn [{:keys [id owner discharge-route status]}]
                             (and (= :undischarged status)
                                  (= discharge-route (get expected-premise-routes id))
                                  (contains? routes discharge-route)
                                  (= owner (get owners-by-route discharge-route))))
                           ps)
                   (= :not-established (:status goal))
                   (= (into [:valid-local-argument] ids)
                      (:depends-on goal))
                   (= #{} (:external-premises-discharged ledger))
                   (false? (:general-refinement-proved? ledger))
                   (false? (:certifies-physical-execution? ledger))
                   (false? (:physical-observations-prove-backend-semantics? ledger))
                   (= (if local-passed? :checked :failed)
                      (get-in ledger [:local-argument :status]))
                   (= (if local-passed? :open-external-premises :invalid-local-argument)
                      (:status ledger)))
      (contract-error! :inconsistent-premise-ledger))
    ledger))

(defn- nonblank-string? [x]
  (and (string? x) (<= 1 (count x) 256) (not (str/blank? x))))

(defn- well-formed-proposal? [proposal instance-sha]
  (and (map? proposal)
       (= (if instance-sha instance-proposal-keys proposal-keys)
          (set (keys proposal)))
       (or (nil? instance-sha)
           (and (string? (:instance-subject-sha256 proposal))
                (boolean (re-matches #"[0-9a-f]{64}"
                                     (:instance-subject-sha256 proposal)))))
       (keyword? (:premise-id proposal))
       (keyword? (:evidence-kind proposal))
       (keyword? (:subject proposal))
       (string? (:artifact-sha256 proposal))
       (boolean (re-matches #"[0-9a-f]{64}" (:artifact-sha256 proposal)))
       (nonblank-string? (:claimed-issuer proposal))
       (nonblank-string? (:artifact-version proposal))))

(defn- review-one
  [by-id subject instance-sha seen index proposal]
  (let [id (:premise-id proposal)
        premise (get by-id id)
        kind (:evidence-kind proposal)
        valid? (well-formed-proposal? proposal instance-sha)
        token (when valid? [id kind (:artifact-sha256 proposal)])
        reason (cond
                 (not valid?) :malformed-proposal
                 (nil? premise) :unknown-premise
                 (not= subject (:subject proposal)) :wrong-formal-subject
                 (and instance-sha
                      (not= instance-sha (:instance-subject-sha256 proposal)))
                 :wrong-instance-subject
                 (not (some #(contains? % kind)
                            (get routes (:discharge-route premise))))
                 :incompatible-evidence-kind
                 (contains? seen token) :duplicate-proposal
                 :else :requires-independent-verification)]
    ;; Never expose claimed issuers, hashes, versions or unknown premise IDs:
    ;; those are untrusted strings and can contain private application data.
    {:index index
     :premise-id (when premise id)
     :status (if (= reason :requires-independent-verification)
               :review-required
               :rejected)
     :reason reason
     :authenticated? false
     :proof-checked? false
     :discharges-premise? false}))

(defn- review*
  "Shared fail-closed review engine. instance-sha is independently recomputed
   by the entrypoint when instance binding is required."
  [abstract-state plan proposals instance-sha]
  (when-not (and (vector? proposals) (<= (count proposals) 64))
    (throw (ex-info "Expected a bounded vector of XTDB evidence proposals."
                    {:error/type ::invalid-proposals})))
  (let [ledger (require-ledger!
                (premises/derive-obligation-ledger abstract-state plan))
        ps (:open-premises ledger)
        by-id (into {} (map (juxt :id identity)) ps)
        subject (:obligation ledger)
        reviews (loop [index 0 seen #{} result []]
                  (if (= index (count proposals))
                    result
                    (let [proposal (nth proposals index)
                          review (review-one by-id subject instance-sha seen index proposal)
                          token (when (well-formed-proposal? proposal instance-sha)
                                  [(:premise-id proposal)
                                   (:evidence-kind proposal)
                                   (:artifact-sha256 proposal)])]
                      (recur (inc index)
                             (if (= :review-required (:status review))
                               (conj seen token) seen)
                             (conj result review)))))
        reviewable (filterv #(= :review-required (:status %)) reviews)
        rejected? (some #(= :rejected (:status %)) reviews)
        local-valid? (= :checked (get-in ledger [:local-argument :status]))
        required (mapv (fn [{:keys [id owner discharge-route needed]}]
                         (let [alternatives (get routes discharge-route)
                               proposed (into #{}
                                              (keep (fn [r]
                                                      (when (and (= :review-required (:status r))
                                                                 (= id (:premise-id r)))
                                                        (:evidence-kind
                                                         (nth proposals (:index r))))))
                                                    reviews)]
                           {:id id
                            :owner owner
                            :needed needed
                            :discharge-route discharge-route
                            :candidate-alternatives alternatives
                            :review-package-complete?
                            (boolean (some #(set/subset? % proposed) alternatives))
                            :status :undischarged}))
                       ps)]
    {:obligation (:obligation (if instance-sha instance-evidence-scope evidence-scope))
     :status (cond
               (not local-valid?) :invalid-local-argument
               rejected? :invalid-evidence-proposals
               (empty? proposals) :awaiting-independent-evidence
               :else :independent-review-required)
     :subject subject
     :plan-specific-subject-bound?
     (boolean (and instance-sha (seq proposals) (not rejected?)
                   (= (count reviewable) (count proposals))))
     :local-argument-status (get-in ledger [:local-argument :status])
     :requirements required
     :proposals reviews
     :reviewable-proposals (count reviewable)
     :external-premises-discharged #{}
     :evidence-authenticated? false
     :any-proof-checked? false
     :backend-contract-admitted? false
     :general-refinement-proved? false
     :certifies-physical-execution? false
     :scope (if instance-sha instance-evidence-scope evidence-scope)}))

(defn review-evidence
  "Review untrusted evidence *proposals* against a freshly derived premise ledger.

   `proposals` must be a vector of at most 64 maps with exactly these keys:
     :premise-id       an existing open premise id
     :evidence-kind    a route-compatible candidate type
     :subject          the generic premise-ledger subject id (NOT plan-specific)
     :artifact-sha256  64 lowercase hexadecimal characters (claimed only)
     :claimed-issuer   nonblank string (unverified identity)
     :artifact-version nonblank string (unverified version)

   A digest is a caller-supplied label, NOT an authenticated content hash;
   this function does not retrieve artifacts or verify issuers or proofs.
   Every external premise stays :undischarged. The result reports whether
   proposals merit independent review, not whether they constitute proof.
   The subject identifier is intentionally ONLY the generic premise-catalog
   identity. It does not bind an artifact to a particular transaction, plan,
   application, dependency closure, or backend build; that work remains open.
   No XTDB I/O occurs. Sensitive proposal fields are never returned."
  [abstract-state plan proposals]
  (review* abstract-state plan proposals nil))

(defn review-instance-evidence
  "Review proposals bound to ONE selected transaction, abstract prestate and
   supplied environment. The additional required proposal key is
   :instance-subject-sha256, computed independently using
   tx-refinement-subject/derive-subject on the same three arguments. The
   ordinary :subject must still identify the generic premise catalog.

   Mismatched, omitted, malformed or extra instance binding is rejected and
   contributes no candidate evidence toward package completeness. The
   instance digest is NOT included in this report to avoid unnecessarily
   exposing a potentially guessable identifier derived from private state.
   The digest and declared source/backend/writer descriptors are *unattested*
   caller declarations. No issuer, artifact, premise or backend assertion
   is authenticated, independently verified, discharged or certified."
  [abstract-state plan environment proposals]
  (when-not (and (vector? proposals) (<= (count proposals) 64))
    (throw (ex-info "Expected a bounded vector of XTDB evidence proposals."
                    {:error/type ::invalid-proposals})))
  (let [computed (instance-subject/derive-subject abstract-state plan environment)
        instance-sha (:subject-sha256 computed)
        report (review* abstract-state plan proposals instance-sha)]
    (assoc report
           :binding-granularity (:binding-granularity computed)
           :instance-identity (:identity computed)
           :instance-subject-derived? true
           :instance-declarations-authenticated? false
           :application-dependencies-proved-complete? false
           :instance-evidence-verified? false)))
