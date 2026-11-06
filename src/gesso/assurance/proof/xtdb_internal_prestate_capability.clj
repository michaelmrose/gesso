(ns gesso.assurance.proof.xtdb-internal-prestate-capability
  "v1483: Version-scoped capabilities for XTDB transaction-INTERNAL prestate.

   Inspect the *loaded* XTDB API artifact version and Gesso's actual query
   versus transaction option filters. Report which interfaces are available
   for prestate investigation, what the controlled v1386/v1482 experiments
   observed, and which claims remain open. This does not invoke XTDB, sample
   a snapshot, replay an operation, or authenticate experiment provenance.

   In particular a pinned read-only snapshot, a public TransactionKey, an
   xt.txs row, or a correlation metadata value does NOT attest the read basis
   used inside a later DML transaction. A complete serial history could
   eventually support a *different*, conditional refinement argument; it is
   not supplied or proved here. The report never issues certificates,
   settles commands, retries transactions, or permits downstream admission.

   All observations here refer to the specific XTDB 2.2.0-beta1 API and
   bounded experiments. A changed backend or changed Gesso option surface
   fails closed rather than carrying beta1 conclusions forward by default."
  (:require [gesso.live.consistency.xtdb :as live])
  (:import [xtdb.api TransactionKey]))

(def report-type
  :gesso.assurance.proof.xtdb-internal-prestate-capability/report)
(def report-version 1)
(def backend-artifact 'com.xtdb/xtdb-api)
(def pinned-backend-version "2.2.0-beta1")

(def ^:private required-query-options
  #{:snapshot-token :snapshot-time :current-time :await-token})
(def ^:private required-write-options
  #{:database :system-time :default-tz :metadata :authn})
(def ^:private forbidden-write-basis-options
  #{:snapshot-token :snapshot-time :current-time :await-token})

;; An *experiment reference* is not a test result acquired by this function.
;; Do not mistake names, comments, or this literal data for authenticated
;; runtime measurements, complete state-space exploration, or a proof.
(def ^:private bounded-evidence-references
  [{:source :gesso-v1386-controlled-xtdb-investigation
    :kind :prior-embedded-xtdb-experiment
    :findings #{:externally-pinned-read-is-repeatable-in-probe
                :stale-read-token-does-not-pin-later-dml-assert
                :show-snapshot-token-refused-in-dml}
    :scope :single-controlled-probe}
   {:source :gesso.assurance.proof.xtdb-internal-prestate-experiment-test
    :kind :prior-embedded-xtdb-regression-tests
    :findings #{:assert-after-write-observes-earlier-dml-write
                :failed-assert-rolls-back-preceding-writes-in-probes
                :canonical-assert-before-put-rejects-stale-version
                :public-tx-key-does-not-contain-internal-prestate}
    :scope :bounded-statement-and-document-cases}])

(def capability-matrix
  [{:id :read-only-snapshot-token
    :api :xtdb.api/q
    :availability :available-in-pinned-api
    :establishes :repeatable-external-read-basis-in-controlled-probes
    :does-not-establish :transaction-internal-dml-read-basis}
   {:id :read-only-fixed-clock
    :api :xtdb.api/q
    :availability :available-in-pinned-api
    :establishes :explicit-query-valid-time-clock
    :does-not-establish :dml-valid-time-or-internal-basis}
   {:id :write-operation-batch
    :api :xtdb.api/execute-tx
    :availability :available-in-pinned-api
    :establishes :submitted-operations-under-trusted-api-semantics
    :does-not-establish :authenticated-executed-payload-or-readset}
   {:id :prewrite-sql-assert
    :api :xtdb.api/execute-tx
    :availability :bounded-observed
    :establishes :assert-before-write-sequencing-for-tested-forms
    :does-not-establish :independent-internal-guard-input-attestation}
   {:id :postwrite-sql-assert
    :api :xtdb.api/execute-tx
    :availability :bounded-observed
    :establishes :earlier-dml-writes-visible-to-later-asserts-in-probes
    :does-not-establish :single-unchanging-statement-prestate}
   {:id :failed-assert-rollback
    :api :xtdb.api/execute-tx
    :availability :bounded-observed
    :establishes :rollback-of-tested-preceding-writes
    :does-not-establish :universal-atomicity-or-serializability}
   {:id :returned-transaction-key
    :api :xtdb.api/execute-tx
    :availability :available-in-pinned-api
    :establishes :public-transaction-identity-and-system-time
    :does-not-establish :internal-snapshot-or-guard-readset}
   {:id :transaction-ledger-readback
    :api :xtdb.api/q
    :availability :available-in-pinned-api
    :establishes :separate-ledger-observation-under-reader-basis
    :does-not-establish :payload-origin-or-transaction-internal-prestate}
   {:id :transaction-metadata
    :api :xtdb.api/execute-tx
    :availability :available-in-pinned-api
    :establishes :caller-supplied-correlation-fields
    :does-not-establish :authenticated-operation-or-prestate-origin}
   {:id :dml-system-time-override
    :api :xtdb.api/execute-tx
    :availability :available-in-pinned-api
    :establishes :requested-system-time-not-a-snapshot-selector
    :does-not-establish :historical-read-basis-for-dml}
   {:id :snapshot-token-as-write-option
    :api :xtdb.api/execute-tx
    :availability :not-supported-in-pinned-write-interface
    :establishes :nothing
    :does-not-establish :dml-internal-read-basis}
   {:id :show-snapshot-token-inside-dml
    :api :xtdb.api/execute-tx
    :availability :rejected-in-controlled-probe
    :establishes :nothing
    :does-not-establish :dml-internal-read-basis}])

(defn- runtime-api-version []
  ;; Package implementation version comes from the jar's manifest. If the
  ;; executable is repackaged without that manifest, its version is UNKNOWN:
  ;; fail closed instead of trusting the declared dependency coordinates.
  (some-> TransactionKey .getPackage .getImplementationVersion))

(defn- interface-checks []
  (let [query (set live/query-option-keys)
        write (set live/tx-option-keys)
        version (runtime-api-version)]
    {:pinned-runtime-api-version (= pinned-backend-version version)
     :gesso-query-basis-options-present
     (every? query required-query-options)
     :gesso-write-options-present
     (every? write required-write-options)
     :gesso-does-not-forward-query-basis-as-write-options
     (not-any? write forbidden-write-basis-options)}))

(defn capability-report!
  "Return a redacted, version-scoped, *non-certifying* capability report.

   :compatible means only that the *loaded* API manifest and Gesso's option
   filters match the pinned beta1 interface assumptions. The lack of an
   exposed internal DML read-basis attestation is a reviewed beta1 API fact,
   not a theorem about all XTDB internals or future releases. No test is run
   here; prior experiment findings are referenced, not replayed or certified.

   On incompatible/unknown runtime versions or changed option forwarding,
   mark the capability report unsupported instead of extrapolating findings.
   Never interpret either result as a positive physical-refinement proof."
  []
  (let [checks (interface-checks)
        compatible? (every? true? (vals checks))]
    {:type report-type
     :version report-version
     :backend {:artifact backend-artifact
               :expected-version pinned-backend-version
               :loaded-api-version (runtime-api-version)}
     :compatibility (if compatible?
                      :pinned-api-and-gesso-option-surface
                      :unsupported-or-changed-interface)
     :checks checks
     :capabilities (if compatible? capability-matrix [])
     :prior-experiments (if compatible? bounded-evidence-references [])
     :prior-experiments-reexecuted? false
     :observed-external-read-basis :not-queried-by-this-report
     :authenticated-internal-prestate :not-established
     :internal-prestate-obligation :open
     :serial-history-proof-route :conditional-proposal-only
     :required-future-evidence
     #{:complete-protected-writer-history
       :admitted-physical-execution-boundaries
       :backend-serialization-and-atomicity-semantics
       :guard-correspondence-at-serial-prestate
       :protected-domain-write-and-frame-correspondence
       :valid-time-correspondence
       :authenticated-operation-outcome-and-history-position}
     :unproved #{:authenticated-transaction-internal-snapshot
                 :complete-writer-coverage
                 :universal-physical-step-refinement
                 :universal-application-certification}
     :proof-status :not-established
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :retry-authority :none
     :settlement-authority :none
     :authority :none}))
