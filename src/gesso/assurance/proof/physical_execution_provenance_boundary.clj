(ns gesso.assurance.proof.physical-execution-provenance-boundary
  "v1371: Boundary between a source-checked executable-writer catalog and
   ONE bounded physical XTDB observation.

   This joins the actual v1368 writer ledger with the ORIGINAL v1302 observed
   application-step source, not with caller-supplied success reports. It
   requires the SAME v1294 application source, operation, catalog/induction
   identities, and issued-plan transition fingerprint across both checks.

   A matching observation is still a COPYABLE, caller-supplied description.
   In particular the v1298 collector does not authenticate the submitted
   transaction payload, transaction-internal reads, deployed executor, or
   unobserved writers. No physical provenance, proof, or certificate is
   established by this join. All v1368 obligations remain OPEN."
  (:require [clojure.set :as set]
            [gesso.assurance.observed-correspondence :as observed]
            [gesso.assurance.proof.executable-writer-correspondence-obligations
             :as writers]))

(def boundary-version 1)
(def boundary-type
  :gesso.assurance.proof.physical-execution-provenance-boundary/step-audit)
(def capture-type
  :gesso.assurance.proof.physical-execution-provenance-boundary/capture-requirements)

(def ^:private input-keys #{:writer-source :observed-source})
(def ^:private observed-source-keys #{:application-source :transition-source})
(def ^:private capture-points
  [{:phase :issued-application :required :current-handler-and-plan-identity}
   {:phase :pre-submission :required :typed-input-and-authorization-binding}
   {:phase :inside-transaction :required :actual-transaction-read-snapshot}
   {:phase :inside-transaction :required :complete-executed-write-set}
   {:phase :commit-resolution :required :transaction-commit-or-reject-identity}
   {:phase :post-commit :required :ledger-and-snapshot-reconciliation}
   {:phase :deployment :required :current-executable-provenance}
   {:phase :global :required :complete-writer-inventory-and-refinement}])
(def ^:private immutable-open
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(def ^:private original-writer-check
  writers/derive-executable-writer-obligations!)
(def ^:private original-observed-check
  observed/check-observed-correspondence!)

(defn- reject! [reason at]
  ;; Do not embed observation documents, inputs, or command bindings in errors.
  (throw (ex-info "Invalid physical execution provenance boundary source."
                  {:physical-execution-provenance-boundary/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [x expected at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= expected (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- recheck-checkers! []
  (when-not (and (identical? original-writer-check
                             (var-get #'writers/derive-executable-writer-obligations!))
                 (identical? original-observed-check
                             (var-get #'observed/check-observed-correspondence!)))
    (reject! :substituted-upstream-checker [])))

(defn- checked-writers! [source]
  (recheck-checkers!)
  (let [r (original-writer-check source)
        rows (:operation-rows r)
        ids (set (map :operation-id rows))]
    (when-not (and (= writers/report-type (:type r))
                   (= writers/report-version (:version r))
                   (= :fresh-v1366-source-rechecked-and-obligations-derived
                      (:checking r))
                   (= ids (:operation-ids r))
                   (= (count rows) (count ids))
                   (= 9 (count (:required-obligation-ids r)))
                   (every? (fn [row]
                             (and (= (:required-obligation-ids r)
                                     (set (map :id (:obligations row))))
                                  (every? #(= :open (:status %))
                                          (:obligations row))
                                  (= :not-established
                                     (:cross-source-transition-equivalence row))
                                  (= :not-established (:executable-refinement row))))
                           rows)
                   (false? (:all-executable-refinement-obligations-discharged? r))
                   (= :not-established (:physical-snapshot-authenticity r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :not-established (:handler-transition-correspondence r))
                   (= :obligations-only (:proof-status r))
                   (= :not-issued (:certificate r))
                   (= :not-admitted (:admission r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :unexpected-writer-ledger-contract [:writer-source]))
    r))

(defn derive-capture-requirements!
  "Recheck current source/issued handlers and describe the capture points
   needed to establish physical provenance. This is NOT a collector or a
   trusted witness. No caller-provided map can discharge these requirements."
  [writer-source]
  (let [r (checked-writers! writer-source)]
    {:type capture-type
     :version boundary-version
     :source-catalog-id (:catalog-id r)
     :issued-application-fingerprint (:application-catalog-fingerprint r)
     :operation-ids (:operation-ids r)
     :capture-points capture-points
     :required-obligation-ids (:required-obligation-ids r)
     :unclassified-entry-point-ids (:observed-unclassified-entry-point-ids r)
     :writer-ledger-status (:status r)
     :status :capture-provenance-not-established
     :open-obligations (set/union immutable-open (:open-obligations r))
     :observed-execution-provenance :not-established
     :actual-submitted-payload-provenance :not-established
     :transaction-internal-state-provenance :not-established
     :deployed-writer-provenance :not-established
     :proof-status :requirements-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn- checked-observation! [source]
  (let [r (original-observed-check source)]
    (when-not (and (= observed/report-type (:type r))
                   (= observed/report-version (:version r))
                   (= :current-application-and-declared-physical-step-rechecked
                      (:checking r))
                   (= :caller-supplied-unauthenticated
                      (:physical-observation-provenance r))
                   (boolean? (:diagnostic-conforms? r))
                   (= (if (:diagnostic-conforms? r)
                        :corresponds-to-declared-step
                        :diverges-from-declared-step)
                      (:diagnostic-correspondence r))
                   (map? (:observed-checks r))
                   (every? boolean? (vals (:observed-checks r)))
                   (= :not-established (:authenticated-physical-correspondence r))
                   (= :not-established (:universal-xtdb-refinement r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :unexpected-observed-step-contract [:observed-source]))
    r))

(defn audit-supplied-observation!
  "Join one ORIGINAL v1302 physical-observation source with the ORIGINAL
   v1368/v1366 writer source. Both are freshly rechecked. Cross-check the
   issued operation/fingerprints and original application source; refuse a
   spliced observation from a different app, operation, or issued plan.

   Successful equality is BOUNDED DIAGNOSTIC agreement, never authenticated
   execution provenance. Divergent observed physical state is a result, not
   a source-shape rejection. This does not discharge ANY physical obligation."
  [source]
  (exact-map! source input-keys [])
  (let [{:keys [writer-source observed-source]} source]
    (exact-map! observed-source observed-source-keys [:observed-source])
    ;; Check same exact original source before expensive independent checks.
    ;; Equality of live functions/handles is source identity, not deployed
    ;; execution identity, nor an attestation that either is current in prod.
    (when-not (= (:application-induction-source writer-source)
                 (:application-source observed-source))
      (reject! :different-application-sources [:observed-source :application-source]))
    (let [writer-report (checked-writers! writer-source)
          observation-report (checked-observation! observed-source)
          operation (:operation observation-report)
          row (first (filter #(= operation (:operation-id %))
                             (:operation-rows writer-report)))]
      (when-not (and row
                     (:declared-abstract-operation? row)
                     (:checked-issued-executor? row)
                     (= (:application-catalog-fingerprint writer-report)
                        (:catalog-fingerprint observation-report))
                     (= (:application-induction-fingerprint writer-report)
                        (:induction-fingerprint observation-report))
                     (= (:application-certificate-fingerprint writer-report)
                        (:certificate-fingerprint observation-report))
                     (= (:application-dependency-fingerprint writer-report)
                        (:dependency-fingerprint observation-report))
                     (= (:issued-plan-transition-fingerprint row)
                        (:transition-fingerprint observation-report)))
        (reject! :physical-observation-not-bound-to-checked-writer
                 [:observed-source :transition-source]))
      (let [matches? (:diagnostic-conforms? observation-report)
            catalog-ready? (= :refinement-obligations-generated
                              (:status writer-report))
            step-ready? (:induction-step-supported? row)
            status (cond
                     (not matches?) :observed-physical-divergence
                     (not catalog-ready?) :open-writer-catalog-or-induction
                     (not step-ready?) :open-independent-step-premises
                     :else :bounded-unauthenticated-observation-agreement)]
        {:type boundary-type
         :version boundary-version
         :checking :current-writer-ledger-and-original-observed-step-rechecked
         :scope :one-caller-supplied-bounded-xtdb-observation
         :operation-id operation
         :claimed-physical-tx-id (:physical-tx-id observation-report)
         :observed-application-fingerprint (:catalog-fingerprint observation-report)
         :observed-issued-plan-fingerprint (:transition-fingerprint observation-report)
         :independent-transition-fingerprint
         (:abstract-transition-fingerprint row)
         :independent-step-supported? step-ready?
         :writer-ledger-status (:status writer-report)
         :diagnostic-checks (:observed-checks observation-report)
         :diagnostic-conforms? matches?
         :status status
         :observation-origin :caller-supplied-unattested
         :claimed-transaction-id-authenticated? false
         :actual-submitted-payload-provenance :not-established
         :transaction-internal-state-provenance :not-established
         :all-physical-writer-paths-enumerated :not-established
         :abstract-to-physical-transition-equivalence :not-established
         :universal-execution-refinement :not-established
         :all-executable-refinement-obligations-discharged? false
         :operation-obligations (:obligations row)
         :open-obligations (set/union immutable-open
                                      (:open-obligations writer-report)
                                      (:open-obligations observation-report))
         :proof-status :bounded-unauthenticated-diagnostic-only
         :certificate :not-issued
         :admission :not-admitted
         :transaction-authority :none
         :authority :none}))))

(defn require-current-supplied-observation-audit!
  "Regenerate the entire bounded diagnostic from ORIGINAL sources; reject a
   stale or fabricated report. Equal copyable maps do NOT attest provenance."
  [prior source]
  (let [current (audit-supplied-observation! source)]
    (when-not (and (plain-map? prior)
                   (nil? (meta prior))
                   (= current prior))
      (reject! :stale-or-forged-provenance-audit []))
    current))
