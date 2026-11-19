(ns gesso.assurance.proof.xtdb-gateway-issued-history-discharge-boundary
  "v1877: Define the INDEPENDENT evidence that a future checker would need
   before it could even consider closing the v1876 history obligations.

   This is a requirements interface, NOT that checker. No evidence producer,
   verification trust root, history-completeness checker, admission path or
   discharge operation is installed here. v1878 adds a separately configured
   detached Ed25519 operator-claim intake, but verifying that a key signed a
   statement is NOT verification of policy enforcement or complete audit.
   The source-bound report is copyable
   diagnostic data and grants no authority. Original kernel proof obligations
   are not rewritten or discharged."
  (:require [gesso.assurance.proof.xtdb-gateway-issued-history-obligations
             :as history]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-history-discharge-boundary/required-evidence)
(def report-version 1)

(def ^:private original-history-check
  history/derive-open-history-conditions!)

;; The names below describe FUTURE independent evidence classes, not XTDB
;; facilities claimed to exist. A query response, locally asserted policy,
;; copied report or successful application callback supplies NONE of them.
(def independent-evidence-requirements
  {:history-assurance/selected-row-version-retention-through-interval
   {:independent-verifier :retention-policy-and-enforcement-verifier-not-installed
    :must-establish #{:effective-retention-window-covers-observed-interval
                      :retention-enforced-for-all-relevant-system-valid-time-versions
                      :retention-policy-and-store-state-authentically-bound}}
   :history-assurance/selected-row-erasure-exclusion-through-interval
   {:independent-verifier :erase-prevention-or-complete-audit-verifier-not-installed
    :must-establish #{:erase-paths-prevented-or-completely-audited-for-interval
                      :audit-or-enforcement-integrity-independently-attested
                      :selected-row-history-not-erased-during-interval}}
   :history-assurance/complete-system-valid-time-interval-visibility
   {:independent-verifier :bitemporal-interval-coverage-verifier-not-installed
    :must-establish #{:all-relevant-system-time-transitions-covered
                      :all-relevant-valid-time-slices-covered
                      :visibility-gaps-and-history-truncation-excluded}}
   :history-assurance/authenticated-database-target-and-snapshot-provenance
   {:independent-verifier :trusted-database-snapshot-identity-verifier-not-installed
    :must-establish #{:database-instance-and-node-authenticity
                      :exact-physical-target-bound-to-certified-operation
                      :interval-endpoints-authentically-bound-to-original-and-later-reads}}})

(def required-witness-bindings
  #{:original-certified-operation-proof-origin
    :authenticated-deployed-handler-and-executor
    :authenticated-database-instance
    :exact-physical-table-and-row-id
    :original-checked-pinned-basis
    :later-checked-pinned-basis
    :relevant-system-and-valid-time-interval
    :trusted-policy-audit-or-history-store-provenance})

(def insufficient-observations
  #{:copyable-caller-supplied-assertions
    :successful-original-executor-candidate-capture
    :source-relative-typed-transition-check
    :matching-pinned-current-document
    :matching-historical-before-document
    :same-or-advanced-database-snapshot-basis
    :visible-system-time-row-boundary
    :bounded-selected-row-history-inventory
    :apparently-complete-current-query-result
    :source-declared-retention-or-no-erase-policy
    :operator-signed-retention-policy-assertion-without-enforcement-proof
    :operator-signed-erase-audit-assertion-without-audit-completeness-proof
    :dual-operator-and-auditor-signatures-without-verified-control-enforcement
    :sealed-auditor-signed-erasure-log-segment-without-proven-log-completeness})

(defn- reject! [reason]
  (throw (ex-info "Independent XTDB history-discharge boundary not established."
                  {:reason reason})))

(defn required-independent-evidence!
  "Derive source-relative *requirements* from the original checked catalog
   scope and the exact v1876 open ledger. The supplied ledger must equal a
   fresh original derivation; caller-edited discharge flags are rejected.

   Original scope authentication is the responsibility of the original
   collector's source check. This function only checks consistency and
   publishes requirements. It has no proof/evidence admission argument and
   cannot close ANY obligation even if the caller has a plausible witness."
  [source-scope original-open-history-ledger]
  (when-not (identical? original-history-check
                         (var-get #'history/derive-open-history-conditions!))
    (reject! :substituted-original-history-obligation-checker))
  (let [original (original-history-check source-scope)
        ids (:certified-operation-ids source-scope)
        origin-map (:certified-operation-proof-origins source-scope)
        open-entries (:certified-operation-open-conditions original)]
    (when-not (and (= original original-open-history-ledger)
                   (= history/required-conditions
                      (set (keys independent-evidence-requirements)))
                   (set? ids) (seq ids)
                   (= ids (set (keys origin-map)) (set (keys open-entries)))
                   (= #{} (:discharged-conditions original))
                   (every? (fn [[operation row]]
                             (and (= (get origin-map operation)
                                     (:original-certified-proof-origin row))
                                  (= history/required-conditions
                                     (:required-conditions row))
                                  (= #{} (:discharged-conditions row))
                                  (= :open-unproven (:disposition row))))
                           open-entries)
                   (= :none (:authority original))
                   (= :not-issued (:certificate original)))
      (reject! :altered-or-incomplete-original-history-obligation-ledger))
    {:type report-type
     :version report-version
     :provenance :original-current-catalog-derived-requirements-only
     :required-witness-bindings required-witness-bindings
     :independent-evidence-by-condition independent-evidence-requirements
     :insufficient-observations insufficient-observations
     :certified-operation-required-evidence
     (into (sorted-map)
           (for [[operation origin] origin-map]
             [operation {:original-certified-proof-origin origin
                         :open-conditions history/required-conditions
                         :required-evidence-by-condition independent-evidence-requirements
                         :discharged-conditions #{}
                         :verification :independent-verifier-not-installed}]))
     :uncertified-operation-ids (:uncertified-operation-ids original)
     :uncertified-route-ids (:uncertified-route-ids original)
     :uncertified-history-evidence-claims :none
     :source-relative-report-authenticates-deployment? false
     :trusted-evidence-producer :not-established
     :independent-evidence-verifier :not-installed
     :operator-signed-claim-intake :available-only-with-externally-pinned-key
     :operator-claim-is-independently-verified-control? false
     :trusted-snapshot-target-binding :not-established
     :history-retention-enforcement :not-established
     :complete-erasure-audit-or-prevention :not-established
     :full-bitemporal-interval-proof :not-established
     :discharged-conditions #{}
     :discharge-authority :none
     :history-continuity :not-established
     :original-kernel-proof-discharge :not-established
     :certificate :not-issued
     :transaction-authority :none
     :retry-authority :none
     :authority :none}))
