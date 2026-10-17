(ns gesso.model.tx-refinement-premises
  "Explicit open proof obligations for Gesso's admitted XTDB serial step.

   This is a *boundary ledger*, not a backend theorem or a certificate. The
   local obligation is recomputed from Gesso's finite model and lowering;
   external obligations are individually typed, assigned to their rightful
   owner, and NEVER discharged by model agreement or physical test samples.

   An application developer writes ordinary Gesso operations and rules, not
   TLA+/Recife/Quint. This ledger identifies which additional independent
   facts a future verifier or versioned backend contract would have to supply.
   In particular, neither a supplied abstract state nor an observer's pinned
   XTDB snapshot attests the transaction's *internal* read state.

   Deliberately no API accepts caller-supplied proof statuses or a trust flag.
   This module cannot be used to certify a concrete physical execution."
  (:require [clojure.set :as set]
            [gesso.model.tx-refinement-prestate :as prestate]))

(def ^:private premise-definitions
  "Independent classifications of *all* open premises from prestate.

   :owner names the boundary responsible for independently establishing a
   premise. :discharge-route is a requirement for future assurance machinery,
   not a present discharge mechanism. No observations or successful samples
   here may change a premise's :status from :undischarged."
  {:xtdb-dml-is-serialized-in-transaction-log-order
   {:owner :xtdb-backend-semantics
    :discharge-route :versioned-backend-contract-or-proof
    :needed :serial-transaction-order}

   :xtdb-asserts-evaluate-against-current-transaction-state
   {:owner :xtdb-backend-semantics
    :discharge-route :versioned-backend-contract-or-proof
    :needed :assertion-statement-read-semantics}

   :xtdb-assert-failure-rolls-back-all-transaction-writes
   {:owner :xtdb-backend-semantics
    :discharge-route :versioned-backend-contract-or-proof
    :needed :atomic-rejection-semantics}

   :xtdb-put-docs-atomically-realizes-command-after-documents
   {:owner :xtdb-backend-semantics
    :discharge-route :versioned-backend-contract-or-proof
    :needed :put-docs-value-and-write-semantics}

   :xtdb-sql-predicates-refine-finite-guard-semantics
   {:owner :xtdb-backend-semantics
    :discharge-route :predicate-equivalence-proof
    :needed :sql-to-finite-predicate-refinement}

   :transaction-valid-time-and-value-representation-is-admitted
   {:owner :xtdb-backend-semantics
    :discharge-route :versioned-backend-contract-and-representation-proof
    :needed :temporal-and-value-encoding-semantics}

   :concrete-transaction-internal-prestate-relates-to-abstract-prestate
   {:owner :concrete-abstract-correspondence
    :discharge-route :internal-read-state-refinement-proof
    :needed :transaction-internal-state-relation}

   :relevant-external-writers-and-policy-dependencies-are-closed
   {:owner :application-admission
    :discharge-route :closed-world-and-dependency-proof
    :needed :complete-guarded-read-and-writer-footprint}

   :supplied-abstract-prestate-is-complete-and-genuine
   {:owner :concrete-abstract-correspondence
    :discharge-route :independent-prestate-provenance-proof
    :needed :complete-authentic-abstract-prestate}})

(def obligation-scope
  {:obligation :gesso.model.tx/serial-refinement-premise-ledger-v1
   :local-source :gesso.model.tx/conditional-serial-step-refinement-v1
   :checked #{:premise-catalog-equals-source-contract
              :local-check-catalog-equals-source-contract
              :recomputed-local-step-obligation
              :no-external-premise-discharged}
   :not-established #{:physical-execution-attestation
                      :internal-xtdb-read-snapshot-refinement
                      :xtdb-backend-semantics-proved
                      :external-writer-closure-proved
                      :general-xtdb-refinement
                      :application-certification}})

(defn- fail-contract!
  [message details]
  (throw (ex-info message
                  {:error/type ::source-contract-drift
                   :error/details details})))

(defn- validate-contract!
  "Fail if upstream adds/removes a premise or local obligation. A source
   change must NOT silently make the ledger omit a correctness dependency."
  [conditional]
  (let [expected (set (keys premise-definitions))
        declared (:undischarged-premises conditional)
        scoped (get-in conditional [:scope :undischarged-premises])
        local-names (get-in conditional [:scope :checked])
        actual-local (set (keys (:checks conditional)))]
    (when-not (and (set? declared)
                   (set? scoped)
                   (= expected declared scoped))
      (fail-contract! "XTDB refinement premise catalog is out of sync."
                      {:missing (set/difference expected declared)
                       :unknown (set/difference declared expected)}))
    (when-not (and (set? local-names)
                   (= local-names actual-local)
                   (every? keyword? local-names))
      (fail-contract! "XTDB local check catalog is out of sync."
                      {:unexpected (set/difference actual-local local-names)
                       :missing (set/difference local-names actual-local)}))
    (when-not (= (:obligation prestate/conditional-step-scope)
                 (:obligation conditional))
      (fail-contract! "XTDB conditional argument identity changed." {}))
    true))

(defn derive-obligation-ledger
  "Recompute all local checks; expose every still-open refinement premise.

   Produces a *reviewable requirement graph* rather than an assertion of
   backend truth. Local checks are :checked only when their EXACT result is
   boolean true. Failed or malformed checks invalidate the local argument.
   A legitimate abstract rejection is not a local-check failure.

   The final proof obligation depends on BOTH the valid local argument AND
   every open backend/application premise. It remains :not-established even
   when the local argument is valid. Returned data contain no transaction
   targets, document values, SQL parameters, or physical execution receipts.

   This function performs no XTDB I/O, accepts no third-party evidence, and
   is not suitable for marking application operations certified."
  [abstract-state plan]
  (let [conditional (prestate/derive-conditional-step abstract-state plan)
        _ (validate-contract! conditional)
        checks (:checks conditional)
        local-passed? (every? true? (vals checks))
        ;; Do not accept a contradictory success flag even if individual
        ;; checks happened to pass (or vice versa).
        coherent? (and (= local-passed? (:local-argument-valid? conditional))
                       (= local-passed? (:conditional-argument-admissible? conditional))
                       (= (if local-passed? :conditional-only :invalid-local-argument)
                          (get-in conditional [:physical-refinement :status])))
        _ (when-not coherent?
            (fail-contract! "XTDB conditional argument reports contradictory validity." {}))
        local (mapv (fn [name]
                      {:id name
                       :owner :gesso-local-construction
                       :status (if (true? (get checks name)) :checked :failed)})
                    (sort-by name (keys checks)))
        premises (mapv (fn [name]
                         (assoc (get premise-definitions name)
                                :id name
                                :status :undischarged
                                :depends-on []))
                       (sort-by name (keys premise-definitions)))
        local-status (if local-passed? :checked :failed)]
    {:obligation (:obligation obligation-scope)
     :status (if local-passed? :open-external-premises :invalid-local-argument)
     :local-argument {:id :valid-local-argument
                      :status local-status
                      :depends-on (mapv :id local)
                      :checks local}
     :open-premises premises
     :refinement-goal {:id :physical-serial-step-refines-finite-model
                       :status :not-established
                       :depends-on (into [:valid-local-argument]
                                         (map :id premises))}
     :external-premises-discharged #{}
     :physical-observations-prove-backend-semantics? false
     :general-refinement-proved? false
     :certifies-physical-execution? false
     :scope obligation-scope}))
