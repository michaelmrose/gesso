(ns gesso.assurance.proof.physical-xtdb-transition-provenance
  "v1467: In-process origin witness for ONE v1465 controlled XTDB observation.

   A copied collector report can pass structural validation without ever
   having come from a physical submission. This module closes that LOCAL
   reporting gap by issuing an opaque, identity-registered witness only AFTER
   invoking the ORIGINAL controlled collector exactly once and validating its
   complete result with v1465. Rechecking NEVER submits or reads XTDB.

   The live witness binds the result to the caller-supplied node OBJECT, the
   original plan and tracked targets, and the redacted classified report. The
   returned provenance description contains no node, plan, payload, document,
   physical transaction identifier, or snapshot token. A forged/de-serialized
   handle cannot pass the private process registry.

   This establishes process-local invocation origin conditional on the trusted
   JVM and unchanged collector implementation. It does NOT authenticate the
   caller's node as production, provide an independent XTDB transaction
   receipt, prove payload or transaction-internal snapshot correspondence,
   exclude other writers, or grant certification/admission/transaction rights.
   It is a controlled TEST diagnostic, not a production execution wrapper.

   observe-provenanced-transition! DOES WRITE through the collector. Call it
   only for a deliberately controlled test node. Unknown/post-submission
   exceptions propagate unchanged; no retry is attempted."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-xtdb-transition-observation :as phase]
            [gesso.model.tx-refinement-xtdb :as collector])
  (:import (java.util WeakHashMap)))

(def provenance-version 1)
(def provenance-type
  :gesso.assurance.proof.physical-xtdb-transition-provenance/process-local-witness)

(deftype ControlledTransitionHandle []
  Object
  (toString [_] "#<gesso.assurance/controlled-transition-witness>"))

(def ^:private ^WeakHashMap live-witnesses (WeakHashMap.))
(def ^:private original-collector collector/observe-and-derive-conditional!)
(def ^:private original-classify phase/classify-controlled-report!)

(def ^:private additional-open
  #{:authenticated-production-node-and-handler
    :independent-physical-transaction-receipt
    :production-executor-to-collected-plan-correspondence
    :physical-submission-and-payload-attestation
    :transaction-internal-snapshot-equivalence
    :complete-physical-writer-inventory
    :universal-xtdb-refinement})

(defn- reject! [reason]
  ;; Neither exception message nor data should expose a private plan or node.
  (throw (ex-info "Invalid process-local controlled transition witness."
                  {:physical-xtdb-transition-provenance/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- validate-input! [input]
  (when-not (and (plain-map? input)
                 (= #{:node :plan :targets} (set (keys input)))
                 (some? (:node input))
                 (some? (:plan input))
                 (vector? (:targets input))
                 (nil? (meta (:targets input))))
    (reject! :invalid-controlled-input))
  input)

(defn- original-boundaries! []
  (when-not (and (identical? original-collector
                             (var-get #'collector/observe-and-derive-conditional!))
                 (identical? original-classify
                             (var-get #'phase/classify-controlled-report!)))
    (reject! :substituted-collector-or-classifier)))

(defn- entry [handle]
  (when (instance? ControlledTransitionHandle handle)
    (locking live-witnesses (.get live-witnesses handle))))

(defn process-local-witness? [handle]
  "Recognize only an identity-registered witness in THIS JVM. This is NOT an
   authorization predicate and has no meaning after process restart."
  (boolean (entry handle)))

(defn- describe-entry [witness]
  {:type provenance-type
   :version provenance-version
   :status :completed-controlled-collector-invocation
   :phase-observation (:report witness)
   :origin :private-in-process-collector-return
   :node-binding :same-caller-supplied-object
   :plan-and-target-binding :original-immutable-values
   :collector-invocations-recorded 1
   :model-step-agreement (get-in witness [:report :abstract-correspondence
                                           :observed-step-agreement?])
   :tx-outcome (get-in witness [:report :outcome :reported-status])
   :live-transaction-key-observed-in-collector? true
   :physical-receipt-authenticated? false
   :open-obligations (set/union additional-open
                                (:open-obligations (:report witness)))
   :scope :one-controlled-single-node-local-invocation
   :proof-status :process-local-diagnostic-only
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})

(defn describe-process-local-witness
  "Return a REDACTED, COPYABLE provenance description for a live handle.
   The description itself is not an authenticated witness."
  [handle]
  (original-boundaries!)
  (if-let [witness (entry handle)]
    (describe-entry witness)
    (reject! :unknown-or-forged-witness)))

(defn observe-provenanced-transition!
  "Submit ONE controlled XTDB transaction through the ORIGINAL collector and
   register its classified report only after the complete observer succeeds.

   Returns {:handle <opaque process-local handle> :report <redacted phase report>}.
   No handle is issued for a thrown/unknown outcome or invalid diagnostic.
   DO NOT retry on an exception: the transaction may already have committed."
  [input]
  (original-boundaries!)
  (validate-input! input)
  ;; Do not wrap this in catch/retry. The raw transaction ID is retained only
  ;; in the collector and never exposed through the witness description.
  (let [raw (original-collector input)
        report (original-classify raw)
        _ (when-not (and (= phase/observation-type (:type report))
                         (= phase/observation-version (:version report))
                         (#{:bounded-committed-step-agreement
                            :bounded-rejected-step-agreement
                            :bounded-observations-diverge} (:status report))
                         (= :none (:authority report))
                         (= :not-issued (:certificate report))
                         (false? (get-in report [:outcome
                                                 :physical-receipt-authenticated?])))
            (reject! :unexpected-phase-classification))
        handle (ControlledTransitionHandle.)
        witness {:node (:node input)
                 :plan (:plan input)
                 :targets (:targets input)
                 :report report}]
    (locking live-witnesses (.put live-witnesses handle witness))
    {:handle handle :report report}))

(defn require-current-process-local-witness!
  "Recheck the SAME in-process witness, original controlled input and exact
   v1465 report. NEVER re-executes a transaction. Returns a diagnostic map,
   NOT a certification or permission to make a second submission.

   The caller must provide the same node OBJECT (not an equal-looking node),
   and equal original plan and target values. The supplied report must equal
   the one produced by the actual observed invocation. Neither this API nor
   the registry authenticates a deployed application/node or physical ledger."
  [handle input report]
  (original-boundaries!)
  (let [witness (or (entry handle) (reject! :unknown-or-forged-witness))]
    (validate-input! input)
    (when-not (identical? (:node input) (:node witness))
      (reject! :controlled-node-substitution))
    (when-not (and (= (:plan input) (:plan witness))
                   (= (:targets input) (:targets witness)))
      (reject! :plan-or-target-substitution))
    (when-not (= report (:report witness))
      (reject! :stale-or-altered-phase-observation))
    ;; No collector replay: it would issue another physical transaction.
    (describe-entry witness)))
