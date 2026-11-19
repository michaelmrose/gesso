(ns gesso.assurance.proof.xtdb-gateway-atomic-write-intent
  "v1887: Opt-in SAME-TRANSACTION physical write-intent production.

   Derive one ordinary Gesso CREATE command for a restricted, generated
   write-intent document and append it to the caller's admitted protected
   application plan BEFORE the original one-shot XTDB gateway re-admits and
   executes its entire vector. Thus a successful atomic XTDB transaction
   contains both the protected application PUT and the intent PUT; an XTDB
   assertion failure cannot commit only the generated intent.

   This is not an erasure/DELETE writer. The generated record is a scoped
   statement of intended non-ERASE work, NOT an authenticated audit event,
   independent observation of execute-tx, durable/complete producer log,
   proof of commit, evidence of transaction causality, writer coverage,
   history retention, or a certificate. The caller supplies the XTDB node.
   The published gateway is the unchanged original one-shot descriptor.
   Its per-call local evidence remains subject to all v1886 restrictions."
  (:require [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as tx-model])
  (:import [java.time Instant]
           [java.util UUID]))

(def intent-table :gesso_guarded_write_intents)
(def intent-kind "owned-guarded-nonerase-write-intent")
(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- reject! [reason]
  (throw (ex-info "Unsupported atomic guarded-write intent."
                  {:xtdb-gateway-atomic-write-intent/rejection true
                   :reason reason})))

(defn prepare-one-shot!
  "Make the ORIGINAL gateway prepare a single vector containing both the
   caller's complete admitted application plan and one GENERATED CREATE for
   a distinct write-intent document. No separate audit writer is used.

   Input is exactly the same source shape as the restricted gateway:
   {:plan <existing Gesso plan with :emit false>
    :protected-documents <finite full-document declarations>}.

   Generated fields are a fresh identifier, version metadata, an explicit
   intent-only kind and count of original application commands. The source
   and target identities, principal, credentials, and returned transaction
   key are never written to the intent record. The generated table is
   reserved here: original commands/protection cannot write it.

   The returned descriptor is exactly the original gateway, including its
   one-shot submit! and summary!. It grants no new ERASE, DELETE or retry."
  [node input]
  (when-not (and (map? input)
                 (= #{:plan :protected-documents} (set (keys input)))
                 (vector? (:protected-documents input)))
    (reject! :invalid-original-source))
  (let [admitted (try
                   (tx-model/admitted-plan (:plan input))
                   (catch Exception _
                     (reject! :unadmitted-original-plan)))
        commands (:commands admitted)
        protections (:protected-documents input)]
    (when-not (and (= false (:emit admitted))
                   (vector? commands)
                   (<= 1 (count commands) 63)
                   (not-any? #(= intent-table (first (command/target %)))
                             commands)
                   (not-any? #(= intent-table (first (:target %)))
                             protections))
      (reject! :reserved-intent-table-or-unsupported-plan))
    (let [stamp (Instant/now)
          record {:xt/id (str (UUID/randomUUID))
                  :revision 0
                  :created-at stamp
                  :updated-at stamp
                  :intent-kind intent-kind
                  :application-command-count (count commands)
                  :intent-scope "one-admitted-xtdb-transaction-vector-only"}
          audit-command (command/create intent-table record version-keys)
          complete-source
          {:plan (update admitted :commands conj audit-command)
           :protected-documents
           (conj protections {:target [intent-table (:xt/id record)]
                              :fields :all})}]
      ;; The ORIGINAL gateway re-admits the entire enlarged plan, including
      ;; the generated intent PUT. It alone owns the physical execute-tx.
      ;; No gateway callback or second transaction may produce this record.
      (gateway/prepare-one-shot! node complete-source))))
