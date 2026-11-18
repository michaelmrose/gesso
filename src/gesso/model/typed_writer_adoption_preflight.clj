(ns gesso.model.typed-writer-adoption-preflight
  "v1769: Fail-closed opt-in pre-submit check at the canonical resolved-operation
   transaction boundary. The current source-bound typed complete-CAS gateway
   accepts ONE domain document UPDATE with :emit false. The canonical resolved
   operation must instead atomically submit its domain work PLUS the Gesso
   command-result CREATE, including the original guards/options/Live policy.

   This seam explicitly REFUSES to substitute the one-document writer for the
   incorporated transaction. No physical writer or proof authority is issued.
   Checking an application-supplied candidate does NOT authenticate that it
   came from a genuine typed gateway or represents the correct business rule.
   A trusted constructor may have side effects before this pre-submit check;
   this gate only prevents the canonical tx/transact! submission."
  (:require [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]))

(def report-type
  :gesso.model.typed-writer-adoption-preflight/atomic-incorporation-compatibility)
(def report-version 1)
(def ^:dynamic ^:private *adoption-inspection* nil)

(defn- reject! [reason]
  (throw (ex-info "Cannot substitute a one-document typed gateway for the canonical model transaction."
                  {:typed-writer-adoption-preflight/rejection true
                   :reason reason})))

(defn- candidate-shape? [candidate]
  ;; This is only the public SHAPE of the currently supported typed gateway,
  ;; not a certificate that the supplied candidate passed its source checks.
  (let [plan (:plan candidate)
        entries (:commands plan)]
    (and (map? candidate) (nil? (meta candidate))
         (map? plan) (nil? (meta plan))
         (vector? entries) (= 1 (count entries))
         (map? (first entries))
         (not= :create (:model/operation (first entries)))
         (not= resolution/command-entity-type
               (:model/entity-type (first entries)))
         (false? (:emit plan)))))

(defn- canonical-ledger? [binding incorporated]
  (let [plan (:plan incorporated)
        record (:record incorporated)
        commands (:commands plan)
        ledger (filterv #(= resolution/command-entity-type
                            (:model/entity-type %)) commands)]
    (and (map? plan) (vector? commands) (<= 2 (count commands))
         (map? record) (= binding (:gesso.command/binding record))
         (try (= record (resolution/require-record record))
              (catch Exception _ false))
         (= 1 (count ledger))
         (= :create (command/operation (first ledger)))
         (= record (command/after (first ledger)))
         (= [resolution/command-entity-type (resolution/record-id binding)]
            (tx/command-target (first ledger)))
         (some #(not= resolution/command-entity-type
                      (:model/entity-type %)) commands))))

(defn require-before-canonical-submit!
  "Called by Gesso at the actual incorporated-plan boundary, before its
   single tx/transact! invocation. Outside an opt-in inspection this is a
   no-op. An inspection refuses an incompatible typed-gateway substitution
   BEFORE any canonical physical submission. Never re-run a failed/uncertain
   operation to obtain more evidence."
  [binding incorporated]
  (when-let [{:keys [operation thread attempted blocker candidate]} *adoption-inspection*]
    (swap! attempted inc)
    (cond
      (not (identical? thread (Thread/currentThread)))
      (do (reset! blocker :cross-thread-canonical-submit)
          (reject! :cross-thread-canonical-submit))

      (not= operation (:operation binding))
      (do (reset! blocker :wrong-canonical-operation)
          (reject! :wrong-canonical-operation))

      (not= 1 @attempted)
      (do (reset! blocker :multiple-canonical-submit-attempts)
          (reject! :multiple-canonical-submit-attempts))

      (not (canonical-ledger? binding incorporated))
      (do (reset! blocker :canonical-incorporation-ledger-not-established)
          (reject! :canonical-incorporation-ledger-not-established))

      (not (candidate-shape? candidate))
      (do (reset! blocker :unsupported-typed-candidate-shape)
          (reject! :unsupported-typed-candidate-shape))

      :else
      ;; The real incorporated plan contains at least a domain command and a
      ;; durable command record CREATE. The checked typed gateway still only
      ;; supports a single document update; accepting it here would silently
      ;; break atomic command incorporation and authoritative reconciliation.
      (do (reset! blocker :typed-single-update-omits-atomic-command-ledger)
          (reject! :typed-single-update-omits-atomic-command-ledger)))))

(defn inspect-typed-writer-adoption!
  "Execute one trusted canonical operation submission under the fail-closed
   compatibility inspection. The candidate is an UNAUTHENTICATED typed-style
   source and cannot replace the built-in transaction writer. A positive
   adoption result is intentionally impossible in the current gateway scope.

   :execution may contain sensitive results/Throwables; never publish it as
   a report. :report is redacted and confers no submit, retry or proof power."
  [operation candidate invoke!]
  (when-not (and (qualified-keyword? operation) (map? candidate)
                 (fn? invoke!))
    (reject! :invalid-adoption-inspection))
  (when *adoption-inspection*
    (reject! :nested-adoption-inspection))
  (let [attempted (atom 0)
        blocker (atom :not-reached)
        context {:operation operation :candidate candidate
                 :thread (Thread/currentThread)
                 :attempted attempted :blocker blocker}
        execution (binding [*adoption-inspection* context]
                    (try {:outcome :returned :result (invoke!)}
                         (catch Throwable e {:outcome :threw :error e})))
        status (cond
                 (> @attempted 1) :ambiguous-multiple-canonical-attempts
                 (zero? @attempted) :no-canonical-submit-reached
                 (and (= 1 @attempted)
                      (= :threw (:outcome execution))
                      (= @blocker (:reason (ex-data (:error execution)))))
                 :incompatible-before-canonical-model-write
                 :else :not-qualified)]
    {:execution execution
     :report {:type report-type :version report-version
              :operation-id operation
              :status status
              :blocked-reason (if (= :incompatible-before-canonical-model-write status)
                                @blocker :not-established)
              :atomic-command-ledger-required :yes
              :current-typed-gateway-scope :one-domain-update-only
              :canonical-tx-submitted
              (if (= :incompatible-before-canonical-model-write status)
                :not-submitted-at-canonical-callsite
                :not-established)
              :typed-gateway-adopted :not-established
              :business-policy-authenticity :not-established
              :deployed-http-adoption :not-established
              :xtdb-commit-authenticity :not-established
              :certificate :not-issued :admission :not-admitted
              :authority :none}}))
