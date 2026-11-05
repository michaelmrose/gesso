(ns gesso.assurance.proof.abstract-serial-history-witness
  "v1402: Replay a caller-supplied *abstract* command event trace and construct
   an inspectable candidate serialization of its successful incorporations.

   The v1400 machine specifies an atomic ledger-create transition, not an XTDB
   implementation. We replay that machine AND independently inspect the ledger
   delta, record, and incorporation-vector append for every event. A claimed
   serial order must match the resulting exact attempt/command-ID sequence.

   An accepted trace is NOT an authenticated physical transaction history or
   proof of serializability, conflict behavior, causal attribution, complete
   writer coverage, retry authorization, or application certification. The
   witness retains private bindings, outcomes, and records. NEVER expose the
   witness itself to clients or general-purpose logs; use redacted-summary."
  (:require [gesso.assurance.proof.durable-command-retry-semantics :as sem]
            [gesso.model.command-resolution :as resolution]))

(def witness-type :gesso.assurance.proof.abstract-serial-history-witness/witness)
(def witness-version 1)
(def max-events 256)

(def assurance-scope
  {:checked #{:exact-replay-of-supplied-abstract-events
              :per-event-ledger-delta-and-incorporation-append
              :declared-serial-order-agrees-with-abstract-commits
              :conflicting-attempts-do-not-append-incorporations
              :non-commit-events-do-not-change-the-durable-ledger
              :recomputable-witness-and-redacted-summary}
   :assumed #{:faithful-input-event-sequence
              :atomic-ledger-create-and-domain-mutation-in-the-abstract-machine
              :trusted-command-bindings-and-abstract-transition-semantics}
   :not-established #{:physical-transaction-order
                      :xtdb-serializability-or-linearizability
                      :authenticated-executed-write-set
                      :independently-acquired-prestate
                      :all-writers-mediated
                      :attempt-to-physical-commit-attribution
                      :retry-or-settlement-authority
                      :physical-refinement-or-certification}})

(def ^:private source-keys #{:events :candidate-order})
(def ^:private order-entry-keys #{:attempt-id :command-id})
(def ^:private witness-keys
  #{:type :version :source :step-observations :serial-history
    :final-abstract-history :classification :order-origin
    :physical-order-authenticated? :physical-refinement
    :safe-to-retry? :safe-to-settle? :authority :certificate
    :proof-status :open-obligations})

(defn- reject! [reason & [details]]
  (throw (ex-info "Invalid abstract serial-history witness."
                  (merge {:abstract-serial-history/rejection true :reason reason}
                         (or details {})))))

(defn- exact-map? [value expected]
  (and (map? value) (= expected (set (keys value)))))

(defn- require-source! [source]
  (when-not (exact-map? source source-keys)
    (reject! :invalid-source-envelope))
  (when-not (and (vector? (:events source))
                 (<= (count (:events source)) max-events)
                 (vector? (:candidate-order source))
                 (<= (count (:candidate-order source)) (count (:events source)))
                 (every? #(exact-map? % order-entry-keys)
                         (:candidate-order source)))
    (reject! :invalid-serial-order-source))
  source)

(defn- require-ledger-delta!
  [before after event event-index]
  ;; Inspect committed inserts separately from the machine's invariant?
  ;; predicate. This is a LOCAL abstract delta check, not physical write
  ;; mediation or a query for XTDB's transaction-internal read basis.
  (let [old-ledger (:ledger before)
        new-ledger (:ledger after)
        old-inc (:incorporations before)
        new-inc (:incorporations after)
        attempt-id (:attempt-id event)
        pre-attempt (get-in before [:attempts attempt-id])
        post-attempt (get-in after [:attempts attempt-id])
        commit-event? (= :physical-commit (:kind event))
        newly-committed? (and commit-event?
                              (= :committed (:physical post-attempt)))
        expected-command-id (when pre-attempt
                              (resolution/record-id (:binding pre-attempt)))
        expected-record (when newly-committed?
                          (resolution/command-record
                           (:binding pre-attempt) (:outcome event) (:now event)))
        expected-inc {:attempt-id attempt-id
                      :command-id expected-command-id
                      :record expected-record}
        unchanged? (and (= old-ledger new-ledger) (= old-inc new-inc))]
    (cond
      newly-committed?
      (when-not (and (= :undecided (:physical pre-attempt))
                     (not (contains? old-ledger expected-command-id))
                     (= new-ledger (assoc old-ledger expected-command-id
                                          expected-record))
                     (= new-inc (conj old-inc expected-inc)))
        (reject! :invalid-abstract-incorporation-delta
                 {:event-index event-index}))

      (not unchanged?)
      (reject! :nonincorporation-changed-ledger
               {:event-index event-index}))
    (when (and commit-event? (not newly-committed?)
               (not= :conflict (:physical post-attempt)))
      (reject! :invalid-conflict-transition {:event-index event-index}))
    (when (and (not commit-event?) (= :committed (:physical post-attempt))
               (not= :committed (:physical pre-attempt)))
      (reject! :noncommit-event-changed-physical-status
               {:event-index event-index}))
    (when newly-committed?
      {:serial-position (count old-inc)
       :event-index event-index
       :attempt-id attempt-id
       :command-id expected-command-id
       :record expected-record})))

(defn- replay! [events]
  (reduce-kv
   (fn [{:keys [history steps serial-history]} event-index event]
     (let [after (try
                   (sem/step history event)
                   (catch clojure.lang.ExceptionInfo e
                     (reject! :abstract-transition-rejected
                              {:event-index event-index
                               :transition-reason (:reason (ex-data e))})))
           incorporation (require-ledger-delta!
                          history after event event-index)]
       {:history after
        :steps (conj steps
                     {:event-index event-index
                      :event-kind (:kind event)
                      :attempt-id (:attempt-id event)
                      :physical-status (get-in after [:attempts (:attempt-id event)
                                                      :physical])
                      :incorporation-position (:serial-position incorporation)})
        :serial-history (cond-> serial-history
                          incorporation (conj incorporation))}))
   {:history (sem/initial-state)
    :steps []
    :serial-history []}
   events))

(defn- construct! [source]
  (require-source! source)
  (let [{:keys [events candidate-order]} source
        {:keys [history steps serial-history]} (replay! events)
        derived-order (mapv #(select-keys % order-entry-keys) serial-history)]
    (when-not (= candidate-order derived-order)
      (reject! :candidate-order-disagrees-with-abstract-history
               {:derived-count (count derived-order)
                :claimed-count (count candidate-order)}))
    {:type witness-type
     :version witness-version
     :source source
     :step-observations steps
     :serial-history serial-history
     :final-abstract-history history
     :classification :candidate-abstract-serial-history-agreement
     :order-origin :caller-supplied-abstract-event-order
     :physical-order-authenticated? false
     :physical-refinement :not-established
     :safe-to-retry? false
     :safe-to-settle? false
     :authority :none
     :certificate :not-issued
     :proof-status :replayed-abstract-witness-only
     :open-obligations (:not-established assurance-scope)}))

(defn audit-history!
  "Given {:events [events-in-proposed-time-order]
          :candidate-order [{:attempt-id ... :command-id ...} ...]},
   replay the v1400 abstract machine and independently check every ledger
   transition and the exact serial incorporation order. Fails on invalid
   transitions or a different claimed order.

   Output contains the PRIVATE original event trace, bindings and records so
   an independent checker can inspect and replay it. It is not an XTDB receipt."
  [source]
  (construct! source))

(defn require-current-witness!
  "Recreate the entire diagnostic from its embedded source and reject any
   stale, incomplete or altered field. Does not authenticate source origin."
  [witness]
  (when-not (and (map? witness)
                 (exact-map? witness witness-keys)
                 (= witness (construct! (:source witness))))
    (reject! :altered-or-stale-abstract-witness))
  witness)

(defn redacted-summary
  "Return only counters and assurance limits. Do not print full witnesses:
   their source trace contains command bindings, outcomes and persisted rows."
  [witness]
  (require-current-witness! witness)
  {:type :gesso.assurance.proof.abstract-serial-history-witness/summary
   :version witness-version
   :event-count (count (get-in witness [:source :events]))
   :abstract-incorporation-count (count (:serial-history witness))
   :classification (:classification witness)
   :physical-order-authenticated? false
   :physical-refinement :not-established
   :safe-to-retry? false
   :safe-to-settle? false
   :authority :none
   :certificate :not-issued
   :proof-status :replayed-abstract-witness-only})
