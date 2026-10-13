(ns gesso.model.resolution-event-observation
  "A controlled, single-attempt bridge from the actual resolved-operation
   transaction hook to the passive XTDB finite-execution witness checker.

   Usage (trusted server/test code only):

     (let [{:keys [observe! complete!]}
           (start-session! {:node xtdb-node :binding command-binding})
           result (operation (assoc ctx
                                    resolved/observation-context-key observe!)
                             {:binding command-binding :mode :submit})
           observation (complete!)]
       ...)

   The :prepared hook occurs before tx/transact!: this bridge uses it to
   capture all initial guarded/written documents via passive/begin-observation!.
   The correlated :physical-outcome event supplies the definitive classification
   already made by resolved-operation, and complete! reads final documents
   after external execution has finished. The bridge NEVER submits a transaction.

   Only one physical submission is admitted. No event (e.g. durable replay),
   duplicate events, indeterminate outcomes, mismatched bindings, failed reads,
   or a sink error swallowed by resolved-operation prevent a witness. Such
   failures cannot affect the actual transaction or Live settlement.

   Observations are still NOT authenticated, pinned XTDB snapshots, or proof
   of business authorization, complete guards, actual submission or universal
   concurrency correctness. The hook may be copied/forged, events may be lost,
   and post-commit classification depends on the trusted Gesso execution path.
   Use solely in a controlled diagnostic window without unrelated writes."
  (:require
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-passive-observation :as passive]
   [gesso.model.resolved-operation :as resolved]))

(def ^:private request-keys #{:node :binding})
(def ^:private event-base-keys
  #{:phase :attempt-id :operation :binding :record :plan :scope})
(def ^:private outcome-keys
  (conj event-base-keys :physical/status :physical/source))

(def observation-scope
  {:bridge :gesso.model.resolution-event-observation/single-attempt-v1
   :checked #{:exact-event-pair
              :trusted-expected-binding-match
              :same-attempt-and-plan-in-both-events
              :initial-xtdb-read-before-commit-call
              :final-xtdb-read-after-physical-outcome
              :only-definitive-physical-outcomes-admitted
              :finite-reference-witness-from-collected-xtdb-documents
              :no-transaction-submission-by-observer
              :swallowed-observer-errors-fail-closed}
   :assumed #{:trusted-injection-of-canonical-execution-hook
              :event-source-is-actual-resolved-operation
              :hook-execution-precedes-physical-submission
              :physical-classification-is-truthful
              :authoritative-xtdb-node
              :no-unrelated-writes-during-observation
              :consistent-unpinned-xtdb-reads
              :synchronous-execution-completed-before-complete}
   :not-established #{:authenticated-event-or-transaction-history
                      :durable-observation-events
                      :pinned-cross-table-snapshot
                      :complete-policy-guards-or-domain-invariants
                      :genuine-application-principal-authentication
                      :correct-live-projection-or-settlement
                      :safe-retry-after-unknown-outcome
                      :multi-attempt-or-unbounded-concurrency-correctness
                      :formal-refinement-proof}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-event-observation
                   :error/kind kind
                   :error/details details})))

(defn- exact-map! [value expected kind]
  (when-not (and (map? value) (= expected (set (keys value))))
    (fail! kind "Expected a closed execution-observation map."
           {:expected expected
            :received-keys (when (map? value) (set (keys value)))}))
  value)

(defn- require-event! [event expected-binding]
  (when-not (map? event)
    (fail! :invalid-event "Observation event must be a map." {}))
  (case (:phase event)
    :prepared
    (exact-map! event event-base-keys :invalid-prepared-event)

    :physical-outcome
    (let [allowed (if (contains? event :physical/receipt)
                    (conj outcome-keys :physical/receipt)
                    outcome-keys)]
      (exact-map! event allowed :invalid-outcome-event))

    (fail! :invalid-phase "Expected a prepared or physical-outcome event."
           {:phase (:phase event)}))
  (when-not (and (= resolved/execution-observation-scope (:scope event))
                 (uuid? (:attempt-id event))
                 (= expected-binding (:binding event))
                 (= (:operation expected-binding) (:operation event))
                 (= (:gesso.command/binding
                     (resolution/require-record (:record event)))
                    expected-binding))
    (fail! :event-identity-mismatch
           "Event scope, attempt, operation, record or binding disagrees."
           {:phase (:phase event)}))
  event)

(defn- definitive-status! [event]
  ;; The source must agree with the actual v809 classified physical result.
  ;; Merely supplying :committed without a recognized source is insufficient.
  (let [status (:physical/status event)
        source (:physical/source event)]
    (case [status source]
      [:committed :gesso-model-transaction-ack] :committed
      [:committed :gesso-live-classified-post-commit-failure] :committed
      [:rejected :xtdb-explicit-assertion-failure] :rejected
      (fail! :nondefinitive-outcome
             "Only a classified committed or explicit XTDB assertion-rejected physical attempt may be checked."
             {:physical/status status :physical/source source}))))

(defn start-session!
  "Create ONE controlled physical-transaction observation session.

   Supply {:node authoritative-xtdb-node :binding canonical-command-binding}.
   Returns {:observe! fn :complete! fn}. Inject :observe! through
   resolved/observation-context-key into a trusted fresh :submit execution.
   Once the operation has returned/settled, invoke :complete! exactly once.

   If a hook callback failed, even though resolved-operation swallowed its
   exception to protect authoritative behavior, complete! WILL fail. It also
   fails on any missing, additional, mismatched or indeterminate event.
   A successful result is an inspectable bounded witness, NOT certification."
  [request]
  (exact-map! request request-keys :invalid-request)
  (let [{:keys [node binding]} request]
    (when (nil? node)
      (fail! :missing-node "A real authoritative XTDB node is required." {}))
    (resolution/require-binding binding)
    (let [state (atom {:phase :awaiting-prepared})
          observe!
          (fn [event]
            (try
              (locking state
                (let [{:keys [phase prepared capture]} @state]
                  (when (= :closed phase)
                    (fail! :session-closed "Observation session is closed." {}))
                  (when (:failure @state)
                    (fail! :session-poisoned "Observation session already failed." {}))
                  (require-event! event binding)
                  (case (:phase event)
                    :prepared
                    (do
                      (when-not (= :awaiting-prepared phase)
                        (fail! :extra-prepared-event
                               "Exactly one physical submission is supported." {}))
                      (let [incorporation {:plan (:plan event)
                                           :record (:record event)
                                           :scope resolution/assurance-scope}
                            capture (passive/begin-observation!
                                     {:node node :incorporations [incorporation]})]
                        (reset! state {:phase :awaiting-outcome
                                       :prepared event :capture capture})))
                    :physical-outcome
                    (do
                      (when-not (= :awaiting-outcome phase)
                        (fail! :unexpected-outcome
                               "Physical outcome must follow exactly one prepared event." {}))
                      (when-not (= (dissoc event :phase :physical/status
                                             :physical/source :physical/receipt)
                                   (dissoc prepared :phase))
                        (fail! :attempt-mismatch
                               "Physical outcome must match the exact submitted attempt, record and plan."
                               {}))
                      (reset! state {:phase :ready
                                     :prepared prepared :capture capture
                                     :outcome event})))))
              (catch Throwable error
                ;; Gesso deliberately isolates observer errors. Retain them
                ;; here so no missing or partial event stream can pass later.
                (locking state
                  (swap! state assoc :failure error))
                (throw error))))
          complete!
          (fn []
            (locking state
              (let [{:keys [phase failure prepared capture outcome]} @state]
                (swap! state assoc :phase :closed)
                (when failure
                  (fail! :observation-hook-failed
                         "The execution observer failed; no trustworthy event pair exists."
                         {:cause (str (class failure))}))
                (when-not (= :ready phase)
                  (fail! :incomplete-event-pair
                         "One prepared event and one physical outcome are required."
                         {:phase phase}))
                (let [status (definitive-status! outcome)
                      result (passive/complete-observation! capture [status])]
                  (assoc result
                         :bridge-scope observation-scope
                         :attempt-id (:attempt-id prepared)
                         :binding binding
                         :physical {:status status
                                    :source (:physical/source outcome)
                                    :receipt (:physical/receipt outcome)})))))]
      {:observe! observe! :complete! complete!})))
