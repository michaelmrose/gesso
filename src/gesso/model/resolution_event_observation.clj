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
   [gesso.model.command :as command]
   [gesso.choreo.identity :as identity]
   [gesso.model.tx-model :as model]
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
                         :submitted-incorporation
                         {:plan (:plan prepared)
                          :record (:record prepared)
                          :scope resolution/assurance-scope}
                         :bridge-scope observation-scope
                         :attempt-id (:attempt-id prepared)
                         :binding binding
                         :physical {:status status
                                    :source (:physical/source outcome)
                                    :receipt (:physical/receipt outcome)})))))]
      {:observe! observe! :complete! complete!})))


;; Deliberately kept separate from the physical witness verdict. The former
;; establishes only consistency with a submitted plan; this comparison checks
;; that a *developer-authored* finite operation model prepared that same plan.
(def family-correspondence-scope
  {:checker :gesso.model.resolution-event-observation/family-preparation-v1
   :checked #{:actual-event-pair-submitted-plan
              :physical-finite-execution-witness
              :modeled-enablement-for-observed-state
              :exact-modeled-domain-plan-and-guards}
   :assumed #{:trusted-controlled-event-source
              :truthful-finite-family-callbacks
              :observed-initial-state-covers-model-dependencies
              :actual-submission-is-from-correlated-event-hook}
   :not-established #{:authenticity-of-detached-observation-maps
                      :model-coverage-of-all-application-executors
                      :complete-principal-or-argument-domains
                      :correctness-of-business-policy-or-guards
                      :behavior-of-other-reachable-states
                      :unbounded-refinement-or-certification}})

(defn- require-model-callback! [value label]
  (when-not (fn? value)
    (fail! :invalid-family-callback
           "Modeled operation callbacks must be actual functions."
           {:callback label}))
  value)

(defn compare-family-preparation
  "Compare a finite family's single state-dependent preparation with the
   EXACT domain transaction plan observed at one controlled physical attempt.

   Input: (completed-session {:enabled? fn :prepare fn} principal arguments).
   Callbacks receive (initial-state principal arguments). The initial state
   comes from XTDB before the actual transaction submission, and the actual
   plan from the same pair of v809 events accepted by start-session!/complete!.
   The single command-result create is removed *only* after verifying that the
   observed incorporation has the canonical command-record shape and binding.
   All other commands and dependency guards must match exactly after
   transaction-plan normalization.

   Returns :matched or a :counterexample with :reason, not a certificate.
   This function accepts a detached diagnostic result and cannot authenticate
   that result or prove that the supplied callback is a registered executor.
   Use only with trusted controlled sessions. No database writes occur here."
  [observation family principal arguments]
  (when-not (and (map? observation)
                 (= observation-scope (:bridge-scope observation))
                 (= :consistent (get-in observation [:evidence :status]))
                 (= :matched-serial-reference
                    (get-in observation [:evidence :reason]))
                 (= #{:enabled? :prepare} (set (keys family)))
                 (map? arguments)
                 (= arguments (get-in observation [:binding :arguments]))
                 (or (= principal (get-in observation [:binding :principal]))
                     (= principal (identity/raw-value
                                   (get-in observation [:binding :principal])))))
    (fail! :invalid-comparison
           "Require a consistent controlled observation and closed model callbacks."
           {}))
  (let [enabled? (require-model-callback! (:enabled? family) :enabled?)
        prepare (require-model-callback! (:prepare family) :prepare)
        {:keys [plan record scope]} (:submitted-incorporation observation)
        _ (when-not (= scope resolution/assurance-scope)
            (fail! :invalid-submitted-incorporation
                   "Observed plan must carry the canonical incorporation scope."
                   {}))
        _ (resolution/require-record record)
        _ (when-not (= (:gesso.command/binding record) (:binding observation))
            (fail! :incorporation-binding-mismatch
                   "Observed command-record binding does not match the physical attempt."
                   {}))
        actual (model/admitted-plan plan)
        record-commands (filterv #(= resolution/command-entity-type
                                     (first (command/target %)))
                                 (:commands actual))
        _ (when-not (and (= 1 (count record-commands))
                         (command/create? (first record-commands))
                         (= (command/after (first record-commands)) record)
                         (= [resolution/command-entity-type (:xt/id record)]
                            (command/target (first record-commands))))
            (fail! :invalid-submitted-incorporation
                   "The observed transaction must contain its exact durable record create."
                   {}))
        state (:initial-state observation)
        _ (model/require-state state)
        enabled (enabled? state principal arguments)
        _ (when-not (or (true? enabled) (false? enabled))
            (fail! :non-boolean-enablement
                   "A modeled enabled? must return a Boolean." {}))]
    (if (false? enabled)
      {:status :counterexample
       :reason :actual-execution-while-model-disabled
       :scope family-correspondence-scope}
      (let [expected (model/admitted-plan (prepare state principal arguments))
            domain (model/admitted-plan
                    (update actual :commands
                            (fn [commands]
                              (filterv #(not= resolution/command-entity-type
                                               (first (command/target %)))
                                       commands))))]
        {:status (if (= expected domain) :matched :counterexample)
         :reason (if (= expected domain)
                   :exact-observed-domain-plan-match
                   :modeled-and-submitted-plans-disagree)
         :scope family-correspondence-scope
         :physical-status (get-in observation [:physical :status])
         :matching-guards? (= (:guards expected) (:guards domain))
         :matching-commands? (= (:commands expected) (:commands domain))}))))
