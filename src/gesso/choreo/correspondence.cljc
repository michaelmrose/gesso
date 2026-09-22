(ns gesso.choreo.correspondence
  "Concrete behavioral correspondence checks between the executable global
   semantics and independently projected role-local realizations.

   This namespace owns witness replay. It deliberately does not own structural
   projection proof obligations, choreography verification, projection itself,
   or either semantic machine.

   The current checked properties are:

     :concrete-lockstep-realization-correspondence-v1
       One explicit schedule replayed in semantic lockstep.

     :concrete-weak-realization-correspondence-v1
       One explicit projected schedule where distributed-unobservable :local and
       :environment steps may occur before their global textual position. Those
       events are buffered and replayed against the executable global semantics
       when their semantic boundary becomes reachable. Observable authoritative
       operations and participant-message delivery remain trace ordered.

   A witness supplies one explicit schedule. The lockstep checker
   executes each semantic event against both:

     * gesso.choreo.semantics -- the executable global transition system; and
     * gesso.choreo.realization -- independently projected role-local machines.

   Physical transport operations (:send, :drop, :duplicate) advance only the
   distributed realization. Delivery is the semantic communication occurrence,
   because that is when the receiving projected machine consumes the exact
   sender-emitted envelope.

   Authoritative environment observations may carry
   :authoritative-basis-progression as witness control evidence. Correspondence
   forwards that evidence only to the projected realization; it is not part of
   the global semantic event data or distributed observable trace. Successful
   authoritative-observation obligations retain the exact progression witness
   used by that concrete step so the correspondence evidence states why a later
   authoritative basis was admissible rather than confusing witness order with
   basis order.

   The distributed observation alphabet and the hide/project relation from global
   semantic occurrences are owned by gesso.choreo.semantics. Correspondence does
   not maintain a second normalization on either side: global traces come from
   semantics/distributed-observable-trace and projected traces come from
   realization/distributed-observable-trace. Realization delegates its individual
   observation shapes back to semantics, so there remains one canonical alphabet.

   Global :terminal observations are intentionally not inserted into the concrete
   projected trace. Projected role-local completion does not itself encode the
   authored global return outcome, so treating local completion as a terminal
   observation would manufacture semantic knowledge. Instead, a complete concrete
   correspondence witness establishes terminal compatibility relationally: the
   projected execution is complete, its canonical nonterminal distributed trace
   matches one admitted global semantic execution, no semantic work remains, and
   that matched global execution supplies the authored terminal outcome. This is
   witness-level evidence only; it does not make the outcome a local-machine value
   and does not quantify over all projected executions.

   A valid result therefore establishes correspondence for one concrete witness
   only. Both checkers expose the explicit witness-level observable trace replay
   relation they checked, including the first divergence when traces disagree.
   The weak checker admits commuting hidden work, but it still checks only the
   supplied schedule. Neither checker is the all-schedules projection/refinement
   theorem, a liveness proof, or a proof that every fair transport schedule
   completes. Checker options are a closed evidence envelope so misspelled
   assumptions cannot silently change what was checked. Witness steps may carry
   additional harness/diagnostic metadata, but replay consumes only the documented
   semantic fields for each operation.

   Keeping this machinery separate from gesso.choreo.proof prevents the
   structural proof/checker namespace from becoming a second distributed runtime
   and gives later refinement work a clean boundary around concrete witnesses."
  (:require
   [gesso.choreo.machine :as machine]
   [gesso.choreo.realization :as realization]
   [gesso.choreo.semantics :as semantics]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity / result contract
;; -----------------------------------------------------------------------------

(def correspondence-version 2)

(def result-type
  :gesso.choreo.correspondence/result)

(def obligation-type
  :gesso.choreo.correspondence/obligation)

(def correspondence-property
  :concrete-lockstep-realization-correspondence-v1)

(def correspondence-classification
  :concrete-lockstep-execution-check)

(def weak-correspondence-property
  :concrete-weak-realization-correspondence-v1)

(def weak-correspondence-classification
  :concrete-weak-execution-check)

(def observable-trace-replay-relation
  :concrete-observable-trace-replay-v1)

(def terminal-compatibility-obligation
  :global-terminal-outcome-compatibility-v2)

(def terminal-compatibility-classification
  :concrete-witness-refinement-obligation)

(def correspondence-properties
  #{correspondence-property
    weak-correspondence-property})

(defn- correspondence-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.correspondence/error
      :error/kind kind}
     data))))

(defn result?
  "True when value is a correspondence checker result."
  [value]
  (and
   (map? value)
   (= result-type
      (:gesso.choreo/type value))
   (= correspondence-version
      (:gesso.choreo/version value))
   (contains? correspondence-properties
              (:property value))))

(defn valid?
  "True only for a valid correspondence result."
  [result]
  (and
   (result? result)
   (true? (:valid? result))))

(defn failures
  "Return the deterministic vector of failed obligations/counterexamples."
  [result]
  (when-not (result? result)
    (correspondence-error
     :invalid-result
     "Expected a Gesso Choreo correspondence result."
     {:value result}))
  (:failures result))

(defn first-counterexample
  "Return the first deterministic counterexample, if any."
  [result]
  (when-not (result? result)
    (correspondence-error
     :invalid-result
     "Expected a Gesso Choreo correspondence result."
     {:value result}))
  (:counterexample result))

(defn observable-trace-replay
  "Compare one realized observable trace with the concrete global trace that
   admits the replay used by a correspondence witness.

   For the currently supported deterministic witness replay, the concrete replay
   relation is exact observable-trace equality after hidden
   local/environment work has been normalized by the chosen checker. The result
   records the common prefix and first divergence so a failed witness identifies
   a useful counterexample path.

   This relation is intentionally witness-level. A valid result does NOT quantify
   over all projected schedules and is therefore not the central projection
   theorem from the v4.5 design."
  [global-admitted-trace realized-trace]
  (when-not (vector? global-admitted-trace)
    (correspondence-error
     :invalid-observable-trace
     "Global admitted observable trace must be a vector."
     {:side :global-admitted
      :trace global-admitted-trace}))

  (when-not (vector? realized-trace)
    (correspondence-error
     :invalid-observable-trace
     "Realized observable trace must be a vector."
     {:side :realized
      :trace realized-trace}))

  (let [global-count
        (count global-admitted-trace)

        realized-count
        (count realized-trace)

        limit
        (min global-count realized-count)

        divergence-index
        (loop [index 0]
          (cond
            (= index limit)
            (when-not (= global-count realized-count)
              index)

            (= (nth global-admitted-trace index)
               (nth realized-trace index))
            (recur (inc index))

            :else
            index))

        valid?
        (nil? divergence-index)

        first-divergence
        (when (some? divergence-index)
          (let [global-present?
                (< divergence-index global-count)

                realized-present?
                (< divergence-index realized-count)]
            {:index divergence-index
             :kind
             (cond
               (and global-present? realized-present?)
               :observable-value-mismatch

               global-present?
               :realization-ended-before-global-trace

               :else
               :realization-produced-extra-observation)
             :global-observation
             (when global-present?
               (nth global-admitted-trace divergence-index))
             :realized-observation
             (when realized-present?
               (nth realized-trace divergence-index))}))]

    {:relation observable-trace-replay-relation
     :valid? valid?
     :common-prefix-count
     (if valid?
       global-count
       divergence-index)
     :global-count global-count
     :realized-count realized-count
     :first-divergence first-divergence}))

;; -----------------------------------------------------------------------------
;; Concrete global/local behavioral correspondence witnesses
;; -----------------------------------------------------------------------------

(def witness-ops
  #{:local
    :authoritative
    :send
    :deliver
    :drop
    :duplicate
    :environment})

(def checker-option-keys
  #{:entry-values-by-role
    :semantic-entry-values
    :machine-options-by-role
    :require-complete?})

(defn- require-options!
  [checker-kind options]
  (when (and (some? options)
             (not (map? options)))
    (correspondence-error
     :invalid-options
     "Concrete correspondence checker options must be a map."
     {:checker checker-kind
      :options options}))

  (when (map? options)
    (let [unknown-keys
          (set
           (remove checker-option-keys
                   (keys options)))]
      (when (seq unknown-keys)
        (correspondence-error
         :unknown-option-keys
         "Concrete correspondence checker options contain unsupported keys."
         {:checker checker-kind
          :unknown-option-keys unknown-keys
          :allowed-option-keys checker-option-keys}))))

  options)

(defn- attempt
  [f]
  (try
    {:ok (f)}
    (catch #?(:clj Throwable
              :cljs :default) ex
      {:error ex})))

(defn- exception-summary
  [ex]
  {:message
   (or (ex-message ex)
       (str ex))
   :data
   (ex-data ex)})

(defn- require-witness!
  [witness]
  (when-not (vector? witness)
    (correspondence-error
     :invalid-witness
     "Concrete realization witness must be a vector of witness steps."
     {:witness witness}))

  (doseq [[index step]
          (map-indexed vector witness)]
    (when-not (map? step)
      (correspondence-error
       :invalid-witness-step
       "Concrete realization witness steps must be maps."
       {:step-index index
        :step step}))

    (when-not (contains? witness-ops
                         (:op step))
      (correspondence-error
       :unsupported-witness-op
       "Concrete realization witness contains an unsupported operation."
       {:step-index index
        :step step
        :supported witness-ops})))
  witness)

(defn- semantic-program-for-witness
  [choreography-or-verified]
  (cond
    (verify/verified? choreography-or-verified)
    (:choreography choreography-or-verified)

    (verify/verification? choreography-or-verified)
    (:choreography choreography-or-verified)

    :else
    choreography-or-verified))

(defn- derive-semantic-entry-values
  [entry-values-by-role]
  (when-not (map? entry-values-by-role)
    (correspondence-error
     :invalid-entry-values
     "Witness :entry-values-by-role must be a map."
     {:entry-values-by-role entry-values-by-role}))

  (reduce
   (fn [values role]
     (let [role-values
           (get entry-values-by-role role)]
       (when-not (map? role-values)
         (correspondence-error
          :invalid-entry-values
          "Each witness role entry-value set must be a map."
          {:role role
           :values role-values}))

       (reduce
        (fn [values key]
          (let [new-value
                (get role-values key)]
            (if (contains? values key)
              (if (= (get values key)
                     new-value)
                values
                (correspondence-error
                 :conflicting-global-entry-value
                 "Role-local witness inputs disagree on one key, so they cannot be derived into the current global semantic value store without an explicit :semantic-entry-values map."
                 {:key key
                  :existing-value (get values key)
                  :role role
                  :role-value new-value}))
              (assoc values key new-value))))
        values
        (sort-by pr-str
                 (keys role-values)))))
   {}
   (sort-by pr-str
            (keys entry-values-by-role))))

(defn- settle-semantic-branches
  [configuration]
  (loop [configuration' configuration
         branches []
         remaining 1024]
    (when (zero? remaining)
      (correspondence-error
       :semantic-branch-limit
       "Concrete witness exceeded the semantic immediate-branch limit."
       {:state
        (semantics/current-state-id
         configuration')}))

    (if (or (semantics/completed?
             configuration')
            (not= :branch
                  (:op
                   (semantics/current-state
                    configuration'))))
      {:configuration configuration'
       :branches branches}

      (let [state
            (semantics/current-state
             configuration')

            role
            (:role state)

            on
            (:on state)

            value
            (semantics/value
             configuration'
             on)

            event
            (semantics/branch-event
             role
             on
             value)]
        (recur
         (semantics/step
          configuration'
          event)
         (conj branches
               {:role role
                :on on
                :value value})
         (dec remaining))))))

(defn- semantic-projectable-trace
  "Return the portion of the canonical global distributed trace currently
   represented by projected realization history.

   :terminal is deliberately excluded here because role-local projected
   completion does not encode the authored global outcome. The corresponding
   missing refinement premise is reported explicitly by terminal-compatibility-
   report rather than being silently treated as proved."
  [configuration]
  (->> (semantics/distributed-observable-trace
        configuration)
       (remove #(= :terminal
                   (:kind %)))
       vec))

(defn- realized-observable-trace
  [realization]
  (realization/distributed-observable-trace
   realization))

(defn- terminal-compatibility-report
  [semantic realized trace-replay pending-work-count]
  (let [global-completed?
        (semantics/completed? semantic)

        realization-completed?
        (realization/completed? realized)

        trace-valid?
        (true? (:valid? trace-replay))

        global-outcome
        (semantics/outcome semantic)

        no-pending-work?
        (zero? pending-work-count)

        established?
        (and global-completed?
             realization-completed?
             trace-valid?
             no-pending-work?
             (some? global-outcome))

        status
        (cond
          (not global-completed?)
          :not-yet-applicable-to-prefix

          established?
          :established-for-concrete-witness

          (not realization-completed?)
          :projected-realization-not-complete

          (not trace-valid?)
          :observable-trace-mismatch

          (not no-pending-work?)
          :pending-semantic-work

          (nil? global-outcome)
          :missing-global-outcome

          :else
          :not-established)

        reason
        (case status
          :not-yet-applicable-to-prefix
          :global-execution-has-not-reached-terminal

          :established-for-concrete-witness
          :matched-global-replay-establishes-authored-terminal-outcome

          :projected-realization-not-complete
          :projected-realization-has-not-reached-local-completion

          :observable-trace-mismatch
          :projected-trace-does-not-match-global-replay

          :pending-semantic-work
          :semantic-replay-still-has-buffered-work

          :missing-global-outcome
          :completed-global-semantics-did-not-report-outcome

          :terminal-compatibility-not-established)]

    {:obligation terminal-compatibility-obligation
     :classification terminal-compatibility-classification
     :required-for-projection-refinement? true
     :established? established?
     :status status
     :global-completed? global-completed?
     :global-outcome global-outcome
     :realization-completed? realization-completed?
     :observable-trace-replay-valid? trace-valid?
     :pending-work-count pending-work-count
     :projected-terminal-observation nil
     :projected-terminal-outcome-encoded? false
     :outcome-source
     (when established?
       :matched-global-semantic-replay)
     :reason reason}))

(defn- witness-obligation
  [index step kind valid? data]
  (merge
   {:gesso.choreo.correspondence/type obligation-type
    :id [:realization-witness index kind]
    :property correspondence-property
    :step-index index
    :step step
    :kind kind
    :valid? (true? valid?)}
   data))

(defn- authoritative-observation-occurrences
  "Return authoritative-observation history entries added after history-count.

   One external environment event should contribute at most one such occurrence;
   the vector shape keeps this helper correct if semantic stepping also records
   branch/terminal history around that event."
  [configuration history-count]
  (->> (subvec (semantics/history configuration)
               history-count)
       (filterv #(= :authoritative-observation
                    (:kind %)))))

(defn- matching-authoritative-observation-provenance?
  [occurrence provenance]
  (and (= :authoritative
          (:kind provenance))
       (= (:authority occurrence)
          (:authority provenance))
       (= (:observation occurrence)
          (:observation provenance))
       (= (:basis occurrence)
          (:basis provenance))))

(defn- authoritative-observation-evidence
  "Compare one global authoritative-observation occurrence with the receiving
   projected role's current machine knowledge.

   Correspondence requires every declared semantic field to have the same
   value in the role-local machine and to carry authoritative provenance naming
   the same logical authority, observation identity, and opaque basis. Runtime
   state locators are deliberately not compared with semantic state ids."
  [realized occurrence]
  (let [role
        (:role occurrence)

        execution
        (realization/execution realized role)

        semantic-data
        (:data occurrence)

        checked-keys
        (set (keys semantic-data))

        runtime-values
        (when execution
          (into
           {}
           (map
            (fn [key]
              [key
               (machine/execution-value execution key)]))
           (sort-by pr-str checked-keys)))

        matching-provenance-by-key
        (when execution
          (into
           {}
           (map
            (fn [key]
              [key
               (first
                (filter
                 #(matching-authoritative-observation-provenance?
                   occurrence
                   %)
                 (or
                  (machine/execution-provenance execution key)
                  [])))])
            (sort-by pr-str checked-keys))))

        values-valid?
        (and execution
             (= semantic-data
                runtime-values))

        provenance-valid?
        (and execution
             (every?
              some?
              (vals matching-provenance-by-key)))]

    {:valid?
     (and values-valid?
          provenance-valid?)

     :semantic-occurrence
     occurrence

     :checked-keys
     checked-keys

     :basis
     (:basis occurrence)

     :runtime-values
     runtime-values

     :matching-provenance-by-key
     matching-provenance-by-key

     :values-valid?
     (true? values-valid?)

     :provenance-valid?
     (true? provenance-valid?)}))

(defn- append-authoritative-observation-obligations
  [state index step occurrences obligation-fn]
  (reduce
   (fn [state occurrence]
     (let [evidence
           (cond->
            (authoritative-observation-evidence
             (:realization state)
             occurrence)
            (contains? step
                       :authoritative-basis-progression)
            (assoc
             :basis-progression
             (:authoritative-basis-progression step)))]
       (update
        state
        :obligations
        conj
        (obligation-fn
         index
         step
         :authoritative-observation-correspondence
         (:valid? evidence)
         (dissoc evidence :valid?)))))
   state
   occurrences))

(defn- resolve-message-id
  [message-refs message]
  (cond
    (and (integer? message)
         (not (neg? message)))
    message

    (contains? message-refs message)
    (get message-refs message)

    :else
    (correspondence-error
     :unknown-witness-message
     "Witness message reference is neither a non-negative message id nor a bound witness message label."
     {:message message
      :known-message-labels
      (set (keys message-refs))})))

(defn- bind-message-ref
  [message-refs label message-id]
  (if (nil? label)
    message-refs
    (do
      (when (contains? message-refs label)
        (correspondence-error
         :duplicate-witness-message-label
         "Concrete witness message labels must be unique."
         {:label label
          :existing-message-id
          (get message-refs label)
          :new-message-id message-id}))
      (assoc message-refs label message-id))))

(defn- expected-message-fields
  [step]
  (select-keys
   step
   [:to :event :via]))

(defn- actual-message-fields
  [message]
  (select-keys
   message
   [:to :event :via]))

(defn- realize-environment-step
  "Apply one witness environment step to the projected realization.

   :authoritative-basis-progression is control evidence for the machine's
   authoritative knowledge transition. It is deliberately forwarded through
   realization options rather than merged into semantic event :data. Witnesses
   without that field retain the ordinary four-argument environment path."
  [realized step]
  (let [role (:role step)
        event (:event step)
        data (:data step)]
    (if (contains? step
                   :authoritative-basis-progression)
      (realization/environment
       realized
       role
       event
       data
       {:authoritative-basis-progression
        (:authoritative-basis-progression step)})
      (realization/environment
       realized
       role
       event
       data))))

(defn- paired-semantic-realization-step
  [state index step semantic-event realization-f]
  (let [history-count
        (count
         (semantics/history
          (:semantic state)))

        semantic-attempt
        (attempt
         #(let [{:keys [configuration branches]}
                (settle-semantic-branches
                 (semantics/step
                  (:semantic state)
                  semantic-event))]
            {:configuration configuration
             :branches branches
             :authoritative-observations
             (authoritative-observation-occurrences
              configuration
              history-count)}))

        realization-attempt
        (attempt realization-f)

        semantic-ok?
        (contains? semantic-attempt :ok)

        realization-ok?
        (contains? realization-attempt :ok)]

    (if (and semantic-ok?
             realization-ok?)
      (let [next-state
            (-> state
                (assoc :semantic
                       (get-in semantic-attempt
                               [:ok :configuration]))
                (assoc :realization
                       (:ok realization-attempt))
                (update :obligations
                        conj
                        (witness-obligation
                         index
                         step
                         :paired-semantic-step
                         true
                         {:semantic-event semantic-event
                          :auto-branches
                          (get-in semantic-attempt
                                  [:ok :branches])})))

            next-state'
            (append-authoritative-observation-obligations
             next-state
             index
             step
             (get-in semantic-attempt
                     [:ok :authoritative-observations])
             witness-obligation)]
        {:ok next-state'})

      {:error
       {:kind
        (cond
          (and semantic-ok?
               (not realization-ok?))
          :realization-rejected-semantic-step

          (and realization-ok?
               (not semantic-ok?))
          :semantic-rejected-realization-step

          :else
          :witness-step-rejected)

        :step-index index
        :step step
        :semantic-event semantic-event
        :semantic-error
        (when-let [ex (:error semantic-attempt)]
          (exception-summary ex))
        :realization-error
        (when-let [ex (:error realization-attempt)]
          (exception-summary ex))}})))

(defn- run-witness-step
  [state index step]
  (let [op
        (:op step)]
    (case op
      :local
      (let [role (:role step)
            action (:action step)
            outputs (or (:outputs step) {})]
        (paired-semantic-realization-step
         state
         index
         step
         (semantics/local-event
          role
          action
          outputs)
         #(realization/complete-local
           (:realization state)
           role
           outputs)))

      :authoritative
      (let [role (:role step)
            operation (:operation step)
            outputs (or (:outputs step) {})]
        (paired-semantic-realization-step
         state
         index
         step
         (semantics/authoritative-event
          role
          operation
          outputs)
         #(realization/complete-authoritative
           (:realization state)
           role
           outputs)))

      :environment
      (let [role (:role step)
            event (:event step)
            data (:data step)]
        (paired-semantic-realization-step
         state
         index
         step
         (semantics/environment-event
          role
          event
          data)
         #(realize-environment-step
           (:realization state)
           step)))

      :send
      (let [attempted
            (attempt
             #(realization/complete-send
               (:realization state)
               (:role step)
               (or (:payload step) {})))]
        (if-let [ex (:error attempted)]
          {:error
           {:kind :witness-send-rejected
            :step-index index
            :step step
            :realization-error
            (exception-summary ex)}}

          (let [{next-realization :realization
                 message-id :message-id
                 message :message}
                (:ok attempted)

                expected
                (expected-message-fields step)

                actual
                (actual-message-fields message)

                identity-valid?
                (= expected
                   (select-keys actual
                                (keys expected)))]
            (if-not identity-valid?
              {:error
               {:kind :witness-send-identity-mismatch
                :step-index index
                :step step
                :expected expected
                :actual actual
                :message message}}

              {:ok
               (-> state
                   (assoc :realization
                          next-realization)
                   (assoc :message-refs
                          (bind-message-ref
                           (:message-refs state)
                           (:as step)
                           message-id))
                   (update :obligations
                           conj
                           (witness-obligation
                            index
                            step
                            :transport-send
                            true
                            {:message-id message-id
                             :message message})))}))))

      :deliver
      (let [resolution
            (attempt
             #(let [message-id
                    (resolve-message-id
                     (:message-refs state)
                     (:message step))

                    queue-entry
                    (or
                     (realization/queued-message
                      (:realization state)
                      message-id)
                     (correspondence-error
                      :unknown-witness-message
                      "Witness delivery names a message that is not currently queued."
                      {:message-id message-id}))

                    message
                    (:message queue-entry)]
                {:message-id message-id
                 :message message}))]
        (if-let [ex (:error resolution)]
          {:error
           {:kind :witness-delivery-resolution-failed
            :step-index index
            :step step
            :error (exception-summary ex)}}

          (let [{:keys [message-id message]}
                (:ok resolution)

                semantic-event
                (semantics/communication-event
                 (:from message)
                 (:to message)
                 (:event message)
                 (:payload message)
                 (when (contains? message :via)
                   {:via (:via message)}))]
            (paired-semantic-realization-step
             state
             index
             step
             semantic-event
             #(realization/deliver-message
               (:realization state)
               message-id)))))

      :drop
      (let [attempted
            (attempt
             #(let [message-id
                    (resolve-message-id
                     (:message-refs state)
                     (:message step))]
                {:message-id message-id
                 :realization
                 (realization/drop-message
                  (:realization state)
                  message-id)}))]
        (if-let [ex (:error attempted)]
          {:error
           {:kind :witness-drop-rejected
            :step-index index
            :step step
            :error (exception-summary ex)}}

          {:ok
           (-> state
               (assoc :realization
                      (get-in attempted
                              [:ok :realization]))
               (update :obligations
                       conj
                       (witness-obligation
                        index
                        step
                        :transport-drop
                        true
                        {:message-id
                         (get-in attempted
                                 [:ok :message-id])})))}))

      :duplicate
      (let [attempted
            (attempt
             #(let [source-id
                    (resolve-message-id
                     (:message-refs state)
                     (:message step))

                    duplicate-result
                    (realization/duplicate-message
                     (:realization state)
                     source-id)]
                (assoc duplicate-result
                       :source-message-id
                       source-id)))]
        (if-let [ex (:error attempted)]
          {:error
           {:kind :witness-duplicate-rejected
            :step-index index
            :step step
            :error (exception-summary ex)}}

          (let [{next-realization :realization
                 message-id :message-id
                 message :message
                 source-message-id :source-message-id}
                (:ok attempted)]
            {:ok
             (-> state
                 (assoc :realization
                        next-realization)
                 (assoc :message-refs
                        (bind-message-ref
                         (:message-refs state)
                         (:as step)
                         message-id))
                 (update :obligations
                         conj
                         (witness-obligation
                          index
                          step
                          :transport-duplicate
                          true
                          {:source-message-id source-message-id
                           :message-id message-id
                           :message message})))})))

      {:error
       {:kind :unsupported-witness-op
        :step-index index
        :step step}})))

(defn- witness-failure-result
  [witness obligations counterexample]
  {:gesso.choreo/type result-type
   :gesso.choreo/version correspondence-version
   :property correspondence-property
   :classification correspondence-classification
   :valid? false
   :scope
   {:kind :concrete-lockstep-witness
    :witness-step-count (count witness)
    :checked-step-count (count obligations)}
   :obligations (vec obligations)
   :failures [counterexample]
   :counterexample counterexample})

(defn- witness-success-result
  [witness state require-complete?]
  (let [semantic
        (:semantic state)

        realized
        (:realization state)

        semantic-trace
        (semantic-projectable-trace semantic)

        realized-trace
        (realized-observable-trace realized)

        trace-replay
        (observable-trace-replay
         semantic-trace
         realized-trace)

        trace-valid?
        (:valid? trace-replay)

        terminal-compatibility
        (terminal-compatibility-report
         semantic
         realized
         trace-replay
         0)

        trace-obligation
        (witness-obligation
         (count witness)
         nil
         :observable-trace
         trace-valid?
         {:expected semantic-trace
          :actual realized-trace
          :replay trace-replay})

        completion-valid?
        (or
         (not require-complete?)
         (and
          (semantics/completed? semantic)
          (realization/completed? realized)))

        completion-obligation
        (witness-obligation
         (inc (count witness))
         nil
         :completion
         completion-valid?
         {:required? require-complete?
          :global-completed?
          (semantics/completed? semantic)
          :realization-completed?
          (realization/completed? realized)
          :global-outcome
          (semantics/outcome semantic)})

        terminal-obligation
        (witness-obligation
         (+ 2 (count witness))
         nil
         :terminal-compatibility
         (or
          (not require-complete?)
          (:established? terminal-compatibility))
         {:required? require-complete?
          :terminal-compatibility terminal-compatibility})

        obligations
        (conj
         (vec (:obligations state))
         trace-obligation
         completion-obligation
         terminal-obligation)

        failures'
        (vec
         (remove :valid?
                 obligations))]

    {:gesso.choreo/type result-type
     :gesso.choreo/version correspondence-version
     :property correspondence-property
     :classification correspondence-classification
     :valid? (empty? failures')
     :scope
     {:kind :concrete-lockstep-witness
      :witness-step-count (count witness)
      :checked-step-count (count (:obligations state))
      :require-complete? require-complete?}
     :obligations obligations
     :failures failures'
     :counterexample (first failures')
     :observable-trace-replay trace-replay
     :terminal-compatibility terminal-compatibility
     :semantic-trace semantic-trace
     :realization-trace realized-trace
     :global-completed?
     (semantics/completed? semantic)
     :realization-completed?
     (realization/completed? realized)
     :global-outcome
     (semantics/outcome semantic)}))

(defn check-witness
  "Check one concrete lockstep execution witness against both global semantics
   and independently projected role machines.

   This is the first behavioral correspondence checker. Complete witnesses also
   establish terminal-outcome compatibility relationally with the matched global
   replay; role-local completion still does not encode that outcome. This checker
   remains deliberately NOT the general projection/refinement theorem.

   A witness is a vector containing explicit operations such as:

     {:op :local
      :role :browser
      :action :prepare
      :outputs {:request-id 17}}

     {:op :send
      :role :browser
      :event :request/claim
      :payload {:request-id 17}
      :as :command}

     {:op :deliver
      :message :command}

   :send, :drop, and :duplicate are physical realization steps and do not move
   the atomic global communication semantics. A :deliver is the semantic
   communication occurrence because that is when the receiver actually consumes
   the sender-emitted envelope.

   Deterministic global :branch states are traversed automatically after each
   semantic witness event, matching the portable machine's immediate branch
   behavior.

   Options:

     :entry-values-by-role
       Concrete role-local values supplied to realization/start.

     :semantic-entry-values
       Optional explicit initial global semantic value map. When omitted it is
       derived from role-local inputs only if equal values are used for every
       repeated key.

     :machine-options-by-role
       Passed through to realization/start.

     :require-complete?
       When true, both the global semantics and independent realization must be
       complete after the witness. Defaults to false so prefixes can be checked.

   A valid result proves only that THIS concrete, globally replayable lockstep
   witness has matching semantic observations and accepted boundary behavior.
   It says nothing yet about all possible projected schedules, commuting hidden
   local steps, liveness, or the full trace-refinement theorem."
  ([choreography-or-verified witness]
   (check-witness
    choreography-or-verified
    witness
    nil))
  ([choreography-or-verified
    witness
    {:keys [entry-values-by-role
            semantic-entry-values
            machine-options-by-role
            require-complete?]
     :or {entry-values-by-role {}
          machine-options-by-role {}
          require-complete? false}
     :as options}]
   (require-options!
    :lockstep
    options)

   (when-not (boolean? require-complete?)
     (correspondence-error
      :invalid-options
      "Concrete witness :require-complete? must be boolean."
      {:require-complete? require-complete?}))

   (require-witness! witness)

   (let [construction
         (attempt
          #(let [semantic-values
                 (if (some? semantic-entry-values)
                   (do
                     (when-not (map? semantic-entry-values)
                       (correspondence-error
                        :invalid-entry-values
                        "Witness :semantic-entry-values must be a map."
                        {:semantic-entry-values
                         semantic-entry-values}))
                     semantic-entry-values)
                   (derive-semantic-entry-values
                    entry-values-by-role))

                 semantic0
                 (semantics/start
                  (semantic-program-for-witness
                   choreography-or-verified)
                  {:values semantic-values})

                 {:keys [configuration branches]}
                 (settle-semantic-branches
                  semantic0)

                 realized0
                 (realization/start
                  choreography-or-verified
                  {:entry-values-by-role
                   entry-values-by-role
                   :machine-options-by-role
                   machine-options-by-role})]
             {:semantic configuration
              :realization realized0
              :message-refs {}
              :obligations
              (if (seq branches)
                [(witness-obligation
                  -1
                  nil
                  :initial-branches
                  true
                  {:auto-branches branches})]
                [])}))]

     (if-let [ex (:error construction)]
       (witness-failure-result
        witness
        []
        {:kind :construction-failure
         :phase :witness-start
         :message
         (or (ex-message ex)
             (str ex))
         :data (ex-data ex)})

       (loop [index 0
              state (:ok construction)]
         (if (= index
                (count witness))
           (witness-success-result
            witness
            state
            require-complete?)

           (let [step
                 (nth witness index)

                 attempted-step
                 (attempt
                  #(run-witness-step
                    state
                    index
                    step))]
             (if-let [ex (:error attempted-step)]
               (witness-failure-result
                witness
                (:obligations state)
                {:kind :witness-step-construction-failure
                 :step-index index
                 :step step
                 :message
                 (or (ex-message ex)
                     (str ex))
                 :data (ex-data ex)})

               (let [step-result
                     (:ok attempted-step)]
                 (if-let [counterexample
                          (:error step-result)]
                   (witness-failure-result
                    witness
                    (:obligations state)
                    counterexample)

                   (recur
                    (inc index)
                    (:ok step-result))))))))))))


;; -----------------------------------------------------------------------------
;; Concrete weak correspondence witnesses
;; -----------------------------------------------------------------------------

(defn- weak-witness-obligation
  [index step kind valid? data]
  (merge
   {:gesso.choreo.correspondence/type obligation-type
    :id [:weak-realization-witness index kind]
    :property weak-correspondence-property
    :step-index index
    :step step
    :kind kind
    :valid? (true? valid?)}
   data))

(defn- remove-vector-index
  [values index]
  (into []
        (concat
         (subvec values 0 index)
         (subvec values (inc index)))))

(defn- first-enabled-pending-index
  [configuration pending]
  (first
   (keep-indexed
    (fn [index {:keys [semantic-event]}]
      (when (semantics/enabled?
             configuration
             semantic-event)
        index))
    pending)))

(defn- first-pending-observable-enabled?
  [configuration pending-observable]
  (when-let [{:keys [semantic-event]}
             (first pending-observable)]
    (semantics/enabled?
     configuration
     semantic-event)))

(defn- settle-weak-semantic
  "Advance deterministic global branches and replay already-observed distributed
   events whenever the executable global semantics reaches their boundary.

   Distributed-unobservable :local/:environment events may be replayed in any
   witness order consistent with global enablement.

   Observable authoritative/communication events are different: the projected
   realization may execute one before an earlier foreign hidden step has occurred
   in the witness, but observable events themselves never reorder. They are kept
   in one FIFO :pending-observable queue and replayed against global semantics
   only when the queue head becomes enabled.

   This lets an independently projected authority legitimately run before an
   unrelated foreign local without inventing outputs for that not-yet-observed
   local step. Once the hidden step actually appears in the witness, semantic
   replay can linearize the two events while preserving the realization's
   observable order."
  [state]
  (loop [state' state
         remaining 2048]
    (when (zero? remaining)
      (correspondence-error
       :weak-semantic-settle-limit
       "Concrete weak witness exceeded the semantic settle limit."
       {:state
        (semantics/current-state-id
         (:semantic state'))
        :pending-unobservable-count
        (count (:pending-unobservable state'))
        :pending-observable-count
        (count (:pending-observable state'))}))

    (let [{settled :configuration
           branches :branches}
          (settle-semantic-branches
           (:semantic state'))

          state''
          (cond->
           (assoc state' :semantic settled)
           (seq branches)
           (update :obligations
                   conj
                   (weak-witness-obligation
                    -1
                    nil
                    :auto-branches
                    true
                    {:auto-branches branches})))

          pending-unobservable
          (:pending-unobservable state'')

          pending-observable
          (:pending-observable state'')]

      (cond
        (semantics/completed? settled)
        state''

        (first-pending-observable-enabled?
         settled
         pending-observable)
        (let [{:keys [step-index
                      step
                      semantic-event]}
              (first pending-observable)

              history-count
              (count
               (semantics/history settled))

              next-semantic
              (semantics/step
               settled
               semantic-event)

              occurrences
              (authoritative-observation-occurrences
               next-semantic
               history-count)

              next-state
              (-> state''
                  (assoc :semantic next-semantic)
                  (assoc :pending-observable
                         (subvec pending-observable 1))
                  (update :obligations
                          conj
                          (weak-witness-obligation
                           step-index
                           step
                           :replayed-observable
                           true
                           {:semantic-event semantic-event})))

              next-state'
              (append-authoritative-observation-obligations
               next-state
               step-index
               step
               occurrences
               weak-witness-obligation)]
          (recur
           next-state'
           (dec remaining)))

        :else
        (if-some [pending-index
                  (first-enabled-pending-index
                   settled
                   pending-unobservable)]
          (let [{:keys [step-index
                        step
                        semantic-event]}
                (nth pending-unobservable pending-index)

                history-count
                (count
                 (semantics/history settled))

                next-semantic
                (semantics/step
                 settled
                 semantic-event)

                occurrences
                (authoritative-observation-occurrences
                 next-semantic
                 history-count)

                next-state
                (-> state''
                    (assoc :semantic next-semantic)
                    (assoc :pending-unobservable
                           (remove-vector-index
                            pending-unobservable
                            pending-index))
                    (update :obligations
                            conj
                            (weak-witness-obligation
                             step-index
                             step
                             :replayed-unobservable
                             true
                             {:semantic-event semantic-event
                              :buffer-position pending-index})))

                next-state'
                (append-authoritative-observation-obligations
                 next-state
                 step-index
                 step
                 occurrences
                 weak-witness-obligation)]
            (recur
             next-state'
             (dec remaining)))

          state'')))))

(defn- realization-boundary-identity
  [realized role]
  (let [boundary
        (or (realization/boundary realized role)
            {})]
    (cond->
     (select-keys
      boundary
      [:action :operation :to :event :via])

      (contains? boundary :kind)
      (assoc :op (:kind boundary)))))

(defn- weak-buffer-unobservable
  [state index step semantic-event realization-f expected-boundary]
  (let [actual-boundary
        (realization-boundary-identity
         (:realization state)
         (:role step))]
    (if (not= expected-boundary
              (select-keys actual-boundary
                           (keys expected-boundary)))
      {:error
       {:kind :weak-realization-boundary-mismatch
        :step-index index
        :step step
        :expected expected-boundary
        :actual actual-boundary}}

      (let [realization-attempt
            (attempt realization-f)]
        (if-let [ex (:error realization-attempt)]
          {:error
           {:kind :weak-realization-rejected-unobservable-step
            :step-index index
            :step step
            :semantic-event semantic-event
            :realization-error
            (exception-summary ex)}}

          {:ok
           (settle-weak-semantic
            (-> state
                (assoc :realization
                       (:ok realization-attempt))
                (update :pending-unobservable
                        conj
                        {:step-index index
                         :step step
                         :semantic-event semantic-event})
                (update :obligations
                        conj
                        (weak-witness-obligation
                         index
                         step
                         :distributed-unobservable-step
                         true
                         {:semantic-event semantic-event}))))})))))

(defn- weak-buffer-observable
  [state index step semantic-event realization-f expected-boundary]
  (let [settled-state
        (settle-weak-semantic state)

        actual-boundary
        (when expected-boundary
          (realization-boundary-identity
           (:realization settled-state)
           (:role step)))]

    (if (and expected-boundary
             (not= expected-boundary
                   (select-keys actual-boundary
                                (keys expected-boundary))))
      {:error
       {:kind :weak-realization-boundary-mismatch
        :step-index index
        :step step
        :expected expected-boundary
        :actual actual-boundary}}

      (let [realization-attempt
            (attempt realization-f)]

        (if-let [ex (:error realization-attempt)]
          {:error
           {:kind :realization-rejected-semantic-step
            :step-index index
            :step step
            :semantic-event semantic-event
            :pending-unobservable
            (:pending-unobservable settled-state)
            :pending-observable
            (:pending-observable settled-state)
            :realization-error
            (exception-summary ex)}}

          {:ok
           (settle-weak-semantic
            (-> settled-state
                (assoc :realization
                       (:ok realization-attempt))
                (update :pending-observable
                        conj
                        {:step-index index
                         :step step
                         :semantic-event semantic-event})
                (update :obligations
                        conj
                        (weak-witness-obligation
                         index
                         step
                         :distributed-observable-step
                         true
                         {:semantic-event semantic-event}))))})))))

(defn- run-weak-witness-step
  [state index step]
  (case (:op step)
    :local
    (let [role (:role step)
          action (:action step)
          outputs (or (:outputs step) {})]
      (weak-buffer-unobservable
       state
       index
       step
       (semantics/local-event
        role
        action
        outputs)
       #(realization/complete-local
         (:realization state)
         role
         outputs)
       {:op :local
        :action action}))

    :environment
    (let [role (:role step)
          event (:event step)
          data (:data step)
          realization-attempt
          (attempt
           #(realize-environment-step
             (:realization state)
             step))]
      ;; Environment waits are suspension points rather than realization/boundary
      ;; endpoint actions, so the projected machine itself performs the identity
      ;; and contract check.
      (if-let [ex (:error realization-attempt)]
        {:error
         {:kind :weak-realization-rejected-unobservable-step
          :step-index index
          :step step
          :semantic-event
          (semantics/environment-event
           role event data)
          :realization-error
          (exception-summary ex)}}

        {:ok
         (settle-weak-semantic
          (-> state
              (assoc :realization
                     (:ok realization-attempt))
              (update :pending-unobservable
                      conj
                      {:step-index index
                       :step step
                       :semantic-event
                       (semantics/environment-event
                        role event data)})
              (update :obligations
                      conj
                      (weak-witness-obligation
                       index
                       step
                       :distributed-unobservable-step
                       true
                       {:semantic-event
                        (semantics/environment-event
                         role event data)}))))}))

    :authoritative
    (let [role (:role step)
          operation (:operation step)
          outputs (or (:outputs step) {})]
      (weak-buffer-observable
       state
       index
       step
       (semantics/authoritative-event
        role
        operation
        outputs)
       #(realization/complete-authoritative
         (:realization state)
         role
         outputs)
       {:op :authoritative
        :operation operation}))

    :send
    ;; Physical send is deliberately not a global semantic occurrence.
    (run-witness-step state index step)

    :drop
    (run-witness-step state index step)

    :duplicate
    (run-witness-step state index step)

    :deliver
    (let [settled-state
          (settle-weak-semantic state)

          resolution
          (attempt
           #(let [message-id
                  (resolve-message-id
                   (:message-refs settled-state)
                   (:message step))

                  queue-entry
                  (or
                   (realization/queued-message
                    (:realization settled-state)
                    message-id)
                   (correspondence-error
                    :unknown-witness-message
                    "Weak witness delivery names a message that is not currently queued."
                    {:message-id message-id}))

                  message
                  (:message queue-entry)]
              {:message-id message-id
               :message message}))]

      (if-let [ex (:error resolution)]
        {:error
         {:kind :witness-delivery-resolution-failed
          :step-index index
          :step step
          :error (exception-summary ex)}}

        (let [{:keys [message-id message]}
              (:ok resolution)

              semantic-event
              (semantics/communication-event
               (:from message)
               (:to message)
               (:event message)
               (:payload message)
               (when (contains? message :via)
                 {:via (:via message)}))]
          (weak-buffer-observable
           settled-state
           index
           step
           semantic-event
           #(realization/deliver-message
             (:realization settled-state)
             message-id)
           nil))))

    {:error
     {:kind :unsupported-witness-op
      :step-index index
      :step step}}))

(defn- pending-observable-trace
  [pending-observable]
  (mapv
   (fn [entry]
     (let [semantic-event
           (:semantic-event entry)

           observation
           (semantics/distributed-observation
            semantic-event)]
       (when-not observation
         (correspondence-error
          :pending-observable-became-hidden
          "Pending observable semantic event projected to no distributed observation."
          {:entry entry
           :semantic-event semantic-event}))
       observation))
   pending-observable))

(defn- weak-witness-failure-result
  [witness obligations pending-unobservable pending-observable counterexample]
  {:gesso.choreo/type result-type
   :gesso.choreo/version correspondence-version
   :property weak-correspondence-property
   :classification weak-correspondence-classification
   :valid? false
   :scope
   {:kind :concrete-weak-witness
    :witness-step-count (count witness)
    :checked-step-count (count obligations)
    :pending-unobservable-count
    (count pending-unobservable)
    :pending-observable-count
    (count pending-observable)}
   :obligations (vec obligations)
   :failures [counterexample]
   :counterexample counterexample
   :pending-unobservable
   (vec pending-unobservable)
   :pending-observable
   (vec pending-observable)})

(defn- weak-witness-success-result
  [witness state require-complete?]
  (let [settled-state
        (settle-weak-semantic state)

        semantic
        (:semantic settled-state)

        realized
        (:realization settled-state)

        pending-unobservable
        (:pending-unobservable settled-state)

        pending-observable
        (:pending-observable settled-state)

        semantic-trace
        (semantic-projectable-trace semantic)

        pending-observable-trace'
        (pending-observable-trace
         pending-observable)

        realized-trace
        (realized-observable-trace realized)

        expected-realized-trace
        (into semantic-trace
              pending-observable-trace')

        trace-replay
        (observable-trace-replay
         expected-realized-trace
         realized-trace)

        trace-valid?
        (:valid? trace-replay)

        pending-work-count
        (+ (count pending-unobservable)
           (count pending-observable))

        terminal-compatibility
        (terminal-compatibility-report
         semantic
         realized
         trace-replay
         pending-work-count)

        pending-unobservable-valid?
        (or
         (empty? pending-unobservable)
         (not
          (semantics/completed?
           semantic)))

        pending-observable-valid?
        (or
         (empty? pending-observable)
         (not
          (semantics/completed?
           semantic)))

        completion-valid?
        (or
         (not require-complete?)
         (and
          (empty? pending-unobservable)
          (empty? pending-observable)
          (semantics/completed? semantic)
          (realization/completed? realized)))

        obligations
        (-> (vec (:obligations settled-state))
            (conj
             (weak-witness-obligation
              (count witness)
              nil
              :observable-trace
              trace-valid?
              {:semantic-prefix semantic-trace
               :pending-observable-trace
               pending-observable-trace'
               :expected expected-realized-trace
               :actual realized-trace
               :replay trace-replay}))
            (conj
             (weak-witness-obligation
              (inc (count witness))
              nil
              :pending-unobservable
              pending-unobservable-valid?
              {:pending pending-unobservable
               :global-completed?
               (semantics/completed? semantic)}))
            (conj
             (weak-witness-obligation
              (+ 2 (count witness))
              nil
              :pending-observable
              pending-observable-valid?
              {:pending pending-observable
               :pending-trace
               pending-observable-trace'
               :global-completed?
               (semantics/completed? semantic)}))
            (conj
             (weak-witness-obligation
              (+ 3 (count witness))
              nil
              :completion
              completion-valid?
              {:required? require-complete?
               :pending-unobservable-count
               (count pending-unobservable)
               :pending-observable-count
               (count pending-observable)
               ;; Retain the older aggregate field for diagnostics/tests that
               ;; only need to know whether semantic work remains.
               :pending-count
               pending-work-count
               :global-completed?
               (semantics/completed? semantic)
               :realization-completed?
               (realization/completed? realized)
               :global-outcome
               (semantics/outcome semantic)}))
            (conj
             (weak-witness-obligation
              (+ 4 (count witness))
              nil
              :terminal-compatibility
              (or
               (not require-complete?)
               (:established? terminal-compatibility))
              {:required? require-complete?
               :terminal-compatibility terminal-compatibility})))

        failures'
        (vec
         (remove :valid?
                 obligations))]

    {:gesso.choreo/type result-type
     :gesso.choreo/version correspondence-version
     :property weak-correspondence-property
     :classification weak-correspondence-classification
     :valid? (empty? failures')
     :scope
     {:kind :concrete-weak-witness
      :witness-step-count (count witness)
      :checked-step-count
      (count (:obligations settled-state))
      :require-complete? require-complete?
      :pending-unobservable-count
      (count pending-unobservable)
      :pending-observable-count
      (count pending-observable)}
     :obligations obligations
     :failures failures'
     :counterexample (first failures')
     :pending-unobservable
     (vec pending-unobservable)
     :pending-observable
     (vec pending-observable)
     :observable-trace-replay trace-replay
     :terminal-compatibility terminal-compatibility
     :semantic-trace semantic-trace
     :pending-observable-trace
     pending-observable-trace'
     :realization-trace realized-trace
     :global-completed?
     (semantics/completed? semantic)
     :realization-completed?
     (realization/completed? realized)
     :global-outcome
     (semantics/outcome semantic)}))

(defn check-weak-witness
  "Check one concrete projected schedule while permitting distributed-hidden
   :local and :environment events to commute ahead of their global textual
   position.

   The independently projected realization always executes the witness in the
   supplied order. Global semantics executes authoritative results and delivered
   participant messages in that same observable order, while local/environment
   events may be buffered and replayed when their matching global state becomes
   reachable. Deterministic global branches are settled automatically.

   Complete weak witnesses establish terminal-outcome compatibility relationally
   with their fully settled matched global replay, while local role completion
   remains outcome-agnostic.

   This closes an important gap in the lockstep checker: projection is allowed
   to skip foreign local work, so an independently projected role may legally do
   hidden work earlier than the sequential global program writes it. Rejecting
   such a schedule merely because the global interpreter has not reached that
   local state would be a false counterexample.

   Options are the same as check-witness. A valid result still covers only THIS
   explicit schedule; it is not an exhaustive schedule search or the general
   trace-refinement theorem."
  ([choreography-or-verified witness]
   (check-weak-witness
    choreography-or-verified
    witness
    nil))
  ([choreography-or-verified
    witness
    {:keys [entry-values-by-role
            semantic-entry-values
            machine-options-by-role
            require-complete?]
     :or {entry-values-by-role {}
          machine-options-by-role {}
          require-complete? false}
     :as options}]
   (require-options!
    :weak
    options)

   (when-not (boolean? require-complete?)
     (correspondence-error
      :invalid-options
      "Concrete weak witness :require-complete? must be boolean."
      {:require-complete? require-complete?}))

   (require-witness! witness)

   (let [construction
         (attempt
          #(let [semantic-values
                 (if (some? semantic-entry-values)
                   (do
                     (when-not (map? semantic-entry-values)
                       (correspondence-error
                        :invalid-entry-values
                        "Weak witness :semantic-entry-values must be a map."
                        {:semantic-entry-values
                         semantic-entry-values}))
                     semantic-entry-values)
                   (derive-semantic-entry-values
                    entry-values-by-role))

                 semantic0
                 (semantics/start
                  (semantic-program-for-witness
                   choreography-or-verified)
                  {:values semantic-values})

                 {:keys [configuration branches]}
                 (settle-semantic-branches
                  semantic0)

                 realized0
                 (realization/start
                  choreography-or-verified
                  {:entry-values-by-role
                   entry-values-by-role
                   :machine-options-by-role
                   machine-options-by-role})]
             {:semantic configuration
              :realization realized0
              :message-refs {}
              :pending-unobservable []
              :pending-observable []
              :obligations
              (if (seq branches)
                [(weak-witness-obligation
                  -1
                  nil
                  :initial-branches
                  true
                  {:auto-branches branches})]
                [])}))]

     (if-let [ex (:error construction)]
       (weak-witness-failure-result
        witness
        []
        []
        []
        {:kind :construction-failure
         :phase :witness-start
         :message
         (or (ex-message ex)
             (str ex))
         :data (ex-data ex)})

       (loop [index 0
              state (:ok construction)]
         (if (= index
                (count witness))
           (weak-witness-success-result
            witness
            state
            require-complete?)

           (let [step
                 (nth witness index)

                 attempted-step
                 (attempt
                  #(run-weak-witness-step
                    state
                    index
                    step))]
             (if-let [ex (:error attempted-step)]
               (weak-witness-failure-result
                witness
                (:obligations state)
                (:pending-unobservable state)
                (:pending-observable state)
                {:kind :witness-step-construction-failure
                 :step-index index
                 :step step
                 :message
                 (or (ex-message ex)
                     (str ex))
                 :data (ex-data ex)})

               (let [step-result
                     (:ok attempted-step)]
                 (if-let [counterexample
                          (:error step-result)]
                   (weak-witness-failure-result
                    witness
                    (:obligations state)
                    (:pending-unobservable state)
                    (:pending-observable state)
                    counterexample)

                   (recur
                    (inc index)
                    (:ok step-result))))))))))))

(defn check-weak-witness!
  "Check one weak concrete realization witness or throw with the complete result."
  ([choreography-or-verified witness]
   (check-weak-witness!
    choreography-or-verified
    witness
    nil))
  ([choreography-or-verified witness options]
   (let [result
         (check-weak-witness
          choreography-or-verified
          witness
          options)]
     (if (valid? result)
       result
       (correspondence-error
        :weak-realization-witness-check-failed
        "Gesso Choreo weak concrete realization witness check failed."
        {:result result})))))

(defn check-witness!
  "Check one concrete realization witness or throw with the complete result."
  ([choreography-or-verified witness]
   (check-witness!
    choreography-or-verified
    witness
    nil))
  ([choreography-or-verified witness options]
   (let [result
         (check-witness
          choreography-or-verified
          witness
          options)]
     (if (valid? result)
       result
       (correspondence-error
        :realization-witness-check-failed
        "Gesso Choreo concrete realization witness check failed."
        {:result result})))))


(defn explain
  "Return a compact stable summary of a correspondence checker result."
  [result]
  (when-not (result? result)
    (correspondence-error
     :invalid-result
     "Expected a Gesso Choreo correspondence result."
     {:value result}))

  {:property
   (:property result)

   :classification
   (:classification result)

   :valid?
   (:valid? result)

   :scope
   (:scope result)

   :failure-count
   (count
    (:failures result))

   :counterexample
   (:counterexample result)})
