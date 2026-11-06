(ns gesso.live.browser.refinement
  "A conservative browser-to-distributed-Choreo observation mapping.

   This mapping runs actual adapter steps and uses actual role-local machine
   history and completion effects. It separates hidden local work from *possible*
   distributed observations. A browser receiver does not prove its sender sent;
   an outbound send does not prove a receiver consumed it; one browser execution
   finishing does not prove the global choreography terminated.

   No return value is a global trace witness, certificate, host observation,
   transport attestation, or XTDB commit record. In particular, importing an
   already-advanced machine at :execution/start cannot attest its past.

   Portable CLJC: suitable for later independent Node/CLJS exploration."
  (:require [gesso.choreo.machine :as machine]
            [gesso.choreo.semantics :as semantics]
            [gesso.live.browser.adapter :as adapter]
            [gesso.live.browser.observation :as observation]))

(def refinement-version 1)
(def step-type :gesso.live.browser.refinement/step)
(def trace-type :gesso.live.browser.refinement/trace)

(def ^:private receive-requirements
  #{:sender-emission-witness
    :delivery-correspondence
    :global-schedule-admissibility})

(def ^:private terminal-requirements
  #{:global-terminal-outcome-witness
    :all-role-terminal-compatibility
    :global-schedule-admissibility})

(def ^:private persistent-limitations
  #{:not-global-choreo-trace-refinement
    :not-physical-transport-correspondence
    :not-authoritative-operation-evidence
    :not-xtdb-commit-evidence
    :not-browser-host-refinement})

(defn- reject! [kind data]
  (throw (ex-info "Browser-to-Choreo refinement mapping rejected the transition."
                  (assoc data
                         :error/type :gesso.live.browser.refinement/error
                         :error/kind kind))))

(defn- record-history [record]
  (when record
    (machine/execution-history (:execution record))))

(defn- hidden [execution-id generation occurrence reason]
  {:execution-id execution-id
   :generation generation
   :occurrence occurrence
   :mapping :hidden
   :reason reason})

(defn- candidate [execution-id generation occurrence semantic requirements origin]
  (when-not (semantics/distributed-observation? semantic)
    (reject! :invalid-distributed-observation {:semantic semantic}))
  {:execution-id execution-id
   :generation generation
   :occurrence occurrence
   :distributed-observation semantic
   :origin origin
   :mapping :candidate-only
   :required-witnesses requirements})

(defn- project-history-entry [execution-id generation entry]
  (case (:kind entry)
    (:local :branch :environment :authoritative-observation)
    (do
      (when (some? (semantics/distributed-observation entry))
        (reject! :unexpected-observable-local-occurrence {:entry entry}))
      {:hidden (hidden execution-id generation entry :distributed-unobservable)})

    :send
    {:hidden (hidden execution-id generation entry :physical-send-not-global-communication)}

    :receive
    (let [semantic (semantics/distributed-observation
                    (cond-> {:kind :communication
                             :from (:from entry) :to (:to entry)
                             :event (:event entry) :payload (:payload entry)}
                      (contains? entry :via) (assoc :via (:via entry))))]
      {:candidate (candidate execution-id generation entry semantic
                             receive-requirements :role-local-receive)})

    :terminal
    ;; The projected machine ends at a synthetic local completion. Its outcome
    ;; is NOT the semantic global :return outcome (see project/complete-outcome).
    {:hidden (hidden execution-id generation entry :role-local-terminal-not-global)}

    :authoritative
    (reject! :browser-authoritative-occurrence {:entry entry :execution-id execution-id})

    (reject! :unknown-role-local-history-kind
             {:entry entry :execution-id execution-id})))

(defn- per-execution-history [before after execution-id]
  (let [old (get-in before [:executions execution-id])
        new (get-in after [:executions execution-id])]
    (cond
      ;; A new/replaced execution is handed to the adapter by the caller. It
      ;; may contain preexisting machine history; none is an observed step here.
      (and new (or (nil? old) (not= (:generation old) (:generation new))))
      {:entries []
       :requirements (cond-> #{}
                       (seq (record-history new))
                       (conj :preexisting-machine-history-unwitnessed))}

      (and old new)
      (let [prior (vec (record-history old))
            current (vec (record-history new))
            n (count prior)]
        (when-not (and (<= n (count current))
                       (= prior (subvec current 0 n)))
          (reject! :nonmonotone-role-local-history
                   {:execution-id execution-id
                    :generation (:generation new)}))
        {:entries (mapv (fn [entry] [execution-id (:generation new) entry])
                        (subvec current n))
         :requirements #{}})

      ;; Retirement erases the machine execution and its final history.
      ;; Terminal effects are still observed below, but any intermediate
      ;; receive/local occurrences must not be silently reconstructed.
      (and old (nil? new))
      {:entries []
       :requirements (if (some? old)
                       #{:retired-machine-history-not-retained}
                       #{})}

      :else
      {:entries [] :requirements #{}})))

(defn- completed-effects [semantic-effects]
  (keep (fn [{:keys [kind data]}]
          (when (= :execution/completed kind)
            (let [result (:result data)
                  occurrence {:kind :terminal :outcome (:outcome result)}]
              {:execution-id (:execution-id data)
               :generation (:generation data)
               :occurrence occurrence})))
        semantic-effects))

(defn- mapping [before after obs]
  (let [ids (sort-by pr-str (set (concat (keys (:executions before))
                                            (keys (:executions after)))))
        histories (mapv #(per-execution-history before after %) ids)
        occurrences (mapcat :entries histories)
        derived (mapv (fn [[execution-id generation entry]]
                        (project-history-entry execution-id generation entry))
                      occurrences)
        completed (mapv (fn [{:keys [execution-id generation occurrence]}]
                          (hidden execution-id generation occurrence
                                  :adapter-role-local-completion-not-global))
                        (completed-effects (:semantic-effects obs)))
        hidden (into (into [] (keep :hidden) derived) completed)
        candidates (into [] (keep :candidate) derived)
        requirements (into (cond-> (reduce into #{} (map :requirements histories))
                             (seq completed) (into terminal-requirements))
                           (mapcat :required-witnesses candidates))]
    {:gesso.live.browser.refinement/type step-type
     :gesso.live.browser.refinement/version refinement-version
     :normalized-event (:normalized-event obs)
     :adapter-observation obs
     :mapping (if (seq candidates) :candidate-distributed-events
                  :hidden-or-stuttering)
     :hidden-occurrences hidden
     :distributed-candidates candidates
     :missing-witnesses requirements
     :limitations persistent-limitations
     :global-trace-admitted? false
     :proof-authority :none}))

(defn step-projected
  "Run one real browser adapter step and derive a conservative Choreo mapping.

   Returned candidates are canonical *shapes*, not validated global events.
   Every candidate carries explicit unfulfilled correspondence witnesses."
  [state event]
  (let [before (adapter/require-state! state)
        {:keys [state effects observation]} (observation/step-observed before event)]
    {:state state
     :effects effects
     :observation observation
     :refinement (mapping before state observation)}))

(defn recheck-projection
  "Reject a copied/substituted refinement report unless exactly rederived from
   its original adapter state and normalized event. Equality is NOT admission."
  [state event proposed]
  (let [actual (:refinement (step-projected state event))]
    (when-not (= actual proposed)
      (reject! :refinement-report-mismatch
               {:event (:event event)}))
    actual))

(defn trace-projected
  "Replay a concrete ordered sequence of adapter events. Return all step
   mappings and candidate observations in order. Does not assert that this is
   a legal distributed Choreo trace or that the host emitted these events."
  [state events]
  (when-not (vector? events)
    (reject! :invalid-event-sequence {:events events}))
  (let [result (reduce (fn [{:keys [state transitions]} event]
                         (let [step (step-projected state event)]
                           {:state (:state step)
                            :transitions (conj transitions
                                               (select-keys step
                                                            [:observation :refinement :effects]))}))
                       {:state (adapter/require-state! state) :transitions []}
                       events)
        mappings (mapv :refinement (:transitions result))]
    {:gesso.live.browser.refinement/type trace-type
     :gesso.live.browser.refinement/version refinement-version
     :state (:state result)
     :transitions (:transitions result)
     :distributed-candidates (into [] (mapcat :distributed-candidates) mappings)
     :missing-witnesses (reduce into #{} (map :missing-witnesses mappings))
     :limitations persistent-limitations
     :global-trace-admitted? false
     :proof-authority :none}))
