(ns gesso.live.browser.refinement-exploration
  "Finite, choice-relative exploration of real browser-adapter transitions.

   Each edge is executed by refinement/step-projected. The caller supplies a
   finite vector of possible normalized events at every reached prefix; Gesso
   cannot infer that this generator enumerates every possible host callback.
   No state merging is performed: different histories reaching the same state
   remain distinct, including stuttering and physical-delivery identities.

   A complete bounded run means every supplied choice was examined through the
   requested depth, NOT that arbitrary browser traces, transport delivery, or
   global Choreo behavior were verified. Adapter-rejected events are recorded
   separately from counterexamples to transition predicates. All reports are
   copyable diagnostics, never certificates or trusted host witnesses.

   Pure CLJC; no DOM, network, scheduler, or physical XTDB dependencies."
  (:require [clojure.set :as set]
            [gesso.live.browser.adapter :as adapter]
            [gesso.live.browser.refinement :as refinement]))

(def exploration-version 1)
(def exploration-type :gesso.live.browser.refinement-exploration/report)

(def ^:private limitations
  #{:caller-event-choices-not-proven-exhaustive
    :only-finite-depth-examined
    :not-all-schedules-trace-refinement
    :not-browser-host-refinement
    :not-physical-transport-correspondence
    :not-authoritative-operation-evidence
    :not-xtdb-commit-evidence})

(def ^:private required-candidate-witnesses
  #{:sender-emission-witness :delivery-correspondence
    :global-schedule-admissibility})

(defn- reject! [kind data]
  (throw (ex-info "Bounded browser exploration rejected its input."
                  (assoc data :error/type
                         :gesso.live.browser.refinement-exploration/error
                         :error/kind kind))))

(defn- bounded-integer? [n min-value]
  (and (integer? n) (<= min-value n)))

(defn- validated-options [opts]
  (when-not (map? opts)
    (reject! :invalid-options {:options opts}))
  (let [known #{:max-depth :max-nodes :max-edges :max-choices
                :transition-predicate}
        unknown (seq (remove known (keys opts)))
        defaults {:max-depth 3 :max-nodes 512 :max-edges 2048
                  :max-choices 64}
        cfg (merge defaults opts)]
    (when unknown
      (reject! :unknown-option-keys {:keys (vec unknown)}))
    (doseq [[k minimum] [[:max-depth 0] [:max-nodes 1]
                         [:max-edges 1] [:max-choices 1]]]
      (when-not (bounded-integer? (get cfg k) minimum)
        (reject! :invalid-exploration-bound {:bound k :value (get cfg k)})))
    (when (and (contains? cfg :transition-predicate)
               (not (fn? (:transition-predicate cfg))))
      (reject! :invalid-transition-predicate {}))
    cfg))

(defn- honest-projection? [projection]
  ;; This is a local non-escalation assertion, not a proof of the global model.
  (let [candidates (:distributed-candidates projection)
        missing (:missing-witnesses projection)]
    (and (= false (:global-trace-admitted? projection))
         (= :none (:proof-authority projection))
         (set? missing)
         (set? (:limitations projection))
         (contains? (:limitations projection)
                    :not-physical-transport-correspondence)
         (vector? candidates)
         (every? (fn [candidate]
                   (and (= :candidate-only (:mapping candidate))
                        (= :role-local-receive (:origin candidate))
                        (set? (:required-witnesses candidate))
                        (set/subset? required-candidate-witnesses
                                     (:required-witnesses candidate))
                        (set/subset? (:required-witnesses candidate) missing)))
                 candidates))))

(defn- checks-for [predicate before event step events]
  (let [projection (:refinement step)
        context {:before before :event event :after (:state step)
                 :effects (:effects step) :observation (:observation step)
                 :refinement projection :events events}
        built-in (honest-projection? projection)
        custom (if predicate (predicate context) true)]
    (when-not (or (true? custom) (false? custom))
      (reject! :nonboolean-transition-predicate
               {:events events :result custom}))
    (cond-> []
      (not built-in) (conj :refinement-authority-or-witness-escalation)
      (not custom) (conj :caller-transition-predicate-failed))))

(defn- choices! [choose node max-choices]
  (let [choices (choose {:state (:state node)
                         :events (:events node)
                         :depth (count (:events node))
                         :missing-witnesses (:missing-witnesses node)
                         :candidate-count (:candidate-count node)})]
    (when-not (vector? choices)
      (reject! :nonvector-event-choices {:events (:events node)}))
    (when (> (count choices) max-choices)
      (reject! :choice-fanout-limit-exceeded
               {:events (:events node) :count (count choices)
                :max-choices max-choices}))
    (when-not (= (count choices) (count (distinct choices)))
      (reject! :duplicate-event-choices {:events (:events node)}))
    choices))

(defn- prefix-summary [node]
  (select-keys node [:events :candidate-count :missing-witnesses]))

(defn- rejected-adapter-transition? [e]
  (= :gesso.live.browser.adapter/error (:error/type (ex-data e))))

(defn- attempted-step [state event]
  ;; Catch ONLY the real adapter invocation. A caller's predicate is arbitrary
  ;; code: even if it forges :error/type, it must not be misreported as a
  ;; rejected browser event and silently erased from verification.
  (try
    {:step (refinement/step-projected state event)}
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (if (rejected-adapter-transition? e)
        {:rejection (select-keys (ex-data e) [:error/type :error/kind])}
        (throw e)))))

(defn- append-choice [acc node event predicate max-nodes]
  (let [events (conj (:events node) event)
        attempt (attempted-step (:state node) event)]
    (if-let [rejection (:rejection attempt)]
      (update acc :rejected-edges conj
              (merge {:events events :depth (count events)} rejection))
      (let [step (:step attempt)
            violations (checks-for predicate (:state node) event step events)
            projection (:refinement step)
            child {:events events :state (:state step)
                   :candidate-count (+ (:candidate-count node)
                                       (count (:distributed-candidates projection)))
                   :missing-witnesses (into (:missing-witnesses node)
                                            (:missing-witnesses projection))}]
        (when (>= (count (:queue acc)) max-nodes)
          (reject! :node-limit-exceeded
                   {:max-nodes max-nodes :events events}))
        (-> acc
            (update :queue conj child)
            (update :counterexamples into
                    (mapv (fn [kind]
                            {:events events :depth (count events) :kind kind
                             :candidate-count (:candidate-count child)
                             :missing-witnesses (:missing-witnesses child)})
                          violations))
            (update :missing-witnesses into (:missing-witnesses projection))
            (update :distributed-candidate-count +
                    (count (:distributed-candidates projection))))))))

(defn explore
  "Explore all supplied browser events through :max-depth (default 3).

   choose-events: (fn [{:keys [state events depth missing-witnesses
                              candidate-count]}] [normalized-event ...]).
   Options: :max-depth, :max-nodes, :max-edges, :max-choices and optional
   :transition-predicate (fn [{:keys [before event after effects observation
                                     refinement events]}] boolean).

   Rejected adapter events are *not* semantic successor states. All other
   exceptions propagate. Budgets fail closed, never return partial success.
   :choice-relative-depth-complete? means only that supplied events were fully
   enumerated within the requested depth, without quotienting histories.
   The caller's event generator, predicate, and host are NOT verified."
  ([initial-state choose-events]
   (explore initial-state choose-events {}))
  ([initial-state choose-events opts]
   (let [state (adapter/require-state! initial-state)
         cfg (validated-options opts)
         {:keys [max-depth max-nodes max-edges max-choices
                 transition-predicate]} cfg]
     (when-not (fn? choose-events)
       (reject! :invalid-event-generator {}))
     (loop [acc {:queue [{:state state :events [] :candidate-count 0
                          :missing-witnesses #{}}]
                 :rejected-edges [] :counterexamples [] :frontier []
                 :missing-witnesses #{} :distributed-candidate-count 0
                 :attempted-edge-count 0}
            index 0]
       (if (= index (count (:queue acc)))
         (let [prefixes (mapv prefix-summary (:queue acc))]
           {:gesso.live.browser.refinement-exploration/type exploration-type
            :gesso.live.browser.refinement-exploration/version exploration-version
            :bounds (select-keys cfg [:max-depth :max-nodes :max-edges :max-choices])
            :status (if (seq (:counterexamples acc)) :counterexamples-found
                        :bounded-no-counterexamples)
            :choice-relative-depth-complete? true
            :global-event-enumeration-established? false
            :global-trace-admitted? false
            :proof-authority :none
            :prefixes prefixes
            :visited-prefix-count (count prefixes)
            :attempted-edge-count (:attempted-edge-count acc)
            :accepted-edge-count (dec (count prefixes))
            :rejected-edges (:rejected-edges acc)
            :distributed-candidate-count (:distributed-candidate-count acc)
            :missing-witnesses (:missing-witnesses acc)
            :counterexamples (:counterexamples acc)
            :frontier (:frontier acc)
            :limitations limitations})
         (let [node (nth (:queue acc) index)
               depth (count (:events node))
               at-bound? (= max-depth depth)
               choices (if at-bound? [] (choices! choose-events node max-choices))
               new-edge-count (+ (:attempted-edge-count acc) (count choices))]
           (when (> new-edge-count max-edges)
             (reject! :edge-limit-exceeded
                      {:max-edges max-edges :attempted new-edge-count
                       :events (:events node)}))
           (let [acc (cond-> (assoc acc :attempted-edge-count new-edge-count)
                       (or at-bound? (empty? choices))
                       (update :frontier conj
                               (assoc (prefix-summary node) :reason
                                      (if at-bound? :depth-bound :no-choices))))
                 next-acc (reduce (fn [result event]
                                    (append-choice result node event
                                                   transition-predicate max-nodes))
                                  acc choices)]
             (recur next-acc (inc index)))))))))

(defn recheck-exploration
  "Compare an untrusted copied report with a fresh, source-executed bounded
   exploration. Equality is diagnostic, not a witness or proof credential."
  ([state choose-events proposed]
   (recheck-exploration state choose-events {} proposed))
  ([state choose-events opts proposed]
   (let [actual (explore state choose-events opts)]
     (when-not (= actual proposed)
       (reject! :exploration-report-mismatch {}))
     actual)))
