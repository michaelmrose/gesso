(ns gesso.live.browser.observation
  "A portable, source-relative observation boundary for one browser adapter step.

   This namespace DOES NOT project the browser directly into the distributed
   Choreo observation alphabet. It distinguishes role-local machine boundaries,
   browser-only effects, transport requests, and installed authoritative HTML.
   In particular, optimistic display, SSE wakeups, an outbound send request,
   and an installed authoritative view are not evidence of a committed XTDB
   operation. Subsequent refinement work may relate these observations to a
   global Choreo trace only with explicit witnesses and hidden/stuttering steps.

   Observations are derived from the actual adapter transition. A caller cannot
   supply a fabricated successor and have it treated as observed execution.
   Pure and portable across JVM and CLJS; no browser or host APIs are invoked."
  (:require [gesso.choreo.machine :as machine]
            [gesso.live.browser.adapter :as adapter]))

(def observation-version 1)
(def observation-type :gesso.live.browser.observation/transition)

(def ^:private effect-domain
  {:machine/local :choreo-request
   :machine/send :choreo-request
   :transport/send :transport-request
   :transport/cancel :transport-request
   :timer/start :timer-request
   :timer/cancel :timer-request
   :execution/completed :role-local-terminal
   :execution/retired :role-local-retirement
   :fragment/refresh :fragment-request
   :fragment/request-failed :fragment-request
   :htmx/allow-request :htmx-decision
   :htmx/cancel-request :htmx-decision
   :htmx/allow-swap :htmx-decision
   :htmx/cancel-swap :htmx-decision
   :continuity/capture :continuity-request
   :continuity/restore :continuity-request
   :continuity/release :continuity-request
   :optimistic/install-provisional :provisional-presentation
   :optimistic/timeout-start :provisional-presentation
   :optimistic/timeout-cancel :provisional-presentation
   :optimistic/finish :provisional-presentation
   :authoritative/installed :authoritative-view-installation})

(def ^:private machine-advance-events
  #{:machine/local-completed :transport/succeeded
    :machine/message :machine/environment})

(defn- observation-error [kind data]
  (throw (ex-info "Browser observation cannot establish the requested transition."
                  (assoc data
                         :error/type :gesso.live.browser.observation/error
                         :error/kind kind))))

(defn- machine-frontier [state execution-id]
  (when-let [record (get-in state [:executions execution-id])]
    {:generation (:generation record)
     :status (:status (:execution record))
     :state (machine/current-state-id (:execution record))
     :history-count (count (machine/execution-history (:execution record)))}))

(defn- semantic-observations [effects]
  (mapv (fn [[kind data :as effect]]
          (when-not (and (vector? effect) (= 2 (count effect))
                         (map? data) (contains? adapter/semantic-effect-kinds kind)
                         (contains? effect-domain kind))
            (observation-error :unclassified-semantic-effect {:effect effect}))
          ;; The original effect remains inspectable. Its category, not its
          ;; presence, determines which refinement obligations are possible.
          {:kind kind :domain (get effect-domain kind) :data data})
        (adapter/semantic-effects effects)))

(defn- frontier-deltas [before after]
  (into {}
        (keep (fn [scope]
                (let [old (get-in before [:authoritative scope])
                      new (get-in after [:authoritative scope])]
                  (when (not= old new)
                    [scope {:before (some-> old :basis)
                            :after (some-> new :basis)}]))))
        (set (concat (keys (:authoritative before))
                     (keys (:authoritative after))))))

(defn- optimistic-deltas [before after]
  (into {}
        (keep (fn [execution-id]
                (let [old (get-in before [:optimistic execution-id])
                      new (get-in after [:optimistic execution-id])]
                  (when (not= old new)
                    [execution-id {:before (some-> old :status)
                                   :after (some-> new :status)
                                   :generation (or (:execution-generation new)
                                                   (:execution-generation old))}]))))
        (set (concat (keys (:optimistic before))
                     (keys (:optimistic after))))))

(defn- execution-deltas [before after]
  (into {}
        (keep (fn [execution-id]
                (let [old (machine-frontier before execution-id)
                      new (machine-frontier after execution-id)]
                  (when (not= old new)
                    [execution-id {:before old :after new}]))))
        (set (concat (keys (:executions before))
                     (keys (:executions after))))))

(defn- classify-transition [before event after effects]
  (let [semantic (semantic-observations effects)
        actual-event (:event event)
        ignored? (and (= before after) (empty? semantic))
        authoritative (frontier-deltas before after)
        installations (filterv #(= :authoritative/installed (:kind %)) semantic)
        installed (set (map (fn [x] [(:scope (:data x)) (:basis (:data x))])
                            installations))
        execution-id (:execution-id event)
        old-machine (machine-frontier before execution-id)
        new-machine (machine-frontier after execution-id)
        advanced-machine? (and (contains? machine-advance-events actual-event)
                               old-machine
                               (or (and new-machine
                                        (= (:generation old-machine)
                                           (:generation new-machine))
                                        (< (:history-count old-machine)
                                           (:history-count new-machine)))
                                   (some #(and (= :execution/completed (:kind %))
                                               (= execution-id
                                                  (get-in % [:data :execution-id])))
                                         semantic))) ]
    (doseq [[scope {:keys [after]}] authoritative]
      (when-not (contains? installed [scope after])
        (observation-error :unwitnessed-authoritative-view
                           {:scope scope :basis after})))
    {:gesso.live.browser.observation/type observation-type
     :gesso.live.browser.observation/version observation-version
     :normalized-event actual-event
     :classification (if ignored? :stuttering :observable)
     :choreo-step (when advanced-machine?
                    ;; Advancing a role-local machine is not a proof that a
                    ;; distributed communication or authoritative operation
                    ;; completed. The browser cannot discharge that premise.
                    {:event actual-event
                     :execution-id (:execution-id event)
                     :generation (:generation event)
                     :scope :role-local-only})
     :semantic-effects semantic
     :execution-deltas (execution-deltas before after)
     :optimistic-deltas (optimistic-deltas before after)
     :authoritative-view-deltas authoritative
     :limitations #{:not-global-choreo-trace
                    :not-physical-transport-correspondence
                    :not-xtdb-commit-evidence
                    :not-browser-host-refinement}}))

(defn step-observed
  "Perform exactly one *real* adapter transition and describe its observable
   boundary. Returns {:state ... :effects ... :observation ...}. The observation
   is diagnostic and is not a certificate, admission token, or physical witness.
   Rejected adapter events propagate their original typed errors."
  [state event]
  (let [before (adapter/require-state! state)
        [after effects] (adapter/step before event)]
    {:state after
     :effects effects
     :observation (classify-transition before event after effects)}))

(defn observe-transition
  "Recheck a caller-provided transition against the *actual* adapter step.
   Reject a different successor or effect vector instead of trusting a trace
   constructed by a caller or replay tool. Effect order is semantically relevant."
  [state event proposed-state proposed-effects]
  (let [{:keys [state effects observation]} (step-observed state event)]
    (when-not (and (= state proposed-state)
                   (= effects proposed-effects))
      (observation-error :transition-mismatch
                         {:event-type (:event event)
                          :state-matches? (= state proposed-state)
                          :effects-match? (= effects proposed-effects)}))
    observation))
