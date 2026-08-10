(ns gesso.live.choreo.verify
  "Static verification for Gesso Live choreographies.

   This namespace checks the protocol graph before projection or execution. It
   is intentionally pure and platform-neutral.

   The verifier owns:
   - choreography/state shape validation
   - role and successor validation
   - reachability and terminal-path analysis
   - send/receive contract validation
   - environment-event validation
   - linear resource ownership/lifecycle analysis
   - conservative knowledge-of-choice validation
   - structured errors and warnings

   It deliberately does not:
   - project endpoint plans
   - execute effects
   - know DOM, HTMX, SSE, Ring, XTDB, Manifold, or Missionary
   - prove arbitrary application effects correct
   - promise unconditional distributed liveness

   A verified choreography establishes protocol safety under the events and
   failures represented by the graph. Environmental failures that matter to an
   execution must be modeled explicitly."
  (:require
   [clojure.set :as set]
   [gesso.live.choreo :as choreo]))

;; -----------------------------------------------------------------------------
;; Result identity
;; -----------------------------------------------------------------------------

(def verification-type
  :gesso.live.choreo/verification)

(def verified-type
  :gesso.live.choreo/verified)

(def ^:private terminal-frontier
  ::terminal)

(def ^:private dead-frontier
  ::dead)

(def ^:private unknown-frontier
  ::unknown)

;; -----------------------------------------------------------------------------
;; Error helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn problem
  "Construct one structured verifier problem."
  ([kind path message]
   (problem kind path message nil))
  ([kind path message data]
   (cond-> {:kind kind
            :path (vec path)
            :message message}
     (some? data)
     (assoc :data data))))

(defn- distinct-problems
  [problems]
  (vec (distinct problems)))

(defn- state-path
  ([state-id]
   [:states state-id])
  ([state-id k]
   [:states state-id k])
  ([state-id k child]
   [:states state-id k child]))

;; -----------------------------------------------------------------------------
;; Primitive predicates
;; -----------------------------------------------------------------------------

(defn- keyword-set?
  [x]
  (and (set? x)
       (every? keyword? x)))

(defn- keyword-map-keys?
  [x]
  (and (map? x)
       (every? keyword? (keys x))))

(defn- keyword-map-values?
  [x]
  (and (map? x)
       (every? keyword? (vals x))))

(defn- optional-keyword?
  [x]
  (or (nil? x)
      (keyword? x)))

(defn- boolean-or-nil?
  [x]
  (or (nil? x)
      (true? x)
      (false? x)))

(defn- semantic-outcome?
  [x]
  (keyword? x))

(defn- known-role?
  [roles role]
  (contains? roles role))

(defn- known-state?
  [states state-id]
  (contains? states state-id))

(defn- known-resource?
  [resources resource-id]
  (contains? resources resource-id))

;; -----------------------------------------------------------------------------
;; Top-level validation
;; -----------------------------------------------------------------------------

(defn- top-level-errors
  [choreography]
  (let [{:keys [name roles initial states resources environment-events]}
        choreography]
    (vec
     (concat
      (when (and (some? name)
                 (not (keyword? name)))
        [(problem
          :invalid-name
          [:name]
          "Choreography :name must be a keyword when present."
          {:name name})])

      (when (empty? roles)
        [(problem
          :missing-roles
          [:roles]
          "Choreography must declare at least one role.")])

      (when-not (keyword-set? roles)
        [(problem
          :invalid-roles
          [:roles]
          "Choreography :roles must contain only keywords."
          {:roles roles})])

      (when-not (keyword? initial)
        [(problem
          :invalid-initial-state
          [:initial]
          "Choreography :initial must be a state keyword."
          {:initial initial})])

      (when (empty? states)
        [(problem
          :missing-states
          [:states]
          "Choreography must declare at least one state.")])

      (when-not (and (map? states)
                     (every? keyword? (keys states)))
        [(problem
          :invalid-state-map
          [:states]
          "Choreography :states must map keyword state ids to state maps.")])

      (when-not (map? resources)
        [(problem
          :invalid-resource-map
          [:resources]
          "Choreography :resources must be a map.")])

      (when-not (keyword-set? environment-events)
        [(problem
          :invalid-environment-events
          [:environment-events]
          "Choreography :environment-events must contain only keywords."
          {:environment-events environment-events})])

      (when (and (keyword? initial)
                 (map? states)
                 (not (known-state? states initial)))
        [(problem
          :unknown-initial-state
          [:initial]
          "Choreography :initial does not name a declared state."
          {:initial initial})])))))

;; -----------------------------------------------------------------------------
;; Resource descriptor validation
;; -----------------------------------------------------------------------------

(defn- resource-descriptor-errors
  [roles resources]
  (vec
   (mapcat
    (fn [[resource-id descriptor]]
      (concat
       (when-not (keyword? resource-id)
         [(problem
           :invalid-resource-id
           [:resources resource-id]
           "Resource ids must be keywords."
           {:resource resource-id})])

       (when-not (map? descriptor)
         [(problem
           :invalid-resource-descriptor
           [:resources resource-id]
           "Resource descriptor must be a map."
           {:resource resource-id
            :descriptor descriptor})])

       (when (map? descriptor)
         (let [{:keys [owner linear? terminal-release? metadata]} descriptor]
           (concat
            (when (and (some? owner)
                       (not (keyword? owner)))
              [(problem
                :invalid-resource-owner
                [:resources resource-id :owner]
                "Resource :owner must be a role keyword."
                {:resource resource-id
                 :owner owner})])

            (when (and (keyword? owner)
                       (not (known-role? roles owner)))
              [(problem
                :unknown-role
                [:resources resource-id :owner]
                "Resource owner is not a declared choreography role."
                {:resource resource-id
                 :owner owner
                 :roles roles})])

            (when-not (boolean-or-nil? linear?)
              [(problem
                :invalid-resource-linearity
                [:resources resource-id :linear?]
                "Resource :linear? must be boolean when present."
                {:resource resource-id
                 :linear? linear?})])

            (when-not (boolean-or-nil? terminal-release?)
              [(problem
                :invalid-resource-terminal-policy
                [:resources resource-id :terminal-release?]
                "Resource :terminal-release? must be boolean when present."
                {:resource resource-id
                 :terminal-release? terminal-release?})])

            (when (and (some? metadata)
                       (not (map? metadata)))
              [(problem
                :invalid-metadata
                [:resources resource-id :metadata]
                "Resource :metadata must be a map when present."
                {:resource resource-id
                 :metadata metadata})]))))))
    resources)))

;; -----------------------------------------------------------------------------
;; State shape validation
;; -----------------------------------------------------------------------------

(defn- state-base-errors
  [state-id state]
  (concat
   (when-not (keyword? state-id)
     [(problem
       :invalid-state-id
       (state-path state-id)
       "State ids must be keywords."
       {:state state-id})])

   (when-not (map? state)
     [(problem
       :invalid-state
       (state-path state-id)
       "Choreography state must be a map."
       {:state state-id
        :value state})])

   (when (and (map? state)
              (not (contains? choreo/state-ops (:op state))))
     [(problem
       :unknown-op
       (state-path state-id :op)
       "State :op is not a supported choreography operation."
       {:state state-id
        :op (:op state)
        :allowed choreo/state-ops})])))

(defn- role-errors
  [roles state-id path-key role]
  (concat
   (when-not (keyword? role)
     [(problem
       :invalid-role
       (state-path state-id path-key)
       "State role must be a keyword."
       {:state state-id
        :role role})])

   (when (and (keyword? role)
              (not (known-role? roles role)))
     [(problem
       :unknown-role
       (state-path state-id path-key)
       "State references a role that is not declared by the choreography."
       {:state state-id
        :role role
        :roles roles})])))

(defn- next-errors
  [state-id state]
  (when-not (keyword? (:next state))
    [(problem
      :invalid-next-state
      (state-path state-id :next)
      "State :next must be a state keyword."
      {:state state-id
       :next (:next state)})]))

(defn- metadata-errors
  [state-id state]
  (when (and (contains? state :metadata)
             (not (map? (:metadata state))))
    [(problem
      :invalid-metadata
      (state-path state-id :metadata)
      "State :metadata must be a map when present."
      {:state state-id
       :metadata (:metadata state)})]))

(defn- payload-key-errors
  [state-id state k]
  (when (contains? state k)
    (let [value (get state k)]
      (when-not (keyword-set? value)
        [(problem
          :invalid-message-keys
          (state-path state-id k)
          "Message key collections must be sets of keywords."
          {:state state-id
           :key k
           :value value})]))))

(defn- send-shape-errors
  [roles state-id state]
  (let [required    (:required state #{})
        optional    (:optional state #{})
        correlation (:correlation state #{})]
    (concat
     (role-errors roles state-id :from (:from state))
     (role-errors roles state-id :to (:to state))
     (when (and (keyword? (:from state))
                (= (:from state) (:to state)))
       [(problem
         :same-role-communication
         (state-path state-id)
         "A choreography send must communicate between distinct roles."
         {:state state-id
          :role (:from state)})])
     (when-not (keyword? (:event state))
       [(problem
         :invalid-event
         (state-path state-id :event)
         "Send :event must be a keyword."
         {:state state-id
          :event (:event state)})])
     (when (and (contains? state :via)
                (not (keyword? (:via state))))
       [(problem
         :invalid-transport
         (state-path state-id :via)
         "Send :via must be a keyword when present."
         {:state state-id
          :via (:via state)})])
     (next-errors state-id state)
     (payload-key-errors state-id state :required)
     (payload-key-errors state-id state :optional)
     (payload-key-errors state-id state :correlation)
     (when (and (keyword-set? required)
                (keyword-set? optional)
                (seq (set/intersection required optional)))
       [(problem
         :ambiguous-message-key
         (state-path state-id)
         "A message key may not be both required and optional."
         {:state state-id
          :overlap (set/intersection required optional)})])
     (when (and (keyword-set? correlation)
                (keyword-set? required)
                (not (set/subset? correlation required)))
       [(problem
         :optional-correlation-key
         (state-path state-id :correlation)
         "Correlation keys must be required message keys."
         {:state state-id
          :correlation correlation
          :required required})])
     (when (contains? state :interrupts)
       (let [interrupts (:interrupts state)]
         (concat
          (when-not (and (keyword-map-keys? interrupts)
                         (keyword-map-values? interrupts))
            [(problem
              :invalid-interrupts
              (state-path state-id :interrupts)
              "Send :interrupts must map event keywords to state keywords."
              {:state state-id
               :interrupts interrupts})]))))
     (metadata-errors state-id state))))

(defn- receive-shape-errors
  [roles state-id state]
  (concat
   (role-errors roles state-id :from (:from state))
   (role-errors roles state-id :to (:to state))
   (when (and (keyword? (:from state))
              (= (:from state) (:to state)))
     [(problem
       :same-role-communication
       (state-path state-id)
       "A choreography receive must communicate between distinct roles."
       {:state state-id
        :role (:from state)})])
   (when-not (keyword? (:event state))
     [(problem
       :invalid-event
       (state-path state-id :event)
       "Receive :event must be a keyword."
       {:state state-id
        :event (:event state)})])
   (when (and (contains? state :via)
              (not (keyword? (:via state))))
     [(problem
       :invalid-transport
       (state-path state-id :via)
       "Receive :via must be a keyword when present."
       {:state state-id
        :via (:via state)})])
   (when (and (contains? state :bind)
              (not (keyword? (:bind state))))
     [(problem
       :invalid-bind
       (state-path state-id :bind)
       "Receive :bind must be a keyword when present."
       {:state state-id
        :bind (:bind state)})])
   (next-errors state-id state)
   (metadata-errors state-id state)))

(defn- effect-shape-errors
  [roles state-id state]
  (concat
   (role-errors roles state-id :role (:role state))
   (when-not (keyword? (:effect state))
     [(problem
       :invalid-effect
       (state-path state-id :effect)
       "Effect id must be a keyword."
       {:state state-id
        :effect (:effect state)})])
   (next-errors state-id state)
   (metadata-errors state-id state)))

(defn- choice-shape-errors
  [roles state-id state]
  (concat
   (role-errors roles state-id :role (:role state))
   (when-not (keyword? (:key state))
     [(problem
       :invalid-choice-key
       (state-path state-id :key)
       "Choice :key must be a keyword."
       {:state state-id
        :key (:key state)})])
   (let [branches (:branches state)]
     (concat
      (when-not (map? branches)
        [(problem
          :invalid-choice-branches
          (state-path state-id :branches)
          "Choice :branches must be a map."
          {:state state-id
           :branches branches})])
      (when (and (map? branches)
                 (empty? branches))
        [(problem
          :empty-choice
          (state-path state-id :branches)
          "Choice must contain at least one branch."
          {:state state-id})])
      (when (and (map? branches)
                 (not (every? keyword? (vals branches))))
        [(problem
          :invalid-choice-target
          (state-path state-id :branches)
          "Every choice branch target must be a state keyword."
          {:state state-id
           :branches branches})])))
   (metadata-errors state-id state)))

(defn- await-shape-errors
  [roles state-id state]
  (concat
   (role-errors roles state-id :role (:role state))
   (let [events (:events state)]
     (concat
      (when-not (map? events)
        [(problem
          :invalid-await-events
          (state-path state-id :events)
          "Await :events must be a map."
          {:state state-id
           :events events})])
      (when (and (map? events)
                 (empty? events))
        [(problem
          :empty-await
          (state-path state-id :events)
          "Await must contain at least one event."
          {:state state-id})])
      (when (and (map? events)
                 (not (keyword-map-keys? events)))
        [(problem
          :invalid-await-event
          (state-path state-id :events)
          "Await event ids must be keywords."
          {:state state-id
           :events events})])
      (when (and (map? events)
                 (not (keyword-map-values? events)))
        [(problem
          :invalid-await-target
          (state-path state-id :events)
          "Await event targets must be state keywords."
          {:state state-id
           :events events})])))
   (when (and (contains? state :bind)
              (not (keyword? (:bind state))))
     [(problem
       :invalid-bind
       (state-path state-id :bind)
       "Await :bind must be a keyword when present."
       {:state state-id
        :bind (:bind state)})])
   (metadata-errors state-id state)))

(defn- resource-state-shape-errors
  [roles resources state-id state]
  (concat
   (role-errors roles state-id :role (:role state))
   (when-not (keyword? (:resource state))
     [(problem
       :invalid-resource-id
       (state-path state-id :resource)
       "Acquire/release :resource must be a keyword."
       {:state state-id
        :resource (:resource state)})])
   (when (and (keyword? (:resource state))
              (not (known-resource? resources (:resource state))))
     [(problem
       :unknown-resource
       (state-path state-id :resource)
       "Acquire/release references an undeclared resource."
       {:state state-id
        :resource (:resource state)
        :resources (set (keys resources))})])
   (next-errors state-id state)
   (metadata-errors state-id state)))

(defn- goto-shape-errors
  [state-id state]
  (next-errors state-id state))

(defn- return-shape-errors
  [roles state-id state]
  (concat
   (role-errors roles state-id :role (:role state))
   (when-not (semantic-outcome? (:outcome state))
     [(problem
       :invalid-terminal-outcome
       (state-path state-id :outcome)
       "Return :outcome must be a keyword."
       {:state state-id
        :outcome (:outcome state)})])
   (when (and (contains? state :value-key)
              (not (keyword? (:value-key state))))
     [(problem
       :invalid-value-key
       (state-path state-id :value-key)
       "Return :value-key must be a keyword when present."
       {:state state-id
        :value-key (:value-key state)})])
   (when (seq (choreo/successors state))
     [(problem
       :terminal-has-successor
       (state-path state-id)
       "A terminal return state may not have successors."
       {:state state-id
        :successors (choreo/successors state)})])
   (metadata-errors state-id state)))

(defn- state-shape-errors
  [roles resources state-id state]
  (vec
   (concat
    (state-base-errors state-id state)
    (when (map? state)
      (case (:op state)
        :effect
        (effect-shape-errors roles state-id state)

        :send
        (send-shape-errors roles state-id state)

        :receive
        (receive-shape-errors roles state-id state)

        :choice
        (choice-shape-errors roles state-id state)

        :await
        (await-shape-errors roles state-id state)

        :acquire
        (resource-state-shape-errors roles resources state-id state)

        :release
        (resource-state-shape-errors roles resources state-id state)

        :goto
        (goto-shape-errors state-id state)

        :return
        (return-shape-errors roles state-id state)

        nil)))))

(defn- all-state-shape-errors
  [{:keys [roles resources states]}]
  (vec
   (mapcat
    (fn [[state-id state]]
      (state-shape-errors roles resources state-id state))
    states)))

;; -----------------------------------------------------------------------------
;; Graph construction
;; -----------------------------------------------------------------------------

(defn- successor-map
  [states]
  (into {}
        (map
         (fn [[state-id state]]
           [state-id
            (if (map? state)
              (choreo/successors state)
              #{})]))
        states))

(defn- predecessor-map
  [states successors]
  (reduce-kv
   (fn [acc state-id targets]
     (reduce
      (fn [acc' target]
        (if (contains? states target)
          (update acc' target (fnil conj #{}) state-id)
          acc'))
      acc
      targets))
   (zipmap (keys states) (repeat #{}))
   successors))

(defn- unknown-successor-errors
  [states successors]
  (vec
   (mapcat
    (fn [[state-id targets]]
      (for [target targets
            :when (not (known-state? states target))]
        (problem
         :unknown-state
         (state-path state-id)
         "State references a successor that is not declared."
         {:state state-id
          :target target})))
    successors)))

(defn- reachable-state-ids
  [initial states successors]
  (if-not (and (keyword? initial)
               (contains? states initial))
    #{}
    (loop [pending [initial]
           index 0
           seen #{}]
      (if (= index (count pending))
        seen
        (let [state-id (nth pending index)]
          (if (contains? seen state-id)
            (recur pending (inc index) seen)
            (let [targets (->> (get successors state-id #{})
                               (filter #(contains? states %))
                               (remove seen))]
              (recur (into pending targets)
                     (inc index)
                     (conj seen state-id)))))))))

(defn- reverse-reachable
  [starts predecessors allowed]
  (loop [pending (vec starts)
         index 0
         seen #{}]
    (if (= index (count pending))
      seen
      (let [state-id (nth pending index)]
        (if (or (contains? seen state-id)
                (not (contains? allowed state-id)))
          (recur pending (inc index) seen)
          (recur (into pending (get predecessors state-id #{}))
                 (inc index)
                 (conj seen state-id)))))))

(defn- terminal-state-ids
  [states reachable]
  (set
   (for [state-id reachable
         :let [state (get states state-id)]
         :when (and (map? state)
                    (choreo/terminal-state? state))]
     state-id)))

(defn- no-terminal-path-errors
  [states reachable predecessors]
  (let [terminals (terminal-state-ids states reachable)
        can-reach-terminal
        (reverse-reachable terminals predecessors reachable)]
    (vec
     (for [state-id reachable
           :when (not (contains? can-reach-terminal state-id))]
       (problem
        :no-terminal-path
        (state-path state-id)
        "Reachable state has no path to a terminal return state."
        {:state state-id})))))

;; -----------------------------------------------------------------------------
;; Environment-event checks
;; -----------------------------------------------------------------------------

(defn- interrupt-event-errors
  [{:keys [states environment-events]} reachable]
  (vec
   (mapcat
    (fn [state-id]
      (let [state (get states state-id)]
        (when (and (= :send (:op state))
                   (map? (:interrupts state)))
          (for [event (keys (:interrupts state))
                :when (not (contains? environment-events event))]
            (problem
             :undeclared-environment-event
             (state-path state-id :interrupts event)
             "Send interrupt event must be declared in :environment-events."
             {:state state-id
              :event event
              :environment-events environment-events})))))
    reachable)))

(defn- sent-events-by-recipient
  [states reachable]
  (reduce
   (fn [acc state-id]
     (let [state (get states state-id)]
       (if (= :send (:op state))
         (update acc (:to state) (fnil conj #{}) (:event state))
         acc)))
   {}
   reachable))

(defn- await-producer-errors
  [{:keys [states environment-events]} reachable]
  (let [sent-events (sent-events-by-recipient states reachable)]
    (vec
     (mapcat
      (fn [state-id]
        (let [state (get states state-id)]
          (when (= :await (:op state))
            (for [event (keys (:events state))
                  :when (and
                         (not (contains? environment-events event))
                         (not (contains? (get sent-events (:role state) #{})
                                        event)))]
              (problem
               :unproducible-event
               (state-path state-id :events event)
               "Awaited event has no reachable participant send and is not declared as an environment event."
               {:state state-id
                :role (:role state)
                :event event})))))
      reachable))))

;; -----------------------------------------------------------------------------
;; Communication contracts
;; -----------------------------------------------------------------------------

(defn communication-key
  "Return the semantic identity used to pair a send and receive."
  [state]
  (select-keys state [:from :to :event :via]))

(defn- matching-communication?
  [send-state receive-state]
  (and (= :send (:op send-state))
       (= :receive (:op receive-state))
       (= (communication-key send-state)
          (communication-key receive-state))))

(defn- send-receive-errors
  [states reachable predecessors]
  (vec
   (concat
    (mapcat
     (fn [state-id]
       (let [state (get states state-id)]
         (when (= :send (:op state))
           (let [receive-id (:next state)
                 receive-state (get states receive-id)]
             (when (and (keyword? receive-id)
                        (contains? states receive-id)
                        (not (matching-communication? state receive-state)))
               [(problem
                 :unmatched-send
                 (state-path state-id :next)
                 "A send's normal successor must be its matching receive state."
                 {:state state-id
                  :send (communication-key state)
                  :next receive-id
                  :next-op (:op receive-state)
                  :receive (when (= :receive (:op receive-state))
                             (communication-key receive-state))})])))))
     reachable)

    (mapcat
     (fn [state-id]
       (let [state (get states state-id)]
         (when (= :receive (:op state))
           (let [preds (get predecessors state-id #{})
                 matching
                 (set
                  (filter
                   (fn [pred-id]
                     (matching-communication?
                      (get states pred-id)
                      state))
                   preds))
                 nonmatching (set/difference preds matching)]
             (concat
              (when (empty? matching)
                [(problem
                  :unmatched-receive
                  (state-path state-id)
                  "A reachable receive must have a matching direct send predecessor."
                  {:state state-id
                   :receive (communication-key state)
                   :predecessors preds})])
              (when (seq nonmatching)
                [(problem
                  :unguarded-receive
                  (state-path state-id)
                  "A receive may only be entered from matching direct send states."
                  {:state state-id
                   :receive (communication-key state)
                   :nonmatching-predecessors nonmatching})]))))))
     reachable))))

(defn- matching-send-predecessors
  [states predecessors receive-id]
  (let [receive-state (get states receive-id)]
    (set
     (filter
      (fn [pred-id]
        (matching-communication?
         (get states pred-id)
         receive-state))
      (get predecessors receive-id #{})))))

;; -----------------------------------------------------------------------------
;; Linear resource analysis
;; -----------------------------------------------------------------------------

(defn- linear-resource?
  [resources resource-id]
  (when-let [descriptor (get resources resource-id)]
    (if (contains? descriptor :linear?)
      (true? (:linear? descriptor))
      true)))

(defn- terminal-release?
  [resources resource-id]
  (when-let [descriptor (get resources resource-id)]
    (if (contains? descriptor :terminal-release?)
      (true? (:terminal-release? descriptor))
      (linear-resource? resources resource-id))))

(defn- expected-resource-owner
  [resources resource-id]
  (get-in resources [resource-id :owner]))

(defn- resource-owner-errors
  [resources state-id state]
  (let [resource-id (:resource state)
        expected (expected-resource-owner resources resource-id)
        actual (:role state)]
    (when (and expected
               (keyword? actual)
               (not= expected actual))
      [(problem
        :resource-owner-mismatch
        (state-path state-id :role)
        "Resource may only be acquired or released by its declared owner."
        {:state state-id
         :resource resource-id
         :expected-owner expected
         :actual-owner actual})])))

(defn- transfer-linear-resources
  [resources state-id state held]
  (let [op (:op state)
        resource-id (:resource state)]
    (cond
      (and (= :acquire op)
           (linear-resource? resources resource-id))
      (if (contains? held resource-id)
        {:held held
         :errors
         [(problem
           :resource-double-acquire
           (state-path state-id :resource)
           "Linear resource is already held on this execution path."
           {:state state-id
            :resource resource-id
            :held held})]}
        {:held (conj held resource-id)
         :errors []})

      (and (= :release op)
           (linear-resource? resources resource-id))
      (if-not (contains? held resource-id)
        {:held held
         :errors
         [(problem
           :resource-double-release
           (state-path state-id :resource)
           "Linear resource is released on a path where it is not held."
           {:state state-id
            :resource resource-id
            :held held})]}
        {:held (disj held resource-id)
         :errors []})

      :else
      {:held held
       :errors []})))

(defn- terminal-resource-errors
  [resources state-id held]
  (let [leaked (set
                (filter
                 #(terminal-release? resources %)
                 held))]
    (when (seq leaked)
      [(problem
        :resource-leak
        (state-path state-id)
        "Terminal state is reachable while terminal-release resources are still held."
        {:state state-id
         :resources leaked})])))

(defn- resource-flow-analysis
  [{:keys [initial states resources]} reachable successors]
  (if-not (contains? reachable initial)
    {:errors []
     :holdings {}}
    (loop [pending [[initial #{}]]
           index 0
           seen #{}
           holdings {}
           errors []]
      (if (= index (count pending))
        {:errors (distinct-problems errors)
         :holdings holdings}
        (let [[state-id held-before] (nth pending index)
              visit-key [state-id held-before]]
          (if (contains? seen visit-key)
            (recur pending
                   (inc index)
                   seen
                   holdings
                   errors)
            (let [state (get states state-id)
                  owner-errors
                  (when (contains? #{:acquire :release} (:op state))
                    (resource-owner-errors resources state-id state))
                  {:keys [held errors] :as transfer}
                  (transfer-linear-resources
                   resources
                   state-id
                   state
                   held-before)
                  terminal-errors
                  (when (choreo/terminal-state? state)
                    (terminal-resource-errors resources state-id held))
                  targets
                  (->> (get successors state-id #{})
                       (filter reachable)
                       (mapv (fn [target] [target held])))
                  holdings'
                  (update holdings state-id (fnil conj #{}) held)
                  errors'
                  (into errors
                        (concat owner-errors
                                (:errors transfer)
                                terminal-errors))]
              (recur (into pending targets)
                     (inc index)
                     (conj seen visit-key)
                     holdings'
                     errors'))))))))

;; -----------------------------------------------------------------------------
;; Knowledge of choice
;; -----------------------------------------------------------------------------

(defn- first-role-frontier
  "Return the first states owned by target-role reachable from start.

   Traversal stops on:
   - a state locally owned by target-role
   - any terminal state

   The terminal sentinel is included when the protocol can terminate before the
   target role receives another local step."
  [states successors start target-role]
  (if-not (contains? states start)
    #{unknown-frontier}
    (loop [pending [start]
           index 0
           seen #{}
           frontier #{}]
      (if (= index (count pending))
        (if (seq frontier)
          frontier
          #{dead-frontier})
        (let [state-id (nth pending index)]
          (if (contains? seen state-id)
            (recur pending (inc index) seen frontier)
            (let [state (get states state-id)]
              (cond
                (nil? state)
                (recur pending
                       (inc index)
                       (conj seen state-id)
                       (conj frontier unknown-frontier))

                (choreo/terminal-state? state)
                (recur pending
                       (inc index)
                       (conj seen state-id)
                       (conj frontier terminal-frontier))

                (= target-role (choreo/state-role state))
                (recur pending
                       (inc index)
                       (conj seen state-id)
                       (conj frontier state-id))

                :else
                (recur (into pending (get successors state-id #{}))
                       (inc index)
                       (conj seen state-id)
                       frontier)))))))))

(defn- choice-frontiers
  [states successors choice-state target-role]
  (into {}
        (map
         (fn [[branch target]]
           [branch
            (first-role-frontier
             states
             successors
             target
             target-role)]))
        (:branches choice-state)))

(defn- branch-dependent?
  [frontiers]
  (> (count (set (vals frontiers))) 1))

(defn- branch-values-by-event
  [states frontiers]
  (reduce-kv
   (fn [acc branch frontier]
     (reduce
      (fn [acc' state-id]
        (if (keyword? state-id)
          (let [state (get states state-id)]
            (if (= :receive (:op state))
              (update acc' (:event state) (fnil conj #{}) branch)
              acc'))
          acc'))
      acc
      frontier))
   {}
   frontiers))

(defn- receive-communicates-choice?
  [states predecessors receive-id choice-key event->branches]
  (let [receive-state (get states receive-id)
        event (:event receive-state)
        event-unique? (= 1 (count (get event->branches event #{})))
        sends (matching-send-predecessors states predecessors receive-id)
        payload-communicates?
        (and (seq sends)
             (every?
              (fn [send-id]
                (contains? (:required (get states send-id) #{})
                           choice-key))
              sends))]
    (or event-unique?
        payload-communicates?)))

(defn- choice-role-errors
  [states predecessors choice-id choice-state target-role frontiers]
  (let [owner (:role choice-state)
        choice-key (:key choice-state)
        event->branches (branch-values-by-event states frontiers)]
    (vec
     (mapcat
      (fn [[branch frontier]]
        (mapcat
         (fn [frontier-id]
           (cond
             (= frontier-id terminal-frontier)
             [(problem
               :uncommunicated-choice
               (state-path choice-id :branches branch)
               "A remote role's behavior differs by choice branch, but this branch terminates before that role can learn the choice."
               {:state choice-id
                :choice-key choice-key
                :choice-owner owner
                :dependent-role target-role
                :branch branch})]

             (= frontier-id dead-frontier)
             [(problem
               :uncommunicated-choice
               (state-path choice-id :branches branch)
               "A remote role's behavior differs by choice branch, but this branch has no observable continuation for that role."
               {:state choice-id
                :choice-key choice-key
                :choice-owner owner
                :dependent-role target-role
                :branch branch})]

             (= frontier-id unknown-frontier)
             [(problem
               :uncommunicated-choice
               (state-path choice-id :branches branch)
               "A remote role's behavior differs by choice branch through an unknown state."
               {:state choice-id
                :choice-key choice-key
                :choice-owner owner
                :dependent-role target-role
                :branch branch})]

             :else
             (let [frontier-state (get states frontier-id)]
               (cond
                 (not= :receive (:op frontier-state))
                 [(problem
                   :uncommunicated-choice
                   (state-path choice-id :branches branch)
                   "A remote role acts differently by choice branch before receiving the authoritative choice."
                   {:state choice-id
                    :choice-key choice-key
                    :choice-owner owner
                    :dependent-role target-role
                    :branch branch
                    :first-remote-state frontier-id
                    :first-remote-op (:op frontier-state)})]

                 (or (not= owner (:from frontier-state))
                     (not= target-role (:to frontier-state)))
                 [(problem
                   :uncommunicated-choice
                   (state-path choice-id :branches branch)
                   "The first remote receive after a branch-dependent choice is not from the role that owns the choice."
                   {:state choice-id
                    :choice-key choice-key
                    :choice-owner owner
                    :dependent-role target-role
                    :branch branch
                    :receive frontier-id
                    :receive-from (:from frontier-state)
                    :receive-to (:to frontier-state)})]

                 (not
                  (receive-communicates-choice?
                   states
                   predecessors
                   frontier-id
                   choice-key
                   event->branches))
                 [(problem
                   :uncommunicated-choice
                   (state-path choice-id :branches branch)
                   "Remote choice communication must encode the branch in the event identity or require the choice key in the message payload."
                   {:state choice-id
                    :choice-key choice-key
                    :choice-owner owner
                    :dependent-role target-role
                    :branch branch
                    :receive frontier-id
                    :event (:event frontier-state)})]

                 :else
                 []))))
         frontier))
      frontiers))))

(defn- knowledge-of-choice-errors
  [{:keys [roles states]} reachable successors predecessors]
  (vec
   (mapcat
    (fn [choice-id]
      (let [choice-state (get states choice-id)]
        (when (= :choice (:op choice-state))
          (mapcat
           (fn [target-role]
             (let [frontiers
                   (choice-frontiers
                    states
                    successors
                    choice-state
                    target-role)]
               (when (branch-dependent? frontiers)
                 (choice-role-errors
                  states
                  predecessors
                  choice-id
                  choice-state
                  target-role
                  frontiers))))
           (disj roles (:role choice-state))))))
    reachable)))

;; -----------------------------------------------------------------------------
;; Warnings
;; -----------------------------------------------------------------------------

(defn- unreachable-state-warnings
  [states reachable]
  (vec
   (for [state-id (sort (set/difference (set (keys states)) reachable))]
     (problem
      :unreachable-state
      (state-path state-id)
      "Declared state is unreachable from the choreography initial state."
      {:state state-id}))))

(defn- unused-environment-event-warnings
  [{:keys [states environment-events]} reachable]
  (let [used
        (reduce
         (fn [acc state-id]
           (let [state (get states state-id)]
             (case (:op state)
               :await
               (into acc (keys (:events state)))

               :send
               (into acc (keys (:interrupts state)))

               acc)))
         #{}
         reachable)]
    (vec
     (for [event (sort (set/difference environment-events used))]
       (problem
        :unused-environment-event
        [:environment-events]
        "Declared environment event is not referenced by a reachable wait or interrupt."
        {:event event})))))

;; -----------------------------------------------------------------------------
;; Verification
;; -----------------------------------------------------------------------------

(defn verify
  "Analyze choreography and return a non-throwing verification result.

   Result shape:

     {:gesso.live.choreo/type :gesso.live.choreo/verification
      :valid? ...
      :choreography ...
      :errors [...]
      :warnings [...]
      :analysis
      {:reachable-state-ids ...
       :unreachable-state-ids ...
       :terminal-state-ids ...
       :successors ...
       :predecessors ...
       :resource-holdings ...}}

   verify normalizes plain choreography maps through choreo/ensure-choreography.
   Constructor-level shape exceptions still indicate malformed values that could
   not be normalized into the choreography representation at all."
  [choreography]
  (let [choreography' (choreo/ensure-choreography choreography)
        {:keys [initial states roles resources]} choreography'
        top-errors (top-level-errors choreography')
        resource-errors
        (if (and (set? roles)
                 (map? resources))
          (resource-descriptor-errors roles resources)
          [])
        shape-errors
        (if (and (set? roles)
                 (map? resources)
                 (map? states))
          (all-state-shape-errors choreography')
          [])
        successors
        (if (map? states)
          (successor-map states)
          {})
        predecessors
        (if (map? states)
          (predecessor-map states successors)
          {})
        unknown-state-errors
        (if (map? states)
          (unknown-successor-errors states successors)
          [])
        reachable
        (if (map? states)
          (reachable-state-ids initial states successors)
          #{})
        graph-errors
        (if (and (map? states)
                 (seq reachable))
          (concat
           (no-terminal-path-errors states reachable predecessors)
           (interrupt-event-errors choreography' reachable)
           (await-producer-errors choreography' reachable)
           (send-receive-errors states reachable predecessors))
          [])
        resource-analysis
        (if (and (map? states)
                 (map? resources)
                 (seq reachable))
          (resource-flow-analysis
           choreography'
           reachable
           successors)
          {:errors []
           :holdings {}})
        choice-errors
        (if (and (map? states)
                 (set? roles)
                 (seq reachable))
          (knowledge-of-choice-errors
           choreography'
           reachable
           successors
           predecessors)
          [])
        errors
        (distinct-problems
         (concat
          top-errors
          resource-errors
          shape-errors
          unknown-state-errors
          graph-errors
          (:errors resource-analysis)
          choice-errors))
        warnings
        (distinct-problems
         (concat
          (if (map? states)
            (unreachable-state-warnings states reachable)
            [])
          (if (and (map? states)
                   (set? (:environment-events choreography')))
            (unused-environment-event-warnings
             choreography'
             reachable)
            [])))
        terminals
        (if (map? states)
          (terminal-state-ids states reachable)
          #{})]
    {:gesso.live.choreo/type verification-type
     :valid? (empty? errors)
     :choreography choreography'
     :errors errors
     :warnings warnings
     :analysis
     {:reachable-state-ids reachable
      :unreachable-state-ids
      (if (map? states)
        (set/difference (set (keys states)) reachable)
        #{})
      :terminal-state-ids terminals
      :successors successors
      :predecessors predecessors
      :resource-holdings (:holdings resource-analysis)}}))

(defn valid?
  "True when choreography passes verification."
  [choreography]
  (:valid? (verify choreography)))

(defn verify!
  "Verify choreography and return a verified compiler value.

   Throws ex-info with structured :errors, :warnings, and :analysis when
   verification fails."
  [choreography]
  (let [result (verify choreography)]
    (when-not (:valid? result)
      (throw
       (ex "Gesso Live choreography failed verification."
           {:error/type :gesso.live.choreo/verification-failed
            :errors (:errors result)
            :warnings (:warnings result)
            :analysis (:analysis result)
            :choreography (:choreography result)})))
    {:gesso.live.choreo/type verified-type
     :verified? true
     :choreography (:choreography result)
     :warnings (:warnings result)
     :analysis (:analysis result)}))

(defn verified?
  "True when x is a value returned by verify!."
  [x]
  (and (map? x)
       (= verified-type
          (:gesso.live.choreo/type x))
       (true? (:verified? x))))

(defn ensure-verified
  "Return an already-verified value or verify a choreography now."
  [x]
  (if (verified? x)
    x
    (verify! x)))

(defn explain
  "Return a compact inspectable verifier summary."
  [verification-or-choreography]
  (let [result
        (cond
          (verified? verification-or-choreography)
          verification-or-choreography

          (and (map? verification-or-choreography)
               (= verification-type
                  (:gesso.live.choreo/type verification-or-choreography)))
          verification-or-choreography

          :else
          (verify verification-or-choreography))
        analysis (:analysis result)]
    {:valid? (if (contains? result :valid?)
               (:valid? result)
               true)
     :error-count (count (:errors result))
     :warning-count (count (:warnings result))
     :reachable-state-count
     (count (:reachable-state-ids analysis))
     :unreachable-state-count
     (count (:unreachable-state-ids analysis))
     :terminal-state-count
     (count (:terminal-state-ids analysis))}))
