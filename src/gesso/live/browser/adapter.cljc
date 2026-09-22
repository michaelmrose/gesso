(ns gesso.live.browser.adapter
  "Pure semantic browser adapter for Gesso Live.

   This namespace is the browser semantic core described by the Choreo v4.5
   design. It is deliberately portable Clojure/ClojureScript and contains no
   DOM, HTMX, EventSource, timer, Promise, XHR/fetch, MutationObserver, or other
   host APIs.

   The public transition boundary is:

     AdapterState x NormalizedEvent -> AdapterState x AbstractEffects

   `step` realizes that boundary as a two-element vector:

     [next-state effects]

   The imperative browser shell must interpret returned effects and report
   completion back as normalized events. The shell must not advance Choreo
   machines, decide execution ownership, compare authoritative generations, or
   resume stale callbacks on its own.

   This first adapter owns the cross-cutting browser invariants that previously
   lived in several mutable CLJS registries:

   - one logical generation for every active Choreo execution
   - one effect generation for every asynchronous local/send callback
   - target ownership and immediate semantic retirement
   - execution-owned timers
   - duplicate participant-message suppression by physical delivery id
   - stale-generation rejection before machine resumption
   - at most one in-flight Live refresh per logical fragment
   - coalesced queued fragment requirements without guessing basis ordering
   - request-generation gating of HTMX request/swap callbacks
   - continuity-slot generations distinct from request generations
   - newer approved fragment swaps revoke older continuity slots before DOM mutation
   - one adapter-owned optimistic effect scope per optimistic execution
   - optimistic provisional derivation/install, settlement, timeout, protocol incompatibility, supersession, and rollback disposition
   - monotone authoritative installation at the HTMX swap gate

   `gesso.choreo.machine` remains the owner of portable choreography protocol
   state. This adapter only invokes its public runtime API. In particular, a
   browser execution that reaches an :authoritative boundary is rejected: an
   untrusted browser can never realize a trusted authoritative operation.

   Authoritative bases are opaque. The adapter never infers ordering from
   arrival order, timestamps, numeric magnitude, or string comparison. Moving
   from one installed basis to a distinct basis requires an explicit normalized
   progression witness whose :from/:to exactly match and whose relation is
   :advances. The truth of that witness remains a trusted boundary outside this
   pure transition system.

   Live invalidations may be advisory or carry opaque authoritative refresh
   requirements. A queued refresh is represented independently from its
   requirement set: an advisory wakeup that arrives during one active request
   must still survive until that request retires. Canonical requirements are
   accumulated conservatively as a set. Once a physical HTMX request is bound,
   re-observing an exact canonical requirement already carried by that request
   is idempotent and does not manufacture a redundant successor. The adapter
   never guesses ordering or subsumption between distinct requirements.

   Continuity remains browser-local rendering state. This namespace owns only
   continuity slot identity/lifetime; it never stores captured DOM state.

   Abstract effects are plain vectors of the form [effect-kind data]. Effects
   beginning with :diagnostic/ are observational and must not participate in
   semantic decisions."
  (:require
   [gesso.choreo.machine :as machine]))

;; =============================================================================
;; Identity / public constants
;; =============================================================================

(def adapter-version 1)

(def adapter-state-type
  :gesso.live.browser.adapter/state)

(def normalized-event-types
  #{:execution/start
    :execution/retire
    :machine/local-completed
    :machine/send-requested
    :machine/message
    :machine/environment
    :machine/retry
    :transport/succeeded
    :transport/failed
    :timer/schedule
    :timer/cancel
    :timer/fired
    :live/invalidated
    :fragment/retire
    :htmx/before-request
    :htmx/before-swap
    :htmx/after-swap
    :htmx/after-request
    :http/failed
    :continuity/completed
    :continuity/failed
    :optimistic/settlement-observed
    :optimistic/timeout-fired
    :optimistic/authoritative-superseded})

(def semantic-effect-kinds
  #{:machine/local
    :machine/send
    :transport/send
    :transport/cancel
    :timer/start
    :timer/cancel
    :execution/completed
    :execution/retired
    :fragment/refresh
    :fragment/request-failed
    :htmx/allow-request
    :htmx/cancel-request
    :htmx/allow-swap
    :htmx/cancel-swap
    :continuity/capture
    :continuity/restore
    :continuity/release
    :optimistic/install-provisional
    :optimistic/timeout-start
    :optimistic/timeout-cancel
    :optimistic/finish
    :authoritative/installed})

(def diagnostic-effect-kinds
  #{:diagnostic/ignored})

(def abstract-effect-kinds
  (into semantic-effect-kinds diagnostic-effect-kinds))

(def pending-effect-kinds
  #{:local
    :send-payload
    :transport})

;; =============================================================================
;; Errors / validation helpers
;; =============================================================================

(defn- adapter-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.adapter/error
      :error/kind kind}
     data))))

(defn- positive-integer?
  [value]
  (and (integer? value)
       (pos? value)))

(defn- require-positive-integer!
  [label value]
  (when-not (positive-integer? value)
    (adapter-error
     :invalid-generation
     (str label " must be a positive integer.")
     {:label label
      :value value}))
  value)

(defn- require-nonnegative-integer!
  [label value]
  (when-not (and (integer? value)
                 (<= 0 value))
    (adapter-error
     :invalid-nonnegative-integer
     (str label " must be a non-negative integer.")
     {:label label
      :value value}))
  value)

(defn- require-non-nil!
  [label value]
  (when (nil? value)
    (adapter-error
     :missing-identity
     (str label " must be non-nil.")
     {:label label}))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (adapter-error
     :invalid-map
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-boolean!
  [label value]
  (when-not (or (true? value)
                (false? value))
    (adapter-error
     :invalid-boolean
     (str label " must be boolean.")
     {:label label
      :value value}))
  value)

(defn- effect
  [kind data]
  [kind data])

(defn diagnostic-effect?
  "True for read-only/observational adapter effects."
  [value]
  (and (vector? value)
       (= 2 (count value))
       (contains? diagnostic-effect-kinds
                  (first value))))

(defn semantic-effects
  "Remove diagnostic effects from one transition's effect vector."
  [effects]
  (into []
        (remove diagnostic-effect?)
        effects))

(defn- ignored-effect
  [event reason data]
  (effect
   :diagnostic/ignored
   (merge
    {:event (:event event)
     :reason reason}
    data)))

(def ^:private event-allowed-keys
  {:execution/start
   #{:event :execution-id :execution :target-id
     :replace-owner? :replace-execution? :optimistic}

   :execution/retire
   #{:event :execution-id :generation :reason}

   :machine/local-completed
   #{:event :execution-id :generation :effect-generation :outputs}

   :machine/send-requested
   #{:event :execution-id :generation :effect-generation :payload}

   :machine/message
   #{:event :execution-id :generation :message-id :envelope}

   :machine/environment
   #{:event :execution-id :generation :envelope}

   :machine/retry
   #{:event :execution-id :generation}

   :transport/succeeded
   #{:event :execution-id :generation :effect-generation}

   :transport/failed
   #{:event :execution-id :generation :effect-generation :reason}

   :timer/schedule
   #{:event :execution-id :generation :timer-id :delay-ms :envelope}

   :timer/cancel
   #{:event :execution-id :generation :timer-id}

   :timer/fired
   #{:event :execution-id :generation :timer-id :timer-generation}

   :live/invalidated
   #{:event :fragment-id :requirement}

   :fragment/retire
   #{:event :fragment-id :reason}

   :htmx/before-request
   #{:event :fragment-id :request-generation :request-id}

   :htmx/before-swap
   #{:event :fragment-id :request-generation :request-id :authoritative}

   :htmx/after-swap
   #{:event :fragment-id :request-generation :request-id}

   :htmx/after-request
   #{:event :fragment-id :request-generation :request-id}

   :http/failed
   #{:event :fragment-id :request-generation :request-id :reason}

   :continuity/completed
   #{:event :slot-id :slot-generation}

   :continuity/failed
   #{:event :slot-id :slot-generation :reason}

   :optimistic/settlement-observed
   #{:event :execution-id :generation :resolution :settlement}

   :optimistic/timeout-fired
   #{:event :execution-id :generation :timeout-generation}

   :optimistic/authoritative-superseded
   #{:event :execution-id :generation :authoritative}})

(defn- require-event!
  [event]
  (require-map! "Normalized browser event" event)
  (let [event-type (:event event)]
    (when-not (contains? normalized-event-types event-type)
      (adapter-error
       :unknown-event
       "Unsupported normalized browser event."
       {:event event
        :supported normalized-event-types}))
    (let [allowed (get event-allowed-keys event-type)
          unknown (seq (remove allowed (keys event)))]
      (when unknown
        (adapter-error
         :unknown-event-keys
         "Normalized browser event contains unsupported keys."
         {:event-type event-type
          :unknown-keys (set unknown)
          :allowed-keys allowed}))))
  event)

;; =============================================================================
;; Adapter state
;; =============================================================================

(defn initial-state
  "Return the empty pure browser adapter state.

   Generations are allocated from one monotonically increasing browser-local
   counter. Different logical resource kinds deliberately share the allocator so
   a stale callback cannot accidentally collide merely because two registries
   happened to start numbering at one."
  []
  {:gesso.live.browser.adapter/type adapter-state-type
   :gesso.live.browser.adapter/version adapter-version
   :next-generation 1
   :executions {}
   :targets {}
   :timers {}
   :fragments {}
   :continuity {}
   :optimistic {}
   :authoritative {}})

(defn- execution-record?
  [value]
  (and (map? value)
       (positive-integer? (:generation value))
       (machine/execution? (:execution value))
       (not (machine/completed? (:execution value)))
       (or (nil? (:target-id value))
           (some? (:target-id value)))
       (set? (:consumed-message-ids value))
       (let [pending (:pending-effect value)]
         (or
          (nil? pending)
          (and (map? pending)
               (contains? pending-effect-kinds (:kind pending))
               (positive-integer? (:generation pending))
               (= (:state pending)
                  (machine/current-state-id
                   (:execution value))))))))

(defn- fragment-record?
  [value]
  (and
   (map? value)
   (boolean? (:queued-refresh? value))
   (set? (:queued-requirements value))
   (let [inflight (:inflight value)]
     (or
      (nil? inflight)
      (and
       (map? inflight)
       (positive-integer? (:generation inflight))
       (set? (:requirements inflight))
       (or (nil? (:request-id inflight))
           (some? (:request-id inflight)))
       (or (nil? (:authoritative inflight))
           (map? (:authoritative inflight))))))))

(defn- timer-record?
  [value]
  (and
   (map? value)
   (some? (:execution-id value))
   (positive-integer? (:execution-generation value))
   (positive-integer? (:generation value))
   (some? (:timer-id value))
   (and (integer? (:delay-ms value))
        (<= 0 (:delay-ms value)))
   (map? (:envelope value))))

(defn- continuity-record?
  [value]
  (and
   (map? value)
   (some? (:slot-id value))
   (positive-integer? (:generation value))
   (some? (:fragment-id value))
   (positive-integer? (:request-generation value))
   (or (true? (:restore-issued? value))
       (false? (:restore-issued? value)))))

(def optimistic-direct-resolutions
  "Protocol-v3 direct settlement outcomes understood by the generic browser
   adapter. Application/model outcome remains opaque and is not interpreted
   here."
  #{:confirmed
    :reconciled
    :rejected
    :already-incorporated
    :failed})

(def optimistic-terminal-resolutions
  (into optimistic-direct-resolutions
        #{:superseded
          :timeout
          :network-failed
          :incompatible-protocol
          :retired}))

(def optimistic-dispositions
  "Physical realization dispositions emitted by the pure adapter. The shell may
   realize these, but it must not choose among them."
  #{:await-authority
    :rollback
    :rollback-and-refresh
    :refresh-authority
    :authoritative
    :release-only})

(defn- optimistic-start-config!
  [value]
  (require-map! "Execution :optimistic configuration" value)
  (let [allowed #{:command-id :provisional-key :rollback-eligible? :timeout-ms}
        required #{:command-id :provisional-key :rollback-eligible?}
        ks (set (keys value))
        missing (set (remove ks required))
        unknown (set (remove allowed ks))]
    (when (seq missing)
      (adapter-error
       :missing-optimistic-fields
       "Optimistic execution configuration is missing required fields."
       {:missing missing :required required :value value}))
    (when (seq unknown)
      (adapter-error
       :unknown-optimistic-fields
       "Optimistic execution configuration contains unsupported fields."
       {:unknown unknown :allowed allowed :value value})))
  (require-non-nil! "Optimistic command id" (:command-id value))
  (when-not (keyword? (:provisional-key value))
    (adapter-error
     :invalid-optimistic-provisional-key
     "Optimistic :provisional-key must be a semantic FactKey keyword."
     {:provisional-key (:provisional-key value)}))
  (require-boolean! ":rollback-eligible?" (:rollback-eligible? value))
  (when (contains? value :timeout-ms)
    (require-nonnegative-integer! "Optimistic timeout" (:timeout-ms value)))
  value)

(defn- optimistic-record?
  [value]
  (and
   (map? value)
   (some? (:execution-id value))
   (positive-integer? (:execution-generation value))
   (some? (:command-id value))
   (some? (:target-id value))
   (keyword? (:provisional-key value))
   (boolean? (:rollback-eligible? value))
   (contains? #{:awaiting-provisional :provisional :settlement-observed}
              (:status value))
   (or (nil? (:timeout-ms value))
       (and (integer? (:timeout-ms value))
            (<= 0 (:timeout-ms value))))
   (or (nil? (:timeout-generation value))
       (positive-integer? (:timeout-generation value)))
   (case (:status value)
     :awaiting-provisional
     (and (nil? (:provisional value))
          (nil? (:timeout-generation value))
          (nil? (:resolution value))
          (nil? (:settlement value)))

     :provisional
     (and (map? (:provisional value))
          (nil? (:resolution value))
          (nil? (:settlement value)))

     :settlement-observed
     (and (map? (:provisional value))
          (contains? optimistic-direct-resolutions (:resolution value))
          (map? (:settlement value)))

     false)))

(defn invariant-errors
  "Return deterministic adapter invariant violations.

   An empty vector means the pure state satisfies the currently declared
   structural/resource invariants. These are runtime checker invariants, not a
   claim that all v4.5 browser properties have already been proved."
  [state]
  (let [errors (transient [])
        add! (fn [kind data]
               (conj! errors (assoc data :kind kind)))]
    (if-not (map? state)
      [{:kind :state-not-map
        :value state}]
      (do
        (when-not (= adapter-state-type
                     (:gesso.live.browser.adapter/type state))
          (add! :wrong-state-type
                {:value (:gesso.live.browser.adapter/type state)}))
        (when-not (= adapter-version
                     (:gesso.live.browser.adapter/version state))
          (add! :wrong-state-version
                {:value (:gesso.live.browser.adapter/version state)}))
        (when-not (positive-integer? (:next-generation state))
          (add! :invalid-next-generation
                {:value (:next-generation state)}))

        (doseq [[execution-id record] (:executions state)]
          (when-not (execution-record? record)
            (add! :invalid-execution-record
                  {:execution-id execution-id
                   :record record}))
          (when-let [machine-id
                     (when (and (map? record)
                                (machine/execution?
                                 (:execution record)))
                       (machine/execution-id
                        (:execution record)))]
            (when-not (= execution-id machine-id)
              (add! :machine-execution-id-mismatch
                    {:execution-id execution-id
                     :machine-execution-id machine-id}))))

        (doseq [[target-id owner] (:targets state)]
          (let [record (get-in state [:executions (:execution-id owner)])]
            (when-not (and
                       record
                       (= (:generation owner)
                          (:generation record))
                       (= target-id
                          (:target-id record)))
              (add! :invalid-target-owner
                    {:target-id target-id
                     :owner owner}))))

        (doseq [[[execution-id timer-id] timer] (:timers state)]
          (when-not (timer-record? timer)
            (add! :invalid-timer-record
                  {:timer-key [execution-id timer-id]
                   :timer timer}))
          (let [record (get-in state [:executions execution-id])]
            (when-not (and
                       record
                       (= (:execution-id timer) execution-id)
                       (= (:timer-id timer) timer-id)
                       (= (:execution-generation timer)
                          (:generation record)))
              (add! :orphan-timer
                    {:timer-key [execution-id timer-id]
                     :timer timer}))))

        (doseq [[fragment-id fragment] (:fragments state)]
          (when-not (fragment-record? fragment)
            (add! :invalid-fragment-record
                  {:fragment-id fragment-id
                   :fragment fragment})))

        (doseq [[slot-id slot] (:continuity state)]
          (when-not (continuity-record? slot)
            (add! :invalid-continuity-record
                  {:slot-id slot-id
                   :slot slot}))
          (when-not (= slot-id (:slot-id slot))
            (add! :continuity-slot-key-mismatch
                  {:slot-id slot-id
                   :record-slot-id (:slot-id slot)})))

        (doseq [[execution-id optimistic] (:optimistic state)]
          (when-not (optimistic-record? optimistic)
            (add! :invalid-optimistic-record
                  {:execution-id execution-id
                   :record optimistic}))
          (let [execution-record (get-in state [:executions execution-id])
                target-owner (get-in state [:targets (:target-id optimistic)])]
            (when-not (and
                       execution-record
                       (= execution-id (:execution-id optimistic))
                       (= (:generation execution-record)
                          (:execution-generation optimistic))
                       (= (:target-id execution-record)
                          (:target-id optimistic))
                       (= {:execution-id execution-id
                           :generation (:generation execution-record)}
                          target-owner))
              (add! :orphan-optimistic-scope
                    {:execution-id execution-id
                     :record optimistic
                     :execution execution-record
                     :target-owner target-owner}))))

        (doseq [[scope frontier] (:authoritative state)]
          (when (nil? (:basis frontier))
            (add! :invalid-authoritative-frontier
                  {:scope scope
                   :frontier frontier})))

        (persistent! errors)))))

(defn state?
  "True when value is a current adapter state satisfying runtime invariants."
  [value]
  (empty? (invariant-errors value)))

(defn require-state!
  "Return state or throw with its invariant counterexamples."
  [state]
  (let [errors (invariant-errors state)]
    (when (seq errors)
      (adapter-error
       :invalid-state
       "Gesso Live browser adapter state violates declared invariants."
       {:errors errors
        :state state})))
  state)

(defn- allocate-generation
  [state]
  (let [generation (:next-generation state)]
    [(update state :next-generation inc)
     generation]))

;; =============================================================================
;; Execution helpers
;; =============================================================================

(defn execution
  "Return the active execution record for execution-id, or nil."
  [state execution-id]
  (get-in (require-state! state)
          [:executions execution-id]))

(defn execution-generation
  "Return the current active logical generation for execution-id, or nil."
  [state execution-id]
  (:generation
   (execution state execution-id)))

(defn target-owner
  "Return {:execution-id ... :generation ...} for one logical target."
  [state target-id]
  (get-in (require-state! state)
          [:targets target-id]))

(defn optimistic-scope
  "Return the active adapter-owned optimistic effect scope for execution-id, or nil."
  [state execution-id]
  (get-in (require-state! state)
          [:optimistic execution-id]))

(defn active-execution?
  "True when execution-id/generation still names the current active execution."
  [state execution-id generation]
  (let [record (get-in state [:executions execution-id])]
    (and record
         (= generation (:generation record)))))

(defn- current-execution-or-ignore
  [state event]
  (let [execution-id (:execution-id event)
        generation (:generation event)
        record (get-in state [:executions execution-id])]
    (cond
      (nil? record)
      {:ignored (ignored-effect
                 event
                 :unknown-or-retired-execution
                 {:execution-id execution-id
                  :generation generation})}

      (not= generation (:generation record))
      {:ignored (ignored-effect
                 event
                 :stale-execution-generation
                 {:execution-id execution-id
                  :generation generation
                  :current-generation (:generation record)})}

      :else
      {:record record})))

(defn- optimistic-disposition
  [resolution rollback-eligible?]
  (case resolution
    (:confirmed :reconciled :already-incorporated)
    :await-authority

    (:rejected :failed)
    (if rollback-eligible?
      :rollback
      :refresh-authority)

    :superseded
    :authoritative

    (:timeout :network-failed :incompatible-protocol)
    (if rollback-eligible?
      :rollback-and-refresh
      :refresh-authority)

    :retired
    :release-only

    (adapter-error
     :unknown-optimistic-resolution
     "Adapter cannot choose a disposition for optimistic resolution."
     {:resolution resolution
      :supported optimistic-terminal-resolutions})))

(defn- optimistic-timeout-cancel-effect
  [scope reason]
  (when-let [timeout-generation (:timeout-generation scope)]
    (effect
     :optimistic/timeout-cancel
     {:execution-id (:execution-id scope)
      :generation (:execution-generation scope)
      :command-id (:command-id scope)
      :timeout-generation timeout-generation
      :reason reason})))

(defn- optimistic-finish-effect
  [scope resolution reason]
  (let [disposition
        (optimistic-disposition
         resolution
         (:rollback-eligible? scope))]
    (effect
     :optimistic/finish
     (cond->
      {:execution-id (:execution-id scope)
       :generation (:execution-generation scope)
       :command-id (:command-id scope)
       :target-id (:target-id scope)
       :provisional (:provisional scope)
       :resolution resolution
       :disposition disposition
       :rollback-eligible? (:rollback-eligible? scope)}
       (:settlement scope)
       (assoc :settlement (:settlement scope))
       (:superseding-authoritative scope)
       (assoc :authoritative (:superseding-authoritative scope))
       reason
       (assoc :reason reason)))))

(defn- completed-optimistic-resolution
  [scope completed-result]
  (when-not (= :settlement-observed (:status scope))
    (adapter-error
     :optimistic-completion-without-settlement
     "Optimistic Choreo completed without an adapter-observed settlement."
     {:execution-id (:execution-id scope)
      :generation (:execution-generation scope)
      :result completed-result
      :scope scope}))
  ;; Projected role-local terminal results intentionally collapse to the
  ;; canonical Choreo completion sentinel. The protocol resolution is a
  ;; semantic value established by the browser resolve action, and the adapter
  ;; already observed the correlated settlement before that message was
  ;; delivered. Therefore the adapter uses the observed settlement resolution
  ;; rather than attempting to reinterpret the projected terminal sentinel.
  (:resolution scope))

(defn- cleanup-optimistic-scope
  [state execution-id generation reason completed-result]
  (if-let [scope (get-in state [:optimistic execution-id])]
    (if-not (= generation (:execution-generation scope))
      [state []]
      (let [resolution
            (cond
              completed-result
              (completed-optimistic-resolution scope completed-result)

              (= reason :optimistic-timeout)
              :timeout

              (= reason :optimistic-network-failed)
              :network-failed

              (= reason :optimistic-incompatible-protocol)
              :incompatible-protocol

              (= reason :authoritative-superseded)
              :superseded

              :else
              :retired)
            timeout-cancel
            (optimistic-timeout-cancel-effect scope reason)
            finish
            (when (map? (:provisional scope))
              (optimistic-finish-effect scope resolution reason))]
        [(update state :optimistic dissoc execution-id)
         (cond-> []
           timeout-cancel (conj timeout-cancel)
           finish (conj finish))]))
    [state []]))

(defn- cleanup-execution-resources
  [state execution-id generation]
  (let [timers
        (->> (:timers state)
             (keep
              (fn [[timer-key timer]]
                (when (and (= execution-id (:execution-id timer))
                           (= generation (:execution-generation timer)))
                  [timer-key timer])))
             vec)
        pending (get-in state [:executions execution-id :pending-effect])
        state'
        (reduce
         (fn [current [timer-key _timer]]
           (update current :timers dissoc timer-key))
         state
         timers)
        effects
        (into
         []
         (concat
          (map
           (fn [[_timer-key timer]]
             (effect
              :timer/cancel
              {:execution-id execution-id
               :generation generation
               :timer-id (:timer-id timer)
               :timer-generation (:generation timer)}))
           timers)
          (when (= :transport (:kind pending))
            [(effect
              :transport/cancel
              {:execution-id execution-id
               :generation generation
               :effect-generation (:generation pending)})])))]
    [state' effects]))

(defn- retire-execution*
  [state execution-id generation reason completed-result]
  (let [record (get-in state [:executions execution-id])]
    (if-not (and record
                 (= generation (:generation record)))
      [state []]
      (let [[state-a optimistic-effects]
            (cleanup-optimistic-scope
             state execution-id generation reason completed-result)
            [state-b cleanup-effects]
            (cleanup-execution-resources
             state-a execution-id generation)
            target-id (:target-id record)
            state-c
            (cond->
             (update state-b :executions dissoc execution-id)
              (and target-id
                   (= {:execution-id execution-id
                       :generation generation}
                      (get-in state-b [:targets target-id])))
              (update :targets dissoc target-id))
            terminal-effect
            (if completed-result
              (effect
               :execution/completed
               {:execution-id execution-id
                :generation generation
                :result completed-result})
              (effect
               :execution/retired
               {:execution-id execution-id
                :generation generation
                :reason reason}))]
        [state-c
         (into []
               (concat optimistic-effects
                       [terminal-effect]
                       cleanup-effects))]))))

(defn- install-pending-effect
  [state execution-id kind]
  (let [[state' effect-generation]
        (allocate-generation state)
        machine-execution
        (get-in state' [:executions execution-id :execution])
        pending
        {:kind kind
         :generation effect-generation
         :state (machine/current-state-id machine-execution)}]
    [(assoc-in state'
               [:executions execution-id :pending-effect]
               pending)
     pending]))

(defn- drive-execution
  "Expose the next machine boundary as an abstract effect.

   Every local/send boundary receives a fresh effect generation. A duplicate
   callback for an older boundary therefore cannot accidentally complete a later
   boundary in the same still-active execution."
  [state execution-id]
  (let [record (get-in state [:executions execution-id])]
    (if-not record
      [state []]
      (let [machine-execution (:execution record)
            generation (:generation record)]
        (cond
          (machine/completed? machine-execution)
          (retire-execution*
           state
           execution-id
           generation
           :completed
           (machine/result machine-execution))

          (machine/waiting-authoritative? machine-execution)
          (adapter-error
           :browser-authoritative-boundary
           "A browser projection may not realize an authoritative Choreo operation."
           {:execution-id execution-id
            :generation generation
            :action (machine/pending-action machine-execution)})

          (:pending-effect record)
          [state []]

          (machine/waiting-local? machine-execution)
          (let [[state' pending]
                (install-pending-effect state execution-id :local)]
            [state'
             [(effect
               :machine/local
               {:execution-id execution-id
                :generation generation
                :effect-generation (:generation pending)
                :action (machine/pending-action machine-execution)})]])

          (machine/waiting-send? machine-execution)
          (let [[state' pending]
                (install-pending-effect state execution-id :send-payload)]
            [state'
             [(effect
               :machine/send
               {:execution-id execution-id
                :generation generation
                :effect-generation (:generation pending)
                :action (machine/pending-action machine-execution)})]])

          :else
          ;; receive/environment suspensions have no physical work until an
          ;; external normalized event arrives.
          [state []])))))

(defn- start-execution
  [state event]
  (let [execution-id
        (require-non-nil! "Execution id" (:execution-id event))
        machine-execution
        (:execution event)
        target-id
        (:target-id event)
        optimistic-config
        (:optimistic event)
        replace-owner?
        (get event :replace-owner? false)
        replace-execution?
        (get event :replace-execution? false)]
    (require-boolean! ":replace-owner?" replace-owner?)
    (require-boolean! ":replace-execution?" replace-execution?)
    (when optimistic-config
      (optimistic-start-config! optimistic-config)
      (when (nil? target-id)
        (adapter-error
         :optimistic-target-required
         "Optimistic execution requires one logical target-id."
         {:execution-id execution-id})))
    (when-not (machine/execution? machine-execution)
      (adapter-error
       :invalid-machine-execution
       "Execution start requires a canonical Gesso Choreo machine execution."
       {:execution-id execution-id
        :execution machine-execution}))
    (when-let [machine-id (machine/execution-id machine-execution)]
      (when-not (= execution-id machine-id)
        (adapter-error
         :execution-id-mismatch
         "Adapter execution id disagrees with the Choreo machine execution id."
         {:execution-id execution-id
          :machine-execution-id machine-id})))
    (let [existing (get-in state [:executions execution-id])
          owner (when target-id
                  (get-in state [:targets target-id]))]
      (when (and existing
                 (not replace-execution?))
        (adapter-error
         :execution-already-active
         "Execution id already has an active browser generation."
         {:execution-id execution-id
          :generation (:generation existing)}))
      (when (and owner
                 (not= execution-id (:execution-id owner))
                 (not replace-owner?))
        (adapter-error
         :target-already-owned
         "Logical browser target already has an active execution owner."
         {:target-id target-id
          :owner owner
          :requested-execution-id execution-id}))

      ;; Semantic retirement happens before a replacement generation is
      ;; allocated. Cleanup effects are best effort and do not block ownership.
      (let [[state-a effects-a]
            (if existing
              (retire-execution*
               state
               execution-id
               (:generation existing)
               :replaced-execution
               nil)
              [state []])
            owner-a (when target-id
                      (get-in state-a [:targets target-id]))
            [state-b effects-b]
            (if (and owner-a
                     (not= execution-id (:execution-id owner-a)))
              (retire-execution*
               state-a
               (:execution-id owner-a)
               (:generation owner-a)
               :target-replaced
               nil)
              [state-a []])
            [state-c generation]
            (allocate-generation state-b)
            record
            {:generation generation
             :execution machine-execution
             :target-id target-id
             :pending-effect nil
             :consumed-message-ids #{}}
            state-d
            (cond->
             (assoc-in state-c [:executions execution-id] record)
              target-id
              (assoc-in [:targets target-id]
                        {:execution-id execution-id
                         :generation generation}))
            [state-e optimistic-scope optimistic-effects]
            (if optimistic-config
              (let [scope
                    {:execution-id execution-id
                     :execution-generation generation
                     :command-id (:command-id optimistic-config)
                     :target-id target-id
                     :provisional-key (:provisional-key optimistic-config)
                     :provisional nil
                     :rollback-eligible? (:rollback-eligible? optimistic-config)
                     :timeout-ms (:timeout-ms optimistic-config)
                     :timeout-generation nil
                     :status :awaiting-provisional
                     :resolution nil
                     :settlement nil}]
                [(assoc-in state-d [:optimistic execution-id] scope)
                 scope
                 []])
              [state-d nil []])
            [state-f drive-effects]
            (drive-execution state-e execution-id)]
        [state-f
         (into []
               (concat effects-a
                       effects-b
                       optimistic-effects
                       drive-effects))]))))

(defn- retire-execution
  [state event]
  (let [execution-id (require-non-nil! "Execution id" (:execution-id event))
        generation (require-positive-integer!
                    "Execution generation"
                    (:generation event))
        reason (or (:reason event) :explicit-retirement)
        {:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (retire-execution*
       state execution-id generation reason nil))))

(defn- complete-local
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [pending (:pending-effect record)
            effect-generation
            (require-positive-integer!
             "Local effect generation"
             (:effect-generation event))]
        (if-not (and pending
                     (= :local (:kind pending))
                     (= effect-generation (:generation pending)))
          [state
           [(ignored-effect
             event
             :stale-local-effect
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :effect-generation effect-generation
              :current-pending pending})]]
          (let [outputs (or (:outputs event) {})
                execution-id (:execution-id event)
                next-execution
                (machine/complete-local
                 (:execution record)
                 outputs)
                state'
                (-> state
                    (assoc-in
                     [:executions execution-id :execution]
                     next-execution)
                    (assoc-in
                     [:executions execution-id :pending-effect]
                     nil))
                scope (get-in state' [:optimistic execution-id])
                awaiting? (= :awaiting-provisional (:status scope))
                provisional
                (when awaiting?
                  (get outputs (:provisional-key scope)))
                _
                (when (and awaiting?
                           (not (map? provisional)))
                  (adapter-error
                   :missing-derived-provisional
                   "Optimistic derive-local completion did not establish the configured provisional value."
                   {:execution-id execution-id
                    :generation (:generation event)
                    :provisional-key (:provisional-key scope)
                    :outputs outputs}))
                [state'' optimistic-effects]
                (if awaiting?
                  (let [timeout-ms (:timeout-ms scope)
                        [state'' timeout-generation]
                        (if (some? timeout-ms)
                          (allocate-generation state')
                          [state' nil])
                        scope'
                        (assoc scope
                               :provisional provisional
                               :timeout-generation timeout-generation
                               :status :provisional)
                        effects
                        (cond->
                         [(effect
                           :optimistic/install-provisional
                           {:execution-id execution-id
                            :generation (:generation event)
                            :command-id (:command-id scope')
                            :target-id (:target-id scope')
                            :provisional provisional
                            :rollback-eligible? (:rollback-eligible? scope')})]
                          timeout-generation
                          (conj
                           (effect
                            :optimistic/timeout-start
                            {:execution-id execution-id
                             :generation (:generation event)
                             :command-id (:command-id scope')
                             :timeout-generation timeout-generation
                             :delay-ms timeout-ms})))]
                    [(assoc-in state'' [:optimistic execution-id] scope')
                     effects])
                  [state' []])
                [state-final drive-effects]
                (drive-execution state'' execution-id)]
            [state-final
             (into [] (concat optimistic-effects drive-effects))]))))))

(defn- request-send
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [pending (:pending-effect record)
            effect-generation
            (require-positive-integer!
             "Send-selection effect generation"
             (:effect-generation event))]
        (if-not (and pending
                     (= :send-payload (:kind pending))
                     (= effect-generation (:generation pending)))
          [state
           [(ignored-effect
             event
             :stale-send-selection
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :effect-generation effect-generation
              :current-pending pending})]]
          (let [payload (or (:payload event) {})
                envelope
                (machine/pending-message
                 (:execution record)
                 payload)
                [state' transport-generation]
                (allocate-generation state)
                transport
                {:kind :transport
                 :generation transport-generation
                 :state (machine/current-state-id
                         (:execution record))
                 :payload payload
                 :message envelope}
                state''
                (assoc-in
                 state'
                 [:executions (:execution-id event) :pending-effect]
                 transport)]
            [state''
             [(effect
               :transport/send
               {:execution-id (:execution-id event)
                :generation (:generation event)
                :effect-generation transport-generation
                :message envelope})]]))))))

(defn- transport-succeeded
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [pending (:pending-effect record)
            effect-generation
            (require-positive-integer!
             "Transport effect generation"
             (:effect-generation event))]
        (if-not (and pending
                     (= :transport (:kind pending))
                     (= effect-generation (:generation pending)))
          [state
           [(ignored-effect
             event
             :stale-transport-success
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :effect-generation effect-generation
              :current-pending pending})]]
          (let [{next-execution :execution}
                (machine/complete-send
                 (:execution record)
                 (:payload pending))
                state'
                (-> state
                    (assoc-in
                     [:executions (:execution-id event) :execution]
                     next-execution)
                    (assoc-in
                     [:executions (:execution-id event) :pending-effect]
                     nil))]
            (drive-execution
             state'
             (:execution-id event))))))))

(defn- transport-failed
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [pending (:pending-effect record)
            effect-generation
            (require-positive-integer!
             "Transport effect generation"
             (:effect-generation event))]
        (if-not (and pending
                     (= :transport (:kind pending))
                     (= effect-generation (:generation pending)))
          [state
           [(ignored-effect
             event
             :stale-transport-failure
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :effect-generation effect-generation
              :current-pending pending})]]
          ;; Generic Choreo leaves transport failure retry policy to its caller.
          ;; An adapter-owned optimistic effect scope has an explicit browser
          ;; recovery path instead: retire semantic ownership, cancel its
          ;; settlement timeout, and emit the adapter-selected rollback/refresh
          ;; disposition. This does not fabricate a trusted :failed settlement.
          (let [state'
                (assoc-in
                 state
                 [:executions (:execution-id event) :pending-effect]
                 nil)]
            (if (get-in state' [:optimistic (:execution-id event)])
              (retire-execution*
               state'
               (:execution-id event)
               (:generation event)
               :optimistic-network-failed
               nil)
              [state'
               [(ignored-effect
                 event
                 :transport-failed
                 {:execution-id (:execution-id event)
                  :generation (:generation event)
                  :effect-generation effect-generation
                  :transport-reason (:reason event)})]])))))))

(defn- retry-machine-boundary
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (if (:pending-effect record)
        [state
         [(ignored-effect
           event
           :effect-already-pending
           {:execution-id (:execution-id event)
            :generation (:generation event)
            :pending-effect (:pending-effect record)})]]
        (drive-execution state (:execution-id event))))))

(defn- deliver-message
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [message-id
            (require-non-nil! "Physical message id" (:message-id event))]
        (if (contains? (:consumed-message-ids record) message-id)
          [state
           [(ignored-effect
             event
             :duplicate-message
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :message-id message-id})]]
          ;; A physical delivery id is single-use for one active execution
          ;; generation. Observation spends it even when the current Choreo
          ;; state does not accept the envelope. Otherwise an early callback
          ;; could be replayed later and become semantically fresh merely
          ;; because the machine advanced to a matching receive boundary.
          (let [observed-state
                (update-in
                 state
                 [:executions (:execution-id event)
                  :consumed-message-ids]
                 conj
                 message-id)]
            (if-not (machine/accepts?
                     (:execution record)
                     (:envelope event))
              [observed-state
               [(ignored-effect
                 event
                 :message-not-accepted
                 {:execution-id (:execution-id event)
                  :generation (:generation event)
                  :message-id message-id})]]
              (let [next-execution
                    (machine/resume
                     (:execution record)
                     (:envelope event))
                    state'
                    (assoc-in
                     observed-state
                     [:executions (:execution-id event) :execution]
                     next-execution)]
                (drive-execution
                 state'
                 (:execution-id event))))))))))

(defn- deliver-environment
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (if-not (machine/accepts?
               (:execution record)
               (:envelope event))
        [state
         [(ignored-effect
           event
           :environment-event-not-accepted
           {:execution-id (:execution-id event)
            :generation (:generation event)})]]
        (let [next-execution
              (machine/resume
               (:execution record)
               (:envelope event))
              state'
              (assoc-in
               state
               [:executions (:execution-id event) :execution]
               next-execution)]
          (drive-execution
           state'
           (:execution-id event)))))))

;; =============================================================================
;; Execution-owned timers
;; =============================================================================

(defn- timer-key
  [execution-id timer-id]
  [execution-id timer-id])

(defn- schedule-timer
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [timer-id (require-non-nil! "Timer id" (:timer-id event))
            delay-ms (require-nonnegative-integer!
                      "Timer delay"
                      (:delay-ms event))
            envelope (:envelope event)
            _ (require-map! "Timer environment envelope" envelope)
            _ (when-not (machine/accepts?
                         (:execution record)
                         envelope)
                (adapter-error
                 :timer-envelope-not-accepted
                 "Timer may only be scheduled with an environment envelope accepted by the current machine suspension."
                 {:execution-id (:execution-id event)
                  :generation (:generation event)
                  :timer-id timer-id
                  :envelope envelope}))
            timer-key' (timer-key (:execution-id event) timer-id)
            existing (get-in state [:timers timer-key'])
            [state' timer-generation]
            (allocate-generation state)
            timer
            {:execution-id (:execution-id event)
             :execution-generation (:generation event)
             :timer-id timer-id
             :generation timer-generation
             :delay-ms delay-ms
             :envelope envelope}
            state'' (assoc-in state' [:timers timer-key'] timer)
            effects
            (cond-> []
              existing
              (conj
               (effect
                :timer/cancel
                {:execution-id (:execution-id event)
                 :generation (:generation event)
                 :timer-id timer-id
                 :timer-generation (:generation existing)}))
              true
              (conj
               (effect
                :timer/start
                {:execution-id (:execution-id event)
                 :generation (:generation event)
                 :timer-id timer-id
                 :timer-generation timer-generation
                 :delay-ms delay-ms})))]
        [state'' effects]))))

(defn- cancel-timer
  [state event]
  (let [{:keys [ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [timer-id (require-non-nil! "Timer id" (:timer-id event))
            timer-key' (timer-key (:execution-id event) timer-id)
            timer (get-in state [:timers timer-key'])]
        (if-not timer
          [state
           [(ignored-effect
             event
             :timer-not-active
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :timer-id timer-id})]]
          [(update state :timers dissoc timer-key')
           [(effect
             :timer/cancel
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :timer-id timer-id
              :timer-generation (:generation timer)})]])))))

(defn- fire-timer
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [timer-id (require-non-nil! "Timer id" (:timer-id event))
            timer-generation
            (require-positive-integer!
             "Timer generation"
             (:timer-generation event))
            timer-key' (timer-key (:execution-id event) timer-id)
            timer (get-in state [:timers timer-key'])]
        (if-not (and timer
                     (= timer-generation (:generation timer))
                     (= (:generation event)
                        (:execution-generation timer)))
          [state
           [(ignored-effect
             event
             :stale-timer
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :timer-id timer-id
              :timer-generation timer-generation
              :current-timer-generation (:generation timer)})]]
          (let [state' (update state :timers dissoc timer-key')]
            (if-not (machine/accepts?
                     (:execution record)
                     (:envelope timer))
              [state'
               [(ignored-effect
                 event
                 :timer-event-no-longer-accepted
                 {:execution-id (:execution-id event)
                  :generation (:generation event)
                  :timer-id timer-id
                  :timer-generation timer-generation})]]
              (let [next-execution
                    (machine/resume
                     (:execution record)
                     (:envelope timer))
                    state''
                    (assoc-in
                     state'
                     [:executions (:execution-id event) :execution]
                     next-execution)]
                (drive-execution
                 state''
                 (:execution-id event))))))))))

;; =============================================================================
;; Fragment refresh coordination
;; =============================================================================

(defn fragment-state
  "Return pure coordination state for one logical Live fragment."
  [state fragment-id]
  (get-in (require-state! state)
          [:fragments fragment-id]))

(defn- requirement-set
  [event]
  (if (contains? event :requirement)
    #{(:requirement event)}
    #{}))

(defn- begin-fragment-request
  [state fragment-id requirements]
  (let [[state' request-generation]
        (allocate-generation state)
        fragment
        (or (get-in state' [:fragments fragment-id])
            {:inflight nil
             :queued-refresh? false
             :queued-requirements #{}})
        inflight
        {:generation request-generation
         :request-id nil
         :requirements (set requirements)
         :authoritative nil}
        state''
        (assoc-in
         state'
         [:fragments fragment-id]
         (assoc fragment
                :inflight inflight
                :queued-refresh? false
                :queued-requirements #{}))]
    [state''
     [(effect
       :fragment/refresh
       {:fragment-id fragment-id
        :request-generation request-generation
        :requirements (set requirements)})]]))

(defn- active-request-covers-requirement?
  [fragment event]
  (let [inflight (:inflight fragment)]
    (and
     inflight
     (some? (:request-id inflight))
     (contains? event :requirement)
     (contains? (:requirements inflight)
                (:requirement event)))))

(defn- invalidate-fragment
  [state event]
  (let [fragment-id
        (require-non-nil! "Fragment id" (:fragment-id event))
        requirements (requirement-set event)
        fragment
        (or (get-in state [:fragments fragment-id])
            {:inflight nil
             :queued-refresh? false
             :queued-requirements #{}})]
    (cond
      (active-request-covers-requirement? fragment event)
      ;; Once HTMX has bound a physical request, configRequest has already
      ;; attached this generation's minimum-read requirement. Re-observing the
      ;; exact same canonical requirement therefore cannot strengthen the read
      ;; and must not manufacture a redundant successor refresh. Preserve any
      ;; independently queued advisory or stronger/different canonical work.
      [state []]

      (:inflight fragment)
      [(-> state
           (assoc-in
            [:fragments fragment-id :queued-refresh?]
            true)
           (update-in
            [:fragments fragment-id :queued-requirements]
            into
            requirements))
       []]

      :else
      (begin-fragment-request
       (assoc-in state [:fragments fragment-id] fragment)
       fragment-id
       requirements))))

(defn- current-fragment-generation?
  [state fragment-id request-generation]
  (let [inflight (get-in state [:fragments fragment-id :inflight])]
    (and inflight
         (= request-generation (:generation inflight)))))

(defn- current-fragment-request?
  [state fragment-id request-generation request-id]
  (let [inflight (get-in state [:fragments fragment-id :inflight])]
    (and inflight
         (= request-generation (:generation inflight))
         (some? (:request-id inflight))
         (= request-id (:request-id inflight)))))

(defn- stale-request-effect
  [event phase]
  (effect
   (case phase
     :request :htmx/cancel-request
     :swap :htmx/cancel-swap)
   {:fragment-id (:fragment-id event)
    :request-generation (:request-generation event)
    :request-id (:request-id event)
    :reason :stale-request-generation}))

(defn- before-request
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        request-generation
        (require-positive-integer!
         "Fragment request generation"
         (:request-generation event))
        request-id (require-non-nil! "HTMX request id" (:request-id event))]
    (if-not (current-fragment-generation?
             state fragment-id request-generation)
      [state [(stale-request-effect event :request)]]
      (let [existing-id
            (get-in state [:fragments fragment-id :inflight :request-id])]
        (if (and existing-id
                 (not= existing-id request-id))
          [state [(stale-request-effect event :request)]]
          [(assoc-in
            state
            [:fragments fragment-id :inflight :request-id]
            request-id)
           [(effect
             :htmx/allow-request
             {:fragment-id fragment-id
              :request-generation request-generation
              :request-id request-id})]])))))

;; =============================================================================
;; Authoritative-install monotonicity
;; =============================================================================

(defn authoritative-frontier
  "Return the currently confirmed installed basis for one logical scope."
  [state scope]
  (get-in (require-state! state)
          [:authoritative scope]))

(defn- progression-witness-valid?
  [current-basis next-basis progression]
  (and
   (map? progression)
   (= #{:from :to :relation}
      (set (keys progression)))
   (= current-basis (:from progression))
   (= next-basis (:to progression))
   (= :advances (:relation progression))))

(defn- authoritative-candidate-valid?
  [candidate]
  (and
   (map? candidate)
   (contains? candidate :scope)
   (some? (:scope candidate))
   (contains? candidate :basis)
   (some? (:basis candidate))
   (every? #{:scope :basis :progression}
           (keys candidate))))

(defn- authoritative-install-allowed?
  [state candidate]
  (let [scope (:scope candidate)
        next-basis (:basis candidate)
        current-basis (get-in state [:authoritative scope :basis])]
    (cond
      (nil? current-basis)
      true

      (= current-basis next-basis)
      true

      :else
      (progression-witness-valid?
       current-basis
       next-basis
       (:progression candidate)))))

(defn- older-continuity-slots
  "Return continuity slots for fragment-id that do not belong to keep-slot-id.

   A current beforeSwap belongs to the newest request generation that HTMX is
   about to install. Any other continuity slot for the same logical fragment is
   therefore physically stale with respect to that impending replacement.

   Results are ordered by request generation so abstract effect order is stable
   across Clojure and ClojureScript map implementations."
  [state fragment-id keep-slot-id]
  (->> (:continuity state)
       (keep
        (fn [[slot-id slot]]
          (when (and (= fragment-id (:fragment-id slot))
                     (not= keep-slot-id slot-id))
            [slot-id slot])))
       (sort-by (fn [[_slot-id slot]]
                  (:request-generation slot)))
       vec))

(defn- revoke-older-continuity
  "Semantically retire continuity resources superseded by a newer swap.

   State retirement happens before any release effect is interpreted. The shell
   can therefore revoke each opaque physical resource before the subsequent
   :continuity/capture and :htmx/allow-swap effects for the new generation run.
   A delayed callback from an older replacement then carries an already-retired
   slot generation and is powerless to mutate the newer DOM."
  [state fragment-id keep-slot-id]
  (let [older-slots (older-continuity-slots state fragment-id keep-slot-id)
        state'
        (reduce
         (fn [current [slot-id _slot]]
           (update current :continuity dissoc slot-id))
         state
         older-slots)
        effects
        (mapv
         (fn [[slot-id slot]]
           (effect
            :continuity/release
            {:slot-id slot-id
             :slot-generation (:generation slot)
             :fragment-id fragment-id
             :request-generation (:request-generation slot)
             :reason :newer-fragment-swap}))
         older-slots)]
    [state' effects]))

(defn- before-swap
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        request-generation
        (require-positive-integer!
         "Fragment request generation"
         (:request-generation event))
        request-id (require-non-nil! "HTMX request id" (:request-id event))
        candidate (:authoritative event)]
    (if-not (current-fragment-request?
             state fragment-id request-generation request-id)
      [state [(stale-request-effect event :swap)]]
      (do
        (when (and candidate
                   (not (authoritative-candidate-valid? candidate)))
          (adapter-error
           :invalid-authoritative-candidate
           "HTMX authoritative swap metadata is malformed."
           {:authoritative candidate
            :event event}))
        (if (and candidate
                 (not (authoritative-install-allowed? state candidate)))
          [state
           [(effect
             :htmx/cancel-swap
             {:fragment-id fragment-id
              :request-generation request-generation
              :request-id request-id
              :reason :non-monotone-authoritative-install
              :current-frontier
              (get-in state
                      [:authoritative (:scope candidate)])
              :candidate candidate})]]
          (let [slot-id [fragment-id request-generation]
                [state-a superseded-effects]
                (revoke-older-continuity state fragment-id slot-id)
                existing-slot (get-in state-a [:continuity slot-id])
                [state-b slot]
                (if existing-slot
                  [state-a existing-slot]
                  (let [[allocated-state slot-generation]
                        (allocate-generation state-a)
                        slot
                        {:slot-id slot-id
                         :generation slot-generation
                         :fragment-id fragment-id
                         :request-generation request-generation
                         :restore-issued? false}]
                    [(assoc-in allocated-state
                               [:continuity slot-id]
                               slot)
                     slot]))
                state-c
                (if candidate
                  (assoc-in
                   state-b
                   [:fragments fragment-id :inflight :authoritative]
                   candidate)
                  state-b)
                effects
                (into
                 superseded-effects
                 (cond-> []
                   (nil? existing-slot)
                   (conj
                    (effect
                     :continuity/capture
                     {:slot-id slot-id
                      :slot-generation (:generation slot)
                      :fragment-id fragment-id
                      :request-generation request-generation
                      :request-id request-id}))
                   true
                   (conj
                    (effect
                     :htmx/allow-swap
                     {:fragment-id fragment-id
                      :request-generation request-generation
                      :request-id request-id}))))]
            [state-c effects]))))))

(defn- after-swap
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        request-generation
        (require-positive-integer!
         "Fragment request generation"
         (:request-generation event))
        request-id (require-non-nil! "HTMX request id" (:request-id event))]
    (if-not (current-fragment-request?
             state fragment-id request-generation request-id)
      [state
       [(ignored-effect
         event
         :stale-after-swap
         {:fragment-id fragment-id
          :request-generation request-generation
          :request-id request-id})]]
      (let [candidate
            (get-in state
                    [:fragments fragment-id :inflight :authoritative])
            state-a
            (if candidate
              (assoc-in state
                        [:authoritative (:scope candidate)]
                        {:basis (:basis candidate)})
              state)
            slot-id [fragment-id request-generation]
            slot (get-in state-a [:continuity slot-id])
            issue-restore?
            (and slot
                 (not (:restore-issued? slot)))
            state-b
            (if issue-restore?
              (assoc-in state-a
                        [:continuity slot-id :restore-issued?]
                        true)
              state-a)
            effects
            (cond-> []
              candidate
              (conj
               (effect
                :authoritative/installed
                {:scope (:scope candidate)
                 :basis (:basis candidate)
                 :fragment-id fragment-id
                 :request-generation request-generation}))
              issue-restore?
              (conj
               (effect
                :continuity/restore
                {:slot-id slot-id
                 :slot-generation (:generation slot)
                 :fragment-id fragment-id
                 :request-generation request-generation
                 :request-id request-id})))]
        [state-b effects]))))

(defn- finish-fragment-request
  ([state fragment-id request-generation request-id]
   (finish-fragment-request
    state fragment-id request-generation request-id #{}))
  ([state
    fragment-id
    request-generation
    request-id
    carry-forward-requirements]
   (if-not (current-fragment-request?
            state fragment-id request-generation request-id)
     [state []]
     (let [queued-refresh?
           (true?
            (get-in state
                    [:fragments fragment-id :queued-refresh?]))
           queued
           (get-in state [:fragments fragment-id :queued-requirements])
           ;; A successful generation has satisfied its own minimum-read
           ;; requirements, so the ordinary completion path carries forward
           ;; nothing. A failed generation has not satisfied them. If queued
           ;; work already warrants a successor, that successor must therefore
           ;; retain the failed generation's opaque requirements as well as the
           ;; independently queued ones.
           successor-requirements
           (into queued carry-forward-requirements)
           state'
           (assoc-in state
                     [:fragments fragment-id]
                     {:inflight nil
                      :queued-refresh? false
                      :queued-requirements #{}})]
       (if queued-refresh?
         (begin-fragment-request
          state'
          fragment-id
          successor-requirements)
         [state' []])))))

(defn- after-request
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        request-generation
        (require-positive-integer!
         "Fragment request generation"
         (:request-generation event))
        request-id (require-non-nil! "HTMX request id" (:request-id event))]
    (if-not (current-fragment-request?
             state fragment-id request-generation request-id)
      [state
       [(ignored-effect
         event
         :stale-after-request
         {:fragment-id fragment-id
          :request-generation request-generation
          :request-id request-id})]]
      (finish-fragment-request
       state fragment-id request-generation request-id))))

(defn- http-failed
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        request-generation
        (require-positive-integer!
         "Fragment request generation"
         (:request-generation event))
        request-id (require-non-nil! "HTMX request id" (:request-id event))]
    (if-not (current-fragment-request?
             state fragment-id request-generation request-id)
      [state
       [(ignored-effect
         event
         :stale-http-failure
         {:fragment-id fragment-id
          :request-generation request-generation
          :request-id request-id})]]
      (let [;; Failure cannot prove that the active generation satisfied its
            ;; minimum-read frontier. Preserve those opaque requirements in any
            ;; successor that queued work already requires; do not manufacture
            ;; an unconditional retry when no successor is queued.
            inflight-requirements
            (get-in state
                    [:fragments fragment-id :inflight :requirements])
            [state' next-effects]
            (finish-fragment-request
             state
             fragment-id
             request-generation
             request-id
             inflight-requirements)]
        [state'
         (into
          [(effect
            :fragment/request-failed
            {:fragment-id fragment-id
             :request-generation request-generation
             :request-id request-id
             :reason (:reason event)})]
          next-effects)]))))

(defn- retire-fragment
  [state event]
  (let [fragment-id (require-non-nil! "Fragment id" (:fragment-id event))
        fragment (get-in state [:fragments fragment-id])
        inflight (:inflight fragment)
        slots
        (->> (:continuity state)
             (keep
              (fn [[slot-id slot]]
                (when (= fragment-id (:fragment-id slot))
                  [slot-id slot])))
             vec)
        state'
        (reduce
         (fn [current [slot-id _slot]]
           (update current :continuity dissoc slot-id))
         (update state :fragments dissoc fragment-id)
         slots)
        effects
        (into
         []
         (concat
          (when (and inflight
                     (:request-id inflight))
            [(effect
              :htmx/cancel-request
              {:fragment-id fragment-id
               :request-generation (:generation inflight)
               :request-id (:request-id inflight)
               :reason (or (:reason event)
                           :fragment-retired)})])
          (map
           (fn [[slot-id slot]]
             (effect
              :continuity/release
              {:slot-id slot-id
               :slot-generation (:generation slot)
               :fragment-id fragment-id
               :reason (or (:reason event)
                           :fragment-retired)}))
           slots)))]
    [state' effects]))

(defn- continuity-completed
  [state event]
  (let [slot-id (require-non-nil! "Continuity slot id" (:slot-id event))
        slot-generation
        (require-positive-integer!
         "Continuity slot generation"
         (:slot-generation event))
        slot (get-in state [:continuity slot-id])]
    (cond
      (not (and slot
                (= slot-generation (:generation slot))))
      [state
       [(ignored-effect
         event
         :stale-continuity-completion
         {:slot-id slot-id
          :slot-generation slot-generation
          :current-slot-generation (:generation slot)})]]

      (not (:restore-issued? slot))
      [state
       [(ignored-effect
         event
         :continuity-restore-not-issued
         {:slot-id slot-id
          :slot-generation slot-generation})]]

      :else
      [(update state :continuity dissoc slot-id)
       []])))

(defn- continuity-failed
  [state event]
  (let [slot-id (require-non-nil! "Continuity slot id" (:slot-id event))
        slot-generation
        (require-positive-integer!
         "Continuity slot generation"
         (:slot-generation event))
        slot (get-in state [:continuity slot-id])]
    (cond
      (not (and slot
                (= slot-generation (:generation slot))))
      [state
       [(ignored-effect
         event
         :stale-continuity-failure
         {:slot-id slot-id
          :slot-generation slot-generation
          :current-slot-generation (:generation slot)})]]

      (not (:restore-issued? slot))
      [state
       [(ignored-effect
         event
         :continuity-restore-not-issued
         {:slot-id slot-id
          :slot-generation slot-generation})]]

      :else
      [(update state :continuity dissoc slot-id)
       [(effect
         :continuity/release
         {:slot-id slot-id
          :slot-generation slot-generation
          :fragment-id (:fragment-id slot)
          :request-generation (:request-generation slot)
          :reason (or (:reason event)
                      :restore-failed)})]])))

(defn- current-optimistic-or-ignore
  [state event]
  (let [{:keys [record ignored]}
        (current-execution-or-ignore state event)]
    (if ignored
      {:ignored ignored}
      (if-let [scope (get-in state [:optimistic (:execution-id event)])]
        {:record record
         :scope scope}
        {:ignored
         (ignored-effect
          event
          :execution-not-optimistic
          {:execution-id (:execution-id event)
           :generation (:generation event)})}))))

(defn- optimistic-settlement-observed
  [state event]
  (let [{:keys [record scope ignored]}
        (current-optimistic-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [resolution (:resolution event)
            settlement (:settlement event)]
        (when-not (contains? optimistic-direct-resolutions resolution)
          (adapter-error
           :invalid-optimistic-settlement-resolution
           "Observed optimistic settlement uses an unsupported direct resolution."
           {:resolution resolution
            :supported optimistic-direct-resolutions}))
        (require-map! "Observed optimistic settlement" settlement)
        (when-not (machine/waiting-receive? (:execution record))
          (adapter-error
           :optimistic-settlement-out-of-phase
           "Optimistic settlement may be observed only while the projected machine waits to receive it."
           {:execution-id (:execution-id event)
            :generation (:generation event)
            :machine (machine/explain (:execution record))}))
        (cond
          (= :settlement-observed (:status scope))
          (if (and (= resolution (:resolution scope))
                   (= settlement (:settlement scope)))
            [state
             [(ignored-effect
               event
               :duplicate-optimistic-settlement
               {:execution-id (:execution-id event)
                :generation (:generation event)
                :resolution resolution})]]
            (adapter-error
             :conflicting-optimistic-settlement
             "One optimistic execution observed conflicting settlement values."
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :existing-resolution (:resolution scope)
              :new-resolution resolution}))

          :else
          (let [timeout-cancel
                (optimistic-timeout-cancel-effect scope :settlement-observed)
                scope'
                (assoc scope
                       :status :settlement-observed
                       :resolution resolution
                       :settlement settlement
                       :timeout-generation nil)]
            [(assoc-in state [:optimistic (:execution-id event)] scope')
             (cond-> []
               timeout-cancel (conj timeout-cancel))]))))))

(defn- optimistic-timeout-fired
  [state event]
  (let [{:keys [scope ignored]}
        (current-optimistic-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [timeout-generation
            (require-positive-integer!
             "Optimistic timeout generation"
             (:timeout-generation event))]
        (if-not (= timeout-generation (:timeout-generation scope))
          [state
           [(ignored-effect
             event
             :stale-optimistic-timeout
             {:execution-id (:execution-id event)
              :generation (:generation event)
              :timeout-generation timeout-generation
              :current-timeout-generation (:timeout-generation scope)})]]
          ;; The physical timer has fired, so clear its semantic ownership before
          ;; retirement; cleanup must not issue a redundant timer cancellation.
          (retire-execution*
           (assoc-in state
                     [:optimistic (:execution-id event) :timeout-generation]
                     nil)
           (:execution-id event)
           (:generation event)
           :optimistic-timeout
           nil))))))

(defn- optimistic-authoritative-superseded
  [state event]
  (let [{:keys [scope ignored]}
        (current-optimistic-or-ignore state event)]
    (if ignored
      [state [ignored]]
      (let [authoritative (:authoritative event)]
        (require-map! "Optimistic authoritative supersession" authoritative)
        ;; Keep the exact trusted observation only long enough for the terminal
        ;; abstract effect. It remains semantic data, never a DOM attachment.
        (retire-execution*
         (assoc-in state
                   [:optimistic (:execution-id event) :superseding-authoritative]
                   authoritative)
         (:execution-id event)
         (:generation event)
         :authoritative-superseded
         nil)))))

;; =============================================================================
;; Public transition
;; =============================================================================

(defn step
  "Apply one normalized browser event to adapter state.

   Returns [next-state effects]. The returned state is checked against adapter
   invariants before it escapes this function.

   Stale physical callbacks normally produce an unchanged state plus one
   :diagnostic/ignored effect. Malformed current events and impossible semantic
   transitions fail closed with ExceptionInfo."
  [state event]
  (let [state' (require-state! state)
        event' (require-event! event)
        [next-state effects]
        (case (:event event')
          :execution/start
          (start-execution state' event')

          :execution/retire
          (retire-execution state' event')

          :machine/local-completed
          (complete-local state' event')

          :machine/send-requested
          (request-send state' event')

          :machine/message
          (deliver-message state' event')

          :machine/environment
          (deliver-environment state' event')

          :machine/retry
          (retry-machine-boundary state' event')

          :transport/succeeded
          (transport-succeeded state' event')

          :transport/failed
          (transport-failed state' event')

          :timer/schedule
          (schedule-timer state' event')

          :timer/cancel
          (cancel-timer state' event')

          :timer/fired
          (fire-timer state' event')

          :live/invalidated
          (invalidate-fragment state' event')

          :fragment/retire
          (retire-fragment state' event')

          :htmx/before-request
          (before-request state' event')

          :htmx/before-swap
          (before-swap state' event')

          :htmx/after-swap
          (after-swap state' event')

          :htmx/after-request
          (after-request state' event')

          :http/failed
          (http-failed state' event')

          :continuity/completed
          (continuity-completed state' event')

          :continuity/failed
          (continuity-failed state' event')

          :optimistic/settlement-observed
          (optimistic-settlement-observed state' event')

          :optimistic/timeout-fired
          (optimistic-timeout-fired state' event')

          :optimistic/authoritative-superseded
          (optimistic-authoritative-superseded state' event'))]
    (require-state! next-state)
    [next-state (vec effects)]))

(defn steps
  "Run a deterministic sequence of normalized events.

   Returns {:state final-state :effects all-effects :transitions [...]}. This is
   intended for model/property tests and counterexample shrinking; production
   shells normally call step one event at a time."
  [state events]
  (reduce
   (fn [{:keys [state effects transitions]} event]
     (let [[next-state transition-effects]
           (step state event)]
       {:state next-state
        :effects (into effects transition-effects)
        :transitions
        (conj transitions
              {:event event
               :effects transition-effects})}))
   {:state (require-state! state)
    :effects []
    :transitions []}
   events))

(defn explain
  "Return a compact pure diagnostic summary without exposing host attachments."
  [state]
  (let [state' (require-state! state)]
    {:gesso.live.browser.adapter/type adapter-state-type
     :gesso.live.browser.adapter/version adapter-version
     :next-generation (:next-generation state')
     :active-executions
     (into
      {}
      (map
       (fn [[execution-id record]]
         [execution-id
          {:generation (:generation record)
           :target-id (:target-id record)
           :machine (machine/explain (:execution record))
           :pending-effect
           (some-> (:pending-effect record)
                   (select-keys [:kind :generation :state]))
           :consumed-message-count
           (count (:consumed-message-ids record))}]))
      (:executions state'))
     :targets (:targets state')
     :timer-count (count (:timers state'))
     :fragments
     (into
      {}
      (map
       (fn [[fragment-id fragment]]
         [fragment-id
          {:inflight
           (some-> (:inflight fragment)
                   (select-keys
                    [:generation
                     :request-id
                     :requirements
                     :authoritative]))
           :queued-refresh?
           (:queued-refresh? fragment)
           :queued-requirements
           (:queued-requirements fragment)}]))
      (:fragments state'))
     :continuity-slot-count
     (count (:continuity state'))
     :optimistic
     (into
      {}
      (map
       (fn [[execution-id scope]]
         [execution-id
          (select-keys scope
                       [:execution-generation
                        :command-id
                        :target-id
                        :rollback-eligible?
                        :timeout-ms
                        :timeout-generation
                        :status
                        :resolution])]))
      (:optimistic state'))
     :authoritative (:authoritative state')}))
