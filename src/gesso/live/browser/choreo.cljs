(ns gesso.live.browser.choreo
  "Browser-process adapter for portable Gesso choreography endpoints.

   gesso.choreo.machine owns protocol execution. This namespace owns the
   stateful browser process around it:

   - active execution storage
   - local FX-machine registration and invocation
   - browser FX-handler registration
   - transport send handoff
   - automatic driving through synchronous :fx/:send boundaries
   - suspension/resumption on modeled events and participant messages
   - execution-owned timers
   - bounded terminal diagnostics
   - harmless rejection of late delivery to retired executions

   It deliberately does not know optimistic semantics, DOM replacement,
   continuity policy, HTMX details, SSE details, or application commands.
   Those concerns register FX machines/handlers and a send adapter here.

   Async work is not an FX feature. A local FX machine may schedule browser
   work, but the choreography must then suspend on an explicit event. The
   callback resumes that suspended execution later."
  (:require
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.fx :as fx]))

;; -----------------------------------------------------------------------------
;; Runtime identity and public context keys
;; -----------------------------------------------------------------------------

(def runtime-type
  :gesso.live.browser.choreo/runtime)

(def execution-id-key
  :gesso.live.browser.choreo/execution-id)

(def action-key
  :gesso.live.browser.choreo/action)

(def metadata-key
  :gesso.live.browser.choreo/metadata)

(def resume-envelope-key
  :gesso.live.browser.choreo/resume-envelope)

(def default-terminal-history-limit 64)

;; -----------------------------------------------------------------------------
;; Browser-process stores
;; -----------------------------------------------------------------------------

(defonce executions (atom {}))
(defonce execution-metadata (atom {}))
(defonce fx-machines (atom {}))
(defonce fx-handlers (atom {}))
(defonce send-handler (atom nil))
(defonce timers (atom {}))
(defonce terminal-history (atom []))
(defonce error-handler (atom nil))

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn now-ms
  []
  (.getTime (js/Date.)))

(defn- non-blank-string?
  [x]
  (and (string? x)
       (not= "" (.trim x))))

(defn- require-execution-id!
  [execution-id]
  (when-not (or (keyword? execution-id)
                (uuid? execution-id)
                (non-blank-string? execution-id))
    (throw
     (ex-info
      "Browser choreography execution id must be a keyword, UUID, or non-blank string."
      {:error/type :gesso.live.browser.choreo/invalid-execution-id
       :execution-id execution-id})))
  execution-id)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (throw
     (ex-info
      (str label " must be a keyword.")
      {:error/type :gesso.live.browser.choreo/invalid-keyword
       :label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (ifn? value)
    (throw
     (ex-info
      (str label " must be callable.")
      {:error/type :gesso.live.browser.choreo/invalid-callable
       :label label
       :value value})))
  value)

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (ex-info
      (str label " must be a map.")
      {:error/type :gesso.live.browser.choreo/invalid-map
       :label label
       :value value})))
  value)

(defn- require-nonnegative-number!
  [label value]
  (when-not (and (number? value)
                 (not (js/isNaN value))
                 (not (neg? value)))
    (throw
     (ex-info
      (str label " must be a non-negative number.")
      {:error/type :gesso.live.browser.choreo/invalid-number
       :label label
       :value value})))
  value)

(defn- trim-history
  [history limit]
  (let [history' (vec history)
        excess (- (count history') limit)]
    (if (pos? excess)
      (subvec history' excess)
      history')))

(defn- notify-error!
  [operation execution-id throwable data]
  (let [payload
        (merge
         {:operation operation
          :execution-id execution-id
          :error throwable}
         data)]
    (when-some [handler @error-handler]
      (try
        (handler payload)
        (catch :default _
          nil)))
    payload))

(defn set-error-handler!
  "Install a diagnostic error observer, or nil to clear it."
  [handler]
  (when-not (or (nil? handler)
                (ifn? handler))
    (throw
     (ex-info
      "Browser choreography error handler must be callable or nil."
      {:error/type :gesso.live.browser.choreo/invalid-error-handler
       :handler handler})))
  (reset! error-handler handler)
  true)

;; -----------------------------------------------------------------------------
;; FX registration
;; -----------------------------------------------------------------------------

(defn register-fx-machine!
  "Register or replace one role-local browser FX machine.

   machine-fn is normally produced by gesso.live.browser.fx/machine."
  [machine-id machine-fn]
  (require-keyword! "Browser FX machine id" machine-id)
  (require-callable! "Browser FX machine" machine-fn)
  (swap! fx-machines assoc machine-id machine-fn)
  machine-id)

(defn unregister-fx-machine!
  [machine-id]
  (swap! fx-machines dissoc machine-id)
  machine-id)

(defn registered-fx-machines
  []
  (set (keys @fx-machines)))

(defn fx-machine
  [machine-id]
  (get @fx-machines machine-id))

(defn register-fx-handler!
  "Register or replace one Biff-style browser FX effect handler."
  [handler-id handler]
  (require-keyword! "Browser FX handler id" handler-id)
  (require-callable! "Browser FX handler" handler)
  (swap! fx-handlers assoc handler-id handler)
  handler-id)

(defn unregister-fx-handler!
  [handler-id]
  (swap! fx-handlers dissoc handler-id)
  handler-id)

(defn registered-fx-handlers
  []
  (set (keys @fx-handlers)))

(defn current-fx-handlers
  []
  @fx-handlers)

;; -----------------------------------------------------------------------------
;; Transport handoff
;; -----------------------------------------------------------------------------

(defn set-send-handler!
  "Install the transport handoff for projected :send boundaries.

   Handler shape:

     (fn [action execution] ...)

   Returning normally means the send has been handed off successfully and the
   protocol machine may advance. Throwing leaves the execution failed at the
   adapter boundary and the error is propagated.

   The handler is deliberately transport-neutral. HTMX, fetch, SSE, or another
   integration may interpret the projected send action outside this namespace."
  [handler]
  (when-not (or (nil? handler)
                (ifn? handler))
    (throw
     (ex-info
      "Browser choreography send handler must be callable or nil."
      {:error/type :gesso.live.browser.choreo/invalid-send-handler
       :handler handler})))
  (reset! send-handler handler)
  true)

(defn current-send-handler
  []
  @send-handler)

;; -----------------------------------------------------------------------------
;; Execution lookup and diagnostics
;; -----------------------------------------------------------------------------

(defn execution
  [execution-id]
  (get @executions execution-id))

(defn active?
  [execution-id]
  (contains? @executions execution-id))

(defn active-execution-ids
  []
  (set (keys @executions)))

(defn execution-count
  []
  (count @executions))

(defn metadata
  [execution-id]
  (get @execution-metadata execution-id))

(defn execution-context
  [execution-id]
  (some-> (execution execution-id)
          machine/execution-context))

(defn- execution-summary
  [execution]
  {:execution-id (:execution-id execution)
   :plan-name (:plan-name execution)
   :role (:role execution)
   :status (:status execution)
   :state (:state execution)
   :action (machine/pending-action execution)
   :awaiting (:awaiting execution)
   :held-resources (machine/held-resources execution)
   :result (machine/execution-result execution)
   :trace (machine/execution-trace execution)})

(defn active-summaries
  []
  (mapv execution-summary
        (vals @executions)))

(defn terminal-summaries
  []
  @terminal-history)

;; -----------------------------------------------------------------------------
;; Timer ownership
;; -----------------------------------------------------------------------------

(defn timer
  [execution-id timer-key]
  (get-in @timers [execution-id timer-key]))

(defn cancel-timer!
  "Cancel one execution-owned browser timeout. Idempotent."
  [execution-id timer-key]
  (when-some [handle (timer execution-id timer-key)]
    (js/clearTimeout handle))
  (swap!
   timers
   (fn [all]
     (let [remaining (dissoc (get all execution-id {}) timer-key)]
       (if (seq remaining)
         (assoc all execution-id remaining)
         (dissoc all execution-id)))))
  true)

(defn cancel-all-timers!
  [execution-id]
  (doseq [[_timer-key handle] (get @timers execution-id)]
    (js/clearTimeout handle))
  (swap! timers dissoc execution-id)
  true)

(declare resume-event!)

(defn schedule-event!
  "Schedule a modeled environmental event for one execution.

   Scheduling may happen while an FX machine is still being driven. JavaScript
   cannot run the timeout callback until the current stack returns; by then a
   successful drive has committed the resulting suspended execution. If the
   drive fails, its cleanup cancels every timer owned by that execution."
  ([execution-id timer-key delay-ms event-id]
   (schedule-event! execution-id timer-key delay-ms event-id nil))
  ([execution-id timer-key delay-ms event-id data]
   (require-execution-id! execution-id)
   (require-keyword! "Browser choreography timer key" timer-key)
   (require-nonnegative-number! "Browser choreography timer delay" delay-ms)
   (require-keyword! "Browser choreography timer event" event-id)
   (cancel-timer! execution-id timer-key)
   (let [handle
         (js/setTimeout
          (fn []
            (swap!
             timers
             (fn [all]
               (let [remaining
                     (dissoc (get all execution-id {}) timer-key)]
                 (if (seq remaining)
                   (assoc all execution-id remaining)
                   (dissoc all execution-id)))))
            (resume-event! execution-id event-id data))
          delay-ms)]
     (swap! timers assoc-in [execution-id timer-key] handle)
     handle)))

;; -----------------------------------------------------------------------------
;; Local boundary execution
;; -----------------------------------------------------------------------------

(defn- fx-context
  [execution action resume-envelope]
  (merge
   (machine/execution-context execution)
   {execution-id-key (:execution-id execution)
    action-key action
    metadata-key (metadata (:execution-id execution))
    fx/handlers-key @fx-handlers}
   (when resume-envelope
     {resume-envelope-key resume-envelope})))

(defn- run-fx-boundary
  [execution resume-envelope]
  (let [action (machine/pending-action execution)
        machine-id (:machine action)
        machine-fn (fx-machine machine-id)]
    (when-not machine-fn
      (throw
       (ex-info
        "No browser FX machine is registered for the choreography boundary."
        {:error/type :gesso.live.browser.choreo/missing-fx-machine
         :execution-id (:execution-id execution)
         :state (:state execution)
         :machine machine-id
         :registered (registered-fx-machines)})))
    (let [result (machine-fn (fx-context execution action resume-envelope))]
      (when-not (or (nil? result)
                    (map? result))
        (throw
         (ex-info
          "Browser FX machine must return a map or nil to choreography."
          {:error/type :gesso.live.browser.choreo/invalid-fx-result
           :execution-id (:execution-id execution)
           :state (:state execution)
           :machine machine-id
           :result result})))
      (machine/complete-fx execution result))))

(defn- handoff-send-boundary
  [execution]
  (let [action (machine/pending-action execution)
        handler @send-handler]
    (when-not handler
      (throw
       (ex-info
        "No browser choreography send handler is installed."
        {:error/type :gesso.live.browser.choreo/missing-send-handler
         :execution-id (:execution-id execution)
         :state (:state execution)
         :action action})))
    (handler action execution)
    (machine/complete-send execution)))

(defn- drive
  "Drive synchronous local boundaries until await or terminal completion."
  [execution resume-envelope]
  (loop [current execution
         envelope resume-envelope]
    (cond
      (machine/waiting-fx? current)
      (recur (run-fx-boundary current envelope) nil)

      (machine/waiting-send? current)
      (recur (handoff-send-boundary current) nil)

      (or (machine/suspended? current)
          (machine/completed? current))
      current

      :else
      (throw
       (ex-info
        "Portable choreography machine returned an unknown browser boundary."
        {:error/type :gesso.live.browser.choreo/invalid-machine-status
         :execution-id (:execution-id current)
         :status (:status current)
         :execution current})))))

;; -----------------------------------------------------------------------------
;; Execution lifecycle
;; -----------------------------------------------------------------------------

(defn- remember-terminal!
  [execution metadata]
  (swap!
   terminal-history
   (fn [history]
     (trim-history
      (conj
       history
       (assoc
        (execution-summary execution)
        :completed-at (now-ms)
        :metadata
        (select-keys metadata
                     [:started-at :updated-at :source :kind])))
      default-terminal-history-limit))))

(defn- retire!
  [execution-id execution]
  (let [metadata' (get @execution-metadata execution-id)]
    (cancel-all-timers! execution-id)
    (swap! executions dissoc execution-id)
    (swap! execution-metadata dissoc execution-id)
    (remember-terminal! execution metadata')
    execution))

(defn- commit!
  [execution-id execution]
  (cond
    (machine/suspended? execution)
    (do
      (swap! executions assoc execution-id execution)
      (swap! execution-metadata update execution-id assoc :updated-at (now-ms))
      execution)

    (machine/completed? execution)
    (retire! execution-id execution)

    :else
    (throw
     (ex-info
      "Browser choreography may only commit suspended or completed executions."
      {:error/type :gesso.live.browser.choreo/invalid-commit
       :execution-id execution-id
       :execution execution}))))

(defn- failed-drive-cleanup!
  [execution-id]
  (cancel-all-timers! execution-id)
  (swap! executions dissoc execution-id)
  (swap! execution-metadata dissoc execution-id)
  true)

(defn start!
  "Start one projected browser endpoint and drive it to await or completion.

   Required:
     :execution-id

   Optional:
     :context
     :metadata
     :now-fn
     :trace-limit
     :max-immediate-steps"
  [plan {:keys [execution-id
                context
                metadata
                now-fn
                trace-limit
                max-immediate-steps]}]
  (require-execution-id! execution-id)
  (when (active? execution-id)
    (throw
     (ex-info
      "Browser choreography execution id is already active."
      {:error/type :gesso.live.browser.choreo/duplicate-execution
       :execution-id execution-id})))
  (let [metadata' (require-map! "Browser choreography metadata"
                                (or metadata {}))]
    (swap!
     execution-metadata
     assoc
     execution-id
     (merge {:started-at (now-ms)
             :updated-at (now-ms)}
            metadata'))
    (try
      (let [execution0
            (machine/start
             plan
             (cond-> {:execution-id execution-id
                      :context (or context {})}
               now-fn (assoc :now-fn now-fn)
               trace-limit (assoc :trace-limit trace-limit)
               max-immediate-steps
               (assoc :max-immediate-steps max-immediate-steps)))
            execution1 (drive execution0 nil)]
        (commit! execution-id execution1))
      (catch :default error
        (failed-drive-cleanup! execution-id)
        (notify-error! :start execution-id error {:plan-name (:name plan)})
        (throw error)))))

(defn resume!
  "Resume one active execution with an event/message envelope and drive again.

   Late delivery to a retired/nonexistent execution returns :ignored and cannot
   resurrect it. An illegal envelope for an active execution remains an error."
  [execution-id envelope]
  (require-execution-id! execution-id)
  (if-some [current (execution execution-id)]
    (try
      (let [execution0 (machine/resume current envelope)
            execution1 (drive execution0 envelope)
            committed (commit! execution-id execution1)]
        (if (machine/completed? committed)
          {:status :completed
           :execution committed
           :result (machine/execution-result committed)}
          {:status :resumed
           :execution committed}))
      (catch :default error
        (notify-error!
         :resume
         execution-id
         error
         {:event envelope
          :state (:state current)})
        (throw error)))
    {:status :ignored
     :reason :inactive-execution
     :execution-id execution-id}))

(defn resume-event!
  "Resume with one modeled environmental event."
  ([execution-id event-id]
   (resume-event! execution-id event-id nil))
  ([execution-id event-id data]
   (resume! execution-id (machine/event event-id data))))

(defn resume-message!
  "Resume with one participant message.

   execution-id remains explicit even when payload carries correlation fields;
   transport adapters must not choose an execution from weak payload data."
  [execution-id descriptor payload]
  (resume! execution-id (machine/message descriptor payload)))

(defn accepts?
  [execution-id envelope]
  (boolean
   (when-some [current (execution execution-id)]
     (machine/accepts-event? current envelope))))

(defn accepts-event?
  [execution-id event-id]
  (accepts? execution-id (machine/event event-id)))

(defn accepts-message?
  [execution-id descriptor payload]
  (accepts? execution-id (machine/message descriptor payload)))

(defn abort!
  "Forget an execution without protocol cleanup.

   This is only a browser-process teardown escape hatch. Recoverable failures
   belong in choreography as modeled events so resources and semantic cleanup
   still run."
  [execution-id reason]
  (require-execution-id! execution-id)
  (if-some [current (execution execution-id)]
    (let [metadata' (metadata execution-id)]
      (cancel-all-timers! execution-id)
      (swap! executions dissoc execution-id)
      (swap! execution-metadata dissoc execution-id)
      (swap!
       terminal-history
       (fn [history]
         (trim-history
          (conj
           history
           {:execution-id execution-id
            :plan-name (:plan-name current)
            :role (:role current)
            :status :aborted
            :reason reason
            :state (:state current)
            :held-resources (machine/held-resources current)
            :trace (machine/execution-trace current)
            :completed-at (now-ms)
            :metadata
            (select-keys metadata'
                         [:started-at :updated-at :source :kind])})
          default-terminal-history-limit)))
      {:status :aborted
       :execution-id execution-id
       :reason reason})
    {:status :ignored
     :reason :inactive-execution
     :execution-id execution-id}))

;; -----------------------------------------------------------------------------
;; Runtime reset and diagnostics
;; -----------------------------------------------------------------------------

(defn reset-executions!
  "Abort all process-local executions/timers while preserving registrations."
  []
  (doseq [execution-id (keys @executions)]
    (cancel-all-timers! execution-id))
  (reset! executions {})
  (reset! execution-metadata {})
  (reset! timers {})
  (reset! terminal-history [])
  true)

(defn reset-runtime!
  "Clear executions and all browser choreography registrations.

   Intended for tests/hot reload or whole-runtime teardown."
  []
  (reset-executions!)
  (reset! fx-machines {})
  (reset! fx-handlers {})
  (reset! send-handler nil)
  (reset! error-handler nil)
  true)

(defn diagnostics
  []
  {:gesso.live.browser.choreo/type runtime-type
   :active-count (execution-count)
   :active (active-summaries)
   :terminal (terminal-summaries)
   :registered-fx-machines (registered-fx-machines)
   :registered-fx-handlers (registered-fx-handlers)
   :send-handler? (boolean @send-handler)
   :timer-count
   (reduce + 0 (map count (vals @timers)))})
