(ns gesso.live.transport.sse
  "SSE transport for gesso.live.

   This namespace is the transport boundary from Missionary back to Manifold:

   - consumes a Missionary flow of live-event maps
   - formats each live event as an SSE frame
   - writes frames onto a Manifold stream suitable as an Aleph response body
   - exposes cancel/close hooks for client disconnect handling
   - supports optional keepalive comments
   - supports conditional debug tracing
   - supports model-backed fragment stream startup

   It does not:
   - expand primary changes
   - filter subscriptions
   - render fragments
   - know XTDB
   - know application authorization except when explicitly using the
     model-backed fragment stream helper

   flow.clj decides which live events a client should receive.
   transport.sse decides how those events become text/event-stream bytes."
  (:require
   [clojure.string :as str]
   [gesso.live.flow :as flow]
   [gesso.live.htmx :as htmx]
   [gesso.live.model :as model]
   [gesso.live.progression :as progression]
   [gesso.live.schema :as schema]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  ;; Debugging is observational: user-provided tracing failures must not
  ;; alter stream startup, framing, delivery, or error/close handling.
  `(when-let [f# ~debug-fn]
     (try
       (f# (assoc ~data :event ~event))
       (catch Exception _#
         nil))))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def content-type
  "text/event-stream; charset=utf-8")

(def default-headers
  {"content-type" content-type
   "cache-control" "no-cache, no-transform"
   "connection" "keep-alive"
   "x-accel-buffering" "no"})

(def default-options
  {:status 200
   :headers nil
   :encoder pr-str
   :debug-fn nil
   :on-error nil
   :on-close nil
   :close-on-complete? true
   :keepalive-ms nil
   :keepalive-comment "ping"})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  ([message data]
   (ex-info message data))
  ([message data cause]
   (ex-info message data cause)))

(defn- opts
  [options]
  (merge default-options options))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live SSE " k " must be a function.")
           {k f}))))
  options)

(defn- require-boolean-option!
  [options k]
  (let [v (get options k)]
    (when-not (or (true? v) (false? v))
      (throw
       (ex (str "gesso.live SSE " k " must be a boolean.")
           {k v}))))
  options)

(defn- require-map-or-nil-option!
  [options k]
  (let [value (get options k)]
    (when-not (or (nil? value) (map? value))
      (throw
       (ex (str "gesso.live SSE " k " must be a map or nil.")
           {k value}))))
  options)

(defn- require-positive-int-option!
  [options k]
  (when-let [v (get options k)]
    ;; Missionary m/sleep and the underlying JVM timer consume a signed long.
    ;; A positive Clojure BigInt outside that range fails only after an SSE
    ;; source subscription has been allocated, potentially orphaning it.
    (when-not (and (integer? v) (pos? v) (<= v Long/MAX_VALUE))
      (throw
       (ex (str "gesso.live SSE " k
                " must be a positive integer within signed 64-bit range.")
           {k v}))))
  options)

(defn prepare-options!
  "Merge defaults and validate SSE transport options.

   This should be called at public API boundaries. Hot-path helpers receive the
   prepared options map directly."
  [options]
  (let [options' (opts options)]
    (require-fn-option! options' :encoder)
    (require-fn-option! options' :debug-fn)
    (require-fn-option! options' :on-error)
    (require-fn-option! options' :on-close)
    (require-boolean-option! options' :close-on-complete?)
    ;; response* merges response headers after starting producers; validate
    ;; them before source registration at the public SSE entry points.
    (require-map-or-nil-option! options' :headers)
    (require-positive-int-option! options' :keepalive-ms)
    options'))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn- once
  "Wrap f so it can only run once."
  [f]
  (let [called? (atom false)]
    (fn [& args]
      (when (compare-and-set! called? false true)
        (apply f args)))))

(defn- cancel-task-safely!
  "Cancellation is best-effort cleanup after SSE termination. A failing
   producer canceller must not block cancellation of the other producer or
   replace the already chosen terminal outcome."
  [cancel! task debug-fn]
  (when cancel!
    (try
      (cancel!)
      (catch Exception e
        (when (instance? InterruptedException e)
          (.interrupt (Thread/currentThread)))
        (debug!
         debug-fn
         :gesso.live.sse/physical-cancel-failed
         {:task task
          :error e
          :at (now-ms)})))))

(defn- install-canceller!
  "Publish a task cancellation handle despite concurrent terminal shutdown.

   If shutdown wins before the task returns its handle, invoke the handle
   immediately after installation. If shutdown races the install, both paths
   may try to cancel, so the installed handle must run exactly once."
  [cancel-ref cancelling? cancel-fn task debug-fn]
  (let [cancel-once! (once cancel-fn)]
    (reset! cancel-ref cancel-once!)
    (when @cancelling?
      (cancel-task-safely! cancel-once! task debug-fn))
    cancel-once!))

(defn- deferred-task
  "Adapt a Manifold deferred to a Missionary task."
  [deferred]
  (fn [success failure]
    (let [cancelled? (atom false)]
      (d/on-realized
       deferred
       (fn [value]
         (when-not @cancelled?
           (success value)))
       (fn [error]
         (when-not @cancelled?
           (failure error))))
      (fn cancel []
        (reset! cancelled? true)))))

;; -----------------------------------------------------------------------------
;; SSE frame formatting
;; -----------------------------------------------------------------------------

(defn- invalid-field-value?
  [s]
  (or (str/includes? s "\n")
      (str/includes? s "\r")))

(defn- field-value
  [field value]
  (let [value' (str value)]
    (when (invalid-field-value? value')
      (throw
       (ex "SSE field value must not contain a newline."
           {:field field
            :value value'})))
    ;; Browsers ignore an id field containing NUL. Reject such values so
    ;; a caller cannot believe it installed a reconnection cursor when it did
    ;; not. The empty string is valid and intentionally clears the cursor.
    (when (and (= :id field) (str/includes? value' "\u0000"))
      (throw
       (ex "SSE id field must not contain NUL."
           {:field field
            :value value'})))
    ;; EventSource accepts retry only as nonempty ASCII decimal digits;
    ;; negative, signed, fractional, or otherwise malformed values are
    ;; silently ignored by browsers.
    (when (and (= :retry field) (not (re-matches #"[0-9]+" value')))
      (throw
       (ex "SSE retry field must contain only ASCII digits."
           {:field field
            :value value'})))
    value'))

(defn- data-lines
  [data]
  (let [data' (str data)
        lines (str/split data' #"\r\n|\n|\r" -1)]
    (apply str
           (map #(str "data: " % "\n") lines))))

(defn comment-frame
  "Build an SSE comment frame.

   Useful for keepalives/pings.

   The returned string always ends in a blank line."
  [comment]
  (let [comment' (str comment)
        lines (str/split comment' #"\r\n|\n|\r" -1)]
    (str
     (apply str
            (map #(str ": " % "\n") lines))
     "\n")))

(defn event-frame
  "Build an SSE event frame.

   Accepted keys:
     :event   optional event name
     :id      optional event id
     :retry   optional reconnection delay in milliseconds
     :data    optional data string

   The returned string always ends in a blank line."
  [{:keys [event id retry data]}]
  (str
   (when id
     (str "id: " (field-value :id id) "\n"))
   (when event
     (str "event: " (field-value :event event) "\n"))
   (when retry
     (str "retry: " (field-value :retry retry) "\n"))
   (data-lines (or data ""))
   "\n"))

(defn- wire-progression
  [value]
  (when (some? value)
    (progression/requirement->wire value)))

(defn live-event-payload
  "Return the portable payload serialized into the SSE data field.

   The event name itself is excluded because it becomes the SSE event field.

   Internal progression requirements are converted to their explicit versioned
   wire representation at this transport boundary. The conversion is performed
   for both the top-level LiveEvent requirement and the copy carried by its
   nested invalidation. SSE does not compare bases or otherwise interpret
   progression semantics."
  [live-event]
  (let [payload (dissoc live-event :event)
        payload' (cond-> payload
                   (contains? payload :progression)
                   (update :progression wire-progression)

                   (contains? (:invalidation payload) :progression)
                   (update-in [:invalidation :progression] wire-progression))]
    (if (seq payload')
      payload'
      {})))

(defn- live-event-frame*
  [options live-event]
  (let [{:keys [encoder]} options
        live-event' (schema/validate-live-event!
                     (update live-event :event htmx/event-name))
        event-name (:event live-event')
        payload (live-event-payload live-event')
        encoded (encoder payload)]
    (event-frame
     {:event event-name
      :data encoded})))

(defn live-event-frame
  "Format a gesso.live live-event map as an SSE frame.

   The live event is validated before formatting.

   Options:
     :encoder
       Function used to encode the SSE data payload. Defaults to pr-str."
  ([live-event]
   (live-event-frame live-event nil))
  ([live-event options]
   (live-event-frame* (prepare-options! options) live-event)))

(defn frames-flow
  "Transform a flow of live-event maps into a flow of SSE frame strings."
  [live-events options]
  (let [options' (prepare-options! options)
        debug-fn (:debug-fn options')]
    (m/eduction
     (map
      (fn [live-event]
        (try
          (let [frame (live-event-frame* options' live-event)]
            (debug!
             debug-fn
             :gesso.live.sse/frame-built
             {:live-event live-event
              :frame-size (count frame)
              :at (now-ms)})
            frame)
          (catch Exception e
            (debug!
             debug-fn
             :gesso.live.sse/frame-build-failed
             {:live-event live-event
              :error e
              :at (now-ms)})
            (throw
             (ex "gesso.live SSE failed to build frame."
                 {:live-event live-event}
                 e))))))
     live-events)))

;; -----------------------------------------------------------------------------
;; Writing frames to Manifold stream
;; -----------------------------------------------------------------------------

(defn- put-frame-task*
  [out frame options]
  (let [debug-fn (:debug-fn options)]
    (m/sp
      (debug!
       debug-fn
       :gesso.live.sse/frame-write-attempted
       {:frame-size (count frame)
        :at (now-ms)})

      (let [accepted? (m/? (deferred-task (s/put! out frame)))]
        (if accepted?
          (do
            (debug!
             debug-fn
             :gesso.live.sse/frame-write-accepted
             {:frame-size (count frame)
              :at (now-ms)})
            true)
          (do
            (debug!
             debug-fn
             :gesso.live.sse/frame-write-rejected
             {:frame-size (count frame)
              :at (now-ms)})
            (throw
             (ex "gesso.live SSE output stream rejected frame."
                 {:frame-size (count frame)}))))))))

(defn put-frame-task
  "Return a task that writes one frame to a Manifold output stream.

   Fails if the output stream rejects the frame."
  [out frame options]
  (put-frame-task* out frame (prepare-options! options)))

(defn- write-frames-task*
  [out frames options]
  (m/reduce
   (fn [_ _] nil)
   nil
   (m/ap
     (let [frame (m/?> frames)]
       (m/? (put-frame-task* out frame options))))))

(defn write-frames-task
  "Return a task that drains a flow of frame strings into a Manifold stream.

   This task is intended to run for the lifetime of an SSE connection."
  [out frames options]
  (write-frames-task* out frames (prepare-options! options)))

(defn write-events-task
  "Return a task that formats and drains live-event maps into an output stream."
  [out live-events options]
  (let [options' (prepare-options! options)]
    (write-frames-task* out
                        (frames-flow live-events options')
                        options')))

(defn- keepalive-task*
  [out options]
  (let [{:keys [keepalive-ms keepalive-comment debug-fn]} options]
    (when keepalive-ms
      (m/sp
        (loop []
          (m/? (m/sleep keepalive-ms))
          (let [frame (comment-frame keepalive-comment)]
            (m/? (put-frame-task* out frame options))
            (debug!
             debug-fn
             :gesso.live.sse/keepalive-sent
             {:frame-size (count frame)
              :at (now-ms)}))
          (recur))))))

;; -----------------------------------------------------------------------------
;; Response helpers
;; -----------------------------------------------------------------------------

(defn- response*
  [stream options]
  {:status (:status options)
   :headers (merge default-headers (:headers options))
   :body stream})

(defn response
  "Build a Ring/Aleph-style SSE response map for a Manifold stream body."
  ([stream]
   (response stream nil))
  ([stream options]
   (response* stream (prepare-options! options))))

(defn start!
  "Start draining a Missionary flow of live-event maps into a Manifold stream.

   Returns:
     {:stream ...
      :response ...
      :cancel! ...
      :closed? ...}

   Call :cancel! when the client disconnects if your server layer does not close
   the returned stream automatically.

   When the output stream is externally closed, the draining task is cancelled.

   Options:
     :status
     :headers
     :encoder
     :debug-fn
     :on-error
     :on-close
     :close-on-complete?
     :keepalive-ms
     :keepalive-comment"
  ([live-events]
   (start! live-events nil))
  ([live-events options]
   (let [options' (prepare-options! options)
         debug-fn (:debug-fn options')
         out (s/stream)
         closed? (atom false)
         cancelling? (atom false)
         cancel-events-ref (atom nil)
         cancel-keepalive-ref (atom nil)

         fail!
         (fn [error]
           (debug!
            debug-fn
            :gesso.live.sse/failed
            {:error error
             :at (now-ms)})
           (call-safely (:on-error options')
                        error
                        {:at (now-ms)}))

         cancel-running!
         (fn []
           (reset! cancelling? true)
           ;; Both producers must be retired independently. A physical
           ;; cancellation callback is user/executor code and may throw.
           (cancel-task-safely! @cancel-events-ref :events debug-fn)
           (cancel-task-safely! @cancel-keepalive-ref :keepalive debug-fn))

         close-once!
         (once
          (fn [reason data]
            ;; A terminal outcome owns the physical stream and producer
            ;; lifecycle. Retire them BEFORE notifying external application
            ;; hooks: a slow or reentrant callback must not hold a terminal
            ;; connection open or leave its producers running.
            (reset! closed? true)
            (s/close! out)
            (cancel-running!)
            (when (#{:error :keepalive-error :startup-error} reason)
              (fail! (:error data)))
            (debug!
             debug-fn
             :gesso.live.sse/closed
             (merge {:reason reason
                     :at (now-ms)}
                    data))
            (call-safely (:on-close options')
                         (merge {:reason reason
                                 :at (now-ms)}
                                data))))

         fail-startup!
         (fn [error]
           ;; The producer may fail synchronously before returning a
           ;; cancellation handle. Start still owns the output stream and any
           ;; producer already installed, so retire both and return a closed
           ;; response rather than throwing away the only cleanup handle.
           ;; Do not report a second error after an earlier terminal event.
           (close-once! :startup-error {:error error}))]

     (debug!
      debug-fn
      :gesso.live.sse/started
      {:at (now-ms)})

     (s/on-closed
      out
      (fn []
        (when-not @closed?
          (close-once! :stream-closed {}))))

     (try
       (when-let [keepalive-task (keepalive-task* out options')]
         (install-canceller!
        cancel-keepalive-ref
        cancelling?
        (keepalive-task
         (fn [_] nil)
         (fn [error]
           ;; Once close-once! has published terminal state, later
           ;; producer failures are cleanup noise even if the cancellation
           ;; flag has not yet been set (for example during :on-close).
           (when-not (or @cancelling? @closed?)
             (close-once! :keepalive-error {:error error}))))
        :keepalive
        debug-fn))
       (catch Exception e
         (fail-startup! e)))

     (let [cancel-events!
           ;; A synchronous keepalive failure or downstream disconnect may
           ;; terminate the stream before event source registration.
           ;; An in-progress installation remains protected by the late
           ;; cancellation handoff below.
           (try
             (when-not (or @cancelling? @closed? (s/closed? out))
               (let [events-task (write-events-task out live-events options')]
               (events-task
                (fn [_]
                  (debug!
                   debug-fn
                   :gesso.live.sse/flow-completed
                   {:at (now-ms)})
                  (when (:close-on-complete? options')
                    (close-once! :completed {})))
                (fn [error]
                  (if (or @cancelling? @closed?)
                    (debug!
                     debug-fn
                     :gesso.live.sse/task-cancelled
                     {:error error
                      :at (now-ms)})
                    (do
                      (close-once! :error {:error error})))))))
             (catch Exception e
               (fail-startup! e)
               nil))

           cancel!
           (once
            (fn []
              (close-once! :cancelled {})))]

       (when cancel-events!
         (install-canceller! cancel-events-ref cancelling? cancel-events!
                             :events debug-fn))

       {:stream out
        :response (response* out options')
        :cancel! cancel!
        :closed? closed?}))))

(defn start-source!
  "Start a Live source-backed SSE stream, releasing its eager subscription
   if transport initialization fails or terminates before collection.

   A source flow registers its Manifold consumer during construction, before
   start! installs a Missionary task. An early terminal SSE result is therefore
   not sufficient to close an unobserved source flow on its own."
  [src flow-options sse-options]
  (let [options' (prepare-options! sse-options)
        {live-events :flow release-flow! :close!}
        (flow/open-source-flow! src flow-options)]
    (try
      (let [started (start! live-events options')]
        ;; The eager source registration remains ours until the transport
        ;; actually terminates. The event collector might never start (or
        ;; might be cancelled before observing the flow), so ordinary SSE
        ;; shutdown must also release it, not just synchronous startup errors.
        ;; Manifold invokes on-closed for streams already closed, preserving
        ;; the handoff if shutdown races with this callback registration.
        (s/on-closed (:stream started) release-flow!)
        (when (true? @(:closed? started))
          (release-flow!))
        started)
      (catch Throwable e
        (release-flow!)
        (throw e)))))

(defn cancel!
  "Cancel a started SSE stream returned by start!."
  [started]
  ((:cancel! started)))

(defn closed?
  "Return true if a started SSE stream has been closed/cancelled/completed."
  [started]
  (true? @(:closed? started)))

;; -----------------------------------------------------------------------------
;; Model-backed fragment streams
;; -----------------------------------------------------------------------------

(defn- with-inherited-debug
  [system options]
  (let [options' (or options {})
        debug-fn (get-in system [:options :debug-fn])]
    (cond-> options'
      (and debug-fn
           (not (contains? options' :debug-fn)))
      (assoc :debug-fn debug-fn))))

(defn start-fragment-stream!
  "Start an SSE stream for a compiled model fragment.

   This is the model-backed equivalent of core/start-sse!, but it lives at the
   lower transport layer so core may safely re-export it without creating a
   dependency cycle.

   It:

   - derives the runtime subscription from compiled model fragment metadata
   - checks the model scope authorization policy
   - builds the Missionary live-event flow from the system source
   - starts the SSE transport with start!

   Options:
     :flow-options
       Passed to flow/flow-for-source.

     :sse-options
       Passed to start!.

   Returns the map from start!:

     {:stream ...
      :response ...
      :cancel! ...
      :closed? ...}"
  ([system compiled ctx fragment-name id]
   (start-fragment-stream! system compiled ctx fragment-name id nil))
  ([system compiled ctx fragment-name id {:keys [flow-options sse-options]}]
   (let [scope-name    (model/fragment-scope-name compiled fragment-name)
         subscription  (model/fragment-scope-instance compiled fragment-name id)
         flow-options' (-> (with-inherited-debug system flow-options)
                           (assoc :subscription subscription))
         sse-options'  (with-inherited-debug system sse-options)]
     (model/require-scope-authorized! compiled ctx scope-name id)
     ;; The source flow registers its subscription eagerly. Validate SSE
     ;; options before that allocation; rejected startup must not retain a
     ;; source tap that no transport task will ever consume or cancel.
     (start-source! (:source system) flow-options' sse-options'))))
