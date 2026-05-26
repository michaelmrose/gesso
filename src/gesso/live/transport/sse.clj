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
  `(when-let [f# ~debug-fn]
     (f# (assoc ~data :event ~event))))

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

(defn- require-positive-int-option!
  [options k]
  (when-let [v (get options k)]
    (when-not (and (integer? v) (pos? v))
      (throw
       (ex (str "gesso.live SSE " k " must be a positive integer.")
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

(defn live-event-payload
  "Return the payload serialized into the SSE data field.

   The event name itself is excluded because it becomes the SSE event field."
  [live-event]
  (let [payload (dissoc live-event :event)]
    (if (seq payload)
      payload
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

         close-once!
         (once
          (fn [reason data]
            (reset! closed? true)
            (debug!
             debug-fn
             :gesso.live.sse/closed
             (merge {:reason reason
                     :at (now-ms)}
                    data))
            (call-safely (:on-close options')
                         (merge {:reason reason
                                 :at (now-ms)}
                                data))
            (s/close! out)))

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
           (when-let [cancel! @cancel-events-ref]
             (cancel!))
           (when-let [cancel! @cancel-keepalive-ref]
             (cancel!)))]

     (debug!
      debug-fn
      :gesso.live.sse/started
      {:at (now-ms)})

     (s/on-closed
      out
      (fn []
        (when-not @closed?
          (close-once! :stream-closed {})
          (cancel-running!))))

     (when-let [keepalive-task (keepalive-task* out options')]
       (reset!
        cancel-keepalive-ref
        (keepalive-task
         (fn [_] nil)
         (fn [error]
           (when-not @cancelling?
             (fail! error)
             (close-once! :keepalive-error {:error error}))))))

     (let [events-task (write-events-task out live-events options')
           cancel-events!
           (events-task
            (fn [_]
              (debug!
               debug-fn
               :gesso.live.sse/flow-completed
               {:at (now-ms)})
              (when (:close-on-complete? options')
                (close-once! :completed {})
                (cancel-running!)))
            (fn [error]
              (if @cancelling?
                (debug!
                 debug-fn
                 :gesso.live.sse/task-cancelled
                 {:error error
                  :at (now-ms)})
                (do
                  (fail! error)
                  (close-once! :error {:error error})))))

           cancel!
           (once
            (fn []
              (close-once! :cancelled {})
              (cancel-running!)))]

       (reset! cancel-events-ref cancel-events!)

       {:stream out
        :response (response* out options')
        :cancel! cancel!
        :closed? closed?}))))

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
     (start!
      (flow/flow-for-source (:source system) flow-options')
      sse-options'))))
