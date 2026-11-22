(ns gesso.live.flow
  "Per-client flow construction for gesso.live.

   This namespace bridges source streams into Missionary flows and applies
   client-local policy:

   - invalidation -> live-event wrapping
   - optional stale invalidation relief/coalescing
   - conditional debug tracing

   The normal source path is indexed: flow-for-source uses source/subscribe so
   unrelated invalidations are never sent to this flow. The older broadcast tap
   path remains available for custom :interested? predicates and tests.

   It does not:
   - expand primary changes
   - publish to source
   - build SSE frames
   - render HTMX attrs
   - read XTDB

   source.clj is the hot in-process indexed source.
   flow.clj is the cold per-client recipe."
  (:require
   [gesso.live.htmx :as htmx]
   [gesso.live.progression :as progression]
   [gesso.live.schema :as schema]
   [gesso.live.source :as source]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [missionary.core :as m]))

;; -----------------------------------------------------------------------------
;; Debug
;; -----------------------------------------------------------------------------

(defn- restore-interrupt!
  "A suppressed callback interruption must not clear the current JVM thread's
   cancellation signal. User hooks may throw after an interruptible wait has
   already cleared that flag."
  [error]
  (when (instance? InterruptedException error)
    (.interrupt (Thread/currentThread))))

(defmacro ^:private debug!
  "Emit a debug event only when debug-fn is truthy.

   The data expression is inside the guard, so with debugging off it is not
   evaluated."
  [debug-fn event data]
  ;; Optional diagnostic callbacks must not alter Live delivery, stream
  ;; progress, or subscription construction when user tracing fails. This
  ;; includes Error subtypes thrown by application assertions.
  `(when-let [f# ~debug-fn]
     (try
       (f# (assoc ~data :event ~event))
       (catch Throwable error#
         (restore-interrupt! error#)
         nil))))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-event
  htmx/default-event)

(def default-options
  {:event default-event
   :data nil
   :consistency-token nil
   :relieve? true
   :debug-fn nil
   :on-close nil

   ;; Internal/test seam. Production callers should normally ignore this.
   :take! s/take!})

(def closed-sentinel
  ;; Retained for compatibility with callers that referenced this old marker.
  ;; New streams use a distinct, locally owned terminal token instead: a
  ;; public value can legitimately occur in ordinary stream data.
  (Object.))

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

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- once
  [f]
  (let [called? (atom false)]
    (fn [& args]
      (when (compare-and-set! called? false true)
        (apply f args)))))

(defn default-interested?
  "Default subscription matcher.

   A subscription is interested in an invalidation when :topic and :id match."
  [subscription invalidation]
  (= (select-keys subscription [:topic :id])
     (select-keys invalidation [:topic :id])))

(defn invalidation-key
  "Return the default coalescing/matching key for an invalidation."
  [invalidation]
  (select-keys invalidation [:topic :id]))

(defn- flow-option-contract
  [options]
  (compact-map
   (select-keys options
                [:subscription
                 :interested?])))

(defn- event-option-contract
  [options]
  (compact-map
   (select-keys options
                [:event
                 :data])))

(defn- require-fn-option!
  [options k]
  (when-let [f (get options k)]
    (when-not (fn? f)
      (throw
       (ex (str "gesso.live flow " k " must be a function.")
           {k f}))))
  options)

(defn- require-boolean-option!
  [options k]
  (let [v (get options k)]
    (when-not (or (true? v) (false? v))
      (throw
       (ex (str "gesso.live flow " k " must be a boolean.")
           {k v}))))
  options)

(defn prepare-options!
  "Merge defaults, normalize optional public options, validate contracts, and
   return the normalized option map.

   Public API note:

   :interested? is optional for callers. Internally it is normalized to
   default-interested? before schema validation so downstream code can treat it
   as present.

   :take! is an internal/test seam used by stream-flow tests to avoid mocking
   manifold.stream/take! globally."
  [options]
  (let [options'  (opts options)
        ;; Only omission requests the default matcher. An explicit nil/false
        ;; policy is invalid and must not silently become an active interest.
        options'' (if (contains? options' :interested?)
                    options'
                    (assoc options' :interested? default-interested?))]
    (schema/validate! :gesso.live/flow-for-subscription-options
                      (flow-option-contract options''))
    (schema/validate! :gesso.live/invalidation-event-options
                      (event-option-contract options''))
    (require-fn-option! options'' :debug-fn)
    (require-fn-option! options'' :on-close)
    ;; :take! is the required Manifold adapter used by every collected flow.
    ;; Explicit nil must not pass optional-callback validation and then fail
    ;; during the first pull, after an eager source interest is registered.
    (when-not (fn? (:take! options''))
      (throw
       (ex "gesso.live flow :take! must be a function."
           {:take! (:take! options'')})))
    (require-boolean-option! options'' :relieve?)
    options''))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      ;; Terminal cleanup must not be interrupted by application hooks that
      ;; throw Error rather than Exception.
      (catch Throwable error
        (restore-interrupt! error)
        nil))))

(defn- data-value
  [data invalidation]
  (cond
    (fn? data)
    (data invalidation)

    (some? data)
    data

    :else
    nil))

;; -----------------------------------------------------------------------------
;; Event wrapping
;; -----------------------------------------------------------------------------

(defn invalidation-event
  "Wrap an invalidation in the canonical live-event shape.

   Options:
     :event
       Event name/ref. Keywords and symbols are normalized via htmx/event-name.

     :data
       Optional static value or function of invalidation.

     :consistency-token
       Optional consistency token attached to the live event.

   When the invalidation carries :progression, the live event carries the same
   normalized requirement at top level. Keeping the requirement explicit on the
   event lets later transport/browser layers consume it without interpreting the
   descriptive invalidation payload."
  ([invalidation]
   (invalidation-event invalidation nil))
  ([invalidation options]
   (let [options' (opts options)
         event'   (htmx/event-name (:event options'))
         data'    (data-value (:data options') invalidation)
         token       (:consistency-token options')
         requirement (when (contains? invalidation :progression)
                       (:progression invalidation))
         value       (cond-> {:event event'
                              :invalidation invalidation}
                       (some? data')
                       (assoc :data data')

                       (contains? invalidation :progression)
                       (assoc :progression requirement)

                       (some? token)
                       (assoc :consistency-token token))]
     (schema/validate-live-event! value))))

(defn- safe-interested?
  [interested? subscription invalidation debug-fn]
  (try
    (boolean (interested? subscription invalidation))
    (catch Exception e
      (debug!
       debug-fn
       :gesso.live.flow/interested-predicate-failed
       {:subscription subscription
        :invalidation invalidation
        :error e
        :at (now-ms)})
      (throw
       (ex "gesso.live flow interested? predicate failed."
           {:subscription subscription
            :invalidation invalidation}
           e)))))

(defn- safe-event
  [invalidation options debug-fn]
  (try
    (invalidation-event invalidation options)
    (catch Exception e
      (debug!
       debug-fn
       :gesso.live.flow/event-build-failed
       {:invalidation invalidation
        :error e
        :at (now-ms)})
      (throw
       (ex "gesso.live flow failed to build live event."
           {:invalidation invalidation}
           e)))))

;; -----------------------------------------------------------------------------
;; Manifold deferred -> Missionary task
;; -----------------------------------------------------------------------------

(defn deferred-task
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
;; Manifold stream -> Missionary flow
;; -----------------------------------------------------------------------------

;; Internal failure envelopes must never be confused with application maps.
(deftype ^:private StreamFailure [error])

(defn- stream-error-value
  [error]
  (StreamFailure. error))

(defn- stream-error-value?
  [value]
  (instance? StreamFailure value))

(defn- unwrap-stream-value
  [value]
  (if (stream-error-value? value)
    (throw (.-error ^StreamFailure value))
    value))

(defn- stream-terminal-xf
  "Turn the m/observe event stream into a finite stream-flow.

   The per-stream closed-sentinel is emitted once when Manifold closes. The
   take-while stage turns that sentinel into normal flow termination.

   stream-error-value is emitted once when take! fails. The map stage unwraps
   it by throwing the wrapped exception so Missionary's downstream machinery
   sees a normal flow failure."
  [closed-sentinel]
  (comp
   (map unwrap-stream-value)
   (take-while #(not (identical? closed-sentinel %)))))

(defn- emit-safely!
  "Emit a value into m/observe.

   Returns true when emit! accepted the value. Returns false when emit! itself
   throws, which normally means the consumer is gone or not ready. In that case
   the caller should stop pulling."
  [emit! value debug-fn]
  (try
    (emit! value)
    true
    (catch Throwable e
      (debug!
       debug-fn
       :gesso.live.flow/emit-failed
       {:value value
        :error e
        :at (now-ms)})
      false)))

(defn stream-flow
  "Convert a Manifold stream into a finite Missionary discrete flow.

   This is the serial Manifold -> Missionary bridge:

   - no s/consume
   - at most one outstanding take! at a time
   - emit the current value before installing the next take!
   - emit closed-sentinel once to trigger normal flow termination
   - emit a wrapped stream error once to trigger normal flow failure
   - rely on m/observe cleanup for downstream cancellation/early termination

   Options:
     :debug-fn
     :on-close
     :take!      internal/test seam; defaults to manifold.stream/take!"
  ([stream]
   (stream-flow stream nil))
  ([stream {:keys [debug-fn on-close take!]
            :or {take! s/take!}}]
   (let [;; Each flow owns its terminal token. Even the formerly public
         ;; flow/closed-sentinel must now be treated as ordinary stream data.
         closed-sentinel (Object.)

         close-once!
         (once
          (fn []
            ;; An application callback may block. Release the underlying
            ;; Manifold stream (and its source registration) first so a
            ;; terminated flow cannot retain a live subscription while the
            ;; observer runs.
            (s/close! stream)
            (call-safely on-close)
            (debug!
             debug-fn
             :gesso.live.flow/stream-flow-closed
             {:at (now-ms)})))]

     (m/eduction
      (stream-terminal-xf closed-sentinel)

      (m/observe
       (fn [emit!]
         ;; A completed Manifold deferred can invoke its on-realized callback
         ;; inline. Calling pull! from that callback used to reenter emit! before
         ;; its preceding call returned, hanging Missionary on buffered input.
         ;; The wip counter trampolines completions without changing the
         ;; one-outstanding-take invariant or introducing a worker pool.
         (let [ready (java.util.concurrent.ConcurrentLinkedQueue.)
               wip (java.util.concurrent.atomic.AtomicInteger. 0)
               ;; An m/observe recipe can be subscribed more than once. Each
               ;; collector owns its own stopped state; only the underlying
               ;; Manifold stream and its physical close are shared.
               stopped? (atom false)]
           (letfn [(stop! []
                     (reset! stopped? true)
                     (close-once!))

                   (emit-and-stop! [value]
                     ;; Terminal completion must retire the underlying stream
                     ;; even if the downstream observer refuses the terminal
                     ;; sentinel or failure envelope. A failed emit is not a
                     ;; reason to leave the source subscription running.
                     (try
                       (emit-safely! emit! value debug-fn)
                       (finally
                         (stop!))))

                   (emit-error-and-stop! [cause]
                     (let [wrapped (ex "gesso.live stream failed."
                                       {}
                                       cause)]
                       (debug!
                        debug-fn
                        :gesso.live.flow/stream-error
                        {:error cause
                         :at (now-ms)})
                       (emit-and-stop! (stream-error-value wrapped))))

                   (pull! []
                     (when-not @stopped?
                       ;; A stream adapter may throw before returning a
                       ;; deferred. Report it through the same finite-flow
                       ;; failure path as an asynchronously failed take!.
                       ;; Otherwise a pull can disappear on the initial
                       ;; executor or leave the completion pump stranded.
                       (try
                         (let [deferred (take! stream closed-sentinel)]
                           (d/on-realized
                            deferred
                            (fn [value]
                              (enqueue! :value value))
                            (fn [cause]
                              (enqueue! :error cause))))
                         (catch Throwable cause
                           (enqueue! :error cause)))))

                   (process-completion! [kind payload]
                     (when-not @stopped?
                       (case kind
                         :error (emit-error-and-stop! payload)
                         :value (if (identical? closed-sentinel payload)
                                  (do
                                    (debug!
                                     debug-fn
                                     :gesso.live.flow/stream-closed
                                     {:at (now-ms)})
                                    (emit-and-stop! closed-sentinel))
                                  (do
                                    (debug!
                                     debug-fn
                                     :gesso.live.flow/stream-value
                                     {:value payload
                                      :at (now-ms)})
                                    ;; Refusal of an ordinary value is also a
                                    ;; terminal condition for this observer.
                                    ;; Without retiring the underlying stream,
                                    ;; its source registration remains live
                                    ;; after the bridge has stopped pulling.
                                    (if (emit-safely! emit! payload debug-fn)
                                      (pull!)
                                      (stop!)))))))

                   (drain-ready! []
                     (loop []
                       (when-let [[kind payload] (.poll ready)]
                         (process-completion! kind payload))
                       (when (pos? (.decrementAndGet wip))
                         (recur))))

                   (enqueue! [kind payload]
                     (.offer ready [kind payload])
                     (when (zero? (.getAndIncrement wip))
                       (drain-ready!)))]
             ;; Do not drain an already-ready stream while m/observe is still
             ;; installing its observer/cleanup. Only this initial pull needs
             ;; asynchronous handoff; later completions run on their callback
             ;; thread through the non-reentrant work-in-progress pump.
             (.execute (java.util.concurrent.ForkJoinPool/commonPool)
                       ^Runnable #(pull!))

             (fn cleanup []
               (stop!))))))))))

;; -----------------------------------------------------------------------------
;; Flow transforms
;; -----------------------------------------------------------------------------

(defn subscription-xf
  "Return a transducer that filters invalidations for a subscription and maps
   matching invalidations to live events.

   This is still used for flow-for-stream and for flow-for-source when a caller
   supplies a custom :interested? predicate that cannot be represented by the
   default source topic/id index."
  [{:keys [subscription interested? debug-fn] :as options}]
  (let [interested-fn (or interested? default-interested?)]
    (mapcat
     (fn [invalidation]
       (debug!
        debug-fn
        :gesso.live.flow/invalidation-received
        {:subscription subscription
         :invalidation invalidation
         :indexed? false
         :at (now-ms)})

       (if (safe-interested? interested-fn subscription invalidation debug-fn)
         (let [event (safe-event invalidation options debug-fn)]
           (debug!
            debug-fn
            :gesso.live.flow/invalidation-matched
            {:subscription subscription
             :invalidation invalidation
             :live-event event
             :at (now-ms)})
           [event])
         (do
           (debug!
            debug-fn
            :gesso.live.flow/invalidation-ignored
            {:subscription subscription
             :invalidation invalidation
             :at (now-ms)})
           []))))))

(defn indexed-subscription-xf
  "Return a transducer for indexed source streams.

   The source has already matched invalidations by topic/id, so this transducer
   only wraps each invalidation as a live event. It does not run the interested?
   predicate on the normal hot path."
  [{:keys [subscription debug-fn] :as options}]
  (map
   (fn [invalidation]
     (debug!
      debug-fn
      :gesso.live.flow/indexed-invalidation-received
      {:subscription subscription
       :invalidation invalidation
       :indexed? true
       :at (now-ms)})
     (let [event (safe-event invalidation options debug-fn)]
       (debug!
        debug-fn
        :gesso.live.flow/indexed-invalidation-matched
        {:subscription subscription
         :invalidation invalidation
         :live-event event
         :at (now-ms)})
       event))))

(defn- event-progression
  [event]
  (let [top-level?    (contains? event :progression)
        invalidation  (:invalidation event)
        nested?       (and (map? invalidation)
                           (contains? invalidation :progression))
        top-level     (:progression event)
        nested        (:progression invalidation)]
    (when (and top-level?
               nested?
               (not= top-level nested))
      (throw
       (ex "gesso.live live event progression disagrees with its invalidation."
           {:event event
            :event/progression top-level
            :invalidation/progression nested})))
    (cond
      top-level? top-level
      nested? nested
      :else nil)))

(defn coalesce-live-events
  "Collapse two pending live events for Missionary relief.

   The newer event owns ordinary descriptive fields, matching m/relieve's
   historical latest-value behavior. Authoritative progression is not
   latest-value state: every suppressed refresh requirement must survive.
   Compose the two optional requirements conservatively and write the result to
   both the live event and its nested invalidation.

   The operation is associative because ordinary fields are right-biased and
   progression/compose is associative. It never compares opaque bases or uses
   arrival order as authority ordering."
  [pending incoming]
  (let [requirement (progression/compose (event-progression pending)
                                         (event-progression incoming))
        invalidation (:invalidation incoming)
        invalidation' (cond-> invalidation
                        requirement
                        (assoc :progression requirement)

                        (nil? requirement)
                        (dissoc :progression))]
    (cond-> (assoc incoming :invalidation invalidation')
      requirement
      (assoc :progression requirement)

      (nil? requirement)
      (dissoc :progression))))

(defn relieve
  "Apply progression-safe Missionary relief/coalescing to a live-event flow.

   Missionary relief consumes upstream as fast as possible and combines pending
   values when downstream is slower. Ordinary event fields remain latest-value,
   but authoritative progression requirements are composed so relief cannot
   silently weaken the eventual refresh requirement.

   This does not cancel expensive render/data-load work. Rendered stream paths
   should use m/?<= around the render branch when newer invalidations make older
   render work stale."
  [flow]
  (m/relieve coalesce-live-events flow))

(defn maybe-relieve
  "Apply relief when :relieve? is truthy."
  [flow {:keys [relieve?]}]
  (if relieve?
    (relieve flow)
    flow))

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(defn- flow-for-stream*
  [stream options]
  (let [debug-fn (:debug-fn options)]
    (debug!
     debug-fn
     :gesso.live.flow/flow-for-stream-created
     {:subscription (:subscription options)
      :relieve? (:relieve? options)
      :indexed? false
      :at (now-ms)})

    (->> (stream-flow stream
                      {:debug-fn debug-fn
                       :on-close (:on-close options)
                       :take! (:take! options)})
         (m/eduction (subscription-xf options))
         (#(maybe-relieve % options)))))

(defn- indexed-flow-for-stream*
  [stream options]
  (let [debug-fn (:debug-fn options)]
    (debug!
     debug-fn
     :gesso.live.flow/indexed-flow-for-stream-created
     {:subscription (:subscription options)
      :relieve? (:relieve? options)
      :indexed? true
      :at (now-ms)})

    (->> (stream-flow stream
                      {:debug-fn debug-fn
                       :on-close (:on-close options)
                       :take! (:take! options)})
         (m/eduction (indexed-subscription-xf options))
         (#(maybe-relieve % options)))))

(defn flow-for-stream
  "Build a client live-event flow from a Manifold stream tap.

   This API treats the supplied stream as a broadcast stream, so it still filters
   invalidations with subscription-xf.

   Options:
     :subscription       required
     :interested?        optional predicate, defaults to topic/id equality
     :event              optional event name/ref
     :data               optional static value or fn of invalidation
     :consistency-token  optional token attached to events
     :relieve?           default true
     :debug-fn           optional pay-for-play debug hook
     :on-close           optional internal cleanup/debug hook
     :take!              internal/test seam; defaults to manifold.stream/take!

   Returns a Missionary flow of live-event maps."
  [stream options]
  (flow-for-stream* stream (prepare-options! options)))

(defn- custom-interested-supplied?
  [options]
  (contains? options :interested?))

(defn open-source-flow!
  "Open a source stream and return {:flow flow :close! release-subscription
                                  :closed? subscription-closed?}.

   The caller owns the returned close handle until it has safely handed the
   flow to an active collector. This is important for eager registrations at
   startup boundaries that can terminate before a Missionary observer starts.

   Build a client live-event flow from an opened source stream.

   The normal non-debug path uses source/subscribe, which registers the
   subscription in source.clj's topic/id interest index. Unrelated invalidations
   are not sent to this flow.

   Compatibility/debug note: callers that provide a custom :interested?
   predicate, or a :debug-fn, use the older source/changes broadcast tap path.
   Arbitrary predicates cannot be safely indexed by the generic source, and the
   existing debug tests intentionally observe ignored invalidations. Production
   callers normally do not pass :debug-fn, so they take the indexed path."
  [src options]
  (let [custom-interested? (custom-interested-supplied? options)
        options'          (prepare-options! options)
        debug-fn          (:debug-fn options')
        subscription      (:subscription options')
        debug-path?       (some? debug-fn)
        indexed?          (and (not custom-interested?)
                               (not debug-path?))
        tap               (if indexed?
                            (source/subscribe src subscription)
                            (source/changes src))
        options''         (assoc options'
                                 :on-close
                                 (fn []
                                   (call-safely (:on-close options'))
                                   (debug!
                                    debug-fn
                                    (if indexed?
                                      :gesso.live.flow/source-subscription-closed
                                      :gesso.live.flow/source-tap-closed)
                                    {:source/id (:id src)
                                     :subscription subscription
                                     :indexed? indexed?
                                     :at (now-ms)})))]
    (try
      (debug!
       debug-fn
       (if indexed?
         :gesso.live.flow/source-subscription-opened
         :gesso.live.flow/source-tap-opened)
       {:source/id (:id src)
        :subscription subscription
        :indexed? indexed?
        :at (now-ms)})

      {:flow (if indexed?
               (indexed-flow-for-stream* tap options'')
               (flow-for-stream* tap options''))
       :close! (fn [] (s/close! tap))
       ;; Per-interest physical evidence, not an aggregate source count.
       :closed? (fn [] (s/closed? tap))
       ;; The owner can observe spontaneous physical release without polling.
       ;; This closes the race between its response-disconnect callback and
       ;; the subscription's own on-closed callback.
       :on-closed! (fn [callback] (s/on-closed tap callback))}
      (catch Throwable e
        (s/close! tap)
        (throw e)))))

(defn flow-for-source
  "Open an indexed subscription (or debug broadcast tap) and return its
   Missionary Live event flow. The flow owns its stream when collected;
   use open-source-flow! at startup boundaries that need to release an
   uncollected registration if initialization fails."
  [src options]
  (:flow (open-source-flow! src options)))

(defn collect-task
  "Return a Missionary task that collects a finite flow into a vector.

   This is mostly useful for tests."
  [flow]
  (m/reduce conj [] flow))
