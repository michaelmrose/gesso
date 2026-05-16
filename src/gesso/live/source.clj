(ns gesso.live.source
  "In-process invalidation source for gesso.live.

   A source is an app-local broadcaster of already-expanded invalidations.

   It does not:
   - expand primary changes
   - know app rules
   - inspect database writes
   - know about HTMX
   - know about SSE
   - know about XTDB

   Writers/app helpers emit expanded invalidations into the source.
   Consumers call changes to receive their own Manifold stream tap of future
   invalidations.

   Manifold is intentionally used here as the async substrate. The thing this
   namespace avoids exposing is an app-facing bus/matcher/subscriber model.

   Source-level coalescing is deferred. :coalesce-window-ms is accepted and
   reported as reserved metadata, but source.clj does not coalesce emissions."
  (:require
   [gesso.live.schema :as schema]
   [manifold.deferred :as d]
   [manifold.stream :as s])
  (:import
   [java.util.concurrent ConcurrentHashMap]
   [java.util.concurrent.atomic AtomicLong LongAdder]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-options
  {:id nil
   :coalesce-window-ms nil
   :on-error nil})

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- now-ms
  []
  (System/currentTimeMillis))

(defn- ex
  [message data]
  (ex-info message data))

(defn- opts
  [options]
  (merge default-options options))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- source-option-contract
  [options]
  (compact-map
   (select-keys options
                [:id
                 :coalesce-window-ms
                 :on-error])))

(defn- validate-options!
  [options]
  (schema/validate! :gesso.live/source-options
                    (source-option-contract options))
  options)

(defn- source-id
  [options]
  (or (:id options)
      (random-uuid)))

(defn- call-safely
  [f & args]
  (when f
    (try
      (apply f args)
      (catch Exception _
        nil))))

(defn closed?
  "Return true if source has been closed."
  [source]
  (true? @(:closed? source)))

(defn- ensure-open!
  [source]
  (when (closed? source)
    (throw
     (ex "gesso.live source is closed."
         {:source/id (:id source)})))
  source)

(defn- validate-invalidation!
  [invalidation]
  (schema/validate-invalidation! invalidation))

(defn- next-tap-id!
  [source]
  (.incrementAndGet ^AtomicLong (:tap-counter source)))

(defn- remove-tap!
  [source tap-id]
  (.remove ^ConcurrentHashMap (:taps source) tap-id)
  nil)

(defn- remove-tap-if-same!
  [source tap-id stream]
  (.remove ^ConcurrentHashMap (:taps source) tap-id stream)
  nil)

(defn- tap-count
  [source]
  (.size ^ConcurrentHashMap (:taps source)))

;; -----------------------------------------------------------------------------
;; Error/rejection recording
;; -----------------------------------------------------------------------------

(defn errors
  "Return recorded source put errors/rejections."
  [source]
  @(:errors source))

(defn clear-errors!
  "Clear recorded source errors."
  [source]
  (reset! (:errors source) [])
  :cleared)

(defn- record-error!
  [source entry]
  (let [entry' (assoc entry
                      :source/id (:id source)
                      :recorded-at (now-ms))]
    (swap! (:errors source) conj entry')
    (call-safely (:on-error source) entry')
    entry'))

(defn- record-put-error!
  [source tap-id invalidation error]
  (record-error!
   source
   {:kind :put-error
    :tap-id tap-id
    :invalidation invalidation
    :error error}))

(defn- record-put-rejected!
  [source tap-id invalidation]
  (record-error!
   source
   {:kind :put-rejected
    :tap-id tap-id
    :invalidation invalidation}))

;; -----------------------------------------------------------------------------
;; Tap helpers
;; -----------------------------------------------------------------------------

(defn- track-put-result!
  [source tap-id stream invalidation put-result]
  (d/on-realized
   put-result
   (fn [accepted?]
     (when-not accepted?
       (remove-tap-if-same! source tap-id stream)
       (record-put-rejected! source tap-id invalidation)))
   (fn [error]
     (remove-tap-if-same! source tap-id stream)
     (record-put-error! source tap-id invalidation error)))
  nil)

(defn- put-tap!
  [source tap-id stream invalidation]
  (cond
    (s/closed? stream)
    (do
      (remove-tap-if-same! source tap-id stream)
      false)

    :else
    (try
      (let [put-result (s/put! stream invalidation)]
        (track-put-result! source tap-id stream invalidation put-result)
        true)
      (catch Throwable e
        (remove-tap-if-same! source tap-id stream)
        (record-put-error! source tap-id invalidation e)
        false))))

;; -----------------------------------------------------------------------------
;; Creation/lifecycle
;; -----------------------------------------------------------------------------

(defn create
  "Create an in-process invalidation source.

   Options:
     :id
       Optional source id.

     :coalesce-window-ms
       Reserved/deferred metadata. Source-level coalescing is not implemented in
       this namespace yet. Client/flow coalescing belongs in gesso.live.flow for
       the first implementation.

     :on-error
       Optional function called with source error entries.

   The initial source implementation broadcasts every emitted invalidation to
   each active tap."
  ([] (create nil))
  ([options]
   (let [options' (validate-options! (opts options))]
     {:id (source-id options')
      :created-at (now-ms)
      :coalesce-window-ms (:coalesce-window-ms options')
      :on-error (:on-error options')
      :closed? (atom false)
      :tap-counter (AtomicLong. 0)
      :taps (ConcurrentHashMap.)
      :emitted-count (LongAdder.)
      :attempted-count (LongAdder.)
      :errors (atom [])})))

(defn close!
  "Close source and all active taps.

   Idempotent."
  [source]
  (when (compare-and-set! (:closed? source) false true)
    (let [taps ^ConcurrentHashMap (:taps source)]
      (doseq [stream (.values taps)]
        (s/close! stream))
      (.clear taps)))
  :closed)

(defn stats
  "Return a small source status map."
  [source]
  {:id (:id source)
   :created-at (:created-at source)
   :closed? (closed? source)
   :tap-count (tap-count source)
   :emitted-count (.sum ^LongAdder (:emitted-count source))
   :attempted-count (.sum ^LongAdder (:attempted-count source))
   :error-count (count @(:errors source))
   :coalesce-window-ms (:coalesce-window-ms source)})

;; -----------------------------------------------------------------------------
;; Taps
;; -----------------------------------------------------------------------------

(defn changes
  "Create a new Manifold stream tap of future expanded invalidations.

   This does not create a new source. It only registers one consumer stream
   against the existing source.

   This is the handoff point from the hot source to gesso.live.flow. App code
   normally should not manipulate this stream directly."
  [source]
  (ensure-open! source)
  (let [tap-id (next-tap-id! source)
        stream (s/stream)
        taps ^ConcurrentHashMap (:taps source)]
    (.put taps tap-id stream)
    (s/on-closed stream #(remove-tap-if-same! source tap-id stream))
    stream))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(defn emit!
  "Emit one already-expanded invalidation into source.

   source/emit! validates against :gesso.live/invalidation, not
   :gesso.live/primary-change.

   Manifold put! is asynchronous. The returned :attempted count means the source
   attempted to put the invalidation onto open tap streams. It does not mean the
   consumer has synchronously processed the value.

   Returns:

     {:status :emitted
      :source/id ...
      :tap-count ...
      :attempted ...
      :invalidation ...}"
  [source invalidation]
  (ensure-open! source)
  (let [invalidation' (validate-invalidation! invalidation)
        taps ^ConcurrentHashMap (:taps source)
        attempted
        (loop [attempted 0
               it (.iterator (.entrySet taps))]
          (if (.hasNext it)
            (let [entry (.next it)
                  tap-id (.getKey entry)
                  stream (.getValue entry)]
              (recur
               (if (put-tap! source tap-id stream invalidation')
                 (inc attempted)
                 attempted)
               it))
            attempted))]
    (.increment ^LongAdder (:emitted-count source))
    (.add ^LongAdder (:attempted-count source) attempted)
    {:status :emitted
     :source/id (:id source)
     :tap-count (.size taps)
     :attempted attempted
     :invalidation invalidation'}))

(defn emit-many!
  "Emit many already-expanded invalidations into source.

   All invalidations are validated before emission starts, preventing ordinary
   malformed-input partial emission.

   This operation is not atomic with respect to concurrent close!. A concurrent
   close! may stop later emissions from occurring."
  [source invalidations]
  (ensure-open! source)
  (let [invalidations' (mapv validate-invalidation! invalidations)
        results (mapv #(emit! source %) invalidations')]
    {:status :emitted-many
     :source/id (:id source)
     :count (count invalidations')
     :results results}))
