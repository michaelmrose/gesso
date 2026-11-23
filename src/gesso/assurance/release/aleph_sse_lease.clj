(ns gesso.assurance.release.aleph-sse-lease
  "Opt-in, per-session handoff from one admitted Ring SSE request to the
   existing managed Gesso Live route, actual Aleph Channel, and host-local
   traffic-fence continuation lease.

   Configure :on-session-admitted! as the managed SSE route's synchronous
   installation callback. Invoke the route handler INSIDE the fence's selected
   business handler; do not call this adapter from a second, unguarded route.
   The route callback supplies its ORIGINAL session (not an app-manufactured
   success report), including the route-owned :cancel! and :route-released?.
   The adapter retains the fence before registering the precise response,
   attaches Gesso's physical Aleph disconnect/recovery owner, and binds its
   existing Netty final-write observer before the Ring response is returned.

   On channel closure the original Aleph hook cancels the route session off
   the Netty event loop. Only after the actual response stream, eager Live
   source and managed route capacity slot are released does that hook admit
   writer abort and producer quiescence. The original Netty terminal bridge
   then completes the fence continuation. Failed cleanup, absent channel
   closure, missing final-write instrumentation, and incomplete route release
   KEEP THE LEASE OUTSTANDING. A host-owned route recovery scheduler must run
   the returned reconciler's :after-step! when earlier cancellation failed.

   This does NOT install Aleph itself, authenticate arbitrary application
   code, discover unregistered work, prove network delivery, or issue Gesso
   release/activation authority. The host must separately drain the route,
   reconciler, listener, and fence before stopping the Live/XTDB providers.
   A failed admission hook can leave an intentionally retained continuation:
   it must never be mistaken for successful shutdown."
  (:require [clojure.string :as str]
            [gesso.live.outbox.http-aleph-live :as aleph-live]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [gesso.assurance.release.traffic-fence :as traffic-fence]
            [manifold.stream :as stream])
  (:import [io.netty.channel Channel ChannelPipeline]
           [java.util.concurrent Executor]))

(def report-type :gesso.assurance.release.aleph-sse-lease/host-local)
(def report-version 1)

(def ^:private original-attach aleph-live/attach!)
(def ^:private original-terminal terminal/prepare!)

(defn- reject! [reason]
  (throw (ex-info "The selected SSE response cannot acquire a verified release lease."
                  {:gesso.release.aleph-sse-lease/rejection true
                   :reason reason})))

(defn- valid-fence? [fence]
  (and (map? fence)
       (every? #(fn? (get fence %))
               [:retain-current! :permit-current-sse! :finish! :summary!])
       (= traffic-fence/report-type (:type ((:summary! fence))))
       (= traffic-fence/report-version (:version ((:summary! fence))))))

(defn- valid-capture? [capture]
  (and (map? capture)
       (every? #(fn? (get capture %))
               [:register-response! :http1-pipeline-transform
                :http2-stream-pipeline-transform :stats])))

(defn- plain-map? [value]
  (and (or (instance? clojure.lang.PersistentArrayMap value)
           (instance? clojure.lang.PersistentHashMap value))
       (nil? (meta value))))

(defn- valid-selected-response? [response]
  ;; Preflight the narrow v2276 fence envelope BEFORE retaining a lease.
  ;; A malformed host-controlled SSE response must not consume capacity or
  ;; install a Netty observation that the fence will inevitably reject.
  (and (plain-map? response)
       (every? #{:status :headers :body} (keys response))
       (= 200 (:status response))
       (plain-map? (:headers response))
       (every? (fn [[k v]] (and (string? k) (string? v)))
               (:headers response))
       (some (fn [[k v]]
               (and (= "content-type" (str/lower-case k))
                    (boolean (re-matches #"(?i)text/event-stream(?:\s*;.*)?" v))))
             (:headers response))
       (stream/stream? (:body response))))

(defn- valid-session? [session]
  (and (map? session)
       (fn? (:cancel! session))
       (fn? (:source-closed? session))
       (fn? (:route-released? session))
       (stream/stream? (:stream session))
       (not (stream/closed? (:stream session)))
       (map? (:response session))
       (identical? (:stream session) (get-in session [:response :body]))
       (valid-selected-response? (:response session))))

(defn- capture-mounted? [^Channel channel]
  ;; Netty pipeline observation is mandatory for this selected profile.
  ;; Merely holding a callback map does not prove that the channel has a
  ;; physical final-write observer or that close can clear capture queues.
  ;; Aleph 0.9.11 uses a named per-channel outbound observer for HTTP/1 and
  ;; an independent stream channel for HTTP/2. Naming alone does not prove
  ;; the transform came from THIS capture object: the host still owns that
  ;; installation-to-identity correspondence obligation.
  (let [^ChannelPipeline pipe (.pipeline channel)]
    (or (some? (.get pipe "gesso-final-http1"))
        (some? (.get pipe "gesso-final-http2")))))

(defn issue!
  "Bind a traffic fence to one original Aleph/Live source-release mechanism.

   Input: {:fence issued-fence, :capture original pipeline observer,
           :executor bounded off-Netty Executor,
           :reconciler original aleph-live/prepare-reconciler! result}.

   Returned :on-session-admitted! is supplied to http/wrap-authorized-sse's
   :on-session-admitted! option. Call the route's :handler INSIDE the fence's
   selected handler; the callback must be executed during that SAME Ring
   request, and the returned response must be the original session response.
   Pass the original Aleph request (with its actual :aleph/channel).

   The expected Aleph capture handler MUST be installed on the actual
   Channel at callback time. The module checks that named handler before
   retaining a lease; the host must still establish that the handler belongs
   to the supplied capture instance, not another identically named adapter.
   Use the reconciler's :after-step! in the route recovery scheduler. No
   callback supplied by an HTTP client is an authority source.

   The raw bridge and lease are NOT returned to callers: only the original
   Aleph owner may certify aborted writes and producer quiescence. The hook is
   registered with the bounded reconciler before the route returns. Terminal
   callbacks are idempotent and a failed fence completion is retryable only
   after the original bridge has observed terminal physical evidence."
  [{:keys [fence capture executor reconciler] :as options}]
  (when-not (and (map? options)
                 (= #{:fence :capture :executor :reconciler} (set (keys options)))
                 (valid-fence? fence)
                 (valid-capture? capture)
                 (instance? Executor executor)
                 (map? reconciler)
                 (fn? (:register! reconciler))
                 (fn? (:after-step! reconciler))
                 (fn? (:pending-count reconciler))
                 (identical? original-attach (var-get #'aleph-live/attach!))
                 (identical? original-terminal (var-get #'terminal/prepare!)))
    (reject! :invalid-or-substituted-original-owners))
  (let [attempts (atom 0)
        bound (atom 0)
        errors (atom 0)]
    {:on-session-admitted!
     (fn [request session]
       (when-not (and (map? request)
                      (instance? Channel (:aleph/channel request))
                      (.isActive ^Channel (:aleph/channel request))
                      (not (.isDone (.closeFuture ^Channel (:aleph/channel request))))
                      (valid-session? session)
                      (identical? original-attach (var-get #'aleph-live/attach!))
                      (identical? original-terminal (var-get #'terminal/prepare!)))
         (reject! :missing-original-channel-or-managed-source-session))
       (when-not (capture-mounted? (:aleph/channel request))
         (reject! :physical-aleph-final-write-hook-not-installed))
       (swap! attempts inc)
       ;; This is the FIRST ownership transfer, before any callback that can
       ;; race a physical disconnect. A later failure must not falsely release
       ;; an unverified source or in-flight response.
       (let [lease ((:retain-current! fence))
             finished? (atom false)
             finish! (fn []
                       (locking finished?
                         (when-not @finished?
                           ((:finish! fence) lease)
                           (reset! finished? true))))
             bridge (original-terminal finish!)]
         (try
           (let [hook (original-attach
                       {:channel (:aleph/channel request)
                        :session session :bridge bridge :executor executor})
                 ;; A bridge invokes its terminal callback at most once. If
                 ;; that callback failed before removing the fence lease, a
                 ;; later route-owned reconciliation MUST be able to retry
                 ;; completion, but only when the ORIGINAL bridge is terminal.
                 owned-hook
                 {:status (:status hook)
                  :reconcile!
                  (fn []
                    (let [outcome ((:reconcile! hook))]
                      (when (and (= :released outcome)
                                 (true? (:terminal? ((:status bridge)))))
                        (finish!))
                      outcome))}]
             ;; Register the recovery owner and actual outbound observation
             ;; BEFORE permitting the response to leave the Ring handler.
             (when-not (uuid? ((:register! reconciler) owned-hook))
               (reject! :reconciliation-owner-not-registered))
             (when-not (= :registered
                          ((:register-response! capture)
                           (:aleph/channel request) bridge))
               (reject! :physical-write-observer-not-registered))
             (when-not (= :current-request-sse-registered
                          (:status ((:permit-current-sse! fence)
                                    lease (:response session))))
               (reject! :exact-fence-sse-permit-not-established))
             (swap! bound inc)
             ;; Do not expose the raw terminal bridge: its producer-ceased!
             ;; and writer-aborted! methods are only legal in the original
             ;; physical owner, NOT arbitrary adapter consumers.
             {:type report-type :version report-version
              :status :registered
              :physical-owner :original-aleph-route-reconciler
              :release-authority :none
              :activation-eligibility :not-issued})
           (catch Throwable error
             (swap! errors inc)
             (when (instance? InterruptedException error)
               (.interrupt (Thread/currentThread)))
             ;; The route will invoke its OWN rollback. It may fail, or the
             ;; channel may still have pending work. Never free the lease
             ;; merely because registration raised an exception.
             (throw error)))))
     :stats (fn [] {:type report-type :version report-version
                    :scope :one-managed-sse-session-physical-disconnect
                    :attempted @attempts :registered @bound
                    :admission-errors @errors
                    :unresolved-recovery ((:pending-count reconciler))
                    :fence ((:summary! fence))
                    :release-authority :none
                    :activation-eligibility :not-issued})}))
