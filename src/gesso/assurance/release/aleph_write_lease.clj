(ns gesso.assurance.release.aleph-write-lease
  "v2272: opt-in Netty final-write lease adapter for an already-admitted,
   host-local traffic-fence Ring handler.

   The handler must be INSTALLED INSIDE a v2270 traffic-fence candidate, and
   its HTTP listener must install the returned Aleph 0.9.11 pipeline hook.
   During an admitted request, retain a continuation lease, register the
   actual Aleph request Channel with Gesso's outbound final-frame capture,
   and mark the finite, eager response producer ceased BEFORE Ring return.
   The final-frame ChannelPromise then independently establishes write
   disposition; only BOTH facts permit release of the fence continuation.

   No final frame, absent pipeline instrumentation, failed promise listener
   installation, or uncertain handler lifecycle => no invented completion.
   Terminal failure is a disposition, NOT delivery. The host must still prove
   that its selected request handler is installed on the actual Aleph server
   and that the capture is the only applicable HTTP response path. This file
   does NOT create an Aleph listener, install Biff, handle SSE/streaming bodies,
   issue a release credential, or certify arbitrary background continuations.
   Any such application work needs an independent lease and quiescence proof.
   Channel-close alone is deliberately not counted as writer termination.
   An exception or unsupported response after entering a business handler
   deliberately retains its token as an unresolved obligation, since the
   handler might have started work whose end was not observed."
  (:require [gesso.live.outbox.http-aleph-pipeline :as pipeline]
            [gesso.live.outbox.http-netty-terminal :as terminal])
  (:import [io.netty.channel Channel]
           [java.util IdentityHashMap]))

(def report-type :gesso.assurance.release.aleph-write-lease/host-local-diagnostic)
(def report-version 1)
(def ^:private byte-array-class (Class/forName "[B"))
(def ^:private response-keys #{:status :headers :body})

(defn- reject! [reason]
  (throw (ex-info "Cannot establish fence ownership for an Aleph response."
                  {:gesso.release.aleph-write-lease/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- eager-ring-response? [response]
  ;; Must be no broader than traffic-fence's synchronous result profile.
  ;; That fence still revalidates and copies mutable byte-array bodies.
  (and (plain-map? response)
       (every? response-keys (keys response))
       (integer? (:status response))
       (<= 100 (:status response) 599)
       (let [headers (:headers response)]
         (or (nil? headers)
             (and (plain-map? headers)
                  (every? (fn [[k v]] (and (string? k) (string? v)))
                          headers))))
       (let [body (:body response)]
         (or (nil? body)
             (string? body)
             (keyword? body)
             (number? body)
             (instance? byte-array-class body)))))

(defn prepare!
  "Prepare a one-host Aleph 0.9.11 final-write continuation adapter.

   Pass the EXACT issued traffic fence (with :retain-current!, :finish!, and
   :summary!) as input. Mount :http1-pipeline-transform or the independently
   qualified :http2-stream-pipeline-transform on the corresponding original
   Aleph listener. Pass (:wrap-handler result) the selected finite-response
   Ring application handler, and install THAT function as a traffic-fence
   candidate's :handler. The transform for EACH enabled HTTP protocol must
   be installed BEFORE serving; HTTP/2 requires separate real-Aleph qualification.

   The wrapped handler accepts only an active real :aleph/channel. It
   acquires a fence lease BEFORE calling business code; only immediately
   realized Ring responses can qualify for terminal-write completion. It
   registers the terminal bridge
   before returning to Aleph; thus the outbound final promise can be paired
   with the correct HTTP/1 FIFO response or HTTP/2 stream. A failed registration
   which did not enqueue any response releases the newly acquired token; once
   registered, uncertain completion retains the token (fail closed).

   Unknown handler/producer exits intentionally create unresolvable leases
   through this API; do not select this adapter for streaming or deferred
   responses. Host-owned cancellation/recovery must be proved separately.

   :stats is a time-of-observation diagnostic, not proof of release admission
   or whole-server ownership. Failed or missing final-write evidence keeps
   retirement blocked; an actual unsuccessful terminal future may complete
   the ownership obligation because the producer has already ceased, but is
   not evidence of delivery or application success."
  [fence]
  (when-not (and (plain-map? fence)
                 (fn? (:handler fence))
                 (fn? (:retain-current! fence))
                 (fn? (:finish! fence))
                 (fn? (:summary! fence)))
    (reject! :issued-continuation-fence-required))
  (let [capture (pipeline/prepare!)
        pending (IdentityHashMap.)
        counters (atom {:registered 0 :disposed 0 :succeeded 0
                        :failed 0 :aborted 0 :errors 0
                        :handler-uncertain 0})
        wrap-handler
        (fn [handler]
          (when-not (fn? handler)
            (reject! :original-ring-handler-required))
          (fn [request]
            (let [^Channel channel (:aleph/channel request)]
              (when-not (and (instance? Channel channel)
                             (.isActive channel))
                (reject! :active-aleph-request-channel-required))
              ;; Acquire the fence continuation BEFORE entering any business
              ;; code. Mounting this wrapper outside the issued fence must
              ;; refuse without running the protected handler.
              (let [token ((:retain-current! fence))
                    response (try
                               (handler request)
                               (catch Throwable error
                                 ;; Unknown post-error work cannot be treated
                                 ;; as a ceased response producer. Hold the
                                 ;; acquired lease until host recovery proves
                                 ;; it safe; no fabricated terminal callback.
                                 (swap! counters update :handler-uncertain inc)
                                 (when (instance? InterruptedException error)
                                   (.interrupt (Thread/currentThread)))
                                 (throw error)))]
                (if-not (eager-ring-response? response)
                  (do
                    ;; Traffic fence will refuse the unsupported result and
                    ;; close a closeable body. That alone does NOT prove a
                    ;; producer/continuation stopped, so retain ownership.
                    (swap! counters update :handler-uncertain inc)
                    response)
                  (let [;; Snapshot mutable bodies BEFORE claiming eager
                        ;; producer completion. Fence will independently
                        ;; copy them again on its public return boundary.
                        response (if (instance? byte-array-class (:body response))
                                   (assoc response :body (aclone ^bytes (:body response)))
                                   response)
                        bridge-ref (atom nil)
                        callback
                        (fn []
                          ;; Called by the original Netty terminal bridge
                          ;; ONLY after producer ceased AND write disposition.
                          ;; It does not take the fence's lifecycle write lock.
                          (let [outcome (:write-outcome
                                         ((:status @bridge-ref)))]
                            (try
                              ((:finish! fence) token)
                              (locking pending (.remove pending token))
                              (swap! counters (fn [s]
                                                (-> s (update :disposed inc)
                                                    (update outcome (fnil inc 0)))))
                              (catch Throwable error
                                ;; The token must remain outstanding on
                                ;; uncertain release; no synthetic success.
                                (when (instance? InterruptedException error)
                                  (.interrupt (Thread/currentThread)))
                                (swap! counters update :errors inc)))))
                        bridge (terminal/prepare! callback)
                        registered? (atom false)]
                    (reset! bridge-ref bridge)
                    (locking pending
                      (.put pending token {:channel channel :bridge bridge}))
                    (try
                      ((:register-response! capture) channel bridge)
                      (reset! registered? true)
                      ((:producer-ceased! bridge))
                      (swap! counters update :registered inc)
                      response
                      (catch Throwable error
                        ;; A known pre-registration refusal has no outbound
                        ;; response associated with its token. Once capture
                        ;; admitted the bridge, conservatively hold the lease
                        ;; until the original terminal evidence fires.
                        (when-not @registered?
                          (try
                            ((:finish! fence) token)
                            (locking pending (.remove pending token))
                            (catch Throwable finish-error
                              (when (instance? InterruptedException finish-error)
                                (.interrupt (Thread/currentThread))))))
                        (swap! counters update :errors inc)
                        (when (instance? InterruptedException error)
                          (.interrupt (Thread/currentThread)))
                        (throw error)))))))))
        stats (fn []
                {:type report-type :version report-version
                 :scope :one-host-finite-ring-aleph-write-only
                 :pending-registered (locking pending (.size pending))
                 :counts @counters
                 :capture ((:stats capture))
                 :fence ((:summary! fence))
                 :missing-final-frame :never-implies-completion
                 :channel-close :never-implies-write-disposition
                 :release-authority :none
                 :activation-eligibility :not-issued
                 :completion-claim :not-issued})]
    {:wrap-handler wrap-handler
     :http1-pipeline-transform (:http1-pipeline-transform capture)
     :http2-stream-pipeline-transform (:http2-stream-pipeline-transform capture)
     :stats stats}))
