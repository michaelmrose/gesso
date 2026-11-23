(ns gesso.assurance.release.traffic-fence
  "v2270: An enforcing, host-local Ring fence with explicit continuation leases.

   This is a narrow runtime MECHANISM for R5.1 Part XI: no business handler
   can run through the issued Ring entrypoint before an explicitly qualified
   compare-and-activate transition. A single immutable handler/guard binding
   is selected atomically. Protected requests pin that binding under a read
   lease until the synchronous Ring invocation finishes; activation, retirement
   and closure hold the exclusive write lease. A stale candidate cannot
   overwrite a newer selected generation or retire its replacement.

   The host supplies :qualify!, the authoritative release-eligibility CHECK.
   Its provenance is a TRUSTED PREMISE, not authenticated in this file. A
   successful callback is not proof of candidate admission, installed Biff
   adoption, writer coverage, browser assets, compatible overlap, or release
   authority. Until an independent Gesso issuer connects the original source
   and physical release gates, this remains an opt-in host mechanism. The
   returned Ring handler MUST actually be installed as the only public
   protected entrypoint; this file cannot discover bypass routes or workers.

   The default Ring return path refuses delayed/streaming response bodies.
   A handler can explicitly pin host-owned continuation work through
   :retain-current!, then call :finish! on that opaque lease when it has
   REALLY ended. Retirement and closure first block new traffic and then
   wait a bounded interval for those registered continuations; an incomplete
   drain FAILS CLOSED and remains retryable, never reports success. These
   leases cannot discover unregistered background work, prove byte delivery,
   or automatically close SSE. Honest caller completion remains a premise.
   No rollback of committed effects or reuse of past request authorization."
  (:import [java.util IdentityHashMap]
           [java.util.concurrent.locks ReentrantReadWriteLock]))

(def report-type :gesso.assurance.release.traffic-fence/host-local-state)
(def report-version 1)
(def ^:private max-staged 64)
(def ^:private max-generations 65536)
(def ^:private max-retained-continuations 4096)
(def ^:private default-drain-timeout-ms 5000)
(def ^:private unavailable
  {:status 503
   :headers {"cache-control" "no-store"}
   :body "Protected application traffic is unavailable"})

(deftype ^:private ContinuationToken []
  Object
  (toString [_] "#<gesso.release/opaque-continuation>"))

(deftype ^:private CandidateToken []
  Object
  (toString [_] "#<gesso.release/opaque-staged-candidate>"))

(defn- reject! [reason]
  (throw (ex-info "Host-local traffic fence cannot perform this transition."
                  {:gesso.release.traffic-fence/rejection true :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- qualifier-input! [input]
  (when-not (and (plain-map? input)
                 (contains? #{#{:qualify!} #{:qualify! :drain-timeout-ms}}
                            (set (keys input)))
                 (fn? (:qualify! input))
                 (or (not (contains? input :drain-timeout-ms))
                     (and (integer? (:drain-timeout-ms input))
                          (<= 1 (:drain-timeout-ms input) 60000))))
    (reject! :trusted-host-qualifier-required))
  (:qualify! input))

(defn- candidate-input! [candidate]
  (when-not (and (plain-map? candidate)
                 (= #{:generation :handler :guard!} (set (keys candidate)))
                 (qualified-keyword? (:generation candidate))
                 (fn? (:handler candidate))
                 (fn? (:guard! candidate)))
    (reject! :invalid-immutable-dispatch-candidate))
  candidate)

(def ^:private eager-response-keys #{:status :headers :body})
(def ^:private byte-array-class (Class/forName "[B"))

(defn- eager-ring-result? [response]
  ;; A Ring response that can realize additional behavior AFTER the handler
  ;; returns does not belong to a synchronous read lease. The narrow opt-in
  ;; profile accepts only plain, immediately materialized status/headers/body
  ;; (no callback, session, file, stream or lazy body). Even this profile does
  ;; NOT establish Netty/socket completion after the handler has returned.
  (and (plain-map? response)
       (every? eager-response-keys (keys response))
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

(defn- refuse-opaque-result [response]
  (when-let [body (and (map? response) (:body response))]
    (when (instance? java.lang.AutoCloseable body)
      (try (.close ^java.lang.AutoCloseable body)
           (catch Throwable error
             (when (instance? InterruptedException error)
               (.interrupt (Thread/currentThread)))))))
  unavailable)

(defn issue-local-ring-traffic-fence!
  "Issue one opaque host-local Ring dispatcher with explicit continuation pins.

   :prepare! stages an immutable candidate, capturing the CURRENT epoch.
   :activate! atomically selects that candidate only if its epoch is current
   and the host's independent :qualify! returns exactly true for that precise
   handler/guard pair (their object references appear in the qualifier input). Qualification
   failure consumes the staged token and keeps the previous binding (whose
   own per-request guard still independently controls its usability).
   Both staged and historically used generation inventories are bounded.
   :retain-current! may be invoked only by the currently executing admitted
   Ring handler. It returns an opaque continuation token. Any thread may call
   :finish! ONCE on that token after the registered operation really ends.
   Continuations are bounded and do not grant authority to start NEW effects.
   :retire! and :close! stop new admission before waiting for those tokens;
   timeout or interrupt leaves a non-serving draining/closing state, to be
   retried with the same active token or :close!. Synchronous requests drain
   under the read/write lock before lifecycle transitions take the write lease.
   Late/deferred response bodies are denied instead of being mistaken for
   completed synchronous work. The host must explicitly account for any
   work launched before return; this cannot detect hidden background tasks.

   The host MUST NOT call activation from arbitrary application requests.
   None of these methods is a source-bound release credential."
  [input]
  (let [qualify! (qualifier-input! input)
        drain-timeout-ms (long (or (:drain-timeout-ms input)
                                   default-drain-timeout-ms))
        ^ReentrantReadWriteLock lock (ReentrantReadWriteLock. true)
        candidates (IdentityHashMap.)
        ^Object continuation-monitor (Object.)
        continuations (IdentityHashMap.)
        ^ThreadLocal request-owner (ThreadLocal.)
        state (atom {:phase :inactive :epoch 0 :active nil :used #{}})
        with-write (fn [f]
                     (when (pos? (.getReadHoldCount lock))
                       (reject! :lifecycle-transition-during-protected-request))
                     ;; A host qualification callback is executed under the
                     ;; write lease. Reentrant close/retire/prepare within it
                     ;; would invalidate the checked epoch mid-activation.
                     (when (.isWriteLockedByCurrentThread lock)
                       (reject! :reentrant-lifecycle-transition))
                     (let [l (.writeLock lock)]
                       (.lock l)
                       (try (f) (finally (.unlock l)))))
        with-read (fn [f]
                    (let [l (.readLock lock)]
                      (.lock l)
                      (try (f) (finally (.unlock l)))))
        drain! (fn []
                 (let [deadline (+ (System/nanoTime)
                                   (* 1000000 drain-timeout-ms))]
                   (locking continuation-monitor
                     (loop []
                       (when (pos? (.size continuations))
                         (let [remaining (- deadline (System/nanoTime))]
                           (when-not (pos? remaining)
                             (reject! :registered-continuation-drain-timeout))
                           (try
                             (.wait continuation-monitor
                                    (max 1 (long (quot (+ remaining 999999)
                                                       1000000))))
                             (catch InterruptedException _
                               (.interrupt (Thread/currentThread))
                               (reject! :registered-continuation-drain-interrupted)))
                           (recur)))))))
        summary (fn []
                  (let [{:keys [phase epoch active]} @state]
                    {:type report-type :version report-version
                     :phase phase :epoch epoch
                     :selected-generation (:generation active)
                     :staged-count (.size candidates)
                     :retained-continuations (locking continuation-monitor
                                               (.size continuations))
                     :scope :one-host-synchronous-ring-entrypoint-only
                     :host-qualifier-provenance :trusted-host-callback-conditional
                     :real-biff-installation :not-established
                     :async-response-drain :registered-continuations-only
                     :unregistered-transferred-work :not-established
                     :physical-wire-completion :not-established
                     :release-authority :none
                     :installation-eligibility :not-issued
                     :activation-eligibility :not-issued
                     :completion-claim :not-issued
                     :runtime-effect-permission :none}))]
    {:handler
     (fn [request]
       ;; No business call may be entered while an exclusive lifecycle lease
       ;; is held by the same thread (including reentrant qualification).
       (if (.isWriteLockedByCurrentThread lock)
         unavailable
         (with-read
           (fn []
             (let [{:keys [phase active]} @state]
               (if (and (= :active phase)
                        active
                        (true? (try ((:guard! active) request)
                                    (catch Throwable error
                                      (when (instance? InterruptedException error)
                                        (.interrupt (Thread/currentThread)))
                                      false))))
                 (let [previous-owner (.get request-owner)]
                   (.set request-owner (:token active))
                   (try
                     (let [response ((:handler active) request)]
                       (if (eager-ring-result? response)
                         ;; Netty may read bytes after the lexical Ring call.
                         ;; Do not expose a mutable application-owned array.
                         (if (instance? byte-array-class (:body response))
                           (assoc response :body (aclone ^bytes (:body response)))
                           response)
                         (refuse-opaque-result response)))
                     (finally
                       (if previous-owner
                         (.set request-owner previous-owner)
                         (.remove request-owner)))))
                 unavailable))))))

     :retain-current!
     (fn []
       (let [owner (.get request-owner)
             {:keys [phase active]} @state]
         (when-not (and owner (= :active phase)
                        (identical? owner (:token active))
                        (pos? (.getReadHoldCount lock)))
           (reject! :no-current-admitted-request))
         (locking continuation-monitor
           (when (>= (.size continuations) max-retained-continuations)
             (reject! :too-many-registered-continuations))
           (let [token (ContinuationToken.)]
             (.put continuations token owner)
             token))))

     :finish!
     (fn [token]
       ;; Crucially, no read/write lease: a closing thread may be waiting
       ;; under the exclusive lease for an asynchronous completion thread.
       (locking continuation-monitor
         (when-not (and (instance? ContinuationToken token)
                        (.containsKey continuations token))
           (reject! :unknown-or-already-finished-continuation))
         (.remove continuations token)
         (.notifyAll continuation-monitor)
         {:status :finished :authority :none}))

     :prepare!
     (fn [candidate]
       (with-write
         (fn []
           (let [candidate (candidate-input! candidate)
                 {:keys [phase epoch used]} @state
                 generation (:generation candidate)]
             (when (contains? #{:closing :closed} phase)
               (reject! :permanently-closed))
             (when (= :draining phase)
               (reject! :previous-generation-still-draining))
             (when (or (contains? used generation)
                       (some #(= generation (:generation %))
                             (iterator-seq (.iterator (.values candidates)))))
               (reject! :reused-or-already-staged-generation))
             (when (>= (.size candidates) max-staged)
               (reject! :too-many-staged-generations))
             (let [token (CandidateToken.)]
               (.put candidates token (assoc candidate :expected-epoch epoch))
               token)))))

     :cancel!
     (fn [token]
       (with-write
         (fn []
           (when-not (and (instance? CandidateToken token)
                          (.containsKey candidates token))
             (reject! :unknown-or-consumed-candidate))
           (.remove candidates token)
           {:status :cancelled :authority :none})))

     :activate!
     (fn [token]
       (with-write
         (fn []
           (when-not (and (instance? CandidateToken token)
                          (.containsKey candidates token))
             (reject! :unknown-or-consumed-candidate))
           ;; Consume the token BEFORE calling untrusted host integration,
           ;; including when qualification fails or throws.
           (let [candidate (.remove candidates token)
                 {:keys [phase epoch active used]} @state
                 generation (:generation candidate)]
             (when (contains? #{:closing :closed} phase)
               (reject! :permanently-closed))
             (when (= :draining phase)
               (reject! :previous-generation-still-draining))
             (when-not (= epoch (:expected-epoch candidate))
               (reject! :stale-predecessor-generation))
             (when (contains? used generation)
               (reject! :reused-generation))
             (when (locking continuation-monitor
                     (pos? (.size continuations)))
               (reject! :registered-continuations-prevent-replacement))
             (when (>= (count used) max-generations)
               (reject! :generation-inventory-exhausted))
             (when (= epoch Long/MAX_VALUE)
               (reject! :generation-counter-exhausted))
             (let [qualification
                   (try {:status :observed
                         :result (qualify! {:candidate-generation generation
                                            ;; The callback must inspect the
                                            ;; ACTUAL functions, not only labels.
                                            :candidate-handler (:handler candidate)
                                            :candidate-guard! (:guard! candidate)
                                            :previous-generation (:generation active)
                                            :expected-epoch epoch
                                            :scope :one-host-synchronous-ring-entrypoint-only})}
                        (catch Throwable error
                          (when (instance? InterruptedException error)
                            (.interrupt (Thread/currentThread)))
                          {:status :unavailable}))]
               (when-not (= :observed (:status qualification))
                 (reject! :qualification-unavailable))
               (when-not (true? (:result qualification))
                 (reject! :qualification-not-established))
               ;; Exactly ONE atomic binding replacement under the write
               ;; lease. Epoch increments even if the public label repeats.
               (reset! state {:phase :active :epoch (inc epoch)
                              :active (assoc candidate :token token)
                              :used (conj used generation)})
               {:status :host-selected-only
                :generation generation :epoch (inc epoch)
                :release-authority :none
                :activation-eligibility :not-issued})))))

     :retire!
     (fn [token]
       (with-write
         (fn []
           (let [{:keys [phase epoch active] :as now} @state]
             (when-not (and (contains? #{:active :draining} phase)
                            (instance? CandidateToken token)
                            (identical? token (:token active)))
               (reject! :not-the-current-active-generation))
             (when (= epoch Long/MAX_VALUE)
               (reject! :generation-counter-exhausted))
             ;; Deny new requests BEFORE waiting, and retain the old token
             ;; if a bounded drain times out. Retry only with that token.
             (reset! state (assoc now :phase :draining))
             (drain!)
             (reset! state (assoc now :phase :inactive :epoch (inc epoch)
                                  :active nil))
             {:status :retired :generation (:generation active)
              :authority :none}))))

     :close!
     (fn []
       (with-write
         (fn []
           (when-not (= :closed (:phase @state))
             ;; Incomplete closure is NOT success. This phase refuses new
             ;; traffic; retry after the owner finishes its continuation.
             (.clear candidates)
             (swap! state assoc :phase :closing)
             (drain!)
             (swap! state assoc :phase :closed :active nil))
           {:status :closed :authority :none})))

     :summary! (fn [] (with-read summary))}))
