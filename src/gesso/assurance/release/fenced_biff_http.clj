(ns gesso.assurance.release.fenced-biff-http
  "v2286: Fail-closed bounded original managed-cleanup recovery for the host-local traffic fence at the actual
   selected Biff adapter's HTTP dispatch point, not merely beside Biff.

   An issued original checked Biff/XTDB startup must complete its real HTTP
   source/route/liveness checks BEFORE ordinary protected Ring traffic can
   enter. The final checked Ring wrapper passes to the selected raw adapter
   behind the v2267 traffic fence; the returned Biff system retains the exact
   original checked wrapper, as required by the original Biff proof chain.

   Original *private, read-only* consumption/liveness and route-observation
   probes are allowed through before fence activation, and while the selected
   host is running. After shutdown begins, even private probes MUST be
   refused: an unconfirmed physical stop may leave the original socket live,
   and no probe marker is an authority to bypass a retired fence.
   Reserved path ownership
   for the consumption nonce prefix is a host-source assumption: an otherwise
   uncertified application route that mimics that reserved UUID path is not
   independently ruled out by the installed catalog evidence. The original final
   wrapper intercepts them and cannot dispatch the business handler. All
   other requests are refused until a fresh source-and-physical installation
   assessment qualifies the exact handler, guard and current fence epoch.
   An additional real-loopback negative challenge verifies that the selected
   adapter actually serves the fence before enabling normal traffic.

   Shutdown must not claim :stopped unless BOTH the traffic fence has
   drained/closed and the original managed Biff stop explicitly confirms its
   own :stopped outcome. An incomplete drain, throwing stop callback or
   unconfirmed managed cleanup leaves this wrapper :cleanup-unconfirmed,
   permanently denying traffic. A retry may attempt cleanup again but the
   original lower-layer issuer may retain unconfirmed terminal state; a
   wrapper retry alone does not establish physical recovery.

   v2289: Private source probes enter the original final checked handler
   ONLY during :starting or :running, under a private read lease. Stop
   acquires the exclusive lease, draining those probes before physical
   cleanup. The previously unconditional bypass allowed a crafted assurance
   header to invoke that handler even after the
   fence was closed but an original listener survived a failed stop. Post-stop
   and retrying traffic now always enters the permanently closed fence.

   v2286: Opt-in :retry-cleanup! consumes ONE attempt after a confirmed fence
   close and an explicitly unconfirmed original managed stop. It delegates
   ONLY to the installed-source original verified retry boundary, then checks
   the same installed-source state and closed fence again. It never restarts
   the listener, changes selected handler identity, or reopens traffic. Host
   idempotence and original-listener shutdown observation are explicit inputs;
   their truth is not independently proved by this outer wrapper.

   This is opt-in LOCAL qualification of ONE selected synchronous HTTP
   entrypoint, NOT authenticated deployment, complete route/source coverage,
   release eligibility, multi-generation overlap, async/SSE drain or release
   authority. Host-supplied callbacks, exclusive public ingress, and the
   original Biff/probe implementation remain explicit premises. No arbitrary
   host code may install another unguarded port through this issuer."
  (:require [clojure.string :as str]
            [gesso.assurance.release.installed-source :as installed]
            [gesso.assurance.release.traffic-fence :as fence]
            [gesso.live.application-preflight :as app])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.util UUID]
           [java.util.concurrent.locks ReentrantReadWriteLock]))

(def report-type :gesso.assurance.release.fenced-biff-http/local-physical-fence)
(def report-version 1)
(def ^:private blocked-body "Protected application traffic is unavailable")
(def ^:private probe-prefix "/__gesso_release_traffic_fence/")
(def ^:private consumption-prefix "/__gesso_assurance_http_consumption/")
(def ^:private liveness-prefix "/__gesso_assurance_http_liveness/")
(def ^:private route-probe-header "x-gesso-operation-route-probe")
(def ^:private liveness-probe-header "x-gesso-local-liveness-probe")
(def ^:private consumption-probe-header "x-gesso-consumption-probe")
(def ^:private generation :gesso.release.fenced-biff-http/one-selected-local-generation)

(defn- reject! [reason]
  (throw (ex-info "Original Biff traffic fence is not current or installed."
                  {:gesso.release.fenced-biff-http/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- original-private-probe? [request]
  (let [headers (:headers request)
        uri (:uri request)
        consumption? (and (= :get (:request-method request))
                          (string? uri)
                          (str/starts-with? uri consumption-prefix)
                          ;; The original issuer creates this precise shape,
                          ;; including a fresh UUID. Never allow an arbitrary
                          ;; GET into the reserved prefix merely by its path.
                          (boolean
                           (re-matches
                            #"/__gesso_assurance_http_consumption/[0-9a-fA-F-]{36}"
                            uri))
                          (contains? headers consumption-probe-header))]
    (and (map? headers)
         (or consumption?
             ;; Unlike the consumption marker, both of these markers are
             ;; unconditionally intercepted by the original final wrapper,
             ;; even when the path or nonce is wrong. They cannot reach its
             ;; business branch before activation.
             (contains? headers liveness-probe-header)
             (contains? headers route-probe-header)))))

(defn- loopback-origin! [value]
  (let [uri (try (URI/create value) (catch Throwable _ nil))]
    (when-not (and uri (= "http" (.getScheme uri))
                   (= "127.0.0.1" (.getHost uri))
                   (pos? (.getPort uri))
                   (<= (.getPort uri) 65535)
                   (contains? #{nil "" "/"} (.getRawPath uri))
                   (every? nil? [(.getRawQuery uri) (.getRawFragment uri)
                                 (.getRawUserInfo uri)]))
      (reject! :unqualified-loopback-origin))
    (str "http://127.0.0.1:" (.getPort uri))))

(defn- require-physical-fence! [origin path marker-hit]
  ;; On the actual selected listening socket, a private otherwise-unrouted
  ;; request MUST traverse our fence and be refused before activation.
  ;; A synthetic 503 from another handler cannot increment marker-hit.
  (let [before @marker-hit
        request (-> (HttpRequest/newBuilder (URI/create (str origin path)))
                    (.timeout (Duration/ofSeconds 5))
                    (.GET)
                    (.build))
        response (try
                   (.send (HttpClient/newHttpClient) request
                          (HttpResponse$BodyHandlers/ofString))
                   (catch Throwable _ nil))]
    (when-not (and response
                   (= 503 (.statusCode response))
                   (= blocked-body (.body response))
                   (= (inc before) @marker-hit))
      (reject! :original-selected-listener-did-not-serve-inactive-fence))
    true))

(defn issue-fenced-checked-biff-http!
  "Issue a ONE-USE checked Biff/XTDB HTTP startup with a physically served
   traffic fence. The original host supplies a selected catalog factory,
   original configured HTTP source (including :listener-instance!), and a
   synchronous per-request authorization guard. The original source remains
   in charge of route, command and principal semantics.

   :start-once! performs the original Biff startup, proves the *served* fence
   denied a real loopback request while inactive, rechecks candidate and
   installation against the currently running Biff instance, then atomically
   enables the exact final checked handler through the traffic fence. Failed
   startup closes the fence and attempts original physical cleanup. There is
   no independent host :activate! callback or reactivation after retirement.

   Explicit recovery requires BOTH :idempotent-stop-retry? true and the
   :listener-shutdown-confirmed? callback in configured-source. Successful
   recovery is an original managed cleanup observation, not a release claim.

   The single selected raw adapter MUST use the supplied Ring handler to
   serve its physical listener; source callbacks and external listeners are
   not independently authenticated by this wrapper. The startup result and
   summary are non-authoritative diagnostics."
  [catalog! configured-source request-guard!]
  (when-not (and (fn? catalog!)
                 (fn? request-guard!)
                 (plain-map? configured-source)
                 (let [required #{:contract-source :adapter-module :base-url!
                                  :listener-instance! :typed-trial}
                       keys (set (keys configured-source))]
                   (or (= required keys)
                       (and (= (conj required :idempotent-stop-retry?
                                     :listener-shutdown-confirmed?) keys)
                            (true? (:idempotent-stop-retry? configured-source))
                            (fn? (:listener-shutdown-confirmed? configured-source)))))
                 (fn? (:base-url! configured-source))
                 (fn? (:listener-instance! configured-source)))
    (reject! :invalid-original-fenced-biff-input))
  (let [raw (:adapter-module configured-source)]
    (when-not (and (plain-map? raw)
                   (= #{:biff.core/id :biff.core/start :biff.core/stop}
                      (set (keys raw)))
                   (qualified-keyword? (:biff.core/id raw))
                   (fn? (:biff.core/start raw))
                   (fn? (:biff.core/stop raw)))
      (reject! :invalid-original-selected-adapter))
    (let [monitor (Object.)
          ^ReentrantReadWriteLock probe-lock (ReentrantReadWriteLock. true)
          status (atom :issued)
          selected-handler (atom nil)
          wire-handler (atom nil)
          marker-hit (atom 0)
          raw-system (atom nil)
          guard-function (atom nil)
          issued (atom nil)
          last-stop-result (atom nil)
          cleanup-retry-attempted? (atom false)
          retry-enabled? (true? (:idempotent-stop-retry? configured-source))
          proof-path (str probe-prefix (UUID/randomUUID))
          fenced (fence/issue-local-ring-traffic-fence!
                  {:qualify!
                   (fn [{:keys [candidate-generation candidate-handler
                                candidate-guard! previous-generation
                                expected-epoch scope]}]
                     (and (= :starting @status)
                          (= generation candidate-generation)
                          (= :one-host-synchronous-ring-entrypoint-only scope)
                          (= 0 expected-epoch)
                          (nil? previous-generation)
                          (identical? candidate-handler @selected-handler)
                          (identical? candidate-guard! @guard-function)
                          (let [assessment ((:assess-current-installation! @issued))]
                            (and (= installed/local-assessment-type (:type assessment))
                                 (= :selected-local-source-and-installation-checks-only
                                    (:status assessment))
                                 (= :checks-passed-only
                                    (get-in assessment [:release-diagnostic :decisions
                                                        :candidate :status]))
                                 (= :checks-passed-only
                                    (get-in assessment [:release-diagnostic :decisions
                                                        :installation :status]))
                                 (= :blocked
                                    (get-in assessment [:release-diagnostic :decisions
                                                        :activation :status]))
                                 (= :none (:release-authority assessment))
                                 (= :not-issued (:installation-eligibility assessment)
                                    (:activation-eligibility assessment))))))})
          protected-guard (fn [request]
                            (and (= :running @status)
                                 (true? (request-guard! request))))
          wrapped-adapter
          (assoc raw
                 :biff.core/start
                 (fn [system]
                   (let [original (get system app/biff-ring-handler-key)]
                     (when-not (and (= :starting @status)
                                    (fn? original)
                                    (nil? @selected-handler))
                       (reject! :original-final-checked-handler-unavailable))
                     (reset! selected-handler original)
                     (let [wire (fn [request]
                                  (let [private? (original-private-probe? request)
                                        read (when private? (.readLock probe-lock))]
                                    ;; Starting/running private probes acquire
                                    ;; a read lease through the original call.
                                    ;; Stop must drain all reads before cleanup.
                                    (when read (.lock read))
                                    (try
                                      (if (and private?
                                               (contains? #{:starting :running}
                                                          @status))
                                        (original request)
                                        (do
                                          (when (= proof-path (:uri request))
                                            (swap! marker-hit inc))
                                          ((:handler fenced) request)))
                                      (finally
                                        (when read (.unlock read))))))
                           out ((:biff.core/start raw)
                                (assoc system app/biff-ring-handler-key wire))]
                       (when-not (and (map? out)
                                      (identical? wire
                                                  (get out app/biff-ring-handler-key)))
                         (reject! :original-adapter-did-not-retain-wire-handler))
                       (reset! raw-system out)
                       (reset! wire-handler wire)
                       (assoc out app/biff-ring-handler-key original))))
                 :biff.core/stop
                 (fn [system]
                   ;; Close admission before the physical listener is stopped.
                   ;; Already-admitted synchronous requests are drained.
                   ((:close! fenced))
                   ((:biff.core/stop raw) system)))
          wrapped-config (assoc configured-source :adapter-module wrapped-adapter)
          local-issued (installed/issue-selected-installed-catalog!
                        catalog! wrapped-config)]
      (reset! guard-function protected-guard)
      (reset! issued local-issued)
      (letfn [(stop! []
                ;; A trusted original probe may not upgrade its read lease
                ;; into exclusive shutdown from within the same thread.
                (when (pos? (.getReadHoldCount probe-lock))
                  (reject! :fenced-biff-stop-during-private-probe))
                (locking monitor
                  (when (contains? #{:stopping :retrying-cleanup} @status)
                    (reject! :fenced-biff-stop-in-progress))
                  (if (= :stopped @status)
                    {:status :stopped :authority :none}
                    (do
                      ;; Drain any original private probes under an exclusive
                      ;; lease, publish :stopping, then perform the physical
                      ;; cleanup. No later private probe may bypass the fence.
                      (let [write (.writeLock probe-lock)]
                        (.lock write)
                        (try (reset! status :stopping)
                             (finally (.unlock write))))
                      (let [outcome
                            (try
                              (let [closed ((:close! fenced))]
                                (if-not (and (= :closed (:status closed))
                                             (= :none (:authority closed)))
                                  {:status :cleanup-unconfirmed
                                   :reason :fence-drain-not-confirmed
                                   :authority :none}
                                  (let [managed ((:stop! local-issued))]
                                    (if (and (= :stopped (:status managed))
                                             (= :none (:authority managed)))
                                      {:status :stopped :authority :none}
                                      {:status :cleanup-unconfirmed
                                       :reason :original-managed-stop-not-confirmed
                                       :authority :none}))))
                              (catch Throwable error
                                (when (instance? InterruptedException error)
                                  (.interrupt (Thread/currentThread)))
                                {:status :cleanup-unconfirmed
                                 :reason :stop-callback-failed
                                 :error-class (.getName (class error))
                                 :authority :none}))]
                        (reset! status (:status outcome))
                        (reset! last-stop-result outcome)
                        outcome)))))
              (retry-cleanup! []
                (locking monitor
                  (when-not retry-enabled?
                    (reject! :fenced-biff-cleanup-retry-not-enabled))
                  (when @cleanup-retry-attempted?
                    (reject! :fenced-biff-cleanup-retry-already-attempted))
                  (when-not (and (= :cleanup-unconfirmed @status)
                                 (= :original-managed-stop-not-confirmed
                                    (:reason @last-stop-result)))
                    (reject! :fenced-biff-managed-cleanup-not-retryable))
                  ;; The fence's own terminal state is prerequisite, never
                  ;; something that a successful physical stop may replace.
                  ;; A stream or request that failed to drain cannot recover
                  ;; through this lower-level Biff stop retry.
                  (when-not (= :closed (:phase ((:summary! fenced))))
                    (reject! :fenced-biff-traffic-drain-not-confirmed))
                  (reset! cleanup-retry-attempted? true)
                  (reset! status :retrying-cleanup)
                  (let [outcome
                        (try
                          (let [before ((:summary! local-issued))]
                            (if-not (and (= :cleanup-unconfirmed (:status before))
                                         (= :explicit-host-idempotence
                                            (:cleanup-retry-policy before))
                                         (false? (:cleanup-retry-attempted? before))
                                         (= :none (:release-authority before))
                                         (fn? (:retry-cleanup! local-issued)))
                              {:status :cleanup-unconfirmed
                               :reason :original-installed-cleanup-not-retryable
                               :authority :none}
                              (let [returned ((:retry-cleanup! local-issued))
                                    after ((:summary! local-issued))
                                    fence-after ((:summary! fenced))]
                                (if (and (= :stopped (:status returned))
                                         (= :none (:authority returned))
                                         (= :stopped (:status after))
                                         (= :explicit-host-idempotence
                                            (:cleanup-retry-policy after))
                                         (true? (:cleanup-retry-attempted? after))
                                         (= (:selected-source-fingerprint before)
                                            (:selected-source-fingerprint after))
                                         (= :none (:release-authority after))
                                         (= :closed (:phase fence-after)))
                                  {:status :stopped :authority :none}
                                  {:status :cleanup-unconfirmed
                                   :reason :original-installed-retry-not-confirmed
                                   :authority :none}))))
                          (catch Throwable error
                            (when (instance? InterruptedException error)
                              (.interrupt (Thread/currentThread)))
                            {:status :cleanup-unconfirmed
                             :reason :original-installed-retry-failed
                             :error-class (.getName (class error))
                             :authority :none}))]
                    (reset! status (:status outcome))
                    (reset! last-stop-result outcome)
                    outcome)))
              (start-once! [system]
                (locking monitor
                  (when-not (= :issued @status)
                    (reject! :fenced-biff-start-not-once-only))
                  (reset! status :starting)
                  (try
                    ((:start-once! local-issued) system)
                    (when-not (and (fn? @selected-handler)
                                   (fn? @wire-handler))
                      (reject! :original-selected-wire-handler-not-installed))
                    (let [origin (loopback-origin!
                                  ((:base-url! configured-source) @raw-system))]
                      (require-physical-fence! origin proof-path marker-hit))
                    (let [token ((:prepare! fenced)
                                 {:generation generation
                                  :handler @selected-handler
                                  :guard! protected-guard})]
                      ((:activate! fenced) token))
                    (reset! status :running)
                    {:type report-type :version report-version
                     :status :one-original-biff-listener-host-locally-fenced
                     :selected-generation generation
                     :source-and-local-installation-checks :passed-conditional
                     :physical-inactive-fence :observed-on-original-listener
                     :release-authority :none
                     :installation-eligibility :not-issued
                     :activation-eligibility :not-issued
                     :completion-claim :not-issued}
                    (catch Throwable error
                      (try (stop!) (catch Throwable _
                                    (reset! status :cleanup-unconfirmed)))
                      (throw error)))))
              (summary! []
                (locking monitor
                  {:type report-type :version report-version
                   :status @status
                   :stop-result @last-stop-result
                   :cleanup-retry-policy (if retry-enabled?
                                           :explicit-host-idempotence
                                           :one-shot-only)
                   :cleanup-retry-attempted? @cleanup-retry-attempted?
                   :traffic-fence ((:summary! fenced))
                   :release-authority :none
                   :installation-eligibility :not-issued
                   :activation-eligibility :not-issued
                   :completion-claim :not-issued}))]
        {:start-once! start-once! :stop! stop!
         :retry-cleanup! retry-cleanup! :summary! summary!}))))
