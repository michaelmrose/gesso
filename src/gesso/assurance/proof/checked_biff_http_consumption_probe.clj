(ns gesso.assurance.proof.checked-biff-http-consumption-probe
  "v1780: Optional Biff 2 module testing whether a SEPARATE HTTP adapter
   actually consumes the final checked Ring handler installed through Biff's
   ordinary module start lifecycle.

   The probe wraps the checked handler AFTER original checked host adoption
   and BEFORE the chosen HTTP server module. It generates a secret, one-use
   read-only route challenge. A loopback HTTP request must reach the exact
   wrapper and return its ephemeral secret response. A copied Biff map,
   startup callback, fabricated 200 response, or server that ignores the
   final handler cannot by itself qualify.

   This is local test evidence for a selected HTTP server module only: Biff
   core 2 does not choose/install an HTTP listener for Gesso. It does not
   authenticate an external deployment or user principal, verify arbitrary
   normal routes, or authorize model transactions, retries or certificates."
  (:require [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.live.application-preflight :as app])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.util WeakHashMap UUID]
           [java.util.concurrent.atomic AtomicBoolean]))

(def module-id
  :gesso.assurance.proof.checked-biff-http-consumption-probe/install)
(def report-type
  :gesso.assurance.proof.checked-biff-http-consumption-probe/local-http-consumption)
(def report-version 1)

(deftype ConsumptionProbeHandle []
  Object
  (toString [_] "#<gesso.assurance/http-consumption-probe>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-host-check host/require-current-biff-host-adoption!)
(def ^:private original-handler-slot app/biff-ring-handler-key)

(defn- reject! [reason]
  (throw (ex-info "Final checked Biff handler HTTP consumption is not established."
                  {:checked-biff-http-consumption-probe/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-check
                             (var-get #'host/require-current-biff-host-adoption!))
                 (= original-handler-slot app/biff-ring-handler-key))
    (reject! :substituted-original-host-or-handler-slot)))

(defn- entry [handle]
  (when (instance? ConsumptionProbeHandle handle)
    (locking issued (.get issued handle))))

(defn- report [state outcome hits]
  {:type report-type :version report-version
   :scope :one-final-ring-handler-loopback-http-consumption-challenge
   :module-state (if (map? state) :started state)
   :status outcome
   :challenge-handler-observations hits
   :final-checked-handler-consumed-over-http
   (if (= :one-final-checked-wrapper-http-challenge-returned outcome)
     :observed-on-selected-loopback-adapter
     :not-established)
   :normal-http-route-consumption :not-established
   :same-request-typed-catalog-execution :not-established
   :deployed-server-identity :not-established
   :external-server-deployment :not-established
   :authenticated-principal :not-established
   :complete-writer-coverage :not-established
   :xtdb-commit-authenticity :not-established
   :actual-live-delivery :not-established
   :certificate :not-issued :admission :not-admitted
   :retry-authority :none :transaction-authority :none :authority :none})

(defn- local-base-uri! [base-url]
  ;; Don't turn a local assurance check into an outbound HTTP/SSRF client.
  ;; The port and adapter come from the *actual* started server, not the
  ;; proof report; reject user info, query, fragments, and nonloopback hosts.
  (let [uri (try (URI/create base-url)
                 (catch Exception _ (reject! :invalid-http-probe-base-url)))]
    (when-not (and (= "http" (.getScheme uri))
                   (= "127.0.0.1" (.getHost uri))
                   (<= 1 (.getPort uri) 65535)
                   (nil? (.getRawUserInfo uri))
                   (nil? (.getRawQuery uri))
                   (nil? (.getRawFragment uri))
                   (contains? #{nil "" "/"} (.getRawPath uri)))
      (reject! :nonloopback-or-unsafe-probe-target))
    uri))

(defn issue-http-consumption-probe-module!
  "Issue a one-use Biff lifecycle module. Start order is:
   application handler -> checked host -> THIS probe -> chosen HTTP server.

   The adapter must capture the final :biff.ring/handler from its own start
   input and serve it over loopback HTTP. Once that server has started, call
   (:verify-once! issued) with the FINAL returned Biff system and the real
   loopback URL. Verifies a single read-only challenge; no data write or
   ordinary operation is executed by the challenge. Normal requests delegate
   unchanged to the originally checked Ring handler."
  [host-handle contract-source]
  (pinned!)
  (when-not (and (host/checked-biff-host-adoption? host-handle)
                 (map? contract-source))
    (reject! :invalid-original-host-source))
  (let [handle (ConsumptionProbeHandle.)
        state (atom :issued)
        claimed (AtomicBoolean. false)
        result (atom :not-attempted)
        challenge-path (str "/__gesso_assurance_http_consumption/"
                            (UUID/randomUUID))
        secret-header (str (UUID/randomUUID))
        secret-response (str "gesso-consumed-" (UUID/randomUUID))
        pending (atom :inactive)
        hits (atom 0)
        module
        {:biff.core/id module-id
         :biff.core/start
         (fn [host-system]
           (when-not (compare-and-set! state :issued :starting)
             (reject! :repeated-or-stopped-consumption-probe-start))
           (pinned!)
           (let [host-report (try (original-host-check
                                   host-handle contract-source host-system)
                                  (catch Throwable error
                                    (reset! state :failed)
                                    (throw error)))
                 original (get host-system original-handler-slot)]
             (when-not (and (= host/report-type (:type host-report))
                            (= :identical-to-guarded-checked-router-wrapper
                               (:handler-slot host-report))
                            (fn? original))
               (reset! state :failed)
               (reject! :not-the-original-checked-biff-handler))
             (let [wrapped (fn [request]
                             (if (= challenge-path (:uri request))
                               (if (and (= :get (:request-method request))
                                        (= secret-header
                                           (get-in request [:headers
                                                            "x-gesso-consumption-probe"]))
                                        (compare-and-set! pending :active :seen))
                                 (do (swap! hits inc)
                                     {:status 200
                                      :headers {"content-type" "text/plain"}
                                      :body secret-response})
                                 {:status 404 :body "Not found"})
                               (original request)))
                   wrapped-system (assoc host-system original-handler-slot
                                         wrapped)]
               (when-not (compare-and-set! state :starting
                                           {:host-system host-system
                                            :wrapper wrapped})
                 (reset! state :failed)
                 (reject! :probe-installation-state-changed))
               wrapped-system)))
         :biff.core/stop
         (fn [_]
           (reset! pending :inactive)
           (reset! state :stopped))}
        current! (fn []
                   (pinned!)
                   (let [started @state]
                     (when (map? started)
                       (original-host-check host-handle contract-source
                                            (:host-system started)))
                     started))
        summary! (fn []
                   (report (current!) @result @hits))
        verify! (fn [final-system base-url]
                  (when-not (.compareAndSet claimed false true)
                    (reject! :http-consumption-probe-already-attempted))
                  (reset! result :attempt-consumed-without-confirmation)
                  (let [started (current!)]
                    (when-not (and (map? started)
                                   (map? final-system)
                                   (identical? (:wrapper started)
                                               (get final-system original-handler-slot)))
                      (reject! :final-biff-system-did-not-retain-probe))
                    (let [url (local-base-uri! base-url)]
                      (when-not (compare-and-set! pending :inactive :active)
                        (reject! :invalid-http-probe-state))
                      (try
                        (let [client (-> (HttpClient/newBuilder)
                                         (.connectTimeout (Duration/ofSeconds 3))
                                         (.build))
                              request (-> (HttpRequest/newBuilder
                                           (URI/create
                                            (str url challenge-path)))
                                          (.timeout (Duration/ofSeconds 5))
                                          (.header "x-gesso-consumption-probe"
                                                   secret-header)
                                          (.GET)
                                          (.build))
                              response (.send client request
                                              (HttpResponse$BodyHandlers/ofString))
                              qualified? (and (= 200 (.statusCode response))
                                              (= secret-response (.body response))
                                              (= :seen @pending)
                                              (= 1 @hits))]
                          (reset! result
                                  (if qualified?
                                    :one-final-checked-wrapper-http-challenge-returned
                                    :http-consumption-challenge-unconfirmed)))
                        (catch Throwable _
                          (reset! result :http-consumption-challenge-unconfirmed))
                        (finally (reset! pending :inactive)))
                      (report (current!) @result @hits))))]
    (locking issued (.put issued handle {:source contract-source}))
    {:handle handle :module module :summary! summary!
     :verify-once! verify!}))
