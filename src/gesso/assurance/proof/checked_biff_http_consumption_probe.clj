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
            [gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial
             :as typed-trial]
            [gesso.assurance.proof.checked-biff-request-principal :as principal]
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
(def ^:private original-typed-issuer
  typed-trial/issue-biff-issued-catalog-canonical-trial!)
(def ^:private original-principal-issuer
  principal/issue-request-principal-gate!)
(def ^:private original-handler-slot app/biff-ring-handler-key)

(defn- reject! [reason]
  (throw (ex-info "Final checked Biff handler HTTP consumption is not established."
                  {:checked-biff-http-consumption-probe/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-check
                             (var-get #'host/require-current-biff-host-adoption!))
                 (identical? original-typed-issuer
                             (var-get #'typed-trial/issue-biff-issued-catalog-canonical-trial!))
                 (identical? original-principal-issuer
                             (var-get #'principal/issue-request-principal-gate!))
                 (= original-handler-slot app/biff-ring-handler-key))
    (reject! :substituted-original-host-or-handler-slot)))

(defn- entry [handle]
  (when (instance? ConsumptionProbeHandle handle)
    (locking issued (.get issued handle))))

(defn- report [state outcome hits trial-enabled? typed-result typed-hits
               principal-enabled? principal-rejections principal-admissions]
  {:type report-type :version report-version
   :scope :one-final-ring-handler-loopback-http-consumption-challenge
   :module-state (if (map? state) :started state)
   :status outcome
   :challenge-handler-observations hits
   :final-checked-handler-consumed-over-http
   (if (= :one-final-checked-wrapper-http-challenge-returned outcome)
     :observed-on-selected-loopback-adapter
     :not-established)
   :normal-http-route-consumption
   (if (and (= outcome :one-final-checked-wrapper-http-challenge-returned)
            (= typed-result :bounded-biff-issued-catalog-atomic-acknowledgment))
     :one-selected-tested-route-through-final-wrapper
     :not-established)
   :same-request-typed-catalog-execution
   (if (and (= outcome :one-final-checked-wrapper-http-challenge-returned)
            (= typed-result :bounded-biff-issued-catalog-atomic-acknowledgment))
     :one-original-catalog-atomic-ack-in-selected-wrapper-call
     :not-established)
   :typed-trial-enabled trial-enabled?
   :typed-trial-attempts typed-hits
   :typed-trial-last-outcome typed-result
   :request-principal-gate
   (if principal-enabled?
     :host-acquired-grant-must-match-pinned-typed-owner
     :not-established)
   :principal-request-denials principal-rejections
   :principal-request-grants principal-admissions
   :host-principal-acquisition-authenticity :not-established
   :principal-policy-catalog-equivalence :not-established
   :csrf-protection :not-established
   :selected-adapter-typed-atomic-correspondence
   (if (and (= outcome :one-final-checked-wrapper-http-challenge-returned)
            (= typed-result :bounded-biff-issued-catalog-atomic-acknowledgment)
            (= 1 typed-hits))
     :bounded-challenge-and-one-typed-canonical-execution
     :not-established)
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
  ([host-handle contract-source]
   (issue-http-consumption-probe-module! host-handle contract-source nil))
  ([host-handle contract-source typed-config]
  (pinned!)
  (when-not (or (nil? typed-config)
                (and (map? typed-config) (nil? (meta typed-config))
                     (contains? #{#{:catalog-source :operation :inputs}
                                  #{:catalog-source :operation :inputs :principal-policy}}
                                (set (keys typed-config)))
                     (map? (:catalog-source typed-config))
                     (map? (:catalog (:catalog-source typed-config)))
                     (qualified-keyword? (:operation typed-config))
                     (map? (:inputs typed-config))))
    (reject! :invalid-typed-adapter-trial-source))
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
        typed-attempts (atom 0)
        typed-outcome (atom :not-established)
        principal-denials (atom 0)
        principal-admissions (atom 0)
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
             ;; Issue the original catalog-bound trial against the checked
             ;; ORIGINAL host-system, before any adapter captures the wrapper.
             (let [selected (when typed-config
                              (try
                                {:principal (when-let [policy (:principal-policy typed-config)]
                                              (original-principal-issuer
                                               policy (:operation typed-config)
                                               (:inputs typed-config)
                                               (:catalog-source typed-config)))
                                 :issued (original-typed-issuer
                                          host-handle contract-source host-system
                                          (:catalog-source typed-config)
                                          (:operation typed-config)
                                          (:inputs typed-config))
                                 :route-keys (set (map (juxt :method :path)
                                                       (get-in (app/operation-summary
                                                                (get-in contract-source
                                                                        [:application-assembly
                                                                         :operation-acquisition-assembly]))
                                                               [(:operation typed-config)
                                                                :routes])))}
                                (catch Throwable error
                                  (reset! state :failed)
                                  (throw error))))
                   wrapped (fn [request]
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
                               (if (and selected
                                        (contains? (:route-keys selected)
                                                   [(:request-method request)
                                                    (:uri request)]))
                                 (if (not= :one-final-checked-wrapper-http-challenge-returned
                                            @result)
                                   {:status 503 :body "Local adapter challenge required"}
                                   (let [authorization (when-let [gate (:principal selected)]
                                                         (try
                                                           ((:check-request! gate) request)
                                                           (catch Throwable _
                                                             {:status :principal-check-uncertain
                                                              :http-status 500})))
                                         admitted? (or (nil? authorization)
                                                       (= :grant-and-typed-owner-matched
                                                          (:status authorization)))]
                                     (if-not admitted?
                                       (do (swap! principal-denials inc)
                                           {:status (:http-status authorization)
                                            :body "Request principal not admitted"})
                                       (do
                                         (when authorization
                                           (swap! principal-admissions inc))
                                         (swap! typed-attempts inc)
                                         (let [observed (try
                                                      ((:observe-once! (:issued selected)) request)
                                                      (catch Throwable _ nil))
                                           qualified? (and
                                                       (= typed-trial/report-type (:type observed))
                                                       (= typed-trial/report-version (:version observed))
                                                       (= (:operation typed-config)
                                                          (:operation-id observed))
                                                       (= :one-original-adopted-biff-handler-returned
                                                          (:host-handler-return observed))
                                                       (= :one-original-issued-executor-returned
                                                          (:certified-executor-return observed))
                                                       (= :one-source-relative-model-transaction-acknowledged
                                                          (:canonical-physical-execution observed))
                                                       (= :bounded-biff-issued-catalog-atomic-acknowledgment
                                                          (:status observed))
                                                       (= :not-issued (:certificate observed))
                                                       (= :none (:authority observed)
                                                          (:retry-authority observed)))
                                           status (if qualified?
                                                    :bounded-biff-issued-catalog-atomic-acknowledgment
                                                    (if (and (= typed-trial/report-type
                                                                (:type observed))
                                                             (= typed-trial/report-version
                                                                (:version observed)))
                                                      (:status observed)
                                                      :unqualified-or-uncertain))]
                                       (reset! typed-outcome status)
                                       ;; Do not replay an uncertain transaction.
                                       ;; A 500 is NOT evidence of no physical commit.
                                       {:status (if qualified? 200 500)
                                        :body (if qualified?
                                                "One local typed atomic operation acknowledged"
                                                "Local typed execution unqualified")})))))
                                 (original request))))
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
                   (report (current!) @result @hits (boolean typed-config)
                           @typed-outcome @typed-attempts
                           (some? (:principal-policy typed-config))
                           @principal-denials @principal-admissions))
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
                      (report (current!) @result @hits (boolean typed-config)
                           @typed-outcome @typed-attempts
                           (some? (:principal-policy typed-config))
                           @principal-denials @principal-admissions))))]
    (locking issued
      (.put issued handle {:source contract-source
                           :module module
                           :summary! summary!
                           :verify-once! verify!}))
    {:handle handle :module module :summary! summary!
     :verify-once! verify!})))

(defn require-current-issued-http-consumption-probe!
  "v1782: Require the ORIGINAL locally issued probe handle, exact lifecycle
   module and exact verification callbacks. A caller-composed map, cloned
   handler, or a synthetic success report is not valid issuance. Returns
   only a redacted source-relative inspection, NEVER a server credential."
  [candidate]
  (pinned!)
  (when-not (and (or (instance? clojure.lang.PersistentArrayMap candidate)
                     (instance? clojure.lang.PersistentHashMap candidate))
                 (nil? (meta candidate))
                 (= #{:handle :module :summary! :verify-once!}
                    (set (keys candidate))))
    (reject! :invalid-original-probe-capability-shape))
  (let [record (entry (:handle candidate))]
    (when-not (and record
                   (identical? (:module candidate) (:module record))
                   (identical? (:summary! candidate) (:summary! record))
                   (identical? (:verify-once! candidate) (:verify-once! record))
                   (= module-id (get-in candidate [:module :biff.core/id])))
      (reject! :unissued-or-substituted-http-consumption-probe))
    {:type report-type :version report-version
     :scope :identity-bound-locally-issued-probe
     :original-probe :issued-original-module-and-callbacks
     :physical-http-consumption :not-established
     :external-server-deployment :not-established
     :certificate :not-issued :transaction-authority :none
     :authority :none}))
