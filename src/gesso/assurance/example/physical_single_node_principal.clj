(ns gesso.assurance.example.physical-single-node-principal
  "v2332: Original-Biff-installed source-checked principal admission for v1495
   single-node Biff/Ring -> XTDB proving fixture.

   An application-supplied declaration names the original reservation catalog,
   gives a closed principal->operation grant map, and supplies one host-owned
   principal-acquisition callback. The wrapper checks the declared grants
   against the actual catalog and the actual HTTP fixture's operation routes.
   For each known mutating route, it calls the acquisition callback ONCE,
   checks the resulting principal against the grants, and invokes the original
   handler at most once. Denied requests never reach that handler.

   THIS IS NOT AUTHENTICATION CERTIFICATION. The host-supplied acquisition
   callback is TRUSTED, not inspected or independently authenticated. A
   callback which reads attacker-controlled headers is not made secure by
   this boundary. Its purity, session validation and deployment provenance
   remain open. Principal IDs are not persisted in the reservation document;
   catalog guards do not mention them, so equivalence with abstract policy is
   NOT established. The wrapped handler IS the exact handler installed at original Biff
   startup in this fixture, rather than a late wrapper; however it is NOT
   a verified network listener, and CSRF protection is NOT provided. Never expose this fixture publicly.
   In the combined Live proving fixture, a second read-boundary wrapper
   uses a separately declared closed :read-principals set for the selected
   physical view, Live panel, fragment and SSE acquisition. Mutation grants
   never imply read rights, and read-only principals need not hold mutation
   rights. This remains a narrow TEST-ONLY read policy, not role-dependent
   fine-grained disclosure or trusted principal authentication. Continuous
   SSE authorization is rechecked when each advisory event is about to be
   encoded, with the originally acquired subject identity retained; this
   terminates the source stream on a failed recheck. This does NOT establish
   instantaneous revocation during idle intervals, recheck of keepalive bytes,
   trusted callback thread-safety, or exclusion of in-flight frames. No proof
   certificate, admission,
   authorization theorem, or physical refinement is issued."
  (:require [clojure.set :as set]
            [gesso.assurance.example.physical-single-node-http :as http]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]))

(def report-type
  :gesso.assurance.example.physical-single-node-principal/request-boundary)
(def report-version 1)

(def ^:private original-http-start http/with-http-proving-application!)
(def ^:private original-catalog pilot/reservation-catalog)
(def ^:private source-keys
  #{:catalog-id :policy-id :grants :read-principals :acquire-principal!})
(def ^:private maximum-principals 64)
(def ^:private open-obligations
  #{:host-principal-acquisition-authenticity-not-established
    :host-session-and-credential-verification-not-established
    :principal-callback-purity-not-established
    :csrf-defense-not-installed
    :principal-identities-not-persisted-in-physical-domain
    :abstract-guard-authorization-equivalence-not-established
    :physical-listener-and-principal-provenance-not-established
    :certified-plan-executor-not-issued
    :complete-writer-coverage-not-established
    :physical-refinement-not-established})

(defn- reject! [reason]
  (throw (ex-info "Invalid single-node principal admission boundary."
                  {:physical-single-node-principal/rejection true
                   :reason reason})))

(defn- closed-map? [m expected-keys]
  (and (map? m)
       (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))
       (= expected-keys (set (clojure.core/keys m)))))

(defn- checked-static-source! [source]
  ;; Fail before Biff/XTDB startup: malformed application policy is a
  ;; construction error, not a request-time or post-bootstrap diagnosis.
  (when-not (and (identical? original-http-start
                             (var-get #'http/with-http-proving-application!))
                 (identical? original-catalog
                             (var-get #'pilot/reservation-catalog)))
    (reject! :substituted-source-or-http-constructor))
  (when-not (closed-map? source source-keys)
    (reject! :invalid-policy-source-shape))
  (let [{:keys [catalog-id policy-id grants read-principals acquire-principal!]} source
        catalog (original-catalog)
        operations (:declared-operation-ids catalog)]
    (when-not (and (= catalog-id (:id catalog))
                   (= operations physical/operation-ids)
                   (keyword? policy-id) (namespace policy-id)
                   (fn? acquire-principal!)
                   (map? grants)
                   (closed-map? grants (set (keys grants)))
                   (<= 1 (count grants) maximum-principals)
                   (every? #(and (keyword? %) (namespace %)) (keys grants))
                   (every? #(and (set? %) (nil? (meta %)) (seq %)
                                 (set/subset? % operations)) (vals grants)))
      (reject! :policy-not-bound-to-catalog-and-http-operations))
    (when-not (= operations (reduce set/union #{} (vals grants)))
      (reject! :policy-not-bound-to-catalog-and-http-operations))
    ;; Read rights are independent of mutation rights: a read-only principal
    ;; may be named here, and a mutating principal may be absent. Never
    ;; infer disclosure permission from the existence of a write grant.
    (when-not (and (set? read-principals)
                   (nil? (meta read-principals))
                   (<= 1 (count read-principals) maximum-principals)
                   (every? #(and (keyword? %) (namespace %)) read-principals))
      (reject! :invalid-independent-read-principals)))
  source)

(defn- admitted-policy! [source app]
  ;; Recheck source and additionally validate the ACTUAL assembled HTTP
  ;; operation routes. A policy-compatible source does not certify deployment.
  (checked-static-source! source)
  (let [catalog (:catalog app)
        routes (:http-routes app)
        poststart-routes (get-in app [:http-report :routes])
        operations (:declared-operation-ids catalog)
        routed-operations (into #{}
                                (filter #(contains? operations %))
                                (vals routes))]
    (when-not (and (map? routes)
                   ;; Once Biff has started, its report must attest the same
                   ;; original route inventory presented to the prestart
                   ;; extension. Absence of that report is only permitted
                   ;; before initial Biff startup.
                   (or (nil? (:http-report app))
                       (= routes poststart-routes))
                   (= catalog (original-catalog))
                   (= (:catalog-id source) (:id catalog))
                   (= physical/operation-ids operations routed-operations)
                   (= (set (vals routes)) (conj operations :read-only))
                   (= (count routes) (inc (count operations)))
                   ;; The original producer's certificate report exists
                   ;; only once Biff has started; prestart source still
                   ;; checks the already-unissued original catalog.
                   (or (nil? (:http-report app))
                       (= :not-issued
                          (get-in app [:http-report :certificate]))))
      (reject! :policy-not-bound-to-catalog-and-http-operations))
    (into {} (filter (fn [[_ op]] (contains? operations op)) routes))))

(defn- acquired-principal! [acquire! request]
  (let [result (acquire! request)]
    (cond
      (closed-map? result #{:status})
      (if (= :anonymous (:status result))
        nil
        (reject! :malformed-principal-acquisition-result))

      (closed-map? result #{:status :principal-id})
      (if (and (= :authenticated (:status result))
               (keyword? (:principal-id result))
               (namespace (:principal-id result)))
        (:principal-id result)
        (reject! :malformed-principal-acquisition-result))

      :else
      (reject! :malformed-principal-acquisition-result))))

(defn- denied-response [status]
  {:status status
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"}
   :body (if (= status 401) "Principal required" "Operation forbidden")})

(defn checked-test-policy!
  "Check a diagnostic request-grant source before a Biff/XTDB fixture starts.
   This is source/shape validation, NOT principal authentication or a
   certificate, and grants are not part of the physical XTDB guard."
  [source]
  (checked-static-source! source))

(defn- conceal-write-only-result
  "A write grant does not grant response-view disclosure. Preserve the
   physical operation's definitive successful HTTP status, but remove the
   rendered resource state, readback metadata and view-refresh directive
   for a caller who has no independent read grant. An error still follows
   the underlying typed rejection path; this is not a proof of generalized
   HTTP response noninterference or trusted identity acquisition."
  [response]
  (if (= 200 (:status response))
    (-> response
        (assoc :body "Operation accepted; resource view is not authorized.")
        (update :headers
                (fn [headers]
                  (-> headers
                      (dissoc "HX-Refresh" "x-gesso-proving-readback")
                      (assoc "content-type" "text/plain; charset=utf-8"
                             "cache-control" "no-store"
                             "x-gesso-proving-response-visibility" "write-only")))))
    response))

(defn guarded-test-handler!
  "Wrap the original operation handler with this source's selected request
   grants BEFORE original Biff installation. Use the owner-derived :http-routes
   inventory, not caller-maintained route strings. This only enforces grants
   for known POST operations that reach this wrapper. Successful POST
   responses for write-only principals do not disclose the rendered resource
   state or instruct the browser to fetch a resource it cannot read. It does
   not certify the host's identity callback, CSRF or any alternate writer."
  [source app original-handler]
  (when-not (fn? original-handler)
    (reject! :missing-original-physical-handler))
  (let [operation-routes (admitted-policy! source app)
        acquire! (:acquire-principal! source)
        grants (:grants source)
        readers (:read-principals source)]
    (fn [request]
      (if (and (map? request)
               (= :post (:request-method request))
               (contains? operation-routes (:uri request))
               (not (contains? request :path-params))
               (not (contains? request :route-params)))
        (let [principal (acquired-principal! acquire! request)
              operation (get operation-routes (:uri request))]
          (cond
            (nil? principal) (denied-response 401)
            (not (contains? (get grants principal #{}) operation))
            (denied-response 403)
            :else (let [response (original-handler request)]
                    (if (contains? readers principal)
                      response
                      (conceal-write-only-result response)))))
        (original-handler request)))))

(def ^:private authorized-reader-key
  ::initial-authorized-reader)

(defn guarded-test-event-encoder!
  "Build a test-only SSE encoder bound to the ORIGINAL checked GET reader.
   The outer read guard sets ::initial-authorized-reader AFTER requesting the
   principal from the host. On every advisory event the encoder independently
   reacquires the current principal from that same host callback and permits
   frames only while the exact initial reader remains authorized. A rejected
   recheck throws before payload encoding, so the SSE transport closes its
   Manifold output and releases its source interest. Keepalives, in-flight
   writes and revocations without an event are not continuously monitored.
   Rechecks run on a Live worker thread, so the trusted host callback must be
   safe to invoke there; this is NOT a production session authenticator."
  [source request]
  (checked-static-source! source)
  (let [initial (get request authorized-reader-key)
        readers (:read-principals source)
        acquire! (:acquire-principal! source)]
    (when-not (and (map? request)
                   (= :get (:request-method request))
                   (contains? readers initial))
      (reject! :missing-initial-sse-reader))
    (fn [payload]
      (let [current (acquired-principal! acquire! request)]
        (when-not (and (= initial current)
                       (contains? readers current))
          (reject! :sse-reader-no-longer-authorized))
        (pr-str payload)))))

(defn guarded-test-read-handler!
  "Guard a closed, selected set of GET routes in the combined Live fixture.
   A principal in the explicitly declared :read-principals may READ the
   diagnostic slot, independently of mutation operation grants. Anonymous and unknown principals are denied BEFORE any
   handler read or SSE stream allocation. The original HTTP owner supplies
   its read-only route; the Live owner supplies its panel/fragment/SSE routes.
   This is a narrow test policy: it cannot infer fine-grained read rights,
   validate host authentication, or revoke an already-open SSE stream
   BEFORE its next advisory event. The event encoder receives the exact
   originally checked identity in a wrapper-owned request entry."
  [source app read-uris handler]
  (when-not (and (fn? handler)
                 (set? read-uris)
                 (nil? (meta read-uris))
                 (seq read-uris)
                 (every? #(and (string? %) (re-matches #"/[a-zA-Z0-9_./-]+" %))
                         read-uris))
    (reject! :invalid-selected-read-boundary))
  (admitted-policy! source app)
  (let [acquire! (:acquire-principal! source)
        readers (:read-principals source)]
    (fn [request]
      (if (and (map? request)
               (= :get (:request-method request))
               (contains? read-uris (:uri request))
               (not (contains? request :path-params))
               (not (contains? request :route-params)))
        (let [who (acquired-principal! acquire! request)]
          (cond
            (nil? who) (denied-response 401)
            (not (contains? readers who)) (denied-response 403)
            :else (handler (assoc request authorized-reader-key who))))
        ;; Forged route parameters still reach the original handler, which
        ;; independently rejects them with 400 before physical access.
        (handler request)))))

(defn with-principal-proving-application!
  "Run f with a real v1495 embedded-XTDB app and :principal-handler.

   `source` is exactly {:catalog-id :slot/reservation-lifecycle
                        :policy-id <namespaced keyword>
                        :grants {<namespaced principal-id> #{operation-ids}}
                        :read-principals #{<namespaced principal-id> ...}
                        :acquire-principal! (fn [ring-request]
                          {:status :authenticated :principal-id <id>}
                          ;; or {:status :anonymous}
                          )}.

   `:principal-handler` admits POSTs for recognized operation routes only
   after consulting that callback once. An anonymous result gets 401; an
   unknown or ungranted principal gets 403. Malformed acquisition results or
   callback exceptions reject without reaching the original transaction path.
   Unrecognized routes and other HTTP methods are delegated to v1495; this
   helper does not replace its routing, error or commit semantics. It never
   calls the callback for a GET, unsupported route, or forged path parameters.

   The caller MUST supply a genuine host-owned authenticator, not a request
   parameter, forged Ring principal, or client-supplied header masquerading as
   proof of identity. This diagnostic installs the wrapper during original Biff startup, but
   does not authenticate the callback, prove CSRF protection, or bind application
   authorization to the abstract catalog's guards."
  ([source f]
   (with-principal-proving-application!
     (str "v1507-" (random-uuid)) source f))
  ([slot-id source f]
   (when-not (fn? f)
     (reject! :invalid-test-callback))
   (checked-static-source! source)
   (original-http-start
    slot-id
    ;; This factory executes BEFORE the original Biff init/start, not after.
    ;; The supplied route inventory is derived by the original HTTP owner,
    ;; so this principal policy does not duplicate the protected routes.
    (fn [app original-handler]
      (guarded-test-handler! source app original-handler))
    (fn [app]
      (let [operation-routes (admitted-policy! source app)
            handler (:handler app)
            original-handler (:delegate-handler app)
            system (:system app)]
        (when-not (and (fn? handler)
                       (fn? original-handler)
                       (not (identical? handler original-handler))
                       (identical? handler (:biff.ring/handler system)))
          (reject! :principal-handler-not-installed-at-original-biff-start))
        (let [report {:type report-type
                      :version report-version
                      :catalog-id (get-in app [:catalog :id])
                      :policy-id (:policy-id source)
                      :declared-operation-count (count operation-routes)
                      :declared-principal-count (count (:grants source))
                      :source-grant-coverage :checked-against-catalog-and-routes
                      :read-principal-declaration :checked-but-not-enforced-here
                      :write-only-success-response :rendered-state-concealed
                      :request-time-grant-enforcement :before-delegated-handler
                      :host-acquisition-callback :trusted-not-authenticated
                      :callback-invocations :one-per-known-mutating-request
                      :handler-delegation :at-most-once
                      :biff-installed-principal-handler :original-biff-start-selected-wrapper
                      :physical-network-listener :not-established
                      :abstract-policy-equivalence :not-established
                      :physical-commit-provenance :not-authenticated
                      :certificate :not-issued
                      :admission :not-admitted
                      :authority :none
                      :open-obligations open-obligations}]
          (f (assoc app
                    :principal-handler handler
                    :principal-report report))))))))
