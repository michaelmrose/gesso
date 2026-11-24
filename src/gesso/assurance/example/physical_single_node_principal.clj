(ns gesso.assurance.example.physical-single-node-principal
  "v2330: Original-Biff-installed source-checked principal admission for v1495
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
   conservatively permits view/fragment/SSE access only to principals present
   in the selected mutation-grant source. This is an explicit TEST-ONLY
   visibility choice, NOT a general operation-to-read policy; the host callback
   is still trusted, and continuous SSE authorization after connection or
   principal revocation is NOT established. No proof certificate, admission,
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
  #{:catalog-id :policy-id :grants :acquire-principal!})
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
  (let [{:keys [catalog-id policy-id grants acquire-principal!]} source
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
      (reject! :policy-not-bound-to-catalog-and-http-operations)))
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

(defn guarded-test-handler!
  "Wrap the original operation handler with this source's selected request
   grants BEFORE original Biff installation. Use the owner-derived :http-routes
   inventory, not caller-maintained route strings. This only enforces grants
   for known POST operations that reach this wrapper. It does not certify
   the host's identity callback, SSE disclosure, CSRF or any alternate writer."
  [source app original-handler]
  (when-not (fn? original-handler)
    (reject! :missing-original-physical-handler))
  (let [operation-routes (admitted-policy! source app)
        acquire! (:acquire-principal! source)
        grants (:grants source)]
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
            :else (original-handler request)))
        (original-handler request)))))

(defn guarded-test-read-handler!
  "Guard a closed, selected set of GET routes in the combined Live fixture.
   A principal listed in the verified mutation-grant source may READ the
   diagnostic slot. Anonymous and unknown principals are denied BEFORE any
   handler read or SSE stream allocation. The original HTTP owner supplies
   its read-only route; the Live owner supplies its panel/fragment/SSE routes.
   This is a narrow test policy: it cannot infer fine-grained read rights,
   validate host authentication, or revoke an already-open SSE stream."
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
        readers (set (keys (:grants source)))]
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
            :else (handler request)))
        ;; Forged route parameters still reach the original handler, which
        ;; independently rejects them with 400 before physical access.
        (handler request)))))

(defn with-principal-proving-application!
  "Run f with a real v1495 embedded-XTDB app and :principal-handler.

   `source` is exactly {:catalog-id :slot/reservation-lifecycle
                        :policy-id <namespaced keyword>
                        :grants {<namespaced principal-id> #{operation-ids}}
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
