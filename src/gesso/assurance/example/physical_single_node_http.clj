(ns gesso.assurance.example.physical-single-node-http
  "v1495: Real Ring/Biff entry points for the v1494 embedded-XTDB reservation
   pilot. This is an *uncertified* physical application fixture, not an issued
   certified-plan executor, a v1478 checked host adoption, or a deployable
   authenticated service.

   The v1494 transaction contains explicit application-owned XTDB ASSERTs and
   unqualified persisted fields. v1251 certified-plan currently admits a
   restricted single-command plan with qualified physical field mappings and
   no extra assertions. Misrepresenting the v1494 callback as a certified-plan
   executor would erase an actual assurance obligation. The HTTP bridge
   deliberately preserves that boundary while exercising real Biff startup,
   Ring dispatch, Gesso HTML rendering and single-shot model transactions.

   No HTTP listener is opened. Callbacks remain valid only during the supplied
   managed application lifetime. There is no principal or CSRF verification:
   NEVER expose this diagnostic handler on a public network. This namespace
   does not register a canonical Gesso ApplicationAssembly, or claim a proof
   certificate, complete writer coverage, physical refinement, or a real
   deployed host. The bootstrap transaction remains a separate writer path."
  (:require [com.biffweb.core :as biff]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.http :as http]))

(def module-id :gesso.assurance.example.physical-single-node-http/install)
(def ^:private view-route "/__gesso_proving/slot")
(def ^:private operation-routes
  {"/__gesso_proving/slot/reserve" :slot/reserve
   "/__gesso_proving/slot/release" :slot/release})
(def ^:private open-obligations
  #{:v1494-operations-not-issued-certified-plan-executors
    :gesso-application-assembly-and-canonical-html-preflight-not-installed
    :request-principal-authorization-and-csrf-not-checked
    :http-server-consumption-of-biff-handler-not-authenticated
    :dynamic-uri-rewriting-and-deployment-not-attested
    :transaction-prestate-and-write-footprint-not-proved
    :complete-physical-writer-coverage-not-established
    :universal-refinement-not-established})

(def ^:dynamic *biff-modules* nil)

(defn- reject! [reason]
  (throw (ex-info "Invalid physical single-node HTTP proving fixture."
                  {:physical-single-node-http/rejection true :reason reason})))

(defn- checked-application! [app]
  (when-not (and (map? app)
                 (fn? (:read! app))
                 (fn? (:abstract! app))
                 (fn? (:execute! app))
                 (some? (:node app))
                 (= physical/operation-ids (get-in app [:report :operation-ids]))
                 (= :not-issued (get-in app [:report :certificate])))
    (reject! :invalid-or-already-certified-physical-application))
  app)

(defn- safe-render [app]
  ;; A response must describe one coherent XTDB observation. Calling
  ;; :abstract! separately would issue a second physical read, and a
  ;; concurrent writer could change the slot between those two snapshots.
  (let [row ((:read! app))
        state (physical/physical->abstract row)]
    (when-not (and (= (:slot/active-claims state) (:claims row))
                   (#{:available :reserved} (:slot/status state)))
      (reject! :physical-readback-mismatch))
    (http/html-response
     [:section {:id "gesso-proving-slot"
                :data-gesso-proving-physical true}
      [:h2 "Single-node reservation proving fixture"]
      [:p {:data-slot-status (name (:slot/status state))}
       "Status: " (name (:slot/status state))]
      [:p {:data-slot-revision (:revision row)}
       "Revision: " (str (:revision row))]])))

(def ^:private readback-header "x-gesso-proving-readback")

(defn- postcommit-readback-unknown []
  ;; The delegated source reported :committed, but an independently performed
  ;; read/interpretation/HTML rendering failed. Do not turn that into a failed
  ;; HTTP command: a client might submit the same mutation a second time.
  ;; Equally, do not fabricate a state or an authenticated commit receipt.
  ;; HTMX 2.0.7 can request a fresh authoritative page GET from HX-Refresh.
  {:status 200
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"
             "HX-Refresh" "true"
             readback-header "unknown"}
   :body (str "The delegated operation reported a commit, but authoritative "
              "presentation is unavailable. Refresh before further actions.")})

(defn- render-after-reported-commit [app]
  (try
    (assoc-in (safe-render app) [:headers readback-header] "current")
    (catch Exception _
      ;; This only catches failures AFTER the delegated :committed outcome.
      ;; An XTDB submission failure or :commit-unknown must still propagate;
      ;; it cannot be promoted to commit success by a subsequent GET.
      ;; A failed readback may also signal bad source or corrupted data,
      ;; so :unknown is diagnostic, not a silently accepted model fact.
      (postcommit-readback-unknown))))

(defn- unsupported [status]
  {:status status
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"}
   :body (case status
           400 "Unsupported Ring request"
           404 "Unknown proving route"
           405 "Method not allowed"
           "Request rejected")})

(defn- ring-handler! [app]
  (let [execute! (:execute! app)]
    (fn [request]
      (when-not (map? request)
        (reject! :invalid-ring-request))
      (let [method (:request-method request)
            uri (:uri request)]
        (cond
          (not (and (keyword? method) (string? uri)))
          (unsupported 400)

          (or (contains? request :path-params)
              (contains? request :route-params))
          (unsupported 400)

          (= uri view-route)
          (if (= :get method) (safe-render app) (unsupported 405))

          (contains? operation-routes uri)
          (if (= :post method)
            (let [outcome (execute! (get operation-routes uri))]
              ;; Exactly one call. Only a definitive committed result allows
              ;; a successful HTTP response. Unknown/rejected outcomes must
              ;; never become success merely because readback appears fresh.
              ;; An error propagates without an automatic second submission.
              (when-not (and (map? outcome)
                             (= :committed (:commit/status outcome)))
                (reject! :nondefinitive-physical-outcome))
              (render-after-reported-commit app))
            (unsupported 405))

          :else
          (unsupported 404))))))

(defn- install-module! [handler]
  (let [init-count (atom 0)
        start-count (atom 0)]
    {:biff.core/id module-id
     :biff.core/init
     (fn [_]
       (when-not (= 1 (swap! init-count inc))
         (reject! :repeated-module-init))
       {:biff.ring/handler handler})
     :biff.core/start
     (fn [system]
       (when-not (= 1 (swap! start-count inc))
         (reject! :repeated-module-start))
       (when-not (and (= 1 @init-count)
                      (identical? handler (:biff.ring/handler system)))
         (reject! :handler-replaced-before-start))
       system)}))

(defn with-http-proving-application!
  "Start one embedded XTDB pilot and a real Biff lifecycle (without a server),
   then call f with :system, :handler, :read!, :abstract!, and the v1494 app.

   The three-argument, TEST-ONLY form accepts a handler-extension! factory
   BEFORE Biff init/start, so a composed HTTP/Live handler can be the exact
   Ring function Biff initially adopts. The extension receives the checked
   physical app with its original :handler, :http-routes derived from the
   same private Ring route inventory used by :http-report, and the original
   handler as its second argument. It must return one callable Ring function. This cannot
   certify the new routes, principal, original-source completeness, browser
   asset, or actual network listener. The returned :handler is the installed
   handler; :delegate-handler retains the original one.

   No arbitrary callback is a release credential. The Biff system is LOCAL,
   never deployment evidence. Handlers are invalid after the callback returns
   and the embedded node is closed."
  ([f] (with-http-proving-application! (str "v1495-" (random-uuid)) f))
  ([slot-id f]
   (with-http-proving-application!
    slot-id (fn [_ original] original) f))
  ([slot-id handler-extension! f]
   (when-not (and (fn? handler-extension!) (fn? f))
     (reject! :invalid-test-callback))
   (physical/with-proving-application!
    slot-id
    (fn [app]
      (checked-application! app)
      (let [original-handler (ring-handler! app)
            ;; Expose the ORIGINAL handler's routes before Biff startup so
            ;; prestart extensions may check the same source-owned route
            ;; inventory used in the post-start fixture report. Never ask an
            ;; extension to maintain an independent operation-route catalog.
            routes (into {view-route :read-only} operation-routes)
            handler (handler-extension! (assoc app :handler original-handler
                                                :http-routes routes)
                                        original-handler)]
        (when-not (fn? handler)
          (reject! :invalid-prestart-handler-extension))
        (let [module (install-module! handler)
              system (binding [*biff-modules* [module]]
                       (biff/start (:biff-context app)
                                   #'*biff-modules* [module-id]))]
          (when-not (identical? handler (:biff.ring/handler system))
            (reject! :adopted-handler-not-identical))
          (let [report
                {:type :gesso.assurance.example.physical-single-node-http/local-biff-fixture
                 :version 1
                 :routes routes
                 :physical-table physical/table
                 :module-start :one-observed-biff-start
                 :handler-in-returned-system :identical
                 :handler-extension :test-only-not-checked-or-certified
                 :physical-transactions :delegated-to-v1494-once-per-post
                 :postcommit-rendering :independent-physical-readback-may-be-unknown
                 :postcommit-render-failure :preserve-reported-commit-and-request-refresh
                 :issued-certified-plan-executors :not-established
                 :certified-plan-compatibility
                 {:persisted-fields :unqualified-v1494-fields
                  :current-v1251-required-fields :qualified-and-exhaustive
                  :application-extra-assertions :not-admitted-by-v1251
                  :safe-next-step :derive-a-supported-physical-plan-without-losing-guards}
                 :installed-checked-router :not-established
                 :canonical-application-preflight :not-established
                 :host-deployment :not-established
                 :writer-completeness :not-established
                 :physical-refinement :not-established
                 :open-obligations open-obligations
                 :certificate :not-issued
                 :admission :not-admitted
                 :authority :none}]
            (f (assoc app :handler handler
                          :delegate-handler original-handler
                          :http-routes routes
                          :system system :module module
                          :http-report report)))))))))
