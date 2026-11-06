(ns gesso.assurance.example.physical-single-node-live
  "v1505: A minimal server-rendered Live panel on the real v1494/v1495
   single-node XTDB reservation fixture.

   A stable adapter-managed fragment root owns the SSE subscription and GET
   trigger; only its inner HTML target is replaced. An in-process Gesso Live
   source carries advisory wakeups AFTER a successful delegated POST, and a
   fragment GET independently reads current XTDB state. No SSE payload is
   installed as authoritative HTML, and no optimistic success is simulated.

   IMPORTANT: This is a *local, uncertified diagnostic fixture*. Its composed
   :live-handler is NOT the handler already installed by v1495's Biff start;
   there is no actual HTTP listener, authenticated principal, CSRF protection,
   authenticated commit receipt, or certified Gesso application assembly.
   Successful HTTP response and Live publication are distinct boundaries:
   a postcommit publication failure must never trigger a second transaction.
   Only browser integration testing can establish HTMX/SSE adapter behavior."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [gesso.assurance.example.physical-single-node-http :as physical-http]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.http :as http]
            [gesso.live.core :as live]
            [gesso.live.ui :as live.ui]))

(def panel-uri "/__gesso_proving/live")
(def fragment-uri "/__gesso_proving/live/fragment")
(def events-uri "/__gesso_proving/live/events")
(def fragment-id "gesso-proving-live-slot-fragment")
(def htmx-asset-uri "/__gesso_proving/live/assets/htmx.js")
(def sse-asset-uri "/__gesso_proving/live/assets/sse.js")
(def runtime-asset-uri "/gesso/gesso-live.js")
(def ^:private htmx-candidates
  ["META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.min.js"
   "META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.js"])
(def ^:private sse-candidates
  ["META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.js"])
(def ^:private asset-uris
  #{htmx-asset-uri sse-asset-uri runtime-asset-uri})
(def ^:private event-name "live-update")
(def ^:private operation-uris
  {"/__gesso_proving/slot/reserve" :slot/reserve
   "/__gesso_proving/slot/release" :slot/release})

(def ^:private obligations
  #{:v1495-delegated-http-outcome-not-authenticated
    :live-wrapper-not-installed-by-biff-host
    :http-listener-and-client-connection-not-observed
    :request-principal-authorization-and-csrf-not-checked
    :postcommit-publication-not-atomic-with-xtdb
    :authoritative-progression-basis-not-attached-to-invalidation
    :browser-adapter-execution-not-observed
    :complete-physical-writer-coverage-not-established
    :physical-refinement-not-established})

(defn- reject! [reason]
  (throw (ex-info "Invalid single-node Live proving fixture."
                  {:physical-single-node-live/rejection true :reason reason})))

(defn- read-resource! [candidates]
  (let [resource (some io/resource candidates)]
    (when-not resource
      (reject! :missing-pinned-browser-webjar))
    (let [content (slurp resource)]
      (when (str/blank? content)
        (reject! :empty-browser-webjar))
      content)))

(defn- browser-assets! []
  (let [runtime (io/file "resources/public/js/gesso-live.js")]
    (when-not (.isFile runtime)
      (reject! :missing-built-browser-runtime))
    (let [runtime-text (slurp runtime)]
      (when (str/blank? runtime-text)
        (reject! :empty-built-browser-runtime))
      {htmx-asset-uri (read-resource! htmx-candidates)
       sse-asset-uri (read-resource! sse-candidates)
       runtime-asset-uri runtime-text})))

(defn- asset-response [content]
  {:status 200
   :headers {"content-type" "text/javascript; charset=utf-8"
             "cache-control" "no-store"}
   :body content})

(defn- live-fragment []
  (live/->fragment {:id fragment-id
                    :src fragment-uri
                    :stream-url events-uri
                    :event event-name
                    :swap "outerHTML"}))

(defn- fragment-node [app]
  (let [row ((:read! app))
        ;; Use ONE physical read for all displayed fields. A second query
        ;; could observe a different revision during concurrent activity.
        abstract (physical/physical->abstract row)
        status (:slot/status abstract)
        claims (:slot/active-claims abstract)
        revision (:revision row)]
    (when-not (and (#{:available :reserved} status)
                   (= claims (:claims row))
                   (integer? revision)
                   (<= 0 revision))
      (reject! :physical-fragment-readback-mismatch))
    [:div {:id fragment-id
           :data-gesso-proving-live-target true
           :data-slot-status (name status)
           :data-slot-revision revision}
     [:p {:data-slot-claims claims}
      "Status: " (name status) ", active claims: " (str claims)]
     [:p "Revision: " (str revision)]
     ;; These POSTs intentionally never swap the response over the managed
     ;; target. The SSE wakeup enters the browser adapter, which owns GET.
     [:button {:type "button"
               :hx-post "/__gesso_proving/slot/reserve"
               :hx-swap "none"
               :disabled (= status :reserved)} "Reserve"]
     [:button {:type "button"
               :hx-post "/__gesso_proving/slot/release"
               :hx-swap "none"
               :disabled (= status :available)} "Release"]]))

(defn- fragment-response [app]
  (assoc-in (http/html-response (fragment-node app))
            [:headers "cache-control"] "no-store"))

(defn- panel-response [app]
  (let [panel (live/fragment-panel (live-fragment))
        target (nth panel 3 nil)
        _ (when-not (and (= 4 (count panel))
                         (vector? target)
                         (= fragment-id (get-in target [1 :id])))
            (reject! :changed-managed-fragment-contract))
        ;; Retain the exact generated root/listener. Replace only the initially
        ;; empty inner target with the authoritative readback's fragment node.
        node (assoc panel 3 (fragment-node app))]
    (assoc-in
     (http/html-response
      [:main {:data-gesso-proving-live-panel true}
       [:h2 "Single-node Live reservation proving fixture"]
       node
       [:script {:src htmx-asset-uri}]
       [:script {:src sse-asset-uri}]
       (live.ui/live-script {:src runtime-asset-uri})])
     [:headers "cache-control"] "no-store")))

(defn- unsupported [status]
  {:status status
   :headers {"content-type" "text/plain; charset=utf-8"
             "cache-control" "no-store"}
   :body (case status
           400 "Unsupported Ring request"
           405 "Method not allowed"
           404 "Unknown proving route"
           "Request rejected")})

(defn- report [app]
  {:type :gesso.assurance.example.physical-single-node-live/local-live-fixture
   :version 1
   :slot-id (:slot-id app)
   :event event-name
   :subscription {:topic :gesso-proving-reservation :id (:slot-id app)}
   :fragment-id fragment-id
   :panel-uri panel-uri
   :fragment-uri fragment-uri
   :events-uri events-uri
   :browser-assets :pinned-local-htmx-2.0.7-sse-2.2.4-and-built-gesso-runtime
   :server-rendered-html :source-readback-on-each-get
   :sse-event :advisory-invalidation-only
   :browser-managed-refresh :declared-not-tested
   :notification-timing :after-delegated-http-success
   :biff-adopted-live-handler :not-established
   :commit-provenance :not-authenticated
   :complete-writer-coverage :not-established
   :physical-refinement :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none
   :open-obligations obligations})

(defn- build-handler [app live-system registered-streams assets]
  (let [original (:handler app)
        scope {:topic :gesso-proving-reservation :id (:slot-id app)}]
    (fn [request]
      (when-not (map? request)
        (reject! :invalid-ring-request))
      (let [method (:request-method request)
            uri (:uri request)]
        (cond
          (not (and (keyword? method) (string? uri)))
          (unsupported 400)

          (and (contains? (into asset-uris [panel-uri fragment-uri events-uri]) uri)
               (or (contains? request :path-params)
                   (contains? request :route-params)))
          (unsupported 400)

          (contains? asset-uris uri)
          (if (= :get method) (asset-response (get assets uri)) (unsupported 405))

          (= uri panel-uri)
          (if (= :get method) (panel-response app) (unsupported 405))

          (= uri fragment-uri)
          (if (= :get method) (fragment-response app) (unsupported 405))

          (= uri events-uri)
          (if (= :get method)
            (let [started (live/start-sse!
                           live-system scope
                           {:flow-options {:event event-name}})]
              (swap! registered-streams conj started)
              (:response started))
            (unsupported 405))

          (and (= :post method) (contains? operation-uris uri))
          (let [response (original request)]
            ;; Not a commit authenticator. This is a purely advisory wakeup
            ;; after the uncertified v1495 path reports a successful response.
            ;; If the publish fails, never resubmit or reinterpret the write.
            (when (= 200 (:status response))
              (live/emit! live-system
                          (assoc scope :change/kind :updated)))
            response)

          :else
          (original request))))))

(defn with-live-proving-application!
  "Run f within a real embedded XTDB, Biff-fixture and Gesso Live lifecycle.

   :live-handler composes the v1495 handler with scoped SSE, HTML panel and
   fresh-fragment GET. :handler and :system still identify the original
   v1495 Biff-adopted handler; no claim is made that Biff installed this
   composed Live wrapper. All opened SSE streams and Live resources are closed
   before the managed application and its embedded XTDB node close.

   No network listener is created. Caller must not retain callbacks/streams
   past f's dynamic extent. No automatic transaction retry is permitted."
  ([f] (with-live-proving-application! (str "v1505-" (random-uuid)) f))
  ([slot-id f]
   (when-not (fn? f)
     (reject! :invalid-application-callback))
   (physical-http/with-http-proving-application!
    slot-id
    (fn [app]
      (let [live-system (live/create)
            registered-streams (atom [])]
        (try
          (let [assets (browser-assets!)
                handler (build-handler app live-system registered-streams assets)]
            (f (assoc app
                      :live-handler handler
                      :live-system live-system
                      :live-report (report app)
                      :live-fragment (live-fragment))))
          (finally
            (doseq [started @registered-streams]
              (live/cancel-sse! started))
            (live/close! live-system))))))))
