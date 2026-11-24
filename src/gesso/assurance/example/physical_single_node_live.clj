(ns gesso.assurance.example.physical-single-node-live
  "v2355: Biff-instantiated server-rendered Live panel on the real v1494/v1495
   single-node XTDB reservation fixture.

   A stable adapter-managed fragment root owns the SSE subscription and GET
   trigger; only its inner HTML target is replaced. An in-process Gesso Live
   source carries advisory wakeups AFTER a successful delegated POST, and a
   fragment GET independently reads current XTDB state. No SSE payload is
   installed as authoritative HTML, and no optimistic success is simulated.

   IMPORTANT: This is a *local, uncertified diagnostic fixture*. The composed
   :live-handler IS installed as the original Ring handler during Biff startup
   by the v2323 prestart extension point; this does NOT certify the handler or
   prove that a real network listener has adopted it. No authenticated
   principal, CSRF protection, authenticated commit receipt, or certified
   Gesso application assembly.
   Successful HTTP response and Live publication are distinct boundaries:
   a postcommit publication failure must never reclassify a committed write
   as a failed HTTP command or trigger a second transaction. The fixture
   exposes publication attempt status, not guaranteed SSE delivery. An
   uncertain notification requests an HTMX full-page reload to reacquire
   authoritative state, without retrying the protected operation.
   Only browser integration testing can establish HTMX/SSE adapter behavior."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [gesso.assurance.example.physical-single-node-http :as physical-http]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-principal :as principal]
            [gesso.http :as http]
            [gesso.live.core :as live]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.ui :as live.ui]
            [manifold.stream :as stream]))

(def panel-uri "/__gesso_proving/live")
(def fragment-uri "/__gesso_proving/live/fragment")
(def events-uri "/__gesso_proving/live/events")
(def fragment-id "gesso-proving-live-slot-fragment")
(def htmx-asset-uri "/__gesso_proving/live/assets/htmx.js")
(def sse-asset-uri "/__gesso_proving/live/assets/sse.js")
(def runtime-asset-uri "/gesso/gesso-live.js")
(def bootstrap-asset-uri "/__gesso_proving/live/assets/bootstrap.js")
(def ^:private bootstrap-js
  ;; ADVANCED compilation exports init! but deliberately never auto-starts.
  ;; This fixed, non-session-specific script runs after the deferred runtime.
  "gesso.live.browser.runtime.init_BANG_();\n")
(def ^:private htmx-candidates
  ["META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.min.js"
   "META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.js"])
(def ^:private sse-candidates
  ["META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.js"])
(def ^:private asset-uris
  #{htmx-asset-uri sse-asset-uri runtime-asset-uri bootstrap-asset-uri})
(def ^:private event-name "live-update")
(def ^:private operation-uris
  {"/__gesso_proving/slot/reserve" :slot/reserve
   "/__gesso_proving/slot/release" :slot/release})

(def ^:private obligations
  #{:v1495-delegated-http-outcome-not-authenticated
    :actual-network-consumption-of-biff-adopted-handler-not-established
    :http-listener-and-client-connection-not-observed
    :host-principal-authenticity-and-csrf-not-established
    :fine-grained-read-policy-and-subject-disclosure-not-established
    :idle-sse-revocation-and-in-flight-event-fence-not-established
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
       runtime-asset-uri runtime-text
       bootstrap-asset-uri bootstrap-js})))

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

(defn- fragment-node [app presentation]
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
     (when (or (nil? presentation)
               (contains? (:operations presentation) :slot/reserve))
       [:button (cond-> {:type "button"
                         :hx-post "/__gesso_proving/slot/reserve"
                         :hx-swap "none"
                         :disabled (= status :reserved)}
                  presentation (assoc :hx-headers (:headers presentation))) "Reserve"])
     (when (or (nil? presentation)
               (contains? (:operations presentation) :slot/release))
       [:button (cond-> {:type "button"
                         :hx-post "/__gesso_proving/slot/release"
                         :hx-swap "none"
                         :disabled (= status :available)}
                  presentation (assoc :hx-headers (:headers presentation))) "Release"])]))

(defn- fragment-response [app presentation]
  (assoc-in (http/html-response (fragment-node app presentation))
            [:headers "cache-control"] "no-store"))

(defn- panel-response [app presentation]
  (let [panel (live/fragment-panel (live-fragment))
        target (nth panel 3 nil)
        _ (when-not (and (= 4 (count panel))
                         (vector? target)
                         (= fragment-id (get-in target [1 :id])))
            (reject! :changed-managed-fragment-contract))
        ;; Retain the exact generated root/listener. Replace only the initially
        ;; empty inner target with the authoritative readback's fragment node.
        node (assoc panel 3 (fragment-node app presentation))]
    (assoc-in
     (http/html-response
      [:html {:lang "en"}
       [:head
        [:meta {:charset "utf-8"}]
        [:title "Single-node Live reservation proving fixture"]
        ;; Avoid an unrelated browser-generated 404 for a nonexistent icon.
        [:link {:rel "icon" :href "data:,"}]]
       [:body
        [:main {:data-gesso-proving-live-panel true}
         [:h2 "Single-node Live reservation proving fixture"]
         node]
        [:script {:src htmx-asset-uri}]
        [:script {:src sse-asset-uri}]
        (live.ui/live-script {:src runtime-asset-uri})
        ;; Both deferred classic scripts execute in document order. Do not
        ;; invoke init! inline ahead of the deferred runtime export.
        [:script {:src bootstrap-asset-uri :defer true}]]])
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
   :uncertain-notification-recovery :htmx-2.0.7-full-page-refresh-requested
   :biff-adopted-live-handler :one-original-biff-start-installed-composed-live-handler
   :biff-network-consumption :not-established
   :commit-provenance :not-authenticated
   :complete-writer-coverage :not-established
   :live-read-and-sse-authorization :unselected-in-plain-fixture
   :physical-refinement :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none
   :open-obligations obligations})

(def ^:private publication-header "x-gesso-proving-live-publication")

(defn- publish-after-committed-response
  "Attempt an advisory wakeup after a definitive HTTP success, without
   changing that result into an apparent failed write if the separate
   publication step fails. Even :emitted denotes source acceptance only:
   Manifold puts and browser adoption are asynchronous. A failure may
   leave publication uncertain, not necessarily absent. This test-only
   boundary does not replace the required durable XTDB outbox. In
   pinned HTMX 2.0.7, HX-Refresh: true reloads the current document."
  [live-system scope response]
  (if (not= 200 (:status response))
    response
    (let [accepted?
          (try
            (contains? #{:emitted :queued}
                       (:status (live/emit!
                                 live-system
                                 (assoc scope :change/kind :updated))))
            (catch Exception _
              ;; The protected operation has already committed. In
              ;; particular, NEVER retry it because notification failed.
              false))]
      (cond-> (assoc-in response [:headers publication-header]
                        (if accepted? "source-accepted" "unknown"))
        ;; HTMX 2.0.7 handles HX-Refresh before swap selection. Thus the
        ;; existing hx-swap=none command can reacquire server state even if
        ;; its advisory SSE wakeup did not reach the source. This is a
        ;; *requested* recovery, not tested browser adoption or durable fanout.
        (not accepted?) (assoc-in [:headers "HX-Refresh"] "true")))))

(defn- register-sse! [registry started]
  ;; Retain outstanding physical sessions, not the history of connections.
  ;; Both the response and the eager source interest must be closed. Manifold
  ;; invokes callbacks registered after closure, covering early termination.
  (let [id (Object.)
        out (:stream started)
        retire! (fn []
                  (when (and (stream/closed? out)
                             (true? ((:source-closed? started))))
                    (swap! registry update :sessions dissoc id)))
        [before _] (swap-vals! registry
                               (fn [state]
                                 (if (:accepting? state)
                                   (assoc-in state [:sessions id] started)
                                   state)))]
    (if (:accepting? before)
      (try
        (stream/on-closed out retire!)
        ((:source-on-closed! started) retire!)
        (retire!)
        started
        (catch Throwable error
          ;; Keep a failed-cleanup handle until physical release is observed.
          (try (live/cancel-sse! started)
               (catch Throwable cleanup
                 (when-not (identical? error cleanup)
                   (.addSuppressed error cleanup))))
          (retire!)
          (throw error)))
      (do
        ;; A late request racing fixture teardown cannot install a new owner.
        (live/cancel-sse! started)
        (reject! :live-session-registry-closed)))))

(defn- retire-scoped-sessions! [registry]
  ;; Ring admission has already drained. Keep an independent shutdown
  ;; snapshot: a close hook may remove a session from :sessions BEFORE its
  ;; cancellation returns or even if that cancellation subsequently throws.
  ;; The lexical Biff/XTDB scope must not unwind on an incomplete release.
  ;; Like scoped worker retirement, this can wait indefinitely on a broken
  ;; provider. It is NOT evidence of downstream Netty writer completion.
  (let [[before _] (swap-vals! registry
                               (fn [state]
                                 (if (:accepting? state)
                                   (assoc state :accepting? false
                                          :retirement {:pending (count (:sessions state))
                                                       :failed-attempts 0
                                                       :completed? false})
                                   state)))]
    (when (:accepting? before)
      (let [interrupted? (volatile! (Thread/interrupted))
            preserve-interrupt! (fn []
                                  (when (Thread/interrupted)
                                    (vreset! interrupted? true)))]
        (try
          (loop [remaining (vec (vals (:sessions before)))]
            (if (empty? remaining)
              (swap! registry assoc-in [:retirement :completed?] true)
              (let [retry (reduce
                           (fn [failed session]
                             (try
                               (live/cancel-sse! session)
                               (when-not (and (stream/closed? (:stream session))
                                              (true? ((:source-closed? session))))
                                 (reject! :incomplete-scoped-sse-release))
                               (swap! registry update-in [:retirement :pending] dec)
                               failed
                               (catch Exception error
                                 (when (instance? InterruptedException error)
                                   (vreset! interrupted? true))
                                 (swap! registry update-in [:retirement :failed-attempts] inc)
                                 (conj failed session))
                               (finally (preserve-interrupt!))))
                           [] remaining)]
                (when (seq retry)
                  (try (Thread/sleep 20)
                       (catch InterruptedException _ (vreset! interrupted? true))))
                (recur retry))))
          (finally
            (preserve-interrupt!)
            (when @interrupted? (.interrupt (Thread/currentThread)))))))))

(defn- build-handler [app live-system registered-streams assets principal-source]
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
          (if (= :get method)
            (panel-response app (principal/session-command-presentation! principal-source request))
            (unsupported 405))

          (= uri fragment-uri)
          (if (= :get method)
            (fragment-response app (principal/session-command-presentation! principal-source request))
            (unsupported 405))

          (= uri events-uri)
          (if (= :get method)
            (let [started (live/start-sse!
                           live-system scope
                           (cond-> {:flow-options {:event event-name}}
                             principal-source
                             (assoc :sse-options
                                    {:encoder
                                     (principal/guarded-test-event-encoder!
                                      principal-source request)})))]
              (:response (register-sse! registered-streams started)))
            (unsupported 405))

          (and (= :post method) (contains? operation-uris uri))
          (let [response (original request)]
            ;; The delegated write remains authoritative for this fixture.
            ;; A later source failure cannot transform committed HTTP success
            ;; into a misleading failed command or cause a second write.
            (if (= :durable-outbox (:publication-selection app))
              ;; The mutation has already recorded its intent atomically.
              ;; Do not emit a second, untracked postcommit wakeup here.
              response
              (publish-after-committed-response live-system scope response)))

          :else
          (original request))))))

(defn- ring-admission []
  ;; This owns synchronous Ring invocations ONLY. A returned SSE body or
  ;; asynchronous HTTP writer is not drained by this boundary. Do not use
  ;; this receipt in place of outbox.http-listener's physical writer drain.
  ;; Startup and retirement are different closed-admission states. A
  ;; request cannot enter while Biff is still starting modules, and a
  ;; failed startup or retirement must never reopen the admission fence.
  (let [state (atom {:phase :starting :active 0})
        drained (promise)
        signal! (fn [{:keys [phase active]}]
                  (when (and (= :closed phase) (zero? active))
                    (deliver drained true)))]
    {:wrap (fn [handler]
             (fn [request]
               (let [[before _] (swap-vals! state
                                           #(if (= :open (:phase %))
                                              (update % :active inc) %))]
                 (if (= :open (:phase before))
                   (try (handler request)
                        (finally (signal! (swap! state update :active dec))))
                   (unsupported 503)))))
     :open! (fn []
              (let [[before _] (swap-vals! state
                                           #(if (= :starting (:phase %))
                                              (assoc % :phase :open) %))]
                (when-not (= :starting (:phase before))
                  (reject! :invalid-ring-admission-open))))
     :status (fn [] (let [{:keys [phase active]} @state]
                      {:accepting? (= :open phase) :active active}))
     :close-and-await! (fn []
                         (signal! (swap! state assoc :phase :closed))
                         ;; Lexically owned providers cannot be closed while
                         ;; a synchronous handler remains active. Interruption
                         ;; is not evidence of a completed drain.
                         (let [interrupted? (volatile! (Thread/interrupted))]
                           (try
                             (loop []
                               (when-not (try @drained
                                              (catch InterruptedException _
                                                (vreset! interrupted? true)
                                                false))
                                 (recur)))
                             (finally
                               (when @interrupted?
                                 (.interrupt (Thread/currentThread)))))))}))

(defn- with-live-application* [slot-id principal-source listener-selection publication-selection storage-selection worker-options f]
  (when-not (fn? f)
    (reject! :invalid-application-callback))
  ;; A source with invalid operation/grant coverage must fail BEFORE
  ;; embedded XTDB and the original Biff lifecycle are constructed.
  (when principal-source
    (principal/checked-test-policy! principal-source))
  (let [live-system (live/create)
        registered-streams (atom {:accepting? true :sessions {}
                                  :retirement {:pending 0 :failed-attempts 0
                                               :completed? false}})
        guarded* (atom nil)
        admission (ring-admission)]
    (try
      (let [assets (browser-assets!)]
        (physical-http/with-http-proving-application!
         slot-id
         (fn [app original-handler]
           ;; Compose the checked grant boundary INSIDE Live and BEFORE the
           ;; original Biff start. The Live source only publishes following a
           ;; 200 response from that protected delegate. A 401/403 therefore
           ;; cannot reach XTDB or trigger an advisory notification.
           (let [delegated (if principal-source
                             (principal/guarded-test-handler!
                              principal-source app original-handler)
                             original-handler)
                 live-handler (build-handler (assoc app :handler delegated)
                                             live-system registered-streams assets
                                             principal-source)
                 ;; Original physical read route comes from the original
                 ;; HTTP owner; Live GET routes come from this Live owner.
                 ;; In the granted fixture, original-source declared read
                 ;; principals, NOT mutation grants, govern the selected
                 ;; panel/fragment/source-view/SSE acquisitions. Public JS
                 ;; assets do not reveal the protected slot state.
                 physical-views (into #{}
                                      (for [[uri operation] (:http-routes app)
                                            :when (= operation :read-only)] uri))
                 read-uris (into physical-views
                                 [panel-uri fragment-uri events-uri])]
             (when-not (= 1 (count physical-views))
               (reject! :incomplete-original-physical-read-routes))
             (reset! guarded* (when principal-source delegated))
             ((:wrap admission)
              (if principal-source
                (principal/guarded-test-read-handler!
                 principal-source app read-uris live-handler)
                live-handler))))
         listener-selection publication-selection storage-selection
         {:live-system live-system :worker-options worker-options
          :close-admission! (:close-and-await! admission)
          :retire-sessions! #(retire-scoped-sessions! registered-streams)}
         (fn [app]
           (let [installed (:biff.ring/handler (:system app))
                 guarded @guarded*]
             (when-not (and (fn? installed)
                            (identical? installed (:handler app))
                            (not (identical? installed (:delegate-handler app)))
                            (or (nil? principal-source)
                                (and (fn? guarded)
                                     (not (identical? guarded installed))
                                     (not (identical? guarded
                                                      (:delegate-handler app)))
                                     ;; Repeat owner-derived route/policy
                                     ;; correspondence AFTER Biff start.
                                     (do (principal/checked-test-policy!
                                          principal-source)
                                         (= (:http-routes app)
                                            (get-in app [:http-report :routes]))))))
               (reject! :composed-live-handler-not-adopted-by-original-biff))
             (try
               ;; Biff has returned the complete selected module inventory,
               ;; and the installed handler and policy have been rechecked.
               ;; Only now may the actual listener enter the delegate.
               (when (and worker-options
                          (nil? (get (:system app) managed/handle-key)))
                 (reject! :missing-started-managed-worker))
               ((:open! admission))
               (f (cond->
                  (assoc app
                         :ring-admission-status (:status admission)
                         :close-ring-admission! (:close-and-await! admission)
                         :live-handler installed
                         :live-system live-system
                         :outstanding-sse-count (fn [] (count (:sessions @registered-streams)))
                         :sse-retirement-status (fn [] (:retirement @registered-streams))
                         :live-report
                         (cond-> (assoc (report app)
                                        :ring-admission-startup :closed-until-biff-and-handler-checks)
                           (= :durable-outbox publication-selection)
                           (assoc :notification-timing :atomic-intent-then-explicit-outbox-cycle
                                  :uncertain-notification-recovery :operator-outbox-reconciliation
                                  :open-obligations
                                  (-> obligations
                                      (disj :postcommit-publication-not-atomic-with-xtdb)
                                      (conj :durable-worker-lifecycle-and-restart-not-established)))
                           principal-source
                           (assoc :selected-mutation-grants
                                  :checked-before-delegated-xtdb-write
                                  :principal-acquisition
                                  :trusted-not-authenticated
                                  :grant-policy-id (:policy-id principal-source)
                                  :grant-catalog-id (:catalog-id principal-source)
                                  :live-subscription-authorization
                                  :request-time-check-of-selected-granted-principal
                                  :live-read-authorization
                                  :request-time-check-of-selected-granted-principal
                                  :live-read-policy
                                  :independent-declared-read-principals
                                  :live-declared-read-principal-count
                                  (count (:read-principals principal-source))
                                  :sse-advisory-event-revalidation
                                  :same-reader-check-before-each-encoded-event
                                  :ongoing-sse-authorization-and-revocation
                                  :event-triggered-only-not-idle-or-in-flight))
                         :live-fragment (live-fragment))
                  (and (= :durable-outbox publication-selection) (nil? worker-options))
                  (assoc :process-outbox! (fn [opts]
                                            (cycle/process-cycle!
                                             (:node app) live-system
                                             (:biff-context app) physical/outbox-source-id opts)))
                  worker-options
                  (assoc :outbox-worker-status
                         #(lifecycle/status (get (:system app) managed/handle-key)))
                  worker-options
                  (update :live-report assoc
                          :notification-timing :atomic-intent-then-scoped-managed-worker
                          :outbox-worker-lifecycle :retired-before-provider-scope-exit
                          :ring-admission-drain :synchronous-before-worker-retirement
                          :worker-adoption :native-biff-start-stop)
                  principal-source
                  (assoc :principal-handler guarded
                         :principal-source principal-source)))
               (finally
                 ;; Native Biff stop owns session retirement in every Live
                 ;; mode, with optional worker retirement before it.
                 ((:close-and-await! admission))))))))
      (finally
        ;; Startup failure fallback; ordinary callback exit has already
        ;; retired sessions while the enclosing providers were available.
        (try
          ((:close-and-await! admission))
          (retire-scoped-sessions! registered-streams)
          (finally (live/close! live-system)))))))

(defn with-live-proving-application!
  "Run the existing unguarded diagnostic Live/Biff/XTDB fixture. The exact
   Live handler is installed at original Biff startup, but this entrypoint
   makes NO authorization claim. For a selected test-only request-grant
   wrapper on the same Biff handler, use with-granted-live-proving-application!.
   Neither entrypoint provides authenticated sessions, CSRF, authorized SSE
   subscriber disclosure, durable outbox or a release credential."
  ([f] (with-live-proving-application! (str "v2323-" (random-uuid)) f))
  ([slot-id f]
   (with-live-application* slot-id nil :no-listener :direct :ephemeral nil f)))

(defn with-granted-live-proving-application!
  "Run one Biff-installed handler whose Live POSTs delegate through the
   checked source-derived principal-grant wrapper BEFORE reaching XTDB.
   The supplied :principal-handler is the installed Live handler's INNER
   mutation delegate, not an alternative serving handler. A separate outer
   GET guard admits the physical view, Live panel/fragment and SSE stream
   only to principals in the independent :read-principals declaration.
   Read permission does NOT follow from mutation permission, and a principal
   can hold read-only permission. Each outgoing advisory SSE event checks that
   the trusted host callback still resolves the same originally admitted
   reader, closing the stream on failed recheck. No periodic idle revocation,
   in-flight delivery exclusion, authenticated callback, general read policy,
   CSRF protection or production certification is established. Test-only."
  ([principal-source f]
   (with-granted-live-proving-application!
    (str "v2329-" (random-uuid)) principal-source f))
  ([slot-id principal-source f]
   ;; nil/invalid source must not silently select the unguarded fixture.
   (principal/checked-test-policy! principal-source)
   (with-live-application* slot-id principal-source :no-listener :direct :ephemeral nil f)))

(defn with-biff-loopback-granted-live-proving-application!
  "Run the SAME guarded Live/principal/XTDB proving fixture with an opt-in
   JDK loopback listener, started and stopped by an additional Biff module.
   Its :loopback-base-url accepts short HTTP replies only; a real streaming
   SSE listener, browser, production Biff server and release remain UNTESTED."
  ([source f]
   (with-biff-loopback-granted-live-proving-application!
    (str "v2333-" (random-uuid)) source f))
  ([slot-id source f]
   (principal/checked-test-policy! source)
   (with-live-application* slot-id source :biff-loopback :direct :ephemeral nil f)))

(defn with-biff-jetty-granted-live-proving-application!
  "Use a REAL Ring Jetty HTTP/1 server attached to the original Biff-started
   and Biff-stopped checked principal->XTDB->Live handler. This is an opt-in
   test-only transport using the :test adapter dependency and loopback port 0.
   The Jetty adapter's handling of Manifold SSE bodies, original principal
   authentication, CSRF, public ingress and production release are NOT
   certified by a successful short-response test."
  ([source f]
   (with-biff-jetty-granted-live-proving-application!
    (str "v2334-" (random-uuid)) source f))
  ([slot-id source f]
   (principal/checked-test-policy! source)
   (with-live-application* slot-id source :biff-jetty-http1 :direct :ephemeral nil f)))

(defn with-biff-aleph-granted-live-proving-application!
  "Select actual Aleph HTTP/1 on loopback with the original Biff-installed
   protected Live handler. Requires the explicit :aleph-integration alias.
   Actual SSE delivery and closure must be observed in the integration gate;
   selection does not establish a release listener, drain, authenticated
   ingress, CSRF, durable publication or browser adoption."
  ([source f]
   (with-biff-aleph-granted-live-proving-application!
    (str "v2335-" (random-uuid)) source f))
  ([slot-id source f]
   (principal/checked-test-policy! source)
   (with-live-application* slot-id source :biff-aleph-http1 :direct :ephemeral nil f)))

(defn with-durable-granted-live-proving-application!
  "Select atomic intent recording on the same principal-guarded Biff handler.
   The trusted host explicitly invokes :process-outbox! with bounded cycle
   options. This is not a background worker, restart-persistence, browser
   delivery, command deduplication or production release guarantee."
  ([slot-id source listener-selection f]
   (with-durable-granted-live-proving-application!
    slot-id source listener-selection :ephemeral f))
  ([slot-id source listener-selection storage-selection f]
   (principal/checked-test-policy! source)
   (with-live-application* slot-id source listener-selection :durable-outbox storage-selection nil f)))

(defn with-managed-durable-granted-live-proving-application!
  "Install the existing managed outbox worker through the original Biff
   lifecycle. Native worker start occurs last; native reverse stop drains
   synchronous admission, retires the worker and retires SSE sessions before
   the listener/lexical provider scope exits. An optional trusted storage
   selection preserves the previous ephemeral arity. The wrapper retries
   timed-out worker drains and owns rollback for this finite fixture inventory.
   This is NOT canonical checked-application startup, bounded shutdown,
   arbitrary crash tolerance, or release qualification."
  ([slot-id source listener-selection worker-options f]
   (with-managed-durable-granted-live-proving-application!
    slot-id source listener-selection :ephemeral worker-options f))
  ([slot-id source listener-selection storage-selection worker-options f]
   (when-not (fn? f) (reject! :invalid-application-callback))
   (principal/checked-test-policy! source)
   (lifecycle/validate-worker-options! worker-options)
   (with-live-application* slot-id source listener-selection :durable-outbox
                           storage-selection worker-options
                           #(f (dissoc % :close-ring-admission!)))))
