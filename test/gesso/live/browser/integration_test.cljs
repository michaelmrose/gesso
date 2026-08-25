(ns gesso.live.browser.integration-test
  "Adapter-backed browser integration tests.

   This namespace exercises the current Gesso-owned browser stack in a real DOM:

     browser.core -> shell -> adapter <- browser.choreo
                           |
                      continuity

   HTMX itself is treated as an external runtime. These tests drive the exact
   documented lifecycle event vocabulary through core's installed document
   listeners while a narrow HTMX seam owns only physical trigger initiation.
   The later pinned HTMX/SSE harness remains responsible for testing the exact
   third-party library versions end-to-end.

   No pre-v4.5 optimistic runtime or browser-global execution registry is used."
  (:require
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.choreo.core :as c]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Real DOM + lifecycle fixture
;; =============================================================================

(defn- element
  ([tag]
   (element tag nil))
  ([tag attrs]
   (let [node (.createElement js/document tag)]
     (doseq [[attribute value] attrs
             :when (some? value)]
       (.setAttribute node (dom/attr-name attribute) (str value)))
     node)))

(defn- append!
  [parent child]
  (.appendChild parent child)
  child)

(defn- text!
  [node value]
  (set! (.-textContent node) value)
  node)

(defn- config-json
  [value]
  (.stringify js/JSON (clj->js value)))

(defn- sandbox!
  []
  (let [root (element "div"
                      {:data-gesso-browser-integration
                       (str "fixture-" (random-uuid))})]
    (.appendChild (.-body js/document) root)
    root))

(defn- xhr
  ([id]
   (xhr id 200))
  ([id status]
   (let [aborted? (atom false)
         value (js-obj)]
     (aset value "id" id)
     (aset value "status" status)
     (aset value "abort"
           (fn []
             (reset! aborted? true)
             true))
     {:xhr value
      :aborted? aborted?})))

(defn- lifecycle-event!
  ([target type detail]
   (lifecycle-event! target type detail true))
  ([target type detail cancelable?]
   (let [event (js/CustomEvent.
                type
                #js {:bubbles true
                     :cancelable cancelable?
                     :detail detail})]
     (.dispatchEvent target event)
     event)))

(defn- authoritative-parser
  [event]
  (let [detail (when event (.-detail event))
        value (when detail (aget detail "authoritative"))]
    (when value
      (js->clj value :keywordize-keys true))))

(defn- fragment-tree!
  ([sandbox-root fragment-id]
   (fragment-tree! sandbox-root fragment-id nil))
  ([sandbox-root fragment-id {:keys [target-tag]
                              :or {target-tag "details"}}]
   (let [target-id (str fragment-id "-card")
         root (append!
               sandbox-root
               (element
                "section"
                {core/fragment-attribute fragment-id
                 continuity/continuity-attr "true"
                 continuity/continuity-fragment-attr-key target-id
                 continuity/continuity-config-attr-key
                 (config-json
                  {:enabled true
                   :preserve {:inputs true
                              :focus true}
                   :boxes [{:type "details-open"}]})}))
         target (append! root (element target-tag {:id target-id}))
         summary (append! target (text! (element "summary") "old server"))
         input (append! target (element "input" {:id (str target-id "-input")
                                                 :type "text"}))]
     (set! (.-value input) "server old")
     {:root root
      :fragment-id fragment-id
      :target-id target-id
      :target target
      :summary summary
      :input input})))

(defn- replacement-target
  [fixture tag text value]
  (let [target (element tag {:id (:target-id fixture)})
        summary (append! target (text! (element "summary") text))
        input (append! target (element "input"
                                       {:id (str (:target-id fixture) "-input")
                                        :type "text"}))]
    (set! (.-value input) value)
    {:target target
     :summary summary
     :input input}))

(defn- sequential-id-generator
  [prefix]
  (let [n (atom 0)]
    (fn []
      (str prefix (swap! n inc)))))

(defn- host-fixture!
  ([]
   (host-fixture! nil))
  ([options]
   (let [sandbox-root (sandbox!)
         triggers (atom [])
         current-xhrs (atom {})
         xhr-counter (atom 0)
         htmx (js-obj)
         runtime* (atom nil)]
     (aset htmx "trigger"
           (fn [root name detail]
             (swap! triggers conj {:root root :name name :detail detail})
             (when (= name core/refresh-event-name)
               ;; Model the one piece HTMX owns before Gesso sees the request:
               ;; a physical XHR exists and the documented beforeRequest event
               ;; is emitted from the managed fragment root.
               (let [request-number (swap! xhr-counter inc)
                     request (xhr (str "xhr-" request-number))
                     fragment-id (core/fragment-id-from-root root)]
                 (swap! current-xhrs assoc fragment-id request)
                 (lifecycle-event!
                  root
                  "htmx:beforeRequest"
                  #js {:elt root
                       :xhr (:xhr request)})))
             true))
     (let [core-runtime
           (core/create
            (merge
             {:document js/document
              :htmx htmx
              :request-id-fn (sequential-id-generator "request-")
              :authoritative-from-event authoritative-parser
              :continuity-options
              {:request-animation-frame!
               (fn [f]
                 ;; Keep layout scheduling deterministic while still exercising
                 ;; the real continuity implementation and Promise completion.
                 (f)
                 1)}}
             options))
           _ (reset! runtime* core-runtime)
           _ (core/start! core-runtime)
           choreo-runtime (choreo/create (core/shell-runtime core-runtime))]
       {:sandbox sandbox-root
        :core core-runtime
        :choreo choreo-runtime
        :shell (core/shell-runtime core-runtime)
        :htmx htmx
        :triggers triggers
        :current-xhrs current-xhrs}))))

(defn- cleanup-host!
  [{:keys [sandbox core choreo]}]
  (when (choreo/runtime? choreo)
    (choreo/detach! choreo))
  (when (core/core? core)
    (core/stop! core))
  (when (and sandbox (.-isConnected sandbox))
    (.remove sandbox))
  true)

(defn- with-host*
  [f]
  (let [host (host-fixture!)]
    (try
      (f host)
      (finally
        (cleanup-host! host)))))

(defn- current-xhr
  [host fragment-id]
  (get-in @(:current-xhrs host) [fragment-id :xhr]))

(defn- current-aborted?
  [host fragment-id]
  (get-in @(:current-xhrs host) [fragment-id :aborted?]))

(defn- request-record
  [host root]
  (core/active-request (:core host) root))

(defn- fragment-state
  [host fragment-id]
  (adapter/fragment-state (core/state (:core host)) fragment-id))

(defn- invariant-clean?
  [host]
  (empty? (adapter/invariant-errors (core/state (:core host)))))

(defn- complete-request!
  [host fixture]
  (let [xhr (current-xhr host (:fragment-id fixture))]
    (lifecycle-event!
     (:root fixture)
     "htmx:afterRequest"
     #js {:elt (:root fixture)
          :xhr xhr
          :successful true})))

(defn- before-swap!
  ([host fixture]
   (before-swap! host fixture nil))
  ([host fixture authoritative]
   (let [xhr (current-xhr host (:fragment-id fixture))
         detail (js-obj)]
     (aset detail "elt" (:root fixture))
     (aset detail "xhr" xhr)
     (aset detail "shouldSwap" true)
     (when authoritative
       (aset detail "authoritative" (clj->js authoritative)))
     {:detail detail
      :event (lifecycle-event! (:root fixture)
                               "htmx:beforeSwap"
                               detail)})))

(defn- after-swap!
  [host fixture]
  (let [xhr (current-xhr host (:fragment-id fixture))]
    (lifecycle-event!
     (:root fixture)
     "htmx:afterSwap"
     #js {:elt (:root fixture)
          :xhr xhr})))

(defn- local-once
  []
  (c/->choreography
   {:initial :work
    :states
    {:work (c/local :browser :browser/work :done {:outputs #{:value}})
     :done (c/return :done)}}))

(defn- browser-execution
  [choreography]
  (machine/start (project/project choreography :browser)))

;; =============================================================================
;; One shared semantic browser runtime
;; =============================================================================

(deftest core-and-choreo-share-one-shell-and-one-adapter-state-test
  (with-host*
   (fn [host]
     (is (identical? (:shell host)
                     (choreo/shell-runtime (:choreo host))))
     (is (= (core/state (:core host))
            (choreo/state (:choreo host))))
     (is (invariant-clean? host))
     (is (not (contains? (:choreo host) :executions)))
     (is (not (contains? (:core host) :semantic-state))))))

;; =============================================================================
;; Invalidation -> HTMX lifecycle -> adapter request ownership
;; =============================================================================

(deftest invalidation-reenters-through-installed-before-request-listener-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" {:basis 1})
       (is (= 1 (count @(:triggers host))))
       (is (= core/refresh-event-name (:name (first @(:triggers host)))))
       (is (= {:fragment-id "fragment-a"
               :request-generation 1
               :request-id "request-1"
               :requirements #{{:basis 1}}}
              (request-record host (:root fixture))))
       (is (nil? (core/pending-refresh (:core host) (:root fixture))))
       (is (invariant-clean? host))))))

(deftest unmanaged-htmx-lifecycle-is-ignored-test
  (with-host*
   (fn [host]
     (let [ordinary (append! (:sandbox host) (element "button"))
           request (xhr "ordinary")
           event (lifecycle-event!
                  ordinary
                  "htmx:beforeRequest"
                  #js {:elt ordinary :xhr (:xhr request)})]
       (is (false? (.-defaultPrevented event)))
       (is (= {} (:fragments (core/state (:core host)))))
       (is (invariant-clean? host))))))

(deftest invalidation-storm-keeps-one-request-and-one-queued-refresh-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (core/notify-fragment! (:core host) "fragment-a" :basis/b)
       (core/notify-fragment! (:core host) "fragment-a" :basis/c)
       (is (= 1 (count @(:triggers host))))
       (is (= #{:basis/b :basis/c}
              (get-in (fragment-state host "fragment-a")
                      [:queued-requirements])))
       (complete-request! host fixture)
       (is (= 2 (count @(:triggers host))))
       (is (= #{:basis/b :basis/c}
              (:requirements (request-record host (:root fixture)))))
       (is (= #{}
              (get-in (fragment-state host "fragment-a")
                      [:queued-requirements])))
       (is (invariant-clean? host))))))

(deftest late-completion-from-old-xhr-cannot-finish-new-request-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [old-xhr (current-xhr host "fragment-a")]
         (core/notify-fragment! (:core host) "fragment-a" :basis/b)
         (complete-request! host fixture)
         (let [new-record (request-record host (:root fixture))]
           (is (= "request-2" (:request-id new-record)))
           (lifecycle-event!
            (:root fixture)
            "htmx:afterRequest"
            #js {:elt (:root fixture)
                 :xhr old-xhr
                 :successful true})
           (is (= new-record (request-record host (:root fixture))))
           (is (some? (get-in (fragment-state host "fragment-a") [:inflight])))
           (is (invariant-clean? host))))))))

;; =============================================================================
;; Actual DOM replacement + continuity + authority
;; =============================================================================

(deftest authoritative-swap-can-change-target-root-tag-and-preserve-local-state-test
  (async done
    (let [host (host-fixture!)
          fixture (fragment-tree! (:sandbox host) "fragment-a" {:target-tag "details"})]
      (set! (.-open (:target fixture)) true)
      (set! (.-value (:input fixture)) "user edit")
      (.focus (:input fixture))
      (.setSelectionRange (:input fixture) 2 6 "forward")
      (core/notify-fragment! (:core host) "fragment-a" :basis/a)
      (let [{:keys [event detail]}
            (before-swap!
             host fixture
             {:scope "request/a"
              :basis "basis/a"})
            replacement (replacement-target fixture "article" "new server" "server new")]
        (is (false? (.-defaultPrevented event)))
        (is (true? (aget detail "shouldSwap")))
        (.replaceWith (:target fixture) (:target replacement))
        (after-swap! host fixture)
        (complete-request! host fixture)
        ;; continuity/restore returns a Promise through shell; give that Promise
        ;; one turn to dispatch :continuity/completed back through the adapter.
        (.then
         (js/Promise.resolve nil)
         (fn [_]
           (try
             (is (= "ARTICLE" (.-tagName (:target replacement))))
             (is (= "new server" (.-textContent (:summary replacement))))
             (is (= "user edit" (.-value (:input replacement))))
             (is (identical? (:input replacement) (.-activeElement js/document)))
             (is (= 2 (.-selectionStart (:input replacement))))
             (is (= 6 (.-selectionEnd (:input replacement))))
             (is (= {:basis "basis/a"}
                    (adapter/authoritative-frontier
                     (core/state (:core host))
                     "request/a")))
             (is (zero? (:continuity (shell/resource-counts (:shell host)))))
             (is (invariant-clean? host))
             (finally
               (cleanup-host! host)
               (done)))))))))

(deftest nonmonotone-authoritative-swap-is-physically-cancelled-test
  (async done
    (let [host (host-fixture!)
          fixture (fragment-tree! (:sandbox host) "fragment-a")]
      ;; First install an authoritative frontier.
      (core/notify-fragment! (:core host) "fragment-a" :basis/a)
      (before-swap! host fixture {:scope "request/a" :basis "basis/a"})
      (after-swap! host fixture)
      (complete-request! host fixture)
      (.then
       (js/Promise.resolve nil)
       (fn [_]
         (try
           ;; Second request attempts a different basis without the exact
           ;; advancement witness required by the adapter.
           (core/notify-fragment! (:core host) "fragment-a" :basis/stale)
           (let [{:keys [event detail]}
                 (before-swap!
                  host fixture
                  {:scope "request/a"
                   :basis "basis/stale"})]
             (is (true? (.-defaultPrevented event)))
             (is (false? (aget detail "shouldSwap")))
             (is (= {:basis "basis/a"}
                    (adapter/authoritative-frontier
                     (core/state (:core host))
                     "request/a")))
             (is (invariant-clean? host)))
           (finally
             (cleanup-host! host)
             (done))))))))

;; =============================================================================
;; Request failure / disappearance
;; =============================================================================

(deftest failed-request-advances-queued-generation-without-leaking-old-request-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [old-xhr (current-xhr host "fragment-a")]
         (core/notify-fragment! (:core host) "fragment-a" :basis/b)
         (lifecycle-event!
          (:root fixture)
          "htmx:sendError"
          #js {:elt (:root fixture)
               :xhr old-xhr})
         (is (= "request-2"
                (:request-id (request-record host (:root fixture)))))
         (is (= #{:basis/b}
                (:requirements (request-record host (:root fixture)))))
         (is (= 2 (count @(:triggers host))))
         (is (invariant-clean? host)))))))

(deftest fragment-cleanup-retires-semantic-request-before-physical-removal-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [aborted? (current-aborted? host "fragment-a")]
         (is (false? @aborted?))
         (lifecycle-event!
          (:root fixture)
          "htmx:beforeCleanupElement"
          #js {:elt (:root fixture)})
         (is (true? @aborted?))
         (is (nil? (fragment-state host "fragment-a")))
         (is (nil? (request-record host (:root fixture))))
         (is (invariant-clean? host)))))))

;; =============================================================================
;; Choreo and fragment lifecycle coexist without alternate ownership
;; =============================================================================

(deftest choreo-local-completion-and-fragment-request-share-shell-without-interference-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")
           local-calls (atom [])]
       (choreo/register-local-action!
        (:choreo host)
        :browser/work
        (fn [context]
          (swap! local-calls conj context)
          {:value 42}))
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [fragment-before (fragment-state host "fragment-a")
             result (choreo/start-execution!
                     (:choreo host)
                     :execution/local
                     (browser-execution (local-once)))]
         (is (nil? (:execution-ref result)))
         (is (= 1 (count @local-calls)))
         (is (= fragment-before (fragment-state host "fragment-a")))
         (is (some? (request-record host (:root fixture))))
         (is (= #{} (choreo/active-execution-ids (:choreo host))))
         (is (invariant-clean? host)))))))

(deftest stale-choreo-callback-cannot-mutate-replacement-execution-while-fragment-active-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")
           execution (browser-execution
                      (c/->choreography
                       {:initial :wait
                        :states
                        {:wait (c/await :browser {:browser/go :done})
                         :done (c/return :done)}}))]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [first-result (choreo/start-execution! (:choreo host) :execution/a execution)
             stale-ref (:execution-ref first-result)
             second-result (choreo/start-execution!
                            (:choreo host)
                            :execution/a
                            execution
                            {:replace-execution? true})
             current-ref (:execution-ref second-result)
             before (choreo/execution (:choreo host) :execution/a)]
         (is (not= (:generation stale-ref) (:generation current-ref)))
         (choreo/deliver-environment!
          (:choreo host)
          stale-ref
          {:event :browser/go})
         (is (= before (choreo/execution (:choreo host) :execution/a)))
         (is (some? (request-record host (:root fixture))))
         (is (invariant-clean? host)))))))

;; =============================================================================
;; Diagnostics / teardown
;; =============================================================================

(deftest diagnostics-and-adapter-state-do-not-retain-dom-or-xhr-objects-test
  (with-host*
   (fn [host]
     (let [fixture (fragment-tree! (:sandbox host) "fragment-a")]
       (core/notify-fragment! (:core host) "fragment-a" :basis/a)
       (let [diagnostics (core/diagnostics (:core host))
             printed-state (pr-str (core/state (:core host)))
             printed-diagnostics (pr-str diagnostics)]
         (is (not (.includes printed-state "HTML")))
         (is (not (.includes printed-state "XMLHttpRequest")))
         (is (not (.includes printed-diagnostics "HTML")))
         (is (not (.includes printed-diagnostics "XMLHttpRequest")))
         (is (= 0 (:timers (shell/resource-counts (:shell host)))))
         (is (some? (request-record host (:root fixture))))
         (is (invariant-clean? host)))))))

(deftest core-stop-removes-listeners-and-semantically-retires-owned-work-test
  (let [host (host-fixture!)
        fixture (fragment-tree! (:sandbox host) "fragment-a")]
    (try
      (core/notify-fragment! (:core host) "fragment-a" :basis/a)
      (is (some? (fragment-state host "fragment-a")))
      (core/stop! (:core host))
      (is (nil? (fragment-state host "fragment-a")))
      (is (shell/closed? (:shell host)))
      ;; The listeners are gone. A physical callback emitted after shutdown
      ;; therefore cannot recreate or mutate semantic ownership.
      (let [event (lifecycle-event!
                   (:root fixture)
                   "htmx:afterRequest"
                   #js {:elt (:root fixture)
                        :xhr (current-xhr host "fragment-a")
                        :successful true})]
        (is (false? (.-defaultPrevented event)))
        (is (nil? (fragment-state host "fragment-a"))))
      (is (invariant-clean? host))
      (finally
        (cleanup-host! host)))))
