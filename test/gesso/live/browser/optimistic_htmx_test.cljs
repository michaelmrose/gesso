(ns gesso.live.browser.optimistic-htmx-test
  "Protocol-v3 HTMX bridge tests.

   These tests exercise the bridge as a physical integration boundary over the
   real browser Core, Choreo, Optimistic, Shell, AdapterState, and portable
   optimistic ExecutablePlan. They intentionally avoid document listeners and
   scheduler sleeps: HTMX lifecycle callbacks are invoked directly and async
   transport handoff is synchronized by the shell's semantic transition seam."
  (:require
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.choreo :as browser-choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.browser.optimistic-htmx :as bridge]
   [gesso.live.browser.shell :as shell]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]))

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- error-kind
  [f]
  (some-> (thrown f) ex-data :error/kind))

(defn- attribute-object
  [name value]
  (doto (js-obj)
    (aset "name" name)
    (aset "value" value)))

(declare fake-element)

(defn- fake-element
  ([tag]
   (fake-element tag {}))
  ([tag initial-attrs]
   (let [node (js-obj)
         attrs (atom (into {} (map (fn [[k v]] [(name k) (str v)])) initial-attrs))
         children (atom [])
         sync-attrs!
         (fn []
           (aset node "attributes"
                 (to-array
                  (map (fn [[k v]] (attribute-object k v)) @attrs))))
         sync-children!
         (fn []
           (let [xs @children]
             (aset node "childNodes" (to-array xs))
             (aset node "children" (to-array xs))
             (aset node "firstChild" (first xs))))]
     (aset node "nodeType" 1)
     (aset node "tagName" (.toUpperCase tag))
     (aset node "id" (get @attrs "id" ""))
     (aset node "isConnected" true)
     (aset node "getAttribute" (fn [attribute] (get @attrs attribute)))
     (aset node "hasAttribute" (fn [attribute] (contains? @attrs attribute)))
     (aset node "setAttribute"
           (fn [attribute value]
             (swap! attrs assoc attribute (str value))
             (when (= "id" attribute)
               (aset node "id" (str value)))
             (sync-attrs!)
             nil))
     (aset node "removeAttribute"
           (fn [attribute]
             (swap! attrs dissoc attribute)
             (when (= "id" attribute)
               (aset node "id" ""))
             (sync-attrs!)
             nil))
     (aset node "appendChild"
           (fn [child]
             (swap! children conj child)
             (aset child "parentNode" node)
             (aset child "parentElement" node)
             (sync-children!)
             child))
     (aset node "removeChild"
           (fn [child]
             (swap! children
                    (fn [xs]
                      (vec (remove #(identical? % child) xs))))
             (aset child "parentNode" nil)
             (aset child "parentElement" nil)
             (sync-children!)
             child))
     (aset node "cloneNode"
           (fn [deep?]
             (let [clone (fake-element tag @attrs)]
               (aset clone "isConnected" false)
               (when deep?
                 (doseq [child @children]
                   (.appendChild clone (.cloneNode child true))))
               clone)))
     (sync-attrs!)
     (sync-children!)
     node)))

(defn- attr
  [element attribute]
  (dom/attr element attribute))

(defn- event
  [type detail]
  (let [prevented? (atom false)
        e (js-obj)]
    (aset e "type" type)
    (aset e "detail" detail)
    (aset e "preventDefault" (fn [] (reset! prevented? true)))
    {:event e
     :prevented? prevented?}))

(defn- xhr
  ([]
   (xhr 200))
  ([status]
   (let [aborted? (atom false)
         x (js-obj)]
     (aset x "status" status)
     (aset x "responseText" "")
     (aset x "abort" (fn [] (reset! aborted? true)))
     {:xhr x
      :aborted? aborted?})))

(defn- parameters
  []
  (js-obj))

(defn- parameter
  [parameters name]
  (aget parameters name))

;; =============================================================================
;; Canonical preverified browser plan fixture
;; =============================================================================

(def browser-plan
  {:gesso.choreo/type :gesso.choreo/executable-plan
   :gesso.choreo/version 1
   :role :browser
   :initial 0
   :states
   {0 {:op :local
       :action optimistic-choreo/derive-provisional-action
       :next 1
       :requires #{:arguments :operation :observed-basis :execution-id :command-id}
       :outputs #{optimistic-choreo/provisional-value-key}}
    1 {:op :send
       :to :authority
       :event protocol/command-event
       :next 2
       :via :http
       :required #{:arguments :operation :observed-basis :execution-id :command-id}
       :optional #{:scope :fact-versions}
       :correlation #{:execution-id :command-id}}
    2 {:op :receive
       :alternatives
       [{:from :authority
         :event protocol/settlement-event
         :next 3
         :via :http
         :required #{optimistic-choreo/settlement-value-key
                     :execution-id
                     :command-id}
         :correlation #{:execution-id :command-id}}]}
    3 {:op :local
       :action optimistic-choreo/resolve-settlement-action
       :next 4
       :requires #{optimistic-choreo/provisional-value-key
                   optimistic-choreo/settlement-value-key}
       :outputs #{optimistic-choreo/resolution-value-key}}
    4 {:op :branch
       :on optimistic-choreo/resolution-value-key
       :cases {:already-incorporated 5
               :confirmed 6
               :failed 7
               :reconciled 8
               :rejected 9}}
    5 {:op :return :outcome :gesso.choreo/complete}
    6 {:op :return :outcome :gesso.choreo/complete}
    7 {:op :return :outcome :gesso.choreo/complete}
    8 {:op :return :outcome :gesso.choreo/complete}
    9 {:op :return :outcome :gesso.choreo/complete}}})

(def command-id
  (identity/command-id "bridge-command-1"))

(def execution-id
  (identity/execution-id "bridge-execution-1"))

(def target-id "request-target")

(def base-action
  {:operation :request/claim
   :arguments {:request-id "request-1"
               :helper-id "helper-1"}
   :observed-basis {:tx-id 10}
   :scope [:request "request-1"]
   :target-id target-id
   :rollback-eligible? true})

(defn- settlement
  [resolution]
  (protocol/settlement
   (cond->
    {:command-id command-id
     :execution-id execution-id
     :resolution resolution}
     (contains? protocol/authoritative-required-resolutions resolution)
     (assoc
      :authoritative
      (protocol/authoritative
       {:presence :present
        :basis {:tx-id 11}
        :projection {:request/status :claimed
                     :request/claimed-by "helper-1"}})))))

;; =============================================================================
;; Deterministic runtime fixture
;; =============================================================================

(defn- transition-bus
  []
  (let [transitions (atom [])
        waiters (atom [])]
    {:transitions transitions
     :on-transition
     (fn [transition]
       (swap! transitions conj transition)
       (doseq [{:keys [pred resolve!]} @waiters]
         (when (pred transition)
           (swap! waiters
                  (fn [xs]
                    (vec (remove #(identical? resolve! (:resolve! %)) xs))))
           (resolve! transition))))
     :wait-for!
     (fn [pred]
       (if-let [existing (first (filter pred @transitions))]
         (js/Promise.resolve existing)
         (js/Promise.
          (fn [resolve _reject]
            (swap! waiters conj {:pred pred :resolve! resolve})))))}))

(defn- fake-document
  []
  (doto (js-obj)
    (aset "addEventListener" (fn [& _] nil))
    (aset "removeEventListener" (fn [& _] nil))
    (aset "querySelectorAll" (fn [& _] (array)))))

(defn- runtime-fixture
  ([]
   (runtime-fixture {}))
  ([{:keys [action project-provisional settlement-value settlement-from-event
             previous-send previous-transport]
     :or {action base-action
          project-provisional
          (fn [{:keys [arguments]}]
            {:request-id (:request-id arguments)
             :state :claimed})}}]
   (let [target (fake-element "details" {"id" target-id
                                         "data-state" "canonical"})
         source (fake-element "button"
                              {bridge/action-attribute (pr-str action)})
         errors (atom [])
         refreshes (atom [])
         bus (transition-bus)
         next-timer (atom 0)
         core-runtime
         (core/create
          {:document (fake-document)
           :htmx (js-obj)
           :shell-options
           {:set-timeout! (fn [_callback _delay-ms]
                            (swap! next-timer inc))
            :clear-timeout! (fn [_] nil)
            :on-transition (:on-transition bus)}})
         choreo-runtime
         (browser-choreo/create
          (core/shell-runtime core-runtime)
          (cond-> {}
            previous-send (assoc :send-payload previous-send)
            previous-transport (assoc :transport-send previous-transport)))
         optimistic-runtime
         (optimistic/create
          choreo-runtime
          {:project-provisional project-provisional
           :render-provisional
           (fn [{:keys [target projection]}]
             (dom/set-attr! target "data-state" (name (:state projection)))
             nil)
           :refresh-authority
           (fn [ctx]
             (swap! refreshes conj ctx)
             nil)
           :resolve-target
           (fn [id]
             (when (= target-id (str id))
               target))
           :process-element identity})
         bridge-runtime
         (bridge/create
          core-runtime
          optimistic-runtime
          {:plan-for (constantly browser-plan)
           :command-id-fn (constantly command-id)
           :execution-id-fn (constantly execution-id)
           :settlement-from-event
           (or settlement-from-event
               (fn [_runtime _event _record]
                 (when settlement-value
                   @settlement-value)))
           :on-error #(swap! errors conj %)})]
     {:target target
      :source source
      :errors errors
      :refreshes refreshes
      :bus bus
      :core core-runtime
      :choreo choreo-runtime
      :optimistic optimistic-runtime
      :bridge bridge-runtime})))

(defn- config-request!
  [fixture]
  (let [params (parameters)
        e (event "htmx:configRequest"
                 #js {:elt (:source fixture)
                      :parameters params})]
    (bridge/on-config-request! (:bridge fixture) (:event e))
    (assoc e :parameters params)))

(defn- before-request!
  [fixture xhr]
  (let [e (event "htmx:beforeRequest"
                 #js {:elt (:source fixture)
                      :xhr xhr})]
    (bridge/on-before-request! (:bridge fixture) (:event e))
    e))

(defn- before-send!
  [fixture xhr]
  (bridge/on-before-send!
   (:bridge fixture)
   (:event (event "htmx:beforeSend" #js {:elt (:source fixture)
                                          :xhr xhr}))))

(defn- after-request!
  [fixture xhr successful?]
  (bridge/on-after-request!
   (:bridge fixture)
   (:event (event "htmx:afterRequest"
                  #js {:elt (:source fixture)
                       :xhr xhr
                       :successful successful?}))))

(defn- transport-success?
  [transition]
  (= :transport/succeeded
     (get-in transition [:event :event])))

;; =============================================================================
;; Construction / ownership
;; =============================================================================

(deftest create-attaches-observers-and-shares-one-shell-test
  (let [fixture (runtime-fixture)
        bridge-runtime (:bridge fixture)
        diagnostics (bridge/diagnostics bridge-runtime)]
    (is (bridge/runtime? bridge-runtime))
    (is (identical? (:core fixture) (bridge/core-runtime bridge-runtime)))
    (is (identical? (:optimistic fixture)
                    (bridge/optimistic-runtime bridge-runtime)))
    (is (identical? (:choreo fixture) (bridge/choreo-runtime bridge-runtime)))
    (is (= (set bridge/observed-events)
           (:observed-events diagnostics)))
    (is (= #{bridge/observer-id}
           (get (core/event-observers (:core fixture))
                "htmx:beforeRequest")))
    (is (true? (:attached? diagnostics)))
    (is (= #{} (:active-executions diagnostics)))))

(deftest create-requires-shared-shell-and-plan-resolver-test
  (let [core-a (core/create {:document (fake-document) :htmx (js-obj)})
        core-b (core/create {:document (fake-document) :htmx (js-obj)})
        choreo-b (browser-choreo/create (core/shell-runtime core-b))
        optimism-b
        (optimistic/create
         choreo-b
         {:project-provisional identity
          :render-provisional (fn [_] nil)
          :refresh-authority (fn [_] nil)})]
    (is (= :different-shell
           (error-kind #(bridge/create core-a optimism-b {:plan-for identity}))))
    (is (= :invalid-callable
           (error-kind #(bridge/create core-b optimism-b {}))))))

(deftest detach-restores-owned-wrappers-and-removes-observers-test
  (let [previous-send (fn [_] {:delegated :send})
        previous-transport (fn [_] {:delegated :transport})
        fixture (runtime-fixture {:previous-send previous-send
                                  :previous-transport previous-transport})
        bridge-runtime (:bridge fixture)
        choreo-runtime (:choreo fixture)]
    (is (= :detached (bridge/detach! bridge-runtime)))
    (is (identical? previous-send
                    (browser-choreo/send-payload-handler choreo-runtime)))
    (is (identical? previous-transport
                    (browser-choreo/transport-handler choreo-runtime)))
    (is (nil? (get (core/event-observers (:core fixture))
                   "htmx:beforeRequest")))
    (is (false? (:attached? (bridge/diagnostics bridge-runtime))))))

;; =============================================================================
;; Wire / preflight
;; =============================================================================

(deftest command-and-settlement-edn-roundtrip-test
  (let [command
        (protocol/command
         {:command-id command-id
          :execution-id execution-id
          :operation :request/claim
          :arguments {:request-id "request-1"}
          :observed-basis {:tx-id 10}
          :scope [:request "request-1"]})
        settlement (settlement :confirmed)]
    (is (= command
           (bridge/decode-command-edn
            (bridge/encode-command-edn command))))
    (is (= settlement
           (bridge/decode-settlement-edn
            (bridge/encode-settlement-edn settlement))))))

(deftest config-request-injects-one-typed-command-with-separated-identities-test
  (let [fixture (runtime-fixture)
        {:keys [parameters prevented?]} (config-request! fixture)
        encoded (parameter parameters bridge/command-parameter)
        command (bridge/decode-command-edn encoded)]
    (is (false? @prevented?))
    (is (= command-id (get command protocol/command-id-key)))
    (is (= execution-id (get command protocol/execution-id-key)))
    (is (not= (get command protocol/command-id-key)
              (get command protocol/execution-id-key)))
    (is (= :request/claim (get command protocol/operation-key)))
    (is (= {:tx-id 10} (get command protocol/observed-basis-key)))
    (is (= #{} (:active-executions (bridge/diagnostics (:bridge fixture)))))))

(deftest malformed-config-is-preserved-until-cancellable-before-request-test
  (let [fixture (runtime-fixture {:action {:operation :request/claim}})
        config (config-request! fixture)
        before (before-request! fixture (:xhr (xhr)))]
    (is (false? @(:prevented? config)))
    (is (true? @(:prevented? before)))
    (is (= [:config-request :before-request]
           (mapv :phase @(:errors fixture))))
    (is (= #{} (:active-executions
                (bridge/diagnostics (:bridge fixture)))))))

(deftest before-request-without-xhr-fails-closed-test
  (let [fixture (runtime-fixture)
        _ (config-request! fixture)
        before (event "htmx:beforeRequest" #js {:elt (:source fixture)})]
    (bridge/on-before-request! (:bridge fixture) (:event before))
    (is (true? @(:prevented? before)))
    (is (= :before-request (:phase (last @(:errors fixture)))))
    (is (= #{} (:active-executions
                (bridge/diagnostics (:bridge fixture)))))))

;; =============================================================================
;; HTMX transport lifecycle
;; =============================================================================

(deftest before-request-installs-provisional-but-before-send-owns-network-handoff-test
  (async done
    (let [fixture (runtime-fixture)
          x (:xhr (xhr))
          _ (config-request! fixture)
          before (before-request! fixture x)
          success ((:wait-for! (:bus fixture)) transport-success?)]
      (is (false? @(:prevented? before)))
      (is (= "claimed" (attr (:target fixture) "data-state")))
      (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
      (is (= #{(protocol/execution-id->wire execution-id)}
             (:active-executions (bridge/diagnostics (:bridge fixture)))))
      (is (nil? (first (filter transport-success?
                               @(:transitions (:bus fixture))))))
      (before-send! fixture x)
      (.then success
             (fn [_]
               (is (= :provisional
                      (:status
                       (adapter/optimistic-scope
                        (shell/state (core/shell-runtime (:core fixture)))
                        execution-id))))
               (done))))))

(deftest successful-response-settles-correlated-execution-and-forgets-http-record-test
  (async done
    (let [settlement* (atom nil)
          fixture (runtime-fixture {:settlement-value settlement*})
          x (:xhr (xhr))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (reset! settlement* (settlement :confirmed))
               (after-request! fixture x true)
               (is (= #{} (:active-executions
                           (bridge/diagnostics (:bridge fixture)))))
               (is (nil? (adapter/optimistic-scope
                          (shell/state (core/shell-runtime (:core fixture)))
                          execution-id)))
               (is (= :await-authority
                      (:reason (last @(:refreshes fixture)))))
               (done))))))

(deftest successful-response-without-settlement-leaves-semantic-optimism-pending-test
  (async done
    (let [settlement* (atom nil)
          fixture (runtime-fixture {:settlement-value settlement*})
          x (:xhr (xhr))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (after-request! fixture x true)
               (is (= #{} (:active-executions
                           (bridge/diagnostics (:bridge fixture)))))
               (is (= :provisional
                      (:status
                       (adapter/optimistic-scope
                        (shell/state (core/shell-runtime (:core fixture)))
                        execution-id))))
               (is (empty? @(:refreshes fixture)))
               (done))))))

(deftest network-failure-after-send-retires-semantic-optimism-without-fabricating-settlement-test
  (async done
    (let [fixture (runtime-fixture)
          x (:xhr (xhr 0))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (bridge/on-request-failed!
                (:bridge fixture)
                (:event (event "htmx:sendError" #js {:elt (:source fixture)
                                                       :xhr x})))
               (is (= #{} (:active-executions
                           (bridge/diagnostics (:bridge fixture)))))
               (is (nil? (adapter/optimistic-scope
                          (shell/state (core/shell-runtime (:core fixture)))
                          execution-id)))
               (is (= :rollback-and-refresh
                      (:disposition
                       (last
                        (for [transition @(:transitions (:bus fixture))
                              [kind data] (:effects transition)
                              :when (= :optimistic/finish kind)]
                          data)))))
               (done))))))

(deftest lost-settlement-response-after-send-recovers-through-authority-test
  (async done
    (let [settlement* (atom nil)
          fixture (runtime-fixture {:settlement-value settlement*})
          x (:xhr (xhr 500))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               ;; Model a server that committed and would have returned a valid
               ;; settlement, but whose HTTP response became unusable before the
               ;; browser could trust it.  A responseError is therefore local
               ;; uncertainty, not a semantic rejection and not permission to
               ;; consume response settlement bytes.
               (reset! settlement* (settlement :confirmed))
               (bridge/on-request-failed!
                (:bridge fixture)
                (:event (event "htmx:responseError"
                               #js {:elt (:source fixture)
                                    :xhr x})))
               (is (= #{} (:active-executions
                           (bridge/diagnostics (:bridge fixture)))))
               (is (nil? (adapter/optimistic-scope
                          (shell/state (core/shell-runtime (:core fixture)))
                          execution-id)))
               (is (= "canonical"
                      (attr (:target fixture) "data-state")))
               (is (= [:rollback-and-refresh]
                      (mapv :reason @(:refreshes fixture))))
               (is (not-any?
                    #(= :optimistic/settlement-observed
                        (get-in % [:event :event]))
                    @(:transitions (:bus fixture))))
               (is (= :network-failed
                      (:resolution
                       (last
                        (for [transition @(:transitions (:bus fixture))
                              [kind data] (:effects transition)
                              :when (= :optimistic/finish kind)]
                          data)))))
               (done))))))

(deftest incompatible-settlement-protocol-retires-and-reconstructs-authority-test
  (async done
    (let [incompatible-wire
          (assoc (protocol/settlement->wire (settlement :confirmed))
                 protocol/protocol-version-key
                 "4")
          fixture
          (runtime-fixture
           {:settlement-from-event
            (fn [_runtime _event _record]
              (bridge/decode-settlement-edn (pr-str incompatible-wire)))})
          x (:xhr (xhr))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (after-request! fixture x true)
               (let [finish
                     (last
                      (for [transition @(:transitions (:bus fixture))
                            [kind data] (:effects transition)
                            :when (= :optimistic/finish kind)]
                        data))
                     diagnostic (last @(:errors fixture))]
                 (is (= #{} (:active-executions
                             (bridge/diagnostics (:bridge fixture)))))
                 (is (nil? (adapter/optimistic-scope
                            (shell/state (core/shell-runtime (:core fixture)))
                            execution-id)))
                 (is (= "canonical"
                        (attr (:target fixture) "data-state")))
                 (is (= [:rollback-and-refresh]
                        (mapv :reason @(:refreshes fixture))))
                 (is (= :incompatible-protocol (:resolution finish)))
                 (is (= :rollback-and-refresh (:disposition finish)))
                 (is (not-any?
                      #(= :optimistic/settlement-observed
                          (get-in % [:event :event]))
                      @(:transitions (:bus fixture))))
                 (is (= :settlement-protocol-incompatible
                        (:phase diagnostic)))
                 (is (= :incompatible
                        (get-in diagnostic
                                [:protocol/version-status :status])))
                 (is (= "4"
                        (get-in diagnostic
                                [:protocol/version-status :encountered])))
                 (is (= protocol/version
                        (get-in diagnostic
                                [:protocol/version-status :supported]))))
               (done))))))

(deftest malformed-future-looking-settlement-does-not-trigger-stale-protocol-recovery-test
  (async done
    (let [malformed-wire
          {protocol/protocol-version-key "4"}
          fixture
          (runtime-fixture
           {:settlement-from-event
            (fn [_runtime _event _record]
              (bridge/decode-settlement-edn (pr-str malformed-wire)))})
          x (:xhr (xhr))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (after-request! fixture x true)
               ;; The physical HTTP correlation is terminal after afterRequest,
               ;; even though malformed settlement bytes cannot manufacture a
               ;; semantic outcome.  The semantic optimistic execution therefore
               ;; remains pending independently of the forgotten XHR record.
               (is (= #{}
                      (:active-executions
                       (bridge/diagnostics (:bridge fixture)))))
               (is (= :provisional
                      (:status
                       (adapter/optimistic-scope
                        (shell/state (core/shell-runtime (:core fixture)))
                        execution-id))))
               (is (= "claimed"
                      (attr (:target fixture) "data-state")))
               (is (empty? @(:refreshes fixture)))
               (is (not-any?
                    (fn [transition]
                      (some
                       (fn [[kind data]]
                         (and (= :optimistic/finish kind)
                              (= :incompatible-protocol (:resolution data))))
                       (:effects transition)))
                    @(:transitions (:bus fixture))))
               (is (= :settlement-response
                      (:phase (last @(:errors fixture)))))
               (is (not= :settlement-protocol-incompatible
                         (:phase (last @(:errors fixture)))))
               (done))))))

(deftest duplicate-terminal-http-callback-is-harmless-test
  (async done
    (let [settlement* (atom nil)
          fixture (runtime-fixture {:settlement-value settlement*})
          x (:xhr (xhr))
          _ (config-request! fixture)
          _ (before-request! fixture x)
          sent ((:wait-for! (:bus fixture)) transport-success?)]
      (before-send! fixture x)
      (.then sent
             (fn [_]
               (reset! settlement* (settlement :confirmed))
               (after-request! fixture x true)
               (let [transition-count (count @(:transitions (:bus fixture)))
                     refresh-count (count @(:refreshes fixture))]
                 (after-request! fixture x true)
                 (bridge/on-request-failed!
                  (:bridge fixture)
                  (:event (event "htmx:abort" #js {:elt (:source fixture)
                                                    :xhr x})))
                 (is (= transition-count
                        (count @(:transitions (:bus fixture)))))
                 (is (= refresh-count (count @(:refreshes fixture))))
                 (is (= #{} (:active-executions
                             (bridge/diagnostics (:bridge fixture))))))
               (done))))))
