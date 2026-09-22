(ns gesso.live.browser.optimistic-test
  "Protocol-v3 browser optimism integration tests.

   The production namespace is intentionally a thin physical realization over
   Choreo + AdapterState + Shell. These tests therefore exercise that vertical
   composition rather than reconstructing an independent optimistic state
   machine in the test suite.

   The fake element host below implements only the DOM primitives used by
   gesso.live.browser.dom structural snapshots and in-place restoration. This
   keeps the suite runnable under Node while still testing opaque physical
   resource ownership, marker safety, rollback, and refresh decisions."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.choreo :as browser-choreo]
   [gesso.live.browser.optimistic :as optimistic]
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

(defn- error-type
  [f]
  (some-> (thrown f) ex-data :error/type))

;; =============================================================================
;; Minimal Node-compatible element host
;; =============================================================================

(declare fake-element)

(defn- attribute-object
  [name value]
  (doto (js-obj)
    (aset "name" name)
    (aset "value" value)))

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
     (aset node "tagName" (str/upper-case tag))
     (aset node "isConnected" true)
     (aset node "getAttribute" (fn [attribute] (get @attrs attribute)))
     (aset node "hasAttribute" (fn [attribute] (contains? @attrs attribute)))
     (aset node "setAttribute"
           (fn [attribute value]
             (swap! attrs assoc attribute (str value))
             (sync-attrs!)
             nil))
     (aset node "removeAttribute"
           (fn [attribute]
             (swap! attrs dissoc attribute)
             (sync-attrs!)
             nil))
     (aset node "appendChild"
           (fn [child]
             (swap! children conj child)
             (aset child "parentNode" node)
             (sync-children!)
             child))
     (aset node "removeChild"
           (fn [child]
             (swap! children
                    (fn [xs]
                      (vec (remove #(identical? % child) xs))))
             (aset child "parentNode" nil)
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
  [element name]
  (.getAttribute element name))

(defn- set-attr!
  [element name value]
  (.setAttribute element name value)
  element)

;; =============================================================================
;; Deterministic timer host
;; =============================================================================

(defn- timer-host
  []
  (let [next-id (atom 0)
        active (atom {})
        cleared (atom [])]
    {:active active
     :cleared cleared
     :set-timeout!
     (fn [callback delay-ms]
       (let [id (swap! next-id inc)]
         (swap! active assoc id {:callback callback
                                 :delay-ms delay-ms})
         id))
     :clear-timeout!
     (fn [id]
       (swap! cleared conj id)
       (swap! active dissoc id)
       nil)
     :fire!
     (fn [id]
       (let [{:keys [callback]} (get @active id)]
         (swap! active dissoc id)
         (when callback
           (callback))))}))

(defn- only-timer-id
  [timers]
  (first (keys @(:active timers))))

(defn- controlled-thenable
  []
  (let [on-success (atom nil)
        on-failure (atom nil)
        value (js-obj)]
    (aset value "then"
          (fn [success failure]
            (reset! on-success success)
            (reset! on-failure failure)
            value))
    {:value value
     :resolve! (fn [x] (when-let [f @on-success] (f x)))
     :reject! (fn [x] (when-let [f @on-failure] (f x)))}))

(defn- deferred-promise
  []
  (let [resolve! (atom nil)
        reject! (atom nil)
        promise
        (js/Promise.
         (fn [resolve reject]
           (reset! resolve! resolve)
           (reset! reject! reject)))]
    {:promise promise
     :resolve! (fn [value] (@resolve! value))
     :reject! (fn [error] (@reject! error))}))

(defn- after-promises
  [f]
  (.then
   (js/Promise.resolve nil)
   (fn [_]
     (js/setTimeout f 0))))

;; =============================================================================
;; Preverified browser ExecutablePlan fixture
;; =============================================================================

;; This is the canonical browser projection emitted by
;; optimistic-choreo/command-plan for one direct command choreography. Production
;; code receives such plans from the verified build artifact; browser code does
;; not project or verify choreography at runtime.
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

;; =============================================================================
;; Protocol fixtures
;; =============================================================================

(defn- command
  [suffix]
  (protocol/command
   {:command-id (identity/command-id (str "command-" suffix))
    :execution-id (identity/execution-id (str "execution-" suffix))
    :operation :request/claim
    :arguments {:request-id (str "request-" suffix)}
    :observed-basis {:tx-id 10}
    :scope [:request (str "request-" suffix)]}))

(defn- authoritative
  [projection]
  (protocol/authoritative
   {:presence :present
    :basis {:tx-id 11}
    :projection projection}))

(defn- settlement
  ([command resolution]
   (settlement command resolution nil))
  ([command resolution authority]
   (protocol/settlement
    (cond->
     {:command-id (get command protocol/command-id-key)
      :execution-id (get command protocol/execution-id-key)
      :resolution resolution}
      authority
      (assoc :authoritative authority)))))

;; =============================================================================
;; Full runtime fixture
;; =============================================================================

(defn- runtime-fixture
  ([]
   (runtime-fixture nil))
  ([{:keys [project-provisional
            render-provisional
            rollback-eligible?
            transport-send
            resolve-target
            process-element
            refresh-authority]
     :or {project-provisional
          (fn [{:keys [arguments]}]
            {:state :claimed
             :request-id (:request-id arguments)})
          render-provisional
          (fn [{:keys [target projection]}]
            (set-attr! target "data-state" (name (:state projection)))
            nil)
          rollback-eligible? true}}]
   (let [target (fake-element "details"
                              {"id" "request-target"
                               "data-state" "canonical"
                               "data-existing" "preserved"})
         timers (timer-host)
         sent (atom [])
         refreshes (atom [])
         processed (atom [])
         transitions (atom [])
         diagnostics (atom [])
         errors (atom [])
         choreo-holder (atom nil)
         shell-runtime
         (shell/create
          {:set-timeout! (:set-timeout! timers)
           :clear-timeout! (:clear-timeout! timers)
           :on-transition #(swap! transitions conj %)
           :on-diagnostic #(swap! diagnostics conj %)
           :on-error #(swap! errors conj %)})
         choreo-runtime
         (browser-choreo/create
          shell-runtime
          {:send-payload
           (fn [ctx]
             (let [execution-id
                   (get ctx browser-choreo/execution-id-key)
                   execution
                   (browser-choreo/execution @choreo-holder execution-id)
                   action
                   (get ctx browser-choreo/action-key)
                   payload-keys
                   (set/union (or (:required action) #{})
                              (or (:optional action) #{}))]
               (select-keys
                (machine/execution-values execution)
                payload-keys)))
           :transport-send
           (or transport-send
               (fn [ctx]
                 (swap! sent conj (get ctx browser-choreo/message-key))
                 nil))})
         _ (reset! choreo-holder choreo-runtime)
         optimistic-runtime
         (optimistic/create
          choreo-runtime
          {:project-provisional project-provisional
           :render-provisional render-provisional
           :refresh-authority
           (or refresh-authority
               (fn [ctx]
                 (swap! refreshes conj ctx)
                 nil))
           :resolve-target
           (or resolve-target
               (fn [target-id]
                 (when (= "request-target" (str target-id))
                   target)))
           :process-element
           (or process-element
               (fn [element]
                 (swap! processed conj element)
                 element))})]
     {:target target
      :timers timers
      :sent sent
      :refreshes refreshes
      :processed processed
      :transitions transitions
      :diagnostics diagnostics
      :errors errors
      :shell shell-runtime
      :choreo choreo-runtime
      :optimistic optimistic-runtime
      :rollback-eligible? rollback-eligible?})))

(defn- start!
  ([fixture command]
   (start! fixture command nil))
  ([fixture command opts]
   (optimistic/start!
    (:optimistic fixture)
    (merge
     {:plan browser-plan
      :command command
      :target-id "request-target"
      :rollback-eligible? (:rollback-eligible? fixture)}
     opts))))

(defn- optimistic-scope
  [fixture command]
  (adapter/optimistic-scope
   (optimistic/state (:optimistic fixture))
   (get command protocol/execution-id-key)))

(defn- effect-data
  [fixture effect-kind]
  (vec
   (for [transition @(:transitions fixture)
         [kind data] (:effects transition)
         :when (= effect-kind kind)]
     data)))

(defn- last-effect-data
  [fixture effect-kind]
  (last (effect-data fixture effect-kind)))

(defn- transition-for-event
  [fixture event-kind]
  (last
   (filter #(= event-kind (get-in % [:event :event]))
           @(:transitions fixture))))

(defn- resource-values
  [fixture resource-kind]
  (vals (get (shell/resources (:shell fixture)) resource-kind)))

;; =============================================================================
;; Runtime identity / attachment
;; =============================================================================

(deftest create-attaches-only-its-owned-boundaries-test
  (let [{:keys [optimistic shell choreo]} (runtime-fixture)
        diagnostics (optimistic/diagnostics optimistic)]
    (is (optimistic/runtime? optimistic))
    (is (identical? shell (optimistic/shell-runtime optimistic)))
    (is (identical? choreo (optimistic/choreo-runtime optimistic)))
    (is (= optimistic/owned-effect-kinds
           (:attached-effect-kinds diagnostics)))
    (is (= #{optimistic-choreo/derive-provisional-action
             optimistic-choreo/resolve-settlement-action}
           (:attached-local-actions diagnostics)))
    (is (= #{} (:active-optimistic-executions diagnostics)))
    (is (not (contains? diagnostics :target)))
    (is (not (contains? diagnostics :resource)))))

(deftest create-validates-required-physical-seams-test
  (let [shell-runtime (shell/create)
        choreo-runtime (browser-choreo/create shell-runtime)]
    (is (= :invalid-callable
           (error-kind #(optimistic/create choreo-runtime {}))))
    (is (= :unknown-options
           (error-kind
            #(optimistic/create
              choreo-runtime
              {:project-provisional identity
               :render-provisional identity
               :refresh-authority identity
               :invented true}))))))

(deftest create-refuses-effect-handler-collision-test
  (let [shell-runtime (shell/create)
        choreo-runtime (browser-choreo/create shell-runtime)]
    (shell/register-handler!
     shell-runtime :optimistic/install-provisional (fn [_] nil))
    (is (= :effect-handler-collision
           (error-kind
            #(optimistic/create
              choreo-runtime
              {:project-provisional identity
               :render-provisional identity
               :refresh-authority identity}))))))

(deftest create-refuses-local-action-collision-test
  (let [shell-runtime (shell/create)
        choreo-runtime (browser-choreo/create shell-runtime)]
    (browser-choreo/register-local-action!
     choreo-runtime optimistic-choreo/derive-provisional-action (fn [_] {}))
    (is (= :local-action-collision
           (error-kind
            #(optimistic/create
              choreo-runtime
              {:project-provisional identity
               :render-provisional identity
               :refresh-authority identity}))))))

(deftest detach-removes-only-installed-optimistic-boundaries-test
  (let [{:keys [optimistic shell choreo]} (runtime-fixture)]
    (is (= :detached (optimistic/detach! optimistic)))
    (is (nil? (get (shell/handlers shell) :optimistic/install-provisional)))
    (is (nil? (get (shell/handlers shell) :optimistic/finish)))
    (is (nil? (get (browser-choreo/local-actions choreo)
                   optimistic-choreo/derive-provisional-action)))
    (is (nil? (get (browser-choreo/local-actions choreo)
                   optimistic-choreo/resolve-settlement-action)))))

;; =============================================================================
;; Start / provisional installation
;; =============================================================================

(deftest start-derives-installs-and-sends-through-one-shared-runtime-test
  (let [fixture (runtime-fixture)
        command (command "start")
        result (start! fixture command)
        ref (:execution-ref result)
        scope (optimistic-scope fixture command)]
    (is (browser-choreo/execution-ref? ref))
    (is (= :provisional (:status scope)))
    (is (= {:state :claimed
            :request-id "request-start"}
           (get-in scope [:provisional protocol/projection-key])))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (= "true" (attr (:target fixture) "aria-busy")))
    (is (= 1 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 1 (:optimistic-timeouts (shell/resource-counts (:shell fixture)))))
    (is (= 1 (count @(:sent fixture))))
    (is (= :optimistic/command (:event (first @(:sent fixture)))))
    (is (= #{(get command protocol/execution-id-key)}
           (:active-optimistic-executions
            (optimistic/diagnostics (:optimistic fixture)))))))

(deftest start-preserves-command-and-execution-identity-separation-test
  (let [fixture (runtime-fixture)
        command (command "identity")
        result (start! fixture command)
        scope (optimistic-scope fixture command)]
    (is (= command (:command result)))
    (is (= (get command protocol/command-id-key)
           (:command-id scope)))
    (is (= (get command protocol/execution-id-key)
           (:execution-id scope)))
    (is (not= (:command-id scope) (:execution-id scope)))))

(deftest start-defaults-settlement-timeout-test
  (let [fixture (runtime-fixture)
        command (command "timeout-default")]
    (start! fixture command)
    (let [timer (val (first @(:active (:timers fixture))))]
      (is (= optimistic/default-timeout-ms (:delay-ms timer))))))

(deftest start-may-disable-local-settlement-timeout-test
  (let [fixture (runtime-fixture)
        command (command "no-timeout")]
    (start! fixture command {:timeout-ms nil})
    (is (empty? @(:active (:timers fixture))))
    (is (= 0 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))))

(deftest start-rejects-wrong-plan-role-before-machine-start-test
  (let [fixture (runtime-fixture)
        command (command "wrong-role")]
    (is (= :wrong-plan-role
           (error-kind
            #(start! fixture command
                     {:plan (assoc browser-plan :role :authority)}))))))

(deftest application-projector-supports-operation-specific-projection-shapes-test
  (let [fixture
        (runtime-fixture
         {:project-provisional
          (fn [{:keys [command arguments observed-basis scope]}]
            {:invented/application-widget
             {:command-id (get command protocol/command-id-key)
              :request (:request-id arguments)
              :basis observed-basis
              :scope scope
              :arbitrary [:future :shape]}})
          :render-provisional
          (fn [{:keys [target projection]}]
            (set-attr! target "data-widget-request"
                       (get-in projection
                               [:invented/application-widget :request]))
            nil)})
        command (command "custom")]
    (start! fixture command)
    (is (= "request-custom"
           (attr (:target fixture) "data-widget-request")))
    (is (= [:future :shape]
           (get-in (optimistic-scope fixture command)
                   [:provisional protocol/projection-key
                    :invented/application-widget :arbitrary])))))

;; =============================================================================
;; Trusted settlement
;; =============================================================================

(deftest confirmed-settlement-cancels-timeout-and-awaits-authority-test
  (let [fixture (runtime-fixture)
        command (command "confirmed")
        ref (:execution-ref (start! fixture command))
        timer-id (only-timer-id (:timers fixture))]
    (optimistic/settle!
     (:optimistic fixture)
     ref
     (settlement command :confirmed
                 (authoritative {:state :claimed})))
    (is (nil? (optimistic-scope fixture command)))
    (is (nil? (browser-choreo/execution
               (:choreo fixture)
               (get command protocol/execution-id-key))))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic-timeouts (shell/resource-counts (:shell fixture)))))
    (is (some #{timer-id} @(:cleared (:timers fixture))))
    ;; Settlement resolves semantics but provisional DOM is not thereby promoted
    ;; to authority. The ordinary Live path must fetch/install canonical state.
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (string? (attr (:target fixture) optimistic/active-attr)))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (= [:await-authority]
           (mapv :reason @(:refreshes fixture))))))

(deftest rejected-settlement-rolls-back-when-adapter-allows-it-test
  (let [fixture (runtime-fixture)
        command (command "rejected")
        ref (:execution-ref (start! fixture command))]
    (optimistic/settle!
     (:optimistic fixture)
     ref
     (settlement command :rejected))
    (is (= "canonical" (attr (:target fixture) "data-state")))
    (is (= "preserved" (attr (:target fixture) "data-existing")))
    (is (nil? (attr (:target fixture) optimistic/active-attr)))
    (is (empty? @(:refreshes fixture)))
    (is (nil? (optimistic-scope fixture command)))))

(deftest rejected-settlement-refreshes-without-rollback-when-ineligible-test
  (let [fixture (runtime-fixture {:rollback-eligible? false})
        command (command "rejected-refresh")
        ref (:execution-ref (start! fixture command))]
    (optimistic/settle!
     (:optimistic fixture)
     ref
     (settlement command :rejected))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (string? (attr (:target fixture) optimistic/active-attr)))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (= [:refresh-authority]
           (mapv :reason @(:refreshes fixture))))
    (is (nil? (optimistic-scope fixture command)))))

(deftest settlement-correlation-mismatch-fails-before-adapter-delivery-test
  (let [fixture (runtime-fixture)
        command-a (command "correlation-a")
        command-b (command "correlation-b")
        ref (:execution-ref (start! fixture command-a))]
    (is (= :settlement-correlation-mismatch
           (error-kind
            #(optimistic/settle!
              (:optimistic fixture)
              ref
              (settlement command-b :rejected)))))
    (is (some? (optimistic-scope fixture command-a)))))

;; =============================================================================
;; Timeout / uncertain transport failure / supersession
;; =============================================================================

(deftest timeout-rolls-back-and-requests-authoritative-refresh-test
  (let [fixture (runtime-fixture)
        command (command "timeout")
        _ (start! fixture command)
        timer-id (only-timer-id (:timers fixture))]
    ((:fire! (:timers fixture)) timer-id)
    (is (= "canonical" (attr (:target fixture) "data-state")))
    (is (= [:rollback-and-refresh]
           (mapv :reason @(:refreshes fixture))))
    (is (nil? (optimistic-scope fixture command)))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic-timeouts (shell/resource-counts (:shell fixture)))))))

(deftest stale-timeout-callback-after-settlement-has-no-physical-authority-test
  (let [fixture (runtime-fixture)
        command (command "stale-timeout")
        ref (:execution-ref (start! fixture command))
        timer-id (only-timer-id (:timers fixture))
        callback (:callback (get @(:active (:timers fixture)) timer-id))]
    (optimistic/settle!
     (:optimistic fixture)
     ref
     (settlement command :confirmed
                 (authoritative {:state :claimed})))
    (is (= 1 (count @(:refreshes fixture))))
    ;; Simulate a hostile/late host callback even though clearTimeout ran.
    (callback)
    (is (= 1 (count @(:refreshes fixture))))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (nil? (optimistic-scope fixture command)))))


(deftest uncertain-transport-failure-uses-adapter-recovery-without-settlement-test
  (let [transport (controlled-thenable)
        fixture
        (runtime-fixture
         {:transport-send
          (fn [_ctx]
            {:completion (:value transport)})})
        command (command "network-failure")
        result (start! fixture command)]
    (is (browser-choreo/execution-ref? (:execution-ref result)))
    (is (some? (optimistic-scope fixture command)))
    ((:reject! transport) (js/Error. "network disappeared"))
    ;; The adapter classifies the local uncertainty as :network-failed and
    ;; chooses recovery. No protocol :failed settlement is invented.
    (is (= "canonical" (attr (:target fixture) "data-state")))
    (is (= [:rollback-and-refresh]
           (mapv :reason @(:refreshes fixture))))
    (is (nil? (optimistic-scope fixture command)))
    (is (= 0 (:transports (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic-timeouts (shell/resource-counts (:shell fixture)))))))

(deftest authoritative-supersession-releases-provisional-without-rollback-test
  (let [fixture (runtime-fixture)
        command (command "superseded")
        ref (:execution-ref (start! fixture command))]
    (optimistic/supersede!
     (:optimistic fixture)
     ref
     (authoritative {:state :claimed-by-someone-else}))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (string? (attr (:target fixture) optimistic/active-attr)))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (empty? @(:refreshes fixture)))
    (is (nil? (optimistic-scope fixture command)))))

(deftest explicit-retirement-is-release-only-test
  (let [fixture (runtime-fixture)
        command (command "retire")
        ref (:execution-ref (start! fixture command))]
    (optimistic/retire! (:optimistic fixture) ref :navigation-away)
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (string? (attr (:target fixture) optimistic/active-attr)))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (empty? @(:refreshes fixture)))
    (is (nil? (optimistic-scope fixture command)))))

;; =============================================================================
;; Physical ownership safety / replacement
;; =============================================================================

(deftest rollback-refuses-to-overwrite-physically-newer-owner-test
  (let [fixture (runtime-fixture)
        command (command "ownership-loss")
        ref (:execution-ref (start! fixture command))]
    ;; Simulate a newer browser representation taking physical ownership before
    ;; the old settlement arrives. Adapter generation ownership alone is not
    ;; enough to authorize restoring an obsolete DOM snapshot.
    (set-attr! (:target fixture) optimistic/active-attr "newer-owner")
    (set-attr! (:target fixture) "data-state" "newer")
    (optimistic/settle!
     (:optimistic fixture)
     ref
     (settlement command :rejected))
    (is (= "newer" (attr (:target fixture) "data-state")))
    (is (= "newer-owner" (attr (:target fixture) optimistic/active-attr)))
    (is (= [:rollback-ownership-lost]
           (mapv :reason @(:refreshes fixture))))))

(deftest replacing-target-owner-releases-old-resource-before-new-install-test
  (let [fixture (runtime-fixture)
        command-a (command "replace-a")
        command-b (command "replace-b")
        ref-a (:execution-ref (start! fixture command-a))]
    (is (browser-choreo/execution-ref? ref-a))
    (let [ref-b
          (:execution-ref
           (start! fixture command-b {:replace-owner? true}))]
      (is (browser-choreo/execution-ref? ref-b))
      (is (nil? (optimistic-scope fixture command-a)))
      (is (some? (optimistic-scope fixture command-b)))
      (is (= (get command-b protocol/execution-id-key)
             (:execution-id
              (adapter/target-owner
               (optimistic/state (:optimistic fixture))
               "request-target"))))
      (is (= "claimed" (attr (:target fixture) "data-state")))
      (is (= 1 (:optimistic (shell/resource-counts (:shell fixture)))))
      (is (= 1 (:optimistic-timeouts (shell/resource-counts (:shell fixture)))))
      ;; release-only for A must not fabricate authority refresh.
      (is (empty? @(:refreshes fixture))))))

;; =============================================================================
;; Physical presentation failure remains non-authoritative
;; =============================================================================

(deftest provisional-render-failure-does-not-fabricate-command-failure-test
  (let [fixture
        (runtime-fixture
         {:render-provisional
          (fn [_]
            (throw (js/Error. "paint failed")))})
        command (command "render-failure")
        result (start! fixture command)]
    (is (browser-choreo/execution-ref? (:execution-ref result)))
    ;; The command still crossed the semantic transport boundary.
    (is (= 1 (count @(:sent fixture))))
    (is (some? (optimistic-scope fixture command)))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 1 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))
    (is (some #(= :optimistic-install-failed (:phase %))
              @(:errors fixture)))))

;; =============================================================================
;; v7.222 adversarial physical-safety checks
;; =============================================================================

(deftest replacement-never-uses-retired-provisional-as-rollback-authority-test
  (let [fixture
        (runtime-fixture
         {:project-provisional
          (fn [{:keys [arguments]}]
            {:state (:request-id arguments)})
          :render-provisional
          (fn [{:keys [target projection]}]
            (set-attr! target "data-state" (:state projection))
            nil)})
        command-a (command "baseline-a")
        command-b (command "baseline-b")
        _ (start! fixture command-a)
        ref-b (:execution-ref
               (start! fixture command-b {:replace-owner? true}))]
    (is (= "request-baseline-b" (attr (:target fixture) "data-state")))
    (optimistic/settle!
     (:optimistic fixture)
     ref-b
     (settlement command-b :rejected))
    ;; A's retired provisional representation is not resurrected as B's
    ;; rollback baseline. B remains visibly provisional until canonical refresh.
    (is (= "request-baseline-b" (attr (:target fixture) "data-state")))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (= 1 (count @(:refreshes fixture))))))

(deftest renderer-partial-mutation-is-compensated-before-command-continues-test
  (let [fixture
        (runtime-fixture
         {:render-provisional
          (fn [{:keys [target]}]
            (set-attr! target "data-state" "half-painted")
            (set-attr! target "data-half" "true")
            (throw (js/Error. "paint failed after mutation")))})
        command (command "partial-render")
        result (start! fixture command)]
    (is (browser-choreo/execution-ref? (:execution-ref result)))
    (is (= "canonical" (attr (:target fixture) "data-state")))
    (is (nil? (attr (:target fixture) "data-half")))
    (is (nil? (attr (:target fixture) optimistic/active-attr)))
    (is (nil? (attr (:target fixture) optimistic/provisional-attr)))
    ;; Semantic command transport continues even though optional local paint failed.
    (is (= 1 (count @(:sent fixture))))))

(deftest replacement-generation-changes-physical-ownership-token-test
  (let [fixture (runtime-fixture)
        command (command "same-execution-generation")
        first-ref (:execution-ref (start! fixture command))
        first-token (attr (:target fixture) optimistic/active-attr)
        second-ref (:execution-ref
                    (start! fixture command {:replace-execution? true}))
        second-token (attr (:target fixture) optimistic/active-attr)]
    (is (= (:execution-id first-ref) (:execution-id second-ref)))
    (is (not= (:generation first-ref) (:generation second-ref)))
    (is (string? first-token))
    (is (string? second-token))
    (is (not= first-token second-token))))

(deftest late-settlement-after-retirement-is-adapter-classified-not-thrown-test
  (let [fixture (runtime-fixture)
        command (command "late-settlement")
        ref (:execution-ref (start! fixture command))
        confirmed (settlement command :confirmed
                              (authoritative {:state :claimed}))]
    (optimistic/settle! (:optimistic fixture) ref confirmed)
    (is (nil? (optimistic-scope fixture command)))
    (let [late (optimistic/settle! (:optimistic fixture) ref confirmed)]
      (is (nil? (:message-dispatch late)))
      (is (some adapter/diagnostic-effect? (:effects late))))
    (is (= 1 (count @(:refreshes fixture))))))

;; =============================================================================
;; Complete protocol-v3 outcome and ordering contract
;; =============================================================================

(deftest start-orders-derive-before-install-timeout-and-send-test
  (let [fixture (runtime-fixture)
        command (command "ordered-start")]
    (start! fixture command)
    (let [events (mapv #(get-in % [:event :event]) @(:transitions fixture))
          local-completed (transition-for-event fixture :machine/local-completed)]
      (is (= [:execution/start
              :machine/local-completed
              :machine/send-requested
              :transport/succeeded]
             events))
      ;; The adapter exposes the physical consequences in the exact order that
      ;; the shell realizes them. There is no timeout or transport before the
      ;; semantic local derivation completes.
      (is (= [:optimistic/install-provisional
              :optimistic/timeout-start
              :machine/send]
             (mapv first (:effects local-completed))))
      (is (= 1 (count (resource-values fixture :optimistic))))
      (is (= 1 (count (resource-values fixture :optimistic-timeouts))))
      (is (= 1 (count @(:sent fixture)))))))

(deftest optimistic-runtime-has-no-independent-semantic-registry-test
  (let [{:keys [optimistic shell]} (runtime-fixture)
        forbidden #{:executions
                    :execution-registry
                    :settlements
                    :target-locks
                    :targets
                    :timers
                    :timeouts
                    :outgoing-actions
                    :commands
                    :provisionals}]
    (is (empty? (set/intersection forbidden (set (keys optimistic)))))
    (is (identical? (optimistic/state optimistic)
                    (shell/state shell)))
    (is (= #{}
           (:active-optimistic-executions
            (optimistic/diagnostics optimistic))))))

(deftest reconciled-and-already-incorporated-await-canonical-authority-test
  (doseq [resolution [:reconciled :already-incorporated]]
    (testing (name resolution)
      (let [fixture (runtime-fixture)
            command (command (name resolution))
            ref (:execution-ref (start! fixture command))]
        (optimistic/settle!
         (:optimistic fixture)
         ref
         (settlement command resolution
                     (authoritative {:state resolution})))
        (is (= :await-authority
               (:disposition
                (last-effect-data fixture :optimistic/finish))))
        (is (= [:await-authority]
               (mapv :reason @(:refreshes fixture))))
        (is (nil? (optimistic-scope fixture command)))
        (is (= 0 (:optimistic
                  (shell/resource-counts (:shell fixture)))))
        (is (= 0 (:optimistic-timeouts
                  (shell/resource-counts (:shell fixture)))))
        ;; Resolution is semantic; canonical DOM installation has not happened.
        (is (= "true"
               (attr (:target fixture) optimistic/provisional-attr)))))))

(deftest trusted-failed-settlement-honors-both-rollback-policies-test
  (doseq [[rollback-eligible? expected-state expected-disposition expected-refresh]
          [[true "canonical" :rollback []]
           [false "claimed" :refresh-authority [:refresh-authority]]]]
    (testing (str "rollback-eligible?=" rollback-eligible?)
      (let [fixture (runtime-fixture {:rollback-eligible? rollback-eligible?})
            command (command (str "failed-" rollback-eligible?))
            ref (:execution-ref (start! fixture command))]
        (optimistic/settle!
         (:optimistic fixture)
         ref
         (settlement command :failed))
        (is (= expected-disposition
               (:disposition
                (last-effect-data fixture :optimistic/finish))))
        (is (= expected-state (attr (:target fixture) "data-state")))
        (is (= expected-refresh
               (mapv :reason @(:refreshes fixture))))
        (is (nil? (optimistic-scope fixture command)))))))

(deftest network-failure-without-rollback-eligibility-refreshes-only-test
  (let [transport (controlled-thenable)
        fixture
        (runtime-fixture
         {:rollback-eligible? false
          :transport-send (fn [_] {:completion (:value transport)})})
        command (command "network-no-rollback")]
    (start! fixture command)
    ((:reject! transport) (js/Error. "network disappeared"))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (= :refresh-authority
           (:disposition
            (last-effect-data fixture :optimistic/finish))))
    (is (= [:refresh-authority]
           (mapv :reason @(:refreshes fixture))))
    (is (nil? (optimistic-scope fixture command)))))

(deftest exact-duplicate-settlement-is-diagnostic-before-machine-delivery-test
  (let [fixture (runtime-fixture)
        command (command "duplicate")
        ref (:execution-ref (start! fixture command))
        settlement' (settlement command :confirmed
                                (authoritative {:state :claimed}))
        {:keys [execution-id generation]} ref]
    ;; Hold the execution between adapter settlement observation and projected
    ;; participant-message delivery. This is the only phase in which an exact
    ;; transport duplicate can arrive before semantic completion.
    (shell/dispatch!
     (:shell fixture)
     {:event :optimistic/settlement-observed
      :execution-id execution-id
      :generation generation
      :resolution :confirmed
      :settlement settlement'})
    (let [result (optimistic/settle! (:optimistic fixture) ref settlement')]
      (is (nil? (:message-dispatch result)))
      (is (= :settlement-observed
             (:status (optimistic-scope fixture command))))
      (is (some #(= :duplicate-optimistic-settlement (:reason %))
                @(:diagnostics fixture)))
      (is (= 1 (count (effect-data fixture :optimistic/timeout-cancel)))))))

(deftest conflicting-settlement-fails-closed-before-machine-delivery-test
  (let [fixture (runtime-fixture)
        command (command "conflicting")
        ref (:execution-ref (start! fixture command))
        confirmed (settlement command :confirmed
                              (authoritative {:state :claimed}))
        rejected (settlement command :rejected)
        {:keys [execution-id generation]} ref]
    (shell/dispatch!
     (:shell fixture)
     {:event :optimistic/settlement-observed
      :execution-id execution-id
      :generation generation
      :resolution :confirmed
      :settlement confirmed})
    (is (= :conflicting-optimistic-settlement
           (error-kind
            #(optimistic/settle! (:optimistic fixture) ref rejected))))
    (is (= :confirmed (:resolution (optimistic-scope fixture command))))
    (is (= :settlement-observed
           (:status (optimistic-scope fixture command))))))

(deftest stale-timeout-generation-is-inert-while-current-execution-remains-active-test
  (let [fixture (runtime-fixture)
        command (command "stale-timeout-generation")
        ref (:execution-ref (start! fixture command))
        scope (optimistic-scope fixture command)]
    (shell/dispatch!
     (:shell fixture)
     {:event :optimistic/timeout-fired
      :execution-id (:execution-id ref)
      :generation (:generation ref)
      :timeout-generation (inc (:timeout-generation scope))})
    (is (some? (optimistic-scope fixture command)))
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (some #(= :stale-optimistic-timeout (:reason %))
              @(:diagnostics fixture)))
    (is (= 1 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))))

(deftest authoritative-supersession-cancels-timeout-and-emits-authoritative-disposition-test
  (let [fixture (runtime-fixture)
        command (command "supersession-disposition")
        ref (:execution-ref (start! fixture command))
        timer-id (only-timer-id (:timers fixture))]
    (optimistic/supersede!
     (:optimistic fixture)
     ref
     (authoritative {:state :other-authority}))
    (is (= :authoritative
           (:disposition
            (last-effect-data fixture :optimistic/finish))))
    (is (some #{timer-id} @(:cleared (:timers fixture))))
    (is (= 0 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic
              (shell/resource-counts (:shell fixture)))))
    ;; Supersession resolves semantic ownership but does not forge canonical DOM.
    (is (= "claimed" (attr (:target fixture) "data-state")))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))))

(deftest missing-target-during-provisional-install-is-presentation-failure-only-test
  (let [fixture (runtime-fixture {:resolve-target (fn [_] nil)})
        command (command "missing-target")
        result (start! fixture command)]
    (is (browser-choreo/execution-ref? (:execution-ref result)))
    (is (= 1 (count @(:sent fixture))))
    (is (some? (optimistic-scope fixture command)))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 1 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))
    (is (some #(= :optimistic-install-failed (:phase %))
              @(:errors fixture)))))

(deftest returned-provisional-element-is-copied-into-stable-target-test
  (let [original-target (atom nil)
        fixture
        (runtime-fixture
         {:render-provisional
          (fn [{:keys [target]}]
            (reset! original-target target)
            (fake-element "details"
                          {"id" "request-target"
                           "data-state" "replacement-render"}))})
        command (command "returned-element")]
    (start! fixture command)
    (is (identical? @original-target (:target fixture)))
    (is (= "replacement-render" (attr (:target fixture) "data-state")))
    (is (= "true" (attr (:target fixture) optimistic/provisional-attr)))
    (is (some #(identical? % (:target fixture)) @(:processed fixture)))))

(deftest async-provisional-derivation-defers-install-timeout-and-transport-test
  (async done
    (let [deferred (deferred-promise)
          fixture
          (runtime-fixture
           {:project-provisional (fn [_] (:promise deferred))})
          command (command "async-derive")]
      (start! fixture command)
      (is (= "canonical" (attr (:target fixture) "data-state")))
      (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
      (is (= 0 (:optimistic-timeouts
                (shell/resource-counts (:shell fixture)))))
      (is (empty? @(:sent fixture)))
      ((:resolve! deferred) {:state :claimed
                             :request-id "request-async-derive"})
      (after-promises
       (fn []
         (is (= "claimed" (attr (:target fixture) "data-state")))
         (is (= 1 (:optimistic (shell/resource-counts (:shell fixture)))))
         (is (= 1 (:optimistic-timeouts
                   (shell/resource-counts (:shell fixture)))))
         (is (= 1 (count @(:sent fixture))))
         (done))))))

(deftest stale-async-provisional-derivation-cannot-resume-replacement-generation-test
  (async done
    (let [first-deferred (deferred-promise)
          second-deferred (deferred-promise)
          calls (atom 0)
          fixture
          (runtime-fixture
           {:project-provisional
            (fn [_]
              (if (= 1 (swap! calls inc))
                (:promise first-deferred)
                (:promise second-deferred)))
            :render-provisional
            (fn [{:keys [target projection]}]
              (set-attr! target "data-state" (:state projection))
              nil)})
          command (command "async-replacement")
          first-ref (:execution-ref (start! fixture command))
          second-ref (:execution-ref
                      (start! fixture command {:replace-execution? true}))]
      (is (= (:execution-id first-ref) (:execution-id second-ref)))
      (is (not= (:generation first-ref) (:generation second-ref)))
      ((:resolve! first-deferred) {:state "stale"})
      (after-promises
       (fn []
         (is (= "canonical" (attr (:target fixture) "data-state")))
         (is (empty? @(:sent fixture)))
         (is (some #(= :stale-execution-generation (:reason %))
                   @(:diagnostics fixture)))
         ((:resolve! second-deferred) {:state "current"})
         (after-promises
          (fn []
            (is (= "current" (attr (:target fixture) "data-state")))
            (is (= 1 (count @(:sent fixture))))
            (is (= (:generation second-ref)
                   (:execution-generation
                    (optimistic-scope fixture command))))
            (done))))))))

(deftest projector-failure-retires-semantic-execution-before-command-send-test
  (let [fixture
        (runtime-fixture
         {:project-provisional
          (fn [_]
            (throw (js/Error. "semantic projection failed")))})
        command (command "projector-failure")
        result (start! fixture command)]
    (is (nil? (:execution-ref result)))
    (is (empty? @(:sent fixture)))
    (is (nil? (optimistic-scope fixture command)))
    (is (= 0 (:optimistic (shell/resource-counts (:shell fixture)))))
    (is (= 0 (:optimistic-timeouts
              (shell/resource-counts (:shell fixture)))))
    (is (some #(and (= :effect-failed (:phase %))
                    (= :machine/local (:effect %)))
              @(:errors fixture)))))

