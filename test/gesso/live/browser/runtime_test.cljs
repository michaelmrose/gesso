(ns gesso.live.browser.runtime-test
  "Composition-root tests for gesso.live.browser.runtime.

   The runtime namespace is intentionally thin: it composes one Core, one Choreo
   realization, optionally one protocol-v3 optimistic realization, and optionally
   one optimistic HTMX transport bridge over the exact same Core/Choreo/shell.
   It owns aggregate lifecycle only and must not
   create a competing semantic state machine.

   These tests concentrate on composition and lifecycle properties:

   - one shared shell and AdapterState owner;
   - optional optimism attaches to that exact Choreo runtime and shell;
   - optional optimistic HTMX attaches to the exact Core, optimism, Choreo, and shell;
   - created/started/stopped invariant shapes;
   - exactly-once document-listener ownership;
   - exact ownership of Choreo/optimistic handler slots and bridge observers/wrappers;
   - fail-closed detection of removed or replaced integration handlers;
   - semantic shell shutdown before bridge/optimistic/Choreo physical detachment;
   - permanent retirement after stop or failed start;
   - delegation through the shared Core/adapter path;
   - host-resource-free diagnostics;
   - no second optimistic execution/target/timer registry at composition root.

   Browser-global init!/shutdown! remains a real-browser integration concern.
   This namespace stays host-independent so it can run under Node and Chromium."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.core :as c]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.browser.optimistic-htmx :as optimistic-htmx]
   [gesso.live.browser.runtime :as runtime]
   [gesso.live.browser.shell :as shell]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.progression :as progression]))

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

(defn- thrown-data
  [f]
  (some-> (thrown f)
          ex-data))

(defn- error-kind
  [f]
  (:error/kind
   (thrown-data f)))

(defn- deep-identical?
  [root needle]
  (cond
    (identical? root needle)
    true

    (map? root)
    (boolean
     (some true?
           (concat
            (map #(deep-identical? % needle) (keys root))
            (map #(deep-identical? % needle) (vals root)))))

    (or (vector? root)
        (list? root)
        (seq? root)
        (set? root))
    (boolean
     (some #(deep-identical? % needle) root))

    :else
    false))

;; =============================================================================
;; Fake physical browser boundary
;; =============================================================================

(defn- make-root
  [fragment-id]
  (let [root (js-obj)]
    (aset root "nodeType" 1)
    (aset root "children" #js [])
    (aset root
          "getAttribute"
          (fn [name]
            (when (= name core/fragment-attribute)
              fragment-id)))
    (aset root
          "closest"
          (fn [selector]
            (when (= selector core/fragment-selector)
              root)))
    (aset root
          "querySelectorAll"
          (fn [_]
            #js []))
    root))

(defn- make-document
  ([]
   (make-document nil))
  ([{:keys [roots fail-after-add]}]
   (let [roots (vec (or roots []))
         added (atom [])
         removed (atom [])
         document (js-obj)]
     (aset document
           "querySelectorAll"
           (fn [selector]
             (if (= selector core/fragment-selector)
               (to-array roots)
               #js [])))
     (aset document
           "addEventListener"
           (fn [name handler capture?]
             (when (and (some? fail-after-add)
                        (>= (count @added) fail-after-add))
               (throw
                (js/Error.
                 "synthetic addEventListener failure")))
             (swap! added conj [name handler capture?])))
     (aset document
           "removeEventListener"
           (fn [name handler capture?]
             (swap! removed conj [name handler capture?])))
     {:document document
      :added added
      :removed removed})))

(defn- make-htmx
  []
  (let [calls (atom [])
        htmx (js-obj)]
    (aset htmx
          "trigger"
          (fn [root event-name detail]
            (swap! calls
                   conj
                   {:root root
                    :event-name event-name
                    :detail detail})
            true))
    {:htmx htmx
     :calls calls}))

(defn- default-optimistic-options
  ([]
   (default-optimistic-options nil))
  ([overrides]
   (merge
    {:project-provisional
     (fn [_]
       {:projection :pending})

     :render-provisional
     (fn [& _]
       nil)

     :refresh-authority
     (fn [& _]
       nil)

     :resolve-target
     (fn [_]
       nil)

     :process-element
     (fn [_]
       nil)}
    overrides)))

(defn- fixture
  ([]
   (fixture {}))
  ([options]
   (let [{:keys [roots
                 document-options
                 core-options
                 choreo-options
                 optimistic-options
                 optimistic-htmx-options]}
         options

         document-fixture
         (make-document
          (merge
           {:roots roots}
           document-options))

         htmx-fixture
         (make-htmx)

         create-options
         (cond->
          {:core-options
           (merge
            {:document (:document document-fixture)
             :htmx (:htmx htmx-fixture)}
            core-options)}
           (contains? options :choreo-options)
           (assoc :choreo-options choreo-options)

           (contains? options :optimistic-options)
           (assoc :optimistic-options optimistic-options)

           (contains? options :optimistic-htmx-options)
           (assoc :optimistic-htmx-options optimistic-htmx-options))

         composed-runtime
         (runtime/create create-options)]
     {:runtime composed-runtime
      :document-fixture document-fixture
      :htmx-fixture htmx-fixture})))

(defn- optimistic-fixture
  ([]
   (optimistic-fixture nil))
  ([overrides]
   (fixture
    {:optimistic-options
     (default-optimistic-options overrides)})))

(defn- default-optimistic-htmx-options
  ([]
   (default-optimistic-htmx-options nil))
  ([overrides]
   (merge
    {:plan-for (fn [_] nil)}
    overrides)))

(defn- optimistic-htmx-fixture
  ([]
   (optimistic-htmx-fixture nil))
  ([bridge-overrides]
   (fixture
    {:optimistic-options
     (default-optimistic-options)
     :optimistic-htmx-options
     (default-optimistic-htmx-options bridge-overrides)})))

(defn- listener-names
  [registrations]
  (mapv first registrations))

(defn- choreo-handler-ownership-error
  [composed-runtime effect-kind]
  (some
   (fn [error]
     (when
      (and
       (= :choreo-handler-ownership
          (:invariant error))
       (= effect-kind
          (:effect-kind error)))
       error))
   (runtime/invariant-errors composed-runtime)))

(defn- optimistic-handler-ownership-error
  [composed-runtime effect-kind]
  (some
   (fn [error]
     (when
      (and
       (= :optimistic-handler-ownership
          (:invariant error))
       (= effect-kind
          (:effect-kind error)))
       error))
   (runtime/invariant-errors composed-runtime)))

(defn- optimistic-action-ownership-error
  [composed-runtime action-id]
  (some
   (fn [error]
     (when
      (and
       (= :optimistic-action-ownership
          (:invariant error))
       (= action-id
          (:action-id error)))
       error))
   (runtime/invariant-errors composed-runtime)))

(defn- optimistic-htmx-observer-ownership-error
  [composed-runtime event-name]
  (some
   (fn [error]
     (when
      (and
       (= :optimistic-htmx-observer-ownership
          (:invariant error))
       (= event-name
          (:event-name error)))
       error))
   (runtime/invariant-errors composed-runtime)))

(defn- invariant-error
  [composed-runtime invariant]
  (some
   #(when (= invariant (:invariant %)) %)
   (runtime/invariant-errors composed-runtime)))

(def expected-listener-names
  (mapv first core/listener-specs))

;; =============================================================================
;; Portable execution fixture for shutdown ordering
;; =============================================================================

(defn- waiting-execution
  []
  (machine/start
   (project/project
    (c/->choreography
     {:initial :waiting
      :states
      {:waiting
       (c/await :browser {:browser/continue :done})

       :done
       (c/return :done)}})
    :browser)))

;; =============================================================================
;; Identity / construction
;; =============================================================================

(deftest runtime-identity-test
  (let [{:keys [runtime]} (fixture)]
    (is (= "1.3.0-dev" runtime/runtime-version))
    (is (= :gesso.live.browser.runtime/runtime
           runtime/runtime-type))
    (is (runtime/runtime? runtime))
    (is (not (runtime/runtime? nil)))
    (is (= :created (runtime/lifecycle runtime)))
    (is (false? (runtime/started? runtime)))
    (is (false? (runtime/stopped? runtime)))
    (is (nil? (runtime/optimistic-runtime runtime)))
    (is (nil? (runtime/optimistic-htmx-runtime runtime)))
    (runtime/stop! runtime)))

(deftest create-composes-one-exact-shared-shell-test
  (let [{:keys [runtime document-fixture]} (fixture)
        core-runtime (runtime/core-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        shared-shell (runtime/shell-runtime runtime)]
    (is (identical? shared-shell
                    (core/shell-runtime core-runtime)))
    (is (identical? shared-shell
                    (choreo/shell-runtime choreo-runtime)))
    (is (= (core/state core-runtime)
           (runtime/state runtime)))
    (is (false? (shell/closed? shared-shell)))
    (is (every? #(contains? (shell/handlers shared-shell) %)
                choreo/owned-effect-kinds))
    (is (empty? @(:added document-fixture)))
    (is (runtime/invariant-clean? runtime))
    (runtime/stop! runtime)))

(deftest create-composes-optimism-on-the-exact-existing-choreo-and-shell-test
  (let [{:keys [runtime]} (optimistic-fixture)
        optimistic-runtime (runtime/optimistic-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        shared-shell (runtime/shell-runtime runtime)
        expected-actions
        #{(:derive-action optimistic-runtime)
          (:resolve-action optimistic-runtime)}]
    (is (optimistic/runtime? optimistic-runtime))
    (is (identical? choreo-runtime
                    (optimistic/choreo-runtime optimistic-runtime)))
    (is (identical? shared-shell
                    (optimistic/shell-runtime optimistic-runtime)))
    (is (= optimistic/owned-effect-kinds
           (set (keys @(:installed-handlers optimistic-runtime)))))
    (is (= expected-actions
           (set (keys @(:installed-actions optimistic-runtime)))))
    (is (every? #(contains? (shell/handlers shared-shell) %)
                optimistic/owned-effect-kinds))
    (is (every? #(contains? (choreo/local-actions choreo-runtime) %)
                expected-actions))
    (is (runtime/invariant-clean? runtime))
    (runtime/stop! runtime)))

(deftest create-composes-optimistic-htmx-on-the-exact-existing-layers-test
  (let [{:keys [runtime]} (optimistic-htmx-fixture)
        core-runtime (runtime/core-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        optimistic-runtime (runtime/optimistic-runtime runtime)
        bridge-runtime (runtime/optimistic-htmx-runtime runtime)
        shared-shell (runtime/shell-runtime runtime)]
    (is (optimistic-htmx/runtime? bridge-runtime))
    (is (identical? core-runtime
                    (optimistic-htmx/core-runtime bridge-runtime)))
    (is (identical? optimistic-runtime
                    (optimistic-htmx/optimistic-runtime bridge-runtime)))
    (is (identical? choreo-runtime
                    (optimistic-htmx/choreo-runtime bridge-runtime)))
    (is (identical? shared-shell
                    (optimistic/shell-runtime optimistic-runtime)))
    (is (= (set optimistic-htmx/observed-events)
           (:observed-events
            (optimistic-htmx/diagnostics bridge-runtime))))
    (is (identical? @(:send-payload-wrapper bridge-runtime)
                    (choreo/send-payload-handler choreo-runtime)))
    (is (identical? @(:transport-wrapper bridge-runtime)
                    (choreo/transport-handler choreo-runtime)))
    (is (runtime/invariant-clean? runtime))
    (runtime/stop! runtime)))

(deftest create-forwards-physical-configuration-without-adding-semantic-state-test
  (let [local-handler (fn [_] {:value :ok})
        {:keys [runtime]}
        (fixture
         {:choreo-options
          {:local-actions
           {:browser/work local-handler}}
          :optimistic-options
          (default-optimistic-options)
          :optimistic-htmx-options
          (default-optimistic-htmx-options)})
        diagnostics (runtime/diagnostics runtime)]
    (is (= #{:browser/work
             optimistic-choreo/derive-provisional-action
             optimistic-choreo/resolve-settlement-action}
           (get-in diagnostics
                   [:choreo :registered-local-actions])))
    (is (= :created (:lifecycle diagnostics)))
    (is (not (contains? runtime :executions)))
    (is (not (contains? runtime :targets)))
    (is (not (contains? runtime :timers)))
    (is (not (contains? runtime :continuity)))
    (is (not (contains? runtime :optimistic-executions)))
    (is (not (contains? runtime :optimistic-targets)))
    (is (not (contains? runtime :optimistic-timers)))
    (runtime/stop! runtime)))

(deftest create-validates-composition-options-test
  (is (= :invalid-map
         (error-kind #(runtime/create [:not :a-map]))))
  (is (= :unknown-options
         (error-kind #(runtime/create {:invented true}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:core-options [:not :a-map]}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:choreo-options [:not :a-map]}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:optimistic-options [:not :a-map]}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:optimistic-htmx-options [:not :a-map]}))))
  (is (= :optimistic-htmx-requires-optimism
         (error-kind
          #(runtime/create
            {:optimistic-htmx-options
             (default-optimistic-htmx-options)})))))

(deftest collapsed-optimistic-local-action-ids-are-rejected-test
  (let [{:keys [document]} (make-document)
        htmx (:htmx (make-htmx))
        action-id :optimistic/collapsed]
    (is (= :invalid-composition
           (error-kind
            #(runtime/create
              {:core-options
               {:document document
                :htmx htmx}
               :optimistic-options
               (default-optimistic-options
                {:derive-action action-id
                 :resolve-action action-id})}))))))

;; =============================================================================
;; Lifecycle ownership
;; =============================================================================

(deftest start-installs-core-listeners-exactly-once-with-optimistic-htmx-attached-test
  (let [{:keys [runtime document-fixture]} (optimistic-htmx-fixture)
        bridge-runtime (runtime/optimistic-htmx-runtime runtime)]
    (is (true? (:attached? (optimistic-htmx/diagnostics bridge-runtime))))
    (is (identical? runtime (runtime/start! runtime)))
    (is (= :started (runtime/lifecycle runtime)))
    (is (= expected-listener-names
           (listener-names @(:added document-fixture))))
    (is (= (set optimistic-htmx/observed-events)
           (set (keys (core/event-observers
                       (runtime/core-runtime runtime))))))
    (is (runtime/invariant-clean? runtime))

    (testing "repeated start does not acquire a second listener set"
      (is (identical? runtime (runtime/start! runtime)))
      (is (= expected-listener-names
             (listener-names @(:added document-fixture)))))

    (runtime/stop! runtime)))

(deftest stop-detaches-optimistic-htmx-and-restores-previous-choreo-transport-handlers-test
  (let [base-send (fn [_] {:base :send})
        base-transport (fn [_] :base-transport)
        {:keys [runtime document-fixture]}
        (fixture
         {:choreo-options
          {:send-payload base-send
           :transport-send base-transport}
          :optimistic-options
          (default-optimistic-options)
          :optimistic-htmx-options
          (default-optimistic-htmx-options)})
        choreo-runtime (runtime/choreo-runtime runtime)
        bridge-runtime (runtime/optimistic-htmx-runtime runtime)]
    (is (not (identical? base-send
                         (choreo/send-payload-handler choreo-runtime))))
    (is (not (identical? base-transport
                         (choreo/transport-handler choreo-runtime))))

    (runtime/start! runtime)
    (is (= :stopped (runtime/stop! runtime)))
    (is (= expected-listener-names
           (listener-names @(:removed document-fixture))))
    (is (false? (:attached? (optimistic-htmx/diagnostics bridge-runtime))))
    (is (empty? (core/event-observers (runtime/core-runtime runtime))))
    (is (identical? base-send
                    (choreo/send-payload-handler choreo-runtime)))
    (is (identical? base-transport
                    (choreo/transport-handler choreo-runtime)))
    (is (runtime/invariant-clean? runtime))))

(deftest start-installs-core-listeners-exactly-once-with-optimism-attached-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        optimistic-runtime (runtime/optimistic-runtime runtime)]
    (is (identical? runtime (runtime/start! runtime)))
    (is (= :started (runtime/lifecycle runtime)))
    (is (runtime/started? runtime))
    (is (= expected-listener-names
           (listener-names @(:added document-fixture))))
    (is (= optimistic/owned-effect-kinds
           (:attached-effect-kinds
            (optimistic/diagnostics optimistic-runtime))))
    (is (runtime/invariant-clean? runtime))

    (testing "repeated start is idempotent"
      (is (identical? runtime (runtime/start! runtime)))
      (is (= expected-listener-names
             (listener-names @(:added document-fixture)))))

    (runtime/stop! runtime)))

(deftest stop-from-started-removes-listeners-closes-shell-and-detaches-both-integrations-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        optimistic-runtime (runtime/optimistic-runtime runtime)
        optimistic-actions
        #{(:derive-action optimistic-runtime)
          (:resolve-action optimistic-runtime)}]
    (runtime/start! runtime)
    (is (= :stopped (runtime/stop! runtime)))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (runtime/stopped? runtime))
    (is (false? (runtime/started? runtime)))
    (is (= expected-listener-names
           (listener-names @(:removed document-fixture))))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  optimistic/owned-effect-kinds))
    (is (not-any? #(contains? (choreo/local-actions choreo-runtime) %)
                  optimistic-actions))
    (is (empty?
         (:attached-effect-kinds
          (choreo/diagnostics choreo-runtime))))
    (is (empty?
         (:attached-effect-kinds
          (optimistic/diagnostics optimistic-runtime))))
    (is (empty?
         (:attached-local-actions
          (optimistic/diagnostics optimistic-runtime))))
    (is (runtime/invariant-clean? runtime))

    (testing "repeated stop performs no second physical teardown"
      (let [removed-count (count @(:removed document-fixture))]
        (is (= :stopped (runtime/stop! runtime)))
        (is (= removed-count
               (count @(:removed document-fixture))))))))

(deftest stop-from-created-retires-without-ever-acquiring-document-listeners-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)
        optimistic-runtime (runtime/optimistic-runtime runtime)]
    (is (= :stopped (runtime/stop! runtime)))
    (is (empty? @(:added document-fixture)))
    (is (empty? @(:removed document-fixture)))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  optimistic/owned-effect-kinds))
    (is (empty? (:attached-effect-kinds
                 (optimistic/diagnostics optimistic-runtime))))
    (is (empty? (:attached-local-actions
                 (optimistic/diagnostics optimistic-runtime))))
    (is (runtime/invariant-clean? runtime))))

(deftest stopped-runtime-cannot-be-restarted-test
  (let [{:keys [runtime]} (optimistic-fixture)]
    (runtime/stop! runtime)
    (is (= :already-stopped
           (error-kind #(runtime/start! runtime))))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (runtime/invariant-clean? runtime))))

(deftest externally-stopped-child-is-detected-before-start-test
  (let [{:keys [runtime]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)]
    ;; Direct child manipulation is unsupported, but the composition root must
    ;; fail closed rather than acquire listeners around a closed semantic shell.
    (core/stop! (runtime/core-runtime runtime))
    (is (not (runtime/invariant-clean? runtime)))
    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  optimistic/owned-effect-kinds))
    (is (runtime/invariant-clean? runtime))))

(deftest removed-choreo-shell-handler-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)
        effect-kind :machine/local
        installed-handler (get (shell/handlers shared-shell) effect-kind)]
    (is (fn? installed-handler))

    ;; Direct shell mutation is unsupported, but composition diagnostics must
    ;; observe the physical registry rather than trusting Choreo bookkeeping.
    (shell/unregister-handler! shared-shell effect-kind)

    (is (= {:invariant :choreo-handler-ownership
            :effect-kind effect-kind
            :status :missing-from-shell}
           (choreo-handler-ownership-error runtime effect-kind)))
    (is (not (runtime/invariant-clean? runtime)))

    (let [diagnostics (runtime/diagnostics runtime)]
      (is (not (deep-identical? diagnostics installed-handler))
          "diagnostics must not expose the missing host handler"))

    (testing "startup refuses the damaged composition before listeners are acquired"
      (is (= :invalid-composition
             (error-kind #(runtime/start! runtime))))
      (is (empty? @(:added document-fixture)))
      (is (= :stopped (runtime/lifecycle runtime)))
      (is (shell/closed? shared-shell))
      (is (not-any? #(contains? (shell/handlers shared-shell) %)
                    choreo/owned-effect-kinds))
      (is (not-any? #(contains? (shell/handlers shared-shell) %)
                    optimistic/owned-effect-kinds))
      (is (runtime/invariant-clean? runtime)))))

(deftest replaced-choreo-shell-handler-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)
        effect-kind :machine/send
        installed-handler (get (shell/handlers shared-shell) effect-kind)
        replacement-handler (fn [_] :corrupt)]
    (is (fn? installed-handler))
    (is (not (identical? installed-handler replacement-handler)))

    (shell/register-handler! shared-shell effect-kind replacement-handler)

    (is (= {:invariant :choreo-handler-ownership
            :effect-kind effect-kind
            :status :replaced-in-shell}
           (choreo-handler-ownership-error runtime effect-kind)))
    (is (not (runtime/invariant-clean? runtime)))

    (let [diagnostics (runtime/diagnostics runtime)]
      (is (not (deep-identical? diagnostics installed-handler))
          "diagnostics must not expose the displaced host handler")
      (is (not (deep-identical? diagnostics replacement-handler))
          "diagnostics must not expose the replacement host handler"))

    (testing "startup refuses the damaged composition before listeners are acquired"
      (is (= :invalid-composition
             (error-kind #(runtime/start! runtime))))
      (is (empty? @(:added document-fixture)))
      (is (= :stopped (runtime/lifecycle runtime)))
      (is (shell/closed? shared-shell))
      (is (identical? replacement-handler
                      (get (shell/handlers shared-shell) effect-kind))
          "detachment must not delete a foreign replacement it does not own")
      (is (not-any? #(contains? (shell/handlers shared-shell) %)
                    (disj choreo/owned-effect-kinds effect-kind)))
      (is (not-any? #(contains? (shell/handlers shared-shell) %)
                    optimistic/owned-effect-kinds))
      (is (empty?
           (:attached-effect-kinds
            (choreo/diagnostics
             (runtime/choreo-runtime runtime)))))
      (is (runtime/invariant-clean? runtime)))))

(deftest removed-optimistic-shell-handler-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        shared-shell (runtime/shell-runtime runtime)
        effect-kind :optimistic/install-provisional
        installed-handler (get (shell/handlers shared-shell) effect-kind)]
    (is (fn? installed-handler))
    (shell/unregister-handler! shared-shell effect-kind)

    (is (= {:invariant :optimistic-handler-ownership
            :effect-kind effect-kind
            :status :missing-from-shell}
           (optimistic-handler-ownership-error runtime effect-kind)))
    (is (not (runtime/invariant-clean? runtime)))
    (is (not (deep-identical? (runtime/diagnostics runtime)
                              installed-handler)))

    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (shell/closed? shared-shell))
    (is (runtime/invariant-clean? runtime))))

(deftest replaced-optimistic-local-action-is-detected-and-foreign-handler-survives-detach-test
  (let [{:keys [runtime document-fixture]} (optimistic-fixture)
        optimistic-runtime (runtime/optimistic-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        action-id (:derive-action optimistic-runtime)
        installed-action (get (choreo/local-actions choreo-runtime) action-id)
        replacement-action (fn [_] :foreign)]
    (is (fn? installed-action))
    (is (not (identical? installed-action replacement-action)))

    (choreo/register-local-action!
     choreo-runtime
     action-id
     replacement-action)

    (is (= {:invariant :optimistic-action-ownership
            :action-id action-id
            :status :replaced-in-choreo}
           (optimistic-action-ownership-error runtime action-id)))
    (is (not (runtime/invariant-clean? runtime)))
    (is (not (deep-identical? (runtime/diagnostics runtime)
                              installed-action)))
    (is (not (deep-identical? (runtime/diagnostics runtime)
                              replacement-action)))

    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (identical? replacement-action
                    (get (choreo/local-actions choreo-runtime) action-id))
        "optimistic detachment must not clobber a foreign replacement")
    (is (runtime/invariant-clean? runtime))))

(deftest removed-optimistic-htmx-observer-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (optimistic-htmx-fixture)
        core-runtime (runtime/core-runtime runtime)
        bridge-runtime (runtime/optimistic-htmx-runtime runtime)
        event-name "htmx:beforeSend"
        installed-handler (get @(:observer-handlers bridge-runtime) event-name)]
    (is (fn? installed-handler))
    (core/unregister-event-observer!
     core-runtime event-name optimistic-htmx/observer-id)

    (is (= {:invariant :optimistic-htmx-observer-ownership
            :event-name event-name
            :status :missing-from-core}
           (optimistic-htmx-observer-ownership-error runtime event-name)))
    (is (not (runtime/invariant-clean? runtime)))
    (is (not (deep-identical? (runtime/diagnostics runtime) installed-handler)))

    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (false? (:attached? (optimistic-htmx/diagnostics bridge-runtime))))
    (is (runtime/invariant-clean? runtime))))

(deftest replaced-optimistic-htmx-observer-is-detected-and-foreign-observer-survives-detach-test
  (let [{:keys [runtime document-fixture]} (optimistic-htmx-fixture)
        core-runtime (runtime/core-runtime runtime)
        event-name "htmx:beforeRequest"
        replacement (fn [_] :foreign)]
    (core/register-event-observer!
     core-runtime event-name optimistic-htmx/observer-id replacement)

    (is (= {:invariant :optimistic-htmx-observer-ownership
            :event-name event-name
            :status :replaced-in-core}
           (optimistic-htmx-observer-ownership-error runtime event-name)))
    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (identical? replacement
                    (get-in @(:event-observers core-runtime)
                            [event-name optimistic-htmx/observer-id])))
    (is (runtime/invariant-clean? runtime))))

(deftest replaced-optimistic-htmx-send-wrapper-is-detected-and-foreign-handler-survives-detach-test
  (let [{:keys [runtime document-fixture]} (optimistic-htmx-fixture)
        choreo-runtime (runtime/choreo-runtime runtime)
        replacement (fn [_] :foreign-send)]
    (choreo/set-send-payload-handler! choreo-runtime replacement)

    (is (= {:invariant :optimistic-htmx-send-wrapper-ownership
            :status :replaced-in-choreo}
           (invariant-error runtime
                            :optimistic-htmx-send-wrapper-ownership)))
    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (identical? replacement
                    (choreo/send-payload-handler choreo-runtime)))
    (is (runtime/invariant-clean? runtime))))

(deftest removed-optimistic-htmx-transport-wrapper-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (optimistic-htmx-fixture)
        choreo-runtime (runtime/choreo-runtime runtime)]
    (choreo/set-transport-handler! choreo-runtime nil)

    (is (= {:invariant :optimistic-htmx-transport-wrapper-ownership
            :status :missing-from-choreo}
           (invariant-error runtime
                            :optimistic-htmx-transport-wrapper-ownership)))
    (is (= :invalid-composition
           (error-kind #(runtime/start! runtime))))
    (is (empty? @(:added document-fixture)))
    (is (nil? (choreo/transport-handler choreo-runtime)))
    (is (runtime/invariant-clean? runtime))))

(deftest failed-core-start-permanently-retires-the-entire-optimistic-composition-test
  (let [document-fixture
        (make-document {:fail-after-add 1})
        htmx-fixture
        (make-htmx)
        composed-runtime
        (runtime/create
         {:core-options
          {:document (:document document-fixture)
           :htmx (:htmx htmx-fixture)}
          :optimistic-options
          (default-optimistic-options)})
        shared-shell (runtime/shell-runtime composed-runtime)
        choreo-runtime (runtime/choreo-runtime composed-runtime)
        optimistic-runtime (runtime/optimistic-runtime composed-runtime)
        optimistic-actions
        #{(:derive-action optimistic-runtime)
          (:resolve-action optimistic-runtime)}
        error (thrown #(runtime/start! composed-runtime))]
    (is (some? error))
    (is (= "synthetic addEventListener failure"
           (.-message error)))
    (is (= :stopped (runtime/lifecycle composed-runtime)))
    (is (= 1 (count @(:added document-fixture))))
    (is (= 1 (count @(:removed document-fixture))))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  optimistic/owned-effect-kinds))
    (is (not-any? #(contains? (choreo/local-actions choreo-runtime) %)
                  optimistic-actions))
    (is (runtime/invariant-clean? composed-runtime))
    (is (= :already-stopped
           (error-kind #(runtime/start! composed-runtime))))))

;; =============================================================================
;; Semantic retirement ordering
;; =============================================================================

(deftest semantic-shutdown-precedes-optimistic-and-choreo-physical-detachment-test
  (let [runtime* (atom nil)
        observed (atom nil)
        document-fixture (make-document)
        htmx-fixture (make-htmx)
        composed-runtime
        (runtime/create
         {:core-options
          {:document (:document document-fixture)
           :htmx (:htmx htmx-fixture)
           :shell-options
           {:on-transition
            (fn [{:keys [event]}]
              (when (= :execution/retire (:event event))
                (let [current-runtime @runtime*
                      shared-shell (runtime/shell-runtime current-runtime)
                      choreo-runtime (runtime/choreo-runtime current-runtime)
                      optimistic-runtime
                      (runtime/optimistic-runtime current-runtime)
                      bridge-runtime
                      (runtime/optimistic-htmx-runtime current-runtime)]
                  (reset!
                   observed
                   {:effects
                    (set (keys (shell/handlers shared-shell)))
                    :actions
                    (set (keys (choreo/local-actions choreo-runtime)))
                    :optimistic-actions
                    #{(:derive-action optimistic-runtime)
                      (:resolve-action optimistic-runtime)}
                    :bridge-attached?
                    (:attached? (optimistic-htmx/diagnostics bridge-runtime))
                    :bridge-observer-events
                    (set (keys (core/event-observers
                                (runtime/core-runtime current-runtime))))
                    :bridge-send-wrapper?
                    (identical? @(:send-payload-wrapper bridge-runtime)
                                (choreo/send-payload-handler choreo-runtime))
                    :bridge-transport-wrapper?
                    (identical? @(:transport-wrapper bridge-runtime)
                                (choreo/transport-handler choreo-runtime))}))))}}
          :optimistic-options
          (default-optimistic-options)
          :optimistic-htmx-options
          (default-optimistic-htmx-options)})
        shared-shell (runtime/shell-runtime composed-runtime)
        choreo-runtime (runtime/choreo-runtime composed-runtime)]
    (reset! runtime* composed-runtime)

    (let [start-result
          (choreo/start-execution!
           choreo-runtime
           "execution-1"
           (waiting-execution))]
      (is (= :dispatched (:status start-result)))
      (is (some? (:execution-ref start-result))))

    (is (= 1
           (:active-executions
            (shell/diagnostics shared-shell))))

    (runtime/stop! composed-runtime)

    (is (every? #(contains? (:effects @observed) %)
                choreo/owned-effect-kinds)
        "semantic retirement must run while Choreo physical handlers are attached")
    (is (every? #(contains? (:effects @observed) %)
                optimistic/owned-effect-kinds)
        "semantic retirement must run while optimistic physical handlers are attached")
    (is (every? #(contains? (:actions @observed) %)
                (:optimistic-actions @observed))
        "semantic retirement must run while optimistic local actions are attached")
    (is (true? (:bridge-attached? @observed))
        "semantic retirement must run while the HTMX bridge is still attached")
    (is (= (set optimistic-htmx/observed-events)
           (:bridge-observer-events @observed))
        "semantic retirement must run while bridge observers remain registered")
    (is (true? (:bridge-send-wrapper? @observed))
        "semantic retirement must run while the bridge send wrapper is installed")
    (is (true? (:bridge-transport-wrapper? @observed))
        "semantic retirement must run while the bridge transport wrapper is installed")
    (is (= 0
           (:active-executions
            (shell/diagnostics shared-shell))))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  optimistic/owned-effect-kinds))
    (is (runtime/invariant-clean? composed-runtime))))

;; =============================================================================
;; Delegation / diagnostics
;; =============================================================================

(deftest runtime-without-optimism-remains-a-valid-composition-test
  (let [{:keys [runtime]} (fixture)]
    (is (nil? (runtime/optimistic-runtime runtime)))
    (is (nil? (runtime/optimistic-htmx-runtime runtime)))
    (is (runtime/invariant-clean? runtime))
    (runtime/start! runtime)
    (is (runtime/invariant-clean? runtime))
    (runtime/stop! runtime)
    (is (runtime/invariant-clean? runtime))))

(deftest notify-fragment-delegates-through-the-shared-core-and-adapter-test
  (let [root (make-root "request-list")
        {:keys [runtime htmx-fixture]}
        (fixture {:roots [root]})
        shared-shell (runtime/shell-runtime runtime)
        requirement
        (progression/requirement
         {:tx-id 42
          :system-time "2026-08-26T22:42:00Z"})
        result
        (runtime/notify-fragment!
         runtime
         "request-list"
         requirement)]
    (is (= :dispatched (:status result)))
    (is (= 1
           (:active-fragments
            (shell/diagnostics shared-shell))))
    (is (= 1 (count @(:calls htmx-fixture))))
    (let [{trigger-root :root
           :keys [event-name detail]}
          (first @(:calls htmx-fixture))]
      (is (identical? root trigger-root))
      (is (= core/refresh-event-name event-name))
      (is (= "request-list" (.-fragmentId detail)))
      (is (number? (.-requestGeneration detail))))
    (runtime/stop! runtime)))

(deftest diagnostics-are-read-only-and-exclude-host-resources-and-callbacks-test
  (let [{:keys [runtime document-fixture htmx-fixture]}
        (optimistic-htmx-fixture)
        document (:document document-fixture)
        htmx (:htmx htmx-fixture)
        shared-shell (runtime/shell-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)
        optimistic-runtime (runtime/optimistic-runtime runtime)
        bridge-runtime (runtime/optimistic-htmx-runtime runtime)
        host-functions
        (concat
         (vals (shell/handlers shared-shell))
         (vals (choreo/local-actions choreo-runtime))
         (vals @(:observer-handlers bridge-runtime))
         [@(:send-payload-wrapper bridge-runtime)
          @(:transport-wrapper bridge-runtime)])
        diagnostics-before (runtime/diagnostics runtime)
        state-before (runtime/state runtime)
        diagnostics-after (runtime/diagnostics runtime)
        expected-actions
        #{(:derive-action optimistic-runtime)
          (:resolve-action optimistic-runtime)}]
    (is (= diagnostics-before diagnostics-after))
    (is (= state-before (runtime/state runtime)))
    (is (not (deep-identical? diagnostics-before document)))
    (is (not (deep-identical? diagnostics-before htmx)))
    (is (every? #(not (deep-identical? diagnostics-before %))
                host-functions))
    (is (= optimistic/owned-effect-kinds
           (get-in diagnostics-before
                   [:optimistic :attached-effect-kinds])))
    (is (= expected-actions
           (get-in diagnostics-before
                   [:optimistic :attached-local-actions])))
    (is (= (set optimistic-htmx/observed-events)
           (get-in diagnostics-before
                   [:optimistic-htmx :observed-events])))
    (is (true? (get-in diagnostics-before
                       [:optimistic-htmx :attached?])))
    (is (= [] (:invariant-errors diagnostics-before)))
    (runtime/stop! runtime)))
