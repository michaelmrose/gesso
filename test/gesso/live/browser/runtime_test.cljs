(ns gesso.live.browser.runtime-test
  "Composition-root tests for gesso.live.browser.runtime.

   The runtime namespace is intentionally thin: it must compose one Core and one
   Choreo binding over the exact same shell/AdapterState, own their aggregate
   lifecycle, and add no competing semantic state machine.

   These tests therefore concentrate on composition and lifecycle properties:

   - one shared shell and AdapterState owner;
   - created/started/stopped invariant shapes;
   - exactly-once document-listener ownership;
   - exact ownership of Choreo-installed shell handler slots;
   - fail-closed detection of removed/replaced Choreo handlers;
   - semantic shell shutdown before Choreo physical detachment;
   - permanent retirement after stop or failed start;
   - delegation through the shared Core/adapter path;
   - host-resource-free diagnostics.

   Browser-global init!/shutdown! is deliberately left to real-browser
   integration tests. This namespace remains host-independent so it can run
   under both Node and Chromium."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.core :as c]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.runtime :as runtime]
   [gesso.live.browser.shell :as shell]))

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

(defn- fixture
  ([]
   (fixture nil))
  ([{:keys [roots document-options core-options choreo-options]}]
   (let [document-fixture
         (make-document
          (merge
           {:roots roots}
           document-options))

         htmx-fixture
         (make-htmx)

         runtime
         (runtime/create
          (cond->
           {:core-options
            (merge
             {:document (:document document-fixture)
              :htmx (:htmx htmx-fixture)}
             core-options)}
            choreo-options
            (assoc :choreo-options choreo-options)))]
     {:runtime runtime
      :document-fixture document-fixture
      :htmx-fixture htmx-fixture})))

(defn- listener-names
  [registrations]
  (mapv first registrations))

(defn- choreo-handler-ownership-error
  [runtime effect-kind]
  (some
   (fn [error]
     (when
      (and
       (= :choreo-handler-ownership
          (:invariant error))
       (= effect-kind
          (:effect-kind error)))
       error))
   (runtime/invariant-errors runtime)))

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
    (is (= "1.1.0-dev" runtime/runtime-version))
    (is (= :gesso.live.browser.runtime/runtime
           runtime/runtime-type))
    (is (runtime/runtime? runtime))
    (is (not (runtime/runtime? nil)))
    (is (= :created (runtime/lifecycle runtime)))
    (is (false? (runtime/started? runtime)))
    (is (false? (runtime/stopped? runtime)))
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

(deftest create-forwards-physical-configuration-without-adding-semantic-state-test
  (let [local-handler (fn [_] {:value :ok})
        {:keys [runtime]}
        (fixture
         {:choreo-options
          {:local-actions
           {:browser/work local-handler}}})
        diagnostics (runtime/diagnostics runtime)]
    (is (= #{:browser/work}
           (get-in diagnostics
                   [:choreo :registered-local-actions])))
    (is (= :created (:lifecycle diagnostics)))
    (is (not (contains? runtime :executions)))
    (is (not (contains? runtime :targets)))
    (is (not (contains? runtime :timers)))
    (is (not (contains? runtime :continuity)))
    (runtime/stop! runtime)))

(deftest create-validates-composition-options-test
  (is (= :invalid-map
         (error-kind #(runtime/create [:not :a-map]))))
  (is (= :unknown-options
         (error-kind #(runtime/create {:invented true}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:core-options [:not :a-map]}))))
  (is (= :invalid-map
         (error-kind #(runtime/create {:choreo-options [:not :a-map]})))))

;; =============================================================================
;; Lifecycle ownership
;; =============================================================================

(deftest start-installs-core-listeners-exactly-once-test
  (let [{:keys [runtime document-fixture]} (fixture)]
    (is (identical? runtime (runtime/start! runtime)))
    (is (= :started (runtime/lifecycle runtime)))
    (is (runtime/started? runtime))
    (is (= expected-listener-names
           (listener-names @(:added document-fixture))))
    (is (runtime/invariant-clean? runtime))

    (testing "repeated start is idempotent"
      (is (identical? runtime (runtime/start! runtime)))
      (is (= expected-listener-names
             (listener-names @(:added document-fixture)))))

    (runtime/stop! runtime)))

(deftest stop-from-started-removes-listeners-closes-shell-and-detaches-choreo-test
  (let [{:keys [runtime document-fixture]} (fixture)
        shared-shell (runtime/shell-runtime runtime)]
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
    (is (empty?
         (:attached-effect-kinds
          (choreo/diagnostics
           (runtime/choreo-runtime runtime)))))
    (is (runtime/invariant-clean? runtime))

    (testing "repeated stop performs no second physical teardown"
      (let [removed-count (count @(:removed document-fixture))]
        (is (= :stopped (runtime/stop! runtime)))
        (is (= removed-count
               (count @(:removed document-fixture))))))))

(deftest stop-from-created-retires-without-ever-acquiring-document-listeners-test
  (let [{:keys [runtime document-fixture]} (fixture)
        shared-shell (runtime/shell-runtime runtime)]
    (is (= :stopped (runtime/stop! runtime)))
    (is (empty? @(:added document-fixture)))
    (is (empty? @(:removed document-fixture)))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (runtime/invariant-clean? runtime))))

(deftest stopped-runtime-cannot-be-restarted-test
  (let [{:keys [runtime]} (fixture)]
    (runtime/stop! runtime)
    (is (= :already-stopped
           (error-kind #(runtime/start! runtime))))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (runtime/invariant-clean? runtime))))

(deftest externally-stopped-child-is-detected-before-start-test
  (let [{:keys [runtime]} (fixture)
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
    (is (runtime/invariant-clean? runtime))))

(deftest removed-choreo-shell-handler-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (fixture)
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
      (is (runtime/invariant-clean? runtime)))))

(deftest replaced-choreo-shell-handler-is-detected-and-start-fails-closed-test
  (let [{:keys [runtime document-fixture]} (fixture)
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
      (is (empty?
           (:attached-effect-kinds
            (choreo/diagnostics
             (runtime/choreo-runtime runtime)))))
      (is (runtime/invariant-clean? runtime)))))

(deftest failed-core-start-permanently-retires-partial-composition-test
  (let [document-fixture
        (make-document {:fail-after-add 1})
        htmx-fixture
        (make-htmx)
        runtime
        (runtime/create
         {:core-options
          {:document (:document document-fixture)
           :htmx (:htmx htmx-fixture)}})
        shared-shell (runtime/shell-runtime runtime)
        error (thrown #(runtime/start! runtime))]
    (is (some? error))
    (is (= "synthetic addEventListener failure"
           (.-message error)))
    (is (= :stopped (runtime/lifecycle runtime)))
    (is (= 1 (count @(:added document-fixture))))
    (is (= 1 (count @(:removed document-fixture))))
    (is (shell/closed? shared-shell))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (runtime/invariant-clean? runtime))
    (is (= :already-stopped
           (error-kind #(runtime/start! runtime))))))

;; =============================================================================
;; Semantic retirement ordering
;; =============================================================================

(deftest semantic-shutdown-precedes-choreo-physical-detachment-test
  (let [shell-runtime* (atom nil)
        handlers-seen-during-retirement (atom nil)
        document-fixture (make-document)
        htmx-fixture (make-htmx)
        runtime
        (runtime/create
         {:core-options
          {:document (:document document-fixture)
           :htmx (:htmx htmx-fixture)
           :shell-options
           {:on-transition
            (fn [{:keys [event]}]
              (when (= :execution/retire (:event event))
                (reset!
                 handlers-seen-during-retirement
                 (set
                  (keys
                   (shell/handlers
                    @shell-runtime*))))))}}})
        shared-shell (runtime/shell-runtime runtime)
        choreo-runtime (runtime/choreo-runtime runtime)]
    (reset! shell-runtime* shared-shell)

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

    (runtime/stop! runtime)

    (is (every? #(contains? @handlers-seen-during-retirement %)
                choreo/owned-effect-kinds)
        "semantic retirement must run while Choreo physical handlers are still attached")
    (is (= 0
           (:active-executions
            (shell/diagnostics shared-shell))))
    (is (not-any? #(contains? (shell/handlers shared-shell) %)
                  choreo/owned-effect-kinds))
    (is (runtime/invariant-clean? runtime))))

;; =============================================================================
;; Delegation / diagnostics
;; =============================================================================

(deftest notify-fragment-delegates-through-the-shared-core-and-adapter-test
  (let [root (make-root "request-list")
        {:keys [runtime htmx-fixture]}
        (fixture {:roots [root]})
        shared-shell (runtime/shell-runtime runtime)
        result
        (runtime/notify-fragment!
         runtime
         "request-list"
         {:basis 42})]
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

(deftest diagnostics-are-read-only-and-exclude-host-resources-test
  (let [{:keys [runtime document-fixture htmx-fixture]} (fixture)
        document (:document document-fixture)
        htmx (:htmx htmx-fixture)
        diagnostics-before (runtime/diagnostics runtime)
        state-before (runtime/state runtime)
        diagnostics-after (runtime/diagnostics runtime)]
    (is (= diagnostics-before diagnostics-after))
    (is (= state-before (runtime/state runtime)))
    (is (not (deep-identical? diagnostics-before document)))
    (is (not (deep-identical? diagnostics-before htmx)))
    (is (= [] (:invariant-errors diagnostics-before)))
    (runtime/stop! runtime)))
