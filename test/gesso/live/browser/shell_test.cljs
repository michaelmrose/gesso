(ns gesso.live.browser.shell-test
  "Boundary tests for the narrow imperative Gesso Live browser shell.

   These tests deliberately do not retest the adapter's semantic transition
   table. They test the contract between that pure state machine and physical
   browser work:

     normalized event
       -> adapter/step
       -> abstract effects
       -> shell physical interpretation
       -> normalized completion/failure event

   Raw host objects may appear only in the ephemeral :physical argument or in
   shell-owned opaque resource registries. They must never enter AdapterState.

   The suite uses controlled thenables and injectable timer seams so stale,
   cancelled, and re-entrant callbacks can be exercised deterministically
   without depending on wall-clock time or a real network stack."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.shell :as shell]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- projected
  [choreography role]
  (project/project choreography role))

(defn- machine-execution
  [choreography role]
  (machine/start (projected choreography role)))

(defn- start-event
  ([execution-id execution]
   (start-event execution-id execution nil nil))
  ([execution-id execution target-id]
   (start-event execution-id execution target-id nil))
  ([execution-id execution target-id opts]
   (merge
    {:event :execution/start
     :execution-id execution-id
     :execution execution}
    (when target-id
      {:target-id target-id})
    opts)))

(defn- local-once-choreography
  []
  (choreo/->choreography
   {:initial :local
    :states
    {:local
     (choreo/local :browser :browser/do-work :done)

     :done
     (choreo/return :done)}}))

(defn- local-twice-choreography
  []
  (choreo/->choreography
   {:initial :first
    :states
    {:first
     (choreo/local :browser :browser/first :second)

     :second
     (choreo/local :browser :browser/second :done)

     :done
     (choreo/return :done)}}))

(defn- send-once-choreography
  []
  (choreo/->choreography
   {:initial :send
    :states
    {:send
     (choreo/communicate
      :browser
      :server
      :browser/command
      :done
      {:via :http
       :open-payload? true})

     :done
     (choreo/return :done)}}))

(defn- await-once-choreography
  []
  (choreo/->choreography
   {:initial :wait
    :states
    {:wait
     (choreo/await
      :browser
      {:browser/timeout :done})

     :done
     (choreo/return :done)}}))

(defn- optimistic-await-choreography
  []
  (choreo/->choreography
   {:initial :derive
    :states
    {:derive
     (choreo/local
      :browser
      :browser/derive-provisional
      :wait
      {:outputs #{:ui/provisional}})

     :wait
     (choreo/await
      :browser
      {:browser/timeout :done})

     :done
     (choreo/return :done)}}))

(defn- optimistic-start-event
  ([execution-id]
   (optimistic-start-event execution-id nil))
  ([execution-id opts]
   (start-event
    execution-id
    (machine-execution (optimistic-await-choreography) :browser)
    :request-card
    (merge
     {:optimistic
      {:command-id (str "command-" execution-id)
       :provisional-key :ui/provisional
       :rollback-eligible? true
       :timeout-ms 1000}}
     opts))))

(defn- timeout-envelope
  []
  (machine/environment-event
   :browser
   :browser/timeout))

(defn- controlled-thenable
  "Return a deterministic Promise-like host object plus manual settlement hooks."
  []
  (let [success (atom nil)
        failure (atom nil)
        value (js-obj)]
    (aset value
          "then"
          (fn [on-success on-failure]
            (reset! success on-success)
            (reset! failure on-failure)
            value))
    {:value value
     :resolve!
     (fn [result]
       (when-let [f @success]
         (f result)))
     :reject!
     (fn [error]
       (when-let [f @failure]
         (f error)))
     :success success
     :failure failure}))

(defn- deep-identical?
  [root needle]
  (cond
    (identical? root needle)
    true

    (map? root)
    (boolean
     (some
      true?
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

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- error-kind
  [f]
  (some-> (thrown f)
          ex-data
          :error/kind))

(defn- invalidation
  ([fragment-id]
   {:event :live/invalidated
    :fragment-id fragment-id})
  ([fragment-id requirement]
   {:event :live/invalidated
    :fragment-id fragment-id
    :requirement requirement}))

(defn- request-generation
  [dispatch-result]
  (->> (:effects dispatch-result)
       (some
        (fn [[kind data]]
          (when (= :fragment/refresh kind)
            (:request-generation data))))))

(defn- begin-fragment!
  [runtime fragment-id]
  (let [result (shell/dispatch! runtime (invalidation fragment-id))]
    {:result result
     :generation (request-generation result)}))

(defn- bind-request!
  [runtime fragment-id generation request-id physical]
  (shell/dispatch!
   runtime
   {:event :htmx/before-request
    :fragment-id fragment-id
    :request-generation generation
    :request-id request-id}
   physical))

(defn- before-swap!
  [runtime fragment-id generation request-id physical]
  (shell/dispatch!
   runtime
   {:event :htmx/before-swap
    :fragment-id fragment-id
    :request-generation generation
    :request-id request-id}
   physical))

;; =============================================================================
;; Construction and physical handler registry
;; =============================================================================

(deftest create-builds-isolated-open-runtime-test
  (let [a (shell/create)
        b (shell/create)]
    (is (shell/shell? a))
    (is (shell/shell? b))
    (is (not (identical? (:state a) (:state b))))
    (is (adapter/state? (shell/state a)))
    (is (= {:timers 0
            :transports 0
            :continuity 0
            :optimistic 0
            :optimistic-timeouts 0}
           (shell/resource-counts a)))
    (is (false? (shell/closed? a)))))

(deftest handler-registry-accepts-only-adapter-effect-kinds-test
  (let [runtime (shell/create)]
    (is (= :machine/local
           (shell/register-handler!
            runtime
            :machine/local
            (fn [_] {}))))
    (is (fn? (get (shell/handlers runtime) :machine/local)))
    (is (= :machine/local
           (shell/unregister-handler! runtime :machine/local)))
    (is (nil? (get (shell/handlers runtime) :machine/local)))
    (is (= :unknown-effect-handler
           (error-kind
            #(shell/register-handler!
              runtime
              :browser/invented-effect
              (fn [_] nil)))))))

;; =============================================================================
;; Synchronous re-entry and ephemeral physical context
;; =============================================================================

(deftest synchronous-local-completion-reenters-after-transition-state-is-installed-test
  (let [seen-actions (atom [])
        seen-physical (atom [])
        completed (atom [])
        raw-event (js-obj "kind" "htmx-event")
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [{:keys [effect physical]}]
             (swap! seen-actions conj (get-in effect [:action :action]))
             (swap! seen-physical conj physical)
             {})

           :execution/completed
           (fn [{:keys [effect]}]
             (swap! completed conj effect))}})]
    (let [result
          (shell/dispatch!
           runtime
           (start-event
            "execution-1"
            (machine-execution (local-twice-choreography) :browser))
           raw-event)]
      (is (= :dispatched (:status result)))
      (is (= [:browser/first :browser/second]
             @seen-actions))
      (is (identical? raw-event (first @seen-physical)))
      (is (nil? (second @seen-physical))
          "Physical context belongs only to the transition that produced the first effect.")
      (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
      (is (= 1 (count @completed)))
      (is (not (deep-identical? (shell/state runtime) raw-event)))
      (is (not (deep-identical? (:transition-state result) raw-event))))))

(deftest transition-observer-never-receives-physical-context-test
  (let [transitions (atom [])
        raw-event (js-obj "opaque" true)
        runtime
        (shell/create
         {:handlers
          {:machine/local (fn [_] {})}
          :on-transition #(swap! transitions conj %)})]
    (shell/dispatch!
     runtime
     (start-event
      "execution-1"
      (machine-execution (local-once-choreography) :browser))
     raw-event)
    (is (seq @transitions))
    (is (every? #(not (contains? % :physical)) @transitions))
    (is (every? #(not (deep-identical? % raw-event)) @transitions))))

;; =============================================================================
;; Promise-like machine completions and failure containment
;; =============================================================================

(deftest pending-local-thenable-completes-only-when-physical-callback-settles-test
  (let [{:keys [value resolve! success]}
        (controlled-thenable)
        runtime
        (shell/create
         {:handlers
          {:machine/local (fn [_] value)}})
        result
        (shell/dispatch!
         runtime
         (start-event
          "execution-1"
          (machine-execution (local-once-choreography) :browser)))]
    (is (= [:pending] (:effect-results result)))
    (is (fn? @success))
    (is (some? (adapter/execution (shell/state runtime) "execution-1")))
    (resolve! {})
    (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest throwing-machine-handler-retires-execution-instead-of-mutating-state-directly-test
  (let [errors (atom [])
        retired (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             (throw (js/Error. "local boom")))

           :execution/retired
           (fn [{:keys [effect]}]
             (swap! retired conj effect))}
          :on-error #(swap! errors conj %)})
        result
        (shell/dispatch!
         runtime
         (start-event
          "execution-1"
          (machine-execution (local-once-choreography) :browser)))]
    (is (= [:failed] (:effect-results result)))
    (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= 1 (count @retired)))
    (is (= :physical-effect-failed
           (get-in @retired [0 :reason :kind])))
    (is (= "local boom" (:message (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

;; =============================================================================
;; Send / transport interpretation
;; =============================================================================

(deftest synchronous-send-and-transport-success-chain-completes-execution-test
  (let [host-attachment (js-obj "opaque" "attachment")
        sent (atom [])
        completed (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/send
           (fn [_]
             {:semantic/value 7
              :adapter/attachment host-attachment})

           :transport/send
           (fn [{:keys [effect]}]
             (swap! sent conj (:message effect))
             :sent)

           :execution/completed
           (fn [{:keys [effect]}]
             (swap! completed conj effect))}})]
    (shell/dispatch!
     runtime
     (start-event
      "execution-1"
      (machine-execution (send-once-choreography) :browser)))
    (is (= 1 (count @sent)))
    (is (= {:semantic/value 7
            :adapter/attachment host-attachment}
           (:payload (first @sent))))
    (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= 1 (count @completed)))
    (is (= {:timers 0 :transports 0 :continuity 0
            :optimistic 0 :optimistic-timeouts 0}
           (shell/resource-counts runtime)))))

(deftest transport-cancellation-is-keyed-by-exact-adapter-generation-test
  (let [{:keys [value resolve!]}
        (controlled-thenable)
        cancelled (atom 0)
        diagnostics (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/send (fn [_] {:value 1})
           :transport/send
           (fn [_]
             {:completion value
              :cancel! #(swap! cancelled inc)})}
          :on-diagnostic #(swap! diagnostics conj %)})]
    (shell/dispatch!
     runtime
     (start-event
      "same"
      (machine-execution (send-once-choreography) :browser)))
    (is (= 1 (:transports (shell/resource-counts runtime))))

    ;; Reusing the logical id creates a new adapter generation and retires the
    ;; transport owned by the old generation immediately.
    (shell/dispatch!
     runtime
     (start-event
      "same"
      (machine-execution (await-once-choreography) :browser)
      nil
      {:replace-execution? true}))

    (is (= 1 @cancelled))
    (is (= 0 (:transports (shell/resource-counts runtime))))

    ;; A physical transport may still race after best-effort cancellation. Its
    ;; completion is normalized through the adapter and cannot affect the new
    ;; generation.
    (let [before (shell/state runtime)]
      (resolve! :late-success)
      (is (= before (shell/state runtime)))
      (is (= :stale-execution-generation
             (:reason (last @diagnostics)))))))

;; =============================================================================
;; Timer ownership and late callbacks
;; =============================================================================

(deftest timer-replacement-cancels-only-old-generation-and-late-old-fire-is-harmless-test
  (let [next-handle (atom 0)
        callbacks (atom {})
        cleared (atom [])
        diagnostics (atom [])
        runtime
        (shell/create
         {:set-timeout!
          (fn [callback _delay-ms]
            (let [handle (swap! next-handle inc)]
              (swap! callbacks assoc handle callback)
              handle))
          :clear-timeout!
          #(swap! cleared conj %)
          :on-diagnostic #(swap! diagnostics conj %)})]
    (shell/dispatch!
     runtime
     (start-event
      "execution-1"
      (machine-execution (await-once-choreography) :browser)))
    (let [generation
          (adapter/execution-generation (shell/state runtime) "execution-1")]
      (shell/dispatch!
       runtime
       {:event :timer/schedule
        :execution-id "execution-1"
        :generation generation
        :timer-id :timeout
        :delay-ms 100
        :envelope (timeout-envelope)})
      (let [first-handle 1
            first-callback (get @callbacks first-handle)]
        (shell/dispatch!
         runtime
         {:event :timer/schedule
          :execution-id "execution-1"
          :generation generation
          :timer-id :timeout
          :delay-ms 200
          :envelope (timeout-envelope)})
        (let [second-handle 2
              second-callback (get @callbacks second-handle)]
          (is (= [first-handle] @cleared))
          (is (= 1 (:timers (shell/resource-counts runtime))))

          ;; Simulate a host callback that escaped clearTimeout.
          (first-callback)
          (is (= 1 (:timers (shell/resource-counts runtime))))
          (is (= :stale-timer (:reason (last @diagnostics))))
          (is (some? (adapter/execution (shell/state runtime) "execution-1")))

          (second-callback)
          (is (= 0 (:timers (shell/resource-counts runtime))))
          (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
          (is (= [] (adapter/invariant-errors (shell/state runtime)))))))))

;; =============================================================================
;; Fragment/HTMX physical disposition
;; =============================================================================

(deftest fragment-refresh-handler-is-synchronous-physical-trigger-only-test
  (let [seen (atom [])
        physical (js-obj "source" "live-invalidation")
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh
           (fn [{:keys [effect physical]}]
             (swap! seen conj [effect physical])
             :triggered)}})]
    (let [{:keys [result generation]}
          (begin-fragment! runtime :request-card)]
      (is (pos-int? generation))
      (is (= [:triggered] (:effect-results result)))
      ;; begin-fragment! had no physical context; the handler therefore sees nil.
      (is (nil? (second (first @seen))))
      (is (some? (get-in (shell/state runtime)
                         [:fragments :request-card :inflight]))))))

(deftest missing-fragment-refresh-handler-retires-logical-fragment-test
  (let [errors (atom [])
        runtime (shell/create {:on-error #(swap! errors conj %)})
        result (shell/dispatch! runtime (invalidation :request-card))]
    (is (= [:failed] (:effect-results result)))
    (is (nil? (get-in (shell/state runtime) [:fragments :request-card])))
    (is (= :fragment-refresh-failed (:phase (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest stale-before-request-is-cancelled-by-physical-handler-without-shell-semantic-guessing-test
  (let [cancellations (atom [])
        raw-htmx (js-obj "request" "xhr-1")
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :htmx/cancel-request
           (fn [{:keys [effect physical]}]
             (swap! cancellations conj [effect physical])
             :cancelled)}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)
        before (shell/state runtime)
        result
        (shell/dispatch!
         runtime
         {:event :htmx/before-request
          :fragment-id :request-card
          :request-generation (inc generation)
          :request-id "xhr-stale"}
         raw-htmx)]
    (is (= [:cancelled] (:effect-results result)))
    (is (= before (shell/state runtime)))
    (is (= :stale-request-generation
           (get-in @cancellations [0 0 :reason])))
    (is (identical? raw-htmx (get-in @cancellations [0 1])))
    (is (not (deep-identical? (shell/state runtime) raw-htmx)))))

(deftest observer-allow-handler-failure-cannot-reverse-adapter-request-binding-test
  (let [errors (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :htmx/allow-request
           (fn [_]
             (throw (js/Error. "observer boom")))}
          :on-error #(swap! errors conj %)})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request!
     runtime
     :request-card
     generation
     "xhr-1"
     (js-obj "raw" true))
    (is (= "xhr-1"
           (get-in (shell/state runtime)
                   [:fragments :request-card :inflight :request-id])))
    (is (= :observer-effect-failed (:phase (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

;; =============================================================================
;; Continuity resource handoff
;; =============================================================================

(deftest continuity-capture-and-restore-pass-opaque-resource-without-putting-it-in-adapter-state-test
  (let [captured (js-obj "focus" "input-1")
        capture-physical (atom nil)
        restore-physical (atom nil)
        restore-resource (atom nil)
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture
           (fn [{:keys [physical]}]
             (reset! capture-physical physical)
             captured)
           :continuity/restore
           (fn [{:keys [physical resource]}]
             (reset! restore-physical physical)
             (reset! restore-resource resource)
             :restored)}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)
        before-swap-physical (js-obj "phase" "before-swap")
        after-swap-physical (js-obj "phase" "after-swap")]
    (bind-request!
     runtime :request-card generation "xhr-1" (js-obj))
    (before-swap!
     runtime :request-card generation "xhr-1" before-swap-physical)

    (is (identical? before-swap-physical @capture-physical))
    (is (= 1 (:continuity (shell/resource-counts runtime))))
    (is (not (deep-identical? (shell/state runtime) captured)))

    (shell/dispatch!
     runtime
     {:event :htmx/after-swap
      :fragment-id :request-card
      :request-generation generation
      :request-id "xhr-1"}
     after-swap-physical)

    (is (identical? after-swap-physical @restore-physical))
    (is (identical? captured @restore-resource))
    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= {} (:continuity (shell/state runtime))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest missing-continuity-capture-handler-fails-closed-before-allow-swap-test
  (let [allowed (atom 0)
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :htmx/allow-swap (fn [_] (swap! allowed inc))}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (is (= :missing-effect-handler
           (error-kind
            #(before-swap!
              runtime :request-card generation "xhr-1" (js-obj)))))
    (is (= 0 @allowed)
        "Effect interpretation stops before HTMX can be told to allow the swap.")
    (is (= 1 (count (:continuity (shell/state runtime)))))
    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest continuity-release-uses-only-exact-slot-owned-resource-test
  (let [captured (js-obj "selection" "opaque")
        released (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture (fn [_] captured)
           :continuity/release
           (fn [{:keys [effect resource]}]
             (swap! released conj [effect resource]))
           :htmx/cancel-request (fn [_] :cancelled)}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)
    (is (= 1 (:continuity (shell/resource-counts runtime))))

    (shell/dispatch!
     runtime
     {:event :fragment/retire
      :fragment-id :request-card
      :reason :test-retirement})

    (is (= 1 (count @released)))
    (is (identical? captured (get-in @released [0 1])))
    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= {} (:continuity (shell/state runtime))))))

(deftest asynchronous-continuity-restore-rejection-releases-through-adapter-test
  (let [{:keys [value reject! failure]}
        (controlled-thenable)
        captured (js-obj "selection" "opaque")
        errors (atom [])
        released (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture (fn [_] captured)
           :continuity/restore (fn [_] value)
           :continuity/release
           (fn [{:keys [effect resource]}]
             (swap! released conj [effect resource]))}
          :on-error #(swap! errors conj %)})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)

    (let [result
          (shell/dispatch!
           runtime
           {:event :htmx/after-swap
            :fragment-id :request-card
            :request-generation generation
            :request-id "xhr-1"})]
      (is (= [:pending] (:effect-results result))))

    (is (fn? @failure))
    (is (= 1 (:continuity (shell/resource-counts runtime))))
    (is (= 1 (count (:continuity (shell/state runtime)))))

    (reject! (js/Error. "restore boom"))

    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= {} (:continuity (shell/state runtime))))
    (is (= 1 (count @released)))
    (is (identical? captured (get-in @released [0 1])))
    (is (= :physical-effect-failed
           (get-in @released [0 0 :reason :kind])))
    (is (= :continuity/restore
           (get-in @released [0 0 :reason :effect])))
    (is (= "restore boom"
           (get-in @released [0 0 :reason :message])))
    (is (= :continuity-restore-failed
           (:phase (first @errors))))
    (is (= "restore boom"
           (:message (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest synchronous-continuity-restore-failure-uses-same-adapter-owned-release-path-test
  (let [captured (js-obj "focus" "opaque")
        errors (atom [])
        released (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture (fn [_] captured)
           :continuity/restore
           (fn [_]
             (throw (js/Error. "sync restore boom")))
           :continuity/release
           (fn [{:keys [effect resource]}]
             (swap! released conj [effect resource]))}
          :on-error #(swap! errors conj %)})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)

    (let [result
          (shell/dispatch!
           runtime
           {:event :htmx/after-swap
            :fragment-id :request-card
            :request-generation generation
            :request-id "xhr-1"})]
      (is (= [:failed] (:effect-results result))))

    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= {} (:continuity (shell/state runtime))))
    (is (= 1 (count @released)))
    (is (identical? captured (get-in @released [0 1])))
    (is (= :physical-effect-failed
           (get-in @released [0 0 :reason :kind])))
    (is (= :continuity/restore
           (get-in @released [0 0 :reason :effect])))
    (is (= "sync restore boom"
           (get-in @released [0 0 :reason :message])))
    (is (= :continuity-restore-failed
           (:phase (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest late-continuity-restore-rejection-after-retirement-is-stale-and-does-not-double-release-test
  (let [{:keys [value reject!]}
        (controlled-thenable)
        captured (js-obj "scroll" "opaque")
        errors (atom [])
        diagnostics (atom [])
        released (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :htmx/cancel-request (fn [_] :cancelled)
           :continuity/capture (fn [_] captured)
           :continuity/restore (fn [_] value)
           :continuity/release
           (fn [{:keys [effect resource]}]
             (swap! released conj [effect resource]))}
          :on-error #(swap! errors conj %)
          :on-diagnostic #(swap! diagnostics conj %)})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)
    (shell/dispatch!
     runtime
     {:event :htmx/after-swap
      :fragment-id :request-card
      :request-generation generation
      :request-id "xhr-1"})

    (is (= 1 (:continuity (shell/resource-counts runtime))))

    (shell/dispatch!
     runtime
     {:event :fragment/retire
      :fragment-id :request-card
      :reason :test-retirement})

    (is (= 0 (:continuity (shell/resource-counts runtime))))
    (is (= {} (:continuity (shell/state runtime))))
    (is (= 1 (count @released)))
    (is (identical? captured (get-in @released [0 1])))

    (let [retired-state (shell/state runtime)]
      (reject! (js/Error. "late restore boom"))

      (is (= retired-state (shell/state runtime)))
      (is (= 1 (count @released))
          "A stale restore callback must not release the already-retired resource twice.")
      (is (= 0 (:continuity (shell/resource-counts runtime))))
      (is (= :continuity-restore-failed
             (:phase (last @errors))))
      (is (= :stale-continuity-failure
             (:reason (last @diagnostics))))
      (is (= [] (adapter/invariant-errors (shell/state runtime)))))))


(deftest newer-fragment-swap-revokes-old-physical-resource-before-allowing-new-swap-test
  (let [{:keys [value reject!]}
        (controlled-thenable)
        captured-a (js-obj "generation" "a" "revoked" false)
        captured-b (js-obj "generation" "b" "revoked" false)
        capture-count (atom 0)
        physical-order (atom [])
        diagnostics (atom [])
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture
           (fn [{:keys [effect]}]
             (let [resource (if (= 1 (swap! capture-count inc))
                              captured-a
                              captured-b)]
               (swap! physical-order conj
                      [:capture (:request-generation effect) resource])
               resource))
           :continuity/restore
           (fn [{:keys [effect resource]}]
             (swap! physical-order conj
                    [:restore (:request-generation effect) resource])
             value)
           :continuity/release
           (fn [{:keys [effect resource]}]
             (aset resource "revoked" true)
             (swap! physical-order conj
                    [:release (:request-generation effect) resource]))
           :htmx/allow-swap
           (fn [{:keys [effect]}]
             (swap! physical-order conj
                    [:allow
                     (:request-generation effect)
                     (aget captured-a "revoked")])
             :allowed)}
          :on-diagnostic #(swap! diagnostics conj %)})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    ;; Generation A installs and begins an asynchronous continuity restore.
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)
    (shell/dispatch!
     runtime
     {:event :htmx/after-swap
      :fragment-id :request-card
      :request-generation generation
      :request-id "xhr-1"})
    (is (= 1 (:continuity (shell/resource-counts runtime))))

    ;; A newer invalidation queues generation B behind A's still-live request.
    (shell/dispatch! runtime (invalidation :request-card :basis-2))
    (let [after-a
          (shell/dispatch!
           runtime
           {:event :htmx/after-request
            :fragment-id :request-card
            :request-generation generation
            :request-id "xhr-1"})
          generation-b (request-generation after-a)]
      (is (some? generation-b))
      (is (not= generation generation-b))

      (bind-request! runtime :request-card generation-b "xhr-2" nil)
      (reset! physical-order [])
      (before-swap! runtime :request-card generation-b "xhr-2" nil)

      (let [[release capture allow] @physical-order]
        (is (= [:release :capture :allow]
               (mapv first @physical-order)))
        (is (= generation (second release)))
        (is (identical? captured-a (nth release 2)))
        (is (= generation-b (second capture)))
        (is (identical? captured-b (nth capture 2)))
        (is (= generation-b (second allow)))
        (is (true? (nth allow 2))
            "The older physical resource must be revoked before the newer swap is allowed."))

      (is (true? (aget captured-a "revoked")))
      (is (false? (aget captured-b "revoked")))
      (is (= 1 (:continuity (shell/resource-counts runtime))))
      (is (= 1 (count (:continuity (shell/state runtime)))))

      ;; The already-pending restore callback from A may still settle, but its
      ;; generation is stale and must not release B's newer physical resource.
      (let [state-after-b (shell/state runtime)]
        (reject! (js/Error. "late generation-a restore"))
        (is (= state-after-b (shell/state runtime)))
        (is (= 1 (:continuity (shell/resource-counts runtime))))
        (is (false? (aget captured-b "revoked")))
        (is (= :stale-continuity-failure
               (:reason (last @diagnostics))))))))

(deftest duplicate-before-swap-does-not-revoke-current-physical-resource-test
  (let [captured (js-obj "generation" "current" "revoked" false)
        released (atom 0)
        captures (atom 0)
        allows (atom 0)
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture
           (fn [_]
             (swap! captures inc)
             captured)
           :continuity/release
           (fn [{:keys [resource]}]
             (swap! released inc)
             (aset resource "revoked" true))
           :htmx/allow-swap
           (fn [_]
             (swap! allows inc)
             :allowed)}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)

    (is (= 1 @captures)
        "A duplicate lifecycle observation must not recapture the current generation.")
    (is (= 0 @released)
        "A generation may not revoke its own continuity resource.")
    (is (= 2 @allows)
        "The duplicate beforeSwap remains an idempotent allow observation.")
    (is (false? (aget captured "revoked")))
    (is (= 1 (:continuity (shell/resource-counts runtime))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

;; =============================================================================
;; Optimistic physical resources
;; =============================================================================

(deftest optimistic-install-and-retirement-use-exact-opaque-resource-and-timeout-test
  (let [opaque (js-obj "snapshot" "opaque")
        installed (atom [])
        finished (atom [])
        next-handle (atom 0)
        callbacks (atom {})
        cleared (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [{:keys [effect]}]
             (is (= :browser/derive-provisional
                    (get-in effect [:action :action])))
             {:ui/provisional {:projection :pending}})
           :optimistic/install-provisional
           (fn [{:keys [effect physical resource]}]
             (swap! installed conj [effect physical resource])
             opaque)
           :optimistic/finish
           (fn [{:keys [effect physical resource]}]
             (swap! finished conj [effect physical resource])
             :finished)}
          :set-timeout!
          (fn [callback delay-ms]
            (let [handle (swap! next-handle inc)]
              (swap! callbacks assoc handle {:callback callback
                                             :delay-ms delay-ms})
              handle))
          :clear-timeout! #(swap! cleared conj %)})
        physical (js-obj "source" "start")]
    (shell/dispatch!
     runtime
     (optimistic-start-event "execution-1")
     physical)
    (let [generation
          (adapter/execution-generation (shell/state runtime) "execution-1")]
      (is (= {:timers 0
              :transports 0
              :continuity 0
              :optimistic 1
              :optimistic-timeouts 1}
             (shell/resource-counts runtime)))
      (is (= 1000 (get-in @callbacks [1 :delay-ms])))
      (is (= {:projection :pending}
             (get-in @installed [0 0 :provisional])))
      ;; The local completion is a re-entrant normalized event, so the physical
      ;; context from the outer start dispatch is intentionally not retained.
      (is (nil? (get-in @installed [0 1])))
      (is (nil? (get-in @installed [0 2])))
      (is (not (deep-identical? (shell/state runtime) opaque)))

      (shell/dispatch!
       runtime
       {:event :execution/retire
        :execution-id "execution-1"
        :generation generation
        :reason :test-retirement})

      (is (= {:timers 0
              :transports 0
              :continuity 0
              :optimistic 0
              :optimistic-timeouts 0}
             (shell/resource-counts runtime)))
      (is (= [1] @cleared))
      (is (= 1 (count @finished)))
      (is (= :release-only (get-in @finished [0 0 :disposition])))
      (is (nil? (get-in @finished [0 1])))
      (is (identical? opaque (get-in @finished [0 2])))
      (is (= [] (adapter/invariant-errors (shell/state runtime)))))))

(deftest optimistic-install-must-be-synchronous-but-failure-does-not-become-command-failure-test
  (let [errors (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             {:ui/provisional {:projection :pending}})
           :optimistic/install-provisional
           (fn [_]
             (js/Promise.resolve (js-obj "late" true)))}
          :on-error #(swap! errors conj %)})]
    (shell/dispatch! runtime (optimistic-start-event "execution-1"))
    (is (= :optimistic-install-failed (:phase (first @errors))))
    (is (= :optimistic/install-provisional (:effect (first @errors))))
    (is (= 0 (:optimistic (shell/resource-counts runtime))))
    ;; Physical presentation failure is not evidence that the trusted command
    ;; failed; the adapter execution and its timeout protection remain active.
    (is (= 1 (:optimistic-timeouts (shell/resource-counts runtime))))
    (is (some? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= :provisional
           (:status
            (adapter/optimistic-scope
             (shell/state runtime)
             "execution-1"))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest missing-optimistic-install-handler-is-diagnostic-and-does-not-retire-command-test
  (let [errors (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             {:ui/provisional {:projection :pending}})}
          :on-error #(swap! errors conj %)})]
    (shell/dispatch! runtime (optimistic-start-event "execution-1"))
    (is (= :missing-optimistic-install-handler
           (:phase (first @errors))))
    (is (= 0 (:optimistic (shell/resource-counts runtime))))
    (is (= 1 (:optimistic-timeouts (shell/resource-counts runtime))))
    (is (some? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest optimistic-finish-removes-resource-before-handler-and-rejects-async-cleanup-test
  (let [opaque (js-obj "snapshot" "opaque")
        errors (atom [])
        resource-visible-during-finish? (atom nil)
        runtime-holder (atom nil)
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             {:ui/provisional {:projection :pending}})
           :optimistic/install-provisional
           (fn [_] opaque)
           :optimistic/finish
           (fn [{:keys [resource]}]
             (reset! resource-visible-during-finish?
                     (pos? (:optimistic
                            (shell/resource-counts @runtime-holder))))
             (is (identical? opaque resource))
             (js/Promise.resolve :too-late))}
          :on-error #(swap! errors conj %)})]
    (reset! runtime-holder runtime)
    (shell/dispatch! runtime (optimistic-start-event "execution-1"))
    (let [generation
          (adapter/execution-generation (shell/state runtime) "execution-1")]
      (shell/dispatch!
       runtime
       {:event :execution/retire
        :execution-id "execution-1"
        :generation generation
        :reason :test-retirement}))
    (is (false? @resource-visible-during-finish?))
    (is (= :optimistic-finish-failed (:phase (last @errors))))
    (is (= :optimistic/finish (:effect (last @errors))))
    (is (= 0 (:optimistic (shell/resource-counts runtime))))
    (is (= 0 (:optimistic-timeouts (shell/resource-counts runtime))))
    (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest replacing-same-optimistic-execution-revokes-old-physical-generation-first-test
  (let [next-handle (atom 0)
        callbacks (atom {})
        cleared (atom [])
        installed (atom [])
        finished (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             {:ui/provisional {:projection :pending}})
           :optimistic/install-provisional
           (fn [{:keys [effect]}]
             (let [resource (js-obj "generation" (:generation effect))]
               (swap! installed conj [(:generation effect) resource])
               resource))
           :optimistic/finish
           (fn [{:keys [effect resource]}]
             (swap! finished conj [(:generation effect) resource])
             :finished)}
          :set-timeout!
          (fn [callback _]
            (let [handle (swap! next-handle inc)]
              (swap! callbacks assoc handle callback)
              handle))
          :clear-timeout! #(swap! cleared conj %)})]
    (shell/dispatch! runtime (optimistic-start-event "same"))
    (let [first-generation
          (adapter/execution-generation (shell/state runtime) "same")
          first-callback (get @callbacks 1)]
      (shell/dispatch!
       runtime
       (optimistic-start-event "same" {:replace-execution? true}))
      (let [second-generation
            (adapter/execution-generation (shell/state runtime) "same")]
        (is (not= first-generation second-generation))
        (is (= [1] @cleared))
        (is (= 2 (count @installed)))
        (is (= 1 (count @finished)))
        (is (= first-generation (ffirst @finished)))
        (is (identical? (second (first @installed))
                        (second (first @finished))))
        (is (= {:timers 0
                :transports 0
                :continuity 0
                :optimistic 1
                :optimistic-timeouts 1}
               (shell/resource-counts runtime)))

        ;; Simulate a host timeout callback that escaped clearTimeout. Its
        ;; exact old timeout generation cannot remove or retire the replacement.
        (let [before (shell/state runtime)]
          (first-callback)
          (is (= before (shell/state runtime)))
          (is (= 1 (:optimistic (shell/resource-counts runtime))))
          (is (= 1 (:optimistic-timeouts
                    (shell/resource-counts runtime))))
          (is (= second-generation
                 (adapter/execution-generation
                  (shell/state runtime)
                  "same"))))))))

(deftest optimistic-timeout-allocation-failure-is-diagnostic-and-keeps-command-active-test
  (let [errors (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local
           (fn [_]
             {:ui/provisional {:projection :pending}})
           :optimistic/install-provisional
           (fn [_] (js-obj "snapshot" true))}
          :set-timeout!
          (fn [_ _]
            (throw (js/Error. "timer allocation failed")))
          :on-error #(swap! errors conj %)})]
    (shell/dispatch! runtime (optimistic-start-event "execution-1"))
    (is (= :optimistic-timeout-start-failed (:phase (last @errors))))
    (is (= :optimistic/timeout-start (:effect (last @errors))))
    (is (= 1 (:optimistic (shell/resource-counts runtime))))
    (is (= 0 (:optimistic-timeouts (shell/resource-counts runtime))))
    (is (some? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

;; =============================================================================
;; Observer noninterference and diagnostics
;; =============================================================================

(deftest terminal-observer-handler-failure-is-diagnostic-only-test
  (let [errors (atom [])
        runtime
        (shell/create
         {:handlers
          {:machine/local (fn [_] {})
           :execution/completed
           (fn [_]
             (throw (js/Error. "completion observer failed")))}
          :on-error #(swap! errors conj %)})]
    (shell/dispatch!
     runtime
     (start-event
      "execution-1"
      (machine-execution (local-once-choreography) :browser)))
    (is (nil? (adapter/execution (shell/state runtime) "execution-1")))
    (is (= :observer-effect-failed (:phase (first @errors))))
    (is (= "completion observer failed" (:message (first @errors))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))))

(deftest diagnostic-observer-failure-is-noninterfering-test
  (let [runtime
        (shell/create
         {:on-diagnostic
          (fn [_]
            (throw (js/Error. "diagnostic observer failed")))})]
    ;; Unknown/retired execution is a valid stale callback and yields one
    ;; diagnostic effect. The observer failure is intentionally swallowed.
    (let [before (shell/state runtime)
          result
          (shell/dispatch!
           runtime
           {:event :execution/retire
            :execution-id "missing"
            :generation 1
            :reason :stale})]
      (is (= :dispatched (:status result)))
      (is (= before (shell/state runtime)))
      (is (= :diagnostic/ignored (ffirst (:effects result)))))))

;; =============================================================================
;; Shutdown and late host callbacks
;; =============================================================================

(deftest shutdown-semantically-retires-work-before-best-effort-physical-cleanup-test
  (let [{transport-value :value
         transport-resolve! :resolve!}
        (controlled-thenable)
        next-handle (atom 0)
        timer-callbacks (atom {})
        cleared (atom [])
        transport-cancelled (atom 0)
        continuity-released (atom 0)
        runtime
        (shell/create
         {:handlers
          {:machine/send (fn [_] {:value 1})
           :transport/send
           (fn [_]
             {:completion transport-value
              :cancel! #(swap! transport-cancelled inc)})
           :fragment/refresh (fn [_] :triggered)
           :htmx/cancel-request (fn [_] :cancelled)
           :continuity/capture (fn [_] (js-obj "captured" true))
           :continuity/release
           (fn [_]
             (swap! continuity-released inc))}
          :set-timeout!
          (fn [callback _]
            (let [handle (swap! next-handle inc)]
              (swap! timer-callbacks assoc handle callback)
              handle))
          :clear-timeout! #(swap! cleared conj %)})]

    ;; One pending transport.
    (shell/dispatch!
     runtime
     (start-event
      "send"
      (machine-execution (send-once-choreography) :browser)))

    ;; One pending timer on a separate execution.
    (shell/dispatch!
     runtime
     (start-event
      "timer"
      (machine-execution (await-once-choreography) :browser)))
    (let [timer-generation
          (adapter/execution-generation (shell/state runtime) "timer")]
      (shell/dispatch!
       runtime
       {:event :timer/schedule
        :execution-id "timer"
        :generation timer-generation
        :timer-id :timeout
        :delay-ms 1000
        :envelope (timeout-envelope)}))

    ;; One captured continuity slot attached to an inflight fragment request.
    (let [{:keys [generation]}
          (begin-fragment! runtime :request-card)]
      (bind-request! runtime :request-card generation "xhr-1" nil)
      (before-swap! runtime :request-card generation "xhr-1" nil))

    (is (= {:timers 1 :transports 1 :continuity 1
            :optimistic 0 :optimistic-timeouts 0}
           (shell/resource-counts runtime)))

    (is (= :closed (shell/shutdown! runtime)))
    (is (shell/closed? runtime))
    (is (= {:timers 0 :transports 0 :continuity 0
            :optimistic 0 :optimistic-timeouts 0}
           (shell/resource-counts runtime)))
    (is (= 1 @transport-cancelled))
    (is (= [1] @cleared))
    (is (= 1 @continuity-released))
    (is (= 0 (count (:executions (shell/state runtime)))))
    (is (= 0 (count (:fragments (shell/state runtime)))))
    (is (= [] (adapter/invariant-errors (shell/state runtime))))

    ;; Physical callbacks can still race after shutdown. They cannot restart or
    ;; mutate semantic work.
    (let [closed-state (shell/state runtime)
          old-timer-callback (get @timer-callbacks 1)]
      (transport-resolve! :late)
      (old-timer-callback)
      (is (= closed-state (shell/state runtime)))
      (is (= :closed
             (:status
              (shell/dispatch!
               runtime
               {:event :live/invalidated
                :fragment-id :late-fragment})))))))

(deftest diagnostics-expose-no-opaque-host-resource-values-test
  (let [opaque (js-obj "secret" "host-resource")
        runtime
        (shell/create
         {:handlers
          {:fragment/refresh (fn [_] :triggered)
           :continuity/capture (fn [_] opaque)}})
        {:keys [generation]}
        (begin-fragment! runtime :request-card)]
    (bind-request! runtime :request-card generation "xhr-1" nil)
    (before-swap! runtime :request-card generation "xhr-1" nil)
    (is (= 1 (:continuity (shell/resource-counts runtime))))
    (is (not (deep-identical? (shell/diagnostics runtime) opaque)))
    (is (not (contains? (shell/diagnostics runtime) :resources)))
    (is (= [] (:invariant-errors (shell/diagnostics runtime))))))
