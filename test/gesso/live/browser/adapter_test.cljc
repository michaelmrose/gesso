(ns gesso.live.browser.adapter-test
  "Architecture-independent tests for the pure Gesso Live browser adapter.

   These tests intentionally describe browser safety properties rather than the
   registries/callback structure of the pre-v4.5 CLJS runtime. The portable
   boundary under test is:

     AdapterState x NormalizedEvent -> AdapterState x AbstractEffects

   Host objects never appear in these fixtures. Choreo executions are ordinary
   projected portable machines; browser work is represented only by normalized
   events and abstract effects."
  (:require
   [gesso.choreo.core :as choreo]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- exception-info?
  [value]
  #?(:clj (instance? clojure.lang.ExceptionInfo value)
     :cljs (instance? cljs.core.ExceptionInfo value)))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) error
      error)))

(defn- error-kind
  [f]
  (some-> (thrown f) ex-data :error/kind))

(defn- projected
  [choreography role]
  (project/project choreography role))

(defn- machine-execution
  [choreography role]
  (machine/start (projected choreography role)))

(defn- effects-of
  [kind effects]
  (filterv #(= kind (first %)) effects))

(defn- effect-of
  [kind effects]
  (first (effects-of kind effects)))

(defn- effect-data
  [kind effects]
  (second (effect-of kind effects)))

(defn- ignored-reason
  [effects]
  (get-in (effect-of :diagnostic/ignored effects) [1 :reason]))

(defn- start
  ([state execution-id execution]
   (start state execution-id execution nil nil))
  ([state execution-id execution target-id]
   (start state execution-id execution target-id nil))
  ([state execution-id execution target-id opts]
   (adapter/step
    state
    (merge
     {:event :execution/start
      :execution-id execution-id
      :execution execution}
     (when target-id {:target-id target-id})
     opts))))

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

(defn- receive-twice-choreography
  []
  (choreo/->choreography
   {:initial :first
    :states
    {:first
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :second
      {:via :http})

     :second
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :done
      {:via :http})

     :done
     (choreo/return :done)}}))

(defn- local-then-receive-choreography
  []
  (choreo/->choreography
   {:initial :local
    :states
    {:local
     (choreo/local :browser :browser/prepare :receive)

     :receive
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :done
      {:via :http})

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

(defn- browser-authoritative-choreography
  []
  (choreo/->choreography
   {:initial :trusted
    :states
    {:trusted
     (choreo/authoritative
      :browser
      :browser/forbidden-authority
      :done)

     :done
     (choreo/return :done)}}))

(defn- settlement-envelope
  []
  (machine/message
   :server
   :browser
   :server/settlement
   {}
   {:via :http}))

(defn- timeout-envelope
  []
  (machine/environment-event
   :browser
   :browser/timeout))

(defn- invalidation
  ([fragment-id]
   {:event :live/invalidated
    :fragment-id fragment-id})
  ([fragment-id requirement]
   {:event :live/invalidated
    :fragment-id fragment-id
    :requirement requirement}))

(defn- begin-fragment
  ([state fragment-id]
   (begin-fragment state fragment-id nil))
  ([state fragment-id requirement]
   (let [[state' effects]
         (adapter/step
          state
          (if (some? requirement)
            (invalidation fragment-id requirement)
            (invalidation fragment-id)))
         refresh (effect-data :fragment/refresh effects)]
     {:state state'
      :effects effects
      :generation (:request-generation refresh)
      :refresh refresh})))

(defn- bind-request
  [state fragment-id generation request-id]
  (adapter/step
   state
   {:event :htmx/before-request
    :fragment-id fragment-id
    :request-generation generation
    :request-id request-id}))

(defn- before-swap
  ([state fragment-id generation request-id]
   (before-swap state fragment-id generation request-id nil))
  ([state fragment-id generation request-id authoritative]
   (adapter/step
    state
    (cond->
     {:event :htmx/before-swap
      :fragment-id fragment-id
      :request-generation generation
      :request-id request-id}
      authoritative
      (assoc :authoritative authoritative)))))

;; =============================================================================
;; State and event boundary
;; =============================================================================

(deftest initial-state-satisfies-declared-invariants-test
  (let [state (adapter/initial-state)]
    (is (adapter/state? state))
    (is (= [] (adapter/invariant-errors state)))
    (is (= {} (:executions state)))
    (is (= {} (:targets state)))
    (is (= {} (:timers state)))
    (is (= {} (:fragments state)))
    (is (= {} (:continuity state)))
    (is (= {} (:authoritative state)))))

(deftest adapter-event-vocabulary-is-closed-test
  (testing "unknown normalized event kinds fail closed"
    (is (= :unknown-event
           (error-kind
            #(adapter/step
              (adapter/initial-state)
              {:event :browser/magic})))))

  (testing "known events reject undeclared fields instead of silently ignoring them"
    (is (= :unknown-event-keys
           (error-kind
            #(adapter/step
              (adapter/initial-state)
              {:event :live/invalidated
               :fragment-id :panel
               :browser/host-object :opaque}))))))

(deftest diagnostics-are-explicitly-removable-from-semantic-effects-test
  (let [effects [[:machine/local {:x 1}]
                 [:diagnostic/ignored {:reason :stale}]
                 [:timer/start {:x 2}]]]
    (is (= [[:machine/local {:x 1}]
            [:timer/start {:x 2}]]
           (adapter/semantic-effects effects)))))

;; =============================================================================
;; Execution generations, target ownership, and local effects
;; =============================================================================

(deftest execution-start-exposes-machine-local-boundary-test
  (let [[state effects]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-once-choreography) :browser)
         :request-card)
        generation (adapter/execution-generation state "execution-1")
        local (effect-data :machine/local effects)]
    (is (pos-int? generation))
    (is (= {:execution-id "execution-1"
            :generation generation}
           (adapter/target-owner state :request-card)))
    (is (= "execution-1" (:execution-id local)))
    (is (= generation (:generation local)))
    (is (pos-int? (:effect-generation local)))
    (is (= :local (get-in local [:action :kind])))
    (is (= :browser/do-work (get-in local [:action :action])))
    (is (adapter/state? state))))

(deftest duplicate-active-execution-start-fails-closed-test
  (let [[state _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-once-choreography) :browser))]
    (is (= :execution-already-active
           (error-kind
            #(start
              state
              "execution-1"
              (machine-execution (local-once-choreography) :browser)))))))

(deftest target-owner-replacement-semantically-retires-old-execution-first-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "old"
         (machine-execution (local-once-choreography) :browser)
         :card)
        old-generation (adapter/execution-generation state-a "old")
        [state-b effects]
        (start
         state-a
         "new"
         (machine-execution (local-once-choreography) :browser)
         :card
         {:replace-owner? true})
        new-generation (adapter/execution-generation state-b "new")]
    (is (nil? (adapter/execution state-b "old")))
    (is (not= old-generation new-generation))
    (is (= {:execution-id "new"
            :generation new-generation}
           (adapter/target-owner state-b :card)))
    (is (= :execution/retired (ffirst effects)))
    (is (= :target-replaced (get-in effects [0 1 :reason])))
    (is (= 1 (count (effects-of :machine/local effects))))))

(deftest same-execution-id-replacement-allocates-new-generation-and-rejects-old-callback-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "same"
         (machine-execution (local-once-choreography) :browser))
        old-generation (adapter/execution-generation state-a "same")
        old-effect-generation (:effect-generation
                               (effect-data :machine/local effects-a))
        [state-b _]
        (start
         state-a
         "same"
         (machine-execution (local-once-choreography) :browser)
         nil
         {:replace-execution? true})
        new-generation (adapter/execution-generation state-b "same")
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :machine/local-completed
          :execution-id "same"
          :generation old-generation
          :effect-generation old-effect-generation
          :outputs {}})]
    (is (not= old-generation new-generation))
    (is (= state-b state-c))
    (is (= :stale-execution-generation
           (ignored-reason effects-c)))))

(deftest local-effect-generations-prevent-replay-into-later-boundary-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-twice-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        first-effect (:effect-generation
                      (effect-data :machine/local effects-a))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :machine/local-completed
          :execution-id "execution-1"
          :generation generation
          :effect-generation first-effect
          :outputs {}})
        second-effect (:effect-generation
                       (effect-data :machine/local effects-b))
        [state-c replay-effects]
        (adapter/step
         state-b
         {:event :machine/local-completed
          :execution-id "execution-1"
          :generation generation
          :effect-generation first-effect
          :outputs {}})]
    (is (not= first-effect second-effect))
    (is (= state-b state-c))
    (is (= :stale-local-effect
           (ignored-reason replay-effects)))
    (is (= second-effect
           (get-in state-c
                   [:executions "execution-1" :pending-effect :generation])))))

(deftest local-completion-retires-terminal-execution-and-releases-target-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-once-choreography) :browser)
         :card)
        generation (adapter/execution-generation state-a "execution-1")
        effect-generation (:effect-generation
                           (effect-data :machine/local effects-a))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :machine/local-completed
          :execution-id "execution-1"
          :generation generation
          :effect-generation effect-generation
          :outputs {}})]
    (is (nil? (adapter/execution state-b "execution-1")))
    (is (nil? (adapter/target-owner state-b :card)))
    (is (= :execution/completed (ffirst effects-b)))
    (is (= {:outcome :gesso.choreo/complete}
           (get-in effects-b [0 1 :result])))))

(deftest browser-authoritative-boundary-fails-closed-test
  (is (= :browser-authoritative-boundary
         (error-kind
          #(start
            (adapter/initial-state)
            "execution-1"
            (machine-execution
             (browser-authoritative-choreography)
             :browser))))))

;; =============================================================================
;; Send / transport boundary
;; =============================================================================

(deftest send-selection-and-transport-completion-are-separate-generations-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (send-once-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        selection-generation (:effect-generation
                              (effect-data :machine/send effects-a))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :machine/send-requested
          :execution-id "execution-1"
          :generation generation
          :effect-generation selection-generation
          :payload {:adapter/transport-extra :opaque}})
        transport (effect-data :transport/send effects-b)
        transport-generation (:effect-generation transport)
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :transport/succeeded
          :execution-id "execution-1"
          :generation generation
          :effect-generation transport-generation})]
    (is (not= selection-generation transport-generation))
    (is (= {:adapter/transport-extra :opaque}
           (get-in transport [:message :payload])))
    (is (nil? (adapter/execution state-c "execution-1")))
    (is (= 1 (count (effects-of :execution/completed effects-c))))))

(deftest transport-failure-does-not-fabricate-choreo-send-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (send-once-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        selection-generation (:effect-generation
                              (effect-data :machine/send effects-a))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :machine/send-requested
          :execution-id "execution-1"
          :generation generation
          :effect-generation selection-generation
          :payload {}})
        transport-generation (:effect-generation
                              (effect-data :transport/send effects-b))
        [state-c failure-effects]
        (adapter/step
         state-b
         {:event :transport/failed
          :execution-id "execution-1"
          :generation generation
          :effect-generation transport-generation
          :reason :network})]
    (is (some? (adapter/execution state-c "execution-1")))
    (is (machine/waiting-send?
         (get-in state-c [:executions "execution-1" :execution])))
    (is (nil? (get-in state-c
                      [:executions "execution-1" :pending-effect])))
    (is (= :transport-failed (ignored-reason failure-effects)))))

(deftest stale-transport-success-cannot-complete-a-retried-send-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (send-once-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        selection-1 (:effect-generation (effect-data :machine/send effects-a))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :machine/send-requested
          :execution-id "execution-1"
          :generation generation
          :effect-generation selection-1
          :payload {}})
        transport-1 (:effect-generation (effect-data :transport/send effects-b))
        [state-c _]
        (adapter/step
         state-b
         {:event :transport/failed
          :execution-id "execution-1"
          :generation generation
          :effect-generation transport-1
          :reason :network})
        [state-d retry-effects]
        (adapter/step
         state-c
         {:event :machine/retry
          :execution-id "execution-1"
          :generation generation})
        selection-2 (:effect-generation (effect-data :machine/send retry-effects))
        [state-e stale-effects]
        (adapter/step
         state-d
         {:event :transport/succeeded
          :execution-id "execution-1"
          :generation generation
          :effect-generation transport-1})]
    (is (not= selection-1 selection-2))
    (is (= state-d state-e))
    (is (= :stale-transport-success
           (ignored-reason stale-effects)))))

;; =============================================================================
;; Participant messages and exact physical delivery identity
;; =============================================================================

(deftest consumed-physical-message-id-cannot-satisfy-later-identical-receive-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (receive-twice-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        envelope (settlement-envelope)
        [state-b _]
        (adapter/step
         state-a
         {:event :machine/message
          :execution-id "execution-1"
          :generation generation
          :message-id :delivery-1
          :envelope envelope})
        [state-c replay-effects]
        (adapter/step
         state-b
         {:event :machine/message
          :execution-id "execution-1"
          :generation generation
          :message-id :delivery-1
          :envelope envelope})
        [state-d final-effects]
        (adapter/step
         state-c
         {:event :machine/message
          :execution-id "execution-1"
          :generation generation
          :message-id :delivery-2
          :envelope envelope})]
    (is (= state-b state-c))
    (is (= :duplicate-message
           (ignored-reason replay-effects)))
    (is (nil? (adapter/execution state-d "execution-1")))
    (is (= 1 (count (effects-of :execution/completed final-effects))))))

(deftest rejected-early-physical-message-id-is-consumed-and-cannot-be-replayed-later-test
  ;; Adversarial regression: one physical callback that arrived before its
  ;; receive gate must not become a fresh delivery merely because the machine
  ;; later reaches a matching receive state.
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-then-receive-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        local-generation (:effect-generation
                          (effect-data :machine/local effects-a))
        envelope (settlement-envelope)
        [state-b early-effects]
        (adapter/step
         state-a
         {:event :machine/message
          :execution-id "execution-1"
          :generation generation
          :message-id :early-delivery
          :envelope envelope})
        [state-c _]
        (adapter/step
         state-b
         {:event :machine/local-completed
          :execution-id "execution-1"
          :generation generation
          :effect-generation local-generation
          :outputs {}})
        [state-d replay-effects]
        (adapter/step
         state-c
         {:event :machine/message
          :execution-id "execution-1"
          :generation generation
          :message-id :early-delivery
          :envelope envelope})]
    (is (= :message-not-accepted
           (ignored-reason early-effects)))
    (is (= state-c state-d)
        "The same physical delivery must remain spent after being observed early.")
    (is (= :duplicate-message
           (ignored-reason replay-effects)))))

;; =============================================================================
;; Execution-owned timers
;; =============================================================================

(deftest timer-replacement-cancels-old-generation-and-stale-fire-is-harmless-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (await-once-choreography) :browser))
        generation (adapter/execution-generation state-a "execution-1")
        envelope (timeout-envelope)
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :timer/schedule
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :delay-ms 100
          :envelope envelope})
        timer-1 (:timer-generation (effect-data :timer/start effects-b))
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :timer/schedule
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :delay-ms 200
          :envelope envelope})
        timer-2 (:timer-generation (effect-data :timer/start effects-c))
        [state-d stale-effects]
        (adapter/step
         state-c
         {:event :timer/fired
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :timer-generation timer-1})]
    (is (not= timer-1 timer-2))
    (is (= timer-1
           (:timer-generation (effect-data :timer/cancel effects-c))))
    (is (= state-c state-d))
    (is (= :stale-timer (ignored-reason stale-effects)))))

(deftest current-timer-fire-resumes-machine-and-retires-execution-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (await-once-choreography) :browser)
         :card)
        generation (adapter/execution-generation state-a "execution-1")
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :timer/schedule
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :delay-ms 10
          :envelope (timeout-envelope)})
        timer-generation (:timer-generation (effect-data :timer/start effects-b))
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :timer/fired
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :timer-generation timer-generation})]
    (is (nil? (adapter/execution state-c "execution-1")))
    (is (nil? (adapter/target-owner state-c :card)))
    (is (empty? (:timers state-c)))
    (is (= 1 (count (effects-of :execution/completed effects-c))))))

(deftest explicit-execution-retirement-cancels-owned-timers-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (await-once-choreography) :browser)
         :card)
        generation (adapter/execution-generation state-a "execution-1")
        [state-b _]
        (adapter/step
         state-a
         {:event :timer/schedule
          :execution-id "execution-1"
          :generation generation
          :timer-id :deadline
          :delay-ms 10
          :envelope (timeout-envelope)})
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :execution/retire
          :execution-id "execution-1"
          :generation generation
          :reason :navigation})]
    (is (empty? (:timers state-c)))
    (is (nil? (adapter/execution state-c "execution-1")))
    (is (= 1 (count (effects-of :execution/retired effects-c))))
    (is (= 1 (count (effects-of :timer/cancel effects-c))))))

;; =============================================================================
;; Fragment single-flight and request generation
;; =============================================================================

(deftest first-invalidation-starts-one-fragment-refresh-test
  (let [{:keys [state effects generation refresh]}
        (begin-fragment
         (adapter/initial-state)
         :request-card
         :basis-1)]
    (is (pos-int? generation))
    (is (= :request-card (:fragment-id refresh)))
    (is (= #{:basis-1} (:requirements refresh)))
    (is (= #{:basis-1}
           (get-in state
                   [:fragments :request-card :inflight :requirements])))
    (is (= #{}
           (get-in state
                   [:fragments :request-card :queued-requirements])))
    (is (= 1 (count (effects-of :fragment/refresh effects))))))

(deftest invalidations-during-one-request-coalesce-without-starting-another-request-test
  (let [{state-a :state generation :generation}
        (begin-fragment
         (adapter/initial-state)
         :request-card
         :basis-1)
        [state-b effects-b]
        (adapter/step state-a (invalidation :request-card :basis-2))
        [state-c effects-c]
        (adapter/step state-b (invalidation :request-card :basis-3))]
    (is (= generation
           (get-in state-c
                   [:fragments :request-card :inflight :generation])))
    (is (= #{:basis-2 :basis-3}
           (get-in state-c
                   [:fragments :request-card :queued-requirements])))
    (is (= [] effects-b))
    (is (= [] effects-c))))

(deftest before-request-binds-one-physical-request-id-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b effects-b]
        (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c]
        (bind-request state-b :panel generation :xhr-2)]
    (is (= :xhr-1
           (get-in state-b [:fragments :panel :inflight :request-id])))
    (is (= 1 (count (effects-of :htmx/allow-request effects-b))))
    (is (= state-b state-c))
    (is (= 1 (count (effects-of :htmx/cancel-request effects-c))))
    (is (= :stale-request-generation
           (get-in effects-c [0 1 :reason])))))

(deftest stale-request-generation-is-cancelled-before-htmx-work-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b effects-b]
        (bind-request state-a :panel (inc generation) :stale-xhr)]
    (is (= state-a state-b))
    (is (= 1 (count (effects-of :htmx/cancel-request effects-b))))
    (is (nil? (effect-of :htmx/allow-request effects-b)))))

(deftest later-htmx-phases-require-a-request-id-previously-accepted-by-before-request-test
  ;; `before-request` is the point where a physical HTMX request becomes owned
  ;; by one logical request generation. before-swap/after-swap/after-request and
  ;; failure callbacks must not be able to bind that identity retroactively.
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b swap-effects]
        (before-swap
         state-a
         :panel
         generation
         :never-accepted
         {:scope :request-1
          :basis :basis-1})
        [state-c request-effects]
        (adapter/step
         state-a
         {:event :htmx/after-request
          :fragment-id :panel
          :request-generation generation
          :request-id :never-accepted})]
    (is (= state-a state-b)
        "before-swap may not implicitly claim an unbound request id.")
    (is (= 1 (count (effects-of :htmx/cancel-swap swap-effects))))
    (is (= state-a state-c)
        "after-request may not finish a refresh whose request never started.")
    (is (= :stale-after-request
           (ignored-reason request-effects)))))

;; =============================================================================
;; Authoritative installation and continuity generations
;; =============================================================================

(deftest accepted-before-swap-captures-continuity-once-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _]
        (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c]
        (before-swap state-b :panel generation :xhr-1)
        [state-d effects-d]
        (before-swap state-c :panel generation :xhr-1)
        capture (effect-data :continuity/capture effects-c)]
    (is (pos-int? (:slot-generation capture)))
    (is (= [:panel generation] (:slot-id capture)))
    (is (= 1 (count (effects-of :continuity/capture effects-c))))
    (is (= 1 (count (effects-of :htmx/allow-swap effects-c))))
    (is (= 0 (count (effects-of :continuity/capture effects-d))))
    (is (= 1 (count (effects-of :htmx/allow-swap effects-d))))
    (is (= state-c state-d))))

(deftest first-authoritative-basis-installs-without-ordering-assumption-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _]
        (bind-request state-a :panel generation :xhr-1)
        [state-c _]
        (before-swap
         state-b
         :panel
         generation
         :xhr-1
         {:scope :request-1
          :basis {:opaque :basis-1}})
        [state-d effects-d]
        (adapter/step
         state-c
         {:event :htmx/after-swap
          :fragment-id :panel
          :request-generation generation
          :request-id :xhr-1})]
    (is (= {:basis {:opaque :basis-1}}
           (adapter/authoritative-frontier state-d :request-1)))
    (is (= 1 (count (effects-of :authoritative/installed effects-d))))
    (is (= 1 (count (effects-of :continuity/restore effects-d))))))

(deftest distinct-authoritative-basis-requires-exact-progression-witness-test
  (let [{state-a :state gen-1 :generation}
        (begin-fragment (adapter/initial-state) :panel :basis-1)
        [state-b _] (bind-request state-a :panel gen-1 :xhr-1)
        [state-c _]
        (before-swap state-b :panel gen-1 :xhr-1
                     {:scope :request-1 :basis :basis-1})
        [state-d _]
        (adapter/step state-c
                      {:event :htmx/after-swap
                       :fragment-id :panel
                       :request-generation gen-1
                       :request-id :xhr-1})
        [state-e _]
        (adapter/step state-d
                      {:event :htmx/after-request
                       :fragment-id :panel
                       :request-generation gen-1
                       :request-id :xhr-1})
        {state-f :state gen-2 :generation}
        (begin-fragment state-e :panel :basis-2)
        [state-g _] (bind-request state-f :panel gen-2 :xhr-2)
        [state-h denied]
        (before-swap state-g :panel gen-2 :xhr-2
                     {:scope :request-1 :basis :basis-2})
        [state-i allowed]
        (before-swap state-g :panel gen-2 :xhr-2
                     {:scope :request-1
                      :basis :basis-2
                      :progression {:from :basis-1
                                    :to :basis-2
                                    :relation :advances}})]
    (is (= state-g state-h))
    (is (= :non-monotone-authoritative-install
           (get-in (effect-of :htmx/cancel-swap denied) [1 :reason])))
    (is (= 1 (count (effects-of :htmx/allow-swap allowed))))
    (is (= {:basis :basis-1}
           (adapter/authoritative-frontier state-i :request-1))
        "A candidate does not advance the confirmed frontier before after-swap.")
    (is (= :basis-2
           (get-in state-i
                   [:fragments :panel :inflight :authoritative :basis])))
    (is (some?
         (get-in state-i [:continuity [:panel gen-2]])))))

(deftest malformed-authoritative-candidate-fails-closed-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _]
        (bind-request state-a :panel generation :xhr-1)]
    (is (= :invalid-authoritative-candidate
           (error-kind
            #(before-swap
              state-b
              :panel
              generation
              :xhr-1
              {:scope :request-1}))))))

(deftest continuity-completion-is-generation-gated-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c] (before-swap state-b :panel generation :xhr-1)
        slot (effect-data :continuity/capture effects-c)
        [state-d stale-effects]
        (adapter/step
         state-c
         {:event :continuity/completed
          :slot-id (:slot-id slot)
          :slot-generation (inc (:slot-generation slot))})]
    (is (= state-c state-d))
    (is (= :stale-continuity-completion
           (ignored-reason stale-effects)))))

(deftest continuity-cannot-complete-before-restore-has-been-issued-test
  ;; The capture effect reveals the slot generation to the physical shell. A
  ;; premature/misordered callback with that exact identity must not release the
  ;; slot before the authoritative swap has produced :continuity/restore.
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c] (before-swap state-b :panel generation :xhr-1)
        slot (effect-data :continuity/capture effects-c)
        [state-d completion-effects]
        (adapter/step
         state-c
         {:event :continuity/completed
          :slot-id (:slot-id slot)
          :slot-generation (:slot-generation slot)})]
    (is (= state-c state-d)
        "Capture is not completion authority; the slot remains live until restore is issued.")
    (is (= :continuity-restore-not-issued
           (ignored-reason completion-effects)))))

(deftest fragment-retirement-releases-continuity-and-cancels-bound-request-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c _] (before-swap state-b :panel generation :xhr-1)
        [state-d effects-d]
        (adapter/step
         state-c
         {:event :fragment/retire
          :fragment-id :panel
          :reason :dom-detached})]
    (is (nil? (adapter/fragment-state state-d :panel)))
    (is (empty? (:continuity state-d)))
    (is (= 1 (count (effects-of :htmx/cancel-request effects-d))))
    (is (= 1 (count (effects-of :continuity/release effects-d))))))

(deftest queued-requirements-start-next-generation-after-current-request-finishes-test
  (let [{state-a :state gen-1 :generation}
        (begin-fragment (adapter/initial-state) :panel :basis-1)
        [state-b _] (bind-request state-a :panel gen-1 :xhr-1)
        [state-c _] (adapter/step state-b (invalidation :panel :basis-2))
        [state-d effects-d]
        (adapter/step
         state-c
         {:event :htmx/after-request
          :fragment-id :panel
          :request-generation gen-1
          :request-id :xhr-1})
        refresh (effect-data :fragment/refresh effects-d)
        gen-2 (:request-generation refresh)]
    (is (not= gen-1 gen-2))
    (is (= #{:basis-2} (:requirements refresh)))
    (is (= gen-2
           (get-in state-d [:fragments :panel :inflight :generation])))
    (is (= #{}
           (get-in state-d [:fragments :panel :queued-requirements])))))

(deftest stale-after-request-cannot-retire-newer-request-generation-test
  (let [{state-a :state gen-1 :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel gen-1 :xhr-1)
        [state-c _]
        (adapter/step state-b (invalidation :panel :basis-2))
        [state-d effects-d]
        (adapter/step state-c
                      {:event :htmx/after-request
                       :fragment-id :panel
                       :request-generation gen-1
                       :request-id :xhr-1})
        gen-2 (:request-generation (effect-data :fragment/refresh effects-d))
        [state-e stale-effects]
        (adapter/step state-d
                      {:event :htmx/after-request
                       :fragment-id :panel
                       :request-generation gen-1
                       :request-id :xhr-1})]
    (is (not= gen-1 gen-2))
    (is (= state-d state-e))
    (is (= :stale-after-request
           (ignored-reason stale-effects)))))

;; =============================================================================
;; Counterexample execution support
;; =============================================================================

(deftest steps-retains-event-effect-counterexample-trace-test
  (let [events [(invalidation :panel :basis-1)]
        result (adapter/steps (adapter/initial-state) events)]
    (is (adapter/state? (:state result)))
    (is (= events (mapv :event (:transitions result))))
    (is (= (:effects result)
           (vec (mapcat :effects (:transitions result)))))
    (is (= 1 (count (effects-of :fragment/refresh (:effects result)))))))

(deftest explain-is-pure-compact-and-does-not-expose-machine-host-state-test
  (let [[state _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-once-choreography) :browser)
         :card)
        explained (adapter/explain state)]
    (is (= adapter/adapter-state-type
           (:gesso.live.browser.adapter/type explained)))
    (is (= :card
           (get-in explained [:active-executions "execution-1" :target-id])))
    (is (map? (get-in explained [:active-executions "execution-1" :machine])))
    (is (not (contains? explained :timers)))
    (is (= 0 (:timer-count explained)))))
