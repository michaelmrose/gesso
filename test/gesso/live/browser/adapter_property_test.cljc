(ns gesso.live.browser.adapter-property-test
  "Generated/model-based tests for the pure Gesso Live browser adapter.

   The authoritative browser design calls for adversarial event orderings to be
   exercised while the transition semantics are still changing. These tests are
   intentionally complementary to the hand-selected acceptance cases in
   adapter-test and to the later mechanical verification work.

   The system under test remains:

     AdapterState x NormalizedEvent -> AdapterState x AbstractEffects

   The generators operate on abstract browser intentions. Each intention is
   resolved against the current pure adapter state into a normalized event. This
   makes generated runs useful rather than mostly malformed noise: they contain
   current callbacks, stale callbacks, replacements, duplicate deliveries,
   invalidations, request lifecycle events, timer events, continuity callbacks,
   and failures in adversarial order.

   ExceptionInfo from an impossible current transition is treated as fail-closed
   behavior for the generated runner: the runner continues from the exact input
   state and keeps checking invariants. Any other throwable escapes and fails the
   property.

   Passing these tests is not a proof. Counterexamples are intended to refine the
   adapter before the bounded/mechanical verification phase."
  (:require
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])))

;; =============================================================================
;; Choreo fixtures
;; =============================================================================

(defn- projected
  [choreography role]
  (project/project choreography role))

(defn- machine-execution
  [choreography role]
  (machine/start (projected choreography role)))

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

(defn- scenario-execution
  [scenario]
  (machine-execution
   (case (mod scenario 4)
     0 (local-twice-choreography)
     1 (send-once-choreography)
     2 (receive-twice-choreography)
     3 (await-once-choreography))
   :browser))

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

;; =============================================================================
;; Test.check helpers
;; =============================================================================

(def property-test-count
  100)

(def mixed-property-test-count
  150)

(defn- check-property!
  [num-tests property]
  (let [result (tc/quick-check num-tests property)]
    (is (= true (:result result))
        (str "Generated browser-adapter property failed.\n"
             (pr-str result)))
    result))

(def small-index-gen
  (gen/choose 0 3))

(def small-natural-gen
  (gen/choose 0 50))

(def operation-kind-gen
  (gen/frequency
   [[5 (gen/return :execution/start)]
    [2 (gen/return :execution/retire)]
    [4 (gen/return :machine/local-completed)]
    [3 (gen/return :machine/send-requested)]
    [2 (gen/return :transport/succeeded)]
    [2 (gen/return :transport/failed)]
    [3 (gen/return :machine/message)]
    [2 (gen/return :machine/environment)]
    [2 (gen/return :machine/retry)]
    [3 (gen/return :timer/schedule)]
    [2 (gen/return :timer/cancel)]
    [3 (gen/return :timer/fired)]
    [5 (gen/return :live/invalidated)]
    [1 (gen/return :fragment/retire)]
    [3 (gen/return :htmx/before-request)]
    [3 (gen/return :htmx/before-swap)]
    [2 (gen/return :htmx/after-swap)]
    [2 (gen/return :htmx/after-request)]
    [2 (gen/return :http/failed)]
    [2 (gen/return :continuity/completed)]]))

(def operation-gen
  (gen/let [op operation-kind-gen
            a small-index-gen
            b small-index-gen
            n small-natural-gen
            flag-a gen/boolean
            flag-b gen/boolean
            stale? gen/boolean]
    {:op op
     :a a
     :b b
     :n n
     :flag-a flag-a
     :flag-b flag-b
     :stale? stale?}))

(def mixed-script-gen
  (gen/vector operation-gen 1 80))

(def requirement-burst-gen
  (gen/vector small-natural-gen 1 40))

;; =============================================================================
;; Pure identifier / state helpers
;; =============================================================================

(defn- execution-id
  [index]
  (str "execution-" index))

(defn- target-id
  [index]
  [:target index])

(defn- timer-id
  [index]
  [:timer index])

(defn- fragment-id
  [index]
  [:fragment index])

(defn- request-id
  [index]
  [:request index])

(defn- message-id
  [index]
  [:message index])

(defn- scope-id
  [index]
  [:scope index])

(defn- basis-id
  [index]
  [:basis index])

(defn- stale-generation
  [state]
  (+ 1000 (:next-generation state)))

(defn- choose-generation
  [state current stale?]
  (if stale?
    (stale-generation state)
    (or current
        (stale-generation state))))

(defn- pending-generation
  [record kind]
  (when (= kind (get-in record [:pending-effect :kind]))
    (get-in record [:pending-effect :generation])))

(defn- fragment-inflight
  [state fragment-id]
  (get-in state [:fragments fragment-id :inflight]))

(defn- continuity-slot-for-fragment
  [state fragment-id]
  (some
   (fn [[slot-id slot]]
     (when (= fragment-id (:fragment-id slot))
       [slot-id slot]))
   (:continuity state)))

(defn- known-effect?
  [value]
  (and (vector? value)
       (= 2 (count value))
       (contains? adapter/abstract-effect-kinds (first value))
       (map? (second value))))

(defn- allocated-generations
  [state]
  (concat
   (map :generation (vals (:executions state)))
   (keep #(get-in % [:pending-effect :generation])
         (vals (:executions state)))
   (map :generation (vals (:targets state)))
   (map :generation (vals (:timers state)))
   (map :execution-generation (vals (:timers state)))
   (keep #(get-in % [:inflight :generation])
         (vals (:fragments state)))
   (map :generation (vals (:continuity state)))
   (map :request-generation (vals (:continuity state)))))

(defn- generations-below-next?
  [state]
  (let [next-generation (:next-generation state)]
    (every?
     (fn [generation]
       (and (integer? generation)
            (pos? generation)
            (< generation next-generation)))
     (allocated-generations state))))

(defn- transition-result-valid?
  [{:keys [before after effects error]}]
  (and
   (adapter/state? before)
   (adapter/state? after)
   (empty? (adapter/invariant-errors after))
   (generations-below-next? after)
   (if error
     (= before after)
     (and
      (<= (:next-generation before)
          (:next-generation after))
      (every? known-effect? effects)))))

;; =============================================================================
;; State-aware generated event resolution
;; =============================================================================

(defn- authoritative-candidate
  [state fragment-index token]
  (let [scope (scope-id fragment-index)
        basis (basis-id (:n token))
        current-basis (get-in state [:authoritative scope :basis])]
    (when (:flag-a token)
      (cond->
       {:scope scope
        :basis basis}
        (:flag-b token)
        (assoc
         :progression
         {:from (if (:stale? token)
                  [:basis :wrong]
                  (or current-basis
                      [:basis :uninstalled]))
          :to basis
          :relation :advances})))))

(defn- resolve-operation
  [state {:keys [op a b n flag-a flag-b stale?] :as token}]
  (let [execution-id' (execution-id a)
        record (adapter/execution state execution-id')
        execution-generation (:generation record)
        selected-execution-generation
        (choose-generation state execution-generation stale?)
        fragment-id' (fragment-id a)
        inflight (fragment-inflight state fragment-id')
        request-generation (:generation inflight)
        selected-request-generation
        (choose-generation state request-generation stale?)
        generated-request-id (request-id n)
        selected-request-id
        (if (and (not stale?)
                 flag-a
                 (:request-id inflight))
          (:request-id inflight)
          generated-request-id)]
    (case op
      :execution/start
      {:event :execution/start
       :execution-id execution-id'
       :execution (scenario-execution n)
       :target-id (target-id b)
       :replace-owner? flag-a
       :replace-execution? flag-b}

      :execution/retire
      {:event :execution/retire
       :execution-id execution-id'
       :generation selected-execution-generation
       :reason :generated-retirement}

      :machine/local-completed
      {:event :machine/local-completed
       :execution-id execution-id'
       :generation selected-execution-generation
       :effect-generation
       (choose-generation
        state
        (pending-generation record :local)
        stale?)
       :outputs {}}

      :machine/send-requested
      {:event :machine/send-requested
       :execution-id execution-id'
       :generation selected-execution-generation
       :effect-generation
       (choose-generation
        state
        (pending-generation record :send-payload)
        stale?)
       :payload {:generated/value n}}

      :transport/succeeded
      {:event :transport/succeeded
       :execution-id execution-id'
       :generation selected-execution-generation
       :effect-generation
       (choose-generation
        state
        (pending-generation record :transport)
        stale?)}

      :transport/failed
      {:event :transport/failed
       :execution-id execution-id'
       :generation selected-execution-generation
       :effect-generation
       (choose-generation
        state
        (pending-generation record :transport)
        stale?)
       :reason :generated-transport-failure}

      :machine/message
      {:event :machine/message
       :execution-id execution-id'
       :generation selected-execution-generation
       :message-id (message-id n)
       :envelope (settlement-envelope)}

      :machine/environment
      {:event :machine/environment
       :execution-id execution-id'
       :generation selected-execution-generation
       :envelope (timeout-envelope)}

      :machine/retry
      {:event :machine/retry
       :execution-id execution-id'
       :generation selected-execution-generation}

      :timer/schedule
      {:event :timer/schedule
       :execution-id execution-id'
       :generation selected-execution-generation
       :timer-id (timer-id b)
       :delay-ms n
       :envelope (timeout-envelope)}

      :timer/cancel
      {:event :timer/cancel
       :execution-id execution-id'
       :generation selected-execution-generation
       :timer-id (timer-id b)}

      :timer/fired
      (let [timer (get-in state [:timers [execution-id' (timer-id b)]])]
        {:event :timer/fired
         :execution-id execution-id'
         :generation selected-execution-generation
         :timer-id (timer-id b)
         :timer-generation
         (choose-generation state (:generation timer) stale?)})

      :live/invalidated
      (cond->
       {:event :live/invalidated
        :fragment-id fragment-id'}
        flag-a
        (assoc :requirement (basis-id n)))

      :fragment/retire
      {:event :fragment/retire
       :fragment-id fragment-id'
       :reason :generated-fragment-retirement}

      :htmx/before-request
      {:event :htmx/before-request
       :fragment-id fragment-id'
       :request-generation selected-request-generation
       :request-id selected-request-id}

      :htmx/before-swap
      (cond->
       {:event :htmx/before-swap
        :fragment-id fragment-id'
        :request-generation selected-request-generation
        :request-id selected-request-id}
        (:flag-a token)
        (assoc :authoritative
               (authoritative-candidate state a token)))

      :htmx/after-swap
      {:event :htmx/after-swap
       :fragment-id fragment-id'
       :request-generation selected-request-generation
       :request-id selected-request-id}

      :htmx/after-request
      {:event :htmx/after-request
       :fragment-id fragment-id'
       :request-generation selected-request-generation
       :request-id selected-request-id}

      :http/failed
      {:event :http/failed
       :fragment-id fragment-id'
       :request-generation selected-request-generation
       :request-id selected-request-id
       :reason :generated-http-failure}

      :continuity/completed
      (let [[slot-id slot]
            (or (continuity-slot-for-fragment state fragment-id')
                [[fragment-id' selected-request-generation] nil])]
        {:event :continuity/completed
         :slot-id slot-id
         :slot-generation
         (choose-generation state (:generation slot) stale?)}))))

(defn- attempt-step
  [state event]
  (try
    (let [[next-state effects]
          (adapter/step state event)]
      {:before state
       :event event
       :after next-state
       :effects effects
       :error nil})
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) error
      {:before state
       :event event
       :after state
       :effects []
       :error error})))

(defn- run-generated-script
  [operations]
  (reduce
   (fn [{:keys [state transitions valid?]} operation]
     (let [event (resolve-operation state operation)
           transition (attempt-step state event)]
       {:state (:after transition)
        :transitions (conj transitions
                           (assoc transition
                                  :operation operation))
        :valid? (and valid?
                     (transition-result-valid? transition))}))
   {:state (adapter/initial-state)
    :transitions []
    :valid? true}
   operations))

;; =============================================================================
;; Generated mixed transition model
;; =============================================================================

(deftest generated-adversarial-orderings-preserve-adapter-invariants-test
  (testing "mixed current/stale browser events preserve declared invariants after every attempted transition"
    (check-property!
     mixed-property-test-count
     (prop/for-all*
      [mixed-script-gen]
      (fn [operations]
        (let [{:keys [state transitions valid?]}
              (run-generated-script operations)]
          (and valid?
               (adapter/state? state)
               (every? transition-result-valid?
                       transitions))))))))

;; =============================================================================
;; Stale-generation noninterference
;; =============================================================================

(defn- local-start
  [execution-id target-id]
  {:event :execution/start
   :execution-id execution-id
   :execution (machine-execution (local-once-choreography) :browser)
   :target-id target-id})

(defn- effects-by-kind
  [kind effects]
  (filterv #(= kind (first %)) effects))

(defn- effect-data
  [kind effects]
  (second (first (effects-by-kind kind effects))))

(deftest generated-stale-execution-callbacks-are-semantic-noops-test
  (testing "inserting stale callbacks does not alter the clean semantic trace or final state"
    (check-property!
     property-test-count
     (prop/for-all*
      [(gen/vector small-natural-gen 0 40)]
      (fn [noise]
        (let [execution-id "execution-stale-noise"
              start-event (local-start execution-id :target/stale-noise)
              [started start-effects]
              (adapter/step (adapter/initial-state) start-event)
              generation (adapter/execution-generation started execution-id)
              effect-generation
              (:effect-generation
               (effect-data :machine/local start-effects))
              completion
              {:event :machine/local-completed
               :execution-id execution-id
               :generation generation
               :effect-generation effect-generation
               :outputs {}}
              clean
              (adapter/steps
               (adapter/initial-state)
               [start-event completion])
              stale-generation'
              (+ 1000 (:next-generation started))
              stale-events
              (mapcat
               (fn [n]
                 [{:event :machine/local-completed
                   :execution-id execution-id
                   :generation stale-generation'
                   :effect-generation (+ stale-generation' n 1)
                   :outputs {}}
                  {:event :machine/message
                   :execution-id execution-id
                   :generation stale-generation'
                   :message-id (message-id n)
                   :envelope (settlement-envelope)}
                  {:event :machine/environment
                   :execution-id execution-id
                   :generation stale-generation'
                   :envelope (timeout-envelope)}
                  {:event :timer/fired
                   :execution-id execution-id
                   :generation stale-generation'
                   :timer-id (timer-id (mod n 4))
                   :timer-generation (+ stale-generation' n 2)}])
               noise)
              noisy
              (adapter/steps
               (adapter/initial-state)
               (into [start-event]
                     (concat stale-events
                             [completion])))]
          (and
           (= (:state clean)
              (:state noisy))
           (= (adapter/semantic-effects (:effects clean))
              (adapter/semantic-effects (:effects noisy)))
           (adapter/state? (:state noisy)))))))))

;; =============================================================================
;; Bounded fragment coordination
;; =============================================================================

(defn- invalidate-with-requirement
  [fragment-id requirement]
  {:event :live/invalidated
   :fragment-id fragment-id
   :requirement requirement})

(deftest generated-invalidation-bursts-remain-singleflight-and-bounded-test
  (testing "one active refresh absorbs an arbitrary burst into one queued requirement set"
    (check-property!
     property-test-count
     (prop/for-all*
      [requirement-burst-gen]
      (fn [raw-requirements]
        (let [fragment-id :fragment/generated-burst
              requirements (mapv basis-id raw-requirements)
              first-requirement (first requirements)
              remaining (rest requirements)
              [state-a first-effects]
              (adapter/step
               (adapter/initial-state)
               (invalidate-with-requirement
                fragment-id
                first-requirement))
              first-refresh
              (effect-data :fragment/refresh first-effects)
              {:keys [state effects-valid?]}
              (reduce
               (fn [{:keys [state effects-valid?]} requirement]
                 (let [[state' effects]
                       (adapter/step
                        state
                        (invalidate-with-requirement
                         fragment-id
                         requirement))]
                   {:state state'
                    :effects-valid?
                    (and effects-valid?
                         (empty? (effects-by-kind
                                  :fragment/refresh
                                  effects)))}))
               {:state state-a
                :effects-valid? true}
               remaining)
              inflight (get-in state [:fragments fragment-id :inflight])
              queued (get-in state [:fragments fragment-id :queued-requirements])
              generation (:generation inflight)
              request-id :request/generated-burst
              [state-b _]
              (adapter/step
               state
               {:event :htmx/before-request
                :fragment-id fragment-id
                :request-generation generation
                :request-id request-id})
              [state-c finish-effects]
              (adapter/step
               state-b
               {:event :htmx/after-request
                :fragment-id fragment-id
                :request-generation generation
                :request-id request-id})
              next-refresh
              (effect-data :fragment/refresh finish-effects)
              expected-queued (set remaining)]
          (and
           (= #{first-requirement}
              (:requirements first-refresh))
           (= #{first-requirement}
              (:requirements inflight))
           (= expected-queued queued)
           effects-valid?
           (<= (count (effects-by-kind
                       :fragment/refresh
                       finish-effects))
               1)
           (if (seq expected-queued)
             (and next-refresh
                  (= expected-queued
                     (:requirements next-refresh))
                  (not= generation
                        (:request-generation next-refresh))
                  (= (:request-generation next-refresh)
                     (get-in state-c
                             [:fragments fragment-id
                              :inflight :generation])))
             (and (nil? next-refresh)
                  (nil? (get-in state-c
                                [:fragments fragment-id :inflight]))))
           (adapter/state? state-c))))))))

;; =============================================================================
;; Authoritative-install monotonicity
;; =============================================================================

(defn- begin-bound-request
  [state fragment-id request-id]
  (let [[state-a refresh-effects]
        (adapter/step
         state
         {:event :live/invalidated
          :fragment-id fragment-id})
        generation
        (:request-generation
         (effect-data :fragment/refresh refresh-effects))
        [state-b _]
        (adapter/step
         state-a
         {:event :htmx/before-request
          :fragment-id fragment-id
          :request-generation generation
          :request-id request-id})]
    {:state state-b
     :generation generation}))

(defn- finish-bound-request
  [state fragment-id generation request-id]
  (first
   (adapter/step
    state
    {:event :htmx/after-request
     :fragment-id fragment-id
     :request-generation generation
     :request-id request-id})))

(deftest generated-authoritative-frontier-requires-explicit-exact-progression-test
  (testing "a distinct installed basis cannot advance without an exact from/to :advances witness"
    (check-property!
     property-test-count
     (prop/for-all*
      [small-natural-gen small-natural-gen]
      (fn [a b]
        (let [b (if (= a b) (inc b) b)
              fragment-id :fragment/authority
              scope :scope/authority
              basis-a (basis-id a)
              basis-b (basis-id b)
              request-a :request/authority-a
              request-b :request/authority-b
              {state-0 :state
               generation-a :generation}
              (begin-bound-request
               (adapter/initial-state)
               fragment-id
               request-a)
              [state-a before-a-effects]
              (adapter/step
               state-0
               {:event :htmx/before-swap
                :fragment-id fragment-id
                :request-generation generation-a
                :request-id request-a
                :authoritative
                {:scope scope
                 :basis basis-a}})
              [state-b after-a-effects]
              (adapter/step
               state-a
               {:event :htmx/after-swap
                :fragment-id fragment-id
                :request-generation generation-a
                :request-id request-a})
              state-c
              (finish-bound-request
               state-b fragment-id generation-a request-a)
              {state-d :state
               generation-b :generation}
              (begin-bound-request
               state-c fragment-id request-b)
              [rejected rejected-effects]
              (adapter/step
               state-d
               {:event :htmx/before-swap
                :fragment-id fragment-id
                :request-generation generation-b
                :request-id request-b
                :authoritative
                {:scope scope
                 :basis basis-b}})
              [accepted accepted-effects]
              (adapter/step
               rejected
               {:event :htmx/before-swap
                :fragment-id fragment-id
                :request-generation generation-b
                :request-id request-b
                :authoritative
                {:scope scope
                 :basis basis-b
                 :progression
                 {:from basis-a
                  :to basis-b
                  :relation :advances}}})
              [installed install-effects]
              (adapter/step
               accepted
               {:event :htmx/after-swap
                :fragment-id fragment-id
                :request-generation generation-b
                :request-id request-b})]
          (and
           (seq (effects-by-kind :htmx/allow-swap before-a-effects))
           (= basis-a
              (get-in state-b [:authoritative scope :basis]))
           (seq (effects-by-kind :authoritative/installed after-a-effects))
           (= rejected state-d)
           (seq (effects-by-kind :htmx/cancel-swap rejected-effects))
           (= :non-monotone-authoritative-install
              (get-in (effect-data :htmx/cancel-swap rejected-effects)
                      [:reason]))
           (seq (effects-by-kind :htmx/allow-swap accepted-effects))
           (= basis-b
              (get-in installed [:authoritative scope :basis]))
           (seq (effects-by-kind :authoritative/installed install-effects))
           (adapter/state? installed))))))))

;; =============================================================================
;; Terminal retirement / resource ownership
;; =============================================================================

(deftest generated-terminal-local-completion-retires-all-execution-ownership-test
  (testing "terminal completion removes execution, target ownership, and execution-owned timers"
    (check-property!
     property-test-count
     (prop/for-all*
      [small-index-gen small-index-gen small-natural-gen]
      (fn [execution-index target-index noise]
        (let [execution-id (str "terminal-" execution-index "-" noise)
              target-id' (target-id target-index)
              [started start-effects]
              (adapter/step
               (adapter/initial-state)
               {:event :execution/start
                :execution-id execution-id
                :execution
                (machine-execution
                 (local-once-choreography)
                 :browser)
                :target-id target-id'})
              generation
              (adapter/execution-generation started execution-id)
              effect-generation
              (:effect-generation
               (effect-data :machine/local start-effects))
              [completed completion-effects]
              (adapter/step
               started
               {:event :machine/local-completed
                :execution-id execution-id
                :generation generation
                :effect-generation effect-generation
                :outputs {}})]
          (and
           (nil? (adapter/execution completed execution-id))
           (nil? (adapter/target-owner completed target-id'))
           (not-any?
            (fn [[[owner-id _timer-id] _timer]]
              (= execution-id owner-id))
            (:timers completed))
           (= 1 (count (effects-by-kind
                        :execution/completed
                        completion-effects)))
           (adapter/state? completed))))))))

(deftest generated-terminal-timer-completion-retires-execution-and-timer-test
  (testing "a current terminal timer fire consumes the timer and retires its execution"
    (check-property!
     property-test-count
     (prop/for-all*
      [small-index-gen small-index-gen small-natural-gen]
      (fn [execution-index timer-index delay-ms]
        (let [execution-id (str "timer-terminal-" execution-index)
              timer-id' (timer-id timer-index)
              [started _]
              (adapter/step
               (adapter/initial-state)
               {:event :execution/start
                :execution-id execution-id
                :execution
                (machine-execution
                 (await-once-choreography)
                 :browser)})
              generation
              (adapter/execution-generation started execution-id)
              [scheduled schedule-effects]
              (adapter/step
               started
               {:event :timer/schedule
                :execution-id execution-id
                :generation generation
                :timer-id timer-id'
                :delay-ms delay-ms
                :envelope (timeout-envelope)})
              timer-generation
              (:timer-generation
               (effect-data :timer/start schedule-effects))
              [completed fire-effects]
              (adapter/step
               scheduled
               {:event :timer/fired
                :execution-id execution-id
                :generation generation
                :timer-id timer-id'
                :timer-generation timer-generation})]
          (and
           (nil? (adapter/execution completed execution-id))
           (nil? (get-in completed [:timers [execution-id timer-id']]))
           (= 1 (count (effects-by-kind
                        :execution/completed
                        fire-effects)))
           (adapter/state? completed))))))))

;; =============================================================================
;; Generated diagnostic/stale noise around fragment lifecycle
;; =============================================================================

(deftest generated-stale-fragment-callbacks-cannot-retire-current-generation-test
  (testing "arbitrary stale request/continuity callbacks leave the current fragment generation intact"
    (check-property!
     property-test-count
     (prop/for-all*
      [(gen/vector small-natural-gen 0 40)]
      (fn [noise]
        (let [fragment-id :fragment/stale-noise
              request-id :request/current
              {:keys [state generation]}
              (begin-bound-request
               (adapter/initial-state)
               fragment-id
               request-id)
              [state-with-slot _]
              (adapter/step
               state
               {:event :htmx/before-swap
                :fragment-id fragment-id
                :request-generation generation
                :request-id request-id})
              slot-id [fragment-id generation]
              slot-generation
              (get-in state-with-slot
                      [:continuity slot-id :generation])
              final
              (reduce
               (fn [current n]
                 (let [stale-generation'
                       (+ 1000 (:next-generation current) n)
                       stale-events
                       [{:event :htmx/after-swap
                         :fragment-id fragment-id
                         :request-generation stale-generation'
                         :request-id [:request :stale n]}
                        {:event :htmx/after-request
                         :fragment-id fragment-id
                         :request-generation stale-generation'
                         :request-id [:request :stale n]}
                        {:event :http/failed
                         :fragment-id fragment-id
                         :request-generation stale-generation'
                         :request-id [:request :stale n]
                         :reason :stale}
                        {:event :continuity/completed
                         :slot-id slot-id
                         :slot-generation (+ slot-generation n 1)}]]
                   (reduce
                    (fn [state event]
                      (first (adapter/step state event)))
                    current
                    stale-events)))
               state-with-slot
               noise)]
          (and
           (= generation
              (get-in final
                      [:fragments fragment-id
                       :inflight :generation]))
           (= request-id
              (get-in final
                      [:fragments fragment-id
                       :inflight :request-id]))
           (= slot-generation
              (get-in final
                      [:continuity slot-id :generation]))
           (adapter/state? final))))))))
