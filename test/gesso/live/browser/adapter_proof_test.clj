(ns gesso.live.browser.adapter-proof-test
  "Bounded executable proof checker for the first v4.5 browser adapter.

   This namespace is deliberately stronger than example/property testing and
   deliberately narrower than an unbounded theorem prover.  It mechanically
   explores finite semantic equivalence classes of the pure transition system

     AdapterState x NormalizedEvent -> AdapterState x AbstractEffects

   and checks the initial adapter claims called out by the v4.5 design:

   - fragment request coordination remains bounded;
   - generation-stale execution callbacks are semantic no-ops;
   - execution-private transitions cannot mutate another execution's slice;
   - authoritative installation advances only through the declared equality /
     explicit-progression classes;
   - terminal completion revokes semantic resource ownership immediately;
   - adapter machine-boundary transitions refine the public portable Choreo
     machine operations they delegate to.

   Generation numbers are quotiented by alpha-renaming in the bounded fragment
   exploration.  Concrete generation magnitude has no modeled meaning; only
   identity/equality and freshness matter.  The authoritative-basis matrix uses
   two opaque distinct representatives because the adapter may only inspect
   equality and the exact explicit progression witness.

   This file does NOT claim a proof of HTMX, JavaScript, browser primitives,
   arbitrary application policy, or every unbounded event trace.  The browser
   FFI and supported HTMX lifecycle remain explicit assumptions, and the finite
   exploration depth below is an explicit bound."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]))

(def proof-checker-version 1)
(def fragment-exploration-depth 6)

(def proof-classification
  :bounded-executable-transition-check)

(def proof-nonclaims
  #{:unbounded-event-traces
    :verified-browser-primitives
    :verified-htmx
    :verified-javascript-engine
    :application-business-policy
    :global-choreo-projection-refinement})

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
     (choreo/local
      :browser
      :browser/do-work
      :done
      {:outputs #{:value}})

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

(defn- send-then-local-choreography
  []
  (choreo/->choreography
   {:initial :send
    :states
    {:send
     (choreo/communicate
      :browser
      :server
      :browser/command
      :local
      {:via :http
       :open-payload? true})

     :local
     (choreo/local :browser :browser/after-send :done)

     :done
     (choreo/return :done)}}))

(defn- receive-then-local-choreography
  []
  (choreo/->choreography
   {:initial :receive
    :states
    {:receive
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :local
      {:via :http})

     :local
     (choreo/local :browser :browser/after-receive :done)

     :done
     (choreo/return :done)}}))

(defn- await-then-local-choreography
  []
  (choreo/->choreography
   {:initial :wait
    :states
    {:wait
     (choreo/await
      :browser
      {:browser/timeout :local})

     :local
     (choreo/local :browser :browser/after-timeout :done)

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

;; =============================================================================
;; Small observation helpers
;; =============================================================================

(defn- effects-of
  [kind effects]
  (filterv #(= kind (first %)) effects))

(defn- effect-data
  [kind effects]
  (second (first (effects-of kind effects))))

(defn- machine-record
  [state execution-id]
  (some-> (adapter/execution state execution-id)
          :execution))

(defn- execution-slice
  "Execution-private semantic state used by the isolation checker.

   Global freshness allocation is intentionally excluded: allocating a fresh
   identity for E1 is not a semantic mutation of E2."
  [state execution-id]
  {:execution
   (adapter/execution state execution-id)

   :targets
   (into {}
         (filter
          (fn [[_target-id owner]]
            (= execution-id (:execution-id owner))))
         (:targets state))

   :timers
   (into {}
         (filter
          (fn [[[owner-id _timer-id] _timer]]
            (= execution-id owner-id)))
         (:timers state))})

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

;; =============================================================================
;; Generation alpha-renaming quotient
;; =============================================================================

(defn- state-generation-values
  [state]
  (->> (concat
        [(:next-generation state)]
        (map :generation (vals (:executions state)))
        (keep (comp :generation :pending-effect)
              (vals (:executions state)))
        (map :generation (vals (:targets state)))
        (mapcat
         (fn [[_ timer]]
           [(:execution-generation timer)
            (:generation timer)])
         (:timers state))
        (keep (comp :generation :inflight)
              (vals (:fragments state)))
        (mapcat
         (fn [[_ slot]]
           [(:generation slot)
            (:request-generation slot)])
         (:continuity state)))
       (filter integer?)
       distinct
       sort
       vec))

(defn- generation-renaming
  [state]
  (zipmap
   (state-generation-values state)
   (range 1 (inc (count (state-generation-values state))))))

(defn- rename-generation
  [renaming value]
  (if (integer? value)
    (get renaming value value)
    value))

(defn- canonical-generation-state
  "Alpha-rename all adapter-owned generation identities.

   This quotient is used only for search-state deduplication.  It preserves
   equality/freshness relationships while discarding concrete numeric magnitude."
  [state]
  (let [renaming (generation-renaming state)
        rg #(rename-generation renaming %)]
    (-> state
        (update :next-generation rg)
        (update
         :executions
         (fn [executions]
           (into
            {}
            (map
             (fn [[execution-id record]]
               [execution-id
                (cond-> (update record :generation rg)
                  (:pending-effect record)
                  (update :pending-effect
                          #(update % :generation rg)))])
             executions))))
        (update
         :targets
         (fn [targets]
           (into {}
                 (map
                  (fn [[target-id owner]]
                    [target-id
                     (update owner :generation rg)])
                  targets))))
        (update
         :timers
         (fn [timers]
           (into
            {}
            (map
             (fn [[timer-key timer]]
               [timer-key
                (-> timer
                    (update :execution-generation rg)
                    (update :generation rg))])
             timers))))
        (update
         :fragments
         (fn [fragments]
           (into
            {}
            (map
             (fn [[fragment-id fragment]]
               [fragment-id
                (if (:inflight fragment)
                  (update-in fragment [:inflight :generation] rg)
                  fragment)])
             fragments))))
        (update
         :continuity
         (fn [continuity]
           (into
            {}
            (map
             (fn [[slot-id slot]]
               (let [slot-id'
                     (if (and (vector? slot-id)
                              (= 2 (count slot-id))
                              (integer? (second slot-id)))
                       [(first slot-id) (rg (second slot-id))]
                       slot-id)]
                 [slot-id'
                  (-> slot
                      (assoc :slot-id slot-id')
                      (update :generation rg)
                      (update :request-generation rg))]))
             continuity)))))))

;; =============================================================================
;; Bounded fragment transition-system exploration
;; =============================================================================

(def proof-fragment-id :fragment/proof)
(def proof-request-id :request/current)
(def proof-other-request-id :request/other)

(defn- proof-inflight
  [state]
  (get-in state [:fragments proof-fragment-id :inflight]))

(defn- current-request-event
  [state event-type]
  (let [inflight (proof-inflight state)]
    (when (and inflight (:request-id inflight))
      {:event event-type
       :fragment-id proof-fragment-id
       :request-generation (:generation inflight)
       :request-id (:request-id inflight)})))

(def fragment-abstract-operations
  [[:invalidate-none
    (fn [_state]
      {:event :live/invalidated
       :fragment-id proof-fragment-id})]

   [:invalidate-a
    (fn [_state]
      {:event :live/invalidated
       :fragment-id proof-fragment-id
       :requirement :requirement/a})]

   [:invalidate-b
    (fn [_state]
      {:event :live/invalidated
       :fragment-id proof-fragment-id
       :requirement :requirement/b})]

   [:before-request-current
    (fn [state]
      (when-let [inflight (proof-inflight state)]
        {:event :htmx/before-request
         :fragment-id proof-fragment-id
         :request-generation (:generation inflight)
         :request-id (or (:request-id inflight)
                         proof-request-id)}))]

   [:before-request-competing
    (fn [state]
      (when-let [inflight (proof-inflight state)]
        {:event :htmx/before-request
         :fragment-id proof-fragment-id
         :request-generation (:generation inflight)
         :request-id proof-other-request-id}))]

   [:before-request-stale
    (fn [state]
      {:event :htmx/before-request
       :fragment-id proof-fragment-id
       :request-generation (:next-generation state)
       :request-id :request/stale})]

   [:before-swap-current
    (fn [state]
      (current-request-event state :htmx/before-swap))]

   [:before-swap-stale
    (fn [state]
      {:event :htmx/before-swap
       :fragment-id proof-fragment-id
       :request-generation (:next-generation state)
       :request-id :request/stale})]

   [:after-swap-current
    (fn [state]
      (current-request-event state :htmx/after-swap))]

   [:after-request-current
    (fn [state]
      (current-request-event state :htmx/after-request))]

   [:http-failed-current
    (fn [state]
      (some-> (current-request-event state :http/failed)
              (assoc :reason :network/failure)))]

   [:continuity-current
    (fn [state]
      (when-let [[slot-id slot] (first (:continuity state))]
        {:event :continuity/completed
         :slot-id slot-id
         :slot-generation (:generation slot)}))]

   [:continuity-stale
    (fn [state]
      (if-let [[slot-id _slot] (first (:continuity state))]
        {:event :continuity/completed
         :slot-id slot-id
         :slot-generation (:next-generation state)}
        {:event :continuity/completed
         :slot-id [:fragment/stale 1]
         :slot-generation (:next-generation state)}))]

   [:retire-fragment
    (fn [_state]
      {:event :fragment/retire
       :fragment-id proof-fragment-id
       :reason :proof/retire})]])

(defn- fragment-proof-errors
  [next-state effects]
  (let [fragment (adapter/fragment-state next-state proof-fragment-id)
        active-request-count (if (:inflight fragment) 1 0)
        queued-refresh-count (if (seq (:queued-requirements fragment)) 1 0)
        refresh-effects (effects-of :fragment/refresh effects)]
    (cond-> []
      (seq (adapter/invariant-errors next-state))
      (conj {:kind :adapter-invariant
             :errors (adapter/invariant-errors next-state)})

      (> active-request-count 1)
      (conj {:kind :multiple-active-fragment-requests
             :count active-request-count})

      (> queued-refresh-count 1)
      (conj {:kind :multiple-queued-refreshes
             :count queued-refresh-count})

      (> (+ active-request-count queued-refresh-count) 2)
      (conj {:kind :unbounded-fragment-coordination
             :active active-request-count
             :queued queued-refresh-count})

      (> (count refresh-effects) 1)
      (conj {:kind :multiple-refresh-effects-in-one-transition
             :effects refresh-effects}))))

(defn- explore-fragment-transition-system
  []
  (let [seen (atom #{})
        stats (atom {:states 0
                     :attempts 0
                     :successful-transitions 0
                     :blocked-transitions 0})
        failure (atom nil)]
    (letfn [(visit! [state path remaining]
              (let [key [remaining (canonical-generation-state state)]]
                (when (and (nil? @failure)
                           (not (contains? @seen key)))
                  (swap! seen conj key)
                  (swap! stats update :states inc)
                  (when (pos? remaining)
                    (doseq [[operation resolve-event]
                            fragment-abstract-operations
                            :while (nil? @failure)]
                      (when-let [event (resolve-event state)]
                        (swap! stats update :attempts inc)
                        (try
                          (let [[next-state effects]
                                (adapter/step state event)
                                errors
                                (fragment-proof-errors next-state effects)]
                            (swap! stats update :successful-transitions inc)
                            (if (seq errors)
                              (reset! failure
                                      {:path (conj path operation)
                                       :event event
                                       :errors errors
                                       :state state
                                       :next-state next-state
                                       :effects effects})
                              (visit! next-state
                                      (conj path operation)
                                      (dec remaining))))
                          (catch clojure.lang.ExceptionInfo _blocked
                            ;; Impossible current transitions fail closed; the
                            ;; immutable input state remains the only reachable
                            ;; state from that attempted edge.
                            (swap! stats update :blocked-transitions inc))
                          (catch Throwable unexpected
                            (reset! failure
                                    {:path (conj path operation)
                                     :event event
                                     :unexpected unexpected})))))))))]
      (visit! (adapter/initial-state)
              []
              fragment-exploration-depth)
      (assoc @stats
             :quotient-keys (count @seen)
             :failure @failure))))

;; =============================================================================
;; Fragment / generation / ownership proof checks
;; =============================================================================

(deftest bounded-fragment-transition-system-preserves-coordination-invariants-test
  (let [result (explore-fragment-transition-system)]
    (testing "the finite alpha-renamed transition space has no invariant counterexample"
      (is (nil? (:failure result))
          (pr-str (:failure result))))

    (testing "the checker actually explores a nontrivial state/transition space"
      (is (> (:states result) 40)
          (pr-str result))
      (is (> (:successful-transitions result) 100)
          (pr-str result))
      (is (zero? (:blocked-transitions result))
          (str "All abstract normalized fragment operations should be handled "
               "as transitions/dispositions rather than throwing. "
               (pr-str result))))))

(defn- start-execution
  [state execution-id execution target-id options]
  (adapter/step
   state
   (merge
    {:event :execution/start
     :execution-id execution-id
     :execution execution}
    (when target-id
      {:target-id target-id})
    options)))

(deftest stale-generation-and-cross-execution-isolation-test
  (let [execution-1 :execution/old
        execution-2 :execution/new
        target-id :target/shared
        timer-id :timer/old
        timeout (timeout-envelope)
        [state-a _]
        (start-execution
         (adapter/initial-state)
         execution-1
         (machine-execution (await-once-choreography) :browser)
         target-id
         nil)
        generation-1
        (adapter/execution-generation state-a execution-1)
        [state-b timer-effects]
        (adapter/step
         state-a
         {:event :timer/schedule
          :execution-id execution-1
          :generation generation-1
          :timer-id timer-id
          :delay-ms 10
          :envelope timeout})
        old-timer-generation
        (:timer-generation
         (effect-data :timer/start timer-effects))
        [replacement-state replacement-effects]
        (start-execution
         state-b
         execution-2
         (machine-execution (local-twice-choreography) :browser)
         target-id
         {:replace-owner? true})
        generation-2
        (adapter/execution-generation replacement-state execution-2)
        stale-events
        [{:event :execution/retire
          :execution-id execution-1
          :generation generation-1
          :reason :late}
         {:event :machine/local-completed
          :execution-id execution-1
          :generation generation-1
          :effect-generation 1
          :outputs {}}
         {:event :machine/send-requested
          :execution-id execution-1
          :generation generation-1
          :effect-generation 1
          :payload {}}
         {:event :transport/succeeded
          :execution-id execution-1
          :generation generation-1
          :effect-generation 1}
         {:event :transport/failed
          :execution-id execution-1
          :generation generation-1
          :effect-generation 1
          :reason :late}
         {:event :machine/message
          :execution-id execution-1
          :generation generation-1
          :message-id :message/late
          :envelope (settlement-envelope)}
         {:event :machine/environment
          :execution-id execution-1
          :generation generation-1
          :envelope timeout}
         {:event :machine/retry
          :execution-id execution-1
          :generation generation-1}
         {:event :timer/schedule
          :execution-id execution-1
          :generation generation-1
          :timer-id timer-id
          :delay-ms 1
          :envelope timeout}
         {:event :timer/cancel
          :execution-id execution-1
          :generation generation-1
          :timer-id timer-id}
         {:event :timer/fired
          :execution-id execution-1
          :generation generation-1
          :timer-id timer-id
          :timer-generation old-timer-generation}]]

    (testing "replacement revokes old ownership before physical cleanup"
      (is (= {:execution-id execution-2
              :generation generation-2}
             (adapter/target-owner replacement-state target-id)))
      (is (nil? (adapter/execution replacement-state execution-1)))
      (is (nil? (get-in replacement-state
                        [:timers [execution-1 timer-id]])))
      (is (= 1 (count (effects-of :timer/cancel replacement-effects)))))

    (testing "every execution-scoped late callback from the retired generation is a semantic no-op"
      (doseq [event stale-events]
        (let [[next-state effects]
              (adapter/step replacement-state event)]
          (is (= replacement-state next-state)
              (str "stale event mutated current state: " (pr-str event)))
          (is (= 1 (count (effects-of :diagnostic/ignored effects)))
              (str "stale event was not diagnosed: " (pr-str event))))))

    (testing "a valid transition for one live execution cannot mutate another live execution's private slice"
      (let [[two-execution-state _]
            (start-execution
             replacement-state
             :execution/other
             (machine-execution (await-once-choreography) :browser)
             :target/other
             nil)
            other-before
            (execution-slice two-execution-state :execution/other)
            current-generation
            (adapter/execution-generation two-execution-state execution-2)
            pending-generation
            (get-in two-execution-state
                    [:executions execution-2 :pending-effect :generation])
            [after-local _]
            (adapter/step
             two-execution-state
             {:event :machine/local-completed
              :execution-id execution-2
              :generation current-generation
              :effect-generation pending-generation
              :outputs {}})]
        (is (= other-before
               (execution-slice after-local :execution/other)))))))

;; =============================================================================
;; Authoritative frontier equivalence classes
;; =============================================================================

(def proof-scope :scope/proof)

(defn- begin-bound-fragment-request
  [state request-id]
  (let [[state-a refresh-effects]
        (adapter/step
         state
         {:event :live/invalidated
          :fragment-id proof-fragment-id
          :requirement :proof/refresh})
        generation
        (:request-generation
         (effect-data :fragment/refresh refresh-effects))
        [state-b _]
        (adapter/step
         state-a
         {:event :htmx/before-request
          :fragment-id proof-fragment-id
          :request-generation generation
          :request-id request-id})]
    {:state state-b
     :generation generation
     :request-id request-id}))

(defn- complete-continuity-if-present
  [state fragment-id request-generation]
  (let [slot-id [fragment-id request-generation]
        slot (get-in state [:continuity slot-id])]
    (if slot
      (first
       (adapter/step
        state
        {:event :continuity/completed
         :slot-id slot-id
         :slot-generation (:generation slot)}))
      state)))

(defn- install-authoritative-basis
  [state basis request-id]
  (let [{bound-state :state
         generation :generation}
        (begin-bound-fragment-request state request-id)
        [before-state before-effects]
        (adapter/step
         bound-state
         {:event :htmx/before-swap
          :fragment-id proof-fragment-id
          :request-generation generation
          :request-id request-id
          :authoritative
          {:scope proof-scope
           :basis basis}})
        _
        (when-not (seq (effects-of :htmx/allow-swap before-effects))
          (throw
           (ex-info "Proof fixture could not install initial authoritative basis."
                    {:basis basis
                     :effects before-effects})))
        [after-state _]
        (adapter/step
         before-state
         {:event :htmx/after-swap
          :fragment-id proof-fragment-id
          :request-generation generation
          :request-id request-id})
        after-continuity
        (complete-continuity-if-present
         after-state proof-fragment-id generation)
        [finished-state _]
        (adapter/step
         after-continuity
         {:event :htmx/after-request
          :fragment-id proof-fragment-id
          :request-generation generation
          :request-id request-id})]
    finished-state))

(deftest authoritative-install-monotonicity-equivalence-class-matrix-test
  (testing "with no installed frontier either opaque basis representative is admissible"
    (doseq [basis [:basis/a :basis/b]]
      (let [{state :state generation :generation request-id :request-id}
            (begin-bound-fragment-request
             (adapter/initial-state)
             [:request :initial basis])
            [_ effects]
            (adapter/step
             state
             {:event :htmx/before-swap
              :fragment-id proof-fragment-id
              :request-generation generation
              :request-id request-id
              :authoritative {:scope proof-scope
                              :basis basis}})]
        (is (= 1 (count (effects-of :htmx/allow-swap effects)))))))

  (let [basis-a :basis/a
        basis-b :basis/b
        basis-c :basis/c
        frontier-state
        (install-authoritative-basis
         (adapter/initial-state)
         basis-a
         :request/install-a)
        matrix
        [{:label :same-basis
          :basis basis-a
          :progression nil
          :allowed? true}
         {:label :distinct-no-witness
          :basis basis-b
          :progression nil
          :allowed? false}
         {:label :exact-advance
          :basis basis-b
          :progression {:from basis-a
                        :to basis-b
                        :relation :advances}
          :allowed? true}
         {:label :wrong-from
          :basis basis-b
          :progression {:from basis-c
                        :to basis-b
                        :relation :advances}
          :allowed? false}
         {:label :wrong-to
          :basis basis-b
          :progression {:from basis-a
                        :to basis-c
                        :relation :advances}
          :allowed? false}
         {:label :wrong-relation
          :basis basis-b
          :progression {:from basis-a
                        :to basis-b
                        :relation :equal}
          :allowed? false}
         {:label :extra-witness-key
          :basis basis-b
          :progression {:from basis-a
                        :to basis-b
                        :relation :advances
                        :extra true}
          :allowed? false}]]
    (doseq [{:keys [label basis progression allowed?]} matrix]
      (testing (name label)
        (let [{state :state generation :generation request-id :request-id}
              (begin-bound-fragment-request
               frontier-state
               [:request label])
              candidate
              (cond-> {:scope proof-scope
                       :basis basis}
                progression
                (assoc :progression progression))
              [before-state effects]
              (adapter/step
               state
               {:event :htmx/before-swap
                :fragment-id proof-fragment-id
                :request-generation generation
                :request-id request-id
                :authoritative candidate})]
          (if allowed?
            (do
              (is (= 1 (count (effects-of :htmx/allow-swap effects))))
              (is (empty? (effects-of :htmx/cancel-swap effects)))
              (let [[installed-state install-effects]
                    (adapter/step
                     before-state
                     {:event :htmx/after-swap
                      :fragment-id proof-fragment-id
                      :request-generation generation
                      :request-id request-id})]
                (is (= basis
                       (get-in installed-state
                               [:authoritative proof-scope :basis])))
                (is (= 1
                       (count
                        (effects-of :authoritative/installed
                                    install-effects))))))
            (do
              (is (= state before-state))
              (is (= 1 (count (effects-of :htmx/cancel-swap effects))))
              (is (= basis-a
                     (get-in before-state
                             [:authoritative proof-scope :basis]))))))))))

;; =============================================================================
;; Terminal/resource ownership
;; =============================================================================

(deftest terminal-completion-revokes-semantic-ownership-before-cleanup-test
  (let [execution-id :execution/terminal
        target-id :target/terminal
        timer-a :timer/a
        timer-b :timer/b
        timeout (timeout-envelope)
        [started _]
        (start-execution
         (adapter/initial-state)
         execution-id
         (machine-execution (await-once-choreography) :browser)
         target-id
         nil)
        generation
        (adapter/execution-generation started execution-id)
        [with-a effects-a]
        (adapter/step
         started
         {:event :timer/schedule
          :execution-id execution-id
          :generation generation
          :timer-id timer-a
          :delay-ms 1
          :envelope timeout})
        timer-a-generation
        (:timer-generation (effect-data :timer/start effects-a))
        [with-b _]
        (adapter/step
         with-a
         {:event :timer/schedule
          :execution-id execution-id
          :generation generation
          :timer-id timer-b
          :delay-ms 2
          :envelope timeout})
        [completed effects]
        (adapter/step
         with-b
         {:event :timer/fired
          :execution-id execution-id
          :generation generation
          :timer-id timer-a
          :timer-generation timer-a-generation})]
    (is (nil? (adapter/execution completed execution-id)))
    (is (nil? (adapter/target-owner completed target-id)))
    (is (empty?
         (filter
          (fn [[[owner-id _] _]]
            (= execution-id owner-id))
          (:timers completed))))
    (is (= 1 (count (effects-of :execution/completed effects))))
    (is (= 1 (count (effects-of :timer/cancel effects))))
    (is (= timer-b
           (:timer-id (effect-data :timer/cancel effects))))
    (is (adapter/state? completed))))

;; =============================================================================
;; Adapter -> portable machine refinement checks
;; =============================================================================

(defn- assert-next-local-boundary!
  [state effects execution-id expected-execution]
  (is (= expected-execution
         (machine-record state execution-id)))
  (is (= 1 (count (effects-of :machine/local effects))))
  (is (= (machine/pending-action expected-execution)
         (:action (effect-data :machine/local effects)))))

(deftest adapter-machine-boundary-refinement-test
  (testing "local completion delegates to the portable machine and exposes exactly its next boundary"
    (let [execution-id :execution/local
          direct-0 (machine-execution (local-twice-choreography) :browser)
          [state-a start-effects]
          (start-execution
           (adapter/initial-state)
           execution-id direct-0 nil nil)
          generation (adapter/execution-generation state-a execution-id)
          effect-generation
          (:effect-generation (effect-data :machine/local start-effects))
          direct-1 (machine/complete-local direct-0 {})
          [state-b effects]
          (adapter/step
           state-a
           {:event :machine/local-completed
            :execution-id execution-id
            :generation generation
            :effect-generation effect-generation
            :outputs {}})]
      (assert-next-local-boundary!
       state-b effects execution-id direct-1)))

  (testing "send selection and transport success use pending-message / complete-send without an alternate semantic transition"
    (let [execution-id :execution/send
          payload {:transport/opaque 42}
          direct-0 (machine-execution (send-then-local-choreography) :browser)
          [state-a start-effects]
          (start-execution
           (adapter/initial-state)
           execution-id direct-0 nil nil)
          generation (adapter/execution-generation state-a execution-id)
          selection-generation
          (:effect-generation (effect-data :machine/send start-effects))
          expected-message (machine/pending-message direct-0 payload)
          direct-send (machine/complete-send direct-0 payload)
          direct-1 (:execution direct-send)
          [state-b send-effects]
          (adapter/step
           state-a
           {:event :machine/send-requested
            :execution-id execution-id
            :generation generation
            :effect-generation selection-generation
            :payload payload})
          transport-generation
          (:effect-generation (effect-data :transport/send send-effects))
          [state-c completion-effects]
          (adapter/step
           state-b
           {:event :transport/succeeded
            :execution-id execution-id
            :generation generation
            :effect-generation transport-generation})]
      (is (= expected-message
             (:message (effect-data :transport/send send-effects))))
      (is (= expected-message (:message direct-send)))
      (assert-next-local-boundary!
       state-c completion-effects execution-id direct-1)))

  (testing "participant delivery refines machine/resume"
    (let [execution-id :execution/receive
          envelope (settlement-envelope)
          direct-0 (machine-execution (receive-then-local-choreography) :browser)
          direct-1 (machine/resume direct-0 envelope)
          [state-a start-effects]
          (start-execution
           (adapter/initial-state)
           execution-id direct-0 nil nil)
          generation (adapter/execution-generation state-a execution-id)
          [state-b effects]
          (adapter/step
           state-a
           {:event :machine/message
            :execution-id execution-id
            :generation generation
            :message-id :message/refinement
            :envelope envelope})]
      (is (empty? start-effects))
      (assert-next-local-boundary!
       state-b effects execution-id direct-1)))

  (testing "environment delivery refines machine/resume"
    (let [execution-id :execution/environment
          envelope (timeout-envelope)
          direct-0 (machine-execution (await-then-local-choreography) :browser)
          direct-1 (machine/resume direct-0 envelope)
          [state-a start-effects]
          (start-execution
           (adapter/initial-state)
           execution-id direct-0 nil nil)
          generation (adapter/execution-generation state-a execution-id)
          [state-b effects]
          (adapter/step
           state-a
           {:event :machine/environment
            :execution-id execution-id
            :generation generation
            :envelope envelope})]
      (is (empty? start-effects))
      (assert-next-local-boundary!
       state-b effects execution-id direct-1)))

  (testing "terminal completion reports the exact portable machine result and retains no adapter execution"
    (let [execution-id :execution/terminal-local
          direct-0 (machine-execution (local-once-choreography) :browser)
          [state-a start-effects]
          (start-execution
           (adapter/initial-state)
           execution-id direct-0 nil nil)
          generation (adapter/execution-generation state-a execution-id)
          effect-generation
          (:effect-generation (effect-data :machine/local start-effects))
          direct-1 (machine/complete-local direct-0 {:value 42})
          [state-b effects]
          (adapter/step
           state-a
           {:event :machine/local-completed
            :execution-id execution-id
            :generation generation
            :effect-generation effect-generation
            :outputs {:value 42}})]
      (is (machine/completed? direct-1))
      (is (nil? (adapter/execution state-b execution-id)))
      (is (= (machine/result direct-1)
             (:result (effect-data :execution/completed effects))))))

  (testing "the browser adapter refuses to realize a portable authoritative boundary"
    (let [data
          (thrown-data
           #(start-execution
             (adapter/initial-state)
             :execution/forbidden-authority
             (machine-execution
              (browser-authoritative-choreography)
              :browser)
             nil
             nil))]
      (is (= :browser-authoritative-boundary
             (:error/kind data))))))

(deftest proof-checker-classification-is-explicit-test
  (is (= 1 proof-checker-version))
  (is (= :bounded-executable-transition-check
         proof-classification))
  (is (= 6 fragment-exploration-depth))
  (is (= #{:unbounded-event-traces
           :verified-browser-primitives
           :verified-htmx
           :verified-javascript-engine
           :application-business-policy
           :global-choreo-projection-refinement}
         proof-nonclaims)))
