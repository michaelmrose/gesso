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


(defn- receive-once-choreography
  []
  (choreo/->choreography
   {:initial :receive
    :states
    {:receive
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :done
      {:via :http})

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
;; Adapter-owned optimistic effect scopes
;; =============================================================================

(def ^:private provisional-key
  :gesso.live.optimistic/provisional)

(def ^:private provisional-value
  {:authority :provisional
   :projection {:status :pending}})

(defn- optimistic-config
  ([rollback-eligible?]
   (optimistic-config rollback-eligible? 15000))
  ([rollback-eligible? timeout-ms]
   {:command-id :command-1
    :provisional-key provisional-key
    :rollback-eligible? rollback-eligible?
    :timeout-ms timeout-ms}))

(defn- optimistic-receive-choreography
  []
  (choreo/->choreography
   {:initial :local
    :states
    {:local
     (choreo/local
      :browser
      :browser/derive-provisional
      :receive
      {:outputs #{provisional-key}})

     :receive
     (choreo/communicate
      :server
      :browser
      :server/settlement
      :done
      {:via :http})

     :done
     (choreo/return :done)}}))

(defn- local-then-send-choreography
  []
  (choreo/->choreography
   {:initial :local
    :states
    {:local
     (choreo/local
      :browser
      :browser/derive-provisional
      :send
      {:outputs #{provisional-key}})

     :send
     (choreo/communicate
      :browser
      :server
      :browser/command
      :done
      {:via :http
       :open-payload? true})

     :done
     (choreo/return :done)}}))

(defn- pending-local-generation
  [state execution-id]
  (get-in (adapter/execution state execution-id)
          [:pending-effect :generation]))

(defn- establish-provisional
  ([state execution-id]
   (establish-provisional state execution-id provisional-value))
  ([state execution-id provisional]
   (adapter/step
    state
    {:event :machine/local-completed
     :execution-id execution-id
     :generation (adapter/execution-generation state execution-id)
     :effect-generation (pending-local-generation state execution-id)
     :outputs {provisional-key provisional}})))

(deftest optimistic-start-awaits-derived-provisional-before-install-and-timeout-test
  (let [[state-a effects-a]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true 2500)})
        generation (adapter/execution-generation state-a "execution-1")
        scope-a (adapter/optimistic-scope state-a "execution-1")
        [state-b effects-b] (establish-provisional state-a "execution-1")
        scope-b (adapter/optimistic-scope state-b "execution-1")
        install (effect-data :optimistic/install-provisional effects-b)
        timeout (effect-data :optimistic/timeout-start effects-b)]
    (testing "start establishes semantic ownership but no physical optimism yet"
      (is (= [:machine/local] (mapv first effects-a)))
      (is (= {:execution-id "execution-1"
              :generation generation}
             (adapter/target-owner state-a :card)))
      (is (= generation (:execution-generation scope-a)))
      (is (= :command-1 (:command-id scope-a)))
      (is (= :card (:target-id scope-a)))
      (is (= provisional-key (:provisional-key scope-a)))
      (is (= :awaiting-provisional (:status scope-a)))
      (is (nil? (:provisional scope-a)))
      (is (nil? (:timeout-generation scope-a)))
      (is (= 2500 (:timeout-ms scope-a))))

    (testing "derived semantic provisional is installed before timeout ownership begins"
      (is (= [:optimistic/install-provisional
              :optimistic/timeout-start]
             (mapv first effects-b)))
      (is (= :provisional (:status scope-b)))
      (is (= provisional-value (:provisional scope-b)))
      (is (= (:timeout-generation scope-b)
             (:timeout-generation timeout)))
      (is (= generation (:generation install)))
      (is (= provisional-value (:provisional install)))
      (is (= 2500 (:delay-ms timeout))))))

(deftest optimistic-start-requires-one-logical-target-test
  (is (= :optimistic-target-required
         (error-kind
          #(start
            (adapter/initial-state)
            "execution-1"
            (machine-execution (optimistic-receive-choreography) :browser)
            nil
            {:optimistic (optimistic-config true)})))))

(deftest optimistic-start-rejects-precomputed-provisional-input-test
  (is (= :missing-optimistic-fields
         (error-kind
          #(start
            (adapter/initial-state)
            "execution-1"
            (machine-execution (optimistic-receive-choreography) :browser)
            :card
            {:optimistic
             {:command-id :command-1
              :provisional provisional-value
              :rollback-eligible? true
              :timeout-ms 100}}))))
  (is (= :unknown-optimistic-fields
         (error-kind
          #(start
            (adapter/initial-state)
            "execution-1"
            (machine-execution (optimistic-receive-choreography) :browser)
            :card
            {:optimistic
             {:command-id :command-1
              :provisional-key provisional-key
              :provisional provisional-value
              :rollback-eligible? true
              :timeout-ms 100}})))))

(deftest optimistic-derived-provisional-must-be-a-map-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})]
    (is (= :missing-derived-provisional
           (error-kind
            #(establish-provisional state-a "execution-1" nil))))
    (is (= :missing-derived-provisional
           (error-kind
            #(establish-provisional state-a "execution-1" :not-a-map))))))

(deftest optimistic-settlement-cancels-timeout-and-is-idempotent-only-for-identical-observation-test
  (let [[state-start _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        [state-a _] (establish-provisional state-start "execution-1")
        generation (adapter/execution-generation state-a "execution-1")
        timeout-generation (:timeout-generation
                            (adapter/optimistic-scope state-a "execution-1"))
        settlement {:resolution :confirmed
                    :authoritative {:presence :present
                                    :basis :basis-2}}
        event {:event :optimistic/settlement-observed
               :execution-id "execution-1"
               :generation generation
               :resolution :confirmed
               :settlement settlement}
        [state-b effects-b] (adapter/step state-a event)
        scope-b (adapter/optimistic-scope state-b "execution-1")
        [state-c effects-c] (adapter/step state-b event)]
    (is (= [:optimistic/timeout-cancel]
           (mapv first effects-b)))
    (is (= timeout-generation
           (:timeout-generation
            (effect-data :optimistic/timeout-cancel effects-b))))
    (is (= :settlement-observed (:status scope-b)))
    (is (= :confirmed (:resolution scope-b)))
    (is (= settlement (:settlement scope-b)))
    (is (nil? (:timeout-generation scope-b)))
    (is (= state-b state-c))
    (is (= :duplicate-optimistic-settlement
           (ignored-reason effects-c)))
    (is (= :conflicting-optimistic-settlement
           (error-kind
            #(adapter/step
              state-b
              (assoc event
                     :settlement (assoc settlement :extra :different))))))))

(deftest successful-optimistic-completion-awaits-authority-test
  (doseq [resolution [:confirmed :reconciled :already-incorporated]]
    (testing (name resolution)
      (let [[state-start _]
            (start
             (adapter/initial-state)
             "execution-1"
             (machine-execution (optimistic-receive-choreography) :browser)
             :card
             {:optimistic (optimistic-config true)})
            [state-a _] (establish-provisional state-start "execution-1")
            generation (adapter/execution-generation state-a "execution-1")
            settlement {:resolution resolution
                        :authoritative {:presence :present
                                        :basis :basis-2}}
            [state-b _]
            (adapter/step
             state-a
             {:event :optimistic/settlement-observed
              :execution-id "execution-1"
              :generation generation
              :resolution resolution
              :settlement settlement})
            [state-c effects-c]
            (adapter/step
             state-b
             {:event :machine/message
              :execution-id "execution-1"
              :generation generation
              :message-id :settlement-1
              :envelope (settlement-envelope)})
            finish (effect-data :optimistic/finish effects-c)]
        (is (nil? (adapter/execution state-c "execution-1")))
        (is (nil? (adapter/optimistic-scope state-c "execution-1")))
        (is (nil? (adapter/target-owner state-c :card)))
        (is (= [:optimistic/finish :execution/completed]
               (mapv first effects-c)))
        (is (= resolution (:resolution finish)))
        (is (= :await-authority (:disposition finish)))
        (is (= settlement (:settlement finish)))
        (is (= 0 (count (effects-of :optimistic/timeout-cancel effects-c)))
            "Settlement observation already relinquished timeout ownership.")))))

(deftest rejected-and-failed-settlements-use-adapter-owned-rollback-policy-test
  (doseq [[resolution rollback-eligible? expected-disposition]
          [[:rejected true :rollback]
           [:rejected false :refresh-authority]
           [:failed true :rollback]
           [:failed false :refresh-authority]]]
    (testing (str resolution " rollback? " rollback-eligible?)
      (let [[state-start _]
            (start
             (adapter/initial-state)
             "execution-1"
             (machine-execution (optimistic-receive-choreography) :browser)
             :card
             {:optimistic (optimistic-config rollback-eligible?)})
            [state-a _] (establish-provisional state-start "execution-1")
            generation (adapter/execution-generation state-a "execution-1")
            settlement {:resolution resolution}
            [state-b _]
            (adapter/step
             state-a
             {:event :optimistic/settlement-observed
              :execution-id "execution-1"
              :generation generation
              :resolution resolution
              :settlement settlement})
            [_ effects]
            (adapter/step
             state-b
             {:event :machine/message
              :execution-id "execution-1"
              :generation generation
              :message-id :settlement-1
              :envelope (settlement-envelope)})
            finish (effect-data :optimistic/finish effects)]
        (is (= resolution (:resolution finish)))
        (is (= expected-disposition (:disposition finish)))))))

(deftest current-timeout-retires-optimistic-execution-without-redundant-cancel-test
  (doseq [[rollback-eligible? expected-disposition]
          [[true :rollback-and-refresh]
           [false :refresh-authority]]]
    (testing (str "rollback? " rollback-eligible?)
      (let [[state-start _]
            (start
             (adapter/initial-state)
             "execution-1"
             (machine-execution (optimistic-receive-choreography) :browser)
             :card
             {:optimistic (optimistic-config rollback-eligible? 100)})
            [state-a _] (establish-provisional state-start "execution-1")
            generation (adapter/execution-generation state-a "execution-1")
            timeout-generation (:timeout-generation
                                (adapter/optimistic-scope state-a "execution-1"))
            [state-b effects-b]
            (adapter/step
             state-a
             {:event :optimistic/timeout-fired
              :execution-id "execution-1"
              :generation generation
              :timeout-generation timeout-generation})
            finish (effect-data :optimistic/finish effects-b)]
        (is (nil? (adapter/execution state-b "execution-1")))
        (is (nil? (adapter/optimistic-scope state-b "execution-1")))
        (is (= [:optimistic/finish :execution/retired]
               (mapv first effects-b)))
        (is (= :timeout (:resolution finish)))
        (is (= expected-disposition (:disposition finish)))
        (is (= 0 (count (effects-of :optimistic/timeout-cancel effects-b)))
            "A timer that has already fired no longer needs cancellation.")))))

(deftest incompatible-protocol-retirement-uses-authority-reconstruction-policy-test
  (doseq [[rollback-eligible? expected-disposition]
          [[true :rollback-and-refresh]
           [false :refresh-authority]]]
    (testing (str "rollback? " rollback-eligible?)
      (let [[state-start _]
            (start
             (adapter/initial-state)
             "execution-1"
             (machine-execution (optimistic-receive-choreography) :browser)
             :card
             {:optimistic (optimistic-config rollback-eligible?)})
            [state-a _] (establish-provisional state-start "execution-1")
            generation (adapter/execution-generation state-a "execution-1")
            [state-b effects-b]
            (adapter/step
             state-a
             {:event :execution/retire
              :execution-id "execution-1"
              :generation generation
              :reason :optimistic-incompatible-protocol})
            finish (effect-data :optimistic/finish effects-b)]
        (is (nil? (adapter/execution state-b "execution-1")))
        (is (nil? (adapter/optimistic-scope state-b "execution-1")))
        (is (nil? (adapter/target-owner state-b :card)))
        (is (= [:optimistic/timeout-cancel
                :optimistic/finish
                :execution/retired]
               (mapv first effects-b)))
        (is (= :incompatible-protocol (:resolution finish)))
        (is (= expected-disposition (:disposition finish)))
        (is (= :optimistic-incompatible-protocol (:reason finish)))
        (is (= :optimistic-incompatible-protocol
               (get-in (effect-of :execution/retired effects-b) [1 :reason])))
        (is (not (contains? finish :settlement))
            "Protocol incompatibility cannot manufacture a trusted settlement.")))))

(deftest stale-optimistic-timeout-is-powerless-test
  (let [[state-start _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true 100)})
        [state-a _] (establish-provisional state-start "execution-1")
        generation (adapter/execution-generation state-a "execution-1")
        current-timeout (:timeout-generation
                         (adapter/optimistic-scope state-a "execution-1"))
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :optimistic/timeout-fired
          :execution-id "execution-1"
          :generation generation
          :timeout-generation (inc current-timeout)})]
    (is (= state-a state-b))
    (is (= :stale-optimistic-timeout
           (ignored-reason effects-b)))
    (is (some? (adapter/optimistic-scope state-b "execution-1")))))

(deftest optimistic-network-failure-does-not-fabricate-trusted-settlement-test
  (let [[state-start _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (local-then-send-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        [state-a effects-a] (establish-provisional state-start "execution-1")
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
        [state-c effects-c]
        (adapter/step
         state-b
         {:event :transport/failed
          :execution-id "execution-1"
          :generation generation
          :effect-generation transport-generation
          :reason :network-down})
        finish (effect-data :optimistic/finish effects-c)]
    (is (nil? (adapter/execution state-c "execution-1")))
    (is (= [:optimistic/timeout-cancel
            :optimistic/finish
            :execution/retired]
           (mapv first effects-c)))
    (is (= :network-failed (:resolution finish)))
    (is (= :rollback-and-refresh (:disposition finish)))
    (is (not (contains? finish :settlement))
        "Transport uncertainty cannot manufacture a trusted protocol settlement.")
    (is (= :optimistic-network-failed
           (get-in (effect-of :execution/retired effects-c) [1 :reason])))))

(deftest authoritative-supersession-wins-and-retires-old-optimistic-generation-test
  (let [[state-start _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        [state-a _] (establish-provisional state-start "execution-1")
        generation (adapter/execution-generation state-a "execution-1")
        authoritative {:presence :present
                       :basis :basis-9
                       :projection {:status :canonical}}
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :optimistic/authoritative-superseded
          :execution-id "execution-1"
          :generation generation
          :authoritative authoritative})
        finish (effect-data :optimistic/finish effects-b)]
    (is (nil? (adapter/execution state-b "execution-1")))
    (is (nil? (adapter/optimistic-scope state-b "execution-1")))
    (is (= [:optimistic/timeout-cancel
            :optimistic/finish
            :execution/retired]
           (mapv first effects-b)))
    (is (= :superseded (:resolution finish)))
    (is (= :authoritative (:disposition finish)))
    (is (= authoritative (:authoritative finish)))
    (is (= :authoritative-superseded
           (get-in (effect-of :execution/retired effects-b) [1 :reason])))))

(deftest explicit-retirement-before-provisional-derivation-needs-no-physical-finish-test
  (let [[state-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        generation (adapter/execution-generation state-a "execution-1")
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :execution/retire
          :execution-id "execution-1"
          :generation generation
          :reason :page-detached})]
    (is (nil? (adapter/optimistic-scope state-b "execution-1")))
    (is (= [:execution/retired] (mapv first effects-b))
        "No physical provisional or timer existed, so there is nothing optimistic to finish.")))

(deftest explicit-retirement-releases-established-optimistic-ownership-without-rollback-test
  (let [[state-start _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        [state-a _] (establish-provisional state-start "execution-1")
        generation (adapter/execution-generation state-a "execution-1")
        [state-b effects-b]
        (adapter/step
         state-a
         {:event :execution/retire
          :execution-id "execution-1"
          :generation generation
          :reason :page-detached})
        finish (effect-data :optimistic/finish effects-b)]
    (is (nil? (adapter/optimistic-scope state-b "execution-1")))
    (is (= [:optimistic/timeout-cancel
            :optimistic/finish
            :execution/retired]
           (mapv first effects-b)))
    (is (= :retired (:resolution finish)))
    (is (= :release-only (:disposition finish)))
    (is (= :page-detached (:reason finish)))))

(deftest replacing-target-retires-old-optimistic-scope-before-new-derivation-test
  (let [[state-start-a _]
        (start
         (adapter/initial-state)
         "execution-a"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (assoc (optimistic-config true) :command-id :command-a)})
        [state-a _] (establish-provisional state-start-a "execution-a")
        [state-b effects-b]
        (start
         state-a
         "execution-b"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:replace-owner? true
          :optimistic (assoc (optimistic-config true) :command-id :command-b)})
        old-finish (effect-data :optimistic/finish effects-b)
        scope-b (adapter/optimistic-scope state-b "execution-b")]
    (is (nil? (adapter/execution state-b "execution-a")))
    (is (nil? (adapter/optimistic-scope state-b "execution-a")))
    (is (some? (adapter/execution state-b "execution-b")))
    (is (= "execution-b"
           (:execution-id (adapter/target-owner state-b :card))))
    (is (= :retired (:resolution old-finish)))
    (is (= :release-only (:disposition old-finish)))
    (is (= :target-replaced (:reason old-finish)))
    (is (= :awaiting-provisional (:status scope-b)))
    (is (= [:optimistic/timeout-cancel
            :optimistic/finish
            :execution/retired
            :machine/local]
           (mapv first effects-b)))
    (is (= 0 (count (effects-of :optimistic/install-provisional effects-b)))
        "Replacement cannot install provisional state until its own Choreo derivation completes.")
    (is (< (.indexOf (mapv first effects-b) :optimistic/finish)
           (.indexOf (mapv first effects-b) :machine/local))
        "Old optimistic ownership retires before replacement derivation begins.")))

(deftest replacing-same-execution-id-allocates-fresh-awaiting-provisional-generation-test
  (let [[state-start-a _]
        (start
         (adapter/initial-state)
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:optimistic (optimistic-config true)})
        [state-a _] (establish-provisional state-start-a "execution-1")
        generation-a (adapter/execution-generation state-a "execution-1")
        [state-b effects-b]
        (start
         state-a
         "execution-1"
         (machine-execution (optimistic-receive-choreography) :browser)
         :card
         {:replace-execution? true
          :optimistic (optimistic-config true)})
        generation-b (adapter/execution-generation state-b "execution-1")
        scope-b (adapter/optimistic-scope state-b "execution-1")]
    (is (not= generation-a generation-b))
    (is (= generation-b (:execution-generation scope-b)))
    (is (= :awaiting-provisional (:status scope-b)))
    (is (nil? (:provisional scope-b)))
    (is (nil? (:timeout-generation scope-b)))
    (is (= [:optimistic/timeout-cancel
            :optimistic/finish
            :execution/retired
            :machine/local]
           (mapv first effects-b)))))

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

(deftest continuity-failure-retires-current-restoring-slot-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c]
        (before-swap
         state-b
         :panel
         generation
         :xhr-1
         {:scope :request-1
          :basis :basis-1})
        slot (effect-data :continuity/capture effects-c)
        [state-d _]
        (adapter/step
         state-c
         {:event :htmx/after-swap
          :fragment-id :panel
          :request-generation generation
          :request-id :xhr-1})
        frontier-before (adapter/authoritative-frontier state-d :request-1)
        [state-e failure-effects]
        (adapter/step
         state-d
         {:event :continuity/failed
          :slot-id (:slot-id slot)
          :slot-generation (:slot-generation slot)
          :reason :application-restore-rejected})
        release (effect-data :continuity/release failure-effects)]
    (is (nil? (get-in state-e [:continuity (:slot-id slot)]))
        "A current restore failure retires exactly the failed continuity slot.")
    (is (= 1 (count (effects-of :continuity/release failure-effects))))
    (is (= (:slot-id slot) (:slot-id release)))
    (is (= (:slot-generation slot) (:slot-generation release)))
    (is (= :panel (:fragment-id release)))
    (is (= generation (:request-generation release)))
    (is (= :application-restore-rejected (:reason release)))
    (is (= frontier-before
           (adapter/authoritative-frontier state-e :request-1))
        "Continuity failure cannot roll back or reinterpret authoritative state.")))

(deftest continuity-failure-is-generation-gated-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c] (before-swap state-b :panel generation :xhr-1)
        slot (effect-data :continuity/capture effects-c)
        [state-d _]
        (adapter/step
         state-c
         {:event :htmx/after-swap
          :fragment-id :panel
          :request-generation generation
          :request-id :xhr-1})
        [state-e stale-effects]
        (adapter/step
         state-d
         {:event :continuity/failed
          :slot-id (:slot-id slot)
          :slot-generation (inc (:slot-generation slot))
          :reason :late-rejection})]
    (is (= state-d state-e)
        "A failure for another slot generation is semantically inert.")
    (is (= :stale-continuity-failure
           (ignored-reason stale-effects)))
    (is (= 0 (count (effects-of :continuity/release stale-effects))))))

(deftest continuity-cannot-fail-before-restore-has-been-issued-test
  ;; Capture allocates a physical resource but does not authorize failure to
  ;; retire its semantic slot. Only a restore that has actually been issued may
  ;; later succeed or fail for that slot generation.
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c] (before-swap state-b :panel generation :xhr-1)
        slot (effect-data :continuity/capture effects-c)
        [state-d failure-effects]
        (adapter/step
         state-c
         {:event :continuity/failed
          :slot-id (:slot-id slot)
          :slot-generation (:slot-generation slot)
          :reason :premature-rejection})]
    (is (= state-c state-d)
        "Capture alone is not authority to retire a continuity slot as failed.")
    (is (= :continuity-restore-not-issued
           (ignored-reason failure-effects)))
    (is (= 0 (count (effects-of :continuity/release failure-effects))))))

(deftest newer-fragment-swap-releases-older-continuity-before-capture-and-allow-test
  ;; Keep generation A's restore outstanding, then let a queued invalidation
  ;; produce generation B. B may replace the fragment only after A has lost
  ;; physical mutation authority.
  (let [{state-a :state gen-1 :generation}
        (begin-fragment (adapter/initial-state) :panel :basis-1)
        [state-b _] (bind-request state-a :panel gen-1 :xhr-1)
        [state-c effects-c] (before-swap state-b :panel gen-1 :xhr-1)
        old-slot (effect-data :continuity/capture effects-c)
        [state-d _]
        (adapter/step
         state-c
         {:event :htmx/after-swap
          :fragment-id :panel
          :request-generation gen-1
          :request-id :xhr-1})
        [state-e _] (adapter/step state-d (invalidation :panel :basis-2))
        [state-f effects-f]
        (adapter/step
         state-e
         {:event :htmx/after-request
          :fragment-id :panel
          :request-generation gen-1
          :request-id :xhr-1})
        gen-2 (:request-generation (effect-data :fragment/refresh effects-f))
        [state-g _] (bind-request state-f :panel gen-2 :xhr-2)
        [state-h effects-h] (before-swap state-g :panel gen-2 :xhr-2)
        release (effect-data :continuity/release effects-h)
        new-slot (effect-data :continuity/capture effects-h)]
    (is (not= gen-1 gen-2))
    (is (= [:continuity/release
            :continuity/capture
            :htmx/allow-swap]
           (mapv first effects-h))
        "The old physical resource must be revoked before capture and swap authority for the newer representation.")
    (is (= (:slot-id old-slot) (:slot-id release)))
    (is (= (:slot-generation old-slot) (:slot-generation release)))
    (is (= :newer-fragment-swap (:reason release)))
    (is (nil? (get-in state-h [:continuity (:slot-id old-slot)])))
    (is (= [:panel gen-2] (:slot-id new-slot)))
    (is (some? (get-in state-h [:continuity (:slot-id new-slot)])))
    (is (= 1 (count (effects-of :continuity/release effects-h))))
    (is (= 1 (count (effects-of :continuity/capture effects-h))))
    (is (= 1 (count (effects-of :htmx/allow-swap effects-h))))))

(deftest duplicate-before-swap-does-not-revoke-current-continuity-test
  (let [{state-a :state generation :generation}
        (begin-fragment (adapter/initial-state) :panel)
        [state-b _] (bind-request state-a :panel generation :xhr-1)
        [state-c effects-c] (before-swap state-b :panel generation :xhr-1)
        slot (effect-data :continuity/capture effects-c)
        [state-d effects-d] (before-swap state-c :panel generation :xhr-1)]
    (is (= state-c state-d))
    (is (= 0 (count (effects-of :continuity/release effects-d)))
        "A duplicate lifecycle observation for the same request generation cannot revoke its own slot.")
    (is (= 0 (count (effects-of :continuity/capture effects-d))))
    (is (= 1 (count (effects-of :htmx/allow-swap effects-d))))
    (let [current-slot (get-in state-d [:continuity (:slot-id slot)])]
      (is (= (:slot-id slot) (:slot-id current-slot)))
      (is (= (:slot-generation slot) (:generation current-slot)))
      (is (= generation (:request-generation current-slot)))
      (is (false? (:restore-issued? current-slot))
          "The duplicate beforeSwap does not advance continuity lifecycle state."))))

(deftest rejected-newer-swap-does-not-revoke-existing-continuity-test
  ;; A non-monotone authoritative candidate never reaches the replacement
  ;; boundary, so it must not revoke continuity belonging to the representation
  ;; that remains installed.
  (let [{state-a :state gen-1 :generation}
        (begin-fragment (adapter/initial-state) :panel :basis-1)
        [state-b _] (bind-request state-a :panel gen-1 :xhr-1)
        [state-c effects-c]
        (before-swap
         state-b :panel gen-1 :xhr-1
         {:scope :request-1 :basis :basis-1})
        old-slot (effect-data :continuity/capture effects-c)
        [state-d _]
        (adapter/step
         state-c
         {:event :htmx/after-swap
          :fragment-id :panel
          :request-generation gen-1
          :request-id :xhr-1})
        [state-e _] (adapter/step state-d (invalidation :panel :basis-2))
        [state-f effects-f]
        (adapter/step
         state-e
         {:event :htmx/after-request
          :fragment-id :panel
          :request-generation gen-1
          :request-id :xhr-1})
        gen-2 (:request-generation (effect-data :fragment/refresh effects-f))
        [state-g _] (bind-request state-f :panel gen-2 :xhr-2)
        [state-h denied-effects]
        (before-swap
         state-g :panel gen-2 :xhr-2
         {:scope :request-1 :basis :basis-2})]
    (is (= state-g state-h)
        "Rejecting the newer representation cannot disturb the currently installed continuity generation.")
    (is (= 1 (count (effects-of :htmx/cancel-swap denied-effects))))
    (is (= :non-monotone-authoritative-install
           (get-in (effect-of :htmx/cancel-swap denied-effects) [1 :reason])))
    (is (= 0 (count (effects-of :continuity/release denied-effects))))
    (is (= 0 (count (effects-of :continuity/capture denied-effects))))
    (is (= 0 (count (effects-of :htmx/allow-swap denied-effects))))
    (is (some? (get-in state-h [:continuity (:slot-id old-slot)])))
    (is (= {:basis :basis-1}
           (adapter/authoritative-frontier state-h :request-1)))))

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
