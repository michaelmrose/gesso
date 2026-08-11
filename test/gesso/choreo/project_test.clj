(ns gesso.choreo.project-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- choreography
  ([initial states]
   (choreography initial states nil))
  ([initial states opts]
   (choreo/->choreography
    (merge
     {:name :test/choreography
      :roles #{:browser :server}
      :initial initial
      :states states}
     opts))))

(defn- single-role-choreography
  ([initial states]
   (single-role-choreography initial states nil))
  ([initial states opts]
   (choreography
    initial
    states
    (merge
     {:roles #{:browser}}
     opts))))

(defn- states-by-op
  [plan op]
  (into {}
        (filter
         (fn [[_ state]]
           (= op (:op state))))
        (:states plan)))

(defn- state-values-by-op
  [plan op]
  (vec
   (vals
    (states-by-op plan op))))

(defn- only-state-by-op
  [plan op]
  (let [states
        (state-values-by-op plan op)]
    (is (= 1 (count states))
        (str "Expected one projected " op " state, found " (count states)))
    (first states)))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn- receive-alternatives
  [plan]
  (vec
   (mapcat
    #(get % :receives [])
    (state-values-by-op plan :await))))

(defn- await-state-containing-event
  [plan event]
  (some
   (fn [state]
     (when (contains? (:events state {}) event)
       state))
   (state-values-by-op plan :await)))

(defn- local-state-ids
  [plan]
  (set
   (for [[state-id state] (:states plan)
         :when (keyword? state-id)]
     state-id)))

;; -----------------------------------------------------------------------------
;; Fixture choreographies
;; -----------------------------------------------------------------------------

(def local-choreography
  (single-role-choreography
   :browser/prepare
   {:browser/prepare
    (choreo/fx
     :browser
     :test/prepare
     :browser/choose
     {:input {:mode :test}
      :metadata {:not :projected}})

    :browser/choose
    (choreo/choice
     :browser
     :path
     {:continue :browser/acquire
      :stop :browser/stopped})

    :browser/acquire
    (choreo/acquire
     :browser
     :target-authority
     :browser/work)

    :browser/work
    (choreo/fx
     :browser
     :test/work
     :browser/release)

    :browser/release
    (choreo/release
     :browser
     :target-authority
     :browser/done)

    :browser/done
    (choreo/return
     :browser
     :done
     {:value-key :result
      :metadata {:not :projected}})

    :browser/stopped
    (choreo/return
     :browser
     :stopped)}
   {:resources
    {:target-authority
     (choreo/resource
      {:owner :browser
       :metadata {:purpose :test}})}}))

(def roundtrip-choreography
  (choreography
   :browser/send-command
   {:browser/send-command
    (choreo/send
     :browser
     :server
     :command
     :server/receive-command
     {:via :http
      :required #{:execution-id :action}
      :optional #{:note}
      :correlation #{:execution-id}
      :interrupts
      {:request-failed :browser/request-failed}})

    :server/receive-command
    (choreo/receive
     :browser
     :server
     :command
     :server/work
     {:via :http
      :bind :command})

    :server/work
    (choreo/fx
     :server
     :test/server-work
     :server/send-settlement)

    :server/send-settlement
    (choreo/send
     :server
     :browser
     :settled
     :browser/receive-settlement
     {:via :http
      :required #{:execution-id :outcome}
      :optional #{:message}
      :correlation #{:execution-id}})

    :browser/receive-settlement
    (choreo/receive
     :server
     :browser
     :settled
     :browser/done
     {:via :http
      :bind :settlement})

    :browser/done
    (choreo/return
     :browser
     :confirmed
     {:value-key :settlement})

    :browser/request-failed
    (choreo/return
     :browser
     :request-failed)}
   {:environment-events
    #{:request-failed}}))

(def one-way-choreography
  (choreography
   :browser/send-command
   {:browser/send-command
    (choreo/send
     :browser
     :server
     :command
     :server/receive-command
     {:via :http
      :required #{:execution-id}
      :correlation #{:execution-id}
      :interrupts
      {:request-failed :browser/request-failed}})

    :server/receive-command
    (choreo/receive
     :browser
     :server
     :command
     :server/done
     {:via :http
      :bind :command})

    :server/done
    (choreo/return
     :server
     :accepted)

    :browser/request-failed
    (choreo/return
     :browser
     :request-failed)}
   {:environment-events
    #{:request-failed}}))

(def payload-choice-choreography
  (choreography
   :server/choose
   {:server/choose
    (choreo/choice
     :server
     :accepted?
     {true :server/send-true
      false :server/send-false})

    :server/send-true
    (choreo/send
     :server
     :browser
     :settled
     :browser/receive-true
     {:via :sse
      :required #{:accepted? :execution-id}
      :correlation #{:execution-id}})

    :browser/receive-true
    (choreo/receive
     :server
     :browser
     :settled
     :browser/accepted
     {:via :sse
      :bind :settlement})

    :browser/accepted
    (choreo/return
     :browser
     :accepted)

    :server/send-false
    (choreo/send
     :server
     :browser
     :settled
     :browser/receive-false
     {:via :sse
      :required #{:accepted? :execution-id}
      :correlation #{:execution-id}})

    :browser/receive-false
    (choreo/receive
     :server
     :browser
     :settled
     :browser/rejected
     {:via :sse
      :bind :settlement})

    :browser/rejected
    (choreo/return
     :browser
     :rejected)}))

(def distinct-event-choice-choreography
  (choreography
   :server/choose
   {:server/choose
    (choreo/choice
     :server
     :accepted?
     {true :server/send-accepted
      false :server/send-rejected})

    :server/send-accepted
    (choreo/send
     :server
     :browser
     :accepted
     :browser/receive-accepted
     {:via :sse})

    :browser/receive-accepted
    (choreo/receive
     :server
     :browser
     :accepted
     :browser/accepted
     {:via :sse})

    :browser/accepted
    (choreo/return
     :browser
     :accepted)

    :server/send-rejected
    (choreo/send
     :server
     :browser
     :rejected
     :browser/receive-rejected
     {:via :sse})

    :browser/receive-rejected
    (choreo/receive
     :server
     :browser
     :rejected
     :browser/rejected
     {:via :sse})

    :browser/rejected
    (choreo/return
     :browser
     :rejected)}))

(def branch-independent-choreography
  (choreography
   :server/choose
   {:server/choose
    (choreo/choice
     :server
     :path
     {:a :browser/work
      :b :browser/work})

    :browser/work
    (choreo/fx
     :browser
     :test/browser-work
     :browser/done)

    :browser/done
    (choreo/return
     :browser
     :done)}))

(def foreign-only-choreography
  (choreography
   :browser/work
   {:browser/work
    (choreo/fx
     :browser
     :test/browser-work
     :browser/done)

    :browser/done
    (choreo/return
     :browser
     :done)}))

(def local-await-choreography
  (choreography
   :browser/wait
   {:browser/wait
    (choreo/await
     :browser
     {:timeout :browser/timeout
      :online :browser/online}
     {:bind :wake})

    :browser/timeout
    (choreo/return
     :browser
     :timed-out)

    :browser/online
    (choreo/return
     :browser
     :online)}
   {:environment-events
    #{:timeout :online :server-only}

    :resources
    {:browser-resource
     (choreo/resource
      {:owner :browser})

     :unused-server-resource
     (choreo/resource
      {:owner :server
       :terminal-release? false})}}))

;; -----------------------------------------------------------------------------
;; Projected representation
;; -----------------------------------------------------------------------------

(deftest projected-plan-constants-test
  (is (= 1 project/projected-plan-version))
  (is (= :gesso.choreo/projected-plan
         project/projected-plan-type))
  (is (= #{:fx
           :send
           :choice
           :await
           :acquire
           :release
           :return}
         project/projected-ops))
  (is (= :gesso.choreo/complete
         project/projected-complete-outcome)))

(deftest projected-plan-predicate-test
  (let [plan
        (project/project local-choreography :browser)]
    (is (project/projected-plan? plan))
    (is (identical?
         plan
         (project/ensure-projected-plan plan))))

  (doseq [value
          [nil
           {}
           {:gesso.choreo/type project/projected-plan-type
            :gesso.choreo/version project/projected-plan-version
            :role "browser"
            :states {}}
           {:gesso.choreo/type project/projected-plan-type
            :gesso.choreo/version 999
            :role :browser
            :states {}}]]
    (is (false?
         (project/projected-plan? value))))

  (let [data
        (thrown-data
         #(project/ensure-projected-plan
           {:not :a-plan}))]
    (is (= :gesso.choreo/projection-failed
           (:error/type data)))
    (is (= :invalid-projected-plan
           (:error/kind data)))))

;; -----------------------------------------------------------------------------
;; Local-state projection
;; -----------------------------------------------------------------------------

(deftest local-operations-project-mechanically-test
  (let [plan
        (project/project local-choreography :browser)]

    (testing "the role-local entry remains the original local state"
      (is (= :browser/prepare
             (:initial plan)))
      (is (= :browser
             (:role plan)))
      (is (= :test/choreography
             (:name plan))))

    (testing "local FX keeps the executable machine and explicit input"
      (is (= {:op :fx
              :role :browser
              :machine :test/prepare
              :next :browser/choose
              :input {:mode :test}}
             (project/state plan :browser/prepare))))

    (testing "local choice remains an ordinary role-local choice"
      (is (= {:op :choice
              :role :browser
              :key :path
              :branches
              {:continue :browser/acquire
               :stop :browser/stopped}}
             (project/state plan :browser/choose))))

    (testing "local acquire/release remain explicit authority operations"
      (is (= {:op :acquire
              :role :browser
              :resource :target-authority
              :next :browser/work}
             (project/state plan :browser/acquire)))
      (is (= {:op :release
              :role :browser
              :resource :target-authority
              :next :browser/done}
             (project/state plan :browser/release))))

    (testing "terminal value-key survives projection"
      (is (= {:op :return
              :role :browser
              :outcome :done
              :value-key :result}
             (project/state plan :browser/done))))

    (testing "compiler/runtime-irrelevant global metadata is not copied"
      (is (not
           (contains?
            (project/state plan :browser/prepare)
            :metadata)))
      (is (not
           (contains?
            (project/state plan :browser/done)
            :metadata))))))

(deftest local-resources-are-retained-test
  (let [plan
        (project/project local-choreography :browser)]
    (is (= {:target-authority
            (choreo/resource
             {:owner :browser
              :metadata {:purpose :test}})}
           (:resources plan)))))

;; -----------------------------------------------------------------------------
;; Foreign-state elision and completion
;; -----------------------------------------------------------------------------

(deftest foreign-implementation-work-disappears-test
  (let [server-plan
        (project/project foreign-only-choreography :server)
        initial-state
        (project/state server-plan (:initial server-plan))]

    (testing "a role with no observable work receives only local completion"
      (is (= :return (:op initial-state)))
      (is (= :server (:role initial-state)))
      (is (= project/projected-complete-outcome
             (:outcome initial-state))))

    (testing "foreign implementation state ids do not leak into the plan"
      (is (not
           (contains?
            (:states server-plan)
            :browser/work)))
      (is (not
           (contains?
            (:states server-plan)
            :browser/done))))))

(deftest remote-choice-with-identical-local-frontier-needs-no-message-test
  (let [browser-plan
        (project/project
         branch-independent-choreography
         :browser)]

    (testing "both remote branches collapse to the same observable local state"
      (is (= :browser/work
             (:initial browser-plan)))
      (is (= {:op :fx
              :role :browser
              :machine :test/browser-work
              :next :browser/done}
             (project/state browser-plan :browser/work))))

    (testing "projection does not invent a choice or receive gate"
      (is (empty?
           (states-by-op browser-plan :choice)))
      (is (empty?
           (receive-alternatives browser-plan))))))

;; -----------------------------------------------------------------------------
;; Outgoing communication
;; -----------------------------------------------------------------------------

(deftest local-send-retains-message-contract-test
  (let [browser-plan
        (project/project roundtrip-choreography :browser)
        send
        (project/state browser-plan :browser/send-command)]

    (is (= :send (:op send)))
    (is (= :browser (:role send)))
    (is (= :server (:to send)))
    (is (= :command (:event send)))
    (is (= :http (:via send)))
    (is (= #{:execution-id :action}
           (:required send)))
    (is (= #{:note}
           (:optional send)))
    (is (= #{:execution-id}
           (:correlation send)))
    (is (contains?
         (:states browser-plan)
         (:next send)))))

(deftest send-interrupt-merges-with-following-receive-wait-test
  (let [browser-plan
        (project/project roundtrip-choreography :browser)
        send
        (project/state browser-plan :browser/send-command)
        wait
        (project/state browser-plan (:next send))]

    (testing "the post-send state is one combined suspension gate"
      (is (= :await (:op wait)))
      (is (= :browser (:role wait)))
      (is (= :browser/request-failed
             (get-in wait [:events :request-failed])))
      (is (= 1
             (count (:receives wait)))))

    (testing "the normal settlement receive contract remains intact"
      (let [receive
            (first (:receives wait))]
        (is (= :server (:from receive)))
        (is (= :browser (:to receive)))
        (is (= :settled (:event receive)))
        (is (= :http (:via receive)))
        (is (= #{:execution-id :outcome}
               (:required receive)))
        (is (= #{:message}
               (:optional receive)))
        (is (= #{:execution-id}
               (:correlation receive)))
        (is (= {}
               (:match receive)))
        (is (= :settlement
               (:bind receive)))
        (is (= :browser/done
               (:next receive)))))))

(deftest send-interrupt-is-dropped-after-protocol-independence-test
  (let [browser-plan
        (project/project one-way-choreography :browser)
        send
        (project/state browser-plan :browser/send-command)
        next-state
        (project/state browser-plan (:next send))]

    (testing "normal completion is enough once no incoming protocol work remains"
      (is (= :return (:op next-state)))
      (is (= project/projected-complete-outcome
             (:outcome next-state))))

    (testing "the request-failed interrupt is not retained as an unbounded wait"
      (is (nil?
           (await-state-containing-event
            browser-plan
            :request-failed))))))

;; -----------------------------------------------------------------------------
;; Incoming communication
;; -----------------------------------------------------------------------------

(deftest foreign-send-and-local-receive-become-await-test
  (let [server-plan
        (project/project roundtrip-choreography :server)
        initial
        (project/state server-plan (:initial server-plan))
        receive
        (first (:receives initial))]

    (testing "the raw receive state is replaced by a projected await gate"
      (is (= :await (:op initial)))
      (is (= :server (:role initial)))
      (is (not
           (contains?
            (:states server-plan)
            :server/receive-command))))

    (testing "the await alternative combines the send contract with receive binding"
      (is (= {:from :browser
              :to :server
              :event :command
              :required #{:execution-id :action}
              :optional #{:note}
              :correlation #{:execution-id}
              :match {}
              :via :http
              :bind :command
              :next :server/work}
             receive)))

    (testing "local execution resumes after the communication gate"
      (is (= {:op :fx
              :role :server
              :machine :test/server-work
              :next :server/send-settlement}
             (project/state server-plan :server/work))))))

(deftest projected-plans-contain-no-raw-receive-op-test
  (doseq [plan
          (vals
           (project/project-all roundtrip-choreography))]
    (is (empty?
         (states-by-op plan :receive)))))

;; -----------------------------------------------------------------------------
;; Knowledge of choice
;; -----------------------------------------------------------------------------

(deftest remote-choice-value-becomes-message-match-test
  (let [browser-plan
        (project/project payload-choice-choreography :browser)
        alternatives
        (receive-alternatives browser-plan)]

    (is (= 2
           (count alternatives)))

    (testing "both alternatives use the same participant message identity"
      (is (= #{[:server :browser :settled :sse]}
             (set
              (map
               (juxt :from :to :event :via)
               alternatives)))))

    (testing "the deciding role communicates the choice through required payload"
      (is (= #{{:accepted? true}
               {:accepted? false}}
             (set
              (map :match alternatives))))
      (is (every?
           #(contains?
             (:required %)
             :accepted?)
           alternatives)))

    (testing "each choice value continues to its corresponding local branch"
      (is (= {true :browser/accepted
              false :browser/rejected}
             (into {}
                   (map
                    (fn [alternative]
                      [(get-in alternative [:match :accepted?])
                       (:next alternative)]))
                   alternatives))))))

(deftest distinct-message-events-carry-choice-without-payload-match-test
  (let [browser-plan
        (project/project
         distinct-event-choice-choreography
         :browser)
        alternatives
        (receive-alternatives browser-plan)]

    (is (= 2
           (count alternatives)))
    (is (= #{:accepted :rejected}
           (set
            (map :event alternatives))))
    (is (= #{{}}
           (set
            (map :match alternatives))))
    (is (= #{:browser/accepted
             :browser/rejected}
           (set
            (map :next alternatives))))))

(deftest local-choice-remains-local-test
  (let [server-plan
        (project/project payload-choice-choreography :server)
        choose
        (project/state server-plan :server/choose)]

    (is (= :choice (:op choose)))
    (is (= :server (:role choose)))
    (is (= :accepted? (:key choose)))
    (is (= #{true false}
           (set
            (keys
             (:branches choose)))))
    (is (= :send
           (:op
            (project/state
             server-plan
             (get-in choose [:branches true])))))
    (is (= :send
           (:op
            (project/state
             server-plan
             (get-in choose [:branches false])))))))

;; -----------------------------------------------------------------------------
;; Local await and projected environment
;; -----------------------------------------------------------------------------

(deftest local-await-remains-local-await-test
  (let [browser-plan
        (project/project local-await-choreography :browser)
        wait
        (project/state browser-plan :browser/wait)]

    (is (= {:op :await
            :role :browser
            :events
            {:timeout :browser/timeout
             :online :browser/online}
            :bind :wake}
           wait))

    (testing "only environment events observable by this endpoint are emitted"
      (is (= #{:timeout :online}
             (:environment-events browser-plan)))
      (is (not
           (contains?
            (:environment-events browser-plan)
            :server-only))))))

(deftest unused-global-resources-do-not-leak-to-endpoint-test
  (let [browser-plan
        (project/project local-await-choreography :browser)]
    (is (= {}
           (:resources browser-plan)))))

;; -----------------------------------------------------------------------------
;; Verification boundary
;; -----------------------------------------------------------------------------

(deftest project-verifies-plain-choreography-test
  (let [invalid
        (choreography
         :browser/send
         {:browser/send
          (choreo/send
           :browser
           :server
           :command
           :missing-receive)})]
    (is (thrown?
         clojure.lang.ExceptionInfo
         (project/project invalid :browser)))))

(deftest project-accepts-preverified-choreography-test
  (let [verified
        (verify/verify! roundtrip-choreography)
        from-plain
        (project/project roundtrip-choreography :browser)
        from-verified
        (project/project verified :browser)]
    (is (= from-plain
           from-verified))))

(deftest project-rejects-unknown-role-test
  (let [data
        (thrown-data
         #(project/project
           roundtrip-choreography
           :ghost))]
    (is (= :gesso.choreo/projection-failed
           (:error/type data)))
    (is (= :unknown-role
           (:error/kind data)))
    (is (= :ghost
           (:role data)))
    (is (= #{:browser :server}
           (:roles data)))))

;; -----------------------------------------------------------------------------
;; project-all, state, and explain
;; -----------------------------------------------------------------------------

(deftest project-all-emits-one-plan-per-role-test
  (let [plans
        (project/project-all roundtrip-choreography)]

    (is (= #{:browser :server}
           (set
            (keys plans))))
    (is (= :browser
           (get-in plans [:browser :role])))
    (is (= :server
           (get-in plans [:server :role])))
    (is (every?
         project/projected-plan?
         (vals plans)))))

(deftest state-helper-test
  (let [plan
        (project/project local-choreography :browser)]
    (is (= (get-in plan [:states :browser/prepare])
           (project/state plan :browser/prepare)))
    (is (nil?
         (project/state plan :missing)))))

(deftest explain-test
  (let [plan
        (project/project local-choreography :browser)
        explanation
        (project/explain plan)]

    (is (= :test/choreography
           (:name explanation)))
    (is (= :browser
           (:role explanation)))
    (is (= project/projected-plan-version
           (:version explanation)))
    (is (= :browser/prepare
           (:initial explanation)))
    (is (= (count (:states plan))
           (:state-count explanation)))
    (is (= (frequencies
            (map
             (comp :op val)
             (:states plan)))
           (:states-by-op explanation)))
    (is (= #{:target-authority}
           (:resources explanation)))
    (is (= #{}
           (:environment-events explanation)))))

;; -----------------------------------------------------------------------------
;; Endpoint locality invariants
;; -----------------------------------------------------------------------------

(deftest projection-emits-only-local-executable-state-roles-test
  (doseq [[role plan]
          (project/project-all roundtrip-choreography)
          [_state-id state]
          (:states plan)]
    (is (= role
           (:role state))
        (str "Projected state belongs to wrong role: " role " " state))))

(deftest original-local-state-ids-remain-stable-test
  (let [browser-plan
        (project/project local-choreography :browser)]
    (is (= #{:browser/prepare
             :browser/choose
             :browser/acquire
             :browser/work
             :browser/release
             :browser/done
             :browser/stopped}
           (local-state-ids browser-plan)))))

(deftest projection-is-deterministic-test
  (is (= (project/project roundtrip-choreography :browser)
         (project/project roundtrip-choreography :browser)))
  (is (= (project/project payload-choice-choreography :browser)
         (project/project payload-choice-choreography :browser))))
