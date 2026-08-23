(ns gesso.choreo.machine-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.verify :as verify]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (:error/kind
       (ex-data e)))))

(defn- projected
  [choreography role]
  (project/project
   choreography
   role))

(defn- start-role
  [choreography role]
  (machine/start
   (projected choreography role)))

(defn- projected-with-entry-values
  [choreography role entry-value-keys]
  (project/project
   (verify/verify!
    choreography
    {:entry-value-keys entry-value-keys})
   role))

(defn- projected-with-entry-knowledge
  [choreography role entry-knowledge]
  (project/project
   (verify/verify!
    choreography
    {:entry-knowledge entry-knowledge})
   role))

(defn- start-role-with-entry-knowledge
  ([choreography role entry-knowledge]
   (machine/start
    (projected-with-entry-knowledge
     choreography
     role
     entry-knowledge)))
  ([choreography role entry-knowledge values]
   (machine/start
    (projected-with-entry-knowledge
     choreography
     role
     entry-knowledge)
    {:values values})))

(deftest local-action-is-an-explicit-endpoint-boundary
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare-command
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :browser)]

    (is (machine/waiting-local?
         execution))

    (is (= {:kind :local
            :execution-id nil
            :state 0
            :role :browser
            :action :prepare-command}
           (machine/pending-action execution)))

    (let [completed
          (machine/complete-local
           execution)]

      (is (machine/completed?
           completed))

      (is (= {:outcome :gesso.choreo/complete}
             (machine/result completed)))

      (is (= [{:kind :local
               :state 0
               :role :browser
               :action :prepare-command}

              {:kind :terminal
               :state (machine/current-state-id completed)
               :outcome :gesso.choreo/complete}]
             (machine/execution-history
              completed))))))

(deftest send-is-an-explicit-endpoint-boundary
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :authority
            :request/claim
            :done
            {:via :http
             :required #{:request-id}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:request-id}}
         {:request-id 17})]

    (is (machine/waiting-send?
         execution))

    (is (= {:kind :send
            :execution-id nil
            :state 0
            :from :browser
            :to :authority
            :event :request/claim
            :via :http
            :required #{:request-id}}
           (machine/pending-action
            execution)))

    (is (= {:kind :message
            :from :browser
            :to :authority
            :event :request/claim
            :payload {:request-id 17}
            :via :http}
           (machine/pending-message
            execution
            {:request-id 17})))

    (let [{next-execution :execution
           envelope :message}
          (machine/complete-send
           execution
           {:request-id 17})]

      (is (= {:kind :message
              :from :browser
              :to :authority
              :event :request/claim
              :payload {:request-id 17}
              :via :http}
             envelope))

      (is (machine/completed?
           next-execution))

      (is (= [{:kind :send
               :state 0
               :from :browser
               :to :authority
               :event :request/claim
               :payload {:request-id 17}
               :via :http}

              {:kind :terminal
               :state (machine/current-state-id next-execution)
               :outcome :gesso.choreo/complete}]
             (machine/execution-history
              next-execution))))))

(deftest receiver-suspends-for-participant-message
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :authority
            :request/claim
            :done
            {:required #{:request-id}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :authority
         {:browser #{:request-id}})]

    (is (machine/waiting-receive?
         execution))

    (is (= {:kind :receive
            :role :authority
            :alternatives
            [{:from :browser
              :event :request/claim
              :required #{:request-id}}]}
           (machine/awaiting
            execution)))

    (testing "the expected participant message is accepted"
      (let [envelope
            (machine/message
             :browser
             :authority
             :request/claim
             {:request-id 17})]

        (is (machine/accepts-message?
             execution
             envelope))

        (is (machine/accepts?
             execution
             envelope))

        (let [completed
              (machine/receive
               execution
               envelope)]

          (is (machine/completed?
               completed))

          (is (= [{:kind :receive
                   :state (:state execution)
                   :from :browser
                   :to :authority
                   :event :request/claim
                   :payload {:request-id 17}}

                  {:kind :terminal
                   :state
                   (machine/current-state-id
                    completed)
                   :outcome
                   :gesso.choreo/complete}]
                 (machine/execution-history
                  completed))))))))

(deftest receive-and-environment-events-cannot-cross-satisfy
  (let [receive-choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :alice
            :bob
            :same/event
            :done)

           :done
           (choreo/return :done)}})

        receive-execution
        (start-role
         receive-choreography
         :bob)

        environment-envelope
        (machine/environment-event
         :bob
         :same/event
         nil)

        await-choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :bob
            {:same/event :done})

           :done
           (choreo/return :done)}})

        await-execution
        (start-role
         await-choreography
         :bob)

        message-envelope
        (machine/message
         :alice
         :bob
         :same/event
         {})]

    (testing "environment event cannot satisfy participant receive"
      (is (false?
           (machine/accepts?
            receive-execution
            environment-envelope)))

      (is (= :invalid-message
             (error-kind
              #(machine/resume
                receive-execution
                environment-envelope)))))

    (testing "participant message cannot satisfy environment await"
      (is (false?
           (machine/accepts?
            await-execution
            message-envelope)))

      (is (= :invalid-environment-event
             (error-kind
              #(machine/resume
                await-execution
                message-envelope)))))))

(deftest environment-await-is-role-local
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/failed :failed
             :request/completed :done})

           :failed
           (choreo/return :failed)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :browser)]

    (is (machine/waiting-environment?
         execution))

    (is (= {:kind :environment
            :role :browser
            :events
            #{:request/failed
              :request/completed}}
           (machine/awaiting
            execution)))

    (testing "same event for another role is not accepted"
      (let [wrong-role
            (machine/environment-event
             :authority
             :request/completed)]

        (is (false?
             (machine/accepts-environment-event?
              execution
              wrong-role)))

        (is (= :environment-event-not-enabled
               (error-kind
                #(machine/resume-environment
                  execution
                  wrong-role))))))

    (testing "declared event for this role advances"
      (let [event
            (machine/environment-event
             :browser
             :request/completed)

            completed
            (machine/resume-environment
             execution
             event)]

        (is (machine/completed?
             completed))

        (is (= {:outcome :gesso.choreo/complete}
               (machine/result completed)))

        (is (= [{:kind :environment
                 :state 0
                 :role :browser
                 :event :request/completed
                 :data nil}

                {:kind :terminal
                 :state (machine/current-state-id completed)
                 :outcome :gesso.choreo/complete}]
               (machine/execution-history
                completed)))))))

(deftest multiple-incoming-communications-form-one-receive-gate
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :alice
            :bob
            :example/one
            :done
            {:required #{:branch}})

           :two
           (choreo/communicate
            :alice
            :bob
            :example/two
            :done
            {:required #{:branch}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :bob
         {:alice #{:branch}})

        one
        (machine/message
         :alice
         :bob
         :example/one
         {:branch 1})

        two
        (machine/message
         :alice
         :bob
         :example/two
         {:branch 2})]

    (is (machine/waiting-receive?
         execution))

    (is (= #{:example/one
             :example/two}
           (set
            (map
             :event
             (get-in execution
                     [:awaiting
                      :alternatives])))))

    (is (machine/accepts-message?
         execution
         one))

    (is (machine/accepts-message?
         execution
         two))))

(deftest wrong-participant-message-is-rejected
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :bob)]

    (doseq [envelope
            [(machine/message
              :carol
              :bob
              :example/hello
              {})

             (machine/message
              :alice
              :bob
              :example/other
              {})

             (machine/message
              :alice
              :carol
              :example/hello
              {})]]

      (is (false?
           (machine/accepts-message?
            execution
            envelope)))

      (is (= :message-not-enabled
             (error-kind
              #(machine/receive
                execution
                envelope)))))))

(deftest via-participates-in-message-identity
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done
            {:via :http})

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :bob)]

    (is
     (machine/accepts-message?
      execution
      (machine/message
       :alice
       :bob
       :example/hello
       {}
       {:via :http})))

    (is
     (false?
      (machine/accepts-message?
       execution
       (machine/message
        :alice
        :bob
        :example/hello
        {}
        {:via :sse}))))))

(deftest completion-is-terminal
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})

        ;; A choreography containing no roles has nothing to project. Use one
        ;; role by placing an unreachable local state in the authored program.
        choreography-with-role
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)

           :unused
           (choreo/local
            :browser
            :unused
            :done)}})

        execution
        (start-role
         choreography-with-role
         :browser)]

    (is (machine/completed?
         execution))

    (is (= {:outcome :gesso.choreo/complete}
           (machine/result execution)))

    (is (= :not-suspended
           (error-kind
            #(machine/resume
              execution
              (machine/environment-event
               :browser
               :anything)))))))

(deftest boundary-completion-functions-are-state-specific
  (let [local-choreography
        (choreo/->choreography
         {:initial :local
          :states
          {:local
           (choreo/local
            :browser
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        local-execution
        (start-role
         local-choreography
         :browser)

        send-choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :authority
            :example/send
            :done)

           :done
           (choreo/return :done)}})

        send-execution
        (start-role
         send-choreography
         :browser)]

    (is (= :not-waiting-send
           (error-kind
            #(machine/complete-send
              local-execution
              {}))))

    (is (= :not-waiting-local
           (error-kind
            #(machine/complete-local
              send-execution))))))

(deftest deterministic-history-has-no-wall-clock-data
  (let [choreography
        (choreo/->choreography
         {:initial :local
          :states
          {:local
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :example/send
            :done
            {:required #{:x}})

           :done
           (choreo/return :done)}})

        first-run
        (-> (start-role-with-entry-knowledge
             choreography
             :browser
             {:browser #{:x}}
             {:x 1})
            machine/complete-local
            (machine/complete-send
             {:x 1})
            :execution)

        second-run
        (-> (start-role-with-entry-knowledge
             choreography
             :browser
             {:browser #{:x}}
             {:x 1})
            machine/complete-local
            (machine/complete-send
             {:x 1})
            :execution)]

    (is (= (machine/execution-history
            first-run)
           (machine/execution-history
            second-run)))

    (is (every?
         #(not (contains? %
                         :at))
         (machine/execution-history
          first-run)))))

(deftest execution-id-is-correlation-not-semantic-state
  (let [choreography
        (choreo/->choreography
         {:initial :local
          :states
          {:local
           (choreo/local
            :browser
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        plan
        (projected
         choreography
         :browser)

        execution-id
        (identity/execution-id
         :execution/example)

        execution
        (machine/start
         plan
         {:execution-id
          execution-id})]

    (is (= execution-id
           (:execution-id execution)))

    (is (= execution-id
           (:execution-id
            (machine/pending-action
             execution))))

    (is (= [{:kind :local
             :state 0
             :role :browser
             :action :prepare}

            {:kind :terminal
             :state (-> execution
                        machine/complete-local
                        machine/current-state-id)
             :outcome :gesso.choreo/complete}]
           (-> execution
               machine/complete-local
               machine/execution-history)))))

(deftest projected-completion-does-not-claim-global-terminal-outcome
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/confirmed :confirmed
             :request/rejected :rejected})

           :confirmed
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        execution
        (start-role
         choreography
         :browser)

        confirmed
        (machine/resume-environment
         execution
         (machine/environment-event
          :browser
          :request/confirmed))

        rejected
        (machine/resume-environment
         execution
         (machine/environment-event
          :browser
          :request/rejected))]

    (testing "role-local completion means no further local protocol work"
      (is (= {:outcome :gesso.choreo/complete}
             (machine/result confirmed)))

      (is (= {:outcome :gesso.choreo/complete}
             (machine/result rejected))))

    (testing "a projected machine does not assert that the global choreography has terminated"
      (is (not= :confirmed
                (get-in confirmed
                        [:result :outcome])))

      (is (not= :rejected
                (get-in rejected
                        [:result :outcome]))))))

(deftest executable-plan-state-identities-are-runtime-locators
  (let [plan
        {:gesso.choreo/type
         :gesso.choreo/executable-plan

         :gesso.choreo/version
         1

         :name
         :example/runtime-locators

         :role
         :browser

         :initial
         0

         :states
         {0
          {:op :local
           :action :prepare
           :next 1}

          1
          {:op :return
           :outcome :done}}}

        execution
        (machine/start plan)

        completed
        (machine/complete-local
         execution)]

    (is (= 0
           (machine/current-state-id
            execution)))

    (is (= 1
           (machine/current-state-id
            completed)))

    (is (machine/completed?
         completed))))

(deftest local-action-receives-only-declared-required-inputs
  (let [choreography
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute-outcome
            :done
            {:requires #{:request-id :attempt}
             :outputs #{:outcome}})

           :done
           (choreo/return :done)}})

        execution
        (machine/start
         (projected-with-entry-values
          choreography
          :browser
          #{:request-id :attempt})
         {:values {:request-id 42
                   :attempt 3
                   :unrelated :must-not-be-exposed}})]

    (is (machine/waiting-local? execution))

    (is (= {:kind :local
            :execution-id nil
            :state 0
            :role :browser
            :action :compute-outcome
            :inputs {:request-id 42
                     :attempt 3}
            :outputs #{:outcome}}
           (machine/pending-action execution)))

    (is (= {:request-id 42
            :attempt 3
            :unrelated :must-not-be-exposed}
           (machine/execution-values execution)))))

(deftest missing-required-local-input-is-rejected-before-endpoint-action
  (let [plan
        {:gesso.choreo/type :gesso.choreo/executable-plan
         :gesso.choreo/version 1
         :role :browser
         :initial 0
         :states
         {0
          {:op :local
           :action :compute
           :requires #{:request-id :attempt}
           :outputs #{:outcome}
           :next 1}

          1
          {:op :return
           :outcome :done}}}]

    (is (= :missing-local-inputs
           (error-kind
            #(machine/start
              plan
              {:values {:request-id 42}}))))))

(deftest local-output-contract-is-closed
  (let [choreography
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute
            :done
            {:outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        execution
        (start-role choreography :browser)]

    (testing "missing declared output is rejected"
      (is (= :local-output-mismatch
             (error-kind
              #(machine/complete-local
                execution
                {:outcome :confirmed})))))

    (testing "undeclared output is rejected"
      (is (= :local-output-mismatch
             (error-kind
              #(machine/complete-local
                execution
                {:outcome :confirmed
                 :revision 4
                 :secret :leak})))))

    (testing "exact declared output set is accepted"
      (let [completed
            (machine/complete-local
             execution
             {:outcome :confirmed
              :revision 4})]

        (is (machine/completed? completed))
        (is (= :confirmed
               (machine/execution-value completed :outcome)))
        (is (= 4
               (machine/execution-value completed :revision)))
        (is (false?
             (machine/has-execution-value? completed :secret)))))))

(deftest local-output-overwrites-the-same-semantic-value-deliberately
  (let [choreography
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute
            :done
            {:requires #{:outcome}
             :outputs #{:outcome}})

           :done
           (choreo/return :done)}})

        execution
        (machine/start
         (projected-with-entry-values
          choreography
          :browser
          #{:outcome})
         {:values {:outcome :old}})

        completed
        (machine/complete-local
         execution
         {:outcome :new})]

    (is (= {:outcome :old}
           (:inputs
            (machine/pending-action execution))))

    (is (= :new
           (machine/execution-value completed :outcome)))))

(deftest branch-is-immediate-and-selected-from-role-local-value
  (let [choreography
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute-outcome
            :decide
            {:outputs #{:outcome}})

           :decide
           (choreo/branch
            :browser
            :outcome
            {:confirmed :send-confirmed
             :rejected :send-rejected})

           :send-confirmed
           (choreo/communicate
            :browser
            :server
            :example/confirmed
            :done)

           :send-rejected
           (choreo/communicate
            :browser
            :server
            :example/rejected
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role choreography :browser)

        confirmed
        (machine/complete-local
         execution
         {:outcome :confirmed})]

    (testing "complete-local traverses branch without another endpoint callback"
      (is (machine/waiting-send? confirmed))
      (is (= :example/confirmed
             (:event
              (machine/pending-action confirmed)))))

    (testing "branch is retained in deterministic history"
      (is (= [{:kind :local
               :state 0
               :role :browser
               :action :compute-outcome
               :outputs {:outcome :confirmed}}

              {:kind :branch
               :state 1
               :role :browser
               :on :outcome
               :value :confirmed}]
             (machine/execution-history confirmed))))))

(deftest branch-can-start-from-explicit-role-local-input
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/branch
            :browser
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/local
            :browser
            :show-confirmed
            :done)

           :rejected
           (choreo/local
            :browser
            :show-rejected
            :done)

           :done
           (choreo/return :done)}})

        execution
        (machine/start
         (projected-with-entry-values
          choreography
          :browser
          #{:outcome})
         {:values {:outcome :rejected}})]

    (is (machine/waiting-local? execution))
    (is (= :show-rejected
           (:action
            (machine/pending-action execution))))
    (is (= {:kind :branch
            :state 0
            :role :browser
            :on :outcome
            :value :rejected}
           (first
            (machine/execution-history execution))))))

(deftest branch-with-uncovered-value-fails-before-endpoint-action
  (let [plan
        {:gesso.choreo/type :gesso.choreo/executable-plan
         :gesso.choreo/version 1
         :role :browser
         :initial 0
         :states
         {0
          {:op :branch
           :on :outcome
           :cases {:confirmed 1}}

          1
          {:op :return
           :outcome :done}}}]

    (is (= :branch-value-not-covered
           (error-kind
            #(machine/start
              plan
              {:values {:outcome :rejected}}))))))

(deftest branch-with-missing-value-fails-before-endpoint-action
  (let [plan
        {:gesso.choreo/type :gesso.choreo/executable-plan
         :gesso.choreo/version 1
         :role :browser
         :initial 0
         :states
         {0
          {:op :branch
           :on :outcome
           :cases {:confirmed 1}}

          1
          {:op :return
           :outcome :done}}}]

    (is (= :missing-branch-value
           (error-kind
            #(machine/start plan))))))

(deftest immediate-branch-loop-is-bounded
  (let [plan
        {:gesso.choreo/type :gesso.choreo/executable-plan
         :gesso.choreo/version 1
         :role :browser
         :initial 0
         :states
         {0
          {:op :branch
           :on :again?
           :cases {true 0
                   false 1}}

          1
          {:op :return
           :outcome :done}}}]

    (is (= :immediate-step-limit
           (error-kind
            #(machine/start
              plan
              {:values {:again? true}
               :max-immediate-steps 8}))))))


(deftest authoritative-operation-is-an-explicit-trusted-endpoint-boundary
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :authority
            :request/claim
            :done
            {:requires #{:request-id :actor-id}
             :outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        plan
        (projected-with-entry-values
         choreography
         :authority
         #{:request-id :actor-id})

        execution-id
        (identity/execution-id
         :execution/claim-1)

        execution
        (machine/start
         plan
         {:execution-id execution-id
          :values {:request-id 17
                   :actor-id 9}})]

    (is (machine/waiting-authoritative?
         execution))

    (is (= {:kind :authoritative
            :execution-id execution-id
            :identity-bindings
            {:role :authority
             :execution-id execution-id}
            :state 0
            :role :authority
            :operation :request/claim
            :inputs {:request-id 17
                     :actor-id 9}
            :outputs #{:outcome :revision}}
           (machine/pending-action
            execution)))

    (is (= {:request-id 17
            :actor-id 9}
           (machine/execution-values
            execution)))))

(deftest authoritative-completion-establishes-only-declared-values
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :authority
            :request/claim
            :branch
            {:requires #{:request-id}
             :outputs #{:outcome :revision}})

           :branch
           (choreo/branch
            :authority
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        plan
        (projected-with-entry-values
         choreography
         :authority
         #{:request-id})

        execution
        (machine/start
         plan
         {:values {:request-id 17}})

        completed
        (machine/complete-authoritative
         execution
         {:outcome :confirmed
          :revision 42})]

    (is (machine/completed?
         completed))

    (is (= {:request-id 17
            :outcome :confirmed
            :revision 42}
           (machine/execution-values
            completed)))

    (is (= [{:kind :authoritative
             :state 0
             :role :authority
             :operation :request/claim
             :outputs {:outcome :confirmed
                       :revision 42}}

            {:kind :branch
             :state 1
             :role :authority
             :on :outcome
             :value :confirmed}

            {:kind :terminal
             :state 2
             :outcome :gesso.choreo/complete}]
           (machine/execution-history
            completed)))))

(deftest authoritative-boundary-rejects-missing-runtime-inputs
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :authority
            :request/claim
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        plan
        (projected-with-entry-values
         choreography
         :authority
         #{:request-id})]

    (is (= :missing-authoritative-inputs
           (error-kind
            #(machine/start
              plan))))))

(deftest authoritative-output-contract-is-closed
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :authority
            :request/claim
            :done
            {:outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :authority)]

    (testing "missing declared output is rejected"
      (is (= :authoritative-output-mismatch
             (error-kind
              #(machine/complete-authoritative
                execution
                {:outcome :confirmed})))))

    (testing "undeclared extra output is rejected"
      (is (= :authoritative-output-mismatch
             (error-kind
              #(machine/complete-authoritative
                execution
                {:outcome :confirmed
                 :revision 42
                 :canonical :unexpected})))))

    (testing "the exact declared output set succeeds"
      (is (machine/completed?
           (machine/complete-authoritative
            execution
            {:outcome :confirmed
             :revision 42}))))))

(deftest authoritative-operation-with-no-outputs-has-a-zero-argument-completion-boundary
  (let [choreography
        (choreo/->choreography
         {:initial :touch
          :states
          {:touch
           (choreo/authoritative
            :authority
            :request/touch
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :authority)

        completed
        (machine/complete-authoritative
         execution)]

    (is (machine/completed?
         completed))

    (is (= [{:kind :authoritative
             :state 0
             :role :authority
             :operation :request/touch}

            {:kind :terminal
             :state 1
             :outcome :gesso.choreo/complete}]
           (machine/execution-history
            completed)))))

(deftest authoritative-completion-is-specific-to-authoritative-boundaries
  (let [local-choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :authority
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        local-execution
        (start-role
         local-choreography
         :authority)

        authoritative-choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :authority
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        authoritative-execution
        (start-role
         authoritative-choreography
         :authority)]

    (is (= :not-waiting-authoritative
           (error-kind
            #(machine/complete-authoritative
              local-execution))))

    (is (= :not-waiting-local
           (error-kind
            #(machine/complete-local
              authoritative-execution))))))

(deftest outbound-send-enforces-closed-payload-contract
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}
             :optional #{:base-revision}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id :base-revision}}
         {:execution-id "execution-1"
          :base-revision 42})]

    (testing "missing required fields are rejected before an envelope exists"
      (is
       (= :invalid-message-payload
          (error-kind
           #(machine/pending-message
             execution
             {}))))

      (is
       (= :invalid-message-payload
          (error-kind
           #(machine/complete-send
             execution
             {})))))

    (testing "undeclared fields are rejected"
      (is
       (= :invalid-message-payload
          (error-kind
           #(machine/pending-message
             execution
             {:execution-id "execution-1"
              :surprise true})))))

    (testing "required plus declared optional fields are accepted"
      (is
       (= {:kind :message
           :from :browser
           :to :server
           :event :example/command
           :payload {:execution-id "execution-1"
                     :base-revision 42}}
          (machine/pending-message
           execution
           {:execution-id "execution-1"
            :base-revision 42}))))))

(deftest explicitly-open-send-still-requires-required-fields
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/open-command
            :done
            {:required #{:execution-id}
             :open-payload? true})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}}
         {:execution-id "execution-1"})]

    (is
     (= :invalid-message-payload
        (error-kind
         #(machine/pending-message
           execution
           {:extra true}))))

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :example/open-command
         :payload {:execution-id "execution-1"
                   :extra true}}
        (machine/pending-message
         execution
         {:execution-id "execution-1"
          :extra true})))))

(deftest receive-rejects-identity-match-with-invalid-payload
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :outcome}
             :optional #{:revision}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:server #{:execution-id :outcome}})

        missing-required
        (machine/message
         :server
         :browser
         :example/result
         {:execution-id "execution-1"})

        undeclared
        (machine/message
         :server
         :browser
         :example/result
         {:execution-id "execution-1"
          :outcome :confirmed
          :surprise true})]

    (doseq [envelope
            [missing-required
             undeclared]]

      (is
       (false?
        (machine/accepts-message?
         execution
         envelope)))

      (is
       (= :message-not-enabled
          (error-kind
           #(machine/receive
             execution
             envelope)))))))

(deftest receive-contract-disambiguates-same-sender-and-event
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/confirmed :confirmed
             :environment/rejected :rejected})

           :confirmed
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :confirmed}})

           :rejected
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :rejected}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:server #{:execution-id :confirmed :rejected}})

        confirmed
        (machine/message
         :server
         :browser
         :example/result
         {:execution-id "execution-1"
          :confirmed true})

        rejected
        (machine/message
         :server
         :browser
         :example/result
         {:execution-id "execution-1"
          :rejected true})]

    (is
     (machine/accepts-message?
      execution
      confirmed))

    (is
     (machine/accepts-message?
      execution
      rejected))

    (let [confirmed-execution
          (machine/receive
           execution
           confirmed)

          rejected-execution
          (machine/receive
           execution
           rejected)]

      (is
       (machine/completed?
        confirmed-execution))

      (is
       (machine/completed?
        rejected-execution))

      (is
       (= {:execution-id "execution-1"
           :confirmed true}
          (:payload
           (first
            (machine/execution-history
             confirmed-execution)))))

      (is
       (= {:execution-id "execution-1"
           :rejected true}
          (:payload
           (first
            (machine/execution-history
             rejected-execution))))))))

(deftest overlapping-receive-contracts-are-rejected-as-ambiguous
  ;; The projector now rejects statically overlapping receive alternatives.
  ;; This hand-constructed malformed ExecutablePlan keeps the machine's
  ;; ambiguity check as defense in depth for invalid/untrusted plan data.
  (let [plan
        {:gesso.choreo/type project/executable-plan-type
         :gesso.choreo/version project/executable-plan-version
         :role :browser
         :initial 0
         :states
         {0
          {:op :receive
           :alternatives
           [{:from :server
             :event :example/result
             :required #{:execution-id}
             :optional #{:detail}
             :next 1}

            {:from :server
             :event :example/result
             :required #{:execution-id}
             :optional #{:other}
             :next 1}]}

          1
          {:op :return
           :outcome :gesso.choreo/complete}}}

        execution
        (machine/start plan)

        envelope
        (machine/message
         :server
         :browser
         :example/result
         {:execution-id "execution-1"})]

    (is
     (false?
      (machine/accepts-message?
       execution
       envelope)))

    (is
     (= :ambiguous-message
        (error-kind
         #(machine/receive
           execution
           envelope))))))

(deftest receive-rejects-non-map-message-payload-even-for-adversarial-envelope
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:open-payload? true})

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :browser)

        adversarial-envelope
        {:kind :message
         :from :server
         :to :browser
         :event :example/result
         :payload [:not-a-map]}]

    (is
     (false?
      (machine/accepts-message?
       execution
       adversarial-envelope)))

    (is
     (= :invalid-message
        (error-kind
         #(machine/receive
           execution
           adversarial-envelope))))))

(deftest pending-send-and-receive-descriptors-expose-message-contracts
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:via :http
             :required #{:execution-id :scope}
             :optional #{:base-revision}
             :correlation #{:execution-id :scope}
             :open-payload? true})

           :done
           (choreo/return :done)}})

        sender
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id :scope}}
         {:execution-id "execution-1"
          :scope :request/example})

        receiver
        (start-role-with-entry-knowledge
         choreography
         :server
         {:browser #{:execution-id :scope}})]

    (is
     (= {:kind :send
         :execution-id nil
         :state 0
         :from :browser
         :to :server
         :event :example/command
         :via :http
         :required #{:execution-id :scope}
         :optional #{:base-revision}
         :correlation #{:execution-id :scope}
         :open-payload? true}
        (machine/pending-action
         sender)))

    (is
     (= {:kind :receive
         :role :server
         :alternatives
         [{:from :browser
           :event :example/command
           :via :http
           :required #{:execution-id :scope}
           :optional #{:base-revision}
           :correlation #{:execution-id :scope}
           :open-payload? true}]}
        (machine/awaiting
         receiver)))))

(deftest machine-start-defensively-rejects-malformed-executable-send-contract
  (let [plan
        {:gesso.choreo/type
         :gesso.choreo/executable-plan

         :gesso.choreo/version
         1

         :role
         :browser

         :initial
         0

         :states
         {0
          {:op :send
           :to :server
           :event :example/command
           :required #{:execution-id}
           :optional #{:execution-id}
           :next 1}

          1
          {:op :return
           :outcome :gesso.choreo/complete}}}]

    (is
     (= :ambiguous-message-key
        (error-kind
         #(machine/start
           plan))))))

(deftest machine-start-defensively-rejects-malformed-executable-receive-contract
  (let [plan
        {:gesso.choreo/type
         :gesso.choreo/executable-plan

         :gesso.choreo/version
         1

         :role
         :browser

         :initial
         0

         :states
         {0
          {:op :receive
           :alternatives
           [{:from :server
             :event :example/result
             :required #{:execution-id}
             :optional #{:scope}
             :correlation #{:execution-id :scope}
             :next 1}]}

          1
          {:op :return
           :outcome :gesso.choreo/complete}}}]

    (is
     (= :optional-correlation-key
        (error-kind
         #(machine/start
           plan))))))

(deftest receive-correlation-must-match-existing-role-local-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id :scope :payload}
             :correlation #{:execution-id :scope}})

           :done
           (choreo/return :done)}})

        execution-id
        (identity/execution-id
         "execution-1")

        waiting
        (start-role-with-entry-knowledge
         choreography
         :server
         {:browser #{:execution-id :scope :payload}
          :server #{:execution-id :scope}}
         {:execution-id execution-id
          :scope :request/example})

        matching
        (machine/message
         :browser
         :server
         :example/command
         {:execution-id execution-id
          :scope :request/example
          :payload :claim})

        wrong-execution
        (machine/message
         :browser
         :server
         :example/command
         {:execution-id
          (identity/execution-id
           "execution-2")
          :scope :request/example
          :payload :claim})

        wrong-scope
        (machine/message
         :browser
         :server
         :example/command
         {:execution-id execution-id
          :scope :request/other
          :payload :claim})]

    (testing "a receive accepts the declared message only when every correlation value agrees with knowledge established before the receive"
      (is
       (machine/accepts-message?
        waiting
        matching))

      (is
       (false?
        (machine/accepts-message?
         waiting
         wrong-execution)))

      (is
       (false?
        (machine/accepts-message?
         waiting
         wrong-scope))))

    (testing "a wrong correlation value is not rescued merely because the message route and payload shape are otherwise valid"
      (is
       (= :message-not-enabled
          (error-kind
           #(machine/receive
             waiting
             wrong-execution))))

      (is
       (= :message-not-enabled
          (error-kind
           #(machine/receive
             waiting
             wrong-scope)))))

    (testing "matching correlation values remain ordinary declared semantic fields after the receive"
      (let [completed
            (machine/receive
             waiting
             matching)]
        (is
         (= execution-id
            (machine/execution-value
             completed
             :execution-id)))

        (is
         (= :request/example
            (machine/execution-value
             completed
             :scope)))

        (is
         (= :claim
            (machine/execution-value
             completed
             :payload)))))))


(deftest receive-correlation-honors-explicit-execution-identity-binding
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id :payload}
             :correlation #{:execution-id}})

           :done
           (choreo/return :done)}})

        execution-id
        (identity/execution-id
         "execution-1")

        waiting
        (machine/start
         (projected-with-entry-knowledge
          choreography
          :server
          {:browser #{:execution-id :payload}})
         {:execution-id execution-id})

        matching
        (machine/message
         :browser
         :server
         :example/command
         {:execution-id execution-id
          :payload :claim})

        wrong-execution
        (machine/message
         :browser
         :server
         :example/command
         {:execution-id
          (identity/execution-id
           "execution-2")
          :payload :claim})]

    (testing "identity bindings constrain correlation without becoming semantic values by themselves"
      (is
       (= execution-id
          (:execution-id waiting)))

      (is
       (= execution-id
          (:execution-id
           (machine/identity-bindings
            waiting))))

      (is
       (false?
        (machine/has-execution-value?
         waiting
         :execution-id)))

      (is
       (machine/accepts-message?
        waiting
        matching))

      (is
       (false?
        (machine/accepts-message?
         waiting
         wrong-execution)))

      (is
       (= :message-not-enabled
          (error-kind
           #(machine/receive
             waiting
             wrong-execution)))))

    (testing "once explicitly declared on the wire, the accepted correlation field becomes ordinary communicated semantic knowledge"
      (let [completed
            (machine/receive
             waiting
             matching)]
        (is
         (= execution-id
            (machine/execution-value
             completed
             :execution-id)))

        (is
         (= #{:communicated}
            (machine/execution-provenance-kinds
             completed
             :execution-id)))))))

(deftest receiving-valid-declared-payload-establishes-communicated-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        waiting
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:server #{:outcome :revision}})

        receive-state-id
        (machine/current-state-id
         waiting)

        completed
        (machine/receive
         waiting
         (machine/message
          :server
          :browser
          :example/result
          {:outcome :confirmed
           :revision 42}))]

    (is
     (= {:outcome :confirmed
         :revision 42}
        (machine/execution-values
         completed)))

    (is
     (machine/has-execution-value?
      completed
      :outcome))

    (is
     (machine/has-execution-value?
      completed
      :revision))

    (is
     (= #{:communicated}
        (machine/execution-provenance-kinds
         completed
         :outcome)))

    (is
     (= [{:kind :communicated
          :from :server
          :event :example/result
          :state receive-state-id}]
        (machine/execution-provenance
         completed
         :revision)))))

(deftest start-values-become-input-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :done
            {:requires #{:request-id :base-revision}})

           :done
           (choreo/return :done)}})

        execution
        (machine/start
         (projected-with-entry-values
          choreography
          :browser
          #{:request-id :base-revision})
         {:values
          {:request-id "request-1"
           :base-revision 41}})]

    (is
     (= {:request-id "request-1"
         :base-revision 41}
        (machine/execution-values
         execution)))

    (is
     (= #{:input}
        (machine/execution-provenance-kinds
         execution
         :request-id)))

    (is
     (= [{:kind :input}]
        (machine/execution-provenance
         execution
         :base-revision)))))

(deftest local-outputs-become-asserted-knowledge-not-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare-command
            :done
            {:outputs #{:prepared?}})

           :done
           (choreo/return :done)}})

        completed
        (machine/complete-local
         (start-role
          choreography
          :browser)
         {:prepared? true})]

    (is
     (= true
        (machine/execution-value
         completed
         :prepared?)))

    (is
     (= #{:asserted}
        (machine/execution-provenance-kinds
         completed
         :prepared?)))

    (is
     (= [{:kind :asserted
          :source :prepare-command
          :metadata {:state 0}}]
        (machine/execution-provenance
         completed
         :prepared?)))

    (is
     (not
      (contains?
       (machine/execution-provenance-kinds
        completed
        :prepared?)
       :authoritative)))))

(deftest authoritative-outputs-become-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        completed
        (machine/complete-authoritative
         (start-role
          choreography
          :server)
         {:outcome :confirmed
          :revision 42})]

    (is
     (= :confirmed
        (machine/execution-value
         completed
         :outcome)))

    (is
     (= #{:authoritative}
        (machine/execution-provenance-kinds
         completed
         :outcome)))

    (is
     (= [{:kind :authoritative
          :operation :request/claim
          :state 0}]
        (machine/execution-provenance
         completed
         :revision)))))

(deftest open-message-extra-fields-remain-transport-only
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:outcome}
             :optional #{:revision}
             :open-payload? true})

           :done
           (choreo/return :done)}})

        completed
        (machine/receive
         (start-role-with-entry-knowledge
          choreography
          :browser
          {:server #{:outcome :revision}})
         (machine/message
          :server
          :browser
          :example/result
          {:outcome :confirmed
           :revision 42
           :transport-debug "debug"
           :future-field :opaque}))]

    (is
     (= {:outcome :confirmed
         :revision 42}
        (machine/execution-values
         completed)))

    (is
     (false?
      (machine/has-execution-value?
       completed
       :transport-debug)))

    (is
     (false?
      (machine/has-execution-value?
       completed
       :future-field)))

    (is
     (nil?
      (machine/execution-provenance
       completed
       :transport-debug)))))

(deftest communicated-value-can-drive-a-local-branch
  (let [plan
        {:gesso.choreo/type :gesso.choreo/executable-plan
         :gesso.choreo/version 1
         :role :browser
         :initial 0
         :states
         {0
          {:op :receive
           :alternatives
           [{:from :server
             :event :request/settled
             :required #{:outcome}
             :next 1}]}

          1
          {:op :branch
           :on :outcome
           :cases
           {:confirmed 2
            :rejected 3}}

          2
          {:op :local
           :action :install-canonical
           :next 4}

          3
          {:op :local
           :action :restore-snapshot
           :next 4}

          4
          {:op :return
           :outcome :gesso.choreo/complete}}}

        waiting
        (machine/start plan)

        after-receive
        (machine/receive
         waiting
         (machine/message
          :server
          :browser
          :request/settled
          {:outcome :confirmed}))]

    ;; This is intentionally an ExecutablePlan-level machine test. The current
    ;; verifier still treats communication as producing no semantic values, so
    ;; choreography-level receive->branch is the next verifier/knowledge step.
    (is
     (machine/waiting-local?
      after-receive))

    (is
     (= :install-canonical
        (:action
         (machine/pending-action
          after-receive))))

    (is
     (= :confirmed
        (machine/execution-value
         after-receive
         :outcome)))

    (is
     (= #{:communicated}
        (machine/execution-provenance-kinds
         after-receive
         :outcome)))))

(deftest authoritative-value-can-drive-a-local-branch-with-authoritative-provenance
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/local
            :server
            :record-confirmed
            :done)

           :rejected
           (choreo/local
            :server
            :record-rejected
            :done)

           :done
           (choreo/return :done)}})

        after-authority
        (machine/complete-authoritative
         (start-role
          choreography
          :server)
         {:outcome :rejected})]

    (is
     (machine/waiting-local?
      after-authority))

    (is
     (= :record-rejected
        (:action
         (machine/pending-action
          after-authority))))

    (is
     (= #{:authoritative}
        (machine/execution-provenance-kinds
         after-authority
         :outcome)))))

(deftest conflicting-authoritative-assignment-requires-progression
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:requires #{:revision}
             :outputs #{:revision}})

           :done
           (choreo/return :done)}})

        started
        (machine/start
         (projected-with-entry-values
          choreography
          :server
          #{:revision})
         {:values {:revision 41}})]

    (testing "the same authoritative value may add justification without progression"
      (let [completed
            (machine/complete-authoritative
             started
             {:revision 41})]

        (is
         (= 41
            (machine/execution-value
             completed
             :revision)))

        (is
         (= #{:input
              :authoritative}
            (machine/execution-provenance-kinds
             completed
             :revision)))))

    (testing "arrival order alone cannot authorize a conflicting authoritative overwrite"
      (is
       (= :authoritative-progression-required
          (error-kind
           #(machine/complete-authoritative
             started
             {:revision 42}))))

      (is
       (= 41
          (machine/execution-value
           started
           :revision)))

      (is
       (= #{:input}
          (machine/execution-provenance-kinds
           started
           :revision))))))

(deftest explain-surfaces-provenance-kinds-without-exposing-a-second-value-store
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :done
            {:requires #{:request-id}
             :outputs #{:prepared?}})

           :done
           (choreo/return :done)}})

        started
        (machine/start
         (projected-with-entry-values
          choreography
          :browser
          #{:request-id})
         {:values {:request-id "request-1"}})

        completed
        (machine/complete-local
         started
         {:prepared? true})

        explanation
        (machine/explain
         completed)]

    (is
     (= #{:request-id :prepared?}
        (:value-keys explanation)))

    (is
     (= {:request-id #{:input}
         :prepared? #{:asserted}}
        (:provenance-kinds-by-key
         explanation)))

    (is
     (= {:request-id "request-1"
         :prepared? true}
        (machine/execution-values
         completed)))))

(deftest runtime-send-rejects-required-field-that-plan-assumed-but-execution-does-not-know
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}})

           :done
           (choreo/return :done)}})

        plan
        (projected-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}})

        ;; The plan was verified under an entry-knowledge assumption, but this
        ;; concrete execution deliberately violates that assumption.
        execution
        (machine/start plan)]

    (is
     (= :invalid-message-knowledge
        (error-kind
         #(machine/pending-message
           execution
           {:execution-id "execution-1"}))))

    (is
     (= :invalid-message-knowledge
        (error-kind
         #(machine/complete-send
           execution
           {:execution-id "execution-1"}))))

    (is
     (machine/waiting-send?
      execution))

    (is
     (false?
      (machine/has-execution-value?
       execution
       :execution-id)))))

(deftest runtime-send-rejects-required-field-whose-value-disagrees-with-sender-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}}
         {:execution-id "execution-1"})]

    (is
     (= :invalid-message-knowledge
        (error-kind
         #(machine/pending-message
           execution
           {:execution-id "execution-2"}))))

    (is
     (= "execution-1"
        (machine/execution-value
         execution
         :execution-id)))

    (is
     (= #{:input}
        (machine/execution-provenance-kinds
         execution
         :execution-id)))))

(deftest runtime-send-accepts-required-field-that-exactly-matches-sender-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}}
         {:execution-id "execution-1"})]

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :example/command
         :payload {:execution-id "execution-1"}}
        (machine/pending-message
         execution
         {:execution-id "execution-1"})))))

(deftest runtime-send-checks-optional-semantic-field-when-it-is-actually-present
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}
             :optional #{:base-revision}})

           :done
           (choreo/return :done)}})]

    (testing "an omitted optional field needs no sender knowledge"
      (let [execution
            (start-role-with-entry-knowledge
             choreography
             :browser
             {:browser #{:execution-id}}
             {:execution-id "execution-1"})]

        (is
         (= {:kind :message
             :from :browser
             :to :server
             :event :example/command
             :payload {:execution-id "execution-1"}}
            (machine/pending-message
             execution
             {:execution-id "execution-1"})))))

    (testing "a present optional field may not be invented"
      (let [execution
            (start-role-with-entry-knowledge
             choreography
             :browser
             {:browser #{:execution-id}}
             {:execution-id "execution-1"})]

        (is
         (= :invalid-message-knowledge
            (error-kind
             #(machine/pending-message
               execution
               {:execution-id "execution-1"
                :base-revision 42}))))))

    (testing "a present optional field must equal the sender's known value"
      (let [execution
            (start-role-with-entry-knowledge
             choreography
             :browser
             {:browser #{:execution-id
                         :base-revision}}
             {:execution-id "execution-1"
              :base-revision 41})]

        (is
         (= :invalid-message-knowledge
            (error-kind
             #(machine/pending-message
               execution
               {:execution-id "execution-1"
                :base-revision 42}))))

        (is
         (= {:kind :message
             :from :browser
             :to :server
             :event :example/command
             :payload {:execution-id "execution-1"
                       :base-revision 41}}
            (machine/pending-message
             execution
             {:execution-id "execution-1"
              :base-revision 41})))))))

(deftest runtime-send-does-not-promote-open-undeclared-fields-into-semantic-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/open-command
            :done
            {:required #{:execution-id}
             :open-payload? true})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}}
         {:execution-id "execution-1"})

        envelope
        (machine/pending-message
         execution
         {:execution-id "execution-1"
          :transport-debug "debug-only"
          :future-field {:opaque true}})]

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :example/open-command
         :payload {:execution-id "execution-1"
                   :transport-debug "debug-only"
                   :future-field {:opaque true}}}
        envelope))

    (is
     (false?
      (machine/has-execution-value?
       execution
       :transport-debug)))

    (is
     (false?
      (machine/has-execution-value?
       execution
       :future-field)))

    (is
     (nil?
      (machine/execution-provenance
       execution
       :transport-debug)))))

(deftest complete-send-enforces-sender-knowledge-before-advancing
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:execution-id}})

           :done
           (choreo/return :done)}})

        execution
        (start-role-with-entry-knowledge
         choreography
         :browser
         {:browser #{:execution-id}}
         {:execution-id "execution-1"})]

    (is
     (= :invalid-message-knowledge
        (error-kind
         #(machine/complete-send
           execution
           {:execution-id "execution-2"}))))

    (is
     (machine/waiting-send?
      execution))

    (let [{completed :execution
           envelope :message}
          (machine/complete-send
           execution
           {:execution-id "execution-1"})]

      (is
       (= {:kind :message
           :from :browser
           :to :server
           :event :example/command
           :payload {:execution-id "execution-1"}}
          envelope))

      (is
       (machine/completed?
        completed))

      (is
       (= "execution-1"
          (machine/execution-value
           completed
           :execution-id)))

      (is
       (= #{:input}
          (machine/execution-provenance-kinds
           completed
           :execution-id))))))

(deftest machine-start-keeps-command-and-execution-identities-distinct
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        raw
        "same-raw-id"

        command-id
        (identity/command-id raw)

        execution-id
        (identity/execution-id raw)

        execution
        (machine/start
         (projected
          choreography
          :browser)
         {:command-id command-id
          :execution-id execution-id})]

    (is
     (= command-id
        (machine/command-id
         execution)))

    (is
     (= execution-id
        (machine/execution-id
         execution)))

    (is
     (not=
      (machine/command-id execution)
      (machine/execution-id execution)))

    (is
     (= {:role :browser
         :command-id command-id
         :execution-id execution-id}
        (machine/identity-bindings
         execution)))))

(deftest machine-rejects-raw-command-and-execution-identifiers
  (let [choreography
        (choreo/->choreography
         {:initial :present
          :states
          {:present
           (choreo/local
            :browser
            :present-role
            :done)

           :done
           (choreo/return :done)}})

        plan
        (projected
         choreography
         :browser)]

    (is
     (= :invalid-command-id
        (error-kind
         #(machine/start
           plan
           {:command-id
            "command-1"}))))

    (is
     (= :invalid-execution-id
        (error-kind
         #(machine/start
           plan
           {:execution-id
            "execution-1"}))))))

(deftest machine-rejects-cross-kind-command-and-execution-identifiers
  (let [choreography
        (choreo/->choreography
         {:initial :present
          :states
          {:present
           (choreo/local
            :browser
            :present-role
            :done)

           :done
           (choreo/return :done)}})

        plan
        (projected
         choreography
         :browser)

        command-id
        (identity/command-id
         "id-1")

        execution-id
        (identity/execution-id
         "id-1")]

    (is
     (= :invalid-command-id
        (error-kind
         #(machine/start
           plan
           {:command-id
            execution-id}))))

    (is
     (= :invalid-execution-id
        (error-kind
         #(machine/start
           plan
           {:execution-id
            command-id}))))))

(deftest explicit-identity-binding-role-must-match-projected-role
  (let [choreography
        (choreo/->choreography
         {:initial :present
          :states
          {:present
           (choreo/local
            :browser
            :present-role
            :done)

           :done
           (choreo/return :done)}})

        plan
        (projected
         choreography
         :browser)]

    (is
     (= :identity-role-mismatch
        (error-kind
         #(machine/start
           plan
           {:identity-bindings
            {:role :server}}))))))

(deftest machine-identity-bindings-are-sparse-and-kind-checked
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :server
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        principal
        (identity/principal
         "user-1")

        actor
        (identity/actor
         "helper-9")

        authority
        (identity/authority
         :request-model)

        host
        (identity/host
         "aleph-node-2")

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        execution
        (machine/start
         (projected
          choreography
          :server)
         {:identity-bindings
          {:principal principal
           :actor actor
           :authority authority
           :host host
           :command-id command-id
           :execution-id execution-id}})]

    (is
     (= {:role :server
         :principal principal
         :actor actor
         :authority authority
         :host host
         :command-id command-id
         :execution-id execution-id}
        (machine/identity-bindings
         execution)))

    (is
     (= command-id
        (machine/command-id
         execution)))

    (is
     (= execution-id
        (machine/execution-id
         execution)))))

(deftest explicit-identity-options-must-agree-with-binding-map
  (let [choreography
        (choreo/->choreography
         {:initial :present
          :states
          {:present
           (choreo/local
            :server
            :present-role
            :done)

           :done
           (choreo/return :done)}})

        plan
        (projected
         choreography
         :server)

        command-1
        (identity/command-id
         "command-1")

        command-2
        (identity/command-id
         "command-2")

        execution-1
        (identity/execution-id
         "execution-1")

        execution-2
        (identity/execution-id
         "execution-2")]

    (is
     (= :identity-binding-conflict
        (error-kind
         #(machine/start
           plan
           {:identity-bindings
            {:command-id command-1}
            :command-id command-2}))))

    (is
     (= :identity-binding-conflict
        (error-kind
         #(machine/start
           plan
           {:identity-bindings
            {:execution-id execution-1}
            :execution-id execution-2}))))))

(deftest identity-bindings-survive-machine-transitions
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :send
            {:outputs #{:request-id}})

           :send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:request-id}})

           :done
           (choreo/return :done)}})

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        host
        (identity/host
         "browser-context-3")

        started
        (machine/start
         (projected
          choreography
          :browser)
         {:identity-bindings
          {:host host}
          :command-id command-id
          :execution-id execution-id})

        after-local
        (machine/complete-local
         started
         {:request-id "request-1"})

        {completed :execution
         envelope :message}
        (machine/complete-send
         after-local
         {:request-id "request-1"})]

    (doseq [execution
            [started
             after-local
             completed]]

      (is
       (= {:role :browser
           :host host
           :command-id command-id
           :execution-id execution-id}
          (machine/identity-bindings
           execution))))

    (is
     (= command-id
        (:command-id
         (machine/pending-action
          started))))

    (is
     (= execution-id
        (:execution-id
         (machine/pending-action
          started))))

    (is
     (= command-id
        (:command-id
         (machine/pending-action
          after-local))))

    (is
     (= execution-id
        (:execution-id
         (machine/pending-action
          after-local))))

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :example/command
         :payload {:request-id "request-1"}}
        envelope))))

(deftest machine-identity-is-not-silently-injected-into-participant-message
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:request-id}})

           :done
           (choreo/return :done)}})

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        execution
        (machine/start
         (projected-with-entry-knowledge
          choreography
          :browser
          {:browser #{:request-id}})
         {:command-id command-id
          :execution-id execution-id
          :values
          {:request-id "request-1"}})

        envelope
        (machine/pending-message
         execution
         {:request-id "request-1"})]

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :example/command
         :payload
         {:request-id "request-1"}}
        envelope))

    (is
     (false?
      (contains?
       (:payload envelope)
       :command-id)))

    (is
     (false?
      (contains?
       (:payload envelope)
       :execution-id)))))

(deftest protocol-must-declare-command-or-execution-identity-before-it-crosses-wire
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required
             #{:request-id
               :command-id
               :execution-id}})

           :done
           (choreo/return :done)}})

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        execution
        (machine/start
         (projected-with-entry-knowledge
          choreography
          :browser
          {:browser
           #{:request-id
             :command-id
             :execution-id}})
         {:command-id command-id
          :execution-id execution-id
          :values
          {:request-id "request-1"
           :command-id command-id
           :execution-id execution-id}})

        envelope
        (machine/pending-message
         execution
         {:request-id "request-1"
          :command-id command-id
          :execution-id execution-id})]

    (is
     (= {:request-id "request-1"
         :command-id command-id
         :execution-id execution-id}
        (:payload envelope)))

    (is
     (= command-id
        (machine/execution-value
         execution
         :command-id)))

    (is
     (= execution-id
        (machine/execution-value
         execution
         :execution-id)))))

(deftest identity-bindings-do-not-become-semantic-values
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :server
            :prepare
            :done)

           :done
           (choreo/return :done)}})

        principal
        (identity/principal
         "user-1")

        actor
        (identity/actor
         "helper-1")

        execution
        (machine/start
         (projected
          choreography
          :server)
         {:identity-bindings
          {:principal principal
           :actor actor}})]

    (is
     (= {}
        (machine/execution-values
         execution)))

    (is
     (false?
      (machine/has-execution-value?
       execution
       :principal)))

    (is
     (false?
      (machine/has-execution-value?
       execution
       :actor)))))

(deftest explain-keeps-runtime-identity-explicitly-separate-from-semantic-values
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        execution
        (machine/start
         (projected-with-entry-knowledge
          choreography
          :browser
          {:browser #{:request-id}})
         {:command-id command-id
          :execution-id execution-id
          :values
          {:request-id "request-1"}})

        explanation
        (machine/explain
         execution)]

    (is
     (= command-id
        (:command-id explanation)))

    (is
     (= execution-id
        (:execution-id explanation)))

    (is
     (= {:role :browser
         :command-id command-id
         :execution-id execution-id}
        (:identity-bindings explanation)))

    (is
     (= #{:request-id}
        (:value-keys explanation)))

    (is
     (not
      (contains?
       (:value-keys explanation)
       :command-id)))

    (is
     (not
      (contains?
       (:value-keys explanation)
       :execution-id)))))

(deftest projected-await-descriptor-retains-environment-event-contracts
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/completed :done
             :request/failed :failed}
            {:event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:revision}}

              :request/failed
              {:required #{:reason}
               :open-data? true}}})

           :done
           (choreo/return :done)

           :failed
           (choreo/return :failed)}})

        execution
        (start-role
         choreography
         :browser)]

    (is
     (= {:kind :environment
         :role :browser
         :events #{:request/completed
                   :request/failed}
         :event-contracts
         {:request/completed
          {:required #{:outcome}
           :optional #{:revision}}
          :request/failed
          {:required #{:reason}
           :open-data? true}}}
        (machine/awaiting
         execution)))))

(deftest environment-event-data-contract-is-enforced-before-resume
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/completed :done}
            {:event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:revision}}}})

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :browser)

        missing-required
        (machine/environment-event
         :browser
         :request/completed
         {:revision 42})

        undeclared
        (machine/environment-event
         :browser
         :request/completed
         {:outcome :confirmed
          :transport-debug true})

        malformed
        (machine/environment-event
         :browser
         :request/completed
         [:not-a-map])

        valid
        (machine/environment-event
         :browser
         :request/completed
         {:outcome :confirmed
          :revision 42})]

    (doseq [event
            [missing-required
             undeclared
             malformed]]
      (is
       (false?
        (machine/accepts-environment-event?
         execution
         event)))

      (is
       (= :invalid-environment-data
          (error-kind
           #(machine/resume-environment
             execution
             event)))))

    (is
     (machine/accepts-environment-event?
      execution
      valid))

    (is
     (machine/accepts?
      execution
      valid))))

(deftest declared-environment-data-becomes-role-local-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/completed :done}
            {:event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:revision}}}})

           :done
           (choreo/return :done)}})

        waiting
        (start-role
         choreography
         :browser)

        completed
        (machine/resume-environment
         waiting
         (machine/environment-event
          :browser
          :request/completed
          {:outcome :confirmed
           :revision 42}))]

    (is
     (machine/completed?
      completed))

    (is
     (= {:outcome :confirmed
         :revision 42}
        (machine/execution-values
         completed)))

    (is
     (= #{:asserted}
        (machine/execution-provenance-kinds
         completed
         :outcome)))

    (is
     (= [{:kind :asserted
          :source :request/completed
          :metadata
          {:origin :environment
           :state 0}}]
        (machine/execution-provenance
         completed
         :outcome)))

    (is
     (= {:kind :environment
         :state 0
         :role :browser
         :event :request/completed
         :data {:outcome :confirmed
                :revision 42}}
        (first
         (machine/execution-history
          completed))))))

(deftest absent-optional-environment-field-does-not-become-known
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/completed :done}
            {:event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:revision}}}})

           :done
           (choreo/return :done)}})

        completed
        (machine/resume-environment
         (start-role
          choreography
          :browser)
         (machine/environment-event
          :browser
          :request/completed
          {:outcome :confirmed}))]

    (is
     (= :confirmed
        (machine/execution-value
         completed
         :outcome)))

    (is
     (false?
      (machine/has-execution-value?
       completed
       :revision)))))

(deftest open-environment-data-does-not-leak-undeclared-host-data-into-portable-state
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:browser/observed :done}
            {:event-contracts
             {:browser/observed
              {:required #{:basis}
               :optional #{:visible?}
               :open-data? true}}})

           :done
           (choreo/return :done)}})

        waiting
        (start-role
         choreography
         :browser)

        event
        (machine/environment-event
         :browser
         :browser/observed
         {:basis :x24
          :visible? true
          :dom-node :host-object-placeholder
          :transport-debug "not semantic"})

        completed
        (machine/resume-environment
         waiting
         event)]

    (is
     (machine/accepts-environment-event?
      waiting
      event))

    (is
     (= {:basis :x24
         :visible? true}
        (machine/execution-values
         completed)))

    (is
     (false?
      (machine/has-execution-value?
       completed
       :dom-node)))

    (is
     (false?
      (machine/has-execution-value?
       completed
       :transport-debug)))

    (is
     (= {:kind :environment
         :state 0
         :role :browser
         :event :browser/observed
         :data {:basis :x24
                :visible? true}}
        (first
         (machine/execution-history
          completed))))))

(deftest environment-event-data-can-drive-a-subsequent-local-branch
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/settled :decide}
            {:event-contracts
             {:request/settled
              {:required #{:outcome}}}})

           :decide
           (choreo/branch
            :browser
            :outcome
            {:confirmed :show-confirmed
             :rejected :show-rejected})

           :show-confirmed
           (choreo/local
            :browser
            :show-confirmed
            :done)

           :show-rejected
           (choreo/local
            :browser
            :show-rejected
            :done)

           :done
           (choreo/return :done)}})

        waiting
        (start-role
         choreography
         :browser)

        confirmed
        (machine/resume-environment
         waiting
         (machine/environment-event
          :browser
          :request/settled
          {:outcome :confirmed}))]

    (is
     (machine/waiting-local?
      confirmed))

    (is
     (= :show-confirmed
        (:action
         (machine/pending-action
          confirmed))))

    (is
     (= :confirmed
        (machine/execution-value
         confirmed
         :outcome)))

    (is
     (= [:environment :branch]
        (mapv
         :kind
         (machine/execution-history
          confirmed))))))

(deftest machine-start-defensively-validates-executable-environment-contracts
  (testing "event contracts may name only declared await events"
    (let [plan
          {:gesso.choreo/type
           :gesso.choreo/executable-plan

           :gesso.choreo/version
           1

           :role
           :browser

           :initial
           0

           :states
           {0
            {:op :await
             :events {:request/completed 1}
             :event-contracts
             {:request/failed
              {:required #{:reason}}}}

            1
            {:op :return
             :outcome :gesso.choreo/complete}}}]

      (is
       (= :unknown-await-event-contract
          (error-kind
           #(machine/start
             plan))))))

  (testing "required and optional environment data keys must be disjoint"
    (let [plan
          {:gesso.choreo/type
           :gesso.choreo/executable-plan

           :gesso.choreo/version
           1

           :role
           :browser

           :initial
           0

           :states
           {0
            {:op :await
             :events {:request/completed 1}
             :event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:outcome}}}}

            1
            {:op :return
             :outcome :gesso.choreo/complete}}}]

      (is
       (= :ambiguous-event-data-key
          (error-kind
           #(machine/start
             plan))))))

  (testing "open-data marker must be boolean"
    (let [plan
          {:gesso.choreo/type
           :gesso.choreo/executable-plan

           :gesso.choreo/version
           1

           :role
           :browser

           :initial
           0

           :states
           {0
            {:op :await
             :events {:request/completed 1}
             :event-contracts
             {:request/completed
              {:open-data? :yes}}}

            1
            {:op :return
             :outcome :gesso.choreo/complete}}}]

      (is
       (= :invalid-open-data
          (error-kind
           #(machine/start
             plan)))))))


;; -----------------------------------------------------------------------------
;; Authoritative observation / reread environment events
;; -----------------------------------------------------------------------------

(def authoritative-reread-observation
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(deftest authoritative-observation-event-establishes-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/reread-complete :done}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :optional #{:request-owner}
               :open-data? true
               :authoritative-observation
               authoritative-reread-observation}}})

           :done
           (choreo/return :done)}})

        waiting
        (start-role
         choreography
         :browser)

        basis
        {:revision 42}

        completed
        (machine/resume-environment
         waiting
         (machine/environment-event
          :browser
          :request/reread-complete
          {:request-status :approved
           :request-owner "helper-7"
           :observed-basis basis
           :host-note "transport-only"}))]

    (is
     (= authoritative-reread-observation
        (get-in
         (machine/awaiting waiting)
         [:event-contracts
          :request/reread-complete
          :authoritative-observation])))

    (is
     (machine/completed?
      completed))

    (is
     (= {:request-status :approved
         :request-owner "helper-7"
         :observed-basis basis}
        (machine/execution-values
         completed)))

    (is
     (not
      (contains?
       (machine/execution-values completed)
       :host-note)))

    (doseq [key [:request-status
                 :request-owner
                 :observed-basis]]
      (is
       (= #{:authoritative}
          (machine/execution-provenance-kinds
           completed
           key))))

    (is
     (= [{:kind :authoritative
          :authority :request/model
          :observation :request/current-projection
          :basis basis
          :state 0
          :metadata
          {:origin :environment
           :event :request/reread-complete}}]
        (machine/execution-provenance
         completed
         :request-status)))

    (is
     (= {:kind :environment
         :state 0
         :role :browser
         :event :request/reread-complete
         :data {:request-status :approved
                :request-owner "helper-7"
                :observed-basis basis}}
        (first
         (machine/execution-history
          completed))))))

(deftest authoritative-observation-requires-a-non-nil-basis-at-runtime
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/reread-complete :done}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               authoritative-reread-observation}}})

           :done
           (choreo/return :done)}})

        waiting
        (start-role
         choreography
         :browser)

        invalid
        (machine/environment-event
         :browser
         :request/reread-complete
         {:request-status :approved
          :observed-basis nil})]

    ;; Presence of :observed-basis is not enough. A nil basis cannot justify an
    ;; authoritative observation, so the machine must not advertise this event
    ;; as consumable merely because its map shape matches the event contract.
    (is
     (false?
      (machine/accepts-environment-event?
       waiting
       invalid)))

    (is
     (false?
      (machine/accepts?
       waiting
       invalid)))

    (is
     (= :invalid-authoritative-observation
        (error-kind
         #(machine/resume-environment
           waiting
           invalid))))))

(defn- raw-authoritative-observation-await-plan
  [contract]
  {:gesso.choreo/type
   :gesso.choreo/executable-plan

   :gesso.choreo/version
   1

   :role
   :browser

   :initial
   0

   :states
   {0
    {:op :await
     :events {:request/reread-complete 1}
     :event-contracts
     {:request/reread-complete contract}}

    1
    {:op :return
     :outcome :gesso.choreo/complete}}})

(deftest machine-defensively-validates-authoritative-observation-contracts
  (testing "descriptor must be a map"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation :trusted}))))))

  (testing "descriptor requires logical authority"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation
             {:observation :request/current-projection
              :basis-key :observed-basis}}))))))

  (testing "descriptor requires observation identity"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation
             {:authority :request/model
              :basis-key :observed-basis}}))))))

  (testing "descriptor requires basis key"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation
             {:authority :request/model
              :observation :request/current-projection}}))))))

  (testing "descriptor is closed"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation
             {:authority :request/model
              :observation :request/current-projection
              :basis-key :observed-basis
              :trust-me true}}))))))

  (testing "descriptor fields must be keywords"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:observed-basis}
             :authoritative-observation
             {:authority "request/model"
              :observation :request/current-projection
              :basis-key :observed-basis}}))))))

  (testing "basis key must be required semantic event data"
    (is
     (= :invalid-authoritative-observation-contract
        (error-kind
         #(machine/start
           (raw-authoritative-observation-await-plan
            {:required #{:request-status}
             :authoritative-observation
             authoritative-reread-observation})))))))
