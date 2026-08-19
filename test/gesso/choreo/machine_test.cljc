(ns gesso.choreo.machine-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
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
            :state :prepare
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
               :state :prepare
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
            {:via :http})

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :browser)]

    (is (machine/waiting-send?
         execution))

    (is (= {:kind :send
            :execution-id nil
            :state :send
            :from :browser
            :to :authority
            :event :request/claim
            :via :http}
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
               :state :send
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
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :authority)]

    (is (machine/waiting-receive?
         execution))

    (is (= {:kind :receive
            :role :authority
            :alternatives
            [{:from :browser
              :event :request/claim}]}
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
             :request/completed
             {:status 200})]

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
             :request/completed
             {:status 200})

            completed
            (machine/resume-environment
             execution
             event)]

        (is (machine/completed?
             completed))

        (is (= {:outcome :gesso.choreo/complete}
               (machine/result completed)))

        (is (= [{:kind :environment
                 :state :wait
                 :role :browser
                 :event :request/completed
                 :data {:status 200}}

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
            :done)

           :two
           (choreo/communicate
            :alice
            :bob
            :example/two
            :done)

           :done
           (choreo/return :done)}})

        execution
        (start-role
         choreography
         :bob)

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
            :done)

           :done
           (choreo/return :done)}})

        first-run
        (-> (start-role
             choreography
             :browser)
            machine/complete-local
            (machine/complete-send
             {:x 1})
            :execution)

        second-run
        (-> (start-role
             choreography
             :browser)
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

        execution
        (machine/start
         plan
         {:execution-id
          :execution/example})]

    (is (= :execution/example
           (:execution-id execution)))

    (is (= :execution/example
           (:execution-id
            (machine/pending-action
             execution))))

    (is (= [{:kind :local
             :state :local
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

(deftest state-identities-remain-opaque
  (let [start
        [:projected :start]

        done
        [:projected :done]

        plan
        {:gesso.choreo/type
         :gesso.choreo/projected-plan

         :gesso.choreo/version
         1

         :name
         :example/opaque

         :role
         :browser

         :initial
         start

         :states
         {start
          {:op :local
           :action :prepare
           :next done}

          done
          {:op :return
           :outcome :done}}}

        execution
        (machine/start plan)

        completed
        (machine/complete-local
         execution)]

    (is (= start
           (machine/current-state-id
            execution)))

    (is (= done
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
            :state :compute
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
        {:gesso.choreo/type :gesso.choreo/projected-plan
         :gesso.choreo/version 1
         :role :browser
         :initial :compute
         :states
         {:compute
          {:op :local
           :action :compute
           :requires #{:request-id :attempt}
           :outputs #{:outcome}
           :next :done}

          :done
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
               :state :compute
               :role :browser
               :action :compute-outcome
               :outputs {:outcome :confirmed}}

              {:kind :branch
               :state :decide
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
            :state :decide
            :role :browser
            :on :outcome
            :value :rejected}
           (first
            (machine/execution-history execution))))))

(deftest branch-with-uncovered-value-fails-before-endpoint-action
  (let [plan
        {:gesso.choreo/type :gesso.choreo/projected-plan
         :gesso.choreo/version 1
         :role :browser
         :initial :decide
         :states
         {:decide
          {:op :branch
           :on :outcome
           :cases {:confirmed :done}}

          :done
          {:op :return
           :outcome :done}}}]

    (is (= :branch-value-not-covered
           (error-kind
            #(machine/start
              plan
              {:values {:outcome :rejected}}))))))

(deftest branch-with-missing-value-fails-before-endpoint-action
  (let [plan
        {:gesso.choreo/type :gesso.choreo/projected-plan
         :gesso.choreo/version 1
         :role :browser
         :initial :decide
         :states
         {:decide
          {:op :branch
           :on :outcome
           :cases {:confirmed :done}}

          :done
          {:op :return
           :outcome :done}}}]

    (is (= :missing-branch-value
           (error-kind
            #(machine/start plan))))))

(deftest immediate-branch-loop-is-bounded
  (let [plan
        {:gesso.choreo/type :gesso.choreo/projected-plan
         :gesso.choreo/version 1
         :role :browser
         :initial :loop
         :states
         {:loop
          {:op :branch
           :on :again?
           :cases {true :loop
                   false :done}}

          :done
          {:op :return
           :outcome :done}}}]

    (is (= :immediate-step-limit
           (error-kind
            #(machine/start
              plan
              {:values {:again? true}
               :max-immediate-steps 8}))))))

