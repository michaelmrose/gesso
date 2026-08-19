(ns gesso.choreo.semantics-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.semantics :as semantics]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (:error/kind
       (ex-data e)))))

(deftest core-authors-the-first-semantic-slice
  (let [program
        (choreo/->choreography
         {:name :example/hello
          :initial :start
          :states
          {:start
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :example/hello
            :wait)

           :wait
           (choreo/await
            :authority
            {:request/failed :failed
             :request/completed :done})

           :failed
           (choreo/return :failed)

           :done
           (choreo/return :done)}})]

    (is (choreo/choreography? program))

    (is (= #{:browser :authority}
           (choreo/roles program)))

    (is (= #{:local
             :communicate
             :await
             :return}
           (set
            (keys
             (:states-by-op
              (choreo/explain program))))))

    (is (= #{:send}
           (choreo/successors
            (choreo/state program :start))))

    (is (= #{:request/failed
             :request/completed}
           (set
            (keys
             (:events
              (choreo/state program :wait))))))))

(deftest local-actions-are-semantic-but-not-distributed-observations
  (let [program
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare-command
            :done)

           :done
           (choreo/return :ok)}})

        initial
        (semantics/start program)

        result
        (semantics/transition
         initial
         (semantics/local-event
          :browser
          :prepare-command))

        completed
        (:configuration result)]

    (is (semantics/completed? completed))
    (is (= :ok
           (semantics/outcome completed)))

    (is (= [{:kind :local
             :state :prepare
             :role :browser
             :action :prepare-command}

            {:kind :terminal
             :state :done
             :outcome :ok}]
           (semantics/history completed)))

    (is (= [{:kind :terminal
             :state :done
             :outcome :ok}]
           (semantics/observable-trace completed)))

    (is (= [{:kind :terminal
             :state :done
             :outcome :ok}]
           (:observations result)))

    (is (= []
           (:effects result)))))

(deftest communication-is-one-global-semantic-occurrence
  (let [program
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
           (choreo/return :submitted)}})

        initial
        (semantics/start program)

        event
        (semantics/communication-event
         :browser
         :authority
         :request/claim
         {:request-id 17}
         {:via :http})

        result
        (semantics/transition
         initial
         event)

        completed
        (:configuration result)

        communication
        {:kind :communication
         :state :send
         :from :browser
         :to :authority
         :event :request/claim
         :payload {:request-id 17}
         :via :http}

        terminal
        {:kind :terminal
         :state :done
         :outcome :submitted}]

    (is (semantics/completed? completed))

    (is (= [communication terminal]
           (semantics/history completed)))

    (is (= [communication terminal]
           (semantics/observable-trace completed)))

    (is (= [communication terminal]
           (:observations result)))))

(deftest await-consumes-only-role-local-environment-events
  (let [program
        (choreo/->choreography
         {:initial :waiting
          :states
          {:waiting
           (choreo/await
            :browser
            {:request/failed :failed
             :request/completed :done})

           :failed
           (choreo/return :failed)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)]

    (testing "the declared environment event is enabled"
      (is
       (semantics/enabled?
        initial
        (semantics/environment-event
         :browser
         :request/completed
         {:status 200}))))

    (testing "the same event keyword in a participant message is not enabled"
      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/communication-event
          :authority
          :browser
          :request/completed
          {})))))

    (testing "an environment event for another role is not enabled"
      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/environment-event
          :authority
          :request/completed
          {:status 200})))))

    (let [result
          (semantics/transition
           initial
           (semantics/environment-event
            :browser
            :request/completed
            {:status 200}))

          completed
          (:configuration result)]

      (is (semantics/completed? completed))

      (is (= [{:kind :environment
               :state :waiting
               :role :browser
               :event :request/completed
               :data {:status 200}}

              {:kind :terminal
               :state :done
               :outcome :done}]
             (semantics/history completed)))

      (is (= [{:kind :terminal
               :state :done
               :outcome :done}]
             (semantics/observable-trace completed))))))

(deftest communication-and-environment-events-do-not-cross-satisfy
  (let [program
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :authority
            :same/event
            :done)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)]

    (is
     (false?
      (semantics/enabled?
       initial
       (semantics/environment-event
        :browser
        :same/event
        nil))))

    (is
     (= :event-not-enabled
        (error-kind
         #(semantics/step
           initial
           (semantics/environment-event
            :browser
            :same/event
            nil)))))))

(deftest unexpected-events-are-rejected
  (let [program
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

        initial
        (semantics/start program)]

    (is
     (= :event-not-enabled
        (error-kind
         #(semantics/step
           initial
           (semantics/local-event
            :browser
            :something-else)))))

    (is
     (= :event-not-enabled
        (error-kind
         #(semantics/step
           initial
           (semantics/local-event
            :authority
            :prepare)))))))

(deftest completed-configurations-cannot-be-resurrected
  (let [program
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})

        completed
        (semantics/start program)]

    (is (semantics/completed? completed))

    (is (= [{:kind :terminal
             :state :done
             :outcome :done}]
           (semantics/history completed)))

    (is
     (= :terminal-transition
        (error-kind
         #(semantics/step
           completed
           (semantics/environment-event
            :browser
            :anything)))))))

(deftest state-identities-are-opaque-edn
  (let [start-id
        [:state :start 1]

        done-id
        [:state :done 2]

        program
        (choreo/->choreography
         {:initial start-id
          :states
          {start-id
           (choreo/local
            :browser
            :prepare
            done-id)

           done-id
           (choreo/return :done)}})

        completed
        (-> program
            semantics/start
            (semantics/step
             (semantics/local-event
              :browser
              :prepare)))]

    (is (semantics/completed? completed))
    (is (= done-id
           (semantics/current-state-id completed)))))

(deftest local-actions-produce-exactly-their-declared-semantic-values
  (let [program
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

        initial
        (semantics/start program)

        event
        (semantics/local-event
         :browser
         :compute
         {:outcome :confirmed
          :revision 42})

        completed
        (semantics/step
         initial
         event)]

    (is (= {:outcome :confirmed
            :revision 42}
           (semantics/values completed)))

    (is (= :confirmed
           (semantics/value
            completed
            :outcome)))

    (is (semantics/knows-value?
         completed
         :revision))

    (is (= [{:kind :local
             :state :compute
             :role :browser
             :action :compute
             :outputs
             {:outcome :confirmed
              :revision 42}}

            {:kind :terminal
             :state :done
             :outcome :done}]
           (semantics/history completed)))

    (is (= [{:kind :terminal
             :state :done
             :outcome :done}]
           (semantics/observable-trace
            completed)))))

(deftest local-output-contract-is-closed
  (let [program
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute
            :done
            {:outputs #{:outcome}})

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)

        missing-output
        (semantics/local-event
         :browser
         :compute
         {})

        extra-output
        (semantics/local-event
         :browser
         :compute
         {:outcome :confirmed
          :revision 42})]

    (testing "missing declared output is not enabled"
      (is (false?
           (semantics/enabled?
            initial
            missing-output)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                initial
                missing-output)))))

    (testing "undeclared extra output is not enabled"
      (is (false?
           (semantics/enabled?
            initial
            extra-output)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                initial
                extra-output)))))))

(deftest local-actions-require-established-input-values
  (let [program
        (choreo/->choreography
         {:initial :use-input
          :states
          {:use-input
           (choreo/local
            :browser
            :use-input
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        missing
        (semantics/start
         program)

        supplied
        (semantics/start
         program
         {:values
          {:request-id 17}})

        event
        (semantics/local-event
         :browser
         :use-input)]

    (testing "missing required value disables the local action"
      (is (false?
           (semantics/enabled?
            missing
            event)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                missing
                event)))))

    (testing "an explicit entry value satisfies the requirement"
      (is (semantics/enabled?
           supplied
           event))

      (is (semantics/completed?
           (semantics/step
            supplied
            event))))))

(deftest local-output-may-deliberately-replace-an-existing-value
  (let [program
        (choreo/->choreography
         {:initial :replace
          :states
          {:replace
           (choreo/local
            :browser
            :replace
            :done
            {:requires #{:outcome}
             :outputs #{:outcome}})

           :done
           (choreo/return :done)}})

        initial
        (semantics/start
         program
         {:values
          {:outcome :old}})

        completed
        (semantics/step
         initial
         (semantics/local-event
          :browser
          :replace
          {:outcome :new}))]

    (is (= :new
           (semantics/value
            completed
            :outcome)))))

(deftest branch-follows-the-already-established-semantic-value
  (let [program
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :server
            :compute-outcome
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        after-compute
        (-> program
            semantics/start
            (semantics/step
             (semantics/local-event
              :server
              :compute-outcome
              {:outcome :confirmed})))

        branch-event
        (semantics/branch-event
         :server
         :outcome
         :confirmed)

        completed
        (semantics/step
         after-compute
         branch-event)]

    (is (= {:kind :branch
            :role :server
            :on :outcome
            :value :confirmed
            :value-present? true
            :cases #{:confirmed :rejected}}
           (semantics/expected-event
            after-compute)))

    (is (semantics/enabled?
         after-compute
         branch-event))

    (is (= :confirmed
           (semantics/outcome
            completed)))

    (is (= [{:kind :local
             :state :compute
             :role :server
             :action :compute-outcome
             :outputs {:outcome :confirmed}}

            {:kind :branch
             :state :branch
             :role :server
             :on :outcome
             :value :confirmed}

            {:kind :terminal
             :state :confirmed
             :outcome :confirmed}]
           (semantics/history completed)))

    (testing "branch decision is semantic history but not distributed observation"
      (is (= [{:kind :terminal
               :state :confirmed
               :outcome :confirmed}]
             (semantics/observable-trace
              completed))))))

(deftest branch-cannot-claim-a-different-value-than-the-semantic-store
  (let [program
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        configuration
        (semantics/start
         program
         {:values
          {:outcome :confirmed}})

        false-branch
        (semantics/branch-event
         :server
         :outcome
         :rejected)]

    (is (false?
         (semantics/enabled?
          configuration
          false-branch)))

    (is (= :event-not-enabled
           (error-kind
            #(semantics/step
              configuration
              false-branch))))))

(deftest branch-requires-a-present-selector-and-a-covered-value
  (let [program
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :done})

           :done
           (choreo/return :done)}})

        missing
        (semantics/start
         program)

        uncovered
        (semantics/start
         program
         {:values
          {:outcome :rejected}})]

    (testing "missing selector is distinguishable from a concrete value"
      (is (= {:kind :branch
              :role :server
              :on :outcome
              :value nil
              :value-present? false
              :cases #{:confirmed}}
             (semantics/expected-event
              missing)))

      (is (false?
           (semantics/enabled?
            missing
            (semantics/branch-event
             :server
             :outcome
             nil)))))

    (testing "an established value not covered by the branch is rejected"
      (is (false?
           (semantics/enabled?
            uncovered
            (semantics/branch-event
             :server
             :outcome
             :rejected))))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                uncovered
                (semantics/branch-event
                 :server
                 :outcome
                 :rejected))))))))

(deftest communication-payload-does-not-magically-enter-the-global-value-store
  (let [program
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start
         program
         {:values
          {:already-known true}})

        completed
        (semantics/step
         initial
         (semantics/communication-event
          :browser
          :server
          :example/command
          {:outcome :confirmed
           :revision 42}))]

    (is (= {:already-known true}
           (semantics/values
            completed)))

    (is (false?
         (semantics/knows-value?
          completed
          :outcome)))))

(deftest environment-event-data-does-not-magically-enter-the-global-value-store
  (let [program
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:browser/ready :done})

           :done
           (choreo/return :done)}})

        completed
        (-> program
            semantics/start
            (semantics/step
             (semantics/environment-event
              :browser
              :browser/ready
              {:revision 42})))]

    (is (= {}
           (semantics/values
            completed)))

    (is (false?
         (semantics/knows-value?
          completed
          :revision)))))

(deftest semantic-explain-shows-value-keys-without-pretending-they-are-role-knowledge
  (let [program
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :done})

           :done
           (choreo/return :done)}})

        configuration
        (semantics/start
         program
         {:values
          {:outcome :confirmed
           :revision 42}})]

    (is (= #{:outcome :revision}
           (:value-keys
            (semantics/explain
             configuration))))))
