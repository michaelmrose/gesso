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
            {:via :http
             :required #{:request-id}})

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
         :request/completed))))

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
          :request/completed)))))

    (let [result
          (semantics/transition
           initial
           (semantics/environment-event
            :browser
            :request/completed))

          completed
          (:configuration result)]

      (is (semantics/completed? completed))

      (is (= [{:kind :environment
               :state :waiting
               :role :browser
               :event :request/completed
               :data nil}

              {:kind :terminal
               :state :done
               :outcome :done}]
             (semantics/history completed)))

      (is (= [{:kind :terminal
               :state :done
               :outcome :done}]
             (semantics/observable-trace completed))))))

(deftest await-event-contract-controls-semantic-data-shape
  (let [program
        (choreo/->choreography
         {:initial :waiting
          :states
          {:waiting
           (choreo/await
            :browser
            {:request/failed :failed
             :request/completed :done}
            {:event-contracts
             {:request/failed
              {:required #{:reason}
               :optional #{:status}}}})

           :failed
           (choreo/return :failed)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)]

    (testing "required semantic event data is required"
      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/environment-event
          :browser
          :request/failed
          {})))))

    (testing "required-only data is accepted"
      (is
       (semantics/enabled?
        initial
        (semantics/environment-event
         :browser
         :request/failed
         {:reason :network}))))

    (testing "declared optional data may also be present"
      (is
       (semantics/enabled?
        initial
        (semantics/environment-event
         :browser
         :request/failed
         {:reason :http
          :status 503}))))

    (testing "closed event data rejects undeclared keys"
      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/environment-event
          :browser
          :request/failed
          {:reason :http
           :status 503
           :xhr :host-object})))))

    (testing "an event without an explicit contract declares no semantic data"
      (is
       (semantics/enabled?
        initial
        (semantics/environment-event
         :browser
         :request/completed)))

      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/environment-event
          :browser
          :request/completed
          {:status 200})))))))

(deftest declared-await-event-data-enters-global-semantic-value-flow
  (let [program
        (choreo/->choreography
         {:initial :waiting
          :states
          {:waiting
           (choreo/await
            :browser
            {:request/completed :branch}
            {:event-contracts
             {:request/completed
              {:required #{:outcome}
               :optional #{:revision}}}})

           :branch
           (choreo/branch
            :browser
            :outcome
            {:confirmed :done
             :rejected :failed})

           :done
           (choreo/return :done)

           :failed
           (choreo/return :failed)}})

        after-event
        (-> program
            semantics/start
            (semantics/step
             (semantics/environment-event
              :browser
              :request/completed
              {:outcome :confirmed
               :revision 42})))]

    (is (= {:outcome :confirmed
            :revision 42}
           (semantics/values after-event)))

    (is (semantics/enabled?
         after-event
         (semantics/branch-event
          :browser
          :outcome
          :confirmed)))))

(deftest open-await-event-data-keeps-undeclared-extras-out-of-semantic-state
  (let [program
        (choreo/->choreography
         {:initial :waiting
          :states
          {:waiting
           (choreo/await
            :browser
            {:browser/observed :done}
            {:event-contracts
             {:browser/observed
              {:required #{:basis}
               :optional #{:reason}
               :open-data? true}}})

           :done
           (choreo/return :done)}})

        event
        (semantics/environment-event
         :browser
         :browser/observed
         {:basis 24
          :reason :canonical-refresh
          :xhr :host-object
          :dom-node :host-object})

        initial
        (semantics/start program)

        completed
        (semantics/step initial event)]

    (is (semantics/enabled? initial event))

    (is (= {:basis 24
            :reason :canonical-refresh}
           (semantics/values completed)))

    (is (= [{:kind :environment
             :state :waiting
             :role :browser
             :event :browser/observed
             :data {:basis 24
                    :reason :canonical-refresh}}

            {:kind :terminal
             :state :done
             :outcome :done}]
           (semantics/history completed)))))

(deftest await-event-contract-is-visible-in-the-expected-event
  (let [program
        (choreo/->choreography
         {:initial :waiting
          :states
          {:waiting
           (choreo/await
            :browser
            {:request/completed :done
             :request/failed :failed}
            {:event-contracts
             {:request/failed
              {:required #{:reason}
               :optional #{:status}
               :open-data? true}}})

           :failed
           (choreo/return :failed)

           :done
           (choreo/return :done)}})]

    (is (= {:kind :environment
            :role :browser
            :events #{:request/completed
                      :request/failed}
            :event-contracts
            {:request/failed
             {:required #{:reason}
              :optional #{:status}
              :open-data? true}}}
           (semantics/expected-event
            (semantics/start program))))))

(deftest authoritative-observation-contract-is-visible-in-global-semantics
  (let [program
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/observed :done}
            {:event-contracts
             {:request/observed
              {:required #{:basis :outcome}
               :optional #{:revision}
               :open-data? true
               :authoritative-observation
               {:authority :request/model
                :observation :request/read
                :basis-key :basis}}}})

           :done
           (choreo/return :done)}})]

    (is
     (= {:kind :environment
         :role :browser
         :events #{:request/observed}
         :event-contracts
         {:request/observed
          {:required #{:basis :outcome}
           :optional #{:revision}
           :open-data? true
           :authoritative-observation
           {:authority :request/model
            :observation :request/read
            :basis-key :basis}}}}
        (semantics/expected-event
         (semantics/start program))))))

(deftest authoritative-observation-is-an-explicit-role-local-semantic-occurrence
  (let [program
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/observed :done}
            {:event-contracts
             {:request/observed
              {:required #{:basis :outcome}
               :optional #{:revision}
               :open-data? true
               :authoritative-observation
               {:authority :request/model
                :observation :request/read
                :basis-key :basis}}}})

           :done
           (choreo/return :done)}})

        event
        (semantics/environment-event
         :browser
         :request/observed
         {:basis [:xtdb-basis 42]
          :outcome :approved
          :revision 42
          :xhr :host-object})

        initial
        (semantics/start program)

        completed
        (semantics/step initial event)]

    (is (semantics/enabled? initial event))

    (is
     (= {:basis [:xtdb-basis 42]
         :outcome :approved
         :revision 42}
        (semantics/values completed)))

    (is
     (= [{:kind :authoritative-observation
          :state :observe
          :role :browser
          :event :request/observed
          :authority :request/model
          :observation :request/read
          :basis-key :basis
          :basis [:xtdb-basis 42]
          :data {:basis [:xtdb-basis 42]
                 :outcome :approved
                 :revision 42}}

         {:kind :terminal
          :state :done
          :outcome :done}]
        (semantics/history completed)))

    (testing "a role-local authoritative reread is not an authoritative mutation"
      (is
       (= [{:kind :terminal
            :state :done
            :outcome :done}]
          (semantics/observable-trace completed))))))

(deftest authoritative-observation-requires-a-present-non-nil-basis
  (let [program
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/observed :done}
            {:event-contracts
             {:request/observed
              {:required #{:basis :outcome}
               :authoritative-observation
               {:authority :request/model
                :observation :request/read
                :basis-key :basis}}}})

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)

        nil-basis
        (semantics/environment-event
         :browser
         :request/observed
         {:basis nil
          :outcome :approved})]

    (is
     (false?
      (semantics/enabled?
       initial
       nil-basis)))

    (is
     (= :event-not-enabled
        (error-kind
         #(semantics/step
           initial
           nil-basis))))))

(deftest semantic-program-validates-authoritative-observation-contract-independently
  (testing "all descriptor fields are required"
    (is
     (= :incomplete-authoritative-observation
        (error-kind
         #(semantics/->program
           {:initial :observe
            :states
            {:observe
             {:op :await
              :role :browser
              :events {:request/observed :done}
              :event-contracts
              {:request/observed
               {:required #{:basis}
                :authoritative-observation
                {:authority :request/model
                 :basis-key :basis}}}}

             :done
             {:op :return
              :outcome :done}}})))))

  (testing "the descriptor is closed"
    (is
     (= :unknown-authoritative-observation-key
        (error-kind
         #(semantics/->program
           {:initial :observe
            :states
            {:observe
             {:op :await
              :role :browser
              :events {:request/observed :done}
              :event-contracts
              {:request/observed
               {:required #{:basis}
                :authoritative-observation
                {:authority :request/model
                 :observation :request/read
                 :basis-key :basis
                 :trusted? true}}}}

             :done
             {:op :return
              :outcome :done}}})))))

  (testing "the basis key must be required semantic event data"
    (is
     (= :authoritative-observation-basis-not-required
        (error-kind
         #(semantics/->program
           {:initial :observe
            :states
            {:observe
             {:op :await
              :role :browser
              :events {:request/observed :done}
              :event-contracts
              {:request/observed
               {:required #{:outcome}
                :optional #{:basis}
                :authoritative-observation
                {:authority :request/model
                 :observation :request/read
                 :basis-key :basis}}}}

             :done
             {:op :return
              :outcome :done}}})))))

  (testing "descriptor fields are typed semantic names"
    (is
     (= :invalid-value
        (error-kind
         #(semantics/->program
           {:initial :observe
            :states
            {:observe
             {:op :await
              :role :browser
              :events {:request/observed :done}
              :event-contracts
              {:request/observed
               {:required #{:basis}
                :authoritative-observation
                {:authority "request/model"
                 :observation :request/read
                 :basis-key :basis}}}}

             :done
             {:op :return
              :outcome :done}}}))))))

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
            :done
            {:required #{:outcome :revision}})

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

(deftest undeclared-environment-data-is-not-a-semantic-backdoor
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

        initial
        (semantics/start program)

        event
        (semantics/environment-event
         :browser
         :browser/ready
         {:revision 42})]

    (is (false?
         (semantics/enabled?
          initial
          event)))

    (is (= :event-not-enabled
           (error-kind
            #(semantics/step
              initial
              event))))

    (is (= {}
           (semantics/values initial)))))

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

(deftest authoritative-operation-is-an-observable-semantic-transition
  (let [program
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:requires #{:request-id}
             :outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        initial
        (semantics/start
         program
         {:values
          {:request-id 17}})

        event
        (semantics/authoritative-event
         :server
         :request/claim
         {:outcome :confirmed
          :revision 42})

        result
        (semantics/transition
         initial
         event)

        completed
        (:configuration result)

        authority-entry
        {:kind :authoritative
         :state :claim
         :role :server
         :operation :request/claim
         :outputs
         {:outcome :confirmed
          :revision 42}}

        terminal-entry
        {:kind :terminal
         :state :done
         :outcome :done}]

    (is (= {:kind :authoritative
            :role :server
            :operation :request/claim
            :requires #{:request-id}
            :outputs #{:outcome :revision}}
           (semantics/expected-event
            initial)))

    (is (semantics/enabled?
         initial
         event))

    (is (= {:request-id 17
            :outcome :confirmed
            :revision 42}
           (semantics/values
            completed)))

    (is (= [authority-entry
            terminal-entry]
           (semantics/history
            completed)))

    (is (= [authority-entry
            terminal-entry]
           (semantics/observable-trace
            completed)))

    (is (= [authority-entry
            terminal-entry]
           (:observations result)))

    (is (= []
           (:effects result)))))

(deftest authoritative-operation-requires-established-semantic-inputs
  (let [program
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        event
        (semantics/authoritative-event
         :server
         :request/claim)

        missing
        (semantics/start
         program)

        supplied
        (semantics/start
         program
         {:values
          {:request-id 17}})]

    (testing "missing required semantic value disables the authority boundary"
      (is (false?
           (semantics/enabled?
            missing
            event)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                missing
                event)))))

    (testing "established input enables the authority boundary"
      (is (semantics/enabled?
           supplied
           event))

      (is (semantics/completed?
           (semantics/step
            supplied
            event))))))

(deftest authoritative-output-contract-is-closed
  (let [program
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

        initial
        (semantics/start
         program)

        missing-output
        (semantics/authoritative-event
         :server
         :request/claim
         {:outcome :confirmed})

        extra-output
        (semantics/authoritative-event
         :server
         :request/claim
         {:outcome :confirmed
          :revision 42
          :unexpected true})]

    (testing "missing declared output is rejected"
      (is (false?
           (semantics/enabled?
            initial
            missing-output)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                initial
                missing-output)))))

    (testing "undeclared authoritative output is rejected"
      (is (false?
           (semantics/enabled?
            initial
            extra-output)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                initial
                extra-output)))))))

(deftest authoritative-operation-identity-and-role-must-match
  (let [program
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start
         program)]

    (doseq [event
            [(semantics/authoritative-event
              :server
              :request/cancel)

             (semantics/authoritative-event
              :browser
              :request/claim)]]

      (is (false?
           (semantics/enabled?
            initial
            event)))

      (is (= :event-not-enabled
             (error-kind
              #(semantics/step
                initial
                event)))))))

(deftest authoritative-output-may-drive-later-deterministic-control-flow
  (let [program
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
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        after-authority
        (-> program
            semantics/start
            (semantics/step
             (semantics/authoritative-event
              :server
              :request/claim
              {:outcome :confirmed})))

        completed
        (semantics/step
         after-authority
         (semantics/branch-event
          :server
          :outcome
          :confirmed))]

    (is (= :confirmed
           (semantics/value
            after-authority
            :outcome)))

    (is (= :branch
           (:op
            (semantics/current-state
             after-authority))))

    (is (= :confirmed
           (semantics/outcome
            completed)))

    (is (= [:authoritative
            :branch
            :terminal]
           (mapv :kind
                 (semantics/history
                  completed))))

    (is (= [:authoritative
            :terminal]
           (mapv :kind
                 (semantics/observable-trace
                  completed))))))

(deftest local-and-authoritative-events-remain-semantically-distinct
  (let [authoritative-program
        (choreo/->choreography
         {:initial :work
          :states
          {:work
           (choreo/authoritative
            :server
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        local-program
        (choreo/->choreography
         {:initial :work
          :states
          {:work
           (choreo/local
            :server
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        authoritative-initial
        (semantics/start
         authoritative-program)

        local-initial
        (semantics/start
         local-program)]

    (is (false?
         (semantics/enabled?
          authoritative-initial
          (semantics/local-event
           :server
           :request/claim))))

    (is (false?
         (semantics/enabled?
          local-initial
          (semantics/authoritative-event
           :server
           :request/claim))))))

(deftest authoritative-result-does-not-imply-a-particular-commit-convention
  (let [program
        (choreo/->choreography
         {:initial :execute
          :states
          {:execute
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:outputs #{:outcome :command-applied?}})

           :done
           (choreo/return :done)}})

        completed
        (-> program
            semantics/start
            (semantics/step
             (semantics/authoritative-event
              :server
              :request/claim
              {:outcome :rejected
               :command-applied? false})))]

    (is (= :rejected
           (semantics/value
            completed
            :outcome)))

    (is (false?
         (semantics/value
          completed
          :command-applied?)))

    (is (semantics/completed?
         completed))))

(deftest communication-payload-is-closed-by-default
  (let [program
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

        initial
        (semantics/start program)]

    (testing "all required keys must be present"
      (let [event
            (semantics/communication-event
             :browser
             :server
             :example/command
             {})]

        (is (false?
             (semantics/enabled?
              initial
              event)))

        (is (= :event-not-enabled
               (error-kind
                #(semantics/step
                  initial
                  event))))))

    (testing "required keys alone are sufficient"
      (is
       (semantics/enabled?
        initial
        (semantics/communication-event
         :browser
         :server
         :example/command
         {:execution-id "execution-1"}))))

    (testing "declared optional keys may be present"
      (is
       (semantics/enabled?
        initial
        (semantics/communication-event
         :browser
         :server
         :example/command
         {:execution-id "execution-1"
          :base-revision 42}))))

    (testing "undeclared keys are rejected"
      (let [event
            (semantics/communication-event
             :browser
             :server
             :example/command
             {:execution-id "execution-1"
              :base-revision 42
              :surprise true})]

        (is (false?
             (semantics/enabled?
              initial
              event)))

        (is (= :event-not-enabled
               (error-kind
                #(semantics/step
                  initial
                  event))))))))

(deftest empty-communication-contract-allows-only-empty-payload
  (let [program
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :alice
            :bob
            :example/ping
            :done)

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)]

    (is
     (semantics/enabled?
      initial
      (semantics/communication-event
       :alice
       :bob
       :example/ping
       {})))

    (is
     (false?
      (semantics/enabled?
       initial
       (semantics/communication-event
        :alice
        :bob
        :example/ping
        {:undeclared true}))))))

(deftest explicitly-open-communication-allows-undeclared-payload-keys
  (let [program
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

        initial
        (semantics/start program)]

    (testing "declared required keys remain required even for open payloads"
      (is
       (false?
        (semantics/enabled?
         initial
         (semantics/communication-event
          :browser
          :server
          :example/open-command
          {:extra true})))))

    (testing "additional keys may cross once required keys are present"
      (is
       (semantics/enabled?
        initial
        (semantics/communication-event
         :browser
         :server
         :example/open-command
         {:execution-id "execution-1"
          :extra true
          :another 42}))))))

(deftest communication-contract-is-visible-in-the-expected-event
  (let [program
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
             :correlation #{:execution-id :scope}})

           :done
           (choreo/return :done)}})

        initial
        (semantics/start program)]

    (is
     (= {:kind :communication
         :from :browser
         :to :server
         :event :example/command
         :required #{:execution-id :scope}
         :optional #{:base-revision}
         :correlation #{:execution-id :scope}
         :open-payload? false
         :via :http}
        (semantics/expected-event
         initial)))))

(deftest semantic-program-rejects-required-optional-overlap
  (is
   (= :ambiguous-message-key
      (error-kind
       #(semantics/->program
         {:initial :send
          :states
          {:send
           {:op :communicate
            :from :browser
            :to :server
            :event :example/command
            :required #{:execution-id}
            :optional #{:execution-id}
            :next :done}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-correlation-that-is-not-required
  (is
   (= :optional-correlation-key
      (error-kind
       #(semantics/->program
         {:initial :send
          :states
          {:send
           {:op :communicate
            :from :browser
            :to :server
            :event :example/command
            :required #{:execution-id}
            :optional #{:scope}
            :correlation #{:execution-id :scope}
            :next :done}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-nonboolean-open-payload-marker
  (is
   (= :invalid-open-payload
      (error-kind
       #(semantics/->program
         {:initial :send
          :states
          {:send
           {:op :communicate
            :from :browser
            :to :server
            :event :example/command
            :open-payload? :yes
            :next :done}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-await-contract-for-unknown-event
  (is
   (= :unknown-await-event-contract
      (error-kind
       #(semantics/->program
         {:initial :wait
          :states
          {:wait
           {:op :await
            :role :browser
            :events {:browser/ready :done}
            :event-contracts
            {:browser/missing
             {:required #{:revision}}}}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-invalid-await-event-value-set
  (is
   (= :invalid-value-set
      (error-kind
       #(semantics/->program
         {:initial :wait
          :states
          {:wait
           {:op :await
            :role :browser
            :events {:browser/ready :done}
            :event-contracts
            {:browser/ready
             {:required [:revision]}}}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-await-required-optional-overlap
  (is
   (= :ambiguous-event-data-key
      (error-kind
       #(semantics/->program
         {:initial :wait
          :states
          {:wait
           {:op :await
            :role :browser
            :events {:browser/ready :done}
            :event-contracts
            {:browser/ready
             {:required #{:revision}
              :optional #{:revision}}}}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest semantic-program-rejects-nonboolean-open-event-data-marker
  (is
   (= :invalid-open-data
      (error-kind
       #(semantics/->program
         {:initial :wait
          :states
          {:wait
           {:op :await
            :role :browser
            :events {:browser/ready :done}
            :event-contracts
            {:browser/ready
             {:open-data? :yes}}}

           :done
           {:op :return
            :outcome :done}}})))))

(deftest closed-message-contract-does-not-imply-semantic-knowledge
  (let [program
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :browser
            :server
            :example/command
            :done
            {:required #{:outcome}
             :optional #{:revision}})

           :done
           (choreo/return :done)}})

        completed
        (-> program
            semantics/start
            (semantics/step
             (semantics/communication-event
              :browser
              :server
              :example/command
              {:outcome :confirmed
               :revision 42})))]

    (is (= {}
           (semantics/values
            completed)))

    (is
     (false?
      (semantics/knows-value?
       completed
       :outcome)))

    (is
     (false?
      (semantics/knows-value?
       completed
       :revision)))))
