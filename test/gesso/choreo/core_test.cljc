(ns gesso.choreo.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]))

(defn- error-kind
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (:error/kind
       (ex-data e)))))

(deftest local-constructor
  (is (= {:op :local
          :role :browser
          :action :prepare
          :next :done}
         (choreo/local
          :browser
          :prepare
          :done)))

  (is (= {:op :local
          :role :browser
          :action :prepare
          :next :done
          :metadata {:source :test}}
         (choreo/local
          :browser
          :prepare
          :done
          {:metadata
           {:source :test}}))))

(deftest communicate-constructor
  (is (= {:op :communicate
          :from :browser
          :to :authority
          :event :request/claim
          :next :done}
         (choreo/communicate
          :browser
          :authority
          :request/claim
          :done)))

  (is (= {:op :communicate
          :from :browser
          :to :authority
          :event :request/claim
          :next :done
          :via :http
          :metadata {:source :test}}
         (choreo/communicate
          :browser
          :authority
          :request/claim
          :done
          {:via :http
           :metadata
           {:source :test}}))))

(deftest communicate-rejects-same-role
  (is (= :same-role-communication
         (error-kind
          #(choreo/communicate
            :browser
            :browser
            :example/event
            :done)))))

(deftest await-constructor
  (is (= {:op :await
          :role :browser
          :events
          {:request/completed :done
           :request/failed :failed}}
         (choreo/await
          :browser
          {:request/completed :done
           :request/failed :failed}))))

(deftest await-may-declare-closed-semantic-event-data-contracts
  (let [state
        (choreo/await
         :browser
         {:request/completed :done
          :request/failed :failed}
         {:event-contracts
          {:request/failed
           {:required #{:reason}
            :optional #{:status :retryable?}}}})]

    (is (= {:op :await
            :role :browser
            :events
            {:request/completed :done
             :request/failed :failed}
            :event-contracts
            {:request/failed
             {:required #{:reason}
              :optional #{:status :retryable?}}}}
           state))

    (testing "an event with no explicit contract has no contract entry"
      (is (nil?
           (get-in state
                   [:event-contracts
                    :request/completed]))))))

(deftest await-event-data-may-be-explicitly-open
  (let [state
        (choreo/await
         :browser
         {:browser/observed :done}
         {:event-contracts
          {:browser/observed
           {:required #{:basis}
            :open-data? true}}})]

    (is (= {:required #{:basis}
            :open-data? true}
           (get-in state
                   [:event-contracts
                    :browser/observed])))

    (is (= true
           (get-in state
                   [:event-contracts
                    :browser/observed
                    :open-data?])))))

(deftest await-event-contracts-must-name-declared-events
  (is (= :unknown-await-event-contract
         (error-kind
          #(choreo/await
            :browser
            {:request/completed :done}
            {:event-contracts
             {:request/failed
              {:required #{:reason}}}})))))

(deftest await-event-contract-key-sets-must-be-valid-and-disjoint
  (is (= :invalid-value-set
         (error-kind
          #(choreo/await
            :browser
            {:request/failed :failed}
            {:event-contracts
             {:request/failed
              {:required [:reason]}}}))))

  (is (= :invalid-value-set
         (error-kind
          #(choreo/await
            :browser
            {:request/failed :failed}
            {:event-contracts
             {:request/failed
              {:optional #{"status"}}}}))))

  (is (= :ambiguous-event-data-key
         (error-kind
          #(choreo/await
            :browser
            {:request/failed :failed}
            {:event-contracts
             {:request/failed
              {:required #{:reason}
               :optional #{:reason}}}})))))

(deftest await-event-open-data-must-be-boolean
  (is (= :invalid-open-data
         (error-kind
          #(choreo/await
            :browser
            {:browser/observed :done}
            {:event-contracts
             {:browser/observed
              {:open-data? :yes}}})))))

(deftest await-requires-at-least-one-environment-event
  (is (= :empty-await
         (error-kind
          #(choreo/await
            :browser
            {})))))

(deftest return-is-global-not-role-owned
  (let [state
        (choreo/return
         :done)]

    (is (= {:op :return
            :outcome :done}
           state))

    (is (= #{}
           (choreo/state-roles
            state)))

    (is (nil?
         (choreo/state-owner
          state)))))

(deftest choreography-construction-validates-the-semantic-program
  (let [choreography
        (choreo/->choreography
         {:name :example/claim
          :initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :request/claim
            :wait
            {:via :http})

           :wait
           (choreo/await
            :authority
            {:request/completed :done
             :request/failed :failed})

           :done
           (choreo/return
            :done)

           :failed
           (choreo/return
            :failed)}})]

    (is (choreo/choreography?
         choreography))

    (is (= :gesso.choreo.semantics/program
           (:gesso.choreo.semantics/type
            choreography)))

    (is (= choreo/choreography-version
           (:gesso.choreo.semantics/version
            choreography)))))

(deftest state-identities-are-opaque-edn
  (let [start
        [:state :start 1]

        done
        {:state :done
         :ordinal 2}

        choreography
        (choreo/->choreography
         {:initial start
          :states
          {start
           (choreo/local
            :browser
            :prepare
            done)

           done
           (choreo/return
            :done)}})]

    (is (= start
           (:initial choreography)))

    (is (= #{start
             done}
           (choreo/state-ids
            choreography)))

    (is (= done
           (:next
            (choreo/state
             choreography
             start))))))

(deftest unknown-successors-are-rejected-at-construction
  (is (= :unknown-successor
         (error-kind
          #(choreo/->choreography
            {:initial :start
             :states
             {:start
              (choreo/local
               :browser
               :prepare
               :missing)}}))))

  (is (= :unknown-successor
         (error-kind
          #(choreo/->choreography
            {:initial :wait
             :states
             {:wait
              (choreo/await
               :browser
               {:environment/done
                :missing})}})))))

(deftest unknown-initial-state-is-rejected
  (is (= :unknown-initial-state
         (error-kind
          #(choreo/->choreography
            {:initial :missing
             :states
             {:done
              (choreo/return
               :done)}})))))

(deftest malformed-state-operations-are-rejected
  (is (= :unsupported-op
         (error-kind
          #(choreo/->choreography
            {:initial :start
             :states
             {:start
              {:op :old/send
               :next :done}

              :done
              (choreo/return
               :done)}})))))

(deftest roles-are-inferred-from-the-authored-program
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :request/claim
            :wait)

           :wait
           (choreo/await
            :authority
            {:environment/completed
             :done})

           :done
           (choreo/return
            :done)}})]

    (is (= #{:browser
             :authority}
           (choreo/roles
            choreography)))))

(deftest graph-inspection-matches-the-small-semantic-language
  (let [local
        (choreo/local
         :browser
         :prepare
         :send)

        communicate
        (choreo/communicate
         :browser
         :authority
         :request/claim
         :wait)

        await
        (choreo/await
         :authority
         {:environment/ok :done
          :environment/fail :failed})

        terminal
        (choreo/return
         :done)]

    (testing "operation predicates"
      (is (= :local
             (choreo/state-op
              local)))

      (is (choreo/communication-state?
           communicate))

      (is (choreo/terminal-state?
           terminal))

      (is (false?
           (choreo/terminal-state?
            await))))

    (testing "participating roles"
      (is (= #{:browser}
             (choreo/state-roles
              local)))

      (is (= #{:browser
               :authority}
             (choreo/state-roles
              communicate)))

      (is (= #{:authority}
             (choreo/state-roles
              await)))

      (is (= #{}
             (choreo/state-roles
              terminal))))

    (testing "single local owner exists only where the semantics has one"
      (is (= :browser
             (choreo/state-owner
              local)))

      (is (= :authority
             (choreo/state-owner
              await)))

      (is (nil?
           (choreo/state-owner
            communicate)))

      (is (nil?
           (choreo/state-owner
            terminal))))

    (testing "successors"
      (is (= #{:send}
             (choreo/successors
              local)))

      (is (= #{:wait}
             (choreo/successors
              communicate)))

      (is (= #{:done
               :failed}
             (choreo/successors
              await)))

      (is (= #{}
             (choreo/successors
              terminal))))))

(deftest metadata-is-retained-but-does-not-change-graph-inspection
  (let [without-metadata
        (choreo/local
         :browser
         :prepare
         :done)

        with-metadata
        (choreo/local
         :browser
         :prepare
         :done
         {:metadata
          {:source
           {:line 10}}})]

    (is (= (:op without-metadata)
           (:op with-metadata)))

    (is (= (choreo/state-roles
            without-metadata)
           (choreo/state-roles
            with-metadata)))

    (is (= (choreo/successors
            without-metadata)
           (choreo/successors
            with-metadata)))))

(deftest ensure-choreography-normalizes-plain-program-data
  (let [plain
        {:name :example/plain
         :initial :start
         :states
         {:start
          {:op :local
           :role :browser
           :action :prepare
           :next :done}

          :done
          {:op :return
           :outcome :done}}}

        normalized
        (choreo/ensure-choreography
         plain)]

    (is (choreo/choreography?
         normalized))

    (is (= :example/plain
           (:name normalized)))

    (is (= #{:browser}
           (choreo/roles
            normalized)))))

(deftest explain-is-small-and-stable
  (let [choreography
        (choreo/->choreography
         {:name :example/explain
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
            :done)

           :done
           (choreo/return
            :done)}})]

    (is (= {:name :example/explain
            :version choreo/choreography-version
            :roles #{:browser
                     :authority}
            :initial :start
            :state-count 3
            :states-by-op
            {:local 1
             :communicate 1
             :return 1}}
           (choreo/explain
            choreography)))))

(deftest local-value-contracts-are-authored-explicitly
  (let [state
        (choreo/local
         :server
         :execute
         :done
         {:requires #{:command :principal}
          :outputs #{:outcome :revision}})]

    (is (= {:op :local
            :role :server
            :action :execute
            :next :done
            :requires #{:command :principal}
            :outputs #{:outcome :revision}}
           state))

    (is (= #{:command :principal}
           (choreo/local-requires state)))

    (is (= #{:outcome :revision}
           (choreo/local-outputs state)))))

(deftest empty-local-value-contracts-remain-absent-from-authored-data
  (let [state
        (choreo/local
         :browser
         :prepare
         :done
         {:requires #{}
          :outputs #{}})]

    (is (= {:op :local
            :role :browser
            :action :prepare
            :next :done}
           state))

    (is (= #{}
           (choreo/local-requires state)))

    (is (= #{}
           (choreo/local-outputs state)))))

(deftest local-value-contracts-must-be-keyword-sets
  (is (= :invalid-value-set
         (error-kind
          #(choreo/local
            :server
            :execute
            :done
            {:requires [:command]}))))

  (is (= :invalid-value-set
         (error-kind
          #(choreo/local
            :server
            :execute
            :done
            {:outputs #{"outcome"}})))))

(deftest branch-constructor
  (is (= {:op :branch
          :role :server
          :on :outcome
          :cases {:confirmed :confirmed
                  :rejected :rejected}}
         (choreo/branch
          :server
          :outcome
          {:confirmed :confirmed
           :rejected :rejected})))

  (is (= {:op :branch
          :role :server
          :on :outcome
          :cases {:confirmed :confirmed}
          :metadata {:source :test}}
         (choreo/branch
          :server
          :outcome
          {:confirmed :confirmed}
          {:metadata {:source :test}}))))

(deftest branch-requires-a-non-empty-case-map
  (is (= :empty-branch
         (error-kind
          #(choreo/branch
            :server
            :outcome
            {}))))

  (is (= :invalid-value
         (error-kind
          #(choreo/branch
            :server
            :outcome
            [:confirmed :done])))))

(deftest branch-rejects-nil-as-a-concrete-case-value
  (is (= :invalid-branch-value
         (error-kind
          #(choreo/branch
            :server
            :outcome
            {nil :missing
             :confirmed :done})))))

(deftest branch-selector-must-be-a-keyword
  (is (= :invalid-value
         (error-kind
          #(choreo/branch
            :server
            "outcome"
            {:confirmed :done})))))

(deftest branch-participates-in-role-owner-and-successor-inspection
  (let [state
        (choreo/branch
         :server
         :outcome
         {:confirmed :confirmed
          :rejected :rejected
          :failed :failed})]

    (is (choreo/branch-state? state))
    (is (false? (choreo/local-state? state)))
    (is (= #{:server}
           (choreo/state-roles state)))
    (is (= :server
           (choreo/state-owner state)))
    (is (= :outcome
           (choreo/branch-key state)))
    (is (= #{:confirmed :rejected :failed}
           (choreo/successors state)))))

(deftest non-local-and-non-branch-inspection-helpers-are-total
  (let [communication
        (choreo/communicate
         :browser
         :server
         :example/command
         :done)

        terminal
        (choreo/return :done)]

    (is (= #{}
           (choreo/local-requires communication)))
    (is (= #{}
           (choreo/local-outputs communication)))
    (is (nil?
         (choreo/branch-key communication)))
    (is (nil?
         (choreo/branch-key terminal)))))

(deftest choreography-with-local-output-and-branch-is-valid-authoring-data
  (let [choreography
        (choreo/->choreography
         {:name :example/settlement
          :initial :execute
          :states
          {:execute
           (choreo/local
            :server
            :execute-command
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
           (choreo/return :rejected)}})]

    (is (choreo/choreography? choreography))
    (is (= #{:server}
           (choreo/roles choreography)))
    (is (= {:local 1
            :branch 1
            :return 2}
           (:states-by-op
            (choreo/explain choreography))))))

(deftest branch-successors-are-validated-by-semantic-normalization
  (is (= :unknown-successor
         (error-kind
          #(choreo/->choreography
            {:initial :branch
             :states
             {:branch
              (choreo/branch
               :server
               :outcome
               {:confirmed :missing})}})))))

(deftest authoritative-constructor
  (is (= {:op :authoritative
          :role :server
          :operation :request/claim
          :next :done}
         (choreo/authoritative
          :server
          :request/claim
          :done)))

  (is (= {:op :authoritative
          :role :server
          :operation :request/claim
          :next :done
          :requires #{:request-id :principal}
          :outputs #{:outcome :revision}
          :metadata {:source :test}}
         (choreo/authoritative
          :server
          :request/claim
          :done
          {:requires #{:request-id :principal}
           :outputs #{:outcome :revision}
           :metadata {:source :test}}))))

(deftest authoritative-constructor-requires-keyword-role-and-operation
  (is (= :invalid-value
         (error-kind
          #(choreo/authoritative
            "server"
            :request/claim
            :done))))

  (is (= :invalid-value
         (error-kind
          #(choreo/authoritative
            :server
            "request/claim"
            :done)))))

(deftest authoritative-contract-keys-must-be-keyword-sets
  (is (= :invalid-value-set
         (error-kind
          #(choreo/authoritative
            :server
            :request/claim
            :done
            {:requires [:request-id]}))))

  (is (= :invalid-value-set
         (error-kind
          #(choreo/authoritative
            :server
            :request/claim
            :done
            {:outputs #{:outcome "revision"}})))))

(deftest authoritative-state-inspection
  (let [state
        (choreo/authoritative
         :server
         :request/claim
         :next
         {:requires #{:request-id}
          :outputs #{:outcome :revision}})]

    (is (choreo/authoritative-state?
         state))

    (is (false?
         (choreo/local-state?
          state)))

    (is (false?
         (choreo/branch-state?
          state)))

    (is (= #{:server}
           (choreo/state-roles
            state)))

    (is (= :server
           (choreo/state-owner
            state)))

    (is (= #{:next}
           (choreo/successors
            state)))

    (is (= :request/claim
           (choreo/authoritative-operation
            state)))

    (is (= #{:request-id}
           (choreo/authoritative-requires
            state)))

    (is (= #{:outcome :revision}
           (choreo/authoritative-outputs
            state)))

    (is (= #{:request-id}
           (choreo/action-requires
            state)))

    (is (= #{:outcome :revision}
           (choreo/action-outputs
            state)))))

(deftest local-and-authoritative-action-contracts-share-inspection-without-sharing-semantics
  (let [local
        (choreo/local
         :browser
         :prepare
         :done
         {:requires #{:request-id}
          :outputs #{:projection}})

        authoritative
        (choreo/authoritative
         :server
         :request/claim
         :done
         {:requires #{:request-id}
          :outputs #{:outcome}})]

    (is (= #{:request-id}
           (choreo/action-requires
            local)))

    (is (= #{:projection}
           (choreo/action-outputs
            local)))

    (is (= #{:request-id}
           (choreo/action-requires
            authoritative)))

    (is (= #{:outcome}
           (choreo/action-outputs
            authoritative)))

    (is (choreo/local-state?
         local))

    (is (false?
         (choreo/authoritative-state?
          local)))

    (is (choreo/authoritative-state?
         authoritative))

    (is (false?
         (choreo/local-state?
          authoritative)))))

(deftest authority-specific-inspection-helpers-are-total
  (let [local
        (choreo/local
         :browser
         :prepare
         :done
         {:requires #{:request-id}
          :outputs #{:projection}})

        communication
        (choreo/communicate
         :browser
         :server
         :example/command
         :done)

        terminal
        (choreo/return
         :done)]

    (doseq [state [local communication terminal]]
      (is (= #{}
             (choreo/authoritative-requires
              state)))

      (is (= #{}
             (choreo/authoritative-outputs
              state)))

      (is (nil?
           (choreo/authoritative-operation
            state))))))

(deftest choreography-with-authoritative-operation-is-valid-authoring-data
  (let [choreography
        (choreo/->choreography
         {:name :example/claim
          :initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :branch
            {:requires #{:request-id}
             :outputs #{:outcome :revision}})

           :branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/return
            :confirmed)

           :rejected
           (choreo/return
            :rejected)}})]

    (is (choreo/choreography?
         choreography))

    (is (= #{:server}
           (choreo/roles
            choreography)))

    (is (= {:authoritative 1
            :branch 1
            :return 2}
           (:states-by-op
            (choreo/explain
             choreography))))

    (is (= :request/claim
           (-> choreography
               (choreo/state :claim)
               choreo/authoritative-operation)))))

(deftest authoritative-successor-is-validated-by-semantic-normalization
  (is (= :unknown-successor
         (error-kind
          #(choreo/->choreography
            {:initial :claim
             :states
             {:claim
              (choreo/authoritative
               :server
               :request/claim
               :missing)}})))))

(deftest communicate-constructor-is-closed-by-default
  (is
   (= {:op :communicate
       :from :browser
       :to :server
       :event :example/command
       :next :done}
      (choreo/communicate
       :browser
       :server
       :example/command
       :done))))

(deftest communicate-constructor-retains-the-declared-payload-contract
  (is
   (= {:op :communicate
       :from :browser
       :to :server
       :event :example/command
       :next :done
       :via :http
       :required #{:execution-id :scope}
       :optional #{:base-revision :consistency-token}
       :correlation #{:execution-id :scope}
       :open-payload? true
       :metadata {:source :test}}
      (choreo/communicate
       :browser
       :server
       :example/command
       :done
       {:via :http
        :required #{:execution-id :scope}
        :optional #{:base-revision :consistency-token}
        :correlation #{:execution-id :scope}
        :open-payload? true
        :metadata {:source :test}}))))

(deftest communicate-omits-empty-contract-fields-and-false-open-marker
  (let [state
        (choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:required #{}
          :optional #{}
          :correlation #{}
          :open-payload? false})]

    (is
     (= {:op :communicate
         :from :browser
         :to :server
         :event :example/command
         :next :done}
        state))

    (is
     (false?
      (contains? state :required)))

    (is
     (false?
      (contains? state :optional)))

    (is
     (false?
      (contains? state :correlation)))

    (is
     (false?
      (contains? state :open-payload?)))))

(deftest communication-contract-keys-must-be-keyword-sets
  (is
   (= :invalid-value-set
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:required [:execution-id]}))))

  (is
   (= :invalid-value-set
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:optional #{:base-revision "reason"}}))))

  (is
   (= :invalid-value-set
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:correlation #{:execution-id 42}})))))

(deftest communication-required-and-optional-contracts-may-not-overlap
  (is
   (= :ambiguous-message-key
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:required #{:execution-id :scope}
          :optional #{:scope :base-revision}})))))

(deftest communication-correlation-keys-must-be-required
  (is
   (= :optional-correlation-key
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:required #{:execution-id}
          :optional #{:scope}
          :correlation #{:execution-id :scope}}))))

  (testing "a required correlation subset is valid"
    (is
     (= #{:execution-id :scope}
        (:correlation
         (choreo/communicate
          :browser
          :server
          :example/command
          :done
          {:required #{:execution-id :scope :transition}
           :correlation #{:execution-id :scope}}))))))

(deftest communication-open-payload-marker-must-be-boolean
  (is
   (= :invalid-open-payload
      (error-kind
       #(choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:open-payload? :yes}))))

  (is
   (true?
    (:open-payload?
     (choreo/communicate
      :browser
      :server
      :example/command
      :done
      {:open-payload? true})))))

(deftest communication-contract-inspection
  (let [state
        (choreo/communicate
         :browser
         :server
         :example/command
         :done
         {:required #{:execution-id :scope}
          :optional #{:base-revision :consistency-token}
          :correlation #{:execution-id :scope}})]

    (is
     (= #{:execution-id :scope}
        (choreo/communication-required state)))

    (is
     (= #{:base-revision :consistency-token}
        (choreo/communication-optional state)))

    (is
     (= #{:execution-id :scope}
        (choreo/communication-correlation state)))

    (is
     (= #{:execution-id
          :scope
          :base-revision
          :consistency-token}
        (choreo/communication-allowed state)))

    (is
     (false?
      (choreo/communication-open-payload?
       state)))))

(deftest communication-open-payload-inspection-remains-explicit
  (let [state
        (choreo/communicate
         :browser
         :server
         :example/open-command
         :done
         {:required #{:execution-id}
          :optional #{:known}
          :open-payload? true})]

    (is
     (choreo/communication-open-payload?
      state))

    (testing "allowed means the declared known contract, not every possible open key"
      (is
       (= #{:execution-id :known}
          (choreo/communication-allowed
           state))))))

(deftest communication-contract-inspection-is-total-on-noncommunications
  (doseq [state
          [(choreo/local
            :browser
            :prepare
            :done)

           (choreo/authoritative
            :server
            :request/claim
            :done)

           (choreo/return
            :done)]]

    (is
     (= #{}
        (choreo/communication-required
         state)))

    (is
     (= #{}
        (choreo/communication-optional
         state)))

    (is
     (= #{}
        (choreo/communication-correlation
         state)))

    (is
     (= #{}
        (choreo/communication-allowed
         state)))

    (is
     (false?
      (choreo/communication-open-payload?
       state)))))

(deftest full-closed-message-contract-survives-choreography-normalization
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
             :correlation #{:execution-id :scope}})

           :done
           (choreo/return :done)}})

        state
        (choreo/state
         choreography
         :send)]

    (is
     (= #{:execution-id :scope}
        (:required state)))

    (is
     (= #{:base-revision}
        (:optional state)))

    (is
     (= #{:execution-id :scope}
        (:correlation state)))

    (is
     (= :http
        (:via state)))

    (is
     (false?
      (choreo/communication-open-payload?
       state)))))
