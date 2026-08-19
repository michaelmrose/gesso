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
