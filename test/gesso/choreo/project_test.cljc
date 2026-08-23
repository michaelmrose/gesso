(ns gesso.choreo.project-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]
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

(defn- projected-state
  [plan state-id]
  (project/state
   plan
   state-id))

(defn- initial-state
  [plan]
  (projected-state
   plan
   (:initial plan)))

(defn- successor-state
  [plan state]
  (projected-state
   plan
   (:next state)))


(defn- verified-with-entry-knowledge
  [choreography entry-knowledge]
  (verify/verify!
   choreography
   {:entry-knowledge
    entry-knowledge}))

(deftest communication-projects-to-send-and-receive
  (let [choreography
        (choreo/->choreography
         {:name :example/hello
          :initial :communicate
          :states
          {:communicate
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done
            {:via :http})

           :done
           (choreo/return :done)}})

        plans
        (project/project-all
         choreography)

        alice
        (:alice plans)

        bob
        (:bob plans)

        alice-send
        (initial-state alice)

        bob-receive
        (initial-state bob)

        bob-alternative
        (first
         (:alternatives
          bob-receive))]

    (is (= #{:alice :bob}
           (set
            (keys plans))))

    (testing "the sender observes a send boundary"
      (is (= :send
             (:op alice-send)))

      (is (= :bob
             (:to alice-send)))

      (is (= :example/hello
             (:event alice-send)))

      (is (= :http
             (:via alice-send)))

      (is (= :return
             (:op
              (successor-state
               alice
               alice-send)))))

    (testing "the receiver observes a receive boundary"
      (is (= :receive
             (:op bob-receive)))

      (is (= 1
             (count
              (:alternatives
               bob-receive))))

      (is (= {:from :alice
              :event :example/hello
              :via :http}
             (select-keys
              bob-alternative
              [:from :event :via])))

      (is (= :return
             (:op
              (projected-state
               bob
               (:next bob-alternative))))))))

(deftest own-local-action-remains-and-foreign-local-action-disappears
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :alice
            :prepare-command
            :communicate)

           :communicate
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done)

           :done
           (choreo/return :done)}})

        plans
        (project/project-all
         choreography)

        alice
        (:alice plans)

        bob
        (:bob plans)

        alice-local
        (initial-state alice)

        bob-receive
        (initial-state bob)]

    (testing "Alice retains her own local semantic action"
      (is (= :local
             (:op alice-local)))

      (is (= :prepare-command
             (:action alice-local)))

      (is (= :send
             (:op
              (successor-state
               alice
               alice-local)))))

    (testing "Bob does not observe Alice's genuinely local action"
      (is (= :receive
             (:op bob-receive)))

      (is (= :example/hello
             (-> bob-receive
                 :alternatives
                 first
                 :event))))))

(deftest own-environment-await-remains-local
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:browser/ready :communicate})

           :communicate
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done)

           :done
           (choreo/return :done)}})

        plans
        (project/project-all
         choreography)

        alice
        (:alice plans)

        bob
        (:bob plans)

        alice-await
        (initial-state alice)

        bob-receive
        (initial-state bob)]

    (testing "the owner must observe its environment event"
      (is (= :await
             (:op alice-await)))

      (is (= #{:browser/ready}
             (set
              (keys
               (:events alice-await)))))

      (is (= :send
             (:op
              (projected-state
               alice
               (get
                (:events alice-await)
                :browser/ready))))))

    (testing "the foreign environment event itself is not projected to Bob"
      (is (= :receive
             (:op bob-receive)))

      (is (= :example/hello
             (-> bob-receive
                 :alternatives
                 first
                 :event))))))

(deftest foreign-environment-branches-may-converge-before-local-observation
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:environment/one :communicate
             :environment/two :communicate})

           :communicate
           (choreo/communicate
            :alice
            :bob
            :example/result
            :done)

           :done
           (choreo/return :done)}})

        bob
        (project/project
         choreography
         :bob)]

    (is (= :receive
           (:op
            (initial-state bob))))

    (is (= #{:example/result}
           (set
            (map
             :event
             (:alternatives
              (initial-state bob))))))))

(deftest foreign-environment-branches-can-be-distinguished-by-incoming-communication
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:environment/one :send-one
             :environment/two :send-two})

           :send-one
           (choreo/communicate
            :alice
            :bob
            :example/one
            :done)

           :send-two
           (choreo/communicate
            :alice
            :bob
            :example/two
            :done)

           :done
           (choreo/return :done)}})

        bob
        (project/project
         choreography
         :bob)

        receive
        (initial-state bob)]

    (is (= :receive
           (:op receive)))

    (is (= #{:example/one
             :example/two}
           (set
            (map
             :event
             (:alternatives receive)))))

    (is (every?
         (fn [alternative]
           (= :return
              (:op
               (projected-state
                bob
                (:next alternative)))))
         (:alternatives receive)))))

(deftest remote-control-flow-cannot-secretly-select-different-local-actions
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:environment/one :bob-one
             :environment/two :bob-two})

           :bob-one
           (choreo/local
            :bob
            :do-one
            :done)

           :bob-two
           (choreo/local
            :bob
            :do-two
            :done)

           :done
           (choreo/return :done)}})]

    (is
     (= :uncommunicated-control-flow
        (error-kind
         #(project/project
           choreography
           :bob))))))

(deftest remote-control-flow-cannot-mix-receive-and-direct-local-action
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :alice
            {:environment/message :send
             :environment/direct :bob-local})

           :send
           (choreo/communicate
            :alice
            :bob
            :example/result
            :done)

           :bob-local
           (choreo/local
            :bob
            :act-without-message
            :done)

           :done
           (choreo/return :done)}})]

    (is
     (= :mixed-observable-frontier
        (error-kind
         #(project/project
           choreography
           :bob))))))

(deftest communication-between-other-roles-is-not-observed
  (let [choreography
        (choreo/->choreography
         {:initial :alice-to-bob
          :states
          {:alice-to-bob
           (choreo/communicate
            :alice
            :bob
            :example/first
            :bob-to-carol)

           :bob-to-carol
           (choreo/communicate
            :bob
            :carol
            :example/second
            :done)

           :done
           (choreo/return :done)}})

        carol
        (project/project
         choreography
         :carol)]

    (is (= :receive
           (:op
            (initial-state carol))))

    (is (= :bob
           (-> carol
               initial-state
               :alternatives
               first
               :from)))

    (is (= :example/second
           (-> carol
               initial-state
               :alternatives
               first
               :event)))))

(deftest projection-rejects-a-role-not-present-in-the-choreography
  (let [choreography
        (choreo/->choreography
         {:initial :communicate
          :states
          {:communicate
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done)

           :done
           (choreo/return :done)}})]

    (is
     (= :unknown-role
        (error-kind
         #(project/project
           choreography
           :carol))))))

(deftest opaque-source-state-identities-do-not-become-runtime-locators
  (let [start
        [:global :start]

        done
        [:global :done]

        choreography
        (choreo/->choreography
         {:initial start
          :states
          {start
           (choreo/communicate
            :alice
            :bob
            :example/hello
            done)

           done
           (choreo/return :done)}})

        alice
        (project/project
         choreography
         :alice)

        runtime-initial
        (:initial alice)]

    (is (and (integer? runtime-initial)
             (not (neg? runtime-initial))))

    (is (not= start
              runtime-initial))

    (is (= :send
           (:op
            (projected-state
             alice
             runtime-initial))))

    (is (not-any? #(= start %)
                  (tree-seq coll? seq alice)))

    (is (not-any? #(= done %)
                  (tree-seq coll? seq alice)))))
(deftest local-value-contracts-are-preserved-in-the-owner-projection
  (let [choreography
        (choreo/->choreography
         {:initial :seed
          :states
          {:seed
           (choreo/local
            :browser
            :seed-request
            :compute
            {:outputs #{:request-id}})

           :compute
           (choreo/local
            :browser
            :compute-outcome
            :done
            {:requires #{:request-id}
             :outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        browser
        (project/project
         choreography
         :browser)

        seed
        (initial-state browser)

        state
        (successor-state
         browser
         seed)]

    (is (= :local
           (:op state)))

    (is (= :compute-outcome
           (:action state)))

    (is (= #{:request-id}
           (:requires state)))

    (is (= #{:outcome :revision}
           (:outputs state)))))

(deftest owner-branch-is-preserved-as-local-deterministic-control-flow
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/local
            :server
            :decide
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :confirmed-send
             :rejected :rejected-send})

           :confirmed-send
           (choreo/communicate
            :server
            :browser
            :settlement/confirmed
            :done)

           :rejected-send
           (choreo/communicate
            :server
            :browser
            :settlement/rejected
            :done)

           :done
           (choreo/return :done)}})

        server
        (project/project
         choreography
         :server)

        decide
        (initial-state server)

        branch
        (successor-state
         server
         decide)]

    (is (= :local
           (:op decide)))

    (is (= #{:outcome}
           (:outputs decide)))

    (is (= :branch
           (:op branch)))

    (is (= :outcome
           (:on branch)))

    (is (= #{:confirmed
             :rejected}
           (set
            (keys
             (:cases branch)))))

    (doseq [[outcome expected-event]
            [[:confirmed :settlement/confirmed]
             [:rejected :settlement/rejected]]]
      (let [send
            (projected-state
             server
             (get
              (:cases branch)
              outcome))]

        (is (= :send
               (:op send)))

        (is (= expected-event
               (:event send)))

        (is (= :browser
               (:to send)))))))

(deftest foreign-branch-may-converge-before-another-role-observes-anything
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/local
            :alice
            :decide
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :alice
            :outcome
            {:one :send
             :two :send})

           :send
           (choreo/communicate
            :alice
            :bob
            :example/result
            :done)

           :done
           (choreo/return :done)}})

        bob
        (project/project
         choreography
         :bob)

        receive
        (initial-state bob)]

    (is (= :receive
           (:op receive)))

    (is (= #{:example/result}
           (set
            (map
             :event
             (:alternatives receive)))))))

(deftest foreign-branch-may-become-distinguishable-through-communication
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/local
            :alice
            :decide
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :alice
            :outcome
            {:one :send-one
             :two :send-two})

           :send-one
           (choreo/communicate
            :alice
            :bob
            :example/one
            :done)

           :send-two
           (choreo/communicate
            :alice
            :bob
            :example/two
            :done)

           :done
           (choreo/return :done)}})

        bob
        (project/project
         choreography
         :bob)

        receive
        (initial-state bob)]

    (is (= :receive
           (:op receive)))

    (is (= #{:example/one
             :example/two}
           (set
            (map
             :event
             (:alternatives receive)))))))

(deftest foreign-branch-cannot-secretly-select-another-roles-local-action
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/local
            :alice
            :decide
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :alice
            :outcome
            {:one :bob-one
             :two :bob-two})

           :bob-one
           (choreo/local
            :bob
            :do-one
            :done)

           :bob-two
           (choreo/local
            :bob
            :do-two
            :done)

           :done
           (choreo/return :done)}})]

    (is (= :uncommunicated-control-flow
           (error-kind
            #(project/project
              choreography
              :bob))))))

(deftest foreign-branch-cannot-mix-local-completion-and-incoming-communication
  (let [choreography
        (choreo/->choreography
         {:initial :decide
          :states
          {:decide
           (choreo/local
            :alice
            :decide
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :alice
            :outcome
            {:stop :done
             :notify :send})

           :send
           (choreo/communicate
            :alice
            :bob
            :example/result
            :done)

           :done
           (choreo/return :done)}})]

    (is (= :mixed-observable-frontier
           (error-kind
            #(project/project
              choreography
              :bob))))))

(deftest projected-explain-counts-branch-states
  (let [choreography
        (choreo/->choreography
         {:initial :compute
          :states
          {:compute
           (choreo/local
            :browser
            :compute
            :branch
            {:outputs #{:choice}})

           :branch
           (choreo/branch
            :browser
            :choice
            {:left :done
             :right :done})

           :done
           (choreo/return :done)}})

        browser
        (project/project
         choreography
         :browser)

        explanation
        (project/explain
         browser)]

    (is (= :browser
           (:role explanation)))

    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :local])))

    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :branch])))

    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :return])))))

(deftest authoritative-operation-is-preserved-in-owner-projection
  (let [choreography
        (choreo/->choreography
         {:initial :seed
          :states
          {:seed
           (choreo/local
            :server
            :seed-authoritative-inputs
            :claim
            {:outputs #{:request-id :principal}})

           :claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:requires #{:request-id :principal}
             :outputs #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        server
        (project/project
         choreography
         :server)

        seed
        (initial-state server)

        claim
        (successor-state server seed)]

    (is (= :authoritative
           (:op claim)))

    (is (= :request/claim
           (:operation claim)))

    (is (= #{:request-id :principal}
           (:requires claim)))

    (is (= #{:outcome :revision}
           (:outputs claim)))

    (is (= :return
           (:op
            (successor-state
             server
             claim))))))

(deftest authoritative-operation-remains-distinct-from-local-work-in-projection
  (let [choreography
        (choreo/->choreography
         {:initial :prepare
          :states
          {:prepare
           (choreo/local
            :server
            :prepare
            :claim)

           :claim
           (choreo/authoritative
            :server
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        server
        (project/project
         choreography
         :server)

        local
        (initial-state server)

        authority
        (successor-state
         server
         local)]

    (is (= :local
           (:op local)))

    (is (= :authoritative
           (:op authority)))

    (is (= :request/claim
           (:operation authority)))))

(deftest foreign-authoritative-predecessor-blocks-unsynchronized-local-work
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :browser-local)

           :browser-local
           (choreo/local
            :browser
            :render-result
            :done)

           :done
           (choreo/return :done)}})]

    (is (= :unobserved-authoritative-predecessor
           (error-kind
            #(project/project
              choreography
              :browser))))))

(deftest incoming-communication-clears-foreign-authoritative-causal-barrier
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :notify)

           :notify
           (choreo/communicate
            :server
            :browser
            :request/settled
            :browser-local)

           :browser-local
           (choreo/local
            :browser
            :install-result
            :done)

           :done
           (choreo/return :done)}})

        browser
        (project/project
         choreography
         :browser)

        receive
        (initial-state browser)

        alternative
        (first
         (:alternatives receive))

        local
        (projected-state
         browser
         (:next alternative))]

    (is (= :receive
           (:op receive)))

    (is (= {:from :server
            :event :request/settled}
           (select-keys
            alternative
            [:from :event])))

    (is (= :local
           (:op local)))

    (is (= :install-result
           (:action local)))))

(deftest unrelated-communication-does-not-clear-authoritative-causal-barrier
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :audit)

           :audit
           (choreo/communicate
            :server
            :auditor
            :request/audited
            :browser-local)

           :browser-local
           (choreo/local
            :browser
            :install-result
            :done)

           :done
           (choreo/return :done)}})]

    (is (= :unobserved-authoritative-predecessor
           (error-kind
            #(project/project
              choreography
              :browser))))))

(deftest foreign-authoritative-predecessor-also-blocks-unsynchronized-send
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :browser-send)

           :browser-send
           (choreo/communicate
            :browser
            :auditor
            :browser/observed-result
            :done)

           :done
           (choreo/return :done)}})]

    (is (= :unobserved-authoritative-predecessor
           (error-kind
            #(project/project
              choreography
              :browser))))))

(deftest foreign-authoritative-predecessor-does-not-force-a-role-to-wait-when-it-has-no-more-work
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done)

           :unused-browser
           (choreo/local
            :browser
            :unused
            :done)

           :done
           (choreo/return :done)}})

        browser
        (project/project
         choreography
         :browser)]

    (is (= :return
           (:op
            (initial-state browser))))

    (is (= :gesso.choreo/complete
           (:outcome
            (initial-state browser))))))

(deftest authoritative-output-can-drive-owner-branch-after-projection
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
           (choreo/communicate
            :server
            :browser
            :settlement/confirmed
            :done)

           :rejected
           (choreo/communicate
            :server
            :browser
            :settlement/rejected
            :done)

           :done
           (choreo/return :done)}})

        server
        (project/project
         choreography
         :server)

        authority
        (initial-state server)

        branch
        (successor-state
         server
         authority)]

    (is (= :authoritative
           (:op authority)))

    (is (= #{:outcome}
           (:outputs authority)))

    (is (= :branch
           (:op branch)))

    (is (= :outcome
           (:on branch)))

    (is (= #{:confirmed :rejected}
           (set
            (keys
             (:cases branch)))))))

(deftest projected-explain-counts-authoritative-states
  (let [choreography
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

        server
        (project/project
         choreography
         :server)

        explanation
        (project/explain
         server)]

    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :authoritative])))

    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :return])))))

(deftest closed-message-contract-is-preserved-on-both-projected-sides
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
             :optional #{:base-revision :consistency-token}
             :correlation #{:execution-id :scope}})

           :done
           (choreo/return :done)}})

        plans
        (project/project-all
         (verified-with-entry-knowledge
          choreography
          {:browser
           #{:execution-id :scope}}))

        browser
        (:browser plans)

        server
        (:server plans)

        send
        (initial-state browser)

        receive
        (initial-state server)

        alternative
        (first
         (:alternatives receive))]

    (testing "the sender retains the exact declared contract"
      (is (= :send
             (:op send)))

      (is (= #{:execution-id :scope}
             (:required send)))

      (is (= #{:base-revision :consistency-token}
             (:optional send)))

      (is (= #{:execution-id :scope}
             (:correlation send)))

      (is (false?
           (contains?
            send
            :open-payload?))))

    (testing "the receiver retains the same declared contract"
      (is (= :receive
             (:op receive)))

      (is (= #{:execution-id :scope}
             (:required alternative)))

      (is (= #{:base-revision :consistency-token}
             (:optional alternative)))

      (is (= #{:execution-id :scope}
             (:correlation alternative)))

      (is (false?
           (contains?
            alternative
            :open-payload?))))))

(deftest explicitly-open-message-contract-is-preserved-on-both-projected-sides
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

        plans
        (project/project-all
         (verified-with-entry-knowledge
          choreography
          {:browser
           #{:execution-id}}))

        send
        (initial-state
         (:browser plans))

        alternative
        (-> plans
            :server
            initial-state
            :alternatives
            first)]

    (is (= true
           (:open-payload? send)))

    (is (= true
           (:open-payload? alternative)))

    (is (= #{:execution-id}
           (:required send)))

    (is (= #{:execution-id}
           (:required alternative)))))

(deftest empty-message-contract-remains-implicit-after-projection
  (let [choreography
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

        plans
        (project/project-all choreography)

        send
        (initial-state
         (:alice plans))

        alternative
        (-> plans
            :bob
            initial-state
            :alternatives
            first)]

    (doseq [projected
            [send alternative]]

      (is
       (false?
        (contains?
         projected
         :required)))

      (is
       (false?
        (contains?
         projected
         :optional)))

      (is
       (false?
        (contains?
         projected
         :correlation)))

      (is
       (false?
        (contains?
         projected
         :open-payload?))))))

(deftest same-message-name-with-different-contracts-remains-distinct-at-receive-frontier
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :confirmed}})

           :two
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :rejected}})

           :done
           (choreo/return :done)}})

        browser
        (project/project
         (verified-with-entry-knowledge
          choreography
          {:server
           #{:execution-id :confirmed :rejected}})
         :browser)

        receive
        (initial-state browser)

        alternatives
        (:alternatives receive)]

    (is (= :receive
           (:op receive)))

    (is (= 2
           (count alternatives)))

    (is (= #{#{:execution-id :confirmed}
             #{:execution-id :rejected}}
           (set
            (map
             :required
             alternatives))))

    (is (= #{:example/result}
           (set
            (map
             :event
             alternatives))))))

(deftest same-message-name-and-contract-still-coalesces-when-continuation-is-the-same
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :outcome}})

           :two
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id :outcome}})

           :done
           (choreo/return :done)}})

        browser
        (project/project
         (verified-with-entry-knowledge
          choreography
          {:server
           #{:execution-id :outcome}})
         :browser)

        alternatives
        (:alternatives
         (initial-state browser))]

    (is (= 1
           (count alternatives)))

    (is (= #{:execution-id :outcome}
           (:required
            (first alternatives))))))

(deftest overlapping-closed-receive-contracts-are-rejected-statically
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :server
            :browser
            :example/result
            :one-done
            {:required #{:execution-id}
             :optional #{:detail}})

           :two
           (choreo/communicate
            :server
            :browser
            :example/result
            :two-done
            {:required #{:execution-id}
             :optional #{:other}})

           :one-done
           (choreo/return :one)

           :two-done
           (choreo/return :two)}})]

    ;; {:execution-id ...} satisfies both contracts. A compiler-generated
    ;; receive gate must never defer this known ambiguity to the runtime
    ;; machine merely because the continuations are different.
    (is (= :ambiguous-receive
           (error-kind
            #(project/project
              (verified-with-entry-knowledge
               choreography
               {:server #{:execution-id}})
              :browser))))))

(deftest overlapping-receive-contracts-are-rejected-even-when-continuation-is-the-same
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id}
             :optional #{:detail}})

           :two
           (choreo/communicate
            :server
            :browser
            :example/result
            :done
            {:required #{:execution-id}
             :optional #{:other}})

           :done
           (choreo/return :done)}})]

    ;; Coalescing these would silently invent a third payload/knowledge contract;
    ;; retaining both would produce a runtime-ambiguous receive. Neither is a
    ;; faithful projection of the authored alternatives.
    (is (= :ambiguous-receive
           (error-kind
            #(project/project
              (verified-with-entry-knowledge
               choreography
               {:server #{:execution-id}})
              :browser))))))

(deftest open-and-closed-overlapping-receive-contracts-are-rejected-statically
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/open :open
             :environment/closed :closed})

           :open
           (choreo/communicate
            :server
            :browser
            :example/result
            :open-done
            {:required #{:execution-id}
             :open-payload? true})

           :closed
           (choreo/communicate
            :server
            :browser
            :example/result
            :closed-done
            {:required #{:execution-id :outcome}})

           :open-done
           (choreo/return :open)

           :closed-done
           (choreo/return :closed)}})]

    ;; {:execution-id ... :outcome ...} satisfies both alternatives.
    (is (= :ambiguous-receive
           (error-kind
            #(project/project
              (verified-with-entry-knowledge
               choreography
               {:server #{:execution-id :outcome}})
              :browser))))))

(deftest two-open-receive-contracts-with-the-same-message-identity-are-rejected-statically
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :server
            {:environment/one :one
             :environment/two :two})

           :one
           (choreo/communicate
            :server
            :browser
            :example/result
            :one-done
            {:required #{:execution-id}
             :open-payload? true})

           :two
           (choreo/communicate
            :server
            :browser
            :example/result
            :two-done
            {:required #{:execution-id :outcome}
             :open-payload? true})

           :one-done
           (choreo/return :one)

           :two-done
           (choreo/return :two)}})]

    ;; For the same from/event/via identity, two open contracts always have a
    ;; non-empty intersection: a payload containing the union of required keys.
    (is (= :ambiguous-receive
           (error-kind
            #(project/project
              (verified-with-entry-knowledge
               choreography
               {:server #{:execution-id :outcome}})
              :browser))))))


;; -----------------------------------------------------------------------------
;; Canonical ExecutablePlan boundary
;; -----------------------------------------------------------------------------

(defn- executable-locators
  [plan]
  (set
   (keys
    (:states plan))))

(defn- successor-locators
  [state]
  (case (:op state)
    :local
    #{(:next state)}

    :authoritative
    #{(:next state)}

    :send
    #{(:next state)}

    :branch
    (set
     (vals
      (:cases state)))

    :receive
    (set
     (map :next
          (:alternatives state)))

    :await
    (set
     (vals
      (:events state)))

    :return
    #{}

    #{}))

(defn- tree-contains-value?
  [root value]
  (boolean
   (some #(= value %)
         (tree-seq coll? seq root))))

(defn- executable-plan-fixture
  [states]
  (choreo/->choreography
   {:name :example/executable-plan
    :initial [:source :prepare 9001]
    :states states}))

(defn- executable-plan-states-a
  []
  (array-map
   [:source :prepare 9001]
   (choreo/local
    :alice
    :prepare
    [:source :send 9002])

   [:source :send 9002]
   (choreo/communicate
    :alice
    :bob
    :example/message
    [:source :done 9003]
    {:via :http
     :required #{:request-id}
     :correlation #{:request-id}})

   [:source :done 9003]
   (choreo/return :done)))

(defn- executable-plan-states-b
  []
  ;; Same semantic graph, deliberately authored in another map insertion order.
  (array-map
   [:source :done 9003]
   (choreo/return :done)

   [:source :send 9002]
   (choreo/communicate
    :alice
    :bob
    :example/message
    [:source :done 9003]
    {:via :http
     :required #{:request-id}
     :correlation #{:request-id}})

   [:source :prepare 9001]
   (choreo/local
    :alice
    :prepare
    [:source :send 9002])))

(deftest projection-emits-canonical-executable-plan-with-compact-runtime-locators
  (let [source-state-ids
        #{[:source :prepare 9001]
          [:source :send 9002]
          [:source :done 9003]}

        plans
        (project/project-all
         (verify/verify!
          (executable-plan-fixture
           (executable-plan-states-a))
          {:entry-knowledge
           {:alice #{:request-id}}}))]

    (doseq [[role plan] plans]
      (testing (str "role " role " receives only executable runtime locators")
        (is (= :gesso.choreo/executable-plan
               (:gesso.choreo/type plan)))

        (is (= 1
               (:gesso.choreo/version plan)))

        (is (= role
               (:role plan)))

        (let [locators
              (executable-locators plan)

              expected-locators
              (set
               (range
                (count
                 (:states plan))))]

          (is (= expected-locators
                 locators))

          (is (contains? locators
                         (:initial plan)))

          (is (every? #(and (integer? %)
                            (not (neg? %)))
                      locators))

          (doseq [[locator state]
                  (:states plan)]
            (is (integer? locator))
            (is (every? locators
                        (successor-locators state)))))

        (doseq [source-state-id source-state-ids]
          (is (false?
               (tree-contains-value?
                plan
                source-state-id))))))))

(deftest compact-runtime-locators-cover-every-current-projected-successor-position
  (let [start [:semantic :start]
        claim [:semantic :claim]
        decide [:semantic :decide]
        send [:semantic :send]
        wait [:semantic :wait]
        done [:semantic :done]

        choreography
        (choreo/->choreography
         {:name :example/all-local-successors
          :initial start
          :states
          {start
           (choreo/local
            :alice
            :prepare
            claim
            {:outputs #{:request-id}})

           claim
           (choreo/authoritative
            :alice
            :request/claim
            decide
            {:requires #{:request-id}
             :outputs #{:outcome}})

           decide
           (choreo/branch
            :alice
            :outcome
            {:confirmed send
             :rejected wait})

           send
           (choreo/communicate
            :alice
            :bob
            :example/message
            done
            {:required #{:request-id}})

           wait
           (choreo/await
            :alice
            {:environment/retry done})

           done
           (choreo/return :done)}})

        plan
        (project/project choreography :alice)

        locators
        (executable-locators plan)]

    (is (= :gesso.choreo/executable-plan
           (:gesso.choreo/type plan)))

    (is (= (set (range (count (:states plan))))
           locators))

    (doseq [[locator state] (:states plan)]
      (is (and (integer? locator)
               (not (neg? locator))))
      (is (every? locators
                  (successor-locators state))))

    (doseq [source-state-id [start claim decide send wait done]]
      (is (false?
           (tree-contains-value?
            plan
            source-state-id))))))

(deftest executable-plan-layout-is-deterministic-across-source-map-insertion-order
  (let [verification-options
        {:entry-knowledge
         {:alice #{:request-id}}}

        first-plans
        (project/project-all
         (verify/verify!
          (executable-plan-fixture
           (executable-plan-states-a))
          verification-options))

        second-plans
        (project/project-all
         (verify/verify!
          (executable-plan-fixture
           (executable-plan-states-b))
          verification-options))]

    (is (= first-plans
           second-plans))))

(deftest executable-plan-does-not-contain-proof-or-diagnostic-sidecar-data
  (let [plan
        (project/project
         (verify/verify!
          (executable-plan-fixture
           (executable-plan-states-a))
          {:entry-knowledge
           {:alice #{:request-id}}})
         :alice)

        forbidden-keys
        #{:verification
          :verified
          :proof
          :proofs
          :obligations
          :counterexample
          :derivation
          :derivations
          :diagnostic
          :diagnostics
          :global-state
          :global-state-id
          :semantic-state-id
          :source-state
          :source-state-id
          :metadata}

        all-maps
        (filter map?
                (tree-seq coll? seq plan))]

    (is (= :gesso.choreo/executable-plan
           (:gesso.choreo/type plan)))

    (is (every?
         (fn [m]
           (empty?
            (set/intersection
             forbidden-keys
             (set (keys m)))))
         all-maps))))

;; -----------------------------------------------------------------------------
;; Authoritative observation / reread as a projection synchronization path
;; -----------------------------------------------------------------------------

(def authoritative-reread-contract
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(defn- projection-result
  [choreography role]
  (try
    {:plan (project/project choreography role)}
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      {:error-kind (:error/kind (ex-data e))
       :error-data (ex-data e)})))

(deftest authoritative-observation-contract-survives-public-await-construction
  (let [wait
        (choreo/await
         :browser
         {:request/reread-complete :done}
         {:event-contracts
          {:request/reread-complete
           {:required #{:request-status :observed-basis}
            :authoritative-observation
            authoritative-reread-contract}}})]

    ;; This descriptor is semantic compiler/runtime data, not metadata.  The
    ;; projector and machine need it to distinguish a trusted authoritative
    ;; reread from an ordinary environment event that merely happens later.
    (is (= authoritative-reread-contract
           (get-in wait
                   [:event-contracts
                    :request/reread-complete
                    :authoritative-observation])))))

(deftest authoritative-observation-clears-foreign-authoritative-causal-barrier
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :observe)

           :observe
           (choreo/await
            :browser
            {:request/reread-complete :browser-local}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               authoritative-reread-contract}}})

           :browser-local
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status}})

           :done
           (choreo/return :done)}})

        result
        (projection-result choreography :browser)

        browser
        (:plan result)]

    ;; A role need not receive a participant message solely to learn that a
    ;; durable authoritative fact changed.  A declared authoritative reread is
    ;; another valid observation frontier.
    (is (nil? (:error-kind result))
        (str "Projection rejected authoritative reread with "
             (:error-kind result)))

    (when browser
      (let [await-state
            (initial-state browser)

            next-state
            (projected-state
             browser
             (get-in await-state
                     [:events :request/reread-complete]))]

        (is (= :await
               (:op await-state)))

        (is (= authoritative-reread-contract
               (get-in await-state
                       [:event-contracts
                        :request/reread-complete
                        :authoritative-observation])))

        (is (= :local
               (:op next-state)))

        (is (= :install-result
               (:action next-state)))))))

(deftest ordinary-environment-event-does-not-clear-foreign-authoritative-causal-barrier
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :wait)

           :wait
           (choreo/await
            :browser
            {:browser/timer-fired :browser-local})

           :browser-local
           (choreo/local
            :browser
            :install-result
            :done)

           :done
           (choreo/return :done)}})]

    ;; Arrival after the authoritative transition in wall-clock time is not a
    ;; causal/knowledge proof.  Timers, visibility events, invalidation nudges,
    ;; and other ordinary environment events must remain unable to clear the
    ;; foreign-authority barrier by coincidence.
    (is (= :unobserved-authoritative-predecessor
           (error-kind
            #(project/project
              choreography
              :browser))))))

;; -----------------------------------------------------------------------------
;; ExecutablePlan integrity boundary
;; -----------------------------------------------------------------------------

(defn- integrity-executable-plan
  []
  (project/project
   (choreo/->choreography
    {:name :example/executable-plan-integrity
     :initial :prepare
     :states
     {:prepare
      (choreo/local
       :browser
       :prepare
       :done)

      :done
      (choreo/return :done)}})
   :browser))

(defn- executable-plan-lookalikes
  [plan]
  (let [initial
        (:initial plan)]
    {:unknown-top-level-key
     (assoc plan
            :diagnostic-only true)

     :negative-runtime-locator
     {:gesso.choreo/type :gesso.choreo/executable-plan
      :gesso.choreo/version project/executable-plan-version
      :role :browser
      :initial -1
      :states
      {-1 {:op :return
           :outcome :gesso.choreo/complete}}}

     :non-integer-runtime-locator
     {:gesso.choreo/type :gesso.choreo/executable-plan
      :gesso.choreo/version project/executable-plan-version
      :role :browser
      :initial "start"
      :states
      {"start" {:op :return
                 :outcome :gesso.choreo/complete}}}

     :non-compact-runtime-locators
     {:gesso.choreo/type :gesso.choreo/executable-plan
      :gesso.choreo/version project/executable-plan-version
      :role :browser
      :initial 0
      :states
      {0 {:op :local
          :action :prepare
          :next 2}
       2 {:op :return
          :outcome :gesso.choreo/complete}}}

     :unsupported-operation
     (assoc-in plan
               [:states initial :op]
               :not-a-choreo-operation)

     :unknown-successor
     (assoc-in plan
               [:states initial :next]
               999)

     :non-map-state
     (assoc-in plan
               [:states initial]
               :not-a-state)

     :unknown-state-key
     (assoc-in plan
               [:states initial :adapter/private]
               true)}))

(deftest executable-plan-predicate-recognizes-the-canonical-runtime-format-not-just-a-tagged-map
  (let [plan
        (integrity-executable-plan)]

    (is (project/executable-plan? plan))
    (is (= plan
           (project/ensure-executable-plan plan)))

    (testing "runtime locators are the canonical compact non-negative integer range"
      (is (= 0 (:initial plan)))
      (is (= (set (range (count (:states plan))))
             (set (keys (:states plan))))))

    (doseq [[label lookalike]
            (executable-plan-lookalikes plan)]
      (testing (name label)
        (is (false?
             (project/executable-plan? lookalike)))))))

(deftest ensure-executable-plan-fails-closed-for-convincing-lookalikes
  (let [plan
        (integrity-executable-plan)]

    (doseq [[label lookalike]
            (executable-plan-lookalikes plan)]
      (testing (name label)
        (is (= :invalid-executable-plan
               (error-kind
                #(project/ensure-executable-plan
                  lookalike))))))))
