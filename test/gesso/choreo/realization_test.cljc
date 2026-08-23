(ns gesso.choreo.realization-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.identity :as identity]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.realization :as realization]
   [gesso.choreo.semantics :as semantics]
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

(defn- claim-choreography
  []
  (choreo/->choreography
   {:name :example/claim
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :browser
      :prepare-command
      :submit
      {:outputs #{:request-id}})

     :submit
     (choreo/communicate
      :browser
      :server
      :request/claim
      :claim
      {:via :http
       :required #{:request-id}})

     :claim
     (choreo/authoritative
      :server
      :request/claim
      :decision
      {:requires #{:request-id}
       :outputs #{:outcome :revision}})

     :decision
     (choreo/branch
      :server
      :outcome
      {:confirmed :settle-confirmed
       :rejected :settle-rejected})

     :settle-confirmed
     (choreo/communicate
      :server
      :browser
      :request/settled
      :done
      {:via :http
       :required #{:outcome :revision}})

     :settle-rejected
     (choreo/communicate
      :server
      :browser
      :request/settled
      :done
      {:via :http
       :required #{:outcome :revision}})

     :done
     (choreo/return :done)}}))

(defn- complete-confirmed-claim
  [started]
  (let [after-local
        (realization/complete-local
         started
         :browser
         {:request-id 17})

        {after-browser-send :realization
         command-message-id :message-id}
        (realization/complete-send
         after-local
         :browser
         {:request-id 17})

        after-command
        (realization/deliver-message
         after-browser-send
         command-message-id)

        after-authority
        (realization/complete-authoritative
         after-command
         :server
         {:outcome :confirmed
          :revision 42})

        {after-server-send :realization
         settlement-message-id :message-id}
        (realization/complete-send
         after-authority
         :server
         {:outcome :confirmed
          :revision 42})

        completed
        (realization/deliver-message
         after-server-send
         settlement-message-id)]
    {:completed completed
     :command-message-id command-message-id
     :settlement-message-id settlement-message-id}))

(deftest start-projects-and-starts-every-role-independently
  (let [started
        (realization/start
         (claim-choreography))

        browser
        (realization/execution
         started
         :browser)

        server
        (realization/execution
         started
         :server)]

    (is
     (realization/realization?
      started))

    (is
     (= #{:browser :server}
        (realization/roles
         started)))

    (is
     (not
      (identical?
       browser
       server)))

    (is
     (= :browser
        (:role browser)))

    (is
     (= :server
        (:role server)))

    (is
     (machine/waiting-local?
      browser))

    (is
     (machine/waiting-receive?
      server))

    (is
     (= #{:browser}
        (set
         (keys
          (realization/boundaries
           started)))))

    (is
     (= []
        (realization/messages
         started)))

    (is
     (= []
        (realization/history
         started)))))

(deftest sender-emits-before-receiver-learns
  (let [started
        (realization/start
         (claim-choreography))

        after-local
        (realization/complete-local
         started
         :browser
         {:request-id 17})

        {after-send :realization
         message-id :message-id
         message :message}
        (realization/complete-send
         after-local
         :browser
         {:request-id 17})

        browser
        (realization/execution
         after-send
         :browser)

        server-before-delivery
        (realization/execution
         after-send
         :server)

        sender-state
        (machine/current-state-id
         (realization/execution
          after-local
          :browser))]

    (is
     (= {:kind :message
         :from :browser
         :to :server
         :event :request/claim
         :payload {:request-id 17}
         :via :http}
        message))

    (is
     (= [{:message-id message-id
          :message message
          :sent-by :browser
          :sender-state sender-state}]
        (realization/messages
         after-send)))

    (testing "send completion advances only the sender"
      (is
       (machine/waiting-receive?
        browser))

      (is
       (machine/waiting-receive?
        server-before-delivery))

      (is
       (false?
        (machine/has-execution-value?
         server-before-delivery
         :request-id))))

    (testing "the emitted message becomes deliverable at the receiver's gate"
      (is
       (= [message-id]
          (realization/deliverable-message-ids
           after-send))))

    (let [after-delivery
          (realization/deliver-message
           after-send
           message-id)

          server
          (realization/execution
           after-delivery
           :server)]

      (is
       (= []
          (realization/messages
           after-delivery)))

      (is
       (machine/waiting-authoritative?
        server))

      (is
       (= 17
          (machine/execution-value
           server
           :request-id)))

      (is
       (= #{:communicated}
          (machine/execution-provenance-kinds
           server
           :request-id))))))

(deftest realization-runs-a-complete-two-role-authoritative-protocol
  (let [{:keys [completed
                command-message-id
                settlement-message-id]}
        (complete-confirmed-claim
         (realization/start
          (claim-choreography)))

        browser
        (realization/execution
         completed
         :browser)

        server
        (realization/execution
         completed
         :server)]

    (is
     (realization/completed?
      completed))

    (is
     (realization/roles-completed?
      completed))

    (is
     (= []
        (realization/messages
         completed)))

    (is
     (machine/completed?
      browser))

    (is
     (machine/completed?
      server))

    (is
     (= :confirmed
        (machine/execution-value
         server
         :outcome)))

    (is
     (= #{:authoritative}
        (machine/execution-provenance-kinds
         server
         :revision)))

    (is
     (= :confirmed
        (machine/execution-value
         browser
         :outcome)))

    (is
     (= 42
        (machine/execution-value
         browser
         :revision)))

    (is
     (= #{:communicated}
        (machine/execution-provenance-kinds
         browser
         :revision)))

    (is
     (= [:local
         :send
         :deliver
         :authoritative
         :send
         :deliver]
        (mapv
         :kind
         (realization/history
          completed))))

    (is
     (= [command-message-id
         command-message-id
         settlement-message-id
         settlement-message-id]
        (->> (realization/history completed)
             (filter #(contains? % :message-id))
             (mapv :message-id))))))

(deftest independent-realization-corresponds-to-global-happy-path-communications
  (let [program
        (claim-choreography)

        global0
        (semantics/start
         program)

        global1
        (semantics/step
         global0
         (semantics/local-event
          :browser
          :prepare-command
          {:request-id 17}))

        global2
        (semantics/step
         global1
         (semantics/communication-event
          :browser
          :server
          :request/claim
          {:request-id 17}
          {:via :http}))

        global3
        (semantics/step
         global2
         (semantics/authoritative-event
          :server
          :request/claim
          {:outcome :confirmed
           :revision 42}))

        global4
        (semantics/step
         global3
         (semantics/branch-event
          :server
          :outcome
          :confirmed))

        global5
        (semantics/step
         global4
         (semantics/communication-event
          :server
          :browser
          :request/settled
          {:outcome :confirmed
           :revision 42}
          {:via :http}))

        {completed :completed}
        (complete-confirmed-claim
         (realization/start
          program))

        realized-messages
        (->> (realization/history completed)
             (filter #(= :send (:kind %)))
             (mapv :message))

        semantic-communications
        (->> (semantics/observable-trace global5)
             (filter #(= :communication (:kind %)))
             (mapv
              #(select-keys
                %
                [:from :to :event :payload :via])))

        realized-communications
        (mapv
         #(select-keys
           %
           [:from :to :event :payload :via])
         realized-messages)]

    (is
     (semantics/completed?
      global5))

    (is
     (realization/completed?
      completed))

    (is
     (= semantic-communications
        realized-communications))

    (is
     (= :done
        (semantics/outcome
         global5)))

    (testing "projected completion remains deliberately weaker than global terminal outcome"
      (doseq [role
              (realization/roles completed)]
        (is
         (= {:outcome :gesso.choreo/complete}
            (machine/result
             (realization/execution
              completed
              role))))))))

(deftest later-global-message-may-be-emitted-before-it-is-causally-deliverable
  (let [program
        (choreo/->choreography
         {:initial :a-to-b
          :states
          {:a-to-b
           (choreo/communicate
            :alice
            :bob
            :example/first
            :c-to-b
            {:required #{:first}})

           :c-to-b
           (choreo/communicate
            :carol
            :bob
            :example/second
            :done
            {:required #{:second}})

           :done
           (choreo/return :done)}})

        started
        (realization/start
         program
         {:entry-values-by-role
          {:alice {:first 1}
           :carol {:second 2}}})

        {after-first-send :realization
         first-id :message-id}
        (realization/complete-send
         started
         :alice
         {:first 1})

        {both-queued :realization
         second-id :message-id}
        (realization/complete-send
         after-first-send
         :carol
         {:second 2})]

    (is
     (= 2
        (realization/message-count
         both-queued)))

    (is
     (= [first-id]
        (realization/deliverable-message-ids
         both-queued)))

    (testing "trying to deliver the later message early fails without consuming it"
      (is
       (= :message-not-enabled
          (error-kind
           #(realization/deliver-message
             both-queued
             second-id))))

      (is
       (= #{first-id second-id}
          (set
           (map :message-id
                (realization/messages
                 both-queued))))))

    (let [after-first-delivery
          (realization/deliver-message
           both-queued
           first-id)]

      (is
       (= [second-id]
          (realization/deliverable-message-ids
           after-first-delivery)))

      (let [completed
            (realization/deliver-message
             after-first-delivery
             second-id)]

        (is
         (realization/completed?
          completed))

        (is
         (= 1
            (machine/execution-value
             (realization/execution
              completed
              :bob)
             :first)))

        (is
         (= 2
            (machine/execution-value
             (realization/execution
              completed
              :bob)
             :second)))))))

(deftest environment-progress-remains-explicit-and-role-local
  (let [program
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:browser/ready :send}
            {:event-contracts
             {:browser/ready
              {:open-data? true}}})

           :send
           (choreo/communicate
            :browser
            :server
            :example/ready
            :done)

           :done
           (choreo/return :done)}})

        started
        (realization/start
         program)

        waiting-state
        (machine/current-state-id
         (realization/execution
          started
          :browser))]

    (is
     (= #{:browser}
        (realization/waiting-environment-roles
         started)))

    (is
     (realization/quiescent?
      started))

    (is
     (false?
      (realization/completed?
       started)))

    (let [after-environment
          (realization/environment
           started
           :browser
           :browser/ready
           {:diagnostic "host-only"})]

      (is
       (= #{:browser}
          (set
           (keys
            (realization/boundaries
             after-environment)))))

      (is
       (= {:kind :environment
           :role :browser
           :state waiting-state
           :event :browser/ready
           :data {}}
          (last
           (realization/history
            after-environment))))

      (testing "open undeclared environment data remains host-only"
        (is
         (false?
          (machine/has-execution-value?
           (realization/execution
            after-environment
            :browser)
           :diagnostic)))))))

(deftest declared-environment-data-survives-independent-realization-and-drives-local-branch
  (let [program
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:browser/observed :decide}
            {:event-contracts
             {:browser/observed
              {:required #{:outcome}
               :optional #{:revision}
               :open-data? true}}})

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

        global0
        (semantics/start
         program)

        global1
        (semantics/step
         global0
         (semantics/environment-event
          :browser
          :browser/observed
          {:outcome :confirmed
           :revision 42
           :host-object :must-not-be-semantic}))

        global2
        (semantics/step
         global1
         (semantics/branch-event
          :browser
          :outcome
          :confirmed))

        started
        (realization/start
         program)

        after-environment
        (realization/environment
         started
         :browser
         :browser/observed
         {:outcome :confirmed
          :revision 42
          :host-object :must-not-be-semantic})

        browser
        (realization/execution
         after-environment
         :browser)

        global-environment
        (last
         (filter
          #(= :environment
              (:kind %))
          (semantics/history
           global2)))

        realized-environment
        (last
         (filter
          #(= :environment
              (:kind %))
          (realization/history
           after-environment)))]

    (testing "declared environment fields become role-local semantic knowledge"
      (is
       (= :confirmed
          (machine/execution-value
           browser
           :outcome)))

      (is
       (= 42
          (machine/execution-value
           browser
           :revision)))

      (is
       (= #{:asserted}
          (machine/execution-provenance-kinds
           browser
           :outcome))))

    (testing "open host-only fields do not enter the portable execution"
      (is
       (false?
        (machine/has-execution-value?
         browser
         :host-object)))

      (is
       (false?
        (contains?
         (:data realized-environment)
         :host-object))))

    (testing "the environment value selects the same semantic branch globally and locally"
      (is
       (= :show-confirmed
          (:action
           (machine/pending-action
            browser))))

      (is
       (= :show-confirmed
          (:action
           (get
            (realization/boundaries
             after-environment)
            :browser))))

      (is
       (= :show-confirmed
          (get-in global2
                  [:program
                   :states
                   (:state global2)
                   :action]))))

    (testing "global semantics and independent realization retain the same semantic event data"
      (is
       (= {:outcome :confirmed
           :revision 42}
          (:data global-environment)))

      (is
       (= (:data global-environment)
          (:data realized-environment))))))

(deftest concrete-entry-values-become-precise-role-local-verifier-assumptions
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
            {:required #{:request-id}})

           :done
           (choreo/return :done)}})

        started
        (realization/start
         program
         {:entry-values-by-role
          {:browser
           {:request-id "request-1"}}})

        verification
        (get-in started
                [:verified
                 :verification])

        browser
        (realization/execution
         started
         :browser)

        server
        (realization/execution
         started
         :server)]

    (is
     (= {:browser #{:request-id}
         :server #{}}
        (get-in verification
                [:analysis
                 :entry-knowledge])))

    (is
     (= "request-1"
        (machine/execution-value
         browser
         :request-id)))

    (is
     (= #{:input}
        (machine/execution-provenance-kinds
         browser
         :request-id)))

    (is
     (false?
      (machine/has-execution-value?
       server
       :request-id)))))

(deftest existing-verification-assumptions-must-be-realized-concretely
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
            {:required #{:request-id}})

           :done
           (choreo/return :done)}})

        verified
        (verify/verify!
         program
         {:entry-knowledge
          {:browser #{:request-id}}})]

    (is
     (= :missing-entry-values
        (error-kind
         #(realization/start
           verified))))

    (is
     (realization/realization?
      (realization/start
       verified
       {:entry-values-by-role
        {:browser
         {:request-id "request-1"}}})))))

(deftest role-machine-options-preserve-explicit-runtime-identities
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

        command-id
        (identity/command-id
         "command-1")

        execution-id
        (identity/execution-id
         "execution-1")

        host
        (identity/host
         "browser-context-1")

        started
        (realization/start
         program
         {:machine-options-by-role
          {:browser
           {:identity-bindings
            {:host host}
            :command-id command-id
            :execution-id execution-id}}})

        execution
        (realization/execution
         started
         :browser)]

    (is
     (= {:role :browser
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
         execution)))

    (is
     (= {}
        (machine/execution-values
         execution)))))

(deftest realization-rejects-unknown-role-configuration
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
           (choreo/return :done)}})]

    (is
     (= :unknown-role
        (error-kind
         #(realization/start
           program
           {:entry-values-by-role
            {:server {:x 1}}}))))

    (is
     (= :unknown-role
        (error-kind
         #(realization/start
           program
           {:machine-options-by-role
            {:server {}}}))))))

(deftest machine-options-cannot-smuggle-a-second-entry-value-store
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
           (choreo/return :done)}})]

    (is
     (= :duplicate-entry-values
        (error-kind
         #(realization/start
           program
           {:machine-options-by-role
            {:browser
             {:values {:x 1}}}}))))))

(deftest failed-boundary-or-delivery-does-not-mutate-the-prior-realization-value
  (let [program
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :alice
            :bob
            :example/value
            :done
            {:required #{:x}})

           :done
           (choreo/return :done)}})

        started
        (realization/start
         program
         {:entry-values-by-role
          {:alice {:x 1}}})]

    (is
     (= :invalid-message-knowledge
        (error-kind
         #(realization/complete-send
           started
           :alice
           {:x 2}))))

    (is
     (= []
        (realization/messages
         started)))

    (is
     (= []
        (realization/history
         started)))

    (let [{queued :realization
           message-id :message-id}
          (realization/complete-send
           started
           :alice
           {:x 1})]

      (is
       (= :unknown-message
          (error-kind
           #(realization/deliver-message
             queued
             (inc message-id)))))

      (is
       (= 1
          (realization/message-count
           queued)))

      (is
       (= [message-id]
          (mapv :message-id
                (realization/messages
                 queued)))))))

(deftest realization-history-is-deterministic-and-wall-clock-free
  (let [run
        (fn []
          (:completed
           (complete-confirmed-claim
            (realization/start
             (claim-choreography)))))

        first-run
        (run)

        second-run
        (run)]

    (is
     (= (realization/history first-run)
        (realization/history second-run)))

    (doseq [entry
            (realization/history first-run)]
      (is
       (false?
        (contains? entry :timestamp)))

      (is
       (false?
        (contains? entry :time)))

      (is
       (false?
        (contains? entry :at))))))

(deftest completion-requires-draining-emitted-transport
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

        started
        (realization/start
         program)

        {queued :realization
         message-id :message-id}
        (realization/complete-send
         started
         :alice
         {})]

    (is
     (realization/role-completed?
      queued
      :alice))

    (is
     (false?
      (realization/role-completed?
       queued
       :bob)))

    (is
     (false?
      (realization/roles-completed?
       queued)))

    (is
     (false?
      (realization/completed?
       queued)))

    (let [completed
          (realization/deliver-message
           queued
           message-id)]

      (is
       (realization/roles-completed?
        completed))

      (is
       (realization/completed?
        completed)))))

(deftest explain-surfaces-distributed-progress-without-collapsing-role-machines
  (let [started
        (realization/start
         (claim-choreography))

        explanation
        (realization/explain
         started)]

    (is
     (= #{:browser :server}
        (:roles explanation)))

    (is
     (false?
      (:completed? explanation)))

    (is
     (false?
      (:roles-completed? explanation)))

    (is
     (false?
      (:quiescent? explanation)))

    (is
     (= #{:browser}
        (set
         (keys
          (:boundaries explanation)))))

    (is
     (= []
        (:queued-message-ids explanation)))

    (is
     (= []
        (:deliverable-message-ids explanation)))

    (is
     (= 0
        (:history-count explanation)))

    (is
     (= #{:waiting-local
          :waiting-receive}
        (set
         (vals
          (:role-status explanation)))))))

;; -----------------------------------------------------------------------------
;; Explicit transport fault semantics
;; -----------------------------------------------------------------------------

(defn- one-message-program
  []
  (choreo/->choreography
   {:name :example/one-message
    :initial :send
    :states
    {:send
     (choreo/communicate
      :alice
      :bob
      :example/value
      :done
      {:required #{:x}})

     :done
     (choreo/return :done)}}))

(defn- queued-one-message
  []
  (let [started
        (realization/start
         (one-message-program)
         {:entry-values-by-role
          {:alice {:x 1}}})

        {queued :realization
         message-id :message-id
         message :message}
        (realization/complete-send
         started
         :alice
         {:x 1})]
    {:realization queued
     :message-id message-id
     :message message}))

(deftest dropped-message-does-not-rewind-sender-or-teach-receiver
  (let [{queued :realization
         message-id :message-id
         message :message}
        (queued-one-message)

        dropped
        (realization/drop-message
         queued
         message-id)

        alice
        (realization/execution
         dropped
         :alice)

        bob
        (realization/execution
         dropped
         :bob)]

    (is
     (machine/completed?
      alice))

    (is
     (machine/waiting-receive?
      bob))

    (is
     (false?
      (machine/has-execution-value?
       bob
       :x)))

    (is
     (= []
        (realization/messages
         dropped)))

    (is
     (= [:send :drop]
        (mapv
         :kind
         (realization/history
          dropped))))

    (is
     (= {:kind :drop
         :message-id message-id
         :message message}
        (last
         (realization/history
          dropped))))

    (is
     (false?
      (realization/completed?
       dropped)))))

(deftest duplicate-message-copies-the-exact-emitted-envelope-with-a-fresh-id
  (let [{queued :realization
         original-id :message-id
         message :message}
        (queued-one-message)

        {duplicated :realization
         duplicate-id :message-id
         duplicate-message :message}
        (realization/duplicate-message
         queued
         original-id)

        original-entry
        (realization/queued-message
         duplicated
         original-id)

        duplicate-entry
        (realization/queued-message
         duplicated
         duplicate-id)]

    (is
     (not=
      original-id
      duplicate-id))

    (is
     (= message
        duplicate-message))

    (is
     (= message
        (:message original-entry)))

    (is
     (= message
        (:message duplicate-entry)))

    (is
     (false?
      (contains?
       original-entry
       :origin-message-id)))

    (is
     (= original-id
        (:origin-message-id
         duplicate-entry)))

    (is
     (= [original-id duplicate-id]
        (mapv
         :message-id
         (realization/messages
          duplicated))))

    (is
     (= {:kind :duplicate
         :source-message-id original-id
         :message-id duplicate-id
         :origin-message-id original-id
         :message message}
        (last
         (realization/history
          duplicated))))))

(deftest duplicate-of-duplicate-preserves-the-root-emitted-message-lineage
  (let [{queued :realization
         original-id :message-id
         message :message}
        (queued-one-message)

        {once :realization
         first-copy-id :message-id}
        (realization/duplicate-message
         queued
         original-id)

        {twice :realization
         second-copy-id :message-id
         second-copy-message :message}
        (realization/duplicate-message
         once
         first-copy-id)

        first-copy
        (realization/queued-message
         twice
         first-copy-id)

        second-copy
        (realization/queued-message
         twice
         second-copy-id)]

    (is
     (= original-id
        (:origin-message-id
         first-copy)))

    (is
     (= original-id
        (:origin-message-id
         second-copy)))

    (is
     (= message
        second-copy-message))

    (is
     (= {:kind :duplicate
         :source-message-id first-copy-id
         :message-id second-copy-id
         :origin-message-id original-id
         :message message}
        (last
         (realization/history
          twice))))))

(deftest surviving-duplicate-can-be-delivered-after-the-original-is-dropped
  (let [{queued :realization
         original-id :message-id}
        (queued-one-message)

        {duplicated :realization
         duplicate-id :message-id
         duplicate-message :message}
        (realization/duplicate-message
         queued
         original-id)

        after-drop
        (realization/drop-message
         duplicated
         original-id)

        completed
        (realization/deliver-message
         after-drop
         duplicate-id)

        bob
        (realization/execution
         completed
         :bob)

        delivery
        (last
         (realization/history
          completed))]

    (is
     (= [duplicate-id]
        (realization/deliverable-message-ids
         after-drop)))

    (is
     (= []
        (realization/messages
         completed)))

    (is
     (machine/completed?
      bob))

    (is
     (= 1
        (machine/execution-value
         bob
         :x)))

    (is
     (= #{:communicated}
        (machine/execution-provenance-kinds
         bob
         :x)))

    (is
     (= :deliver
        (:kind delivery)))

    (is
     (= duplicate-id
        (:message-id delivery)))

    (is
     (= original-id
        (:origin-message-id delivery)))

    (is
     (= duplicate-message
        (:message delivery)))

    (is
     (realization/completed?
      completed))))

(deftest duplicate-that-arrives-after-receiver-completion-is-stale-not-a-second-semantic-receive
  (let [{queued :realization
         original-id :message-id}
        (queued-one-message)

        {duplicated :realization
         duplicate-id :message-id}
        (realization/duplicate-message
         queued
         original-id)

        after-original
        (realization/deliver-message
         duplicated
         original-id)]

    (is
     (machine/completed?
      (realization/execution
       after-original
       :bob)))

    (is
     (= []
        (realization/deliverable-message-ids
         after-original)))

    (is
     (= [duplicate-id]
        (mapv
         :message-id
         (realization/messages
          after-original))))

    (is
     (= :not-waiting-receive
        (error-kind
         #(realization/deliver-message
           after-original
           duplicate-id))))

    (testing "failed stale delivery leaves the queued duplicate intact for diagnosis or explicit drop"
      (is
       (= [duplicate-id]
          (mapv
           :message-id
           (realization/messages
            after-original))))

      (let [drained
            (realization/drop-message
             after-original
             duplicate-id)]
        (is
         (realization/completed?
          drained))))))

(deftest fault-operations-reject-unknown-message-ids-without-changing-the-prior-value
  (let [{queued :realization
         message-id :message-id}
        (queued-one-message)

        missing-id
        (inc message-id)]

    (is
     (= :unknown-message
        (error-kind
         #(realization/drop-message
           queued
           missing-id))))

    (is
     (= :unknown-message
        (error-kind
         #(realization/duplicate-message
           queued
           missing-id))))

    (is
     (= [message-id]
        (mapv
         :message-id
         (realization/messages
          queued))))

    (is
     (= [:send]
        (mapv
         :kind
         (realization/history
          queued))))))
