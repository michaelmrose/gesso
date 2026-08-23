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

;; -----------------------------------------------------------------------------
;; Authoritative observation / reread through independent role realization
;; -----------------------------------------------------------------------------

(def authoritative-reread-contract
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(defn- authoritative-reread-choreography
  []
  (choreo/->choreography
   {:name :example/authoritative-reread
    :initial :claim
    :states
    {:claim
     (choreo/authoritative
      :server
      :request/claim
      :observe)

     :observe
     (choreo/await
      :browser
      {:request/reread-complete :install}
      {:event-contracts
       {:request/reread-complete
        {:required #{:request-status :observed-basis}
         :open-data? true
         :authoritative-observation
         authoritative-reread-contract}}})

     :install
     (choreo/local
      :browser
      :install-result
      :done
      {:requires #{:request-status :observed-basis}})

     :done
     (choreo/return :done)}}))

(deftest authoritative-reread-realizes-without-participant-message
  (let [started
        (realization/start
         (authoritative-reread-choreography))

        browser-before
        (realization/execution started :browser)

        server-before
        (realization/execution started :server)

        browser-await-state
        (machine/current-state-id browser-before)

        after-authority
        (realization/complete-authoritative
         started
         :server
         {})

        basis
        {:revision 42
         :tx-id "tx-42"}

        after-reread
        (realization/environment
         after-authority
         :browser
         :request/reread-complete
         {:request-status :approved
          :observed-basis basis
          :host-only "not-semantic"})

        browser-after
        (realization/execution after-reread :browser)

        reread-history
        (last
         (realization/history after-reread))

        completed
        (realization/complete-local
         after-reread
         :browser
         {})]

    (testing "roles begin as independent projected machines"
      (is (machine/waiting-environment? browser-before))
      (is (machine/waiting-authoritative? server-before))
      (is (= [] (realization/messages started))))

    (testing "foreign authoritative completion does not manufacture a participant message"
      (is
       (machine/completed?
        (realization/execution after-authority :server)))
      (is
       (machine/waiting-environment?
        (realization/execution after-authority :browser)))
      (is (= [] (realization/messages after-authority))))

    (testing "the declared authoritative reread establishes authoritative browser knowledge"
      (is (machine/waiting-local? browser-after))
      (is (= :approved
             (machine/execution-value
              browser-after
              :request-status)))
      (is (= basis
             (machine/execution-value
              browser-after
              :observed-basis)))
      (is (= #{:authoritative}
             (machine/execution-provenance-kinds
              browser-after
              :request-status)))
      (is (= [{:kind :authoritative
               :authority :request/model
               :observation :request/current-projection
               :basis basis
               :state browser-await-state
               :metadata {:origin :environment
                          :event :request/reread-complete}}]
             (machine/execution-provenance
              browser-after
              :request-status))))

    (testing "open adapter data remains outside portable knowledge and deterministic history"
      (is
       (false?
        (machine/has-execution-value?
         browser-after
         :host-only)))
      (is
       (= {:kind :environment
           :role :browser
           :state browser-await-state
           :event :request/reread-complete
           :data {:request-status :approved
                  :observed-basis basis}}
          reread-history)))

    (testing "the reread is a synchronization path, not a synthetic message"
      (is (= [] (realization/messages after-reread)))
      (is (realization/completed? completed))
      (is (= [] (realization/messages completed))))))


;; -----------------------------------------------------------------------------
;; Authoritative basis progression through independent realization
;; -----------------------------------------------------------------------------

(def repeated-authoritative-reread-contract
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(defn- repeated-authoritative-reread-choreography
  []
  (choreo/->choreography
   {:name :example/repeated-authoritative-reread
    :initial :observe-first
    :states
    {:observe-first
     (choreo/await
      :browser
      {:request/reread-complete :observe-second}
      {:event-contracts
       {:request/reread-complete
        {:required #{:request-status :observed-basis}
         :open-data? true
         :authoritative-observation
         repeated-authoritative-reread-contract}}})

     :observe-second
     (choreo/await
      :browser
      {:request/reread-complete :done}
      {:event-contracts
       {:request/reread-complete
        {:required #{:request-status :observed-basis}
         :open-data? true
         :authoritative-observation
         repeated-authoritative-reread-contract}}})

     :done
     (choreo/return :done)}}))

(defn- basis-progression
  [from-basis to-basis relation]
  {:kind :authoritative-basis-progression
   :authority :request/model
   :observation :request/current-projection
   :from-basis from-basis
   :to-basis to-basis
   :relation relation})

(defn- invoke-environment-with-options
  "Invoke the intended five-argument realization/environment contract.

   During the test-first red phase, the current realization namespace has only
   four arguments. Convert only that expected missing-arity condition into an
   explicit result so the new behavioral tests fail as assertions instead of
   aborting with harness errors. Any other unexpected throwable is rethrown."
  [realization-value role event data options]
  (let [f realization/environment]
    #?(:clj
       (try
         {:status :ok
          :value
          (apply f
                 [realization-value
                  role
                  event
                  data
                  options])}
         (catch Throwable e
           (let [message (or (.getMessage e) "")]
             (cond
               (re-find #"(?i)(cannot call environment with 5 arguments|wrong number of args.*5)"
                        message)
               {:status :unsupported-arity}

               (instance? clojure.lang.ExceptionInfo e)
               {:status :exception
                :error-kind (:error/kind (ex-data e))
                :exception e}

               :else
               (throw e)))))

       :cljs
       (try
         {:status :ok
          :value
          (apply f
                 [realization-value
                  role
                  event
                  data
                  options])}
         (catch :default e
           (let [message (or (.-message e) "")]
             (cond
               (re-find #"(?i)(invalid arity|wrong number of args)" message)
               {:status :unsupported-arity}

               (ex-data e)
               {:status :exception
                :error-kind (:error/kind (ex-data e))
                :exception e}

               :else
               (throw e))))))))

(deftest repeated-authoritative-reread-requires-explicit-basis-progression
  (let [basis-1
        {:revision 41
         :tx-id "tx-41"}

        basis-2
        {:revision 42
         :tx-id "tx-42"}

        started
        (realization/start
         (repeated-authoritative-reread-choreography))

        after-first
        (realization/environment
         started
         :browser
         :request/reread-complete
         {:request-status :pending
          :observed-basis basis-1})

        browser-after-first
        (realization/execution after-first :browser)]

    (is (machine/waiting-environment? browser-after-first))
    (is (= :pending
           (machine/execution-value
            browser-after-first
            :request-status)))
    (is (= basis-1
           (machine/execution-value
            browser-after-first
            :observed-basis)))

    (testing "arrival order alone cannot advance the authoritative frontier"
      (is
       (= :authoritative-progression-required
          (error-kind
           #(realization/environment
             after-first
             :browser
             :request/reread-complete
             {:request-status :approved
              :observed-basis basis-2}))))

      (is (= :pending
             (machine/execution-value
              (realization/execution after-first :browser)
              :request-status)))
      (is (= 1
             (count
              (realization/history after-first)))))))

(deftest advancing-basis-witness-flows-through-realization-without-becoming-semantic-data
  (let [basis-1
        {:revision 41
         :tx-id "tx-41"}

        basis-2
        {:revision 42
         :tx-id "tx-42"}

        witness
        (basis-progression basis-1 basis-2 :advances)

        started
        (realization/start
         (repeated-authoritative-reread-choreography))

        after-first
        (realization/environment
         started
         :browser
         :request/reread-complete
         {:request-status :pending
          :observed-basis basis-1
          :host-only :first})

        attempt
        (invoke-environment-with-options
         after-first
         :browser
         :request/reread-complete
         {:request-status :approved
          :observed-basis basis-2
          :host-only :second}
         {:authoritative-basis-progression witness})]

    (is (= :ok (:status attempt)))

    (when-let [completed (:value attempt)]
      (let [browser
            (realization/execution completed :browser)

            history
            (realization/history completed)

            second-environment
            (last history)]

        (is (realization/completed? completed))
        (is (= [] (realization/messages completed)))
        (is (= :approved
               (machine/execution-value browser :request-status)))
        (is (= basis-2
               (machine/execution-value browser :observed-basis)))
        (is (= #{:authoritative}
               (machine/execution-provenance-kinds
                browser
                :request-status)))

        (testing "progression evidence is control evidence, not role-local semantic knowledge"
          (is
           (false?
            (machine/has-execution-value?
             browser
             :authoritative-basis-progression)))
          (is
           (false?
            (contains?
             (:data second-environment)
             :authoritative-basis-progression)))
          (is
           (false?
            (contains?
             (:data second-environment)
             :host-only))))

        (is (= {:kind :environment
                :role :browser
                :state (:state second-environment)
                :event :request/reread-complete
                :data {:request-status :approved
                       :observed-basis basis-2}}
               second-environment))))))

(deftest nonadvancing-or-mismatched-progression-witness-does-not-mutate-realization
  (let [basis-1
        {:revision 41}

        basis-2
        {:revision 42}

        started
        (realization/start
         (repeated-authoritative-reread-choreography))

        after-first
        (realization/environment
         started
         :browser
         :request/reread-complete
         {:request-status :pending
          :observed-basis basis-1})

        candidates
        [{:label :precedes
          :witness
          (basis-progression basis-1 basis-2 :precedes)}

         {:label :incomparable
          :witness
          (basis-progression basis-1 basis-2 :incomparable)}

         {:label :wrong-authority
          :witness
          (assoc
           (basis-progression basis-1 basis-2 :advances)
           :authority
           :other/model)}

         {:label :wrong-observation
          :witness
          (assoc
           (basis-progression basis-1 basis-2 :advances)
           :observation
           :request/other-projection)}

         {:label :wrong-from-basis
          :witness
          (assoc
           (basis-progression basis-1 basis-2 :advances)
           :from-basis
           {:revision 40})}

         {:label :wrong-to-basis
          :witness
          (assoc
           (basis-progression basis-1 basis-2 :advances)
           :to-basis
           {:revision 43})}]]

    (doseq [{:keys [label witness]} candidates]
      (testing (name label)
        (let [attempt
              (invoke-environment-with-options
               after-first
               :browser
               :request/reread-complete
               {:request-status :approved
                :observed-basis basis-2}
               {:authoritative-basis-progression witness})]

          (is (= :exception (:status attempt)))
          (when (= :exception (:status attempt))
            (is
             (contains?
              #{:authoritative-basis-not-advancing
                :authoritative-progression-mismatch}
              (:error-kind attempt)))))))

    (testing "all failed attempts leave the prior immutable realization untouched"
      (is (= :pending
             (machine/execution-value
              (realization/execution after-first :browser)
              :request-status)))
      (is (= basis-1
             (machine/execution-value
              (realization/execution after-first :browser)
              :observed-basis)))
      (is (= 1
             (count
              (realization/history after-first)))))))

(deftest same-value-at-new-basis-still-needs-progression-through-realization
  (let [basis-1
        {:revision 41}

        basis-2
        {:revision 42}

        started
        (realization/start
         (repeated-authoritative-reread-choreography))

        after-first
        (realization/environment
         started
         :browser
         :request/reread-complete
         {:request-status :approved
          :observed-basis basis-1})]

    (is
     (= :authoritative-progression-required
        (error-kind
         #(realization/environment
           after-first
           :browser
           :request/reread-complete
           {:request-status :approved
            :observed-basis basis-2}))))

    (let [attempt
          (invoke-environment-with-options
           after-first
           :browser
           :request/reread-complete
           {:request-status :approved
            :observed-basis basis-2}
           {:authoritative-basis-progression
            (basis-progression
             basis-1
             basis-2
             :advances)})]

      (is (= :ok (:status attempt)))
      (when-let [completed (:value attempt)]
        (is (= basis-2
               (machine/execution-value
                (realization/execution completed :browser)
                :observed-basis)))
        (is (realization/completed? completed))))))

(deftest realization-environment-progression-options-are-closed
  (let [basis-1 {:revision 41}
        basis-2 {:revision 42}
        witness (basis-progression basis-1 basis-2 :advances)
        started
        (realization/start
         (repeated-authoritative-reread-choreography))
        after-first
        (realization/environment
         started
         :browser
         :request/reread-complete
         {:request-status :pending
          :observed-basis basis-1})]

    (doseq [options
            [[:not-a-map]
             {:authoritative-basis-progression witness
              :adapter/extra true}]]
      (let [attempt
            (invoke-environment-with-options
             after-first
             :browser
             :request/reread-complete
             {:request-status :approved
              :observed-basis basis-2}
             options)]
        (is (= :exception (:status attempt)))
        (when (= :exception (:status attempt))
          (is (= :invalid-environment-options
                 (:error-kind attempt))))))

    (testing "invalid option shapes do not mutate the prior realization"
      (is (= :pending
             (machine/execution-value
              (realization/execution after-first :browser)
              :request-status)))
      (is (= 1
             (count
              (realization/history after-first)))))))
