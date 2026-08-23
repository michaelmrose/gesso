(ns gesso.choreo.reference-chat-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.realization :as realization]
   [gesso.choreo.verify :as verify]))

(def transcript-observation
  {:authority :chat/model
   :observation :chat/current-transcript
   :basis-key :transcript-basis})

(def transcript-basis
  {:tx-id "tx-chat-12"
   :valid-time "2026-08-22T23:50:00-07:00"})

(defn- chat-choreography
  []
  (choreo/->choreography
   {:name :reference/chat
    :initial :compose
    :states
    {:compose
     (choreo/local
      :sender
      :chat/compose
      :submit
      {:outputs #{:message-id :message-body}})

     :submit
     (choreo/communicate
      :sender
      :server
      :chat/send
      :store
      {:via :http
       :required #{:message-id :message-body}
       :optional #{:client-note}})

     :store
     (choreo/authoritative
      :server
      :chat/store
      :observe-transcript
      {:requires #{:message-id :message-body}
       :outputs #{:stored-message-id
                  :stored-message-body
                  :message-status}})

     :observe-transcript
     (choreo/await
      :recipient
      {:chat/transcript-refreshed :observe-typing}
      {:event-contracts
       {:chat/transcript-refreshed
        {:required #{:stored-message-id
                     :stored-message-body
                     :message-status
                     :transcript-basis}
         :optional #{:display-time}
         :open-data? true
         :authoritative-observation transcript-observation}}})

     :observe-typing
     (choreo/await
      :recipient
      {:chat/typing-changed :render}
      {:event-contracts
       {:chat/typing-changed
        {:required #{:typing-user :typing?}
         :optional #{:typing-since}
         :open-data? true}}})

     :render
     (choreo/local
      :recipient
      :chat/render
      :done
      {:requires #{:stored-message-id
                   :stored-message-body
                   :message-status
                   :transcript-basis
                   :typing-user
                   :typing?}})

     :done
     (choreo/return :done)}}))

(defn- chat-witness
  []
  [{:op :local
    :role :sender
    :action :chat/compose
    :outputs {:message-id "message-12"
              :message-body "hello"}}

   {:op :send
    :role :sender
    :to :server
    :event :chat/send
    :via :http
    :payload {:message-id "message-12"
              :message-body "hello"}
    :as :chat-command}

   {:op :deliver
    :message :chat-command}

   {:op :authoritative
    :role :server
    :operation :chat/store
    :outputs {:stored-message-id "message-12"
              :stored-message-body "hello"
              :message-status :stored}}

   {:op :environment
    :role :recipient
    :event :chat/transcript-refreshed
    :data {:stored-message-id "message-12"
           :stored-message-body "hello"
           :message-status :stored
           :transcript-basis transcript-basis}}

   {:op :environment
    :role :recipient
    :event :chat/typing-changed
    :data {:typing-user "sender-7"
           :typing? true}}

   {:op :local
    :role :recipient
    :action :chat/render
    :outputs {}}])

(deftest chat-reference-keeps-durable-and-ephemeral-knowledge-distinct
  (let [result
        (verify/verify
         (chat-choreography))

        analysis
        (:analysis result)

        before-render
        (get-in analysis
                [:definitely-known-before-state
                 :render
                 :recipient])

        authoritative-before-render
        (get-in analysis
                [:definitely-authoritatively-known-before-state
                 :render
                 :recipient])]

    (is (:valid? result))

    (testing "the durable transcript reread is the only authoritative observation"
      (is (= transcript-observation
             (get-in analysis
                     [:authoritative-observations-by-state
                      :observe-transcript
                      :chat/transcript-refreshed])))
      (is (nil?
           (get-in analysis
                   [:authoritative-observations-by-state
                    :observe-typing
                    :chat/typing-changed]))))

    (testing "both kinds of data are usable by the recipient"
      (is (every? before-render
                  #{:stored-message-id
                    :stored-message-body
                    :message-status
                    :transcript-basis
                    :typing-user
                    :typing?})))

    (testing "only the durable transcript facts are definitely authoritative"
      (is (every? authoritative-before-render
                  #{:stored-message-id
                    :stored-message-body
                    :message-status
                    :transcript-basis}))
      (is (not (contains? authoritative-before-render :typing-user)))
      (is (not (contains? authoritative-before-render :typing?))))))

(deftest recipient-projection-needs-no-participant-message-for-transcript-or-typing
  (let [plan
        (project/project
         (chat-choreography)
         :recipient)

        states
        (vals (:states plan))

        ops
        (mapv :op states)

        awaits
        (filterv #(= :await (:op %)) states)]

    (is (= 2 (count awaits)))
    (is (some #{:local} ops))
    (is (some #{:return} ops))

    (testing "recipient learns the durable fact by authoritative observation and typing locally"
      (is (not-any? #{:send :receive} ops)))

    (testing "only one projected await contract carries authority metadata"
      (is (= 1
             (count
              (for [state awaits
                    [_ contract] (:event-contracts state)
                    :when (:authoritative-observation contract)]
                contract)))))))

(deftest chat-realization-keeps-ephemeral-typing-nonauthoritative
  (let [started
        (realization/start
         (chat-choreography))

        after-compose
        (realization/complete-local
         started
         :sender
         {:message-id "message-12"
          :message-body "hello"})

        {after-send :realization
         command-id :message-id}
        (realization/complete-send
         after-compose
         :sender
         {:message-id "message-12"
          :message-body "hello"})

        after-delivery
        (realization/deliver-message
         after-send
         command-id)

        after-store
        (realization/complete-authoritative
         after-delivery
         :server
         {:stored-message-id "message-12"
          :stored-message-body "hello"
          :message-status :stored})

        after-transcript
        (realization/environment
         after-store
         :recipient
         :chat/transcript-refreshed
         {:stored-message-id "message-12"
          :stored-message-body "hello"
          :message-status :stored
          :transcript-basis transcript-basis
          :display-time "11:50 PM"
          :xhr-object :host-only})

        after-typing
        (realization/environment
         after-transcript
         :recipient
         :chat/typing-changed
         {:typing-user "sender-7"
          :typing? true
          :typing-since 12345
          :browser-event :host-only})

        recipient
        (realization/execution
         after-typing
         :recipient)

        completed
        (realization/complete-local
         after-typing
         :recipient
         {})]

    (testing "the durable transcript facts carry authoritative provenance"
      (doseq [key [:stored-message-id
                   :stored-message-body
                   :message-status
                   :transcript-basis]]
        (is (= #{:authoritative}
               (machine/execution-provenance-kinds
                recipient
                key)))))

    (testing "typing is useful semantic state without pretending to be durable authority"
      (is (= true
             (machine/execution-value recipient :typing?)))
      (is (= "sender-7"
             (machine/execution-value recipient :typing-user)))
      (is (= #{:asserted}
             (machine/execution-provenance-kinds recipient :typing?)))
      (is (= #{:asserted}
             (machine/execution-provenance-kinds recipient :typing-user))))

    (testing "declared optional data may enter semantic state; open host extras do not"
      (is (= "11:50 PM"
             (machine/execution-value recipient :display-time)))
      (is (= 12345
             (machine/execution-value recipient :typing-since)))
      (is (false?
           (machine/has-execution-value? recipient :xhr-object)))
      (is (false?
           (machine/has-execution-value? recipient :browser-event))))

    (testing "there is still only the sender-to-server participant message"
      (is (= []
             (realization/messages after-delivery)))
      (is (= []
             (realization/messages after-transcript)))
      (is (= []
             (realization/messages after-typing))))

    (is (realization/completed? completed))))

(deftest chat-proof-assigns-progression-trust-only-to-durable-observation
  (let [result
        (proof/check-projection-boundaries
         (chat-choreography))

        runtime-obligations
        (:runtime-obligations result)

        trusted-assumptions
        (:trusted-assumptions result)]

    (is (proof/valid? result))
    (is (= 1 (count runtime-obligations)))
    (is (= 1 (count trusted-assumptions)))

    (is (= [:authoritative-basis-progression
            :observe-transcript
            :chat/transcript-refreshed]
           (:id (first runtime-obligations))))

    (is (= [:authoritative-basis-ordering
            :observe-transcript
            :chat/transcript-refreshed]
           (:id (first trusted-assumptions))))

    (is (not-any?
         #(= :observe-typing (:state %))
         (concat runtime-obligations
                 trusted-assumptions)))))

(deftest chat-correspondence-keeps-local-observations-out-of-distributed-trace
  (let [result
        (correspondence/check-witness
         (chat-choreography)
         (chat-witness)
         {:require-complete? true})

        observation-obligations
        (filterv
         #(= :authoritative-observation-correspondence
             (:kind %))
         (:obligations result))]

    (is (correspondence/valid? result))
    (is (true? (:global-completed? result)))
    (is (true? (:realization-completed? result)))

    (testing "only participant communication and durable mutation are distributed observables"
      (is (= [{:kind :communication
               :from :sender
               :to :server
               :event :chat/send
               :payload {:message-id "message-12"
                         :message-body "hello"}
               :via :http}
              {:kind :authoritative
               :role :server
               :operation :chat/store
               :outputs {:stored-message-id "message-12"
                         :stored-message-body "hello"
                         :message-status :stored}}]
             (:semantic-trace result)))
      (is (= (:semantic-trace result)
             (:realization-trace result))))

    (testing "the durable reread is checked as authority correspondence; typing is not"
      (is (= 1 (count observation-obligations)))
      (is (= transcript-basis
             (:basis (first observation-obligations))))
      (is (= #{:stored-message-id
               :stored-message-body
               :message-status
               :transcript-basis}
             (:checked-keys
              (first observation-obligations)))))))
