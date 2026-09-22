(ns gesso.choreo.reference-checkout-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.realization :as realization]
   [gesso.choreo.verify :as verify]))

(defn- checkout-choreography
  []
  (choreo/->choreography
   {:name :reference/checkout
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :buyer
      :checkout/prepare
      :submit
      {:outputs #{:checkout-id :amount :currency}})

     :submit
     (choreo/communicate
      :buyer
      :server
      :checkout/submit
      :reserve
      {:via :http
       :required #{:checkout-id :amount :currency}})

     :reserve
     (choreo/authoritative
      :server
      :checkout/reserve
      :request-payment
      {:requires #{:checkout-id :amount :currency}
       :outputs #{:payment-request-id :reservation-version}})

     :request-payment
     (choreo/communicate
      :server
      :payment-provider
      :payment/authorize
      :authorize
      {:via :https
       :required #{:checkout-id
                   :payment-request-id
                   :amount
                   :currency}})

     :authorize
     (choreo/authoritative
      :payment-provider
      :payment-provider/authorize
      :payment-result
      {:requires #{:checkout-id
                   :payment-request-id
                   :amount
                   :currency}
       :outputs #{:payment-outcome
                  :provider-reference}})

     :payment-result
     (choreo/communicate
      :payment-provider
      :server
      :payment/result
      :record-payment
      {:via :https
       :required #{:checkout-id
                   :payment-request-id
                   :payment-outcome
                   :provider-reference}})

     :record-payment
     (choreo/authoritative
      :server
      :checkout/record-payment
      :settle
      {:requires #{:checkout-id
                   :payment-request-id
                   :payment-outcome
                   :provider-reference}
       :outputs #{:checkout-status
                  :checkout-version}})

     :settle
     (choreo/communicate
      :server
      :buyer
      :checkout/settled
      :render
      {:via :http
       :required #{:checkout-id
                   :checkout-status
                   :checkout-version}})

     :render
     (choreo/local
      :buyer
      :checkout/render
      :done
      {:requires #{:checkout-status :checkout-version}})

     :done
     (choreo/return :done)}}))

(defn- checkout-witness
  []
  [{:op :local
    :role :buyer
    :action :checkout/prepare
    :outputs {:checkout-id "checkout-17"
              :amount 2500
              :currency :usd}}

   {:op :send
    :role :buyer
    :to :server
    :event :checkout/submit
    :via :http
    :payload {:checkout-id "checkout-17"
              :amount 2500
              :currency :usd}
    :as :checkout-command}

   {:op :deliver
    :message :checkout-command}

   {:op :authoritative
    :role :server
    :operation :checkout/reserve
    :outputs {:payment-request-id "payment-request-17"
              :reservation-version 1}}

   {:op :send
    :role :server
    :to :payment-provider
    :event :payment/authorize
    :via :https
    :payload {:checkout-id "checkout-17"
              :payment-request-id "payment-request-17"
              :amount 2500
              :currency :usd}
    :as :payment-command}

   {:op :deliver
    :message :payment-command}

   {:op :authoritative
    :role :payment-provider
    :operation :payment-provider/authorize
    :outputs {:payment-outcome :approved
              :provider-reference "provider-42"}}

   {:op :send
    :role :payment-provider
    :to :server
    :event :payment/result
    :via :https
    :payload {:checkout-id "checkout-17"
              :payment-request-id "payment-request-17"
              :payment-outcome :approved
              :provider-reference "provider-42"}
    :as :payment-result}

   {:op :deliver
    :message :payment-result}

   {:op :authoritative
    :role :server
    :operation :checkout/record-payment
    :outputs {:checkout-status :paid
              :checkout-version 2}}

   {:op :send
    :role :server
    :to :buyer
    :event :checkout/settled
    :via :http
    :payload {:checkout-id "checkout-17"
              :checkout-status :paid
              :checkout-version 2}
    :as :checkout-settlement}

   {:op :deliver
    :message :checkout-settlement}

   {:op :local
    :role :buyer
    :action :checkout/render
    :outputs {}}])

(deftest checkout-reference-keeps-external-authority-explicit
  (let [result
        (verify/verify
         (checkout-choreography))

        analysis
        (:analysis result)]

    (is (:valid? result))

    (testing "internal persistence and external payment authority are separate semantic operations"
      (is (= {:reserve :checkout/reserve
              :authorize :payment-provider/authorize
              :record-payment :checkout/record-payment}
             (:authoritative-operations-by-state analysis))))

    (testing "the payment provider is an explicit participant rather than hidden inside server atomicity"
      (is (= #{:buyer :server :payment-provider}
             (:roles analysis))))))

(deftest payment-provider-projection-is-an-independent-authority-boundary
  (let [plan
        (project/project
         (checkout-choreography)
         :payment-provider)

        states
        (vals (:states plan))

        ops
        (mapv :op states)]

    (is (some #{:receive} ops))
    (is (some #{:authoritative} ops))
    (is (some #{:send} ops))
    (is (some #{:return} ops))

    (testing "the provider cannot be projected away as one server-local action"
      (is (= 1
             (count
              (filter
               #(= :authoritative (:op %))
               states))))

      (is (= :payment-provider/authorize
             (:operation
              (first
               (filter
                #(= :authoritative (:op %))
                states))))))))

(deftest checkout-realization-does-not-turn-provider-result-into-server-authority
  (let [started
        (realization/start
         (checkout-choreography))

        after-prepare
        (realization/complete-local
         started
         :buyer
         {:checkout-id "checkout-17"
          :amount 2500
          :currency :usd})

        {after-submit :realization
         checkout-command-id :message-id}
        (realization/complete-send
         after-prepare
         :buyer
         {:checkout-id "checkout-17"
          :amount 2500
          :currency :usd})

        after-command
        (realization/deliver-message
         after-submit
         checkout-command-id)

        after-reserve
        (realization/complete-authoritative
         after-command
         :server
         {:payment-request-id "payment-request-17"
          :reservation-version 1})

        {after-payment-send :realization
         payment-command-id :message-id}
        (realization/complete-send
         after-reserve
         :server
         {:checkout-id "checkout-17"
          :payment-request-id "payment-request-17"
          :amount 2500
          :currency :usd})

        after-payment-command
        (realization/deliver-message
         after-payment-send
         payment-command-id)

        after-provider
        (realization/complete-authoritative
         after-payment-command
         :payment-provider
         {:payment-outcome :approved
          :provider-reference "provider-42"})

        {after-result-send :realization
         payment-result-id :message-id}
        (realization/complete-send
         after-provider
         :payment-provider
         {:checkout-id "checkout-17"
          :payment-request-id "payment-request-17"
          :payment-outcome :approved
          :provider-reference "provider-42"})

        after-result
        (realization/deliver-message
         after-result-send
         payment-result-id)

        server-before-record
        (realization/execution
         after-result
         :server)

        after-record
        (realization/complete-authoritative
         after-result
         :server
         {:checkout-status :paid
          :checkout-version 2})

        server-after-record
        (realization/execution
         after-record
         :server)]

    (testing "provider decision reaches the server as communicated knowledge"
      (is (= :approved
             (machine/execution-value
              server-before-record
              :payment-outcome)))

      (is (= #{:communicated}
             (machine/execution-provenance-kinds
              server-before-record
              :payment-outcome)))

      (is (= #{:communicated}
             (machine/execution-provenance-kinds
              server-before-record
              :provider-reference))))

    (testing "the server establishes its own durable checkout result only at the later model operation"
      (is (= :paid
             (machine/execution-value
              server-after-record
              :checkout-status)))

      (is (= #{:authoritative}
             (machine/execution-provenance-kinds
              server-after-record
              :checkout-status))))))

(deftest checkout-correspondence-retains-the-external-round-trip
  (let [result
        (correspondence/check-witness
         (checkout-choreography)
         (checkout-witness)
         {:require-complete? true})

        trace
        (:semantic-trace result)]

    (is (correspondence/valid? result))
    (is (true? (:global-completed? result)))
    (is (true? (:realization-completed? result)))
    (is (= trace
           (:realization-trace result)))

    (testing "the observable trace cannot collapse checkout and payment into one atomic server step"
      (is (= [:communication
              :authoritative
              :communication
              :authoritative
              :communication
              :authoritative
              :communication]
             (mapv :kind trace)))

      (is (= [:checkout/reserve
              :payment-provider/authorize
              :checkout/record-payment]
             (mapv
              :operation
              (filter
               #(= :authoritative (:kind %))
               trace)))))))

(deftest checkout-proof-does-not-invent-cross-authority-atomicity
  (let [result
        (proof/check-projection-boundaries
         (checkout-choreography))]

    (is (proof/valid? result))

    (testing "structural projection proof is not a proof that two authorities commit atomically"
      (doseq [unsupported
              [:cross-authority-atomicity-proof
               :payment-provider-correctness-proof
               :external-side-effect-rollback-proof]]
        (is (false?
             (contains?
              result
              unsupported)))))))
