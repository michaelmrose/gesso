(ns gesso.choreo.refinement-test
  "Small finite refinement/property checks over the portable Choreo semantics.

   These tests deliberately enumerate small semantic classes instead of claiming
   that a bounded concrete range proves an unbounded theorem.  The properties
   are tied to concrete failure classes and require useful counterexample paths
   when a generated witness is rejected."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]))

(defn- transport-choreography
  []
  (choreo/->choreography
   {:name :example/refinement-transport
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :client
      :prepare
      :send-command
      {:outputs #{:request-id}})

     :send-command
     (choreo/communicate
      :client
      :server
      :request/command
      :apply-command
      {:via :http
       :required #{:request-id}
       :correlation #{:request-id}})

     :apply-command
     (choreo/authoritative
      :server
      :request/apply
      :send-result
      {:requires #{:request-id}
       :outputs #{:status :revision}})

     :send-result
     (choreo/communicate
      :server
      :client
      :request/result
      :render
      {:via :sse
       :required #{:status :revision}})

     :render
     (choreo/local
      :client
      :render
      :done
      {:requires #{:status :revision}})

     :done
     (choreo/return :done)}}))

(defn- command-prefix
  [request-id]
  [{:op :local
    :role :client
    :action :prepare
    :outputs {:request-id request-id}}

   {:op :send
    :role :client
    :to :server
    :event :request/command
    :via :http
    :payload {:request-id request-id}
    :as :command}

   {:op :duplicate
    :message :command
    :as :command-copy}])

(defn- command-fault-schedule
  [request-id first-action second-action]
  (into
   (command-prefix request-id)
   [(assoc first-action :message
           (if (= :copy (:which first-action))
             :command-copy
             :command))
    (assoc second-action :message
           (if (= :copy (:which second-action))
             :command-copy
             :command))
    {:op :authoritative
     :role :server
     :operation :request/apply
     :outputs {:status :accepted
               :revision 9}}
    {:op :send
     :role :server
     :to :client
     :event :request/result
     :via :sse
     :payload {:status :accepted
               :revision 9}
     :as :result}
    {:op :deliver
     :message :result}
    {:op :local
     :role :client
     :action :render
     :outputs {}}]))

(def successful-command-fault-schedules
  [{:name :deliver-original-drop-copy
    :first {:op :deliver :which :original}
    :second {:op :drop :which :copy}}
   {:name :drop-copy-deliver-original
    :first {:op :drop :which :copy}
    :second {:op :deliver :which :original}}
   {:name :deliver-copy-drop-original
    :first {:op :deliver :which :copy}
    :second {:op :drop :which :original}}
   {:name :drop-original-deliver-copy
    :first {:op :drop :which :original}
    :second {:op :deliver :which :copy}}])

(def expected-transport-trace
  [{:kind :communication
    :from :client
    :to :server
    :event :request/command
    :payload {:request-id 17}
    :via :http}
   {:kind :authoritative
    :role :server
    :operation :request/apply
    :outputs {:status :accepted
              :revision 9}}
   {:kind :communication
    :from :server
    :to :client
    :event :request/result
    :payload {:status :accepted
              :revision 9}
    :via :sse}])

(deftest finite-duplicate-drop-schedules-refine-the-same-semantic-trace
  (doseq [{:keys [name first second]}
          successful-command-fault-schedules]
    (testing (str name)
      (let [result
            (correspondence/check-witness
             (transport-choreography)
             (command-fault-schedule 17 first second)
             {:require-complete? true})]
        (is (correspondence/valid? result))
        (is (= expected-transport-trace
               (:semantic-trace result)))
        (is (= expected-transport-trace
               (:realization-trace result)))
        (is (true? (:global-completed? result)))
        (is (true? (:realization-completed? result)))
        (is (nil? (correspondence/first-counterexample result)))))))

(deftest weak-and-lockstep-checkers-agree-on-the-generated-fault-class
  (doseq [{:keys [name first second]}
          successful-command-fault-schedules]
    (testing (str name)
      (let [witness
            (command-fault-schedule 17 first second)

            lockstep
            (correspondence/check-witness
             (transport-choreography)
             witness
             {:require-complete? true})

            weak
            (correspondence/check-weak-witness
             (transport-choreography)
             witness
             {:require-complete? true})]
        (is (correspondence/valid? lockstep))
        (is (correspondence/valid? weak))
        (is (= (:semantic-trace lockstep)
               (:semantic-trace weak)))
        (is (= (:realization-trace lockstep)
               (:realization-trace weak)))
        (is (= (:global-outcome lockstep)
               (:global-outcome weak)))
        (is (= (:realization-outcomes lockstep)
               (:realization-outcomes weak)))))))

(defn- bad-delivery-witness
  []
  (into
   (command-prefix 17)
   [{:op :drop
     :message :command}
    {:op :deliver
     :message :command}]))

(deftest generated-invalid-schedule-reports-the-first-bad-witness-step
  (doseq [checker
          [correspondence/check-witness
           correspondence/check-weak-witness]]
    (let [result
          (checker
           (transport-choreography)
           (bad-delivery-witness))

          counterexample
          (correspondence/first-counterexample result)]
      (is (false? (correspondence/valid? result)))
      (is (= 4 (:step-index counterexample)))
      (is (= :witness-delivery-resolution-failed
             (:kind counterexample)))
      (is (= :unknown-witness-message
             (get-in counterexample
                     [:error :data :error/kind])))
      (is (= {:op :deliver
              :message :command}
             (:step counterexample))))))

(defn- successive-observation-choreography
  []
  (choreo/->choreography
   {:name :example/refinement-authoritative-basis
    :initial :first
    :states
    {:first
     (choreo/await
      :reader
      {:request/observed :second}
      {:event-contracts
       {:request/observed
        {:required #{:status :basis}
         :authoritative-observation
         {:authority :request-db
          :observation :request/read
          :basis-key :basis}}}})

     :second
     (choreo/await
      :reader
      {:request/observed :done}
      {:event-contracts
       {:request/observed
        {:required #{:status :basis}
         :authoritative-observation
         {:authority :request-db
          :observation :request/read
          :basis-key :basis}}}})

     :done
     (choreo/return :done)}}))

(def basis-pairs
  [[0 1]
   [41 42]
   [{:tx 41} {:tx 42}]
   [[:opaque :a] [:opaque :b]]])

(defn- progression-witness
  [from-basis to-basis relation]
  {:kind :authoritative-basis-progression
   :authority :request-db
   :observation :request/read
   :from-basis from-basis
   :to-basis to-basis
   :relation relation})

(defn- successive-observation-witness
  [from-basis to-basis progression]
  (cond->
   [{:op :environment
     :role :reader
     :event :request/observed
     :data {:status :pending
            :basis from-basis}}
    {:op :environment
     :role :reader
     :event :request/observed
     :data {:status :approved
            :basis to-basis}}]
    (some? progression)
    (assoc-in [1 :authoritative-basis-progression]
              progression)))

(defn- realization-error-kind
  [result]
  (get-in
   (correspondence/first-counterexample result)
   [:realization-error :data :error/kind]))

(deftest opaque-basis-values-never-acquire-order-from-their-concrete-representation
  (doseq [[from-basis to-basis] basis-pairs
          checker [correspondence/check-witness
                   correspondence/check-weak-witness]]
    (let [without-progression
          (checker
           (successive-observation-choreography)
           (successive-observation-witness
            from-basis
            to-basis
            nil)
           {:require-complete? true})

          advances
          (checker
           (successive-observation-choreography)
           (successive-observation-witness
            from-basis
            to-basis
            (progression-witness
             from-basis
             to-basis
             :advances))
           {:require-complete? true})]
      (is (false? (correspondence/valid? without-progression)))
      (is (= :authoritative-progression-required
             (realization-error-kind without-progression)))
      (is (correspondence/valid? advances)))))

(deftest explicit-progression-relation-not-concrete-basis-shape-controls-admissibility
  (doseq [[from-basis to-basis] basis-pairs
          checker [correspondence/check-witness
                   correspondence/check-weak-witness]
          relation [:precedes :incomparable]]
    (let [result
          (checker
           (successive-observation-choreography)
           (successive-observation-witness
            from-basis
            to-basis
            (progression-witness
             from-basis
             to-basis
             relation))
           {:require-complete? true})]
      (is (false? (correspondence/valid? result)))
      (is (= :authoritative-basis-not-advancing
             (realization-error-kind result))))))
