(ns gesso.choreo.correspondence-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]
   [gesso.choreo.realization :as realization]))

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(defn- all-boundary-choreography
  []
  (choreo/->choreography
   {:name :example/proof-all-boundaries
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :browser
      :prepare
      :command
      {:outputs #{:request-id}})

     :command
     (choreo/communicate
      :browser
      :authority
      :request/claim
      :claim
      {:via :http
       :required #{:request-id}
       :optional #{:client-note}
       :correlation #{:request-id}})

     :claim
     (choreo/authoritative
      :authority
      :request/claim
      :decide
      {:requires #{:request-id}
       :outputs #{:outcome :revision}})

     :decide
     (choreo/branch
      :authority
      :outcome
      {:confirmed :settled
       :rejected :rejected})

     :settled
     (choreo/communicate
      :authority
      :browser
      :request/settled
      :observe
      {:via :sse
       :required #{:outcome :revision}})

     :observe
     (choreo/await
      :browser
      {:browser/observed :show}
      {:event-contracts
       {:browser/observed
        {:required #{:basis}
         :optional #{:detail}}}})

     :show
     (choreo/local
      :browser
      :show
      :done
      {:requires #{:basis}})

     :rejected
     (choreo/communicate
      :authority
      :browser
      :request/rejected
      :done
      {:required #{:outcome :revision}})

     :done
     (choreo/return :done)}}))

(defn- confirmed-fault-witness
  []
  [{:op :local
    :role :browser
    :action :prepare
    :outputs {:request-id 17}}

   {:op :send
    :role :browser
    :to :authority
    :event :request/claim
    :via :http
    :payload {:request-id 17}
    :as :command}

   {:op :duplicate
    :message :command
    :as :command-copy}

   {:op :drop
    :message :command}

   {:op :deliver
    :message :command-copy}

   {:op :authoritative
    :role :authority
    :operation :request/claim
    :outputs {:outcome :confirmed
              :revision 24}}

   {:op :send
    :role :authority
    :to :browser
    :event :request/settled
    :via :sse
    :payload {:outcome :confirmed
              :revision 24}
    :as :settlement}

   {:op :deliver
    :message :settlement}

   {:op :environment
    :role :browser
    :event :browser/observed
    :data {:basis 24
           :detail :canonical}}

   {:op :local
    :role :browser
    :action :show
    :outputs {}}])

(deftest correspondence-property-is-explicitly-concrete-and-narrow
  (is (= 1
         correspondence/correspondence-version))
  (is (= :gesso.choreo.correspondence/result
         correspondence/result-type))
  (is (= :gesso.choreo.correspondence/obligation
         correspondence/obligation-type))
  (is (= :concrete-lockstep-realization-correspondence-v1
         correspondence/correspondence-property))
  (is (= :concrete-lockstep-execution-check
         correspondence/correspondence-classification))
  (is (= #{:local
           :authoritative
           :send
           :deliver
           :drop
           :duplicate
           :environment}
         correspondence/witness-ops)))

(deftest concrete-fault-witness-preserves-one-semantic-communication-per-delivery
  (let [witness
        (confirmed-fault-witness)

        result
        (correspondence/check-witness
         (all-boundary-choreography)
         witness
         {:require-complete? true})

        expected-trace
        [{:kind :communication
          :from :browser
          :to :authority
          :event :request/claim
          :payload {:request-id 17}
          :via :http}

         {:kind :authoritative
          :role :authority
          :operation :request/claim
          :outputs {:outcome :confirmed
                    :revision 24}}

         {:kind :communication
          :from :authority
          :to :browser
          :event :request/settled
          :payload {:outcome :confirmed
                    :revision 24}
          :via :sse}]]

    (is (correspondence/result? result))
    (is (correspondence/valid? result))
    (is (= correspondence/correspondence-property
           (:property result)))
    (is (= correspondence/correspondence-classification
           (:classification result)))

    (is (= {:kind :concrete-lockstep-witness
            :witness-step-count (count witness)
            :checked-step-count (count witness)
            :require-complete? true}
           (:scope result)))

    (is (= expected-trace
           (:semantic-trace result)))
    (is (= expected-trace
           (:realization-trace result)))

    (testing "physical duplicate/drop steps do not invent semantic observations"
      (is (= 3
             (count
              (:semantic-trace result))))
      (is (= 3
             (count
              (:realization-trace result)))))

    (is (true?
         (:global-completed? result)))
    (is (true?
         (:realization-completed? result)))
    (is (= :done
           (:global-outcome result)))
    (is (empty?
         (correspondence/failures result)))
    (is (nil?
         (correspondence/first-counterexample result)))))

(deftest deterministic-global-branches-are-traversed-as-part-of-the-paired-step
  (let [result
        (correspondence/check-witness
         (all-boundary-choreography)
         (confirmed-fault-witness)
         {:require-complete? true})

        authoritative-obligation
        (first
         (filter
          #(and (= :paired-semantic-step
                   (:kind %))
                (= :authoritative
                   (get-in % [:step :op])))
          (:obligations result)))]

    (is (correspondence/valid? result))
    (is (= [{:role :authority
             :on :outcome
             :value :confirmed}]
           (:auto-branches
            authoritative-obligation)))))

(deftest incomplete-prefix-can-be-checked-without-claiming-completion
  (let [prefix
        [{:op :local
          :role :browser
          :action :prepare
          :outputs {:request-id 17}}]

        prefix-result
        (correspondence/check-witness
         (all-boundary-choreography)
         prefix)

        completion-required
        (correspondence/check-witness
         (all-boundary-choreography)
         prefix
         {:require-complete? true})]

    (testing "a legal concrete prefix is a valid prefix correspondence check"
      (is (correspondence/valid? prefix-result))
      (is (false?
           (:global-completed? prefix-result)))
      (is (false?
           (:realization-completed? prefix-result)))
      (is (false?
           (get-in prefix-result
                   [:scope :require-complete?]))))

    (testing "completion is a separate, explicit obligation"
      (is (false?
           (correspondence/valid? completion-required)))
      (is (= :completion
             (:kind
              (correspondence/first-counterexample
               completion-required))))
      (is (= true
             (get-in completion-required
                     [:counterexample :required?])))
      (is (= (:semantic-trace completion-required)
             (:realization-trace completion-required))))))

(deftest bad-concrete-schedule-returns-a-witness-counterexample
  (let [result
        (correspondence/check-witness
         (all-boundary-choreography)
         [{:op :deliver
           :message :never-emitted}])

        counterexample
        (correspondence/first-counterexample result)]

    (is (correspondence/result? result))
    (is (false?
         (correspondence/valid? result)))
    (is (= :witness-delivery-resolution-failed
           (:kind counterexample)))
    (is (= 0
           (:step-index counterexample)))
    (is (= :unknown-witness-message
           (get-in counterexample
                   [:error :data :error/kind])))
    (is (= [counterexample]
           (correspondence/failures result)))))

(deftest witness-send-identity-is-checked-against-the-projected-machine
  (let [result
        (correspondence/check-witness
         (all-boundary-choreography)
         [{:op :local
           :role :browser
           :action :prepare
           :outputs {:request-id 17}}
          {:op :send
           :role :browser
           :to :authority
           :event :request/not-the-projected-event
           :via :http
           :payload {:request-id 17}}])

        counterexample
        (correspondence/first-counterexample result)]

    (is (false?
         (correspondence/valid? result)))
    (is (= :witness-send-identity-mismatch
           (:kind counterexample)))
    (is (= {:to :authority
            :event :request/not-the-projected-event
            :via :http}
           (:expected counterexample)))
    (is (= {:to :authority
            :event :request/claim
            :via :http}
           (:actual counterexample)))))

(deftest conflicting-role-local-entry-values-require-an-explicit-global-semantic-value
  (let [entry-values
        {:browser {:shared :browser-view}
         :authority {:shared :authority-view}}

        inferred
        (correspondence/check-witness
         (all-boundary-choreography)
         []
         {:entry-values-by-role entry-values})

        explicit
        (correspondence/check-witness
         (all-boundary-choreography)
         []
         {:entry-values-by-role entry-values
          :semantic-entry-values
          {:shared :authoritative-view}})]

    (testing "the current single global semantic store cannot silently collapse disagreement"
      (is (false?
           (correspondence/valid? inferred)))
      (is (= :construction-failure
             (:kind
              (correspondence/first-counterexample inferred))))
      (is (= :witness-start
             (:phase
              (correspondence/first-counterexample inferred))))
      (is (= :conflicting-global-entry-value
             (get-in inferred
                     [:counterexample :data :error/kind]))))

    (testing "an explicit semantic entry map makes the abstraction choice visible"
      (is (correspondence/valid? explicit))
      (is (= []
             (:semantic-trace explicit)))
      (is (= []
             (:realization-trace explicit))))))

(deftest duplicate-of-duplicate-can-carry-the-only-surviving-semantic-delivery
  (let [witness
        [{:op :local
          :role :browser
          :action :prepare
          :outputs {:request-id 17}}

         {:op :send
          :role :browser
          :to :authority
          :event :request/claim
          :via :http
          :payload {:request-id 17}
          :as :command}

         {:op :duplicate
          :message :command
          :as :copy-1}

         {:op :duplicate
          :message :copy-1
          :as :copy-2}

         {:op :drop
          :message :command}

         {:op :drop
          :message :copy-1}

         {:op :deliver
          :message :copy-2}]

        result
        (correspondence/check-witness
         (all-boundary-choreography)
         witness)

        duplicate-obligations
        (filterv #(= :transport-duplicate (:kind %))
                 (:obligations result))]

    (is (correspondence/valid? result))
    (is (= 2 (count duplicate-obligations)))

    (testing "each transport copy has a fresh physical id and names its immediate source"
      (let [[first-copy second-copy] duplicate-obligations]
        (is (not= (:source-message-id first-copy)
                  (:message-id first-copy)))
        (is (= (:message-id first-copy)
               (:source-message-id second-copy)))
        (is (not= (:message-id first-copy)
                  (:message-id second-copy)))
        (is (= (:message first-copy)
               (:message second-copy)))))

    (testing "copy lineage does not create extra semantic communications"
      (is (= 1 (count (:semantic-trace result))))
      (is (= (:semantic-trace result)
             (:realization-trace result)))
      (is (= :communication
             (get-in result [:semantic-trace 0 :kind])))
      (is (= :request/claim
             (get-in result [:semantic-trace 0 :event]))))))

(deftest failed-witness-counterexamples-are-deterministic
  (let [witness
        [{:op :deliver
          :message :never-emitted}]
        first-result
        (correspondence/check-witness
         (all-boundary-choreography)
         witness)
        second-result
        (correspondence/check-witness
         (all-boundary-choreography)
         witness)]
    (is (= first-result second-result))
    (is (= (correspondence/first-counterexample first-result)
           (correspondence/first-counterexample second-result)))
    (is (= :witness-delivery-resolution-failed
           (get-in first-result [:counterexample :kind])))))

(deftest result-api-rejects-values-that-are-not-correspondence-results
  (is (false? (correspondence/result? {})))
  (is (false? (correspondence/valid? {})))
  (is (= :invalid-result
         (:error/kind
          (error-data #(correspondence/failures {})))))
  (is (= :invalid-result
         (:error/kind
          (error-data #(correspondence/first-counterexample {})))))
  (is (= :invalid-result
         (:error/kind
          (error-data #(correspondence/explain {}))))))

(deftest bang-correspondence-checker-returns-valid-result-or-throws-with-complete-result
  (let [valid-result
        (correspondence/check-witness!
         (all-boundary-choreography)
         (confirmed-fault-witness)
         {:require-complete? true})]

    (is (correspondence/valid? valid-result)))

  (let [data
        (error-data
         #(correspondence/check-witness!
           (all-boundary-choreography)
           [{:op :deliver
             :message :missing}]))]

    (is (= :gesso.choreo.correspondence/error
           (:error/type data)))
    (is (= :realization-witness-check-failed
           (:error/kind data)))
    (is (correspondence/result?
         (:result data)))
    (is (false?
         (correspondence/valid?
          (:result data))))
    (is (= :witness-delivery-resolution-failed
           (get-in data
                   [:result :counterexample :kind])))))

(deftest correspondence-result-explanation-remains-honest-about-proof-scope
  (let [witness
        (confirmed-fault-witness)

        result
        (correspondence/check-witness
         (all-boundary-choreography)
         witness
         {:require-complete? true})

        explanation
        (correspondence/explain result)]

    (is (= {:property
            :concrete-lockstep-realization-correspondence-v1
            :classification
            :concrete-lockstep-execution-check
            :valid? true
            :scope
            {:kind :concrete-lockstep-witness
             :witness-step-count (count witness)
             :checked-step-count (count witness)
             :require-complete? true}
            :failure-count 0
            :counterexample nil}
           explanation))

    (doseq [unsupported-key
            [:all-schedules
             :trace-refinement
             :trace-preservation
             :projection-refinement
             :liveness-proof
             :knowledge-proof]]
      (is (false?
           (contains? result
                      unsupported-key))))))

(deftest witness-and-option-shape-errors-remain-programmer-errors
  (is (= :invalid-witness
         (:error/kind
          (error-data
           #(correspondence/check-witness
             (all-boundary-choreography)
             '({:op :local}))))))

  (is (= :unsupported-witness-op
         (:error/kind
          (error-data
           #(correspondence/check-witness
             (all-boundary-choreography)
             [{:op :teleport}])))))

  (is (= :invalid-options
         (:error/kind
          (error-data
           #(correspondence/check-witness
             (all-boundary-choreography)
             []
             [:not :a :map])))))

  (is (= :invalid-options
         (:error/kind
          (error-data
           #(correspondence/check-witness
             (all-boundary-choreography)
             []
             {:require-complete? :yes})))))

  (is (= :invalid-entry-values
         (get-in
          (correspondence/first-counterexample
           (correspondence/check-witness
            (all-boundary-choreography)
            []
            {:entry-values-by-role
             {:browser [:not :a :map]}}))
          [:data :error/kind]))))


;; -----------------------------------------------------------------------------
;; Weak concrete correspondence
;; -----------------------------------------------------------------------------

(defn- independent-locals-choreography
  []
  (choreo/->choreography
   {:name :example/independent-locals
    :initial :alice-local
    :states
    {:alice-local
     (choreo/local
      :alice
      :alice/work
      :bob-local
      {})

     :bob-local
     (choreo/local
      :bob
      :bob/work
      :done
      {})

     :done
     (choreo/return :done)}}))

(defn- local-before-environment-choreography
  []
  (choreo/->choreography
   {:name :example/local-before-environment
    :initial :alice-local
    :states
    {:alice-local
     (choreo/local
      :alice
      :alice/work
      :bob-await
      {})

     :bob-await
     (choreo/await
      :bob
      {:bob/ready :done}
      {:event-contracts
       {:bob/ready
        {:required #{:basis}
         :optional #{:detail}}}})

     :done
     (choreo/return :done)}}))

(defn- local-before-foreign-authority-choreography
  []
  (choreo/->choreography
   {:name :example/local-before-foreign-authority
    :initial :alice-local
    :states
    {:alice-local
     (choreo/local
      :alice
      :alice/work
      :bob-authority
      {})

     :bob-authority
     (choreo/authoritative
      :bob
      :bob/commit
      :done
      {})

     :done
     (choreo/return :done)}}))

(defn- ordered-authority-choreography
  []
  (choreo/->choreography
   {:name :example/ordered-authority
    :initial :first
    :states
    {:first
     (choreo/authoritative
      :authority
      :authority/first
      :second
      {})

     :second
     (choreo/authoritative
      :authority
      :authority/second
      :done
      {})

     :done
     (choreo/return :done)}}))

(deftest weak-correspondence-property-is-explicitly-concrete-and-narrow
  (is (= :concrete-weak-realization-correspondence-v1
         correspondence/weak-correspondence-property))
  (is (= :concrete-weak-execution-check
         correspondence/weak-correspondence-classification))
  (is (= #{correspondence/correspondence-property
           correspondence/weak-correspondence-property}
         correspondence/correspondence-properties))

  (let [result
        (correspondence/check-weak-witness
         (independent-locals-choreography)
         [])]
    (is (correspondence/result? result))
    (is (= correspondence/weak-correspondence-property
           (:property result)))
    (is (= correspondence/weak-correspondence-classification
           (:classification result)))
    (is (= :concrete-weak-witness
           (get-in result [:scope :kind]))))

  (doseq [unsupported-key
          [:all-schedules
           :trace-refinement
           :trace-preservation
           :projection-refinement
           :liveness-proof
           :knowledge-proof]]
    (is (false?
         (contains?
          (correspondence/check-weak-witness
           (independent-locals-choreography)
           [])
          unsupported-key)))))

(deftest weak-checker-commutes-independent-hidden-locals
  (let [choreography
        (independent-locals-choreography)

        witness
        [{:op :local
          :role :bob
          :action :bob/work
          :outputs {}}
         {:op :local
          :role :alice
          :action :alice/work
          :outputs {}}]

        lockstep
        (correspondence/check-witness
         choreography
         witness
         {:require-complete? true})

        weak
        (correspondence/check-weak-witness
         choreography
         witness
         {:require-complete? true})]

    (testing "strict lockstep rejects the projected schedule because Bob runs before the global textual state"
      (is (false?
           (correspondence/valid? lockstep))))

    (testing "weak correspondence linearizes the same distributed-hidden work against the global semantics"
      (is (correspondence/valid? weak))
      (is (= [] (:semantic-trace weak)))
      (is (= [] (:realization-trace weak)))
      (is (empty? (:pending-unobservable weak)))
      (is (true? (:global-completed? weak)))
      (is (true? (:realization-completed? weak)))
      (is (= :done (:global-outcome weak))))

    (testing "the later Alice event may replay before an earlier-buffered Bob event when that is what the global state enables"
      (let [replays
            (filterv #(= :replayed-unobservable
                         (:kind %))
                     (:obligations weak))]
        (is (= 2 (count replays)))
        (is (= [:alice :bob]
               (mapv #(get-in % [:semantic-event :role])
                     replays)))
        (is (= [1 0]
               (mapv :buffer-position replays)))))))

(deftest weak-checker-commutes-an-environment-observation-ahead-of-a-foreign-local
  (let [choreography
        (local-before-environment-choreography)

        witness
        [{:op :environment
          :role :bob
          :event :bob/ready
          :data {:basis 24
                 :detail :canonical}}
         {:op :local
          :role :alice
          :action :alice/work
          :outputs {}}]

        result
        (correspondence/check-weak-witness
         choreography
         witness
         {:require-complete? true})]

    (is (correspondence/valid? result))
    (is (empty? (:pending-unobservable result)))
    (is (= [] (:semantic-trace result)))
    (is (= [] (:realization-trace result)))
    (is (true? (:global-completed? result)))
    (is (true? (:realization-completed? result)))

    (let [distributed
          (first
           (filter #(and (= :distributed-unobservable-step (:kind %))
                         (= :environment (get-in % [:step :op])))
                   (:obligations result)))

          replayed
          (first
           (filter #(and (= :replayed-unobservable (:kind %))
                         (= :environment (get-in % [:step :op])))
                   (:obligations result)))]
      (is (= {:basis 24
              :detail :canonical}
             (get-in distributed [:semantic-event :data])))
      (is (= (:semantic-event distributed)
             (:semantic-event replayed))))))

(deftest weak-checker-does-not-reorder-authoritative-steps-that-the-local-machine-orders
  (let [result
        (correspondence/check-weak-witness
         (ordered-authority-choreography)
         [{:op :authoritative
           :role :authority
           :operation :authority/second
           :outputs {}}])

        counterexample
        (correspondence/first-counterexample result)]

    (is (false?
         (correspondence/valid? result)))
    (is (= :weak-realization-boundary-mismatch
           (:kind counterexample)))
    (is (= {:op :authoritative
            :operation :authority/second}
           (:expected counterexample)))
    (is (= :authoritative
           (get-in counterexample [:actual :op])))
    (is (= :authority/first
           (get-in counterexample [:actual :operation])))))

(deftest weak-checker-linearizes-an-authority-step-ahead-of-an-earlier-foreign-local
  (let [choreography
        (local-before-foreign-authority-choreography)

        realized0
        (realization/start choreography)

        projected-boundary
        (realization/boundary realized0 :bob)

        ;; Projection legitimately exposes Bob's authority boundary immediately:
        ;; Alice's preceding :local is distributed-unobservable and establishes
        ;; no value Bob requires. Therefore this concrete distributed step is
        ;; executable even before Alice performs her local action.
        realized1
        (realization/complete-authoritative
         realized0
         :bob
         {})

        witness
        [{:op :authoritative
          :role :bob
          :operation :bob/commit
          :outputs {}}
         {:op :local
          :role :alice
          :action :alice/work
          :outputs {}}]

        result
        (correspondence/check-weak-witness
         choreography
         witness
         {:require-complete? true})

        replay-kinds
        (->> (:obligations result)
             (keep (fn [obligation]
                     (when (contains? #{:replayed-unobservable
                                       :replayed-observable}
                                     (:kind obligation))
                       (:kind obligation))))
             vec)]

    (testing "the independently projected machine permits the authority step first"
      (is (= :authoritative
             (:kind projected-boundary)))
      (is (= :bob/commit
             (:operation projected-boundary)))
      (is (map? realized1)))

    (testing "weak correspondence linearizes the later-observed hidden local before replaying the buffered authority"
      (is (correspondence/valid? result))
      (is (= [:replayed-unobservable
              :replayed-observable]
             replay-kinds))
      (is (empty? (:pending-unobservable result)))
      (is (empty? (:pending-observable result))))

    (testing "observable behavior and completion still agree"
      (is (= [{:kind :authoritative
               :role :bob
               :operation :bob/commit
               :outputs {}}]
             (:semantic-trace result)))
      (is (= (:semantic-trace result)
             (:realization-trace result)))
      (is (true? (:global-completed? result)))
      (is (true? (:realization-completed? result)))
      (is (= :done
             (:global-outcome result)))
      (is (nil?
           (correspondence/first-counterexample result))))))

(deftest weak-correspondence-completion-remains-an-explicit-obligation
  (let [prefix
        [{:op :local
          :role :bob
          :action :bob/work
          :outputs {}}]

        prefix-result
        (correspondence/check-weak-witness
         (independent-locals-choreography)
         prefix)

        completion-required
        (correspondence/check-weak-witness
         (independent-locals-choreography)
         prefix
         {:require-complete? true})]

    (is (correspondence/valid? prefix-result))
    (is (= 1
           (count (:pending-unobservable prefix-result))))
    (is (false?
         (:global-completed? prefix-result)))

    (is (false?
         (correspondence/valid? completion-required)))
    (is (= :completion
           (:kind
            (correspondence/first-counterexample
             completion-required))))
    (is (= 1
           (get-in completion-required
                   [:counterexample :pending-count])))))

(deftest weak-bang-checker-and-explanation-use-the-weak-result-contract
  (let [witness
        [{:op :local
          :role :bob
          :action :bob/work
          :outputs {}}
         {:op :local
          :role :alice
          :action :alice/work
          :outputs {}}]

        result
        (correspondence/check-weak-witness!
         (independent-locals-choreography)
         witness
         {:require-complete? true})]

    (is (correspondence/valid? result))
    (is (= correspondence/weak-correspondence-property
           (:property result)))
    (is (= correspondence/weak-correspondence-classification
           (:classification result)))
    (is (= {:property correspondence/weak-correspondence-property
            :classification correspondence/weak-correspondence-classification
            :valid? true
            :scope (:scope result)
            :failure-count 0
            :counterexample nil}
           (correspondence/explain result))))

  (let [data
        (error-data
         #(correspondence/check-weak-witness!
           (ordered-authority-choreography)
           [{:op :authoritative
             :role :authority
             :operation :authority/second
             :outputs {}}]))]
    (is (= :gesso.choreo.correspondence/error
           (:error/type data)))
    (is (= :weak-realization-witness-check-failed
           (:error/kind data)))
    (is (= correspondence/weak-correspondence-property
           (get-in data [:result :property])))
    (is (= :weak-realization-boundary-mismatch
           (get-in data [:result :counterexample :kind])))))
