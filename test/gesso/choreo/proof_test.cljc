(ns gesso.choreo.proof-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.verify :as verify]))

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

(defn- verification-failure-choreography
  []
  (choreo/->choreography
   {:initial :use
    :states
    {:use
     (choreo/local
      :browser
      :use
      :done
      {:requires #{:missing}})

     :done
     (choreo/return :done)}}))

(defn- projection-failure-choreography
  []
  (choreo/->choreography
   {:initial :choose
    :states
    {:choose
     (choreo/branch
      :alice
      :choice
      {:one :bob-one
       :two :bob-two})

     :bob-one
     (choreo/local
      :bob
      :one
      :done)

     :bob-two
     (choreo/local
      :bob
      :two
      :done)

     :done
     (choreo/return :done)}}))

(deftest first-proof-property-is-explicitly-narrow
  (is (= 2
         proof/proof-version))
  (is (= :projection-boundary-preservation-v1
         proof/projection-boundary-property))
  (is (= :exhaustive-finite-structural-check
         proof/result-classification)))

(deftest all-current-reachable-boundary-kinds-are-checked
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))

        obligations
        (:obligations result)]

    (is (proof/result? result))
    (is (proof/valid? result))
    (is (empty?
         (proof/failures result)))
    (is (nil?
         (proof/first-counterexample result)))

    (is (= {:kind :exact-finite-choreography-structure
            :reachable-state-count 9
            :role-count 2
            :obligation-count 11}
           (:scope result)))

    (is (= [[:projection-boundary :claim :owner]
            [:projection-boundary :command :sender]
            [:projection-boundary :command :receiver]
            [:projection-boundary :decide :owner]
            [:projection-boundary :observe :owner]
            [:projection-boundary :prepare :owner]
            [:projection-boundary :rejected :sender]
            [:projection-boundary :rejected :receiver]
            [:projection-boundary :settled :sender]
            [:projection-boundary :settled :receiver]
            [:projection-boundary :show :owner]]
           (mapv :id obligations)))

    (is (every?
         :valid?
         obligations))

    (is (= #{:owner :sender :receiver}
           (set
            (map :endpoint obligations))))))

(deftest local-authoritative-branch-and-await-contracts-are-preserved-exactly
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))

        by-id
        (into {}
              (map
               (juxt :id identity))
              (:obligations result))]

    (is (= {:op :local
            :action :prepare
            :requires #{}
            :outputs #{:request-id}}
           (:expected
            (get by-id
                 [:projection-boundary :prepare :owner]))))

    (is (= {:op :authoritative
            :operation :request/claim
            :requires #{:request-id}
            :outputs #{:outcome :revision}}
           (:expected
            (get by-id
                 [:projection-boundary :claim :owner]))))

    (is (= {:op :branch
            :on :outcome
            :case-values #{:confirmed :rejected}}
           (:expected
            (get by-id
                 [:projection-boundary :decide :owner]))))

    (is (= {:op :await
            :events #{:browser/observed}
            :event-contracts
            {:browser/observed
             {:required #{:basis}
              :optional #{:detail}}}}
           (:expected
            (get by-id
                 [:projection-boundary :observe :owner]))))

    (doseq [id
            [[:projection-boundary :prepare :owner]
             [:projection-boundary :claim :owner]
             [:projection-boundary :decide :owner]
             [:projection-boundary :observe :owner]]]
      (is (= (:expected (get by-id id))
             (:actual (get by-id id)))))))

(deftest communication-checks-both-sender-and-receiver-projections
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))

        by-id
        (into {}
              (map
               (juxt :id identity))
              (:obligations result))

        sender
        (get by-id
             [:projection-boundary :command :sender])

        receiver
        (get by-id
             [:projection-boundary :command :receiver])]

    (is (= {:op :send
            :to :authority
            :event :request/claim
            :via :http
            :required #{:request-id}
            :optional #{:client-note}
            :correlation #{:request-id}
            :open-payload? false}
           (:expected sender)))

    (is (= (:expected sender)
           (:actual sender)))

    (is (= {:from :browser
            :to :authority
            :event :request/claim
            :via :http
            :required #{:request-id}
            :optional #{:client-note}
            :correlation #{:request-id}
            :open-payload? false}
           (:expected receiver)))

    (is (= 1
           (count
            (get-in receiver
                    [:actual
                     :matching-alternatives]))))

    (testing "receiver correspondence does not require global and projected state ids to be equal"
      (let [location
            (first
             (get-in receiver
                     [:actual
                      :matching-alternatives]))]
        (is (not= :command
                  (:projected-state location)))
        (is (= (:expected receiver)
               (:alternative location)))))))

(deftest only-reachable-global-boundaries-create-obligations
  (let [choreography
        (choreo/->choreography
         {:initial :active
          :states
          {:active
           (choreo/local
            :browser
            :active
            :done)

           :done
           (choreo/return :done)

           :unused
           (choreo/local
            :unused-role
            :must-not-be-checked
            :done)}})

        result
        (proof/check-projection-boundaries
         choreography)]

    (is (proof/valid? result))
    (is (= 2
           (get-in result
                   [:scope
                    :reachable-state-count])))
    (is (= 1
           (get-in result
                   [:scope
                    :obligation-count])))
    (is (= [[:projection-boundary :active :owner]]
           (mapv :id
                 (:obligations result))))
    (is (not-any?
         #(= :unused
             (:state %))
         (:obligations result)))))

(deftest global-return-does-not-create-a-false-role-local-terminal-proof
  (let [result
        (proof/check-projection-boundaries
         (choreo/->choreography
          {:initial :done
           :states
           {:done
            (choreo/return :confirmed)}}))]

    (is (proof/valid? result))
    (is (= []
           (:obligations result)))
    (is (= 0
           (get-in result
                   [:scope
                    :obligation-count])))))

(deftest obligation-and-counterexample-ordering-is-deterministic
  (let [choreography
        (all-boundary-choreography)

        first-result
        (proof/check-projection-boundaries
         choreography)

        second-result
        (proof/check-projection-boundaries
         choreography)]

    (is (= first-result
           second-result))
    (is (= (mapv :id
                 (:obligations first-result))
           (mapv :id
                 (:obligations second-result))))))

(deftest verifier-failure-becomes-a-nonthrowing-construction-counterexample
  (let [result
        (proof/check-projection-boundaries
         (verification-failure-choreography))

        counterexample
        (proof/first-counterexample result)]

    (is (proof/result? result))
    (is (false?
         (proof/valid? result)))
    (is (= {:kind :not-checked
            :reason :construction-failure}
           (:scope result)))
    (is (= :construction-failure
           (:kind counterexample)))
    (is (= :verification
           (:phase counterexample)))
    (is (= :gesso.choreo/verification-failed
           (get-in counterexample
                   [:data
                    :error/type])))
    (is (= [counterexample]
           (proof/failures result)))))

(deftest projector-failure-becomes-a-nonthrowing-construction-counterexample
  (let [choreography
        (projection-failure-choreography)

        verification-options
        {:entry-knowledge
         {:alice #{:choice}}}

        verification
        (verify/verify
         choreography
         verification-options)

        result
        (proof/check-projection-boundaries
         choreography
         {:verification-options
          verification-options})

        counterexample
        (proof/first-counterexample result)]

    (testing "the verifier accepts the program under the declared entry knowledge"
      (is (:valid? verification)))

    (testing "projection still rejects hidden remote control flow"
      (is (false?
           (proof/valid? result)))
      (is (= :projection
             (:phase counterexample)))
      (is (= :uncommunicated-control-flow
             (get-in counterexample
                     [:data
                      :error/kind])))
      (is (= :bob
             (get-in counterexample
                     [:data
                      :role])))
      (is (= #{:bob-one :bob-two}
             (get-in counterexample
                     [:data
                      :local-frontier]))))))

(deftest verifier-options-are-part-of-the-checked-premises
  (let [choreography
        (choreo/->choreography
         {:initial :use
          :states
          {:use
           (choreo/local
            :browser
            :use-request
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        without-entry
        (proof/check-projection-boundaries
         choreography)

        with-entry
        (proof/check-projection-boundaries
         choreography
         {:verification-options
          {:entry-knowledge
           {:browser #{:request-id}}}})]

    (is (false?
         (proof/valid? without-entry)))
    (is (proof/valid? with-entry))
    (is (= {:entry-value-keys #{}
            :entry-knowledge
            {:browser #{:request-id}}}
           (:verification-options
            with-entry)))))

(deftest verifier-options-cannot-be-reapplied-to-a-verification-artifact
  (let [verified
        (verify/verify!
         (choreo/->choreography
          {:initial :done
           :states
           {:done
            (choreo/return :done)}}))

        result
        (proof/check-projection-boundaries
         verified
         {:verification-options
          {:entry-value-keys #{:x}}})

        counterexample
        (proof/first-counterexample result)]

    (is (false?
         (proof/valid? result)))
    (is (= :verification
           (:phase counterexample)))
    (is (= :options-with-verification-artifact
           (get-in counterexample
                   [:data
                    :error/kind])))))

(deftest bang-checker-returns-valid-result-or-throws-with-complete-failed-result
  (let [valid-result
        (proof/check-projection-boundaries!
         (all-boundary-choreography))]

    (is (proof/valid? valid-result)))

  (let [data
        (error-data
         #(proof/check-projection-boundaries!
           (verification-failure-choreography)))]

    (is (= :gesso.choreo.proof/error
           (:error/type data)))
    (is (= :projection-boundary-proof-failed
           (:error/kind data)))
    (is (proof/result?
         (:result data)))
    (is (false?
         (proof/valid?
          (:result data))))))

(deftest proof-result-inspection-is-small-and-does-not-claim-trace-refinement
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))

        explanation
        (proof/explain result)]

    (is (= {:property
            :projection-boundary-preservation-v1
            :classification
            :exhaustive-finite-structural-check
            :valid? true
            :scope
            {:kind :exact-finite-choreography-structure
             :reachable-state-count 9
             :role-count 2
             :obligation-count 11}
            :failure-count 0
            :runtime-obligation-count 0
            :trusted-assumption-count 0
            :counterexample nil}
           explanation))

    (doseq [unsupported-key
            [:trace-refinement
             :trace-preservation
             :projection-refinement
             :knowledge-proof
             :liveness-proof]]
      (is (false?
           (contains? result
                      unsupported-key))))))

(deftest opaque-state-identities-survive-proof-obligation-locators
  (let [start
        [:state :start 1]

        done
        {:state :done
         :ordinal 2}

        result
        (proof/check-projection-boundaries
         (choreo/->choreography
          {:initial start
           :states
           {start
            (choreo/local
             :browser
             :prepare
             done)

            done
            (choreo/return :done)}}))]

    (is (proof/valid? result))
    (is (= [[:projection-boundary
             start
             :owner]]
           (mapv :id
                 (:obligations result))))
    (is (= start
           (:state
            (first
             (:obligations result)))))))

(deftest result-api-rejects-values-that-are-not-proof-results
  (is (false?
       (proof/result?
        {})))
  (is (false?
       (proof/valid?
        {})))

  (is (= :invalid-result
         (:error/kind
          (error-data
           #(proof/failures {})))))

  (is (= :invalid-result
         (:error/kind
          (error-data
           #(proof/explain {}))))))

(deftest checker-options-must-be-a-map-when-present
  (is (= :invalid-options
         (:error/kind
          (error-data
           #(proof/check-projection-boundaries
             (all-boundary-choreography)
             [:not :a :map]))))))



;; -----------------------------------------------------------------------------
;; Authoritative observation / reread projection obligations
;; -----------------------------------------------------------------------------

(def authoritative-observation-contract
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(defn- authoritative-observation-choreography
  []
  (choreo/->choreography
   {:name :example/proof-authoritative-observation
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
      {:request/reread-complete :show}
      {:event-contracts
       {:request/reread-complete
        {:required #{:request-status :observed-basis}
         :optional #{:display-label}
         :authoritative-observation
         authoritative-observation-contract}}})

     :show
     (choreo/local
      :browser
      :show-current-request
      :done
      {:requires #{:request-status :observed-basis}})

     :done
     (choreo/return :done)}}))

(deftest authoritative-observation-has-an-explicit-knowledge-projection-obligation
  (let [result
        (proof/check-projection-boundaries
         (authoritative-observation-choreography))

        observation-obligations
        (filterv
         #(= :authoritative-observation
             (:endpoint %))
         (:obligations result))

        observation
        (first observation-obligations)]

    (testing "the structural checker names authoritative observation separately from generic await preservation"
      (is (proof/valid? result))
      (is (= 1
             (count observation-obligations)))
      (is (= 4
             (get-in result
                     [:scope :obligation-count])))
      (is (= [:projection-boundary
              :observe
              :authoritative-observation
              :request/reread-complete]
             (:id observation)))
      (is (= :browser
             (:role observation)))
      (is (= :authoritative-observation
             (:endpoint observation))))

    (testing "the obligation preserves the knowledge-producing authority contract"
      (is (= {:event :request/reread-complete
              :authority :request/model
              :observation :request/current-projection
              :basis-key :observed-basis
              :required #{:request-status :observed-basis}
              :optional #{:display-label}
              :semantic-keys
              #{:request-status :observed-basis :display-label}
              :open-data? false}
             (:expected observation)))
      (is (= (:expected observation)
             (:actual observation)))
      (is (nat-int?
           (:runtime-locator observation)))
      (is (= :preserved
             (:reason observation))))))

(deftest ordinary-await-does-not-acquire-an-authoritative-observation-proof-obligation
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))]

    (is (proof/valid? result))
    (is (empty?
         (filter
          #(= :authoritative-observation
              (:endpoint %))
          (:obligations result))))))

(deftest only-events-declared-as-authoritative-observations-create-observation-obligations
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/reread-complete :done
             :browser/timer-fired :done}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               authoritative-observation-contract}

              :browser/timer-fired
              {:required #{:timer-id}}}})

           :done
           (choreo/return :done)}})

        result
        (proof/check-projection-boundaries
         choreography)

        observation-obligations
        (filterv
         #(= :authoritative-observation
             (:endpoint %))
         (:obligations result))]

    (is (proof/valid? result))
    (is (= 1
           (count observation-obligations)))
    (is (= :request/reread-complete
           (get-in observation-obligations
                   [0 :expected :event])))
    (is (not-any?
         #(= :browser/timer-fired
             (get-in % [:expected :event]))
         observation-obligations))))

;; -----------------------------------------------------------------------------
;; Authoritative basis progression proof/report boundary
;; -----------------------------------------------------------------------------

(defn- progression-runtime-obligation
  [state-id role event authority observation basis-key]
  {:id
   [:authoritative-basis-progression
    state-id
    event]
   :property :authoritative-basis-progression
   :classification :runtime-enforced
   :state state-id
   :role role
   :event event
   :authority authority
   :observation observation
   :basis-key basis-key
   :when :advancing-distinct-existing-authoritative-basis
   :requires
   #{:explicit-progression-witness
     :exact-authority-match
     :exact-observation-match
     :exact-from-basis-match
     :exact-to-basis-match
     :advances-relation}})

(defn- progression-trusted-assumption
  [state-id role event authority observation basis-key]
  {:id
   [:authoritative-basis-ordering
    state-id
    event]
   :property :authoritative-basis-ordering
   :classification :trusted
   :state state-id
   :role role
   :event event
   :authority authority
   :observation observation
   :basis-key basis-key
   :assumption
   :advances-witness-truth-is-supplied-by-trusted-authority})

(deftest authoritative-basis-progression-is-reported-as-runtime-enforced-not-statically-proved
  (let [result
        (proof/check-projection-boundaries
         (authoritative-observation-choreography))

        expected-runtime
        (progression-runtime-obligation
         :observe
         :browser
         :request/reread-complete
         :request/model
         :request/current-projection
         :observed-basis)

        expected-trusted
        (progression-trusted-assumption
         :observe
         :browser
         :request/reread-complete
         :request/model
         :request/current-projection
         :observed-basis)]

    (testing "the finite structural proof exposes the dynamic progression contract instead of claiming to prove it"
      (is (proof/valid? result))
      (is (= [expected-runtime]
             (:runtime-obligations result)))
      (is (= [expected-trusted]
             (:trusted-assumptions result))))

    (testing "runtime/trusted claims are siblings of structural proof obligations, not disguised proved obligations"
      (is (= 4
             (get-in result [:scope :obligation-count])))
      (is (not-any?
           #(= :authoritative-basis-progression
               (:property %))
           (:obligations result)))
      (is (not-any?
           #(= :authoritative-basis-ordering
               (:property %))
           (:obligations result))))

    (testing "the compact explanation advertises the assumption boundary"
      (is (= 1
             (:runtime-obligation-count
              (proof/explain result))))
      (is (= 1
             (:trusted-assumption-count
              (proof/explain result)))))))

(deftest ordinary-environment-observation-does-not-create-authoritative-progression-assumptions
  (let [result
        (proof/check-projection-boundaries
         (all-boundary-choreography))]

    (is (proof/valid? result))
    (is (= []
           (:runtime-obligations result)))
    (is (= []
           (:trusted-assumptions result)))
    (is (= 0
           (:runtime-obligation-count
            (proof/explain result))))
    (is (= 0
           (:trusted-assumption-count
            (proof/explain result))))))

(deftest each-authoritative-observation-scope-gets-its-own-progression-contract-and-trust-assumption
  (let [second-contract
        {:authority :inventory/model
         :observation :inventory/current-stock
         :basis-key :inventory-basis}

        choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:request/reread-complete :wait-inventory}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               authoritative-observation-contract}}})

           :wait-inventory
           (choreo/await
            :browser
            {:inventory/reread-complete :done}
            {:event-contracts
             {:inventory/reread-complete
              {:required #{:stock :inventory-basis}
               :authoritative-observation
               second-contract}}})

           :done
           (choreo/return :done)}})

        result
        (proof/check-projection-boundaries
         choreography)]

    (is (proof/valid? result))

    (is (= [(progression-runtime-obligation
             :wait-inventory
             :browser
             :inventory/reread-complete
             :inventory/model
             :inventory/current-stock
             :inventory-basis)
            (progression-runtime-obligation
             :wait
             :browser
             :request/reread-complete
             :request/model
             :request/current-projection
             :observed-basis)]
           (:runtime-obligations result)))

    (is (= [(progression-trusted-assumption
             :wait-inventory
             :browser
             :inventory/reread-complete
             :inventory/model
             :inventory/current-stock
             :inventory-basis)
            (progression-trusted-assumption
             :wait
             :browser
             :request/reread-complete
             :request/model
             :request/current-projection
             :observed-basis)]
           (:trusted-assumptions result)))))

(deftest proof-report-never-upgrades-runtime-progression-to-freshness-or-ordering-proof
  (let [result
        (proof/check-projection-boundaries
         (authoritative-observation-choreography))]

    (is (proof/valid? result))

    (doseq [unsupported-key
            [:authoritative-basis-progression-proof
             :authoritative-basis-ordering-proof
             :observation-freshness-proof
             :authority-authenticity-proof]]
      (is (false?
           (contains? result unsupported-key))))

    (is (= :runtime-enforced
           (get-in result
                   [:runtime-obligations 0 :classification])))
    (is (= :trusted
           (get-in result
                   [:trusted-assumptions 0 :classification])))))

