(ns gesso.choreo.proof-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.semantics :as semantics]
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

;; -----------------------------------------------------------------------------
;; Exact compiler provenance at the proof boundary
;; -----------------------------------------------------------------------------

(deftest proof-obligations-distinguish-identical-runtime-boundaries-by-semantic-state
  (let [choreography
        (choreo/->choreography
         {:name :example/proof-exact-local-provenance
          :initial :choose
          :states
          {:choose
           (choreo/branch
            :browser
            :choice
            {:first :first
             :second :second})

           :first
           (choreo/local
            :browser
            :same-action
            :done)

           :second
           (choreo/local
            :browser
            :same-action
            :done)

           :done
           (choreo/return :done)}})

        result
        (proof/check-projection-boundaries
         choreography
         {:verification-options
          {:entry-knowledge
           {:browser #{:choice}}}})

        by-id
        (into {}
              (map (juxt :id identity))
              (:obligations result))

        first-obligation
        (get by-id
             [:projection-boundary :first :owner])

        second-obligation
        (get by-id
             [:projection-boundary :second :owner])]

    (is (proof/valid? result))

    (testing "identical executable boundary values do not erase semantic identity"
      (is (= (:expected first-obligation)
             (:expected second-obligation)))
      (is (= (:actual first-obligation)
             (:actual second-obligation)))
      (is (= {:op :local
              :action :same-action
              :requires #{}
              :outputs #{}}
             (:actual first-obligation))))

    (testing "each authored semantic state is proved against its exact compiler-recorded runtime location"
      (is (= :first
             (:state first-obligation)))
      (is (= :second
             (:state second-obligation)))
      (is (= 1
             (:runtime-locator first-obligation)))
      (is (= 2
             (:runtime-locator second-obligation)))
      (is (not= (:runtime-locator first-obligation)
                (:runtime-locator second-obligation))))))

(deftest proof-obligations-preserve-intentional-many-to-one-receive-provenance
  (let [choreography
        (choreo/->choreography
         {:name :example/proof-exact-collapsed-receive-provenance
          :initial :choose
          :states
          {:choose
           (choreo/branch
            :server
            :choice
            {:first :send-first
             :second :send-second})

           :send-first
           (choreo/communicate
            :server
            :browser
            :request/update
            :done
            {:required #{:request-id}
             :correlation #{:request-id}})

           :send-second
           (choreo/communicate
            :server
            :browser
            :request/update
            :done
            {:required #{:request-id}
             :correlation #{:request-id}})

           :done
           (choreo/return :done)}})

        result
        (proof/check-projection-boundaries
         choreography
         {:verification-options
          {:entry-knowledge
           {:server #{:choice :request-id}}}})

        by-id
        (into {}
              (map (juxt :id identity))
              (:obligations result))

        first-receiver
        (get by-id
             [:projection-boundary :send-first :receiver])

        second-receiver
        (get by-id
             [:projection-boundary :send-second :receiver])

        first-location
        (first
         (get-in first-receiver
                 [:actual :matching-alternatives]))

        second-location
        (first
         (get-in second-receiver
                 [:actual :matching-alternatives]))]

    (is (proof/valid? result))

    (testing "both semantic communications remain distinct proof obligations"
      (is (= :send-first
             (:state first-receiver)))
      (is (= :send-second
             (:state second-receiver)))
      (is (= (:expected first-receiver)
             (:expected second-receiver))))

    (testing "projection may intentionally collapse both authored communications onto one exact receive alternative"
      (is (= 1
             (count
              (get-in first-receiver
                      [:actual :matching-alternatives]))))
      (is (= 1
             (count
              (get-in second-receiver
                      [:actual :matching-alternatives]))))
      (is (= 0
             (:runtime-locator first-location)))
      (is (= 0
             (:runtime-locator second-location)))
      (is (= 0
             (:alternative-index first-location)))
      (is (= 0
             (:alternative-index second-location)))
      (is (= first-location
             second-location)))))

;; -----------------------------------------------------------------------------
;; Exact projected-successor preservation
;; -----------------------------------------------------------------------------

(defn- foreign-local-skip-choreography
  []
  (choreo/->choreography
   {:name :example/proof-successor-foreign-local-skip
    :initial :alice-work
    :states
    {:alice-work
     (choreo/local
      :alice
      :alice/work
      :bob-work)

     :bob-work
     (choreo/local
      :bob
      :bob/work
      :alice-send)

     :alice-send
     (choreo/communicate
      :alice
      :bob
      :example/done
      :done)

     :done
     (choreo/return :done)}}))

(deftest successor-proof-property-is-explicitly-narrow
  (is (= :projection-successor-preservation-v1
         proof/projection-successor-property))

  (is (= :exhaustive-finite-successor-check
         proof/successor-result-classification))

  (is (contains?
       proof/structural-properties
       proof/projection-successor-property))

  (let [result
        (proof/check-projection-successors
         (all-boundary-choreography))]

    (is (proof/result? result))
    (is (proof/valid? result))

    (is (= proof/projection-successor-property
           (:property result)))

    (is (= proof/successor-result-classification
           (:classification result)))

    (is (= :exact-finite-choreography-transition-structure
           (get-in result [:scope :kind])))

    (is (false?
         (contains? result :trace-refinement)))

    (is (false?
         (contains? result :projection-refinement)))

    (is (= proof/projection-successor-property
           (:property
            (proof/explain result))))))

(deftest all-current-reachable-successor-kinds-are-checked
  (let [result
        (proof/check-projection-successors
         (all-boundary-choreography))

        obligations
        (:obligations result)]

    (is (proof/valid? result))
    (is (empty?
         (proof/failures result)))
    (is (nil?
         (proof/first-counterexample result)))

    (is (= {:kind :exact-finite-choreography-transition-structure
            :reachable-state-count 9
            :role-count 2
            :obligation-count 13}
           (:scope result)))

    (is (= [[:projection-successor :initial :authority]
            [:projection-successor :initial :browser]
            [:projection-successor :claim :owner]
            [:projection-successor :command :sender]
            [:projection-successor :command :receiver]
            [:projection-successor :decide :owner]
            [:projection-successor :observe :owner]
            [:projection-successor :prepare :owner]
            [:projection-successor :rejected :sender]
            [:projection-successor :rejected :receiver]
            [:projection-successor :settled :sender]
            [:projection-successor :settled :receiver]
            [:projection-successor :show :owner]]
           (mapv :id obligations)))

    (is (every?
         :valid?
         obligations))

    (is (every?
         #(= proof/projection-successor-property
             (:property %))
         obligations))

    (is (= #{:initial :owner :sender :receiver}
           (set
            (map :endpoint obligations))))))

(deftest direct-branch-await-and-communication-successors-use-exact-compiler-continuations
  (let [result
        (proof/check-projection-successors
         (all-boundary-choreography))

        by-id
        (into {}
              (map (juxt :id identity))
              (:obligations result))]

    (testing "ordinary direct successors compare projected :next with the exact semantic continuation"
      (is (= {:semantic-target :command
              :runtime-target 1}
             (:expected
              (get by-id
                   [:projection-successor :prepare :owner]))))

      (is (= {:source-runtime-locators [0]
              :runtime-targets [1]}
             (:actual
              (get by-id
                   [:projection-successor :prepare :owner])))))

    (testing "communication sender and receiver continuations are checked independently"
      (is (= {:semantic-target :claim
              :runtime-target 2}
             (:expected
              (get by-id
                   [:projection-successor :command :sender]))))

      (is (= {:semantic-target :claim
              :runtime-target 1}
             (:expected
              (get by-id
                   [:projection-successor :command :receiver]))))

      (is (= {:source-locations
              [{:runtime-locator 0
                :alternative-index 0}]
              :runtime-targets [1]}
             (:actual
              (get by-id
                   [:projection-successor :command :receiver])))))

    (testing "every branch case is tied to its own exact continuation"
      (is (= {:cases
              {:confirmed
               {:semantic-target :settled
                :runtime-target 3}
               :rejected
               {:semantic-target :rejected
                :runtime-target 4}}}
             (:expected
              (get by-id
                   [:projection-successor :decide :owner])))))

    (testing "every environment event is tied to its own exact continuation"
      (is (= {:events
              {:browser/observed
               {:semantic-target :show
                :runtime-target 5}}}
             (:expected
              (get by-id
                   [:projection-successor :observe :owner])))))))

(deftest successor-proof-preserves-legitimate-foreign-role-skips
  (let [result
        (proof/check-projection-successors
         (foreign-local-skip-choreography))

        by-id
        (into {}
              (map (juxt :id identity))
              (:obligations result))

        alice-work
        (get by-id
             [:projection-successor :alice-work :owner])

        bob-work
        (get by-id
             [:projection-successor :bob-work :owner])]

    (is (proof/valid? result))

    (testing "Alice semantically enters Bob's work but her local machine continues at her next observable boundary"
      (is (= :bob-work
             (get-in alice-work
                     [:expected :semantic-target])))

      (is (= (get-in alice-work
                     [:expected :runtime-target])
             (first
              (get-in alice-work
                      [:actual :runtime-targets]))))

      (is (= :preserved
             (:reason alice-work))))

    (testing "Bob's own work is checked against Bob's independently projected continuation"
      (is (= :alice-send
             (get-in bob-work
                     [:expected :semantic-target])))

      (is (= (get-in bob-work
                     [:expected :runtime-target])
             (first
              (get-in bob-work
                      [:actual :runtime-targets]))))

      (is (= :preserved
             (:reason bob-work))))))

(deftest successor-proof-rejects-a-wrong-but-structurally-valid-continuation-witness
  (let [choreography
        (all-boundary-choreography)

        original-compile-all
        project/compile-all

        result
        (with-redefs
          [project/compile-all
           (fn [verified]
             (let [compiled
                   (original-compile-all verified)

                   browser
                   (get compiled :browser)

                   wrong-locator
                   (project/semantic-continuation
                    browser
                    :prepare)]

               (assoc-in
                compiled
                [:browser
                 :semantic-continuations
                 :command]
                wrong-locator)))]

          (proof/check-projection-successors
           choreography))]

    (is (false?
         (proof/valid? result)))

    (is (= :successor-mismatch
           (:reason
            (proof/first-counterexample result))))

    (is (= [:projection-successor :prepare :owner]
           (:id
            (proof/first-counterexample result))))

    (is (= :command
           (get-in
            (proof/first-counterexample result)
            [:expected :semantic-target])))

    (is (not=
         (get-in
          (proof/first-counterexample result)
          [:expected :runtime-target])
         (first
          (get-in
           (proof/first-counterexample result)
           [:actual :runtime-targets]))))))

(deftest successor-proof-bang-form-throws-with-the-complete-counterexample-result
  (let [original-compile-all
        project/compile-all

        data
        (with-redefs
          [project/compile-all
           (fn [verified]
             (let [compiled
                   (original-compile-all verified)

                   browser
                   (get compiled :browser)

                   wrong-locator
                   (project/semantic-continuation
                    browser
                    :prepare)]

               (assoc-in
                compiled
                [:browser
                 :semantic-continuations
                 :command]
                wrong-locator)))]

          (error-data
           #(proof/check-projection-successors!
             (all-boundary-choreography))))]

    (is (= :projection-successor-proof-failed
           (:error/kind data)))

    (is (= proof/projection-successor-property
           (get-in data
                   [:result :property])))

    (is (= :successor-mismatch
           (get-in data
                   [:result :counterexample :reason])))))

(deftest proof-checker-options-fail-closed-for-both-structural-properties
  (doseq [checker
          [proof/check-projection-boundaries
           proof/check-projection-successors]]
    (let [data
          (error-data
           #(checker
             (all-boundary-choreography)
             {:verifiction-options
              {:entry-value-keys #{:request-id}}}))]

      (is (= :unknown-option-keys
             (:error/kind data)))

      (is (= #{:verifiction-options}
             (:unknown-option-keys data)))

      (is (= #{:verification-options}
             (:allowed-option-keys data))))))

(deftest successor-proof-keeps-runtime-and-trusted-assumption-claims-empty
  (let [result
        (proof/check-projection-successors
         (authoritative-observation-choreography))]

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

;; -----------------------------------------------------------------------------
;; Projection structural certificate
;; -----------------------------------------------------------------------------

(defn- historical-v1-certificate
  [current-certificate]
  (-> current-certificate
      (assoc
       :gesso.choreo/version
       proof/projection-structural-certificate-v1-version

       :properties
       proof/projection-structural-certificate-v1-properties

       :nonclaims
       proof/projection-structural-certificate-v1-nonclaims

       :valid?
       (and
        (proof/valid? (:boundary-proof current-certificate))
        (proof/valid? (:successor-proof current-certificate))))
      (dissoc :completion-proof
              :observable-origin-proof
              :runtime-origin-proof)
      (assoc :failures [])))


(defn- historical-v2-certificate
  [current-certificate]
  (-> current-certificate
      (assoc
       :gesso.choreo/version
       proof/projection-structural-certificate-v2-version

       :properties
       proof/projection-structural-certificate-v2-properties

       :nonclaims
       proof/projection-structural-certificate-v2-nonclaims

       :valid?
       (and
        (proof/valid? (:boundary-proof current-certificate))
        (proof/valid? (:successor-proof current-certificate))
        (proof/valid? (:completion-proof current-certificate))))
      (dissoc :observable-origin-proof
              :runtime-origin-proof)
      (assoc :failures [])))

(defn- historical-v3-certificate
  [current-certificate]
  (-> current-certificate
      (assoc
       :gesso.choreo/version
       proof/projection-structural-certificate-v3-version

       :properties
       proof/projection-structural-certificate-v3-properties

       :nonclaims
       proof/projection-structural-certificate-v3-nonclaims

       :valid?
       (and
        (proof/valid? (:boundary-proof current-certificate))
        (proof/valid? (:successor-proof current-certificate))
        (proof/valid? (:completion-proof current-certificate))
        (proof/valid? (:observable-origin-proof current-certificate))))
      (dissoc :runtime-origin-proof)
      (assoc :failures [])))

(deftest projection-structural-certificate-v4-is-an-explicitly-versioned-composition
  (is (= :gesso.choreo.proof/projection-structural-certificate
         proof/projection-structural-certificate-type))

  (is (= 1
         proof/projection-structural-certificate-v1-version))

  (is (= 2
         proof/projection-structural-certificate-v2-version))

  (is (= 3
         proof/projection-structural-certificate-v3-version))

  (is (= 4
         proof/projection-structural-certificate-version))

  (is (= :composed-finite-structural-certificate
         proof/projection-structural-certificate-classification))

  (is (= #{proof/projection-boundary-property
           proof/projection-successor-property}
         proof/projection-structural-certificate-v1-properties))

  (is (= #{proof/projection-boundary-property
           proof/projection-successor-property
           proof/projection-completion-property}
         proof/projection-structural-certificate-v2-properties))

  (is (= #{proof/projection-boundary-property
           proof/projection-successor-property
           proof/projection-completion-property
           proof/projection-observable-origin-property}
         proof/projection-structural-certificate-v3-properties))

  (is (= #{proof/projection-boundary-property
           proof/projection-successor-property
           proof/projection-completion-property
           proof/projection-observable-origin-property
           proof/projection-runtime-origin-property}
         proof/projection-structural-certificate-properties))

  (is (= #{:trace-refinement
           :projection-refinement}
         proof/projection-structural-certificate-v1-nonclaims))

  (is (= #{:terminal-outcome-preservation
           :trace-refinement
           :projection-refinement}
         proof/projection-structural-certificate-v2-nonclaims))

  (is (= #{:projected-step-simulation
           :terminal-outcome-preservation
           :trace-refinement
           :projection-refinement}
         proof/projection-structural-certificate-v3-nonclaims))

  (is (= proof/projection-structural-certificate-v3-nonclaims
         proof/projection-refinement-nonclaims))

  (let [certificate
        (proof/check-projection-structure
         (all-boundary-choreography))]

    (is (proof/projection-structural-certificate? certificate))
    (is (proof/structural-certificate-valid? certificate))
    (is (true? (:valid? certificate)))

    (is (= proof/projection-structural-certificate-version
           (:gesso.choreo/version certificate)))

    (is (= proof/projection-structural-certificate-properties
           (:properties certificate)))

    (is (= proof/projection-boundary-property
           (get-in certificate [:boundary-proof :property])))

    (is (= proof/projection-successor-property
           (get-in certificate [:successor-proof :property])))

    (is (= proof/projection-completion-property
           (get-in certificate [:completion-proof :property])))

    (is (= proof/projection-observable-origin-property
           (get-in certificate [:observable-origin-proof :property])))

    (is (= proof/projection-runtime-origin-property
           (get-in certificate [:runtime-origin-proof :property])))

    (is (proof/valid? (:boundary-proof certificate)))
    (is (proof/valid? (:successor-proof certificate)))
    (is (proof/valid? (:completion-proof certificate)))
    (is (proof/valid? (:observable-origin-proof certificate)))
    (is (proof/valid? (:runtime-origin-proof certificate)))))

(deftest projection-structural-certificate-v4-binds-the-exact-formal-contract-versions
  (let [certificate
        (proof/check-projection-structure
         (all-boundary-choreography))]

    (is (= verify/verification-version
           (:verification-version certificate)))

    (is (= project/executable-plan-version
           (:executable-plan-version certificate)))

    (is (= project/compiler-projection-version
           (:compiler-projection-version certificate)))

    (is (= semantics/semantics-version
           (:semantics-version certificate)))

    (is (= {:observable-kinds semantics/distributed-observation-kinds
            :hidden-kinds semantics/distributed-hidden-kinds}
           (:distributed-observation-relation certificate)))

    (is (= proof/projection-refinement-nonclaims
           (:nonclaims certificate)))

    (is (false?
         (contains? certificate :terminal-outcome-preservation)))

    (is (false?
         (contains? certificate :trace-refinement)))

    (is (false?
         (contains? certificate :projection-refinement)))))

(deftest projection-structural-certificate-v4-explanation-reports-all-five-structural-proofs
  (let [certificate
        (proof/check-projection-structure
         (all-boundary-choreography))

        explanation
        (proof/explain-structural-certificate certificate)]

    (is (= proof/projection-structural-certificate-classification
           (:classification explanation)))

    (is (true? (:valid? explanation)))

    (is (= proof/projection-structural-certificate-properties
           (:properties explanation)))

    (is (= 11
           (:boundary-obligation-count explanation)))

    (is (= 13
           (:successor-obligation-count explanation)))

    (is (= 2
           (:completion-obligation-count explanation)))

    (is (= 4
           (:observable-origin-obligation-count explanation)))

    (is (= 13
           (:runtime-origin-obligation-count explanation)))

    (is (= (count (:runtime-obligations certificate))
           (:runtime-obligation-count explanation)))

    (is (= (count (:trusted-assumptions certificate))
           (:trusted-assumption-count explanation)))

    (is (= 0
           (:failure-count explanation)))

    (is (= proof/projection-refinement-nonclaims
           (:nonclaims explanation)))

    (is (false?
         (contains? explanation :terminal-outcome-preservation)))

    (is (false?
         (contains? explanation :trace-refinement)))

    (is (false?
         (contains? explanation :projection-refinement)))))

(deftest historical-v3-structural-certificates-remain-recognizable-with-their-original-meaning
  (let [v4
        (proof/check-projection-structure
         (all-boundary-choreography))

        v3
        (historical-v3-certificate v4)

        explanation
        (proof/explain-structural-certificate v3)]

    (is (= proof/projection-structural-certificate-v3-version
           (:gesso.choreo/version v3)))

    (is (= proof/projection-structural-certificate-v3-properties
           (:properties v3)))

    (is (= proof/projection-structural-certificate-v3-nonclaims
           (:nonclaims v3)))

    (is (contains? v3 :observable-origin-proof))
    (is (false? (contains? v3 :runtime-origin-proof)))
    (is (proof/projection-structural-certificate? v3))
    (is (proof/structural-certificate-valid? v3))

    (is (= 4 (:observable-origin-obligation-count explanation)))
    (is (false? (contains? explanation :runtime-origin-obligation-count)))

    (testing "v3 remains closed to the v4 runtime-origin field and claim"
      (is (false?
           (proof/projection-structural-certificate?
            (assoc v3
                   :runtime-origin-proof
                   (:runtime-origin-proof v4)))))

      (is (false?
           (proof/projection-structural-certificate?
            (assoc v3
                   :properties
                   proof/projection-structural-certificate-properties)))))))

(deftest historical-v2-structural-certificates-remain-recognizable-with-their-original-meaning
  (let [v4
        (proof/check-projection-structure
         (all-boundary-choreography))

        v2
        (historical-v2-certificate v4)

        explanation
        (proof/explain-structural-certificate v2)]

    (is (= proof/projection-structural-certificate-v2-version
           (:gesso.choreo/version v2)))

    (is (= proof/projection-structural-certificate-v2-properties
           (:properties v2)))

    (is (= proof/projection-structural-certificate-v2-nonclaims
           (:nonclaims v2)))

    (is (false? (contains? v2 :observable-origin-proof)))
    (is (proof/projection-structural-certificate? v2))
    (is (proof/structural-certificate-valid? v2))

    (is (= 2 (:completion-obligation-count explanation)))
    (is (false? (contains? explanation :observable-origin-obligation-count)))

    (is (false?
         (proof/projection-structural-certificate?
          (assoc v2
                 :observable-origin-proof
                 (:observable-origin-proof v4)))))

    (is (false?
         (proof/projection-structural-certificate?
          (assoc v2
                 :runtime-origin-proof
                 (:runtime-origin-proof v4)))))))

(deftest historical-v1-structural-certificates-remain-recognizable-with-their-original-meaning
  (let [v4
        (proof/check-projection-structure
         (all-boundary-choreography))

        v2
        (historical-v2-certificate v4)

        v1
        (historical-v1-certificate v2)

        explanation
        (proof/explain-structural-certificate v1)]

    (is (= proof/projection-structural-certificate-v1-version
           (:gesso.choreo/version v1)))

    (is (= proof/projection-structural-certificate-v1-properties
           (:properties v1)))

    (is (= proof/projection-structural-certificate-v1-nonclaims
           (:nonclaims v1)))

    (is (false?
         (contains? v1 :completion-proof)))

    (is (proof/projection-structural-certificate? v1))
    (is (proof/structural-certificate-valid? v1))

    (is (= proof/projection-structural-certificate-v1-properties
           (:properties explanation)))

    (is (= proof/projection-structural-certificate-v1-nonclaims
           (:nonclaims explanation)))

    (is (false?
         (contains? explanation :completion-obligation-count)))

    (testing "v1 remains closed to both v2 fields and v2 claims"
      (is (false?
           (proof/projection-structural-certificate?
            (assoc v1
                   :completion-proof
                   (:completion-proof v2)))))

      (is (false?
           (proof/projection-structural-certificate?
            (assoc v1
                   :nonclaims
                   proof/projection-refinement-nonclaims)))))))

(deftest projection-structural-certificate-v4-predicate-is-closed-and-version-bound
  (let [certificate
        (proof/check-projection-structure
         (all-boundary-choreography))]

    (is (proof/projection-structural-certificate? certificate))

    (doseq [invalid
            [(assoc certificate
                    :gesso.choreo/version
                    (inc proof/projection-structural-certificate-version))

             (assoc certificate
                    :verification-version
                    (inc verify/verification-version))

             (assoc certificate
                    :executable-plan-version
                    (inc project/executable-plan-version))

             (assoc certificate
                    :compiler-projection-version
                    (inc project/compiler-projection-version))

             (assoc certificate
                    :semantics-version
                    (inc semantics/semantics-version))

             (assoc certificate
                    :distributed-observation-relation
                    {:observable-kinds #{}
                     :hidden-kinds #{}})

             (assoc certificate
                    :properties
                    proof/projection-structural-certificate-v1-properties)

             (dissoc certificate
                     :completion-proof)

             (dissoc certificate
                     :observable-origin-proof)

             (dissoc certificate
                     :runtime-origin-proof)

             (assoc certificate
                    :nonclaims
                    proof/projection-structural-certificate-v1-nonclaims)

             (assoc certificate
                    :unexpected true)]]

      (is (false?
           (proof/projection-structural-certificate? invalid)))

      (is (false?
           (proof/structural-certificate-valid? invalid))))))

(deftest projection-structural-certificate-v4-propagates-the-owning-successor-subproof-failure
  (let [original-compile-all
        project/compile-all

        certificate
        (with-redefs
          [project/compile-all
           (fn [verified]
             (let [compiled
                   (original-compile-all verified)

                   browser
                   (get compiled :browser)

                   wrong-locator
                   (project/semantic-continuation
                    browser
                    :prepare)]

               (assoc-in
                compiled
                [:browser
                 :semantic-continuations
                 :command]
                wrong-locator)))]

          (proof/check-projection-structure
           (all-boundary-choreography)))]

    (is (proof/projection-structural-certificate? certificate))
    (is (false? (proof/structural-certificate-valid? certificate)))
    (is (false? (:valid? certificate)))

    (is (proof/valid?
         (:boundary-proof certificate)))

    (is (false?
         (proof/valid?
          (:successor-proof certificate))))

    (is (proof/valid?
         (:completion-proof certificate)))

    (is (= 1
           (count (:failures certificate))))

    (is (= proof/projection-successor-property
           (get-in certificate [:failures 0 :property])))

    (is (= :successor-mismatch
           (get-in certificate
                   [:failures 0 :counterexample :reason])))

    (is (= [:projection-successor :prepare :owner]
           (get-in certificate
                   [:failures 0 :counterexample :id])))

    (is (= proof/projection-refinement-nonclaims
           (:nonclaims certificate)))))

(defn- forge-valid-wrong-completion-projection
  [compiled]
  (let [browser
        (get compiled :browser)

        wrong-locator
        (project/semantic-continuation
         browser
         :show)

        completion-locator
        (project/semantic-continuation
         browser
         :done)

        forged-source
        ::forged-completion-source]

    (-> compiled
        (assoc-in
         [:browser :semantic-continuations :done]
         wrong-locator)
        (assoc-in
         [:browser :semantic-continuations forged-source]
         completion-locator)
        (assoc-in
         [:browser :runtime-origins completion-locator]
         {:kind :synthetic-completion
          :source-semantic-state forged-source}))))

(deftest completion-corruption-fixture-remains-a-valid-compiler-projection-v3
  (let [compiled
        (project/compile-all
         (verify/verify
          (all-boundary-choreography)))

        forged
        (forge-valid-wrong-completion-projection
         compiled)]

    (is (= project/compiler-projection-version
           (:gesso.choreo/version
            (get forged :browser))))

    (is (every?
         project/compiler-projection?
         (vals forged)))

    (is (= :local
           (get-in
            forged
            [:browser
             :executable-plan
             :states
             (project/semantic-continuation
              (get forged :browser)
              :done)
             :op])))))

(defn- corrupted-completion-proof-result
  []
  (let [original-compile-all
        project/compile-all]

    (with-redefs
      [project/compile-all
       (fn [verified]
         (forge-valid-wrong-completion-projection
          (original-compile-all verified)))]

      (proof/check-projection-completions
       (all-boundary-choreography)))))

(deftest projection-structural-certificate-v4-propagates-an-isolated-completion-subproof-failure
  (let [failed-completion
        (corrupted-completion-proof-result)

        certificate
        (with-redefs
          [proof/check-projection-completions
           (fn
             ([_]
              failed-completion)
             ([_ _]
              failed-completion))]

          (proof/check-projection-structure
           (all-boundary-choreography)))]

    (is (false?
         (proof/valid? failed-completion)))

    (is (proof/projection-structural-certificate? certificate))
    (is (false? (proof/structural-certificate-valid? certificate)))

    (is (proof/valid?
         (:boundary-proof certificate)))

    (is (proof/valid?
         (:successor-proof certificate)))

    (is (false?
         (proof/valid?
          (:completion-proof certificate))))

    (is (= 1
           (count (:failures certificate))))

    (is (= proof/projection-completion-property
           (get-in certificate [:failures 0 :property])))

    (is (= :frontier-not-role-local-completion
           (get-in certificate
                   [:failures 0 :counterexample :reason])))))

(deftest projection-structural-certificate-bang-form-throws-the-complete-invalid-v4-certificate
  (let [original-compile-all
        project/compile-all

        data
        (with-redefs
          [project/compile-all
           (fn [verified]
             (let [compiled
                   (original-compile-all verified)

                   browser
                   (get compiled :browser)

                   wrong-locator
                   (project/semantic-continuation
                    browser
                    :prepare)]

               (assoc-in
                compiled
                [:browser
                 :semantic-continuations
                 :command]
                wrong-locator)))]

          (error-data
           #(proof/check-projection-structure!
             (all-boundary-choreography))))]

    (is (= :projection-structural-certificate-failed
           (:error/kind data)))

    (is (proof/projection-structural-certificate?
         (:certificate data)))

    (is (= proof/projection-structural-certificate-version
           (get-in data
                   [:certificate :gesso.choreo/version])))

    (is (contains?
         (:certificate data)
         :completion-proof))

    (is (false?
         (proof/structural-certificate-valid?
          (:certificate data))))

    (is (= proof/projection-successor-property
           (get-in data
                   [:certificate :failures 0 :property])))

    (is (= :successor-mismatch
           (get-in data
                   [:certificate :failures 0 :counterexample :reason])))))

(deftest projection-structural-certificate-options-fail-closed
  (let [data
        (error-data
         #(proof/check-projection-structure
           (all-boundary-choreography)
           {:verifiction-options
            {:entry-value-keys #{:request-id}}}))]

    (is (= :unknown-option-keys
           (:error/kind data)))

    (is (= #{:verifiction-options}
           (:unknown-option-keys data)))

    (is (= #{:verification-options}
           (:allowed-option-keys data)))))

(deftest projection-structural-certificate-v4-preserves-runtime-obligations-and-trusted-assumptions-without-proving-them
  (let [certificate
        (proof/check-projection-structure
         (authoritative-observation-choreography))

        expected-runtime-obligations
        (vec
         (distinct
          (concat
           (get-in certificate
                   [:boundary-proof :runtime-obligations])
           (get-in certificate
                   [:successor-proof :runtime-obligations])
           (get-in certificate
                   [:completion-proof :runtime-obligations])
           (get-in certificate
                   [:observable-origin-proof :runtime-obligations])
           (get-in certificate
                   [:runtime-origin-proof :runtime-obligations]))))

        expected-trusted-assumptions
        (vec
         (distinct
          (concat
           (get-in certificate
                   [:boundary-proof :trusted-assumptions])
           (get-in certificate
                   [:successor-proof :trusted-assumptions])
           (get-in certificate
                   [:completion-proof :trusted-assumptions])
           (get-in certificate
                   [:observable-origin-proof :trusted-assumptions])
           (get-in certificate
                   [:runtime-origin-proof :trusted-assumptions]))))]

    (is (proof/structural-certificate-valid? certificate))

    (is (= expected-runtime-obligations
           (:runtime-obligations certificate)))

    (is (= expected-trusted-assumptions
           (:trusted-assumptions certificate)))

    (is (= proof/projection-refinement-nonclaims
           (:nonclaims certificate)))

    (is (false?
         (contains? (:properties certificate)
                    :authoritative-basis-truth)))

    (is (false?
         (contains? (:properties certificate)
                    :terminal-outcome-preservation)))

    (is (false?
         (contains? (:properties certificate)
                    :trace-refinement)))))

;; -----------------------------------------------------------------------------
;; Exact projected-completion preservation
;; -----------------------------------------------------------------------------

(defn- multi-terminal-completion-choreography
  []
  (choreo/->choreography
   {:name :example/proof-completion-multi-terminal
    :initial :alice-send
    :states
    {:alice-send
     (choreo/communicate
      :alice
      :bob
      :example/start
      :bob-choose)

     :bob-choose
     (choreo/local
      :bob
      :bob/choose
      :decide
      {:outputs #{:choice}})

     :decide
     (choreo/branch
      :bob
      :choice
      {:confirmed :confirmed
       :rejected :rejected})

     :confirmed
     (choreo/return :confirmed)

     :rejected
     (choreo/return :rejected)}}))

(deftest completion-proof-property-is-explicitly-narrow
  (is (= :projection-completion-preservation-v1
         proof/projection-completion-property))

  (is (= :exhaustive-finite-completion-check
         proof/completion-result-classification))

  (is (contains?
       proof/structural-properties
       proof/projection-completion-property))

  (testing "current certificate v4 includes completion while historical v1 does not"
    (is (contains?
         proof/projection-structural-certificate-properties
         proof/projection-completion-property))

    (is (false?
         (contains?
          proof/projection-structural-certificate-v1-properties
          proof/projection-completion-property))))

  (let [result
        (proof/check-projection-completions
         (all-boundary-choreography))]

    (is (proof/result? result))
    (is (proof/valid? result))

    (is (= proof/projection-completion-property
           (:property result)))

    (is (= proof/completion-result-classification
           (:classification result)))

    (is (= {:kind :exact-finite-choreography-completion-structure
            :reachable-state-count 9
            :reachable-return-count 1
            :role-count 2
            :obligation-count 2}
           (:scope result)))

    (is (false?
         (contains? result :trace-refinement)))

    (is (false?
         (contains? result :projection-refinement)))))

(deftest completion-proof-checks-every-role-at-every-reachable-global-terminal
  (let [result
        (proof/check-projection-completions
         (all-boundary-choreography))

        obligations
        (:obligations result)]

    (is (proof/valid? result))

    (is (= [[:projection-completion :done :authority]
            [:projection-completion :done :browser]]
           (mapv :id obligations)))

    (is (every? :valid? obligations))

    (is (every?
         #(= :preserved (:reason %))
         obligations))

    (is (every?
         #(= {:global-outcome :done
              :local-completion
              {:op :return
               :outcome project/complete-outcome}
              :coverage
              :every-reachable-control-flow-path-to-terminal}
             (:expected %))
         obligations))

    (is (every?
         #(empty?
           (get-in % [:actual :uncovered-entry-states]))
         obligations))

    (is (every?
         #(every?
           (fn [entry]
             (= {:op :return
                 :outcome project/complete-outcome}
                (:local-state entry)))
           (get-in % [:actual :frontier]))
         obligations))))

(deftest completion-frontier-allows-a-role-to-finish-before-the-global-return
  (let [result
        (proof/check-projection-completions
         (multi-terminal-completion-choreography))

        by-id
        (into {}
              (map (juxt :id identity))
              (:obligations result))

        confirmed-alice
        (get by-id
             [:projection-completion :confirmed :alice])

        rejected-alice
        (get by-id
             [:projection-completion :rejected :alice])

        confirmed-bob
        (get by-id
             [:projection-completion :confirmed :bob])

        rejected-bob
        (get by-id
             [:projection-completion :rejected :bob])]

    (is (proof/valid? result))

    (is (= {:kind :exact-finite-choreography-completion-structure
            :reachable-state-count 5
            :reachable-return-count 2
            :role-count 2
            :obligation-count 4}
           (:scope result)))

    (testing "Alice is already locally complete before Bob performs his private choice"
      (is (= :bob-choose
             (get-in confirmed-alice
                     [:actual :frontier 0 :semantic-state])))

      (is (= :bob-choose
             (get-in rejected-alice
                     [:actual :frontier 0 :semantic-state])))

      (is (= {:op :return
              :outcome project/complete-outcome}
             (get-in confirmed-alice
                     [:actual :frontier 0 :local-state])))

      (is (= (get-in confirmed-alice
                     [:actual :frontier])
             (get-in rejected-alice
                     [:actual :frontier]))))

    (testing "Bob reaches distinct local completion continuations on the two terminal branches"
      (is (= :confirmed
             (get-in confirmed-bob
                     [:actual :frontier 0 :semantic-state])))

      (is (= :rejected
             (get-in rejected-bob
                     [:actual :frontier 0 :semantic-state]))))

    (testing "global authored outcomes remain diagnostic and are not encoded in role-local completion"
      (is (= :confirmed
             (get-in confirmed-alice
                     [:expected :global-outcome])))

      (is (= :rejected
             (get-in rejected-alice
                     [:expected :global-outcome])))

      (is (= (get-in confirmed-alice
                     [:expected :local-completion])
             (get-in rejected-alice
                     [:expected :local-completion])))

      (is (= project/complete-outcome
             (get-in confirmed-alice
                     [:expected :local-completion :outcome])))

      (is (not=
           (get-in confirmed-alice
                   [:expected :global-outcome])
           (get-in confirmed-alice
                   [:expected :local-completion :outcome]))))))

(deftest completion-proof-rejects-a-wrong-but-structurally-valid-completion-frontier
  (let [original-compile-all
        project/compile-all

        result
        (with-redefs
          [project/compile-all
           (fn [verified]
             (forge-valid-wrong-completion-projection
              (original-compile-all verified)))]

          (proof/check-projection-completions
           (all-boundary-choreography)))

        counterexample
        (proof/first-counterexample result)]

    (is (false?
         (proof/valid? result)))

    (is (= [:projection-completion :done :browser]
           (:id counterexample)))

    (is (= :frontier-not-role-local-completion
           (:reason counterexample)))

    (is (= :done
           (get-in counterexample
                   [:expected :global-outcome])))

    (is (= {:op :return
            :outcome project/complete-outcome}
           (get-in counterexample
                   [:expected :local-completion])))

    (is (= :done
           (get-in counterexample
                   [:actual :frontier 0 :semantic-state])))

    (is (= :local
           (get-in counterexample
                   [:actual :frontier 0 :local-state :op])))

    (is (= :show
           (get-in counterexample
                   [:actual :frontier 0 :local-state :action])))))

(deftest completion-proof-bang-form-throws-with-the-complete-counterexample-result
  (let [original-compile-all
        project/compile-all

        data
        (with-redefs
          [project/compile-all
           (fn [verified]
             (forge-valid-wrong-completion-projection
              (original-compile-all verified)))]

          (error-data
           #(proof/check-projection-completions!
             (all-boundary-choreography))))]

    (is (= :projection-completion-proof-failed
           (:error/kind data)))

    (is (= proof/projection-completion-property
           (get-in data [:result :property])))

    (is (= :frontier-not-role-local-completion
           (get-in data
                   [:result :counterexample :reason])))

    (is (= [:projection-completion :done :browser]
           (get-in data
                   [:result :counterexample :id])))))

(deftest completion-proof-options-fail-closed
  (let [data
        (error-data
         #(proof/check-projection-completions
           (all-boundary-choreography)
           {:verifiction-options
            {:entry-value-keys #{:request-id}}}))]

    (is (= :unknown-option-keys
           (:error/kind data)))

    (is (= #{:verifiction-options}
           (:unknown-option-keys data)))

    (is (= #{:verification-options}
           (:allowed-option-keys data)))))

(deftest completion-proof-makes-no-authoritative-outcome-or-trust-claim
  (let [result
        (proof/check-projection-completions
         (multi-terminal-completion-choreography))]

    (is (proof/valid? result))

    (is (= []
           (:runtime-obligations result)))

    (is (= []
           (:trusted-assumptions result)))

    (is (false?
         (contains? result :terminal-outcome-preservation)))

    (is (false?
         (contains? result :trace-refinement)))

    (is (false?
         (contains? result :projection-refinement)))))

;; -----------------------------------------------------------------------------
;; Converse projected observable-origin preservation
;; -----------------------------------------------------------------------------

(defn- first-receive-origin
  [compiled]
  (first
   (for [[runtime-locator origin] (project/runtime-origins compiled)
         :when (= :synthetic-receive (:kind origin))]
     [runtime-locator origin])))

(defn- forge-valid-missing-observable-origin
  [compiled-by-role]
  (let [role
        :browser

        compiled
        (get compiled-by-role role)

        [runtime-locator origin]
        (first-receive-origin compiled)

        semantic-state
        (get-in origin [:alternative-semantic-states 0 0])

        forged-semantic-state
        :forged/missing-observable-origin

        locations
        (get-in compiled [:semantic-locations semantic-state])

        forged
        (-> compiled
            (update :semantic-locations dissoc semantic-state)
            (assoc-in [:semantic-locations forged-semantic-state] locations)
            (assoc-in [:runtime-origins
                       runtime-locator
                       :alternative-semantic-states
                       0
                       0]
                      forged-semantic-state))]
    (assoc compiled-by-role role forged)))

(defn- forge-valid-observable-contract-mismatch
  [compiled-by-role]
  (let [role
        :browser

        compiled
        (get compiled-by-role role)

        [runtime-locator _]
        (first-receive-origin compiled)

        forged
        (assoc-in compiled
                  [:executable-plan
                   :states
                   runtime-locator
                   :alternatives
                   0
                   :event]
                  :forged/event)]
    (assoc compiled-by-role role forged)))

(deftest observable-origin-property-is-explicitly-converse-and-narrow
  (is (= :projection-observable-origin-preservation-v1
         proof/projection-observable-origin-property))

  (is (= :exhaustive-finite-observable-origin-check
         proof/observable-origin-result-classification))

  (is (= #{:authoritative :receive}
         proof/projected-observable-runtime-ops))

  (is (contains? proof/structural-properties
                 proof/projection-observable-origin-property))

  (is (contains? proof/projection-structural-certificate-properties
                 proof/projection-observable-origin-property)))

(deftest all-projected-observable-boundaries-have-exact-authored-origins
  (let [result
        (proof/check-projection-observable-origins
         (all-boundary-choreography))

        obligations
        (:obligations result)

        by-id
        (into {}
              (map (juxt :id identity))
              obligations)]

    (is (proof/result? result))
    (is (proof/valid? result))
    (is (= proof/projection-observable-origin-property
           (:property result)))
    (is (= proof/observable-origin-result-classification
           (:classification result)))

    (is (= {:kind :exact-finite-projected-observable-origin-structure
            :reachable-state-count 9
            :role-count 2
            :projected-observable-runtime-ops #{:authoritative :receive}
            :obligation-count 4}
           (:scope result)))

    (is (= [[:projection-observable-origin
             :authority 0 :receive 0 :command]
            [:projection-observable-origin
             :authority 1 :authoritative]
            [:projection-observable-origin
             :browser 2 :receive 0 :rejected]
            [:projection-observable-origin
             :browser 2 :receive 1 :settled]]
           (mapv :id obligations)))

    (is (every? :valid? obligations))
    (is (every? #(= :authored-observable-origin-preserved
                    (:reason %))
                obligations))

    (testing "authoritative projected observation origin is exact"
      (is (= {:kind :authored-authoritative
              :semantic-state :claim
              :role :authority
              :operation :request/claim
              :outputs #{:outcome :revision}}
             (:expected
              (get by-id
                   [:projection-observable-origin
                    :authority 1 :authoritative])))))

    (testing "participant receive origin retains the full authored communication contract"
      (is (= {:kind :authored-communication
              :semantic-state :command
              :from :browser
              :to :authority
              :event :request/claim
              :via :http
              :required #{:request-id}
              :optional #{:client-note}
              :correlation #{:request-id}
              :open-payload? false}
             (:expected
              (get by-id
                   [:projection-observable-origin
                    :authority 0 :receive 0 :command])))))

    (is (= [] (:runtime-obligations result)))
    (is (= [] (:trusted-assumptions result)))
    (is (empty? (:failures result)))
    (is (nil? (:counterexample result)))))

(deftest observable-origin-proof-preserves-intentional-many-to-one-receive-provenance
  (let [choreography
        (choreo/->choreography
         {:name :example/proof-observable-origin-collapsed-receive
          :initial :choose
          :states
          {:choose
           (choreo/branch
            :server
            :choice
            {:first :send-first
             :second :send-second})

           :send-first
           (choreo/communicate
            :server
            :browser
            :request/update
            :done
            {:required #{:request-id}
             :correlation #{:request-id}})

           :send-second
           (choreo/communicate
            :server
            :browser
            :request/update
            :done
            {:required #{:request-id}
             :correlation #{:request-id}})

           :done
           (choreo/return :done)}})

        result
        (proof/check-projection-observable-origins
         choreography
         {:verification-options
          {:entry-knowledge
           {:server #{:choice :request-id}}}})

        browser-obligations
        (filterv #(= :browser (:role %))
                 (:obligations result))]

    (is (proof/valid? result))
    (is (= 2 (count browser-obligations)))

    (is (= #{:send-first :send-second}
           (set (map :state browser-obligations))))

    (is (= #{0}
           (set (map :runtime-locator browser-obligations))))

    (is (= #{0}
           (set (map :alternative-index browser-obligations))))

    (is (every? #(= :authored-observable-origin-preserved
                    (:reason %))
                browser-obligations))

    (is (apply =
               (map #(dissoc (:expected %) :semantic-state)
                    browser-obligations)))))

(deftest observable-origin-proof-rejects-valid-compiler-provenance-pointing-outside-the-choreography
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-missing-observable-origin
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))]

    (is (every? project/compiler-projection?
                (vals forged-projections)))

    (let [result
          (with-redefs
            [project/compile-all
             (fn [_]
               forged-projections)]
            (proof/check-projection-observable-origins
             (all-boundary-choreography)))

          counterexample
          (:counterexample result)]

      (is (proof/result? result))
      (is (false? (proof/valid? result)))
      (is (= :missing-authored-semantic-state
             (:reason counterexample)))
      (is (= :forged/missing-observable-origin
             (:state counterexample)))
      (is (= :browser (:role counterexample)))
      (is (= :receiver (:endpoint counterexample))))))

(deftest observable-origin-proof-rejects-valid-compiler-contract-forgery
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-observable-contract-mismatch
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))]

    (is (every? project/compiler-projection?
                (vals forged-projections)))

    (let [result
          (with-redefs
            [project/compile-all
             (fn [_]
               forged-projections)]
            (proof/check-projection-observable-origins
             (all-boundary-choreography)))

          counterexample
          (:counterexample result)]

      (is (false? (proof/valid? result)))
      (is (= :authored-communication-contract-mismatch
             (:reason counterexample)))
      (is (= :forged/event
             (get-in counterexample [:actual :event])))
      (is (= :request/rejected
             (get-in counterexample [:expected :event])))
      (is (= :rejected (:state counterexample))))))

(deftest observable-origin-proof-bang-form-throws-with-complete-counterexample
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-missing-observable-origin
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))

        data
        (with-redefs
          [project/compile-all
           (fn [_]
             forged-projections)]
          (error-data
           #(proof/check-projection-observable-origins!
             (all-boundary-choreography))))]

    (is (= :projection-observable-origin-proof-failed
           (:error/kind data)))

    (is (= proof/projection-observable-origin-property
           (get-in data [:result :property])))

    (is (= :missing-authored-semantic-state
           (get-in data [:result :counterexample :reason])))))

(deftest observable-origin-proof-options-fail-closed
  (let [data
        (error-data
         #(proof/check-projection-observable-origins
           (all-boundary-choreography)
           {:verifiction-options
            {:entry-value-keys #{:request-id}}}))]

    (is (= :unknown-option-keys
           (:error/kind data)))

    (is (= #{:verifiction-options}
           (:unknown-option-keys data)))

    (is (= #{:verification-options}
           (:allowed-option-keys data)))))

(deftest observable-origin-proof-does-not-overclaim-dynamic-refinement
  (let [result
        (proof/check-projection-observable-origins
         (all-boundary-choreography))

        certificate
        (proof/check-projection-structure
         (all-boundary-choreography))]

    (is (proof/valid? result))
    (is (= [] (:runtime-obligations result)))
    (is (= [] (:trusted-assumptions result)))

    (doseq [claim [:all-projected-executions
                   :sender-occurrence-delivery-justification
                   :terminal-outcome-preservation
                   :trace-refinement
                   :projection-refinement]]
      (is (false? (contains? result claim))))

    (testing "the v4 structural certificate composes both stabilized origin properties without claiming dynamic simulation"
      (is (= 4 (:gesso.choreo/version certificate)))
      (is (proof/structural-certificate-valid? certificate))
      (is (= proof/projection-structural-certificate-properties
             (:properties certificate)))
      (is (contains? (:properties certificate)
                     proof/projection-observable-origin-property))
      (is (contains? (:properties certificate)
                     proof/projection-runtime-origin-property))
      (is (= proof/projection-refinement-nonclaims
             (:nonclaims certificate)))
      (is (contains? (:nonclaims certificate)
                     :projected-step-simulation)))))



;; -----------------------------------------------------------------------------
;; Converse projected runtime-origin preservation
;; -----------------------------------------------------------------------------

(deftest runtime-origin-property-is-explicitly-converse-and-exhaustive
  (is (= :projection-runtime-origin-preservation-v1
         proof/projection-runtime-origin-property))

  (is (= :exhaustive-finite-runtime-origin-check
         proof/runtime-origin-result-classification))

  (is (= #{:local
           :authoritative
           :branch
           :await
           :send
           :receive
           :return}
         proof/projected-runtime-ops))

  (is (contains? proof/structural-properties
                 proof/projection-runtime-origin-property))

  (is (contains? proof/projection-structural-certificate-properties
                 proof/projection-runtime-origin-property))

  (is (false?
       (contains? proof/projection-structural-certificate-v3-properties
                  proof/projection-runtime-origin-property))))

(deftest every-emitted-runtime-state-has-exact-semantic-origin-justification
  (let [result
        (proof/check-projection-runtime-origins
         (all-boundary-choreography))

        obligations
        (:obligations result)]

    (is (proof/result? result))
    (is (proof/valid? result))
    (is (= proof/projection-runtime-origin-property
           (:property result)))
    (is (= proof/runtime-origin-result-classification
           (:classification result)))

    (is (= {:kind :exact-finite-projected-runtime-origin-structure
            :reachable-state-count 9
            :role-count 2
            :projected-runtime-ops proof/projected-runtime-ops
            :obligation-count 13}
           (:scope result)))

    (is (= [[:projection-runtime-origin :authority 0 :synthetic-receive]
            [:projection-runtime-origin :authority 1 :authored-boundary]
            [:projection-runtime-origin :authority 2 :authored-boundary]
            [:projection-runtime-origin :authority 3 :authored-boundary]
            [:projection-runtime-origin :authority 4 :authored-boundary]
            [:projection-runtime-origin :authority 5 :synthetic-completion]
            [:projection-runtime-origin :authority 6 :synthetic-completion]
            [:projection-runtime-origin :browser 0 :authored-boundary]
            [:projection-runtime-origin :browser 1 :authored-boundary]
            [:projection-runtime-origin :browser 2 :synthetic-receive]
            [:projection-runtime-origin :browser 3 :synthetic-completion]
            [:projection-runtime-origin :browser 4 :authored-boundary]
            [:projection-runtime-origin :browser 5 :authored-boundary]]
           (mapv :id obligations)))

    (is (= {:authored-runtime-origin-preserved 8
            :synthetic-receive-origin-preserved 2
            :synthetic-completion-origin-preserved 3}
           (frequencies (map :reason obligations))))

    (is (every? :valid? obligations))
    (is (every? #(= :runtime-state (:endpoint %)) obligations))
    (is (every? nat-int? (map :runtime-locator obligations)))
    (is (= [] (:runtime-obligations result)))
    (is (= [] (:trusted-assumptions result)))
    (is (empty? (:failures result)))
    (is (nil? (:counterexample result)))))

(deftest runtime-origin-proof-rejects-valid-receive-provenance-pointing-outside-the-choreography
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-missing-observable-origin
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))]

    (is (every? project/compiler-projection?
                (vals forged-projections)))

    (let [result
          (with-redefs
            [project/compile-all
             (fn [_]
               forged-projections)]
            (proof/check-projection-runtime-origins
             (all-boundary-choreography)))

          counterexample
          (:counterexample result)]

      (is (proof/result? result))
      (is (false? (proof/valid? result)))
      (is (= proof/projection-runtime-origin-property
             (:property result)))
      (is (= :missing-authored-semantic-state
             (:reason counterexample)))
      (is (= :browser (:role counterexample)))
      (is (= :claim (:state counterexample)))
      (is (= :synthetic-receive
             (last (:id counterexample)))))))

(deftest runtime-origin-proof-rejects-valid-forged-completion-source
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-wrong-completion-projection
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))]

    (is (every? project/compiler-projection?
                (vals forged-projections)))

    (let [result
          (with-redefs
            [project/compile-all
             (fn [_]
               forged-projections)]
            (proof/check-projection-runtime-origins
             (all-boundary-choreography)))

          counterexample
          (:counterexample result)]

      (is (false? (proof/valid? result)))
      (is (= :missing-completion-source-semantic-state
             (:reason counterexample)))
      (is (= :browser (:role counterexample)))
      (is (= :synthetic-completion
             (last (:id counterexample)))))))

(deftest runtime-origin-proof-bang-form-throws-with-complete-counterexample
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-wrong-completion-projection
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))

        data
        (with-redefs
          [project/compile-all
           (fn [_]
             forged-projections)]
          (error-data
           #(proof/check-projection-runtime-origins!
             (all-boundary-choreography))))]

    (is (= :projection-runtime-origin-proof-failed
           (:error/kind data)))

    (is (= proof/projection-runtime-origin-property
           (get-in data [:result :property])))

    (is (= :missing-completion-source-semantic-state
           (get-in data [:result :counterexample :reason])))))

(deftest runtime-origin-proof-options-fail-closed
  (let [data
        (error-data
         #(proof/check-projection-runtime-origins
           (all-boundary-choreography)
           {:verifiction-options
            {:entry-value-keys #{:request-id}}}))]

    (is (= :unknown-option-keys
           (:error/kind data)))

    (is (= #{:verifiction-options}
           (:unknown-option-keys data)))

    (is (= #{:verification-options}
           (:allowed-option-keys data)))))

(deftest projection-structural-certificate-v4-propagates-an-isolated-runtime-origin-subproof-failure
  (let [original-compile-all
        project/compile-all

        forged-projections
        (forge-valid-wrong-completion-projection
         (original-compile-all
          (verify/ensure-verified
           (all-boundary-choreography))))

        failed-runtime-origin
        (with-redefs
          [project/compile-all
           (fn [_]
             forged-projections)]
          (proof/check-projection-runtime-origins
           (all-boundary-choreography)))

        certificate
        (with-redefs
          [proof/check-projection-runtime-origins
           (fn
             ([_]
              failed-runtime-origin)
             ([_ _]
              failed-runtime-origin))]
          (proof/check-projection-structure
           (all-boundary-choreography)))]

    (is (false? (proof/valid? failed-runtime-origin)))
    (is (proof/projection-structural-certificate? certificate))
    (is (false? (proof/structural-certificate-valid? certificate)))

    (is (proof/valid? (:boundary-proof certificate)))
    (is (proof/valid? (:successor-proof certificate)))
    (is (proof/valid? (:completion-proof certificate)))
    (is (proof/valid? (:observable-origin-proof certificate)))
    (is (false? (proof/valid? (:runtime-origin-proof certificate))))

    (is (= 1 (count (:failures certificate))))
    (is (= proof/projection-runtime-origin-property
           (get-in certificate [:failures 0 :property])))
    (is (= :missing-completion-source-semantic-state
           (get-in certificate
                   [:failures 0 :counterexample :reason])))))

(deftest runtime-origin-proof-and-v4-certificate-remain-honest-about-dynamic-refinement
  (let [result
        (proof/check-projection-runtime-origins
         (all-boundary-choreography))

        certificate
        (proof/check-projection-structure
         (all-boundary-choreography))]

    (is (proof/valid? result))
    (is (= [] (:runtime-obligations result)))
    (is (= [] (:trusted-assumptions result)))

    (doseq [claim [:all-projected-executions
                   :projected-step-simulation
                   :sender-occurrence-delivery-justification
                   :terminal-outcome-preservation
                   :trace-refinement
                   :projection-refinement]]
      (is (false? (contains? result claim))))

    (is (= proof/projection-structural-certificate-version
           (:gesso.choreo/version certificate)))
    (is (proof/structural-certificate-valid? certificate))
    (is (contains? (:properties certificate)
                   proof/projection-runtime-origin-property))
    (is (= proof/projection-refinement-nonclaims
           (:nonclaims certificate)))
    (is (contains? (:nonclaims certificate)
                   :projected-step-simulation))))
