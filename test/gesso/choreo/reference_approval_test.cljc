(ns gesso.choreo.reference-approval-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.correspondence :as correspondence]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.realization :as realization]
   [gesso.choreo.verify :as verify]))

(def approval-observation
  {:authority :approval/model
   :observation :approval/current
   :basis-key :observed-basis})

(def approval-basis
  {:revision 7
   :tx-id "tx-approval-7"})

(defn- approval-choreography
  []
  (choreo/->choreography
   {:name :reference/approval
    :initial :prepare
    :states
    {:prepare
     (choreo/local
      :approver
      :approval/prepare
      :submit
      {:outputs #{:request-id :decision}})

     :submit
     (choreo/communicate
      :approver
      :server
      :approval/submit
      :record
      {:via :http
       :required #{:request-id :decision}})

     :record
     (choreo/authoritative
      :server
      :approval/record
      :observe
      {:requires #{:request-id :decision}
       :outputs #{:approval-status :approval-version}})

     :observe
     (choreo/await
      :reviewer
      {:approval/reread-complete :route}
      {:event-contracts
       {:approval/reread-complete
        {:required #{:approval-status
                     :approval-version
                     :observed-basis}
         :optional #{:display-summary}
         :open-data? true
         :authoritative-observation
         approval-observation}}})

     :route
     (choreo/branch
      :reviewer
      :approval-status
      {:approved :show-approved
       :rejected :show-rejected})

     :show-approved
     (choreo/local
      :reviewer
      :approval/show-approved
      :done
      {:requires #{:approval-version :observed-basis}})

     :show-rejected
     (choreo/local
      :reviewer
      :approval/show-rejected
      :done
      {:requires #{:approval-version :observed-basis}})

     :done
     (choreo/return :done)}}))

(defn- approval-witness
  []
  [{:op :local
    :role :approver
    :action :approval/prepare
    :outputs {:request-id 17
              :decision :approve}}

   {:op :send
    :role :approver
    :to :server
    :event :approval/submit
    :via :http
    :payload {:request-id 17
              :decision :approve}
    :as :approval-command}

   {:op :deliver
    :message :approval-command}

   {:op :authoritative
    :role :server
    :operation :approval/record
    :outputs {:approval-status :approved
              :approval-version 7}}

   {:op :environment
    :role :reviewer
    :event :approval/reread-complete
    :data {:approval-status :approved
           :approval-version 7
           :observed-basis approval-basis}}

   {:op :local
    :role :reviewer
    :action :approval/show-approved
    :outputs {}}])

(deftest approval-reference-verifies-reread-as-authoritative-knowledge-path
  (let [result
        (verify/verify
         (approval-choreography))

        analysis
        (:analysis result)]

    (is (:valid? result))

    (testing "the approval mutation and the later authoritative observation stay distinct"
      (is (= {:record :approval/record}
             (:authoritative-operations-by-state analysis)))
      (is (= approval-observation
             (get-in analysis
                     [:authoritative-observations-by-state
                      :observe
                      :approval/reread-complete]))))

    (testing "the reread gives only the reviewer definite authoritative knowledge"
      (is (= #{:approval-status
               :approval-version
               :observed-basis}
             (get-in analysis
                     [:definitely-authoritatively-known-before-state
                      :route
                      :reviewer])))
      (is (not
           (contains?
            (get-in analysis
                    [:definitely-authoritatively-known-before-state
                     :route
                     :reviewer])
            :display-summary))))))

(deftest reviewer-projection-uses-authoritative-observation-not-a-synthetic-message
  (let [plans
        (project/project-all
         (approval-choreography))

        reviewer
        (:reviewer plans)

        ops
        (mapv :op
              (vals (:states reviewer)))]

    (is (= :await
           (:op
            (project/state reviewer
                           (:initial reviewer)))))

    (is (some #{:branch} ops))
    (is (some #{:local} ops))
    (is (some #{:return} ops))

    (testing "nothing in the reviewer projection fabricates server-to-reviewer transport"
      (is (not-any? #{:send :receive} ops)))))

(deftest approval-realization-learns-durable-state-through-reread
  (let [started
        (realization/start
         (approval-choreography))

        reviewer-before
        (realization/execution started :reviewer)

        after-prepare
        (realization/complete-local
         started
         :approver
         {:request-id 17
          :decision :approve})

        {after-submit :realization
         command-id :message-id}
        (realization/complete-send
         after-prepare
         :approver
         {:request-id 17
          :decision :approve})

        after-command
        (realization/deliver-message
         after-submit
         command-id)

        after-record
        (realization/complete-authoritative
         after-command
         :server
         {:approval-status :approved
          :approval-version 7})

        after-reread
        (realization/environment
         after-record
         :reviewer
         :approval/reread-complete
         {:approval-status :approved
          :approval-version 7
          :observed-basis approval-basis
          :display-summary "Approved"
          :xhr-object :host-only})

        reviewer-after
        (realization/execution after-reread :reviewer)

        completed
        (realization/complete-local
         after-reread
         :reviewer
         {})]

    (testing "the independent reviewer may already be suspended on the observation boundary"
      (is (machine/waiting-environment? reviewer-before))
      (is (false?
           (machine/has-execution-value?
            reviewer-before
            :approval-status))))

    (testing "the only participant message is the approver command to the server"
      (is (= []
             (realization/messages after-command)))
      (is (= []
             (realization/messages after-record)))
      (is (= []
             (realization/messages after-reread))))

    (testing "the reread establishes current durable approval as authoritative reviewer knowledge"
      (is (machine/waiting-local? reviewer-after))
      (is (= :approved
             (machine/execution-value
              reviewer-after
              :approval-status)))
      (is (= 7
             (machine/execution-value
              reviewer-after
              :approval-version)))
      (is (= approval-basis
             (machine/execution-value
              reviewer-after
              :observed-basis)))
      (is (= #{:authoritative}
             (machine/execution-provenance-kinds
              reviewer-after
              :approval-status)))
      (is (= #{:authoritative}
             (machine/execution-provenance-kinds
              reviewer-after
              :observed-basis))))

    (testing "optional declared semantic data may enter knowledge but open host extras may not"
      (is (= "Approved"
             (machine/execution-value
              reviewer-after
              :display-summary)))
      (is (false?
           (machine/has-execution-value?
            reviewer-after
            :xhr-object))))

    (is (realization/completed? completed))))

(deftest approval-reference-has-explicit-static-observation-proof-obligation
  (let [result
        (proof/check-projection-boundaries
         (approval-choreography))

        observations
        (filterv
         #(= :authoritative-observation
             (:endpoint %))
         (:obligations result))

        observation
        (first observations)]

    (is (proof/valid? result))
    (is (= 1 (count observations)))
    (is (= :reviewer (:role observation)))
    (is (nat-int? (:runtime-locator observation)))
    (is (= :preserved (:reason observation)))
    (is (= {:event :approval/reread-complete
            :authority :approval/model
            :observation :approval/current
            :basis-key :observed-basis
            :required #{:approval-status
                        :approval-version
                        :observed-basis}
            :optional #{:display-summary}
            :semantic-keys #{:approval-status
                             :approval-version
                             :observed-basis
                             :display-summary}
            :open-data? true}
           (:expected observation)))
    (is (= (:expected observation)
           (:actual observation)))))

(deftest approval-reference-corresponds-without-observer-message
  (let [result
        (correspondence/check-witness
         (approval-choreography)
         (approval-witness)
         {:require-complete? true})

        observation
        (first
         (filter
          #(= :authoritative-observation-correspondence
              (:kind %))
          (:obligations result)))]

    (is (correspondence/valid? result))
    (is (true? (:global-completed? result)))
    (is (true? (:realization-completed? result)))

    (testing "the distributed trace contains the command communication and durable mutation, not the reread"
      (is (= [{:kind :communication
               :from :approver
               :to :server
               :event :approval/submit
               :payload {:request-id 17
                         :decision :approve}
               :via :http}
              {:kind :authoritative
               :role :server
               :operation :approval/record
               :outputs {:approval-status :approved
                         :approval-version 7}}]
             (:semantic-trace result)))
      (is (= (:semantic-trace result)
             (:realization-trace result))))

    (testing "the local reread is nevertheless an explicit checked correspondence obligation"
      (is (some? observation))
      (is (true? (:valid? observation)))
      (is (= approval-basis
             (:basis observation)))
      (is (= #{:approval-status
               :approval-version
               :observed-basis}
             (:checked-keys observation))))))

(deftest approval-reference-classifies-authoritative-basis-progression-boundary
  (let [proof-result
        (proof/check-projection-boundaries
         (approval-choreography))

        explanation
        (proof/explain proof-result)

        runtime-obligation
        (first (:runtime-obligations proof-result))

        trusted-assumption
        (first (:trusted-assumptions proof-result))]

    (testing "projection preservation is structurally proved"
      (is (proof/valid? proof-result))
      (is (= :exhaustive-finite-structural-check
             (:classification explanation)))
      (is (= 0 (:failure-count explanation))))

    (testing "advancing an existing authoritative basis is a separate runtime obligation"
      (is (= 1 (:runtime-obligation-count explanation)))
      (is (= {:id [:authoritative-basis-progression
                   :observe
                   :approval/reread-complete]
              :property :authoritative-basis-progression
              :classification :runtime-enforced
              :state :observe
              :role :reviewer
              :event :approval/reread-complete
              :authority :approval/model
              :observation :approval/current
              :basis-key :observed-basis
              :when :advancing-distinct-existing-authoritative-basis
              :requires #{:explicit-progression-witness
                          :exact-authority-match
                          :exact-observation-match
                          :exact-from-basis-match
                          :exact-to-basis-match
                          :advances-relation}}
             runtime-obligation)))

    (testing "authority-specific ordering truth remains an explicit trusted assumption"
      (is (= 1 (:trusted-assumption-count explanation)))
      (is (= {:id [:authoritative-basis-ordering
                   :observe
                   :approval/reread-complete]
              :property :authoritative-basis-ordering
              :classification :trusted
              :state :observe
              :role :reviewer
              :event :approval/reread-complete
              :authority :approval/model
              :observation :approval/current
              :basis-key :observed-basis
              :assumption
              :advances-witness-truth-is-supplied-by-trusted-authority}
             trusted-assumption)))

    (testing "a green structural proof does not silently discharge runtime or trust obligations"
      (is (not (contains? runtime-obligation :valid?)))
      (is (not (contains? trusted-assumption :valid?)))
      (is (not (contains? trusted-assumption :proved?)))
      (doseq [unsupported
              [:basis-progression-proof
               :freshness-proof
               :authority-authenticity-proof
               :authorization-proof]]
        (is (false?
             (contains? proof-result unsupported)))))))
