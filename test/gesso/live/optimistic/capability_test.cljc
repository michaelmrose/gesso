(ns gesso.live.optimistic.capability-test
  (:require
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.live.optimistic.capability :as capability]
   #?(:clj [clojure.test :refer [deftest is testing]]
      :cljs [cljs.test :refer-macros [deftest is testing]])
   #?(:clj [gesso.live.ui :as ui])))

(def basis
  {:tx-id 42
   :system-time "2026-08-26T17:00:00Z"})

(def fact-versions
  {:request/status 7
   :request/claim "claim-v7"})

(def full-capability-options
  {:operation :request/claim
   :plan-key :request/claim
   :rollback-eligible? true
   :timeout-ms 5000
   :replace-owner? false
   :replace-execution? true})

(def full-binding
  {:arguments {:request-id "request-1"
               :helper-id "helper-1"}
   :observed-basis basis
   :scope [:request "request-1"]
   :fact-versions fact-versions
   :target-id "request-request-1"})

(defn- one-role-plan
  [operation]
  (project/project
   (choreo/->choreography
    {:name :example/optimistic-capability-plan
     :initial :run
     :states
     {:run
      (choreo/local :browser operation :done)

      :done
      (choreo/return :done)}})
   :browser))

(def browser-plans
  {:request/claim
   (one-role-plan :request/claim)

   :request/cancel
   (one-role-plan :request/cancel)})

(def derived-policy
  {:request/claim
   {:rollback-eligible? true
    :timeout-ms 5000
    :replace-owner? false
    :replace-execution? true}})

(def authority-or-correlation-keys
  #{:principal
    :authority
    :authorities
    :role
    :roles
    :command-id
    :execution-id
    :settlement
    :resolution
    :authoritative})

(defn- error-data
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Throwable
              :cljs :default) ex
      (ex-data ex))))

(defn- error-kind
  [f]
  (:error/kind (error-data f)))

(deftest operation-capability-construction-is-canonical-and-closed
  (let [minimal (capability/operation-capability
                 {:operation :request/claim})
        full (capability/operation-capability
              full-capability-options)]
    (testing "minimal capabilities default plan selection to the semantic operation"
      (is (= {capability/capability-type-key
              capability/operation-capability-type
              :operation :request/claim
              :plan-key :request/claim}
             minimal))
      (is (true? (capability/operation-capability? minimal))))

    (testing "configured browser policy is retained exactly"
      (is (= (merge
              {capability/capability-type-key
               capability/operation-capability-type}
              full-capability-options)
             full))
      (is (true? (capability/operation-capability? full))))

    (testing "capability construction rejects unknown fields"
      (let [data (error-data
                  #(capability/operation-capability
                    {:operation :request/claim
                     :principal :forged}))]
        (is (= :unknown-fields (:error/kind data)))
        (is (= #{:principal} (:unknown data)))))

    (testing "capability construction requires a semantic operation"
      (is (= :missing-fields
             (error-kind
              #(capability/operation-capability {})))))))

(deftest operation-capability-validates-policy-value-kinds
  (testing "semantic operation and explicit plan keys are keywords"
    (is (= :invalid-keyword
           (error-kind
            #(capability/operation-capability
              {:operation "request/claim"}))))
    (is (= :invalid-keyword
           (error-kind
            #(capability/operation-capability
              {:operation :request/claim
               :plan-key "request/claim"})))))

  (testing "boolean policy cannot be smuggled through truthy values"
    (doseq [key [:rollback-eligible?
                 :replace-owner?
                 :replace-execution?]]
      (is (= :invalid-boolean
             (error-kind
              #(capability/operation-capability
                {:operation :request/claim
                 key :yes}))))))

  (testing "timeouts are non-negative integers"
    (doseq [timeout [-1 1.5 "5000" nil]]
      (is (= :invalid-timeout
             (error-kind
              #(capability/operation-capability
                {:operation :request/claim
                 :timeout-ms timeout})))))
    (is (= 0
           (:timeout-ms
            (capability/operation-capability
             {:operation :request/claim
              :timeout-ms 0}))))))

(deftest canonical-capability-predicate-rejects-tampering-and-lookalikes
  (let [canonical (capability/operation-capability
                   full-capability-options)]
    (is (true? (capability/operation-capability? canonical)))
    (doseq [candidate
            [nil
             []
             {}
             (dissoc canonical capability/capability-type-key)
             (assoc canonical capability/capability-type-key :other/type)
             (assoc canonical :operation "request/claim")
             (assoc canonical :timeout-ms -1)
             (assoc canonical :principal :forged)]]
      (is (false? (capability/operation-capability? candidate))))

    (is (= canonical
           (capability/require-operation-capability canonical)))
    (is (= :invalid-capability
           (error-kind
            #(capability/require-operation-capability
              (assoc canonical :principal :forged)))))))

(deftest operation-capabilities-derive-one-capability-per-canonical-browser-plan
  (let [capabilities
        (capability/operation-capabilities browser-plans)]
    (is (= (set (keys browser-plans))
           (set (keys capabilities))))
    (is (true?
         (capability/operation-capabilities? capabilities)))
    (is (= capabilities
           (capability/require-operation-capabilities capabilities)))
    (doseq [operation (keys browser-plans)]
      (let [derived (get capabilities operation)]
        (is (capability/operation-capability? derived))
        (is (= operation (:operation derived)))
        (is (= operation (:plan-key derived)))))

    (testing "the derived map does not copy executable plans into view capabilities"
      (is (every?
           #(empty? (set/intersection
                     (set (keys %))
                     #{:plan
                       :executable-plan
                       :states
                       :browser-role
                       :transport}))
           (vals capabilities))))))

(deftest operation-capabilities-preserve-only-explicit-per-operation-browser-policy
  (let [capabilities
        (capability/operation-capabilities
         browser-plans
         derived-policy)
        claim
        (get capabilities :request/claim)
        cancel
        (get capabilities :request/cancel)]
    (is (= :request/claim (:operation claim)))
    (is (= :request/claim (:plan-key claim)))
    (is (= true (:rollback-eligible? claim)))
    (is (= 5000 (:timeout-ms claim)))
    (is (= false (:replace-owner? claim)))
    (is (= true (:replace-execution? claim)))

    (testing "operations without explicit policy remain minimal"
      (is (= {capability/capability-type-key
              capability/operation-capability-type
              :operation :request/cancel
              :plan-key :request/cancel}
             cancel)))

    (testing "derived policy cannot reintroduce an independently declared plan key"
      (let [data
            (error-data
             #(capability/operation-capabilities
               browser-plans
               {:request/claim
                {:plan-key :request/cancel}}))]
        (is (= :unknown-fields (:error/kind data)))
        (is (= #{:plan-key} (:unknown data)))))))

(deftest operation-capabilities-require-a-nonempty-operation-keyed-canonical-plan-map
  (testing "the registry itself is required and nonempty"
    (is (= :invalid-shape
           (error-kind
            #(capability/operation-capabilities nil))))
    (is (= :empty-browser-plans
           (error-kind
            #(capability/operation-capabilities {})))))

  (testing "semantic operation ids are keywords"
    (let [data
          (error-data
           #(capability/operation-capabilities
             {"request/claim"
              (get browser-plans :request/claim)}))]
      (is (= :invalid-keyword (:error/kind data)))
      (is (= "request/claim" (:value data)))))

  (testing "plan values must be canonical current ExecutablePlans, not tagged lookalikes"
    (let [plan
          (get browser-plans :request/claim)
          stale
          (assoc plan
                 :gesso.choreo/version
                 (inc project/executable-plan-version))
          malformed
          (dissoc plan :states)]
      (doseq [candidate [nil {} stale malformed]]
        (let [data
              (error-data
               #(capability/operation-capabilities
                 {:request/claim candidate}))]
          (is (= :invalid-browser-plan (:error/kind data)))
          (is (= :request/claim (:operation data)))
          (is (= candidate (:plan data))))))))

(deftest operation-capabilities-policy-is-closed-over-the-plan-operation-set
  (testing "policy cannot name an operation absent from the canonical plan map"
    (let [data
          (error-data
           #(capability/operation-capabilities
             browser-plans
             {:request/unclaim
              {:timeout-ms 10}}))]
      (is (= :unknown-policy-operations (:error/kind data)))
      (is (= #{:request/unclaim}
             (:unknown-operations data)))
      (is (= #{:request/claim :request/cancel}
             (:available-operations data)))))

  (testing "each policy value is itself a closed map"
    (is (= :invalid-shape
           (error-kind
            #(capability/operation-capabilities
              browser-plans
              {:request/claim :fast}))))
    (let [data
          (error-data
           #(capability/operation-capabilities
             browser-plans
             {:request/claim
              {:principal :forged}}))]
      (is (= :unknown-fields (:error/kind data)))
      (is (= #{:principal} (:unknown data))))))

(deftest operation-capabilities-predicate-establishes-local-shape-only
  (let [derived
        (capability/operation-capabilities browser-plans)
        claim
        (get derived :request/claim)]
    (is (true? (capability/operation-capabilities? derived)))

    (doseq [candidate
            [nil
             []
             {}
             {:request/claim
              (assoc claim :operation :request/cancel)}
             {:request/claim
              (assoc claim :plan-key :request/cancel)}
             {:request/claim
              (assoc claim :principal :forged)}
             {"request/claim" claim}]]
      (is (false?
           (capability/operation-capabilities? candidate))))

    (testing "a locally canonical map is accepted without pretending to reconstruct plan provenance"
      (let [local-shape-only
            {:request/claim
             (capability/operation-capability
              {:operation :request/claim})}]
        (is (true?
             (capability/operation-capabilities? local-shape-only)))
        (is (= local-shape-only
               (capability/require-operation-capabilities
                local-shape-only)))))))

(deftest capability-for-operation-is-exact-and-fails-closed
  (let [capabilities
        (capability/operation-capabilities browser-plans)]
    (is (= (get capabilities :request/claim)
           (capability/capability-for-operation
            capabilities
            :request/claim)))

    (testing "unknown semantic ids are never guessed or silently substituted"
      (let [data
            (error-data
             #(capability/capability-for-operation
               capabilities
               :request/claime))]
        (is (= :unknown-operation (:error/kind data)))
        (is (= :request/claime (:operation data)))
        (is (= #{:request/claim :request/cancel}
               (:available-operations data)))))

    (testing "lookup requires a keyword semantic operation"
      (is (= :invalid-keyword
             (error-kind
              #(capability/capability-for-operation
                capabilities
                "request/claim")))))

    (testing "lookup rejects a noncanonical capability registry before resolving from it"
      (is (= :invalid-operation-capabilities
             (error-kind
              #(capability/capability-for-operation
                {:request/claim
                 (assoc
                  (get capabilities :request/claim)
                  :plan-key :request/cancel)}
                :request/claim)))))))

(deftest bind-produces-the-existing-inert-v3-action-shape
  (let [cap (capability/operation-capability
             full-capability-options)
        action (capability/bind cap full-binding)]
    (is (= {:operation :request/claim
            :arguments {:request-id "request-1"
                        :helper-id "helper-1"}
            :observed-basis basis
            :plan-key :request/claim
            :rollback-eligible? true
            :timeout-ms 5000
            :replace-owner? false
            :replace-execution? true
            :scope [:request "request-1"]
            :fact-versions fact-versions
            :target-id "request-request-1"}
           action))
    (is (= basis (:observed-basis action)))
    (is (= [:request "request-1"] (:scope action)))
    (is (= fact-versions (:fact-versions action)))
    (is (not (contains? action capability/capability-type-key)))))

(deftest bind-keeps-capability-policy-out-of-per-render-control
  (let [cap (capability/operation-capability
             full-capability-options)]
    (doseq [[key value]
            [[:operation :request/unclaim]
             [:plan-key :request/unclaim]
             [:rollback-eligible? false]
             [:timeout-ms 1]
             [:replace-owner? true]
             [:replace-execution? false]]]
      (let [data (error-data
                  #(capability/bind
                    cap
                    (assoc full-binding key value)))]
        (is (= :unknown-fields (:error/kind data)))
        (is (= #{key} (:unknown data)))))))

(deftest bind-is-closed-and-requires-only-render-varying-inputs
  (let [cap (capability/operation-capability
             {:operation :request/claim})]
    (testing "required binding fields are explicit"
      (doseq [binding
              [(dissoc full-binding :arguments)
               (dissoc full-binding :observed-basis)]]
        (is (= :missing-fields
               (error-kind #(capability/bind cap binding))))))

    (testing "unknown per-render data is rejected"
      (let [data (error-data
                  #(capability/bind
                    cap
                    (assoc full-binding :principal :forged)))]
        (is (= :unknown-fields (:error/kind data)))
        (is (= #{:principal} (:unknown data)))))

    (testing "arguments remain portable operation data, not arbitrary shapes"
      (is (= :invalid-arguments
             (error-kind
              #(capability/bind
                cap
                (assoc full-binding
                       :arguments [:request-id "request-1"]))))))))

(deftest bind-delegates-shared-protocol-normalization
  (let [cap (capability/operation-capability
             {:operation :request/claim})]
    (testing "authoritative basis remains required"
      (is (= :missing-basis
             (error-kind
              #(capability/bind
                cap
                (assoc full-binding :observed-basis nil))))))

    (testing "blank scope remains invalid through the shared protocol vocabulary"
      (is (= :invalid-scope
             (error-kind
              #(capability/bind
                cap
                (assoc full-binding :scope "   "))))))

    (testing "fact-version validation is delegated rather than recreated"
      (is (= :invalid-fact-version-key
             (error-kind
              #(capability/bind
                cap
                (assoc full-binding
                       :fact-versions {"request/status" 7}))))))))

(deftest bind-validates-logical-target-identity
  (let [cap (capability/operation-capability
             {:operation :request/claim})]
    ;; Presence is intentional: explicit nil is invalid rather than being
    ;; silently treated as absence.
    (doseq [target [nil "" "   " :request-target 42]]
      (is (= :invalid-target-id
             (error-kind
              #(capability/bind
                cap
                (assoc full-binding :target-id target))))))
    (is (= "request-1"
           (:target-id
            (capability/bind
             cap
             (assoc full-binding :target-id "request-1")))))))

(deftest omitted-optional-binding-and-policy-fields-stay-omitted
  (let [cap (capability/operation-capability
             {:operation :request/claim})
        action (capability/bind
                cap
                {:arguments {:request-id "request-1"}
                 :observed-basis basis})]
    (is (= {:operation :request/claim
            :arguments {:request-id "request-1"}
            :observed-basis basis
            :plan-key :request/claim}
           action))
    (doseq [key [:scope
                 :fact-versions
                 :target-id
                 :rollback-eligible?
                 :timeout-ms
                 :replace-owner?
                 :replace-execution?]]
      (is (not (contains? action key))))))

(deftest bound-actions-cannot-carry-authority-or-runtime-correlation
  (let [cap (capability/operation-capability
             full-capability-options)
        action (capability/bind cap full-binding)]
    (is (empty?
         (select-keys action authority-or-correlation-keys)))
    (is (empty?
         (set/intersection
          (set (keys action))
          authority-or-correlation-keys)))
    (is (= :request/claim (:operation action)))
    (is (= :request/claim (:plan-key action)))))

#?(:clj
   (deftest bound-actions-are-directly-accepted-by-existing-ui-validator
     (let [cap (capability/operation-capability
                full-capability-options)
           action (capability/bind cap full-binding)]
       (is (= action
              (ui/optimistic-action action)))
       (is (= action
              (-> action
                  ui/optimistic-action
                  ui/optimistic-action))))))
