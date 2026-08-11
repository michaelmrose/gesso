(ns gesso.choreo.verify-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- choreography
  ([initial states]
   (choreography initial states nil))
  ([initial states opts]
   (choreo/->choreography
    (merge
     {:name :test/choreography
      :roles #{:browser :server}
      :initial initial
      :states states}
     opts))))

(defn- single-role-choreography
  ([initial states]
   (single-role-choreography initial states nil))
  ([initial states opts]
   (choreography
    initial
    states
    (merge {:roles #{:browser}} opts))))

(defn- error-kinds
  [verification]
  (set (map :kind (:errors verification))))

(defn- warning-kinds
  [verification]
  (set (map :kind (:warnings verification))))

(defn- problems-of-kind
  [verification kind]
  (filterv #(= kind (:kind %)) (:errors verification)))

(defn- warnings-of-kind
  [verification kind]
  (filterv #(= kind (:kind %)) (:warnings verification)))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn- corrupt-state
  [value state-id f & args]
  (apply update-in value [:states state-id] f args))

;; -----------------------------------------------------------------------------
;; Valid fixture choreographies
;; -----------------------------------------------------------------------------

(def simple-choreography
  (single-role-choreography
   :browser/work
   {:browser/work
    (choreo/fx
     :browser
     :test/work
     :browser/done)

    :browser/done
    (choreo/return
     :browser
     :done)}))

(def communication-choreography
  (choreography
   :browser/send
   {:browser/send
    (choreo/send
     :browser
     :server
     :command
     :server/receive
     {:via :http
      :required #{:execution-id :action}
      :optional #{:note}
      :correlation #{:execution-id}})

    :server/receive
    (choreo/receive
     :browser
     :server
     :command
     :server/done
     {:via :http
      :bind :command})

    :server/done
    (choreo/return
     :server
     :accepted)}))

(def distinct-event-choice-choreography
  (choreography
   :server/choose
   {:server/choose
    (choreo/choice
     :server
     :accepted?
     {true :server/send-accepted
      false :server/send-rejected})

    :server/send-accepted
    (choreo/send
     :server
     :browser
     :accepted
     :browser/receive-accepted
     {:via :sse})

    :browser/receive-accepted
    (choreo/receive
     :server
     :browser
     :accepted
     :browser/accepted
     {:via :sse})

    :browser/accepted
    (choreo/return
     :browser
     :accepted)

    :server/send-rejected
    (choreo/send
     :server
     :browser
     :rejected
     :browser/receive-rejected
     {:via :sse})

    :browser/receive-rejected
    (choreo/receive
     :server
     :browser
     :rejected
     :browser/rejected
     {:via :sse})

    :browser/rejected
    (choreo/return
     :browser
     :rejected)}))

(def payload-choice-choreography
  (choreography
   :server/choose
   {:server/choose
    (choreo/choice
     :server
     :accepted?
     {true :server/send-true
      false :server/send-false})

    :server/send-true
    (choreo/send
     :server
     :browser
     :settled
     :browser/receive-true
     {:via :sse
      :required #{:accepted?}})

    :browser/receive-true
    (choreo/receive
     :server
     :browser
     :settled
     :browser/true
     {:via :sse})

    :browser/true
    (choreo/return
     :browser
     :accepted)

    :server/send-false
    (choreo/send
     :server
     :browser
     :settled
     :browser/receive-false
     {:via :sse
      :required #{:accepted?}})

    :browser/receive-false
    (choreo/receive
     :server
     :browser
     :settled
     :browser/false
     {:via :sse})

    :browser/false
    (choreo/return
     :browser
     :rejected)}))

(def resource-choreography
  (single-role-choreography
   :browser/acquire
   {:browser/acquire
    (choreo/acquire
     :browser
     :target-authority
     :browser/work)

    :browser/work
    (choreo/fx
     :browser
     :test/work
     :browser/release)

    :browser/release
    (choreo/release
     :browser
     :target-authority
     :browser/done)

    :browser/done
    (choreo/return
     :browser
     :done)}
   {:resources
    {:target-authority
     (choreo/resource
      {:owner :browser})}}))

;; -----------------------------------------------------------------------------
;; Public result contract
;; -----------------------------------------------------------------------------

(deftest verify-result-contract-test
  (let [result (verify/verify simple-choreography)]
    (is (= verify/verification-type
           (:gesso.choreo/type result)))
    (is (true? (:valid? result)))
    (is (= simple-choreography
           (:choreography result)))
    (is (= [] (:errors result)))
    (is (= [] (:warnings result)))

    (testing "analysis exposes graph facts used by projection and diagnostics"
      (is (= #{:browser/work :browser/done}
             (get-in result [:analysis :reachable-state-ids])))
      (is (= #{}
             (get-in result [:analysis :unreachable-state-ids])))
      (is (= #{:browser/done}
             (get-in result [:analysis :terminal-state-ids])))
      (is (= #{:browser/done}
             (get-in result [:analysis :successors :browser/work])))
      (is (= #{:browser/work}
             (get-in result [:analysis :predecessors :browser/done]))))))

(deftest verify-normalizes-plain-choreography-test
  (let [result
        (verify/verify
         {:roles [:browser]
          :initial :done
          :states
          {:done
           {:op :return
            :role :browser
            :outcome :done}}})]
    (is (:valid? result))
    (is (choreo/choreography?
         (:choreography result)))
    (is (= #{:browser}
           (get-in result [:choreography :roles])))))

(deftest valid-and-verify-bang-test
  (is (verify/valid? simple-choreography))
  (is (false?
       (verify/valid?
        (assoc simple-choreography :initial :missing))))

  (let [verified (verify/verify! simple-choreography)]
    (is (verify/verified? verified))
    (is (= verify/verified-type
           (:gesso.choreo/type verified)))
    (is (true? (:verified? verified)))
    (is (= simple-choreography
           (:choreography verified)))
    (is (map? (:analysis verified))))

  (let [bad (assoc simple-choreography :initial :missing)
        data (thrown-data #(verify/verify! bad))]
    (is (= :gesso.choreo/verification-failed
           (:error/type data)))
    (is (seq (:errors data)))
    (is (vector? (:warnings data)))
    (is (map? (:analysis data)))
    (is (= :missing
           (get-in data [:choreography :initial])))))

(deftest ensure-verified-test
  (let [verified (verify/verify! simple-choreography)]
    (is (identical?
         verified
         (verify/ensure-verified verified)))
    (is (verify/verified?
         (verify/ensure-verified simple-choreography)))))

(deftest explain-test
  (is (= {:valid? true
          :error-count 0
          :warning-count 0
          :reachable-state-count 2
          :unreachable-state-count 0
          :terminal-state-count 1}
         (verify/explain simple-choreography)))

  (let [with-warning
        (assoc-in simple-choreography
                  [:states :browser/unreachable]
                  (choreo/return :browser :unused))]
    (is (= {:valid? true
            :error-count 0
            :warning-count 1
            :reachable-state-count 2
            :unreachable-state-count 1
            :terminal-state-count 1}
           (verify/explain with-warning)))))

;; -----------------------------------------------------------------------------
;; Problem construction and communication identity
;; -----------------------------------------------------------------------------

(deftest problem-constructor-test
  (is (= {:kind :test/problem
          :path [:states :x]
          :message "Problem"}
         (verify/problem
          :test/problem
          [:states :x]
          "Problem")))

  (is (= {:kind :test/problem
          :path [:states :x :next]
          :message "Problem"
          :data {:target :missing}}
         (verify/problem
          :test/problem
          [:states :x :next]
          "Problem"
          {:target :missing}))))

(deftest communication-key-test
  (is (= {:from :browser
          :to :server
          :event :command
          :via :http}
         (verify/communication-key
          (choreo/state
           communication-choreography
           :browser/send))))

  (testing "payload and correlation details do not change communication identity"
    (is (= (verify/communication-key
            (choreo/state communication-choreography :browser/send))
           (verify/communication-key
            (choreo/state communication-choreography :server/receive))))))

;; -----------------------------------------------------------------------------
;; Top-level and state-shape validation
;; -----------------------------------------------------------------------------

(deftest top-level-validation-test
  (doseq [[kind value]
          [[:invalid-name
            (assoc simple-choreography :name "not-a-keyword")]

           [:missing-roles
            (assoc simple-choreography :roles #{})]

           [:invalid-roles
            (assoc simple-choreography :roles #{:browser "server"})]

           [:invalid-initial-state
            (assoc simple-choreography :initial "browser/work")]

           [:unknown-initial-state
            (assoc simple-choreography :initial :missing)]

           [:missing-states
            (assoc simple-choreography :states {})]

           [:invalid-environment-events
            (assoc simple-choreography
                   :environment-events
                   #{:timeout "disconnect"})]]]
    (let [result (verify/verify value)]
      (is (false? (:valid? result)) (str "Expected " kind))
      (is (contains? (error-kinds result) kind)
          (str "Missing verifier problem " kind)))))

(deftest state-shape-validation-test
  (let [cases
        [[:unknown-op
          #(assoc % :op :dance)]

         [:invalid-role
          #(assoc % :role "browser")]

         [:unknown-role
          #(assoc % :role :ghost)]

         [:invalid-fx-machine
          #(assoc % :machine "work")]

         [:invalid-next-state
          #(assoc % :next "done")]

         [:invalid-metadata
          #(assoc % :metadata [:not :a :map])]]]
    (doseq [[kind mutate] cases]
      (let [value (corrupt-state simple-choreography :browser/work mutate)
            result (verify/verify value)]
        (is (contains? (error-kinds result) kind)
            (str "Missing verifier problem " kind))))))

(deftest terminal-shape-validation-test
  (let [invalid-outcome
        (corrupt-state
         simple-choreography
         :browser/done
         assoc
         :outcome
         "done")

        invalid-value-key
        (corrupt-state
         simple-choreography
         :browser/done
         assoc
         :value-key
         "result")

        successor
        (corrupt-state
         simple-choreography
         :browser/done
         assoc
         :next
         :browser/done)]
    (is (contains?
         (error-kinds (verify/verify invalid-outcome))
         :invalid-terminal-outcome))
    (is (contains?
         (error-kinds (verify/verify invalid-value-key))
         :invalid-value-key))
    (is (contains?
         (error-kinds (verify/verify successor))
         :terminal-has-successor))))

(deftest message-shape-validation-test
  (let [send-id :browser/send
        same-role
        (-> communication-choreography
            (corrupt-state send-id assoc :to :browser))

        invalid-event
        (corrupt-state communication-choreography send-id assoc :event "command")

        invalid-transport
        (corrupt-state communication-choreography send-id assoc :via "http")

        ambiguous-key
        (corrupt-state communication-choreography send-id assoc :optional #{:action})

        optional-correlation
        (corrupt-state communication-choreography send-id assoc
                       :correlation #{:note})

        invalid-required
        (corrupt-state communication-choreography send-id assoc
                       :required [:execution-id])]
    (is (contains? (error-kinds (verify/verify same-role))
                   :same-role-communication))
    (is (contains? (error-kinds (verify/verify invalid-event))
                   :invalid-event))
    (is (contains? (error-kinds (verify/verify invalid-transport))
                   :invalid-transport))
    (is (contains? (error-kinds (verify/verify ambiguous-key))
                   :ambiguous-message-key))
    (is (contains? (error-kinds (verify/verify optional-correlation))
                   :optional-correlation-key))
    (is (contains? (error-kinds (verify/verify invalid-required))
                   :invalid-message-keys))))

(deftest choice-and-await-shape-validation-test
  (let [choice-base
        (single-role-choreography
         :browser/choose
         {:browser/choose
          (choreo/choice
           :browser
           :answer
           {true :browser/done})
          :browser/done
          (choreo/return :browser :done)})

        await-base
        (single-role-choreography
         :browser/wait
         {:browser/wait
          (choreo/await
           :browser
           {:timeout :browser/done})
          :browser/done
          (choreo/return :browser :done)}
         {:environment-events #{:timeout}})]
    (doseq [[kind value]
            [[:invalid-choice-key
              (corrupt-state choice-base :browser/choose assoc :key "answer")]
             [:invalid-choice-branches
              (corrupt-state choice-base :browser/choose assoc :branches [])]
             [:empty-choice
              (corrupt-state choice-base :browser/choose assoc :branches {})]
             [:invalid-choice-target
              (corrupt-state choice-base :browser/choose assoc
                             :branches {true "done"})]
             [:invalid-await-events
              (corrupt-state await-base :browser/wait assoc :events [])]
             [:empty-await
              (corrupt-state await-base :browser/wait assoc :events {})]
             [:invalid-await-event
              (corrupt-state await-base :browser/wait assoc
                             :events {"timeout" :browser/done})]
             [:invalid-await-target
              (corrupt-state await-base :browser/wait assoc
                             :events {:timeout "done"})]
             [:invalid-bind
              (corrupt-state await-base :browser/wait assoc :bind "event")]]]
      (is (contains? (error-kinds (verify/verify value)) kind)
          (str "Missing verifier problem " kind)))))

;; -----------------------------------------------------------------------------
;; Graph topology and reachability
;; -----------------------------------------------------------------------------

(deftest unknown-successor-test
  (let [value
        (corrupt-state
         simple-choreography
         :browser/work
         assoc
         :next
         :browser/missing)
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :unknown-state))
    (is (= :browser/missing
           (get-in (first (problems-of-kind result :unknown-state))
                   [:data :target])))))

(deftest reachable-cycle-without-terminal-test
  (let [value
        (single-role-choreography
         :browser/loop
         {:browser/loop
          (choreo/goto :browser/loop)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :no-terminal-path))
    (is (= #{:browser/loop}
           (get-in result [:analysis :reachable-state-ids])))
    (is (= #{}
           (get-in result [:analysis :terminal-state-ids])))))

(deftest unreachable-states-are-warnings-test
  (let [value
        (assoc-in
         (assoc-in simple-choreography
                   [:states :browser/unreachable-work]
                   (choreo/fx
                    :browser
                    :test/unreachable
                    :browser/unreachable-done))
         [:states :browser/unreachable-done]
         (choreo/return :browser :unused))
        result (verify/verify value)]
    (is (:valid? result))
    (is (= #{:unreachable-state}
           (warning-kinds result)))
    (is (= #{:browser/unreachable-work
            :browser/unreachable-done}
           (get-in result [:analysis :unreachable-state-ids])))
    (is (= 2
           (count (warnings-of-kind result :unreachable-state))))))

;; -----------------------------------------------------------------------------
;; Communication pairing
;; -----------------------------------------------------------------------------

(deftest matching-send-receive-test
  (let [result (verify/verify communication-choreography)]
    (is (:valid? result))
    (is (empty? (:errors result)))))

(deftest send-must-transition-directly-to-matching-receive-test
  (let [value
        (corrupt-state
         communication-choreography
         :server/receive
         assoc
         :event
         :different-event)
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :unmatched-send))
    (is (contains? (error-kinds result) :unmatched-receive))))

(deftest reachable-receive-requires-matching-send-predecessor-test
  (let [value
        (choreography
         :server/receive
         {:server/receive
          (choreo/receive
           :browser
           :server
           :command
           :server/done
           {:via :http})
          :server/done
          (choreo/return :server :done)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :unmatched-receive))))

(deftest receive-rejects-nonmatching-entry-path-test
  (let [value
        (choreography
         :browser/choose
         {:browser/choose
          (choreo/choice
           :browser
           :path
           {:send :browser/send
            :skip :browser/skip})

          :browser/send
          (choreo/send
           :browser
           :server
           :command
           :server/receive
           {:via :http})

          :browser/skip
          (choreo/goto :server/receive)

          :server/receive
          (choreo/receive
           :browser
           :server
           :command
           :server/done
           {:via :http})

          :server/done
          (choreo/return :server :done)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :unguarded-receive))))

;; -----------------------------------------------------------------------------
;; Environment-event provenance
;; -----------------------------------------------------------------------------

(deftest declared-environment-event-may-drive-await-test
  (let [value
        (single-role-choreography
         :browser/wait
         {:browser/wait
          (choreo/await
           :browser
           {:timeout :browser/done}
           {:bind :wake})
          :browser/done
          (choreo/return :browser :done)}
         {:environment-events #{:timeout}})]
    (is (verify/valid? value))))

(deftest await-rejects-event-with-no-producer-test
  (let [value
        (single-role-choreography
         :browser/wait
         {:browser/wait
          (choreo/await
           :browser
           {:mystery :browser/done})
          :browser/done
          (choreo/return :browser :done)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :unproducible-event))))

(deftest send-interrupt-must-be-declared-environment-event-test
  (let [states
        {:browser/send
         (choreo/send
          :browser
          :server
          :command
          :server/receive
          {:via :http
           :interrupts
           {:request-failed :browser/failed}})

         :server/receive
         (choreo/receive
          :browser
          :server
          :command
          :server/done
          {:via :http})

         :server/done
         (choreo/return :server :done)

         :browser/failed
         (choreo/return :browser :failed)}

        undeclared
        (choreography :browser/send states)

        declared
        (choreography
         :browser/send
         states
         {:environment-events #{:request-failed}})]
    (is (contains?
         (error-kinds (verify/verify undeclared))
         :undeclared-environment-event))
    (is (verify/valid? declared))))

(deftest unused-environment-events-are-warnings-test
  (let [value
        (assoc simple-choreography
               :environment-events #{:timeout :disconnect})
        result (verify/verify value)]
    (is (:valid? result))
    (is (= #{:unused-environment-event}
           (warning-kinds result)))
    (is (= #{:timeout :disconnect}
           (set
            (map #(get-in % [:data :event])
                 (warnings-of-kind result :unused-environment-event)))))))

;; -----------------------------------------------------------------------------
;; Resource descriptors and linear-resource lifecycle
;; -----------------------------------------------------------------------------

(deftest valid-linear-resource-lifecycle-test
  (let [result (verify/verify resource-choreography)]
    (is (:valid? result))
    (is (= #{#{:target-authority}}
           (get-in result [:analysis :resource-holdings :browser/acquire])))
    (is (= #{#{:target-authority}}
           (get-in result [:analysis :resource-holdings :browser/work])))
    (is (= #{#{}}
           (get-in result [:analysis :resource-holdings :browser/release])))
    (is (= #{#{}}
           (get-in result [:analysis :resource-holdings :browser/done])))))

(deftest resource-descriptor-validation-test
  (let [base
        (single-role-choreography
         :browser/done
         {:browser/done
          (choreo/return :browser :done)}
         {:resources
          {:resource (choreo/resource {:owner :browser})}})]
    (doseq [[kind descriptor]
            [[:unknown-role
              {:owner :ghost
               :linear? true
               :terminal-release? true
               :metadata {}}]
             [:invalid-resource-linearity
              {:owner :browser
               :linear? :yes
               :terminal-release? true
               :metadata {}}]
             [:invalid-resource-terminal-policy
              {:owner :browser
               :linear? true
               :terminal-release? :yes
               :metadata {}}]
             [:invalid-metadata
              {:owner :browser
               :linear? true
               :terminal-release? true
               :metadata []}]]]
      (let [result
            (verify/verify
             (assoc-in base [:resources :resource] descriptor))]
        (is (contains? (error-kinds result) kind)
            (str "Missing verifier problem " kind))))))

(deftest resource-operation-requires-declared-resource-test
  (let [value
        (corrupt-state
         resource-choreography
         :browser/acquire
         assoc
         :resource
         :missing)
        result (verify/verify value)]
    (is (contains? (error-kinds result) :unknown-resource))))

(deftest resource-owner-is-authoritative-test
  (let [value
        (choreography
         :server/acquire
         {:server/acquire
          (choreo/acquire
           :server
           :target-authority
           :server/release)
          :server/release
          (choreo/release
           :server
           :target-authority
           :server/done)
          :server/done
          (choreo/return :server :done)}
         {:resources
          {:target-authority
           (choreo/resource {:owner :browser})}})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :resource-owner-mismatch))))

(deftest linear-resource-double-acquire-test
  (let [value
        (single-role-choreography
         :browser/acquire-1
         {:browser/acquire-1
          (choreo/acquire
           :browser
           :target-authority
           :browser/acquire-2)
          :browser/acquire-2
          (choreo/acquire
           :browser
           :target-authority
           :browser/release)
          :browser/release
          (choreo/release
           :browser
           :target-authority
           :browser/done)
          :browser/done
          (choreo/return :browser :done)}
         {:resources
          {:target-authority
           (choreo/resource {:owner :browser})}})
        result (verify/verify value)]
    (is (contains? (error-kinds result) :resource-double-acquire))))

(deftest linear-resource-double-release-test
  (let [value
        (single-role-choreography
         :browser/acquire
         {:browser/acquire
          (choreo/acquire
           :browser
           :target-authority
           :browser/release-1)
          :browser/release-1
          (choreo/release
           :browser
           :target-authority
           :browser/release-2)
          :browser/release-2
          (choreo/release
           :browser
           :target-authority
           :browser/done)
          :browser/done
          (choreo/return :browser :done)}
         {:resources
          {:target-authority
           (choreo/resource {:owner :browser})}})
        result (verify/verify value)]
    (is (contains? (error-kinds result) :resource-double-release))))

(deftest linear-resource-must-not-leak-at-terminal-test
  (let [value
        (single-role-choreography
         :browser/acquire
         {:browser/acquire
          (choreo/acquire
           :browser
           :target-authority
           :browser/done)
          :browser/done
          (choreo/return :browser :done)}
         {:resources
          {:target-authority
           (choreo/resource {:owner :browser})}})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :resource-leak))
    (is (= #{:target-authority}
           (get-in (first (problems-of-kind result :resource-leak))
                   [:data :resources])))))

(deftest resource-terminal-release-policy-test
  (let [value
        (single-role-choreography
         :browser/acquire
         {:browser/acquire
          (choreo/acquire
           :browser
           :cache
           :browser/done)
          :browser/done
          (choreo/return :browser :done)}
         {:resources
          {:cache
           (choreo/resource
            {:owner :browser
             :terminal-release? false})}})]
    (is (verify/valid? value))))

(deftest non-linear-resource-does-not-participate-in-linear-holding-errors-test
  (let [value
        (single-role-choreography
         :browser/acquire-1
         {:browser/acquire-1
          (choreo/acquire :browser :cache :browser/acquire-2)
          :browser/acquire-2
          (choreo/acquire :browser :cache :browser/done)
          :browser/done
          (choreo/return :browser :done)}
         {:resources
          {:cache
           (choreo/resource
            {:owner :browser
             :linear? false})}})]
    (is (verify/valid? value))))

(deftest branch-specific-resource-leak-is-rejected-test
  (let [value
        (single-role-choreography
         :browser/acquire
         {:browser/acquire
          (choreo/acquire
           :browser
           :target-authority
           :browser/choose)

          :browser/choose
          (choreo/choice
           :browser
           :release?
           {true :browser/release
            false :browser/leak})

          :browser/release
          (choreo/release
           :browser
           :target-authority
           :browser/clean)

          :browser/clean
          (choreo/return :browser :clean)

          :browser/leak
          (choreo/return :browser :leaked)}
         {:resources
          {:target-authority
           (choreo/resource {:owner :browser})}})
        result (verify/verify value)]
    (is (contains? (error-kinds result) :resource-leak))
    (is (= #{:browser/leak}
           (set
            (map #(get-in % [:data :state])
                 (problems-of-kind result :resource-leak)))))))

;; -----------------------------------------------------------------------------
;; Knowledge of choice
;; -----------------------------------------------------------------------------

(deftest branch-specific-event-identity-communicates-choice-test
  (let [result (verify/verify distinct-event-choice-choreography)]
    (is (:valid? result))
    (is (not (contains? (error-kinds result) :uncommunicated-choice)))))

(deftest required-choice-payload-communicates-choice-test
  (let [result (verify/verify payload-choice-choreography)]
    (is (:valid? result))
    (is (not (contains? (error-kinds result) :uncommunicated-choice)))))

(deftest same-event-without-choice-payload-does-not-communicate-choice-test
  (let [value
        (-> payload-choice-choreography
            (corrupt-state :server/send-true dissoc :required)
            (corrupt-state :server/send-false dissoc :required))
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :uncommunicated-choice))))

(deftest remote-role-may-not-act-before-learning-branch-test
  (let [value
        (choreography
         :server/choose
         {:server/choose
          (choreo/choice
           :server
           :accepted?
           {true :browser/accepted-work
            false :browser/rejected-work})

          :browser/accepted-work
          (choreo/fx
           :browser
           :test/accepted
           :browser/accepted)

          :browser/accepted
          (choreo/return :browser :accepted)

          :browser/rejected-work
          (choreo/fx
           :browser
           :test/rejected
           :browser/rejected)

          :browser/rejected
          (choreo/return :browser :rejected)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :uncommunicated-choice))
    (is (some
         #(= :fx (get-in % [:data :first-remote-op]))
         (problems-of-kind result :uncommunicated-choice)))))

(deftest branch-may-not-terminate-before-dependent-role-learns-choice-test
  (let [value
        (choreography
         :server/choose
         {:server/choose
          (choreo/choice
           :server
           :accepted?
           {true :server/send-accepted
            false :server/rejected})

          :server/send-accepted
          (choreo/send
           :server
           :browser
           :accepted
           :browser/receive-accepted)

          :browser/receive-accepted
          (choreo/receive
           :server
           :browser
           :accepted
           :browser/accepted)

          :browser/accepted
          (choreo/return :browser :accepted)

          :server/rejected
          (choreo/return :server :rejected)})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :uncommunicated-choice))
    (is (some
         #(and (= false (get-in % [:data :branch]))
               (= :browser (get-in % [:data :dependent-role])))
         (problems-of-kind result :uncommunicated-choice)))))

(deftest choice-must-be-communicated-by-authoritative-owner-test
  (let [value
        (choreography
         :server/choose
         {:server/choose
          (choreo/choice
           :server
           :accepted?
           {true :third/send-accepted
            false :third/send-rejected})

          :third/send-accepted
          (choreo/send
           :third
           :browser
           :accepted
           :browser/receive-accepted)

          :browser/receive-accepted
          (choreo/receive
           :third
           :browser
           :accepted
           :browser/accepted)

          :browser/accepted
          (choreo/return :browser :accepted)

          :third/send-rejected
          (choreo/send
           :third
           :browser
           :rejected
           :browser/receive-rejected)

          :browser/receive-rejected
          (choreo/receive
           :third
           :browser
           :rejected
           :browser/rejected)

          :browser/rejected
          (choreo/return :browser :rejected)}
         {:roles #{:browser :server :third}})
        result (verify/verify value)]
    (is (false? (:valid? result)))
    (is (contains? (error-kinds result) :uncommunicated-choice))))

(deftest branch-independent-remote-behavior-needs-no-choice-message-test
  (let [value
        (choreography
         :server/choose
         {:server/choose
          (choreo/choice
           :server
           :accepted?
           {true :browser/common
            false :browser/common})

          :browser/common
          (choreo/fx
           :browser
           :test/common
           :browser/done)

          :browser/done
          (choreo/return :browser :done)})]
    (is (verify/valid? value))))

;; -----------------------------------------------------------------------------
;; Warnings do not weaken verification identity
;; -----------------------------------------------------------------------------

(deftest warnings-do-not-make-choreography-invalid-test
  (let [value
        (-> simple-choreography
            (assoc :environment-events #{:unused})
            (assoc-in [:states :browser/unreachable]
                      (choreo/return :browser :unused)))
        result (verify/verify value)
        verified (verify/verify! value)]
    (is (:valid? result))
    (is (= #{:unused-environment-event
            :unreachable-state}
           (warning-kinds result)))
    (is (verify/verified? verified))
    (is (= (:warnings result)
           (:warnings verified)))))
