(ns gesso.choreo.verify-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.verify :as verify]))

(defn- verification-error
  [f]
  (try
    (f)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (ex-data e))))

(defn- problem-kinds
  [problems]
  (set
   (map :kind problems)))

(deftest simple-terminating-choreography-verifies
  (let [choreography
        (choreo/->choreography
         {:name :example/simple
          :initial :prepare
          :states
          {:prepare
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :example/command
            :done)

           :done
           (choreo/return
            :done)}})

        result
        (verify/verify choreography)]

    (is (verify/verification?
         result))

    (is (:valid? result))
    (is (empty?
         (:errors result)))
    (is (empty?
         (:warnings result)))

    (is (= #{:prepare
             :send
             :done}
           (get-in result
                   [:analysis
                    :reachable-state-ids])))

    (is (= #{:done}
           (get-in result
                   [:analysis
                    :terminal-state-ids])))

    (is (= #{:browser
             :authority}
           (get-in result
                   [:analysis
                    :roles])))

    (is (= {:local 1
            :communicate 1
            :return 1}
           (get-in result
                   [:analysis
                    :states-by-op])))))

(deftest verifier-distinguishes-graph-terminal-path-from-liveness
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:environment/completed
             :done})

           :done
           (choreo/return
            :done)}})

        result
        (verify/verify choreography)]

    (testing "the graph is valid because a terminal path exists"
      (is (:valid? result))
      (is (empty?
           (:errors result))))

    (testing "verification does not require the environment event to occur"
      (is (= #{:wait
               :done}
             (get-in result
                     [:analysis
                      :can-reach-terminal-state-ids]))))))

(deftest choreography-with-no-reachable-terminal-is-invalid
  (let [choreography
        (choreo/->choreography
         {:initial :loop
          :states
          {:loop
           (choreo/local
            :browser
            :spin
            :loop)}})

        result
        (verify/verify choreography)]

    (is (false?
         (:valid? result)))

    (is (= #{:missing-reachable-terminal
             :no-terminal-path}
           (problem-kinds
            (:errors result))))

    (is (= #{}
           (get-in result
                   [:analysis
                    :terminal-state-ids])))

    (is (= #{}
           (get-in result
                   [:analysis
                    :can-reach-terminal-state-ids])))))

(deftest verifier-identifies-only-the-reachable-region-without-terminal-path
  (let [choreography
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/await
            :browser
            {:environment/good
             :done

             :environment/bad
             :loop})

           :loop
           (choreo/local
            :browser
            :spin
            :loop)

           :done
           (choreo/return
            :done)}})

        result
        (verify/verify choreography)

        no-terminal
        (filter
         #(= :no-terminal-path
             (:kind %))
         (:errors result))]

    (is (false?
         (:valid? result)))

    (is (= 1
           (count no-terminal)))

    (is (= :loop
           (get-in
            (first no-terminal)
            [:data :state])))

    (is (= #{:branch
             :done}
           (get-in result
                   [:analysis
                    :can-reach-terminal-state-ids])))))

(deftest unreachable-states-are-warnings-not-errors
  (let [choreography
        (choreo/->choreography
         {:initial :start
          :states
          {:start
           (choreo/local
            :browser
            :prepare
            :done)

           :done
           (choreo/return
            :done)

           :unused
           (choreo/local
            :authority
            :never-runs
            :unused-done)

           :unused-done
           (choreo/return
            :unused)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))
    (is (empty?
         (:errors result)))

    (is (= #{:unreachable-state}
           (problem-kinds
            (:warnings result))))

    (is (= #{:unused
             :unused-done}
           (get-in result
                   [:analysis
                    :unreachable-state-ids])))

    (is (= #{:start
             :done}
           (get-in result
                   [:analysis
                    :reachable-state-ids])))))

(deftest analysis-identifies-current-semantic-state-categories
  (let [choreography
        (choreo/->choreography
         {:initial :local
          :states
          {:local
           (choreo/local
            :browser
            :prepare
            :send)

           :send
           (choreo/communicate
            :browser
            :authority
            :example/command
            :wait)

           :wait
           (choreo/await
            :authority
            {:environment/complete
             :done})

           :done
           (choreo/return
            :done)}})

        analysis
        (:analysis
         (verify/verify
          choreography))]

    (is (= #{:local}
           (:local-state-ids
            analysis)))

    (is (= #{:send}
           (:communication-state-ids
            analysis)))

    (is (= #{:wait}
           (:await-state-ids
            analysis)))

    (is (= #{:browser}
           (get-in analysis
                   [:roles-by-state
                    :local])))

    (is (= #{:browser
             :authority}
           (get-in analysis
                   [:roles-by-state
                    :send])))

    (is (= #{:authority}
           (get-in analysis
                   [:roles-by-state
                    :wait])))

    (is (= #{}
           (get-in analysis
                   [:roles-by-state
                    :done])))))

(deftest verify!-returns-a-verified-wrapper
  (let [choreography
        (choreo/->choreography
         {:initial :start
          :states
          {:start
           (choreo/local
            :browser
            :prepare
            :done)

           :done
           (choreo/return
            :done)}})

        verified
        (verify/verify!
         choreography)]

    (is (verify/verified?
         verified))

    (is (= choreography
           (:choreography
            verified)))

    (is (verify/verification?
         (:verification
          verified)))

    (is (true?
         (get-in verified
                 [:verification
                  :valid?])))))

(deftest verify!-throws-with-the-complete-invalid-result
  (let [choreography
        (choreo/->choreography
         {:initial :loop
          :states
          {:loop
           (choreo/local
            :browser
            :spin
            :loop)}})

        error
        (verification-error
         #(verify/verify!
           choreography))]

    (is (= :gesso.choreo/verification-failed
           (:error/type error)))

    (is (= #{:missing-reachable-terminal
             :no-terminal-path}
           (problem-kinds
            (:errors error))))

    (is (verify/verification?
         (:verification
          error)))))

(deftest ensure-verified-accepts-all-successful-input-forms
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return
            :done)}})

        verification
        (verify/verify
         choreography)

        verified
        (verify/verify!
         choreography)]

    (is (verify/verified?
         (verify/ensure-verified
          choreography)))

    (is (verify/verified?
         (verify/ensure-verified
          verification)))

    (is (= verified
           (verify/ensure-verified
            verified)))))

(deftest ensure-verified-rejects-an-invalid-verification-result
  (let [choreography
        (choreo/->choreography
         {:initial :loop
          :states
          {:loop
           (choreo/local
            :browser
            :spin
            :loop)}})

        verification
        (verify/verify
         choreography)

        error
        (verification-error
         #(verify/ensure-verified
           verification))]

    (is (= :gesso.choreo/verification-failed
           (:error/type error)))

    (is (= verification
           (:verification
            error)))))

(deftest opaque-state-identities-survive-verifier-analysis
  (let [start
        [:state :start 1]

        wait
        [:state :wait 2]

        done
        [:state :done 3]

        choreography
        (choreo/->choreography
         {:initial start
          :states
          {start
           (choreo/local
            :browser
            :prepare
            wait)

           wait
           (choreo/await
            :browser
            {:environment/done
             done})

           done
           (choreo/return
            :done)}})

        analysis
        (:analysis
         (verify/verify
          choreography))]

    (is (= #{start
             wait
             done}
           (:reachable-state-ids
            analysis)))

    (is (= #{done}
           (:terminal-state-ids
            analysis)))

    (is (= #{wait}
           (:await-state-ids
            analysis)))))

(deftest explain-reports-only-current-verifier-claims
  (let [choreography
        (choreo/->choreography
         {:initial :start
          :states
          {:start
           (choreo/communicate
            :alice
            :bob
            :example/hello
            :done)

           :done
           (choreo/return
            :done)}})

        explanation
        (verify/explain
         choreography)]

    (is (= {:valid? true
            :error-count 0
            :warning-count 0
            :roles #{:alice :bob}
            :reachable-state-count 2
            :terminal-state-count 1
            :states-by-op
            {:communicate 1
             :return 1}}
           explanation))))

(deftest locally-produced-value-can-drive-a-later-branch
  (let [choreography
        (choreo/->choreography
         {:initial :produce
          :states
          {:produce
           (choreo/local
            :browser
            :compute-outcome
            :branch
            {:outputs #{:outcome}})

           :branch
           (choreo/branch
            :browser
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/return :confirmed)

           :rejected
           (choreo/return :rejected)}})

        result
        (verify/verify choreography)

        analysis
        (:analysis result)]

    (is (:valid? result))
    (is (empty? (:errors result)))

    (is (= #{:outcome}
           (:produced-value-keys analysis)))

    (is (= #{:outcome}
           (:required-value-keys analysis)))

    (is (= #{:produce}
           (get-in analysis
                   [:value-producers :outcome])))

    (is (= #{:branch}
           (get-in analysis
                   [:value-consumers :outcome])))

    (is (= #{:outcome}
           (get-in analysis
                   [:definitely-established-before-state
                    :branch])))

    (is (= #{:branch}
           (:branch-state-ids analysis)))))

(deftest branch-selector-must-be-definitely-established
  (let [choreography
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/branch
            :browser
            :outcome
            {:confirmed :done})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        error
        (first
         (filter
          #(= :value-not-definitely-established
              (:kind %))
          (:errors result)))]

    (is (false? (:valid? result)))

    (is (= :value-not-definitely-established
           (:kind error)))

    (is (= [:states :branch]
           (:path error)))

    (is (= {:state :branch
            :op :branch
            :role :browser
            :value-key :outcome
            :required #{:outcome}
            :definitely-established #{}}
           (:data error)))))

(deftest local-requirements-must-be-definitely-established
  (let [choreography
        (choreo/->choreography
         {:initial :consume
          :states
          {:consume
           (choreo/local
            :server
            :install-canonical
            :done
            {:requires #{:canonical}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        error
        (first
         (filter
          #(= :value-not-definitely-established
              (:kind %))
          (:errors result)))]

    (is (false? (:valid? result)))

    (is (= {:state :consume
            :op :local
            :role :server
            :value-key :canonical
            :required #{:canonical}
            :definitely-established #{}}
           (:data error)))))

(deftest explicit-entry-values-can-satisfy-low-level-dataflow
  (let [choreography
        (choreo/->choreography
         {:initial :branch
          :states
          {:branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :done
             :rejected :done})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-value-keys #{:outcome}})

        analysis
        (:analysis result)]

    (is (:valid? result))

    (is (= #{:outcome}
           (get-in result
                   [:options :entry-value-keys])))

    (is (= #{:outcome}
           (:entry-value-keys analysis)))

    (is (= #{:outcome}
           (get-in analysis
                   [:definitely-established-before-state
                    :branch])))))

(deftest value-produced-on-only-one-path-is-not-definite-after-join
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:environment/produce :produce
             :environment/skip :join})

           :produce
           (choreo/local
            :browser
            :produce-result
            :join
            {:outputs #{:result}})

           :join
           (choreo/local
            :browser
            :consume-result
            :done
            {:requires #{:result}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        analysis
        (:analysis result)

        error
        (first
         (filter
          #(and (= :value-not-definitely-established
                   (:kind %))
                (= :join
                   (get-in % [:data :state])))
          (:errors result)))]

    (is (false? (:valid? result)))

    (is (= #{}
           (get-in analysis
                   [:definitely-established-before-state
                    :join])))

    (is (= :result
           (get-in error
                   [:data :value-key])))))

(deftest value-produced-on-every-path-is-definite-after-join
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:environment/one :produce-one
             :environment/two :produce-two})

           :produce-one
           (choreo/local
            :browser
            :produce-one
            :join
            {:outputs #{:result}})

           :produce-two
           (choreo/local
            :browser
            :produce-two
            :join
            {:outputs #{:result}})

           :join
           (choreo/local
            :browser
            :consume-result
            :done
            {:requires #{:result}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is (= #{:result}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :join])))))

(deftest verifier-options-are-not-reapplied-to-existing-artifacts
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})

        verification
        (verify/verify choreography)

        error
        (verification-error
         #(verify/ensure-verified
           verification
           {:entry-value-keys #{:outcome}}))]

    (is (= :options-with-verification-artifact
           (:error/kind error)))))

(deftest verifier-options-fail-closed-on-unknown-keys
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})]

    (doseq [[options expected-unknown]
            [[{:entry-knolwedge
               {:server #{:request-id}}}
              #{:entry-knolwedge}]

             [{:entry-value-keys #{}
               :future-assumption true}
              #{:future-assumption}]]]
      (let [error
            (verification-error
             #(verify/verify choreography options))]
        (is (= :gesso.choreo.verify/error
               (:error/type error)))
        (is (= :unknown-option-keys
               (:error/kind error)))
        (is (= expected-unknown
               (:unknown-option-keys error)))
        (is (= #{:entry-value-keys
                 :entry-knowledge}
               (:allowed-option-keys error)))))))

(deftest verifier-option-values-fail-closed-on-invalid-portable-shapes
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})]

    (doseq [entry-value-keys
            [[:request-id]
             #{"request-id"}
             #{:request-id "principal"}]]
      (let [error
            (verification-error
             #(verify/verify
               choreography
               {:entry-value-keys entry-value-keys}))]
        (is (= :gesso.choreo.verify/error
               (:error/type error)))
        (is (= :invalid-entry-value-keys
               (:error/kind error)))))

    (let [error
          (verification-error
           #(verify/verify
             choreography
             {:entry-knowledge
              {:server #{:request-id "principal"}}}))]
      (is (= :gesso.choreo.verify/error
             (:error/type error)))
      (is (= :invalid-entry-value-keys
             (:error/kind error))))))

(deftest verifier-does-not-own-entry-role-participant-validation
  (let [choreography
        (choreo/->choreography
         {:initial :done
          :states
          {:done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-knowledge
          {:external-role #{:request-id}}})]

    (testing "verify validates the portable Role shape, not caller-specific participation"
      (is (:valid? result))
      (is (= {:external-role #{:request-id}}
             (get-in result
                     [:options :entry-knowledge]))))

    (testing "the assumption still contributes to the low-level protocol value universe"
      (is (= #{:request-id}
             (get-in result
                     [:analysis :protocol-entry-value-keys]))))))

(deftest authoritative-operation-consumes-and-produces-definite-values
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :branch
            {:requires #{:request-id :principal}
             :outputs #{:outcome :revision}})

           :branch
           (choreo/branch
            :server
            :outcome
            {:confirmed :done
             :rejected :done})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-value-keys
          #{:request-id :principal}})

        analysis
        (:analysis result)]

    (is (:valid? result))

    (is (= #{:claim}
           (:authoritative-state-ids analysis)))

    (is (= {:claim :request/claim}
           (:authoritative-operations-by-state analysis)))

    (is (= #{:request-id :principal}
           (get-in analysis
                   [:definitely-established-before-state
                    :claim])))

    (is (= #{:request-id :principal :outcome :revision}
           (get-in analysis
                   [:definitely-established-after-state
                    :claim])))

    (is (= #{:request-id :principal :outcome :revision}
           (get-in analysis
                   [:definitely-established-before-state
                    :branch])))

    (is (= #{:outcome :revision}
           (:produced-value-keys analysis)))

    (is (= #{:request-id :principal :outcome}
           (:required-value-keys analysis)))

    (is (= #{:claim}
           (get-in analysis
                   [:value-producers
                    :outcome])))

    (is (= #{:claim}
           (get-in analysis
                   [:value-producers
                    :revision])))

    (is (= #{:branch}
           (get-in analysis
                   [:value-consumers
                    :outcome])))))

(deftest missing-authoritative-input-is-a-verification-error
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done
            {:requires #{:request-id :principal}
             :outputs #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-value-keys
          #{:request-id}})

        missing
        (filter
         #(and (= :value-not-definitely-established
                  (:kind %))
               (= :principal
                  (get-in % [:data :value-key])))
         (:errors result))]

    (is (false? (:valid? result)))
    (is (= 1 (count missing)))
    (is (= :claim
           (get-in (first missing)
                   [:data :state])))
    (is (= :principal
           (get-in (first missing)
                   [:data :value-key])))
    (is (= :authoritative
           (get-in (first missing)
                   [:data :op])))))

(deftest authoritative-output-satisfies-later-local-requirement
  (let [choreography
        (choreo/->choreography
         {:initial :load
          :states
          {:load
           (choreo/authoritative
            :server
            :request/read
            :render
            {:requires #{:request-id}
             :outputs #{:request}})

           :render
           (choreo/local
            :server
            :render-request
            :done
            {:requires #{:request}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-value-keys
          #{:request-id}})]

    (is (:valid? result))
    (is (= #{:request-id :request}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :render])))))

(deftest authoritative-output-must-exist-on-every-path-before-a-join-consumer
  (let [choreography
        (choreo/->choreography
         {:initial :choice
          :states
          {:choice
           (choreo/await
            :server
            {:environment/authoritative :claim
             :environment/skip :join})

           :claim
           (choreo/authoritative
            :server
            :request/claim
            :join
            {:outputs #{:outcome}})

           :join
           (choreo/local
            :server
            :render
            :done
            {:requires #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (false? (:valid? result)))
    (is (= #{:value-not-definitely-established}
           (problem-kinds (:errors result))))
    (is (= #{}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :join])))))

(deftest authoritative-output-on-every-path-is-definite-at-the-join
  (let [choreography
        (choreo/->choreography
         {:initial :choice
          :states
          {:choice
           (choreo/await
            :server
            {:environment/first :first
             :environment/second :second})

           :first
           (choreo/authoritative
            :server
            :request/first
            :join
            {:outputs #{:outcome}})

           :second
           (choreo/authoritative
            :server
            :request/second
            :join
            {:outputs #{:outcome}})

           :join
           (choreo/local
            :server
            :render
            :done
            {:requires #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))
    (is (= #{:outcome}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :join])))
    (is (= #{:first :second}
           (:authoritative-state-ids
            (:analysis result))))
    (is (= {:first :request/first
            :second :request/second}
           (:authoritative-operations-by-state
            (:analysis result))))))

(deftest verifier-identifies-authoritative-operation-without-claiming-authority-proof
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :done)

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)
        explanation
        (verify/explain result)]

    (is (:valid? result))
    (is (= #{:claim}
           (get-in result
                   [:analysis
                    :authoritative-state-ids])))
    (is (= {:claim :request/claim}
           (get-in result
                   [:analysis
                    :authoritative-operations-by-state])))
    (is (= 1
           (get-in explanation
                   [:states-by-op
                    :authoritative])))))

(deftest required-communicated-field-must-be-definitely-known-by-sender
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :request/settled
            :done
            {:required #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        sender-errors
        (filter
         #(and (= :value-not-definitely-established
                  (:kind %))
               (= :send
                  (get-in % [:data :state]))
               (= :outcome
                  (get-in % [:data :value-key])))
         (:errors result))]

    (is (false? (:valid? result)))

    (is (= 1
           (count sender-errors)))

    (is (= :communicate
           (get-in
            (first sender-errors)
            [:data :op])))))

(deftest authoritative-output-may-be-communicated-and-then-consumed-by-receiver
  (let [choreography
        (choreo/->choreography
         {:initial :claim
          :states
          {:claim
           (choreo/authoritative
            :server
            :request/claim
            :settle
            {:outputs #{:outcome}})

           :settle
           (choreo/communicate
            :server
            :browser
            :request/settled
            :branch
            {:required #{:outcome}})

           :branch
           (choreo/branch
            :browser
            :outcome
            {:confirmed :confirmed
             :rejected :rejected})

           :confirmed
           (choreo/local
            :browser
            :install-canonical
            :done)

           :rejected
           (choreo/local
            :browser
            :restore-snapshot
            :done)

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is
     (= #{:outcome}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :settle
                 :server])))

    (is
     (= #{:outcome}
        (get-in result
                [:analysis
                 :definitely-known-after-state
                 :settle
                 :browser])))

    (is
     (= #{:outcome}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :branch
                 :browser])))

    (is
     (= #{:settle}
        (get-in result
                [:analysis
                 :knowledge-producers-by-role
                 :browser
                 :outcome])))

    (is
     (= #{:branch}
        (get-in result
                [:analysis
                 :knowledge-consumers-by-role
                 :browser
                 :outcome])))))

(deftest protocol-value-existing-globally-does-not-let-unrelated-role-consume-it
  (let [choreography
        (choreo/->choreography
         {:initial :load
          :states
          {:load
           (choreo/authoritative
            :server
            :request/read
            :browser-render
            {:outputs #{:request}})

           :browser-render
           (choreo/local
            :browser
            :render-request
            :done
            {:requires #{:request}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        problems
        (filter
         #(= :knowledge-not-definitely-established
             (:kind %))
         (:errors result))]

    (is (false? (:valid? result)))

    (is (= 1
           (count problems)))

    (is (= :browser-render
           (get-in
            (first problems)
            [:data :state])))

    (is (= :browser
           (get-in
            (first problems)
            [:data :role])))

    (is (= :request
           (get-in
            (first problems)
            [:data :value-key])))

    (testing "the problem is distributed knowledge, not global value production"
      (is
       (= #{:request}
          (get-in result
                  [:analysis
                   :definitely-established-before-state
                   :browser-render]))))))

(deftest required-communication-establishes-definite-receiver-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :produce
          :states
          {:produce
           (choreo/local
            :server
            :produce-result
            :send
            {:outputs #{:outcome :revision}})

           :send
           (choreo/communicate
            :server
            :browser
            :request/settled
            :render
            {:required #{:outcome :revision}})

           :render
           (choreo/local
            :browser
            :render-result
            :done
            {:requires #{:outcome :revision}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is
     (= #{:outcome :revision}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :render
                 :browser])))

    (is
     (= #{:outcome :revision}
        (get-in result
                [:analysis
                 :communicated-required-keys-by-state
                 :send])))))

(deftest optional-communicated-field-is-not-definite-receiver-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :produce
          :states
          {:produce
           (choreo/local
            :server
            :produce-result
            :send
            {:outputs #{:outcome :revision}})

           :send
           (choreo/communicate
            :server
            :browser
            :request/settled
            :render
            {:required #{:outcome}
             :optional #{:revision}})

           :render
           (choreo/local
            :browser
            :render-result
            :done
            {:requires #{:revision}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        knowledge-errors
        (filter
         #(and (= :knowledge-not-definitely-established
                  (:kind %))
               (= :render
                  (get-in % [:data :state]))
               (= :browser
                  (get-in % [:data :role]))
               (= :revision
                  (get-in % [:data :value-key])))
         (:errors result))]

    (is (false? (:valid? result)))

    (is (= 1
           (count knowledge-errors)))

    (is
     (= #{:revision}
        (get-in result
                [:analysis
                 :communicated-optional-keys-by-state
                 :send])))

    (is
     (= #{:outcome}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :render
                 :browser])))))


(deftest required-environment-field-establishes-definite-value-and-role-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :render}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis :outcome}}}})

           :render
           (choreo/local
            :browser
            :render-canonical
            :done
            {:requires #{:basis :outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is (= #{:basis :outcome}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :render])))

    (is (= #{:basis :outcome}
           (get-in result
                   [:analysis
                    :definitely-known-before-state
                    :render
                    :browser])))))

(deftest await-event-contracts-produce-edge-specific-definite-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :canonical
             :request/failed :failed}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis}}

              :request/failed
              {:required #{:reason}}}})

           :canonical
           (choreo/local
            :browser
            :install-canonical
            :done
            {:requires #{:basis}})

           :failed
           (choreo/local
            :browser
            :recover
            :done
            {:requires #{:reason}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is (= #{:basis}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :canonical])))

    (is (= #{:reason}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :failed])))

    (is (= #{:basis}
           (get-in result
                   [:analysis
                    :definitely-known-before-state
                    :canonical
                    :browser])))

    (is (= #{:reason}
           (get-in result
                   [:analysis
                    :definitely-known-before-state
                    :failed
                    :browser])))))

(deftest optional-environment-field-is-not-definite-after-event
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :render}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis}
               :optional #{:revision}}}})

           :render
           (choreo/local
            :browser
            :render-canonical
            :done
            {:requires #{:revision}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (false? (:valid? result)))

    (is (contains?
         (problem-kinds (:errors result))
         :value-not-definitely-established))

    (testing "global absence is the root cause, so the verifier does not emit a
              redundant role-local knowledge error for the same missing value"
      (is (not
           (contains?
            (problem-kinds (:errors result))
            :knowledge-not-definitely-established))))

    (is (= #{:basis}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :render])))

    (is (= #{:basis}
           (get-in result
                   [:analysis
                    :definitely-known-before-state
                    :render
                    :browser])))))

(deftest environment-observation-establishes-knowledge-only-for-awaiting-role
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :server-use}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis}}}})

           :server-use
           (choreo/local
            :server
            :use-browser-observation
            :done
            {:requires #{:basis}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        knowledge-errors
        (filter
         #(= :knowledge-not-definitely-established
             (:kind %))
         (:errors result))]

    (is (false? (:valid? result)))

    (testing "the event did establish the protocol value globally"
      (is (= #{:basis}
             (get-in result
                     [:analysis
                      :definitely-established-before-state
                      :server-use]))))

    (testing "but it did not teach an unrelated role"
      (is (= 1 (count knowledge-errors)))
      (is (= :server-use
             (get-in (first knowledge-errors)
                     [:data :state])))
      (is (= :server
             (get-in (first knowledge-errors)
                     [:data :role])))
      (is (= :basis
             (get-in (first knowledge-errors)
                     [:data :value-key]))))))

(deftest await-path-that-does-not-establish-field-prevents-definite-join-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :join
             :request/failed :join}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis}}

              :request/failed
              {:required #{:reason}}}})

           :join
           (choreo/local
            :browser
            :after-either
            :done
            {:requires #{:basis}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (false? (:valid? result)))

    (is (= #{}
           (get-in result
                   [:analysis
                    :definitely-established-before-state
                    :join])))

    (is (= #{}
           (get-in result
                   [:analysis
                    :definitely-known-before-state
                    :join
                    :browser])))))

(deftest verifier-reports-environment-event-contract-analysis
  (let [choreography
        (choreo/->choreography
         {:initial :wait
          :states
          {:wait
           (choreo/await
            :browser
            {:canonical/observed :done
             :request/failed :done}
            {:event-contracts
             {:canonical/observed
              {:required #{:basis}
               :optional #{:revision}}

              :request/failed
              {:required #{:reason}
               :open-data? true}}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (is (= {:canonical/observed #{:basis}
            :request/failed #{:reason}}
           (get-in analysis
                   [:environment-required-keys-by-state
                    :wait])))

    (is (= {:canonical/observed #{:revision}
            :request/failed #{}}
           (get-in analysis
                   [:environment-optional-keys-by-state
                    :wait])))))

(deftest precise-entry-knowledge-is-role-local
  (let [choreography
        (choreo/->choreography
         {:initial :server-use
          :states
          {:server-use
           (choreo/local
            :server
            :prepare
            :browser-use
            {:requires #{:request-id}})

           :browser-use
           (choreo/local
            :browser
            :render
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-knowledge
          {:server #{:request-id}}})

        knowledge-errors
        (filter
         #(= :knowledge-not-definitely-established
             (:kind %))
         (:errors result))]

    (is (false? (:valid? result)))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :entry-knowledge
                 :server])))

    (is
     (= #{}
        (get-in result
                [:analysis
                 :entry-knowledge
                 :browser])))

    (is (= 1
           (count knowledge-errors)))

    (is (= :browser-use
           (get-in
            (first knowledge-errors)
            [:data :state])))))

(deftest broad-entry-value-keys-remain-known-to-every-role
  (let [choreography
        (choreo/->choreography
         {:initial :server-use
          :states
          {:server-use
           (choreo/local
            :server
            :prepare
            :browser-use
            {:requires #{:request-id}})

           :browser-use
           (choreo/local
            :browser
            :render
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-value-keys
          #{:request-id}})]

    (is (:valid? result))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :entry-knowledge
                 :server])))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :entry-knowledge
                 :browser])))))

(deftest precise-entry-knowledge-can-authorize-sender-dataflow-without-global-entry-option
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :request/selected
            :render
            {:required #{:request-id}})

           :render
           (choreo/local
            :browser
            :render-request
            :done
            {:requires #{:request-id}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify
         choreography
         {:entry-knowledge
          {:server #{:request-id}}})]

    (is (:valid? result))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :protocol-entry-value-keys])))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :send
                 :server])))

    (is
     (= #{:request-id}
        (get-in result
                [:analysis
                 :definitely-known-after-state
                 :send
                 :browser])))))

(deftest required-field-produced-on-only-one-path-is-not-safe-to-send-after-join
  (let [choreography
        (choreo/->choreography
         {:initial :choose
          :states
          {:choose
           (choreo/await
            :server
            {:environment/produce :produce
             :environment/skip :join})

           :produce
           (choreo/local
            :server
            :produce-result
            :join
            {:outputs #{:outcome}})

           :join
           (choreo/communicate
            :server
            :browser
            :request/settled
            :done
            {:required #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (false? (:valid? result)))

    (is
     (= #{:value-not-definitely-established}
        (problem-kinds
         (:errors result))))

    (is
     (= #{}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :join
                 :server])))))

(deftest required-field-produced-on-every-path-is-safe-to-send-after-join
  (let [choreography
        (choreo/->choreography
         {:initial :choose
          :states
          {:choose
           (choreo/await
            :server
            {:environment/first :first
             :environment/second :second})

           :first
           (choreo/local
            :server
            :produce-first
            :join
            {:outputs #{:outcome}})

           :second
           (choreo/local
            :server
            :produce-second
            :join
            {:outputs #{:outcome}})

           :join
           (choreo/communicate
            :server
            :browser
            :request/settled
            :done
            {:required #{:outcome}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    (is (:valid? result))

    (is
     (= #{:outcome}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :join
                 :server])))))

(deftest verifier-does-not-pretend-optional-sender-knowledge-is-proved
  (let [choreography
        (choreo/->choreography
         {:initial :send
          :states
          {:send
           (choreo/communicate
            :server
            :browser
            :example/open-result
            :done
            {:optional #{:debug-detail}
             :open-payload? true})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)]

    ;; This verifier proves required-field flow only. If the concrete sender
    ;; chooses to include :debug-detail, a later runtime/proof obligation must
    ;; establish that the sender is entitled to transmit that actual value.
    (is (:valid? result))

    (is
     (= #{}
        (get-in result
                [:analysis
                 :definitely-known-before-state
                 :send
                 :server])))

    (is
     (= #{:debug-detail}
        (get-in result
                [:analysis
                 :communicated-optional-keys-by-state
                 :send])))))

(deftest invalid-entry-knowledge-shape-is-rejected
  (is
   (= :invalid-entry-knowledge
      (:error/kind
       (verification-error
        #(verify/verify
          (choreo/->choreography
           {:initial :done
            :states
            {:done
             (choreo/return :done)}})
          {:entry-knowledge
           [:server #{:request-id}]})))))

  (is
   (= :invalid-entry-knowledge
      (:error/kind
       (verification-error
        #(verify/verify
          (choreo/->choreography
           {:initial :done
            :states
            {:done
             (choreo/return :done)}})
          {:entry-knowledge
           {"server" #{:request-id}}}))))))

;; -----------------------------------------------------------------------------
;; Authoritative observation: static knowledge/provenance distinction
;; -----------------------------------------------------------------------------

(def authoritative-reread-contract
  {:authority :request/model
   :observation :request/current-projection
   :basis-key :observed-basis})

(deftest authoritative-observation-is-an-explicit-static-authoritative-knowledge-source
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/reread-complete :consume}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :optional #{:display-label}
               :authoritative-observation
               authoritative-reread-contract}}})

           :consume
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status :observed-basis}})

           :done
           (choreo/return :done)}})

        result
        (verify/verify choreography)

        analysis
        (:analysis result)]

    (is (:valid? result))

    (testing "the verifier exposes the exact declared authoritative observation"
      (is (= authoritative-reread-contract
             (get-in analysis
                     [:authoritative-observations-by-state
                      :observe
                      :request/reread-complete]))))

    (testing "required reread fields are definite role-local knowledge"
      (is (= #{:request-status :observed-basis}
             (get-in analysis
                     [:definitely-known-before-state
                      :consume
                      :browser]))))

    (testing "the same required fields are definitely authoritative knowledge"
      (is (= #{:request-status :observed-basis}
             (get-in analysis
                     [:definitely-authoritatively-known-before-state
                      :consume
                      :browser]))))

    (testing "optional observation fields are not statically definite"
      (is (not
           (contains?
            (get-in analysis
                    [:definitely-authoritatively-known-before-state
                     :consume
                     :browser])
            :display-label))))))

(deftest ordinary-environment-observation-does-not-become-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:environment/current :consume}
            {:event-contracts
             {:environment/current
              {:required #{:request-status}}}})

           :consume
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (is (= #{:request-status}
           (get-in analysis
                   [:definitely-known-before-state
                    :consume
                    :browser])))

    (is (= #{}
           (get-in analysis
                   [:definitely-authoritatively-known-before-state
                    :consume
                    :browser])))

    (is (= {}
           (:authoritative-observations-by-state analysis)))))

(deftest authoritative-operation-output-is-definitely-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :read
          :states
          {:read
           (choreo/authoritative
            :server
            :request/read-current
            :consume
            {:outputs #{:request-status}})

           :consume
           (choreo/local
            :server
            :use-current
            :done
            {:requires #{:request-status}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (is (= #{:request-status}
           (get-in analysis
                   [:definitely-authoritatively-known-before-state
                    :consume
                    :server])))))

(deftest communication-establishes-communicated-not-authoritative-receiver-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :read
          :states
          {:read
           (choreo/authoritative
            :server
            :request/read-current
            :send
            {:outputs #{:request-status}})

           :send
           (choreo/communicate
            :server
            :browser
            :request/current
            :consume
            {:required #{:request-status}})

           :consume
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (is (= #{:request-status}
           (get-in analysis
                   [:definitely-known-before-state
                    :consume
                    :browser])))

    ;; The runtime records the receiver's acquisition as :communicated.  The
    ;; verifier must not silently preserve the sender's :authoritative provenance
    ;; kind across a participant-message boundary.
    (is (= #{}
           (get-in analysis
                   [:definitely-authoritatively-known-before-state
                    :consume
                    :browser])))))

(deftest authoritative-knowledge-at-a-join-is-a-must-property
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/reread-complete :join
             :environment/current :join}
            {:event-contracts
             {:request/reread-complete
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               authoritative-reread-contract}

              :environment/current
              {:required #{:request-status :observed-basis}}}})

           :join
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status :observed-basis}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (testing "the values are definitely known because both event edges require them"
      (is (= #{:request-status :observed-basis}
             (get-in analysis
                     [:definitely-known-before-state
                      :join
                      :browser]))))

    (testing "they are not definitely authoritative because one edge is ordinary"
      (is (= #{}
             (get-in analysis
                     [:definitely-authoritatively-known-before-state
                      :join
                      :browser]))))))

(deftest converging-authoritative-observations-preserve-definite-authoritative-knowledge
  (let [choreography
        (choreo/->choreography
         {:initial :observe
          :states
          {:observe
           (choreo/await
            :browser
            {:request/reread-primary :join
             :request/reread-secondary :join}
            {:event-contracts
             {:request/reread-primary
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               {:authority :request/model
                :observation :request/primary-projection
                :basis-key :observed-basis}}

              :request/reread-secondary
              {:required #{:request-status :observed-basis}
               :authoritative-observation
               {:authority :request/model
                :observation :request/secondary-projection
                :basis-key :observed-basis}}}})

           :join
           (choreo/local
            :browser
            :install-result
            :done
            {:requires #{:request-status :observed-basis}})

           :done
           (choreo/return :done)}})

        analysis
        (:analysis
         (verify/verify choreography))]

    (is (= #{:request-status :observed-basis}
           (get-in analysis
                   [:definitely-authoritatively-known-before-state
                    :join
                    :browser])))

    (is (= #{:request/reread-primary
             :request/reread-secondary}
           (set
            (keys
             (get-in analysis
                     [:authoritative-observations-by-state
                      :observe])))))))

;; -----------------------------------------------------------------------------
;; Verification-artifact integrity
;; -----------------------------------------------------------------------------

(defn- completed-choreography
  [name outcome]
  (choreo/->choreography
   {:name name
    :initial :done
    :states
    {:done
     (choreo/return outcome)}}))

(deftest verification-predicate-rejects-structurally-impossible-lookalikes
  (let [choreography
        (completed-choreography
         :example/verification-artifact-a
         :done)

        verification
        (verify/verify choreography)

        invalid-choreography
        {:not :a-choreography}]

    (is (verify/verification? verification))

    (testing "a result for this verifier version must carry the choreography it analyzed"
      (is (false?
           (verify/verification?
            (dissoc verification
                    :choreography))))

      (is (false?
           (verify/verification?
            (assoc verification
                   :choreography
                   invalid-choreography)))))

    (testing "a result emitted by verify always carries its normalized verifier options"
      (is (false?
           (verify/verification?
            (dissoc verification
                    :options)))))

    (testing ":valid? must agree with whether verifier errors are present"
      (is (false?
           (verify/verification?
            (assoc verification
                   :valid?
                   false))))

      (is (false?
           (verify/verification?
            (assoc verification
                   :errors
                   [(verify/problem
                     :fabricated
                     [:artifact]
                     "Fabricated verifier error." )])))))))

(deftest verified-wrapper-must-bind-the-exact-choreography-that-was-verified
  (let [choreography-a
        (completed-choreography
         :example/verified-artifact-a
         :a)

        choreography-b
        (completed-choreography
         :example/verified-artifact-b
         :b)

        verification-a
        (verify/verify choreography-a)

        verification-b
        (verify/verify choreography-b)

        verified-a
        (verify/verify! choreography-a)]

    (is (verify/verified? verified-a))

    (testing "changing only the wrapper choreography breaks the artifact binding"
      (is (false?
           (verify/verified?
            (assoc verified-a
                   :choreography
                   choreography-b)))))

    (testing "changing only the nested verification breaks the artifact binding"
      (is (false?
           (verify/verified?
            (assoc verified-a
                   :verification
                   verification-b)))))

    (testing "the nested verification is specifically for the wrapper choreography"
      (is (= choreography-a
             (:choreography verification-a)))
      (is (not= choreography-a
                (:choreography verification-b))))))

(deftest ensure-verified-rejects-malformed-tagged-verification-artifacts
  (let [choreography-a
        (completed-choreography
         :example/ensure-artifact-a
         :a)

        choreography-b
        (completed-choreography
         :example/ensure-artifact-b
         :b)

        verification
        (verify/verify choreography-a)

        verified
        (verify/verify! choreography-a)

        malformed
        [(dissoc verification
                 :choreography)

         (assoc verification
                :choreography
                {:not :a-choreography})

         (dissoc verification
                 :options)

         (assoc verified
                :choreography
                choreography-b)]]

    (doseq [artifact malformed]
      (let [error
            (verification-error
             #(verify/ensure-verified artifact))]
        (is (= :gesso.choreo.verify/error
               (:error/type error)))
        (is (= :invalid-verification-artifact
               (:error/kind error)))))))
