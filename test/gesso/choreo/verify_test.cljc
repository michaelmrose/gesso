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
