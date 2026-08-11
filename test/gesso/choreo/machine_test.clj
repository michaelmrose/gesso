(ns gesso.choreo.machine-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.machine :as machine]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- plan
  ([initial states]
   (plan :browser initial states nil))
  ([role initial states]
   (plan role initial states nil))
  ([role initial states opts]
   (merge
    {:name :test/machine
     :role role
     :initial initial
     :states states
     :resources {}
     :environment-events #{}}
    opts)))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f) ex-data))

(defn- thrown-message
  [f]
  (some-> (thrown f) ex-message))

(defn- timestamps
  [& values]
  (let [remaining (atom (vec values))]
    (fn []
      (let [value (first @remaining)]
        (swap! remaining subvec 1)
        value))))

(defn- trace-states
  [execution]
  (mapv :state
        (machine/execution-trace execution)))

(defn- trace-ops
  [execution]
  (mapv :op
        (machine/execution-trace execution)))

;; -----------------------------------------------------------------------------
;; Canonical fixture plans
;; -----------------------------------------------------------------------------

(def immediate-return-plan
  (plan
   :browser/done
   {:browser/done
    {:op :return
     :role :browser
     :outcome :done}}))

(def value-return-plan
  (plan
   :browser/done
   {:browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :answer}}))

(def fx-plan
  (plan
   :browser/load
   {:browser/load
    {:op :fx
     :role :browser
     :machine :test/load
     :input {:source :fixture}
     :next :browser/done}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :answer}}))

(def send-plan
  (plan
   :browser/send
   {:browser/send
    {:op :send
     :role :browser
     :to :server
     :event :command
     :via :http
     :required #{:execution-id :action}
     :optional #{:note :ignored-if-absent}
     :correlation #{:execution-id}
     :next :browser/done}

    :browser/done
    {:op :return
     :role :browser
     :outcome :sent}}))

(def event-await-plan
  (plan
   :browser/wait
   {:browser/wait
    {:op :await
     :role :browser
     :events
     {:online :browser/online
      :timeout :browser/timed-out}
     :bind :wake-data
     :receives []}

    :browser/online
    {:op :return
     :role :browser
     :outcome :online
     :value-key :wake-data}

    :browser/timed-out
    {:op :return
     :role :browser
     :outcome :timed-out}}))

(def message-await-plan
  (plan
   :browser/wait
   {:browser/wait
    {:op :await
     :role :browser
     :events {}
     :receives
     [{:from :server
       :to :browser
       :event :settled
       :via :sse
       :required #{:execution-id :outcome}
       :optional #{:message}
       :correlation #{:execution-id}
       :match {:outcome :confirmed}
       :bind :settlement
       :next :browser/done}]}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :settlement}}))

(def choice-plan
  (plan
   :browser/choose
   {:browser/choose
    {:op :choice
     :role :browser
     :key :path
     :branches
     {:left :browser/left
      :right :browser/right}}

    :browser/left
    {:op :return
     :role :browser
     :outcome :left}

    :browser/right
    {:op :return
     :role :browser
     :outcome :right}}))

(def resource-plan
  (plan
   :browser
   :browser/acquire
   {:browser/acquire
    {:op :acquire
     :role :browser
     :resource :target
     :next :browser/work}

    :browser/work
    {:op :fx
     :role :browser
     :machine :test/work
     :next :browser/release}

    :browser/release
    {:op :release
     :role :browser
     :resource :target
     :next :browser/done}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done}}
   {:resources
    {:target
     {:owner :browser
      :linear? true
      :terminal-release? true}}}))

;; -----------------------------------------------------------------------------
;; Runtime constants and minimal plan boundary
;; -----------------------------------------------------------------------------

(deftest runtime-identity-test
  (is (= :gesso.choreo.machine/execution
         machine/execution-type))
  (is (= #{:waiting-fx
           :waiting-send
           :suspended
           :completed}
         machine/execution-statuses))
  (is (= 64
         machine/default-trace-limit))
  (is (= 1024
         machine/default-max-immediate-steps)))

(deftest plan-predicate-test
  (is (machine/plan?
       immediate-return-plan))

  (testing "the runtime intentionally accepts only the minimal projected shape"
    (is (machine/plan?
         {:role :browser
          :initial :done
          :states
          {:done
           {:op :return
            :role :browser
            :outcome :done}}
          :resources {}})))

  (doseq [value
          [nil
           {}
           {:role "browser"
            :initial :done
            :states {:done {}}
            :resources {}}
           {:role :browser
            :initial :missing
            :states {:done {}}
            :resources {}}
           {:role :browser
            :initial :done
            :states []
            :resources {}}
           {:role :browser
            :initial :done
            :states {:done {}}
            :resources []}]]
    (is (false?
         (machine/plan? value)))))

(deftest start-rejects-invalid-plan-test
  (let [bad
        {:role :browser
         :initial :missing
         :states {}
         :resources {}}
        error
        (thrown
         #(machine/start bad {}))]
    (is (= "Expected a projected Gesso choreography plan."
           (ex-message error)))
    (is (= bad
           (:plan (ex-data error))))))

;; -----------------------------------------------------------------------------
;; Event and message envelopes
;; -----------------------------------------------------------------------------

(deftest event-envelope-construction-test
  (is (= {:kind :event
          :event :timeout
          :data nil}
         (machine/event :timeout)))

  (is (= {:kind :event
          :event :online
          :data {:attempt 2}}
         (machine/event
          :online
          {:attempt 2})))

  (is (machine/event-envelope?
       (machine/event :timeout)))

  (is (false?
       (machine/event-envelope?
        {:kind :event
         :event "timeout"}))))

(deftest event-requires-keyword-test
  (let [data
        (thrown-data
         #(machine/event "timeout"))]
    (is (= "timeout"
           (:event data)))))

(deftest message-envelope-construction-test
  (is (= {:kind :message
          :from :server
          :to :browser
          :event :settled
          :payload {:execution-id "e-1"}}
         (machine/message
          {:from :server
           :to :browser
           :event :settled}
          {:execution-id "e-1"})))

  (is (= {:kind :message
          :from :server
          :to :browser
          :event :settled
          :via :sse
          :payload {:execution-id "e-1"}}
         (machine/message
          {:from :server
           :to :browser
           :event :settled
           :via :sse}
          {:execution-id "e-1"})))

  (is (machine/event-envelope?
       (machine/message
        {:from :server
         :to :browser
         :event :settled}
        {}))))

(deftest message-validates-descriptor-and-payload-test
  (doseq [[descriptor payload expected-key]
          [[{:from "server"
             :to :browser
             :event :settled}
            {}
            :descriptor]

           [{:from :server
             :to "browser"
             :event :settled}
            {}
            :descriptor]

           [{:from :server
             :to :browser
             :event "settled"}
            {}
            :descriptor]

           [{:from :server
             :to :browser
             :event :settled
             :via "sse"}
            {}
            :descriptor]

           [{:from :server
             :to :browser
             :event :settled}
            [:not :a-map]
            :value]]]
    (let [data
          (thrown-data
           #(machine/message descriptor payload))]
      (is (contains? data expected-key)
          (str "Expected error data key " expected-key
               " for " descriptor " / " payload)))))

(deftest event-envelope-predicate-rejects-unrelated-values-test
  (doseq [value
          [nil
           {}
           {:kind :other}
           {:kind :message
            :from :server
            :to :browser
            :event :settled
            :payload nil}
           {:kind :message
            :from "server"
            :to :browser
            :event :settled
            :payload {}}]]
    (is (false?
         (machine/event-envelope? value)))))

;; -----------------------------------------------------------------------------
;; Execution predicates and accessors
;; -----------------------------------------------------------------------------

(deftest execution-predicates-test
  (let [completed
        (machine/start immediate-return-plan {})
        waiting-fx
        (machine/start fx-plan {})
        waiting-send
        (machine/start
         send-plan
         {:context
          {:execution-id "e-1"
           :action :claim}})
        suspended
        (machine/start event-await-plan {})]

    (is (machine/execution? completed))
    (is (machine/completed? completed))
    (is (false? (machine/waiting-fx? completed)))

    (is (machine/waiting-fx? waiting-fx))
    (is (machine/waiting-send? waiting-send))
    (is (machine/suspended? suspended))

    (is (false?
         (machine/execution?
          {:status :completed})))

    (is (false?
         (machine/execution?
          {:gesso.choreo.machine/type machine/execution-type
           :status :invented})))))

(deftest execution-accessors-test
  (let [execution
        (machine/start
         fx-plan
         {:execution-id "execution-1"
          :context {:existing 1}
          :now-fn (constantly :t0)})]

    (is (= {:existing 1}
           (machine/execution-context execution)))

    (is (= {}
           (machine/held-resources execution)))

    (is (= {:kind :fx
            :execution-id "execution-1"
            :state :browser/load
            :role :browser
            :machine :test/load
            :input {:source :fixture}}
           (machine/pending-action execution)))

    (is (= [{:state :browser/load
             :op :fx
             :at :t0
             :machine :test/load}]
           (machine/execution-trace execution)))

    (is (nil?
         (machine/execution-result execution)))))

(deftest accessors-reject-non-executions-test
  (doseq [accessor
          [machine/execution-context
           machine/execution-trace
           machine/held-resources
           machine/pending-action]]
    (is (thrown?
         clojure.lang.ExceptionInfo
         (accessor {})))))

;; -----------------------------------------------------------------------------
;; Starting and immediate advancement
;; -----------------------------------------------------------------------------

(deftest immediate-return-test
  (let [execution
        (machine/start
         immediate-return-plan
         {:execution-id "e-1"
          :context {:ignored true}
          :now-fn (constantly :t0)})]

    (is (machine/completed? execution))
    (is (= "e-1"
           (:execution-id execution)))
    (is (= :browser/done
           (:state execution)))
    (is (= {:ignored true}
           (machine/execution-context execution)))
    (is (= {:outcome :done}
           (machine/execution-result execution)))
    (is (= [:browser/done]
           (trace-states execution)))
    (is (= [:return]
           (trace-ops execution)))))

(deftest value-key-return-test
  (let [present
        (machine/start
         value-return-plan
         {:context {:answer 42}})
        absent
        (machine/start
         value-return-plan
         {:context {}})]

    (is (= 42
           (machine/execution-result present)))

    (testing "a value-key return is an ordinary context lookup"
      (is (nil?
           (machine/execution-result absent))))))

(deftest choice-advances-immediately-test
  (let [left
        (machine/start
         choice-plan
         {:context {:path :left}})
        right
        (machine/start
         choice-plan
         {:context {:path :right}})]

    (is (= {:outcome :left}
           (machine/execution-result left)))
    (is (= {:outcome :right}
           (machine/execution-result right)))

    (is (= [:browser/choose
            :browser/left]
           (trace-states left)))

    (is (= {:choice-key :path
            :choice-value :left}
           (select-keys
            (first (machine/execution-trace left))
            [:choice-key :choice-value])))))

(deftest choice-requires-context-value-test
  (let [data
        (thrown-data
         #(machine/start choice-plan {}))]
    (is (= :browser/choose
           (:state data)))
    (is (= :path
           (:choice-key data)))
    (is (= #{}
           (:available-keys data)))))

(deftest choice-requires-existing-branch-test
  (let [data
        (thrown-data
         #(machine/start
           choice-plan
           {:context {:path :center}}))]
    (is (= :browser/choose
           (:state data)))
    (is (= :path
           (:choice-key data)))
    (is (= :center
           (:choice-value data)))
    (is (= #{:left :right}
           (:branches data)))))

(deftest unknown-continuation-is-runtime-error-test
  (let [bad-plan
        (plan
         :browser/choose
         {:browser/choose
          {:op :choice
           :role :browser
           :key :path
           :branches {:go :missing}}})
        data
        (thrown-data
         #(machine/start
           bad-plan
           {:context {:path :go}}))]
    (is (= :browser
           (:role data)))
    (is (= :missing
           (:state data)))))

(deftest unsupported-runtime-operation-test
  (let [bad-plan
        (plan
         :browser/goto
         {:browser/goto
          {:op :goto
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}})
        data
        (thrown-data
         #(machine/start bad-plan {}))]
    (is (= :browser/goto
           (:state data)))
    (is (= :goto
           (:op data)))
    (is (= :browser
           (:role data)))))

(deftest immediate-step-limit-test
  (let [loop-plan
        (plan
         :browser/loop
         {:browser/loop
          {:op :choice
           :role :browser
           :key :again?
           :branches {true :browser/loop}}})
        data
        (thrown-data
         #(machine/start
           loop-plan
           {:context {:again? true}
            :max-immediate-steps 3}))]
    (is (= :browser
           (:role data)))
    (is (= :browser/loop
           (:state data)))
    (is (= 3
           (:max-immediate-steps data)))))

(deftest start-validates-options-test
  (doseq [[opts expected-label]
          [[{:context []}
            "Choreography execution context"]

           [{:trace-limit 0}
            "Choreography trace limit"]

           [{:trace-limit -1}
            "Choreography trace limit"]

           [{:max-immediate-steps 0}
            "Choreography max immediate steps"]

           [{:max-immediate-steps 1.5}
            "Choreography max immediate steps"]]]
    (let [data
          (thrown-data
           #(machine/start
             immediate-return-plan
             opts))]
      (is (= expected-label
             (:label data))))))

;; -----------------------------------------------------------------------------
;; FX boundary
;; -----------------------------------------------------------------------------

(deftest start-stops-at-fx-boundary-test
  (let [execution
        (machine/start
         fx-plan
         {:execution-id "fx-1"
          :context {:existing :value}
          :now-fn (constantly :t0)})]

    (is (machine/waiting-fx? execution))
    (is (= :browser/load
           (:state execution)))
    (is (= {:existing :value}
           (machine/execution-context execution)))
    (is (= :test/load
           (get-in execution [:action :machine])))
    (is (= {:source :fixture}
           (get-in execution [:action :input])))))

(deftest complete-fx-merges-result-and-continues-test
  (let [started
        (machine/start
         fx-plan
         {:context
          {:answer :old
           :preserved true}
          :now-fn
          (constantly :start)})

        completed
        (machine/complete-fx
         started
         {:answer 42
          :new-value :yes}
         {:now-fn
          (constantly :finish)})]

    (is (machine/completed? completed))
    (is (= {:answer 42
            :preserved true
            :new-value :yes}
           (machine/execution-context completed)))
    (is (= 42
           (machine/execution-result completed)))
    (is (= [:browser/load
            :browser/done]
           (trace-states completed)))
    (is (= [:start :finish]
           (mapv :at
                 (machine/execution-trace completed))))))

(deftest complete-fx-nil-preserves-context-test
  (let [started
        (machine/start
         fx-plan
         {:context {:answer 7}})
        completed
        (machine/complete-fx started nil)]

    (is (= {:answer 7}
           (machine/execution-context completed)))
    (is (= 7
           (machine/execution-result completed)))))

(deftest complete-fx-validates-status-and-result-test
  (is (thrown?
       clojure.lang.ExceptionInfo
       (machine/complete-fx
        (machine/start immediate-return-plan {})
        {})))

  (let [started
        (machine/start fx-plan {})
        data
        (thrown-data
         #(machine/complete-fx
           started
           [:not :a-map]))]
    (is (= (:execution-id started)
           (:execution-id data)))
    (is (= :browser/load
           (:state data)))
    (is (= [:not :a-map]
           (:result data)))))

;; -----------------------------------------------------------------------------
;; Send boundary
;; -----------------------------------------------------------------------------

(deftest start-stops-at-send-boundary-test
  (let [execution
        (machine/start
         send-plan
         {:execution-id "machine-execution"
          :context
          {:execution-id "wire-correlation"
           :action :claim
           :note "hello"
           :private-value :must-not-send}})

        action
        (machine/pending-action execution)]

    (is (machine/waiting-send? execution))
    (is (= :send
           (:kind action)))
    (is (= "machine-execution"
           (:execution-id action)))
    (is (= :browser/send
           (:state action)))
    (is (= :browser
           (:from action)))
    (is (= :server
           (:to action)))
    (is (= :command
           (:event action)))
    (is (= :http
           (:via action)))
    (is (= #{:execution-id :action}
           (:required action)))
    (is (= #{:note :ignored-if-absent}
           (:optional action)))
    (is (= #{:execution-id}
           (:correlation action)))

    (testing "only declared required/optional protocol fields leave the endpoint"
      (is (= {:execution-id "wire-correlation"
              :action :claim
              :note "hello"}
             (:payload action)))
      (is (not
           (contains?
            (:payload action)
            :private-value))))))

(deftest optional-send-values-are-not-required-test
  (let [execution
        (machine/start
         send-plan
         {:context
          {:execution-id "e-1"
           :action :claim}})]
    (is (= {:execution-id "e-1"
            :action :claim}
           (get-in execution [:action :payload])))))

(deftest send-requires-all-required-context-values-test
  (let [data
        (thrown-data
         #(machine/start
           send-plan
           {:context
            {:execution-id "e-1"}}))]
    (is (= :browser/send
           (:state data)))
    (is (= :command
           (:event data)))
    (is (= #{:action}
           (:missing data)))
    (is (= #{:execution-id :action}
           (:required data)))))

(deftest complete-send-continues-without-implying-remote-response-test
  (let [started
        (machine/start
         send-plan
         {:context
          {:execution-id "e-1"
           :action :claim}})
        completed
        (machine/complete-send started)]

    (is (machine/completed? completed))
    (is (= {:outcome :sent}
           (machine/execution-result completed)))
    (is (= {:execution-id "e-1"
            :action :claim}
           (machine/execution-context completed)))))

(deftest complete-send-requires-send-boundary-test
  (is (thrown?
       clojure.lang.ExceptionInfo
       (machine/complete-send
        (machine/start fx-plan {})))))

;; -----------------------------------------------------------------------------
;; Environmental await
;; -----------------------------------------------------------------------------

(deftest start-stops-at-await-boundary-test
  (let [execution
        (machine/start event-await-plan {})]

    (is (machine/suspended? execution))
    (is (= :browser/wait
           (:state execution)))
    (is (= {:events #{:online :timeout}
            :receives []}
           (:awaiting execution)))
    (is (nil?
         (machine/pending-action execution)))))

(deftest accepts-environment-event-test
  (let [execution
        (machine/start event-await-plan {})]

    (is (machine/accepts-event?
         execution
         (machine/event :online {:attempt 2})))

    (is (machine/accepts-event?
         execution
         (machine/event :timeout)))

    (is (false?
         (machine/accepts-event?
          execution
          (machine/event :offline))))

    (is (false?
         (machine/accepts-event?
          execution
          {:kind :bad})))))

(deftest resume-environment-event-binds-data-test
  (let [execution
        (machine/start event-await-plan {})
        completed
        (machine/resume
         execution
         (machine/event
          :online
          {:attempt 2}))]

    (is (machine/completed? completed))
    (is (= {:attempt 2}
           (get
            (machine/execution-context completed)
            :wake-data)))
    (is (= {:attempt 2}
           (machine/execution-result completed)))
    (is (= [:browser/wait
            :browser/online]
           (trace-states completed)))))

(deftest resume-event-without-value-key-returns-outcome-test
  (let [execution
        (machine/start event-await-plan {})
        completed
        (machine/resume
         execution
         (machine/event :timeout {:ignored true}))]

    (is (= {:outcome :timed-out}
           (machine/execution-result completed)))

    (testing "the await bind still records event data before terminal outcome"
      (is (= {:ignored true}
             (get
              (machine/execution-context completed)
              :wake-data))))))

(deftest unexpected-environment-event-is-rejected-test
  (let [execution
        (machine/start event-await-plan {})
        data
        (thrown-data
         #(machine/resume
           execution
           (machine/event :offline)))]

    (is (= (:execution-id execution)
           (:execution-id data)))
    (is (= :browser
           (:role data)))
    (is (= :browser/wait
           (:state data)))
    (is (= :offline
           (get-in data [:event :event])))
    (is (= #{:online :timeout}
           (get-in data [:awaiting :events])))))

;; -----------------------------------------------------------------------------
;; Participant-message await
;; -----------------------------------------------------------------------------

(deftest accepts-matching-message-test
  (let [execution
        (machine/start
         message-await-plan
         {:context
          {:execution-id "e-1"}})

        matching
        (machine/message
         {:from :server
          :to :browser
          :event :settled
          :via :sse}
         {:execution-id "e-1"
          :outcome :confirmed
          :message "ok"})]

    (is (machine/accepts-event?
         execution
         matching))))

(deftest message-matching-requires-identity-test
  (let [execution
        (machine/start
         message-await-plan
         {:context
          {:execution-id "e-1"}})

        payload
        {:execution-id "e-1"
         :outcome :confirmed}]

    (doseq [descriptor
            [{:from :other
              :to :browser
              :event :settled
              :via :sse}

             {:from :server
              :to :other
              :event :settled
              :via :sse}

             {:from :server
              :to :browser
              :event :other
              :via :sse}

             {:from :server
              :to :browser
              :event :settled
              :via :http}

             {:from :server
              :to :browser
              :event :settled}]]
      (is (false?
           (machine/accepts-event?
            execution
            (machine/message descriptor payload)))
          (str "Unexpectedly accepted descriptor " descriptor)))))

(deftest message-matching-requires-required-payload-test
  (let [execution
        (machine/start
         message-await-plan
         {:context
          {:execution-id "e-1"}})]

    (is (false?
         (machine/accepts-event?
          execution
          (machine/message
           {:from :server
            :to :browser
            :event :settled
            :via :sse}
           {:execution-id "e-1"}))))))

(deftest message-matching-requires-choice-match-test
  (let [execution
        (machine/start
         message-await-plan
         {:context
          {:execution-id "e-1"}})]

    (is (false?
         (machine/accepts-event?
          execution
          (machine/message
           {:from :server
            :to :browser
            :event :settled
            :via :sse}
           {:execution-id "e-1"
            :outcome :rejected}))))))

(deftest message-matching-requires-correlation-on-both-sides-test
  (let [descriptor
        {:from :server
         :to :browser
         :event :settled
         :via :sse}

        correct-payload
        {:execution-id "e-1"
         :outcome :confirmed}]

    (testing "different correlation values reject the message"
      (let [execution
            (machine/start
             message-await-plan
             {:context {:execution-id "e-1"}})]
        (is (false?
             (machine/accepts-event?
              execution
              (machine/message
               descriptor
               (assoc correct-payload
                      :execution-id "e-2")))))))

    (testing "missing correlation in execution context also rejects"
      (let [execution
            (machine/start
             message-await-plan
             {:context {}})]
        (is (false?
             (machine/accepts-event?
              execution
              (machine/message
               descriptor
               correct-payload))))))

    (testing "missing correlation in payload rejects"
      (let [execution
            (machine/start
             message-await-plan
             {:context {:execution-id "e-1"}})]
        (is (false?
             (machine/accepts-event?
              execution
              (machine/message
               descriptor
               {:outcome :confirmed}))))))))

(deftest resume-message-merges-payload-and-binds-whole-payload-test
  (let [execution
        (machine/start
         message-await-plan
         {:context
          {:execution-id "e-1"
           :local-only :preserved}})

        payload
        {:execution-id "e-1"
         :outcome :confirmed
         :message "done"
         :server-extra 9}

        completed
        (machine/resume
         execution
         (machine/message
          {:from :server
           :to :browser
           :event :settled
           :via :sse}
          payload))

        context
        (machine/execution-context completed)]

    (is (machine/completed? completed))

    (testing "payload fields become protocol context"
      (is (= "e-1"
             (:execution-id context)))
      (is (= :confirmed
             (:outcome context)))
      (is (= "done"
             (:message context)))
      (is (= 9
             (:server-extra context)))
      (is (= :preserved
             (:local-only context))))

    (testing "bind captures the complete message payload"
      (is (= payload
             (:settlement context)))
      (is (= payload
             (machine/execution-result completed))))))

(deftest ambiguous-message-match-is-runtime-error-test
  (let [ambiguous-plan
        (plan
         :browser/wait
         {:browser/wait
          {:op :await
           :role :browser
           :events {}
           :receives
           [{:from :server
             :to :browser
             :event :settled
             :required #{}
             :correlation #{}
             :match {}
             :next :browser/a}

            {:from :server
             :to :browser
             :event :settled
             :required #{}
             :correlation #{}
             :match {}
             :next :browser/b}]}

          :browser/a
          {:op :return
           :role :browser
           :outcome :a}

          :browser/b
          {:op :return
           :role :browser
           :outcome :b}})

        execution
        (machine/start ambiguous-plan {})

        envelope
        (machine/message
         {:from :server
          :to :browser
          :event :settled}
         {})

        data
        (thrown-data
         #(machine/accepts-event?
           execution
           envelope))]

    (is (= :browser/wait
           (:state data)))
    (is (= 2
           (count (:matches data))))))

;; -----------------------------------------------------------------------------
;; Resume boundary validation
;; -----------------------------------------------------------------------------

(deftest resume-requires-suspended-execution-test
  (doseq [execution
          [(machine/start immediate-return-plan {})
           (machine/start fx-plan {})
           (machine/start
            send-plan
            {:context
             {:execution-id "e-1"
              :action :claim}})]]
    (is (thrown?
         clojure.lang.ExceptionInfo
         (machine/resume
          execution
          (machine/event :timeout))))))

(deftest resume-requires-envelope-test
  (let [execution
        (machine/start event-await-plan {})
        data
        (thrown-data
         #(machine/resume
           execution
           {:not :an-envelope}))]

    (is (= {:not :an-envelope}
           (:event data)))))

(deftest completed-execution-cannot-be-resurrected-test
  (let [completed
        (machine/start immediate-return-plan {})]

    (is (thrown?
         clojure.lang.ExceptionInfo
         (machine/resume
          completed
          (machine/event :anything))))

    (is (thrown?
         clojure.lang.ExceptionInfo
         (machine/complete-fx completed {})))

    (is (thrown?
         clojure.lang.ExceptionInfo
         (machine/complete-send completed)))))

;; -----------------------------------------------------------------------------
;; Resource accounting
;; -----------------------------------------------------------------------------

(deftest acquire-holds-resource-before-external-boundary-test
  (let [execution
        (machine/start resource-plan {})]

    (is (machine/waiting-fx? execution))
    (is (= {:target 1}
           (machine/held-resources execution)))
    (is (= [:browser/acquire
            :browser/work]
           (trace-states execution)))))

(deftest release-removes-resource-before-terminal-test
  (let [started
        (machine/start resource-plan {})
        completed
        (machine/complete-fx started nil)]

    (is (machine/completed? completed))
    (is (= {}
           (machine/held-resources completed)))
    (is (= [:browser/acquire
            :browser/work
            :browser/release
            :browser/done]
           (trace-states completed)))))

(deftest unknown-acquire-resource-is-runtime-error-test
  (let [bad-plan
        (plan
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :missing
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}})
        data
        (thrown-data
         #(machine/start bad-plan {}))]

    (is (= :browser/acquire
           (:state data)))
    (is (= :missing
           (:resource data)))))

(deftest release-without-acquire-is-runtime-error-test
  (let [bad-plan
        (plan
         :browser
         :browser/release
         {:browser/release
          {:op :release
           :role :browser
           :resource :target
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:target
           {:owner :browser
            :linear? true}}})
        data
        (thrown-data
         #(machine/start bad-plan {}))]

    (is (= :browser/release
           (:state data)))
    (is (= :target
           (:resource data)))
    (is (= 0
           (:held data)))))

(deftest linear-resource-cannot-be-double-acquired-test
  (let [bad-plan
        (plan
         :browser
         :browser/acquire-1
         {:browser/acquire-1
          {:op :acquire
           :role :browser
           :resource :target
           :next :browser/acquire-2}

          :browser/acquire-2
          {:op :acquire
           :role :browser
           :resource :target
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:target
           {:owner :browser
            :linear? true}}})
        data
        (thrown-data
         #(machine/start bad-plan {}))]

    (is (= :browser/acquire-2
           (:state data)))
    (is (= :target
           (:resource data)))
    (is (= 1
           (:held data)))))

(deftest non-linear-resource-may-be-counted-test
  (let [counted-plan
        (plan
         :browser
         :browser/acquire-1
         {:browser/acquire-1
          {:op :acquire
           :role :browser
           :resource :counter
           :next :browser/acquire-2}

          :browser/acquire-2
          {:op :acquire
           :role :browser
           :resource :counter
           :next :browser/work}

          :browser/work
          {:op :fx
           :role :browser
           :machine :test/work
           :next :browser/release-1}

          :browser/release-1
          {:op :release
           :role :browser
           :resource :counter
           :next :browser/release-2}

          :browser/release-2
          {:op :release
           :role :browser
           :resource :counter
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:counter
           {:owner :browser
            :linear? false
            :terminal-release? false}}})

        started
        (machine/start counted-plan {})
        completed
        (machine/complete-fx started nil)]

    (is (= {:counter 2}
           (machine/held-resources started)))
    (is (= {}
           (machine/held-resources completed)))
    (is (machine/completed? completed))))

(deftest terminal-linear-resource-leak-is-rejected-test
  (let [leaky-plan
        (plan
         :browser
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :target
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:target
           {:owner :browser
            :linear? true}}})

        data
        (thrown-data
         #(machine/start leaky-plan {}))]

    (is (= :browser/done
           (:state data)))
    (is (= {:target 1}
           (:resources data)))))

(deftest terminal-release-false-allows-held-resource-at-completion-test
  (let [allowed-plan
        (plan
         :browser
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :lease
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:lease
           {:owner :browser
            :linear? true
            :terminal-release? false}}})

        completed
        (machine/start allowed-plan {})]

    (is (machine/completed? completed))
    (is (= {:lease 1}
           (machine/held-resources completed)))))

(deftest non-linear-resource-does-not-default-to-terminal-release-test
  (let [allowed-plan
        (plan
         :browser
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :counter
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:counter
           {:owner :browser
            :linear? false}}})

        completed
        (machine/start allowed-plan {})]

    (is (machine/completed? completed))
    (is (= {:counter 1}
           (machine/held-resources completed)))))

;; -----------------------------------------------------------------------------
;; Trace semantics
;; -----------------------------------------------------------------------------

(deftest trace-describes-boundaries-and-immediate-operations-test
  (let [trace-plan
        (plan
         :browser
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :target
           :next :browser/choose}

          :browser/choose
          {:op :choice
           :role :browser
           :key :path
           :branches {:work :browser/send}}

          :browser/send
          {:op :send
           :role :browser
           :to :server
           :event :command
           :required #{}
           :optional #{}
           :correlation #{}
           :next :browser/release}

          :browser/release
          {:op :release
           :role :browser
           :resource :target
           :next :browser/done}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:target
           {:owner :browser
            :linear? true}}})

        started
        (machine/start
         trace-plan
         {:context {:path :work}
          :now-fn
          (timestamps :t1 :t2 :t3)})

        completed
        (machine/complete-send
         started
         {:now-fn
          (timestamps :t4 :t5)})

        trace
        (machine/execution-trace completed)]

    (is (= [:acquire
            :choice
            :send
            :release
            :return]
           (mapv :op trace)))

    (is (= [:t1 :t2 :t3 :t4 :t5]
           (mapv :at trace)))

    (is (= {:resource :target}
           (select-keys
            (first trace)
            [:resource])))

    (is (= {:choice-key :path
            :choice-value :work}
           (select-keys
            (second trace)
            [:choice-key :choice-value])))

    (is (= {:event :command
            :to :server}
           (select-keys
            (nth trace 2)
            [:event :to])))

    (is (= {:outcome :done}
           (select-keys
            (last trace)
            [:outcome])))))

(deftest trace-limit-keeps-most-recent-entries-test
  (let [trace-plan
        (plan
         :browser
         :browser/acquire
         {:browser/acquire
          {:op :acquire
           :role :browser
           :resource :target
           :next :browser/release}

          :browser/release
          {:op :release
           :role :browser
           :resource :target
           :next :browser/choose}

          :browser/choose
          {:op :choice
           :role :browser
           :key :path
           :branches {:done :browser/done}}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done}}
         {:resources
          {:target
           {:owner :browser
            :linear? true}}})

        completed
        (machine/start
         trace-plan
         {:context {:path :done}
          :trace-limit 2
          :now-fn
          (timestamps :t1 :t2 :t3 :t4)})]

    (is (= 2
           (count
            (machine/execution-trace completed))))

    (is (= [:browser/choose
            :browser/done]
           (trace-states completed)))

    (is (= [:t3 :t4]
           (mapv :at
                 (machine/execution-trace completed))))))

;; -----------------------------------------------------------------------------
;; Boundary-to-boundary integration
;; -----------------------------------------------------------------------------

(deftest fx-send-await-return-roundtrip-test
  (let [roundtrip-plan
        (plan
         :browser/prepare
         {:browser/prepare
          {:op :fx
           :role :browser
           :machine :test/prepare
           :next :browser/send}

          :browser/send
          {:op :send
           :role :browser
           :to :server
           :event :command
           :via :http
           :required #{:execution-id :action}
           :optional #{}
           :correlation #{:execution-id}
           :next :browser/wait}

          :browser/wait
          {:op :await
           :role :browser
           :events
           {:request-failed :browser/failed}
           :receives
           [{:from :server
             :to :browser
             :event :settled
             :via :http
             :required #{:execution-id :outcome}
             :optional #{}
             :correlation #{:execution-id}
             :match {}
             :bind :settlement
             :next :browser/done}]}

          :browser/done
          {:op :return
           :role :browser
           :outcome :done
           :value-key :settlement}

          :browser/failed
          {:op :return
           :role :browser
           :outcome :request-failed}})

        fx-boundary
        (machine/start
         roundtrip-plan
         {:execution-id "machine-1"
          :context {:action :claim}})

        send-boundary
        (machine/complete-fx
         fx-boundary
         {:execution-id "wire-1"})

        suspended
        (machine/complete-send
         send-boundary)

        completed
        (machine/resume
         suspended
         (machine/message
          {:from :server
           :to :browser
           :event :settled
           :via :http}
          {:execution-id "wire-1"
           :outcome :confirmed}))]

    (is (machine/waiting-fx? fx-boundary))
    (is (machine/waiting-send? send-boundary))
    (is (= {:execution-id "wire-1"
            :action :claim}
           (get-in send-boundary
                   [:action :payload])))
    (is (machine/suspended? suspended))
    (is (machine/completed? completed))
    (is (= {:execution-id "wire-1"
            :outcome :confirmed}
           (machine/execution-result completed)))
    (is (= [:fx :send :await :return]
           (trace-ops completed)))))

(deftest await-environment-failure-path-remains-explicit-test
  (let [roundtrip-plan
        (plan
         :browser/send
         {:browser/send
          {:op :send
           :role :browser
           :to :server
           :event :command
           :required #{}
           :optional #{}
           :correlation #{}
           :next :browser/wait}

          :browser/wait
          {:op :await
           :role :browser
           :events
           {:request-failed :browser/failed}
           :receives []}

          :browser/failed
          {:op :return
           :role :browser
           :outcome :request-failed}})

        send-boundary
        (machine/start roundtrip-plan {})
        suspended
        (machine/complete-send send-boundary)
        completed
        (machine/resume
         suspended
         (machine/event
          :request-failed
          {:status 500}))]

    (is (= {:outcome :request-failed}
           (machine/execution-result completed)))
    (is (= [:send :await :return]
           (trace-ops completed)))))

;; -----------------------------------------------------------------------------
;; Runtime isolation guarantees
;; -----------------------------------------------------------------------------

(deftest machine-never-executes-fx-itself-test
  (let [execution
        (machine/start fx-plan {})]
    (is (machine/waiting-fx? execution))
    (is (= :test/load
           (:machine
            (machine/pending-action execution))))
    (is (= {}
           (machine/execution-context execution)))))

(deftest machine-never-performs-send-itself-test
  (let [execution
        (machine/start
         send-plan
         {:context
          {:execution-id "e-1"
           :action :claim}})]
    (is (machine/waiting-send? execution))
    (is (= :send
           (:kind
            (machine/pending-action execution))))))

(deftest accepts-event-does-not-resume-test
  (let [execution
        (machine/start
         event-await-plan
         {:context {:before true}})
        envelope
        (machine/event :online {:after true})]

    (is (machine/accepts-event?
         execution
         envelope))

    (testing "inspection is side-effect free"
      (is (machine/suspended? execution))
      (is (= {:before true}
             (machine/execution-context execution)))
      (is (= [:browser/wait]
             (trace-states execution))))))
