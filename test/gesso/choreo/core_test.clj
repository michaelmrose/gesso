(ns gesso.choreo.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.core :as choreo]))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def sample-resource
  (choreo/resource
   {:owner :browser
    :metadata {:purpose :test}}))

(def sample-choreography
  (choreo/->choreography
   {:name :test/request-action
    :roles [:browser :server]
    :initial :browser/prepare
    :resources
    {:target-authority sample-resource}
    :environment-events
    [:request-failed :timeout]
    :metadata
    {:suite :core}
    :states
    {:browser/prepare
     (choreo/fx
      :browser
      :browser/prepare
      :browser/acquire
      {:input {:mode :optimistic}
       :metadata {:phase :prepare}})

     :browser/acquire
     (choreo/acquire
      :browser
      :target-authority
      :browser/send)

     :browser/send
     (choreo/send
      :browser
      :server
      :command
      :server/receive
      {:via :http
       :required [:execution-id :action]
       :optional [:consistency-token]
       :correlation [:execution-id]
       :interrupts
       {:request-failed :browser/failed
        :timeout :browser/failed}
       :metadata {:phase :command}})

     :server/receive
     (choreo/receive
      :browser
      :server
      :command
      :server/decide
      {:via :http
       :bind :command})

     :server/decide
     (choreo/choice
      :server
      :accepted?
      {true :server/accepted
       false :server/rejected})

     :server/accepted
     (choreo/return
      :server
      :accepted)

     :server/rejected
     (choreo/return
      :server
      :rejected)

     :browser/failed
     (choreo/release
      :browser
      :target-authority
      :browser/return-failed)

     :browser/return-failed
     (choreo/return
      :browser
      :failed
      {:value-key :failure})}}))

(defn- thrown-data
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

;; -----------------------------------------------------------------------------
;; Choreography identity and normalization
;; -----------------------------------------------------------------------------

(deftest choreography-normalization-test
  (testing "plain choreography data receives stable runtime identity"
    (is (= choreo/choreography-type
           (:gesso.choreo/type sample-choreography)))
    (is (= choreo/choreography-version
           (:gesso.choreo/version sample-choreography)))
    (is (choreo/choreography? sample-choreography)))

  (testing "declared collections normalize to the compiler's canonical shapes"
    (is (= #{:browser :server}
           (:roles sample-choreography)))
    (is (= #{:request-failed :timeout}
           (:environment-events sample-choreography)))
    (is (map? (:states sample-choreography)))
    (is (map? (:resources sample-choreography))))

  (testing "missing optional collections normalize to empty values"
    (let [minimal
          (choreo/->choreography
           {:roles [:browser]
            :initial :done
            :states {:done (choreo/return :browser :done)}})]
      (is (= #{} (:environment-events minimal)))
      (is (= {} (:resources minimal)))
      (is (= {} (:metadata minimal)))))

  (testing "ensure-choreography preserves an already normalized value"
    (is (identical?
         sample-choreography
         (choreo/ensure-choreography sample-choreography))))

  (testing "ensure-choreography normalizes an ordinary map"
    (let [value
          (choreo/ensure-choreography
           {:roles #{:browser}
            :initial :done
            :states {:done {:op :return
                            :role :browser
                            :outcome :done}}})]
      (is (choreo/choreography? value))
      (is (= #{:browser} (:roles value))))))

(deftest choreography-normalization-rejects-invalid-container-shapes-test
  (testing ":roles must be set-like"
    (is (= "Choreography :roles must be a set or sequential collection."
           (try
             (choreo/->choreography
              {:roles :browser
               :states {}})
             nil
             (catch clojure.lang.ExceptionInfo error
               (ex-message error))))))

  (testing ":states must be a map"
    (is (= "Choreography :states must be a map."
           (try
             (choreo/->choreography
              {:roles #{:browser}
               :states []})
             nil
             (catch clojure.lang.ExceptionInfo error
               (ex-message error))))))

  (testing ":environment-events must be set-like"
    (is (= "Choreography :environment-events must be a set or sequential collection."
           (try
             (choreo/->choreography
              {:roles #{:browser}
               :states {}
               :environment-events :timeout})
             nil
             (catch clojure.lang.ExceptionInfo error
               (ex-message error)))))))

;; -----------------------------------------------------------------------------
;; Resource descriptors
;; -----------------------------------------------------------------------------

(deftest resource-constructor-test
  (testing "linear terminal-release authority is the default"
    (is (= {:linear? true
            :terminal-release? true
            :metadata {}}
           (choreo/resource))))

  (testing "owner and metadata are preserved"
    (is (= {:linear? true
            :terminal-release? true
            :owner :browser
            :metadata {:purpose :test}}
           sample-resource)))

  (testing "non-linear resources do not require terminal release by default"
    (is (= {:linear? false
            :terminal-release? false
            :metadata {}}
           (choreo/resource {:linear? false}))))

  (testing "terminal-release policy may be explicit"
    (is (= false
           (:terminal-release?
            (choreo/resource
             {:linear? true
              :terminal-release? false})))))

  (testing "resource owner must be a role keyword"
    (let [data
          (thrown-data
           #(choreo/resource {:owner "browser"}))]
      (is (= "Choreography role" (:label data)))
      (is (= "browser" (:value data))))))

;; -----------------------------------------------------------------------------
;; State constructors
;; -----------------------------------------------------------------------------

(deftest fx-state-constructor-test
  (is (= {:op :fx
          :role :browser
          :machine :browser/prepare
          :next :browser/send}
         (choreo/fx
          :browser
          :browser/prepare
          :browser/send)))

  (is (= {:op :fx
          :role :browser
          :machine :browser/prepare
          :next :browser/send
          :input {:x 1}
          :metadata {:phase :prepare}}
         (choreo/fx
          :browser
          :browser/prepare
          :browser/send
          {:input {:x 1}
           :metadata {:phase :prepare}}))))

(deftest communication-state-constructor-test
  (testing "send normalizes payload contracts and interrupts"
    (is (= {:op :send
            :from :browser
            :to :server
            :event :command
            :next :server/receive
            :via :http
            :required #{:execution-id :action}
            :optional #{:consistency-token}
            :correlation #{:execution-id}
            :interrupts
            {:request-failed :browser/failed
             :timeout :browser/failed}
            :metadata {:phase :command}}
           (choreo/state sample-choreography :browser/send))))

  (testing "receive preserves participant identity, transport, and binding"
    (is (= {:op :receive
            :from :browser
            :to :server
            :event :command
            :next :server/decide
            :via :http
            :bind :command}
           (choreo/state sample-choreography :server/receive))))

  (testing "minimal communication states stay minimal"
    (is (= {:op :send
            :from :a
            :to :b
            :event :ping
            :next :receive}
           (choreo/send :a :b :ping :receive)))
    (is (= {:op :receive
            :from :a
            :to :b
            :event :ping
            :next :done}
           (choreo/receive :a :b :ping :done)))))

(deftest choice-and-await-constructor-test
  (is (= {:op :choice
          :role :server
          :key :accepted?
          :branches
          {true :accepted
           false :rejected}}
         (choreo/choice
          :server
          :accepted?
          {true :accepted
           false :rejected})))

  (is (= {:op :await
          :role :browser
          :events
          {:timeout :failed
           :settled :done}
          :bind :event-value}
         (choreo/await
          :browser
          {:timeout :failed
           :settled :done}
          {:bind :event-value}))))

(deftest resource-and-terminal-state-constructor-test
  (is (= {:op :acquire
          :role :browser
          :resource :target
          :next :work}
         (choreo/acquire :browser :target :work)))

  (is (= {:op :release
          :role :browser
          :resource :target
          :next :done}
         (choreo/release :browser :target :done)))

  (is (= {:op :goto
          :next :done}
         (choreo/goto :done)))

  (is (= {:op :return
          :role :browser
          :outcome :done}
         (choreo/return :browser :done)))

  (is (= {:op :return
          :role :browser
          :outcome :done
          :value-key :result
          :metadata {:terminal true}}
         (choreo/return
          :browser
          :done
          {:value-key :result
           :metadata {:terminal true}}))))

(deftest constructors-reject-non-keyword-semantic-identities-test
  (doseq [[label thunk]
          [["role"
            #(choreo/fx "browser" :machine :next)]
           ["machine"
            #(choreo/fx :browser "machine" :next)]
           ["next"
            #(choreo/goto "next")]
           ["event"
            #(choreo/send :browser :server "command" :next)]
           ["bind"
            #(choreo/receive
              :browser
              :server
              :command
              :next
              {:bind "command"})]
           ["resource"
            #(choreo/acquire :browser "target" :next)]
           ["return value-key"
            #(choreo/return :browser :done {:value-key "result"})]]]
    (testing label
      (is (thrown? clojure.lang.ExceptionInfo
                   (thunk))))))

;; -----------------------------------------------------------------------------
;; Graph inspection
;; -----------------------------------------------------------------------------

(deftest graph-inspection-test
  (testing "state and state ids expose declared graph data"
    (is (= :fx
           (choreo/state-op
            (choreo/state sample-choreography :browser/prepare))))
    (is (nil?
         (choreo/state sample-choreography :missing)))
    (is (= (set (keys (:states sample-choreography)))
           (choreo/state-ids sample-choreography))))

  (testing "roles, resources, and environment events expose canonical collections"
    (is (= #{:browser :server}
           (choreo/roles sample-choreography)))
    (is (= {:target-authority sample-resource}
           (choreo/resource-descriptors sample-choreography)))
    (is (= #{:request-failed :timeout}
           (choreo/environment-events sample-choreography))))

  (testing "terminal and communication predicates classify by operation"
    (is (choreo/terminal-state?
         (choreo/state sample-choreography :server/accepted)))
    (is (false?
         (choreo/terminal-state?
          (choreo/state sample-choreography :browser/send))))
    (is (choreo/communication-state?
         (choreo/state sample-choreography :browser/send)))
    (is (choreo/communication-state?
         (choreo/state sample-choreography :server/receive)))
    (is (false?
         (choreo/communication-state?
          (choreo/state sample-choreography :server/decide)))))

  (testing "state-role reflects the participant that acts locally"
    (is (= :browser
           (choreo/state-role
            (choreo/state sample-choreography :browser/send))))
    (is (= :server
           (choreo/state-role
            (choreo/state sample-choreography :server/receive))))
    (is (= :server
           (choreo/state-role
            (choreo/state sample-choreography :server/decide))))
    (is (nil?
         (choreo/state-role
          (choreo/goto :server/accepted)))))

  (testing "successors include next, branches, and environmental interrupts"
    (is (= #{:server/receive
            :browser/failed}
           (choreo/successors
            (choreo/state sample-choreography :browser/send))))
    (is (= #{:server/accepted
            :server/rejected}
           (choreo/branch-successors
            (choreo/state sample-choreography :server/decide))))
    (is (= #{:browser/failed}
           (choreo/interrupt-successors
            (choreo/state sample-choreography :browser/send))))
    (is (= #{}
           (choreo/successors
            (choreo/state sample-choreography :server/accepted))))))

(deftest explain-test
  (is (= {:name :test/request-action
          :version choreo/choreography-version
          :roles #{:browser :server}
          :initial :browser/prepare
          :state-count 9
          :states-by-op
          {:fx 1
           :acquire 1
           :send 1
           :receive 1
           :choice 1
           :return 3
           :release 1}
          :resources #{:target-authority}
          :environment-events #{:request-failed :timeout}}
         (choreo/explain sample-choreography))))
