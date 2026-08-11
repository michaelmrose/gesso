(ns gesso.live.browser.choreo-test
  (:require
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.choreo :as choreo]
   [gesso.live.browser.fx :as fx]))

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
    {:name :test/browser-choreo
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
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f)
          ex-data))

(defn- with-runtime*
  [f]
  (choreo/reset-runtime!)
  (try
    (f)
    (finally
      (choreo/reset-runtime!))))

(defn- terminal-by-id
  [execution-id]
  (some
   #(when
      (= execution-id
         (:execution-id %))
      %)
   (choreo/terminal-summaries)))

(defn- only-active-summary
  []
  (let [summaries
        (choreo/active-summaries)]
    (is (= 1
           (count summaries)))
    (first summaries)))

(defn- registered-machine
  [machine-id result-fn]
  (choreo/register-fx-machine!
   machine-id
   (fn [ctx]
     (result-fn ctx))))

(defn- start-suspended!
  ([execution-id]
   (start-suspended!
    execution-id
    nil))
  ([execution-id opts]
   (choreo/start!
    (plan
     :browser/wait
     {:browser/wait
      {:op :await
       :role :browser
       :events
       {:continue :browser/done}
       :bind :resume-data
       :receives []}

      :browser/done
      {:op :return
       :role :browser
       :outcome :done
       :value-key :resume-data}})
    (merge
     {:execution-id execution-id}
     opts))))

;; -----------------------------------------------------------------------------
;; Fixture plans
;; -----------------------------------------------------------------------------

(def immediate-plan
  (plan
   :browser/done
   {:browser/done
    {:op :return
     :role :browser
     :outcome :done}}))

(def fx-plan
  (plan
   :browser/prepare
   {:browser/prepare
    {:op :fx
     :role :browser
     :machine :test/prepare
     :input {:phase :prepare}
     :next :browser/done}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :result}}))

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
     :optional #{:note}
     :correlation #{:execution-id}
     :next :browser/done}

    :browser/done
    {:op :return
     :role :browser
     :outcome :sent}}))

(def await-plan
  (plan
   :browser/wait
   {:browser/wait
    {:op :await
     :role :browser
     :events
     {:continue :browser/done
      :failed :browser/failed}
     :bind :resume-data
     :receives []}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :resume-data}

    :browser/failed
    {:op :return
     :role :browser
     :outcome :failed}}))

(def message-plan
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
       :match {}
       :bind :settlement
       :next :browser/done}]}

    :browser/done
    {:op :return
     :role :browser
     :outcome :done
     :value-key :settlement}}))

(def fx-send-await-plan
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
     :outcome :request-failed}}))

;; -----------------------------------------------------------------------------
;; Runtime identity and public context keys
;; -----------------------------------------------------------------------------

(deftest runtime-identity-test
  (is (= :gesso.live.browser.choreo/runtime
         choreo/runtime-type))

  (is (= :gesso.live.browser.choreo/execution-id
         choreo/execution-id-key))

  (is (= :gesso.live.browser.choreo/action
         choreo/action-key))

  (is (= :gesso.live.browser.choreo/metadata
         choreo/metadata-key))

  (is (= :gesso.live.browser.choreo/resume-envelope
         choreo/resume-envelope-key))

  (is (= 64
         choreo/default-terminal-history-limit)))

(deftest now-ms-is-number-test
  (is (number?
       (choreo/now-ms))))

;; -----------------------------------------------------------------------------
;; Error observer
;; -----------------------------------------------------------------------------

(deftest set-error-handler-test
  (with-runtime*
   (fn []
     (let [handler
           (fn [_payload]
             nil)]

       (is (true?
            (choreo/set-error-handler!
             handler)))

       (is (identical?
            handler
            @choreo/error-handler))

       (is (true?
            (choreo/set-error-handler!
             nil)))

       (is (nil?
            @choreo/error-handler))))))

(deftest invalid-error-handler-test
  (with-runtime*
   (fn []
     (let [data
           (thrown-data
            #(choreo/set-error-handler!
              :not-callable))]

       (is (= :gesso.live.browser.choreo/invalid-error-handler
              (:error/type data)))

       (is (= :not-callable
              (:handler data)))))))

(deftest error-observer-receives-start-failure-test
  (with-runtime*
   (fn []
     (let [seen
           (atom nil)]

       (choreo/set-error-handler!
        #(reset!
          seen
          %))

       (let [error
             (thrown
              #(choreo/start!
                fx-plan
                {:execution-id
                 "execution-1"}))]

         (is (some?
              error))

         (is (= :start
                (:operation @seen)))

         (is (= "execution-1"
                (:execution-id @seen)))

         (is (identical?
              error
              (:error @seen)))

         (is (= :test/browser-choreo
                (:plan-name @seen))))))))

(deftest throwing-error-observer-cannot-mask-original-error-test
  (with-runtime*
   (fn []
     (choreo/set-error-handler!
      (fn [_payload]
        (throw
         (js/Error.
          "observer exploded"))))

     (let [error
           (thrown
            #(choreo/start!
              fx-plan
              {:execution-id "execution-1"}))]

       (is (= :gesso.live.browser.choreo/missing-fx-machine
              (:error/type
               (ex-data error))))))))

;; -----------------------------------------------------------------------------
;; FX machine registration
;; -----------------------------------------------------------------------------

(deftest register-and-unregister-fx-machine-test
  (with-runtime*
   (fn []
     (let [machine-fn
           (fn [_ctx]
             {})]

       (is (= :test/machine
              (choreo/register-fx-machine!
               :test/machine
               machine-fn)))

       (is (= #{:test/machine}
              (choreo/registered-fx-machines)))

       (is (identical?
            machine-fn
            (choreo/fx-machine
             :test/machine)))

       (is (= :test/machine
              (choreo/unregister-fx-machine!
               :test/machine)))

       (is (= #{}
              (choreo/registered-fx-machines)))

       (is (nil?
            (choreo/fx-machine
             :test/machine)))))))

(deftest registering-fx-machine-replaces-existing-registration-test
  (with-runtime*
   (fn []
     (let [first-machine
           (fn [_ctx]
             {:value :first})

           second-machine
           (fn [_ctx]
             {:value :second})]

       (choreo/register-fx-machine!
        :test/machine
        first-machine)

       (choreo/register-fx-machine!
        :test/machine
        second-machine)

       (is (= #{:test/machine}
              (choreo/registered-fx-machines)))

       (is (identical?
            second-machine
            (choreo/fx-machine
             :test/machine)))))))

(deftest fx-machine-registration-validates-id-and-callability-test
  (with-runtime*
   (fn []
     (let [bad-id
           (thrown-data
            #(choreo/register-fx-machine!
              "not-keyword"
              (fn [_ctx]
                {})))

           bad-machine
           (thrown-data
            #(choreo/register-fx-machine!
              :test/machine
              :not-callable))]

       (is (= :gesso.live.browser.choreo/invalid-keyword
              (:error/type bad-id)))

       (is (= "Browser FX machine id"
              (:label bad-id)))

       (is (= :gesso.live.browser.choreo/invalid-callable
              (:error/type bad-machine)))

       (is (= "Browser FX machine"
              (:label bad-machine)))))))

;; -----------------------------------------------------------------------------
;; Browser FX handler registration
;; -----------------------------------------------------------------------------

(deftest register-and-unregister-fx-handler-test
  (with-runtime*
   (fn []
     (let [handler
           (fn [_ctx]
             :handled)]

       (is (= :test/handler
              (choreo/register-fx-handler!
               :test/handler
               handler)))

       (is (= #{:test/handler}
              (choreo/registered-fx-handlers)))

       (is (identical?
            handler
            (:test/handler
             (choreo/current-fx-handlers))))

       (is (= :test/handler
              (choreo/unregister-fx-handler!
               :test/handler)))

       (is (= {}
              (choreo/current-fx-handlers)))))))

(deftest registering-fx-handler-replaces-existing-registration-test
  (with-runtime*
   (fn []
     (let [first-handler
           (fn [_ctx]
             :first)

           second-handler
           (fn [_ctx]
             :second)]

       (choreo/register-fx-handler!
        :test/handler
        first-handler)

       (choreo/register-fx-handler!
        :test/handler
        second-handler)

       (is (identical?
            second-handler
            (:test/handler
             (choreo/current-fx-handlers))))))))

(deftest fx-handler-registration-validates-id-and-callability-test
  (with-runtime*
   (fn []
     (is (= :gesso.live.browser.choreo/invalid-keyword
            (:error/type
             (thrown-data
              #(choreo/register-fx-handler!
                "bad"
                (fn [_ctx]
                  nil))))))

     (is (= :gesso.live.browser.choreo/invalid-callable
            (:error/type
             (thrown-data
              #(choreo/register-fx-handler!
                :test/handler
                42))))))))

;; -----------------------------------------------------------------------------
;; Transport handoff registration
;; -----------------------------------------------------------------------------

(deftest send-handler-registration-test
  (with-runtime*
   (fn []
     (let [handler
           (fn [_action _execution]
             nil)]

       (is (true?
            (choreo/set-send-handler!
             handler)))

       (is (identical?
            handler
            (choreo/current-send-handler)))

       (is (true?
            (choreo/set-send-handler!
             nil)))

       (is (nil?
            (choreo/current-send-handler)))))))

(deftest invalid-send-handler-test
  (with-runtime*
   (fn []
     (let [data
           (thrown-data
            #(choreo/set-send-handler!
              :not-callable))]

       (is (= :gesso.live.browser.choreo/invalid-send-handler
              (:error/type data)))

       (is (= :not-callable
              (:handler data)))))))

;; -----------------------------------------------------------------------------
;; Start validation and execution ids
;; -----------------------------------------------------------------------------

(deftest execution-id-accepted-types-test
  (with-runtime*
   (fn []
     (doseq [execution-id
             [:execution/id
              (random-uuid)
              "execution-1"]]

       (let [execution
             (choreo/start!
              await-plan
              {:execution-id
               execution-id})]

         (is (= execution-id
                (:execution-id execution)))

         (is (choreo/active?
              execution-id))

         (choreo/abort!
          execution-id
          :test-cleanup))))))

(deftest invalid-execution-id-test
  (with-runtime*
   (fn []
     (doseq [execution-id
             [nil
              ""
              "   "
              42
              {}
              []]]

       (let [data
             (thrown-data
              #(choreo/start!
                await-plan
                {:execution-id
                 execution-id}))]

         (is (= :gesso.live.browser.choreo/invalid-execution-id
                (:error/type data)))

         (is (= execution-id
                (:execution-id data))))))))

(deftest metadata-must-be-map-test
  (with-runtime*
   (fn []
     (let [data
           (thrown-data
            #(choreo/start!
              await-plan
              {:execution-id "execution-1"
               :metadata
               [:not :a-map]}))]

       (is (= :gesso.live.browser.choreo/invalid-map
              (:error/type data)))

       (is (= "Browser choreography metadata"
              (:label data)))))))

(deftest duplicate-active-execution-id-is-rejected-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"})

     (let [data
           (thrown-data
            #(choreo/start!
              await-plan
              {:execution-id "execution-1"}))]

       (is (= :gesso.live.browser.choreo/duplicate-execution
              (:error/type data)))

       (is (= "execution-1"
              (:execution-id data)))))))

(deftest retired-execution-id-may-be-reused-test
  (with-runtime*
   (fn []
     (is (machine/completed?
          (choreo/start!
           immediate-plan
           {:execution-id
            "execution-1"})))

     (is (false?
          (choreo/active?
           "execution-1")))

     (let [second
           (choreo/start!
            await-plan
            {:execution-id
             "execution-1"})]

       (is (machine/suspended?
            second))

       (is (choreo/active?
            "execution-1"))))))

;; -----------------------------------------------------------------------------
;; Immediate completion and retirement
;; -----------------------------------------------------------------------------

(deftest immediate-completion-is-retired-test
  (with-runtime*
   (fn []
     (let [execution
           (choreo/start!
            immediate-plan
            {:execution-id "execution-1"
             :metadata
             {:source "button"
              :kind :request-action
              :private "not-diagnostic"}})]

       (is (machine/completed?
            execution))

       (is (false?
            (choreo/active?
             "execution-1")))

       (is (nil?
            (choreo/execution
             "execution-1")))

       (is (nil?
            (choreo/metadata
             "execution-1")))

       (is (nil?
            (choreo/execution-context
             "execution-1")))

       (let [terminal
             (terminal-by-id
              "execution-1")]

         (is (= :completed
                (:status terminal)))

         (is (= :done
                (get-in terminal
                        [:result
                         :outcome])))

         (is (= "button"
                (get-in terminal
                        [:metadata
                         :source])))

         (is (= :request-action
                (get-in terminal
                        [:metadata
                         :kind])))

         (is (not
              (contains?
               (:metadata terminal)
               :private)))

         (is (number?
              (:completed-at terminal))))))))

(deftest terminal-history-is-bounded-test
  (with-runtime*
   (fn []
     (dotimes [index
               (inc
                choreo/default-terminal-history-limit)]

       (choreo/start!
        immediate-plan
        {:execution-id
         (str "execution-" index)}))

     (let [history
           (choreo/terminal-summaries)]

       (is (= choreo/default-terminal-history-limit
              (count history)))

       (is (nil?
            (some
             #(= "execution-0"
                 (:execution-id %))
             history)))

       (is (some
            #(= (str
                 "execution-"
                 choreo/default-terminal-history-limit)
                (:execution-id %))
            history))))))

;; -----------------------------------------------------------------------------
;; Suspended execution storage and metadata
;; -----------------------------------------------------------------------------

(deftest suspended-start-is-committed-test
  (with-runtime*
   (fn []
     (with-redefs
      [choreo/now-ms
       (let [values
             (atom
              [100
               101
               102])]
         (fn []
           (let [value
                 (first @values)]
             (swap!
              values
              subvec
              1)
             value)))]

       (let [execution
             (choreo/start!
              await-plan
              {:execution-id "execution-1"
               :context
               {:request-id "request-1"}
               :metadata
               {:source "claim-button"
                :kind :request-action
                :extra "preserved while active"}})]

         (is (machine/suspended?
              execution))

         (is (identical?
              execution
              (choreo/execution
               "execution-1")))

         (is (= #{"execution-1"}
                (choreo/active-execution-ids)))

         (is (= 1
                (choreo/execution-count)))

         (is (= {:request-id "request-1"}
                (choreo/execution-context
                 "execution-1")))

         (is (= {:started-at 100
                 :updated-at 102
                 :source "claim-button"
                 :kind :request-action
                 :extra "preserved while active"}
                (choreo/metadata
                 "execution-1"))))))))

(deftest active-summary-is-dom-neutral-machine-summary-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"
       :context
       {:value 1}})

     (let [summary
           (only-active-summary)]

       (is (= "execution-1"
              (:execution-id summary)))

       (is (= :test/browser-choreo
              (:plan-name summary)))

       (is (= :browser
              (:role summary)))

       (is (= :suspended
              (:status summary)))

       (is (= :browser/wait
              (:state summary)))

       (is (nil?
            (:action summary)))

       (is (= {:events
               #{:continue :failed}
               :receives []}
              (:awaiting summary)))

       (is (= {}
              (:held-resources summary)))

       (is (nil?
            (:result summary)))

       (is (vector?
            (:trace summary)))))))

;; -----------------------------------------------------------------------------
;; FX boundary driving
;; -----------------------------------------------------------------------------

(deftest registered-fx-machine-is-run-automatically-test
  (with-runtime*
   (fn []
     (let [seen
           (atom nil)]

       (registered-machine
        :test/prepare
        (fn [ctx]
          (reset!
           seen
           ctx)

          {:result 42}))

       (let [execution
             (choreo/start!
              fx-plan
              {:execution-id "execution-1"
               :context
               {:request-id "request-1"}
               :metadata
               {:source "button"
                :kind :action}})]

         (is (machine/completed?
              execution))

         (is (= 42
                (machine/execution-result
                 execution)))

         (is (= "execution-1"
                (get
                 @seen
                 choreo/execution-id-key)))

         (is (= :fx
                (get-in
                 @seen
                 [choreo/action-key
                  :kind])))

         (is (= :test/prepare
                (get-in
                 @seen
                 [choreo/action-key
                  :machine])))

         (is (= {:phase :prepare}
                (get-in
                 @seen
                 [choreo/action-key
                  :input])))

         (is (= "button"
                (get-in
                 @seen
                 [choreo/metadata-key
                  :source])))

         (is (= "request-1"
                (:request-id @seen))))))))

(deftest registered-browser-fx-handlers-are-injected-into-local-machine-test
  (with-runtime*
   (fn []
     (choreo/register-fx-handler!
      :test/double
      (fn [_ctx value]
        (* 2
           value)))

     (choreo/register-fx-machine!
      :test/prepare
      (fx/machine
       :test/prepare-machine

       :start
       (fn [_ctx]
         {:result
          [:test/double 21]})))

     (let [execution
           (choreo/start!
            fx-plan
            {:execution-id
             "execution-1"})]

       (is (= 42
              (machine/execution-result
               execution)))))))

(deftest fx-machine-nil-result-is-empty-context-contribution-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [_ctx]
        nil))

     (let [execution
           (choreo/start!
            fx-plan
            {:execution-id
             "execution-1"
             :context
             {:result 7}})]

       (is (= 7
              (machine/execution-result
               execution)))))))

(deftest missing-fx-machine-fails-start-and-cleans-process-state-test
  (with-runtime*
   (fn []
     (let [data
           (thrown-data
            #(choreo/start!
              fx-plan
              {:execution-id "execution-1"
               :metadata
               {:source "button"}}))]

       (is (= :gesso.live.browser.choreo/missing-fx-machine
              (:error/type data)))

       (is (= "execution-1"
              (:execution-id data)))

       (is (= :browser/prepare
              (:state data)))

       (is (= :test/prepare
              (:machine data)))

       (is (= #{}
              (:registered data)))

       (is (false?
            (choreo/active?
             "execution-1")))

       (is (nil?
            (choreo/metadata
             "execution-1")))

       (is (= {}
              @choreo/timers))))))

(deftest fx-machine-must-return-map-or-nil-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [_ctx]
        [:not :a-map]))

     (let [data
           (thrown-data
            #(choreo/start!
              fx-plan
              {:execution-id
               "execution-1"}))]

       (is (= :gesso.live.browser.choreo/invalid-fx-result
              (:error/type data)))

       (is (= [:not :a-map]
              (:result data)))

       (is (false?
            (choreo/active?
             "execution-1")))))))

(deftest thrown-fx-machine-error-propagates-and-cleans-start-test
  (with-runtime*
   (fn []
     (let [cause
           (js/Error.
            "FX exploded")]

       (registered-machine
        :test/prepare
        (fn [_ctx]
          (throw cause)))

       (let [error
             (thrown
              #(choreo/start!
                fx-plan
                {:execution-id
                 "execution-1"}))]

         (is (identical?
              cause
              error))

         (is (false?
              (choreo/active?
               "execution-1")))

         (is (nil?
              (choreo/metadata
               "execution-1"))))))))

;; -----------------------------------------------------------------------------
;; Send boundary driving
;; -----------------------------------------------------------------------------

(deftest send-handler-is-run-automatically-test
  (with-runtime*
   (fn []
     (let [seen
           (atom nil)]

       (choreo/set-send-handler!
        (fn [action execution]
          (reset!
           seen
           {:action action
            :execution execution})))

       (let [execution
             (choreo/start!
              send-plan
              {:execution-id "machine-execution"
               :context
               {:execution-id "wire-execution"
                :action :claim
                :note "hello"
                :private "local"}})]

         (is (machine/completed?
              execution))

         (is (= {:outcome :sent}
                (machine/execution-result
                 execution)))

         (is (= {:execution-id "wire-execution"
                 :action :claim
                 :note "hello"}
                (get-in
                 @seen
                 [:action
                  :payload])))

         (is (= :send
                (get-in
                 @seen
                 [:action
                  :kind])))

         (is (= :server
                (get-in
                 @seen
                 [:action
                  :to])))

         (is (= :command
                (get-in
                 @seen
                 [:action
                  :event])))

         (is (= "machine-execution"
                (get-in
                 @seen
                 [:execution
                  :execution-id]))))))))

(deftest missing-send-handler-fails-start-and-cleans-state-test
  (with-runtime*
   (fn []
     (let [data
           (thrown-data
            #(choreo/start!
              send-plan
              {:execution-id "execution-1"
               :context
               {:execution-id "wire-1"
                :action :claim}}))]

       (is (= :gesso.live.browser.choreo/missing-send-handler
              (:error/type data)))

       (is (= "execution-1"
              (:execution-id data)))

       (is (= :browser/send
              (:state data)))

       (is (= :send
              (get-in
               data
               [:action
                :kind])))

       (is (false?
            (choreo/active?
             "execution-1")))

       (is (nil?
            (choreo/metadata
             "execution-1")))))))

(deftest thrown-send-handler-error-propagates-and-cleans-start-test
  (with-runtime*
   (fn []
     (let [cause
           (js/Error.
            "transport exploded")]

       (choreo/set-send-handler!
        (fn [_action _execution]
          (throw cause)))

       (let [error
             (thrown
              #(choreo/start!
                send-plan
                {:execution-id "execution-1"
                 :context
                 {:execution-id "wire-1"
                  :action :claim}}))]

         (is (identical?
              cause
              error))

         (is (false?
              (choreo/active?
               "execution-1"))))))))

;; -----------------------------------------------------------------------------
;; Automatic multi-boundary driving
;; -----------------------------------------------------------------------------

(deftest fx-and-send-are-driven-before-suspension-test
  (with-runtime*
   (fn []
     (let [fx-seen
           (atom nil)

           send-seen
           (atom nil)]

       (registered-machine
        :test/prepare
        (fn [ctx]
          (reset!
           fx-seen
           ctx)

          {:execution-id "wire-1"
           :action :claim}))

       (choreo/set-send-handler!
        (fn [action execution]
          (reset!
           send-seen
           {:action action
            :execution execution})))

       (let [execution
             (choreo/start!
              fx-send-await-plan
              {:execution-id "process-1"
               :metadata
               {:source "claim"}})]

         (is (machine/suspended?
              execution))

         (is (choreo/active?
              "process-1"))

         (is (= "process-1"
                (get
                 @fx-seen
                 choreo/execution-id-key)))

         (is (= {:execution-id "wire-1"
                 :action :claim}
                (get-in
                 @send-seen
                 [:action
                  :payload])))

         (is (= #{:request-failed}
                (get-in
                 execution
                 [:awaiting
                  :events])))

         (is (= 1
                (count
                 (get-in
                  execution
                  [:awaiting
                   :receives])))))))))

;; -----------------------------------------------------------------------------
;; Resume with environment events
;; -----------------------------------------------------------------------------

(deftest resume-event-completes-and-retires-execution-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"})

     (let [result
           (choreo/resume-event!
            "execution-1"
            :continue
            {:value 42})]

       (is (= :completed
              (:status result)))

       (is (machine/completed?
            (:execution result)))

       (is (= {:value 42}
              (:result result)))

       (is (false?
            (choreo/active?
             "execution-1")))

       (is (= {:value 42}
              (:result
               (terminal-by-id
                "execution-1"))))))))

(deftest resume-event-may-drive-new-fx-boundary-test
  (with-runtime*
   (fn []
     (let [event-fx-plan
           (plan
            :browser/wait
            {:browser/wait
             {:op :await
              :role :browser
              :events
              {:continue
               :browser/process}
              :bind :resume-data
              :receives []}

             :browser/process
             {:op :fx
              :role :browser
              :machine :test/process
              :next :browser/done}

             :browser/done
             {:op :return
              :role :browser
              :outcome :done
              :value-key :processed}})

           seen
           (atom nil)]

       (registered-machine
        :test/process
        (fn [ctx]
          (reset!
           seen
           ctx)

          {:processed
           (:resume-data ctx)}))

       (choreo/start!
        event-fx-plan
        {:execution-id
         "execution-1"})

       (let [result
             (choreo/resume-event!
              "execution-1"
              :continue
              {:value 42})]

         (is (= :completed
                (:status result)))

         (is (= {:value 42}
                (:result result)))

         (is (= {:value 42}
                (:resume-data @seen)))

         (is (= :event
                (get-in
                 @seen
                 [choreo/resume-envelope-key
                  :kind])))

         (is (= :continue
                (get-in
                 @seen
                 [choreo/resume-envelope-key
                  :event]))))))))

(deftest resume-envelope-is-visible-only-to-first-post-resume-fx-boundary-test
  (with-runtime*
   (fn []
     (let [two-fx-plan
           (plan
            :browser/wait
            {:browser/wait
             {:op :await
              :role :browser
              :events
              {:continue :browser/first}
              :receives []}

             :browser/first
             {:op :fx
              :role :browser
              :machine :test/first
              :next :browser/second}

             :browser/second
             {:op :fx
              :role :browser
              :machine :test/second
              :next :browser/done}

             :browser/done
             {:op :return
              :role :browser
              :outcome :done}})

           first-context
           (atom nil)

           second-context
           (atom nil)]

       (registered-machine
        :test/first
        (fn [ctx]
          (reset!
           first-context
           ctx)
          nil))

       (registered-machine
        :test/second
        (fn [ctx]
          (reset!
           second-context
           ctx)
          nil))

       (choreo/start!
        two-fx-plan
        {:execution-id
         "execution-1"})

       (choreo/resume-event!
        "execution-1"
        :continue
        {:value 42})

       (is (= :continue
              (get-in
               @first-context
               [choreo/resume-envelope-key
                :event])))

       (is (not
            (contains?
             @second-context
             choreo/resume-envelope-key)))))))

(deftest resume-invalid-event-is-error-but-keeps-active-execution-test
  (with-runtime*
   (fn []
     (let [seen
           (atom nil)]

       (choreo/set-error-handler!
        #(reset!
          seen
          %))

       (let [original
             (choreo/start!
              await-plan
              {:execution-id "execution-1"})

             error
             (thrown
              #(choreo/resume-event!
                "execution-1"
                :not-awaited))]

         (is (some?
              error))

         (is (= :resume
                (:operation @seen)))

         (is (= :browser/wait
                (:state @seen)))

         (is (= :not-awaited
                (get-in
                 @seen
                 [:event
                  :event])))

         (testing "resume failure does not destroy the suspended process"
           (is (choreo/active?
                "execution-1"))

           (is (identical?
                original
                (choreo/execution
                 "execution-1"))))

         (testing "a later valid modeled event may still complete it"
           (is (= :completed
                  (:status
                   (choreo/resume-event!
                    "execution-1"
                    :continue
                    {:ok true}))))

           (is (false?
                (choreo/active?
                 "execution-1")))))))))

;; -----------------------------------------------------------------------------
;; Participant messages
;; -----------------------------------------------------------------------------

(deftest accepts-message-test
  (with-runtime*
   (fn []
     (choreo/start!
      message-plan
      {:execution-id "process-1"
       :context
       {:execution-id
        "wire-1"}})

     (is (choreo/accepts-message?
          "process-1"
          {:from :server
           :to :browser
           :event :settled
           :via :sse}
          {:execution-id "wire-1"
           :outcome :confirmed}))

     (is (false?
          (choreo/accepts-message?
           "process-1"
           {:from :server
            :to :browser
            :event :settled
            :via :sse}
           {:execution-id "wire-2"
            :outcome :confirmed}))))))

(deftest resume-message-completes-and-binds-payload-test
  (with-runtime*
   (fn []
     (choreo/start!
      message-plan
      {:execution-id "process-1"
       :context
       {:execution-id "wire-1"}})

     (let [payload
           {:execution-id "wire-1"
            :outcome :confirmed
            :message "Claimed"}

           result
           (choreo/resume-message!
            "process-1"
            {:from :server
             :to :browser
             :event :settled
             :via :sse}
            payload)]

       (is (= :completed
              (:status result)))

       (is (= payload
              (:result result)))

       (is (= payload
              (:result
               (terminal-by-id
                "process-1"))))))))

(deftest transport-must-address-explicit-process-id-test
  (with-runtime*
   (fn []
     (choreo/start!
      message-plan
      {:execution-id "process-1"
       :context
       {:execution-id "wire-1"}})

     (choreo/start!
      message-plan
      {:execution-id "process-2"
       :context
       {:execution-id "wire-2"}})

     (testing "payload correlation cannot silently choose a different process"
       (is (thrown?
            cljs.core.ExceptionInfo
            (choreo/resume-message!
             "process-2"
             {:from :server
              :to :browser
              :event :settled
              :via :sse}
             {:execution-id "wire-1"
              :outcome :confirmed})))

       (is (choreo/active?
            "process-1"))

       (is (choreo/active?
            "process-2")))

     (is (= :completed
            (:status
             (choreo/resume-message!
              "process-1"
              {:from :server
               :to :browser
               :event :settled
               :via :sse}
              {:execution-id "wire-1"
               :outcome :confirmed})))))))

;; -----------------------------------------------------------------------------
;; accepts? inspection
;; -----------------------------------------------------------------------------

(deftest accepts-event-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (is (choreo/accepts-event?
          "execution-1"
          :continue))

     (is (choreo/accepts-event?
          "execution-1"
          :failed))

     (is (false?
          (choreo/accepts-event?
           "execution-1"
           :other))))))

(deftest accepts-is-false-for-inactive-execution-test
  (with-runtime*
   (fn []
     (is (false?
          (choreo/accepts?
           "missing"
           (machine/event
            :anything))))

     (is (false?
          (choreo/accepts-event?
           "missing"
           :anything)))

     (is (false?
          (choreo/accepts-message?
           "missing"
           {:from :server
            :to :browser
            :event :settled}
           {}))))))

(deftest accepts-does-not-resume-or-mutate-test
  (with-runtime*
   (fn []
     (let [execution
           (choreo/start!
            await-plan
            {:execution-id "execution-1"
             :context
             {:before true}})]

       (is (choreo/accepts?
            "execution-1"
            (machine/event
             :continue
             {:after true})))

       (is (identical?
            execution
            (choreo/execution
             "execution-1")))

       (is (= {:before true}
              (choreo/execution-context
               "execution-1")))))))

;; -----------------------------------------------------------------------------
;; Late delivery
;; -----------------------------------------------------------------------------

(deftest late-event-to-retired-execution-is-harmless-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (choreo/resume-event!
      "execution-1"
      :continue)

     (is (= {:status :ignored
             :reason :inactive-execution
             :execution-id "execution-1"}
            (choreo/resume-event!
             "execution-1"
             :continue)))

     (is (false?
          (choreo/active?
           "execution-1"))))))

(deftest late-message-to-retired-execution-is-harmless-test
  (with-runtime*
   (fn []
     (choreo/start!
      message-plan
      {:execution-id "process-1"
       :context
       {:execution-id
        "wire-1"}})

     (choreo/resume-message!
      "process-1"
      {:from :server
       :to :browser
       :event :settled
       :via :sse}
      {:execution-id "wire-1"
       :outcome :confirmed})

     (is (= {:status :ignored
             :reason :inactive-execution
             :execution-id "process-1"}
            (choreo/resume-message!
             "process-1"
             {:from :server
              :to :browser
              :event :settled
              :via :sse}
             {:execution-id "wire-1"
              :outcome :confirmed}))))))

;; -----------------------------------------------------------------------------
;; Abort semantics
;; -----------------------------------------------------------------------------

(deftest abort-active-execution-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"
       :metadata
       {:source "button"
        :kind :request-action
        :private "not-terminal"}})

     (is (= {:status :aborted
             :execution-id "execution-1"
             :reason :page-removed}
            (choreo/abort!
             "execution-1"
             :page-removed)))

     (is (false?
          (choreo/active?
           "execution-1")))

     (is (nil?
          (choreo/metadata
           "execution-1")))

     (let [terminal
           (terminal-by-id
            "execution-1")]

       (is (= :aborted
              (:status terminal)))

       (is (= :page-removed
              (:reason terminal)))

       (is (= :browser/wait
              (:state terminal)))

       (is (= "button"
              (get-in terminal
                      [:metadata
                       :source])))

       (is (not
            (contains?
             (:metadata terminal)
             :private)))))))

(deftest abort-inactive-execution-is-harmless-test
  (with-runtime*
   (fn []
     (is (= {:status :ignored
             :reason :inactive-execution
             :execution-id "missing"}
            (choreo/abort!
             "missing"
             :cleanup))))))

(deftest abort-validates-execution-id-test
  (with-runtime*
   (fn []
     (is (= :gesso.live.browser.choreo/invalid-execution-id
            (:error/type
             (thrown-data
              #(choreo/abort!
                nil
                :cleanup))))))))

;; -----------------------------------------------------------------------------
;; Timer ownership
;; -----------------------------------------------------------------------------

(deftest schedule-event-validates-input-test
  (with-runtime*
   (fn []
     (doseq [[args expected-type expected-label]
             [[[nil
                :timer
                1
                :continue]
               :gesso.live.browser.choreo/invalid-execution-id
               nil]

              [["execution-1"
                "timer"
                1
                :continue]
               :gesso.live.browser.choreo/invalid-keyword
               "Browser choreography timer key"]

              [["execution-1"
                :timer
                -1
                :continue]
               :gesso.live.browser.choreo/invalid-number
               "Browser choreography timer delay"]

              [["execution-1"
                :timer
                js/NaN
                :continue]
               :gesso.live.browser.choreo/invalid-number
               "Browser choreography timer delay"]

              [["execution-1"
                :timer
                1
                "continue"]
               :gesso.live.browser.choreo/invalid-keyword
               "Browser choreography timer event"]]]

       (let [data
             (thrown-data
              #(apply
                choreo/schedule-event!
                args))]

         (is (= expected-type
                (:error/type data)))

         (when expected-label
           (is (= expected-label
                  (:label data)))))))))

(deftest schedule-and-cancel-timer-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (let [handle
           (choreo/schedule-event!
            "execution-1"
            :timeout
            60000
            :continue
            {:late true})]

       (is (some?
            handle))

       (is (= handle
              (choreo/timer
               "execution-1"
               :timeout)))

       (is (= 1
              (get-in
               (choreo/diagnostics)
               [:timer-count])))

       (is (true?
            (choreo/cancel-timer!
             "execution-1"
             :timeout)))

       (is (nil?
            (choreo/timer
             "execution-1"
             :timeout)))

       (is (= {}
              @choreo/timers))))))

(deftest scheduling-same-timer-key-replaces-prior-handle-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (let [first-handle
           (choreo/schedule-event!
            "execution-1"
            :timeout
            60000
            :continue)

           second-handle
           (choreo/schedule-event!
            "execution-1"
            :timeout
            60000
            :failed)]

       (is (= second-handle
              (choreo/timer
               "execution-1"
               :timeout)))

       (is (= 1
              (count
               (get
                @choreo/timers
                "execution-1"))))

       ;; Browsers generally allocate distinct timeout handles. The registry
       ;; contract does not depend on that, so only make this a sanity check
       ;; when they are observably different.
       (when (not=
              first-handle
              second-handle)
         (is (not=
              first-handle
              (choreo/timer
               "execution-1"
               :timeout))))

       (choreo/cancel-all-timers!
        "execution-1")))))

(deftest cancel-all-timers-removes-only-owned-execution-timers-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (choreo/start!
      await-plan
      {:execution-id
       "execution-2"})

     (choreo/schedule-event!
      "execution-1"
      :one
      60000
      :continue)

     (choreo/schedule-event!
      "execution-1"
      :two
      60000
      :failed)

     (choreo/schedule-event!
      "execution-2"
      :other
      60000
      :continue)

     (is (true?
          (choreo/cancel-all-timers!
           "execution-1")))

     (is (nil?
          (get
           @choreo/timers
           "execution-1")))

     (is (= 1
            (count
             (get
              @choreo/timers
              "execution-2"))))

     (choreo/cancel-all-timers!
      "execution-2"))))

(deftest completion-cancels-owned-timers-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (choreo/schedule-event!
      "execution-1"
      :late
      60000
      :failed)

     (is (some?
          (choreo/timer
           "execution-1"
           :late)))

     (choreo/resume-event!
      "execution-1"
      :continue)

     (is (nil?
          (choreo/timer
           "execution-1"
           :late)))

     (is (= {}
            @choreo/timers)))))

(deftest abort-cancels-owned-timers-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (choreo/schedule-event!
      "execution-1"
      :late
      60000
      :continue)

     (choreo/abort!
      "execution-1"
      :cleanup)

     (is (= {}
            @choreo/timers)))))

(deftest scheduled-event-resumes-and-retires-execution-test
  (async done
    (choreo/reset-runtime!)

    (choreo/start!
     await-plan
     {:execution-id
      "execution-1"})

    (choreo/schedule-event!
     "execution-1"
     :continue
     0
     :continue
     {:timer true})

    (js/setTimeout
     (fn []
       (try
         (is (false?
              (choreo/active?
               "execution-1")))

         (is (= {}
                @choreo/timers))

         (is (= {:timer true}
                (:result
                 (terminal-by-id
                  "execution-1"))))

         (finally
           (choreo/reset-runtime!)
           (done))))
     20)))

;; -----------------------------------------------------------------------------
;; Timers scheduled by a local FX machine
;; -----------------------------------------------------------------------------

(deftest fx-machine-may-schedule-event-before-suspended-commit-test
  (async done
    (choreo/reset-runtime!)
    (let [timer-plan
          (plan
           :browser/schedule
           {:browser/schedule
            {:op :fx
             :role :browser
             :machine :test/schedule
             :next :browser/wait}

            :browser/wait
            {:op :await
             :role :browser
             :events
             {:timer-fired
              :browser/done}
             :bind :timer-data
             :receives []}

            :browser/done
            {:op :return
             :role :browser
             :outcome :done
             :value-key :timer-data}})]

      (registered-machine
       :test/schedule
       (fn [ctx]
         (choreo/schedule-event!
          (get
           ctx
           choreo/execution-id-key)
          :timer
          0
          :timer-fired
          {:from-fx true})
         nil))

      (choreo/start!
       timer-plan
       {:execution-id
        "execution-1"})

      (js/setTimeout
       (fn []
         (try
           (is (false?
                (choreo/active?
                 "execution-1")))

           (is (= {:from-fx true}
                  (:result
                   (terminal-by-id
                    "execution-1"))))

           (finally
             (choreo/reset-runtime!)
             (done))))
       20))))

(deftest failed-start-cleans-timers-scheduled-by-fx-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [ctx]
        (choreo/schedule-event!
         (get
          ctx
          choreo/execution-id-key)
         :late
         60000
         :never
         nil)

        (throw
         (js/Error.
          "fail after scheduling"))))

     (is (some?
          (thrown
           #(choreo/start!
             fx-plan
             {:execution-id
              "execution-1"}))))

     (is (= {}
            @choreo/timers))

     (is (false?
          (choreo/active?
           "execution-1"))))))

;; -----------------------------------------------------------------------------
;; Resume failures after timer scheduling
;; -----------------------------------------------------------------------------

(deftest resume-drive-error-is-reported-without-retiring-current-suspension-test
  (with-runtime*
   (fn []
     (let [post-event-fx-plan
           (plan
            :browser/wait
            {:browser/wait
             {:op :await
              :role :browser
              :events
              {:continue
               :browser/missing-fx}
              :receives []}

             :browser/missing-fx
             {:op :fx
              :role :browser
              :machine :test/missing
              :next :browser/done}

             :browser/done
             {:op :return
              :role :browser
              :outcome :done}})

           original
           (choreo/start!
            post-event-fx-plan
            {:execution-id
             "execution-1"})

           error
           (thrown
            #(choreo/resume-event!
              "execution-1"
              :continue))]

       (is (= :gesso.live.browser.choreo/missing-fx-machine
              (:error/type
               (ex-data error))))

       (testing "the pre-resume suspended execution remains active"
         (is (choreo/active?
              "execution-1"))

         (is (identical?
              original
              (choreo/execution
               "execution-1"))))))))

;; -----------------------------------------------------------------------------
;; Reset behavior
;; -----------------------------------------------------------------------------

(deftest reset-executions-preserves-registrations-test
  (with-runtime*
   (fn []
     (let [machine-fn
           (fn [_ctx]
             nil)

           handler
           (fn [_ctx]
             nil)

           sender
           (fn [_action _execution]
             nil)

           errors
           (fn [_payload]
             nil)]

       (choreo/register-fx-machine!
        :test/machine
        machine-fn)

       (choreo/register-fx-handler!
        :test/handler
        handler)

       (choreo/set-send-handler!
        sender)

       (choreo/set-error-handler!
        errors)

       (choreo/start!
        await-plan
        {:execution-id
         "execution-1"})

       (choreo/schedule-event!
        "execution-1"
        :late
        60000
        :continue)

       (is (true?
            (choreo/reset-executions!)))

       (is (= 0
              (choreo/execution-count)))

       (is (= {}
              @choreo/timers))

       (is (= []
              (choreo/terminal-summaries)))

       (is (identical?
            machine-fn
            (choreo/fx-machine
             :test/machine)))

       (is (identical?
            handler
            (:test/handler
             (choreo/current-fx-handlers))))

       (is (identical?
            sender
            (choreo/current-send-handler)))

       (is (identical?
            errors
            @choreo/error-handler))))))

(deftest reset-runtime-clears-everything-test
  (choreo/reset-runtime!)

  (choreo/register-fx-machine!
   :test/machine
   (fn [_ctx]
     nil))

  (choreo/register-fx-handler!
   :test/handler
   (fn [_ctx]
     nil))

  (choreo/set-send-handler!
   (fn [_action _execution]
     nil))

  (choreo/set-error-handler!
   (fn [_payload]
     nil))

  (choreo/start!
   await-plan
   {:execution-id
    "execution-1"})

  (choreo/schedule-event!
   "execution-1"
   :late
   60000
   :continue)

  (is (true?
       (choreo/reset-runtime!)))

  (is (= {}
         @choreo/executions))

  (is (= {}
         @choreo/execution-metadata))

  (is (= {}
         @choreo/fx-machines))

  (is (= {}
         @choreo/fx-handlers))

  (is (nil?
       @choreo/send-handler))

  (is (= {}
         @choreo/timers))

  (is (= []
         @choreo/terminal-history))

  (is (nil?
       @choreo/error-handler)))

;; -----------------------------------------------------------------------------
;; Diagnostics
;; -----------------------------------------------------------------------------

(deftest diagnostics-test
  (with-runtime*
   (fn []
     (choreo/register-fx-machine!
      :test/machine
      (fn [_ctx]
        nil))

     (choreo/register-fx-handler!
      :test/handler
      (fn [_ctx]
        nil))

     (choreo/set-send-handler!
      (fn [_action _execution]
        nil))

     (choreo/start!
      await-plan
      {:execution-id
       "active-1"})

     (choreo/start!
      immediate-plan
      {:execution-id
       "terminal-1"})

     (choreo/schedule-event!
      "active-1"
      :one
      60000
      :continue)

     (choreo/schedule-event!
      "active-1"
      :two
      60000
      :failed)

     (let [diagnostics
           (choreo/diagnostics)]

       (is (= choreo/runtime-type
              (:gesso.live.browser.choreo/type
               diagnostics)))

       (is (= 1
              (:active-count diagnostics)))

       (is (= 1
              (count
               (:active diagnostics))))

       (is (= 1
              (count
               (:terminal diagnostics))))

       (is (= #{:test/machine}
              (:registered-fx-machines
               diagnostics)))

       (is (= #{:test/handler}
              (:registered-fx-handlers
               diagnostics)))

       (is (true?
            (:send-handler?
             diagnostics)))

       (is (= 2
              (:timer-count
               diagnostics)))))))

;; -----------------------------------------------------------------------------
;; Metadata lifecycle
;; -----------------------------------------------------------------------------

(deftest metadata-is-visible-to-each-local-fx-boundary-test
  (with-runtime*
   (fn []
     (let [contexts
           (atom [])

           two-fx-plan
           (plan
            :browser/one
            {:browser/one
             {:op :fx
              :role :browser
              :machine :test/one
              :next :browser/two}

             :browser/two
             {:op :fx
              :role :browser
              :machine :test/two
              :next :browser/wait}

             :browser/wait
             {:op :await
              :role :browser
              :events
              {:done :browser/done}
              :receives []}

             :browser/done
             {:op :return
              :role :browser
              :outcome :done}})]

       (doseq [machine-id
               [:test/one
                :test/two]]

         (registered-machine
          machine-id
          (fn [ctx]
            (swap!
             contexts
             conj
             ctx)
            nil)))

       (choreo/start!
        two-fx-plan
        {:execution-id
         "execution-1"

         :metadata
         {:source "button"
          :kind :request-action
          :arbitrary {:kept true}}})

       (is (= 2
              (count
               @contexts)))

       (doseq [ctx
               @contexts]

         (is (= "button"
                (get-in
                 ctx
                 [choreo/metadata-key
                  :source])))

         (is (= :request-action
                (get-in
                 ctx
                 [choreo/metadata-key
                  :kind])))

         (is (= {:kept true}
                (get-in
                 ctx
                 [choreo/metadata-key
                  :arbitrary]))))))))

(deftest terminal-diagnostic-metadata-is-intentionally-narrower-than-active-metadata-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"
       :metadata
       {:source "button"
        :kind :request-action
        :arbitrary :private}})

     (is (= :private
            (:arbitrary
             (choreo/metadata
              "execution-1"))))

     (choreo/resume-event!
      "execution-1"
      :continue)

     (let [terminal
           (terminal-by-id
            "execution-1")]

       (is (= "button"
              (get-in
               terminal
               [:metadata
                :source])))

       (is (= :request-action
              (get-in
               terminal
               [:metadata
                :kind])))

       (is (not
            (contains?
             (:metadata terminal)
             :arbitrary)))))))

;; -----------------------------------------------------------------------------
;; Resource state survives browser-process suspension
;; -----------------------------------------------------------------------------

(deftest held-resources-remain-visible-while-suspended-test
  (with-runtime*
   (fn []
     (let [resource-plan
           (plan
            :browser/acquire
            {:browser/acquire
             {:op :acquire
              :role :browser
              :resource :target
              :next :browser/wait}

             :browser/wait
             {:op :await
              :role :browser
              :events
              {:release
               :browser/release}
              :receives []}

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
               :terminal-release? true}}})

           execution
           (choreo/start!
            resource-plan
            {:execution-id
             "execution-1"})]

       (is (= {:target 1}
              (machine/held-resources
               execution)))

       (is (= {:target 1}
              (:held-resources
               (only-active-summary))))

       (let [result
             (choreo/resume-event!
              "execution-1"
              :release)]

         (is (= :completed
                (:status result)))

         (is (= {}
                (machine/held-resources
                 (:execution result)))))))))

;; -----------------------------------------------------------------------------
;; Browser FX async contract
;; -----------------------------------------------------------------------------

(deftest local-fx-machine-may-schedule-but-must-return-synchronously-test
  (with-runtime*
   (fn []
     (let [scheduled?
           (atom false)]

       (registered-machine
        :test/prepare
        (fn [_ctx]
          (js/setTimeout
           #(reset!
             scheduled?
             true)
           0)

          {:result
           :synchronous}))

       (let [execution
             (choreo/start!
              fx-plan
              {:execution-id
               "execution-1"})]

         (is (= :synchronous
                (machine/execution-result
                 execution)))

         ;; The timer callback belongs to the browser event loop, not FX
         ;; completion. No assertion about its later value is necessary here.
         (is (boolean?
              @scheduled?)))))))

(deftest promise-return-from-local-fx-machine-is-invalid-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [_ctx]
        (js/Promise.resolve
         {:result 42})))

     (let [data
           (thrown-data
            #(choreo/start!
              fx-plan
              {:execution-id
               "execution-1"}))]

       (is (= :gesso.live.browser.choreo/invalid-fx-result
              (:error/type data)))

       (is (instance?
            js/Promise
            (:result data)))))))

;; -----------------------------------------------------------------------------
;; A complete browser process
;; -----------------------------------------------------------------------------

(deftest complete-fx-send-message-process-test
  (with-runtime*
   (fn []
     (let [fx-context
           (atom nil)

           sent
           (atom nil)

           payload
           {:execution-id "wire-1"
            :outcome :confirmed}]

       (registered-machine
        :test/prepare
        (fn [ctx]
          (reset!
           fx-context
           ctx)

          {:execution-id "wire-1"
           :action :claim}))

       (choreo/set-send-handler!
        (fn [action execution]
          (reset!
           sent
           {:action action
            :execution execution})))

       (let [started
             (choreo/start!
              fx-send-await-plan
              {:execution-id "process-1"
               :metadata
               {:source "claim-button"
                :kind :request-action}})]

         (is (machine/suspended?
              started))

         (is (= {:execution-id "wire-1"
                 :action :claim}
                (get-in
                 @sent
                 [:action
                  :payload])))

         (is (= "claim-button"
                (get-in
                 @fx-context
                 [choreo/metadata-key
                  :source])))

         (let [finished
               (choreo/resume-message!
                "process-1"
                {:from :server
                 :to :browser
                 :event :settled
                 :via :http}
                payload)]

           (is (= :completed
                  (:status finished)))

           (is (= payload
                  (:result finished)))

           (is (false?
                (choreo/active?
                 "process-1")))

           (is (= payload
                  (:result
                   (terminal-by-id
                    "process-1"))))))))))

;; -----------------------------------------------------------------------------
;; Failure path remains modeled
;; -----------------------------------------------------------------------------

(deftest complete-fx-send-environment-failure-process-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [_ctx]
        {:execution-id "wire-1"
         :action :claim}))

     (choreo/set-send-handler!
      (fn [_action _execution]
        nil))

     (let [started
           (choreo/start!
            fx-send-await-plan
            {:execution-id
             "process-1"})]

       (is (machine/suspended?
            started))

       (let [finished
             (choreo/resume-event!
              "process-1"
              :request-failed
              {:status 500})]

         (is (= :completed
                (:status finished)))

         (is (= {:outcome :request-failed}
                (:result finished))))))))

;; -----------------------------------------------------------------------------
;; Error observer on resume
;; -----------------------------------------------------------------------------

(deftest resume-error-observer-gets-current-state-and-envelope-test
  (with-runtime*
   (fn []
     (let [seen
           (atom nil)]

       (choreo/set-error-handler!
        #(reset!
          seen
          %))

       (choreo/start!
        await-plan
        {:execution-id
         "execution-1"})

       (let [envelope
             (machine/event
              :not-accepted
              {:x 1})

             error
             (thrown
              #(choreo/resume!
                "execution-1"
                envelope))]

         (is (some?
              error))

         (is (= :resume
                (:operation @seen)))

         (is (= "execution-1"
                (:execution-id @seen)))

         (is (= :browser/wait
                (:state @seen)))

         (is (= envelope
                (:event @seen)))

         (is (identical?
              error
              (:error @seen))))))))

;; -----------------------------------------------------------------------------
;; reset-executions cancels active timers but does not record abort history
;; -----------------------------------------------------------------------------

(deftest reset-executions-is-process-teardown-not-semantic-abort-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"})

     (choreo/schedule-event!
      "execution-1"
      :late
      60000
      :continue)

     (choreo/reset-executions!)

     (is (= 0
            (choreo/execution-count)))

     (is (= {}
            @choreo/timers))

     (is (= []
            (choreo/terminal-summaries))))))

;; -----------------------------------------------------------------------------
;; Diagnostics stay bounded and do not expose active metadata wholesale
;; -----------------------------------------------------------------------------

(deftest active-summary-does-not-embed-browser-metadata-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id
       "execution-1"

       :metadata
       {:source
        (js-obj
         "large"
         true)

        :kind
        :request-action}})

     (let [summary
           (only-active-summary)]

       (is (not
            (contains?
             summary
             :metadata)))

       (is (= :suspended
              (:status summary)))))))

;; -----------------------------------------------------------------------------
;; Terminal trace survives retirement
;; -----------------------------------------------------------------------------

(deftest terminal-summary-retains-machine-trace-test
  (with-runtime*
   (fn []
     (registered-machine
      :test/prepare
      (fn [_ctx]
        {:result 42}))

     (choreo/start!
      fx-plan
      {:execution-id
       "execution-1"})

     (let [terminal
           (terminal-by-id
            "execution-1")]

       (is (= [:fx
               :return]
              (mapv
               :op
               (:trace terminal))))

       (is (= :browser/prepare
              (get-in
               terminal
               [:trace
                0
                :state])))

       (is (= :browser/done
              (get-in
               terminal
               [:trace
                1
                :state])))))))

;; -----------------------------------------------------------------------------
;; Public lookup behavior for missing executions
;; -----------------------------------------------------------------------------

(deftest missing-execution-lookups-test
  (with-runtime*
   (fn []
     (is (nil?
          (choreo/execution
           "missing")))

     (is (false?
          (choreo/active?
           "missing")))

     (is (nil?
          (choreo/metadata
           "missing")))

     (is (nil?
          (choreo/execution-context
           "missing")))

     (is (nil?
          (choreo/timer
           "missing"
           :timer))))))

;; -----------------------------------------------------------------------------
;; Cancel operations are deliberately idempotent
;; -----------------------------------------------------------------------------

(deftest timer-cancellation-is-idempotent-test
  (with-runtime*
   (fn []
     (is (true?
          (choreo/cancel-timer!
           "missing"
           :timer)))

     (is (true?
          (choreo/cancel-all-timers!
           "missing")))

     (is (= {}
            @choreo/timers)))))

;; -----------------------------------------------------------------------------
;; Current registrations are process-global, not execution-local
;; -----------------------------------------------------------------------------

(deftest registration-changes-affect-subsequent-boundaries-test
  (with-runtime*
   (fn []
     (let [dynamic-plan
           (plan
            :browser/wait
            {:browser/wait
             {:op :await
              :role :browser
              :events
              {:continue
               :browser/process}
              :receives []}

             :browser/process
             {:op :fx
              :role :browser
              :machine :test/process
              :next :browser/done}

             :browser/done
             {:op :return
              :role :browser
              :outcome :done
              :value-key :value}})]

       (choreo/start!
        dynamic-plan
        {:execution-id
         "execution-1"})

       (registered-machine
        :test/process
        (fn [_ctx]
          {:value
           :registered-later}))

       (is (= :registered-later
              (:result
               (choreo/resume-event!
                "execution-1"
                :continue))))))))

;; -----------------------------------------------------------------------------
;; Multiple active executions remain isolated
;; -----------------------------------------------------------------------------

(deftest multiple-active-executions-are-isolated-test
  (with-runtime*
   (fn []
     (choreo/start!
      await-plan
      {:execution-id "execution-1"
       :context
       {:owner :one}})

     (choreo/start!
      await-plan
      {:execution-id "execution-2"
       :context
       {:owner :two}})

     (is (= #{"execution-1"
              "execution-2"}
            (choreo/active-execution-ids)))

     (choreo/resume-event!
      "execution-1"
      :continue
      {:done :one})

     (is (false?
          (choreo/active?
           "execution-1")))

     (is (choreo/active?
          "execution-2"))

     (is (= {:owner :two}
            (choreo/execution-context
             "execution-2")))

     (is (= :completed
            (:status
             (choreo/resume-event!
              "execution-2"
              :continue
              {:done :two})))))))

;; -----------------------------------------------------------------------------
;; Terminal result shape is exactly portable-machine result
;; -----------------------------------------------------------------------------

(deftest terminal-result-is-not-wrapped-by-browser-adapter-test
  (with-runtime*
   (fn []
     (let [value-plan
           (plan
            :browser/done
            {:browser/done
             {:op :return
              :role :browser
              :outcome :done
              :value-key :value}})

           value
           {:arbitrary
            [:portable
             :result]}

           execution
           (choreo/start!
            value-plan
            {:execution-id "execution-1"
             :context
             {:value value}})]

       (is (= value
              (machine/execution-result
               execution)))

       (is (= value
              (:result
               (terminal-by-id
                "execution-1"))))))))

;; -----------------------------------------------------------------------------
;; Browser adapter does not know DOM/HTMX/application semantics
;; -----------------------------------------------------------------------------

(deftest metadata-and-context-are-opaque-to-browser-choreography-adapter-test
  (with-runtime*
   (fn []
     (let [opaque-context
           {:source-element
            #js {:fake true}

            :application-command
            :request/claim

            :optimistic-target
            "closest [data-request-card]"}

           execution
           (choreo/start!
            await-plan
            {:execution-id "execution-1"
             :context
             opaque-context})]

       (is (= opaque-context
              (machine/execution-context
               execution)))

       (is (= opaque-context
              (choreo/execution-context
               "execution-1")))))))
