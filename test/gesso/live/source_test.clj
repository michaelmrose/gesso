(ns gesso.live.source-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.progression :as progression]
   [gesso.live.source :as source]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def timeout-ms 1000)

(def invalidation-1
  {:topic :request
   :id "req-1"
   :change/kind :updated})

(def invalidation-2
  {:topic :store-queue
   :id "store-1"
   :change/kind :updated})

(def invalid-invalidation
  {:topic :request})

(defn take-value
  [stream]
  (deref (s/take! stream) timeout-ms ::timeout))

(defn sleep-briefly
  []
  (Thread/sleep 25))

;; -----------------------------------------------------------------------------
;; Creation and stats
;; -----------------------------------------------------------------------------

(deftest create-builds-open-source-test
  (let [src (source/create {:id :test/source})]
    (try
      (let [stats (source/stats src)]
        (is (= :test/source (:id stats)))
        (is (= false (:closed? stats)))
        (is (= 0 (:tap-count stats)))
        (is (= 0 (:emitted-count stats)))
        (is (= 0 (:attempted-count stats)))
        (is (= 0 (:error-count stats))))
      (finally
        (source/close! src)))))

(deftest create-generates-id-when-missing-test
  (let [src (source/create)]
    (try
      (is (some? (:id (source/stats src))))
      (finally
        (source/close! src)))))

(deftest create-preserves-coalesce-window-metadata-test
  (let [src (source/create {:coalesce-window-ms 50})]
    (try
      (is (= 50 (:coalesce-window-ms (source/stats src))))
      (finally
        (source/close! src)))))

(deftest create-rejects-invalid-options-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (source/create {:coalesce-window-ms -1}))))

(deftest create-rejects-invalid-on-error-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid gesso.live value"
       (source/create {:on-error :not-a-function}))))

;; -----------------------------------------------------------------------------
;; Lifecycle
;; -----------------------------------------------------------------------------

(deftest close-marks-source-closed-test
  (let [src (source/create)]
    (is (= :closed (source/close! src)))
    (is (source/closed? src))))

(deftest close-is-idempotent-test
  (let [src (source/create)]
    (is (= :closed (source/close! src)))
    (is (= :closed (source/close! src)))
    (is (source/closed? src))))

(deftest changes-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/changes src)))))

(deftest emit-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/emit! src invalidation-1)))))

(deftest close-closes-active-taps-test
  (let [src (source/create)
        tap (source/changes src)]
    (source/close! src)
    (is (s/closed? tap))
    (is (= 0 (:tap-count (source/stats src))))))

(deftest subscribe-rejects-invalid-interest-before-registration-test
  ;; A direct source subscriber must satisfy the same admitted interest
  ;; contract as a flow subscriber. An invalid topic/id would otherwise
  ;; register successfully but could never match a valid invalidation.
  (let [src (source/create)]
    (try
      (doseq [subscription [{:topic nil :id "req-1"}
                            {:topic "request" :id "req-1"}
                            {:topic :request :id nil}]]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"Invalid gesso.live value"
             (source/subscribe src subscription))
            (str "Invalid subscription must be rejected: " (pr-str subscription)))
        (is (zero? (:subscription-count (source/stats src)))
            "A rejected subscription must not install an indexed tap"))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Taps
;; -----------------------------------------------------------------------------

(deftest changes-registers-tap-test
  (let [src (source/create)]
    (try
      (let [tap (source/changes src)]
        (is (some? tap))
        (is (= 1 (:tap-count (source/stats src)))))
      (finally
        (source/close! src)))))

(deftest closing-tap-removes-it-from-source-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (is (= 1 (:tap-count (source/stats src))))
      (s/close! tap)
      (sleep-briefly)
      (is (= 0 (:tap-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest closed-tap-is-cleaned-up-on-emit-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (s/close! tap)
      (sleep-briefly)
      (let [result (source/emit! src invalidation-1)]
        (is (= 0 (:tap-count result)))
        (is (= 0 (:attempted result))))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(deftest emit-with-no-taps-records-no-attempts-test
  (let [src (source/create {:id :test/source})]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= :emitted (:status result)))
        (is (= :test/source (:source/id result)))
        (is (= 0 (:tap-count result)))
        (is (= 0 (:attempted result)))
        (is (= invalidation-1 (:invalidation result))))
      (finally
        (source/close! src)))))

(deftest emit-increments-emitted-count-test
  (let [src (source/create)]
    (try
      (source/emit! src invalidation-1)
      (is (= 1 (:emitted-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest emit-to-one-tap-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= 1 (:tap-count result)))
        (is (= 1 (:attempted result)))
        (is (= invalidation-1 (take-value tap))))
      (finally
        (source/close! src)))))

(deftest emit-to-many-taps-test
  (let [src (source/create)
        tap-a (source/changes src)
        tap-b (source/changes src)]
    (try
      (let [result (source/emit! src invalidation-1)]
        (is (= 2 (:tap-count result)))
        (is (= 2 (:attempted result)))
        (is (= invalidation-1 (take-value tap-a)))
        (is (= invalidation-1 (take-value tap-b))))
      (finally
        (source/close! src)))))

(deftest emit-increments-attempted-count-test
  (let [src (source/create)
        tap-a (source/changes src)
        tap-b (source/changes src)]
    (try
      (source/emit! src invalidation-1)
      (is (= invalidation-1 (take-value tap-a)))
      (is (= invalidation-1 (take-value tap-b)))
      (is (= 2 (:attempted-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest emit-rejects-primary-change-without-id-test
  (let [src (source/create)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (source/emit! src invalid-invalidation)))
      (is (= 0 (:emitted-count (source/stats src))))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Emit many
;; -----------------------------------------------------------------------------

(deftest emit-many-emits-all-invalidations-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (let [result (source/emit-many! src [invalidation-1 invalidation-2])]
        (is (= :emitted-many (:status result)))
        (is (= 2 (:count result)))
        (is (= 2 (count (:results result))))
        (is (= invalidation-1 (take-value tap)))
        (is (= invalidation-2 (take-value tap))))
      (finally
        (source/close! src)))))

(deftest emit-many-validates-before-emission-test
  (let [src (source/create)
        tap (source/changes src)]
    (try
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid gesso.live value"
           (source/emit-many! src [invalidation-1 invalid-invalidation])))
      (is (= 0 (:emitted-count (source/stats src))))
      (is (= ::timeout (take-value tap)))
      (finally
        (source/close! src)))))

(deftest emit-many-after-close-throws-test
  (let [src (source/create)]
    (source/close! src)
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"source is closed"
         (source/emit-many! src [invalidation-1])))))


;; -----------------------------------------------------------------------------
;; Source-level coalescing
;; -----------------------------------------------------------------------------

(def coalesce-window-ms 100)

(def basis-a
  {:tx-id "10"
   :system-time "2026-08-25T20:00:00Z"})

(def basis-b
  {:tx-id "11"
   :system-time "2026-08-25T20:00:01Z"})

(def requirement-a
  (progression/requirement basis-a))

(def requirement-b
  (progression/requirement basis-b))

(deftest coalescing-is-leading-edge-plus-trailing-edge-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1 :marker :leading)
        pending-a (assoc invalidation-1 :marker :pending-a)
        pending-b (assoc invalidation-1
                         :marker :pending-b
                         :change/kind :deleted)]
    (try
      (let [leading-result (source/emit! src leading)]
        (is (= :emitted (:status leading-result)))
        (is (= :leading (:coalescing leading-result)))
        (is (false? (:coalesced? leading-result)))
        (is (= leading (take-value tap))))

      (let [pending-result-a (source/emit! src pending-a)
            pending-result-b (source/emit! src pending-b)]
        (is (= :queued (:status pending-result-a)))
        (is (= :suppressed (:coalescing pending-result-a)))
        (is (true? (:coalesced? pending-result-a)))
        (is (= 0 (:attempted pending-result-a)))
        (is (= :queued (:status pending-result-b)))
        (is (= :suppressed (:coalescing pending-result-b)))
        (is (true? (:coalesced? pending-result-b)))
        (is (= 0 (:attempted pending-result-b))))

      (is (= pending-b (take-value tap)))

      (let [stats (source/stats src)]
        (is (= 3 (:accepted-count stats)))
        (is (= 2 (:fanout-count stats)))
        (is (= 1 (:leading-count stats)))
        (is (= 1 (:trailing-count stats)))
        (is (= 2 (:suppressed-count stats)))
        (is (= 2 (:coalesced-count stats))))
      (finally
        (source/close! src)))))

(deftest coalescing-disabled-delivers-every-invalidation-immediately-test
  (let [src (source/create {:coalesce-window-ms nil})
        tap (source/changes src)
        first-value (assoc invalidation-1 :marker :first)
        second-value (assoc invalidation-1 :marker :second)]
    (try
      (is (= :emitted (:status (source/emit! src first-value))))
      (is (= :emitted (:status (source/emit! src second-value))))
      (is (= first-value (take-value tap)))
      (is (= second-value (take-value tap)))
      (let [stats (source/stats src)]
        (is (= 2 (:accepted-count stats)))
        (is (= 2 (:fanout-count stats)))
        (is (= 0 (:leading-count stats)))
        (is (= 0 (:trailing-count stats)))
        (is (= 0 (:suppressed-count stats)))
        (is (= 0 (:coalesced-count stats))))
      (finally
        (source/close! src)))))

(deftest coalescing-is-independent-per-scope-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        request-leading (assoc invalidation-1 :marker :request-leading)
        store-leading (assoc invalidation-2 :marker :store-leading)]
    (try
      (is (= :leading (:coalescing (source/emit! src request-leading))))
      (is (= :leading (:coalescing (source/emit! src store-leading))))
      (is (= request-leading (take-value tap)))
      (is (= store-leading (take-value tap)))
      (let [stats (source/stats src)]
        (is (= 2 (:leading-count stats)))
        (is (= 0 (:suppressed-count stats)))
        (is (= 2 (:scheduled-count stats))))
      (finally
        (source/close! src)))))

(deftest coalescing-composes-progression-across-suppressed-invalidations-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1 :marker :leading)
        pending-a (assoc invalidation-1
                         :marker :pending-a
                         :progression requirement-a)
        pending-b (assoc invalidation-1
                         :marker :pending-b
                         :change/kind :deleted
                         :progression requirement-b)]
    (try
      (source/emit! src leading)
      (is (= leading (take-value tap)))
      (is (= :queued (:status (source/emit! src pending-a))))
      (is (= :queued (:status (source/emit! src pending-b))))

      (let [trailing (take-value tap)]
        (is (= :pending-b (:marker trailing)))
        (is (= :deleted (:change/kind trailing)))
        (is (= #{basis-a basis-b}
               (progression/required-bases (:progression trailing)))))
      (finally
        (source/close! src)))))

(deftest leading-progression-is-not-folded-into-trailing-progression-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1
                       :marker :leading
                       :progression requirement-a)
        pending (assoc invalidation-1
                       :marker :pending
                       :progression requirement-b)]
    (try
      (source/emit! src leading)
      (is (= leading (take-value tap)))
      (is (= :queued (:status (source/emit! src pending))))
      (let [trailing (take-value tap)]
        (is (= :pending (:marker trailing)))
        (is (= #{basis-b}
               (progression/required-bases (:progression trailing)))))
      (finally
        (source/close! src)))))

(deftest pending-progression-survives-newer-descriptive-value-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1 :marker :leading)
        pending-with-progression (assoc invalidation-1
                                        :marker :with-progression
                                        :progression requirement-a)
        newest-without-progression (assoc invalidation-1
                                          :marker :newest
                                          :change/kind :deleted)]
    (try
      (source/emit! src leading)
      (is (= leading (take-value tap)))
      (source/emit! src pending-with-progression)
      (source/emit! src newest-without-progression)
      (let [trailing (take-value tap)]
        (is (= :newest (:marker trailing)))
        (is (= :deleted (:change/kind trailing)))
        (is (= #{basis-a}
               (progression/required-bases (:progression trailing)))))
      (finally
        (source/close! src)))))

(deftest newer-progression-attaches-to-pending-descriptive-value-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1 :marker :leading)
        pending-without-progression (assoc invalidation-1 :marker :older)
        newest-with-progression (assoc invalidation-1
                                       :marker :newer
                                       :progression requirement-b)]
    (try
      (source/emit! src leading)
      (is (= leading (take-value tap)))
      (source/emit! src pending-without-progression)
      (source/emit! src newest-with-progression)
      (let [trailing (take-value tap)]
        (is (= :newer (:marker trailing)))
        (is (= #{basis-b}
               (progression/required-bases (:progression trailing)))))
      (finally
        (source/close! src)))))

(deftest repeated-identical-progression-is-idempotent-under-coalescing-test
  (let [src (source/create {:coalesce-window-ms coalesce-window-ms})
        tap (source/changes src)
        leading (assoc invalidation-1 :marker :leading)
        pending-a (assoc invalidation-1
                         :marker :a
                         :progression requirement-a)
        pending-b (assoc invalidation-1
                         :marker :b
                         :progression requirement-a)]
    (try
      (source/emit! src leading)
      (is (= leading (take-value tap)))
      (source/emit! src pending-a)
      (source/emit! src pending-b)
      (let [trailing (take-value tap)]
        (is (= :b (:marker trailing)))
        (is (= #{basis-a}
               (progression/required-bases (:progression trailing)))))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(deftest errors-start-empty-test
  (let [src (source/create)]
    (try
      (is (= [] (source/errors src)))
      (finally
        (source/close! src)))))

(deftest clear-errors-returns-cleared-test
  (let [src (source/create)]
    (try
      (is (= :cleared (source/clear-errors! src)))
      (is (= [] (source/errors src)))
      (finally
        (source/close! src)))))

;; -----------------------------------------------------------------------------
;; Source lifecycle and indexed-bucket races
;; -----------------------------------------------------------------------------

(defn await-closed-briefly
  [source]
  ;; In the broken implementation close can finish while registration is
  ;; paused between its open check and insertion. In the corrected version
  ;; close waits on the registration lock. Neither case blocks the test.
  (loop [remaining 35]
    (when (and (pos? remaining) (not (source/closed? source)))
      (Thread/sleep 5)
      (recur (dec remaining)))))

(deftest closing-while-registering-closes-every-new-stream-test
  (doseq [[kind counter-var register]
          [[:broadcast #'gesso.live.source/next-tap-id!
            (fn [src] (source/changes src))]
           [:indexed #'gesso.live.source/next-subscription-id!
            (fn [src] (source/subscribe src {:topic :request :id "req-1"}))]]]
    (let [src (source/create)
          entered (promise)
          resume (promise)
          original (var-get counter-var)]
      (try
        (with-redefs-fn
          {counter-var
           (fn [value]
             (deliver entered :entered)
             (deref resume 2000 ::timeout)
             (original value))}
          (fn []
            (let [registration (future (register src))]
              (try
                (is (= :entered (deref entered 2000 ::timeout))
                    (str kind " entered registration"))
                (let [shutdown (future (source/close! src))]
                  (await-closed-briefly src)
                  (deliver resume :resume)
                  (let [stream (deref registration 2000 ::timeout)]
                    (is (not= ::timeout stream))
                    (is (= :closed (deref shutdown 2000 ::timeout)))
                    (is (s/closed? stream)
                        (str kind " subscription must not survive shutdown"))
                    (is (zero? (:tap-count (source/stats src))))
                    (is (zero? (:indexed-scope-count (source/stats src))))))
                (finally
                  (deliver resume :resume))))))
        (finally
          (source/close! src))))))

(deftest replacing-last-scope-subscriber-does-not-retire-live-bucket-test
  (let [src (source/create)
        scope {:topic :request :id "req-1"}
        original-stream (source/subscribe src scope)
        entered (promise)
        resume (promise)
        original-bucket (var-get #'gesso.live.source/scope-bucket)]
    (try
      (with-redefs-fn
        {#'gesso.live.source/scope-bucket
         (fn [s k]
           (let [bucket (original-bucket s k)]
             (deliver entered :entered)
             (deref resume 2000 ::timeout)
             bucket))}
        (fn []
          (let [registration (future (source/subscribe src scope))]
            (try
              (is (= :entered (deref entered 2000 ::timeout)))
              (let [retirement (future (s/close! original-stream))]
                ;; Without lifecycle synchronization, retirement detaches the
                ;; bucket returned to the paused registration.
                (loop [remaining 35]
                  (when (and (pos? remaining)
                             (not (zero? (:indexed-scope-count (source/stats src)))))
                    (Thread/sleep 5)
                    (recur (dec remaining))))
                (deliver resume :resume)
                (let [replacement (deref registration 2000 ::timeout)]
                  (is (not= ::timeout replacement))
                  (is (not= ::timeout (deref retirement 2000 ::timeout)))
                  (is (= 1 (:subscription-count (source/stats src))))
                  (is (= 1 (:indexed-scope-count (source/stats src))))
                  (source/emit! src invalidation-1)
                  (is (= invalidation-1 (take-value replacement))
                      "a replacement subscriber must remain in the scope index")
                  (s/close! replacement)))
              (finally
                (deliver resume :resume))))))
      (finally
        (source/close! src)))))

(deftest emission-paused-in-validation-cannot-reopen-closed-source-test
  ;; Force close! after emit!'s initial open check but before validation
  ;; returns. An invalidation must not be accepted or recreate cooldown state
  ;; after shutdown has already discarded every pending source value.
  (doseq [window-ms [nil 500]]
    (let [src (source/create {:coalesce-window-ms window-ms})
          validation-entered (promise)
          resume-validation (promise)
          original (var-get #'gesso.live.source/validate-invalidation!)]
      (try
        (with-redefs-fn
          {#'gesso.live.source/validate-invalidation!
           (fn [value]
             (deliver validation-entered :entered)
             (deref resume-validation 2000 ::timeout)
             (original value))}
          (fn []
            (let [attempt (future
                            (try
                              (source/emit! src invalidation-1)
                              (catch clojure.lang.ExceptionInfo e e)))]
              (try
                (is (= :entered (deref validation-entered 2000 ::timeout)))
                (is (= :closed (source/close! src)))
                (deliver resume-validation :continue)
                (let [outcome (deref attempt 2000 ::timeout)
                      stats (source/stats src)]
                  (is (instance? clojure.lang.ExceptionInfo outcome)
                      (str "closed source must reject in-flight validation, window=" window-ms))
                  (is (= "gesso.live source is closed."
                         (when (instance? clojure.lang.ExceptionInfo outcome)
                           (ex-message outcome))))
                  (is (zero? (:accepted-count stats)))
                  (is (zero? (:pending-count stats)))
                  (is (zero? (:scheduled-count stats))))
                (finally
                  (deliver resume-validation :continue))))))
        (finally
          (deliver resume-validation :continue)
          (source/close! src))))))

(deftest coalesced-leading-scheduler-shutdown-race-is-not-delivery-failure-test
  ;; Reproduce the accepted-leading-invalidation race between the schedule
  ;; open check and ScheduledThreadPoolExecutor.schedule. In the old source,
  ;; close! could shut the scheduler down during this gap. A fanout that had
  ;; already happened then threw RejectedExecutionException to the caller.
  (let [src (source/create {:coalesce-window-ms 500})
        pre-schedule (promise)
        allow-schedule (promise)
        checks (atom 0)
        original-closed? (var-get #'gesso.live.source/closed?)]
    (try
      (with-redefs-fn
        {#'gesso.live.source/closed?
         (fn [s]
           (let [was-closed? (original-closed? s)]
             ;; 1: emit! entry; 2: coalescing acceptance; 3: scheduler.
             (when (= 3 (swap! checks inc))
               (deliver pre-schedule :ready)
               (deref allow-schedule 2000 ::timeout))
             was-closed?))}
        (fn []
          (let [emission (future
                           (try
                             (source/emit! src invalidation-1)
                             (catch Throwable e e)))]
            (try
              (is (= :ready (deref pre-schedule 2000 ::timeout)))
              (let [closing-started (promise)
                    shutdown (future
                               (deliver closing-started :started)
                               (source/close! src))]
                (is (= :started (deref closing-started 2000 ::timeout)))
                ;; On the old implementation shutdown can finish before the
                ;; paused schedule call resumes; on the fixed one it waits
                ;; for the lock surrounding the complete schedule attempt.
                (deref shutdown 150 ::waiting-for-schedule)
                (deliver allow-schedule :continue)
                (let [result (deref emission 2000 ::timeout)]
                  (is (map? result)
                      (str "accepted leading emission must not throw after "
                           "shutdown races with scheduling: " result))
                  (is (= :emitted (:status result))))
                (is (= :closed (deref shutdown 2000 ::timeout)))
                (is (zero? (:scheduled-count (source/stats src)))))
              (finally
                (deliver allow-schedule :continue))))))
      (finally
        (deliver allow-schedule :continue)
        (source/close! src)))))

(deftest source-close-stream-hooks-run-outside-lifecycle-lock-test
  ;; Manifold runs a stream's on-closed hooks as part of close!. Such a hook
  ;; may wait for another thread that needs the source registration lock.
  ;; Source shutdown must never hold that lock while invoking app callbacks.
  (doseq [consumer [:broadcast :indexed]]
    (let [src (source/create)
          stream (case consumer
                   :broadcast (source/changes src)
                   :indexed (source/subscribe src {:topic :request :id "req-1"}))
          acquired (promise)]
      (s/on-closed stream
                   (fn []
                     (let [other-thread (future
                                          (locking (:coalesce-lock src)
                                            :lock-available))]
                       (deliver acquired (deref other-thread 500 ::timeout)))))
      (is (= :closed (source/close! src)))
      (is (s/closed? stream))
      (is (= :lock-available (deref acquired 1000 ::timeout))
          (str consumer " on-closed callbacks must not hold the source lock"))
      (is (= 0 (:tap-count (source/stats src)))))))

(deftest rejected-or-failed-source-put-closes-consumer-stream-test
  ;; Removing a failed consumer from routing maps without closing its stream
  ;; strands its Live flow: the transport keeps waiting for a wakeup it will
  ;; never receive. Both indexed subscriptions and broadcast taps must receive
  ;; a physical terminal close on rejected and failed asynchronous puts.
  (doseq [[kind subscribe!]
          [[:indexed #(source/subscribe % {:topic :request :id "req-1"})]
           [:broadcast source/changes]]
          outcome [:rejected :failed]]
    (let [src (source/create)
          stream (subscribe! src)
          pending (d/deferred)
          original-put @#'gesso.live.source/put-consumer!
          close-called (promise)]
      (s/on-closed stream #(deliver close-called :closed))
      (try
        (with-redefs-fn {#'gesso.live.source/put-consumer!
                         (fn [target value]
                               (if (identical? stream target)
                                 pending
                                 (original-put target value)))}
          (fn []
            (is (= 1 (:attempted (source/emit! src invalidation-1)))
                (str kind " must attempt delivery before " outcome))))
        (is (not (s/closed? stream)))
        (case outcome
          :rejected (d/success! pending false)
          :failed (d/error! pending (ex-info "put failed" {:kind kind})))
        (is (= :closed (deref close-called timeout-ms ::timeout))
            (str kind " " outcome " must close the consumer stream"))
        (is (s/closed? stream))
        (is (zero? (:tap-count (source/stats src))))
        (is (= (if (= outcome :rejected) :put-rejected :put-error)
               (:kind (last (source/errors src)))))
        (finally
          (source/close! src))))))

(deftest throwing-source-error-hook-does-not-abort-other-fanout-test
  ;; A failed broadcast consumer must not prevent a healthy indexed consumer
  ;; from seeing the same invalidation merely because the diagnostic callback
  ;; throws an Error rather than an Exception.
  (let [reported (promise)
        src (source/create
             {:on-error (fn [_]
                          (deliver reported :called)
                          (throw (AssertionError. "diagnostic hook failure")))})
        broken (source/changes src)
        healthy (source/subscribe src {:topic :request :id "req-1"})
        original-put (var-get #'gesso.live.source/put-consumer!)]
    (try
      (with-redefs-fn
        {#'gesso.live.source/put-consumer!
         (fn [stream value]
           (if (identical? stream broken)
             (throw (ex-info "broadcast put failed" {}))
             (original-put stream value)))}
        (fn []
          (let [result (try
                         (source/emit! src invalidation-1)
                         (catch Throwable e e))]
            (is (= :emitted (:status result)))
            (is (= 1 (:attempted result))
                "Healthy indexed fanout must still be attempted"))))
      (is (= :called (deref reported timeout-ms ::timeout)))
      (is (= invalidation-1 (take-value healthy)))
      (is (s/closed? broken))
      (is (= :put-error (:kind (first (source/errors src)))))
      (finally
        (source/close! src)))))

(deftest throwing-source-error-hook-does-not-poison-coalescing-test
  ;; The leading invalidation establishes the cooldown schedule. A failing
  ;; on-error callback during fanout must not leave that cooldown unscheduled,
  ;; silently retaining a later invalidation forever.
  (let [src (source/create
             {:coalesce-window-ms 100
              :on-error (fn [_]
                          (throw (AssertionError. "diagnostic hook failure")))})
        broken (source/changes src)
        healthy (source/subscribe src {:topic :request :id "req-1"})
        trailing (assoc invalidation-1 :marker :trailing)
        original-put (var-get #'gesso.live.source/put-consumer!)]
    (try
      (with-redefs-fn
        {#'gesso.live.source/put-consumer!
         (fn [stream value]
           (if (identical? stream broken)
             (throw (ex-info "broadcast put failed" {}))
             (original-put stream value)))}
        (fn []
          (let [leading-result (try
                                 (source/emit! src invalidation-1)
                                 (catch Throwable e e))]
            (is (= :emitted (:status leading-result)))
            (is (= :queued (:status (source/emit! src trailing)))))))
      (is (= invalidation-1 (take-value healthy)))
      (is (= trailing (take-value healthy))
          "A trailing invalidation must not be stranded by diagnostics")
      (is (= 0 (:pending-count (source/stats src))))
      (finally
        (source/close! src)))))

(deftest interrupted-source-put-retires-consumer-and-preserves-interrupt-test
  ;; Both indexed and broadcast physical put boundaries catch external errors.
  ;; A synchronous interrupted put still retires its stream, records the exact
  ;; cause and preserves the caller's cancellation signal.
  (doseq [[kind subscribe!]
          [[:indexed #(source/subscribe % {:topic :request :id "req-1"})]
           [:broadcast source/changes]]]
    (let [src (source/create)
          broken (subscribe! src)
          healthy (source/subscribe src {:topic :request :id "req-1"})
          original-put @#'gesso.live.source/put-consumer!
          cause (InterruptedException. "put interrupted")]
      (try
        (Thread/interrupted)
        (with-redefs-fn
          {#'gesso.live.source/put-consumer!
           (fn [target value]
             (if (identical? target broken)
               (throw cause)
               (original-put target value)))}
          (fn []
            (let [result (source/emit! src invalidation-1)
                  interrupted? (.isInterrupted (Thread/currentThread))]
              (Thread/interrupted)
              (is (= :emitted (:status result)))
              (is (= 1 (:attempted result))
                  "The healthy subscriber must still receive invalidation")
              (is interrupted? (str kind " must restore thread interrupt")))))
        (is (s/closed? broken) "Interrupted put must retire its stream")
        (is (identical? cause (:error (last (source/errors src)))))
        (is (= invalidation-1 (take-value healthy)))
        (finally
          (Thread/interrupted)
          (source/close! src))))))

(deftest interrupted-source-error-hook-restores-thread-flag-test
  ;; Error-reporting is optional: its callback may fail, but must not erase an
  ;; interrupt during a synchronous fanout on the publishing thread.
  (let [src (source/create
             {:on-error (fn [_]
                          (throw (InterruptedException.
                                  "source diagnostic interrupted")))})
        broken (source/changes src)
        healthy (source/subscribe src {:topic :request :id "req-1"})
        original-put @#'gesso.live.source/put-consumer!]
    (try
      (Thread/interrupted)
      (with-redefs-fn
        {#'gesso.live.source/put-consumer!
         (fn [target value]
           (if (identical? target broken)
             (throw (ex-info "put failed" {}))
             (original-put target value)))}
        (fn []
          (let [result (source/emit! src invalidation-1)
                interrupted? (.isInterrupted (Thread/currentThread))]
            (Thread/interrupted)
            (is (= :emitted (:status result)))
            (is (= 1 (:attempted result)))
            (is interrupted? "Suppressed diagnostic interrupt must survive"))))
      (is (s/closed? broken))
      (is (= :put-error (:kind (last (source/errors src)))))
      (is (= invalidation-1 (take-value healthy)))
      (finally
        (Thread/interrupted)
        (source/close! src)))))

(deftest interrupted-source-cooldown-reporter-sees-interrupt-test
  ;; Scheduler exceptions are reported as failures instead of crashing its
  ;; thread. Restore InterruptedException before invoking that reporter.
  (let [reported (promise)
        src (source/create
             {:coalesce-window-ms 10
              :on-error (fn [entry]
                          (let [interrupted?
                                (.isInterrupted (Thread/currentThread))]
                            (Thread/interrupted)
                            (deliver reported [entry interrupted?])))})
        failure (InterruptedException. "coalesce flush interrupted")]
    (try
      (with-redefs-fn
        {#'gesso.live.source/flush-cooldown!
         (fn [& _] (throw failure))}
        (fn []
          (is (= :emitted (:status (source/emit! src invalidation-1))))
          (let [observed (deref reported timeout-ms ::timeout)]
            (is (not= ::timeout observed) "Timer exception must be recorded")
            (when (vector? observed)
              (let [[entry interrupted?] observed]
                (is (= :coalesce-flush-error (:kind entry)))
                (is (identical? failure (:error entry)))
                (is interrupted?
                    "Reporter must observe restored scheduler interrupt"))))))
      (finally
        (source/close! src)))))

(deftest synchronous-source-put-error-closes-consumer-stream-test
  ;; A thrown put! call must use the same terminal cleanup as an asynchronously
  ;; rejected or failed put. No abandoned open stream may outlive registration.
  (doseq [[kind subscribe!]
          [[:indexed #(source/subscribe % {:topic :request :id "req-1"})]
           [:broadcast source/changes]]]
    (let [src (source/create)
          stream (subscribe! src)
          original-put @#'gesso.live.source/put-consumer!
          boom (ex-info "put threw" {:consumer kind})]
      (try
        (with-redefs-fn {#'gesso.live.source/put-consumer!
                         (fn [target value]
                               (if (identical? stream target)
                                 (throw boom)
                                 (original-put target value)))}
          (fn []
            (is (= 0 (:attempted (source/emit! src invalidation-1))))))
        (is (s/closed? stream)
            (str kind " must close when put! throws synchronously"))
        (is (zero? (:tap-count (source/stats src))))
        (is (= :put-error (:kind (last (source/errors src)))))
        (finally
          (source/close! src))))))

(deftest source-error-history-is-bounded-and-reports-all-failed-deliveries-test
  ;; A failing subscriber can cause arbitrarily many put errors over the life
  ;; of a server. The source retains only its newest N records, while the
  ;; diagnostic callback receives every failure and the source remains usable.
  (let [reported (atom [])
        src (source/create {:max-recorded-errors 3
                            :on-error (fn [entry]
                                        (swap! reported conj
                                               (get-in entry [:invalidation :seq])))})
        original-put (var-get #'gesso.live.source/put-consumer!)]
    (try
      (doseq [i (range 12)]
        (let [broken (source/subscribe src {:topic :request :id "req-1"})]
          (with-redefs-fn
            {#'gesso.live.source/put-consumer!
             (fn [stream value]
               (if (identical? stream broken)
                 (throw (ex-info "permanent sink failure" {:i i}))
                 (original-put stream value)))}
            (fn []
              (is (= :emitted
                     (:status (source/emit! src
                                            (assoc invalidation-1 :seq i)))))))
          (is (s/closed? broken))))
      (is (= (vec (range 12)) @reported)
          "Diagnostics must not disappear when bounded history overflows")
      (is (= [9 10 11]
             (mapv #(get-in % [:invalidation :seq]) (source/errors src))))
      (let [stats (source/stats src)]
        (is (= 3 (:max-recorded-errors stats)))
        (is (= 3 (:error-count stats)))
        (is (= 12 (:total-error-count stats)))
        (is (= 9 (:evicted-error-count stats)))
        (is (zero? (:tap-count stats))))
      (is (= :cleared (source/clear-errors! src)))
      (is (= [] (source/errors src)))
      (is (= 12 (:total-error-count (source/stats src)))
          "Clearing retained objects must not erase lifetime error evidence")
      (finally (source/close! src)))))

(deftest source-errors-may-be-unretained-without-suppressing-reporting-test
  (let [seen (atom [])
        src (source/create {:max-recorded-errors 0
                            :on-error #(swap! seen conj (:sequence %))})]
    (try
      (doseq [i (range 5)]
        (#'gesso.live.source/record-error! src
         {:kind :test/synthetic-failure :sequence i}))
      (is (= [] (source/errors src)))
      (is (= [0 1 2 3 4] @seen))
      (is (= {:error-count 0 :total-error-count 5
              :evicted-error-count 5 :max-recorded-errors 0}
             (select-keys (source/stats src)
                          [:error-count :total-error-count
                           :evicted-error-count :max-recorded-errors])))
      (finally (source/close! src)))))

(deftest source-errors-bound-concurrent-recording-without-lost-counters-test
  ;; Repeated CAS retries under concurrent failed callbacks must not cause
  ;; either unbounded retention or overcounted evictions.
  (let [src (source/create {:max-recorded-errors 7})]
    (try
      (let [workers (doall (for [worker (range 8)]
                             (future
                               (dotimes [i 80]
                                 (#'gesso.live.source/record-error! src
                                  {:kind :test/concurrent-failure
                                   :sequence [worker i]})))))]
        (doseq [task workers]
          (is (not= ::timeout (deref task 10000 ::timeout)))))
      (let [stats (source/stats src)
            recent (source/errors src)]
        (is (= 7 (count recent)))
        (is (= 640 (:total-error-count stats)))
        (is (= 633 (:evicted-error-count stats)))
        (is (= 7 (:error-count stats)))
        (is (= 7 (count (distinct (map :sequence recent))))))
      (finally (source/close! src)))))

(deftest source-error-retention-option-is-validated-before-stream-creation-test
  (doseq [bad [-1 4097 1.5 nil "unbounded"]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Invalid gesso.live value"
         (source/create {:max-recorded-errors bad}))
        (str "Invalid diagnostic retention must fail preflight: " bad)))
  (let [src (source/create)]
    (try
      (is (= 256 (:max-recorded-errors (source/stats src))))
      (finally (source/close! src)))))

(deftest coalescing-scope-cap-bypasses-new-scopes-without-dropping-notices-test
  ;; A large unique-scope burst cannot allocate unbounded cooldowns/timers.
  ;; Capacity bypass preserves immediate source delivery including progression.
  (let [src (source/create {:coalesce-window-ms 30000
                            :max-active-coalesce-scopes 2})
        tap (source/changes src)
        a (assoc invalidation-1 :marker :a)
        b (assoc invalidation-2 :marker :b)
        c (assoc invalidation-1 :id "req-overflow" :marker :c
                 :progression requirement-a)
        d (assoc c :marker :d :progression requirement-b)]
    (try
      (is (= :leading (:coalescing (source/emit! src a))))
      (is (= :leading (:coalescing (source/emit! src b))))
      (is (= :capacity-bypass (:coalescing (source/emit! src c))))
      (is (= :capacity-bypass (:coalescing (source/emit! src d))))
      (is (= :queued (:status (source/emit! src (assoc a :marker :pending)))))
      (is (= [a b c d] (repeatedly 4 #(take-value tap))))
      (let [stats (source/stats src)]
        (is (= 2 (:max-active-coalesce-scopes stats)))
        (is (= 2 (:scheduled-count stats)))
        (is (= 1 (:pending-count stats)))
        (is (= 2 (:coalesce-bypass-count stats)))
        (is (= 5 (:accepted-count stats)))
        (is (= 4 (:fanout-count stats)))
        (is (= 1 (:suppressed-count stats))))
      ;; The unchanged progression requirement from each bypass is carried
      ;; by the physical invalidation, not collapsed or silently discarded.
      (is (= #{basis-a} (progression/required-bases (:progression c))))
      (is (= #{basis-b} (progression/required-bases (:progression d))))
      (finally (source/close! src)))))

(deftest coalescing-capacity-reopens-after-scope-timer-retires-test
  (let [src (source/create {:coalesce-window-ms 100
                            :max-active-coalesce-scopes 1})
        first-scope invalidation-1
        next-scope invalidation-2]
    (try
      (is (= :leading (:coalescing (source/emit! src first-scope))))
      (is (= :capacity-bypass (:coalescing (source/emit! src next-scope))))
      (is (= :capacity-bypass (:coalescing (source/emit! src next-scope))))
      (is (= 1 (:scheduled-count (source/stats src))))
      (is (loop [n 0]
            (cond (zero? (:scheduled-count (source/stats src))) true
                  (>= n 200) false
                  :else (do (Thread/sleep 10) (recur (inc n)))))
          "An expired cooldown must free capacity for another scope")
      (is (= :leading (:coalescing (source/emit! src next-scope))))
      (is (= :queued (:status (source/emit! src next-scope))))
      (is (= 2 (:coalesce-bypass-count (source/stats src))))
      (is (= 1 (:scheduled-count (source/stats src))))
      (finally (source/close! src)))))

(deftest zero-coalesce-cap-disables-timers-without-disabling-delivery-test
  (let [src (source/create {:coalesce-window-ms 1000
                            :max-active-coalesce-scopes 0})
        tap (source/subscribe src {:topic :request :id "req-1"})]
    (try
      (dotimes [i 3]
        (is (= :capacity-bypass
               (:coalescing (source/emit! src
                                          (assoc invalidation-1 :seq i))))))
      (is (= [0 1 2] (mapv :seq (repeatedly 3 #(take-value tap)))))
      (let [stats (source/stats src)]
        (is (zero? (:scheduled-count stats)))
        (is (zero? (:pending-count stats)))
        (is (= 3 (:coalesce-bypass-count stats)))
        (is (= 3 (:fanout-count stats))))
      (finally (source/close! src)))))

(deftest concurrent-unique-scope-storm-stays-within-cooldown-cap-test
  (let [src (source/create {:coalesce-window-ms 30000
                            :max-active-coalesce-scopes 4})]
    (try
      (let [tasks (doall
                   (for [worker (range 8)]
                     (future
                       (dotimes [i 25]
                         (source/emit! src
                                       (assoc invalidation-1
                                              :id (str "scope-" worker "-" i)))))))]
        (doseq [task tasks]
          (is (not= ::timeout (deref task 10000 ::timeout)))))
      (let [stats (source/stats src)]
        (is (= 200 (:accepted-count stats)))
        (is (= 200 (:fanout-count stats)))
        (is (= 4 (:scheduled-count stats)))
        (is (= 196 (:coalesce-bypass-count stats)))
        (is (zero? (:pending-count stats))))
      (finally (source/close! src)))))

(deftest coalescing-scope-cap-option-fails-closed-before-source-creation-test
  (doseq [bad [-1 65537 nil 1.5 "unlimited"]]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Invalid gesso.live value"
         (source/create {:max-active-coalesce-scopes bad}))
        (str "invalid cap: " bad)))
  (let [src (source/create)]
    (try
      (is (= 4096 (:max-active-coalesce-scopes (source/stats src))))
      (finally (source/close! src)))))
