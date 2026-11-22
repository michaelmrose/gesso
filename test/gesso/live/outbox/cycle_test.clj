(ns gesso.live.outbox.cycle-test
  "v2097: synchronous end-to-end worker cycles and fail-closed partial results.
   Tests exercise embedded XTDB and the real Live source, not browser receipt."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.source :as source]
            [manifold.stream :as s]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util UUID]))

(def ^:private source-id "gesso/test-cycle")

(defn- intent-id [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- make-system []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ctx change]
                                   [(select-keys change [:topic :id :change/kind])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- seed! [node ids]
  (let [plan (outbox/prepare!
              {:intent-ids ids :source-id source-id
               :operation :request/claim :purpose :gesso.live/refresh}
              (mapv (fn [id] {:topic :request :id (str "request-" id)
                              :change/kind :updated}) ids))]
    (xt/execute-tx node
                   (into (vec (:guard-ops plan))
                         [(into [:put-docs :requests]
                                (mapv (fn [id] {:xt/id (str "request-" id)
                                                :status :claimed}) ids))
                          (:tx-op plan)]))))

(defn- physical-row [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- exception-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest interrupted-acquisition-cycle-preserves-prefix-and-thread-signal-test
  ;; A physical sweep may throw after a previous candidate completed.
  ;; The cycle must retain that prefix and the original cause without erasing
  ;; the caller's JVM cancellation signal when the failing sweep interrupted.
  (let [first-id (intent-id 901)
        interruption (InterruptedException. "discovery interrupted")
        sweeps (atom 0)
        system {:source ::source}]
    (try
      (Thread/interrupted)
      (let [failure
            (with-redefs [worker/acquire-sweep!
                          (fn [_ _]
                            (if (= 1 (swap! sweeps inc))
                              {:results [{:intent-id first-id
                                          :status :contended
                                          :reason :lease-held}]
                               :attempted 1
                               :halt-reason :page-budget
                               :next-after first-id}
                              (throw interruption)))]
              (try
                (cycle/process-cycle! :node system {} source-id
                                      {:worker-id "w" :max-items 2})
                (catch Throwable error error)))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (Thread/interrupted)
        (is (instance? clojure.lang.ExceptionInfo failure))
        (is (= :gesso.live.outbox/cycle-interrupted
               (:error/type (ex-data failure))))
        (is (= :acquisition (:phase (ex-data failure))))
        (is (= :acquisition-exception (:reason (ex-data failure))))
        (is (identical? interruption (.getCause ^Throwable failure)))
        (is (= 1 (:completed-count (ex-data failure))))
        (is (= first-id (:last-completed-cursor (ex-data failure))))
        (is (= :contended (get-in (ex-data failure)
                                  [:completed 0 :acquisition-status])))
        (is (nil? (:in-flight (ex-data failure))))
        (is (= 2 @sweeps))
        (is interrupted?
            "An interrupted acquisition must not erase JVM cancellation"))
      (finally
        (Thread/interrupted)))))

(deftest interrupted-delivery-cycle-preserves-lease-and-thread-signal-test
  ;; A leased intent may already have been delivered when dispatch throws.
  ;; Keep its uncertainty and physical lease identity, plus the JVM interrupt
  ;; that the interruptible external operation may have cleared.
  (let [id (intent-id 902)
        lease (random-uuid)
        interruption (InterruptedException. "delivery interrupted")
        calls (atom [])
        system {:source ::source}]
    (try
      (Thread/interrupted)
      (let [failure
            (with-redefs [worker/acquire-sweep!
                          (fn [_ _]
                            (swap! calls conj :acquire)
                            {:results [{:intent-id id :status :acquired
                                        :lease-id lease :worker-id "w"}]
                             :attempted 1
                             :halt-reason :page-budget
                             :next-after id})
                          delivery/dispatch-acquired!
                          (fn [& _]
                            (swap! calls conj :dispatch)
                            (throw interruption))]
              (try
                (cycle/process-cycle! :node system {} source-id
                                      {:worker-id "w" :max-items 2})
                (catch Throwable error error)))
            interrupted? (.isInterrupted (Thread/currentThread))
            details (ex-data failure)]
        (Thread/interrupted)
        (is (instance? clojure.lang.ExceptionInfo failure))
        (is (= :gesso.live.outbox/cycle-interrupted (:error/type details)))
        (is (= :delivery (:phase details)))
        (is (= :delivery-exception (:reason details)))
        (is (= :unknown (:source-acceptance details)))
        (is (identical? interruption (.getCause ^Throwable failure)))
        (is (= 0 (:completed-count details)))
        (is (= id (get-in details [:in-flight :intent-id])))
        (is (= lease (get-in details [:in-flight :lease-id])))
        (is (true? (:rescan-required? details)))
        (is (= [:acquire :dispatch] @calls))
        (is interrupted?
            "An interrupted dispatch must preserve the caller's signal"))
      (finally
        (Thread/interrupted)))))

(deftest invalid-options-reject-before-discovery-test
  (let [system (make-system)]
    (try
      (doseq [[opts expected]
              [[nil :invalid-options]
               [{} :invalid-worker-id]
               [{:worker-id " "} :invalid-worker-id]
               [{:worker-id "w" :max-items 0} :invalid-max-items]
               [{:worker-id "w" :max-items 129} :invalid-max-items]
               [{:worker-id "w" :lease-millis 0} :invalid-lease-millis]
               [{:worker-id "w" :lease-millis 300001} :invalid-lease-millis]
               [{:worker-id "w" :after nil} :invalid-cursor]
               [{:worker-id "w" :extra true} :unknown-options]]]
        (is (= expected (:reason (exception-data
                                  #(cycle/process-cycle! :no-node system {}
                                                         source-id opts))))))
      (is (= :invalid-source-id
             (:reason (exception-data
                       #(cycle/process-cycle! :no-node system {} " "
                                              {:worker-id "w"})))))
      (is (= :invalid-context
             (:reason (exception-data
                       #(cycle/process-cycle! :no-node system nil source-id
                                              {:worker-id "w"})))))
      (finally (live/close! system)))))

(deftest physical-cycle-delivers-and-acknowledges-two-intents-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          ids [(intent-id 11) (intent-id 12)]]
      (try
        (seed! node ids)
        (let [streams (mapv (fn [id]
                              (source/subscribe (:source system)
                                                {:topic :request
                                                 :id (str "request-" id)})) ids)
              result (cycle/process-cycle! node system {} source-id
                                           {:worker-id "single-node-a"
                                            :max-items 4 :lease-millis 60000})]
          (is (= :cycle-completed (:status result)))
          (is (= 2 (:processed result)))
          (is (= 2 (:acquired result)))
          (is (= 2 (:dispatch-accepted result)))
          (is (zero? (:contended result)))
          (is (= :observed-end (:halt-reason result)))
          (is (nil? (:next-after result)))
          (is (= ids (mapv :intent-id (:results result))))
          (doseq [[id stream entry] (map vector ids streams (:results result))]
            (let [event (deref (s/take! stream) 1500 ::timeout)
                  physical (physical-row node id)]
              (is (= (str "request-" id) (:id event)))
              (is (= (:progression entry) (:progression event)))
              (is (= :dispatch-accepted (:status entry)))
              (is (= :dispatch-accepted (:gesso.live.outbox/status physical)))
              (is (= (:receipt-id entry)
                     (:gesso.live.outbox/dispatch-receipt-id physical)))
              (s/close! stream)))
          (is (empty? (:candidates (discovery/discover-eligible! node {:source-id source-id}))))
          (is (zero? (:processed (cycle/process-cycle! node system {} source-id
                                                        {:worker-id "fresh"})))))
        (finally (live/close! system))))))

(deftest physical-budget-and-resume-do-not-leak-unprocessed-leases-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          ids [(intent-id 21) (intent-id 22) (intent-id 23)]]
      (try
        (seed! node ids)
        (let [a (cycle/process-cycle! node system {} source-id
                                      {:worker-id "w" :max-items 1})]
          (is (= 1 (:processed a)))
          (is (= :page-budget (:halt-reason a)))
          (is (= (first ids) (:next-after a)))
          (is (= :pending (:gesso.live.outbox/status (physical-row node (second ids)))))
          (is (= :pending (:gesso.live.outbox/status (physical-row node (last ids)))))
          (let [b (cycle/process-cycle! node system {} source-id
                                        {:worker-id "w" :max-items 1
                                         :after (:next-after a)})
                c (cycle/process-cycle! node system {} source-id
                                        {:worker-id "w" :max-items 1
                                         :after (:next-after b)})]
            (is (= :page-budget (:halt-reason b)))
            (is (= :observed-end (:halt-reason c)))
            (is (= ids (mapv (comp :intent-id first :results) [a b c])))
            (is (every? #(= :dispatch-accepted
                            (:gesso.live.outbox/status (physical-row node %))) ids))))
        (finally (live/close! system))))))

(deftest physical-contention-continues-but-requires-fresh-sweep-for-leftovers-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          ids [(intent-id 31) (intent-id 32)]
          original acquisition/acquire-pending!]
      (try
        (seed! node ids)
        (let [result (with-redefs [acquisition/acquire-pending!
                                   (fn [n claim]
                                     (if (= (first ids) (:intent-id claim))
                                       {:status :contended :intent-id (:intent-id claim)}
                                       (original n claim)))]
                       (cycle/process-cycle! node system {} source-id
                                             {:worker-id "w" :max-items 2}))]
          (is (= 2 (:processed result)))
          (is (= 1 (:contended result)))
          (is (= 1 (:dispatch-accepted result)))
          (is (= :pending (:gesso.live.outbox/status (physical-row node (first ids)))))
          (is (= :dispatch-accepted (:gesso.live.outbox/status (physical-row node (second ids)))))
          (is (= 1 (:dispatch-accepted
                     (cycle/process-cycle! node system {} source-id
                                           {:worker-id "w" :max-items 2})))))
        (finally (live/close! system))))))

(deftest failed-second-delivery-retains-completion-and-current-lease-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          ids [(intent-id 41) (intent-id 42) (intent-id 43)]
          calls (atom 0)
          original delivery/dispatch-acquired!]
      (try
        (seed! node ids)
        (let [failure
              (with-redefs [delivery/dispatch-acquired!
                            (fn [& args]
                              (if (= 2 (swap! calls inc))
                                (throw (ex-info "simulated send uncertainty"
                                                {:error/type :test/indeterminate}))
                                (apply original args)))]
                (exception-data
                 #(cycle/process-cycle! node system {} source-id
                                        {:worker-id "w" :max-items 3})))]
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
          (is (= :delivery (:phase failure)))
          (is (= :delivery-exception (:reason failure)))
          (is (= :test/indeterminate (:original-error/type failure)))
          (is (= :unknown (:source-acceptance failure)))
          (is (true? (:rescan-required? failure)))
          (is (= 1 (:completed-count failure)))
          (is (= (first ids) (:last-completed-cursor failure)))
          (is (= (first ids) (get-in failure [:completed 0 :intent-id])))
          (is (= (second ids) (get-in failure [:in-flight :intent-id])))
          (is (uuid? (get-in failure [:in-flight :lease-id])))
          (is (= :dispatch-accepted (:gesso.live.outbox/status
                                     (physical-row node (first ids)))))
          (is (= :leased (:gesso.live.outbox/status
                           (physical-row node (second ids)))))
          (is (= :pending (:gesso.live.outbox/status
                            (physical-row node (last ids)))))
          (is (nil? (:gesso.live.outbox/dispatch-receipt-id
                    (physical-row node (second ids))))))
        (finally (live/close! system))))))

(deftest post-send-unacknowledged-result-interrupts-cycle-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          id (intent-id 51)]
      (try
        (seed! node [id])
        (let [failure
              (with-redefs [delivery/dispatch-acquired!
                            (fn [_ _ _ _ _]
                              {:status :dispatched-unacknowledged
                               :acknowledgment {:status :contended}})]
                (exception-data
                 #(cycle/process-cycle! node system {} source-id
                                        {:worker-id "w" :max-items 1})))]
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
          (is (= :acknowledgment (:phase failure)))
          (is (= :not-dispatch-accepted (:reason failure)))
          (is (= :reported (:source-acceptance failure)))
          (is (= :contended (:acknowledgment-status failure)))
          (is (zero? (:completed-count failure)))
          (is (= id (get-in failure [:in-flight :intent-id])))
          (is (= :leased (:gesso.live.outbox/status (physical-row node id)))))
        (finally (live/close! system))))))

(deftest unknown-acquisition-error-preserves-earlier-completion-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          ids [(intent-id 61) (intent-id 62)]
          calls (atom 0)
          original worker/acquire-sweep!]
      (try
        (seed! node ids)
        (let [failure
              (with-redefs [worker/acquire-sweep!
                            (fn [& args]
                              (if (= 2 (swap! calls inc))
                                (throw (ex-info "unknown claim state"
                                                {:error/type :test/claim-unknown}))
                                (apply original args)))]
                (exception-data
                 #(cycle/process-cycle! node system {} source-id
                                        {:worker-id "w" :max-items 2})))]
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
          (is (= :acquisition (:phase failure)))
          (is (= :test/claim-unknown (:original-error/type failure)))
          (is (= 1 (:completed-count failure)))
          (is (= (first ids) (:last-completed-cursor failure)))
          (is (nil? (:in-flight failure)))
          (is (= :dispatch-accepted (:gesso.live.outbox/status
                                     (physical-row node (first ids)))))
          (is (= :pending (:gesso.live.outbox/status
                            (physical-row node (second ids))))))
        (finally (live/close! system))))))

(deftest physical-empty-cycle-is-bounded-and-does-not-claim-work-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)]
      (try
        (let [out (cycle/process-cycle! node system {} source-id
                                        {:worker-id "w"})]
          (is (= :cycle-completed (:status out)))
          (is (= :observed-end (:halt-reason out)))
          (is (zero? (:processed out)))
          (is (nil? (:next-after out))))
        (finally (live/close! system))))))

(deftest physical-accepted-source-with-unknown-ack-propagates-with-context-test
  ;; The v2096 adapter has emitted to the actual Live source before the
  ;; injected acknowledgment uncertainty. Preserve that distinction.
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          id (intent-id 71)]
      (try
        (seed! node [id])
        (let [stream (source/subscribe (:source system)
                                       {:topic :request :id (str "request-" id)})
              failure
              (with-redefs [acknowledgment/acknowledge-dispatch!
                            (fn [& _] (throw (ex-info "unknown commit" {})))]
                (exception-data #(cycle/process-cycle! node system {} source-id
                                                       {:worker-id "w" :max-items 1})))
              event (deref (s/take! stream) 1500 ::timeout)]
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type failure)))
          (is (= :delivery (:phase failure)))
          (is (= :gesso.live.outbox/post-dispatch-ack-unknown
                 (:original-error/type failure)))
          (is (= :reported (:source-acceptance failure)))
          (is (= (str "request-" id) (:id event)))
          (is (zero? (:completed-count failure)))
          (is (= :leased (:gesso.live.outbox/status (physical-row node id))))
          (s/close! stream))
        (finally (live/close! system))))))

(deftest physical-cycle-skips-interleaved-foreign-source-before-lease-test
  (with-open [node (xtn/start-node {})]
    (let [system (make-system)
          foreign-id (intent-id 88)
          own-id (intent-id 89)]
      (try
        (seed! node [own-id])
        (let [plan (outbox/prepare!
                    {:intent-ids [foreign-id] :source-id "another/source"
                     :operation :request/claim :purpose :gesso.live/refresh}
                    [{:topic :request :id "foreign-request"}])]
          (xt/execute-tx node (conj (vec (:guard-ops plan)) (:tx-op plan))))
        (let [result (cycle/process-cycle! node system {} source-id
                                           {:worker-id "own-worker" :max-items 1})]
          (is (= :cycle-completed (:status result)))
          (is (= 1 (:dispatch-accepted result)))
          (is (= [own-id] (mapv :intent-id (:results result))))
          (is (= :pending
                 (:gesso.live.outbox/status (physical-row node foreign-id))))
          (is (= :dispatch-accepted
                 (:gesso.live.outbox/status (physical-row node own-id))))
          (is (empty? (:candidates
                       (discovery/discover-eligible! node {:source-id source-id})))))
        (finally (live/close! system))))))

(deftest malformed-cycle-page-cursors-fail-before-delivery-test
  ;; A one-item worker page is not proof of progress unless its identity
  ;; agrees with its continuation cursor and moves beyond the input cursor.
  ;; Rejected pages must not be recorded as completed or reach delivery.
  (let [a (intent-id 711)
        b (intent-id 712)
        c (intent-id 713)
        cases [[:wrong-continuation nil a b :page-budget]
               [:backward-continuation b a a :page-budget]
               [:repeated-continuation a a a :page-budget]
               [:backward-terminal b a nil :observed-end]
               [:equal-terminal a a nil :observed-end]
               [:missing-identity nil nil c :page-budget]]]
    (doseq [[label after current cursor halt] cases]
      (let [delivery-attempts (atom 0)
            page {:attempted 1
                  :results [{:status :contended :intent-id current}]
                  :halt-reason halt :next-after cursor}
            opts (cond-> {:worker-id "w" :max-items 1}
                   after (assoc :after after))
            failure (with-redefs [worker/acquire-sweep! (fn [_ _] page)
                                  delivery/dispatch-acquired!
                                  (fn [& _] (swap! delivery-attempts inc)
                                    {:status :dispatch-accepted})]
                      (try
                        (cycle/process-cycle! :node {:source ::source} {} source-id opts)
                        (catch clojure.lang.ExceptionInfo e e)))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            (str label " must not return a completed cycle"))
        (when (instance? clojure.lang.ExceptionInfo failure)
          (is (= :gesso.live.outbox/cycle-interrupted (:error/type (ex-data failure)))
              (str label " must demand reconciliation"))
          (is (= :acquisition (:phase (ex-data failure)))
              (str label " must fail at the page boundary"))
          (is (= :invalid-worker-page (:reason (ex-data (.getCause failure))))
              (str label " must expose the malformed page"))
          (is (zero? (:completed-count (ex-data failure)))
              (str label " must not invent a completed prefix"))
          (is (:rescan-required? (ex-data failure))))
        (is (zero? @delivery-attempts)
            (str label " must not dispatch"))))))

(deftest malformed-cycle-page-with-acquired-lease-never-dispatches-test
  ;; Even a purported successfully acquired lease cannot be sent if the
  ;; worker's claimed page cursor contradicts its identity. The operator
  ;; must reconcile the potential physical lease, not report dispatch.
  (let [a (intent-id 721)
        b (intent-id 722)
        calls (atom 0)
        page {:attempted 1
              :results [{:status :acquired :intent-id a
                         :lease-id (random-uuid) :worker-id "w"}]
              :halt-reason :page-budget :next-after b}
        failure (with-redefs [worker/acquire-sweep! (fn [_ _] page)
                              delivery/dispatch-acquired!
                              (fn [& _]
                                (swap! calls inc)
                                {:status :dispatch-accepted})]
                  (try
                    (cycle/process-cycle! :node {:source ::source} {} source-id
                                          {:worker-id "w" :max-items 1})
                    (catch clojure.lang.ExceptionInfo e e)))]
    (is (instance? clojure.lang.ExceptionInfo failure))
    (when (instance? clojure.lang.ExceptionInfo failure)
      (is (= :acquisition (:phase (ex-data failure))))
      (is (zero? (:completed-count (ex-data failure))))
      (is (= :invalid-worker-page (:reason (ex-data (.getCause failure))))))
    (is (zero? @calls)
        "Malformed lease page must be refused before external source emission")))

(deftest cycle-page-accepts-canonical-uuid-forward-progress-test
  ;; XTDB's unsigned UUID keyset order differs from Java UUID.compareTo.
  ;; A legitimate crossing from 7fff... to 8000... must stay admissible.
  (let [low (UUID/fromString "7fffffff-ffff-ffff-ffff-ffffffffffff")
        high (UUID/fromString "80000000-0000-0000-0000-000000000000")
        pages (atom [])
        outcome (with-redefs [worker/acquire-sweep!
                             (fn [_ opts]
                               (swap! pages conj opts)
                               {:attempted 1
                                :results [{:status :contended :intent-id high}]
                                :halt-reason :page-budget :next-after high})]
                  (cycle/process-cycle! :node {:source ::source} {} source-id
                                        {:worker-id "w" :after low :max-items 1}))]
    (is (= :cycle-completed (:status outcome)))
    (is (= :page-budget (:halt-reason outcome)))
    (is (= high (:next-after outcome)))
    (is (= 1 (:processed outcome)))
    (is (= 1 (:contended outcome)))
    (is (= low (:after (first @pages))))))

(deftest malformed-delivery-success-cannot-complete-cycle-test
  ;; A delivery adapter's :dispatch-accepted label is not itself evidence
  ;; that the claimed intent, lease, receipt and source observation agree.
  ;; Fail closed at the cycle boundary rather than return :cycle-completed.
  (let [id (intent-id 811)
        lease-id (random-uuid)
        receipt-id (random-uuid)
        progression {:basis :mock-lease}
        acquired {:status :acquired :intent-id id :lease-id lease-id
                  :worker-id "checked-worker" :progression progression}
        valid {:status :dispatch-accepted :intent-id id :lease-id lease-id
               :receipt-id receipt-id :progression progression
               :source-result {:status :emitted-many :count 1
                               :results [{:status :emitted}]}
               :acknowledgment {:status :acknowledged :intent-id id
                                :lease-id lease-id :receipt-id receipt-id}}
        cases [[:missing-receipt (dissoc valid :receipt-id)]
               [:wrong-intent (assoc valid :intent-id (random-uuid))]
               [:wrong-lease (assoc valid :lease-id (random-uuid))]
               [:wrong-progression (assoc valid :progression {:basis :different})]
               [:missing-ack (dissoc valid :acknowledgment)]
               [:wrong-ack-receipt (assoc-in valid [:acknowledgment :receipt-id]
                                             (random-uuid))]
               [:wrong-ack-intent (assoc-in valid [:acknowledgment :intent-id]
                                            (random-uuid))]
               [:unacknowledged (assoc-in valid [:acknowledgment :status] :contended)]
               [:no-source-result (dissoc valid :source-result)]
               [:false-source-count (assoc-in valid [:source-result :count] 2)]
               [:unaccepted-source (assoc-in valid [:source-result :results 0 :status]
                                             :rejected)]]]
    (doseq [[label result] cases]
      (let [sweeps (atom 0)
            dispatches (atom 0)
            failure (with-redefs [worker/acquire-sweep!
                                  (fn [_ _]
                                    (swap! sweeps inc)
                                    {:attempted 1 :results [acquired]
                                     :halt-reason :page-budget :next-after id})
                                  delivery/dispatch-acquired!
                                  (fn [& _] (swap! dispatches inc) result)]
                      (try
                        (cycle/process-cycle! :node {:source ::source} {}
                                              source-id {:worker-id "checked-worker"
                                                         :max-items 2})
                        (catch clojure.lang.ExceptionInfo error error)))]
        (is (instance? clojure.lang.ExceptionInfo failure)
            (str label " cannot certify a completed cycle"))
        (when (instance? clojure.lang.ExceptionInfo failure)
          (let [data (ex-data failure)]
            (is (= :gesso.live.outbox/cycle-interrupted (:error/type data))
                (str label " must demand reconciliation"))
            (is (= :delivery (:phase data)) (str label " failure phase"))
            (is (= :invalid-delivery-result (:reason data))
                (str label " must identify the violated adapter contract"))
            (is (= :reported (:source-acceptance data))
                (str label " cannot retract reported emission"))
            (is (zero? (:completed-count data)))
            (is (= id (get-in data [:in-flight :intent-id])))
            (is (= lease-id (get-in data [:in-flight :lease-id])))))
        (is (= 1 @sweeps) (str label " must not scan another page"))
        (is (= 1 @dispatches) (str label " must not retry emission"))))))

(deftest matched-delivery-success-completes-one-cycle-test
  ;; A correctly matched physical adapter result must remain accepted.
  (let [id (intent-id 812)
        lease-id (random-uuid)
        receipt-id (random-uuid)
        progression {:basis :mock-lease}
        acquired {:status :acquired :intent-id id :lease-id lease-id
                  :worker-id "checked-worker" :progression progression}
        delivered {:status :dispatch-accepted :intent-id id :lease-id lease-id
                   :receipt-id receipt-id :progression progression
                   :source-result {:status :emitted-many :count 1
                                   :results [{:status :queued}]}
                   :acknowledgment {:status :acknowledged :intent-id id
                                    :lease-id lease-id :receipt-id receipt-id}}
        outcome (with-redefs [worker/acquire-sweep!
                              (fn [_ _] {:attempted 1 :results [acquired]
                                         :halt-reason :observed-end :next-after nil})
                              delivery/dispatch-acquired! (fn [& _] delivered)]
                  (cycle/process-cycle! :node {:source ::source} {}
                                        source-id {:worker-id "checked-worker"
                                                   :max-items 2}))]
    (is (= :cycle-completed (:status outcome)))
    (is (= 1 (:processed outcome)))
    (is (= 1 (:dispatch-accepted outcome)))
    (is (= :observed-end (:halt-reason outcome)))
    (is (= receipt-id (get-in outcome [:results 0 :receipt-id])))
    (is (= progression (get-in outcome [:results 0 :progression])))))

(deftest malformed-second-delivery-preserves-completed-cycle-prefix-test
  ;; A malformed positive result on the second intent may follow a committed
  ;; first intent. Preserve that known completed prefix, not an invented final
  ;; cursor or permission to emit the second intent again.
  (let [a (intent-id 813) b (intent-id 814)
        token (random-uuid) receipt (random-uuid)
        progression {:basis :mock-lease}
        sweeps (atom 0)
        dispatches (atom 0)
        outcome
        (with-redefs [worker/acquire-sweep!
                      (fn [_ _]
                        (let [id (if (= 1 (swap! sweeps inc)) a b)]
                          {:attempted 1
                           :results [{:status :acquired :intent-id id
                                      :lease-id token :worker-id "w"
                                      :progression progression}]
                           :halt-reason :page-budget :next-after id}))
                      delivery/dispatch-acquired!
                      (fn [_ _ _ acquisition _]
                        (swap! dispatches inc)
                        (if (= a (:intent-id acquisition))
                          {:status :dispatch-accepted :intent-id a
                           :lease-id token :receipt-id receipt
                           :progression progression
                           :source-result {:status :emitted-many :count 1
                                           :results [{:status :emitted}]}
                           :acknowledgment {:status :acknowledged
                                            :intent-id a :lease-id token
                                            :receipt-id receipt}}
                          {:status :dispatch-accepted :intent-id b}))]
          (try
            (cycle/process-cycle! :node {:source ::source} {} source-id
                                  {:worker-id "w" :max-items 3})
            (catch clojure.lang.ExceptionInfo error error)))]
    (is (instance? clojure.lang.ExceptionInfo outcome))
    (when (instance? clojure.lang.ExceptionInfo outcome)
      (let [data (ex-data outcome)]
        (is (= :delivery (:phase data)))
        (is (= :invalid-delivery-result (:reason data)))
        (is (= 1 (:completed-count data)))
        (is (= a (:last-completed-cursor data)))
        (is (= a (get-in data [:completed 0 :intent-id])))
        (is (= receipt (get-in data [:completed 0 :receipt-id])))
        (is (= b (get-in data [:in-flight :intent-id])))))
    (is (= 2 @sweeps) "No third page after uncertainty")
    (is (= 2 @dispatches) "No duplicate physical send after mismatch")))

(deftest malformed-worker-page-retains-reported-lease-for-reconciliation-test
  ;; Acquisition happens before the worker returns its page. If the page
  ;; cursor is malformed, rejecting it is necessary but a nil :in-flight
  ;; would discard the acquired lease token needed for physical reconciliation.
  (doseq [status [:acquired :reclaimed]
          shape [:wrong-cursor :wrong-halt]]
    (let [id (intent-id 915)
          other (intent-id 916)
          lease (random-uuid)
          sends (atom 0)
          page (cond-> {:attempted 1
                        :results [{:intent-id id :status status
                                   :lease-id lease :worker-id "w"}]
                        :halt-reason :page-budget :next-after other}
                 (= shape :wrong-halt)
                 (assoc :halt-reason :observed-end))
          failure (with-redefs [worker/acquire-sweep! (fn [_ _] page)
                                delivery/dispatch-acquired!
                                (fn [& _] (swap! sends inc))]
                    (try
                      (cycle/process-cycle! :node {:source ::source} {} source-id
                                            {:worker-id "w" :max-items 1})
                      (catch clojure.lang.ExceptionInfo e e)))
          data (ex-data failure)]
      (is (= :gesso.live.outbox/cycle-interrupted (:error/type data)))
      (is (= :acquisition (:phase data)))
      (is (= :acquisition-exception (:reason data)))
      (is (= :invalid-worker-page (:reason (ex-data (.getCause failure)))))
      (is (zero? (:completed-count data)))
      (is (true? (:rescan-required? data)))
      (is (= {:intent-id id :lease-id lease :worker-id "w"
              :acquisition-status status
              :evidence :unverified-worker-page}
             (:in-flight data))
          (str status " / " shape " must preserve reported lease evidence"))
      (is (zero? @sends) "Malformed page must be refused before dispatch"))))

(deftest malformed-second-worker-page-retains-prefix-and-reported-lease-test
  (let [a (intent-id 918)
        b (intent-id 919)
        wrong (intent-id 920)
        lease (random-uuid)
        calls (atom 0)
        sent (atom 0)
        failure (with-redefs [worker/acquire-sweep!
                              (fn [_ _]
                                (if (= 1 (swap! calls inc))
                                  {:attempted 1
                                   :results [{:intent-id a :status :contended
                                              :reason :lease-held}]
                                   :halt-reason :page-budget :next-after a}
                                  {:attempted 1
                                   :results [{:intent-id b :status :acquired
                                              :lease-id lease :worker-id "w"}]
                                   :halt-reason :page-budget :next-after wrong}))
                              delivery/dispatch-acquired!
                              (fn [& _] (swap! sent inc))]
                  (try
                    (cycle/process-cycle! :node {:source ::source} {} source-id
                                          {:worker-id "w" :max-items 3})
                    (catch clojure.lang.ExceptionInfo e e)))
        data (ex-data failure)]
    (is (= :gesso.live.outbox/cycle-interrupted (:error/type data)))
    (is (= 1 (:completed-count data)))
    (is (= a (:last-completed-cursor data)))
    (is (= a (get-in data [:completed 0 :intent-id])))
    (is (= :contended (get-in data [:completed 0 :acquisition-status])))
    (is (= b (get-in data [:in-flight :intent-id])))
    (is (= lease (get-in data [:in-flight :lease-id])))
    (is (= :unverified-worker-page (get-in data [:in-flight :evidence])))
    (is (= 2 @calls) "No third acquisition after invalid second page")
    (is (zero? @sent) "Malformed second page cannot dispatch")))

(deftest malformed-unleased-worker-page-makes-no-false-lease-claim-test
  (let [id (intent-id 921)
        bad {:attempted 1
             :results [{:intent-id id :status :contended
                        :lease-id (random-uuid) :worker-id "w"}]
             :halt-reason :page-budget :next-after (intent-id 922)}
        error (with-redefs [worker/acquire-sweep! (fn [_ _] bad)]
                (try
                  (cycle/process-cycle! :node {:source ::source} {} source-id
                                        {:worker-id "w" :max-items 1})
                  (catch clojure.lang.ExceptionInfo e e)))]
    (is (= :gesso.live.outbox/cycle-interrupted (:error/type (ex-data error))))
    (is (nil? (:in-flight (ex-data error)))
        "A contended page cannot fabricate successful lease evidence")))

(deftest thrown-worker-acquisition-retains-unverified-physical-claim-test
  ;; Exercise the real worker -> cycle boundary. A failed physical CAS can
  ;; commit before reporting an exception; capture the attempted token without
  ;; claiming the caller acquired or safely dispatched the lease.
  (doseq [action [:acquire-pending :reclaim-expired]]
    (let [id (intent-id 941)
          claimed (atom [])
          dispatch-count (atom 0)
          cause (ex-info "XTDB outcome not observed" {:phase :physical-submit})
          failure (with-redefs
                   [discovery/discover-eligible!
                    (fn [_ _]
                      {:candidates [{:intent-id id :action action}]
                       :has-more? false :next-after nil})
                    acquisition/acquire-pending!
                    (fn [_ claim] (swap! claimed conj claim) (throw cause))
                    acquisition/reclaim-expired!
                    (fn [_ claim] (swap! claimed conj claim) (throw cause))
                    delivery/dispatch-acquired!
                    (fn [& _] (swap! dispatch-count inc)
                      {:status :dispatch-accepted})]
                   (try
                     (cycle/process-cycle! :node {:source ::source} {} source-id
                                           {:worker-id "checked-worker" :max-items 1})
                     (catch clojure.lang.ExceptionInfo e e)))
          data (ex-data failure)
          evidence (:in-flight data)
          request (first @claimed)]
      (is (= :gesso.live.outbox/cycle-interrupted (:error/type data)))
      (is (= :acquisition (:phase data)))
      (is (= :acquisition-exception (:reason data)))
      (is (= :physical-submit (:phase (ex-data (.getCause (.getCause failure))))))
      (is (= 0 (:completed-count data)))
      (is (true? (:rescan-required? data)))
      (is (= :unverified-acquisition-attempt (:evidence evidence)))
      (is (= (:intent-id request) (:intent-id evidence)))
      (is (= (:lease-id request) (:lease-id evidence)))
      (is (= (:worker-id request) (:worker-id evidence)))
      (is (= (:source-id request) (:source-id evidence)))
      (is (= action (:acquisition-action evidence)))
      (is (= 1 (count @claimed)))
      (is (zero? @dispatch-count)))))

(deftest uncertain-second-acquisition-retains-completed-prefix-and-token-test
  (let [first-id (intent-id 942)
        second-id (intent-id 943)
        attempted (atom [])
        sends (atom 0)
        cause (InterruptedException. "physical lease timeout")
        failure (try
                  (Thread/interrupted)
                  (with-redefs
                   [discovery/discover-eligible!
                    (fn [_ request]
                      (let [id (if (:after request) second-id first-id)]
                        {:candidates [{:intent-id id :action :acquire-pending}]
                         :has-more? (= id first-id)
                         :next-after (when (= id first-id) first-id)}))
                    acquisition/acquire-pending!
                    (fn [_ claim]
                      (swap! attempted conj claim)
                      (if (= (:intent-id claim) first-id)
                        {:status :contended :intent-id first-id}
                        (throw cause)))
                    delivery/dispatch-acquired!
                    (fn [& _] (swap! sends inc))]
                    (try
                      (cycle/process-cycle! :node {:source ::source} {} source-id
                                            {:worker-id "checked-worker" :max-items 3})
                      (catch Throwable error error)))
                  (finally
                    ;; Test flags below are captured separately in the assertion.
                    (Thread/interrupted)))
        data (ex-data failure)]
    (is (= :gesso.live.outbox/cycle-interrupted (:error/type data)))
    (is (= 1 (:completed-count data)))
    (is (= first-id (:last-completed-cursor data)))
    (is (= :contended (get-in data [:completed 0 :acquisition-status])))
    (is (= second-id (get-in data [:in-flight :intent-id])))
    (is (= (:lease-id (second @attempted))
           (get-in data [:in-flight :lease-id])))
    (is (= :unverified-acquisition-attempt (get-in data [:in-flight :evidence])))
    (is (= 2 (count @attempted)))
    (is (zero? @sends))))
