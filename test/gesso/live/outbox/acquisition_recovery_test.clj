(ns gesso.live.outbox.acquisition-recovery-test
  "Explicit, source-scoped recovery after physical acquisition ambiguity.

   No Live source dispatch is allowed on an unverified acquisition token;
   a successor must independently rediscover and acquire or reclaim with
   XTDB's physical transaction guards, not inherit that reported token."
  (:require [clojure.test :refer [deftest is]]
            [com.biffweb.core :as biff]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.worker :as worker]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(def ^:private source-id "gesso/test-acquisition-recovery")
(def ^:dynamic *modules* [])

(defn- system! []
  (live/create {:rules [{:when-topic :request
                         :expand (fn [_ctx change]
                                   [(select-keys change [:topic :id :change/kind])])}]
                :source-options {:coalesce-window-ms 0}
                :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- seed! [node id]
  (let [p (outbox/prepare!
           {:intent-ids [id] :source-id source-id
            :operation :request/claim :purpose :gesso.live/refresh}
           [{:topic :request :id (str "request-" id) :change/kind :updated}])]
    (xt/execute-tx node (into (vec (:guard-ops p)) [(:tx-op p)]))))

(defn- current! [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 300) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex
         (when (= :gesso.live.outbox/invalid-lifecycle
                  (:error/type (ex-data ex)))
           (:reason (ex-data ex))))))

(deftest physically-committed-acquisition-with-lost-reply-is-reclaimed-test
  (with-open [node (xtn/start-node {})]
    (let [system (system!)
          intent (random-uuid)
          original-acquire acquisition/acquire-pending!
          send-count (atom 0)]
      (try
        (seed! node intent)
        (let [sub (source/subscribe (:source system)
                                    {:topic :request :id (str "request-" intent)})]
          (try
            (let [old (with-redefs
                        [acquisition/acquire-pending!
                         (fn [node claim]
                           (swap! send-count inc)
                           (is (= :acquired
                                  (:status (original-acquire node claim))))
                           (throw (ex-info "physical acquisition committed, reply lost"
                                           {:error/type :test/acquisition-reply-lost})))]
                        (let [handle (lifecycle/start!
                                      node system {} source-id
                                      {:worker-id "acquisition-old"
                                       :poll-ms 20 :lease-millis 1300})]
                          (is (eventually
                               #(= :failed (:status (lifecycle/status handle)))))
                          (is (= :failed (:outcome (lifecycle/stop! handle 3000))))
                          handle))
                  failure (:failure (lifecycle/status old))
                  reported (:in-flight failure)
                  old-physical (current! node intent)]
              (is (= :acquisition (:phase failure)))
              (is (= :acquisition-exception (:reason failure)))
              (is (= :unverified-acquisition-attempt (:evidence reported)))
              (is (= intent (:intent-id reported)))
              (is (= :leased (:gesso.live.outbox/status old-physical)))
              (is (= (:lease-id reported)
                     (:gesso.live.outbox/lease-id old-physical)))
              (is (= 1 @send-count) "Only one pre-dispatch acquisition attempt")
              (is (= 0 (:total-dispatch-accepted (lifecycle/status old))))
              (let [{:keys [handle recovery-mode reconciliation]}
                    (lifecycle/restart-after-acquisition!
                     old {:worker-id "acquisition-successor"
                          :poll-ms 20 :lease-millis 1300 :max-items 1})]
                (try
                  (is (= :acquisition-rescan recovery-mode))
                  (is (= :same-lease-active (:status reconciliation)))
                  (is (= :already-restarted
                         (reason #(lifecycle/restart-after-acquisition!
                                   old {:worker-id "another-successor"}))))
                  (is (eventually #(= :dispatch-accepted
                                      (:gesso.live.outbox/status
                                       (current! node intent)))))
                  (is (= (str "request-" intent)
                         (:id (deref (stream/take! sub) 3000 ::timeout))))
                  (is (not= (:lease-id reported)
                            (:gesso.live.outbox/lease-id (current! node intent))))
                  (is (= "acquisition-successor"
                         (:gesso.live.outbox/lease-owner (current! node intent))))
                  (is (= 0 (:total-dispatch-accepted (lifecycle/status old))))
                  (finally
                    (is (= :drained (:outcome (lifecycle/stop! handle 5000))))))))
            (finally (stream/close! sub))))
        (finally (live/close! system))))))

(deftest unidentified-acquisition-failure-can-rescan-through-biff-test
  ;; The worker's discovery read failed before selecting any identity. The
  ;; explicitly managed successor must perform a new physical source scan,
  ;; not claim a non-existent lease or replay a delivery.
  (with-open [node (xtn/start-node {})]
    (let [id (random-uuid)
          _ (seed! node id)
          sys (system!)
          provider {:biff.core/id :fixture/acquisition-provider
                    :biff.core/start
                    (fn [system]
                      (assoc system :xtdb/connectable node :gesso.live/system sys))
                    :biff.core/stop (fn [_] (live/close! sys))}
          actual-worker worker/acquire-sweep!]
      (try
        (binding [*modules* [provider managed/module]]
          (let [started
                (with-redefs [worker/acquire-sweep!
                              (fn [_ _]
                                (throw (ex-info "discovery failed without identity"
                                                {:error/type :test/discovery-unknown})))]
                  (let [system (biff/start
                                {managed/source-id-key source-id
                                 managed/context-key {}
                                 managed/options-key
                                 {:worker-id "unidentified-old" :poll-ms 20}}
                                #'*modules*
                                [:fixture/acquisition-provider managed/module-id])
                        handle (get system managed/handle-key)]
                    (is (eventually #(= :failed (:status (lifecycle/status handle)))))
                    system))
                original (get started managed/handle-key)]
            (try
              (is (= :unidentified
                     (:status (lifecycle/inspect-failure! original))))
              (is (nil? (get-in (lifecycle/status original)
                                [:failure :in-flight])))
              (let [{:keys [handle recovery-mode reconciliation]}
                    (managed/recover-module!
                     started :acquisition-rescan
                     {:worker-id "unidentified-new" :poll-ms 20})]
                (is (= :acquisition-rescan recovery-mode))
                (is (= :unidentified (:status reconciliation)))
                (is (eventually #(= :dispatch-accepted
                                    (:gesso.live.outbox/status (current! node id)))))
                (is (= "unidentified-new"
                       (:gesso.live.outbox/lease-owner (current! node id))))
                (is (not= :failed (:status (lifecycle/status handle)))))
              (finally (biff/stop started)))))
        (finally (live/close! sys))))))

(deftest acquisition-rescan-refuses-delivery-failure-or-contradictory-prefix-test
  (with-open [node (xtn/start-node {})]
    (let [sys (system!)
          id (random-uuid)
          _ (seed! node id)
          now (Instant/now)
          lease-id (random-uuid)
          acquired (acquisition/acquire-pending!
                    node {:intent-id id :lease-id lease-id
                          :worker-id "prefix-old" :source-id source-id
                          :now now :lease-until (.plusSeconds now 60)})
          receipt (random-uuid)
          ack (acknowledgment/acknowledge-dispatch!
               node {:intent-id id :lease-id lease-id :worker-id "prefix-old"
                     :receipt {:receipt-id receipt
                               :adapter :gesso.live/synchronous-source
                               :result :accepted}})
          prefix [{:intent-id id :acquisition-status :acquired
                   :status :dispatch-accepted :worker-id "prefix-old"
                   :lease-id lease-id :receipt-id receipt
                   :progression (:progression acquired)}]
          phase (atom :acquisition)
          old (with-redefs [cycle/process-cycle!
                            (fn [& _]
                              (throw (ex-info "synthetic cycle interruption"
                                              {:error/type :gesso.live.outbox/cycle-interrupted
                                               :phase @phase
                                               :reason :acquisition-exception
                                               :completed prefix :completed-count 1
                                               :last-completed-cursor id
                                               :in-flight nil :rescan-required? true})))]
                (let [handle (lifecycle/start! node sys {} source-id
                                               {:worker-id "prefix-old" :poll-ms 20})]
                  (is (eventually #(= :failed (:status (lifecycle/status handle)))))
                  (is (= :failed (:outcome (lifecycle/stop! handle 3000))))
                  handle))]
      (try
        (is (= :acknowledged (:status ack)))
        (is (= :matching-acknowledgment-recorded
               (get-in (lifecycle/inspect-failure! old)
                       [:completed-observations 0 :status])))
        ;; Retain the lease and receipt but change the physical worker owner.
        (let [before (current! node id)]
          (xt/execute-tx node [[:put-docs outbox/intent-table
                                (assoc before :gesso.live.outbox/lease-owner "wrong-worker")]])
          (is (= :recovery-not-qualified
                 (reason #(lifecycle/restart-after-acquisition!
                           old {:worker-id "rejected-prefix"}))))
          (is (= :different-acknowledgment-recorded
                 (get-in (lifecycle/inspect-failure! old)
                         [:completed-observations 0 :status])))
          (xt/execute-tx node [[:put-docs outbox/intent-table before]])
          (is (= :matching-acknowledgment-recorded
                 (get-in (lifecycle/inspect-failure! old)
                         [:completed-observations 0 :status]))))
        (let [{:keys [handle reconciliation]}
              (lifecycle/restart-after-acquisition!
               old {:worker-id "qualified-prefix" :poll-ms 20})]
          (try
            (is (= :matching-acknowledgment-recorded
                   (get-in reconciliation [:completed-observations 0 :status])))
            (is (eventually #(pos? (:cycles (lifecycle/status handle)))))
            (is (= 0 (:total-dispatch-accepted (lifecycle/status handle))))
            (finally (lifecycle/stop! handle 3000))))
        (finally (live/close! sys))))))

(deftest acquisition-only-recovery-refuses-post-send-cycle-failure-test
  ;; A deliberately named acquisition recovery must NOT stand in for the
  ;; duplicate-tolerant policy required after an uncertain source emission.
  (let [fake-failure {:error/type :gesso.live.outbox/cycle-interrupted
                      :phase :delivery :reason :acquisition-exception
                      :completed [] :completed-count 0
                      :last-completed-cursor nil :in-flight nil
                      :rescan-required? true}
        handle (with-redefs [cycle/process-cycle!
                            (fn [& _] (throw (ex-info "send outcome unknown"
                                                     fake-failure)))]
                 (let [h (lifecycle/start! :unused {:source :fake} {} source-id
                                           {:worker-id "failed-send" :poll-ms 20})]
                   (is (eventually #(= :failed (:status (lifecycle/status h)))))
                   (is (= :failed (:outcome (lifecycle/stop! h 3000))))
                   h))]
    (is (= :recovery-not-qualified
           (reason #(lifecycle/restart-after-acquisition!
                     handle {:worker-id "cannot-launder-send"}))))
    (is (= :failed (:status (lifecycle/status handle))))))
