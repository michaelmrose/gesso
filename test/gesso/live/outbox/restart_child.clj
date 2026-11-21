(ns gesso.live.outbox.restart-child
  "Isolated subprocess entrypoint for physical XTDB log/storage restart evidence.

   The :seed and :post-dispatch-crash modes exit with Runtime.halt,
   deliberately bypassing shutdown hooks and XTDB close. The latter halts
   precisely after a subscribed Live source accepts an invalidation but
   before its fenced XTDB acknowledgment can execute. Subsequent modes
   recover and verify in independent JVMs. This is a test-only fixture."
  (:require [clojure.edn :as edn]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.source :as source]
            [manifold.stream :as s]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]
           [java.nio.file Files Paths]
           [java.util UUID]))

(def ^:private own-source "gesso/restart-physical")
(def ^:private foreign-source "gesso/foreign-restart")
(def ^:private own-ids
  [(UUID/fromString "00000000-0000-0000-0000-000000021001")
   (UUID/fromString "00000000-0000-0000-0000-000000021002")])
(def ^:private foreign-id
  (UUID/fromString "00000000-0000-0000-0000-000000021003"))

(defn- config [root]
  {:log [:local {:path (str root "/log")}]
   :storage [:local {:path (str root "/storage")}]})

(defn- row [node id]
  (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- require! [pred reason info]
  (when-not pred (throw (ex-info reason info))))

(defn- seed! [node]
  (let [plan (outbox/prepare!
              {:intent-ids own-ids :source-id own-source
               :operation :request/claim :purpose :gesso.live/refresh}
              (mapv (fn [id]
                      {:topic :request :id (str "request-" id)
                       :change/kind :updated}) own-ids))
        foreign (outbox/prepare!
                 {:intent-ids [foreign-id] :source-id foreign-source
                  :operation :request/claim :purpose :gesso.live/refresh}
                 [{:topic :request :id "foreign-request"}])]
    ;; Same physical XTDB transaction: authoritative row and pending intent.
    (xt/execute-tx node
                   (into (vec (:guard-ops plan))
                         [[:put-docs :requests
                           {:xt/id "original-claim" :status :claimed}]
                          (:tx-op plan)]))
    (xt/execute-tx node (into (vec (:guard-ops foreign)) [(:tx-op foreign)]))
    (let [now (Instant/now)
          claimed (acquisition/acquire-pending!
                   node {:intent-id (second own-ids)
                         :worker-id "abruptly-terminated-worker"
                         :source-id own-source
                         :lease-id (random-uuid)
                         :now now
                         :lease-until (.plusMillis now 1250)})]
      (require! (= :acquired (:status claimed))
                "Physical seed lease acquisition failed" {:result claimed})
      (require! (= :leased (:gesso.live.outbox/status
                           (row node (second own-ids))))
                "Claim was not physically indexed" {})
      {:pid (.pid (java.lang.ProcessHandle/current))
       :seeded true :pending-id (first own-ids)
       :leased-id (second own-ids) :foreign-id foreign-id
       :lease-id (:lease-id claimed)})))

(defn- eventually [pred]
  (loop [tries 0]
    (cond (pred) true
          (>= tries 500) false
          :else (do (Thread/sleep 20) (recur (inc tries))))))

(defn- recover! [node]
  (let [initial-pending (row node (first own-ids))
        initial-leased (row node (second own-ids))
        initial-foreign (row node foreign-id)
        domain (first (xt/q node ["SELECT * FROM requests WHERE _id = ?"
                                  "original-claim"]))]
    (require! (and (= :claimed (:status domain))
                   (= :pending (:gesso.live.outbox/status initial-pending))
                   (= :leased (:gesso.live.outbox/status initial-leased))
                   (= :pending (:gesso.live.outbox/status initial-foreign)))
              "State did not survive abrupt seed-process halt"
              {:domain domain :pending initial-pending :leased initial-leased
               :foreign initial-foreign})
    ;; Let the original 1.25s lease expire. On a new JVM, this does not
    ;; depend on the old process' in-memory state.
    (require! (eventually
               #(= #{(first own-ids) (second own-ids)}
                   (set (map :intent-id
                             (:candidates
                              (discovery/discover-eligible!
                               node {:source-id own-source}))))))
              "The restarted node never discovered pending and expired work" {})
    (let [system (live/create
                  {:rules [{:when-topic :request
                            :expand (fn [_ctx change]
                                      [(select-keys change
                                                    [:topic :id :change/kind])])}]
                   :source-options {:coalesce-window-ms 0}
                   :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}})]
      (try
        (let [streams (mapv #(source/subscribe (:source system)
                                                 {:topic :request
                                                  :id (str "request-" %)}) own-ids)
              handle (lifecycle/start!
                      node system {} own-source
                      {:worker-id "fresh-process" :poll-ms 20 :max-items 4})]
          (try
            (require! (eventually #(every?
                                    (fn [id]
                                      (= :dispatch-accepted
                                         (:gesso.live.outbox/status (row node id))))
                                    own-ids))
                      "Recovered worker did not terminalize both intents"
                      {:status (lifecycle/status handle)})
            (require! (eventually #(= 2 (:total-dispatch-accepted
                                         (lifecycle/status handle))))
                      "Recovered worker counters did not converge" {})
            (let [events (mapv #(deref (s/take! %) 3000 ::timeout) streams)
                  stopped (lifecycle/stop! handle 10000)]
              (require! (= :drained (:outcome stopped))
                        "Restarted worker failed to drain" {:stopped stopped})
              (require! (= (mapv #(str "request-" %) own-ids)
                           (mapv :id events))
                        "Expected actual Live invalidations were not observed"
                        {:events events})
              (require! (every? :progression events)
                        "Recovered Live notice lacks progression requirement"
                        {:events events})
              (require! (= :pending (:gesso.live.outbox/status
                                      (row node foreign-id)))
                        "Foreign-source intent was modified" {})
              {:pid (.pid (java.lang.ProcessHandle/current))
               :recovered true
               :before-statuses [:pending :leased :pending]
               :after-statuses (mapv
                                #(-> (row node %) :gesso.live.outbox/status)
                                (conj own-ids foreign-id))
               :events (mapv (fn [e] (select-keys e [:topic :id :change/kind])) events)
               :all-have-progression? (every? :progression events)
               :accepted-count (get-in stopped [:snapshot :total-dispatch-accepted])
               :foreign-remains-pending? (= :pending
                                            (:gesso.live.outbox/status
                                             (row node foreign-id)))})
            (finally
              (doseq [stream streams] (s/close! stream))
              (lifecycle/stop! handle 10000))))
        (finally (live/close! system))))))

(defn- verify! [node]
  ;; A third, entirely new process checks that terminal dispatch-acceptance
  ;; records persisted and that a fresh worker has no owned work to replay.
  (let [before (mapv #(-> (row node %) :gesso.live.outbox/status)
                     (conj own-ids foreign-id))]
    (require! (= [:dispatch-accepted :dispatch-accepted :pending] before)
              "Terminal acknowledgment did not survive another process restart"
              {:states before})
    (let [system (live/create
                  {:rules [{:when-topic :request
                            :expand (fn [_ctx change]
                                      [(select-keys change [:topic :id :change/kind])])}]
                   :source-options {:coalesce-window-ms 0}
                   :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}})]
      (try
        (let [handle (lifecycle/start!
                      node system {} own-source
                      {:worker-id "verification-process" :poll-ms 20 :max-items 4})]
          (try
            (require! (eventually #(>= (:cycles (lifecycle/status handle)) 1))
                      "Verification worker failed to poll" {})
            (let [stopped (lifecycle/stop! handle 10000)]
              (require! (= :drained (:outcome stopped))
                        "Verification worker shutdown failed" {})
              (require! (zero? (get-in stopped [:snapshot :total-dispatch-accepted]))
                        "Restarted worker replayed a terminal intent" {})
              {:pid (.pid (java.lang.ProcessHandle/current))
               :verified true
               :statuses before
               :resends (get-in stopped [:snapshot :total-dispatch-accepted])})
            (finally (lifecycle/stop! handle 10000))))
        (finally (live/close! system))))))

(defn- write-result! [path result]
  (spit path (pr-str result)))

(def ^:private interrupted-source "gesso/restart-after-source-acceptance")
(def ^:private interrupted-id
  (UUID/fromString "00000000-0000-0000-0000-000000021101"))
(def ^:private interrupted-subject "request-after-source-acceptance")

(defn- interrupted-system []
  (live/create
   {:rules [{:when-topic :request
             :expand (fn [_ctx change]
                       [(select-keys change [:topic :id :change/kind])])}]
    :source-options {:coalesce-window-ms 0}
    :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}))

(defn- post-dispatch-crash! [node output]
  ;; We intentionally crash in the callback that dispatch-acquired! calls
  ;; AFTER synchronous Live emission has returned and BEFORE acknowledgment
  ;; can execute. This tests an actual process-loss window rather than an
  ;; exception or a simulated rollback inside the same JVM.
  (let [plan (outbox/prepare!
              {:intent-ids [interrupted-id] :source-id interrupted-source
               :operation :request/claim :purpose :gesso.live/refresh}
              [{:topic :request :id interrupted-subject
                :change/kind :updated}])
        _ (xt/execute-tx node
                         (into (vec (:guard-ops plan))
                               [[:put-docs :requests
                                 {:xt/id interrupted-subject :status :claimed}]
                                (:tx-op plan)]))
        now (Instant/now)
        lease (acquisition/acquire-pending!
               node {:intent-id interrupted-id
                     :worker-id "source-accepted-then-crashed"
                     :source-id interrupted-source
                     :lease-id (random-uuid)
                     :now now :lease-until (.plusMillis now 2000)})
        system (interrupted-system)]
    (require! (= :acquired (:status lease))
              "Initial physical lease acquisition failed" {:lease lease})
    (let [tap (source/subscribe (:source system)
                                {:topic :request :id interrupted-subject})]
      (with-redefs [acknowledgment/acknowledge-dispatch!
                    (fn [_node _receipt]
                      ;; Source acceptance has occurred; the original method
                      ;; has NOT yet been called. Confirm an actual stream
                      ;; listener observed the event before abrupt halt.
                      (let [event (deref (s/take! tap) 4000 ::timeout)
                            persisted (row node interrupted-id)]
                        (require! (= interrupted-subject (:id event))
                                  "Source did not emit expected invalidation"
                                  {:event event})
                        (require! (and (some? (:progression event))
                                       (= (:progression event)
                                          (:progression lease)))
                                  "First accepted event lost its lease basis"
                                  {:event event})
                        (require! (and (= :leased
                                           (:gesso.live.outbox/status persisted))
                                       (= (:lease-id lease)
                                          (:gesso.live.outbox/lease-id persisted))
                                       (nil? (:gesso.live.outbox/dispatch-receipt-id
                                              persisted)))
                                  "Acknowledgment occurred before the crash point"
                                  {:persisted persisted})
                        (write-result! output
                                       {:pid (.pid (java.lang.ProcessHandle/current))
                                        :source-accepted-before-crash? true
                                        :event (select-keys event
                                                            [:topic :id :change/kind])
                                        :progression-present? (some? (:progression event))
                                        :lease-id (:lease-id lease)
                                        :pre-ack-status (:gesso.live.outbox/status
                                                         persisted)
                                        :receipt-before-crash
                                        (:gesso.live.outbox/dispatch-receipt-id
                                         persisted)})
                        (flush)
                        ;; No XTDB close or Live source shutdown can run here.
                        (.halt (Runtime/getRuntime) 0)))]
        (delivery/dispatch-acquired! node system {} lease interrupted-source))
      (throw (ex-info "Dispatch returned despite mandatory JVM halt"
                      {:intent-id interrupted-id})))))

(defn- post-dispatch-recover! [node]
  (let [before (row node interrupted-id)
        domain (first (xt/q node ["SELECT * FROM requests WHERE _id = ?"
                                  interrupted-subject]))
        former-lease-id (:gesso.live.outbox/lease-id before)]
    (require! (and (= :claimed (:status domain))
                   (= :leased (:gesso.live.outbox/status before))
                   (uuid? former-lease-id)
                   (nil? (:gesso.live.outbox/dispatch-receipt-id before)))
              "Accepted-but-unacknowledged state did not survive JVM halt"
              {:domain domain :outbox before})
    (require! (eventually
               #(= [{:intent-id interrupted-id :action :reclaim-expired}]
                   (:candidates (discovery/discover-eligible!
                                 node {:source-id interrupted-source}))))
              "Expired lease never became physically discoverable after crash" {})
    (let [system (interrupted-system)]
      (try
        (let [tap (source/subscribe (:source system)
                                    {:topic :request :id interrupted-subject})
              ;; The long poll interval keeps the completed recovery cycle's
              ;; :reclaimed count available until the test stops the worker.
              handle (lifecycle/start!
                      node system {} interrupted-source
                      {:worker-id "post-crash-recovery-worker"
                       :poll-ms 60000 :max-items 4})]
          (try
            (require! (eventually
                       #(= :dispatch-accepted
                           (:gesso.live.outbox/status (row node interrupted-id))))
                      "Recovery did not terminalize the accepted-but-unacked intent"
                      {:worker-status (lifecycle/status handle)})
            (require! (eventually
                       #(= 1 (:total-dispatch-accepted
                               (lifecycle/status handle))))
                      "Physical source acceptance was not recorded" {})
            (let [event (deref (s/take! tap) 4000 ::timeout)
                  stopped (lifecycle/stop! handle 10000)
                  after (row node interrupted-id)
                  last-cycle (get-in stopped [:snapshot :last-cycle])]
              (require! (= :drained (:outcome stopped))
                        "Recovery worker failed to stop" {:stop stopped})
              (require! (and (= interrupted-subject (:id event))
                             (= :updated (:change/kind event))
                             (some? (:progression event)))
                        "Recovered Live source did not re-emit invalidation"
                        {:event event})
              (require! (and (= 1 (:reclaimed last-cycle))
                             (= 1 (:dispatch-accepted last-cycle))
                             (not= former-lease-id
                                   (:gesso.live.outbox/lease-id after))
                             (= :dispatch-accepted
                                (:gesso.live.outbox/status after))
                             (uuid? (:gesso.live.outbox/dispatch-receipt-id after)))
                        "Recovery did not durably reclaim and acknowledge"
                        {:cycle last-cycle :row after})
              {:pid (.pid (java.lang.ProcessHandle/current))
               :recovered-after-source-acceptance? true
               :before-status (:gesso.live.outbox/status before)
               :after-status (:gesso.live.outbox/status after)
               :reclaimed-count (:reclaimed last-cycle)
               :accepted-count (:dispatch-accepted last-cycle)
               :new-lease-token? (not= former-lease-id
                                       (:gesso.live.outbox/lease-id after))
               :receipt-recorded? (uuid? (:gesso.live.outbox/dispatch-receipt-id
                                          after))
               :event (select-keys event [:topic :id :change/kind])
               :progression-present? (some? (:progression event))})
            (finally
              (s/close! tap)
              (lifecycle/stop! handle 10000))))
        (finally (live/close! system))))))

(defn- post-dispatch-verify! [node]
  ;; A THIRD process confirms the successful recovery state is durable and
  ;; a fresh lifecycle has no dispatchable work for this source.
  (let [persisted (row node interrupted-id)
        domain (first (xt/q node ["SELECT * FROM requests WHERE _id = ?"
                                  interrupted-subject]))]
    (require! (and (= :claimed (:status domain))
                   (= :dispatch-accepted (:gesso.live.outbox/status persisted))
                   (uuid? (:gesso.live.outbox/dispatch-receipt-id persisted))
                   (empty? (:candidates (discovery/discover-eligible!
                                         node {:source-id interrupted-source}))))
              "Post-recovery terminal state was not durable"
              {:domain domain :outbox persisted})
    (let [system (interrupted-system)]
      (try
        (let [handle (lifecycle/start!
                      node system {} interrupted-source
                      {:worker-id "post-recovery-verifier" :poll-ms 60000})]
          (try
            (require! (eventually #(>= (:cycles (lifecycle/status handle)) 1))
                      "Third JVM worker did not poll" {})
            (let [stopped (lifecycle/stop! handle 10000)]
              (require! (and (= :drained (:outcome stopped))
                             (zero? (get-in stopped
                                            [:snapshot :total-dispatch-accepted])))
                        "Terminal outbox intent was replayed after acknowledgment"
                        {:stop stopped})
              {:pid (.pid (java.lang.ProcessHandle/current))
               :terminal-state-survived? true
               :status (:gesso.live.outbox/status persisted)
               :resends (get-in stopped
                                [:snapshot :total-dispatch-accepted])})
            (finally (lifecycle/stop! handle 10000))))
        (finally (live/close! system))))))


(defn -main [mode root output]
  (try
    (case mode
      "seed"
      (do
        ;; Intentionally NO with-open: sudden process exit is the failure
        ;; this scenario tests, not an ordinary node shutdown.
        (let [node (xtn/start-node (config root))
              result (seed! node)]
          (write-result! output result)
          (flush)
          (.halt (Runtime/getRuntime) 0)))
      "recover"
      (let [result (with-open [node (xtn/start-node (config root))]
                     (recover! node))]
        (write-result! output result)
        (shutdown-agents))
      "verify"
      (let [result (with-open [node (xtn/start-node (config root))]
                     (verify! node))]
        (write-result! output result)
        (shutdown-agents))
      "post-dispatch-crash"
      (post-dispatch-crash! (xtn/start-node (config root)) output)
      "post-dispatch-recover"
      (let [result (with-open [node (xtn/start-node (config root))]
                     (post-dispatch-recover! node))]
        (write-result! output result)
        (shutdown-agents))
      "post-dispatch-verify"
      (let [result (with-open [node (xtn/start-node (config root))]
                     (post-dispatch-verify! node))]
        (write-result! output result)
        (shutdown-agents))
      (throw (ex-info "Unknown restart test mode" {:mode mode})))
    (catch Throwable cause
      (binding [*out* *err*]
        (.printStackTrace cause))
      (System/exit 2))))
