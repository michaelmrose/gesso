(ns gesso.live.outbox.restart-child
  "Isolated subprocess entrypoint for physical XTDB log/storage restart evidence.

   Only the :seed mode exits by Runtime.halt, deliberately bypassing
   shutdown hooks and XTDB close. The :recover mode boots a fresh XTDB and
   Live source with the same physical directories and runs the worker.
   This is a test fixture, never a production entrypoint."
  (:require [clojure.edn :as edn]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
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
      (throw (ex-info "Unknown restart test mode" {:mode mode})))
    (catch Throwable cause
      (binding [*out* *err*]
        (.printStackTrace cause))
      (System/exit 2))))
