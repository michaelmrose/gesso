(ns gesso.live.outbox.host-restart-child
  "v2107: concrete, canonical Gesso/Biff host across three independent JVMs.

   :crash starts real host providers and atomically writes the domain/outbox,
   witnesses source acceptance, then HALTS before XTDB acknowledgment.
   :recover boots that same host over the persisted files and reclaims.
   :verify boots a third host to confirm terminal state without replay.
   This is a test fixture, not a deployed HTTP server or browser proof."
  (:require [clojure.edn :as edn]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acknowledgment :as acknowledgment]
            [gesso.live.outbox.delivery :as delivery]
            [gesso.live.outbox.discovery :as discovery]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import [java.util UUID]))

(def ^:private source-id "gesso/physical-host-restart")
(def ^:private other-source "gesso/foreign-host-restart")
(def ^:private intent-id
  (UUID/fromString "00000000-0000-0000-0000-000000021701"))
(def ^:private foreign-id
  (UUID/fromString "00000000-0000-0000-0000-000000021702"))
(def ^:private subject-id "v2107-claim")

(defn- config [root worker]
  {:xtdb-config {:log [:local {:path (str root "/log")}]
                 :storage [:local {:path (str root "/storage")}]}
   :source-id source-id
   :worker-options {:worker-id worker
                    :poll-ms 25
                    ;; The first source emits, then its entire process dies.
                    ;; The new host must reclaim, not immediately assume
                    ;; that the previous lease was unacquired.
                    :lease-millis 12000}
   :rules-context {}
   :live-options {:rules [{:when-topic :request
                           :expand (fn [_ change]
                                     [(select-keys change [:topic :id :change/kind])])}]
                  :source-options {:coalesce-window-ms 0}
                  :dispatch-options {:threads 1 :queue-size 8
                                     :on-overflow :throw}}})

(defn- assembly-host! [root worker f]
  (let [ops (standard-operations)
        render (render-context ops)]
    (with-surfaced-application
      {:request-board
       [:main (rendered-operation-button render :request/claim
                                         "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (let [handler
              (fn [_]
                (g/html-response
                 [:main (rendered-operation-button
                         render :request/claim "/operations/request/claim")]))
              started (host/start! assembly handler (config root worker))]
          (f started))))))

(defn- stable-query [node query]
  ;; XTDB 2.2.0-beta1 occasionally invalidates a SELECT * plan while the
  ;; outbox document adds lease/receipt fields. Retry ONLY that typed schema
  ;; replan conflict and cap retries; do not conceal unrelated query failures.
  (loop [remaining 8]
    (let [result (try {:value (xt/q node query)}
                      (catch Throwable e {:failure e}))]
      (if-let [error (:failure result)]
        (if (and (pos? remaining)
                 (= :prepared-query-out-of-date
                    (:xtdb.error/code (ex-data error))))
          (do (Thread/sleep 30) (recur (dec remaining)))
          (throw error))
        (:value result)))))

(defn- row [node id]
  (first (stable-query node
                       ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id])))

(defn- domain [node]
  (first (stable-query node
                       ["SELECT * FROM requests WHERE _id = ?" subject-id])))

(defn- require! [pred reason details]
  (when-not pred (throw (ex-info reason details))))

(defn- eventually [pred]
  (loop [i 0]
    (cond (pred) true
          (>= i 1000) false
          :else (do (Thread/sleep 20) (recur (inc i))))))

(defn- verified-handler! [started]
  (require! (= 200 (:status ((:biff.ring/handler started) {:uri "/"})))
            "Canonical Gesso Ring handler failed" {}))

(defn- seed! [node]
  (let [primary (outbox/prepare!
                 {:intent-ids [intent-id] :source-id source-id
                  :operation :request/claim :purpose :gesso.live/refresh}
                 [{:topic :request :id subject-id :change/kind :updated}])
        foreign (outbox/prepare!
                 {:intent-ids [foreign-id] :source-id other-source
                  :operation :request/claim :purpose :gesso.live/refresh}
                 [{:topic :request :id "other-source-only"}])]
    (xt/execute-tx node
                   (into (vec (:guard-ops primary))
                         [[:put-docs :requests
                           {:xt/id subject-id :status :claimed}]
                          (:tx-op primary)]))
    (xt/execute-tx node
                   (conj (vec (:guard-ops foreign)) (:tx-op foreign)))))

(defn- write-result! [file m]
  (spit file (pr-str (assoc m :pid (.pid (java.lang.ProcessHandle/current)))))
  (flush))

(def ^:private crash-context (atom {}))

(defn- crash! [root output]
  (let [intercepted? (atom false)]
    (with-redefs
      [acknowledgment/acknowledge-dispatch!
       (fn [node receipt]
         ;; Synchronous Live source emission has already completed, but no
         ;; acknowledgment CAS has executed. Explicitly witness the stream.
         (require! (compare-and-set! intercepted? false true)
                   "Acknowledgment called more than once" {})
         (let [tap (:v2107/tap @crash-context)
               event (deref (stream/take! tap) 4000 ::timeout)
               pending (row node intent-id)
               foreign (row node foreign-id)]
           (require! (and (= subject-id (:id event))
                          (= :updated (:change/kind event))
                          (some? (:progression event)))
                     "Actual source acceptance was not observed" {:event event})
           (require! (and (= :leased (:gesso.live.outbox/status pending))
                          (uuid? (:gesso.live.outbox/lease-id pending))
                          (nil? (:gesso.live.outbox/dispatch-receipt-id pending))
                          (= :pending (:gesso.live.outbox/status foreign)))
                     "Source-accepted crash point not physically leased"
                     {:pending pending :foreign foreign})
           (write-result! output
                          {:phase :crash
                           :source-accepted? true
                           :pre-ack-status (:gesso.live.outbox/status pending)
                           :pre-ack-receipt (:gesso.live.outbox/dispatch-receipt-id
                                              pending)
                           :lease-id (:gesso.live.outbox/lease-id pending)
                           :logical-event (select-keys event
                                                       [:topic :id :change/kind])
                           :progression? (some? (:progression event))
                           :foreign-status (:gesso.live.outbox/status foreign)
                           :module-ids (host/module-ids)})
           ;; Neither host/stop! nor XTDB close nor JVM shutdown hooks run.
           (.halt (Runtime/getRuntime) 0)))]
      (assembly-host! root "before-hard-crash"
                      (fn [started]
                        (verified-handler! started)
                        (let [tap (source/subscribe
                                   (:source (:gesso.live/system started))
                                   {:topic :request :id subject-id})]
                          (swap! crash-context assoc :v2107/tap tap)
                          (seed! (:xtdb/connectable started))
                          ;; Give the installed worker time to discover,
                          ;; acquire and emit. The interception hard-halts
                          ;; this JVM; getting here after the deadline fails.
                          (Thread/sleep 20000)
                          (throw (ex-info "Worker failed to reach crash point"
                                          {:test :v2107
                                           :status (lifecycle/status
                                                    (get started managed/handle-key))}))))))))

(defn- recover! [root output]
  ;; A barrier on the dispatch adapter makes the subscription deterministic
  ;; even if the original lease expired before this JVM finished starting.
  ;; The real adapter is still used after the barrier is released.
  (let [original delivery/dispatch-acquired!
        crashed-lease (:lease-id
                       (edn/read-string (slurp (str root "/crash.edn"))))
        ready (promise)]
    (with-redefs [delivery/dispatch-acquired!
                  (fn [& args]
                    (require! (true? (deref ready 25000 false))
                              "Recovery subscription not ready" {})
                    (apply original args))]
      (assembly-host!
       root "after-hard-crash"
       (fn [started]
         (let [node (:xtdb/connectable started)
               system (:gesso.live/system started)
               handle (get started managed/handle-key)
               tap (source/subscribe (:source system)
                                     {:topic :request :id subject-id})]
           (try
             (verified-handler! started)
             (let [old (row node intent-id)
                   foreign (row node foreign-id)]
               (require! (and (= :claimed (:status (domain node)))
                              (= :leased (:gesso.live.outbox/status old))
                              (nil? (:gesso.live.outbox/dispatch-receipt-id old))
                              (= :pending (:gesso.live.outbox/status foreign)))
                         "Host did not reopen the crash-time persisted state"
                         {:old old :domain (domain node) :foreign foreign})
               (deliver ready true)
               (require! (eventually
                          #(= :dispatch-accepted
                              (:gesso.live.outbox/status (row node intent-id))))
                         "Reopened host did not finish recovered dispatch"
                         {:worker (lifecycle/status handle)})
               (require! (eventually
                          #(= 1 (:total-dispatch-accepted
                                  (lifecycle/status handle))))
                         "Reopened host did not report source acceptance" {})
               (let [event (deref (stream/take! tap) 4000 ::timeout)
                     after (row node intent-id)]
                 (require! (and (= subject-id (:id event))
                                (some? (:progression event))
                                (uuid? crashed-lease)
                                (not= crashed-lease
                                      (:gesso.live.outbox/lease-id after))
                                (= "after-hard-crash"
                                   (:gesso.live.outbox/lease-owner after))
                                (uuid? (:gesso.live.outbox/dispatch-receipt-id after)))
                           "Reclaimed host work was not physically witnessed"
                           {:event event :after after :crashed-lease crashed-lease})
                 (let [result {:phase :recover
                               :recovered? true
                               :before-status (:gesso.live.outbox/status old)
                               :after-status (:gesso.live.outbox/status after)
                               :reclaimed? (and (not= crashed-lease
                                                      (:gesso.live.outbox/lease-id after))
                                                (= "after-hard-crash"
                                                   (:gesso.live.outbox/lease-owner after)))
                               :new-lease? (not= crashed-lease
                                                (:gesso.live.outbox/lease-id after))
                               :receipt? (uuid? (:gesso.live.outbox/dispatch-receipt-id
                                                 after))
                               :logical-event (select-keys event
                                                           [:topic :id :change/kind])
                               :progression? (some? (:progression event))
                               :foreign-status (:gesso.live.outbox/status
                                                (row node foreign-id))
                               :module-ids (host/module-ids)}]
                   (host/stop! started)
                   (require! (live/closed? system)
                             "Recovered host did not close Live" {})
                   (require! (try (xt/q node ["SELECT 1 AS x"]) false
                                  (catch Throwable _ true))
                             "Recovered host did not close XTDB" {})
                   (write-result! output result))))
             (finally
               (deliver ready true)
               (stream/close! tap)
               (when-not (live/closed? system)
                 (host/stop! started))))))))))

(defn- verify! [root output]
  (assembly-host!
   root "terminal-verification"
   (fn [started]
     (let [node (:xtdb/connectable started)
           system (:gesso.live/system started)
           worker (get started managed/handle-key)]
       (try
         (verified-handler! started)
         (require! (= :claimed (:status (domain node)))
                   "Domain write did not survive second restart" {})
         (require! (= :dispatch-accepted (:gesso.live.outbox/status
                                          (row node intent-id)))
                   "Terminal acknowledgment did not persist" {})
         (require! (= :pending (:gesso.live.outbox/status (row node foreign-id)))
                   "Foreign outbox source was modified" {})
         (require! (empty? (:candidates
                            (discovery/discover-eligible!
                             node {:source-id source-id})))
                   "Terminal owned intent reappeared in discovery" {})
         (require! (eventually #(>= (:cycles (lifecycle/status worker)) 1))
                   "Terminal host did not poll" {})
         (require! (and (zero? (:total-dispatch-accepted
                                 (lifecycle/status worker)))
                        (zero? (:total-processed (lifecycle/status worker))))
                   "Terminal intent replayed from third JVM" {})
         (let [report {:phase :verify
                       :verified? true
                       :status (:gesso.live.outbox/status (row node intent-id))
                       :foreign-status (:gesso.live.outbox/status
                                         (row node foreign-id))
                       :resends (:total-dispatch-accepted (lifecycle/status worker))
                       :module-ids (host/module-ids)}]
           (host/stop! started)
           (require! (live/closed? system)
                     "Verification host failed to close Live" {})
           (write-result! output report))
         (finally
           (when-not (live/closed? system)
             (host/stop! started))))))))

(defn -main [phase root output]
  (try
    (case phase
      "crash" (crash! root output)
      "recover" (recover! root output)
      "verify" (verify! root output)
      (throw (ex-info "Unknown host restart phase" {:phase phase})))
    (shutdown-agents)
    (catch Throwable ex
      (binding [*out* *err*] (.printStackTrace ex))
      (System/exit 2))))
