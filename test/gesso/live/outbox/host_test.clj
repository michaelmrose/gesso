(ns gesso.live.outbox.host-test
  "v2106: concrete canonical Biff host inventory and physically owned XTDB,
   Live, outbox worker and handler. No production HTTP listener is started."
  (:require [clojure.test :refer [deftest is testing]]
            [com.biffweb.core :as biff]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.core :as g]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button]]
            [gesso.live.core :as live]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- setup [f]
  (let [ops (standard-operations)
        ctx (render-context ops)]
    (with-surfaced-application
      {:request-board [:main (rendered-operation-button
                              ctx :request/claim "/operations/request/claim")]}
      (fn [{:keys [assembly]}]
        (f assembly
           (fn [_] (g/html-response
                    [:main (rendered-operation-button
                            ctx :request/claim "/operations/request/claim")])))))))

(defn- config []
  {:xtdb-config {}
   :source-id "gesso/live-host-v2106"
   :rules-context {}
   :worker-options {:worker-id "v2106-host-worker" :poll-ms 20}
   :live-options {:rules [{:when-topic :request
                           :expand (fn [_ change]
                                     [(select-keys change [:topic :id])])}]
                  :source-options {:coalesce-window-ms 0}
                  :dispatch-options {:threads 1 :queue-size 8 :on-overflow :throw}}})

(defn- problem [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- eventually [f]
  (loop [tries 0]
    (cond (f) true
          (>= tries 250) false
          :else (do (Thread/sleep 20) (recur (inc tries))))))

(deftest concrete-module-inventory-is-fixed-and-dependency-ordered-test
  (is (= [application/application-handler-module-id
          host/xtdb-module-id
          host/live-module-id
          managed/module-id]
         (host/module-ids)))
  (is (= host/start-order (host/module-ids)))
  (is (fn? (:biff.core/start host/xtdb-module)))
  (is (fn? (:biff.core/start host/live-module)))
  (is (fn? (:biff.core/stop host/xtdb-module)))
  (is (fn? (:biff.core/stop host/live-module))))

(deftest concrete-host-fails-closed-before-starting-invalid-config-test
  (doseq [[change reason]
          [[{:xtdb-config nil} :invalid-xtdb-config]
           [{:live-options {:rules {}}} :invalid-live-rules]
           [{:live-options {:rules [] :source (atom nil)}} :unowned-live-component]
           [{:live-options {:rules [] :dispatcher (atom nil)}} :unowned-live-component]
           [{:live-options {:rules [] :fragment-manager (atom nil)}} :unowned-live-component]
           [{:source-id " "} :invalid-source-id]
           [{:rules-context nil} :invalid-rules-context]
           [{:worker-options {:worker-id "w" :poll-ms 0}} :invalid-worker-options]
           [{:stop-timeout-ms -1} :invalid-stop-timeout]
           [{:extra :forged-module} :unknown-config-fields]]]
    (is (= reason (:reason (problem #(host/start! nil identity
                                                (merge (config) change)))))))
  (is (= :invalid-config
         (:reason (problem #(host/start! nil identity :invalid)))))
  (is (= :invalid-handler
         (:reason (problem #(host/start! nil :not-a-handler (config))))))
  (is (= :invalid-application-assembly
         (:reason (problem #(host/start! nil identity (config)))))))

(deftest concrete-host-starts-actual-providers-and-canonical-handler-test
  (setup
   (fn [assembly handler]
     (let [started (host/start! assembly handler (config))
           node (:xtdb/connectable started)
           live-system (:gesso.live/system started)
           worker (get started managed/handle-key)]
       (try
         (is (some? node))
         (is (some? live-system))
         (is (some? worker) "Lifecycle handle should be present")
         (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
         (is (not (identical? handler (:biff.ring/handler started))))
         (is (= 200 (:status ((:biff.ring/handler started) {:uri "/"}))))
         (is (eventually #(pos? (:cycles (lifecycle/status worker)))))
         (is (= "gesso/live-host-v2106" (:source-id (lifecycle/status worker))))
         (finally (host/stop! started)))
       (is (= :stopped (:status (lifecycle/status worker))))
       (is (live/closed? live-system))
       (is (thrown? Exception (xt/q node ["SELECT 1 AS x"])))))))

(deftest concrete-host-processes-new-persisted-outbox-intent-test
  (setup
   (fn [assembly handler]
     (let [started (host/start! assembly handler (config))
           node (:xtdb/connectable started)
           system (:gesso.live/system started)
           work (get started managed/handle-key)
           id (random-uuid)
           intent (outbox/prepare!
                   {:intent-ids [id]
                    :source-id "gesso/live-host-v2106"
                    :operation :request/claim
                    :purpose :gesso.live/refresh}
                   [{:topic :request :id "real-request"}])
           sub (source/subscribe (:source system) {:topic :request :id "real-request"})]
       (try
         (xt/execute-tx node (into (vec (:guard-ops intent))
                                   [[:put-docs :requests
                                     {:xt/id "real-request" :status :claimed}]
                                    (:tx-op intent)]))
         (is (= :claimed (:status (first (xt/q node ["SELECT * FROM requests WHERE _id = ?" "real-request"])))))
         (is (eventually
              #(= :dispatch-accepted
                  (:gesso.live.outbox/status
                   (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id]))))))
         (is (eventually #(= 1 (:total-dispatch-accepted
                               (lifecycle/status work)))))
         (let [event (deref (stream/take! sub) 1500 ::timeout)]
           (is (= "real-request" (:id event)))
           (is (some? (:progression event))))
         (finally
           (stream/close! sub)
           (host/stop! started)))
       (is (= :stopped (:status (lifecycle/status work))))
       (is (live/closed? system))))))

(deftest concrete-host-cleans-up-real-xtdb-if-live-provider-start-fails-test
  (setup
   (fn [assembly handler]
     (let [opened (atom nil)
           original-start xtn/start-node
           explosion (ex-info "live constructor failed" {:test :provider})
           data (with-redefs [xtn/start-node
                             (fn [opts]
                               (let [node (original-start opts)]
                                 (reset! opened node)
                                 node))
                             live/create (fn [& _] (throw explosion))]
                  (problem #(host/start! assembly handler (config))))]
       (is (= :gesso.live.outbox/checked-startup-failed (:error/type data)))
       (is (= :drained (:rollback/status data)))
       (is (some? @opened))
       (is (thrown? Exception (xt/q @opened ["SELECT 1 AS x"])))))))

(deftest concrete-host-ignores-a-foreign-source-without-leasing-it-test
  (setup
   (fn [assembly handler]
     (let [started (host/start! assembly handler (config))
           node (:xtdb/connectable started)
           worker (get started managed/handle-key)
           foreign-id (random-uuid)
           foreign (outbox/prepare!
                    {:intent-ids [foreign-id] :source-id "different/installed-live-source"
                     :operation :request/claim :purpose :gesso.live/refresh}
                    [{:topic :request :id "foreign-request"}])]
       (try
         (xt/execute-tx node (conj (vec (:guard-ops foreign)) (:tx-op foreign)))
         (is (eventually #(>= (:cycles (lifecycle/status worker)) 2)))
         (is (= :pending
                (:gesso.live.outbox/status
                 (first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?"
                                    foreign-id])))))
         (is (zero? (:total-processed (lifecycle/status worker))))
         (is (zero? (:total-dispatch-accepted (lifecycle/status worker))))
         (finally (host/stop! started)))))))

(deftest concrete-host-stop-timeout-fences-real-live-and-xtdb-providers-test
  (setup
   (fn [assembly handler]
     (let [started (host/start! assembly handler (config))
           node (:xtdb/connectable started)
           system (:gesso.live/system started)
           worker (get started managed/handle-key)
           original-stop lifecycle/retire!
           initial-timeout? (atom true)]
       (try
         (let [failure (with-redefs [lifecycle/retire!
                                    (fn [handle timeout]
                                      (if (compare-and-set! initial-timeout? true false)
                                        {:outcome :timed-out}
                                        (original-stop handle timeout)))]
                         (problem #(host/stop! started)))]
           (is (= :drain-timed-out (:reason failure)))
           (is (false? (live/closed? system)))
           (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
           (is (not (#{:stopped :failed} (:status (lifecycle/status worker))))))
         (finally
           (host/stop! started)))
       (is (= :stopped (:status (lifecycle/status worker))))
       (is (live/closed? system))
       (is (thrown? Exception (xt/q node ["SELECT 1 AS x"])))))))

(deftest concrete-host-binds-owned-providers-and-fences-retained-handler
  (setup
   (fn [assembly render]
     (let [seen (atom nil)
           during-start (atom nil)
           real-start managed/start-biff-application!
           started (with-redefs [managed/start-biff-application!
                                 (fn [& args]
                                   (let [system (apply real-start args)]
                                     (reset! during-start
                                             ((:biff.ring/handler system) {:uri "/early"}))
                                     system))]
                     (host/start! assembly
                                  (fn [request]
                                    (reset! seen request)
                                    (is (= [{:x 1}] (xt/q (live.xtdb/connectable-from request)
                                                          ["SELECT 1 AS x"])))
                                    (render request))
                                  (config)))
           handler (:biff.ring/handler started)]
       (try
         (is (= 503 (:status @during-start)))
         (is (nil? @seen))
         (is (= 200 (:status (handler {:uri "/"
                                      :xtdb/connectable :forged
                                      :xtdb/node :forged :xtdb/conn :forged
                                      :biff.xtdb/node :forged
                                      :biff.xtdb/connection-pool :forged
                                      :gesso.live/system :forged
                                      managed/source-id-key "foreign"}))))
         (is (identical? (:xtdb/connectable started) (:xtdb/connectable @seen)))
         (is (identical? (:xtdb/connectable started) (:biff.xtdb/node @seen)))
         (is (identical? (:gesso.live/system started) (:gesso.live/system @seen)))
         (is (= (:source-id (config)) (get @seen managed/source-id-key)))
         (is (not-any? #(contains? @seen %) [:xtdb/node :xtdb/conn :biff.xtdb/connection-pool]))
         (is (= "/" (:uri @seen)))
         (finally (biff/stop started)))
       (let [before @seen]
         (is (= 503 (:status (handler {:uri "/late"}))))
         (is (identical? before @seen)))))))

(deftest concrete-host-request-drain-fences-native-provider-stop-and-retries
  (setup
   (fn [assembly render]
     (let [entered (promise)
           release (promise)
           started (host/start! assembly
                                (fn [request]
                                  (deliver entered true)
                                  (when-not (= true (deref release 10000 false))
                                    (throw (ex-info "test request timed out" {})))
                                  (is (= [{:x 1}] (xt/q (:xtdb/connectable request)
                                                        ["SELECT 1 AS x"])))
                                  (render request))
                                (assoc (config) :stop-timeout-ms 50))
           handler (:biff.ring/handler started)
           node (:xtdb/connectable started)
           source (:gesso.live/system started)
           response (future (handler {:uri "/active"}))]
       (try
         (is (= true (deref entered 5000 false)))
         (let [failure (problem #(biff/stop started))]
           (is (= :request-drain-timed-out (:reason failure)))
           (is (= 1 (:active failure))))
         (is (= 503 (:status (handler {:uri "/new"}))))
         (is (false? (live/closed? source)))
         (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
         (finally
           (deliver release true)
           (is (= 200 (:status (deref response 10000 {}))))
           (loop [attempt 0]
             (let [failure (problem #(host/stop! started))]
               (when failure
                 (if (and (= :drain-timed-out (:reason failure)) (< attempt 100))
                   (do (Thread/sleep 20) (recur (inc attempt)))
                   (throw (ex-info "Host teardown failed" failure))))))))
       (is (live/closed? source))
       (is (= 503 (:status (handler {:uri "/after"}))))))))
