(ns gesso.live.outbox.host-test
  "v2106: concrete canonical Biff host inventory and physically owned XTDB,
   Live, outbox worker, handler and explicitly selected JDK listener."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.biffweb.core :as biff]
            [gesso.live.consistency.xtdb :as live.xtdb]
            [gesso.core :as g]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.fixture
             :refer [with-surfaced-application standard-operations render-context
                     rendered-operation-button custom-route-capabilities]]
            [gesso.live.core :as live]
            [gesso.live.flow :as flow]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-jdk :as jdk]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.progression :as progression]
            [gesso.live.source :as source]
            [manifold.stream :as stream]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.nio.charset StandardCharsets]))

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
           [{:xtdb-submission-policy! nil} :invalid-xtdb-submission-policy]
           [{:history-audit {}} :history-audit-requires-mapped-writer]
           [{:mapped-writer-policy nil} :invalid-mapped-writer-policy]
           [{:mapped-writer-policy {} :xtdb-submission-policy! identity}
            :conflicting-xtdb-submission-policies]
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
         (is (nil? (host/history-status started)))
         (is (= :history-audit-not-selected (:reason (problem #(host/audit-history! started)))))
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
                  (:outbox-status
                   (first (xt/q node ["SELECT gesso$live$outbox$status AS outbox_status FROM gesso_live_outbox WHERE _id = ?" id]))))))
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

(defn- network-request [listener path]
  (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))
      (.timeout (Duration/ofSeconds 5))
      (.build)))

(deftest canonical-network-host-owns-actual-checked-listener-and-startup-fence
  (setup
   (fn [assembly render]
     (let [listener* (atom nil)
           early* (atom nil)
           seen (atom nil)
           real-jdk jdk/start!
           real-worker lifecycle/start!
           client (HttpClient/newHttpClient)
           started (with-redefs [jdk/start! (fn [& args]
                                              (let [listener (apply real-jdk args)]
                                                (reset! listener* listener)
                                                listener))
                                 lifecycle/start! (fn [& args]
                                                    (reset! early*
                                                            (.send client (network-request @listener* "/early")
                                                                   (HttpResponse$BodyHandlers/ofString)))
                                                    (apply real-worker args))]
                     (host/start! assembly
                                  (fn [request]
                                    (reset! seen request)
                                    (render request))
                                  (assoc (config) :http-jdk {})))
           listener (get started host/listener-key)]
       (try
         (is (identical? listener @listener*))
         (is (= 503 (.statusCode @early*)))
         (is (nil? @seen))
         (is (= [application/application-handler-module-id host/xtdb-module-id
                 host/live-module-id host/listener-module-id managed/module-id]
                (host/module-ids true)))
         (let [response (.send client (network-request listener "/ready?x=1")
                               (HttpResponse$BodyHandlers/ofString))]
           (is (= 200 (.statusCode response)))
           (is (.contains (.body response) "/operations/request/claim"))
           (is (= "/ready" (:uri @seen)))
           (is (= "x=1" (:query-string @seen)))
           (is (identical? (:xtdb/connectable started) (:xtdb/connectable @seen)))
           (is (identical? (:gesso.live/system started) (:gesso.live/system @seen))))
         (finally (biff/stop started)))
       (is ((:drained? listener)))
       (is (live/closed? (:gesso.live/system started)))
       (is (thrown? java.io.IOException
                    (.send client (network-request listener "/late")
                           (HttpResponse$BodyHandlers/ofString))))))))

(deftest canonical-network-startup-rolls-back-listener-on-worker-failure
  (setup
   (fn [assembly render]
     (let [listener* (atom nil)
           node* (atom nil)
           live* (atom nil)
           real-jdk jdk/start!
           real-node xtn/start-node
           real-live live/create
           failure (with-redefs [jdk/start! (fn [& args]
                                              (let [listener (apply real-jdk args)]
                                                (reset! listener* listener) listener))
                                 xtn/start-node (fn [& args]
                                                  (let [node (apply real-node args)]
                                                    (reset! node* node) node))
                                 live/create (fn [& args]
                                               (let [source (apply real-live args)]
                                                 (reset! live* source) source))
                                 lifecycle/start! (fn [& _] (throw (ex-info "worker start failed" {})))]
                     (problem #(host/start! assembly render (assoc (config) :http-jdk {}))))]
       (is (= :gesso.live.outbox/checked-startup-failed (:error/type failure)))
       (is (= :drained (:rollback/status failure)))
       (is (some? @listener*))
       (is ((:drained? @listener*)))
       (is (live/closed? @live*))
       (is (thrown? Exception (xt/q @node* ["SELECT 1 AS x"])))))))

(deftest canonical-network-host-retains-providers-until-physical-writer-drains
  (setup
   (fn [assembly _]
     (let [body (stream/stream 4)
           started (host/start! assembly
                                (fn [_] {:status 200 :headers {"content-type" "text/event-stream"}
                                         :body body})
                                (assoc (config) :http-jdk {:drain-timeout-ms 0}))
           listener (get started host/listener-key)
           node (:xtdb/connectable started)
           source (:gesso.live/system started)
           client (HttpClient/newHttpClient)]
       (try
         (with-open [input (.body (.send client (network-request listener "/held")
                                        (HttpResponse$BodyHandlers/ofInputStream)))]
           (is (eventually #(= 1 ((:active-count listener)))))
           (is (= :listener-not-drained (:reason (problem #(biff/stop started)))))
           (is (false? ((:drained? listener))))
           (is (false? (live/closed? source)))
           (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
           (is (= 503 (:status ((:biff.ring/handler started) {:uri "/late"})))))
         (finally
           (stream/close! body)
           (is (eventually #((:drained? listener))))
           (host/stop! started)))
       (is (live/closed? source))
       (is (thrown? Exception (xt/q node ["SELECT 1 AS x"])))))))

(deftest canonical-host-owns-authorized-sse-over-real-http
  (setup
   (fn [assembly render]
     (let [seen (atom [])
           started (host/start! assembly render
                                (assoc (config) :http-jdk {}
                                       :authorized-sse
                                       {:path "/events"
                                        :authorize-subscription
                                        (fn [request]
                                          (swap! seen conj request)
                                          (when (= "allowed" (get-in request [:headers "authorization"]))
                                            {:topic :request :id "owned"}))
                                        :sse-options {:keepalive-ms 50}}))
           listener (get started host/listener-key)
           status (get started host/sse-status-key)
           client (HttpClient/newHttpClient)
           handler (:biff.ring/handler started)
           request (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/events")))
                       (.timeout (Duration/ofSeconds 5))
                       (.header "authorization" "allowed")
                       (.build))]
       (try
         (is (= 405 (:status (handler {:uri "/events" :request-method :post}))))
         (is (empty? @seen))
         (is (= 403 (.statusCode (.send client (network-request listener "/events?scope=owned")
                                         (HttpResponse$BodyHandlers/ofString)))))
         (is (= 200 (.statusCode (.send client (network-request listener "/ready")
                                         (HttpResponse$BodyHandlers/ofString)))))
         (let [response (.send client request (HttpResponse$BodyHandlers/ofInputStream))]
           (with-open [input (.body response)]
             (is (= 200 (.statusCode response)))
             (is (= 1 ((:active-count status))))
             (is (= 1 ((:occupied-count status))))
             (is (identical? (:xtdb/connectable started) (:xtdb/connectable (peek @seen))))
             (is (identical? (:gesso.live/system started) (:gesso.live/system (peek @seen))))
             (is (not= -1 (.read input)))
             (biff/stop started)
             (is ((:drained? status)))
             (is ((:drained? listener)))
             (is (live/closed? (:gesso.live/system started)))
             (is (= 503 (:status (handler {:uri "/events" :request-method :get}))))
             (is (= 2 (count @seen)))))
         (finally (host/stop! started)))))))

(deftest canonical-host-sse-retirement-failure-retains-providers-for-retry
  (setup
   (fn [assembly render]
     (let [started (host/start! assembly render
                                (assoc (config) :authorized-sse
                                       {:path "/events"
                                        :authorize-subscription (constantly {:topic :request :id "owned"})}))
           status (get started host/sse-status-key)
           handler (:biff.ring/handler started)
           node (:xtdb/connectable started)
           response (handler {:uri "/events" :request-method :get})
           failure (ex-info "injected SSE cancellation failure" {})]
       (try
         (is (= 200 (:status response)))
         (with-redefs [live/cancel-sse! (fn [_] (throw failure))]
           (is (identical? failure (try (biff/stop started) nil (catch Exception e e)))))
         (is ((:closed? status)))
         (is (not ((:drained? status))))
         (is (= 1 ((:occupied-count status))))
         (is (= 503 (:status (handler {:uri "/events" :request-method :get}))))
         (is (false? (live/closed? (:gesso.live/system started))))
         (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
         (finally (host/stop! started)))
       (is ((:drained? status)))
       (is (stream/closed? (:body response)))
       (is (live/closed? (:gesso.live/system started)))))))

(deftest canonical-host-invalid-sse-route-rolls-back-started-providers
  (setup
   (fn [assembly render]
     (let [system* (atom nil)
           real-start managed/start-biff-application!
           failure (with-redefs [managed/start-biff-application!
                                 (fn [& args]
                                   (let [system (apply real-start args)]
                                     (reset! system* system) system))]
                     (problem #(host/start! assembly render
                                             (assoc (config) :http-jdk {}
                                                    :authorized-sse {:path "/bad/../path"
                                                                     :authorize-subscription (constantly nil)}))))]
       (is (= :invalid-path (:reason failure)))
       (is (some? @system*))
       (is ((:drained? (get @system* host/listener-key))))
       (is (live/closed? (:gesso.live/system @system*)))
       (is (thrown? Exception (xt/q (:xtdb/connectable @system*) ["SELECT 1 AS x"])))))))

(deftest canonical-host-recovery-requires-owned-route-and-numeric-options
  (doseq [[change reason]
          [[{:sse-recovery {}} :recovery-requires-authorized-sse]
           [{:authorized-sse {} :sse-recovery nil} :invalid-sse-recovery-options]
           [{:authorized-sse {} :sse-recovery {:after-step! identity}}
            :invalid-sse-recovery-options]]]
    (is (= reason (:reason (problem #(host/start! nil identity
                                                  (merge (config) change))))))))

(deftest canonical-host-recovers-orphan-capacity-and-preserves-healthy-peer
  (setup
   (fn [assembly render]
     (let [opened (atom 0)
           allowed? (atom false)
           original-open flow/open-source-flow!
           failure (ex-info "source release unavailable" {})]
       (with-redefs-fn
         {#'flow/open-source-flow!
          (fn [src opts]
            (let [result (original-open src opts)
                  close! (:close! result)]
              (if (= 1 (swap! opened inc))
                (assoc result :close! #(if @allowed? (close!) (throw failure)))
                result)))
          #'gesso.live.transport.sse/write-events-task
          (fn [_ _ _] (fn [_ _] (fn [] nil)))}
         (fn []
           (let [started (host/start! assembly render
                                      (assoc (config)
                                             :authorized-sse
                                             {:path "/events" :max-connections 2
                                              :authorize-subscription (constantly {:topic :request :id "owned"})}
                                             :sse-recovery {:poll-ms 10 :min-interval-ms 40
                                                            :max-failure-interval-ms 80 :max-attempts 1}))
                 route (get started host/sse-status-key)
                 scheduler (get started host/recovery-status-key)
                 src (:source (:gesso.live/system started))
                 handler (:biff.ring/handler started)
                 request {:uri "/events" :request-method :get}]
             (try
               (is (= :running ((:status scheduler))))
               (is (= #{:status :stats :drained?} (set (keys scheduler))))
               (let [orphan (handler request) healthy (handler request)]
                 (is (= [200 200] [(:status orphan) (:status healthy)]))
                 (stream/close! (:body orphan))
                 (is (eventually #(pos? (:failure-count ((:stats scheduler))))))
                 (is (identical? failure (:last-error ((:stats scheduler)))))
                 (is (= 2 ((:occupied-count route))))
                 (is (= 2 (:subscription-count (source/stats src))))
                 (is (= 503 (:status (handler request))))
                 (reset! allowed? true)
                 (is (eventually #(and (zero? ((:disconnected-count route)))
                                       (= 1 (:subscription-count (source/stats src))))))
                 (is (= 1 ((:occupied-count route))))
                 (is (not (stream/closed? (:body healthy))))
                 (let [replacement (handler request)]
                   (is (= 200 (:status replacement)))
                   (stream/close! (:body replacement))))
               (finally
                 (reset! allowed? true)
                 (host/stop! started)))
             (is ((:drained? scheduler)))
             (is ((:drained? route)))
             (is (live/closed? (:gesso.live/system started))))))))))

(deftest canonical-host-recovery-drain-timeout-retains-actual-providers
  (setup
   (fn [assembly render]
     (let [first? (atom true)
           entered (promise)
           release (promise)
           original-open flow/open-source-flow!]
       (with-redefs-fn
         {#'flow/open-source-flow!
          (fn [src opts]
            (let [result (original-open src opts)
                  close! (:close! result)]
              (assoc result :close!
                     (fn []
                       (if (compare-and-set! first? true false)
                         (throw (ex-info "initial release failed" {}))
                         (do (deliver entered true)
                             (when-not (true? (deref release 10000 false))
                               (throw (ex-info "test release timed out" {})))
                             (close!)))))))
          #'gesso.live.transport.sse/write-events-task
          (fn [_ _ _] (fn [_ _] (fn [] nil)))}
         (fn []
           (let [started (host/start! assembly render
                                      (assoc (config) :stop-timeout-ms 50
                                             :http-jdk {}
                                             :authorized-sse
                                             {:path "/events"
                                              :authorize-subscription (constantly {:topic :request :id "owned"})}
                                             :sse-recovery {:poll-ms 10 :min-interval-ms 20}))
                 route (get started host/sse-status-key)
                 scheduler (get started host/recovery-status-key)
                 listener (get started host/listener-key)
                 node (:xtdb/connectable started)
                 handler (:biff.ring/handler started)
                 request {:uri "/events" :request-method :get}]
             (try
               (let [response (handler request)]
                 (is (= 200 (:status response)))
                 (stream/close! (:body response)))
               (is (= true (deref entered 5000 false)))
               (is (= :sse-recovery-not-drained (:reason (problem #(biff/stop started)))))
               (is (= :stopping ((:status scheduler))))
               (is (false? ((:closed? route))))
               (is (false? ((:drained? listener))))
               (is (false? (live/closed? (:gesso.live/system started))))
               (is (= [{:x 1}] (xt/q node ["SELECT 1 AS x"])))
               (is (= 503 (:status (handler request))))
               (finally
                 (deliver release true)
                 (is (eventually #((:drained? scheduler))))
                 (loop [attempt 0]
                   (when-let [failure (problem #(host/stop! started))]
                     (if (and (< attempt 100) (= :drain-timed-out (:reason failure)))
                       (do (Thread/sleep 20) (recur (inc attempt)))
                       (throw (ex-info "Teardown failed" failure)))))))
             (is ((:drained? route)))
             (is ((:drained? listener)))
             (is (live/closed? (:gesso.live/system started))))))))))

(deftest canonical-host-recovery-startup-failure-rolls-back-owned-resources
  (setup
   (fn [assembly render]
     (let [system* (atom nil)
           original-start managed/start-biff-application!
           failure (with-redefs [managed/start-biff-application!
                                 (fn [& args]
                                   (let [system (apply original-start args)]
                                     (reset! system* system) system))]
                     (problem #(host/start! assembly render
                                             (assoc (config) :http-jdk {}
                                                    :authorized-sse {:path "/events"
                                                                     :authorize-subscription (constantly nil)}
                                                    :sse-recovery {:poll-ms 0}))))]
       (is (= :invalid-poll-ms (:reason failure)))
       (is (some? @system*))
       (is ((:drained? (get @system* host/listener-key))))
       (is (live/closed? (:gesso.live/system @system*)))
       (is (thrown? Exception (xt/q (:xtdb/connectable @system*) ["SELECT 1 AS x"])))))))

(deftest canonical-sse-path-cannot-shadow-original-operation-or-fragment
  (let [ops (standard-operations)
        ctx (render-context ops)
        starts (atom 0)]
    (with-surfaced-application
      {:operations ops
       :route-capabilities
       (custom-route-capabilities
        {:request/claim {:operation :request/claim :path "/operations/request/:id/claim"}
         :request/cancel {:operation :request/cancel :path "/operations/request/cancel"}
         :request/reassign {:operation :request/reassign :path "/operations/request/reassign"}})
       :rendered-surfaces {:board [:main (rendered-operation-button
                                        ctx :request/claim "/operations/request/r1/claim")]}}
      (fn [{:keys [assembly]}]
        (is (some? assembly))
        (with-redefs [managed/start-biff-application!
                      (fn [& _] (swap! starts inc) (throw (ex-info "unexpected startup" {})))]
          (doseq [[path kind] [["/operations/request/r1/claim" :operation]
                               ["/operations/request/cancel" :operation]
                               ["/fragments/request-toolbar" :fragment]]]
            (let [failure (problem #(host/start! assembly identity
                                                (assoc (config) :authorized-sse
                                                       {:path path :authorize-subscription (constantly nil)})))]
              (is (= :sse-path-shadows-application-route (:reason failure)))
              (is (= path (:path failure)))
              (is (= [kind] (mapv :kind (:conflicts failure)))))))
        (is (zero? @starts))))))

(deftest canonical-owned-http-mutation-outbox-and-sse-convergence
  ;; A selected trusted Ring callback, not an independently certified executor.
  ;; The actual HTTP write, database commit, worker, SSE delivery and refetch
  ;; must all cross the reusable host-owned boundary, not external adapters.
  (setup
   (fn [assembly render]
     (let [id "v2361-owned-request"
           intent-id (random-uuid)
           operation-path "/operations/request/claim"
           fragment-path "/fragments/request-toolbar"
           stream-path "/streams/request-toolbar"
           scope {:topic :request :id id}
           read-state (fn [node]
                        (:status (first (xt/q node ["SELECT status FROM requests WHERE _id = ?" id]))))
           handler (fn [{:keys [uri request-method headers] :as request}]
                     (let [node (:xtdb/connectable request)]
                       (cond
                         (and (= operation-path uri) (= :post request-method))
                         (if-not (= "allowed" (get headers "authorization"))
                           {:status 403 :headers {} :body "Denied"}
                           (let [plan (outbox/prepare!
                                       {:intent-ids [intent-id]
                                        :source-id (get request managed/source-id-key)
                                        :operation :request/claim :purpose :gesso.live/refresh}
                                       [(assoc scope :change/kind :updated)])]
                             (try
                               (xt/execute-tx node
                                              (into (vec (:guard-ops plan))
                                                    [[:put-docs :requests {:xt/id id :status :claimed}]
                                                     (:tx-op plan)]))
                               {:status 200 :headers {} :body "Mutation"}
                               (catch Exception e
                                 (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                                   {:status 409 :headers {} :body "Rejected"}
                                   (throw e))))))
                         (= fragment-path uri)
                         (g/html-response [:div {:id "request-status"} (name (or (read-state node) :open))])
                         :else (render request))))
           started (host/start! assembly handler
                                (assoc (config) :http-jdk {}
                                       :authorized-sse {:path stream-path
                                                        :authorize-subscription
                                                        #(when (= "allowed" (get-in % [:headers "authorization"])) scope)
                                                        :sse-options {:keepalive-ms 50}}))
           listener (get started host/listener-key)
           node (:xtdb/connectable started)
           worker (get started managed/handle-key)
           client (HttpClient/newHttpClient)
           req (fn [path method allowed?]
                 (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))]
                   (.timeout builder (Duration/ofSeconds 5))
                   (when allowed? (.header builder "authorization" "allowed"))
                   (when (= method :post) (.POST builder (HttpRequest$BodyPublishers/noBody)))
                   (.build builder)))
           send! (fn [path method allowed?]
                   (.send client (req path method allowed?) (HttpResponse$BodyHandlers/ofString)))]
       (try
         (is (= 403 (.statusCode (send! stream-path :get false))))
         (is (= 403 (.statusCode (send! operation-path :post false))))
         (is (nil? (read-state node)))
         (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id])))
         (is (str/includes? (.body (send! fragment-path :get true)) ">open<"))
         (let [response (.send client (req stream-path :get true) (HttpResponse$BodyHandlers/ofInputStream))]
           (with-open [body (.body response)
                       reader (BufferedReader. (InputStreamReader. body StandardCharsets/UTF_8))]
             (is (= 200 (.statusCode response)))
             (is (= 200 (.statusCode (send! operation-path :post true))))
             (is (= :claimed (read-state node)))
             (is (eventually #(= 1 (:total-dispatch-accepted (lifecycle/status worker)))))
             (let [pending (future (loop []
                                     (when-let [line (.readLine reader)]
                                       (if (= "event: live-update" line)
                                         (.readLine reader)
                                         (recur)))))
                   line (deref pending 5000 nil)]
               (try
                 (is (and (string? line) (str/starts-with? line "data: ")))
                 (when (and line (str/starts-with? line "data: "))
                   (let [payload (edn/read-string (subs line 6))]
                     (is (= scope (select-keys (:invalidation payload) [:topic :id])))
                     (is (some? (progression/wire->requirement (:progression payload))))))
                 (finally (future-cancel pending))))
             (is (str/includes? (.body (send! fragment-path :get true)) ">claimed<"))
             (is (= 409 (.statusCode (send! operation-path :post true))))
             (is (= :claimed (read-state node)))
             (is (= 1 (:total-dispatch-accepted (lifecycle/status worker))))
             (host/stop! started)
             (is ((:drained? listener)))))
         (finally (host/stop! started) (.close client)))))))

(deftest host-policy-refuses-manifest-and-rolls-back-node-test
  (setup
   (fn [assembly handler]
     (let [opened (atom nil) decisions (atom []) original-start xtn/start-node
           data (with-redefs [xtn/start-node (fn [opts] (let [node (original-start opts)]
                                                         (reset! opened node) node))]
                  (problem #(host/start! assembly handler
                               (assoc (config) :live-options {}
                                      :assembled-publication {:subject-keys {:request :request/location-id
                                                                            :request-assignment :request/location-id
                                                                            :audit :request/location-id}}
                                      :xtdb-submission-policy!
                                      (fn [input] (swap! decisions conj input) false)))))]
       (is (= :gesso.live.outbox/checked-startup-failed (:error/type data)))
       (is (= :drained (:rollback/status data)))
       (is (= 1 (count @decisions)))
       (is (= :execute-tx (:api (first @decisions))))
       (is (some? @opened))
       (is (thrown? Exception (xt/q @opened ["SELECT 1 AS x"])))
       ;; The rollback releases the registration after closing the node.
       (let [release (live.xtdb/register-submission-policy! @opened (constantly false))]
         (is (fn? release))
         (release))))))
