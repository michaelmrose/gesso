(ns gesso.assurance.example.physical-single-node-durable-live-test
  "v2355: Explicit atomic publication mode through the installed session handler."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.live.core :as live]
            [com.biffweb.core :as biff]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.cycle :as cycle]
            [gesso.model.tx :as tx]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import (java.net HttpURLConnection URL)
           (java.nio.file Files)))

(def ^:private policy
  {:catalog-id :slot/reservation-lifecycle
   :policy-id :proving/durable-live
   :grants {:actor/writer #{:slot/reserve :slot/release}}
   :read-principals #{:actor/reader :actor/writer}})
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")

(defn- headers [issued]
  {"cookie" (:cookie issued) "x-gesso-proving-csrf" (:csrf-token issued)})

(defn- post! [base uri headers]
  (let [^HttpURLConnection c (.openConnection (URL. (str base uri)))]
    (try
      (.setRequestMethod c "POST")
      (.setConnectTimeout c 3000)
      (.setReadTimeout c 3000)
      (doseq [[k v] headers] (.setRequestProperty c k v))
      (let [status (.getResponseCode c)]
        (when-let [body (if (< status 400) (.getInputStream c) (.getErrorStream c))]
          (with-open [in body] (slurp in)))
        {:status status
         :publication (.getHeaderField c "x-gesso-proving-live-publication")})
      (finally (.disconnect c)))))

(defn- rows [node]
  (xt/q node ["SELECT * FROM gesso_live_outbox"]))

(deftest protected-http-mutation-records-intent-before-any-source-dispatch
  (session/with-session-policy!
   policy
   (fn [{:keys [source issue!]}]
     (let [reader (issue! :actor/reader)
           writer (issue! :actor/writer)]
       (fixture/with-durable-granted-live-proving-application!
        "durable-reservation" source :biff-jetty-http1
        (fn [{:keys [handler system node read! loopback-base-url process-outbox! live-report]}]
          (is (identical? handler (:biff.ring/handler system)))
          (is (= :atomic-intent-then-explicit-outbox-cycle (:notification-timing live-report)))
          (is (empty? (rows node)))
          (let [response (handler {:request-method :get :uri fixture/events-uri
                                   :headers (headers reader)})
                body (:body response)
                opts {:worker-id "reservation-test-worker" :max-items 8}]
            (try
              (is (= 200 (:status response)))
              (is (= 403 (:status (post! loopback-base-url reserve (headers reader)))))
              (is (= 401 (:status (post! loopback-base-url reserve {"cookie" (:cookie writer)}))))
              (is (zero? (:revision (read!))))
              (is (empty? (rows node)))
              (is (= {:status 200 :publication "intent-recorded"}
                     (post! loopback-base-url reserve (headers writer))))
              (is (= 1 (:revision (read!))))
              (let [[intent :as pending] (rows node)]
                (is (= 1 (count pending)))
                (is (= :pending (:gesso.live.outbox/status intent)))
                (is (= physical/outbox-source-id (:gesso.live.outbox/source-id intent)))
                (is (= :slot/reserve (:gesso.live.outbox/operation intent)))
                (is (= "durable-reservation" (:gesso.live.outbox/subject-id intent)))
                (is (= ::timeout @(stream/try-take! body ::closed 100 ::timeout))
                    "No direct publication before the durable worker runs")
                (let [cycle (process-outbox! opts)]
                  (is (= 1 (:dispatch-accepted cycle)))
                  (is (= (:xt/id intent) (get-in cycle [:results 0 :intent-id])))
                  (is (= :dispatch-accepted (:gesso.live.outbox/status (first (rows node)))))
                  (is (string? @(stream/try-take! body ::closed 3000 ::timeout)))
                  (is (zero? (:processed (process-outbox! opts))))
                  (is (= ::timeout @(stream/try-take! body ::closed 100 ::timeout)))))
              (is (= 200 (:status (post! loopback-base-url release (headers writer)))))
              (is (= 2 (:revision (read!))))
              (is (= #{:pending :dispatch-accepted} (set (map :gesso.live.outbox/status (rows node)))))
              (is (= 1 (:dispatch-accepted (process-outbox! opts))))
              (is (string? @(stream/try-take! body ::closed 3000 ::timeout)))
              (is (= 2 (count (rows node))))
              (is (every? #(= :dispatch-accepted (:gesso.live.outbox/status %)) (rows node)))
              (finally (stream/close! body))))))))))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 250) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(deftest managed-worker-delivers-protected-http-writes-without-manual-processing
  (let [status* (atom nil)]
    (session/with-session-policy!
     policy
     (fn [{:keys [source issue!]}]
       (let [reader (issue! :actor/reader)
             writer (issue! :actor/writer)]
         (fixture/with-managed-durable-granted-live-proving-application!
          "managed-reservation" source :biff-jetty-http1
          {:worker-id "managed-reservation-test" :poll-ms 20 :max-items 8}
          (fn [{:keys [handler system node read! loopback-base-url
                       outbox-worker-status live-report] :as app}]
            (reset! status* outbox-worker-status)
            (is (identical? handler (:biff.ring/handler system)))
            (is (not (contains? app :process-outbox!)))
            (is (= :started-last (get-in app [:http-report :native-worker-module])))
            (is (= :native-biff-start-stop (:worker-adoption live-report)))
            (is (= "managed-reservation-test" (:worker-id (lifecycle/status (get system managed/handle-key)))))
            (is (= :atomic-intent-then-scoped-managed-worker
                   (:notification-timing live-report)))
            (let [response (handler {:request-method :get :uri fixture/events-uri
                                     :headers (headers reader)})
                  body (:body response)]
              (try
                (is (= 200 (:status response)))
                (is (= 403 (:status (post! loopback-base-url reserve (headers reader)))))
                (is (empty? (rows node)))
                (doseq [[uri revision] [[reserve 1] [release 2]]]
                  (is (= {:status 200 :publication "intent-recorded"}
                         (post! loopback-base-url uri (headers writer))))
                  (is (= revision (:revision (read!))))
                  (is (string? @(stream/try-take! body ::closed 5000 ::timeout)))
                  (is (eventually #(and (= revision (:total-dispatch-accepted
                                                    (outbox-worker-status)))
                                        (= revision (count (rows node)))
                                        (every? (fn [row]
                                                  (= :dispatch-accepted
                                                     (:gesso.live.outbox/status row)))
                                                (rows node))))))
                (is (not= :failed (:status (outbox-worker-status))))
                (finally (stream/close! body)))))))))
    (is (= :stopped (:status (@status*))))
    (is (true? (:stop-requested? (@status*))))))

(deftest managed-worker-scope-retirement-precedes-provider-close-on-exception
  (let [entered (promise)
        release-cycle (promise)
        callback-exiting (promise)
        status* (atom nil)
        provider* (atom nil)
        real-cycle @#'cycle/process-cycle!
        expected (ex-info "intentional callback exit" {})]
    (with-redefs [cycle/process-cycle!
                  (fn [node live-system & args]
                    (deliver entered true)
                    @release-cycle
                    (apply real-cycle node live-system args))]
      (let [task (future
                   (try
                     (session/with-session-policy!
                      policy
                      (fn [{:keys [source]}]
                        (fixture/with-managed-durable-granted-live-proving-application!
                         "managed-drain" source :no-listener
                         {:worker-id "managed-drain-test" :poll-ms 20}
                         (fn [{:keys [node live-system outbox-worker-status]}]
                           (reset! provider* [node live-system])
                           (reset! status* outbox-worker-status)
                           (when-not (= true (deref entered 5000 ::timeout))
                             (throw (ex-info "worker did not enter" {})))
                           (deliver callback-exiting true)
                           (throw expected)))))
                     (catch Throwable e e)))]
        (try
          (is (= true (deref callback-exiting 10000 ::timeout)))
          (is (eventually #(true? (:stop-requested? (@status*)))))
          ;; Cross the first drain timeout. Neither provider may close while
          ;; the cycle remains blocked, even on exceptional callback exit.
          (is (= ::blocked (deref task 1200 ::blocked)))
          (let [[node live-system] @provider*]
            (is (not (live/closed? live-system)))
            (is (empty? (rows node))))
          (finally (deliver release-cycle true)))
        (is (identical? expected (deref task 10000 ::timeout)))
        (is (= :stopped (:status (@status*))))
        (is (live/closed? (second @provider*)))))))

(deftest closed-application-reopens-persisted-reservation-and-recovers-pending-intent
  ;; Three complete application scopes, with fresh session authorities, Biff,
  ;; Live and XTDB nodes. This is orderly close/reopen, NOT abrupt JVM death.
  (let [root (Files/createTempDirectory "gesso-reservation-restart-"
                                        (make-array java.nio.file.attribute.FileAttribute 0))
        slot-id "persistent-reservation"
        old-cookie (atom nil)
        original-id (atom nil)
        run! (fn [mode f]
               (session/with-session-policy!
                policy
                (fn [{:keys [source issue!]}]
                  (fixture/with-durable-granted-live-proving-application!
                   slot-id source :biff-jetty-http1 {:mode mode :root root}
                   #(f % (issue! :actor/writer))))))]
    (try
      (run! :create
            (fn [{:keys [node read! loopback-base-url storage-mode]} writer]
              (reset! old-cookie (headers writer))
              (is (= :create storage-mode))
              (is (= {:status 200 :publication "intent-recorded"}
                     (post! loopback-base-url reserve (headers writer))))
              (is (= 1 (:revision (read!))))
              (let [[intent :as pending] (rows node)]
                (is (= 1 (count pending)))
                (is (= :pending (:gesso.live.outbox/status intent)))
                (reset! original-id (:xt/id intent)))))
      (is (= :local-storage-already-exists
             (try (run! :create (fn [& _] (is false "must not reseed"))) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (run! :reopen
            (fn [{:keys [node handler read! loopback-base-url process-outbox! storage-mode]} writer]
              (is (= :reopen storage-mode))
              (is (= 1 (:revision (read!))))
              (is (= "reserved" (:status (read!))))
              (is (= 401 (:status (post! loopback-base-url release @old-cookie))))
              (is (= [@original-id] (mapv :xt/id (rows node))))
              (is (= :pending (:gesso.live.outbox/status (first (rows node)))))
              (let [response (handler {:request-method :get :uri fixture/events-uri
                                       :headers (headers writer)})
                    body (:body response)
                    opts {:worker-id "reopened-reservation" :max-items 8}]
                (try
                  (is (= 200 (:status response)))
                  (is (= 1 (:dispatch-accepted (process-outbox! opts))))
                  (is (string? @(stream/try-take! body ::closed 3000 ::timeout)))
                  (is (= :dispatch-accepted (:gesso.live.outbox/status (first (rows node)))))
                  (is (= {:status 200 :publication "intent-recorded"}
                         (post! loopback-base-url release (headers writer))))
                  (is (= 2 (:revision (read!))))
                  (is (= 1 (:dispatch-accepted (process-outbox! opts))))
                  (is (string? @(stream/try-take! body ::closed 3000 ::timeout)))
                  (finally (stream/close! body))))))
      (run! :reopen
            (fn [{:keys [node read! process-outbox!]} _]
              (is (= 2 (:revision (read!))))
              (is (= "available" (:status (read!))))
              (is (= 2 (count (rows node))))
              (is (every? #(= :dispatch-accepted (:gesso.live.outbox/status %)) (rows node)))
              (is (= 0 (:processed (process-outbox! {:worker-id "verify-reopened"}))))))
      (finally
        (doseq [file (reverse (file-seq (.toFile root)))]
          (io/delete-file file true))))))

(deftest managed-retirement-fences-new-ring-invocations-before-draining-admitted-command
  (let [entered (promise)
        release-command (promise)
        app* (promise)
        admitted* (promise)
        real-transact @#'tx/transact-with-outbox!]
    (with-redefs [tx/transact-with-outbox!
                  (fn [& args]
                    (deliver entered true)
                    @release-command
                    (apply real-transact args))]
      (let [scope (future
                    (session/with-session-policy!
                     policy
                     (fn [{:keys [source issue!]}]
                       (let [writer (issue! :actor/writer)]
                         (fixture/with-managed-durable-granted-live-proving-application!
                          "managed-ring-drain" source :biff-jetty-http1
                          {:worker-id "managed-ring-drain-worker" :poll-ms 20}
                          (fn [{:keys [handler] :as app}]
                            (deliver app* [app writer])
                            ;; An already admitted synchronous Ring execution
                            ;; may still own the domain transaction. Its return
                            ;; is not an assertion about a socket writer.
                            (deliver admitted*
                                     (future (handler {:request-method :post
                                                       :uri reserve
                                                       :headers (headers writer)})))
                            (when-not (= true (deref entered 5000 ::timeout))
                              (throw (ex-info "command did not reach transaction" {})))
                            :callback-finished))))))]
        (try
          (is (= true (deref entered 10000 ::timeout)))
          (let [[{:keys [node read! live-system loopback-base-url handler
                         ring-admission-status outbox-worker-status]} writer]
                (deref app* 5000 nil)]
            (is (eventually #(= {:accepting? false :active 1}
                               (ring-admission-status))))
            (is (false? (:stop-requested? (outbox-worker-status))))
            (is (= 503 (:status (post! loopback-base-url release (headers writer)))))
            (is (= 503 (:status (handler {:request-method :get :uri fixture/events-uri
                                         :headers (headers writer)}))))
            (is (zero? (:revision (read!))))
            (is (empty? (rows node)))
            (is (not (live/closed? live-system)))
            (is (= ::blocked (deref scope 100 ::blocked))))
          (finally (deliver release-command true)))
        (is (= 200 (:status (deref (deref admitted* 5000 nil) 5000 nil))))
        (is (= :callback-finished (deref scope 10000 ::timeout)))
        (let [[{:keys [ring-admission-status outbox-worker-status live-system]} _] @app*]
          (is (= {:accepting? false :active 0} (ring-admission-status)))
          (is (= :stopped (:status (outbox-worker-status))))
          (is (live/closed? live-system)))))))

(deftest managed-sse-retirement-retries-after-close-hooks-before-provider-shutdown
  (let [app* (promise)
        session* (promise)
        retry-entered (promise)
        release-retry (promise)
        attempts (atom 0)
        real-cancel @#'live/cancel-sse!
        real-start @#'live/start-sse!]
    (with-redefs [live/start-sse! (fn [& args]
                                  (let [started (apply real-start args)]
                                    (deliver session* started)
                                    started))
                  live/cancel-sse! (fn [started]
                                     (case (swap! attempts inc)
                                       1 (do (real-cancel started)
                                             (throw (ex-info "post-close cleanup failure" {})))
                                       2 (do (deliver retry-entered true)
                                             @release-retry
                                             (real-cancel started))
                                       (real-cancel started)))]
      (let [scope (future
                    (session/with-session-policy!
                     policy
                     (fn [{:keys [source issue!]}]
                       (let [writer (issue! :actor/writer)]
                         (fixture/with-managed-durable-granted-live-proving-application!
                          "managed-sse-retirement" source :biff-jetty-http1
                          {:worker-id "managed-sse-retirement-worker" :poll-ms 20}
                          (fn [{:keys [handler] :as app}]
                            (let [response (handler {:request-method :get
                                                     :uri fixture/events-uri
                                                     :headers (headers writer)})]
                              (deliver app* [app writer (:status response)]))
                            :callback-finished))))))]
        (try
          (is (= true (deref retry-entered 10000 ::timeout)))
          (let [[{:keys [node live-system loopback-base-url outstanding-sse-count
                         sse-retirement-status outbox-worker-status]} writer status]
                (deref app* 5000 nil)
                started (deref session* 5000 nil)]
            (is (= 200 status))
            (is (stream/closed? (:stream started)))
            (is (true? ((:source-closed? started))))
            ;; The close hooks have retired the open-session inventory, but
            ;; the failed cancellation still owns its shutdown obligation.
            (is (zero? (outstanding-sse-count)))
            (is (= {:pending 1 :failed-attempts 1 :completed? false}
                   (sse-retirement-status)))
            (is (= :stopped (:status (outbox-worker-status))))
            (is (empty? (rows node)))
            (is (not (live/closed? live-system)))
            (is (= 503 (:status (post! loopback-base-url release (headers writer)))))
            (is (= ::blocked (deref scope 100 ::blocked))))
          (finally (deliver release-retry true)))
        (is (= :callback-finished (deref scope 10000 ::timeout)))
        (let [[{:keys [live-system sse-retirement-status]} _ _] @app*]
          (is (= 2 @attempts))
          (is (= {:pending 0 :failed-attempts 1 :completed? true}
                 (sse-retirement-status)))
          (is (live/closed? live-system)))))))

(deftest native-worker-startup-failure-retires-returned-worker-before-providers-close
  (let [started* (atom nil)
        real-start @#'biff/start
        failure (ex-info "post-start validation failure" {})]
    (with-redefs [biff/start (fn [initial modules order]
                              (let [started (real-start initial modules order)]
                                (reset! started* started)
                                (is (= 503 (:status ((:biff.ring/handler started)
                                                     {:request-method :get :uri fixture/panel-uri}))))
                                (throw failure)))]
      (is (identical? failure
                      (try
                        (session/with-session-policy!
                         policy
                         (fn [{:keys [source]}]
                           (fixture/with-managed-durable-granted-live-proving-application!
                            "native-startup-failure" source :biff-jetty-http1
                            {:worker-id "native-startup-failure" :poll-ms 20}
                            (fn [_] (throw (ex-info "callback must not run" {}))))))
                        (catch Exception e e)))))
    (let [system @started*
          handle (get system managed/handle-key)]
      (is (some? handle))
      (is (= :stopped (:status (lifecycle/status handle))))
      (is (true? (:stop-requested? (lifecycle/status handle))))
      (is (live/closed? (:gesso.live/system system)))
      (is (= 503 (:status ((:biff.ring/handler system)
                           {:request-method :post :uri reserve})))))))

(deftest native-startup-fences-authenticated-socket-traffic-until-complete
  (let [entered (promise)
        release-start (promise)
        listener (promise)
        writer* (promise)
        jetty-var (requiring-resolve 'ring.adapter.jetty9/run-jetty)
        real-jetty @jetty-var
        real-start @#'managed/start-module!]
    (with-redefs-fn
      {jetty-var (fn [& args]
                   (let [server (apply real-jetty args)]
                     (deliver listener server)
                     server))
       #'managed/start-module! (fn [system]
                                 (deliver entered system)
                                 (when-not (= true (deref release-start 15000 false))
                                   (throw (ex-info "startup test barrier timed out" {})))
                                 (real-start system))}
      (fn []
        (let [scope (future
                      (session/with-session-policy!
                       policy
                       (fn [{:keys [source issue!]}]
                         (let [writer (issue! :actor/writer)]
                           (deliver writer* writer)
                           (fixture/with-managed-durable-granted-live-proving-application!
                            "native-startup-fence" source :biff-jetty-http1
                            {:worker-id "native-startup-fence-worker" :poll-ms 20}
                            (fn [{:keys [node read! loopback-base-url ring-admission-status
                                         outbox-worker-status live-report]}]
                              (is (= {:accepting? true :active 0} (ring-admission-status)))
                              (is (= :closed-until-biff-and-handler-checks
                                     (:ring-admission-startup live-report)))
                              (is (= 0 (:revision (read!))))
                              (is (= {:status 200 :publication "intent-recorded"}
                                     (post! loopback-base-url reserve (headers writer))))
                              (is (= 1 (:revision (read!))))
                              (is (eventually #(= 1 (:total-dispatch-accepted
                                                     (outbox-worker-status)))))
                              :ready))))))]
          (try
            (let [system (deref entered 10000 nil)
                  server (deref listener 1000 nil)
                  writer (deref writer* 1000 nil)]
              (is (some? system))
              (is (some? server))
              (is (some? writer))
              (when (and system server writer)
                (let [port (.getLocalPort (first (.getConnectors server)))
                      base (str "http://127.0.0.1:" port)
                      node (:biff.xtdb/node system)]
                  ;; Valid credentials already exist and the real listener
                  ;; accepts sockets, but the worker has NOT started yet.
                  (is (nil? (get system managed/handle-key)))
                  (is (= 503 (:status (post! base reserve (headers writer)))))
                  (is (= 503 (:status ((:biff.ring/handler system)
                                       {:request-method :get :uri fixture/events-uri
                                        :headers (headers writer)}))))
                  (is (empty? (rows node)))
                  (is (= ::blocked (deref scope 100 ::blocked))))))
            (finally (deliver release-start true)))
          (is (= :ready (deref scope 15000 ::timeout))))))))

(deftest unmanaged-live-modes-retire-sessions-through-native-biff-stop
  (doseq [[mode run!] [[:direct
                        (fn [source f]
                          (fixture/with-biff-jetty-granted-live-proving-application!
                           "native-direct-retirement" source f))]
                       [:manual
                        (fn [source f]
                          (fixture/with-durable-granted-live-proving-application!
                           "native-manual-retirement" source :biff-jetty-http1 f))]]]
    (let [app* (atom nil)
          writer* (atom nil)
          stop-entered? (atom false)
          cancellation* (atom nil)
          real-stop @#'biff/stop
          real-cancel @#'live/cancel-sse!
          failure (ex-info "callback failure" {:mode mode})]
      (with-redefs [biff/stop (fn [system]
                                (when-let [app @app*]
                                  (is (= 1 ((:outstanding-sse-count app))))
                                  (is (= {:accepting? false :active 0}
                                         ((:ring-admission-status app)))))
                                (reset! stop-entered? true)
                                (real-stop system))
                    live/cancel-sse! (fn [started]
                                       (let [{:keys [node loopback-base-url]} @app*]
                                         (reset! cancellation*
                                                 {:native-stop? @stop-entered?
                                                  :outbox-empty? (empty? (rows node))
                                                  :socket-status (:status (post! loopback-base-url reserve
                                                                                 (headers @writer*)))}))
                                       (real-cancel started))]
        (is (identical?
             failure
             (try
               (session/with-session-policy!
                policy
                (fn [{:keys [source issue!]}]
                  (let [writer (issue! :actor/writer)]
                    (reset! writer* writer)
                    (run! source
                          (fn [{:keys [handler http-report] :as app}]
                            (reset! app* app)
                            (is (= :before-listener-stop (:native-live-retirement http-report)))
                            (is (= :not-selected (:native-worker-module http-report)))
                            (is (= 200 (:status (handler {:request-method :get
                                                         :uri fixture/events-uri
                                                         :headers (headers writer)}))))
                            (throw failure))))))
               (catch Exception e e)))))
      (is (= {:native-stop? true :outbox-empty? true :socket-status 503}
             @cancellation*))
      (let [{:keys [live-system outstanding-sse-count sse-retirement-status
                    loopback-listener-state]} @app*]
        (is (zero? (outstanding-sse-count)))
        (is (= {:pending 0 :failed-attempts 0 :completed? true}
               (sse-retirement-status)))
        (is (nil? @loopback-listener-state))
        (is (live/closed? live-system))))))

(deftest unmanaged-live-startup-failure-rolls-back-actual-listener
  (let [server* (atom nil)
        handler* (atom nil)
        writer* (atom nil)
        jetty-var (requiring-resolve 'ring.adapter.jetty9/run-jetty)
        real-jetty @jetty-var
        real-start @#'biff/start
        failure (ex-info "post-start failure without worker" {})]
    (with-redefs-fn
      {jetty-var (fn [& args]
                   (let [server (apply real-jetty args)]
                     (reset! server* server)
                     server))
       #'biff/start (fn [initial modules order]
                      (let [system (real-start initial modules order)
                            handler (:biff.ring/handler system)
                            port (.getLocalPort (first (.getConnectors @server*)))
                            base (str "http://127.0.0.1:" port)]
                        (reset! handler* handler)
                        (is (= 503 (:status (post! base reserve (headers @writer*)))))
                        (is (empty? (rows (:biff.xtdb/node system))))
                        (throw failure)))}
      (fn []
        (is (identical?
             failure
             (try
               (session/with-session-policy!
                policy
                (fn [{:keys [source issue!]}]
                  (reset! writer* (issue! :actor/writer))
                  (fixture/with-biff-jetty-granted-live-proving-application!
                   "native-direct-startup-failure" source
                   (fn [_] (throw (ex-info "callback must not run" {}))))))
               (catch Exception e e))))))
    (is (.isStopped ^org.eclipse.jetty.server.Server @server*))
    (is (= 503 (:status (@handler* {:request-method :post :uri reserve
                                    :headers (headers @writer*)}))))))
