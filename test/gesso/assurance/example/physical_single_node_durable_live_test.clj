(ns gesso.assurance.example.physical-single-node-durable-live-test
  "v2347: Explicit atomic publication mode through the installed session handler."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.live.core :as live]
            [gesso.live.outbox.cycle :as cycle]
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
