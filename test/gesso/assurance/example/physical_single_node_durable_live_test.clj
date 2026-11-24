(ns gesso.assurance.example.physical-single-node-durable-live-test
  "v2344: Explicit atomic publication mode through the installed session handler."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.physical-single-node-session :as session]
            [manifold.stream :as stream]
            [xtdb.api :as xt])
  (:import (java.net HttpURLConnection URL)))

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
