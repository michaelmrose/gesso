(ns gesso.assurance.example.physical-single-node-session-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.live.core :as live]
            [manifold.stream :as stream])
  (:import (java.net HttpURLConnection URL)))

(def ^:private policy
  {:catalog-id :slot/reservation-lifecycle
   :policy-id :proving/session
   :grants {:actor/reserver #{:slot/reserve}
            :actor/releaser #{:slot/release}}
   :read-principals #{:actor/reader :actor/releaser}})
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")

(defn- request [method uri headers]
  {:request-method method :uri uri :headers headers})

(defn- credentials [issued]
  {"cookie" (:cookie issued)
   "x-gesso-proving-csrf" (:csrf-token issued)})

(defn- socket! [base method uri headers]
  (let [^HttpURLConnection c (.openConnection (URL. (str base uri)))]
    (try
      (.setRequestMethod c method)
      (.setConnectTimeout c 3000)
      (.setReadTimeout c 3000)
      (doseq [[k v] headers] (.setRequestProperty c k v))
      (let [status (.getResponseCode c)
            body (if (< status 400) (.getInputStream c) (.getErrorStream c))]
        {:status status :body (if body (with-open [in body] (slurp in)) "")
         :visibility (.getHeaderField c "x-gesso-proving-response-visibility")})
      (finally (.disconnect c)))))

(deftest actual-jetty-requests-require-server-issued-session-and-csrf
  (session/with-session-policy!
   policy
   (fn [{:keys [source issue! revoke!]}]
     (let [reader (issue! :actor/reader)
           writer (issue! :actor/reserver)
           both (issue! :actor/releaser)]
       (is (not= (:session-id reader) (:session-id writer)))
       (is (not= (:csrf-token reader) (:csrf-token writer)))
       (is (thrown? clojure.lang.ExceptionInfo (issue! :actor/outsider)))
       (fixture/with-biff-jetty-granted-live-proving-application!
        source
        (fn [{:keys [loopback-base-url read! system handler]}]
          (is (identical? handler (:biff.ring/handler system)))
          (let [call (partial socket! loopback-base-url)
                writes (atom 0)]
            (with-redefs [live/emit! (fn [& _] (swap! writes inc) {:status :emitted})]
              (is (= 401 (:status (call "GET" fixture/panel-uri {"x-proving-principal" "reader"}))))
              (is (= 401 (:status (call "GET" fixture/panel-uri {"cookie" (str "gesso-proving-session=" (apply str (repeat 64 "0")))}))))
              (is (= 401 (:status (call "GET" fixture/panel-uri {"cookie" (str (:cookie reader) "; " (:cookie writer))}))))
              (is (= 200 (:status (call "GET" fixture/panel-uri {"cookie" (:cookie reader)}))))
              (is (= 403 (:status (call "POST" reserve (credentials reader)))))
              (is (= 403 (:status (call "GET" fixture/panel-uri (credentials writer)))))
              (is (= 401 (:status (call "POST" reserve {"cookie" (:cookie writer)}))))
              (is (= 401 (:status (call "POST" reserve (assoc (credentials writer) "x-gesso-proving-csrf" (:csrf-token reader))))))
              (is (zero? (:revision (read!))))
              (is (zero? @writes))
              (let [r (call "POST" reserve (credentials writer))]
                (is (= 200 (:status r)))
                (is (= "write-only" (:visibility r)))
                (is (not (re-find #"data-slot-status|data-slot-revision" (:body r)))))
              (is (= 1 (:revision (read!))))
              (is (= 1 @writes))
              (revoke! (:session-id writer))
              (is (= 401 (:status (call "POST" reserve (credentials writer)))))
              (is (= 200 (:status (call "POST" release (credentials both)))))
              (is (= 2 (:revision (read!))))
              (is (= 2 @writes))))))))))

(deftest session-revocation-is-observed-by-real-live-event-recheck
  (session/with-session-policy!
   policy
   (fn [{:keys [source issue! revoke!]}]
     (let [reader (issue! :actor/reader)]
       (fixture/with-granted-live-proving-application!
        source
        (fn [{:keys [handler live-system slot-id]}]
          (let [req (request :get fixture/events-uri (credentials reader))
                response (handler req)
                body (:body response)
                emit! #(live/emit! live-system {:topic :gesso-proving-reservation
                                                :id slot-id :change/kind :updated})]
            (is (= 200 (:status response)))
            (is (contains? #{:emitted :queued} (:status (emit!))))
            (is (string? (deref (stream/take! body) 3000 ::timeout)))
            (revoke! (:session-id reader))
            (is (contains? #{:emitted :queued} (:status (emit!))))
            (is (= ::closed (deref (stream/take! body ::closed) 3000 ::timeout)))
            (is (= 401 (:status (handler req)))))))))))

(deftest session-authority-scope-exit-invalidates-credentials
  (let [captured (atom nil)]
    (session/with-session-policy!
     policy
     (fn [{:keys [source issue!] :as authority}]
       (let [reader (issue! :actor/reader)
             req (request :get fixture/panel-uri (credentials reader))]
         (reset! captured [authority req])
         (is (= {:status :authenticated :principal-id :actor/reader}
                ((:acquire-principal! source) req))))))
    (let [[{:keys [source issue!]} req] @captured]
      (is (= {:status :anonymous} ((:acquire-principal! source) req)))
      (is (thrown? clojure.lang.ExceptionInfo (issue! :actor/reader))))))
