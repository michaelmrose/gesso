(ns gesso.assurance.example.physical-single-node-aleph-integration-test
  "Real Aleph HTTP/1, original Biff lifecycle, XTDB and Live. Never substitute
   an in-process transport or interpret this diagnostic as release evidence."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.live.core :as live]
            [manifold.stream :as stream])
  (:import (java.net HttpURLConnection URL)
           (java.io BufferedReader InputStreamReader)
           (java.nio.charset StandardCharsets)))

(defn- connection [base method uri actor]
  (doto ^HttpURLConnection (.openConnection (URL. (str base uri)))
    (.setRequestMethod method)
    (.setConnectTimeout 3000)
    (.setReadTimeout 10000)
    (.setRequestProperty "x-proving-principal" actor)))

(defn- finite-response [base method uri actor]
  (let [^HttpURLConnection c (connection base method uri actor)]
    (try
      (let [status (.getResponseCode c)
            body (if (< status 400) (.getInputStream c) (.getErrorStream c))]
        {:status status
         :body (if body (with-open [in body] (slurp in)) "")})
      (finally (.disconnect c)))))

(defn- eventually [pred]
  (loop [n 0]
    (cond (pred) true
          (>= n 200) false
          :else (do (Thread/sleep 20) (recur (inc n))))))

(deftest original-biff-aleph-socket-delivers-actual-live-event-and-revalidates
  (let [revoked? (atom false)
        ;; Header identities are deliberately TEST-ONLY credentials. Each
        ;; request has an independent identity, not a shared current user.
        source {:catalog-id :slot/reservation-lifecycle
                :policy-id :proving/aleph-live
                :grants {:actor/reserver #{:slot/reserve}
                         :actor/releaser #{:slot/release}}
                :read-principals #{:actor/reader :actor/releaser}
                :acquire-principal!
                (fn [request]
                  (let [who (case (get-in request [:headers "x-proving-principal"])
                              "reader" (when-not @revoked? :actor/reader)
                              "reserver" :actor/reserver
                              "releaser" :actor/releaser
                              nil)]
                    (if who {:status :authenticated :principal-id who}
                        {:status :anonymous})))}
        listener* (atom nil)]
    (fixture/with-biff-aleph-granted-live-proving-application!
     source
     (fn [{:keys [system handler loopback-base-url loopback-listener-state
                  read! http-report transport-write-stats]}]
       (reset! listener* loopback-listener-state)
       (is (identical? handler (:biff.ring/handler system)))
       (is (identical? handler (:handler @loopback-listener-state)))
       (is (= :aleph-http1-test-adapter (:biff-owned-loopback-listener http-report)))
       (is (= :final-frame-capture-not-listener-drain
              (:aleph-write-observation http-report)))
       (is (= 401 (:status (finite-response loopback-base-url "GET"
                                          fixture/events-uri "anonymous"))))
       (is (= 403 (:status (finite-response loopback-base-url "POST"
                                          "/__gesso_proving/slot/reserve" "reader"))))
       (is (= 0 (:revision (read!))))
       (let [started (promise)
             first-frame (promise)
             real-start @#'live/start-sse!
             ^HttpURLConnection c (connection loopback-base-url "GET"
                                              fixture/events-uri "reader")]
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (let [session (apply real-start args)]
                           (deliver started session)
                           session))]
           (let [client (future
                          (with-open [reader (BufferedReader.
                                              (InputStreamReader. (.getInputStream c)
                                                                  StandardCharsets/UTF_8))]
                            (deliver first-frame {:status (.getResponseCode c)
                                                  :type (.getHeaderField c "Content-Type")
                                                  :event (.readLine reader)
                                                  :data (.readLine reader)
                                                  :separator (.readLine reader)})
                            ;; EOF is required after a revoked reader's next
                            ;; event. A timeout or additional frame is a failure.
                            (.readLine reader)))]
             (try
               (let [session (deref started 5000 ::timeout)]
                 (when (= ::timeout session)
                   (throw (ex-info "Real Aleph SSE did not start" {})))
                 (is (= 200 (:status (finite-response loopback-base-url "POST"
                                                     "/__gesso_proving/slot/reserve" "reserver"))))
                 (is (= 1 (:revision (read!))))
                 (let [frame (deref first-frame 5000 ::timeout)]
                   (is (map? frame))
                   (is (= 200 (:status frame)))
                   (is (str/starts-with? (or (:type frame) "") "text/event-stream"))
                   (is (= "event: live-update" (:event frame)))
                   (is (str/starts-with? (or (:data frame) "") "data: "))
                   (is (= "" (:separator frame))))
                 (let [fresh (finite-response loopback-base-url "GET"
                                              fixture/fragment-uri "reader")]
                   (is (= 200 (:status fresh)))
                   (is (str/includes? (:body fresh) "data-slot-status=\"reserved\""))
                   (is (str/includes? (:body fresh) "data-slot-revision=\"1\"")))
                 (reset! revoked? true)
                 (is (= 200 (:status (finite-response loopback-base-url "POST"
                                                     "/__gesso_proving/slot/release" "releaser"))))
                 (is (= 2 (:revision (read!))))
                 (is (nil? (deref client 6000 ::timeout))
                     "Revocation must end the actual HTTP response before another event")
                 (is (eventually #(and (stream/closed? (:stream session))
                                       (true? ((:source-closed? session)))))
                     "Both the SSE stream and eager source interest must close")
                 (is (= 401 (:status (finite-response loopback-base-url "GET"
                                                     fixture/events-uri "reader"))))
                 ;; The real transport must associate every final write with
                 ;; the corresponding response. The SSE writer's disposition
                 ;; is deliberately not counted as producer retirement.
                 (is (eventually
                      #(let [s (transport-write-stats)]
                         (and (= 6 (:finite-terminal s))
                              (= 1 (+ (:stream-final-succeeded s)
                                      (:stream-final-failed s)))))))
                 (let [s (transport-write-stats)]
                   (is (= 6 (:finite-registered s) (:finite-succeeded s)))
                   (is (zero? (:finite-failed s)))
                   (is (= 1 (:stream-registered s)))
                   (is (zero? (:unobserved s)))
                   (is (= 7 (get-in s [:capture :registered])
                            (get-in s [:capture :matched])))
                   (is (every? zero? (map (:capture s) [:pending :unmatched :orphaned])))))
               (finally
                 (.disconnect c)
                 (future-cancel client))))))))
    (is (instance? clojure.lang.IAtom @listener*))
    (is (nil? @@listener*) "Biff must release the selected listener handle")))
