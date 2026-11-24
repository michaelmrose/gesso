(ns gesso.assurance.example.physical-single-node-granted-live-test
  "v2337: One actual Biff-installed diagnostic handler combines source-derived
   principal operation grants, authoritative XTDB writes and advisory Live.
   The fixture also rechecks the original reader identity before each outgoing
   advisory SSE event and closes the stream if the recheck fails. It does not
   authenticate the callback, provide periodic idle revocation or an in-flight
   delivery fence, enforce CSRF, run Biff's Netty listener or issue certificates."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.live.core :as live]
            [gesso.model.tx :as tx]
            [manifold.stream :as stream])
  (:import (com.sun.net.httpserver HttpHandler HttpServer)
           (java.net HttpURLConnection InetSocketAddress URL)
           (java.nio.charset StandardCharsets)))

(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")
(def ^:private panel fixture/panel-uri)

(defn- policy [acquire!]
  {:catalog-id :slot/reservation-lifecycle
   :policy-id :proving/granted-live
   :grants {:actor/reserver #{:slot/reserve}
            :actor/releaser #{:slot/release}}
   :read-principals #{:actor/reserver :actor/releaser}
   :acquire-principal! acquire!})

(defn- authenticated [who]
  {:status :authenticated :principal-id who})

(defn- request [method uri]
  {:request-method method :uri uri})

(deftest original-biff-installed-live-handler-must-delegate-through-checked-grants
  (let [principal (atom :anonymous)
        acquisitions (atom 0)]
    (fixture/with-granted-live-proving-application!
     (policy (fn [_]
               (swap! acquisitions inc)
               (if (= :anonymous @principal)
                 {:status :anonymous}
                 (authenticated @principal))))
     (fn [{:keys [system handler live-handler principal-handler
                  delegate-handler read! live-report]}]
       (let [installed (:biff.ring/handler system)
             original-tx @#'tx/transact!
             writes (atom 0)
             emissions (atom 0)]
         (is (identical? installed handler))
         (is (identical? installed live-handler))
         (is (fn? principal-handler))
         (is (not (identical? installed principal-handler)))
         (is (not (identical? principal-handler delegate-handler)))
         (is (= :checked-before-delegated-xtdb-write
                (:selected-mutation-grants live-report)))
         (is (= :trusted-not-authenticated
                (:principal-acquisition live-report)))
         (is (= :proving/granted-live (:grant-policy-id live-report)))
         (is (= :slot/reservation-lifecycle (:grant-catalog-id live-report)))
         (is (= :request-time-check-of-selected-granted-principal
                (:live-subscription-authorization live-report)))
         (is (= :request-time-check-of-selected-granted-principal
                (:live-read-authorization live-report)))
         (is (= :independent-declared-read-principals
                (:live-read-policy live-report)))
         (is (= 2 (:live-declared-read-principal-count live-report)))
         (is (= :event-triggered-only-not-idle-or-in-flight
                (:ongoing-sse-authorization-and-revocation live-report)))
         (is (= :same-reader-check-before-each-encoded-event
                (:sse-advisory-event-revalidation live-report)))
         (is (= :none (:authority live-report)))
         (with-redefs [tx/transact! (fn [& args]
                                      (swap! writes inc)
                                      (apply original-tx args))
                       live/emit! (fn [_ message]
                                    (is (= :gesso-proving-reservation (:topic message)))
                                    (swap! emissions inc)
                                    {:status :emitted})]
           (is (= 401 (:status (installed (request :post reserve)))))
           (is (= 0 @writes))
           (is (= 0 @emissions))
           (reset! principal :actor/releaser)
           (is (= 403 (:status (installed (request :post reserve)))))
           (is (= 0 @writes))
           (is (= 0 @emissions))
           (reset! principal :actor/reserver)
           (let [result (installed (request :post reserve))]
             (is (= 200 (:status result)))
             (is (= "source-accepted"
                    (get-in result [:headers "x-gesso-proving-live-publication"]))))
           (is (= 1 @writes))
           (is (= 1 @emissions))
           (is (= 1 (:revision (read!))))
           (is (= 403 (:status (installed (request :post release)))))
           (is (= 1 @writes))
           (is (= 1 @emissions))
           (reset! principal :actor/releaser)
           (is (= 200 (:status (installed (request :post release)))))
           (is (= 2 @writes))
           (is (= 2 @emissions))
           (is (= 2 (:revision (read!))))
           (is (= 5 @acquisitions))
         (let [fresh (installed (request :get panel))]
           (is (= 200 (:status fresh)))
           (is (.contains ^String (:body fresh) "data-slot-status=\"available\""))
           (is (= 6 @acquisitions)
               "Selected read access is acquired and checked separately")
           (is (= 2 (:revision (read!)))))))))))

(deftest selected-read-and-sse-routes-reject-unauthorized-before-source-access
  ;; In the combined fixture the selected read policy is independently
  ;; declared; matching mutation grants here are only this test's choice. Denied SSE requests
  ;; must not allocate Live streams; denied reads must not reach XTDB.
  (let [who (atom :anonymous)
        acquisitions (atom 0)]
    (fixture/with-granted-live-proving-application!
     (policy (fn [_]
               (swap! acquisitions inc)
               (if (= @who :anonymous)
                 {:status :anonymous}
                 (authenticated @who))))
     (fn [{:keys [handler read! system live-report]}]
       (is (identical? handler (:biff.ring/handler system)))
       (let [orig-start @#'live/start-sse!
             stream-starts (atom 0)
             read-uris ["/__gesso_proving/slot" panel fixture/fragment-uri
                        fixture/events-uri]]
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (swap! stream-starts inc)
                         (apply orig-start args))]
           (doseq [[principal expected] [[:anonymous 401]
                                         [:actor/outsider 403]]]
             (reset! who principal)
             (doseq [uri read-uris]
               (is (= expected (:status (handler (request :get uri))))
                   (str "Denied original Biff read access: " principal " " uri)))
             (is (zero? @stream-starts)
                 "No Live subscription may start on denied initial GET"))
           (is (= 8 @acquisitions)
               "One trusted acquisition per selected original read route")
           (is (zero? (:revision (read!))))
           (reset! who :actor/reserver)
           (doseq [uri ["/__gesso_proving/slot" panel fixture/fragment-uri]]
             (is (= 200 (:status (handler (request :get uri))))))
           (is (= 11 @acquisitions))
           (let [stream-response (handler (request :get fixture/events-uri))]
             (is (= 200 (:status stream-response)))
             (is (= 1 @stream-starts)
                 "One real SSE stream starts after principal acceptance"))
           (is (= 12 @acquisitions))
           (is (= :event-triggered-only-not-idle-or-in-flight
                  (:ongoing-sse-authorization-and-revocation live-report)))))))))

(deftest independent-read-grants-admit-read-only-and-deny-writers-without-read-access
  ;; This is the desired operational distinction: a principal may read but
  ;; cannot mutate, and a principal may mutate but cannot read/subscribe.
  ;; Never equate the declarations simply because older tests used the same
  ;; two principal names for both permission classes.
  (let [who (atom :actor/reader)
        source (assoc (policy (fn [_] (authenticated @who)))
                      :read-principals #{:actor/reader :actor/releaser})]
    (fixture/with-granted-live-proving-application!
     source
     (fn [{:keys [handler system read! live-report]}]
       (let [installed (:biff.ring/handler system)
             emit-attempts (atom 0)
             orig-start @#'live/start-sse!
             stream-starts (atom 0)]
         (is (identical? handler installed))
         (is (= :independent-declared-read-principals
                (:live-read-policy live-report)))
         (is (= 2 (:live-declared-read-principal-count live-report)))
         (with-redefs [live/emit! (fn [& _]
                                    (swap! emit-attempts inc)
                                    {:status :emitted})
                       live/start-sse! (fn [& args]
                                         (swap! stream-starts inc)
                                         (apply orig-start args))]
           ;; Reader can read protected data and open Live; cannot write.
           (is (= 200 (:status (installed (request :get panel)))))
           (is (= 200 (:status (installed (request :get fixture/events-uri)))))
           (is (= 1 @stream-starts))
           (is (= 403 (:status (installed (request :post reserve)))))
           (is (= 0 @emit-attempts))
           (is (= 0 (:revision (read!))))
           ;; Writer has reserve grant but no read grant, so cannot read.
           (reset! who :actor/reserver)
           (is (= 403 (:status (installed (request :get panel)))))
           (is (= 403 (:status (installed (request :get fixture/events-uri)))))
           (is (= 1 @stream-starts) "Denied SSE may not allocate a stream")
           (let [result (installed (request :post reserve))]
             (is (= 200 (:status result)))
             (is (= "write-only"
                    (get-in result [:headers "x-gesso-proving-response-visibility"])))
             (is (= "no-store" (get-in result [:headers "cache-control"])))
             (is (nil? (get-in result [:headers "x-gesso-proving-readback"])))
             (is (nil? (get-in result [:headers "HX-Refresh"])))
             (is (not (re-find #"data-slot-status|data-slot-revision|reserved"
                               (:body result)))
                 "A write grant cannot disclose the restricted resource view"))
           (is (= 1 @emit-attempts))
           (is (= 1 (:revision (read!))))
           ;; Releaser has both permission types and can observe status.
           (reset! who :actor/releaser)
           (is (= 200 (:status (installed (request :get panel)))))
           (is (= 200 (:status (installed (request :post release)))))
           (is (= 2 @emit-attempts))
           (is (= 2 (:revision (read!))))))))))

(deftest invalid-read-policy-rejects-before-physical-application-start
  (let [source (assoc (policy (constantly {:status :anonymous}))
                      :read-principals #{:unqualified})
        starts (atom 0)
        original @#'physical/with-proving-application!]
    (with-redefs [physical/with-proving-application!
                  (fn [& args]
                    (swap! starts inc)
                    (apply original args))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (fixture/with-granted-live-proving-application!
                    source (fn [_] (is false "bad read source admitted")))))
      (is (zero? @starts)))))

(deftest malformed-read-acquisition-rejects-before-sse-allocation
  (fixture/with-granted-live-proving-application!
   (policy (constantly {:status :authenticated
                        :principal-id "forged"}))
   (fn [{:keys [handler read!]}]
     (let [calls (atom 0)
           orig-start @#'live/start-sse!]
       (with-redefs [live/start-sse! (fn [& args]
                                      (swap! calls inc)
                                      (apply orig-start args))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (handler (request :get fixture/events-uri))))
         (is (zero? @calls))
         (is (zero? (:revision (read!)))))))))

(deftest postcommit-live-failure-still-preserves-protected-write
  (fixture/with-granted-live-proving-application!
   (policy (constantly (authenticated :actor/reserver)))
   (fn [{:keys [handler read!]}]
     (let [calls (atom 0)
           result (with-redefs [live/emit! (fn [& _]
                                            (swap! calls inc)
                                            (throw (ex-info "notification failed" {})))]
                    (handler (request :post reserve)))]
       (is (= 200 (:status result)))
       (is (= "unknown" (get-in result [:headers "x-gesso-proving-live-publication"])))
       (is (= "true" (get-in result [:headers "HX-Refresh"])))
       (is (= 1 @calls))
       (is (= 1 (:revision (read!))))))))

(deftest invalid-authorization-source-fails-before-xtdb-start
  (let [starts (atom 0)
        original @#'physical/with-proving-application!
        source (assoc (policy (constantly (authenticated :actor/reserver)))
                      :grants {:actor/reserver #{:slot/reserve}})
        error (with-redefs [physical/with-proving-application!
                            (fn [& args]
                              (swap! starts inc)
                              (apply original args))]
                (try
                  (fixture/with-granted-live-proving-application!
                   source (fn [_] (is false "Must not enter callback")))
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e))))]
    (is (= :policy-not-bound-to-catalog-and-http-operations (:reason error)))
    (is (zero? @starts)))
  (is (thrown? clojure.lang.ExceptionInfo
               (fixture/with-granted-live-proving-application!
                nil (fn [_] nil)))
      "nil must not silently select the unguarded Live fixture"))

(defn- eventually
  "Poll a bounded physical stream predicate; never mistake timeout for pass."
  [pred]
  (loop [attempt 0]
    (cond
      (pred) true
      (>= attempt 150) false
      :else (do (Thread/sleep 20) (recur (inc attempt))))))

(deftest revoked-sse-reader-closes-original-biff-stream-before-an-event-frame
  ;; The actual connected Biff handler starts the real Live source-backed SSE
  ;; transport. A host callback now reporting anonymous must prevent a later
  ;; source invalidation from being encoded onto that existing stream.
  ;; This is event-triggered revalidation, NOT immediate idle revocation.
  (let [who (atom :actor/reader)
        calls (atom 0)
        source (assoc (policy (fn [_]
                                (swap! calls inc)
                                (if (= @who :anonymous)
                                  {:status :anonymous}
                                  (authenticated @who))))
                      :read-principals #{:actor/reader})]
    (fixture/with-granted-live-proving-application!
     source
     (fn [{:keys [system handler live-system slot-id live-report]}]
       (let [installed (:biff.ring/handler system)
             response (installed (request :get fixture/events-uri))
             body (:body response)]
         (is (identical? installed handler))
         (is (= 200 (:status response)))
         (is (some? body))
         (is (= :event-triggered-only-not-idle-or-in-flight
                (:ongoing-sse-authorization-and-revocation live-report)))
         (is (= 1 @calls) "Initial SSE GET checks the host callback once")
         ;; An unchanged identity may receive an advisory event. Use a real
         ;; source and Manifold response, not a fabricated encoder-only call.
         (is (contains? #{:emitted :queued}
                        (:status (live/emit! live-system
                                             {:topic :gesso-proving-reservation
                                              :id slot-id
                                              :change/kind :updated}))))
         (let [first-frame (deref (stream/take! body) 3000 ::timeout)]
           (is (string? first-frame))
           (is (re-find #"event: " (str first-frame))))
         (is (= 2 @calls)
             "The actual SSE formatter rechecks before encoding an event")
         (reset! who :anonymous)
         (is (contains? #{:emitted :queued}
                        (:status (live/emit! live-system
                                             {:topic :gesso-proving-reservation
                                              :id slot-id
                                              :change/kind :updated}))))
         (is (= ::closed (deref (stream/take! body ::closed) 3000 ::timeout))
             "Revoked reader must not receive a new SSE frame")
         (is (eventually #(stream/closed? body))
             "A failed event-time reader check must close the transport")
         (is (= 3 @calls))
         (is (= 401 (:status (installed (request :get fixture/events-uri))))
             "Reconnect must be denied by the original Biff-installed guard"))))))

(deftest changed-reader-identity-closes-instead-of-inheriting-open-stream
  ;; A new authorized principal on the same request does not inherit an
  ;; EventSource acquired by a different principal. This is a fixed original
  ;; connection subject, not simply a recheck that some reader is authorized.
  (let [who (atom :actor/reader)
        source (assoc (policy (fn [_] (authenticated @who)))
                      :read-principals #{:actor/reader :actor/releaser})]
    (fixture/with-granted-live-proving-application!
     source
     (fn [{:keys [handler live-system slot-id]}]
       (let [response (handler (request :get fixture/events-uri))
             body (:body response)]
         (is (= 200 (:status response)))
         (reset! who :actor/releaser)
         (is (contains? #{:emitted :queued}
                        (:status (live/emit! live-system
                                             {:topic :gesso-proving-reservation
                                              :id slot-id
                                              :change/kind :updated}))))
         (is (= ::closed (deref (stream/take! body ::closed) 3000 ::timeout))
             "An initially different reader must never receive the frame")
         (is (eventually #(stream/closed? body))))))))

(defn- with-socket! [handler f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (try
      (.createContext server "/"
                      (reify HttpHandler
                        (handle [_ exchange]
                          (try
                            (let [method (keyword (.toLowerCase
                                                   (.getRequestMethod exchange)))
                                  uri (.getPath (.getRequestURI exchange))
                                  response (handler (request method uri))
                                  bytes (.getBytes ^String (str (:body response))
                                                   StandardCharsets/UTF_8)]
                              ;; A transport test must preserve handler headers
                              ;; or concealment/recovery claims cannot actually
                              ;; be observed across the HTTP socket boundary.
                              (doseq [[header value] (:headers response)
                                      :when (some? value)]
                                (.add (.getResponseHeaders exchange)
                                      (str header) (str value)))
                              (.sendResponseHeaders exchange
                                                    (int (:status response))
                                                    (long (alength bytes)))
                              (with-open [out (.getResponseBody exchange)]
                                (.write out bytes)))
                            (finally (.close exchange))))))
      (.start server)
      (f (str "http://127.0.0.1:" (.getPort (.getAddress server))))
      (finally (.stop server 0)))))

(defn- socket-status [base method uri]
  (let [^HttpURLConnection connection
        (.openConnection (URL. (str base uri)))]
    (try
      (.setRequestMethod connection (.toUpperCase (name method)))
      (.setConnectTimeout connection 3000)
      (.setReadTimeout connection 3000)
      (.getResponseCode connection)
      (finally (.disconnect connection)))))

(defn- socket-response [base method uri]
  ;; Verify the actual HTTP serialization of the write-only response. The
  ;; selected handler is Biff-adopted; this transport is a JDK test bridge.
  (let [^HttpURLConnection connection
        (.openConnection (URL. (str base uri)))]
    (try
      (.setRequestMethod connection (.toUpperCase (name method)))
      (.setConnectTimeout connection 3000)
      (.setReadTimeout connection 3000)
      (let [status (.getResponseCode connection)
            stream (if (< status 400)
                     (.getInputStream connection)
                     (.getErrorStream connection))]
        {:status status
         :visibility (.getHeaderField connection
                                      "x-gesso-proving-response-visibility")
         :readback (.getHeaderField connection
                                    "x-gesso-proving-readback")
         :body (if stream (with-open [input stream] (slurp input)) "")})
      (finally (.disconnect connection)))))

(deftest real-socket-distinguishes-read-only-and-write-only-principals
  (let [who (atom :actor/reader)
        src (assoc (policy (fn [_] (authenticated @who)))
                   :read-principals #{:actor/reader :actor/releaser})]
    (fixture/with-granted-live-proving-application!
     src
     (fn [{:keys [system read!]}]
       (with-redefs [live/emit! (fn [_ _] {:status :emitted})]
         (with-socket! (:biff.ring/handler system)
           (fn [base]
             (is (= 200 (socket-status base :get panel)))
             (is (= 403 (socket-status base :post reserve)))
             (reset! who :actor/reserver)
             (is (= 403 (socket-status base :get panel)))
             (let [response (socket-response base :post reserve)]
               (is (= 200 (:status response)))
               (is (= "write-only" (:visibility response)))
               (is (nil? (:readback response)))
               (is (not (re-find #"data-slot-status|data-slot-revision|reserved"
                                 (:body response)))
                   "Physical HTTP must not disclose a write-only resource view"))
             (is (= 1 (:revision (read!))))
             (reset! who :actor/releaser)
             (is (= 200 (socket-status base :get panel)))
             (is (= 200 (socket-status base :post release)))
             (is (= 2 (:revision (read!)))))))))))

(deftest original-biff-lifecycle-serves-actual-jetty-http1-protected-operations
  ;; Unlike the JDK test adapter, this is the real Ring Jetty HTTP adapter
  ;; started by Biff AFTER it chooses the same protected Ring function. The
  ;; observed scope is short HTTP/1 operations and request-time denials only;
  ;; this does not certify Manifold SSE streaming or a deployed Aleph service.
  (let [who (atom :anonymous)
        source (assoc (policy (fn [_]
                                (if (= :anonymous @who)
                                  {:status :anonymous}
                                  (authenticated @who))))
                      :read-principals #{:actor/reader :actor/releaser})
        captured (atom nil)
        server* (atom nil)]
    (fixture/with-biff-jetty-granted-live-proving-application!
     source
     (fn [{:keys [handler system loopback-base-url loopback-listener-state
                  read! http-report live-report]}]
       (reset! captured loopback-listener-state)
       (reset! server* (:server @loopback-listener-state))
       (is (identical? handler (:biff.ring/handler system)))
       (is (identical? handler (:handler @loopback-listener-state)))
       (is (= :ring-jetty-http1-test-adapter
              (:biff-owned-loopback-listener http-report)))
       (is (= :not-established (:biff-network-consumption live-report))
           "The diagnostic Jetty listener isn't deployed production evidence")
       (is (instance? org.eclipse.jetty.server.Server @server*))
       (is (.isStarted ^org.eclipse.jetty.server.Server @server*))
       (is (re-matches #"http://127\.0\.0\.1:[1-9][0-9]*"
                       loopback-base-url))
       (is (= 401 (socket-status loopback-base-url :get panel)))
       (is (= 401 (socket-status loopback-base-url :post reserve)))
       (is (= 0 (:revision (read!))))
       (reset! who :actor/reader)
       (let [view (socket-response loopback-base-url :get panel)]
         (is (= 200 (:status view)))
         (is (re-find #"data-slot-status=\"available\"" (:body view))))
       (is (= 403 (socket-status loopback-base-url :post reserve)))
       (is (= 0 (:revision (read!))))
       (reset! who :actor/reserver)
       (is (= 403 (socket-status loopback-base-url :get panel)))
       (is (= 403 (socket-status loopback-base-url :get fixture/events-uri)))
       (with-redefs [live/emit! (fn [_ _] {:status :emitted})]
         (let [result (socket-response loopback-base-url :post reserve)]
           (is (= 200 (:status result)))
           (is (= "write-only" (:visibility result)))
           (is (nil? (:readback result)))
           (is (not (re-find #"data-slot-status|data-slot-revision|reserved"
                             (:body result)))))
         (is (= 1 (:revision (read!))))
         (reset! who :actor/releaser)
         (let [view (socket-response loopback-base-url :get panel)]
           (is (= 200 (:status view)))
           (is (re-find #"data-slot-status=\"reserved\"" (:body view))))
         (is (= 200 (socket-status loopback-base-url :post release)))
         (is (= 2 (:revision (read!)))))))
    (is (instance? clojure.lang.IAtom @captured))
    (is (nil? @@captured) "Biff must clear the Jetty listener state on stop")
    (is (.isStopped ^org.eclipse.jetty.server.Server @server*)
        "The actual Jetty server must be stopped by Biff")))

(deftest biff-owned-jetty-is-stopped-on-exceptional-callback-exit
  (let [state* (atom nil)
        server* (atom nil)
        expected (ex-info "Synthetic pilot callback failure" {:reason :test-end})
        observed (try
                   (fixture/with-biff-jetty-granted-live-proving-application!
                    (policy (constantly (authenticated :actor/releaser)))
                    (fn [{:keys [loopback-listener-state loopback-base-url]}]
                      (reset! state* loopback-listener-state)
                      (reset! server* (:server @loopback-listener-state))
                      (is (= 200 (socket-status loopback-base-url :get panel)))
                      (throw expected)))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (identical? expected observed))
    (is (instance? clojure.lang.IAtom @state*))
    (is (nil? @@state*))
    (is (.isStopped ^org.eclipse.jetty.server.Server @server*))))

(deftest disconnected-sse-sessions-retire-before-fixture-shutdown
  (let [count* (atom nil)
        final-session (atom nil)]
    (fixture/with-granted-live-proving-application!
     (policy (constantly (authenticated :actor/releaser)))
     (fn [{:keys [handler outstanding-sse-count]}]
       (reset! count* outstanding-sse-count)
       (is (zero? (outstanding-sse-count)))
       (let [real-start @#'live/start-sse!
             captured (atom nil)]
         (with-redefs [live/start-sse!
                       (fn [& args]
                         (let [session (apply real-start args)]
                           (reset! captured session)
                           session))]
           (dotimes [_ 5]
             (let [response (handler (request :get fixture/events-uri))
                   session @captured]
               (is (= 200 (:status response)))
               (is (= 1 (outstanding-sse-count)))
               (stream/close! (:body response))
               (is (eventually #(and (zero? (outstanding-sse-count))
                                     (true? ((:source-closed? session))))))))
           (handler (request :get fixture/events-uri))
           (reset! final-session @captured)
           (is (= 1 (outstanding-sse-count)))))))
    (is (stream/closed? (:stream @final-session)))
    (is (true? ((:source-closed? @final-session))))
    (is (zero? (@count*)))))

(deftest sse-closed-before-registration-is-not-retained
  (fixture/with-granted-live-proving-application!
   (policy (constantly (authenticated :actor/releaser)))
   (fn [{:keys [handler outstanding-sse-count]}]
     (let [real-start @#'live/start-sse!]
       (with-redefs [live/start-sse!
                     (fn [& args]
                       (let [session (apply real-start args)]
                         (live/cancel-sse! session)
                         session))]
         (let [response (handler (request :get fixture/events-uri))]
           (is (stream/closed? (:body response)))
           (is (eventually #(zero? (outstanding-sse-count))))))))))

(deftest short-http-listener-releases-an-admitted-but-unsupported-sse-response
  (fixture/with-biff-loopback-granted-live-proving-application!
   (policy (constantly (authenticated :actor/releaser)))
   (fn [{:keys [loopback-base-url outstanding-sse-count]}]
     (let [real-start @#'live/start-sse!
           captured (atom nil)]
       (with-redefs [live/start-sse!
                     (fn [& args]
                       (let [session (apply real-start args)]
                         (reset! captured session)
                         session))]
         (is (= 501 (socket-status loopback-base-url :get fixture/events-uri)))
         (is (some? @captured))
         (is (eventually #(and (stream/closed? (:stream @captured))
                               (true? ((:source-closed? @captured)))
                               (zero? (outstanding-sse-count))))))))))

(deftest original-biff-lifecycle-starts-and-stops-a-real-protected-loopback-listener
  ;; Unlike the separately managed JDK bridge used by earlier tests, this
  ;; listener is launched from :biff.core/start AFTER original Biff handler
  ;; adoption and stopped from :biff.core/stop. It serves only short replies,
  ;; not SSE, and is emphatically NOT the deployed Biff/Netty HTTP server.
  (let [who (atom :anonymous)
        principal-checks (atom 0)
        source (assoc (policy (fn [_]
                                (swap! principal-checks inc)
                                (if (= @who :anonymous)
                                  {:status :anonymous}
                                  (authenticated @who))))
                      :read-principals #{:actor/releaser})
        state* (atom ::never-entered)]
    (fixture/with-biff-loopback-granted-live-proving-application!
     source
     (fn [{:keys [handler system loopback-base-url loopback-listener-state
                  read! http-report live-report]}]
       (reset! state* loopback-listener-state)
       (is (identical? handler (:biff.ring/handler system)))
       (is (identical? handler (:handler @loopback-listener-state)))
       (is (= :jdk-short-http-only (:biff-owned-loopback-listener http-report)))
       (is (= :not-established (:biff-network-consumption live-report))
           "The JDK test listener must not be labeled a production server")
       (is (re-matches #"http://127\.0\.0\.1:[1-9][0-9]*"
                       loopback-base-url))
       (is (= 401 (socket-status loopback-base-url :get panel)))
       (is (= 401 (socket-status loopback-base-url :post reserve)))
       (is (= 0 (:revision (read!))))
       (reset! who :actor/reserver)
       (is (= 403 (socket-status loopback-base-url :get panel)))
       (is (= 403 (socket-status loopback-base-url :get fixture/events-uri))
           "Denied SSE requests must never allocate a stream")
       (let [result (socket-response loopback-base-url :post reserve)]
         (is (= 200 (:status result)))
         (is (= "write-only" (:visibility result)))
         (is (nil? (:readback result)))
         (is (not (re-find #"data-slot-revision|data-slot-status|reserved"
                           (:body result)))))
       (is (= 1 (:revision (read!))))
       (reset! who :actor/releaser)
       (is (= 200 (socket-status loopback-base-url :get panel)))
       (is (= 200 (socket-status loopback-base-url :post release)))
       (is (= 2 (:revision (read!))))
       (is (= 7 @principal-checks)
           "One selected principal check per protected HTTP operation")))
    (is (instance? clojure.lang.IAtom @state*))
    (is (nil? @@state*)
        "The Biff lifecycle must close its owned listener before callback exit")))

(deftest biff-owned-listener-is-stopped-even-on-callback-exception
  ;; Resource release is a necessary real host guarantee, not just a passing
  ;; short HTTP sequence. Failure in a user callback must not leak a server.
  (let [state* (atom nil)
        expected (ex-info "Intentional proving callback error" {:reason :test-end})
        caught (try
                 (fixture/with-biff-loopback-granted-live-proving-application!
                  (policy (constantly (authenticated :actor/releaser)))
                  (fn [{:keys [loopback-listener-state]}]
                    (reset! state* loopback-listener-state)
                    (throw expected)))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (identical? expected caught))
    (is (instance? clojure.lang.IAtom @state*))
    (is (nil? @@state*))))

(deftest real-socket-denies-selected-read-routes-before-sse-allocation
  ;; Exercise the actual Biff-selected handler over a real loopback socket.
  ;; The socket is an explicit test bridge, not Biff's production listener.
  (let [who (atom :anonymous)
        acquired (atom 0)
        starts (atom 0)]
    (fixture/with-granted-live-proving-application!
     (policy (fn [_]
               (swap! acquired inc)
               (if (= @who :anonymous)
                 {:status :anonymous}
                 (authenticated @who))))
     (fn [{:keys [system read!]}]
       (let [original-start @#'live/start-sse!]
         (with-redefs [live/start-sse! (fn [& args]
                                        (swap! starts inc)
                                        (apply original-start args))]
           (with-socket! (:biff.ring/handler system)
             (fn [base]
               (doseq [uri [panel fixture/fragment-uri fixture/events-uri
                            "/__gesso_proving/slot"]]
                 (is (= 401 (socket-status base :get uri))))
               (is (zero? @starts))
               (is (zero? (:revision (read!))))
               (reset! who :actor/outsider)
               (is (= 403 (socket-status base :get fixture/events-uri)))
               (is (zero? @starts))
               (reset! who :actor/reserver)
               (is (= 200 (socket-status base :get panel)))
               (is (zero? @starts))
               (is (= 6 @acquired))))))))))

(deftest real-socket-biff-selected-handler-enforces-grants-and-publishes
  (let [principal (atom :actor/releaser)
        acquired (atom 0)]
    (fixture/with-granted-live-proving-application!
     (policy (fn [_]
               (swap! acquired inc)
               (authenticated @principal)))
     (fn [{:keys [system read! live-handler]}]
       (let [installed (:biff.ring/handler system)
             emissions (atom 0)]
         (is (identical? installed live-handler))
         (with-redefs [live/emit! (fn [_ _]
                                    (swap! emissions inc)
                                    {:status :emitted})]
           (with-socket! installed
             (fn [base]
               (is (= 200 (socket-status base :get panel)))
               (is (= 403 (socket-status base :post reserve)))
               (is (zero? (:revision (read!))))
               (is (zero? @emissions))
               (reset! principal :actor/reserver)
               (is (= 200 (socket-status base :post reserve)))
               (is (= 1 (:revision (read!))))
               (is (= 1 @emissions))
               (is (= 403 (socket-status base :post release)))
               (is (= 1 @emissions))
               (reset! principal :actor/releaser)
               (is (= 200 (socket-status base :post release)))
               (is (= 2 (:revision (read!))))
               (is (= 2 @emissions)))))
         (is (= 5 @acquired)))))))
