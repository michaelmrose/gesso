(ns gesso.live.outbox.http-aleph-integration-test
  "Explicit, strict real-Aleph qualification. Exists on the :aleph-integration
   extra path only: this MUST fail at namespace load if Aleph is absent.
   The checks here qualify HTTP/1. HTTP/2, cancel/disconnect and SSE producer
   completion still require separate evidence before claiming a production
   Aleph adapter."
  (:require [aleph.http :as aleph]
            [clojure.string :as str]
            [aleph.netty :as aleph-netty]
            [clojure.test :refer [deftest is]]
            [gesso.live.outbox.http :as http]
            [gesso.live.outbox.http-aleph-pipeline :as capture]
            [gesso.live.outbox.http-aleph-live :as aleph-live]
            [gesso.live.flow :as flow]
            [gesso.live.outbox.http-recovery :as recovery]
            [gesso.live.source :as source]
            [gesso.live.transport.sse :as sse]
            [gesso.live.outbox.http-listener :as listener]
            [gesso.live.outbox.http-netty-terminal :as terminal]
            [manifold.stream :as stream])
  (:import [java.io Closeable]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]))

(defn- eventually [pred]
  (loop [n 0]
    (cond
      (pred) true
      (>= n 200) false
      :else (do (Thread/sleep 20) (recur (inc n))))))

(defn- real-http1-server!
  "This is a *qualification harness*, not a production adapter. Only finite
   responses are allowed to certify producer quiescence on Ring return.
   For streams, the test independently controls :producer-ceased! once its
   application work has actually ended. No other application may reuse this
   rule without proving its own producer and continuation lifecycle."
  [ring-handler]
  (let [capture (capture/prepare!)
        responses (atom [])
        aleph-server (atom nil)
        guard (listener/prepare!
               {:dispatch-async!
                (fn [request terminal!]
                  (let [response (ring-handler request)
                        body (:body response)
                        finite? (or (nil? body) (string? body) (bytes? body))
                        bridge (terminal/prepare! terminal!)
                        channel (:aleph/channel request)]
                    (when-not (and (map? response) (integer? (:status response)))
                      (throw (ex-info "Unsupported qualification response"
                                      {:reason :invalid-ring-response})))
                    ;; Register BEFORE returning to Aleph, otherwise the
                    ;; outgoing final frame can race its capture registration.
                    ((:register-response! capture) channel bridge)
                    (swap! responses conj {:bridge bridge :response response
                                           :channel channel})
                    (when finite?
                      ;; A fully materialized finite body in this harness has
                      ;; no provider-using continuation after Ring returns.
                      ((:producer-ceased! bridge)))
                    response))
                :max-in-flight 16
                :on-rejected! (constantly {:status 503 :headers {} :body "draining"})
                :stop-accept! (fn []
                                (.close ^Closeable @aleph-server)
                                {:outcome :stopped})})]
    (try
      (reset! aleph-server
              (aleph/start-server (:handle! guard)
                                  {:port 0
                                   :http-versions [:http1]
                                   :transport :nio
                                   :shutdown-timeout 0
                                   :http1-pipeline-transform
                                   (:http1-pipeline-transform capture)}))
      {:url (str "http://127.0.0.1:" (aleph-netty/port @aleph-server))
       :guard guard
       :capture capture
       :responses responses
       :stop! (fn [] ((:stop! guard) 2000))}
      (catch Throwable error
        (when-let [server @aleph-server] (.close ^Closeable server))
        (throw error)))))

(defn- get-request [url]
  (.build (HttpRequest/newBuilder (URI/create url))))

(deftest real-aleph-http1-final-write-enables-owned-drain-test
  (let [server (real-http1-server!
                (fn [_] {:status 200
                         :headers {"content-type" "text/plain"}
                         :body "real Aleph HTTP/1"}))
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (get-request (str (:url server) "/finite"))
                            (HttpResponse$BodyHandlers/ofString))]
        (is (= 200 (.statusCode response)))
        (is (= "real Aleph HTTP/1" (.body response)))
        (is (eventually #(= 1 (:matched ((:stats (:capture server))))))
            "Real outbound Aleph context must reach Gesso final-frame capture")
        (is (eventually #(zero? ((:active-count (:guard server)))))
            "The Netty final write plus finite-body quiescence must retire ownership")
        (is (= :succeeded
               (:write-outcome ((:status (:bridge (first @(:responses server)))))))))
      (is (= {:outcome :drained} ((:stop! server))))
      (is (true? ((:drained? (:guard server)))))
      (finally ((:stop! server))))))

(deftest real-aleph-stream-writer-requires-separate-producer-quiescence-test
  (let [source (stream/stream 4)
        server (real-http1-server!
                (fn [_] {:status 200
                         :headers {"content-type" "text/event-stream"}
                         :body source}))
        client (HttpClient/newHttpClient)]
    (try
      (let [response (.send client (get-request (str (:url server) "/stream"))
                            (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [input (.body response)]
          (is (= 200 (.statusCode response)))
          (is (true? (deref (stream/put! source "event: live-update\ndata: one\n\n")
                            2000 false)))
          (let [read-task (future (let [buf (byte-array 30)
                                        n (.read input buf)]
                                    (when (pos? n)
                                      (String. buf 0 n StandardCharsets/UTF_8))))]
            (try
              (is (= "event: live-update\ndata: one\n\n"
                     (deref read-task 3000 ::timeout)))
              (finally (future-cancel read-task))))
          (stream/close! source)
          (is (eventually #(= 1 (:matched ((:stats (:capture server))))))
              "Closing source must drive Aleph's outbound last-frame write")
          (let [bridge (:bridge (first @(:responses server)))]
            (is (eventually #(= :succeeded
                              (:write-outcome ((:status bridge)))))
                "Await the actual Netty write future, not only frame capture")
            (is (= 1 ((:active-count (:guard server))))
                "Final-write evidence alone cannot retire the source owner")
            ;; The test owns this producer and knows it has no outstanding
            ;; provider-using continuations. A production SSE adapter must
            ;; establish that independently (not merely on stream/close!).
            ((:producer-ceased! bridge))
            (is (eventually #(zero? ((:active-count (:guard server)))))))))
      (is (= {:outcome :drained} ((:stop! server))))
      (finally
        (stream/close! source)
        ;; Deliberately retain source debt if qualification fails; calling
        ;; stop! does not invent missing producer quiescence evidence.
        ((:stop! server))))))

(defn- read-raw-http1-response!
  "Read exactly one finite HTTP/1.1 response from an existing keep-alive
   socket. Require Content-Length rather than assuming the socket closes or
   consuming bytes from the following pipelined response. Test-only parser."
  [^java.io.BufferedReader reader]
  (let [status-line (.readLine reader)
        _ (when-not (and status-line
                         (re-find #"^HTTP/1\.1 200(?:\s|$)" status-line))
            (throw (ex-info "Unexpected HTTP/1 response status"
                            {:status-line status-line})))
        headers (loop [found {}]
                  (let [line (.readLine reader)]
                    (cond
                      (nil? line)
                      (throw (ex-info "Unexpected end of HTTP/1 headers" {}))

                      (= "" line) found

                      :else
                      (let [[header value] (str/split line #":" 2)]
                        (when-not value
                          (throw (ex-info "Malformed HTTP/1 header"
                                          {:line line})))
                        (recur (assoc found (str/lower-case header)
                                      (str/trim value)))))))]
    (when-not (contains? headers "content-length")
      (throw (ex-info "Finite HTTP/1 response needs Content-Length"
                      {:headers headers})))
    (let [length (Long/parseLong (get headers "content-length"))]
      (when-not (<= 0 length 65536)
        (throw (ex-info "Invalid response length" {:length length})))
      (let [chars (char-array (int length))]
        (loop [n 0]
          (if (= n length)
            {:status 200 :headers headers :body (String. chars)}
            (let [read (.read reader chars n (int (- length n)))]
              (when (neg? read)
                (throw (ex-info "Truncated HTTP/1 response body"
                                {:expected length :received n})))
              (recur (+ n read)))))))))

(deftest real-aleph-http1-persistent-socket-pairs-each-final-write-test
  ;; Two separately admitted Ring requests share ONE physical HTTP/1.1
  ;; keep-alive socket. Each LastHttpContent write must match precisely its
  ;; own Gesso bridge; the second must neither reuse the first completion
  ;; nor leak a pending response association.
  (let [server (real-http1-server!
                (fn [req] {:status 200
                           :headers {"content-type" "text/plain"}
                           :body (case (:uri req)
                                   "/first" "first response"
                                   "/second" "second response"
                                   "unexpected request")}))
        socket (java.net.Socket.)]
    (try
      (.connect socket
                (java.net.InetSocketAddress.
                 "127.0.0.1" (.getPort (URI/create (:url server))))
                2000)
      (.setSoTimeout socket 5000)
      (with-open [reader (java.io.BufferedReader.
                         (java.io.InputStreamReader.
                          (.getInputStream socket) StandardCharsets/UTF_8))]
        (doseq [[i uri expected] [[1 "/first" "first response"]
                                  [2 "/second" "second response"]]]
          (let [bytes (.getBytes
                       (str "GET " uri " HTTP/1.1\r\n"
                            "Host: 127.0.0.1\r\n"
                            "Connection: keep-alive\r\n\r\n")
                       StandardCharsets/US_ASCII)]
            (.write (.getOutputStream socket) bytes)
            (.flush (.getOutputStream socket)))
          (let [response (read-raw-http1-response! reader)]
            (is (= 200 (:status response)))
            (is (= expected (:body response)))
            (is (= "keep-alive" (some-> response :headers (get "connection") str/lower-case)))
            (is (eventually #(= i (:matched ((:stats (:capture server)))))))
            (is (eventually #(zero? ((:active-count (:guard server))))))
            (is (zero? (:pending ((:stats (:capture server)))))))))
      (let [owned @(:responses server)]
        (is (= 2 (count owned)))
        (is (identical? (:channel (first owned))
                        (:channel (second owned)))
            "Both physical HTTP responses must belong to the same socket")
        (is (every? #(= :succeeded
                        (:write-outcome ((:status (:bridge %)))))
                    owned)))
      (is (= {:outcome :drained} ((:stop! server))))
      (is ((:drained? (:guard server))))
      (finally
        (.close socket)
        ((:stop! server))))))

(deftest real-aleph-http1-abrupt-client-disconnect-preserves-owner-until-confirmed-cleanup-test
  ;; A real RST/close of an active streaming client makes Aleph's HTTP/1
  ;; channel inactive and causes it to stop consuming its Manifold source.
  ;; That does NOT prove the source owner's provider-using work has ceased.
  ;; The listener must retain the obligation until (1) final write outcome
  ;; or confirmed channel abort, AND (2) explicit producer quiescence.
  (let [source (stream/stream 4)
        producer-closed (promise)
        server (real-http1-server!
                (fn [_] {:status 200
                         :headers {"content-type" "text/event-stream"}
                         :body source}))
        socket (java.net.Socket.)]
    (stream/on-closed source #(deliver producer-closed true))
    (try
      (.connect socket
                (java.net.InetSocketAddress.
                 "127.0.0.1" (.getPort (URI/create (:url server))))
                2000)
      (.setSoTimeout socket 5000)
      (.write (.getOutputStream socket)
              (.getBytes (str "GET /disconnect HTTP/1.1\r\n"
                              "Host: 127.0.0.1\r\n"
                              "Connection: keep-alive\r\n\r\n")
                         StandardCharsets/US_ASCII))
      (.flush (.getOutputStream socket))
      (let [reader (java.io.BufferedReader.
                    (java.io.InputStreamReader.
                     (.getInputStream socket) StandardCharsets/UTF_8))
            status (.readLine reader)]
        (is (some? (re-find #"^HTTP/1\.1 200 " (or status ""))))
        (loop []
          (let [line (.readLine reader)]
            (when-not line
              (throw (ex-info "HTTP SSE response headers unexpectedly ended" {})))
            (when-not (empty? line) (recur))))
        (is (true? (deref (stream/put! source
                                    "event: live-update\ndata: before-disconnect\n\n")
                          2000 false)))
        (is (loop [n 0]
              (cond
                (> n 20) false
                :else (let [line (.readLine reader)]
                        (cond (nil? line) false
                              (= "event: live-update" line) true
                              :else (recur (inc n))))))
            "Actual streaming HTTP/1 data must precede the disconnect")
        (is (eventually #(= 1 (count @(:responses server)))))
        (let [{:keys [bridge channel]} (first @(:responses server))
              guard (:guard server)]
          (is (= 1 ((:active-count guard))))
          (is (false? (:terminal? ((:status bridge)))))
          ;; Send an abrupt real client-side reset. No direct source/bridge
          ;; cancellation is initiated by the test until Aleph observes it.
          (.setSoLinger socket true 0)
          (.close socket)
          (is (eventually #(and (.isDone (.closeFuture channel))
                                (not (.isActive channel))))
              "Aleph must observe actual channel disconnection")
          (is (= true (deref producer-closed 4000 ::timeout))
              "Aleph's disconnect cleanup must close its response source")
          (is (stream/closed? source))
          (is (= 1 ((:active-count guard)))
              "Disconnection/source close alone is not owner retirement")
          (is (false? (:terminal? ((:status bridge)))))
          (is (zero? (:pending ((:stats (:capture server)))))
              "Closed channels do not retain pipeline-registry references")
          (is (= {:outcome :timed-out} ((:stop! server)))
              "Host must not drain before producer and writer completion")
          (is (false? ((:drained? guard))))
          ;; The channel closeFuture proves this socket can never resume its
          ;; response. A final-write failure/success, if already observed,
          ;; is also terminal. Otherwise explicitly certify an aborted writer
          ;; *after* channel closure, not on the upstream stream-close event.
          (when (#{:unregistered :pending} (:write-outcome ((:status bridge))))
            ((:writer-aborted! bridge)))
          (is (#{:succeeded :failed :aborted}
               (:write-outcome ((:status bridge)))))
          (is (= 1 ((:active-count guard)))
              "Writer termination alone must still wait for producer cleanup")
          ;; This harness's response producer has no additional callbacks
          ;; after its observed stream-close callback has returned. Production
          ;; source owners must supply their own such evidence.
          ((:producer-ceased! bridge))
          (is (eventually #(zero? ((:active-count guard)))))
          (is (= {:outcome :drained} ((:stop! server))))
          (is ((:drained? guard)))
          (is (true? (:terminal? ((:status bridge)))))))
      (finally
        (.close socket)
        (stream/close! source)
        ((:stop! server))))))

(deftest real-aleph-http1-disconnect-releases-actual-gesso-source-automatically-test
  ;; Unlike the prior pure-transport disconnect test, this test requires the
  ;; real source-backed SSE :cancel! returned by Gesso Live. No test code
  ;; invokes bridge/producer-ceased!, writer-aborted!, or source cancellation.
  ;; The Aleph channel close future invokes the new off-eventloop handoff.
  (let [src (source/create {:max-active-consumers 2})
        executor (java.util.concurrent.Executors/newSingleThreadExecutor)
        capture (capture/prepare!)
        server-ref (atom nil)
        owned-session (promise)
        guard (listener/prepare!
               {:dispatch-async!
                (fn [request terminal!]
                  (let [session (sse/start-source!
                                 src {:subscription {:topic :request :id "v2254"}}
                                 {:keepalive-ms 15000
                                  :frame-write-timeout-ms 30000})
                        bridge (terminal/prepare! terminal!)
                        ch (:aleph/channel request)]
                    ((:register-response! capture) ch bridge)
                    (let [release (aleph-live/attach!
                                   {:channel ch :session session :bridge bridge
                                    :executor executor})]
                      (deliver owned-session {:session session :bridge bridge
                                              :release release :channel ch})
                      (:response session))))
                :max-in-flight 2
                :on-rejected! (constantly {:status 503 :headers {} :body "overloaded"})
                :stop-accept! (fn []
                                (.close ^Closeable @server-ref)
                                {:outcome :stopped})})
        socket (java.net.Socket.)]
    (try
      (reset! server-ref
              (aleph/start-server (:handle! guard)
                                  {:port 0 :http-versions [:http1]
                                   :transport :nio :shutdown-timeout 0
                                   :http1-pipeline-transform
                                   (:http1-pipeline-transform capture)}))
      (.connect socket
                (java.net.InetSocketAddress.
                 "127.0.0.1" (aleph-netty/port @server-ref))
                2000)
      (.setSoTimeout socket 5000)
      (.write (.getOutputStream socket)
              (.getBytes (str "GET /live/events HTTP/1.1\r\n"
                              "Host: 127.0.0.1\r\n"
                              "Connection: keep-alive\r\n\r\n")
                         StandardCharsets/US_ASCII))
      (.flush (.getOutputStream socket))
      (let [reader (java.io.BufferedReader.
                    (java.io.InputStreamReader.
                     (.getInputStream socket) StandardCharsets/UTF_8))]
        (is (re-find #"^HTTP/1\.1 200 " (or (.readLine reader) "")))
        (loop []
          (let [line (.readLine reader)]
            (when-not line (throw (ex-info "Lost response headers" {})))
            (when (seq line) (recur))))
        (let [{:keys [session bridge release channel]}
              (deref owned-session 5000 ::timeout)]
          (is (map? session))
          (is (eventually #(= 1 (:subscription-count (source/stats src)))))
          (is (false? (:released? ((:status release)))))
          (is (= 1 ((:active-count guard))))
          (source/emit! src {:topic :request :id "v2254"
                             :change/kind :updated})
          (is (loop [n 0]
                (cond (> n 30) false
                      :else (let [line (.readLine reader)]
                              (cond (nil? line) false
                                    (= "event: live-update" line) true
                                    :else (recur (inc n))))))
              "Receive an SSE frame produced from the real Gesso source")
          ;; No manual cancellation or evidence notification follows.
          (.setSoLinger socket true 0)
          (.close socket)
          (is (eventually #(.isDone (.closeFuture channel))))
          (is (eventually #(:released? ((:status release)))))
          (is (= 1 (:release-attempts ((:status release)))))
          (is (nil? (:last-error ((:status release)))))
          (is (stream/closed? (:stream session)))
          (is (true? ((:source-closed? session))))
          (is (eventually #(zero? (:subscription-count (source/stats src)))))
          (is (eventually #(zero? ((:active-count guard)))))
          (is (true? (:terminal? ((:status bridge)))))
          (is (= {:outcome :drained} ((:stop! guard) 2000)))
          (is (= 0 (:pending ((:stats capture)))))))
      (finally
        (.close socket)
        (when-let [server @server-ref]
          (try (.close ^Closeable server) (catch Throwable _ nil)))
        (source/close! src)
        (.shutdownNow executor)))))

(deftest real-aleph-authorized-route-disconnect-retires-owned-capacity-test
  ;; Exercise the actual Biff/XTDB-hosted authorized SSE route, NOT a
  ;; separately constructed Live session. The session-admission callback
  ;; hands the Aleph listener the route-owned cancellation handle, so
  ;; disconnect cleanup and explicit host retry share a single ledger.
  (let [standard ((requiring-resolve
                   'gesso.live.application-preflight.fixture/standard-operations))
        render ((requiring-resolve
                   'gesso.live.application-preflight.fixture/render-context) standard)
        button ((requiring-resolve
                   'gesso.live.application-preflight.fixture/rendered-operation-button)
                render :request/claim "/operations/request/claim")
        fixture (requiring-resolve
                 'gesso.live.application-preflight.fixture/with-surfaced-application)]
    (fixture
     {:request-board [:main button]}
     (fn [{:keys [assembly]}]
       (let [host-start (requiring-resolve 'gesso.live.outbox.host/start!)
             host-stop (requiring-resolve 'gesso.live.outbox.host/stop!)
             config {:xtdb-config {}
                     :source-id "gesso/v2255-aleph-route"
                     :rules-context {}
                     :worker-options {:worker-id "v2255-aleph-worker"
                                      :poll-ms 25}
                     :live-options
                     {:rules [{:when-topic :request
                               :expand (fn [_ change]
                                         [(select-keys change
                                                       [:topic :id :change/kind])])}]
                      :source-options {:coalesce-window-ms 0}
                      :dispatch-options {:threads 1 :queue-size 8
                                         :on-overflow :throw}}}
             app (host-start assembly
                             (fn [_] {:status 200 :headers {} :body "ordinary"})
                             config)
             src (:source (:gesso.live/system app))
             capture (capture/prepare!)
             executor (java.util.concurrent.Executors/newFixedThreadPool 2)
             server-ref (atom nil)
             admitted (atom [])
             host-stopped? (atom false)
             route (http/wrap-authorized-sse
                    app {:path "/live/events" :max-connections 1
                         :authorize-subscription
                         (constantly {:topic :request :id "v2255-item"})
                         :on-session-admitted!
                         (fn [request owned-session]
                           (let [bridge (::route-bridge request)
                                 channel (:aleph/channel request)
                                 hook (aleph-live/attach!
                                       {:channel channel
                                        :session owned-session
                                        :bridge bridge
                                        :executor executor})]
                             (swap! admitted conj {:session owned-session
                                                   :bridge bridge
                                                   :channel channel
                                                   :hook hook})))})
             guard (listener/prepare!
                    {:dispatch-async!
                     (fn [request done!]
                       (let [bridge (terminal/prepare! done!)
                             response ((:handler route)
                                       (assoc request ::route-bridge bridge))]
                         ((:register-response! capture)
                          (:aleph/channel request) bridge)
                         (when-not (stream/stream? (:body response))
                           ((:producer-ceased! bridge)))
                         response))
                     :max-in-flight 4
                     :on-rejected!
                     (constantly {:status 503 :headers {} :body "draining"})
                     :stop-accept! (fn []
                                     (.close ^Closeable @server-ref)
                                     {:outcome :stopped})})
             shutdown (http/prepare-managed-http-shutdown
                       {:route route
                        :stop-listener! (fn [] ((:stop! guard) 3000))
                        :stop-host! (fn []
                                      (host-stop app)
                                      (reset! host-stopped? true)
                                      {:outcome :stopped})
                        :drain-timeout-ms 3000})
             socket (java.net.Socket.)]
         (try
           (reset! server-ref
                   (aleph/start-server (:handle! guard)
                                       {:port 0 :http-versions [:http1]
                                        :transport :nio :shutdown-timeout 0
                                        :http1-pipeline-transform
                                        (:http1-pipeline-transform capture)}))
           (.connect socket
                     (java.net.InetSocketAddress.
                      "127.0.0.1" (aleph-netty/port @server-ref))
                     2000)
           (.setSoTimeout socket 5000)
           (.write (.getOutputStream socket)
                   (.getBytes (str "GET /live/events HTTP/1.1\r\n"
                                   "Host: 127.0.0.1\r\n"
                                   "Connection: keep-alive\r\n\r\n")
                              StandardCharsets/US_ASCII))
           (.flush (.getOutputStream socket))
           (let [reader (java.io.BufferedReader.
                         (java.io.InputStreamReader.
                          (.getInputStream socket) StandardCharsets/UTF_8))]
             (is (re-find #"^HTTP/1\.1 200 " (or (.readLine reader) "")))
             (loop []
               (let [line (.readLine reader)]
                 (when-not line (throw (ex-info "Lost SSE headers" {})))
                 (when (seq line) (recur))))
             (is (eventually #(= 1 (:subscription-count (source/stats src)))))
             (is (= 1 ((:occupied-count route))))
             (is (= 1 (count @admitted)))
             (let [response ((:handler route)
                             {:uri "/live/events" :request-method :get})]
               (is (= 503 (:status response))
                   "Route cap still applies while Aleph owns first stream"))
             (source/emit! src {:topic :request :id "v2255-item"
                                :change/kind :updated})
             (is (loop [n 0]
                   (cond (> n 30) false
                         :else (let [line (.readLine reader)]
                                 (cond (nil? line) false
                                       (= "event: live-update" line) true
                                       :else (recur (inc n)))))))
             (.setSoLinger socket true 0)
             (.close socket)
             (let [{:keys [bridge hook channel]} (first @admitted)]
               (is (eventually #(.isDone (.closeFuture channel))))
               (is (eventually #(:released? ((:status hook)))))
               (is (= 1 (:release-attempts ((:status hook)))))
               (is (nil? (:last-error ((:status hook)))))
               (is (true? (:terminal? ((:status bridge))))))
             (is (eventually #(and (zero? ((:occupied-count route)))
                                   (zero? ((:disconnected-count route)))
                                   (zero? (:subscription-count (source/stats src)))
                                   (zero? ((:active-count guard))))))
             (is (= 0 (:pending ((:stats capture)))))
             (is (= :stopped (:outcome ((:stop! shutdown)))))
             (is @host-stopped?)
             (is (true? ((:drained? route))))
             (is (= :stopped (:outcome ((:stop! shutdown))))))
           (finally
             (.close socket)
             (when-not @host-stopped?
               (try ((:stop! shutdown)) (catch Throwable _ nil)))
             (when-let [server @server-ref]
               (try (.close ^Closeable server) (catch Throwable _ nil)))
             (.shutdownNow executor)
             ;; If shutdown was not certified, the fixture must still
             ;; terminate its temporary XTDB host; never leak test resources.
             (when-not @host-stopped? (host-stop app)))))))))

(deftest real-aleph-failed-release-scheduled-recovery-preserves-healthy-peer-test
  ;; One genuine Gesso/XTDB host, two independently authorized source-backed
  ;; SSE sessions and two real Aleph sockets. Failure injection is restricted
  ;; to the first session's physical source-release callback: its route slot
  ;; and disconnected debt must remain owned while scheduled recovery runs.
  ;; Once the scheduler succeeds, Aleph's failed per-channel handoff must
  ;; separately reconcile terminal completion (not infer it from route drain).
  (let [original-open flow/open-source-flow!
        release-blocked? (atom true)
        bad-close-attempts (atom 0)
        fault (ex-info "transient physical source-release failure" {:test :v2256})
        standard ((requiring-resolve
                   'gesso.live.application-preflight.fixture/standard-operations))
        render ((requiring-resolve
                 'gesso.live.application-preflight.fixture/render-context) standard)
        button ((requiring-resolve
                 'gesso.live.application-preflight.fixture/rendered-operation-button)
                render :request/claim "/operations/request/claim")
        fixture (requiring-resolve
                 'gesso.live.application-preflight.fixture/with-surfaced-application)]
    (with-redefs-fn
      {#'flow/open-source-flow!
       (fn [src options]
         (let [opened (original-open src options)]
           (if (= "v2256-bad" (get-in options [:subscription :id]))
             ;; Model an additional physical indexed source interest that
             ;; the bad session must release. The ordinary collector is free
             ;; to close its own stream/tap independently; this extra tap
             ;; remains genuinely registered until source cleanup succeeds.
             (let [retained (source/subscribe src (:subscription options))
                   close-original! (:close! opened)
                   original-closed? (:closed? opened)]
               (assoc opened
                      :close! (fn []
                                (close-original!)
                                (swap! bad-close-attempts inc)
                                (if @release-blocked?
                                  (throw fault)
                                  (stream/close! retained)))
                      :closed? (fn []
                                 (and (original-closed?)
                                      (stream/closed? retained)))
                      :on-closed! (fn [callback]
                                    (stream/on-closed retained callback))))
             opened)))}
      (fn []
        (fixture
         {:request-board [:main button]}
         (fn [{:keys [assembly]}]
           (let [host-start (requiring-resolve 'gesso.live.outbox.host/start!)
                 host-stop (requiring-resolve 'gesso.live.outbox.host/stop!)
                 app (host-start
                      assembly
                      (fn [_] {:status 200 :headers {} :body "ordinary"})
                      {:xtdb-config {}
                       :source-id "gesso/v2256-aleph-recovery"
                       :rules-context {}
                       :worker-options {:worker-id "v2256-recovery-worker"
                                        :poll-ms 25}
                       :live-options
                       {:rules [{:when-topic :request
                                 :expand (fn [_ change]
                                           [(select-keys change
                                                         [:topic :id :change/kind])])}]
                        :source-options {:coalesce-window-ms 0
                                         :max-active-consumers 4}
                        :dispatch-options {:threads 1 :queue-size 8
                                           :on-overflow :throw}}})
                 src (:source (:gesso.live/system app))
                 observer (capture/prepare!)
                 pool (java.util.concurrent.Executors/newFixedThreadPool 2)
                 server (atom nil)
                 admitted (atom {})
                 stopped? (atom false)
                 scheduler (atom nil)
                 shutdown (atom nil)
                 sockets (atom [])
                 route (http/wrap-authorized-sse
                        app {:path "/live/events" :max-connections 2
                             :authorize-subscription
                             (fn [request]
                               {:topic :request
                                :id (if (= "client=bad" (:query-string request))
                                      "v2256-bad" "v2256-good")})
                             :on-session-admitted!
                             (fn [request owned-session]
                               (let [bridge (::route-bridge request)
                                     ch (:aleph/channel request)
                                     hook (aleph-live/attach!
                                           {:channel ch :session owned-session
                                            :bridge bridge :executor pool})]
                                 (swap! admitted assoc (:query-string request)
                                        {:bridge bridge :hook hook
                                         :channel ch :session owned-session})))})
                 guard (listener/prepare!
                        {:dispatch-async!
                         (fn [request done!]
                           (let [bridge (terminal/prepare! done!)
                                 response ((:handler route)
                                           (assoc request ::route-bridge bridge))]
                             ((:register-response! observer)
                              (:aleph/channel request) bridge)
                             (when-not (stream/stream? (:body response))
                               ((:producer-ceased! bridge)))
                             response))
                         :max-in-flight 4
                         :on-rejected!
                         (constantly {:status 503 :headers {}
                                      :body "listener draining"})
                         :stop-accept! (fn []
                                         (.close ^Closeable @server)
                                         {:outcome :stopped})})
                 open-client!
                 (fn [id]
                   (let [socket (java.net.Socket.)]
                     (swap! sockets conj socket)
                     (.connect socket
                               (java.net.InetSocketAddress.
                                "127.0.0.1" (aleph-netty/port @server)) 2000)
                     (.setSoTimeout socket 5000)
                     (let [writer (.getOutputStream socket)
                           request (str "GET /live/events?client=" id " HTTP/1.1\r\n"
                                        "Host: 127.0.0.1\r\n"
                                        "Connection: keep-alive\r\n\r\n")
                           reader (java.io.BufferedReader.
                                   (java.io.InputStreamReader.
                                    (.getInputStream socket)
                                    StandardCharsets/UTF_8))]
                       (.write writer (.getBytes request StandardCharsets/US_ASCII))
                       (.flush writer)
                       (when-not (re-find #"^HTTP/1\.1 200 " (or (.readLine reader) ""))
                         (throw (ex-info "Aleph SSE connection was not admitted"
                                         {:client id})))
                       (loop []
                         (let [line (.readLine reader)]
                           (when-not line (throw (ex-info "Missing SSE headers" {})))
                           (when (seq line) (recur))))
                       {:socket socket :reader reader})))
                 read-update!
                 (fn [reader]
                   (loop [lines 0]
                     (cond (> lines 40) false
                           :else (let [line (.readLine reader)]
                                   (cond (nil? line) false
                                         (= "event: live-update" line) true
                                         :else (recur (inc lines)))))))]
             (try
               (reset! server
                       (aleph/start-server
                        (:handle! guard)
                        {:port 0 :http-versions [:http1]
                         :transport :nio :shutdown-timeout 0
                         :http1-pipeline-transform
                         (:http1-pipeline-transform observer)}))
               (let [bad (open-client! "bad")
                     good (open-client! "good")]
                 (is (eventually #(and (= 2 ((:occupied-count route)))
                                      (= 3 (:subscription-count (source/stats src)))
                                      (= 2 (count @admitted)))))
                 (is (= 2 ((:active-count guard))))
                 (is (= 503
                        (.statusCode
                         (.send (HttpClient/newHttpClient)
                                (get-request (str "http://127.0.0.1:"
                                                  (aleph-netty/port @server)
                                                  "/live/events?client=third"))
                                (HttpResponse$BodyHandlers/ofString))))
                     "The occupied route cannot admit a third session")
                 (source/emit! src {:topic :request :id "v2256-good"
                                    :change/kind :updated})
                 (is (read-update! (:reader good))
                     "A healthy peer receives SSE before recovery")
                 (.setSoLinger ^java.net.Socket (:socket bad) true 0)
                 (.close ^java.net.Socket (:socket bad))
                 (is (eventually #(some? (:last-error
                                         ((:status (:hook (get @admitted "client=bad")))))))
                     "Aleph close attempts the route-owned release, which fails")
                 (is (pos? @bad-close-attempts))
                 (is (= 1 ((:disconnected-count route))))
                 (is (= 2 (:subscription-count (source/stats src)))
                     "The failing cleanup retains a REAL indexed source tap")
                 (is (= 2 ((:occupied-count route)))
                     "Failed physical release preserves the route's occupied slot")
                 (is (= 2 ((:active-count guard)))
                     "The failed Aleph terminal callback cannot fake drain")
                 (is (false? (:released?
                              ((:status (get-in @admitted ["client=bad" :hook]))))))
                 (reset! scheduler
                         (recovery/start-scheduled!
                          route {:poll-ms 20 :min-interval-ms 30
                                 :max-attempts 1 :max-failure-interval-ms 100}))
                 (is (eventually #(pos? (:failure-count ((:stats @scheduler)))))
                     "Real route-owned scheduler observes physical release failures")
                 (is (= 2 ((:occupied-count route))))
                 (is (= 1 ((:disconnected-count route))))
                 (is (zero? (:release-attempts
                             ((:status (get-in @admitted ["client=good" :hook])))))
                     "The healthy peer must not be canceled by recovery")
                 (source/emit! src {:topic :request :id "v2256-good"
                                    :change/kind :updated})
                 (is (read-update! (:reader good))
                     "The healthy peer must remain live during failed retries")
                 (reset! release-blocked? false)
                 (is (eventually #(and (zero? ((:disconnected-count route)))
                                      (= 1 ((:occupied-count route)))))
                     "Scheduled retry releases the failed route slot")
                 (is (pos? (get-in ((:stats @scheduler))
                                   [:last-physical-outcome :recovered] 0)))
                 (is (= 2 ((:active-count guard)))
                     "The Aleph terminal obligation is separate from route recovery")
                 (is (= :released
                        ((:retry! (:hook (get @admitted "client=bad")))))
                     "Reconcile confirmed route release with the dead Netty channel")
                 (is (eventually #(= 1 ((:active-count guard)))))
                 (is (true? (:terminal?
                             ((:status (get-in @admitted ["client=bad" :bridge]))))))
                 (is (= 1 ((:occupied-count route))))
                 (is (= 1 (:subscription-count (source/stats src))))
                 (source/emit! src {:topic :request :id "v2256-good"
                                    :change/kind :updated})
                 (is (read-update! (:reader good))
                     "Recovered failure must not disrupt healthy peer delivery")
                 (.setSoLinger ^java.net.Socket (:socket good) true 0)
                 (.close ^java.net.Socket (:socket good))
                 (is (eventually #(and
                                  (true? (:released?
                                          ((:status (get-in @admitted ["client=good" :hook])))))
                                  (zero? ((:occupied-count route)))
                                  (zero? (:subscription-count (source/stats src)))
                                  (zero? ((:active-count guard))))))
                 (is (= 0 (:pending ((:stats observer)))))
                 (reset! shutdown
                         (recovery/prepare-owned-shutdown
                          @scheduler
                          {:route route
                           :stop-listener! (fn [] ((:stop! guard) 3000))
                           :stop-host! (fn []
                                         (host-stop app)
                                         (reset! stopped? true)
                                         {:outcome :stopped})
                           :drain-timeout-ms 3000}))
                 (is (= :stopped (:outcome ((:stop! @shutdown)))))
                 (is @stopped?)
                 (is (true? ((:drained? route)))))
               (finally
                 (reset! release-blocked? false)
                 (doseq [socket @sockets]
                   (try (.close ^java.net.Socket socket)
                        (catch Throwable _ nil)))
                 (when-let [handle @scheduler]
                   (try ((:stop! handle) 3000) (catch Throwable _ nil)))
                 (when-let [coordinator @shutdown]
                   (when-not @stopped?
                     (try ((:stop! coordinator)) (catch Throwable _ nil))))
                 (when-let [aleph-server @server]
                   (try (.close ^Closeable aleph-server) (catch Throwable _ nil)))
                 (.shutdownNow pool)
                 (when-not @stopped?
                   (try (host-stop app) (catch Throwable _ nil))))))))))))
