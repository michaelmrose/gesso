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
            (is (= :succeeded (:write-outcome ((:status bridge)))))
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
