(ns gesso.assurance.proof.xtdb-gateway-monotonic-floor-register-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.xtdb-gateway-monotonic-floor-register :as register]
            [gesso.assurance.proof.xtdb-gateway-externally-anchored-floor :as floor]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed]
            [gesso.assurance.proof.xtdb-gateway-local-key-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test])
  (:import [java.nio.file Files Path LinkOption]
           [java.security KeyPairGenerator Signature]
           [java.time Instant]
           [java.util Base64 UUID]
           [java.util.concurrent TimeUnit]))

(defn- pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))
(defonce ^:private authorized-client-pair (pair))
(defn- b64 [data] (.encodeToString (Base64/getEncoder) data))
(defn- spki [p] (b64 (.getEncoded (.getPublic p))))
(defn- pkcs8 [p] (b64 (.getEncoded (.getPrivate p))))
(defn- sign [^bytes data p]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (.getPrivate p))
    (.update s data)
    (b64 (.sign s))))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn- with-dir [f]
  (let [^Path dir (Files/createTempDirectory "gesso-floor-register-"
                                                (make-array java.nio.file.attribute.FileAttribute 0))]
    (try (f dir)
         (finally
           (doseq [file (reverse (file-seq (.toFile dir)))]
             (Files/deleteIfExists (.toPath file)))))))
(defn- config [dir pair init?]
  {:directory dir :signing-private-pkcs8-base64 (pkcs8 pair)
   :signing-public-spki-base64 (spki pair)
   :authorized-client-spki-base64 (spki authorized-client-pair)
   :initialize-empty? init?})
(defn- genesis [] {:revision 0 :checkpoint-digest "genesis"})
(defn- f1 [] {:revision 1 :checkpoint-digest (str "sha256:" (apply str (repeat 64 "a")))})
(defn- f2 [] {:revision 2 :checkpoint-digest (str "sha256:" (apply str (repeat 64 "b")))})
(defn- advance-request [before after]
  {:type floor/statement-type :version 1 :kind :advance
   :nonce (str (UUID/randomUUID))
   :expected-floor before :next-floor after :accepted? true})
(defn- read-request [] {:kind :read :nonce (str (UUID/randomUUID))})
(defn- signed-envelope? [envelope key]
  (try
    (let [sig (.decode (Base64/getDecoder) ^String (:signature-base64 envelope))
          v (Signature/getInstance "Ed25519")]
      (.initVerify v (.getPublic key))
      (.update v ^bytes (floor/floor-signing-bytes (:statement envelope)))
      (.verify v sig))
    (catch Throwable _ false)))

(deftest v1904-persisted-register-cas-and-signature
  (with-dir
    (fn [dir]
      (let [key (pair)
            r (register/prepare-durable-register! (config dir key true))
            initial ((:read! r) (read-request))]
        (is (signed-envelope? initial key))
        (is (= (genesis) (get-in initial [:statement :floor])))
        (is (= :none (:authority r) (:proof-discharge r)))
        (is (= :invalid-or-occupied-socket-path
               (reason #(register/serve-unix! r
                          (.resolve ^Path (.getParent dir) "outside.sock")))))
        (let [req (advance-request (genesis) (f1))
              result ((:advance! r) req)]
          (is (signed-envelope? result key))
          (is (= req (:statement result)))
          (is (= :stale-compare-and-advance
                 (reason #((:advance! r) req)))))
        (is (= (f1) (get-in ((:read! (register/prepare-durable-register!
                                       (config dir key false)))
                             (read-request)) [:statement :floor])))
        (is (= :invalid-floor-statement
               (reason #((:advance! r)
                          (assoc (advance-request (f1) (f2))
                                 :certificate :issued)))))
        (is (= :invalid-read-request
               (reason #((:read! r) (assoc (read-request) :authority :write)))))
        (is (= :configured-signing-keypair-mismatch
               (reason #(register/prepare-durable-register!
                          (assoc (config dir key false)
                                 :signing-public-spki-base64 (spki (pair)))))))
        (is (= :persisted-floor-signature-not-verified
               (do (spit (.toFile (.resolve ^Path dir "external-rollback-floor.edn"))
                         (pr-str {:type register/register-type :version 1
                                  :signed-state {:statement {:type floor/statement-type
                                                             :version 1 :kind :read
                                                             :nonce "00000000-0000-0000-0000-000000000000"
                                                             :floor (f2)}
                                                 :signature-base64
                                                 (:signature-base64 initial)}}))
                   (reason #((:read! r) (read-request))))))
        (is (= :persisted-floor-signature-not-verified
               (reason #(register/prepare-durable-register! (config dir key true)))))))))

(deftest v1904-no-silent-reinitialization-and-symlink
  (with-dir
    (fn [dir]
      (let [key (pair)
            cfg (config dir key false)]
        (is (= :explicit-register-genesis-required
               (reason #(register/prepare-durable-register! cfg))))
        (register/prepare-durable-register! (assoc cfg :initialize-empty? true))
        (let [file (.resolve ^Path dir "external-rollback-floor.edn")]
          (Files/delete file)
          (is (= :explicit-register-genesis-required
                 (reason #(register/prepare-durable-register! cfg))))
          (let [outside (.resolve ^Path dir "other")]
            (spit (.toFile outside) "x")
            (Files/createSymbolicLink file outside
                                      (make-array java.nio.file.attribute.FileAttribute 0))
            (is (= :non-regular-or-symlink-register-file
                   (reason #(register/prepare-durable-register!
                             (assoc cfg :initialize-empty? true)))))))))))

(defn- start-server! [dir key socket-path init?]
  (let [java-bin (str (System/getProperty "java.home") "/bin/java")
        command [java-bin "-Xmx1g" "--add-opens=java.base/java.nio=ALL-UNNAMED"
                 "--enable-native-access=ALL-UNNAMED"
                 "-cp" (System/getProperty "java.class.path")
                 "clojure.main" "-m"
                 "gesso.assurance.proof.xtdb-gateway-monotonic-floor-register"
                 (str dir) (str socket-path) (if init? "init" "existing")]
        pb (ProcessBuilder. ^java.util.List command)
        env (.environment pb)]
    (.put env "GESSO_FLOOR_PKCS8" (pkcs8 key))
    (.put env "GESSO_FLOOR_SPKI" (spki key))
    (.put env "GESSO_FLOOR_CLIENT_SPKI" (spki authorized-client-pair))
    (.redirectErrorStream pb true)
    (.redirectOutput pb (.toFile (.resolve ^Path dir
                                           (str ".server-" (UUID/randomUUID) ".log"))))
    (let [process (.start pb)
          client (register/prepare-unix-client
                  socket-path {:client-signing-private-pkcs8-base64
                               (pkcs8 authorized-client-pair)})]
      (loop [attempt 0]
        (when (>= attempt 160)
          (.destroyForcibly process)
          (throw (ex-info "server did not bind socket" {:socket (str socket-path)})))
        (if (and (Files/exists socket-path (make-array LinkOption 0))
                 (.isAlive process)
                 (try ((:read-external-floor! client) (read-request))
                      true
                      (catch Throwable _ false)))
          nil
          (do (Thread/sleep 40)
              (recur (inc attempt)))))
      {:process process :socket socket-path :client client})))

(defn- stop-server! [{:keys [^Process process ^Path socket]}]
  (when process
    (.destroy process)
    (when-not (.waitFor process 3 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (.waitFor process 3 TimeUnit/SECONDS)))
  (when socket (Files/deleteIfExists socket)))

(deftest v1904-unix-socket-separate-jvms-restart-and-cas
  (with-dir
    (fn [dir]
      (let [key (pair)
            socket1 (.resolve ^Path dir "floor1.sock")
            socket2 (.resolve ^Path dir "floor2.sock")
            first-server (start-server! dir key socket1 true)]
        (try
          (let [second-server (start-server! dir key socket2 false)]
            (try
              (let [c1 (:client first-server) c2 (:client second-server)
                    nonce (read-request)
                    initial ((:read-external-floor! c1) nonce)
                    r1 (advance-request (genesis) (f1))]
                (is (signed-envelope? initial key))
                (is (= (:nonce nonce) (get-in initial [:statement :nonce])))
                (is (= (genesis) (get-in initial [:statement :floor])))
                (is (signed-envelope? ((:advance-external-floor! c1) r1) key))
                (is (= :external-register-refused-request
                       (reason #((:advance-external-floor! c2) r1))))
                (is (= (f1) (get-in ((:read-external-floor! c2) (read-request))
                                    [:statement :floor])))
                ;; Two JVMs, same on-disk lock: concurrent CAS never both win.
                (let [req (advance-request (f1) (f2))
                      gate (java.util.concurrent.CountDownLatch. 1)
                      outcomes (atom [])
                      workers (mapv (fn [client]
                                      (Thread.
                                       (fn []
                                         (.await gate)
                                         (swap! outcomes conj
                                                (try ((:advance-external-floor! client) req)
                                                     :ok
                                                     (catch Throwable _ :rejected))))))
                                    [c1 c2])]
                  (doseq [^Thread worker workers] (.start worker))
                  (.countDown gate)
                  (doseq [^Thread worker workers] (.join worker 10000))
                  (is (every? #(not (.isAlive ^Thread %)) workers))
                  (is (= #{:ok :rejected} (set @outcomes))))
                (is (= (f2) (get-in ((:read-external-floor! c1) (read-request))
                                    [:statement :floor]))))
              (finally (stop-server! second-server))))
          (stop-server! first-server)
          (let [restarted (start-server! dir key socket1 false)]
            (try
              (is (= (f2) (get-in ((:read-external-floor! (:client restarted))
                                   (read-request)) [:statement :floor])))
              (finally (stop-server! restarted))))
          (finally (stop-server! first-server)))))))

(defn- delegation [root delegate]
  (let [now (Instant/now)
        g {:version 1 :delegation-id (str (UUID/randomUUID))
           :audience "gesso.xtdb.transaction-causality-candidate.v1"
           :obligation :exclusive-transaction-causality
           :witness-family :transaction-coupled-apply-provenance
           :delegated-ed25519-spki-base64 (spki delegate)
           :valid-from (str (.minusSeconds now 60))
           :valid-until (str (.plusSeconds now 3600))}
        sign-grant (requiring-resolve
                    'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)]
    {:root-ed25519-spki-base64 (spki root)
     :delegation g
     :root-signature-base64 (sign (sign-grant g) root)}))
(defn- policy []
  (lifecycle/prepare-process-local-root-policy!
   (delegation (pair) (pair))))
(defn- checkpoint [p previous key]
  (let [d {:type signed/checkpoint-type :version 1
           :previous-digest previous :snapshot ((:checkpoint-material! p))}]
    {:checkpoint d
     :signature-base64 (sign (signed/checkpoint-signing-bytes d) key)}))

(deftest v1904-separate-jvm-register-integrates-with-real-xtdb-checkpoint
  (physical/with-proving-application!
   (fn [app]
     (with-dir
       (fn [dir]
         (let [p (policy) cp (pair) rk (pair)
               socket (.resolve ^Path dir "separate-register.sock")
               s (start-server! dir rk socket true)]
           (try
             (let [client (:client s)
                   cfg {:directory dir :policy p
                        :checkpoint-authority-ed25519-spki-base64 (spki cp)
                        :external-floor-ed25519-spki-base64 (spki rk)
                        :read-external-floor! (:read-external-floor! client)
                        :advance-external-floor! (:advance-external-floor! client)
                        :initialize-empty? true}
                   adapter (floor/prepare-external-floor-adapter! cfg)
                   source (var-get
                           (ns-resolve
                            'gesso.assurance.proof.xtdb-restricted-submission-gateway-test
                            'source-for))
                   g (intent/prepare-one-shot! (:node app)
                                               (source [((:read! app))]))
                   returned ((:submit! g))
                   evidence (intent/inspect-pinned-intent! g returned)
                   after ((:append! adapter) (checkpoint p "genesis" cp))]
               (is (= :bounded-private-before-after-intent-ledger-coobservation
                      (:protected-before-after-intent-ledger-correlation evidence)))
               (is (= 7 (get-in evidence
                                [:correspondence-premises :assurance-frontier :open-count])))
               (is (= 1 (:head-revision after)))
               (is (= :signed-compare-and-advance-confirmed
                      (:external-advance after)))
               (is (= :not-established (:external-register-independence after)))
               (is (= :none (:authority after) (:proof-discharge after)))
               (is (= :not-issued (:certificate after)))
               (is (= 1 (:head-revision
                         ((:status! (floor/prepare-external-floor-adapter!
                                     (assoc cfg :initialize-empty? false))))))))
             (finally (stop-server! s)))))))))


(deftest v1905-server-refuses-unsigned-or-foreign-signed-clients
  (with-dir
    (fn [dir]
      (let [root (pair)
            socket (.resolve ^Path dir "authenticated.sock")
            server (start-server! dir root socket true)]
        (try
          (let [good (:client server)
                unauthorized (register/prepare-unix-client
                              socket {:client-signing-private-pkcs8-base64
                                      (pkcs8 (pair))})
                invalid (fn [f] (reason f))
                floor-before (get-in ((:read-external-floor! good)
                                      (read-request)) [:statement :floor])]
            (is (= (genesis) floor-before))
            (is (= :external-register-refused-request
                   (invalid #((:read-external-floor! unauthorized)
                              (read-request)))))
            (is (= :external-register-refused-request
                   (invalid #((:advance-external-floor! unauthorized)
                              (advance-request (genesis) (f1))))))
            ;; Raw local clients can reach a socket with their UID, but an
            ;; unsigned old-style v1904 frame has no authority to READ/CAS.
            (with-open [raw (java.nio.channels.SocketChannel/open
                             java.net.StandardProtocolFamily/UNIX)]
              (.connect raw (java.net.UnixDomainSocketAddress/of socket))
              (let [out (java.io.DataOutputStream.
                         (java.nio.channels.Channels/newOutputStream raw))
                    in (java.io.DataInputStream.
                        (java.nio.channels.Channels/newInputStream raw))
                    bytes (.getBytes (str (pr-str (read-request)) "\n")
                                     java.nio.charset.StandardCharsets/UTF_8)]
                (.writeInt out (alength bytes))
                (.write out bytes)
                (.flush out)
                (let [len (.readInt in) answer (byte-array len)]
                  (is (<= 1 len 4096))
                  (.readFully in answer)
                  (is (= :unauthenticated-client-request
                         (:register-error (edn/read-string
                           (String. answer
                            java.nio.charset.StandardCharsets/UTF_8))))))))
            (is (= (genesis) (get-in ((:read-external-floor! good)
                                     (read-request)) [:statement :floor])))
            (is (= :invalid-client-configuration
                   (invalid #(register/prepare-unix-client
                              socket {:client-signing-private-pkcs8-base64
                                      (pkcs8 authorized-client-pair)
                                      :claim-independent true}))))
            (is (= :invalid-register-configuration
                   (invalid #(register/prepare-durable-register!
                              (dissoc (config dir root false)
                                      :authorized-client-spki-base64)))))
            (is (= :none (:authority (register/prepare-durable-register!
                                      (config dir root false)))))
            ;; A legitimate signer still advances after invalid attempts.
            (is (signed-envelope?
                 ((:advance-external-floor! good)
                  (advance-request (genesis) (f1))) root)))
          (finally (stop-server! server)))))))

(deftest v1905-connected-silent-client-cannot-block-register-forever
  (with-dir
    (fn [dir]
      (let [root (pair)
            socket (.resolve ^Path dir "bounded.sock")
            server (start-server! dir root socket true)]
        (try
          ;; This unauthenticated connection sends NO frame bytes. A
          ;; blocking read would permanently starve the single accept loop.
          (with-open [silent (java.nio.channels.SocketChannel/open
                             java.net.StandardProtocolFamily/UNIX)]
            (.connect silent (java.net.UnixDomainSocketAddress/of socket))
            (Thread/sleep 150)
            (let [started (System/nanoTime)]
              (is (= (genesis)
                     (get-in ((:read-external-floor! (:client server))
                              (read-request)) [:statement :floor])))
              (is (< (/ (- (System/nanoTime) started) 1000000.0)
                     4500.0)
                  "A silent connection is terminated by the bounded receive deadline")))
          ;; Reject an oversized, partially sent message at the HEADER.
          (with-open [raw (java.nio.channels.SocketChannel/open
                           java.net.StandardProtocolFamily/UNIX)]
            (.connect raw (java.net.UnixDomainSocketAddress/of socket))
            (let [out (java.io.DataOutputStream.
                       (java.nio.channels.Channels/newOutputStream raw))]
              (.writeInt out 4097)
              (.flush out)))
          (is (= (genesis)
                 (get-in ((:read-external-floor! (:client server))
                          (read-request)) [:statement :floor])))
          (finally (stop-server! server)))))))
