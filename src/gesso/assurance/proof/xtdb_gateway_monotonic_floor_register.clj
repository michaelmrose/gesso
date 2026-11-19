(ns gesso.assurance.proof.xtdb-gateway-monotonic-floor-register
  "v1904: A SEPARATE Unix-domain socket process for the v1903 signed floor
   protocol. The server serializes compare-and-advance under a file lock,
   forces an atomic replacement of the monotonic floor, and signs read and
   accepted-CAS responses. An explicit genesis flag is required for first
   initialization; missing or corrupt non-genesis state is never repaired.

   SECURITY LIMIT: filesystem control, the configured Ed25519 signing key,
   socket directory and same-UID access are not independently authenticated.
   This is a process and persistence boundary, not a separately administered
   trust authority or a rollback-proof hardware monotonic register. Operators
   must guard the directory, socket and private key. No remote-network API,
   proof discharge, witness admission or certificate is provided.

   v1905: Authenticate EACH Unix client request with an Ed25519 key pinned
   from register-process configuration. No unauthenticated socket request
   can reach read or compare-and-advance; the private caller signing key is
   never sent over the socket. A fixed total receive deadline also prevents
   one connected but silent client from indefinitely blocking this serial
   service. Client-key provisioning, UID compromise, replay after a register
   storage rollback and service independence are NOT established here."
  (:require [clojure.edn :as edn]
            [gesso.assurance.proof.xtdb-gateway-externally-anchored-floor :as floor])
  (:import [java.io ByteArrayOutputStream DataOutputStream DataInputStream
            PushbackReader StringReader]
           [java.nio ByteBuffer]
           [java.nio.channels Channels FileChannel ServerSocketChannel SocketChannel
            Selector SelectionKey]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path LinkOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermission]
           [java.security KeyFactory PrivateKey PublicKey Signature MessageDigest]
           [java.security.spec PKCS8EncodedKeySpec X509EncodedKeySpec]
           [java.util Base64 UUID Arrays]))

(def register-type
  :gesso.assurance.proof.xtdb-gateway-monotonic-floor-register/persisted-floor)
(def ^:private max-payload 4096)
(def ^:private request-auth-type
  :gesso.assurance.proof.xtdb-gateway-monotonic-floor-register/signed-client-request)
(def ^:private client-request-domain "gesso/xtdb/monotonic-register/client-request/v1")
(def ^:private server-receive-timeout-ms 1500)
(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private read-options (into-array java.nio.file.OpenOption
                                         [StandardOpenOption/READ]))
(def ^:private write-options (into-array java.nio.file.OpenOption
                                          [StandardOpenOption/WRITE]))
(def ^:private lock-options (into-array java.nio.file.OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/WRITE
                                          LinkOption/NOFOLLOW_LINKS]))
(defonce ^:private same-jvm-lock (Object.))
(def ^:private zero-floor {:revision 0 :checkpoint-digest "genesis"})
(def ^:private snapshot-nonce "00000000-0000-0000-0000-000000000000")

(defn- reject! [reason]
  (throw (ex-info "Invalid monotonic rollback-floor register."
                  {:xtdb-floor-register/rejection true :reason reason})))

(defn- safe-file! [^Path p]
  (when (or (Files/isSymbolicLink p)
            (and (Files/exists p no-links)
                 (not (Files/isRegularFile p no-links))))
    (reject! :non-regular-or-symlink-register-file)))

(defn- private-key! [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-configured-signing-key))
  (try
    (let [der (.decode (Base64/getDecoder) ^String encoded)]
      (when-not (= encoded (.encodeToString (Base64/getEncoder) der))
        (reject! :invalid-configured-signing-key))
      (.generatePrivate (KeyFactory/getInstance "Ed25519")
                        (PKCS8EncodedKeySpec. der)))
    (catch java.security.GeneralSecurityException _
      (reject! :invalid-configured-signing-key))
    (catch IllegalArgumentException _
      (reject! :invalid-configured-signing-key))))

(defn- public-key! [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-configured-public-key))
  (try
    (let [der (.decode (Base64/getDecoder) ^String encoded)
          key (.generatePublic (KeyFactory/getInstance "Ed25519")
                               (X509EncodedKeySpec. der))]
      (when-not (and (= encoded (.encodeToString (Base64/getEncoder) der))
                     (Arrays/equals der (.getEncoded ^PublicKey key)))
        (reject! :invalid-configured-public-key))
      key)
    (catch java.security.GeneralSecurityException _
      (reject! :invalid-configured-public-key))
    (catch IllegalArgumentException _
      (reject! :invalid-configured-public-key))))

(defn- signature [^bytes data ^PrivateKey key]
  (let [signer (Signature/getInstance "Ed25519")]
    (.initSign signer key)
    (.update signer data)
    (.encodeToString (Base64/getEncoder) (.sign signer))))

(defn- request-signing-bytes [request]
  ;; Canonical wire request, separate from the signed floor response and from
  ;; the payload-carrying EDN. For reads, the floor field is deliberately the
  ;; fixed genesis value; the only variable request field is its UUID nonce.
  (let [statement
        (case (:kind request)
          :read (do
                  (when-not (and (map? request)
                                 (= #{:kind :nonce} (set (keys request))))
                    (reject! :invalid-authenticated-request))
                  {:type floor/statement-type :version 1 :kind :read
                   :nonce (:nonce request) :floor zero-floor})
          :advance request
          (reject! :invalid-authenticated-request))
        signed-data (floor/floor-signing-bytes statement)
        out (ByteArrayOutputStream.)]
    (with-open [writer (DataOutputStream. out)]
      (.writeUTF writer client-request-domain)
      (.writeInt writer (alength ^bytes signed-data))
      (.write writer ^bytes signed-data))
    (.toByteArray out)))

(defn- authenticated-request [request ^PrivateKey client-private-key]
  {:type request-auth-type
   :version 1
   :request request
   :signature-base64 (signature (request-signing-bytes request)
                                client-private-key)})

(defn- require-authenticated-request!
  [envelope ^PublicKey authorized-client-key]
  (when-not (and (map? envelope)
                 (= #{:type :version :request :signature-base64}
                    (set (keys envelope)))
                 (= request-auth-type (:type envelope))
                 (= 1 (:version envelope))
                 (string? (:signature-base64 envelope))
                 (<= 1 (count (:signature-base64 envelope)) 128))
    (reject! :unauthenticated-client-request))
  (let [bytes (request-signing-bytes (:request envelope))
        encoded (:signature-base64 envelope)
        raw (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _ nil))]
    (when-not (and raw (= 64 (alength ^bytes raw))
                   (= encoded (.encodeToString (Base64/getEncoder) raw))
                   (try
                     (let [verifier (Signature/getInstance "Ed25519")]
                       (.initVerify verifier authorized-client-key)
                       (.update verifier ^bytes bytes)
                       (.verify verifier ^bytes raw))
                     (catch java.security.GeneralSecurityException _ false)))
      (reject! :client-request-signature-not-verified)))
  (:request envelope))

(defn- signed! [statement key]
  {:statement statement
   :signature-base64 (signature (floor/floor-signing-bytes statement) key)})

(defn- verify-envelope! [envelope ^PublicKey key]
  (when-not (and (map? envelope)
                 (= #{:statement :signature-base64} (set (keys envelope)))
                 (string? (:signature-base64 envelope)))
    (reject! :invalid-register-record))
  (let [bytes (floor/floor-signing-bytes (:statement envelope))
        value (:signature-base64 envelope)
        decoded (try (.decode (Base64/getDecoder) ^String value)
                     (catch IllegalArgumentException _ nil))]
    (when-not (and decoded (= 64 (alength ^bytes decoded))
                   (= value (.encodeToString (Base64/getEncoder) decoded))
                   (let [v (Signature/getInstance "Ed25519")]
                     (.initVerify v key)
                     (.update v ^bytes bytes)
                     (.verify v ^bytes decoded)))
      (reject! :persisted-floor-signature-not-verified)))
  (:statement envelope))

(defn- snapshot-statement [floor-value]
  {:type floor/statement-type :version 1 :kind :read
   :nonce snapshot-nonce :floor floor-value})

(defn- strict-edn! [^bytes data]
  (when-not (<= 1 (alength data) max-payload)
    (reject! :register-message-size-out-of-bounds))
  (try
    (with-open [reader (PushbackReader.
                       (StringReader. (String. data StandardCharsets/UTF_8)))]
      (let [options {:eof ::end :readers {}
                     :default (fn [_ _] (reject! :unrecognized-register-tag))}
            value (edn/read options reader)
            second-form (edn/read options reader)]
        (when (or (= ::end value) (not= ::end second-form))
          (reject! :extra-or-missing-register-form))
        value))
    (catch clojure.lang.ExceptionInfo ex (throw ex))
    (catch Throwable _ (reject! :malformed-register-message))))

(defn- bytes-for [value]
  (let [bytes (.getBytes (str (pr-str value) "\n") StandardCharsets/UTF_8)]
    (when-not (<= 1 (alength bytes) max-payload)
      (reject! :register-message-size-out-of-bounds))
    bytes))

(defn- load-floor! [^Path target public-key]
  (safe-file! target)
  (if-not (Files/exists target no-links)
    nil
    (let [size (Files/size target)]
      (when-not (<= 1 size max-payload)
        (reject! :invalid-register-file-size))
      (let [record (strict-edn! (Files/readAllBytes target))]
        (when-not (and (map? record)
                       (= #{:type :version :signed-state} (set (keys record)))
                       (= register-type (:type record))
                       (= 1 (:version record)))
          (reject! :invalid-register-file-envelope))
        (let [statement (verify-envelope! (:signed-state record) public-key)]
          (when-not (and (= :read (:kind statement))
                         (= snapshot-nonce (:nonce statement)))
            (reject! :invalid-register-snapshot))
          (:floor statement))))))

(defn- force-parent! [^Path directory]
  (with-open [channel (FileChannel/open directory read-options)]
    (.force channel true)))

(defn- write-floor! [^Path dir ^Path target floor-value key]
  (let [record {:type register-type :version 1
                :signed-state (signed! (snapshot-statement floor-value) key)}
        bytes (bytes-for record)
        tmp (Files/createTempFile dir ".floor-staging-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/setPosixFilePermissions tmp
                                     (java.util.EnumSet/of
                                      PosixFilePermission/OWNER_READ
                                      PosixFilePermission/OWNER_WRITE))
      (with-open [channel (FileChannel/open tmp write-options)]
        (let [buffer (ByteBuffer/wrap bytes)]
          (while (.hasRemaining buffer)
            (.write channel buffer))
          (.force channel true)))
      (safe-file! target)
      (Files/move tmp target
                  (into-array java.nio.file.CopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (force-parent! dir)
      (finally
        (Files/deleteIfExists tmp)))))

(defn prepare-durable-register!
  "Create a durable, signed, monotonic register with strict CAS.
   Exact config {:directory Path :signing-private-pkcs8-base64 str
    :signing-public-spki-base64 str :authorized-client-spki-base64 str
    :initialize-empty? bool}.
   All reads and CAS acquire an OS file lock. No automatic repair or
   initialization when the persisted state is missing or corrupt.
   Neither configured key ownership nor external administrative control
   is established here."
  [config]
  (when-not (and (map? config)
                 (= #{:directory :signing-private-pkcs8-base64
                      :signing-public-spki-base64 :authorized-client-spki-base64
                      :initialize-empty?}
                    (set (keys config)))
                 (instance? Path (:directory config))
                 (boolean? (:initialize-empty? config)))
    (reject! :invalid-register-configuration))
  (let [^Path dir (.toAbsolutePath ^Path (:directory config))
        _ (when-not (and (Files/exists dir no-links)
                         (Files/isDirectory dir no-links)
                         (not (Files/isSymbolicLink dir)))
            (reject! :register-directory-unavailable))
        ^PrivateKey private-key (private-key! (:signing-private-pkcs8-base64 config))
        ^PublicKey public-key (public-key! (:signing-public-spki-base64 config))
        ^PublicKey authorized-client-key
        (public-key! (:authorized-client-spki-base64 config))
        _ (let [bytes (.getBytes "gesso-monotonic-register-self-test"
                                 StandardCharsets/UTF_8)
                sig (.decode (Base64/getDecoder) (signature bytes private-key))
                v (Signature/getInstance "Ed25519")]
            (.initVerify v public-key)
            (.update v bytes)
            (when-not (.verify v sig)
              (reject! :configured-signing-keypair-mismatch)))
        target (.resolve dir "external-rollback-floor.edn")
        lock-file (.resolve dir "external-rollback-floor.lock")
        _ (safe-file! lock-file)
        with-lock (fn [f]
                    (locking same-jvm-lock
                      (with-open [channel (FileChannel/open lock-file lock-options)
                                  lock (.lock channel)]
                        (f))))
        _ (with-lock
            #(let [existing (load-floor! target public-key)]
               (when-not existing
                 (when-not (:initialize-empty? config)
                   (reject! :explicit-register-genesis-required))
                 (write-floor! dir target zero-floor private-key))))
        read! (fn [request]
                (when-not (and (map? request)
                               (= #{:kind :nonce} (set (keys request)))
                               (= :read (:kind request)))
                  (reject! :invalid-read-request))
                (let [statement {:type floor/statement-type :version 1
                                 :kind :read :nonce (:nonce request)
                                 :floor zero-floor}]
                  ;; Have floor-signing-bytes reject missing/invalid nonces.
                  (floor/floor-signing-bytes statement))
                (with-lock
                  (fn []
                    (let [current (or (load-floor! target public-key)
                                      (reject! :register-floor-file-missing))]
                      (signed! {:type floor/statement-type :version 1
                                :kind :read :nonce (:nonce request)
                                :floor current} private-key)))))
        advance! (fn [request]
                   (when-not (and (map? request)
                                  (= :advance (:kind request)))
                     (reject! :invalid-advance-request))
                   (floor/floor-signing-bytes request)
                   (with-lock
                     (fn []
                       (let [current (or (load-floor! target public-key)
                                         (reject! :register-floor-file-missing))]
                         (when-not (= current (:expected-floor request))
                           (reject! :stale-compare-and-advance))
                         ;; A valid one-step request was checked by the
                         ;; canonical statement serializer above.
                         (write-floor! dir target (:next-floor request) private-key)
                         (signed! request private-key)))))]
    {:type register-type :read! read! :advance! advance!
     :require-authenticated-request!
     (fn [envelope]
       (require-authenticated-request! envelope authorized-client-key))
     :directory dir
     :client-authentication :ed25519-process-configured-key-only
     :client-key-provisioning-independence :not-established
     :config-trust :process-configured-not-authenticated
     :persistence :atomic-file-and-directory-forced
     :independent-rollback-prevention :not-established
     :authority :none :certificate :not-issued :proof-discharge :none}))

(defn- send-frame! [out data]
  (let [bytes (bytes-for data)]
    (.writeInt ^DataOutputStream out (alength bytes))
    (.write ^DataOutputStream out bytes)
    (.flush ^DataOutputStream out)))

(defn- read-frame! [in]
  (let [size (.readInt ^DataInputStream in)]
    (when-not (<= 1 size max-payload)
      (reject! :register-frame-size-out-of-bounds))
    (let [bytes (byte-array size)]
      (.readFully ^DataInputStream in bytes)
      (strict-edn! bytes))))

(defn- read-client-frame-before-deadline!
  "Read exactly one framed message from a client with a SINGLE wall-clock
   deadline covering both the header and body. Incomplete bytes, slow
   trickles and oversize frames never block the serial accept loop forever."
  [^SocketChannel client]
  (let [deadline (+ (System/nanoTime)
                    (* 1000000 server-receive-timeout-ms))]
    (.configureBlocking client false)
    (try
      (with-open [selector (Selector/open)]
        (.register client selector SelectionKey/OP_READ)
        (letfn [(fill! [^ByteBuffer buffer]
                  (while (.hasRemaining buffer)
                    ;; Enforce a total deadline even when bytes trickle in.
                    (when (<= (- deadline (System/nanoTime)) 0)
                      (reject! :register-client-frame-deadline-exceeded))
                    (let [n (.read client buffer)]
                      (cond
                        (neg? n) (reject! :client-closed-before-frame-complete)
                        (pos? n) nil
                        :else
                        (let [remaining (- deadline (System/nanoTime))]
                          (when (<= remaining 0)
                            (reject! :register-client-frame-deadline-exceeded))
                          (.select selector
                                   (long (max 1 (quot (+ remaining 999999)
                                                      1000000))))
                          (.clear (.selectedKeys selector)))))))]
          (let [header (ByteBuffer/allocate 4)]
            (fill! header)
            (.flip header)
            (let [size (.getInt header)]
              (when-not (<= 1 size max-payload)
                (reject! :register-frame-size-out-of-bounds))
              (let [body (ByteBuffer/allocate size)]
                (fill! body)
                (strict-edn! (.array body)))))))
      (finally
        ;; The selector is closed before switching back to blocking mode.
        ;; The caller only writes its bounded response after this point.
        (.configureBlocking client true)))))

(defn prepare-unix-client
  "Provide the exact v1903 callback keys over a Unix socket. Each request
   is signed with the independently configured CLIENT private key before
   sending. The private key is NOT transmitted. A caller controlling both
   the server's authorized public key and this private key can authenticate
   itself: no deployment independence or trusted key custody is implied.
   The callbacks do NOT authenticate the socket pathname or process; v1903
   verifies all response signatures and fresh read nonces separately."
  [^Path socket config]
  (when-not (instance? Path socket)
    (reject! :invalid-socket-path))
  (when-not (and (map? config)
                 (= #{:client-signing-private-pkcs8-base64} (set (keys config))))
    (reject! :invalid-client-configuration))
  (let [^PrivateKey client-key (private-key!
                                (:client-signing-private-pkcs8-base64 config))
        request! (fn [message]
                   (let [envelope (authenticated-request message client-key)]
                     (with-open [client (SocketChannel/open StandardProtocolFamily/UNIX)]
                       (.connect client (UnixDomainSocketAddress/of socket))
                       (let [out (DataOutputStream. (Channels/newOutputStream client))
                             in (DataInputStream. (Channels/newInputStream client))]
                         (send-frame! out envelope)
                         (let [answer (read-frame! in)]
                           (when (and (map? answer)
                                      (contains? answer :register-error))
                             (reject! :external-register-refused-request))
                           answer)))))]
    {:read-external-floor! request!
     :advance-external-floor! request!
     :client-authentication :ed25519-process-configured-key-only
     :transport :local-unix-socket-not-an-independent-authority}))

(defn serve-unix!
  "BLOCKING server. Bind a new Unix socket inside the protected directory
   and answer framed EDN requests. A socket path left by a crashed process
   is never silently deleted: operator must verify/repair and remove it.
   For use in its own JVM via -main; no network TCP port is opened."
  [register ^Path socket]
  (when-not (and (instance? Path socket)
                 (map? register) (fn? (:read! register))
                 (fn? (:advance! register))
                 (fn? (:require-authenticated-request! register))
                 (= (.normalize (.toAbsolutePath (.getParent socket)))
                    (.normalize ^Path (:directory register)))
                 (not (Files/exists socket no-links))
                 (not (Files/isSymbolicLink socket)))
    (reject! :invalid-or-occupied-socket-path))
  (with-open [server (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
    (.bind server (UnixDomainSocketAddress/of socket))
    (Files/setPosixFilePermissions socket
                                   (java.util.EnumSet/of
                                    PosixFilePermission/OWNER_READ
                                    PosixFilePermission/OWNER_WRITE))
    (try
      (loop []
        (with-open [client (.accept server)]
          (try
            (let [out (DataOutputStream. (Channels/newOutputStream client))
                  envelope (read-client-frame-before-deadline! client)
                  request ((:require-authenticated-request! register) envelope)
                  result (case (:kind request)
                           :read ((:read! register) request)
                           :advance ((:advance! register) request)
                           (reject! :unknown-register-request))]
              (send-frame! out result))
            (catch Throwable ex
              (try (send-frame! (DataOutputStream. (Channels/newOutputStream client))
                                {:register-error (or (:reason (ex-data ex))
                                                     :register-request-failed)})
                   (catch Throwable _)))))
        (recur))
      (finally
        (Files/deleteIfExists socket)))))

(defn -main
  "Run a dedicated register process. Usage:
     -m ...xtdb-gateway-monotonic-floor-register DIR SOCKET init|existing
   Environment must set GESSO_FLOOR_PKCS8 and GESSO_FLOOR_SPKI
   (base64 DER) plus GESSO_FLOOR_CLIENT_SPKI (authorized CLIENT public key).
   Both are intentionally not passed as argv. Process/network independence,
   secure provisioning and storage administrator are deployment concerns."
  [& argv]
  (when-not (= 3 (count argv))
    (reject! :invalid-register-process-arguments))
  (let [[directory socket init] argv]
    (when-not (contains? #{"init" "existing"} init)
      (reject! :invalid-register-process-arguments))
    (serve-unix!
     (prepare-durable-register!
      {:directory (Path/of directory (make-array String 0))
       :signing-private-pkcs8-base64 (System/getenv "GESSO_FLOOR_PKCS8")
       :signing-public-spki-base64 (System/getenv "GESSO_FLOOR_SPKI")
       :authorized-client-spki-base64 (System/getenv "GESSO_FLOOR_CLIENT_SPKI")
       :initialize-empty? (= "init" init)})
     (Path/of socket (make-array String 0)))))
