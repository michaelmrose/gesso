(ns gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store
  "v1902: Bounded, local-filesystem durable storage for v1901 signed policy
   checkpoint CANDIDATES. Every operation takes a cross-process file lock;
   successful append atomically replaces a complete signed chain after
   forcing the temporary file and then forces the directory. No non-atomic
   fallback is permitted. Reads reject malformed, truncated, tampered or
   noncontiguous chains rather than reinitializing a missing/corrupt store.

   A caller MUST provide an explicit rollback floor (revision+digest), and
   a separately configured Ed25519 public key. Both are CALLER-SUPPLIED,
   not an authenticated external authority. Removing/replacing the store
   and supplying an older floor and authority can roll it back. The file
   system, OS, process policy and signer are not independently authenticated.
   No crash test constitutes a physical guarantee for every filesystem.
   This is NOT durable cross-deployment revocation or a proof certificate."
  (:require [clojure.edn :as edn]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed])
  (:import [java.io PushbackReader StringReader]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path LinkOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermission]
           [java.security KeyFactory MessageDigest PublicKey Signature]
           [java.security.spec X509EncodedKeySpec]
           [java.util Arrays Base64]))

(def store-type
  :gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store/signed-chain)
(def status-type
  :gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store/status)
(def ^:private max-checkpoints 128)
;; Java NIO FileLock is inter-process, but throws OverlappingFileLockException
;; instead of queueing competing channels in one JVM. Serialize the local
;; path to that lock as well, before acquiring the OS-managed file lock.
(defonce ^:private same-jvm-lock-monitor (Object.))
(def ^:private max-file-bytes (* 16 1024 1024))
(def ^:private digest-pattern #"sha256:[0-9a-f]{64}")
(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private file-open-options (into-array java.nio.file.OpenOption
                                         [StandardOpenOption/READ]))
(def ^:private write-options (into-array java.nio.file.OpenOption
                                          [StandardOpenOption/WRITE
                                           StandardOpenOption/TRUNCATE_EXISTING]))
(def ^:private lock-options (into-array java.nio.file.OpenOption
                                         [StandardOpenOption/CREATE
                                          StandardOpenOption/WRITE]))

(defn- reject! [reason]
  (throw (ex-info "Invalid local signed policy checkpoint storage."
                  {:xtdb-durable-checkpoint/rejection true :reason reason})))

(defn- sha256-hex [^bytes x]
  (str "sha256:"
       (apply str (map #(format "%02x" (bit-and 0xff %))
                       (.digest (MessageDigest/getInstance "SHA-256") x)))))

(defn- valid-digest? [x]
  (and (string? x) (boolean (re-matches digest-pattern x))))

(defn- pinned-key [encoded]
  (when-not (and (string? encoded) (<= 1 (count encoded) 512))
    (reject! :invalid-configured-authority-key))
  (let [der (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-configured-authority-key)))
        ^PublicKey key (try (.generatePublic (KeyFactory/getInstance "Ed25519")
                                             (X509EncodedKeySpec. der))
                            (catch java.security.GeneralSecurityException _
                              (reject! :invalid-configured-authority-key)))]
    (when-not (and (<= 32 (alength der) 128)
                   (= encoded (.encodeToString (Base64/getEncoder) der))
                   (= "EdDSA" (.getAlgorithm key))
                   (Arrays/equals ^bytes der ^bytes (.getEncoded key)))
      (reject! :invalid-configured-authority-key))
    {:public-key key :fingerprint (sha256-hex der)}))

(defn- validate-floor [floor]
  (when-not (and (map? floor)
                 (= #{:revision :checkpoint-digest} (set (keys floor)))
                 (integer? (:revision floor))
                 (<= 0 (:revision floor) 1000000)
                 (if (zero? (:revision floor))
                   (= "genesis" (:checkpoint-digest floor))
                   (valid-digest? (:checkpoint-digest floor))))
    (reject! :invalid-caller-provided-rollback-floor))
  floor)

(defn- regular-file-or-missing! [^Path p]
  (when (Files/isSymbolicLink p)
    (reject! :symbolic-link-not-accepted))
  (when (and (Files/exists p no-links)
             (not (Files/isRegularFile p no-links)))
    (reject! :not-a-regular-checkpoint-file)))

(defn- signed-entry! [signed-entry ^PublicKey key]
  (when-not (and (map? signed-entry)
                 (= #{:checkpoint :signature-base64} (set (keys signed-entry)))
                 (string? (:signature-base64 signed-entry))
                 (<= 1 (count (:signature-base64 signed-entry)) 128))
    (reject! :invalid-persisted-signed-checkpoint))
  (let [bytes (signed/checkpoint-signing-bytes (:checkpoint signed-entry))
        encoded (:signature-base64 signed-entry)
        sig (try (.decode (Base64/getDecoder) ^String encoded)
                 (catch IllegalArgumentException _
                   (reject! :invalid-persisted-signature)))
        verified (and (= 64 (alength sig))
                      (= encoded (.encodeToString (Base64/getEncoder) sig))
                      (try (let [v (Signature/getInstance "Ed25519")]
                             (.initVerify v key)
                             (.update v ^bytes bytes)
                             (.verify v ^bytes sig))
                           (catch java.security.GeneralSecurityException _ false)))]
    (when-not verified
      (reject! :persisted-signature-not-verified))
    {:digest (sha256-hex bytes)
     :snapshot (get-in signed-entry [:checkpoint :snapshot])}))

(defn- extension! [before after]
  (when before
    (when-not (and (= (:policy-id before) (:policy-id after))
                   (<= (:root-generation before) (:root-generation after)
                       (inc (:root-generation before)))
                   (<= (:delegation-generation before)
                       (:delegation-generation after)
                       (inc (:delegation-generation before)))
                   (every? (set (:seen-root-fingerprints after))
                           (:seen-root-fingerprints before))
                   (every? (set (:retired-delegate-fingerprints after))
                           (:retired-delegate-fingerprints before))
                   (every? (set (:retired-delegation-ids after))
                           (:retired-delegation-ids before))
                   (or (not= (:root-generation before) (:root-generation after))
                       (= (:root-fingerprint before) (:root-fingerprint after)))
                   (or (not= (:delegation-generation before)
                             (:delegation-generation after))
                       (= (:delegate-fingerprint before)
                          (:delegate-fingerprint after))))
      (reject! :nonmonotonic-persisted-retirement))))

(defn- verify-history! [records ^PublicKey key floor]
  (when-not (and (vector? records) (<= (count records) max-checkpoints))
    (reject! :invalid-checkpoint-history-shape))
  (loop [remaining records
         previous "genesis"
         revision 1
         last-snapshot nil
         floor-present? (zero? (:revision floor))]
    (if-let [entry (first remaining)]
      (let [{:keys [digest snapshot]} (signed-entry! entry key)
            actual-revision (:policy-revision snapshot)]
        (when-not (and (= previous (get-in entry [:checkpoint :previous-digest]))
                       (= revision actual-revision))
          (reject! :persisted-chain-gap-or-rollback))
        (extension! last-snapshot snapshot)
        (recur (next remaining) digest (inc revision) snapshot
               (or floor-present?
                   (and (= revision (:revision floor))
                        (= digest (:checkpoint-digest floor))))))
      (do
        (when-not floor-present?
          (reject! :external-rollback-floor-not-in-persisted-history))
        {:revision (dec revision)
         :digest previous
         :snapshot last-snapshot
         :entry-count (dec revision)}))))

(defn- load-history! [^Path history-file ^PublicKey key floor]
  (regular-file-or-missing! history-file)
  (if-not (Files/exists history-file no-links)
    (do (when (pos? (:revision floor))
          (reject! :expected-checkpoint-history-missing))
        {:records [] :head (verify-history! [] key floor)})
    (let [size (Files/size history-file)]
      (when-not (<= 1 size max-file-bytes)
        (reject! :invalid-persisted-history-size))
      (let [raw (Files/readAllBytes history-file)
            document (try
                       (with-open [reader (PushbackReader.
                                          (StringReader.
                                           (String. ^bytes raw StandardCharsets/UTF_8)))]
                         (let [read-options {:eof ::end-of-file
                                             :readers {}
                                             :default (fn [_ _]
                                                        (reject! :unrecognized-edn-tag))}
                               one (edn/read read-options reader)
                               trailing (edn/read read-options reader)]
                           (when (or (= ::end-of-file one)
                                     (not= ::end-of-file trailing))
                             (reject! :extra-or-missing-checkpoint-form))
                           one))
                       (catch Throwable _
                         (reject! :malformed-persisted-history-file)))]
        (when-not (and (map? document)
                       (= #{:type :version :entries} (set (keys document)))
                       (= store-type (:type document))
                       (= 1 (:version document)))
          (reject! :invalid-persisted-history-envelope))
        {:records (:entries document)
         :head (verify-history! (:entries document) key floor)}))))

(defn- bytes-for [records]
  (let [serialized (pr-str (sorted-map :type store-type
                                       :version 1
                                       :entries records))
        encoded (.getBytes (str serialized "\n") StandardCharsets/UTF_8)]
    (when (> (alength encoded) max-file-bytes)
      (reject! :persisted-history-capacity-exceeded))
    encoded))

(defn- force-directory! [^Path dir]
  (with-open [channel (FileChannel/open dir file-open-options)]
    (.force channel true)))

(defn- atomically-write! [^Path dir ^Path target records]
  (let [tmp (Files/createTempFile dir ".checkpoint-staging-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (Files/setPosixFilePermissions
       tmp (java.util.EnumSet/of PosixFilePermission/OWNER_READ
                                      PosixFilePermission/OWNER_WRITE))
      (with-open [channel (FileChannel/open tmp write-options)]
        (let [bytes (bytes-for records)
              buffer (ByteBuffer/wrap bytes)]
          (while (.hasRemaining buffer)
            (.write channel buffer))
          (.force channel true)))
      (regular-file-or-missing! target)
      ;; Fail CLOSED if filesystem does not implement atomic replacement.
      (Files/move tmp target
                  (into-array java.nio.file.CopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (force-directory! dir)
      (finally
        (Files/deleteIfExists tmp)))))

(defn prepare-local-durable-store!
  "Prepare the bounded, cross-process locked local store.

   Config is EXACTLY {:directory <existing non-symlink Path>
    :policy <v1900 policy with :checkpoint-material!>
    :authority-ed25519-spki-base64 <canonical Ed25519 SPKI>
    :external-rollback-floor {:revision N :checkpoint-digest DIGEST}
    :initialize-empty? <boolean>}.

   No automatic genesis/recovery: when the store file is absent, explicit
   initialization and a genesis floor are both required. Existing signed
   records are verified on every :status! and :append!; the full chain must
   include the supplied floor's exact digest at its revision. The floor and
   authority are caller-configured, NOT independently authenticated.

   :append! [strict signed-envelope] verifies a live local snapshot and
   signature then replaces the entire bounded chain atomically. A successful
   return means file and directory force calls completed; no attempt to
   certify durability under a hostile OS, storage controller or filesystem.
   A failed/uncertain append must be reconciled by :status! before retry."
  [config]
  (when-not (and (map? config)
                 (= #{:directory :policy :authority-ed25519-spki-base64
                      :external-rollback-floor :initialize-empty?}
                    (set (keys config)))
                 (instance? Path (:directory config))
                 (boolean? (:initialize-empty? config))
                 (map? (:policy config))
                 (fn? (get-in config [:policy :checkpoint-material!])))
    (reject! :invalid-durable-store-configuration))
  (let [^Path dir (.toAbsolutePath ^Path (:directory config))
        floor (validate-floor (:external-rollback-floor config))
        {:keys [public-key fingerprint]}
        (pinned-key (:authority-ed25519-spki-base64 config))
        history-file (.resolve dir "policy-checkpoints.edn")
        lock-file (.resolve dir "policy-checkpoints.lock")]
    (when-not (and (Files/exists dir no-links)
                   (Files/isDirectory dir no-links)
                   (not (Files/isSymbolicLink dir)))
      (reject! :checkpoint-directory-unavailable))
    (regular-file-or-missing! lock-file)
    (let [with-lock (fn [f]
                      (locking same-jvm-lock-monitor
                        (with-open [channel (FileChannel/open lock-file lock-options)
                                    lock (.lock channel)]
                          (f))))
          read! (fn [] (load-history! history-file public-key floor))
          _ (with-lock
              #(do (when (and (not (Files/exists history-file no-links))
                              (or (not (:initialize-empty? config))
                                  (pos? (:revision floor))))
                     (reject! :explicit-genesis-initialization-required))
                   (read!)))
          status! (fn []
                    (with-lock
                      (fn []
                        (let [{:keys [head]} (read!)]
                          {:type status-type
                           :version 1
                           :head-revision (:revision head)
                           :head-digest (:digest head)
                           :signed-checkpoint-count (:entry-count head)
                           :rollback-floor-revision (:revision floor)
                           :rollback-floor-source :caller-supplied-not-authenticated
                           :configured-signing-key-fingerprint fingerprint
                           :local-file-consistency :checked-from-signed-chain
                           :successful-append-durability :file-and-directory-forced
                           :cross-process-and-hostile-filesystem-trust :not-established
                           :proof-discharge :none
                           :certificate :not-issued
                           :authority :none}))))
          append! (fn [signed-entry]
                    (with-lock
                      (fn []
                        (let [{:keys [records head]} (read!)
                              snapshot (get-in signed-entry [:checkpoint :snapshot])
                              live! (get-in config [:policy :checkpoint-material!])]
                          (when (>= (count records) max-checkpoints)
                            (reject! :bounded-history-full))
                          (when-not (and (map? signed-entry)
                                         (= (:digest head)
                                            (get-in signed-entry [:checkpoint :previous-digest]))
                                         (= (inc (:revision head))
                                            (:policy-revision snapshot)))
                            (reject! :checkpoint-rollback-gap-or-wrong-anchor))
                          (when-not (= snapshot (live!))
                            (reject! :checkpoint-does-not-match-current-local-policy))
                          (signed-entry! signed-entry public-key)
                          (when-not (= snapshot (live!))
                            (reject! :checkpoint-local-policy-changed-during-check))
                          (extension! (:snapshot head) snapshot)
                          (let [new-records (conj records signed-entry)
                                new-head (verify-history! new-records public-key floor)]
                            (atomically-write! dir history-file new-records)
                            {:type status-type
                             :version 1
                             :head-revision (:revision new-head)
                             :head-digest (:digest new-head)
                             :signed-checkpoint-count (:entry-count new-head)
                             :rollback-floor-revision (:revision floor)
                             :rollback-floor-source :caller-supplied-not-authenticated
                             :local-file-consistency :atomically-replaced-and-forced
                             :successful-append-durability :file-and-directory-forced
                             :proof-discharge :none
                             :certificate :not-issued
                             :authority :none})))))]
      {:type :gesso.assurance.proof.xtdb-gateway-durable-checkpoint-store/store
       :version 1
       :status! status!
       :append! append!
       :bounded-checkpoint-cap max-checkpoints
       :rollback-floor-source :caller-supplied-not-authenticated
       :signing-key-provisioning :process-configured-not-authenticated
       :durable-cross-process-revocation :not-established
       :trusted-storage :not-established
       :proof-discharge :none
       :certificate :not-issued
       :authority :none})))
