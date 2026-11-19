(ns gesso.assurance.proof.xtdb-gateway-recovery-audit-store
  "v1910: Durable, bounded LOCAL audit journal for v1909 quarantined recovery
   assessments. Record creation invokes the original adapter's READ-ONLY
   inspection, retains a strict redacted diagnostic, and appends a deterministic
   SHA-256 linked entry. Every operation rechecks the entire journal under a
   filesystem lock. No entry is a cryptographic signature, independent audit
   producer, proof, certificate, durable cross-node record or permission to
   recover. The audit rollback floor and adapter are CALLER-SUPPLIED; an actor
   controlling them and the filesystem can rewrite valid hash chains.

   Audit writes are separate from the original local checkpoint and external
   register. The journal never calls either :append! or the register's CAS;
   failure to record an assessment cannot undo or confirm the original
   ambiguous advance. On an uncertain file write, there is NO retry here."
  (:require [clojure.edn :as edn])
  (:import [java.io PushbackReader StringReader]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path LinkOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermission]
           [java.security MessageDigest]
           [java.util UUID]))

(def journal-type
  :gesso.assurance.proof.xtdb-gateway-recovery-audit-store/local-audit-journal)
(def entry-type
  :gesso.assurance.proof.xtdb-gateway-recovery-audit-store/redacted-assessment)
(def ^:private recovery-type
  :gesso.assurance.proof.xtdb-gateway-externally-anchored-floor/recovery-assessment)
(def ^:private digest-pattern #"sha256:[0-9a-f]{64}")
(def ^:private max-entries 128)
(def ^:private max-file-bytes (* 1024 1024))
(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private read-options (into-array java.nio.file.OpenOption [StandardOpenOption/READ]))
(def ^:private write-options (into-array java.nio.file.OpenOption
                                 [StandardOpenOption/WRITE StandardOpenOption/TRUNCATE_EXISTING]))
(def ^:private lock-options (into-array java.nio.file.OpenOption
                                [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
(defonce ^:private local-lock (Object.))

(defn- reject! [reason]
  (throw (ex-info "Invalid local recovery assessment journal."
                  {:xtdb-recovery-audit/rejection true :reason reason})))

(defn- digest [^bytes b]
  (str "sha256:"
       (apply str (map #(format "%02x" (bit-and 255 %))
                       (.digest (MessageDigest/getInstance "SHA-256") b)))))

(defn- canonical-uuid? [x]
  (and (string? x) (<= 1 (count x) 64)
       (try (= x (str (UUID/fromString x)))
            (catch IllegalArgumentException _ false))))

(defn- valid-floor? [floor]
  (and (map? floor)
       (= #{:sequence :digest} (set (keys floor)))
       (integer? (:sequence floor)) (<= 0 (:sequence floor) max-entries)
       (if (zero? (:sequence floor))
         (= "genesis" (:digest floor))
         (and (string? (:digest floor))
              (boolean (re-matches digest-pattern (:digest floor)))))))

(def ^:private assessment-keys
  #{:type :version :attempt-id :original-advance-classification
    :original-local-checkpoint :original-prior-revision
    :original-committed-revision :observation :reconciliation
    :signed-register-exact-head-match? :quarantine :next-step
    :automatic-cas-retry? :write-or-repair-performed?
    :persistent-audit-record :remote-execution-causality
    :external-register-independence :proof-discharge :certificate :authority})

(def ^:private classifications
  #{:client-reported-verified-stale-cas :unconfirmed-outcome})
(def ^:private reconciliations
  #{:reconciliation-observation-unavailable
    :local-history-diverges-from-committed-attempt
    :fresh-signed-head-matches-committed-local-head
    :fresh-signed-register-still-at-prior-head
    :fresh-signed-register-head-diverges})

(defn- valid-assessment? [x]
  (and (map? x) (= assessment-keys (set (keys x)))
       (= recovery-type (:type x)) (= 1 (:version x))
       (canonical-uuid? (:attempt-id x))
       (contains? classifications (:original-advance-classification x))
       (= :already-committed (:original-local-checkpoint x))
       (integer? (:original-prior-revision x))
       (<= 0 (:original-prior-revision x) 1000000)
       (= (inc (:original-prior-revision x))
          (:original-committed-revision x))
       (contains? reconciliations (:reconciliation x))
       (contains? #{:fresh-signed-read-and-local-signed-chain
                    :unable-to-observe-both-heads}
                  (:observation x))
       (boolean? (:signed-register-exact-head-match? x))
       (if (= :unable-to-observe-both-heads (:observation x))
         (and (= :reconciliation-observation-unavailable (:reconciliation x))
              (false? (:signed-register-exact-head-match? x)))
         (and (not= :reconciliation-observation-unavailable (:reconciliation x))
              (= (= :fresh-signed-head-matches-committed-local-head
                    (:reconciliation x))
                 (:signed-register-exact-head-match? x))))
       (= :remains-permanent (:quarantine x))
       (= :new-adapter-required-with-fresh-signed-exact-head (:next-step x))
       (false? (:automatic-cas-retry? x))
       (false? (:write-or-repair-performed? x))
       (= :not-written (:persistent-audit-record x))
       (= :not-established (:remote-execution-causality x)
          (:external-register-independence x))
       (= :none (:proof-discharge x) (:authority x))
       (= :not-issued (:certificate x))))

(defn- as-entry [sequence previous assessment]
  ;; sorted-map fixes the representation before hashing. The digest is
  ;; neither a signature nor proof that the producer observed a real socket.
  (sorted-map :type entry-type :version 1 :sequence sequence
              :previous-digest previous :assessment (into (sorted-map) assessment)))

(defn- entry-digest [entry]
  (digest (.getBytes (str "gesso/recovery-audit/v1\n" (pr-str entry))
                     StandardCharsets/UTF_8)))

(defn- check-history! [entries floor]
  (when-not (and (vector? entries) (<= (count entries) max-entries))
    (reject! :invalid-audit-entry-vector))
  (loop [tail entries previous "genesis" seqno 1 attempt nil
         found? (zero? (:sequence floor))]
    (if-let [row (first tail)]
      (let [entry (:entry row) report (:assessment entry)
            expected (as-entry seqno previous report)
            fingerprint (entry-digest expected)]
        (when-not (and (map? row) (= #{:entry :digest} (set (keys row)))
                       (= expected entry)
                       (valid-assessment? report)
                       (= fingerprint (:digest row))
                       (or (nil? attempt) (= attempt (:attempt-id report))))
          (reject! :invalid-audit-hash-chain-or-assessment))
        (recur (next tail) fingerprint (inc seqno) (:attempt-id report)
               (or found? (and (= seqno (:sequence floor))
                               (= fingerprint (:digest floor))))))
      (do
        (when-not found? (reject! :audit-rollback-floor-missing))
        {:sequence (dec seqno) :digest previous :attempt-id attempt
         :record-count (dec seqno)}))))

(defn- safe-file! [^Path p]
  (when (Files/isSymbolicLink p) (reject! :symbolic-link-not-accepted))
  (when (and (Files/exists p no-links)
             (not (Files/isRegularFile p no-links)))
    (reject! :not-a-regular-audit-file)))

(defn- read-file! [^Path file floor]
  (safe-file! file)
  (if-not (Files/exists file no-links)
    (do (when (pos? (:sequence floor))
          (reject! :expected-audit-history-missing))
        {:entries [] :head (check-history! [] floor) :present? false})
    (do
      (when-not (<= 1 (Files/size file) max-file-bytes)
        (reject! :invalid-audit-file-size))
      (let [document (try
                       (with-open [reader (PushbackReader.
                                          (StringReader.
                                           (String. ^bytes (Files/readAllBytes file)
                                                    StandardCharsets/UTF_8)))]
                         (let [options {:eof ::eof :readers {}
                                        :default (fn [_ _] (reject! :unrecognized-edn-tag))}
                               first-form (edn/read options reader)
                               extra (edn/read options reader)]
                           (when (or (= ::eof first-form) (not= ::eof extra))
                             (reject! :extra-or-missing-audit-form))
                           first-form))
                       (catch Throwable _ (reject! :malformed-audit-file)))]
        (when-not (and (map? document)
                       (= #{:type :version :entries} (set (keys document)))
                       (= journal-type (:type document)) (= 1 (:version document)))
          (reject! :invalid-audit-envelope))
        {:entries (:entries document)
         :head (check-history! (:entries document) floor)
         :present? true}))))

(defn- force-directory! [^Path dir]
  (with-open [channel (FileChannel/open dir read-options)]
    (.force channel true)))

(defn- atomic-write! [^Path dir ^Path file entries]
  (let [content (str (pr-str (sorted-map :type journal-type
                                         :version 1 :entries entries)) "\n")
        bytes (.getBytes content StandardCharsets/UTF_8)]
    (when (> (alength bytes) max-file-bytes)
      (reject! :audit-file-capacity-exceeded))
    (let [tmp (Files/createTempFile dir ".recovery-audit-staging-" ".tmp"
                                    (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (Files/setPosixFilePermissions
         tmp (java.util.EnumSet/of PosixFilePermission/OWNER_READ
                                    PosixFilePermission/OWNER_WRITE))
        (with-open [channel (FileChannel/open tmp write-options)]
          (let [buf (ByteBuffer/wrap bytes)]
            (while (.hasRemaining buf) (.write channel buf))
            (.force channel true)))
        (safe-file! file)
        (Files/move tmp file
                    (into-array java.nio.file.CopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (force-directory! dir)
        (finally (Files/deleteIfExists tmp))))))

(defn prepare-recovery-audit-journal!
  "Prepare a local diagnostic-only journal.

   Exact config {:directory existing-Path :adapter v1909-quarantined-adapter
                 :audit-floor {:sequence n :digest sha256-or-genesis}
                 :initialize-empty? boolean}.
   :record! invokes :inspect-quarantined-recovery! itself, under the journal
   lock; callers cannot pass a fabricated assessment to :record!. :status!
   checks the entire persisted journal, and :records! exposes ONLY validated
   redacted assessments. All records are restricted to one original attempt
   ID. Failure during an append is :audit-write-outcome :unknown, never a
   retry, recovery, register CAS, XTDB write, or certificate.
   The caller-provided rollback floor and adapter are not authenticated."
  [config]
  (when-not (and (map? config)
                 (= #{:directory :adapter :audit-floor :initialize-empty?}
                    (set (keys config)))
                 (instance? Path (:directory config))
                 (map? (:adapter config))
                 (fn? (get-in config [:adapter :inspect-quarantined-recovery!]))
                 (valid-floor? (:audit-floor config))
                 (boolean? (:initialize-empty? config)))
    (reject! :invalid-audit-configuration))
  (let [^Path dir (.toAbsolutePath ^Path (:directory config))
        floor (:audit-floor config)
        file (.resolve dir "recovery-assessments.edn")
        lock-file (.resolve dir "recovery-assessments.lock")
        inspect! (get-in config [:adapter :inspect-quarantined-recovery!])]
    (when-not (and (Files/isDirectory dir no-links)
                   (not (Files/isSymbolicLink dir)))
      (reject! :audit-directory-unavailable))
    (safe-file! lock-file)
    (let [with-lock (fn [f]
                      (locking local-lock
                        (with-open [channel (FileChannel/open lock-file lock-options)
                                    lock (.lock channel)]
                          (f))))
          load! (fn [] (read-file! file floor))
          _ (with-lock
              #(let [{:keys [present?]} (load!)]
                 (when (and (not present?) (not (:initialize-empty? config)))
                   (reject! :explicit-audit-initialization-required))))
          status! (fn []
                    (with-lock
                      #(let [{:keys [head]} (load!)]
                         {:type journal-type :version 1
                          :head-sequence (:sequence head)
                          :head-digest (:digest head)
                          :attempt-id (:attempt-id head)
                          :record-count (:record-count head)
                          :caller-audit-rollback-floor :not-authenticated
                          :record-authenticity :not-established
                          :audit-completeness :not-established
                          :proof-discharge :none :certificate :not-issued
                          :authority :none})))
          records! (fn []
                     (with-lock
                       #(mapv (comp :assessment :entry) (:entries (load!)))))
          record! (fn []
                    (with-lock
                      (fn []
                        (let [{:keys [entries head]} (load!)
                              assessment (inspect!)
                              next-no (inc (:sequence head))]
                          (when-not (valid-assessment? assessment)
                            (reject! :invalid-or-nonquarantined-assessment))
                          (when (and (:attempt-id head)
                                     (not= (:attempt-id head)
                                           (:attempt-id assessment)))
                            (reject! :audit-attempt-binding-mismatch))
                          (when (> next-no max-entries)
                            (reject! :audit-entry-capacity-exceeded))
                          (let [entry (as-entry next-no (:digest head) assessment)
                                new-digest (entry-digest entry)
                                updated (conj entries {:entry entry
                                                       :digest new-digest})]
                            (try (atomic-write! dir file updated)
                                 (catch Throwable ex
                                   (throw (ex-info "Recovery audit write outcome unknown; inspect disk before intervention."
                                                   {:xtdb-recovery-audit/rejection true
                                                    :reason :audit-append-unconfirmed
                                                    :audit-write-outcome :unknown
                                                    :no-automatic-retry true
                                                    :proof-discharge :none
                                                    :certificate :not-issued
                                                    :authority :none}
                                                   ex))))
                            {:type entry-type :version 1
                             :sequence next-no :digest new-digest
                             :attempt-id (:attempt-id assessment)
                             :assessment-reconciliation (:reconciliation assessment)
                             :assessment-observation (:observation assessment)
                             :audit-file :locally-atomically-replaced
                             :audit-producer-independence :not-established
                             :audit-log-completeness :not-established
                             :quarantine :remains-permanent
                             :no-automatic-retry true
                             :proof-discharge :none
                             :certificate :not-issued
                             :authority :none})))))]
      {:type journal-type :version 1
       :status! status! :records! records! :record! record!
       :audit-floor-provisioning :caller-supplied-not-authenticated
       :audit-producer-independence :not-established
       :external-register-independence :not-established
       :physical-execution-causality :not-established
       :complete-audit-event-ingestion :not-established
       :quarantine-clearance :not-supported
       :proof-discharge :none :certificate :not-issued :authority :none})))
