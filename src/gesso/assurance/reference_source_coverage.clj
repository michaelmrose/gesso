(ns gesso.assurance.reference-source-coverage
  "Bounded identity coverage of the installed embedded source logs."
  (:require [gesso.assurance.reference-operation-correspondence :as operation]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-replay-input :as replay-input]
            [xtdb.db-catalog :as db])
  (:import (java.io RandomAccessFile)
           (java.lang ReflectiveOperationException)
           (java.nio ByteBuffer)
           (java.nio.file Path)
           (java.security MessageDigest)
           (java.time Instant)
           (java.util Arrays HexFormat)
           (xtdb.time InstantUtil)
           (xtdb.api.log LocalLog InMemoryLog Log$Record SourceMessage$Tx
                         SourceMessage$FlushBlock SourceMessage$TriesAdded SourceMessage$BlockUploaded)
           (xtdb.database Database)))

(def profile :selected-in-memory-source-coverage-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
(def ^:private max-records 1024)
(defn- result [status reason] (assoc base :status status :reason reason))

(defn- record-kind [message]
  (cond
    (instance? SourceMessage$Tx message) :transaction
    (instance? SourceMessage$FlushBlock message) :flush-block
    (instance? SourceMessage$TriesAdded message) :tries-added
    (instance? SourceMessage$BlockUploaded message) :block-uploaded
    :else :unsupported))

(defn- classify-records [records end]
  ;; In this selected epoch-zero in-memory log, each offset is an ID.
  ;; Do not apply this contiguity test to LocalLog's byte offsets.
  (cond
    (not= (mapv #(.getMsgId ^Log$Record %) records) (vec (range (inc end))))
    (result :unavailable :source-record-coverage-gap)
    :else
    (let [kinds (mapv #(record-kind (.getMessage ^Log$Record %)) records)]
      (if (some #{:unsupported} kinds)
        (result :unsupported :unsupported-source-message-kind)
        (assoc base :status :observed :last-message-id end
               :transaction-ids (into [] (keep-indexed #(when (= :transaction %2) %1)) kinds)
               :maintenance-count (count (remove #{:transaction} kinds)))))))

(defn observe-coverage!
  "Traverse the entire bounded epoch-zero in-memory source log from offset
   zero through the captured submitted end. Compare every identity with the
   contiguous expected offset range and distinguish selected maintenance
   messages from transactions. Unknown message kinds fail closed. Recheck
   the submitted end; this is not a processed frontier or proof of retention,
   concurrent snapshot atomicity or completeness for another log backend."
  [node]
  (if-not (db/<-node node)
    (result :unsupported :embedded-primary-log-required)
    (try
      (let [^Database database (db/primary-db node)
            log (.getSourceLog database)]
        (if-not (and (instance? InMemoryLog log) (zero? (.getEpoch ^InMemoryLog log)))
          (result :unsupported :unsupported-source-log)
          (let [^InMemoryLog log log
                end (.getLatestSubmittedOffset log)]
            (if-not (<= -1 end (dec max-records))
              (result :unsupported :source-record-limit)
              (let [records (if (neg? end) []
                              (into [] (take (inc max-records))
                                    (iterator-seq (.iterator (.readRecords log 0 (inc end))))))
                    report (classify-records records end)]
                (if (not= end (.getLatestSubmittedOffset log))
                  (result :unavailable :source-log-changed-during-observation)
                  report))))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch Exception _ (result :unavailable :source-log-read-failed)))))

(def processed-profile :selected-processed-source-frontier-v1)

(defn- read-frontier! [node]
  (let [^Database database (db/primary-db node)
        log (.getSourceLog database)]
    (when (and (instance? InMemoryLog log) (zero? (.getEpoch ^InMemoryLog log)))
      {:submitted (.getLatestSubmittedOffset ^InMemoryLog log)
       :processed (.getLatestProcessedMsgId database)
       :ingestion-failed? (some? (.getIngestionError database))})))

(defn- frontier-correspondence* [source-profile frontier-profile before coverage after]
  (let [tag #(assoc % :processed-profile frontier-profile)
        fail #(tag (assoc (result %1 %2) :profile source-profile))]
    (cond
      (or (nil? before) (nil? after)) (fail :unsupported :unsupported-source-log)
      (or (:ingestion-failed? before) (:ingestion-failed? after))
      (fail :unavailable :source-ingestion-failed)
      (not= :observed (:status coverage)) (tag coverage)
      (not= before after) (fail :unavailable :source-frontier-changed-during-observation)
      (not= (:submitted before) (:last-message-id coverage))
      (fail :unavailable :source-frontier-coverage-mismatch)
      (not (and (integer? (:processed before)) (<= -1 (:processed before))))
      (fail :unavailable :source-frontier-contradiction)
      (> (:processed before) (:submitted before)) (fail :unavailable :source-frontier-contradiction)
      (and (contains? coverage :record-offsets) (not= -1 (:processed before))
           (not (some #{(:processed before)} (:record-offsets coverage))))
      (fail :unavailable :source-processed-offset-not-framed)
      (< (:processed before) (:submitted before)) (fail :unavailable :source-processing-incomplete)
      :else (tag (assoc coverage :processed-message-id (:processed before))))))

(defn observe-processed-coverage!
  "Require the selected source coverage and stable submitted/processed
   coordinates, including maintenance messages. Read only: do not wait for
   processing, silently advance the expected frontier, or establish atomic
   observation or absence of future writes. Provider watcher semantics and
   complete retention remain premises."
  [node]
  (if-not (db/<-node node)
    (assoc (result :unsupported :embedded-primary-log-required) :processed-profile processed-profile)
    (try
      (let [before (read-frontier! node)
            coverage (observe-coverage! node)
            after (read-frontier! node)]
        (frontier-correspondence* profile processed-profile before coverage after))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch Exception _
        (assoc (result :unavailable :source-frontier-read-failed) :processed-profile processed-profile)))))

(def local-profile :xtdb-2.2.0-beta1/selected-local-source-coverage-v1)
(def ^:private max-local-bytes (* 64 1024 1024))
(def ^:private max-local-message-bytes (* 16 1024 1024))
(defn- local-result [status reason]
  (assoc (result status reason) :profile local-profile))

(defn- local-log-path [^LocalLog log]
  ;; Version-specific read-only access to the actual log object's path, not
  ;; a caller-supplied path that could identify another database.
  (let [field (.getDeclaredField LocalLog "logFilePath")]
    (.setAccessible field true)
    (.get field log)))

(defn- local-frames [^RandomAccessFile file]
  ;; Installed LocalLog: separator byte 30, BE int payload length, BE long
  ;; timestamp in micros, payload, BE long record-start offset. Do not
  ;; decode or allocate the payload here. Check every frame through EOF.
  (let [size (.length file)]
    (if (> size max-local-bytes)
      (local-result :unsupported :source-byte-limit)
      (loop [offset 0 frames []]
        (cond
          (= offset size) {:status :framed :size size :frames frames}
          (>= (count frames) max-records) (local-result :unsupported :source-record-limit)
          (< (- size offset) 21) (local-result :unavailable :invalid-source-frame)
          :else
          (do (.seek file offset)
              (let [separator (.readUnsignedByte file)
                    length (.readInt file)
                    micros (.readLong file)
                    next-offset (+ offset 21 length)]
                (cond
                  (or (not= 30 separator) (neg? length) (> next-offset size))
                  (local-result :unavailable :invalid-source-frame)
                  (> length max-local-message-bytes) (local-result :unsupported :source-message-byte-limit)
                  :else
                  (do (.seek file (- next-offset 8))
                      (if (not= offset (.readLong file))
                        (local-result :unavailable :invalid-source-frame)
                        (recur next-offset (conj frames [offset micros length]))))))))))))

(defn- classify-local-records [framing records end]
  (let [frames (:frames framing)
        coordinates (mapv (fn [^Log$Record r]
                            [(.getMsgId r) (InstantUtil/getAsMicros (.getLogTimestamp r))]) records)
        expected (mapv #(subvec % 0 2) frames)]
    (cond
      (not= end (or (ffirst (rseq frames)) -1))
      (local-result :unavailable :source-frame-end-mismatch)
      (not= coordinates expected)
      (local-result :unavailable :source-record-coverage-gap)
      :else
      (let [kinds (mapv #(record-kind (.getMessage ^Log$Record %)) records)]
        (if (some #{:unsupported} kinds)
          (local-result :unsupported :unsupported-source-message-kind)
          (assoc base :profile local-profile :status :observed :last-message-id end
                 :record-offsets (mapv first frames) :file-bytes (:size framing)
                 :transaction-ids (into [] (keep (fn [[frame kind]]
                                                  (when (= :transaction kind) (first frame))))
                                        (map vector frames kinds))
                 :maintenance-count (count (remove #{:transaction} kinds))))))))

(def local-payload-profile :xtdb-2.2.0-beta1/selected-local-payload-correspondence-v1)

(defn- read-local-payloads! [^RandomAccessFile file frames]
  ;; The framing scan bounds both total file bytes and individual messages.
  (mapv (fn [[offset _ length]]
          (.seek file (+ offset 13))
          (let [bytes (byte-array length)]
            (.readFully file bytes)
            bytes))
        frames))

(defn- same-payloads? [a b]
  (and (= (count a) (count b))
       (every? true? (map (fn [^bytes x ^bytes y] (Arrays/equals x y)) a b))))

(defn- payload-mismatch [message provider]
  ;; Compare opaque operation and metadata bytes by content, not identity.
  ;; Do not include either raw payload or decoded values in diagnostics.
  (case (:kind message)
    :transaction
    (if-not (instance? SourceMessage$Tx provider)
      :kind
      (let [^SourceMessage$Tx p provider]
        (cond
          (some? (.getExternalSourceToken p)) :external-source-token
          (not (Arrays/equals ^bytes (:tx-ops message) ^bytes (.getTxOps p))) :tx-ops
          (not= (:system-time message) (.getSystemTime p)) :system-time
          (not= (:default-tz message) (.getDefaultTz p)) :default-tz
          (not= (:user message) (.getUser p)) :user
          (not (Arrays/equals ^bytes (:user-metadata message) ^bytes (.getUserMetadata p))) :user-metadata)))
    :flush-block
    (cond
      (not (instance? SourceMessage$FlushBlock provider)) :kind
      (not= (:expected-block-index message) (.getExpectedBlockIdx ^SourceMessage$FlushBlock provider))
      :expected-block-index)
    :kind))

(defn- compare-selected-operation [node offset message]
  (let [r (operation/compare-operation-bytes! node (:tx-ops message))]
    (when-not (and (= :matched (:status r))
                   (= operation/profile (:profile r))
                   (= :matched (:operation-correspondence r))
                   (= :none (:authority r))
                   (= :not-issued (:certificate r)))
      (cond-> (assoc (local-result (case (:status r)
                                    :mismatch :mismatch
                                    :unsupported :unsupported
                                    :unavailable :unavailable
                                    :unsupported)
                                  :retained-operation-not-matched)
                     :message-id offset :operation-reason (:reason r))
        (:field r) (assoc :field (:field r))
        (:decode-reason r) (assoc :decode-reason (:decode-reason r))))))

(def ^:private max-replay-transactions 256)

(defn- qualified-replay-input? [offset tx]
  (and (map? tx) (= #{:tx-id :system-time :ops} (set (keys tx)))
       (= offset (:tx-id tx)) (instance? Instant (:system-time tx))
       (vector? (:ops tx)) (<= 1 (count (:ops tx)) 256)))

(defn- project-replay-input [offset micros body prior-time]
  (let [r (replay-input/decode-replay-input offset body micros prior-time)]
    (if (and (= :decoded (:status r)) (= replay-input/profile (:profile r))
             (= :none (:authority r)) (= :not-issued (:certificate r))
             (qualified-replay-input? offset (:transaction r)))
      {:input (:transaction r)}
      {:failure (assoc (local-result :unsupported :retained-replay-input-not-qualified)
                       :message-id offset :decode-reason (:reason r))})))

(defn- qualified-clocked-input? [offset micros requested prior r]
  (let [clock (replay-input/resolve-retained-clock micros requested prior)]
    (and (= :decoded (:status r)) (= replay-input/clocked-input-profile (:profile r))
         (= :none (:authority r)) (= :not-issued (:certificate r))
         (= #{:status :profile :authority :certificate :transaction :clock
              :requested-system-time :frame-micros} (set (keys r)))
         (= micros (:frame-micros r)) (= requested (:requested-system-time r))
         (= :resolved (:status clock)) (= clock (:clock r))
         (qualified-replay-input? offset (:transaction r))
         (= (:system-time clock) (get-in r [:transaction :system-time])))))

(defn- project-clocked-input [offset micros body requested prior]
  (let [r (replay-input/decode-clocked-replay-input offset body micros prior)]
    (if (qualified-clocked-input? offset micros requested prior r)
      {:input r}
      {:failure (assoc (local-result :unsupported :retained-clocked-input-not-qualified)
                       :message-id offset)})))

(defn- qualified-clocked-history? [ids inputs]
  (and (vector? inputs) (<= (count inputs) max-replay-transactions)
       (= ids (mapv #(get-in % [:transaction :tx-id]) inputs))
       (loop [prior nil remaining (seq inputs)]
         (if-let [r (first remaining)]
           (and (qualified-clocked-input? (get-in r [:transaction :tx-id])
                                          (:frame-micros r) (:requested-system-time r) prior r)
                (recur (get-in r [:transaction :system-time]) (next remaining)))
           true))))

(defn- increasing-replay-times? [inputs]
  (every? (fn [[a b]] (.isBefore ^Instant (:system-time a) ^Instant (:system-time b)))
          (partition 2 1 inputs)))

(defn- compare-local-payloads [report frames bytes records node target]
  ;; Compare only after coordinates, order and cardinality have matched.
  ;; Operation comparisons and replay projection use the same framed bytes.
  (let [replay? (= :replay target)
        clocked? (= :clocked-replay target)
        all? (or replay? clocked? (= :all target))]
    (cond
      (not= :observed (:status report)) report
      (not= (count frames) (count bytes) (count records))
      (local-result :unavailable :source-payload-coverage-gap)
      (and (or replay? clocked?) (> (count (:transaction-ids report)) max-replay-transactions))
      (local-result :unsupported :retained-replay-transaction-limit)
      (and (some? target) (not all?) (not (some #{target} (:transaction-ids report))))
      (assoc (local-result :unsupported :retained-transaction-not-framed) :message-id target)
      :else
      (loop [samples (seq (map vector frames bytes records)) matched [] inputs []]
        (if-let [[[offset micros _] body ^Log$Record record] (first samples)]
          (let [decoded (payload/decode-payload body)
                message (:message decoded)]
            (if-not (= :decoded (:status decoded))
              (assoc (local-result :unsupported :unsupported-source-payload)
                     :message-id offset :decode-reason (:reason decoded))
              (if-let [field (payload-mismatch message (.getMessage record))]
                (assoc (local-result :mismatch :source-payload-mismatch)
                       :message-id offset :field field)
                (if (or (= target offset) (and all? (= :transaction (:kind message))))
                  (if-let [failure (compare-selected-operation node offset message)]
                    failure
                    (let [{:keys [input failure]} (cond
                                                 replay? (project-replay-input offset micros body (:system-time (peek inputs)))
                                                 clocked? (project-clocked-input offset micros body (:system-time message)
                                                                                (get-in (peek inputs) [:transaction :system-time])))]
                      (if failure failure
                          (recur (next samples) (conj matched offset)
                                 (cond-> inputs (or replay? clocked?) (conj input))))))
                  (recur (next samples) matched inputs)))))
          (let [report (assoc report :payload-correspondence :matched)]
            (cond
              all?
              (cond
                (not= matched (:transaction-ids report))
                (local-result :unavailable :retained-operation-coverage-gap)
                (and replay? (not= matched (mapv :tx-id inputs)))
                (local-result :unavailable :retained-replay-input-coverage-gap)
                (and replay? (not (increasing-replay-times? inputs)))
                (local-result :unsupported :retained-replay-system-time-order)
                (and clocked? (not (qualified-clocked-history? matched inputs)))
                (local-result :unsupported :retained-clocked-history-not-qualified)
                :else
                (cond-> (assoc report :operation-correspondence (if (seq matched) :matched :not-applicable)
                               :operation-message-ids matched :operation-profile operation/profile)
                  replay? (assoc :replay-inputs inputs :replay-input-profile replay-input/profile)
                  clocked? (assoc :clocked-replay-inputs inputs :clocked-replay-input-profile replay-input/clocked-input-profile)))
              (some? target)
              (assoc report :operation-correspondence :matched
                     :operation-message-id target :operation-profile operation/profile)
              :else report)))))))

(def payload-binding-profile :xtdb-2.2.0-beta1/selected-local-payload-binding-v1)

(defn- payload-fingerprint [frames bytes]
  ;; Fixed-width big-endian coordinates and lengths bind the ordered
  ;; payloads, including maintenance. Internal equality sample, not a receipt.
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (doseq [[[offset micros length] ^bytes body] (map vector frames bytes)]
      (let [header (doto (ByteBuffer/allocate 24)
                     (.putLong (long offset))
                     (.putLong (long micros))
                     (.putLong (long length)))]
        (.update digest (.array header))
        (.update digest body)))
    (.formatHex (HexFormat/of) (.digest digest))))

(defn- observe-local* [node payload? bind? target]
  (if-not (db/<-node node)
    (local-result :unsupported :embedded-primary-log-required)
    (try
      (let [^Database database (db/primary-db node)
            log (.getSourceLog database)]
        (if-not (and (instance? LocalLog log) (zero? (.getEpoch ^LocalLog log)))
          (local-result :unsupported :unsupported-source-log)
          (let [^LocalLog log log
                end (.getLatestSubmittedOffset log)
                path (local-log-path log)]
            (with-open [file (RandomAccessFile. (.toFile ^Path path) "r")]
              (let [framing (local-frames file)
                    frames (:frames framing)]
                (if-not (= :framed (:status framing))
                  framing
                  (if (not= end (or (ffirst (rseq frames)) -1))
                    (local-result :unavailable :source-frame-end-mismatch)
                    (let [bytes (when payload? (read-local-payloads! file frames))
                          records (if (neg? end) []
                                    (into [] (take (inc max-records))
                                          (iterator-seq (.iterator (.readRecords log 0 (inc end))))))
                          report (classify-local-records framing records end)
                          report (if payload? (compare-local-payloads report frames bytes records node target) report)]
                      (cond
                        (and payload? (not= framing (local-frames file)))
                        (local-result :unavailable :source-log-changed-during-observation)
                        (and payload? (not (same-payloads? bytes (read-local-payloads! file frames))))
                        (local-result :unavailable :source-payload-changed-during-observation)
                        (or (not= framing (local-frames file))
                            (not= end (.getLatestSubmittedOffset log)))
                        (local-result :unavailable :source-log-changed-during-observation)
                        :else (cond-> report
                                (and bind? (= :observed (:status report)))
                                (assoc :payload-fingerprint (payload-fingerprint frames bytes))))))))))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch ReflectiveOperationException _ (local-result :unsupported :unsupported-local-log-layout))
      (catch Exception _ (local-result :unavailable :source-log-read-failed)))))

(defn observe-local-coverage!
  "Selected epoch-zero LocalLog coverage. Independently walk byte frames
   from zero to EOF and compare decoded record identities and timestamps.
   Recheck framing and submitted end. Requires the installed private path
   field and stable quiescent file; not independent payload decoding,
   retention, an atomic snapshot, processed frontier or tamper detection."
  [node]
  (observe-local* node false false nil))

(defn observe-local-payload-coverage!
  "Additionally compare every selected independently decoded envelope with
   the provider message at the same framed coordinate. Re-read raw bytes
   within the observation; reject unsupported variants and mismatches.
   Return no raw payloads. No independent Arrow interpretation, processed
   frontier, atomic snapshot, cross-observation content binding or proof."
  [node]
  (assoc (observe-local* node true false nil) :payload-profile local-payload-profile
         :decoder-profile payload/profile))

(defn observe-local-payload-bound-coverage!
  "Internal sample binding ordered frame coordinates, timestamps, lengths
   and raw payload content. Do not publish or persist the fingerprint as a
   receipt. Equality assumes collision resistance and stable acquisition;
   not authenticated provenance, atomic snapshot or independent Arrow semantics."
  [node]
  (assoc (observe-local* node true true nil) :payload-profile local-payload-profile
         :decoder-profile payload/profile :payload-binding-profile payload-binding-profile))

(def retained-operation-profile :xtdb-2.2.0-beta1/selected-retained-operation-correspondence-v2)

(defn observe-local-retained-operation!
  "Compare selected envelopes across the framed local log and independently
   compare operations at one requested transaction offset. Both comparisons
   occur within the same bounded acquisition and final framing/byte rechecks.
   Other transactions receive envelope checks only. No processed frontier,
   atomic snapshot, execution, authenticated provenance or certification."
  [node tx-id]
  (let [report (if (and (integer? tx-id) (<= 0 tx-id Long/MAX_VALUE))
                 (observe-local* node true false tx-id)
                 (local-result :unsupported :invalid-retained-transaction-id))]
    (assoc report :retained-operation-profile retained-operation-profile
           :payload-profile local-payload-profile :decoder-profile payload/profile)))

(def local-processed-profile :xtdb-2.2.0-beta1/selected-local-processed-frontier-v1)

(defn- read-local-frontier! [node]
  (let [^Database database (db/primary-db node)
        log (.getSourceLog database)]
    (when (and (instance? LocalLog log) (zero? (.getEpoch ^LocalLog log)))
      {:submitted (.getLatestSubmittedOffset ^LocalLog log)
       :processed (.getLatestProcessedMsgId database)
       :ingestion-failed? (some? (.getIngestionError database))})))

(defn- observe-local-processed* [node observe!]
  (let [fail #(assoc (local-result %1 %2) :processed-profile local-processed-profile)]
    (if-not (db/<-node node)
      (fail :unsupported :embedded-primary-log-required)
      (try
        (let [before (read-local-frontier! node)
              coverage (observe! node)
              after (read-local-frontier! node)]
          (frontier-correspondence* local-profile local-processed-profile before coverage after))
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (throw e))
        (catch Exception _ (fail :unavailable :source-frontier-read-failed))))))

(defn observe-local-processed-coverage!
  "Require stable submitted/processed byte-offset coordinates around local
   framing and decoded coverage. A nonnegative processed coordinate must
   identify a framed record, not an interior byte or the end of file.
   No wait, atomic snapshot, retention or independent watcher semantics."
  [node]
  (observe-local-processed* node observe-local-coverage!))

(def local-processed-payload-profile :xtdb-2.2.0-beta1/selected-local-processed-payload-v1)

(defn observe-local-processed-payload-bound-coverage!
  "Internal sample of independently compared selected payloads bracketed
   by stable, fully processed local source frontiers. No wait or atomic
   snapshot; fingerprint is an internal equality sample, not a receipt."
  [node]
  (let [sample (observe-local-processed* node observe-local-payload-bound-coverage!)]
    (assoc sample :processed-payload-profile local-processed-payload-profile
           :payload-profile local-payload-profile :decoder-profile payload/profile
           :payload-binding-profile payload-binding-profile)))

(def local-processed-retained-operation-profile
  :xtdb-2.2.0-beta1/selected-local-processed-retained-operation-v2)

(defn observe-local-processed-retained-operation!
  "Require selected retained operation correspondence bracketed by stable,
   fully processed local source frontiers. The requested transaction must
   belong to the observed framed history. No wait, execution outcome,
   independent watcher semantics, atomic snapshot or certification."
  [node tx-id]
  (let [sample (if (and (integer? tx-id) (<= 0 tx-id Long/MAX_VALUE))
                 (observe-local-processed*
                  node #(observe-local-retained-operation! % tx-id))
                 (local-result :unsupported :invalid-retained-transaction-id))
        sample (if (and (= :observed (:status sample))
                        (not (and (= local-profile (:profile sample))
                                  (= local-processed-profile (:processed-profile sample))
                                  (= retained-operation-profile (:retained-operation-profile sample))
                                  (= :matched (:payload-correspondence sample) (:operation-correspondence sample))
                                  (= operation/profile (:operation-profile sample))
                                  (= tx-id (:operation-message-id sample))
                                  (some #{tx-id} (:transaction-ids sample))
                                  (some #{tx-id} (:record-offsets sample))
                                  (<= tx-id (:processed-message-id sample))
                                  (= :none (:authority sample))
                                  (= :not-issued (:certificate sample)))))
                 (local-result :unsupported :invalid-processed-retained-operation)
                 sample)]
    (assoc sample :processed-retained-operation-profile local-processed-retained-operation-profile
           :processed-profile local-processed-profile
           :retained-operation-profile retained-operation-profile
           :payload-profile local-payload-profile :decoder-profile payload/profile)))

(def retained-history-profile
  :xtdb-2.2.0-beta1/selected-retained-operation-history-v1)

(defn observe-local-retained-history!
  "Compare every framed transaction's operations within one bounded local
   log observation. Maintenance receives envelope checks only. Empty
   transaction history has no operation obligation. No partial matches,
   execution, atomic snapshot, or certification."
  [node]
  (assoc (observe-local* node true false :all)
         :retained-history-profile retained-history-profile
         :payload-profile local-payload-profile :decoder-profile payload/profile))

(def local-processed-retained-history-profile
  :xtdb-2.2.0-beta1/selected-local-processed-retained-history-v1)

(defn- observe-processed-retained-history* [node observe!]
  (let [sample (observe-local-processed* node observe!)
        ids (:transaction-ids sample)
        sample (if (and (= :observed (:status sample))
                        (not (and (= local-profile (:profile sample))
                                  (= local-processed-profile (:processed-profile sample))
                                  (= retained-history-profile (:retained-history-profile sample))
                                  (= :matched (:payload-correspondence sample))
                                  (= operation/profile (:operation-profile sample))
                                  (vector? ids)
                                  (= ids (:operation-message-ids sample))
                                  (= (if (seq ids) :matched :not-applicable) (:operation-correspondence sample))
                                  (= (count ids) (count (distinct ids)))
                                  (= ids (filterv (set ids) (:record-offsets sample)))
                                  (every? #(and (integer? %) (<= 0 % (:processed-message-id sample))) ids)
                                  (= :none (:authority sample))
                                  (= :not-issued (:certificate sample)))))
                 (local-result :unsupported :invalid-processed-retained-history)
                 sample)]
    (assoc sample :processed-retained-history-profile local-processed-retained-history-profile
           :processed-profile local-processed-profile
           :retained-history-profile retained-history-profile
           :payload-profile local-payload-profile :decoder-profile payload/profile)))

(defn observe-local-processed-retained-history!
  "Bracket complete selected retained operation history correspondence
   with stable fully processed source frontiers. No execution outcome or
   independent watcher semantics."
  [node]
  (observe-processed-retained-history* node observe-local-retained-history!))

(def processed-retained-history-binding-profile
  :xtdb-2.2.0-beta1/selected-processed-retained-history-binding-v1)

(defn observe-local-processed-retained-history-bound!
  "Internal equality sample: bind all qualified operation comparisons to
   the exact framed content within the same processed observation. Never
   use the fingerprint as an authenticated receipt."
  [node]
  (let [sample (observe-processed-retained-history*
                node #(assoc (observe-local* % true true :all)
                             :retained-history-profile retained-history-profile
                             :payload-profile local-payload-profile
                             :decoder-profile payload/profile))]
    (assoc sample :payload-binding-profile payload-binding-profile
           :processed-retained-history-binding-profile processed-retained-history-binding-profile)))

(def processed-retained-replay-inputs-profile
  :xtdb-2.2.0-beta1/selected-processed-retained-replay-inputs-v8)

(defn observe-local-processed-retained-replay-inputs!
  "Internal sensitive sample of independently projected replay inputs from
   the same framed payloads as complete operation correspondence. Require
   stable bytes and processed frontiers, exact ordered identities and
   increasing independently resolved system times. Empty history yields no inputs.
   Do not publish inputs or fingerprints. No execution comparison or proof."
  [node]
  (let [sample (observe-processed-retained-history*
                node #(assoc (observe-local* % true true :replay)
                             :retained-history-profile retained-history-profile
                             :payload-profile local-payload-profile
                             :decoder-profile payload/profile))
        inputs (:replay-inputs sample)
        sample (if (and (= :observed (:status sample))
                        (not (and (= replay-input/profile (:replay-input-profile sample))
                                  (vector? inputs) (<= (count inputs) max-replay-transactions)
                                  (= (:transaction-ids sample) (mapv :tx-id inputs))
                                  (every? #(qualified-replay-input? (:tx-id %) %) inputs)
                                  (increasing-replay-times? inputs)
                                  (string? (:payload-fingerprint sample))
                                  (re-matches #"[0-9a-f]{64}" (:payload-fingerprint sample)))))
                 (local-result :unsupported :invalid-processed-retained-replay-inputs)
                 sample)]
    (cond-> (assoc sample :processed-retained-replay-inputs-profile processed-retained-replay-inputs-profile
                   :processed-retained-history-binding-profile processed-retained-history-binding-profile
                   :payload-binding-profile payload-binding-profile)
      (not= :observed (:status sample)) (dissoc :replay-inputs :payload-fingerprint))))

(def processed-retained-clocked-inputs-profile
  :xtdb-2.2.0-beta1/selected-processed-retained-clocked-inputs-v1)

(defn observe-local-processed-retained-clocked-inputs!
  "Internal sensitive sample of ordered clock-qualified attempts. Bind each
   decision to the actual frame and requested time, including the assigned
   time of a clock-rejected attempt. Maintenance does not advance the clock.
   Require stable complete history and processed frontiers; no execution
   prediction, publication or authenticated provenance."
  [node]
  (let [sample (observe-processed-retained-history*
                node #(assoc (observe-local* % true true :clocked-replay)
                             :retained-history-profile retained-history-profile
                             :payload-profile local-payload-profile
                             :decoder-profile payload/profile))
        sample (if (and (= :observed (:status sample))
                        (not (and (= replay-input/clocked-input-profile (:clocked-replay-input-profile sample))
                                  (qualified-clocked-history? (:transaction-ids sample) (:clocked-replay-inputs sample))
                                  (string? (:payload-fingerprint sample))
                                  (re-matches #"[0-9a-f]{64}" (:payload-fingerprint sample)))))
                 (local-result :unsupported :invalid-processed-retained-clocked-inputs)
                 sample)]
    (cond-> (assoc sample :processed-retained-clocked-inputs-profile processed-retained-clocked-inputs-profile
                   :processed-retained-history-binding-profile processed-retained-history-binding-profile
                   :payload-binding-profile payload-binding-profile)
      (not= :observed (:status sample)) (dissoc :clocked-replay-inputs :payload-fingerprint))))
