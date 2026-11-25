(ns gesso.assurance.reference-source-coverage
  "Bounded identity coverage of the installed embedded source logs."
  (:require [xtdb.db-catalog :as db])
  (:import (java.io RandomAccessFile)
           (java.lang ReflectiveOperationException)
           (java.nio.file Path)
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

(defn- frontier-correspondence [before coverage after]
  (let [tag #(assoc % :processed-profile processed-profile)
        fail #(tag (result %1 %2))]
    (cond
      (or (nil? before) (nil? after)) (fail :unsupported :unsupported-source-log)
      (or (:ingestion-failed? before) (:ingestion-failed? after))
      (fail :unavailable :source-ingestion-failed)
      (not= :observed (:status coverage)) (tag coverage)
      (not= before after) (fail :unavailable :source-frontier-changed-during-observation)
      (not= (:submitted before) (:last-message-id coverage))
      (fail :unavailable :source-frontier-coverage-mismatch)
      (< (:processed before) (:submitted before)) (fail :unavailable :source-processing-incomplete)
      (> (:processed before) (:submitted before)) (fail :unavailable :source-frontier-contradiction)
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
        (frontier-correspondence before coverage after))
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

(defn observe-local-coverage!
  "Selected epoch-zero LocalLog coverage. Independently walk byte frames
   from zero to EOF and compare decoded record identities and timestamps.
   Recheck framing and submitted end. Requires the installed private path
   field and stable quiescent file; not independent payload decoding,
   retention, an atomic snapshot, processed frontier or tamper detection."
  [node]
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
              (let [framing (local-frames file)]
                (if-not (= :framed (:status framing))
                  framing
                  (if (not= end (or (ffirst (rseq (:frames framing))) -1))
                    (local-result :unavailable :source-frame-end-mismatch)
                    (let [records (if (neg? end) []
                                    (into [] (take (inc max-records))
                                          (iterator-seq (.iterator (.readRecords log 0 (inc end))))))
                          report (classify-local-records framing records end)]
                      (if (or (not= framing (local-frames file))
                              (not= end (.getLatestSubmittedOffset log)))
                        (local-result :unavailable :source-log-changed-during-observation)
                        report)))))))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch ReflectiveOperationException _ (local-result :unsupported :unsupported-local-log-layout))
      (catch Exception _ (local-result :unavailable :source-log-read-failed)))))
