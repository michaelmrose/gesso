(ns gesso.assurance.reference-source-coverage
  "Bounded identity coverage of the installed embedded in-memory source log."
  (:require [xtdb.db-catalog :as db])
  (:import (xtdb.api.log InMemoryLog Log$Record SourceMessage$Tx
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
