(ns gesso.assurance.reference-retained-acquisition
  "Process-local issuance of the complete selected retained replay sample."
  (:require [gesso.assurance.reference-source-coverage :as coverage]
            [xtdb.db-catalog :as db])
  (:import [java.util WeakHashMap]
           [xtdb.database Database]))

(def profile :selected-local-retained-acquisition-v1)
(deftype RetainedAcquisition [])
(def ^:private ^WeakHashMap acquisitions (WeakHashMap.))
(def ^:private original-reader coverage/observe-local-processed-retained-replay-inputs!)

(defn- report [status reason]
  {:profile profile :status status :reason reason
   :observation-storage-identity :not-established
   :authorized-historical-lineage :not-established
   :authority :none :certificate :not-issued})

(defn capture!
  "Acquire the actual complete sample, without caller-supplied observations.
   Issue only after the selected database and source-log objects agree across
   acquisition. Keep the exact sample and originating node in a local weak-key
   registry. The handle identifies a historical acquisition, not a live
   snapshot, authenticated storage identity, or cross-restart evidence.
   The reader and installed JVM/provider remain trusted implementation seams."
  [node]
  (if-not (identical? original-reader coverage/observe-local-processed-retained-replay-inputs!)
    (report :unsupported :retained-reader-replaced)
    (try
      (if-not (db/<-node node)
        (report :unsupported :embedded-primary-log-required)
        (let [^Database database (db/primary-db node)
              log (.getSourceLog database)
              sample (original-reader node)
              ^Database after (db/primary-db node)]
          (cond
            (not (and (identical? database after)
                      (identical? log (.getSourceLog after))))
            (report :unavailable :source-object-changed-during-acquisition)

            (not= :observed (:status sample))
            (report (:status sample) (:reason sample))

            (not (and (= coverage/processed-retained-replay-inputs-profile
                         (:processed-retained-replay-inputs-profile sample))
                      (= :none (:authority sample))
                      (= :not-issued (:certificate sample))))
            (report :unsupported :invalid-retained-acquisition)

            :else
            (let [handle (RetainedAcquisition.)]
              (locking acquisitions
                (.put acquisitions handle {:node node :sample sample}))
              (assoc (dissoc (report :issued nil) :reason) :acquisition handle)))))
      (catch InterruptedException e
        (.interrupt (Thread/currentThread))
        (throw e))
      (catch Exception _ (report :unavailable :retained-acquisition-failed)))))

(defn read-acquisition!
  "Return the exact sensitive historical sample for the issued handle and
   identical original node object. Never accept a supplied sample or expose
   the stored node. A reopened node is not the original local acquisition
   source, even if all its retained data match. No fresh read or live-state
   check is performed. Do not publish the returned sample."
  [node handle]
  (when (instance? RetainedAcquisition handle)
    (locking acquisitions
      (let [entry (.get acquisitions handle)]
        (when (and entry (identical? node (:node entry)))
          (:sample entry))))))
