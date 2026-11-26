(ns gesso.assurance.reference-historical-cut
  "Sensitive cut derivation and exact content comparison of issued samples."
  (:require [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-storage-identity :as storage]))

(def profile :selected-issued-historical-cut-v1)
(def ^:private original-reader acquisition/read-acquisition!)
(def ^:private cut-keys
  [:payload-binding-profile :record-offsets :transaction-ids
   :processed-message-id :payload-fingerprint])

(defn- result [status reason]
  {:profile profile :status status :reason reason
   :observation-storage-identity :not-established
   :authorized-historical-lineage :not-established
   :authority :none :certificate :not-issued})

(defn derive-cut!
  "Project the exact cut from a locally issued, qualified historical sample.
   No caller-provided sample, new physical read, or durable receipt. The cut
   contains sensitive content binding material; do not publish it."
  [node handle]
  (if-not (identical? original-reader acquisition/read-acquisition!)
    (result :unsupported :acquisition-reader-replaced)
    (if-let [sample (original-reader node handle)]
      (let [cut (select-keys sample cut-keys)]
        (if (and (= :observed (:status sample))
                 (= coverage/processed-retained-replay-inputs-profile
                    (:processed-retained-replay-inputs-profile sample))
                 (= :none (:authority sample)) (= :not-issued (:certificate sample))
                 (storage/historical-cut? cut))
          (assoc (dissoc (result :derived nil) :reason) :historical-cut cut)
          (result :unsupported :invalid-issued-historical-cut)))
      (result :unsupported :unissued-retained-acquisition))))

(defn- prefix? [required observed]
  (and (<= (count required) (count observed))
       (= required (subvec observed 0 (count required)))))

(defn compare-exact-cut!
  "Compare a declared required cut with an issued target acquisition.
   Match only equal complete cuts, including the payload digest. A longer
   coordinate-compatible history still needs a qualified prefix digest;
   never infer preservation from offset or transaction membership alone.
   A matched cut is content correspondence, not proof of original cut
   provenance, storage custody, or authorized historical lineage."
  [required node handle]
  (if-not (storage/historical-cut? required)
    (result :unsupported :invalid-required-cut)
    (let [r (derive-cut! node handle)
          observed (:historical-cut r)]
      (cond
        (not= :derived (:status r)) (result (:status r) (:reason r))
        (= required observed)
        (assoc (dissoc (result :matched nil) :reason) :cut-correspondence :exact-content)
        (not (and (prefix? (:record-offsets required) (:record-offsets observed))
                  (prefix? (:transaction-ids required) (:transaction-ids observed))))
        (result :mismatched :required-cut-coordinates-not-preserved)
        (= (:record-offsets required) (:record-offsets observed))
        (result :mismatched :required-cut-content-mismatch)
        :else (result :unsupported :qualified-prefix-digest-required)))))
