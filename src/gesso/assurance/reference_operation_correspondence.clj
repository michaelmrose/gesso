(ns gesso.assurance.reference-operation-correspondence
  "Compare independently interpreted operation IPC with provider decoding.
   This is a selected, non-authoritative byte correspondence diagnostic."
  (:require [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.xtdb-source-log :as provider]))

(def profile :xtdb-2.2.0-beta1/selected-mixed-operation-correspondence-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
(def ^:private max-bytes (* 16 1024 1024))

(defn compare-operation-bytes!
  "Independently decode a bounded mixed SQL/put stream before calling the
   provider on a copy of the same byte sample. Compare the complete ordered
   operations (including document IIDs) and invocation spans. No values or
   raw bytes are returned. Unsupported input on either side fails closed;
   no fallback to a weaker profile. No retained-record identity, execution,
   admission, or universal refinement follows from equality."
  [node bytes]
  (if-not (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes))
    (assoc base :status :unsupported :reason :operation-byte-limit)
    (let [sample (aclone ^bytes bytes)
          independent (arrow/decode-mixed-bounded-sql-put (aclone sample))]
      (if-not (and (= :decoded (:status independent))
                   (= arrow/mixed-bounded-sql-put-profile (:profile independent))
                   (= :matched (:iid-correspondence independent)))
        (assoc base :status :unsupported :reason :independent-operation-unsupported
               :decode-reason (:reason independent))
        (let [observed (provider/observe-operation-bytes! node (aclone sample))]
          (cond
            (not= :operation-bytes-observed (:status observed))
            (assoc base :status (if (= :unsupported (:status observed)) :unsupported :unavailable)
                   :reason :provider-operation-unavailable :decode-reason (:reason observed))
            (not= provider/operation-bytes-profile (:provider-profile observed))
            (assoc base :status :unsupported :reason :unexpected-provider-profile)
            (not= (:operations independent) (:operations observed))
            (assoc base :status :mismatch :reason :operation-correspondence-mismatch :field :operations)
            (not= (:logged-operation-spans independent) (:logged-operation-spans observed))
            (assoc base :status :mismatch :reason :operation-correspondence-mismatch :field :logged-operation-spans)
            :else
            (assoc base :status :matched :operation-correspondence :matched
                   :decoder-profile arrow/mixed-bounded-sql-put-profile
                   :provider-profile provider/operation-bytes-profile)))))))
