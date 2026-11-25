(ns gesso.assurance.reference-source-payload
  "Independent bounded wire decoding of selected XTDB 2.2.0-beta1 source
   envelopes. No XTDB or generated protobuf decoder is used. Raw operation
   and metadata bytes are sensitive internal observations, not receipts."
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets CharacterCodingException)
           (java.time Instant ZoneId DateTimeException)))

(def profile :xtdb-2.2.0-beta1/selected-source-payload-v1)
(def ^:private max-bytes (* 16 1024 1024))
(def ^:private base {:profile profile :authority :none :certificate :not-issued})

(defn- reject! [reason]
  (throw (ex-info "Unsupported selected source payload" {:reason reason})))

(defn- read-varint! [^ByteBuffer buf]
  ;; Canonical unsigned wire encoding of a 64-bit bit pattern, interpreted
  ;; as signed for int64 fields. The tenth group has at most one bit.
  (loop [i 0 acc 0]
    (when-not (.hasRemaining buf) (reject! :truncated-varint))
    (let [b (bit-and 255 (.get buf))
          bits (bit-and b 127)]
      (when (and (= i 9) (> b 1)) (reject! :varint-overflow))
      (let [acc (bit-or acc (bit-shift-left bits (* 7 i)))]
        (if (< b 128)
          (do (when (and (pos? i) (zero? bits)) (reject! :noncanonical-varint))
              acc)
          (recur (inc i) acc))))))

(defn- read-bytes! [^ByteBuffer buf]
  (let [n (read-varint! buf)]
    (when-not (<= 0 n (.remaining buf)) (reject! :invalid-field-length))
    (let [bytes (byte-array (int n))]
      (.get buf bytes)
      bytes)))

(defn- utf8! [^bytes bytes]
  ;; The new decoder reports malformed/unmappable input by default.
  (str (.decode (.newDecoder StandardCharsets/UTF_8) (ByteBuffer/wrap bytes))))

(defn- fields! [^ByteBuffer buf schema]
  ;; Exact allowed wire tags, not a permissive protobuf merge. Unknown,
  ;; duplicate and wrong-wire-type fields are outside this selected profile.
  (loop [out {}]
    (if-not (.hasRemaining buf)
      out
      (let [tag (read-varint! buf)
            [k kind] (get schema tag)]
        (when-not k (reject! :unsupported-field))
        (when (contains? out k) (reject! :duplicate-field))
        (recur (assoc out k (case kind
                             :int64 (read-varint! buf)
                             :bytes (read-bytes! buf)
                             :string (utf8! (read-bytes! buf)))))))))

(defn- micros->instant [n]
  (Instant/ofEpochSecond (Math/floorDiv (long n) 1000000)
                         (* 1000 (Math/floorMod (long n) 1000000))))

(defn- tx! [^bytes body]
  (let [fields (fields! (ByteBuffer/wrap body)
                        {10 [:tx-ops :bytes] 16 [:system-time-micros :int64]
                         26 [:default-tz :string] 34 [:user :string]
                         42 [:user-metadata :bytes]})]
    (when-not (and (contains? fields :tx-ops)
                   (pos? (alength ^bytes (:tx-ops fields)))
                   (contains? fields :default-tz))
      (reject! :missing-transaction-field))
    (when (and (contains? fields :user-metadata)
               (zero? (alength ^bytes (:user-metadata fields))))
      ;; Provider collapses empty metadata into absent; do not silently
      ;; erase that distinction in an independent selected observation.
      (reject! :empty-user-metadata))
    (assoc (dissoc fields :system-time-micros)
           :kind :transaction
           :default-tz (ZoneId/of (:default-tz fields))
           :system-time (some-> (:system-time-micros fields) micros->instant))))

(defn decode-payload
  "Decode exactly one selected source envelope, with canonical varints and
   strict field presence. Supports transaction (no external source token)
   and flush-block with explicit expected block index. Other formats and
   message variants fail closed. Arrow IPC, metadata semantics, source
   coverage, and provider refinement are not established by this decoder."
  [bytes]
  (if-not (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes))
    (assoc base :status :unsupported :reason :source-payload-byte-limit)
    (try
      (let [buf (ByteBuffer/wrap ^bytes bytes)]
        (when-not (= 3 (.get buf)) (reject! :unsupported-source-header))
        (let [fields (fields! buf {10 [:flush-block :bytes] 50 [:transaction :bytes]})]
          (when-not (= 1 (count fields)) (reject! :ambiguous-source-message))
          (assoc base :status :decoded
                 :message (if-let [body (:transaction fields)]
                            (tx! body)
                            (let [flush (fields! (ByteBuffer/wrap ^bytes (:flush-block fields))
                                                 {16 [:expected-block-index :int64]})]
                              (when-not (contains? flush :expected-block-index)
                                (reject! :missing-flush-field))
                              (assoc flush :kind :flush-block))))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :unsupported :reason (:reason (ex-data e))))
      (catch CharacterCodingException _
        (assoc base :status :unsupported :reason :invalid-source-utf8))
      (catch DateTimeException _
        (assoc base :status :unsupported :reason :invalid-source-time-or-zone)))))
