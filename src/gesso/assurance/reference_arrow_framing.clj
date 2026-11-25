(ns gesso.assurance.reference-arrow-framing
  "Internal bounded Arrow IPC framing and buffer qualification primitives."
  (:import (java.nio ByteBuffer ByteOrder)
           (org.apache.arrow.flatbuf Message MetadataVersion RecordBatch Field)))

(def max-bytes (* 16 1024 1024))
(def max-rows 16384)

(defn require! [condition reason]
  (when-not condition (throw (ex-info "Unsupported selected Arrow stream" {:reason reason}))))

(defn- slice! ^ByteBuffer [^ByteBuffer b n]
  (require! (<= 0 n (.remaining b)) :invalid-arrow-length)
  (let [part (doto (.slice b) (.limit (int n)) (.order ByteOrder/LITTLE_ENDIAN))]
    (.position b (+ (.position b) (int n)))
    part))

(defn message! [^ByteBuffer stream kind]
  (require! (and (<= 8 (.remaining stream)) (= -1 (.getInt stream))) :invalid-arrow-framing)
  (let [n (.getInt stream)]
    (require! (and (pos? n) (zero? (mod n 8))) :invalid-arrow-metadata-length)
    (let [m (Message/getRootAsMessage (slice! stream n))
          size (.bodyLength m)]
      (require! (and (= MetadataVersion/V5 (.version m)) (= kind (.headerType m))
                     (zero? (.customMetadataLength m))) :unsupported-arrow-message)
      (require! (and (<= 0 size max-bytes) (zero? (mod size 8))) :invalid-arrow-body-length)
      [m (slice! stream size)])))

(defn field! [^Field field name type nullable? children]
  (require! (and field (= name (.name field)) (= type (.typeType field))
                 (= nullable? (.nullable field)) (= children (.childrenLength field))
                 (nil? (.dictionary field)) (zero? (.customMetadataLength field)))
            :unsupported-arrow-field)
  field)

(defn buffers! [^RecordBatch batch ^ByteBuffer body expected-count]
  (require! (= expected-count (.buffersLength batch)) :unsupported-arrow-buffer-count)
  (loop [i 0 end 0 out []]
    (if (= i expected-count)
      (do (require! (= (.limit body) (+ end (mod (- end) 8))) :invalid-arrow-body-extent) out)
      (let [b (.buffers batch i) offset (.offset b) length (.length b)]
        (require! (and (= offset (+ end (mod (- end) 8)))
                       (<= 0 offset (.limit body)) (<= 0 length (- (.limit body) offset)))
                  :invalid-arrow-buffer-extent)
        (let [copy (.duplicate body)]
          (.position copy (int offset))
          (recur (inc i) (+ offset length) (conj out (slice! copy length))))))))

(defn offsets! [^ByteBuffer offsets ^ByteBuffer values n]
  (require! (= (* 4 (inc n)) (.remaining offsets)) :invalid-arrow-offset-count)
  (let [xs (mapv #(.getInt offsets (* 4 %)) (range (inc n)))]
    (require! (and (zero? (first xs)) (= (.limit values) (peek xs)) (apply <= xs))
              :invalid-arrow-value-offsets)
    xs))

(defn value-bytes! [^ByteBuffer values offsets i]
  (let [start (nth offsets i) end (nth offsets (inc i))
        b (.duplicate values) result (byte-array (- end start))]
    (.position b (int start))
    (.get b result)
    result))

(defn present? [^ByteBuffer bitmap i]
  (or (zero? (.limit bitmap))
      (not (zero? (bit-and (.get bitmap (quot i 8)) (bit-shift-left 1 (mod i 8)))))))

