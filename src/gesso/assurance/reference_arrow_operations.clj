(ns gesso.assurance.reference-arrow-operations
  "Selected XTDB 2.2.0-beta1 SQL-only Arrow operation stream decoder.
   Use Apache Arrow FlatBuffer metadata accessors, not XTDB's relation or
   operation reader. Interpret selected data buffers directly. Returned
   SQL and nested argument bytes are sensitive internal observations."
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets CharacterCodingException)
           (org.apache.arrow.flatbuf Message MessageHeader MetadataVersion
                                    Schema RecordBatch Field Type Union UnionMode Endianness)))

(def profile :xtdb-2.2.0-beta1/selected-sql-arrow-operations-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
(def ^:private max-bytes (* 16 1024 1024))
(def ^:private max-rows 16384)

(defn- require! [condition reason]
  (when-not condition (throw (ex-info "Unsupported selected Arrow stream" {:reason reason}))))

(defn- slice! ^ByteBuffer [^ByteBuffer b n]
  (require! (<= 0 n (.remaining b)) :invalid-arrow-length)
  (let [part (doto (.slice b) (.limit (int n)) (.order ByteOrder/LITTLE_ENDIAN))]
    (.position b (+ (.position b) (int n)))
    part))

(defn- message! [^ByteBuffer stream kind]
  (require! (and (<= 8 (.remaining stream)) (= -1 (.getInt stream))) :invalid-arrow-framing)
  (let [n (.getInt stream)]
    (require! (and (pos? n) (zero? (mod n 8))) :invalid-arrow-metadata-length)
    (let [m (Message/getRootAsMessage (slice! stream n))
          size (.bodyLength m)]
      (require! (and (= MetadataVersion/V5 (.version m)) (= kind (.headerType m))
                     (zero? (.customMetadataLength m))) :unsupported-arrow-message)
      (require! (and (<= 0 size max-bytes) (zero? (mod size 8))) :invalid-arrow-body-length)
      [m (slice! stream size)])))

(defn- field! [^Field field name type nullable? children]
  (require! (and field (= name (.name field)) (= type (.typeType field))
                 (= nullable? (.nullable field)) (= children (.childrenLength field))
                 (nil? (.dictionary field)) (zero? (.customMetadataLength field)))
            :unsupported-arrow-field)
  field)

(defn- schema! [^Message m ^ByteBuffer body]
  (let [^Schema schema (.header m (Schema.))]
    (require! (and (zero? (.remaining body)) schema (= Endianness/Little (.endianness schema))
                   (= 1 (.fieldsLength schema)) (zero? (.featuresLength schema))
                   (zero? (.customMetadataLength schema))) :unsupported-arrow-schema)
    (let [^Field root (field! (.fields schema 0) "tx-ops" Type/Union false 1)
          ^Union union (.type root (Union.))
          ^Field sql (field! (.children root 0) "sql" Type/Struct_ false 2)]
      ;; Installed XTDB emits implicit zero-based type ids (empty array).
      (require! (and union (= UnionMode/Dense (.mode union)) (zero? (.typeIdsLength union)))
                :unsupported-arrow-union)
      (field! (.children sql 0) "query" Type/Utf8 false 0)
      (let [^Field args (.children sql 1) nullable? (.nullable args)]
        (field! args "args" Type/Binary nullable? 0)
        nullable?))))

(defn- buffers! [^RecordBatch batch ^ByteBuffer body]
  (require! (= 9 (.buffersLength batch)) :unsupported-arrow-buffer-count)
  (loop [i 0 end 0 out []]
    (if (= i 9)
      (do (require! (= (.limit body) (+ end (mod (- end) 8))) :invalid-arrow-body-extent) out)
      (let [b (.buffers batch i) offset (.offset b) length (.length b)]
        (require! (and (= offset (+ end (mod (- end) 8)))
                       (<= 0 offset (.limit body)) (<= 0 length (- (.limit body) offset)))
                  :invalid-arrow-buffer-extent)
        (let [copy (.duplicate body)]
          (.position copy (int offset))
          (recur (inc i) (+ offset length) (conj out (slice! copy length))))))))

(defn- offsets! [^ByteBuffer offsets ^ByteBuffer values n]
  (require! (= (* 4 (inc n)) (.remaining offsets)) :invalid-arrow-offset-count)
  (let [xs (mapv #(.getInt offsets (* 4 %)) (range (inc n)))]
    (require! (and (zero? (first xs)) (= (.limit values) (peek xs)) (apply <= xs))
              :invalid-arrow-value-offsets)
    xs))

(defn- value-bytes! [^ByteBuffer values offsets i]
  (let [start (nth offsets i) end (nth offsets (inc i))
        b (.duplicate values) result (byte-array (- end start))]
    (.position b (int start))
    (.get b result)
    result))

(defn- present? [^ByteBuffer bitmap i]
  (or (zero? (.limit bitmap))
      (not (zero? (bit-and (.get bitmap (quot i 8)) (bit-shift-left 1 (mod i 8)))))))

(defn- batch! [^Message m ^ByteBuffer body nullable?]
  (let [^RecordBatch batch (.header m (RecordBatch.)) n (.length batch)]
    (require! (and (<= 1 n max-rows) (= 4 (.nodesLength batch))
                   (nil? (.compression batch)) (zero? (.variadicBufferCountsLength batch)))
              :unsupported-arrow-batch)
    (let [nodes (mapv #(let [node (.nodes batch %)] [(.length node) (.nullCount node)]) (range 4))
          _ (require! (and (= [n n n n] (mapv first nodes))
                           (= [-1 0 0] (mapv second (subvec nodes 0 3)))
                           (<= -1 (second (peek nodes)) n)) :unsupported-arrow-nodes)
          [^ByteBuffer tags ^ByteBuffer indexes ^ByteBuffer struct-valid ^ByteBuffer query-valid
           query-offsets queries ^ByteBuffer arg-valid arg-offsets args] (buffers! batch body)]
      (require! (and (= n (.limit tags)) (= (* 4 n) (.limit indexes))
                     (zero? (.limit struct-valid)) (zero? (.limit query-valid))
                     (or (zero? (.limit arg-valid)) (= (quot (+ n 7) 8) (.limit arg-valid))))
                :unsupported-arrow-buffer-layout)
      (let [qs (offsets! query-offsets queries n) as (offsets! arg-offsets args n)
            nulls (count (remove #(present? arg-valid %) (range n)))]
        (require! (and (or nullable? (zero? nulls))
                       (or (= -1 (second (peek nodes))) (= nulls (second (peek nodes)))))
                  :arrow-null-count-mismatch)
        (mapv (fn [i]
                (require! (and (zero? (.get tags i)) (= i (.getInt indexes (* 4 i)))) :unsupported-arrow-union-selection)
                (let [query (str (.decode (.newDecoder StandardCharsets/UTF_8)
                                          (ByteBuffer/wrap (value-bytes! queries qs i))))
                      arg (value-bytes! args as i)]
                  (require! (not (empty? query)) :empty-arrow-query)
                  (require! (if (present? arg-valid i) (pos? (alength arg)) (zero? (alength arg)))
                            :unsupported-arrow-argument-slot)
                  {:kind :sql :query query :argument-bytes (when (present? arg-valid i) arg)}))
              (range n))))))

(defn decode-operations
  "Decode one selected SQL-only stream into ordered SQL groups. Nested
   argument IPC remains opaque. No Arrow metadata verification proof,
   mixed operation kinds, SQL semantics or provider refinement claim."
  [bytes]
  (if-not (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes))
    (assoc base :status :unsupported :reason :arrow-byte-limit)
    (try
      (let [stream (doto (ByteBuffer/wrap ^bytes bytes) (.order ByteOrder/LITTLE_ENDIAN))
            [sm sb] (message! stream MessageHeader/Schema)
            nullable? (schema! sm sb)
            [bm bb] (message! stream MessageHeader/RecordBatch)
            groups (batch! bm bb nullable?)]
        ;; XTDB's installed writer uses a four-byte zero terminator.
        (require! (and (= 4 (.remaining stream)) (zero? (.getInt stream))) :unsupported-arrow-terminator)
        (assoc base :status :decoded :groups groups))
      (catch clojure.lang.ExceptionInfo e (assoc base :status :unsupported :reason (:reason (ex-data e))))
      (catch CharacterCodingException _ (assoc base :status :unsupported :reason :invalid-arrow-utf8))
      (catch Exception _ (assoc base :status :unsupported :reason :invalid-arrow-stream)))))
