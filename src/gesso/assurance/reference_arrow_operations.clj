(ns gesso.assurance.reference-arrow-operations
  "Selected XTDB 2.2.0-beta1 SQL-only Arrow operation stream decoder.
   Use Apache Arrow FlatBuffer metadata accessors, not XTDB's relation or
   operation reader. Interpret selected data buffers directly. Returned
   SQL and nested argument bytes are sensitive internal observations."
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets CharacterCodingException)
           (java.util UUID)
           (org.apache.arrow.flatbuf Message MessageHeader MetadataVersion
                                    Schema RecordBatch Field Type Union UnionMode Endianness Int FixedSizeBinary)))

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

(defn- buffers! [^RecordBatch batch ^ByteBuffer body expected-count]
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
           query-offsets queries ^ByteBuffer arg-valid arg-offsets args] (buffers! batch body 9)]
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

(def parameterless-profile :xtdb-2.2.0-beta1/selected-parameterless-arrow-operations-v1)
(def ^:private parameterless-base (assoc base :profile parameterless-profile))

(defn- parameterless-rows! [bytes]
  ;; An absent slot represents one invocation. An explicit zero-column
  ;; relation carries its own count, including coalesced invocations.
  (if (nil? bytes)
    1
    (do
      (require! (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes))
                :arrow-argument-byte-limit)
      (let [stream (doto (ByteBuffer/wrap ^bytes bytes) (.order ByteOrder/LITTLE_ENDIAN))
            [^Message sm ^ByteBuffer sb] (message! stream MessageHeader/Schema)
            ^Schema schema (.header sm (Schema.))]
        (require! (and schema (zero? (.remaining sb))
                       (= Endianness/Little (.endianness schema))
                       (zero? (.fieldsLength schema)) (zero? (.featuresLength schema))
                       (zero? (.customMetadataLength schema)))
                  :unsupported-parameterless-schema)
        (let [[^Message bm ^ByteBuffer bb] (message! stream MessageHeader/RecordBatch)
              ^RecordBatch batch (.header bm (RecordBatch.))]
          (require! (and batch (zero? (.remaining bb))
                         (zero? (.nodesLength batch)) (zero? (.buffersLength batch))
                         (nil? (.compression batch)) (zero? (.variadicBufferCountsLength batch)))
                    :unsupported-parameterless-batch)
          (let [n (.length batch)]
            (require! (<= 1 n max-rows) :arrow-argument-row-limit)
            (require! (and (= 4 (.remaining stream)) (zero? (.getInt stream)))
                      :unsupported-arrow-terminator)
            n))))))

(defn decode-parameterless-arguments
  "Decode an absent argument slot or one selected zero-column Arrow relation.
   Return the exact invocation count. This does not check SQL arity."
  [bytes]
  (try
    (assoc parameterless-base :status :decoded
           :encoding (if (nil? bytes) :absent :zero-column-relation)
           :row-count (parameterless-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc parameterless-base :status :unsupported :reason (:reason (ex-data e))))
    (catch Exception _
      (assoc parameterless-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-parameterless-operations
  "Expand selected SQL groups into ordered invocations with empty argument
   vectors. Reject the entire stream if any group is unsupported or total
   expanded rows exceed the bound. SQL arity, nonempty parameter relations,
   execution semantics and provider refinement remain open."
  [bytes]
  (let [decoded (decode-operations bytes)]
    (if-not (= :decoded (:status decoded))
      (assoc parameterless-base :status :unsupported :reason (:reason decoded))
      (try
        (let [groups (:groups decoded)
              spans (mapv #(parameterless-rows! (:argument-bytes %)) groups)]
          (require! (<= (reduce + 0 spans) max-rows) :arrow-expanded-row-limit)
          (assoc parameterless-base :status :decoded :logged-operation-spans spans
                 :operations (into [] cat
                                   (map (fn [{:keys [query]} n]
                                          (repeat n {:kind :sql :query query :args []}))
                                        groups spans))))
        (catch clojure.lang.ExceptionInfo e
          (assoc parameterless-base :status :unsupported :reason (:reason (ex-data e))))
        (catch Exception _
          (assoc parameterless-base :status :unsupported :reason :invalid-arrow-stream))))))

(def integer-profile :xtdb-2.2.0-beta1/selected-int64-arrow-operations-v1)
(def ^:private integer-base (assoc base :profile integer-profile))
(def ^:private max-argument-columns 64)

(defn- scalar-kind! [^Field f i allowed]
  (let [type (.typeType f)
        extension? (pos? (.customMetadataLength f))
        kind (if extension?
               (let [_ (require! (= 2 (.customMetadataLength f)) :unsupported-arrow-field)
                     metadata (into {} (map (fn [j]
                                             (let [kv (.customMetadata f j)]
                                               [(.key kv) (.value kv)]))
                                           (range (.customMetadataLength f))))
                     name (get metadata "ARROW:extension:name")]
                 (require! (and (= 2 (.customMetadataLength f))
                                (= {"ARROW:extension:name" name
                                    "ARROW:extension:metadata" ""} metadata))
                           :unsupported-arrow-field)
                 (cond
                   (and (= Type/Utf8 type) (= "xt/clj-keyword" name)) :keyword
                   (and (= Type/FixedSizeBinary type) (= "uuid" name)) :uuid
                   :else (require! false :unsupported-arrow-field)))
               (get {Type/Int :integer Type/Bool :boolean Type/Utf8 :string} type))]
    (require! (contains? allowed kind) :unsupported-arrow-field)
    (if (#{:keyword :uuid} kind)
      (require! (and (= (str "?_" i) (.name f)) (not (.nullable f))
                     (zero? (.childrenLength f)) (nil? (.dictionary f)))
                :unsupported-arrow-field)
      (field! f (str "?_" i) type false 0))
    (when (= :integer kind)
      (let [^Int t (.type f (Int.))]
        (require! (and t (= 64 (.bitWidth t)) (.isSigned t)) :unsupported-integer-type)))
    (when (= :uuid kind)
      (let [^FixedSizeBinary t (.type f (FixedSizeBinary.))]
        (require! (and t (= 16 (.byteWidth t))) :unsupported-uuid-width)))
    kind))

(defn- keyword-value! [s]
  ;; Match the selected reference transaction keyword domain. The retained
  ;; symbol spelling has no leading colon. Reject before interning.
  (require! (and (<= 1 (count s) 4095)
                 (re-matches #"(?:[a-zA-Z][a-zA-Z0-9_.-]*/)?[a-zA-Z][a-zA-Z0-9_-]*" s))
            :unsupported-arrow-keyword)
  (keyword s))

(defn- scalar-rows! [bytes allowed]
  (if (nil? bytes)
    [[]]
    (do
      (require! (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes))
                :arrow-argument-byte-limit)
      (let [stream (doto (ByteBuffer/wrap ^bytes bytes) (.order ByteOrder/LITTLE_ENDIAN))
            [^Message sm ^ByteBuffer sb] (message! stream MessageHeader/Schema)
            ^Schema schema (.header sm (Schema.))
            columns (.fieldsLength schema)]
        (require! (and (zero? (.remaining sb)) (= Endianness/Little (.endianness schema))
                       (zero? (.featuresLength schema)) (zero? (.customMetadataLength schema))
                       (<= 0 columns max-argument-columns)) :unsupported-integer-schema)
        (let [kinds (mapv #(scalar-kind! (.fields schema %) % allowed) (range columns))
              starts (vec (reductions + 0 (map #(if (#{:string :keyword} %) 3 2) kinds)))]
          (let [[^Message bm ^ByteBuffer bb] (message! stream MessageHeader/RecordBatch)
                ^RecordBatch batch (.header bm (RecordBatch.))
                n (.length batch)]
            (require! (<= 1 n max-rows) :arrow-argument-row-limit)
            (require! (and (= columns (.nodesLength batch)) (nil? (.compression batch))
                           (zero? (.variadicBufferCountsLength batch))) :unsupported-integer-batch)
            (let [buffers (buffers! batch bb (peek starts))
                  readers
                  (mapv (fn [i]
                          (let [node (.nodes batch i)
                                start (nth starts i)
                                kind (nth kinds i)
                                ^ByteBuffer validity (nth buffers start)
                                ^ByteBuffer values (nth buffers (inc start))]
                            (require! (and (= n (.length node)) (zero? (.nullCount node))
                                           (zero? (.limit validity))) :unsupported-integer-buffer-layout)
                            (case kind
                              (:string :keyword)
                              (let [^ByteBuffer data (nth buffers (+ start 2))
                                    offsets (offsets! values data n)]
                                (fn [row]
                                  (let [s (str (.decode (.newDecoder StandardCharsets/UTF_8)
                                                        (ByteBuffer/wrap (value-bytes! data offsets row))))]
                                    (if (= :keyword kind) (keyword-value! s) s))))
                              :boolean
                              (do (require! (= (quot (+ n 7) 8) (.limit values))
                                            :unsupported-integer-buffer-layout)
                                  #(present? values %))
                              :uuid
                              (do (require! (= (* 16 n) (.limit values))
                                            :unsupported-uuid-buffer-layout)
                                  ;; Fixed binary is a byte string, not an Arrow
                                  ;; little-endian numeric value.
                                  (let [v (doto (.duplicate values) (.order ByteOrder/BIG_ENDIAN))]
                                    (fn [row]
                                      (let [offset (* 16 row)]
                                        (UUID. (.getLong v offset) (.getLong v (+ offset 8)))))))
                              :integer
                              (do (require! (= (* 8 n) (.limit values))
                                            :unsupported-integer-buffer-layout)
                                  #(.getLong values (* 8 %))))))
                        (range columns))]
              (require! (and (= 4 (.remaining stream)) (zero? (.getInt stream)))
                        :unsupported-arrow-terminator)
              (mapv (fn [row] (mapv #(% row) readers)) (range n)))))))))

(defn- fixed-rows! [bytes allow-boolean?]
  (scalar-rows! bytes (if allow-boolean? #{:integer :boolean} #{:integer})))

(defn- integer-rows! [bytes] (fixed-rows! bytes false))

(defn decode-integer-arguments
  "Decode selected nonnullable signed int64 columns in positional order,
   or a parameterless relation. No numeric coercion or SQL arity claim."
  [bytes]
  (try
    (assoc integer-base :status :decoded :rows (integer-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc integer-base :status :unsupported :reason (:reason (ex-data e))))
    (catch Exception _ (assoc integer-base :status :unsupported :reason :invalid-arrow-stream))))

(defn- expand-fixed-operations [bytes result-base rows!]
  (let [decoded (decode-operations bytes)]
    (if-not (= :decoded (:status decoded))
      (assoc result-base :status :unsupported :reason (:reason decoded))
      (try
        (let [{:keys [operations spans]}
              (reduce (fn [{:keys [operations spans]} {:keys [query argument-bytes]}]
                        (let [rows (rows! argument-bytes)]
                          (require! (<= (+ (count operations) (count rows)) max-rows)
                                    :arrow-expanded-row-limit)
                          {:operations (into operations (map #(hash-map :kind :sql :query query :args %) rows))
                           :spans (conj spans (count rows))}))
                      {:operations [] :spans []} (:groups decoded))]
          (assoc result-base :status :decoded :operations operations :logged-operation-spans spans))
        (catch clojure.lang.ExceptionInfo e
          (assoc result-base :status :unsupported :reason (:reason (ex-data e))))
        (catch CharacterCodingException _
          (assoc result-base :status :unsupported :reason :invalid-arrow-utf8))
        (catch Exception _ (assoc result-base :status :unsupported :reason :invalid-arrow-stream))))))

(defn decode-integer-operations
  "Expand selected SQL groups with int64 or empty argument rows. Fail
   closed on unsupported types, layouts or excess total invocations."
  [bytes]
  (expand-fixed-operations bytes integer-base integer-rows!))

(def boolean-profile :xtdb-2.2.0-beta1/selected-int64-boolean-arrow-operations-v1)
(def ^:private boolean-base (assoc base :profile boolean-profile))

(defn decode-boolean-arguments
  "Decode selected nonnullable int64 and bit-packed Boolean positional
   columns, including mixed columns and zero-column relations."
  [bytes]
  (try
    (assoc boolean-base :status :decoded :rows (fixed-rows! bytes true))
    (catch clojure.lang.ExceptionInfo e
      (assoc boolean-base :status :unsupported :reason (:reason (ex-data e))))
    (catch Exception _ (assoc boolean-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-boolean-operations
  "Expand selected SQL groups with Boolean, int64 or empty argument rows.
   No nullable columns, SQL arity or execution semantics claim."
  [bytes]
  (expand-fixed-operations bytes boolean-base #(fixed-rows! % true)))

(def string-profile :xtdb-2.2.0-beta1/selected-int64-boolean-utf8-arrow-operations-v1)
(def ^:private string-base (assoc base :profile string-profile))

(defn- string-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string}))

(defn decode-string-arguments
  "Decode selected nonnullable UTF-8, int64 and Boolean positional columns.
   Validate exact offsets and decode each UTF-8 value strictly."
  [bytes]
  (try
    (assoc string-base :status :decoded :rows (string-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc string-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc string-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc string-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-string-operations
  "Expand selected SQL groups with UTF-8, Boolean, int64 or empty rows.
   No nullable columns, coercion, SQL arity or execution semantics claim."
  [bytes]
  (expand-fixed-operations bytes string-base string-rows!))

(def keyword-profile :xtdb-2.2.0-beta1/selected-int64-boolean-utf8-keyword-arrow-operations-v1)
(def ^:private keyword-base (assoc base :profile keyword-profile))

(defn- keyword-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword}))

(defn decode-keyword-arguments
  "Decode selected keyword extension columns and earlier scalar types.
   Require exact extension metadata and the restricted keyword grammar."
  [bytes]
  (try
    (assoc keyword-base :status :decoded :rows (keyword-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc keyword-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc keyword-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc keyword-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-keyword-operations
  "Expand selected SQL groups with keyword, UTF-8, Boolean, int64 or empty
   rows. Other extensions, nullable columns and SQL semantics are excluded."
  [bytes]
  (expand-fixed-operations bytes keyword-base keyword-rows!))

(def uuid-profile :xtdb-2.2.0-beta1/selected-int64-boolean-utf8-keyword-uuid-arrow-operations-v1)
(def ^:private uuid-base (assoc base :profile uuid-profile))

(defn- uuid-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid}))

(defn decode-uuid-arguments
  "Decode selected UUID extension columns and earlier scalar types.
   Require exact extension metadata, fixed width and big-endian UUID bytes."
  [bytes]
  (try
    (assoc uuid-base :status :decoded :rows (uuid-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc uuid-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc uuid-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc uuid-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-uuid-operations
  "Expand selected SQL groups including UUIDs and earlier scalar rows.
   Other extensions, nullable columns and SQL semantics are excluded."
  [bytes]
  (expand-fixed-operations bytes uuid-base uuid-rows!))
