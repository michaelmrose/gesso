(ns gesso.assurance.reference-arrow-operations
  "Selected XTDB 2.2.0-beta1 SQL-only Arrow operation stream decoder.
   Use Apache Arrow FlatBuffer metadata accessors, not XTDB's relation or
   operation reader. Interpret selected data buffers directly. Returned
   SQL and nested argument bytes are sensitive internal observations."
  (:require [gesso.assurance.reference-arrow-framing :refer [max-bytes max-rows require! message! field! buffers! offsets! value-bytes! present?]]
            [gesso.assurance.reference-arrow-schema :refer [scalar-kind!]])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets CharacterCodingException)
           (java.util UUID)
           (java.time Instant ZoneId)
           (org.apache.arrow.flatbuf Message MessageHeader
                                    Schema RecordBatch Field Type Union UnionMode Endianness)))

(def profile :xtdb-2.2.0-beta1/selected-sql-arrow-operations-v1)
(def ^:private base {:profile profile :authority :none :certificate :not-issued})
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

(def ^:private timestamp-min (Instant/parse "0001-01-01T00:00:00Z"))
(def ^:private timestamp-end (Instant/parse "9999-01-01T00:00:00Z"))

(defn- timestamp-value! [micros zone]
  (let [instant (Instant/ofEpochSecond (Math/floorDiv (long micros) (long 1000000))
                                       (* 1000 (Math/floorMod (long micros) (long 1000000))))]
    (require! (and (not (.isBefore instant timestamp-min)) (.isBefore instant timestamp-end))
              :unsupported-arrow-timestamp-range)
    (.atZone instant zone)))

(defn- keyword-value! [s]
  ;; Match the selected reference transaction keyword domain. The retained
  ;; symbol spelling has no leading colon. Reject before interning.
  (require! (and (<= 1 (count s) 4095)
                 (re-matches #"(?:[a-zA-Z][a-zA-Z0-9_.-]*/)?[a-zA-Z][a-zA-Z0-9_-]*" s))
            :unsupported-arrow-keyword)
  (keyword s))

(defn- list-kind? [kind] (and (vector? kind) (= :list (first kind))))
(defn- union-kind? [kind] (and (vector? kind) (= :union (first kind))))
(defn- struct-kind? [kind] (and (vector? kind) (= :struct (first kind))))

(defn- nullable-kind? [kind] (and (vector? kind) (= :nullable (first kind))))
(def ^:private absent-field (Object.))

(defn- node-count [kind]
  (cond (nullable-kind? kind) (node-count (second kind))
        (list-kind? kind) (inc (node-count (second kind)))
        (union-kind? kind) (inc (count (second kind)))
        (struct-kind? kind) (inc (reduce + 0 (map (comp node-count second) (second kind))))
        :else 1))

(defn- buffer-count [kind]
  (cond (nullable-kind? kind) (buffer-count (second kind))
        (#{:empty-child :null-only-field} kind) 0
        (list-kind? kind) (+ 2 (buffer-count (second kind)))
        (union-kind? kind) (+ 2 (reduce + (map buffer-count (second kind))))
        (struct-kind? kind) (inc (reduce + 0 (map (comp buffer-count second) (second kind))))
        :else (if (#{:string :keyword} kind) 3 2)))

(defn- scalar-reader! [kind buffers start n]
  (let [^ByteBuffer values (nth buffers (inc start))]
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
        (:timestamp :timestamp-utc)
        (do (require! (= (* 8 n) (.limit values))
                      :unsupported-arrow-timestamp-buffer-layout)
            (let [zone (ZoneId/of (if (= :timestamp-utc kind) "UTC" "Z"))]
              #(timestamp-value! (.getLong values (* 8 %)) zone)))
        :integer
        (do (require! (= (* 8 n) (.limit values))
                      :unsupported-integer-buffer-layout)
            #(.getLong values (* 8 %))))))

(defn- union-reader! [kinds ^RecordBatch batch node-index buffers start n]
  (let [^ByteBuffer tags (nth buffers start)
        ^ByteBuffer indexes (nth buffers (inc start))
        starts (vec (reductions + (+ start 2) (map buffer-count kinds)))
        counts (mapv #(.length (.nodes batch (+ node-index 1 %))) (range (count kinds)))]
    (require! (and (= n (.limit tags)) (= (* 4 n) (.limit indexes))
                   (every? #(<= 1 % n) counts) (= n (reduce + counts)))
              :unsupported-arrow-list-union-layout)
    (let [readers (mapv (fn [i kind]
                          (let [node (.nodes batch (+ node-index 1 i))
                                pos (nth starts i)]
                            (require! (and (zero? (.nullCount node))
                                           (zero? (.limit ^ByteBuffer (nth buffers pos))))
                                      :unsupported-arrow-list-union-layout)
                            (scalar-reader! kind buffers pos (nth counts i))))
                        (range (count kinds)) kinds)
          consumed (reduce (fn [seen i]
                             (let [tag (int (.get tags i)) index (.getInt indexes (* 4 i))]
                               (require! (and (<= 0 tag (dec (count kinds)))
                                              (= index (nth seen tag))
                                              (< index (nth counts tag)))
                                         :unsupported-arrow-list-union-selection)
                               (update seen tag inc)))
                           (vec (repeat (count kinds) 0)) (range n))]
      (require! (= consumed counts) :unsupported-arrow-list-union-selection)
      (fn [i] ((nth readers (int (.get tags i))) (.getInt indexes (* 4 i)))))))

(defn- list-reader! [kind ^RecordBatch batch node-index buffers start n]
  (let [child (.nodes batch (inc node-index))
        child-count (.length child)
        ^ByteBuffer offsets (nth buffers (inc start))
        union? (union-kind? kind)
        empty? (= :empty-child kind)]
    (require! (and (<= 0 child-count (min (quot max-bytes 8) (* 256 n)))
                   (cond union? (= -1 (.nullCount child))
                         empty? (and (zero? child-count) (zero? (.nullCount child)))
                         :else (and (zero? (.nullCount child))
                                    (zero? (.limit ^ByteBuffer (nth buffers (+ start 2))))))
                   (= (* 4 (inc n)) (.limit offsets)))
              :unsupported-arrow-list-buffer-layout)
    ;; Preserve the original integer-list buffer rejection reason.
    (when (= :integer kind)
      (require! (= (* 8 child-count) (.limit ^ByteBuffer (nth buffers (+ start 3))))
                :unsupported-arrow-list-buffer-layout))
    (let [xs (mapv #(.getInt offsets (* 4 %)) (range (inc n)))]
      (require! (and (zero? (first xs)) (= child-count (peek xs)) (apply <= xs))
                :invalid-arrow-list-offsets)
      (require! (every? #(<= (- (second %) (first %)) 256) (partition 2 1 xs))
                :arrow-list-element-limit)
      (let [read-child (cond empty? (fn [_] (require! false :unsupported-arrow-list-buffer-layout))
                             union? (union-reader! (second kind) batch (inc node-index) buffers (+ start 2) child-count)
                             (list-kind? kind) (list-reader! (second kind) batch (inc node-index) buffers (+ start 2) child-count)
                             :else (scalar-reader! kind buffers (+ start 2) child-count))]
        (fn [row]
          (mapv read-child (range (nth xs row) (nth xs (inc row)))))))))

(declare value-reader!)

(defn- struct-reader! [fields ^RecordBatch batch node-index buffers start n]
  (let [kinds (mapv second fields)
        node-starts (vec (reductions + (inc node-index) (map node-count kinds)))
        starts (vec (reductions + (inc start) (map buffer-count kinds)))
        readers (mapv (fn [i kind]
                        (value-reader! kind batch (nth node-starts i) buffers (nth starts i) n))
                      (range (count fields)) kinds)
        keys (mapv (comp keyword first) fields)]
    (fn [row]
      (into {} (keep (fn [[k reader]]
                       (let [v (reader row)]
                         (when-not (identical? absent-field v) [k v]))))
            (map vector keys readers)))))

(defn- struct-padding? [v]
  ;; Selected retained parent-null padding, not a general Arrow rule.
  (cond (map? v) (every? struct-padding? (vals v))
        (vector? v) (empty? v)
        (string? v) (empty? v)
        (integer? v) (zero? v)
        :else (false? v)))

(defn- nullable-reader! [kind ^RecordBatch batch node-index buffers start n]
  (let [node (.nodes batch node-index)
        ^ByteBuffer validity (nth buffers start)]
    (require! (and (= n (.length node))
                   (= (quot (+ n 7) 8) (.limit validity)))
              :unsupported-arrow-nullable-layout)
    (let [nulls (count (remove #(present? validity %) (range n)))
          reported (.nullCount node)]
      (require! (or (= -1 reported) (= nulls reported)) :arrow-null-count-mismatch))
    (when (#{:string :keyword} kind)
      (let [offsets (offsets! (nth buffers (inc start)) (nth buffers (+ start 2)) n)]
        (require! (every? #(or (present? validity %) (= (nth offsets %) (nth offsets (inc %))))
                          (range n))
                  :unsupported-arrow-nullable-value-extent)))
    (let [reader (cond
                   (struct-kind? kind)
                   (let [read-struct (struct-reader! (second kind) batch node-index buffers start n)]
                     (doseq [row (range n) :when (not (present? validity row))]
                       (require! (struct-padding? (read-struct row))
                                 :unsupported-arrow-nullable-struct-padding))
                     read-struct)
                   (list-kind? kind)
                   (let [read-list (list-reader! (second kind) batch node-index buffers start n)
                         ^ByteBuffer offsets (nth buffers (inc start))]
                     (require! (every? #(or (present? validity %)
                                           (= (.getInt offsets (* 4 %))
                                              (.getInt offsets (* 4 (inc %)))))
                                       (range n))
                               :unsupported-arrow-nullable-list-extent)
                     read-list)
                   :else (scalar-reader! kind buffers start n))]
      (fn [row] (if (present? validity row) (reader row) absent-field)))))

(defn- value-reader! [kind ^RecordBatch batch node-index buffers start n]
  (cond
    (= :null-only-field kind)
    (let [node (.nodes batch node-index)]
      (require! (and (= n (.length node)) (= n (.nullCount node)))
                :unsupported-arrow-null-only-layout)
      (constantly absent-field))
    (nullable-kind? kind)
    (nullable-reader! (second kind) batch node-index buffers start n)
    :else
    (let [node (.nodes batch node-index)
          ^ByteBuffer validity (nth buffers start)]
      (require! (and (= n (.length node)) (zero? (.nullCount node))
                     (zero? (.limit validity))) :unsupported-integer-buffer-layout)
      (cond (list-kind? kind) (list-reader! (second kind) batch node-index buffers start n)
            (struct-kind? kind) (struct-reader! (second kind) batch node-index buffers start n)
            :else (scalar-reader! kind buffers start n)))))

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
        (let [kinds (mapv #(scalar-kind! (.fields schema %) (str "?_" %) allowed) (range columns))
              starts (vec (reductions + 0 (map buffer-count kinds)))
              node-starts (vec (reductions + 0 (map node-count kinds)))]
          (let [[^Message bm ^ByteBuffer bb] (message! stream MessageHeader/RecordBatch)
                ^RecordBatch batch (.header bm (RecordBatch.))
                n (.length batch)]
            (require! (<= 1 n max-rows) :arrow-argument-row-limit)
            (require! (and (= (peek node-starts) (.nodesLength batch)) (nil? (.compression batch))
                           (zero? (.variadicBufferCountsLength batch))) :unsupported-integer-batch)
            (let [buffers (buffers! batch bb (peek starts))
                  readers
                  (mapv (fn [i]
                          (value-reader! (nth kinds i) batch (nth node-starts i)
                                         buffers (nth starts i) n))
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

(def timestamp-profile :xtdb-2.2.0-beta1/selected-z-micros-scalar-arrow-operations-v1)
(def ^:private timestamp-base (assoc base :profile timestamp-profile))

(defn- timestamp-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp}))

(defn decode-timestamp-arguments
  "Decode selected nonnullable Z-zone epoch-microsecond timestamp columns
   and earlier scalar values. Preserve the provider's ZonedDateTime value;
   reject other timezone labels, units and out-of-domain instants."
  [bytes]
  (try
    (assoc timestamp-base :status :decoded :rows (timestamp-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc timestamp-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc timestamp-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc timestamp-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-timestamp-operations
  "Expand selected SQL groups with Z-zone timestamps and earlier scalars.
   No SQL temporal semantics, coercion or general timezone support."
  [bytes]
  (expand-fixed-operations bytes timestamp-base timestamp-rows!))

(def utc-timestamp-profile :xtdb-2.2.0-beta1/selected-z-utc-micros-scalar-arrow-operations-v1)
(def ^:private utc-timestamp-base (assoc base :profile utc-timestamp-profile))

(defn- utc-timestamp-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc}))

(defn decode-utc-timestamp-arguments
  "Decode selected nonnullable epoch-microsecond columns with Z or UTC
   timezone labels. Preserve exact zone identity, not merely the offset."
  [bytes]
  (try
    (assoc utc-timestamp-base :status :decoded :rows (utc-timestamp-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc utc-timestamp-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc utc-timestamp-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc utc-timestamp-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-utc-timestamp-operations
  "Expand selected SQL groups with Z and UTC timestamps and earlier scalars.
   No other timezone, SQL temporal semantics or document decoding claim."
  [bytes]
  (expand-fixed-operations bytes utc-timestamp-base utc-timestamp-rows!))

(def int64-list-profile :xtdb-2.2.0-beta1/selected-int64-list-scalar-arrow-operations-v1)
(def ^:private int64-list-base (assoc base :profile int64-list-profile))

(defn- int64-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :int64-list}))

(defn decode-int64-list-arguments
  "Decode selected nonnullable homogeneous int64 vectors and earlier scalars.
   Bound each vector to 256 elements. Other child schemas remain excluded."
  [bytes]
  (try
    (assoc int64-list-base :status :decoded :rows (int64-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc int64-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc int64-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc int64-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-int64-list-operations
  "Expand selected SQL groups containing homogeneous int64 vectors and
   earlier scalar values. No heterogeneous, nested or nullable lists."
  [bytes]
  (expand-fixed-operations bytes int64-list-base int64-list-rows!))

(def scalar-list-profile :xtdb-2.2.0-beta1/selected-homogeneous-scalar-list-arrow-operations-v1)
(def ^:private scalar-list-base (assoc base :profile scalar-list-profile))

(defn- scalar-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :scalar-list}))

(defn decode-scalar-list-arguments
  "Decode bounded homogeneous nonnullable int64, Boolean, UTF-8, keyword
   and UUID vectors alongside earlier scalar columns. No recursive lists."
  [bytes]
  (try
    (assoc scalar-list-base :status :decoded :rows (scalar-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc scalar-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc scalar-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc scalar-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-scalar-list-operations
  "Expand selected SQL groups with homogeneous non-temporal scalar vectors
   and earlier scalar values. No heterogeneous or nullable list children."
  [bytes]
  (expand-fixed-operations bytes scalar-list-base scalar-list-rows!))

(def temporal-list-profile :xtdb-2.2.0-beta1/selected-homogeneous-temporal-scalar-list-arrow-operations-v1)
(def ^:private temporal-list-base (assoc base :profile temporal-list-profile))

(defn- temporal-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :temporal-list}))

(defn decode-temporal-list-arguments
  "Decode bounded homogeneous nonnullable scalar vectors including Z and UTC
   microsecond timestamps. Preserve exact child timezone labels and bounds."
  [bytes]
  (try
    (assoc temporal-list-base :status :decoded :rows (temporal-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc temporal-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc temporal-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc temporal-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-temporal-list-operations
  "Expand selected SQL groups with homogeneous scalar vectors including
   temporal children. No heterogeneous, nested or nullable children."
  [bytes]
  (expand-fixed-operations bytes temporal-list-base temporal-list-rows!))

(def union-list-profile :xtdb-2.2.0-beta1/selected-dense-union-scalar-list-arrow-operations-v1)
(def ^:private union-list-base (assoc base :profile union-list-profile))

(defn- union-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :union-list}))

(defn decode-union-list-arguments
  "Decode homogeneous vectors and selected heterogeneous vectors of int64,
   Boolean, UTF-8, keyword and UUID values. Require dense canonical child
   selections; no null, temporal union leg or recursive child support."
  [bytes]
  (try
    (assoc union-list-base :status :decoded :rows (union-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc union-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc union-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc union-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-union-list-operations
  "Expand selected SQL groups including bounded dense-union scalar lists."
  [bytes]
  (expand-fixed-operations bytes union-list-base union-list-rows!))

(def temporal-union-list-profile :xtdb-2.2.0-beta1/selected-temporal-dense-union-scalar-list-arrow-operations-v1)
(def ^:private temporal-union-list-base (assoc base :profile temporal-union-list-profile))

(defn- temporal-union-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :temporal-union-list}))

(defn decode-temporal-union-list-arguments
  "Decode selected heterogeneous scalar lists including the retained UTC
   microsecond timestamp leg. Decode retained zone identity, not submitted
   zone identity. Homogeneous Z and UTC lists remain supported."
  [bytes]
  (try
    (assoc temporal-union-list-base :status :decoded :rows (temporal-union-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc temporal-union-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc temporal-union-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc temporal-union-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-temporal-union-list-operations
  "Expand selected SQL groups including heterogeneous scalar and UTC timestamp lists."
  [bytes]
  (expand-fixed-operations bytes temporal-union-list-base temporal-union-list-rows!))

(def empty-list-profile :xtdb-2.2.0-beta1/selected-empty-and-scalar-list-arrow-operations-v1)
(def ^:private empty-list-base (assoc base :profile empty-list-profile))

(defn- empty-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :empty-list}))

(defn decode-empty-list-arguments
  "Decode selected scalar lists, including all-empty vectors with a zero-length
   Null child. Null-valued elements and top-level null arguments are excluded."
  [bytes]
  (try
    (assoc empty-list-base :status :decoded :rows (empty-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc empty-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc empty-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc empty-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-empty-list-operations
  "Expand selected SQL groups including all-empty and typed scalar vectors."
  [bytes]
  (expand-fixed-operations bytes empty-list-base empty-list-rows!))

(def recursive-list-profile :xtdb-2.2.0-beta1/selected-recursive-list-arrow-operations-v1)
(def ^:private recursive-list-base (assoc base :profile recursive-list-profile))

(defn- recursive-list-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc :empty-list :recursive-list}))

(defn decode-recursive-list-arguments
  "Decode selected homogeneous nesting of vectors up to eight list levels,
   with existing scalar, scalar-union or empty terminal children. This is
   physical diagnostic decoding, not admission to the reference domain."
  [bytes]
  (try
    (assoc recursive-list-base :status :decoded :rows (recursive-list-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc recursive-list-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc recursive-list-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc recursive-list-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-recursive-list-operations
  "Expand selected SQL groups including bounded homogeneous nesting of lists.
   No union leg containing a list or map, or reference document admission."
  [bytes]
  (expand-fixed-operations bytes recursive-list-base recursive-list-rows!))

(def struct-map-profile :xtdb-2.2.0-beta1/selected-struct-map-arrow-operations-v1)
(def ^:private struct-map-base (assoc base :profile struct-map-profile))

(defn- struct-map-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map}))

(defn decode-struct-map-arguments
  "Decode selected nonnullable nonempty structs with distinct simple lowercase
   field names and recursively qualified children. Diagnostic only; does not
   admit map-valued SQL parameters to the reference or provider semantics."
  [bytes]
  (try
    (assoc struct-map-base :status :decoded :rows (struct-map-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc struct-map-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc struct-map-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc struct-map-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-struct-map-operations
  "Expand selected SQL groups including diagnostic struct-map values."
  [bytes]
  (expand-fixed-operations bytes struct-map-base struct-map-rows!))

(def document-key-profile :xtdb-2.2.0-beta1/selected-document-key-arrow-operations-v1)
(def ^:private document-key-base (assoc base :profile document-key-profile))

(defn- document-key-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys}))

(defn decode-document-key-arguments
  "Decode selected structs with canonical reference document keys, including
   the outer _id spelling. Interpret retained field names, not original
   caller aliases. No document identity, admission or execution claim."
  [bytes]
  (try
    (assoc document-key-base :status :decoded :rows (document-key-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc document-key-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc document-key-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc document-key-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-document-key-operations
  "Expand selected SQL groups with diagnostic canonical document key maps."
  [bytes]
  (expand-fixed-operations bytes document-key-base document-key-rows!))

(def empty-struct-profile :xtdb-2.2.0-beta1/selected-empty-struct-arrow-operations-v1)
(def ^:private empty-struct-base (assoc base :profile empty-struct-profile))

(defn- empty-struct-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys :empty-struct}))

(defn decode-empty-struct-arguments
  "Decode selected canonical-key structs including zero-field nonnullable
   maps. Preserve row cardinality; nullable and absent fields are excluded."
  [bytes]
  (try
    (assoc empty-struct-base :status :decoded :rows (empty-struct-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc empty-struct-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc empty-struct-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc empty-struct-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-empty-struct-operations
  "Expand selected diagnostic SQL groups including empty struct maps."
  [bytes]
  (expand-fixed-operations bytes empty-struct-base empty-struct-rows!))

(def nullable-struct-profile :xtdb-2.2.0-beta1/selected-nullable-scalar-struct-arrow-operations-v1)
(def ^:private nullable-struct-base (assoc base :profile nullable-struct-profile))

(defn- nullable-struct-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys :empty-struct
                        :nullable-struct-fields}))

(defn decode-nullable-struct-arguments
  "Decode selected nullable scalar struct fields by omitting invalid entries.
   The retained representation cannot distinguish explicit null from absent
   keys. Diagnostic only; nullable IDs, containers and SQL parameters reject."
  [bytes]
  (try
    (assoc nullable-struct-base :status :decoded :rows (nullable-struct-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc nullable-struct-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc nullable-struct-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc nullable-struct-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-nullable-struct-operations
  "Expand selected diagnostic SQL groups with nullable scalar struct fields."
  [bytes]
  (expand-fixed-operations bytes nullable-struct-base nullable-struct-rows!))

(def null-only-struct-profile :xtdb-2.2.0-beta1/selected-null-only-scalar-struct-arrow-operations-v1)
(def ^:private null-only-struct-base (assoc base :profile null-only-struct-profile))

(defn- null-only-struct-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys :empty-struct
                        :nullable-struct-fields :null-only-fields}))

(defn decode-null-only-struct-arguments
  "Decode selected nullable scalar and null-only struct fields by omission.
   Null-only fields require exact row and null counts and no child buffers.
   Diagnostic only; original absent-versus-null identity is unavailable."
  [bytes]
  (try
    (assoc null-only-struct-base :status :decoded :rows (null-only-struct-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc null-only-struct-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc null-only-struct-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc null-only-struct-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-null-only-struct-operations
  "Expand selected diagnostic SQL groups with null-only struct fields."
  [bytes]
  (expand-fixed-operations bytes null-only-struct-base null-only-struct-rows!))

(def nullable-list-struct-profile :xtdb-2.2.0-beta1/selected-nullable-list-struct-arrow-operations-v1)
(def ^:private nullable-list-struct-base (assoc base :profile nullable-list-struct-profile))

(defn- nullable-list-struct-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys :empty-struct
                        :nullable-struct-fields :null-only-fields :nullable-list-fields}))

(defn decode-nullable-list-struct-arguments
  "Decode selected nullable vector fields by omission, requiring invalid
   entries to cover no child values. Present empty vectors remain present.
   Diagnostic only; nullable struct containers and list elements reject."
  [bytes]
  (try
    (assoc nullable-list-struct-base :status :decoded :rows (nullable-list-struct-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc nullable-list-struct-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc nullable-list-struct-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc nullable-list-struct-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-nullable-list-struct-operations
  "Expand selected diagnostic SQL groups with nullable vector fields."
  [bytes]
  (expand-fixed-operations bytes nullable-list-struct-base nullable-list-struct-rows!))

(def nullable-map-struct-profile :xtdb-2.2.0-beta1/selected-nullable-map-struct-arrow-operations-v1)
(def ^:private nullable-map-struct-base (assoc base :profile nullable-map-struct-profile))

(defn- nullable-map-struct-rows! [bytes]
  (scalar-rows! bytes #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                        :empty-list :recursive-list :struct-map :document-keys :empty-struct
                        :nullable-struct-fields :null-only-fields :nullable-list-fields :nullable-map-fields}))

(defn decode-nullable-map-struct-arguments
  "Decode selected nullable struct fields by omission, requiring hidden
   child values to be absent or selected zero/false/empty padding.
   Present empty maps remain present. Diagnostic only."
  [bytes]
  (try
    (assoc nullable-map-struct-base :status :decoded :rows (nullable-map-struct-rows! bytes))
    (catch clojure.lang.ExceptionInfo e
      (assoc nullable-map-struct-base :status :unsupported :reason (:reason (ex-data e))))
    (catch CharacterCodingException _
      (assoc nullable-map-struct-base :status :unsupported :reason :invalid-arrow-utf8))
    (catch Exception _ (assoc nullable-map-struct-base :status :unsupported :reason :invalid-arrow-stream))))

(defn decode-nullable-map-struct-operations
  "Expand selected diagnostic SQL groups with nullable structured-map fields."
  [bytes]
  (expand-fixed-operations bytes nullable-map-struct-base nullable-map-struct-rows!))
