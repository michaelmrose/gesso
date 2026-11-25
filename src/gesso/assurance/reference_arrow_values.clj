(ns gesso.assurance.reference-arrow-values
  "Internal interpretation of qualified Arrow scalar and composite values."
  (:require [gesso.assurance.reference-arrow-framing :refer [max-bytes require! offsets! value-bytes! present?]])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets)
           (java.util UUID)
           (java.time Instant ZoneId)
           (org.apache.arrow.flatbuf RecordBatch)))

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
(defn struct-kind? [kind] (and (vector? kind) (= :struct (first kind))))

(defn- nullable-kind? [kind] (and (vector? kind) (= :nullable (first kind))))
(def ^:private absent-field (Object.))

(def max-value-slots 65536)
(def ^:dynamic ^:private *remaining-value-slots* nil)

(defn with-value-budget
  "Run one synchronous interpretation with a fresh aggregate value-slot
   budget. Count logical field slots (including nulls), containers and
   list descendants; union selections count once, not once per leg.
   This is a diagnostic resource bound, not an admission judgment."
  [decode!]
  (binding [*remaining-value-slots* (volatile! max-value-slots)]
    (decode!)))

(defn- charge-value-slots! [n]
  (when-let [remaining *remaining-value-slots*]
    (require! (<= 0 n @remaining) :arrow-aggregate-value-limit)
    (vswap! remaining - n)))

(defn node-count [kind]
  (cond (nullable-kind? kind) (node-count (second kind))
        (list-kind? kind) (inc (node-count (second kind)))
        (union-kind? kind) (inc (count (second kind)))
        (struct-kind? kind) (inc (reduce + 0 (map (comp node-count second) (second kind))))
        :else 1))

(defn buffer-count [kind]
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
    (charge-value-slots! child-count)
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

(defn value-reader! [kind ^RecordBatch batch node-index buffers start n]
  (charge-value-slots! n)
  (cond
    (= :null-only-field kind)
    (let [node (.nodes batch node-index)]
      (require! (and (= n (.length node)) (= n (.nullCount node)))
                :unsupported-arrow-null-only-layout)
      (constantly absent-field))
    (nullable-kind? kind)
    (nullable-reader! (second kind) batch node-index buffers start n)
    (union-kind? kind)
    (let [node (.nodes batch node-index)]
      (require! (and (= n (.length node)) (= -1 (.nullCount node)))
                :unsupported-arrow-list-union-layout)
      (union-reader! (second kind) batch node-index buffers start n))
    :else
    (let [node (.nodes batch node-index)
          ^ByteBuffer validity (nth buffers start)]
      (require! (and (= n (.length node)) (zero? (.nullCount node))
                     (zero? (.limit validity))) :unsupported-integer-buffer-layout)
      (cond (list-kind? kind) (list-reader! (second kind) batch node-index buffers start n)
            (struct-kind? kind) (struct-reader! (second kind) batch node-index buffers start n)
            :else (scalar-reader! kind buffers start n)))))


(defn sql-argument-reader!
  "Preserve a qualified absent top-level column value as positional nil.
   The struct reader continues to omit absent document fields."
  [kind batch node-index buffers start n]
  (let [reader (value-reader! kind batch node-index buffers start n)]
    (fn [row]
      (let [v (reader row)]
        (when-not (identical? absent-field v) v)))))
