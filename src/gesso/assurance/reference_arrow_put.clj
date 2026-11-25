(ns gesso.assurance.reference-arrow-put
  "Internal physical interpretation of selected retained put envelopes.
   Profile selection and IID verification remain in reference-arrow-operations."
  (:require [gesso.assurance.reference-arrow-framing :refer [max-bytes require! message! field! buffers!]]
            [gesso.assurance.reference-arrow-schema :refer [scalar-kind!]]
            [gesso.assurance.reference-arrow-values :refer [struct-kind? node-count buffer-count value-reader!]])
  (:import (java.nio ByteBuffer ByteOrder)
           (java.util HexFormat)
           (org.apache.arrow.flatbuf Message MessageHeader Schema RecordBatch Field Type Union UnionMode Endianness
                                    FixedSizeBinary Timestamp TimeUnit)))

(defn- single-union-field! [^Field f name]
  (field! f name Type/Union false 1)
  (let [^Union u (.type f (Union.))]
    (require! (and u (= UnionMode/Dense (.mode u)) (zero? (.typeIdsLength u)))
              :unsupported-arrow-union))
  (.children f 0))

(defn- put-table! [^Field table field-options]
  (let [table-name (.name table)]
    (require! (and table-name (<= 1 (count table-name) 4095)
                   (re-matches #"[a-z][a-z0-9_]*/[a-z][a-z0-9_]*" table-name)) :unsupported-arrow-put-table)
    (field! table table-name Type/List false 1)
    (let [kind (scalar-kind! (.children table 0) "$data$"
                             (into #{:integer :boolean :string :keyword :uuid :timestamp :timestamp-utc
                                     :empty-list :recursive-list :struct-map :document-keys :empty-struct}
                                   field-options))]
      (require! (struct-kind? kind) :unsupported-arrow-put-document)
      {:table (keyword table-name) :kind kind})))

(defn- put-schema! [^Message m ^ByteBuffer body field-options]
  (let [^Schema schema (.header m (Schema.))]
    (require! (and schema (zero? (.remaining body)) (= Endianness/Little (.endianness schema))
                   (= 1 (.fieldsLength schema)) (zero? (.featuresLength schema))
                   (zero? (.customMetadataLength schema))) :unsupported-arrow-schema)
    (let [^Field put (field! (single-union-field! (.fields schema 0) "tx-ops") "put-docs" Type/Struct_ false 4)
          ^Field iids (field! (.children put 0) "iids" Type/List false 1)
          ^Field iid (field! (.children iids 0) "$data$" Type/FixedSizeBinary false 0)
          ^FixedSizeBinary binary (.type iid (FixedSizeBinary.))
          ^Field tables (.children put 1)
          t (.childrenLength tables)]
      (require! (and binary (= 16 (.byteWidth binary))) :unsupported-arrow-iid-width)
      (require! (<= 1 t (if (contains? field-options :multiple-put-tables) 16 1)) :unsupported-arrow-field)
      (field! tables "documents" Type/Union false t)
      (let [^Union u (.type tables (Union.))]
        (require! (and u (= UnionMode/Dense (.mode u)) (zero? (.typeIdsLength u))) :unsupported-arrow-union))
      (doseq [[i name] [[2 "_valid_from"] [3 "_valid_to"]]]
        (let [^Field f (field! (.children put i) name Type/Timestamp true 0)
              ^Timestamp t (.type f (Timestamp.))]
          (require! (and t (= TimeUnit/MICROSECOND (.unit t)) (= "UTC" (.timezone t)))
                    :unsupported-arrow-put-bound-type)))
      (let [legs (mapv #(put-table! (.children tables %) field-options) (range t))]
        (require! (= t (count (distinct (map :table legs)))) :unsupported-arrow-put-table)
        legs))))

(defn- put-list-offsets! [^ByteBuffer offsets g n]
  (require! (and (= (* 4 (inc g)) (.limit offsets)) (zero? (.getInt offsets 0)))
            :unsupported-arrow-put-list-layout)
  (let [xs (mapv #(.getInt offsets (* 4 %)) (range (inc g)))]
    (require! (and (= n (peek xs))
                   (every? (fn [[a b]] (and (<= 0 a b n) (<= 1 (- b a) 256))) (partition 2 1 xs)))
              :unsupported-arrow-put-list-layout)
    xs))

(defn- put-table-reader! [^RecordBatch batch buffers {:keys [table kind]} node start groups total]
  (let [g (.length (.nodes batch node))
        n (.length (.nodes batch (inc node)))
        ^ByteBuffer validity (nth buffers start)
        ^ByteBuffer offsets (nth buffers (inc start))]
    (require! (and (<= 1 g groups) (<= 1 n total) (zero? (.nullCount (.nodes batch node))))
              :unsupported-arrow-put-nodes)
    (require! (zero? (.limit validity)) :unsupported-arrow-put-selection)
    (let [xs (put-list-offsets! offsets g n)
          read-doc (value-reader! kind batch (inc node) buffers (+ start 2) n)
          docs (mapv read-doc (range n))]
      (require! (every? #(let [id (:xt/id %)] (or (integer? id) (string? id) (uuid? id) (keyword? id))) docs)
                :unsupported-arrow-put-document-id)
      {:table table :groups g :documents n
       :rows (mapv (fn [[a b]] (subvec docs a b)) (partition 2 1 xs))})))

(defn- put-batch! [^Message m ^ByteBuffer body legs multi-groups?]
  (let [^RecordBatch batch (.header m (RecordBatch.))
        g (.length batch)
        nodes-at (vec (reductions + 5 (map #(inc (node-count (:kind %))) legs)))
        buffers-at (vec (reductions + 9 (map #(+ 2 (buffer-count (:kind %))) legs)))
        bound-node (peek nodes-at) bound-buffer (peek buffers-at)]
    (require! (and batch (<= 1 g (if multi-groups? 256 1))
                   (= (+ bound-node 2) (.nodesLength batch))
                   (nil? (.compression batch)) (zero? (.variadicBufferCountsLength batch))) :unsupported-arrow-put-batch)
    (let [n (.length (.nodes batch 3))
          nodes (mapv #(let [node (.nodes batch %)] [(.length node) (.nullCount node)]) (range 5))
          buffers (buffers! batch body (+ bound-buffer 4))
          [^ByteBuffer tags ^ByteBuffer indexes _ _ ^ByteBuffer iid-offsets _ ^ByteBuffer iids
           ^ByteBuffer table-tags ^ByteBuffer table-indexes] buffers]
      (require! (and (<= 1 n (if multi-groups? 4096 256))
                     (= [[g -1] [g 0] [g 0] [n 0] [g -1]] nodes)) :unsupported-arrow-put-nodes)
      (require! (and (every? #(zero? (.limit ^ByteBuffer (nth buffers %))) [2 3 5])
                     (= g (.limit tags) (.limit table-tags)) (= (* 4 g) (.limit indexes) (.limit table-indexes))
                     (every? (fn [i] (and (zero? (.get tags i)) (= i (.getInt indexes (* 4 i))))) (range g)))
                :unsupported-arrow-put-selection)
      (require! (= (* 16 n) (.limit iids)) :unsupported-arrow-put-list-layout)
      (let [xs (put-list-offsets! iid-offsets g n)
            counts (mapv #(.length (.nodes batch %)) (pop nodes-at))
            doc-counts (mapv #(.length (.nodes batch (inc %))) (pop nodes-at))]
        (require! (and (every? #(<= 1 % g) counts) (= g (reduce + counts))
                       (every? #(<= 1 % n) doc-counts) (= n (reduce + doc-counts))) :unsupported-arrow-put-nodes)
        (let [selections (mapv (fn [i] [(int (.get table-tags i)) (.getInt table-indexes (* 4 i))]) (range g))
              consumed (reduce (fn [seen [tag index]]
                                 (require! (and (<= 0 tag (dec (count legs))) (= index (nth seen tag))
                                                (< index (nth counts tag))) :unsupported-arrow-put-selection)
                                 (update seen tag inc)) (vec (repeat (count legs) 0)) selections)]
          (require! (= counts consumed) :unsupported-arrow-put-selection)
          (doseq [i (range 2)]
            (let [node (.nodes batch (+ bound-node i))
                  ^ByteBuffer validity (nth buffers (+ bound-buffer (* 2 i)))
                  ^ByteBuffer values (nth buffers (+ bound-buffer (* 2 i) 1))]
              (require! (and (= g (.length node)) (or (= -1 (.nullCount node)) (= g (.nullCount node)))
                             (= (quot (+ g 7) 8) (.limit validity))
                             (every? #(zero? (.get validity %)) (range (.limit validity)))
                             (= (* 8 g) (.limit values))
                             (every? #(zero? (.getLong values (* 8 %))) (range g))) :unsupported-arrow-put-bounds)))
          (let [readers (mapv #(put-table-reader! batch buffers %1 %2 %3 g n) legs (pop nodes-at) (pop buffers-at))
                ids (mapv (fn [i] (let [b (byte-array 16) copy (.duplicate iids)]
                                    (.position copy (* 16 i)) (.get copy b) (.formatHex (HexFormat/of) b))) (range n))]
            (mapv (fn [[tag index] [a b]]
                    (let [{:keys [table rows]} (nth readers tag)
                          docs (nth rows index)]
                      (require! (= (- b a) (count docs)) :unsupported-arrow-put-list-layout)
                      {:kind :put-docs :table table :documents docs :iids (subvec ids a b) :bounds {}}))
                  selections (partition 2 1 xs))))))))

(defn read-envelope!
  "Interpret selected put groups from the complete retained Arrow stream.
   Throws on unsupported physical representations. Does not verify IIDs."
  [bytes field-options]
  (require! (and (bytes? bytes) (<= 1 (alength ^bytes bytes) max-bytes)) :arrow-byte-limit)
  (let [stream (doto (ByteBuffer/wrap ^bytes bytes) (.order ByteOrder/LITTLE_ENDIAN))
        [sm sb] (message! stream MessageHeader/Schema)
        schema (put-schema! sm sb field-options)
        [bm bb] (message! stream MessageHeader/RecordBatch)
        groups (put-batch! bm bb schema (contains? field-options :multiple-put-groups))]
    (require! (and (= 4 (.remaining stream)) (zero? (.getInt stream))) :unsupported-arrow-terminator)
    groups))
