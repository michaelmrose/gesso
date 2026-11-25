(ns gesso.assurance.reference-arrow-operations-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.xtdb-source-log :as provider]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.io RandomAccessFile)
           (java.nio ByteBuffer ByteOrder)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)
           (java.util Arrays)
           (org.apache.arrow.memory RootAllocator)
           (org.apache.arrow.flatbuf Message RecordBatch)
           (xtdb.arrow Relation)
           (xtdb.tx TxWriter TxOp$Sql)))

(defn- edit-byte [^bytes data offset value]
  (let [copy (aclone data)] (aset-byte copy offset (unchecked-byte value)) copy))

(defn- edit-int [^bytes data offset value]
  (let [copy (aclone data)]
    (.putInt (doto (ByteBuffer/wrap copy) (.order ByteOrder/LITTLE_ENDIAN)) offset (int value))
    copy))

(defn- buffer-starts [^bytes data]
  ;; Fixture layout discovery only; expected SQL is independently specified.
  (let [b (doto (ByteBuffer/wrap data) (.order ByteOrder/LITTLE_ENDIAN))
        batch-start (+ 8 (.getInt b 4))
        metadata-start (+ 8 batch-start)
        body-start (+ metadata-start (.getInt b (+ 4 batch-start)))
        m (Message/getRootAsMessage (.slice b metadata-start (- body-start metadata-start)))
        ^RecordBatch batch (.header m (RecordBatch.))]
    (mapv #(+ body-start (.offset (.buffers batch %))) (range (.buffersLength batch)))))

(deftest selected-sql-buffer-interpretation
  (with-open [al (RootAllocator. (* 16 1024 1024))]
    (let [data (TxWriter/toArrowBytes [(TxOp$Sql. "ASSERT TRUE" nil)
                                      (TxOp$Sql. "ASSERT FALSE" nil)] al)
          r (arrow/decode-operations data)
          [tags indexes _ _ query-offsets queries arg-valid arg-offsets] (buffer-starts data)]
      (is (= :decoded (:status r)))
      (is (= arrow/profile (:profile r)))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))
      (is (= [{:kind :sql :query "ASSERT TRUE" :argument-bytes nil}
              {:kind :sql :query "ASSERT FALSE" :argument-bytes nil}] (:groups r)))
      (doseq [[bad reason] [[(edit-byte data 0 0) :invalid-arrow-framing]
                            [(edit-int data 4 7) :invalid-arrow-metadata-length]
                            [(edit-byte data tags 1) :unsupported-arrow-union-selection]
                            [(edit-int data indexes 1) :unsupported-arrow-union-selection]
                            [(edit-int data (+ query-offsets 4) -1) :invalid-arrow-value-offsets]
                            [(edit-byte data queries 255) :invalid-arrow-utf8]
                            [(edit-byte data arg-valid 1) :unsupported-arrow-argument-slot]
                            [(edit-int data (+ arg-offsets 4) 1) :invalid-arrow-value-offsets]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(Arrays/copyOf data (inc (alength data))) :unsupported-arrow-terminator]]]
        (let [r (arrow/decode-operations bad)]
          (is (= :unsupported (:status r)) (pr-str r))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :groups)))))
      (doseq [bad [nil (byte-array 0) (byte-array (inc (* 16 1024 1024)))]]
        (is (= :arrow-byte-limit (:reason (arrow/decode-operations bad))))))))

(defn- zero-column-bytes [^RootAllocator al n]
  (with-open [rel (Relation. al)]
    (.setRowCount rel (int n))
    (.getAsArrowStream rel)))

(deftest parameterless-relation-decoding
  (with-open [al (RootAllocator. (* 16 1024 1024))]
    (is (= {:profile arrow/parameterless-profile :authority :none
            :certificate :not-issued :status :decoded :encoding :absent :row-count 1}
           (arrow/decode-parameterless-arguments nil)))
    (doseq [n [1 2 16384]]
      (let [r (arrow/decode-parameterless-arguments (zero-column-bytes al n))]
        (is (= :decoded (:status r)))
        (is (= :zero-column-relation (:encoding r)))
        (is (= n (:row-count r)))))
    (let [data (zero-column-bytes al 3)
          batch-start (+ 8 (.getInt (doto (ByteBuffer/wrap data) (.order ByteOrder/LITTLE_ENDIAN)) 4))
          extra-batch (byte-array (concat (take (- (alength data) 4) data)
                                          (drop batch-start data)))]
      (doseq [[bad reason] [[(zero-column-bytes al 0) :arrow-argument-row-limit]
                            [(zero-column-bytes al 16385) :arrow-argument-row-limit]
                            [(byte-array 0) :arrow-argument-byte-limit]
                            [42 :arrow-argument-byte-limit]
                            [(byte-array (inc (* 16 1024 1024))) :arrow-argument-byte-limit]
                            [(edit-byte data 0 0) :invalid-arrow-framing]
                            [(edit-int data 4 7) :invalid-arrow-metadata-length]
                            [(edit-byte data batch-start 0) :invalid-arrow-framing]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(Arrays/copyOf data (inc (alength data))) :unsupported-arrow-terminator]
                            [extra-batch :unsupported-arrow-terminator]]]
        (let [r (arrow/decode-parameterless-arguments bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :row-count))))))
    (with-open [args (Relation. al) too-many (Relation. al)]
      (.setRowCount args 3)
      (.setRowCount too-many 16384)
      (let [data (TxWriter/toArrowBytes [(TxOp$Sql. "ASSERT TRUE" args)
                                        (TxOp$Sql. "ASSERT FALSE" nil)] al)
            r (arrow/decode-parameterless-operations data)]
        (is (= :decoded (:status r)))
        (is (= [3 1] (:logged-operation-spans r)))
        (is (= (mapv (fn [q] {:kind :sql :query q :args []})
                     ["ASSERT TRUE" "ASSERT TRUE" "ASSERT TRUE" "ASSERT FALSE"])
               (:operations r))))
      (let [data (TxWriter/toArrowBytes [(TxOp$Sql. "ASSERT TRUE" too-many)
                                        (TxOp$Sql. "ASSERT FALSE" nil)] al)
            r (arrow/decode-parameterless-operations data)]
        (is (= :unsupported (:status r)))
        (is (= :arrow-expanded-row-limit (:reason r)))
        (is (not (contains? r :operations)))))))

(defn- check-integer-arguments! [groups]
  (let [data (:argument-bytes (first groups))
        expected [[42 42] [Long/MIN_VALUE Long/MAX_VALUE] [-1 0]]
        r (arrow/decode-integer-arguments data)
        [_ values] (buffer-starts data)]
    (is (= :decoded (:status r)))
    (is (= arrow/integer-profile (:profile r)))
    (is (= expected (:rows r)))
    (is (= (assoc-in expected [0 0] 43)
           (:rows (arrow/decode-integer-arguments (edit-byte data values 43)))))
    (doseq [[bad reason] [[(edit-byte data 0 0) :invalid-arrow-framing]
                          [(edit-int data 4 7) :invalid-arrow-metadata-length]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                          [(Arrays/copyOf data (inc (alength data))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-integer-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)))
        (is (not (contains? r :rows)))))))

(deftest integer-profile-parameterless-and-bounds
  (is (= [[]] (:rows (arrow/decode-integer-arguments nil))))
  (with-open [al (RootAllocator. (* 16 1024 1024))]
    (is (= [[] []] (:rows (arrow/decode-integer-arguments (zero-column-bytes al 2)))))
    (is (= :arrow-argument-row-limit
           (:reason (arrow/decode-integer-arguments (zero-column-bytes al 0)))))
    (is (= :arrow-argument-row-limit
           (:reason (arrow/decode-integer-arguments (zero-column-bytes al 16385)))))
    (with-open [args (Relation. al)]
      (.setRowCount args 16384)
      (let [r (arrow/decode-integer-operations
                (TxWriter/toArrowBytes [(TxOp$Sql. "ASSERT TRUE" args)
                                        (TxOp$Sql. "ASSERT FALSE" nil)] al))]
        (is (= :unsupported (:status r)))
        (is (= :arrow-expanded-row-limit (:reason r)))
        (is (not (contains? r :operations)))))))

(defn- check-retained! [node expected]
  (let [log (.getSourceLog (db/primary-db node))
        path (#'coverage/local-log-path log)]
    (with-open [file (RandomAccessFile. (.toFile path) "r")]
      (let [framing (#'coverage/local-frames file)]
        (is (= :framed (:status framing)))
        (doseq [[id _ length] (:frames framing) :when (contains? expected id)]
          (.seek file (+ id 13))
          (let [bytes (byte-array length)
                _ (.readFully file bytes)
                envelope (payload/decode-payload bytes)
                decoded (arrow/decode-operations (get-in envelope [:message :tx-ops]))
                observed (provider/observe-source-message! node id)
                queries (get expected id)]
            (is (= :decoded (:status envelope)))
            (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :groups)))
            (is (= :source-message-observed (:status observed)))
            (is (= (mapv first (partition-by identity queries)) (mapv :query (:groups decoded))))
            (is (= queries (mapv :query (:operations observed))))
            (let [expanded (arrow/decode-parameterless-operations (get-in envelope [:message :tx-ops]))]
              (if (some #{"ASSERT $1 = $2" "ASSERT $1"} queries)
                (do (is (= :unsupported (:status expanded)))
                    (is (= :unsupported-parameterless-schema (:reason expanded)))
                    (is (not (contains? expanded :operations))))
                (do (is (= :decoded (:status expanded)))
                    (is (= (mapv (fn [q] {:kind :sql :query q :args []}) queries)
                           (:operations expanded) (:operations observed)))
                    (is (= (mapv count (partition-by identity queries))
                           (:logged-operation-spans expanded) (:logged-operation-spans observed))))))
            (let [expanded (arrow/decode-integer-operations (get-in envelope [:message :tx-ops]))]
              (if (some #{"ASSERT $1"} queries)
                (do (is (= :unsupported (:status expanded)))
                    (is (= :unsupported-arrow-field (:reason expanded)))
                    (is (not (contains? expanded :operations))))
                (let [args (if (some #{"ASSERT $1 = $2"} queries)
                             [[42 42] [Long/MIN_VALUE Long/MAX_VALUE] [-1 0] []]
                             (repeat (count queries) []))]
                  (is (= :decoded (:status expanded)) (pr-str (dissoc expanded :operations)))
                  (is (= (mapv (fn [q row] {:kind :sql :query q :args row}) queries args)
                         (:operations expanded) (:operations observed)))
                  (is (= (:logged-operation-spans expanded) (:logged-operation-spans observed)))
                  (when (some #{"ASSERT $1 = $2"} queries)
                    (check-integer-arguments! (:groups decoded))))))
            (let [expanded (arrow/decode-boolean-operations (get-in envelope [:message :tx-ops]))
                  args (cond
                         (some #{"ASSERT $1"} queries) [[true]]
                         (some #{"ASSERT $1 = $2"} queries) [[42 42] [Long/MIN_VALUE Long/MAX_VALUE] [-1 0] []]
                         :else (repeat (count queries) []))]
              (is (= :decoded (:status expanded)))
              (is (= (mapv (fn [q row] {:kind :sql :query q :args row}) queries args)
                     (:operations expanded) (:operations observed)))
              (is (= (:logged-operation-spans expanded) (:logged-operation-spans observed))))
            (is (every? #(and (bytes? %) (pos? (alength ^bytes %))) (map :argument-bytes (:groups decoded)))))))
      (is (= (set (keys expected)) (set (map first (:frames (#'coverage/local-frames file)))))))))

(deftest selected-sql-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-ops-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                 (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                             a (submit! [["ASSERT TRUE"] ["ASSERT TRUE"] ["ASSERT FALSE"]])
                             b (submit! [["ASSERT ? = ?" 42 42]
                                         ["ASSERT ? = ?" Long/MIN_VALUE Long/MAX_VALUE]
                                         ["ASSERT ? = ?" -1 0] ["ASSERT TRUE"]])
                             c (submit! [["ASSERT ?" true]])
                             ;; XTDB rewrites positional placeholders on submission.
                             expected {a ["ASSERT TRUE" "ASSERT TRUE" "ASSERT FALSE"] b ["ASSERT $1 = $2" "ASSERT $1 = $2" "ASSERT $1 = $2" "ASSERT TRUE"]
                                       c ["ASSERT $1"]}]
                         (check-retained! node expected) expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (apply max (keys expected)) (Duration/ofSeconds 10))
          (check-retained! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- retained-operation-bytes [node id]
  (let [log (.getSourceLog (db/primary-db node))
        path (#'coverage/local-log-path log)]
    (with-open [file (RandomAccessFile. (.toFile path) "r")]
      (let [[offset _ length] (some #(when (= id (first %)) %) (:frames (#'coverage/local-frames file)))
            bytes (byte-array length)]
        (.seek file (+ offset 13))
        (.readFully file bytes)
        (get-in (payload/decode-payload bytes) [:message :tx-ops])))))

(defn- check-boolean-retained! [node id string-id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-boolean-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 AND ($2 = 0) AND $3)" :args row}) rows)
        groups (:groups (arrow/decode-operations data))
        args (:argument-bytes (first groups))
        [_ bool-values _ _ _ other-bool-values] (buffer-starts args)]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/boolean-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-boolean-arguments args))))
    ;; Change one bit on each side of the byte boundary, without changing
    ;; any metadata, column positions or integer values.
    (doseq [[row col offset mask] [[0 0 bool-values 1] [8 2 (inc other-bool-values) 1]]]
      (let [mutated (edit-byte args offset (bit-xor mask (aget ^bytes args offset)))]
        (is (= (update-in rows [row col] not)
               (:rows (arrow/decode-boolean-arguments mutated))))))
    (is (= :unsupported (:status (arrow/decode-integer-operations data))))
    (let [r (arrow/decode-boolean-operations (retained-operation-bytes node string-id))]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-arrow-field (:reason r)))
      (is (not (contains? r :operations))))
    (doseq [bad [(edit-byte args 0 0)
                 (Arrays/copyOf args (dec (alength args)))
                 (Arrays/copyOf args (inc (alength args)))]]
      (let [r (arrow/decode-boolean-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :rows)))))))

(deftest selected-boolean-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-bool-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        rows (mapv (fn [i] [(even? i) (long (- i 4)) (zero? (mod i 3))]) (range 10))]
    (try
      (let [[id string-id] (with-open [node (xtn/start-node config)]
                             (let [database (db/primary-db node)
                                   submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                       (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                                   id (submit! (mapv #(into ["ASSERT (? AND (? = 0) AND ?)"] %) rows))
                                   string-id (submit! [["ASSERT ? = 'x'" "x"]])]
                               (check-boolean-retained! node id string-id rows)
                               [id string-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) string-id (Duration/ofSeconds 10))
          (check-boolean-retained! node id string-id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-string-retained! [node id empty-id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-string-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = 0) AND $4)" :args row}) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ offsets values] (buffer-starts args)]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/string-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-string-arguments args))))
    (is (= :unsupported (:status (arrow/decode-boolean-operations data))))
    (is (= :unsupported (:status (arrow/decode-integer-operations data))))
    ;; First row is empty, second starts with ASCII x. Mutate only its byte.
    (is (= (assoc-in rows [1 0] "y")
           (:rows (arrow/decode-string-arguments (edit-byte args values 121)))))
    (doseq [[bad reason] [[(edit-int args offsets 1) :invalid-arrow-value-offsets]
                          [(edit-int args (+ offsets 4) -1) :invalid-arrow-value-offsets]
                          [(edit-int args (+ offsets (* 4 (count rows))) Integer/MAX_VALUE) :invalid-arrow-value-offsets]
                          [(edit-byte args values 255) :invalid-arrow-utf8]
                          [(edit-byte args values 195) :invalid-arrow-utf8]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-string-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))
    (let [data (retained-operation-bytes node empty-id)
          r (arrow/decode-string-operations data)]
      (is (= :decoded (:status r)))
      (is (= [{:kind :sql :query "ASSERT $1 = ''" :args [""]}]
             (:operations r) (:operations (provider/observe-source-message! node empty-id)))))))

(deftest selected-utf8-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-utf8-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        rows [["" "" 0 true] ["x" "x" 1 false] ["café" "café" -1 true]
              ["漢字" "漢字" 0 true] ["😀" "😀" 0 true] ["é" "é" 0 false]]]
    (is (= [[]] (:rows (arrow/decode-string-arguments nil))))
    (try
      (let [[id empty-id] (with-open [node (xtn/start-node config)]
                            (let [database (db/primary-db node)
                                  submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                      (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                                  id (submit! (mapv #(into ["ASSERT (? = ? AND (? = 0) AND ?)"] %) rows))
                                  empty-id (submit! [["ASSERT ? = ''" ""]])]
                              (check-string-retained! node id empty-id rows)
                              [id empty-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) empty-id (Duration/ofSeconds 10))
          (check-string-retained! node id empty-id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
