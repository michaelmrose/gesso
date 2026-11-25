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

(defn- find-byte-sequence [^bytes data text]
  (let [pattern (.getBytes ^String text java.nio.charset.StandardCharsets/UTF_8)]
    (first (filter (fn [i] (every? #(= (aget pattern %) (aget data (+ i %)))
                                 (range (alength pattern))))
                   (range (inc (- (alength data) (alength pattern))))))))

(defn- check-keyword-retained! [node id invalid-id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-keyword-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = $4) AND ($5 = 0) AND $6)" :args row}) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ offsets values] (buffer-starts args)
        extension (find-byte-sequence args "xt/clj-keyword")]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/keyword-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-keyword-arguments args))))
    (is (keyword? (get-in (:rows (arrow/decode-keyword-arguments args)) [0 0])))
    (is (string? (get-in (:rows (arrow/decode-keyword-arguments args)) [0 2])))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-string-operations data))))
    (is (= (assoc-in rows [0 0] :zpen)
           (:rows (arrow/decode-keyword-arguments (edit-byte args values 122)))))
    (doseq [[bad reason] [[(edit-byte args extension 121) :unsupported-arrow-field]
                          [(edit-byte args values 58) :unsupported-arrow-keyword]
                          [(edit-byte args values 47) :unsupported-arrow-keyword]
                          [(edit-byte args values 255) :invalid-arrow-utf8]
                          [(edit-int args (+ offsets 4) -1) :invalid-arrow-value-offsets]]]
      (let [r (arrow/decode-keyword-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))
    (let [r (arrow/decode-keyword-operations (retained-operation-bytes node invalid-id))]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-arrow-keyword (:reason r)))
      (is (not (contains? r :operations))))))

(deftest selected-keyword-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-keyword-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        rows [[:open :open "open" "open" 0 true]
              [:claim/status :claim/status "claim/status" "claim/status" -1 false]
              [:Gesso.test/Status_1 :Gesso.test/Status_1 "" "" 0 true]]]
    (is (= [[]] (:rows (arrow/decode-keyword-arguments nil))))
    (try
      (let [[id invalid-id] (with-open [node (xtn/start-node config)]
                              (let [database (db/primary-db node)
                                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                                    id (submit! (mapv #(into ["ASSERT (? = ? AND (? = ?) AND (? = 0) AND ?)"] %) rows))
                                    invalid-id (submit! [["ASSERT ? = ?" :123 :123]])]
                                (check-keyword-retained! node id invalid-id rows)
                                [id invalid-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) invalid-id (Duration/ofSeconds 10))
          (check-keyword-retained! node id invalid-id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-uuid-retained! [node id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-uuid-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = $4) AND ($5 = 0) AND $6 AND ($7 = $8))" :args row}) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ values] (buffer-starts args)
        extension (find-byte-sequence args "uuid")]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/uuid-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-uuid-arguments args))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-keyword-operations data))))
    (is (= (vec (range 0 256 17))
           (mapv #(bit-and 255 %) (take 16 (drop values args)))))
    (is (= (assoc-in rows [0 0] #uuid "80112233-4455-6677-8899-aabbccddeeff")
           (:rows (arrow/decode-uuid-arguments (edit-byte args values 128)))))
    (is (= (assoc-in rows [0 0] #uuid "00112233-4455-6677-8899-aabbccddee00")
           (:rows (arrow/decode-uuid-arguments (edit-byte args (+ values 15) 0)))))
    (doseq [[bad reason] [[(edit-byte args extension 120) :unsupported-arrow-field]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]
                          [(edit-byte args 0 0) :invalid-arrow-framing]]]
      (let [r (arrow/decode-uuid-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))))

(deftest selected-uuid-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-uuid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        ids [#uuid "00112233-4455-6677-8899-aabbccddeeff"
             #uuid "ffeeddcc-bbaa-9988-7766-554433221100"
             #uuid "00000000-0000-0000-0000-000000000000"
             #uuid "ffffffff-ffff-ffff-ffff-ffffffffffff"]
        rows (mapv (fn [id] [id id :claim/status :claim/status 0 true (str id) (str id)]) ids)]
    (is (= [[]] (:rows (arrow/decode-uuid-arguments nil))))
    (try
      (let [id (with-open [node (xtn/start-node config)]
                 (let [database (db/primary-db node)
                       id (:tx-id (xt/submit-tx node (mapv #(into ["ASSERT (? = ? AND (? = ?) AND (? = 0) AND ? AND (? = ?))"] %) rows)))]
                   (.awaitTxBlocking database id (Duration/ofSeconds 10))
                   (check-uuid-retained! node id rows)
                   id))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (check-uuid-retained! node id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- edit-long [^bytes data offset value]
  (let [copy (aclone data)]
    (.putLong (doto (ByteBuffer/wrap copy) (.order ByteOrder/LITTLE_ENDIAN)) offset (long value))
    copy))

(defn- check-timestamp-retained! [node id other-id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-timestamp-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = 0) AND $4)" :args row}) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ values] (buffer-starts args)]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/timestamp-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-timestamp-arguments args))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-uuid-operations data))))
    (is (= -1 (.getLong (doto (ByteBuffer/wrap args) (.order ByteOrder/LITTLE_ENDIAN)) values)))
    (doseq [[micros spelling] [[-1000001 "1969-12-31T23:59:58.999999Z"]
                              [0 "1970-01-01T00:00:00Z"] [1 "1970-01-01T00:00:00.000001Z"]]]
      (is (= (assoc-in rows [0 0] (java.time.ZonedDateTime/parse spelling))
             (:rows (arrow/decode-timestamp-arguments (edit-long args values micros))))))
    (doseq [[bad reason] [[(edit-long args values Long/MIN_VALUE) :unsupported-arrow-timestamp-range]
                          [(edit-long args values Long/MAX_VALUE) :unsupported-arrow-timestamp-range]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-timestamp-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))
    (let [r (arrow/decode-timestamp-operations (retained-operation-bytes node other-id))]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-arrow-timestamp-type (:reason r)))
      (is (not (contains? r :operations))))))

(deftest selected-timestamp-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-timestamp-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        instants (mapv #(java.time.Instant/parse %) ["1969-12-31T23:59:59.999999Z"
                                                "1970-01-01T00:00:00Z" "2026-01-01T00:00:00.123456Z"
                                                "0001-01-01T00:00:00Z" "9998-12-31T23:59:59.999999Z"])
        input (mapv (fn [i] [i i 0 true]) instants)
        rows (mapv (fn [i] (let [z (.atZone i (java.time.ZoneId/of "Z"))] [z z 0 true])) instants)]
    (is (= [[]] (:rows (arrow/decode-timestamp-arguments nil))))
    (try
      (let [[id other-id] (with-open [node (xtn/start-node config)]
                            (let [database (db/primary-db node)
                                  submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                      (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                                  id (submit! (mapv #(into ["ASSERT (? = ? AND (? = 0) AND ?)"] %) input))
                                  z (java.time.ZonedDateTime/parse "2026-01-01T00:00:00-08:00[America/Los_Angeles]")
                                  other-id (submit! [["ASSERT ? = ?" z z]])]
                              (check-timestamp-retained! node id other-id rows)
                              [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-timestamp-retained! node id other-id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-utc-retained! [node id other-id rows deadline]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-utc-timestamp-operations data)
        expected (conj (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = 0))" :args row}) rows)
                       {:kind :sql :query "ASSERT CURRENT_TIMESTAMP < $1" :args [deadline]})
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ values _ z-values] (buffer-starts args)
        zone-id #(str (.getZone ^java.time.ZonedDateTime %))]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/utc-timestamp-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows) 1] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-utc-timestamp-arguments args))))
    (is (= ["UTC" "Z"] (mapv zone-id (take 2 (first (:rows (arrow/decode-utc-timestamp-arguments args)))))))
    (is (not= (ffirst rows) (second (first rows))))
    (is (= (.toInstant ^java.time.ZonedDateTime (ffirst rows))
           (.toInstant ^java.time.ZonedDateTime (second (first rows)))))
    (is (= :unsupported-arrow-timestamp-type (:reason (arrow/decode-timestamp-operations data))))
    (is (= :unsupported-arrow-timestamp-type (:reason (arrow/decode-utc-timestamp-operations
                                                       (retained-operation-bytes node other-id)))))
    (doseq [[offset col zone] [[values 0 "UTC"] [z-values 1 "Z"]]]
      (is (= (assoc-in rows [0 col] (.atZone java.time.Instant/EPOCH (java.time.ZoneId/of zone)))
             (:rows (arrow/decode-utc-timestamp-arguments (edit-long args offset 0))))))
    (doseq [value [Long/MIN_VALUE Long/MAX_VALUE]]
      (let [r (arrow/decode-utc-timestamp-arguments (edit-long args values value))]
        (is (= :unsupported (:status r)))
        (is (= :unsupported-arrow-timestamp-range (:reason r)))
        (is (not (contains? r :rows)))))))

(deftest selected-utc-timestamp-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-utc-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        instants (mapv #(java.time.Instant/parse %) ["1969-12-31T23:59:59.999999Z"
                                                     "0001-01-01T00:00:00Z"
                                                     "9998-12-31T23:59:59.999999Z"])
        utc (java.time.ZoneId/of "UTC")
        z (java.time.ZoneId/of "Z")
        rows (mapv (fn [i] [(.atZone i utc) (.atZone i z) 0]) instants)
        deadline (.atZone (java.time.Instant/parse "2026-01-01T00:00:00.000001Z") utc)]
    (is (= [[]] (:rows (arrow/decode-utc-timestamp-arguments nil))))
    (try
      (let [[id other-id] (with-open [node (xtn/start-node config)]
                            (let [database (db/primary-db node)
                                  submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                                      (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                                  id (submit! (conj (mapv #(into ["ASSERT (? = ? AND (? = 0))"] %) rows)
                                                    ["ASSERT CURRENT_TIMESTAMP < ?" deadline]))
                                  other-zone (java.time.ZonedDateTime/parse "2026-01-01T00:00:00-08:00[America/Los_Angeles]")
                                  other-id (submit! [["ASSERT ? = ?" other-zone other-zone]])]
                              (check-utc-retained! node id other-id rows deadline)
                              [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-utc-retained! node id other-id rows deadline)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-int64-list-retained! [node id oversized-id other-id rows]
  (let [data (retained-operation-bytes node id)
        observed (provider/observe-source-message! node id)
        decoded (arrow/decode-int64-list-operations data)
        expected (mapv (fn [row] {:kind :sql :query "ASSERT ($1 = $2 AND ($3 = 0))" :args row}) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ offsets _ values] (buffer-starts args)]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/int64-list-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded) (:operations observed)))
    (is (= [(count rows)] (:logged-operation-spans decoded) (:logged-operation-spans observed)))
    (is (= rows (:rows (arrow/decode-int64-list-arguments args))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-utc-timestamp-operations data))))
    (is (= (assoc-in rows [0 0 0] 99)
           (:rows (arrow/decode-int64-list-arguments (edit-long args values 99)))))
    (doseq [[bad reason] [[(edit-int args offsets 1) :invalid-arrow-list-offsets]
                          [(edit-int args (+ offsets 4) -1) :invalid-arrow-list-offsets]
                          [(edit-int args (+ offsets (* 4 (count rows))) Integer/MAX_VALUE) :invalid-arrow-list-offsets]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-int64-list-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))
    (doseq [[id reason] [[oversized-id :arrow-list-element-limit]
                         [other-id :unsupported-arrow-field]]]
      (let [r (arrow/decode-int64-list-operations (retained-operation-bytes node id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :operations)))))))

(deftest selected-int64-list-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-list-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        rows [[[1 2] [3 4] 0] [[] [5] 1]
              [[Long/MIN_VALUE Long/MAX_VALUE] [] -1]
              [(vec (range 256)) [0] 0]]]
    (is (= [[]] (:rows (arrow/decode-int64-list-arguments nil))))
    (try
      (let [[id oversized-id other-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! (mapv #(into ["ASSERT (? = ? AND (? = 0))"] %) rows))
                    oversized-id (submit! [["ASSERT ? = ?" (vec (range 257)) [1]]
                                           ["ASSERT ? = ?" [1] [1]]])
                    other-id (submit! [["ASSERT ? = ?" ["x"] ["x"]]])]
                (check-int64-list-retained! node id oversized-id other-id rows)
                [id oversized-id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-int64-list-retained! node id oversized-id other-id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
