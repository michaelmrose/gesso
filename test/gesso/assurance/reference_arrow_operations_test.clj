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
            (is (= queries (mapv :query (:groups decoded)) (mapv :query (:operations observed))))
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
                             a (submit! [["ASSERT TRUE"] ["ASSERT FALSE"]])
                             b (submit! [["ASSERT ? = ?" 42 42]])
                             ;; XTDB rewrites positional placeholders on submission.
                             expected {a ["ASSERT TRUE" "ASSERT FALSE"] b ["ASSERT $1 = $2"]}]
                         (check-retained! node expected) expected))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (apply max (keys expected)) (Duration/ofSeconds 10))
          (check-retained! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
