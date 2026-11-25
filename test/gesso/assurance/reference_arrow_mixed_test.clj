(ns gesso.assurance.reference-arrow-mixed-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-arrow-operations-test :as fixture]
            [gesso.assurance.reference-arrow-struct-test :as struct-fixture]
            [gesso.assurance.xtdb-source-log :as provider]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)
           (java.util Arrays)))

(def ^:private sql-true {:kind :sql :query "ASSERT TRUE" :args []})
(def ^:private foo {:kind :put-docs :table :public/foo :documents [{:xt/id 1 :name "a"}]
                    :iids ["4cd9b7672d7fbee8fb51fb1e049f6903"] :bounds {}})
(def ^:private bar {:kind :put-docs :table :public/bar :documents [{:xt/id "x" :ok true}]
                    :iids ["9740c9dcdb4487d8cd63d2a544d8e657"] :bounds {}})

(defn- check-mixed! [node id reversed-id rejected]
  (let [decode arrow/decode-mixed-sql-put
        data (#'fixture/retained-operation-bytes node id)
        r (decode data)
        expected [sql-true foo sql-true sql-true bar]
        buffers (#'fixture/buffer-starts data)
        [tags indexes query arg-bytes iids table-tags] (mapv #(nth buffers %) [0 1 5 8 13 14])]
    (is (= :decoded (:status r)) (pr-str (dissoc r :operations)))
    (is (= arrow/mixed-sql-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:operations r) (:operations (provider/observe-source-message! node id))))
    (is (= [1 1 2 1] (:logged-operation-spans r)
           (:logged-operation-spans (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-operations data))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-multi-table-put data))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data tags 2) :unsupported-arrow-union-selection]
                          [(#'fixture/edit-int data (+ indexes 8) 0) :unsupported-arrow-union-selection]
                          [(#'fixture/edit-int data (+ indexes 12) -1) :unsupported-arrow-union-selection]
                          [(#'fixture/edit-byte data query 255) :invalid-arrow-utf8]
                          [(#'fixture/edit-byte data arg-bytes 0) :invalid-arrow-framing]
                          [(#'fixture/edit-byte data iids 0) :document-iid-mismatch]
                          [(#'fixture/edit-byte data (inc table-tags) 2) :unsupported-arrow-put-selection]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))
    (let [r (decode (#'fixture/retained-operation-bytes node reversed-id))
          observed (provider/observe-source-message! node reversed-id)]
      (is (= :matched (:iid-correspondence r)) (pr-str (dissoc r :operations)))
      (is (= [foo {:kind :sql :query "ASSERT $1 = $2" :args [42 42]}
                   {:kind :sql :query "ASSERT $1 = $2" :args [-1 -1]} bar]
             (:operations r) (:operations observed)))
      (is (= [1 2 1] (:logged-operation-spans r) (:logged-operation-spans observed))))
    (doseq [[id reason] rejected]
      (let [r (decode (#'fixture/retained-operation-bytes node id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))))

(deftest selected-mixed-sql-put-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-operations-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        p1 [:put-docs :foo {:xt/id 1 :name "a"}]
        p2 [:put-docs :bar {:xt/id "x" :ok true}]]
    (try
      (let [[id reversed-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! [["ASSERT TRUE"] p1 ["ASSERT TRUE"] ["ASSERT TRUE"] p2])
                    reversed-id (submit! [p1 ["ASSERT ? = ?" 42 42] ["ASSERT ? = ?" -1 -1] p2])
                    rejected [[(submit! [p1 ["ASSERT ?" true]]) :unsupported-arrow-field]
                              [(submit! [p1]) :unsupported-arrow-field]
                              [(submit! [["ASSERT TRUE"]]) :unsupported-arrow-field]]]
                (check-mixed! node id reversed-id rejected)
                [id reversed-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (first (peek rejected)) (Duration/ofSeconds 15))
          (check-mixed! node id reversed-id rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(def ^:private scalar-query
  "ASSERT (? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL)")
(def ^:private retained-scalar-query
  "ASSERT ($1 IS NOT NULL AND $2 IS NOT NULL AND $3 IS NOT NULL AND $4 IS NOT NULL AND $5 IS NOT NULL AND $6 IS NOT NULL AND $7 IS NOT NULL)")

(defn- check-mixed-scalars! [node id integer-id rejected rows]
  (let [decode arrow/decode-mixed-scalar-sql-put
        data (#'fixture/retained-operation-bytes node id)
        expected (into [foo] (concat (map #(hash-map :kind :sql :query retained-scalar-query :args %) rows)
                                    [bar sql-true]))
        r (decode data)
        observed (provider/observe-source-message! node id)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :operations)))
    (is (= arrow/mixed-scalar-sql-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:operations r) (:operations observed)))
    (is (= [1 2 1 1] (:logged-operation-spans r) (:logged-operation-spans observed)))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-mixed-sql-put data))))
    (let [text (#'fixture/find-byte-sequence data "scalar-café")
          keyword-text (#'fixture/find-byte-sequence data "claim/open")]
      (is (some? text))
      (is (some? keyword-text))
      (is (= (assoc-in expected [1 :args 2] "Scalar-café")
             (:operations (decode (#'fixture/edit-byte data text 83)))))
      (doseq [[bad reason] [[(#'fixture/edit-byte data text 255) :invalid-arrow-utf8]
                            [(#'fixture/edit-byte data keyword-text 49) :unsupported-arrow-keyword]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
        (let [r (decode bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence])))))
    (let [data (#'fixture/retained-operation-bytes node integer-id)
          old (arrow/decode-mixed-sql-put data)
          new (decode data)]
      (is (= :matched (:iid-correspondence new)))
      (is (= (dissoc old :profile) (dissoc new :profile))))
    (doseq [[other-id reason] rejected]
      (let [r (decode (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))))

(deftest selected-mixed-scalars-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-scalars-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        p1 [:put-docs :foo {:xt/id 1 :name "a"}]
        p2 [:put-docs :bar {:xt/id "x" :ok true}]
        utc (java.time.ZoneId/of "UTC")
        z (java.time.ZoneId/of "Z")
        a (java.time.Instant/parse "1969-12-31T23:59:59.999999Z")
        b (java.time.Instant/parse "2026-01-01T00:00:00.000001Z")
        rows [[Long/MIN_VALUE false "scalar-café" :claim/open #uuid "00112233-4455-6677-8899-aabbccddeeff" (.atZone a z) (.atZone a utc)]
              [Long/MAX_VALUE true "" :claim/done #uuid "ffffffff-ffff-ffff-ffff-ffffffffffff" (.atZone b z) (.atZone b utc)]]]
    (try
      (let [[id integer-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! (into [p1] (concat (map #(into [scalar-query] %) rows) [p2 ["ASSERT TRUE"]])))
                    integer-id (submit! [p1 ["ASSERT ? = ?" 1 1] p2])
                    rejected [[(submit! [p1 ["ASSERT ? IS NOT NULL" [1 2]]]) :unsupported-arrow-field]
                              [(submit! [p1 ["ASSERT ? IS NULL" nil]]) :unsupported-arrow-field]
                              [(submit! [p1 ["ASSERT ? IS NOT NULL" (java.time.ZonedDateTime/parse "2026-01-01T00:00:00-08:00[America/Los_Angeles]")]]) :unsupported-arrow-timestamp-type]]]
                (check-mixed-scalars! node id integer-id rejected rows)
                [id integer-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (first (peek rejected)) (Duration/ofSeconds 15))
          (check-mixed-scalars! node id integer-id rejected rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-mixed-recursive-lists! [node id depth-id scalar-id rejected rows]
  (let [decode arrow/decode-mixed-recursive-list-sql-put
        data (#'fixture/retained-operation-bytes node id)
        expected (into (mapv #(hash-map :kind :sql
                                       :query "ASSERT ($1 IS NOT NULL AND ($2 = 0) AND $3 IS NOT NULL)"
                                       :args %) rows)
                       [foo sql-true bar])
        r (decode data)
        observed (provider/observe-source-message! node id)
        ;; SQL is the first physical union leg. Locate its first argument
        ;; relation using the fixture's Arrow metadata buffer offsets.
        buffers (#'fixture/buffer-starts data)
        start (nth buffers 8)
        b (doto (java.nio.ByteBuffer/wrap data) (.order java.nio.ByteOrder/LITTLE_ENDIAN))
        end (+ start (.getInt b (+ (nth buffers 7) 4)))
        args (Arrays/copyOfRange data start end)
        [_ offsets _ inner-offsets _ values] (#'fixture/buffer-starts args)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :operations)))
    (is (= arrow/mixed-recursive-list-sql-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:operations r) (:operations observed)))
    (is (= [3 1 1 1] (:logged-operation-spans r) (:logged-operation-spans observed)))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-mixed-scalar-sql-put data))))
    (is (= (assoc-in expected [0 :args 0 0 0] 99)
           (:operations (decode (#'fixture/edit-long data (+ start values) 99)))))
    (doseq [[bad reason] [[(#'fixture/edit-int data (+ start offsets) 1) :invalid-arrow-list-offsets]
                          [(#'fixture/edit-int data (+ start inner-offsets) 1) :invalid-arrow-list-offsets]
                          [(#'fixture/edit-int data (+ start inner-offsets 4) -1) :invalid-arrow-list-offsets]
                          [(#'fixture/edit-byte data start 0) :invalid-arrow-framing]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))
    (let [r (decode (#'fixture/retained-operation-bytes node depth-id))]
      (is (= :decoded (:status r)))
      (is (= [foo {:kind :sql :query "ASSERT $1 IS NOT NULL" :args [(nth (iterate vector 1) 8)]} bar]
             (:operations r) (:operations (provider/observe-source-message! node depth-id))))
      (is (= [1 1 1] (:logged-operation-spans r))))
    (let [data (#'fixture/retained-operation-bytes node scalar-id)
          old (arrow/decode-mixed-scalar-sql-put data)
          new (decode data)]
      (is (= :matched (:iid-correspondence new)))
      (is (= (dissoc old :profile) (dissoc new :profile))))
    (doseq [[other-id reason] rejected]
      (let [r (decode (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))))

(deftest selected-mixed-recursive-lists-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-recursive-lists-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        p1 [:put-docs :foo {:xt/id 1 :name "a"}]
        p2 [:put-docs :bar {:xt/id "x" :ok true}]
        rows [[[[1 2] [] [3]] 0 [[1 "x"] [true :open]]]
              [[[4] [5 6]] 1 [[] [2 false]]]
              [[] -1 []]]]
    (try
      (let [[id depth-id scalar-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! (into (mapv #(into ["ASSERT (? IS NOT NULL AND (? = 0) AND ? IS NOT NULL)"] %) rows)
                                      [p1 ["ASSERT TRUE"] p2]))
                    depth-id (submit! [p1 ["ASSERT ? IS NOT NULL" (nth (iterate vector 1) 8)] p2])
                    scalar-id (submit! [p1 ["ASSERT ? IS NOT NULL" "scalar"] p2])
                    rejected [[(submit! [p1 ["ASSERT ? IS NOT NULL" (nth (iterate vector 1) 9)]]) :arrow-list-depth-limit]
                              [(submit! [p1 ["ASSERT ? IS NOT NULL" [1 [2]]]]) :unsupported-arrow-list-union-leg]
                              [(submit! [p1 ["ASSERT ? IS NULL" nil]]) :unsupported-arrow-field]]]
                (check-mixed-recursive-lists! node id depth-id scalar-id rejected rows)
                [id depth-id scalar-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (first (peek rejected)) (Duration/ofSeconds 15))
          (check-mixed-recursive-lists! node id depth-id scalar-id rejected rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(def ^:private nullable-query
  "ASSERT (? IS NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL AND ? IS NOT NULL)")

(def ^:private retained-nullable-query
  "ASSERT ($1 IS NULL AND $2 IS NOT NULL AND $3 IS NOT NULL AND $4 IS NOT NULL AND $5 IS NOT NULL AND $6 IS NOT NULL AND $7 IS NOT NULL AND $8 IS NOT NULL)")

(defn- check-mixed-nullable! [node id utc-id old-id rejected rows utc]
  (let [decode arrow/decode-mixed-nullable-sql-put
        data (#'fixture/retained-operation-bytes node id)
        expected (into (mapv #(hash-map :kind :sql :query retained-nullable-query :args %) rows)
                       [foo sql-true bar])
        r (decode data)
        buffers (#'fixture/buffer-starts data)
        start (nth buffers 8)
        b (doto (java.nio.ByteBuffer/wrap data) (.order java.nio.ByteOrder/LITTLE_ENDIAN))
        end (+ start (.getInt b (+ (nth buffers 7) 4)))
        args (Arrays/copyOfRange data start end)
        [int-valid int-values bool-valid _ text-valid text-offsets _ :as arg-buffers] (#'fixture/buffer-starts args)
        list-valid (nth arg-buffers 14)
        null-node (#'struct-fixture/node-position args 0)
        int-node (#'struct-fixture/node-position args 1)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :operations)))
    (is (= arrow/mixed-nullable-sql-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:operations r)))
    (is (= [3 1 1 1] (:logged-operation-spans r)))
    (is (= :unsupported-sql-argument (:reason (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-mixed-recursive-list-sql-put data))))
    (is (= (assoc-in expected [0 :args 1] 99)
           (:operations (decode (#'fixture/edit-long data (+ start int-values) 99)))))
    ;; Null fixed-width padding is not a logical value. Valid false and
    ;; present empty text/vector must not become nil merely by truthiness.
    (is (= expected (:operations (decode (#'fixture/edit-long data (+ start int-values 8) 99)))))
    (is (= (assoc-in expected [1 :args 1] 0)
           (:operations (decode (#'fixture/edit-byte data (+ start int-valid) 7)))))
    (is (= (assoc-in expected [0 :args 2] nil)
           (:operations (decode (#'fixture/edit-byte data (+ start bool-valid) 4)))))
    (is (= (assoc-in expected [1 :args 7] [])
           (:operations (decode (#'fixture/edit-byte data (+ start list-valid) 7)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data (+ start list-valid) 4) :unsupported-arrow-nullable-list-extent]
                          [(#'fixture/edit-long data (+ start null-node) 0) :unsupported-arrow-null-only-layout]
                          [(#'fixture/edit-long data (+ start null-node 8) 0) :unsupported-arrow-null-only-layout]
                          [(#'fixture/edit-long data (+ start int-node 8) 0) :arrow-null-count-mismatch]
                          [(#'fixture/edit-byte data (+ start text-valid) 4) :unsupported-arrow-nullable-value-extent]
                          [(#'fixture/edit-int data (+ start text-offsets 4) -1) :invalid-arrow-value-offsets]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))
    (let [r (decode (#'fixture/retained-operation-bytes node utc-id))]
      (is (= :decoded (:status r)))
      (is (= [foo {:kind :sql :query "ASSERT $1 IS NOT NULL" :args [utc]}
              {:kind :sql :query "ASSERT $1 IS NOT NULL" :args [nil]} bar]
             (:operations r)))
      (is (= [1 2 1] (:logged-operation-spans r))))
    (let [data (#'fixture/retained-operation-bytes node old-id)
          old (arrow/decode-mixed-recursive-list-sql-put data)]
      (is (= :decoded (:status old)))
      (is (= (dissoc old :profile) (dissoc (decode data) :profile))))
    (doseq [other-id rejected]
      (let [r (decode (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))))

(deftest selected-mixed-nullable-columns-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-nullable-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        p1 [:put-docs :foo {:xt/id 1 :name "a"}]
        p2 [:put-docs :bar {:xt/id "x" :ok true}]
        t (java.time.Instant/parse "2026-01-01T00:00:00.000001Z")
        z (.atZone t (java.time.ZoneId/of "Z"))
        utc (.atZone t (java.time.ZoneId/of "UTC"))
        rows [[nil 7 false "café" :claim/open #uuid "00112233-4455-6677-8899-aabbccddeeff" z [[1 2]]]
              [nil nil nil nil nil nil nil nil]
              [nil -1 true "" :claim/done #uuid "ffffffff-ffff-ffff-ffff-ffffffffffff" z []]]]
    (try
      (let [[id utc-id old-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! (into (mapv #(into [nullable-query] %) rows) [p1 ["ASSERT TRUE"] p2]))
                    utc-id (submit! [p1 ["ASSERT ? IS NOT NULL" utc] ["ASSERT ? IS NOT NULL" nil] p2])
                    old-id (submit! [p1 ["ASSERT ? IS NOT NULL" [[1 2]]] p2])
                    rejected [(submit! [p1 ["ASSERT ? IS NOT NULL" [1 nil]]])
                              (submit! [p1 ["ASSERT ? IS NOT NULL" {:a 1}]])]]
                (check-mixed-nullable! node id utc-id old-id rejected rows utc)
                [id utc-id old-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek rejected) (Duration/ofSeconds 15))
          (check-mixed-nullable! node id utc-id old-id rejected rows utc)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- argument-name-offset [^bytes data start name]
  ;; Search only the embedded schema metadata, not argument values.
  (let [b (doto (java.nio.ByteBuffer/wrap data) (.order java.nio.ByteOrder/LITTLE_ENDIAN))
        end (+ start 8 (.getInt b (+ start 4)))
        needle (vec (.getBytes ^String name java.nio.charset.StandardCharsets/UTF_8))
        positions (filterv (fn [i] (= needle (mapv #(aget data %) (range i (+ i (count needle))))))
                           (range (+ start 8) (- (inc end) (count needle))))]
    (assert (= 1 (count positions)))
    (first positions)))

(defn- check-mixed-reordered! [node id old-id rows query]
  (let [decode arrow/decode-mixed-positional-sql-put
        data (#'fixture/retained-operation-bytes node id)
        expected (into (mapv #(hash-map :kind :sql :query query :args %) rows) [foo sql-true bar])
        r (decode data)
        start (nth (#'fixture/buffer-starts data) 8)
        name-start (argument-name-offset data start "?_8")]
    (is (= :decoded (:status r)) (pr-str (dissoc r :operations)))
    (is (= arrow/mixed-positional-sql-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:operations r)))
    (is (= [3 1 1 1] (:logged-operation-spans r)))
    ;; This fixture is physically reordered on the pinned XTDB writer.
    (is (= :unsupported-arrow-field (:reason (arrow/decode-mixed-nullable-sql-put data))))
    (doseq [bad [(#'fixture/edit-byte data (+ name-start 2) (int \0))
                 (#'fixture/edit-byte data (+ name-start 2) (int \9))
                 (#'fixture/edit-byte data name-start (int \x))]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)))
        (is (= :unsupported-arrow-argument-column-names (:reason r)))
        (is (not-any? #(contains? r %) [:groups :operations :logged-operation-spans :iid-correspondence]))))
    ;; Exchange the names of two differently typed columns without moving
    ;; any physical data. The positional values, not their types, move.
    (let [other (argument-name-offset data start "?_1")
          swapped (-> data (#'fixture/edit-byte (+ name-start 2) (int \1))
                      (#'fixture/edit-byte (+ other 2) (int \8)))
          swapped-rows (mapv #(assoc % 1 (nth % 8) 8 (nth % 1)) rows)]
      (is (= (into (mapv #(hash-map :kind :sql :query query :args %) swapped-rows) [foo sql-true bar])
             (:operations (decode swapped)))))
    (let [data (#'fixture/retained-operation-bytes node old-id)
          old (arrow/decode-mixed-nullable-sql-put data)]
      (is (= :decoded (:status old)))
      (is (= (dissoc old :profile) (dissoc (decode data) :profile))))))

(deftest selected-mixed-reordered-columns-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-positional-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        p1 [:put-docs :foo {:xt/id 1 :name "a"}]
        p2 [:put-docs :bar {:xt/id "x" :ok true}]
        z (java.time.ZonedDateTime/parse "2026-01-01T00:00:00.000001Z")
        utc (java.time.ZonedDateTime/parse "2026-01-01T00:00:00.000001Z[UTC]")
        rows [[nil 7 false "café" :claim/open #uuid "00112233-4455-6677-8899-aabbccddeeff" z utc [[1 2]]]
              [nil nil nil nil nil nil nil nil nil]
              [nil -1 true "" :claim/done #uuid "ffffffff-ffff-ffff-ffff-ffffffffffff" z utc []]]
        query (str nullable-query " AND ? IS NOT NULL")
        retained-query (str retained-nullable-query " AND $9 IS NOT NULL")]
    (try
      (let [[id old-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! (into (mapv #(into [query] %) rows) [p1 ["ASSERT TRUE"] p2]))
                    old-id (submit! [p1 ["ASSERT ? IS NOT NULL" nil] p2])]
                (check-mixed-reordered! node id old-id rows retained-query)
                [id old-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) old-id (Duration/ofSeconds 15))
          (check-mixed-reordered! node id old-id rows retained-query)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
