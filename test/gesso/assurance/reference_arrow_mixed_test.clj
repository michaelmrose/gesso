(ns gesso.assurance.reference-arrow-mixed-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-arrow-operations-test :as fixture]
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
