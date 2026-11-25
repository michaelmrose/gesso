(ns gesso.assurance.reference-arrow-struct-test
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

(defn- check-retained! [node id unsupported rows]
  (let [data (#'fixture/retained-operation-bytes node id)
        decoded (arrow/decode-struct-map-operations data)
        expected (mapv #(hash-map :kind :sql :query "ASSERT ($1 IS NOT NULL AND ($2 = 0))" :args %) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        [_ _ _ values _ counts] (#'fixture/buffer-starts args)
        key-offset (#'fixture/find-byte-sequence args "status")]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/struct-map-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded)))
    (is (= [(count rows)] (:logged-operation-spans decoded)))
    (is (= rows (:rows (arrow/decode-struct-map-arguments args))))
    ;; The provider observer deliberately does not admit map-valued SQL args.
    (is (= :unsupported-sql-argument (:reason (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-recursive-list-operations data))))
    (is (= (assoc-in rows [0 0 :count] 99)
           (:rows (arrow/decode-struct-map-arguments (#'fixture/edit-long args counts 99)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte args key-offset 83) :unsupported-arrow-struct-key]
                          [(#'fixture/edit-byte args values 255) :invalid-arrow-utf8]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-struct-map-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))
    (doseq [[other-id reason] unsupported]
      (let [r (arrow/decode-struct-map-operations (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :operations)))))))

(deftest selected-structs-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-struct-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        rows [[{:status :open :count 1 :nested {:ok true :items [1 "x"]}} 0]
              [{:status :closed :count 2 :nested {:ok false :items []}} 1]]]
    (is (= [[]] (:rows (arrow/decode-struct-map-arguments nil))))
    (try
      (let [[id unsupported]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! (mapv #(into ["ASSERT (? IS NOT NULL AND (? = 0))"] %) rows))
                    bad-key (submit! [["ASSERT ? IS NOT NULL" {:claim/status :open}]])
                    deep (submit! [["ASSERT ? IS NOT NULL" (nth (iterate #(hash-map :child %) 1) 9)]])
                    unsupported [[bad-key :unsupported-arrow-struct-key] [deep :arrow-struct-depth-limit]]]
                (check-retained! node id unsupported rows)
                [id unsupported]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (ffirst (reverse unsupported)) (Duration/ofSeconds 10))
          (check-retained! node id unsupported rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-document-keys! [node id rows]
  (let [data (#'fixture/retained-operation-bytes node id)
        decoded (arrow/decode-document-key-operations data)
        expected (mapv #(hash-map :kind :sql :query "ASSERT $1 IS NOT NULL" :args %) rows)
        args (:argument-bytes (first (:groups (arrow/decode-operations data))))
        request (#'fixture/find-byte-sequence args "request_id")
        other (#'fixture/find-byte-sequence args "request_ix")
        lease (#'fixture/find-byte-sequence args "gesso$live$outbox$lease_until")]
    (is (= :decoded (:status decoded)) (pr-str (dissoc decoded :operations)))
    (is (= arrow/document-key-profile (:profile decoded)))
    (is (= :none (:authority decoded)))
    (is (= :not-issued (:certificate decoded)))
    (is (= expected (:operations decoded)))
    (is (= [(count rows)] (:logged-operation-spans decoded)))
    (is (= rows (:rows (arrow/decode-document-key-arguments args))))
    (is (= :unsupported-arrow-struct-key (:reason (arrow/decode-struct-map-operations data))))
    (is (= :unsupported-sql-argument (:reason (provider/observe-source-message! node id))))
    (is (some? (#'fixture/find-byte-sequence args "_id")))
    (is (some? (#'fixture/find-byte-sequence args "gesso$live$journal$command_id")))
    (is (= (mapv #(update % 0 (fn [m] (assoc (dissoc m :request-id) :request-iz (:request-id m)))) rows)
           (:rows (arrow/decode-document-key-arguments (#'fixture/edit-byte args (+ request 9) 122)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte args (+ other 9) 100) :duplicate-arrow-struct-key]
                          [(#'fixture/edit-byte args (+ request 8) 95) :unsupported-arrow-struct-key]
                          [(#'fixture/edit-byte args (+ lease 6) 95) :unsupported-arrow-struct-key]
                          [(Arrays/copyOf args (dec (alength args))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-document-key-arguments bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :rows)))))))

(deftest selected-document-key-spellings
  (let [key! #'arrow/struct-key! allowed #{:document-keys}]
    (doseq [[raw expected] [["_id" "xt/id"] ["claim$status" "claim/status"]
                            ["gesso$live$outbox$lease_until" "gesso.live.outbox/lease-until"]
                            ["request_id" "request-id"] ["a$b" "a/b"] ["a_b" "a-b"]]]
      (is (= expected (key! raw allowed 0))))
    (doseq [[raw depth] [["_id" 1] ["xt$id" 0] ["xt$valid_from" 0]
                         ["a$$b" 0] ["a$" 0] ["$a" 0] ["a__b" 0]
                         ["a_" 0] ["a-b" 0] ["a.b$c" 0] ["a_b$c" 0] ["A" 0]]]
      (is (= :unsupported-arrow-struct-key
             (try (key! raw allowed depth) :accepted
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest selected-document-keys-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-document-keys-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        deadline (java.time.ZonedDateTime/parse "2026-01-01T00:00:00Z[UTC]")
        rows [[{:xt/id "doc1" :request-id "r1" :request-ix "x1"
                :gesso.live.outbox/lease-until deadline
                :nested {:gesso.live.journal/command-id "c1" :a-b 1 :a/b 2}}]
              [{:xt/id "doc2" :request-id "r2" :request-ix "x2"
                :gesso.live.outbox/lease-until deadline
                :nested {:gesso.live.journal/command-id "c2" :a-b 3 :a/b 4}}]]]
    (is (= [[]] (:rows (arrow/decode-document-key-arguments nil))))
    (try
      (let [id (with-open [node (xtn/start-node config)]
                 (let [database (db/primary-db node)
                       id (:tx-id (xt/submit-tx node (mapv #(into ["ASSERT ? IS NOT NULL"] %) rows)))]
                   (.awaitTxBlocking database id (Duration/ofSeconds 10))
                   (check-document-keys! node id rows)
                   id))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) id (Duration/ofSeconds 10))
          (check-document-keys! node id rows)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
