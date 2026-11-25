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
