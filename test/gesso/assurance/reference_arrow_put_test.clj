(ns gesso.assurance.reference-arrow-put-test
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
           (java.time Duration Instant)
           (java.util Arrays)))

(defn- check-put! [node id nested-id rejected]
  (let [data (#'fixture/retained-operation-bytes node id)
        r (arrow/decode-put-envelope data)
        expected {:kind :put-docs :table :public/foo
                  :documents [{:xt/id 1 :name "a"} {:xt/id 2 :name "b"}]
                  :iids ["4cd9b7672d7fbee8fb51fb1e049f6903" "420fce314175df402adbeae3cfbbb856"] :bounds {}}
        [tags indexes _ _ iid-offsets _ iids table-tags table-indexes _ doc-offsets _ _ ids _ _ names from-valid from-values]
        (#'fixture/buffer-starts data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/put-envelope-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :unchecked (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-operations data))))
    ;; The diagnostic envelope intentionally does not verify IID identity.
    (is (= [(assoc-in expected [:iids 0] "00d9b7672d7fbee8fb51fb1e049f6903")]
           (:groups (arrow/decode-put-envelope (#'fixture/edit-byte data iids 0)))))
    (is (= [(assoc-in expected [:documents 0 :xt/id] 99)]
           (:groups (arrow/decode-put-envelope (#'fixture/edit-long data ids 99)))))
    (is (= [(assoc-in expected [:documents 0 :name] "z")]
           (:groups (arrow/decode-put-envelope (#'fixture/edit-byte data names 122)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data tags 1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data indexes -1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-byte data table-tags 1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data table-indexes 1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data (+ iid-offsets 4) 1) :unsupported-arrow-put-list-layout]
                          [(#'fixture/edit-int data (+ doc-offsets 4) 3) :unsupported-arrow-put-list-layout]
                          [(#'fixture/edit-byte data from-valid 1) :unsupported-arrow-put-bounds]
                          [(#'fixture/edit-long data from-values 1) :unsupported-arrow-put-bounds]
                          [(#'fixture/edit-byte data names 255) :invalid-arrow-utf8]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-put-envelope bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :groups)))))
    (let [r (arrow/decode-put-envelope (#'fixture/retained-operation-bytes node nested-id))]
      (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
      (is (= :public/gesso_commands (:table (first (:groups r)))))
      (is (= [{:xt/id "c1" :gesso.live.journal/command-id "c1" :nested {:ok true :items [1 "x"]}}]
             (:documents (first (:groups r)))))
      (is (= (:groups r) (:operations (provider/observe-source-message! node nested-id)))))
    (doseq [other-id rejected]
      (let [r (arrow/decode-put-envelope (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)) (pr-str (dissoc r :groups)))
        (is (not (contains? r :groups)))))))

(deftest selected-put-envelope-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        doc {:xt/id "c1" :gesso.live.journal/command-id "c1" :nested {:ok true :items [1 "x"]}}]
    (try
      (let [[id nested-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [[:put-docs :foo {:xt/id 1 :name "a"} {:xt/id 2 :name "b"}]])
                    nested-id (submit! [[:put-docs :gesso_commands doc]])
                    rejected [(submit! [["ASSERT TRUE"]])
                              (submit! [[:put-docs :foo {:xt/id 3}] [:put-docs :bar {:xt/id 4}]])
                              (submit! [["ASSERT TRUE"] [:put-docs :foo {:xt/id 5}]])
                              (submit! [[:put-docs {:into :foo :valid-from (Instant/parse "2026-01-01T00:00:00Z")} {:xt/id 6}]])
                              (submit! [[:put-docs :foo {:xt/id 7 :name "a"} {:xt/id 8}]])]]
                (check-put! node id nested-id rejected)
                [id nested-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek rejected) (Duration/ofSeconds 10))
          (check-put! node id nested-id rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
