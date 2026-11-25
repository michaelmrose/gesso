(ns gesso.assurance.reference-operation-correspondence-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-arrow-operations-test :as fixture]
            [gesso.assurance.reference-operation-correspondence :as correspondence]
            [gesso.assurance.xtdb-source-log :as provider]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)))

(defn- check! [node id nullable-id]
  (let [bytes (#'fixture/retained-operation-bytes node id)
        nullable (#'fixture/retained-operation-bytes node nullable-id)
        observed (provider/observe-source-message! node id)
        read! provider/observe-operation-bytes!
        compare! #(correspondence/compare-operation-bytes! node %)
        r (compare! bytes)]
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched (:operation-correspondence r)))
    (is (= correspondence/profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= (select-keys observed [:operations :logged-operation-spans])
           (select-keys (read! node bytes) [:operations :logged-operation-spans])))
    (is (= :decoded (:status (arrow/decode-mixed-bounded-sql-put nullable))))
    (let [r (compare! nullable)]
      (is (= :unsupported (:status r)))
      (is (= :provider-operation-unavailable (:reason r)))
      (is (= :unsupported-sql-argument (:decode-reason r)))
      (is (not (contains? r :operation-correspondence))))
    (with-redefs [provider/observe-operation-bytes! (fn [& _] (throw (AssertionError. "provider must not run")))]
      (is (= :independent-operation-unsupported (:reason (compare! (byte-array [1 2 3])))))
      (is (= :operation-byte-limit (:reason (compare! nil)))))
    (doseq [[field change] [[:operations #(assoc-in % [1 :args 0] [99])]
                            [:operations #(assoc-in % [0 :iids 0] "wrong")]
                            [:operations #(vec (reverse %))]
                            [:logged-operation-spans #(vec (repeat (count %) 1))]]]
      (with-redefs [provider/observe-operation-bytes!
                    (fn [node bytes] (update (read! node bytes) field change))]
        (let [r (compare! bytes)]
          (is (= :mismatch (:status r)))
          (is (= field (:field r)))
          (is (= :operation-correspondence-mismatch (:reason r)))
          (is (not (contains? r :operation-correspondence)))
          (is (not-any? #(contains? r %) [:operations :logged-operation-spans :bytes])))))
    (with-redefs [provider/observe-operation-bytes! (fn [& _] {:status :unavailable :reason :provider-read-failed})]
      (is (= :unavailable (:status (compare! bytes)))))
    (is (= r (compare! bytes)))))

(deftest selected-mixed-operation-correspondence
  (let [root (Files/createTempDirectory "gesso-operation-correspondence-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        put [:put-docs :foo {:xt/id 1 :name "a"}]]
    (try
      (let [[id nullable-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! [put ["ASSERT ? IS NOT NULL" [[1 2] []]]
                                 ["ASSERT ? IS NOT NULL" [[3]]] ["ASSERT TRUE"]])
                    nullable-id (submit! [put ["ASSERT ? IS NULL" nil]])]
                (check! node id nullable-id)
                [id nullable-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) nullable-id (Duration/ofSeconds 15))
          (check! node id nullable-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
