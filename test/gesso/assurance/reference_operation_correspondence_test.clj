(ns gesso.assurance.reference-operation-correspondence-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-arrow-operations-test :as fixture]
            [gesso.assurance.reference-arrow-values :as values]
            [gesso.assurance.reference-source-coverage :as coverage]
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

(defn- check-single-layouts! [node [sql-id put-id nullable-id unsupported-id]]
  (let [decode arrow/decode-bounded-operations
        compare! #(correspondence/compare-operation-bytes! node %)
        read! #(#'fixture/retained-operation-bytes node %)]
    (doseq [[id kind iid-status spans] [[sql-id :sql :not-applicable [2 1]]
                                       [put-id :put-docs :matched [1 1]]]]
      (let [bytes (read! id)
            r (decode bytes)
            observed (provider/observe-source-message! node id)
            match (compare! bytes)
            tags (first (#'fixture/buffer-starts bytes))]
        (is (= :decoded (:status r)) (pr-str r))
        (is (= arrow/bounded-operation-profile (:profile r)))
        (is (= iid-status (:iid-correspondence r)))
        (is (every? #(= kind (:kind %)) (:operations r)))
        (is (= (:operations r) (:operations observed)))
        (is (= spans (:logged-operation-spans r) (:logged-operation-spans observed)))
        (is (= :matched (:status match)) (pr-str match))
        (is (= arrow/bounded-operation-profile (:decoder-profile match)))
        (is (= :unsupported (:status (arrow/decode-mixed-bounded-sql-put bytes))))
        (is (not-any? #(contains? match %) [:operations :logged-operation-spans :bytes]))
        (with-redefs [provider/observe-operation-bytes!
                      (fn [& _] (throw (AssertionError. "provider must not run")))]
          (is (= :independent-operation-unsupported
                 (:reason (compare! (#'fixture/edit-byte bytes tags 1))))))
        (let [retained (coverage/observe-local-processed-retained-operation! node id)]
          (is (= :observed (:status retained)) (pr-str retained))
          (is (= id (:operation-message-id retained)))
          (is (= :matched (:operation-correspondence retained))))))
    (let [r (compare! (read! nullable-id))]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-sql-argument (:decode-reason r))))
    (with-redefs [provider/observe-operation-bytes!
                  (fn [& _] (throw (AssertionError. "provider must not run")))]
      (is (= :independent-operation-unsupported (:reason (compare! (read! unsupported-id)))))
      (doseq [id [sql-id put-id]]
        (with-redefs [values/max-value-slots 0]
          (let [r (compare! (read! id))]
            (is (= :unsupported (:status r)))
            (is (= :arrow-aggregate-value-limit (:decode-reason r)))
            (is (not (contains? r :operation-correspondence)))))))))

(deftest selected-single-layout-correspondence
  (let [root (Files/createTempDirectory "gesso-single-layout-correspondence-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                            (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                        ids [(submit! [["ASSERT ? IS NOT NULL" [[1 2] []]]
                                       ["ASSERT ? IS NOT NULL" [[3]]] ["ASSERT TRUE"]])
                             (submit! [[:put-docs :foo {:xt/id 1 :items [[1 2]]}]
                                       [:put-docs :bar {:xt/id "x" :ok false}]])
                             (submit! [["ASSERT ? IS NULL" nil]])
                             (submit! [["ASSERT ? IS NOT NULL" 1.5]])]]
                    (check-single-layouts! node ids)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
          (check-single-layouts! node ids)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
