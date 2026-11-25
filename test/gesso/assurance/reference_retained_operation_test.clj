(ns gesso.assurance.reference-retained-operation-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-operation-correspondence :as operation]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-source-payload :as payload]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)
           (xtdb.api.log Log SourceMessage$FlushBlock)))

(defn- no-match! [r]
  (is (not-any? #(contains? r %) [:operation-correspondence :operation-message-id
                                 :payload-correspondence :payload-fingerprint
                                 :operations :tx-ops :message :bytes]))
  (is (= :none (:authority r)))
  (is (= :not-issued (:certificate r))))

(defn- check-retained! [node [flush-id tx-id other-id]]
  (let [observe! #(coverage/observe-local-retained-operation! node %)
        compare! operation/compare-operation-bytes!
        payload! payload/decode-payload
        calls (atom 0)
        r (with-redefs [operation/compare-operation-bytes!
                        (fn [node bytes] (swap! calls inc) (compare! node bytes))]
            (observe! tx-id))]
    (is (= 1 @calls))
    (is (= :observed (:status r)) (pr-str r))
    (is (= coverage/retained-operation-profile (:retained-operation-profile r)))
    (is (= operation/profile (:operation-profile r)))
    (is (= :matched (:payload-correspondence r) (:operation-correspondence r)))
    (is (= tx-id (:operation-message-id r)))
    (is (= [tx-id other-id] (:transaction-ids r)))
    (is (= [flush-id tx-id other-id] (:record-offsets r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (not-any? #(contains? r %) [:operations :tx-ops :payload-fingerprint :processed-message-id]))
    (doseq [id [flush-id (inc tx-id) Long/MAX_VALUE]]
      (let [r (observe! id)]
        (is (= :unsupported (:status r)))
        (is (= :retained-transaction-not-framed (:reason r)))
        (no-match! r)))
    (let [r (observe! other-id)]
      (is (= :unsupported (:status r)))
      (is (= :retained-operation-not-matched (:reason r)))
      (is (= other-id (:message-id r)))
      (no-match! r))
    (with-redefs [operation/compare-operation-bytes!
                  (fn [& _] (throw (AssertionError. "must not compare mismatched envelope")))
                  payload/decode-payload
                  (fn [bytes] (let [r (payload! bytes)]
                                (cond-> r (= :transaction (get-in r [:message :kind]))
                                  (assoc-in [:message :user] "different"))))]
      (let [r (observe! tx-id)]
        (is (= :mismatch (:status r)))
        (is (= :source-payload-mismatch (:reason r)))
        (is (= tx-id (:message-id r)))
        (no-match! r)))
    (doseq [bad [{:status :mismatch :reason :operation-correspondence-mismatch :field :operations}
                 {:status :unavailable :reason :provider-operation-unavailable}
                 {:status :matched :operation-correspondence :matched}]]
      (with-redefs [operation/compare-operation-bytes! (constantly bad)]
        (let [r (observe! tx-id)]
          (is (= (if (= :matched (:status bad)) :unsupported (:status bad)) (:status r)))
          (is (= :retained-operation-not-matched (:reason r)))
          (is (= tx-id (:message-id r)))
          (no-match! r))))
    ;; A later envelope mismatch must invalidate an earlier operation match.
    (let [seen (atom 0)]
      (with-redefs [payload/decode-payload
                    (fn [bytes] (let [r (payload! bytes)]
                                  (cond-> r (and (= :transaction (get-in r [:message :kind]))
                                                  (= 2 (swap! seen inc)))
                                    (assoc-in [:message :user] "different"))))]
        (let [r (observe! tx-id)]
          (is (= :mismatch (:status r)))
          (is (= other-id (:message-id r)))
          (no-match! r))))
    ;; The final byte reread remains inside the same observation.
    (let [read! @#'coverage/read-local-payloads!
          seen (atom 0)]
      (with-redefs-fn {#'coverage/read-local-payloads!
                      (fn [file frames]
                        (let [sample (read! file frames)]
                          (if (= 2 (swap! seen inc))
                            (let [copy (aclone ^bytes (nth sample 1))]
                              (aset-byte copy 0 (unchecked-byte (bit-xor 1 (aget copy 0))))
                              (assoc sample 1 copy))
                            sample)))}
        (fn [] (let [r (observe! tx-id)]
                 (is (= :unavailable (:status r)))
                 (is (= :source-payload-changed-during-observation (:reason r)))
                 (no-match! r)))))
    (is (= r (observe! tx-id)))))

(deftest retained-operation-requires-exact-local-transaction
  (doseq [id [nil -1 "0" 1.5 (inc (bigint Long/MAX_VALUE))]]
    (let [r (coverage/observe-local-retained-operation! nil id)]
      (is (= :invalid-retained-transaction-id (:reason r)))
      (no-match! r)))
  (is (= :embedded-primary-log-required (:reason (coverage/observe-local-retained-operation! nil 0))))
  (with-open [node (xtn/start-node {})]
    (is (= :unsupported-source-log (:reason (coverage/observe-local-retained-operation! node 0))))))

(deftest retained-operation-correspondence-from-persistent-log
  (let [root (Files/createTempDirectory "gesso-retained-operation-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        put [:put-docs :foo {:xt/id 1 :name "a"}]]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        ^Log log (.getSourceLog database)
                        flush-id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))
                        tx-id (:tx-id (xt/submit-tx node [put ["ASSERT ? IS NOT NULL" [[1 2] []]]]))
                        other-id (:tx-id (xt/submit-tx node [["ASSERT TRUE"]]))
                        ids [flush-id tx-id other-id]]
                    (.awaitSourceBlocking database other-id (Duration/ofSeconds 15))
                    (check-retained! node ids)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 15))
          (check-retained! node ids)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
