(ns gesso.assurance.reference-source-payload-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-payload :as payload]
            [gesso.assurance.reference-source-coverage :as coverage]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.io RandomAccessFile)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant ZoneId)
           (java.util Arrays)
           (xtdb.api.log Log Log$Record SourceMessage$Tx SourceMessage$FlushBlock)))

(defn- wire [xs] (byte-array (map unchecked-byte xs)))
(defn- decode [xs] (payload/decode-payload (wire xs)))

(deftest independent-wire-decoding
  ;; Fixed wire fixtures do not derive their expectations from XTDB's codec.
  (let [tx [3 50 12 10 2 7 8 16 0 26 1 90 34 1 117]
        r (decode tx)]
    (is (= :decoded (:status r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= {:kind :transaction :tx-ops [7 8] :system-time Instant/EPOCH
            :default-tz (ZoneId/of "Z") :user "u"}
           (update (:message r) :tx-ops vec)))
    (is (= {:kind :flush-block :expected-block-index 0}
           (:message (decode [3 10 2 16 0]))))
    (is (= -1 (get-in (decode (concat [3 10 11 16] (repeat 9 255) [1]))
                      [:message :expected-block-index])))
    (is (= Long/MAX_VALUE
           (get-in (decode (concat [3 10 10 16] (repeat 8 255) [127]))
                   [:message :expected-block-index])))
    (is (= Long/MIN_VALUE
           (get-in (decode (concat [3 10 11 16] (repeat 9 128) [1]))
                   [:message :expected-block-index])))
    (is (= (Instant/parse "1969-12-31T23:59:59.999999Z")
           (get-in (decode (concat [3 50 17 10 1 7 16] (repeat 9 255) [1 26 1 90]))
                   [:message :system-time])))
    (let [message (:message (decode [3 50 6 10 1 7 26 1 90]))]
      (is (= :transaction (:kind message)))
      (is (nil? (:system-time message)))
      (is (not (contains? message :user)))
      (is (not (contains? message :user-metadata))))
    (let [message (:message (decode [3 50 12 10 1 7 26 1 90 34 0 42 2 5 6]))]
      (is (= "" (:user message)))
      (is (= [5 6] (vec (:user-metadata message)))))
    (doseq [[bad reason] [[[] :source-payload-byte-limit]
                          [[2] :unsupported-source-header]
                          [[3] :ambiguous-source-message]
                          [[3 10 2 16] :invalid-field-length]
                          [[3 10 0] :missing-flush-field]
                          [[3 18 0] :unsupported-field]
                          [[3 10 2 16 0 10 2 16 1] :duplicate-field]
                          [(concat tx [10 2 16 0]) :ambiguous-source-message]
                          [[3 10 4 16 0 16 1] :duplicate-field]
                          [[3 10 2 8 0] :unsupported-field]
                          [[3 10 2 16 128] :truncated-varint]
                          [[3 10 3 16 128 0] :noncanonical-varint]
                          [(concat [3 10 11 16] (repeat 9 255) [2]) :varint-overflow]
                          [[3 50 0] :missing-transaction-field]
                          [[3 50 6 10 1 7 26 1 255] :invalid-source-utf8]
                          [[3 50 6 10 1 7 26 1 63] :invalid-source-time-or-zone]
                          [[3 50 8 10 1 7 26 1 90 50 0] :unsupported-field]
                          [[3 50 8 10 1 7 26 1 90 42 0] :empty-user-metadata]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)) (pr-str reason r))
        (is (= reason (:reason r)) (pr-str reason r))
        (is (not (contains? r :message)))))
    (is (= :unsupported (:status (payload/decode-payload (byte-array (inc (* 16 1024 1024)))))))))

(defn- compare-physical! [node expected-ids]
  (let [database (db/primary-db node)
        ^Log log (.getSourceLog database)
        end (.getLatestSubmittedOffset log)
        records (vec (iterator-seq (.iterator (.readRecords log 0 (inc end)))))
        path (#'coverage/local-log-path log)]
    (with-open [file (RandomAccessFile. (.toFile path) "r")]
      (let [frames (:frames (#'coverage/local-frames file))]
        (is (= expected-ids (mapv first frames) (mapv #(.getMsgId ^Log$Record %) records)))
        (doseq [[[offset _ length] ^Log$Record record] (map vector frames records)]
          (.seek file (+ offset 13))
          (let [bytes (byte-array length)
                _ (.readFully file bytes)
                r (payload/decode-payload bytes)
                message (:message r)
                provider (.getMessage record)]
            (is (= :decoded (:status r)))
            (is (= :not-issued (:certificate r)))
            (cond
              (instance? SourceMessage$Tx provider)
              (let [^SourceMessage$Tx p provider]
                (is (= :transaction (:kind message)))
                (is (Arrays/equals ^bytes (.getTxOps p) ^bytes (:tx-ops message)))
                (is (= (.getSystemTime p) (:system-time message)))
                (is (= (.getDefaultTz p) (:default-tz message)))
                (is (= (.getUser p) (:user message)))
                (is (nil? (.getExternalSourceToken p)))
                (is (Arrays/equals ^bytes (.getUserMetadata p) ^bytes (:user-metadata message))))
              (instance? SourceMessage$FlushBlock provider)
              (is (= {:kind :flush-block :expected-block-index (.getExpectedBlockIdx ^SourceMessage$FlushBlock provider)} message))
              :else (is false "unexpected provider source variant"))))))))

(defn- assert-payload-coverage! [node ids]
  (let [plain (coverage/observe-local-coverage! node)
        checked (coverage/observe-local-payload-coverage! node)]
    (is (= :observed (:status checked)) (pr-str checked))
    (is (= :matched (:payload-correspondence checked)))
    (is (= coverage/local-payload-profile (:payload-profile checked)))
    (is (= payload/profile (:decoder-profile checked)))
    (is (= plain (dissoc checked :payload-profile :decoder-profile :payload-correspondence)))
    (is (= ids (:record-offsets checked)))
    (is (= [(nth ids 1) (nth ids 2)] (:transaction-ids checked)))
    (is (= 2 (:maintenance-count checked)))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))))

(defn- assert-payload-rejections! [node ids]
  (let [decode! payload/decode-payload]
    ;; Perturb the independent observation without modifying provider data.
    (doseq [[kind field value id]
            [[:transaction :tx-ops (wire [1 2 3]) (nth ids 1)]
             [:transaction :system-time Instant/EPOCH (nth ids 1)]
             [:transaction :default-tz (ZoneId/of "Asia/Tokyo") (nth ids 1)]
             [:transaction :user "unexpected" (nth ids 1)]
             [:transaction :user-metadata (wire [1]) (nth ids 1)]
             [:transaction :kind :flush-block (nth ids 1)]
             [:flush-block :expected-block-index 7 (first ids)]]]
      (with-redefs [payload/decode-payload
                    (fn [bytes]
                      (let [r (decode! bytes)]
                        (cond-> r (= kind (get-in r [:message :kind]))
                          (assoc-in [:message field] value))))]
        (let [r (coverage/observe-local-payload-coverage! node)]
          (is (= :mismatch (:status r)) (pr-str r))
          (is (= :source-payload-mismatch (:reason r)))
          (is (= field (:field r)))
          (is (= id (:message-id r)))
          (is (not (contains? r :payload-correspondence)))
          (is (= :not-issued (:certificate r)))
          (is (= :observed (:status (coverage/observe-local-coverage! node)))))))
    (with-redefs [payload/decode-payload (fn [_] {:status :unsupported :reason :unsupported-field})]
      (let [r (coverage/observe-local-payload-coverage! node)]
        (is (= :unsupported (:status r)))
        (is (= :unsupported-source-payload (:reason r)))
        (is (= :unsupported-field (:decode-reason r)))
        (is (= (first ids) (:message-id r)))
        (is (not (contains? r :payload-correspondence)))))
    (let [read! @#'coverage/read-local-payloads!
          calls (atom 0)]
      (with-redefs-fn {#'coverage/read-local-payloads!
                      (fn [file frames]
                        (let [sample (read! file frames)]
                          (if (= 2 (swap! calls inc))
                            (let [copy (aclone ^bytes (first sample))]
                              (aset-byte copy 0 (unchecked-byte (bit-xor 1 (aget copy 0))))
                              (assoc sample 0 copy))
                            sample)))}
        (fn []
          (let [r (coverage/observe-local-payload-coverage! node)]
            (is (= 2 @calls))
            (is (= :unavailable (:status r)))
            (is (= :source-payload-changed-during-observation (:reason r)))
            (is (not (contains? r :payload-correspondence)))))))))

(deftest payload-coverage-requires-local-backend
  (is (= :embedded-primary-log-required (:reason (coverage/observe-local-payload-coverage! nil))))
  (with-open [node (xtn/start-node {})]
    (let [r (coverage/observe-local-payload-coverage! node)]
      (is (= :unsupported (:status r)))
      (is (= :unsupported-source-log (:reason r)))
      (is (= coverage/local-payload-profile (:payload-profile r)))
      (is (not (contains? r :payload-correspondence))))))

(deftest decode-actual-persistent-source-messages
  (let [root (Files/createTempDirectory "gesso-source-payload-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [db (db/primary-db node)
                        ^Log log (.getSourceLog db)
                        flush! (fn [] (let [id (.getMsgId (.appendMessageBlocking log (SourceMessage$FlushBlock. Long/MAX_VALUE)))]
                                        (.awaitSourceBlocking db id (Duration/ofSeconds 10)) id))
                        tx! (fn [ops opts] (let [id (:tx-id (xt/submit-tx node ops opts))]
                                             (.awaitTxBlocking db id (Duration/ofSeconds 10)) id))
                        ids [(flush!) (tx! [["ASSERT TRUE"]] {:system-time (Instant/parse "2020-01-01T00:00:00Z")})
                             (tx! [["ASSERT FALSE"]] {}) (flush!)]]
                    (compare-physical! node ids)
                    (assert-payload-coverage! node ids)
                    (assert-payload-rejections! node ids)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 10))
          (compare-physical! node ids)
          (assert-payload-coverage! node ids)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
