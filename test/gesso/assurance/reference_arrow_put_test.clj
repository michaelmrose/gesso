(ns gesso.assurance.reference-arrow-put-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-document-iid :as iid]
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

(def ^:private integer-iid-vectors
  [[Long/MIN_VALUE "2df43a3eaece5bb912a43ce16ebdf392"]
   [-42 "1f0370ed2a176aa73868570d90204370"]
   [-1 "f105b11df43d5d321f5c773ef904af97"]
   [0 "a4e167a76a05add8a8654c169b07b044"]
   [1 "4cd9b7672d7fbee8fb51fb1e049f6903"]
   [2 "420fce314175df402adbeae3cfbbb856"]
   [42 "ebc35dc1b8e2602b72beb8d8e5bcdb2b"]
   [Long/MAX_VALUE "eda7a99bc44462f5181f63a88e2ab9d8"]])

(deftest selected-integer-iid-known-answers
  (doseq [[id expected] integer-iid-vectors]
    (is (= expected (iid/integer-iid id))))
  (doseq [id [nil true "1" :one #uuid "00112233-4455-6677-8899-aabbccddeeff"
              1.0 (inc (bigint Long/MAX_VALUE)) (dec (bigint Long/MIN_VALUE))]]
    (is (= :unsupported-reference-integer-id
           (try (iid/integer-iid id) :accepted
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn- replace-iid [^bytes data offset hex]
  (let [copy (aclone data)
        replacement (.parseHex (java.util.HexFormat/of) ^String hex)]
    (System/arraycopy replacement 0 copy offset 16)
    copy))

(defn- check-unified-iids! [data expected iids]
  (let [r (arrow/decode-verified-put data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/verified-iid-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r)))
    (doseq [[bad reason] [[(#'fixture/edit-byte data iids
                                          (bit-xor 1 (bit-and 255 (aget ^bytes data iids))))
                           :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (arrow/decode-verified-put bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)))
        (is (= arrow/verified-iid-put-profile (:profile r)))
        (is (not (contains? r :groups)))
        (is (not (contains? r :iid-correspondence)))))))

(defn- check-integer-iids! [node id other-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents (mapv (fn [[id _]] {:xt/id id}) integer-iid-vectors)
                  :iids (mapv second integer-iid-vectors) :bounds {}}
        [_ _ _ _ _ _ iids _ _ _ _ _ _ ids] (#'fixture/buffer-starts data)
        r (arrow/decode-integer-iid-put data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/integer-iid-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unchecked (:iid-correspondence (arrow/decode-put-envelope data))))
    (check-unified-iids! data expected iids)
    (let [changed-id (#'fixture/edit-long data ids 99)
          changed-iid (replace-iid data iids "b69e060cf32e96bdbfe841366924c30a")
          swapped (-> data (replace-iid iids (second (nth integer-iid-vectors 1)))
                      (replace-iid (+ iids 16) (second (first integer-iid-vectors))))
          matching (replace-iid changed-id iids "b69e060cf32e96bdbfe841366924c30a")]
      (doseq [[bad reason] [[changed-id :document-iid-mismatch]
                            [changed-iid :document-iid-mismatch]
                            [swapped :document-iid-mismatch]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(#'fixture/retained-operation-bytes node other-id) :unsupported-reference-integer-id]]]
        (let [r (arrow/decode-integer-iid-put bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :groups)))
          (is (not (contains? r :iid-correspondence)))))
      (is (= [(-> expected (assoc-in [:documents 0 :xt/id] 99)
                  (assoc-in [:iids 0] "b69e060cf32e96bdbfe841366924c30a"))]
             (:groups (arrow/decode-integer-iid-put matching))))
      (is (= :decoded (:status (arrow/decode-put-envelope changed-id))))
      (is (= :decoded (:status (arrow/decode-put-envelope changed-iid)))))))

(deftest selected-integer-iids-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-integer-iid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs (mapv (fn [[id _]] {:xt/id id}) integer-iid-vectors)]
    (try
      (let [[id other-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    other-id (submit! [[:put-docs :foo {:xt/id "1"}]])]
                (check-integer-iids! node id other-id)
                [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-integer-iids! node id other-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(def ^:private string-iid-vectors
  [["a" "4cf6829aa93728e8f3c97df913fb1bfa"]
   ["1" "e8bc163c82eee18733288c7d4ac636db"]
   ["" "043a718774c572bd8a25adbeb1bfcd5c"]
   [" " "0382da550604f12454021e4fb989f6ea"]
   ["café" "a54a5be2ac9bbd41c79f6b00c7e9edb2"]
   ["cafe\u0301" "807ca4ae083c2b0a363f98e4d55a6f83"]
   ["東京" "9887c12485b538f82638be20ca2926d0"]
   ["😀" "4230acd329d78cb68c99dc6587d2e687"]
   ["a\u0000b" "272c7bb3ace4ac06dab05b9eb26dc513"]])

(deftest selected-string-iid-known-answers
  (doseq [[id expected] string-iid-vectors]
    (is (= expected (iid/string-iid id))))
  (is (not= (iid/string-iid "1") (iid/integer-iid 1)))
  (is (not= (iid/string-iid "café") (iid/string-iid "cafe\u0301")))
  (is (= 32 (count (iid/string-iid (apply str (repeat 4096 "a"))))))
  (doseq [id [nil 1 true :one #uuid "00112233-4455-6677-8899-aabbccddeeff"
              (str (char 0xd800)) (str (char 0xdc00)) (apply str (repeat 4097 "a"))]]
    (is (= :unsupported-reference-string-id
           (try (iid/string-iid id) :accepted
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn- check-string-iids! [node id other-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents (mapv (fn [[id _]] {:xt/id id}) string-iid-vectors)
                  :iids (mapv second string-iid-vectors) :bounds {}}
        [_ _ _ _ _ _ iids _ _ _ _ _ _ _ ids] (#'fixture/buffer-starts data)
        r (arrow/decode-string-iid-put data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/string-iid-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unchecked (:iid-correspondence (arrow/decode-put-envelope data))))
    (check-unified-iids! data expected iids)
    (is (= :unsupported-reference-integer-id (:reason (arrow/decode-integer-iid-put data))))
    (let [changed-id (#'fixture/edit-byte data ids 98)
          changed-iid (replace-iid data iids "b831b33e0c05b45e15bbdd9b3bfa4382")
          swapped (-> data (replace-iid iids (second (nth string-iid-vectors 1)))
                      (replace-iid (+ iids 16) (second (first string-iid-vectors))))
          matching (replace-iid changed-id iids "b831b33e0c05b45e15bbdd9b3bfa4382")]
      (doseq [[bad reason] [[changed-id :document-iid-mismatch]
                            [changed-iid :document-iid-mismatch]
                            [swapped :document-iid-mismatch]
                            [(#'fixture/edit-byte data ids 255) :invalid-arrow-utf8]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(#'fixture/retained-operation-bytes node other-id) :unsupported-reference-string-id]]]
        (let [r (arrow/decode-string-iid-put bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :groups)))
          (is (not (contains? r :iid-correspondence)))))
      (is (= [(-> expected (assoc-in [:documents 0 :xt/id] "b")
                  (assoc-in [:iids 0] "b831b33e0c05b45e15bbdd9b3bfa4382"))]
             (:groups (arrow/decode-string-iid-put matching)))))))

(deftest selected-string-iids-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-string-iid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs (mapv (fn [[id _]] {:xt/id id}) string-iid-vectors)]
    (try
      (let [[id other-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    other-id (submit! [[:put-docs :foo {:xt/id 1}]])]
                (check-string-iids! node id other-id)
                [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-string-iids! node id other-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(def ^:private keyword-iid-vectors
  [[:a "9665ad29257f8c97f62ddd4bbe177054"]
   [:open "ee6d26a49abe73900bdf8c9d9807756d"]
   [:claim/open "54f1c47257f82fcbee523fbd0d4d1287"]
   [:gesso.live.journal/command-id "5b00ff3b7f7a4926f1e9eacb3128a03f"]
   [:A_B "17a1ecc8da1a300a78fa9b5b843ad670"]
   [:a.b-c/x_2 "4ab368cc2fc630803b5bfdb035d44b13"]
   [:a/b "523c97ec16edf8bb6a12a7ddf959feae"]
   [:a-b "d367823239fb9313b359b8fb67d04c4e"]])

(deftest selected-keyword-iid-known-answers
  (doseq [[id expected] keyword-iid-vectors]
    (is (= expected (iid/keyword-iid id))))
  (is (not= (iid/keyword-iid :a) (iid/string-iid ":a")))
  (is (not= (iid/keyword-iid :a/b) (iid/keyword-iid :a-b)))
  (is (= 32 (count (iid/keyword-iid (keyword (apply str (repeat 4095 "a")))))))
  (doseq [id [nil 1 true "a" #uuid "00112233-4455-6677-8899-aabbccddeeff"
              (keyword "1") (keyword "a/b" "c") (keyword "a b")
              (keyword (apply str (repeat 4096 "a")))]]
    (is (= :unsupported-reference-keyword-id
           (try (iid/keyword-iid id) :accepted
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn- check-keyword-iids! [node id other-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents (mapv (fn [[id _]] {:xt/id id}) keyword-iid-vectors)
                  :iids (mapv second keyword-iid-vectors) :bounds {}}
        [_ _ _ _ _ _ iids _ _ _ _ _ _ _ ids] (#'fixture/buffer-starts data)
        r (arrow/decode-keyword-iid-put data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/keyword-iid-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unchecked (:iid-correspondence (arrow/decode-put-envelope data))))
    (check-unified-iids! data expected iids)
    (is (= :unsupported-reference-integer-id (:reason (arrow/decode-integer-iid-put data))))
    (is (= :unsupported-reference-string-id (:reason (arrow/decode-string-iid-put data))))
    (let [changed-id (#'fixture/edit-byte data ids 98)
          changed-iid (replace-iid data iids "822e154e3215ea83ef6bcba5b85860d7")
          swapped (-> data (replace-iid iids (second (nth keyword-iid-vectors 1)))
                      (replace-iid (+ iids 16) (second (first keyword-iid-vectors))))
          matching (replace-iid changed-id iids "822e154e3215ea83ef6bcba5b85860d7")]
      (doseq [[bad reason] [[changed-id :document-iid-mismatch]
                            [changed-iid :document-iid-mismatch]
                            [swapped :document-iid-mismatch]
                            [(#'fixture/edit-byte data ids 255) :invalid-arrow-utf8]
                            [(#'fixture/edit-byte data ids 49) :unsupported-arrow-keyword]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(#'fixture/retained-operation-bytes node other-id) :unsupported-reference-keyword-id]]]
        (let [r (arrow/decode-keyword-iid-put bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :groups)))
          (is (not (contains? r :iid-correspondence)))))
      (is (= [(-> expected (assoc-in [:documents 0 :xt/id] :b)
                  (assoc-in [:iids 0] "822e154e3215ea83ef6bcba5b85860d7"))]
             (:groups (arrow/decode-keyword-iid-put matching)))))))

(deftest selected-keyword-iids-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-keyword-iid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs (mapv (fn [[id _]] {:xt/id id}) keyword-iid-vectors)]
    (try
      (let [[id other-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    other-id (submit! [[:put-docs :foo {:xt/id 1}]])]
                (check-keyword-iids! node id other-id)
                [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-keyword-iids! node id other-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(def ^:private uuid-iid-vectors
  [[#uuid "00000000-0000-0000-0000-000000000000" "00000000000000000000000000000000"]
   [#uuid "00112233-4455-6677-8899-aabbccddeeff" "00112233445566778899aabbccddeeff"]
   [#uuid "ffffffff-ffff-ffff-ffff-ffffffffffff" "ffffffffffffffffffffffffffffffff"]
   [#uuid "80000000-0000-0000-0000-000000000001" "80000000000000000000000000000001"]
   [#uuid "00000000-0000-0001-8000-000000000000" "00000000000000018000000000000000"]
   [#uuid "01234567-89ab-cdef-fedc-ba9876543210" "0123456789abcdeffedcba9876543210"]])

(deftest selected-uuid-iid-known-answers
  (doseq [[id expected] uuid-iid-vectors]
    (is (= expected (iid/uuid-iid id))))
  (doseq [id [nil 1 true :one "00112233-4455-6677-8899-aabbccddeeff" (byte-array 16)]]
    (is (= :unsupported-reference-uuid-id
           (try (iid/uuid-iid id) :accepted
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn- check-uuid-iids! [node id other-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents (mapv (fn [[id _]] {:xt/id id}) uuid-iid-vectors)
                  :iids (mapv second uuid-iid-vectors) :bounds {}}
        [_ _ _ _ _ _ iids _ _ _ _ _ _ ids] (#'fixture/buffer-starts data)
        r (arrow/decode-uuid-iid-put data)
        changed-hex "ff000000000000000000000000000000"
        changed-id (#'fixture/edit-byte data ids 255)
        changed-iid (replace-iid data iids changed-hex)
        swapped (-> data (replace-iid iids (second (nth uuid-iid-vectors 1)))
                    (replace-iid (+ iids 16) (second (first uuid-iid-vectors))))]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/uuid-iid-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unchecked (:iid-correspondence (arrow/decode-put-envelope data))))
    (check-unified-iids! data expected iids)
    (doseq [[decoder reason] [[arrow/decode-integer-iid-put :unsupported-reference-integer-id]
                              [arrow/decode-string-iid-put :unsupported-reference-string-id]
                              [arrow/decode-keyword-iid-put :unsupported-reference-keyword-id]]]
      (is (= reason (:reason (decoder data)))))
    (doseq [[bad reason] [[changed-id :document-iid-mismatch]
                          [changed-iid :document-iid-mismatch]
                          [swapped :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                          [(#'fixture/retained-operation-bytes node other-id) :unsupported-reference-uuid-id]]]
      (let [r (arrow/decode-uuid-iid-put bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :groups)))
        (is (not (contains? r :iid-correspondence)))))
    (is (= [(-> expected (assoc-in [:documents 0 :xt/id] #uuid "ff000000-0000-0000-0000-000000000000")
                (assoc-in [:iids 0] changed-hex))]
           (:groups (arrow/decode-uuid-iid-put (replace-iid changed-id iids changed-hex)))))))

(deftest selected-uuid-iids-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-uuid-iid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs (mapv (fn [[id _]] {:xt/id id}) uuid-iid-vectors)]
    (try
      (let [[id other-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    other-id (submit! [[:put-docs :foo {:xt/id 1}]])]
                (check-uuid-iids! node id other-id)
                [id other-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-id (Duration/ofSeconds 10))
          (check-uuid-iids! node id other-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest selected-unified-iid-dispatch
  (doseq [[id expected] (concat integer-iid-vectors string-iid-vectors
                                keyword-iid-vectors uuid-iid-vectors)]
    (is (= expected (iid/document-iid id))))
  (doseq [[id reason] [[nil :unsupported-reference-document-id]
                       [true :unsupported-reference-document-id]
                       [1.5 :unsupported-reference-document-id]
                       [(inc (bigint Long/MAX_VALUE)) :unsupported-reference-integer-id]
                       [(str (char 0xd800)) :unsupported-reference-string-id]
                       [(keyword "1") :unsupported-reference-keyword-id]]]
    (is (= reason (try (iid/document-iid id) :accepted
                       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
  ;; A raw UUID may equal a truncated hash from another ID family.
  ;; Correspondence does not establish cross-family injectivity.
  (is (= (iid/document-iid 1)
         (iid/document-iid #uuid "4cd9b767-2d7f-bee8-fb51-fb1e049f6903"))))

(def ^:private mixed-id-vectors
  [[1 "4cd9b7672d7fbee8fb51fb1e049f6903"]
   ["1" "e8bc163c82eee18733288c7d4ac636db"]
   [:claim/open "54f1c47257f82fcbee523fbd0d4d1287"]
   [#uuid "00112233-4455-6677-8899-aabbccddeeff" "00112233445566778899aabbccddeeff"]
   [-2 "b49a40f47ff38b672a86e70156a7a8c1"]])

(defn- check-mixed-ids! [node id homogeneous-id other-union-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents (mapv (fn [[id _]] {:xt/id id}) mixed-id-vectors)
                  :iids (mapv second mixed-id-vectors) :bounds {}}
        [_ _ _ _ _ _ iids _ _ _ _ _ tags indexes _ integers _ _ strings]
        (#'fixture/buffer-starts data)
        r (arrow/decode-mixed-id-put data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/mixed-id-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (doseq [decoder [arrow/decode-put-envelope arrow/decode-verified-put
                     arrow/decode-integer-iid-put arrow/decode-string-iid-put
                     arrow/decode-keyword-iid-put arrow/decode-uuid-iid-put]]
      (let [r (decoder data)]
        (is (= :unsupported-arrow-field (:reason r)))
        (is (not (contains? r :groups)))))
    (let [homogeneous (#'fixture/retained-operation-bytes node homogeneous-id)]
      (is (= :matched (:iid-correspondence (arrow/decode-mixed-id-put homogeneous))))
      (is (= (:groups (arrow/decode-verified-put homogeneous))
             (:groups (arrow/decode-mixed-id-put homogeneous)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data tags 4) :unsupported-arrow-list-union-selection]
                          [(#'fixture/edit-byte data tags 255) :unsupported-arrow-list-union-selection]
                          [(#'fixture/edit-int data indexes -1) :unsupported-arrow-list-union-selection]
                          [(#'fixture/edit-int data indexes 1) :unsupported-arrow-list-union-selection]
                          [(#'fixture/edit-int data (+ indexes 16) 0) :unsupported-arrow-list-union-selection]
                          [(#'fixture/edit-long data integers 99) :document-iid-mismatch]
                          [(#'fixture/edit-byte data strings 50) :document-iid-mismatch]
                          [(#'fixture/edit-byte data strings 255) :invalid-arrow-utf8]
                          [(replace-iid data iids (second (nth mixed-id-vectors 1))) :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                          [(#'fixture/retained-operation-bytes node other-union-id) :unsupported-arrow-field]]]
      (let [r (arrow/decode-mixed-id-put bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :groups)))
        (is (not (contains? r :iid-correspondence)))))))

(deftest selected-mixed-document-iids-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-mixed-iid-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs (mapv (fn [[id _]] {:xt/id id}) mixed-id-vectors)]
    (try
      (let [[id homogeneous-id other-union-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    homogeneous-id (submit! [[:put-docs :foo {:xt/id "other"}]])
                    other-union-id (submit! [[:put-docs :foo {:xt/id 1 :value 1}
                                             {:xt/id "1" :value "x"}]])]
                (check-mixed-ids! node id homogeneous-id other-union-id)
                [id homogeneous-id other-union-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) other-union-id (Duration/ofSeconds 10))
          (check-mixed-ids! node id homogeneous-id other-union-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-nullable-documents! [node id plain-id unsupported-id]
  (let [data (#'fixture/retained-operation-bytes node id)
        expected {:kind :put-docs :table :public/foo
                  :documents [{:xt/id 1 :a 1 :b false :items [1 2] :nested {:ok true} :empty {}}
                              {:xt/id "1"}
                              {:xt/id 2 :items [] :nested {} :empty {}}]
                  :iids ["4cd9b7672d7fbee8fb51fb1e049f6903" "e8bc163c82eee18733288c7d4ac636db"
                         "420fce314175df402adbeae3cfbbb856"] :bounds {}}
        buffers (#'fixture/buffer-starts data)
        [iids a-valid a-values list-valid list-offsets nested-valid empty-valid]
        (mapv #(nth buffers %) [6 19 20 23 24 27 30])
        decode arrow/decode-nullable-document-put
        r (decode data)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/nullable-document-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [expected] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-mixed-id-put data))))
    (is (= 1 (bit-and 255 (aget ^bytes data a-valid))))
    (is (= [(assoc-in expected [:documents 0 :a] 99)]
           (:groups (decode (#'fixture/edit-long data a-values 99)))))
    (is (= [(assoc-in expected [:documents 1 :items] [])]
           (:groups (decode (#'fixture/edit-byte data list-valid 7)))))
    (is (= [(assoc-in expected [:documents 1 :empty] {})]
           (:groups (decode (#'fixture/edit-byte data empty-valid 7)))))
    (let [plain (#'fixture/retained-operation-bytes node plain-id)]
      (is (= :matched (:iid-correspondence (decode plain))))
      (is (= (:groups (arrow/decode-mixed-id-put plain)) (:groups (decode plain)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data list-valid 4) :unsupported-arrow-nullable-list-extent]
                          [(#'fixture/edit-int data (+ list-offsets 4) 1) :unsupported-arrow-nullable-list-extent]
                          [(#'fixture/edit-byte data nested-valid 4) :unsupported-arrow-nullable-struct-padding]
                          [(#'fixture/edit-byte data iids 0) :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                          [(#'fixture/retained-operation-bytes node unsupported-id) :unsupported-arrow-field]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)) (pr-str (dissoc r :groups)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :groups)))
        (is (not (contains? r :iid-correspondence)))))))

(deftest selected-nullable-documents-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-nullable-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        docs [{:xt/id 1 :a 1 :b false :items [1 2] :nested {:ok true} :empty {} :nullonly nil}
              {:xt/id "1"}
              {:xt/id 2 :a nil :b nil :items [] :nested {} :empty {}}]]
    (try
      (let [[id plain-id unsupported-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitTxBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    plain-id (submit! [[:put-docs :foo {:xt/id "plain" :a 1}]])
                    unsupported-id (submit! [[:put-docs :foo {:xt/id 1 :value 1}
                                              {:xt/id 2 :value "x"}]])]
                (check-nullable-documents! node id plain-id unsupported-id)
                [id plain-id unsupported-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) unsupported-id (Duration/ofSeconds 10))
          (check-nullable-documents! node id plain-id unsupported-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-temporal-documents! [node ids expected]
  (let [decode arrow/decode-temporal-document-put]
    (doseq [[id group] (map vector ids expected)]
      (let [data (#'fixture/retained-operation-bytes node id)
            r (decode data)]
        (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
        (is (= arrow/temporal-document-put-profile (:profile r)))
        (is (= :none (:authority r)))
        (is (= :not-issued (:certificate r)))
        (is (= :matched (:iid-correspondence r)))
        (is (= [group] (:groups r) (:operations (provider/observe-source-message! node id))))
        (if (some #(or (contains? % :xt/valid-from) (contains? % :xt/valid-to)) (:documents group))
          (is (= :unsupported-arrow-struct-key (:reason (arrow/decode-nullable-document-put data))))
          (is (= (:groups r) (:groups (arrow/decode-nullable-document-put data)))))))
    (let [data (#'fixture/retained-operation-bytes node (first ids))
          group (first expected)
          [_ _ _ _ _ _ iids _ _ _ _ _ _ from _ to _ _ _ _ _ outer-valid]
          (#'fixture/buffer-starts data)
          utc (java.time.ZoneId/of "UTC")
          epoch (.atZone Instant/EPOCH utc)
          latest (.atZone (Instant/parse "9998-12-31T23:59:59.999999Z") utc)
          field (#'fixture/find-byte-sequence data "_valid_from")
          row-field (.lastIndexOf (String. ^bytes data java.nio.charset.StandardCharsets/ISO_8859_1) "_valid_from")]
      (is (< field row-field))
      (is (= [(assoc-in group [:documents 0 :xt/valid-from] epoch)]
             (:groups (decode (#'fixture/edit-long data from 0)))))
      (is (= [(assoc-in group [:documents 0 :xt/valid-to] latest)]
             (:groups (decode (#'fixture/edit-long data to 253370764799999999)))))
      ;; Reversed intervals are retained facts, not admitted operations.
      (is (= [(assoc-in group [:documents 0 :xt/valid-to] epoch)]
             (:groups (decode (#'fixture/edit-long data to 0)))))
      (doseq [[bad reason] [[(#'fixture/edit-long data from Long/MIN_VALUE) :unsupported-arrow-timestamp-range]
                            [(#'fixture/edit-long data to Long/MAX_VALUE) :unsupported-arrow-timestamp-range]
                            [(#'fixture/edit-byte data (+ field 10) 120) :unsupported-arrow-field]
                            [(#'fixture/edit-byte data (+ row-field 10) 120) :unsupported-arrow-struct-key]
                            [(#'fixture/edit-byte data outer-valid 1) :unsupported-arrow-put-bounds]
                            [(#'fixture/edit-byte data iids 0) :document-iid-mismatch]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
        (let [r (decode bad)]
          (is (= :unsupported (:status r)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not (contains? r :groups)))
          (is (not (contains? r :iid-correspondence))))))))

(deftest selected-temporal-documents-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-temporal-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        from (Instant/parse "2025-01-01T00:00:00Z")
        to (Instant/parse "2026-01-01T00:00:00Z")
        negative (Instant/parse "1969-12-31T23:59:59.999999Z")
        future (Instant/parse "2030-01-01T00:00:00Z")
        utc (java.time.ZoneId/of "UTC")
        docs [{:xt/id 1 :name "a"} {:xt/id 2 :name "b"}]
        options [{:valid-from from :valid-to to} {:valid-from negative} {:valid-to future} {}]
        expected (mapv (fn [opts]
                         {:kind :put-docs :table :public/foo :bounds {}
                          :iids ["4cd9b7672d7fbee8fb51fb1e049f6903" "420fce314175df402adbeae3cfbbb856"]
                          :documents (mapv #(merge % (into {} (map (fn [[k v]] [(keyword "xt" (name k)) (.atZone ^Instant v utc)]) opts))) docs)})
                       options)]
    (try
      (let [ids (with-open [node (xtn/start-node config)]
                  (let [database (db/primary-db node)
                        ids (mapv (fn [opts]
                                    (let [id (:tx-id (xt/submit-tx node [(into [:put-docs (assoc opts :into :foo)] docs)]))]
                                      (.awaitSourceBlocking database id (Duration/ofSeconds 10)) id)) options)]
                    (check-temporal-documents! node ids expected)
                    ids))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (peek ids) (Duration/ofSeconds 10))
          (check-temporal-documents! node ids expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-nullable-temporal-documents! [node id plain-id group]
  (let [decode arrow/decode-nullable-temporal-document-put
        data (#'fixture/retained-operation-bytes node id)
        r (decode data)
        [_ _ _ _ _ _ iids _ _ _ _ _ _ _ from-valid from to-valid to outer-valid]
        (#'fixture/buffer-starts data)
        epoch (.atZone Instant/EPOCH (java.time.ZoneId/of "UTC"))]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/nullable-temporal-document-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= [group] (:groups r) (:operations (provider/observe-source-message! node id))))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-temporal-document-put data))))
    (is (= :unsupported-arrow-struct-key (:reason (arrow/decode-nullable-document-put data))))
    (is (= 1 (bit-and 255 (aget ^bytes data from-valid))))
    (is (= 2 (bit-and 255 (aget ^bytes data to-valid))))
    (is (= [(assoc-in group [:documents 0 :xt/valid-from] epoch)]
           (:groups (decode (#'fixture/edit-long data from 0)))))
    (is (= [(update-in group [:documents 0] dissoc :xt/valid-from)]
           (:groups (decode (#'fixture/edit-byte data from-valid 0)))))
    (is (= [(assoc-in group [:documents 2 :xt/valid-to] epoch)]
           (:groups (decode (#'fixture/edit-byte data to-valid 6)))))
    (let [plain (#'fixture/retained-operation-bytes node plain-id)]
      (is (= :matched (:iid-correspondence (decode plain))))
      (is (= (:groups (arrow/decode-temporal-document-put plain)) (:groups (decode plain)))))
    (doseq [[bad reason] [[(#'fixture/edit-long data from Long/MIN_VALUE) :unsupported-arrow-timestamp-range]
                          [(#'fixture/edit-long data (+ to 8) Long/MAX_VALUE) :unsupported-arrow-timestamp-range]
                          [(#'fixture/edit-byte data outer-valid 1) :unsupported-arrow-put-bounds]
                          [(#'fixture/edit-byte data iids 0) :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)) (pr-str (dissoc r :groups)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not (contains? r :groups)))
        (is (not (contains? r :iid-correspondence)))))))

(deftest selected-nullable-temporal-documents-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-nullable-temporal-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        from (Instant/parse "1969-12-31T23:59:59.999999Z")
        to (Instant/parse "2030-01-01T00:00:00Z")
        utc (java.time.ZoneId/of "UTC")
        docs [{:xt/id 1 :xt/valid-from from} {:xt/id 2 :xt/valid-to to}
              {:xt/id 3} {:xt/id 4 :xt/valid-from nil :xt/valid-to nil}]
        group {:kind :put-docs :table :public/foo :bounds {}
               :documents [{:xt/id 1 :xt/valid-from (.atZone from utc)}
                           {:xt/id 2 :xt/valid-to (.atZone to utc)} {:xt/id 3} {:xt/id 4}]
               :iids ["4cd9b7672d7fbee8fb51fb1e049f6903" "420fce314175df402adbeae3cfbbb856"
                      "9a83c6cb1126d93de4a30715b28f1f4b" "6ed5045938d710d075142228a0a53aed"]}]
    (try
      (let [[id plain-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 10)) id))
                    id (submit! [(into [:put-docs :foo] docs)])
                    plain-id (submit! [[:put-docs :foo {:xt/id 5 :xt/valid-from from}]])]
                (check-nullable-temporal-documents! node id plain-id group)
                [id plain-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) plain-id (Duration/ofSeconds 10))
          (check-nullable-temporal-documents! node id plain-id group)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-multi-group-puts! [node id nine-id single-id rejected]
  (let [decode arrow/decode-multi-group-put
        data (#'fixture/retained-operation-bytes node id)
        r (decode data)
        expected [{:kind :put-docs :table :public/foo :documents [{:xt/id 1 :name "a"}]
                   :iids ["4cd9b7672d7fbee8fb51fb1e049f6903"] :bounds {}}
                  {:kind :put-docs :table :public/foo :documents [{:xt/id 2 :name "b"} {:xt/id 3 :name "c"}]
                   :iids ["420fce314175df402adbeae3cfbbb856" "9a83c6cb1126d93de4a30715b28f1f4b"] :bounds {}}]
        [tags indexes _ _ iid-offsets _ iids table-tags table-indexes _ doc-offsets _ _ ids
         _ _ _ outer-valid outer-values] (#'fixture/buffer-starts data)
        observed (provider/observe-source-message! node id)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/multi-group-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:groups r) (:operations observed)))
    (is (= [1 1] (:logged-operation-spans r) (:logged-operation-spans observed)))
    (is (= :unsupported-arrow-put-batch (:reason (arrow/decode-nullable-temporal-document-put data))))
    (is (= :unsupported-arrow-put-batch (:reason (arrow/decode-put-envelope data))))
    ;; Moving both boundaries preserves the flat documents but changes physical grouping.
    (let [changed (-> data (#'fixture/edit-int (+ iid-offsets 4) 2)
                       (#'fixture/edit-int (+ doc-offsets 4) 2))
          result (decode changed)]
      (is (= :matched (:iid-correspondence result)))
      (is (= [2 1] (mapv (comp count :documents) (:groups result))))
      (is (= (mapcat :documents expected) (mapcat :documents (:groups result)))))
    (doseq [[bad reason] [[(#'fixture/edit-byte data (inc tags) 1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data (+ indexes 4) 0) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-byte data (inc table-tags) 1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data (+ table-indexes 4) -1) :unsupported-arrow-put-selection]
                          [(#'fixture/edit-int data (+ iid-offsets 4) 2) :unsupported-arrow-put-list-layout]
                          [(#'fixture/edit-int data (+ doc-offsets 8) 2) :unsupported-arrow-put-list-layout]
                          [(-> data (#'fixture/edit-int (+ iid-offsets 4) 0) (#'fixture/edit-int (+ doc-offsets 4) 0)) :unsupported-arrow-put-list-layout]
                          [(-> data (#'fixture/edit-int (+ iid-offsets 4) 4) (#'fixture/edit-int (+ doc-offsets 4) 4)) :unsupported-arrow-put-list-layout]
                          [(#'fixture/edit-byte data outer-valid 2) :unsupported-arrow-put-bounds]
                          [(#'fixture/edit-long data (+ outer-values 8) 1) :unsupported-arrow-put-bounds]
                          [(#'fixture/edit-long data (+ ids 8) 99) :document-iid-mismatch]
                          [(#'fixture/edit-byte data (+ iids 16) 0) :document-iid-mismatch]
                          [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]]]
      (let [r (decode bad)]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :logged-operation-spans :iid-correspondence]))))
    (let [nine (#'fixture/retained-operation-bytes node nine-id)
          r (decode nine)
          observed (provider/observe-source-message! node nine-id)]
      (is (= :matched (:iid-correspondence r)))
      (is (= (vec (repeat 9 1)) (:logged-operation-spans r) (:logged-operation-spans observed)))
      (is (= (:operations observed) (:groups r)))
      (is (= (range 10 19) (map (comp :xt/id first :documents) (:groups r))))
      ;; Both outer-bound bitmaps span two bytes at nine groups.
      (let [valid (nth (#'fixture/buffer-starts nine) 14)
            bad (decode (#'fixture/edit-byte nine (inc valid) 1))]
        (is (= :unsupported-arrow-put-bounds (:reason bad)))))
    (let [single (#'fixture/retained-operation-bytes node single-id)
          r (decode single)]
      (is (= :matched (:iid-correspondence r)))
      (is (= [1] (:logged-operation-spans r)))
      (is (= (:groups (arrow/decode-nullable-temporal-document-put single)) (:groups r))))
    (doseq [[other-id reason] rejected]
      (let [r (decode (#'fixture/retained-operation-bytes node other-id))]
        (is (= :unsupported (:status r)))
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:groups :logged-operation-spans :iid-correspondence]))))))

(deftest selected-multi-group-puts-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-multi-group-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        put (fn [ids] (into [:put-docs :foo] (map #(hash-map :xt/id %) ids)))]
    (try
      (let [[id nine-id single-id rejected]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! [[:put-docs :foo {:xt/id 1 :name "a"}]
                                 [:put-docs :foo {:xt/id 2 :name "b"} {:xt/id 3 :name "c"}]])
                    nine-id (submit! (mapv #(put [%]) (range 10 19)))
                    single-id (submit! [[:put-docs :foo {:xt/id "single" :xt/valid-from (Instant/parse "2025-01-01T00:00:00Z")}]])
                    rejected [[(submit! [[:put-docs :foo {:xt/id 1}] [:put-docs :bar {:xt/id 2}]]) :unsupported-arrow-field]
                              [(submit! [[:put-docs :foo {:xt/id 1}] ["ASSERT TRUE"]]) :unsupported-arrow-field]
                              [(submit! (mapv #(put [%]) (range 1000 1257))) :unsupported-arrow-put-batch]
                              [(submit! [(put (range 2000 2257))]) :unsupported-arrow-put-list-layout]
                              [(submit! (mapv put (partition-all 256 (range 3000 7097)))) :unsupported-arrow-put-nodes]]]
                (check-multi-group-puts! node id nine-id single-id rejected)
                [id nine-id single-id rejected]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (first (peek rejected)) (Duration/ofSeconds 15))
          (check-multi-group-puts! node id nine-id single-id rejected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(defn- check-multi-table-puts! [node id single-id too-many-id]
  (let [decode arrow/decode-multi-table-put
        data (#'fixture/retained-operation-bytes node id)
        expected [{:kind :put-docs :table :public/foo :documents [{:xt/id 1 :name "a"}]
                   :iids ["4cd9b7672d7fbee8fb51fb1e049f6903"] :bounds {}}
                  {:kind :put-docs :table :public/bar :documents [{:xt/id "x" :ok true}]
                   :iids ["9740c9dcdb4487d8cd63d2a544d8e657"] :bounds {}}
                  {:kind :put-docs :table :public/foo :documents [{:xt/id 2 :name "b"} {:xt/id 3 :name "c"}]
                   :iids ["420fce314175df402adbeae3cfbbb856" "9a83c6cb1126d93de4a30715b28f1f4b"] :bounds {}}]
        r (decode data)
        buffers (#'fixture/buffer-starts data)
        [iids tags indexes iid-offsets foo-offsets bar-offsets bar-id outer-valid]
        (mapv #(nth buffers %) [6 7 8 4 10 18 22 25])
        observed (provider/observe-source-message! node id)]
    (is (= :decoded (:status r)) (pr-str (dissoc r :groups)))
    (is (= arrow/multi-table-put-profile (:profile r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (is (= :matched (:iid-correspondence r)))
    (is (= expected (:groups r) (:operations observed)))
    (is (= [1 1 1] (:logged-operation-spans r) (:logged-operation-spans observed)))
    (is (= :unsupported-arrow-field (:reason (arrow/decode-multi-group-put data))))
    (let [field (#'fixture/find-byte-sequence data "public/bar")
          duplicate (-> data (#'fixture/edit-byte (+ field 7) 102)
                        (#'fixture/edit-byte (+ field 8) 111)
                        (#'fixture/edit-byte (+ field 9) 111))]
      (doseq [[bad reason] [[duplicate :unsupported-arrow-put-table]
                            [(#'fixture/edit-byte data (inc tags) 2) :unsupported-arrow-put-selection]
                            [(#'fixture/edit-int data (+ indexes 8) 0) :unsupported-arrow-put-selection]
                            [(#'fixture/edit-int data (+ indexes 4) -1) :unsupported-arrow-put-selection]
                            [(#'fixture/edit-int data (+ foo-offsets 4) 2) :unsupported-arrow-put-list-layout]
                            [(#'fixture/edit-int data (+ bar-offsets 4) 0) :unsupported-arrow-put-list-layout]
                            [(#'fixture/edit-int data (+ iid-offsets 8) 3) :unsupported-arrow-put-list-layout]
                            [(#'fixture/edit-byte data bar-id 121) :document-iid-mismatch]
                            [(#'fixture/edit-byte data (+ iids 16) 0) :document-iid-mismatch]
                            [(#'fixture/edit-byte data outer-valid 2) :unsupported-arrow-put-bounds]
                            [(Arrays/copyOf data (dec (alength data))) :unsupported-arrow-terminator]
                            [(#'fixture/retained-operation-bytes node too-many-id) :unsupported-arrow-field]]]
        (let [r (decode bad)]
          (is (= :unsupported (:status r)) (pr-str (dissoc r :groups)))
          (is (= reason (:reason r)) (pr-str r))
          (is (not-any? #(contains? r %) [:groups :logged-operation-spans :iid-correspondence])))))
    (let [single (#'fixture/retained-operation-bytes node single-id)]
      (is (= :matched (:iid-correspondence (decode single))))
      (is (= (:groups (arrow/decode-multi-group-put single)) (:groups (decode single)))))))

(deftest selected-multi-table-puts-from-persistent-source-bytes
  (let [root (Files/createTempDirectory "gesso-arrow-multi-table-put-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}]
    (try
      (let [[id single-id too-many-id]
            (with-open [node (xtn/start-node config)]
              (let [database (db/primary-db node)
                    submit! (fn [ops] (let [id (:tx-id (xt/submit-tx node ops))]
                                        (.awaitSourceBlocking database id (Duration/ofSeconds 15)) id))
                    id (submit! [[:put-docs :foo {:xt/id 1 :name "a"}]
                                 [:put-docs :bar {:xt/id "x" :ok true}]
                                 [:put-docs :foo {:xt/id 2 :name "b"} {:xt/id 3 :name "c"}]])
                    single-id (submit! [[:put-docs :foo {:xt/id 4}] [:put-docs :foo {:xt/id 5}]])
                    too-many-id (submit! (mapv #(vector :put-docs (keyword (str "table_" %)) {:xt/id %}) (range 17)))]
                (check-multi-table-puts! node id single-id too-many-id)
                [id single-id too-many-id]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) too-many-id (Duration/ofSeconds 15))
          (check-multi-table-puts! node id single-id too-many-id)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
