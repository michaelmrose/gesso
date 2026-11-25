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
