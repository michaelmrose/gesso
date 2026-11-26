(ns gesso.assurance.reference-mapped-retained-execution-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(def ^:private source
  {:subject {:kind :operation :id :example/journal :interface {:operation :example/journal}}
   :state-types {:example/outcome {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {} :frame #{:example/outcome}})

(def ^:private targets {:example/outcome [:journal-slot "helper-a" :request/claim]})

(deftest bind-source-relative-prefix-to-retained-history
  (let [root (Files/createTempDirectory "gesso-mapped-retained-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :request/claim :arguments {:request-id "r"}}
        row (resolution/command-record binding {:status :claimed :items (vec (range 32))} "t1")
        changed (assoc-in row [:gesso.command/outcome :items 16] 999)
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping {} [row])
        bad-prefix (frame/formatted-prestate-assertions! mapping {} [changed])
        marker [:put-docs :guard_probe {:xt/id "probe" :written true}]
        txs [[[:put-docs :gesso_commands row]] (conj prefix marker) (conj bad-prefix marker)]
        tables #{:gesso_commands :guard_probe}
        compare! (fn [node expectation prepared n scope]
                   (reference/compare-local-processed-retained-mapped-execution!
                    node scope source targets {} [prepared] expectation n))
        no-match! (fn [r status reason]
                    (is (= status (:status r)) (pr-str r))
                    (is (= reason (:reason r)))
                    (is (= :none (:authority r)))
                    (is (= :not-issued (:certificate r)))
                    (is (not-any? #(contains? r %) [:source-lowering :selected-transaction-binding
                                                   :checks :replay-inputs :payload-fingerprint])))
        check! (fn [node expected prepared n]
                 (let [r (compare! node expected prepared n tables)]
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= reference/mapped-retained-execution-profile (:mapped-retained-execution-profile r)))
                   (is (= :checked-exact-prefix (:source-lowering r)))
                   (is (= :matched (:selected-transaction-binding r)))
                   (is (= :not-asserted (:selected-transaction-commitment r)))
                   (is (= 5 (:mapped-prestate-lowering-version r)))
                   (is (= :not-established (:provider-refinement r)))
                   (is (= :none (:authority r)))
                   (is (= :not-issued (:certificate r)))
                   (is (= 3 (:transaction-count r)))
                   (is (not-any? #(contains? r %) [:replay-inputs :payload-fingerprint :operations :transaction-ids :groups]))))]
    (try
      (let [history (with-open [node (xtn/start-node config)]
                      (let [database (db/primary-db node)
                            history (mapv (fn [i ops]
                                            (let [time (.plusSeconds t i)
                                                  id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                              (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                              {:tx-id id :system-time time :ops ops})) (range) txs)
                            _ (.awaitSourceBlocking database (:tx-id (peek history)) (Duration/ofSeconds 30))
                            expected (second history)
                            sample (coverage/observe-local-processed-retained-replay-inputs! node)
                            report (reference/compare-source-bound-execution! node history tables)]
                        (is (> (count prefix) 9))
                        (is (= history (:replay-inputs sample)))
                        (is (= :matched (:status report)))
                        (check! node expected row (count prefix))
                        (is (= [{:committed false}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id (peek history))])))
                        (check! node (peek history) changed (count bad-prefix))
                        (with-redefs [coverage/observe-local-processed-retained-replay-inputs! (constantly sample)
                                      reference/compare-source-bound-execution! (constantly report)]
                          (doseq [bad [(update expected :tx-id inc)
                                       (assoc expected :system-time (.plusSeconds t 10))
                                       (update expected :ops pop)
                                       (assoc-in expected [:ops (count prefix) 2 :written] false)]]
                            (no-match! (compare! node bad row (count prefix) tables)
                                       :mismatch :selected-retained-transaction-mismatch))
                          (doseq [[bad prepared n] [[expected changed (count prefix)]
                                                    [expected row 0]
                                                    [expected row (dec (count prefix))]
                                                    [(update expected :ops #(into [["ASSERT TRUE"]] %)) row (count prefix)]]]
                            (no-match! (compare! node bad prepared n tables)
                                       :unsupported :invalid-mapped-retained-prefix)))
                        (let [samples (atom [sample (update sample :payload-fingerprint #(str (if (= \0 (first %)) "1" "0") (subs % 1)))])]
                          (with-redefs [coverage/observe-local-processed-retained-replay-inputs!
                                        (fn [_] (let [s (first @samples)] (swap! samples subvec 1) s))
                                        reference/compare-source-bound-execution! (constantly report)]
                            (no-match! (compare! node expected row (count prefix) tables)
                                       :unavailable :source-log-changed-during-comparison)))
                        (is (= :unsupported (:status (compare! node expected row (count prefix) #{:gesso_commands}))))
                        history))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (check! node (second history) row (count prefix))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
