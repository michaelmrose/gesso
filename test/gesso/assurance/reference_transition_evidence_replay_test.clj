(ns gesso.assurance.reference-transition-evidence-replay-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-replay-input-test :as fixture]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(defn- fingerprint [id]
  (:fingerprint (subject/describe-subject {:kind :rule :id id :interface {:fixture true}})))

(defn- evidence [id prefix]
  ;; Use the production record encoder with fixture premises, not an
  ;; admitted application or an independently checked abstract witness.
  (#'inductive/transition-evidence
   {:catalog-fingerprint (fingerprint :example/catalog)
    :induction-fingerprint (fingerprint :example/induction)
    :input-derivations {:example/claim {}}}
   :example/claim {:fingerprint (fingerprint :example/mapping)
                   :transition-fingerprint (fingerprint :example/transition)}
   id {:example/count 2 :example/owner nil :example/status :example/open}
   {:example/next-count 3 :example/enabled true :example/helper "helper-a"}
   {:example/count 3 :example/owner "helper-a" :example/status :claimed}
   (frame/prefix-evidence! prefix)))

(defn- guarded [id]
  (let [prefix [["ASSERT ? = ?" 1 1]]
        command (resolution/command-record {:command-id (identity/command-id id)
                                             :principal (identity/principal "helper-a")
                                             :operation :example/claim :arguments {}}
                                            {:status :claimed} "t1")]
    (envelope/bind-transition-envelope!
     (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]]
           (concat prefix [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" id]
                           [:put-docs :gesso_commands command]
                           [:put-docs :gesso_transition_evidence (evidence id prefix)]])))))

(deftest strict-retained-transition-evidence-writes
  (let [root (Files/createTempDirectory "gesso-retained-transition-evidence-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        a (guarded "c1") b (guarded "c2")
        witness (peek (peek a))
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        txs [a (into [probe] a) b]
        tables #{:gesso_transition_evidence :gesso_commands :guard_probe}
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) expected))
                       report (reference/compare-local-processed-retained-replay-execution! node tables)
                       id (:tx-id (first expected)) body (get (#'fixture/read-bytes node) id)
                       arrow! arrow/decode-bounded-operations]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true false true] (mapv :committed? (:history prediction))))
                   (is (= 1 (:failed-assertion-index (second (:history prediction)))))
                   (is (= tables (temporal/required-observation-tables (mapv #(dissoc % :tx-id) expected))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :not-established (:provider-refinement report)))
                   (is (= :unsupported (:status (reference/compare-local-processed-retained-replay-execution! node (disj tables :gesso_transition_evidence)))))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (is (= [witness] (xt/q node ["SELECT * FROM gesso_transition_evidence WHERE _id = ?" "c1"])))
                   (is (empty? (xt/q node "SELECT * FROM guard_probe")))
                   (doseq [[i mutate] [[0 #(update % :query str/replace "$1" "$2")]
                                       [0 #(update % :query str/replace "$1" "$01")]
                                       [0 #(assoc % :args [(random-uuid)])]
                                       [0 #(assoc % :args [""])]
                                       [0 #(update % :args conj "c2")]
                                       [0 #(update % :query str " -- comment")]
                                       [4 #(assoc-in % [:documents 0 :prestate-witness] [["example" "count" 2]])]
                                       [4 #(assoc-in % [:documents 0 :input-witness] (vec (repeat 65 ["example" "x" "nil"])))]
                                       [4 #(assoc-in % [:documents 0 :guard-profile] :example/unsupported.v5)]]]
                     (with-redefs [arrow/decode-bounded-operations #(update-in (arrow! %) [:operations i] mutate)]
                       (let [r (input/decode-replay-input id body)]
                         (is (= :unsupported (:status r)) (pr-str r))
                         (is (not (contains? r :transaction))))))))]
    (is (envelope/bound-transition-envelope? a))
    (is (= 4 (:profile-version witness)))
    (is (= frame/guard-profile (:guard-profile witness)))
    (is (= [["example" "count" "integer" "2"] ["example" "owner" "nil"]
            ["example" "status" "qualified-keyword" "example" "open"]]
           (:prestate-witness witness)))
    (is (= :unsupported (:status (current/execute {} [[:put-docs :gesso_other witness]]))))
    (is (= :unsupported (:status (current/execute {} [[:put-docs :gesso_transition_evidence (assoc witness :profile-version 3)]]))))
    (try
      (let [expected (with-open [node (xtn/start-node config)]
                       (let [database (db/primary-db node)
                             history (mapv (fn [i ops]
                                             (let [time (.plusSeconds t i)
                                                   id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                               (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                               {:tx-id id :system-time time :ops ops})) (range) txs)]
                         (.awaitSourceBlocking database (:tx-id (peek history)) (Duration/ofSeconds 30))
                         (check! node history)
                         history))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek expected)) (Duration/ofSeconds 30))
          (check! node expected)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
