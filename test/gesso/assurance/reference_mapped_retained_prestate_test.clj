(ns gesso.assurance.reference-mapped-retained-prestate-test
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
  {:subject {:kind :operation :id :example/prestate :interface {:operation :example/prestate}}
   :state-types {:example/count {:type :integer}
                 :example/owner {:type :string :nullable? true}
                 :example/outcome {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {}
   :frame #{:example/count :example/owner :example/outcome}})

(def ^:private targets
  {:example/count [:document :gesso_requests "r1" :amount]
   :example/owner [:nullable-document :gesso_assignments "a1" :owner]
   :example/outcome [:journal-slot "helper-a" :request/claim]})

(defn- record [id principal]
  (resolution/command-record {:command-id (identity/command-id id)
                              :principal (identity/principal principal)
                              :operation :request/claim :arguments {:request-id "r1"}}
                             {:status :claimed :items (vec (range 32))} "t1"))

(deftest match-prepared-rows-at-selected-transaction-valid-instant
  (let [root (Files/createTempDirectory "gesso-retained-prestate-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z") at #(.plusSeconds t %)
        row {:xt/id "r1" :amount 2 :enabled true}
        changed (assoc row :enabled false)
        owner {:xt/id "a1" :owner "helper-a"}
        journal (record "c1" "helper-a")
        empty-source (-> source (update :state-types dissoc :example/count)
                         (update :frame disj :example/count))
        empty-targets (dissoc targets :example/count)
        initial-prefix (frame/formatted-prestate-assertions!
                        (plan/check-target-mapping! empty-source empty-targets) {:gesso_assignments []} [])
        mapping (plan/check-target-mapping! source targets)
        absent {:gesso_requests [row] :gesso_assignments []}
        occupied (assoc absent :gesso_assignments [owner])
        expired {:gesso_requests [changed] :gesso_assignments []}
        prefix #(frame/formatted-prestate-assertions! mapping %1 %2)
        early (prefix absent []) active (prefix occupied [journal]) late (prefix expired [])
        future (fn [table doc] [:put-docs {:into table :valid-from (at 3) :valid-to (at 6)} doc])
        transactions [(into initial-prefix [[:put-docs :gesso_requests row {:xt/id "r2" :amount 7 :enabled true}]
                                            (future :gesso_assignments owner) (future :gesso_commands journal)
                                            [:put-docs :gesso_commands (record "unrelated" "helper-b")]])
                      (conj early ["ASSERT FALSE"])
                      (conj active [:put-docs :gesso_requests changed])
                      active
                      (conj late [:put-docs :gesso_assignments owner])]
        tables #{:gesso_requests :gesso_assignments :gesso_commands}
        compare! (fn [node original targets docs rows tx n]
                   (reference/compare-local-processed-retained-mapped-prestate!
                    node tables original targets docs rows tx n))
        success! (fn [r]
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= reference/mapped-retained-prestate-profile (:mapped-retained-prestate-profile r)))
                   (is (= :matched-retained-predecessor (:prepared-prestate r)))
                   (is (= :not-asserted (:selected-transaction-commitment r)))
                   (is (= :none (:authority r))) (is (= :not-issued (:certificate r)))
                   (is (= :not-established (:provider-refinement r)))
                   (is (not-any? #(contains? r %) [:groups :replay-inputs :payload-fingerprint :rows])))
        mismatch! (fn [r]
                    (is (= :mismatch (:status r)) (pr-str r))
                    (is (= :selected-retained-prestate-mismatch (:reason r)))
                    (is (not (contains? r :prepared-prestate)))
                    (is (not (contains? r :source-lowering))))]
    (try
      (with-open [node (xtn/start-node config)]
        (let [database (db/primary-db node)
              history (mapv (fn [n ops]
                              (let [time (at n) id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                {:tx-id id :system-time time :ops ops})) [0 1 3 4 6] transactions)
              _ (.awaitSourceBlocking database (:tx-id (peek history)) (Duration/ofSeconds 30))
              sample (coverage/observe-local-processed-retained-replay-inputs! node)
              report (reference/compare-source-bound-execution! node history tables)]
          (is (= :observed (:status sample)))
          (is (= history (:replay-inputs sample)))
          (is (= :matched (:status report)))
          (is (= [true false true false true]
                 (mapv (fn [tx] (:committed (first (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id tx)])))) history)))
          ;; Reuse the same acquired history for selected-transaction checks.
          (with-redefs [coverage/observe-local-processed-retained-replay-inputs! (constantly sample)
                        reference/compare-source-bound-execution! (constantly report)]
            (success! (compare! node empty-source empty-targets {:gesso_assignments []} [] (first history) (count initial-prefix)))
            (success! (compare! node source targets absent [] (second history) (count early)))
            (success! (compare! node source targets occupied [journal] (nth history 2) (count active)))
            (mismatch! (compare! node source targets occupied [journal] (nth history 3) (count active)))
            (success! (compare! node source targets expired [] (peek history) (count late))))
          (success! (compare! node source targets occupied [journal] (nth history 2) (count active)))
          (mismatch! (compare! node source targets occupied [journal] (nth history 3) (count active)))
          (is (= :matched (:status (reference/compare-local-processed-retained-mapped-execution!
                                    node tables source targets occupied [journal] (nth history 3) (count active)))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
