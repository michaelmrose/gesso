(ns gesso.assurance.reference-residual-replay-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-arrow-operations :as arrow]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-replay-input-test :as fixture]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-mapped-residual :as reference]
            [gesso.assurance.reference-mapped-writes-test :as writes]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-residual :as residual]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(deftest retained-production-residual-fragment
  (let [root (Files/createTempDirectory "gesso-retained-residual-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z") at #(.plusSeconds t %)
        fields [:example/count :example/owner :example/outcome]
        source (-> @#'writes/source
                   (assoc :guard {:readers (set fields)
                                  :expression [:and [:= [:read :example/count] [:literal 2]]
                                               [:= [:read :example/owner] [:literal nil]]
                                               [:= [:read :example/outcome] [:literal nil]]]})
                   (update :reads into fields))
        targets @#'writes/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :updated-at "t0" :request/location-id "l1"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        declaration {:document-layout {[:gesso_requests "r1"] [[:prestate :revision] [:state :example/count] [:prestate :updated-at]]}
                     :subject-checks [[:gesso_requests "r1" :request/location-id]]}
        guards (residual/reconstruct! source (plan/check-target-mapping! source targets) declaration binding docs)
        document-index (first (keep-indexed #(when (str/includes? (first %2) "(revision = ?)") %1) guards))
        subject-index (first (keep-indexed #(when (str/includes? (first %2) "request$location_id") %1) guards))
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        put (fn [r] [:put-docs :gesso_requests r])
        txs [[[:put-docs {:into :gesso_requests :valid-from (at 2) :valid-to (at 6)} row]]
             guards guards
             (into [(put (assoc row :revision 1)) probe] guards)
             (into [(put (assoc row :request/location-id "l2")) probe] guards)
             guards (into [(put row)] (conj guards probe))]
        tables #{:gesso_requests :gesso_assignments :gesso_commands :guard_probe}
        check! (fn [node expected]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) (:replay-inputs sample)))
                       report (provider/compare-local-processed-retained-replay-execution! node tables)
                       id (:tx-id (nth expected 2)) body (get (#'fixture/read-bytes node) id)
                       decode! arrow/decode-bounded-operations]
                   (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs)))
                   (is (= expected (:replay-inputs sample)))
                   (is (= :predicted (:status prediction)))
                   (is (= [true false true false false false true] (mapv :committed? (:history prediction))))
                   (is (= [document-index (+ 2 document-index) (+ 2 subject-index) document-index]
                          (mapv :failed-assertion-index (filter #(not (:committed? %)) (:history prediction)))))
                   (is (= :matched (:status report)) (pr-str report))
                   (is (= 7 (:transaction-count report)))
                   (is (= :none (:authority report)))
                   (is (= :not-issued (:certificate report)))
                   (is (= :not-established (:provider-refinement report)))
                   (is (= :unsupported (:status (coverage/observe-local-processed-retained-clocked-inputs! node))))
                   (is (= :unsupported (:status (provider/compare-local-processed-retained-replay-execution! node (disj tables :gesso_assignments)))))
                   (doseq [mutate [#(update-in % [document-index :query] str/replace "$3" "$03")
                                   #(update-in % [document-index :query] str/replace "$3" "$2")
                                   #(update-in % [document-index :args] pop)
                                   #(assoc-in % [document-index :args 0] 0)
                                   #(assoc-in % [document-index :args 1] 2)
                                   #(update-in % [document-index :query] str " -- comment")
                                   #(update-in % [subject-index :query] str/replace "request$location_id" "request$1")]]
                     (with-redefs [arrow/decode-bounded-operations #(update (decode! %) :operations mutate)]
                       (let [r (input/decode-replay-input id body)]
                         (is (= :unsupported (:status r)) (pr-str r))
                         (is (not (contains? r :transaction))))))))]
    (try
      (is (= 5 (count guards)))
      (is (= guards (reference/derive-assertions! source targets docs [] binding {:example/claim declaration})))
      (is (some? document-index)) (is (some? subject-index))
      (let [history (with-open [node (xtn/start-node config)]
                      (let [database (db/primary-db node)
                            history (mapv (fn [n ops]
                                            (let [time (at n) id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                              (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                              {:tx-id id :system-time time :ops ops})) [0 1 2 3 4 6 7] txs)]
                        (.awaitSourceBlocking database (:tx-id (peek history)) (Duration/ofSeconds 30))
                        (check! node history)
                        history))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (check! node history)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
