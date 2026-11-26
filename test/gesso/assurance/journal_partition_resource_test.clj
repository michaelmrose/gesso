(ns gesso.assurance.journal-partition-resource-test
  "Bounded resource observations, not a universal XTDB compilation limit."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.reference-provider :as reference]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.reference-transaction :as current]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(def ^:private binding-value
  {:command-id (identity/command-id "c1")
   :principal (identity/principal "helper-a")
   :operation :request/claim :arguments {:request-id "r"}})

(defn- row [n depth]
  (resolution/command-record
   binding-value
   {:status :claimed
    :details (reduce (fn [v _] {:nested v})
                     {:items (vec (range n)) :empty {}} (range depth))}
   "t1"))

(defn- guards [r]
  (frame/formatted-prestate-assertions!
   {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}} {} [r]))

(defn- ops [r]
  (into [[:put-docs :gesso_commands r]] (guards r)))

(defn- tx [n operations]
  {:system-time (.plusSeconds (Instant/parse "2026-01-01T00:00:00Z") n)
   :ops operations})

(deftest partition-resource-boundaries
  (doseq [[n depth predicates groups] [[15 0 96 1] [15 1 97 9]
                                       [32 4 151 13] [128 0 435 37]
                                       [256 0 819 69]]]
    (let [r (row n depth) p (guards r) chunks (subvec p 2)
          widths (mapv #(- (count %) 3) chunks)
          result (current/execute {} (ops r))]
      (is (= predicates (count (#'lowering/journal-constraints r))))
      (is (= groups (count chunks)))
      (is (= predicates (reduce + widths)))
      (is (= (if (<= predicates 96) [predicates]
                 (vec (map count (partition-all 12 (range predicates))))) widths))
      (is (= :predicted (:status result)))
      (is (true? (:committed? result)))
      (is (= #{:gesso_commands} (temporal/required-observation-tables [(tx 1 (ops r))])))
      (is (= :predicted (:status (temporal/replay [(tx 1 (ops r))]))))))
  (let [wide (ops (row 257 0))
        too-deep (ops (row 32 8))
        p (ops (row 256 0))
        padded #(into p (repeat (- % (count p)) ["ASSERT TRUE"]))]
    (doseq [bad [wide too-deep]]
      (is (= :unsupported (:status (current/execute {} bad))))
      (is (nil? (temporal/required-observation-tables [(tx 1 bad)])))
      (is (= :unsupported (:status (temporal/replay [(tx 1 bad)])))))
    ;; Operation-count ceilings are not equivalent to predicate-count limits.
    (is (= 256 (count (padded 256))))
    (is (= :predicted (:status (temporal/replay [(tx 1 (padded 256))]))))
    (is (= :predicted (:status (current/execute {} (padded 257)))))
    (is (nil? (temporal/required-observation-tables [(tx 1 (padded 257))])))
    (is (= :unsupported (:status (temporal/replay [(tx 1 (padded 257))]))))))

(deftest retained-large-partition-resource-observation
  (let [root (Files/createTempDirectory "gesso-retained-guard-resources-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        ;; Test the first partitioned width, a nested case, and a maximum
        ;; supported scalar-vector length; do not infer a general bound.
        transactions (mapv (fn [i [n d]] (tx (inc i) (ops (row n d))))
                           (range) [[15 1] [32 4] [256 0]])
        tables #{:gesso_commands}]
    (try
      (with-open [node (xtn/start-node config)]
        (let [database (db/primary-db node)
              expected (mapv (fn [{:keys [system-time ops] :as t}]
                               (let [id (:tx-id (xt/submit-tx node ops {:system-time system-time}))]
                                 (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                 (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])))
                                 (assoc t :tx-id id))) transactions)
              _ (.awaitSourceBlocking database (:tx-id (peek expected)) (Duration/ofSeconds 30))
              sample (coverage/observe-local-processed-retained-replay-inputs! node)
              report (reference/compare-local-processed-retained-replay-execution! node tables)]
          (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs)))
          (is (= expected (:replay-inputs sample)))
          (is (= :matched (:status report)) (pr-str report))
          (is (= 3 (:transaction-count report)))
          (is (= :not-established (:provider-refinement report)))
          (is (= :none (:authority report)))
          (is (= :not-issued (:certificate report)))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
