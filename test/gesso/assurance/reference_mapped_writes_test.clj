(ns gesso.assurance.reference-mapped-writes-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-writes :as writes]
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
  {:subject {:kind :operation :id :example/claim :interface {:operation :example/claim}}
   :state-types {:example/count {:type :integer} :example/enabled {:type :boolean}
                 :example/owner {:type :string :nullable? true}
                 :example/outcome {:type :keyword :nullable? true}}
   :input-types {:example/next-count {:type :integer}}
   :guard {:readers #{} :expression [:literal true]}
   :reads #{:example/next-count}
   :writes #{:example/count :example/owner :example/outcome}
   :replacements {:example/count [:read :example/next-count]
                  :example/owner [:literal "helper-a"] :example/outcome [:literal :claimed]}
   :frame #{:example/enabled}})

(def ^:private targets
  {:example/count [:document :gesso_requests "r1" :amount]
   :example/enabled [:document :gesso_requests "r1" :enabled]
   :example/owner [:nullable-document :gesso_assignments "a1" :owner]
   :example/outcome [:journal-slot "helper-a" :example/claim]})

(deftest forward-write-derivation-bound-to-retained-prestate
  (let [root (Files/createTempDirectory "gesso-retained-writes-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        spec {:inputs {:example/next-count 3} :effects {[:document :gesso_requests "r1"] {:revision 1}
                                                       [:document :gesso_assignments "a1"] {:revision 0}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        journal (resolution/command-record binding (:outcome spec) (:now spec))
        puts [[:put-docs :gesso_requests (assoc row :amount 3 :revision 1)]
              [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a" :revision 0}]
              [:put-docs :gesso_commands journal]]
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! source targets) docs [])
        tables #{:gesso_requests :gesso_assignments :gesso_commands}
        txs [[[:put-docs :gesso_requests row]] (into prefix puts)]
        compare! (fn [node expected s scope]
                   (writes/compare-local-processed-retained-mapped-writes!
                    node scope source targets docs [] expected (count prefix) s))]
    (is (= (set puts) (set (writes/derive-puts! source targets docs [] spec))))
    (doseq [bad [(assoc-in spec [:outcome :status] :other)
                 (assoc-in spec [:effects [:document :gesso_requests "r1"] :amount] 2)
                 (assoc spec :effects {})
                 (assoc-in spec [:binding :operation] :example/other)
                 (assoc spec :inputs {})]]
      (is (thrown? clojure.lang.ExceptionInfo (writes/derive-puts! source targets docs [] bad))))
    (try
      (with-open [node (xtn/start-node config)]
        (let [database (db/primary-db node)
              history (mapv (fn [i ops]
                              (let [time (.plusSeconds t i)
                                    id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                {:tx-id id :system-time time :ops ops})) (range) txs)
              expected (peek history)
              _ (.awaitSourceBlocking database (:tx-id expected) (Duration/ofSeconds 30))
              r (compare! node expected spec tables)]
          (is (= :matched (:status r)) (pr-str r))
          (is (= :matched-source-derived-puts (:mapped-write-suffix r)))
          (is (= :matched-retained-predecessor (:prepared-prestate r)))
          (is (= :not-asserted (:selected-transaction-commitment r)))
          (is (= :none (:authority r)))
          (is (= :not-issued (:certificate r)))
          (is (= :not-established (:provider-refinement r)))
          (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id expected)])))
          (doseq [bad [(pop puts) (conj puts (first puts))
                       (assoc-in puts [0 2 :amount] 4)
                       (assoc-in puts [0 2 :extra] "changed")
                       (update-in puts [0 2] dissoc :enabled)
                       (assoc-in puts [1 2 :revision] 1)
                       (assoc-in puts [2 2 :gesso.command/created-at] "t2")
                       (conj puts [:put-docs :gesso_requests {:xt/id "other" :amount 3}])]]
            (let [r (compare! node (assoc expected :ops (into prefix bad)) spec tables)]
              (is (= :mismatch (:status r)) (pr-str r))
              (is (= :selected-retained-write-suffix-mismatch (:reason r)))
              (is (not (contains? r :mapped-write-suffix)))))
          (doseq [bad [(conj puts ["ASSERT TRUE"])
                       (assoc-in puts [0 1] {:into :gesso_requests :valid-from t})]]
            (let [r (compare! node (assoc expected :ops (into prefix bad)) spec tables)]
              (is (= :unsupported (:status r)))
              (is (not (contains? r :mapped-write-suffix)))))
          (is (= :mismatch (:status (compare! node expected (assoc-in spec [:inputs :example/next-count] 4) tables))))
          (is (= :mismatch (:status (compare! node (update expected :tx-id inc) spec tables))))
          ;; Row correspondence ignores batching/order, but the retained
          ;; transaction binding does not allow a reordered expectation.
          (is (= :mismatch (:status (compare! node (assoc expected :ops (into prefix (reverse puts))) spec tables))))
          (is (= :unsupported (:status (compare! node expected spec #{:gesso_requests :gesso_commands}))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest global-command-absence-is-not-slot-absence
  (let [root (Files/createTempDirectory "gesso-global-command-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        spec {:inputs {:example/next-count 3} :effects {[:document :gesso_requests "r1"] {:revision 1}
                                                       [:document :gesso_assignments "a1"] {:revision 0}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        fresh (assoc-in spec [:binding :command-id] (identity/command-id "c2"))
        occupied (resolution/command-record (assoc binding :principal (identity/principal "helper-b"))
                                            {:status :claimed} "t0")
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! source targets) docs [])
        literal ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" "c1"]
        bound ["ASSERT ? = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" 0 "c2"]
        puts (writes/derive-puts! source targets docs [] fresh)
        txs [[[:put-docs :gesso_requests row] [:put-docs :gesso_commands occupied]]
             (into (conj prefix literal) (writes/derive-puts! source targets docs [] spec))
             (into (conj prefix bound) puts)]
        tables #{:gesso_requests :gesso_assignments :gesso_commands}
        check! (fn [node tx s]
                 (writes/compare-local-processed-retained-guarded-mapped-writes!
                  node tables source targets docs [] tx (count prefix) s))]
    (try
      (let [history (with-open [node (xtn/start-node config)]
                      (let [database (db/primary-db node)
                            history (mapv (fn [i ops]
                                            (let [time (.plusSeconds t i)
                                                  id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                              (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                              {:tx-id id :system-time time :ops ops})) (range) txs)]
                        (.awaitSourceBlocking database (:tx-id (peek history)) (Duration/ofSeconds 30))
                        (is (= [true false true]
                               (mapv (fn [tx] (:committed (first (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id tx)])))) history)))
                        (is (= [occupied] (xt/q node ["SELECT * FROM gesso_commands WHERE _id = ?" "c1"])))
                        (let [collision (check! node (second history) spec)
                              success (check! node (peek history) fresh)]
                          (is (= :mismatch (:status collision)) (pr-str collision))
                          (is (= :selected-retained-command-id-present (:reason collision)))
                          (is (not (contains? collision :global-command-id)))
                          (is (= :matched (:status success)) (pr-str success))
                          (is (= writes/guarded-profile (:mapped-retained-writes-profile success)))
                          (is (= :absent-at-retained-predecessor (:global-command-id success)))
                          (is (= :matched (:command-id-absence-guard success)))
                          (is (= :matched-source-derived-puts (:mapped-write-suffix success)))
                          (is (= :not-asserted (:selected-transaction-commitment success)))
                          (is (= :none (:authority success)))
                          (is (= :not-issued (:certificate success))))
                        (doseq [ops [(into prefix puts)
                                     (into (conj prefix (assoc bound 2 "other")) puts)
                                     (into (conj prefix (assoc bound 1 1)) puts)
                                     (conj (into prefix puts) bound)
                                     (into (conj prefix bound) (conj puts bound))]]
                          (let [r (check! node (assoc (peek history) :ops ops) fresh)]
                            (is (= :unsupported (:status r)) (pr-str r))
                            (is (not (contains? r :global-command-id)))))
                        history))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (is (= :matched (:status (check! node (peek history) fresh))))
          (is (= :selected-retained-command-id-present (:reason (check! node (second history) spec))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))
