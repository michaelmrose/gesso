(ns gesso.live.outbox.initialization-test
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.operational-report :as operational-report]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.xtdb-source-log :as source-log]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.initialization :as init]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.acquisition :as acquisition]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.outbox.history-coverage :as history]
            [gesso.live.outbox.history-controller :as controller]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.writer-policy :as policy]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.inductive-plan-test]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn]))

(defn- admission!
  ([] (admission! :requests #{}))
  ([table auxiliary] (admission! table auxiliary :example/initialization :assignments))
  ([table auxiliary catalog-id nullable-table]
   (admission! table auxiliary catalog-id nullable-table false))
  ([table auxiliary catalog-id nullable-table declared?]
   (admission! table auxiliary catalog-id nullable-table declared? false))
  ([table auxiliary catalog-id nullable-table declared? residual?]
  (let [id :example/initialize
        source {:subject {:kind :operation :id id :interface {:operation id}}
                :state-types {:example/count {:type :integer}
                              :example/assignee {:type :string :nullable? true}}
                :input-types {} :guard {:readers #{} :expression [:literal true]}
                :reads #{} :writes #{:example/count}
                :replacements {:example/count [:literal 3]} :frame #{:example/assignee}}
        invariant {:readers #{:example/count}
                   :expression [:>= [:read :example/count] [:literal 0]]}
        catalog {:id catalog-id :initial-condition invariant
                 :invariants {:example/nonnegative invariant}
                 :declared-operation-ids #{id} :operations [{:id id :construction source}]}]
    (inductive/build-inductive-mapped-admission!
     (cond-> {:inductive-source ((deref (ns-resolve 'gesso.model.inductive-plan-test 'proof-package)) catalog)
      :target-map {:example/count [:document table "r1" :item_count]
                   :example/assignee [:nullable-document nullable-table "a1" :helper]}
      :decode-outcome! identity
      :transaction-footprint! (fn [_] (throw (ex-info "Unexpected mapped operation" {})))
      :auxiliary-tables auxiliary :document-effects {id {[:document table "r1"] {}}}}
       declared? (assoc :publication-derivations {id nil})
       residual? (assoc :residual-assertion-derivations {id nil}))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- assert-result [f]
  (try (f) :committed (catch Exception e
                       (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                         :assert-failed (throw e)))))

(deftest initialization-is-atomic-and-durable-across-node-and-policy-restart
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2522-init-" (make-array java.nio.file.attribute.FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        source "source"
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]
              [:put-docs :memberships {:xt/id "m1" :active true}]]
        changed (assoc-in seed [0 2 :item-count] 99)
        observed (atom [])
        scope {:source-id source :worker-id "worker" :descriptor nil}
        admission (admission!)
        checked (inductive/check-mapped-initialization! admission seed)
        marker (init/marker source checked)
        changed-checked (inductive/check-mapped-initialization! admission changed)
        install! (fn [node seed]
                   (xtdb/register-submission-policy!
                    node (policy/build-policy! node {:admission admission :initializer seed
                                                     :observe! #(swap! observed conj %)} scope)))
        read-requests #(xt/q % ["SELECT * FROM requests"])
        read-marker #(xt/q % ["SELECT * FROM gesso_initializations"])]
    (try
      (with-open [node (xtn/start-node config)]
        (let [release (install! node seed)]
          (try
            (is (false? (init/initialized? node source checked)))
            (is (= :submission-policy-refused (reason #(xtdb/execute-tx! node seed))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/execute-tx! node (pop (init/transaction source seed #{:assignments} checked))))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/submit-tx! node (init/transaction source seed #{:assignments} checked)))))
            (is (nil? (init/initialize! node source seed #{:assignments} checked)))
            (is (= [marker] (read-marker node)))
            (is (= [{:xt/id "r1" :item-count 0}] (read-requests node)))
            (is (= [{:kind :initializer}] @observed))
            ;; Simulate subsequent authorized domain evolution via raw XTDB.
            ;; This deliberately bypasses the adapter, not a writer-closure test.
            (xt/execute-tx node [[:delete-docs :requests "r1"]])
            (finally (release)))))
      (with-open [node (xtn/start-node config)]
        (let [release (install! node changed)]
          (try
            (is (true? (init/initialized? node source checked)))
            (is (= :initialization-evidence-mismatch
                   (reason #(policy/initialize! node source {:admission admission :initializer changed}))))
            (is (= :missing-mapped-initializer
                   (reason #(policy/initialize! node source {:admission admission :initializer nil}))))
            (is (nil? (policy/initialize! node source {:admission (admission!) :initializer seed})))
            (is (= :initialization-evidence-mismatch
                   (reason #(xtdb/execute-tx! node (init/transaction source changed #{:assignments} changed-checked)))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/execute-tx! node (init/transaction source seed #{:assignments} checked)))))
            (is (= [marker] (read-marker node)))
            (is (empty? (read-requests node)))
            (is (= [{:kind :initializer}] @observed))
            (finally (release)))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))] (io/delete-file entry true))))
    (with-open [node (xtn/start-node {})]
      ;; A legacy/unmarked database with an unrelated row in a seed table
      ;; must not be adopted or partially initialized.
      (xt/execute-tx node [[:put-docs :requests {:xt/id "other" :item-count 42}]])
      (let [release (install! node seed)]
        (try
          (is (= :assert-failed (assert-result #(init/initialize! node source seed #{:assignments} checked))))
          (is (= [{:xt/id "other" :item-count 42}] (read-requests node)))
          (is (empty? (xt/q node ["SELECT * FROM memberships"])))
          (is (empty? (read-marker node)))
          (finally (release))))
      (xt/execute-tx node [[:put-docs :gesso_initializations (assoc marker :profile-version 1)]])
      (is (= :initialization-evidence-mismatch (reason #(init/initialize! node source seed #{:assignments} checked))))
      (is (= :invalid-mapped-writer-policy
             (reason #(policy/validate-config! {:admission admission :initializer [[:put-docs init/table marker]]
                                                :observe! identity})))))))

(deftest selected-writer-policy-reserves-infrastructure-tables
  (let [config #(hash-map :admission % :initializer [[:put-docs :requests {:xt/id "r1" :item-count 0}]] :observe! identity)
        check #(reason (fn [] (policy/validate-config! (config %))))
        canonical? (deref (ns-resolve 'gesso.live.outbox.writer-policy 'mapped-operations?))]
    (doseq [table [:gesso_initializations :gesso_publication_manifests :gesso_live_outbox :gesso_transition_evidence]]
      (is (= :reserved-mapped-document-table (check (admission! table #{})))))
    (doseq [table [:gesso_initializations :gesso_publication_manifests :other_effects :gesso_transition_evidence]]
      (is (= :unsupported-mapped-auxiliary-table (check (admission! :requests #{table})))))
    (is (nil? (check (admission! :requests #{:gesso_live_outbox}))))
    (let [base [["ASSERT TRUE"] [:put-docs :requests {:xt/id "r1"}]
                [:put-docs :gesso_commands {:xt/id "c1"}]
                [:put-docs :gesso_live_outbox {:xt/id "o1"}]]]
      (is (canonical? base))
      (doseq [table [:gesso_initializations :gesso_publication_manifests
                     :gesso-initializations :public/gesso_initializations
                     :public/gesso_publication_manifests :gesso-live-outbox
                     :public/gesso_commands :Requests :public/requests]]
        (is (not (canonical? (assoc-in base [1 1] table))) (str table)))
      (is (not (canonical? (conj base [:delete-docs :requests "r1"])))))))

(deftest selected-initializer-establishes-the-declared-initial-case
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config (fn [seed] {:admission admission :initializer seed :observe! identity})
        check #(reason (fn [] (policy/validate-config! (config %))))
        scope {:source-id "source" :worker-id "worker" :descriptor nil}
        checked (inductive/check-mapped-initialization! admission seed)]
    (is (= {:example/count 0 :example/assignee nil} (:state checked)))
    (is (= #{:requests :assignments} (:tables checked)))
    (is (= :none (:authority checked)))
    (is (= :constructor-outside-initial-condition (check (assoc-in seed [0 2 :item-count] -1))))
    (is (= :missing-mapped-field (check (update-in seed [0 2] dissoc :item-count))))
    (is (= :aliased-initial-document-key (check (assoc-in seed [0 2 :item_count] 5))))
    (is (= :duplicate-initial-document (check (into seed seed))))
    (is (= :unexpected-row (check (conj seed [:put-docs :requests {:xt/id "other" :item-count 0}]))))
    (is (= :unsupported-null-initial-field
           (check (conj seed [:put-docs :assignments {:xt/id "a1" :helper nil}]))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :assignments {:xt/id "a1" :helper "old"}]])
      (let [release (xtdb/register-submission-policy! node (policy/build-policy! node (config seed) scope))]
        (try
          (is (= :submission-policy-refused
                 (reason #(xtdb/execute-tx! node (init/transaction "source" seed #{} checked)))))
          (is (= :assert-failed (assert-result #(policy/initialize! node "source" (config seed)))))
          (is (empty? (xt/q node ["SELECT * FROM requests"])))
          (is (empty? (xt/q node ["SELECT * FROM gesso_initializations"])))
          (is (= [{:xt/id "a1" :helper "old"}] (xt/q node ["SELECT * FROM assignments"])))
          (finally (release)))))))

(deftest initialization-evidence-binds-catalog-mapping-and-complete-seed
  (let [seed [[:put-docs :requests {:xt/id "r1" :item-count 0 :hidden "original"}]]
        admission (admission!)
        check #(inductive/check-mapped-initialization! %1 %2)
        baseline (check admission seed)
        different-catalog (admission! :requests #{} :example/other :assignments)
        different-map (admission! :requests #{} :example/initialization :other_assignments)
        altered (assoc-in seed [0 2 :hidden] "different")]
    (is (= baseline (check (admission!) seed)))
    (is (= baseline (check admission (update-in seed [0 2] #(into (sorted-map) %)))))
    (doseq [other [(check different-catalog seed) (check different-map seed) (check admission altered)]]
      (is (= (:state baseline) (:state other)))
      (is (not= (:initialization-fingerprint baseline) (:initialization-fingerprint other))))
    (with-open [node (xtn/start-node {})]
      (let [cfg {:admission admission :initializer seed :observe! identity}
            release (xtdb/register-submission-policy!
                     node (policy/build-policy! node cfg {:source-id "s"}))]
        (try
          (is (nil? (policy/initialize! node "s" cfg)))
          (doseq [other [(assoc cfg :admission different-catalog)
                         (assoc cfg :admission different-map)
                         (assoc cfg :initializer altered)]]
            (is (= :initialization-evidence-mismatch
                   (reason #(policy/initialize! node "s" other)))))
          (is (= [(init/marker "s" baseline)] (xt/q node ["SELECT * FROM gesso_initializations"])))
          (is (= [(get-in seed [0 2])] (xt/q node ["SELECT * FROM requests"])))
          (finally (release)))))))

(deftest initialization-readback-is-pinned-and-rechecked-on-restart
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        scope {:source-id "s"}
        checked (inductive/check-mapped-initialization! admission seed)
        changed [[:put-docs :requests {:xt/id "r1" :item-count 9}]]
        execute xtdb/execute-tx!]
    (with-open [node (xtn/start-node {})]
      (let [release (xtdb/register-submission-policy! node (policy/build-policy! node config scope))]
        (try
          ;; An intervening later commit must not replace the actual receipt's
          ;; snapshot, nor affect the historical observation on restart.
          (with-redefs [xtdb/execute-tx! (fn [n ops & [opts]]
                                         (let [result (execute n ops opts)]
                                           (xt/execute-tx n changed)
                                           result))]
            (is (nil? (policy/initialize! node "s" config))))
          (is (= [{:xt/id "r1" :item-count 9}] (xt/q node ["SELECT * FROM requests"])))
          (is (nil? (policy/initialize! node "s" config)))
          (finally (release)))))
    (with-open [node (xtn/start-node {})]
      ;; Simulate a marker from an incompatible physical execution. Even if
      ;; current rows are repaired, the original historical state must fail.
      ;; Raw XTDB is used deliberately to simulate missing correspondence.
      (xt/execute-tx node (conj changed [:put-docs init/table (init/marker "s" checked)]))
      (xt/execute-tx node seed)
      (let [release (xtdb/register-submission-policy! node (policy/build-policy! node config scope))]
        (try
          (is (true? (init/initialized? node "s" checked)))
          (is (= :initialization-readback-state-mismatch
                 (reason #(policy/initialize! node "s" config))))
          (is (= :initialization-readback-state-mismatch
                 (reason #(policy/initialize! node "s" config))))
          (is (= [{:xt/id "r1" :item-count 0}] (xt/q node ["SELECT * FROM requests"])))
          (finally (release)))))))

(defn- empty-admission! []
  (let [id :example/empty-init
        source {:subject {:kind :operation :id id :interface {:operation id}}
                :state-types {:example/status {:type :keyword :nullable? true}}
                :input-types {} :guard {:readers #{} :expression [:literal true]}
                :reads #{} :writes #{:example/status}
                :replacements {:example/status [:literal :claimed]} :frame #{}}
        invariant {:readers #{} :expression [:literal true]}
        catalog {:id :example/empty-initialization
                 :initial-condition {:readers #{:example/status}
                                     :expression [:= [:read :example/status] [:literal nil]]}
                 :invariants {:example/valid invariant}
                 :declared-operation-ids #{id} :operations [{:id id :construction source}]}]
    (inductive/build-inductive-mapped-admission!
     {:inductive-source ((deref (ns-resolve 'gesso.model.inductive-plan-test 'proof-package)) catalog)
      :target-map {:example/status [:nullable-document :requests "r1" :status]}
      :decode-outcome! identity
      :transaction-footprint! (fn [_] (throw (ex-info "Unexpected mapped operation" {})))
      :auxiliary-tables #{} :document-effects {id {[:document :requests "r1"] {}}}})))

(deftest mapped-initialization-requires-an-explicit-checked-seed
  (let [admission (empty-admission!)
        config {:admission admission :initializer [] :observe! identity}
        omitted (assoc config :initializer nil)
        scope {:source-id "s"}
        checked (inductive/check-mapped-initialization! admission [])]
    (is (= {:example/status nil} (:state checked)))
    (is (= :missing-mapped-initializer (reason #(policy/validate-config! omitted))))
    (is (= :missing-required-document
           (reason #(policy/validate-config! (assoc config :admission (admission!))))))
    (is (= :constructor-outside-initial-condition
           (reason #(policy/validate-config!
                     (assoc config :initializer [[:put-docs :requests {:xt/id "r1" :status :claimed}]])))))
    (with-open [node (xtn/start-node {})]
      (is (= :missing-mapped-initializer (reason #(policy/build-policy! node omitted scope))))
      (is (= :missing-mapped-initializer (reason #(policy/initialize! node "s" omitted))))
      (is (empty? (xt/q node ["SELECT * FROM gesso_initializations"])))
      (let [release (xtdb/register-submission-policy! node (policy/build-policy! node config scope))]
        (try
          (is (nil? (policy/initialize! node "s" config)))
          (is (= [(init/marker "s" checked)] (xt/q node ["SELECT * FROM gesso_initializations"])))
          (is (empty? (xt/q node ["SELECT * FROM requests"])))
          (is (nil? (policy/initialize! node "s" config)))
          (finally (release)))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :status :claimed}]])
      (let [release (xtdb/register-submission-policy! node (policy/build-policy! node config scope))]
        (try
          (is (= :assert-failed (assert-result #(policy/initialize! node "s" config))))
          (is (empty? (xt/q node ["SELECT * FROM gesso_initializations"])))
          (is (= [{:xt/id "r1" :status :claimed}] (xt/q node ["SELECT * FROM requests"])))
          (finally (release)))))))

(deftest mapped-writer-activation-requires-physical-init-of-exact-policy
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        scope {:source-id "s"}
        checked (inductive/check-mapped-initialization! admission seed)
        envelope {:api :execute-tx :tx-options {} :tx-ops seed}
        handoffs (atom 0)
        ;; Isolate only the one-use handoff for this gate regression. Real
        ;; framed handoffs are exercised by the physical Claim suite.
        claim (fn [& _]
                (swap! handoffs inc)
                {:identity {:alternative :example/initialize}
                 :candidate {:authority :none}})]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config scope)
            another (policy/build-policy! node config scope)]
        (is (= :uninstalled-mapped-writer-policy (reason #(policy/activate-policy! writer))))
        (let [release (xtdb/register-submission-policy! node writer)]
          (try
            (is (= :uninstalled-mapped-writer-policy (reason #(policy/activate-policy! another))))
            (with-redefs [publication/claim-framed-submission! claim]
              (is (false? (writer envelope)))
              (is (= 0 @handoffs))
              ;; A compatible committed marker by itself does not open the gate.
              (is (nil? (policy/initialize! node "s" config)))
              (is (false? (writer envelope)))
              (is (= 0 @handoffs))
              (is (nil? (correspondence/initialization-observation node admission)))
              (is (nil? (policy/activate-policy! writer)))
              (let [observation (correspondence/initialization-observation node admission)
                    handle (xtdb/adapter-connectable! node)]
                (is (= (:initialization-fingerprint checked) (:initialization-fingerprint observation)))
                (is (= (:catalog-fingerprint checked) (:catalog-fingerprint observation)))
                (is (= :none (:authority observation)))
                (is (not (contains? observation :state)))
                (is (= observation (correspondence/initialization-observation handle admission)))
                (is (nil? (correspondence/initialization-observation node (admission!)))))
              (is (true? (writer envelope)))
              (is (= 1 @handoffs))
              (is (= :invalid-mapped-writer-activation (reason #(policy/activate-policy! writer)))))
            (finally (release))))
        (is (nil? (correspondence/initialization-observation node admission)))
        ;; Installing the same callable again is a different generation.
        (let [release (xtdb/register-submission-policy! node writer)]
          (try
            (is (nil? (correspondence/initialization-observation node admission)))
            (with-redefs [publication/claim-framed-submission! claim]
              (is (false? (writer envelope)))
              (is (= 1 @handoffs)))
            (finally (release))))
        (let [release (xtdb/register-submission-policy! node another)]
          (try
            (is (nil? (policy/activate-policy! another)))
            (with-redefs [publication/claim-framed-submission! claim]
              (is (true? (another envelope)))
              (is (= 2 @handoffs)))
            (finally (release))))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node (conj (assoc-in seed [0 2 :item-count] 9)
                               [:put-docs init/table (init/marker "s" checked)]))
      (let [writer (policy/build-policy! node config scope)
            release (xtdb/register-submission-policy! node writer)]
        (try
          (is (= :initialization-readback-state-mismatch
                 (reason #(policy/activate-policy! writer))))
          (is (nil? (correspondence/initialization-observation node admission)))
          (with-redefs [publication/claim-framed-submission! claim]
            (is (false? (writer envelope)))
            (is (= 2 @handoffs)))
          (is (= :invalid-mapped-writer-activation (reason #(policy/activate-policy! writer))))
          (finally (release)))))))

(deftest bounded-history-audit-never-hides-an-intervening-mapped-change
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (is (= :missing-history-initialization (reason #(history/audit! node admission 16))))
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                initial (history/audit! handle admission 16)]
            (is (= :observed-interval-checked (:status initial)))
            (is (= [] (:steps initial)))
            ;; Raw writes deliberately bypass mediation to test the independent audit.
            (xt/execute-tx node [["ASSERT TRUE"]])
            (is (= :assert-failed (assert-result #(xt/execute-tx node [["ASSERT FALSE"]]))))
            (let [report (history/audit! handle admission 16)]
              (is (= :observed-interval-checked (:status report)))
              (is (= [:observed-mapped-stutter :aborted-stutter] (mapv :classification (:steps report))))
              (is (= :none (:authority report)))
              (is (contains? (:open-obligations report) :infrastructure-writer-footprint-coverage)))
            (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count 99}]])
            (xt/execute-tx node seed)
            (let [report (history/audit! handle admission 16)]
              (is (= :unclassified-steps (:status report)))
              (is (= [:observed-mapped-stutter :aborted-stutter :unclassified :unclassified]
                     (mapv :classification (:steps report))))
              (is (= :history-step-limit-exceeded (reason #(history/audit! handle admission 3))))
              (is (= :missing-history-initialization (reason #(history/audit! handle (admission!) 16))))
              ;; Changes confined to infrastructure are not automatically stutters.
              (xt/execute-tx node [[:put-docs :gesso_live_outbox {:xt/id (random-uuid) :unexpected true}]])
              (xt/execute-tx node [[:put-docs :gesso_publication_manifests {:xt/id "s" :descriptor "unexplained"}]])
              (is (= [:unclassified :unclassified]
                     (mapv :classification (take-last 2 (:steps (history/audit! handle admission 16))))))))
          (finally (release)))))))

(deftest incremental-history-cursors-compose-without-dropping-findings
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)
            checkpoint (atom nil)]
        (try
          (policy/activate-policy! writer)
          (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count 99}]])
          (xt/execute-tx node seed)
          (is (= :assert-failed (assert-result #(xt/execute-tx node [["ASSERT FALSE"]]))))
          (xt/execute-tx node [["ASSERT TRUE"]])
          (let [handle (xtdb/adapter-connectable! node)
                a (history/audit-chunk! handle admission 1)
                b (history/audit-chunk! handle admission 2 (:checkpoint a))
                c (history/audit-chunk! handle admission 1 (:checkpoint b))
                whole (history/audit! handle admission 16)]
            (is (= :unclassified-steps (:status a)))
            (is (false? (:caught-up? a)))
            (is (= {:unclassified 1} (:counts a)))
            (is (= {:unclassified 2 :aborted-stutter 1} (:counts b)))
            (is (= :unclassified-steps (:status c)))
            (is (true? (:caught-up? c)))
            (is (= (:target-basis c) (:through-basis c) (:through-basis whole)))
            (is (= (:steps whole) (vec (concat (:steps a) (:steps b) (:steps c)))))
            (is (= (frequencies (map :classification (:steps whole))) (:counts c)))
            (is (= (first (:steps a)) (:first-unclassified a) (:first-unclassified c)))
            (is (= :invalid-history-checkpoint (reason #(history/audit-chunk! handle admission 1 a))))
            (is (= :invalid-history-checkpoint
                   (reason #(history/audit-chunk! handle admission 1 (history/->HistoryCheckpoint)))))
            (let [empty (history/audit-chunk! handle admission 1 (:checkpoint c))]
              (is (= [] (:steps empty)))
              (is (= (:counts c) (:counts empty)))
              (is (= (:through-basis c) (:through-basis empty))))
            ;; The next chunk may extend the target; it must start at the old
            ;; anchor and retain all earlier unresolved observations.
            (xt/execute-tx node [["ASSERT TRUE"]])
            (let [extended (history/audit-chunk! handle admission 1 (:checkpoint c))]
              (is (= [:observed-mapped-stutter] (mapv :classification (:steps extended))))
              (is (= {:unclassified 2 :aborted-stutter 1 :observed-mapped-stutter 2} (:counts extended)))
              (is (= :unclassified-steps (:status extended)))
              (is (true? (:caught-up? extended)))
              (reset! checkpoint (:checkpoint extended))))
          (finally (release)))
        (let [another (policy/build-policy! node config {:source-id "s"})
              release (xtdb/register-submission-policy! node another)]
          (try
            (policy/activate-policy! another)
            (is (= :invalid-history-checkpoint
                   (reason #(history/audit-chunk! node admission 1 @checkpoint))))
            (finally (release))))))))

(deftest authenticated-history-checkpoint-survives-node-restart
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2540-history-" (make-array java.nio.file.attribute.FileAttribute 0))
        config {:log [:local {:path (str root "/log")}]
                :storage [:local {:path (str root "/storage")}]}
        file (io/file (.toFile root) "checkpoint.edn")
        key (byte-array 32)
        _ (.nextBytes (java.security.SecureRandom.) key)
        wrong-key (aclone key)
        _ (aset-byte wrong-key 0 (unchecked-byte (bit-xor 1 (aget wrong-key 0))))
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        install! (fn [node admission]
                   (let [writer (policy/build-policy! node {:admission admission :initializer seed :observe! identity}
                                                      {:source-id "s"})
                         release (xtdb/register-submission-policy! node writer)]
                     (policy/activate-policy! writer)
                     release))]
    (try
      (with-open [node (xtn/start-node config)]
        (let [admission (admission!) release (install! node admission)]
          (try
            (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count 99}]])
            (let [report (history/audit-chunk! node admission 1)
                  sealed (history/seal-checkpoint! node admission (:checkpoint report) key "db/audit-v1")]
              (is (= {:unclassified 1} (:counts report)))
              (is (= sealed (edn/read-string (pr-str sealed))))
              (is (= :invalid-history-checkpoint
                     (reason #(history/seal-checkpoint! node admission report key "db/audit-v1"))))
              (spit file (pr-str sealed)))
            (finally (release)))))
      (with-open [node (xtn/start-node config)]
        (let [admission (admission!) release (install! node admission)
              sealed (edn/read-string (slurp file))
              restore #(history/restore-checkpoint! node admission %1 %2 %3)]
          (try
            (is (= :invalid-durable-history-checkpoint
                   (reason #(restore (assoc-in sealed [:payload :counts] {}) key "db/audit-v1"))))
            (is (= :invalid-durable-history-checkpoint
                   (reason #(restore sealed wrong-key "db/audit-v1"))))
            (is (= :incompatible-durable-history-checkpoint
                   (reason #(restore sealed key "db/audit-v2"))))
            (let [cursor (restore sealed key "db/audit-v1")]
              (xt/execute-tx node seed)
              (let [extended (history/audit-chunk! node admission 1 cursor)
                    whole (history/audit! node admission 16)]
                (is (= :unclassified-steps (:status extended)))
                (is (true? (:caught-up? extended)))
                (is (= {:unclassified 2} (:counts extended)))
                (is (= (first (:steps whole)) (:first-unclassified extended)))
                (is (= [(second (:steps whole))] (:steps extended)))
                (is (= (:through-basis whole) (:through-basis extended)))))
            (finally (release)))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))] (io/delete-file entry true))))))

(deftest owner-history-controller-persistence-and-drain
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        sealed (atom nil)
        fail? (atom false)
        block? (atom false)
        entered (promise)
        proceed (promise)
        opts {:key (byte-array 32) :deployment-id "controller-test" :max-steps 1
              :load! #(deref sealed)
              :save! (fn [value]
                       (when @fail? (throw (ex-info "Storage unavailable" {:reason :test-save-failed})))
                       (when @block? (deliver entered true) @proceed)
                       (reset! sealed value))}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (reset! fail? true)
          (is (= :test-save-failed (reason #(controller/start! node admission opts))))
          (reset! fail? false)
          (let [c (controller/start! node admission opts)
                advance (:advance! c) status (:status c) close (:close! c)
                initial (:report (status))
                initial-sealed @sealed]
            (is (= :observed-interval-checked (:status initial)))
            (is (= {} (:counts initial)))
            (is (not (contains? initial :checkpoint)))
            (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count 99}]])
            (reset! fail? true)
            (is (= :test-save-failed (reason advance)))
            (is (= initial (:report (status))))
            (is (= initial-sealed @sealed))
            (is (= :history-advance-failed (:last-error (status))))
            (let [report (operational-report/describe-history (status))]
              (is (= :audit-failed (:status report)))
              (is (= {} (get-in report [:coverage :classification-counts])))
              (is (string? (get-in report [:controller :last-failure-at]))))
            (reset! fail? false)
            (is (= {:unclassified 1} (:counts (advance))))
            (is (nil? (:last-error (status))))
            (close 10)
            (is (= :history-controller-closed (reason advance)))
            (let [c (controller/start! node admission opts)
                  advance (:advance! c) status (:status c) close (:close! c)]
              (is (= {:unclassified 1} (get-in (status) [:report :counts])))
              (is (empty? (get-in (status) [:report :steps])))
              (xt/execute-tx node seed)
              (reset! block? true)
              (let [task (future (advance))]
                (try
                  (is (= true (deref entered 5000 :timeout)))
                  (is (= :history-controller-busy (reason advance)))
                  (is (= :history-controller-not-drained (reason #(close 0))))
                  (is (= :closing (:phase (status))))
                  (is (true? (get-in (operational-report/describe-history (status))
                                     [:controller :advance-in-progress?])))
                  (finally (deliver proceed true)))
                (is (= {:unclassified 2} (:counts (deref task 5000 :timeout))))
                (close 1000)
                (is (= :closed (:phase (status))))
                (is (= :history-controller-closed (reason advance))))))
          (finally (release)))))))

(deftest scheduled-history-controller-advances-and-persists
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        sealed (atom nil)
        saved (promise)
        saves (atom 0)
        opts {:key (byte-array 32) :deployment-id "scheduled-test" :max-steps 1
              :load! #(deref sealed)
              :save! (fn [value] (reset! sealed value)
                       (when (= 2 (swap! saves inc)) (deliver saved true)))
              :schedule {:catch-up-ms 10 :idle-ms 1000 :failure-ms 100 :max-failure-ms 400}}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count 99}]])
          (xt/execute-tx node seed)
          (let [c (controller/start! node admission opts)]
            (try
              (is (= true (deref saved 10000 :timeout)))
              (finally ((:close! c) 10000)))
            (let [status ((:status c))]
              (is (= :closed (:phase status)))
              (let [r (operational-report/describe-history status)]
                (is (= :inactive (:status r)))
                (is (= :stopped (get-in r [:schedule :phase])))
                (is (= {:unclassified 2} (get-in r [:coverage :classification-counts])))
                (is (= r (edn/read-string (pr-str r)))))
              (is (= :stopped (get-in status [:schedule :phase])))
              (is (= :success (get-in status [:schedule :last-outcome])))
              (is (= {:unclassified 2} (get-in status [:report :counts])))
              (is (true? (get-in status [:report :caught-up?])))
              (is (= {:unclassified 2} (get-in @sealed [:payload :counts])))
              (is (= :history-controller-closed (reason (:advance! c))))))
          (finally (release)))))))

(deftest mapped-induction-report-rechecks-issued-source
  (let [issued (admission!)
        description (inductive/describe-mapped-admission issued)
        abstract (inductive/recheck-mapped-induction! issued)
        source (select-keys description [:catalog-fingerprint :induction-fingerprint])
        joined (operational-report/describe-application abstract {:phase :open :source source})]
    (is (= source (select-keys (:identity abstract) (keys source))))
    (is (= :source-bound-conditional-abstract-safety (:verdict abstract)))
    (is (= #{:example/initialize} (:declared-operation-ids abstract)))
    (is (= :matching-source-descriptions (:source-join joined)))
    (is (= :not-observed (get-in joined [:history :status])))
    (is (contains? (:open-obligations joined) :history-coverage-not-observed))
    (is (= :unregistered-mapped-admission
           (reason #(inductive/recheck-mapped-induction! description))))
    (is (= joined (edn/read-string (pr-str joined))))))

(deftest history-refuses-a-valid-time-discontinuity-between-transactions
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                scheduled (.plusSeconds (java.time.Instant/now) 5)
                key (byte-array 32)]
            ;; Deliberate raw writer bypass. At the first ledger sample the
            ;; scheduled row is not yet valid, but its retained write is refused.
            (xt/execute-tx node [[:put-docs {:into :requests :valid-from scheduled}
                                 {:xt/id "r1" :item-count -1}]])
            (let [prefix (history/audit-chunk! handle admission 8)
                  sealed (history/seal-checkpoint! handle admission (:checkpoint prefix) key "sampled-v2")
                  cursor (history/restore-checkpoint! handle admission sealed key "sampled-v2")
                  old-payload (assoc (:payload sealed) :profile :selected-mapped-history-checkpoint-v18)
                  old-mac ((deref (ns-resolve 'gesso.live.outbox.history-coverage 'checkpoint-mac)) key old-payload)
                  old-sealed {:payload old-payload :mac (.formatHex (java.util.HexFormat/of) ^bytes old-mac)}]
              (is (= :selected-mapped-history-checkpoint-v19 (get-in sealed [:payload :profile])))
              (is (= :invalid-durable-history-checkpoint
                     (reason #(history/restore-checkpoint! handle admission old-sealed key "sampled-v2"))))
              (is (= [:unclassified] (mapv :classification (:steps prefix))))
              (is (= :historical-stutter-contains-write (get-in prefix [:first-unclassified :reason])))
              ;; An unrelated transaction does not change the selected rows
              ;; at its own clock, but valid-time advancement changed them.
              (let [remaining (.toMillis (java.time.Duration/between (java.time.Instant/now) scheduled))]
                (when (>= remaining 0) (Thread/sleep (+ 100 remaining))))
              (xt/execute-tx node [["ASSERT TRUE"]])
              (let [extended (history/audit-chunk! handle admission 8 cursor)
                    whole (history/audit! handle admission 8)]
                (is (= :unclassified-steps (:status extended)))
                (is (= :sampled-valid-time-discontinuity (get-in extended [:steps 0 :reason])))
                (is (= {:unclassified 2} (:counts extended)))
                (is (= (:steps whole) (into (:steps prefix) (:steps extended))))
                (is (contains? (:open-obligations extended) :valid-time-continuity-between-steps)))))
          (finally (release)))))))

(deftest history-refuses-transient-valid-time-changes-with-equal-endpoints
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                start (.plusSeconds (java.time.Instant/now) 5)
                end (.plusSeconds start 1)]
            (xt/execute-tx node [[:put-docs {:into :requests :valid-from start :valid-to end}
                                 {:xt/id "r1" :item-count -1}]])
            (let [prefix (history/audit-chunk! handle admission 8)
                  key (byte-array 32)
                  sealed (history/seal-checkpoint! handle admission (:checkpoint prefix) key "interior-v3")
                  cursor (history/restore-checkpoint! handle admission sealed key "interior-v3")]
              (is (= [:unclassified] (mapv :classification (:steps prefix))))
              (is (= :historical-stutter-contains-write (get-in prefix [:first-unclassified :reason])))
              (let [remaining (.toMillis (java.time.Duration/between (java.time.Instant/now) end))]
                (when (>= remaining 0) (Thread/sleep (+ 100 remaining))))
              (xt/execute-tx node [["ASSERT TRUE"]])
              (let [extended (history/audit-chunk! handle admission 8 cursor)
                    whole (history/audit! handle admission 8)]
                (is (= :unclassified-steps (:status extended)))
                (is (= :interior-valid-time-boundary (get-in extended [:steps 0 :reason])))
                (is (= {:unclassified 2} (:counts extended)))
                (is (= (:steps whole) (into (:steps prefix) (:steps extended))))
                (is (contains? (:open-obligations extended) :valid-time-continuity-between-steps)))))
          (finally (release)))))))

(deftest physical-prefix-argument-from-issued-admission-and-observed-history
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        source (select-keys (inductive/describe-mapped-admission admission)
                            [:catalog-fingerprint :induction-fingerprint])
        abstract (inductive/recheck-mapped-induction! admission)]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (xt/execute-tx node [["ASSERT TRUE"]])
          (let [handle (xtdb/adapter-connectable! node)
                prefix (history/audit-chunk! handle admission 8)
                describe #(operational-report/describe-application
                           abstract {:phase :open :source source :report %})
                checked (describe prefix)]
            (is (= :conditional-observed-prefix-argument (get-in checked [:physical-induction :status])))
            (is (= #{:example/nonnegative} (get-in checked [:physical-induction :invariant-ids])))
            (is (= {:observed-mapped-stutter 1} (:counts prefix)))
            (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :item-count -1}]])
            (let [extended (history/audit-chunk! handle admission 8 (:checkpoint prefix))
                  refused (describe extended)]
              (is (= :unclassified-steps (:status extended)))
              (is (= :not-established (get-in refused [:physical-induction :status])))
              (is (contains? (:open-obligations refused) :unclassified-historical-steps))
              (is (= :not-issued (:certificate refused)))))
          (finally (release)))))))

(deftest history-requires-retained-envelope-for-mapped-transition
  ;; Exercise the history/envelope boundary with real retained transactions.
  ;; Stub only semantic projection judgments: this is not a Claim proof.
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        ops (fn [id]
              [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]
               ["ASSERT ? = ?" 1 1]
               [:put-docs :gesso_commands {:xt/id id}]
               [:put-docs :gesso_transition_evidence
                {:xt/id id :profile-version 4 :guard-prefix-fingerprint "guard"}]])]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                read-source source-log/observe-source-message!]
            (with-redefs [inductive/reconstruct-mapped-prestate-prefix! (constantly [["ASSERT ? = ?" 1 1]])
                          inductive/reconstruct-historical-mapped-writes!
                          (fn [_ record & _] {:tables #{:requests :gesso_commands}
                                             :puts [[:put-docs :gesso_commands record]]})]
              (xt/execute-tx node (envelope/bind-transition-envelope! (ops "good")))
              (let [prefix (history/audit-chunk! handle admission 8)]
                (is (= :selected-mapped-ledger-chunk-v19 (:profile prefix)))
                (is (= :observed-interval-checked (:status prefix)))
                (is (= [:matched] (mapv :recorded-envelope (:steps prefix))))
                (is (= [:matched] (mapv :intended-guard-prefix (:steps prefix))))
                (is (= [:matched] (mapv :intended-mapped-writes (:steps prefix))))
                (is (= [:matched] (mapv :transition-evidence-write (:steps prefix))))
                (is (= [:matched] (mapv :publication-outbox (:steps prefix))))
                (is (= [:matched] (mapv :write-table-closure (:steps prefix))))
                (is (= [:canonical-derived] (mapv :publication-records (:steps prefix))))
                (is (= :not-declared (:publication-expectation-profile prefix)))
                (is (= :not-declared (:residual-assertion-profile prefix)))
                (is (= [:not-declared] (mapv :residual-assertion-expectation (:steps prefix))))
                (is (= [:not-declared] (mapv :publication-expectation (:steps prefix))))
                (xt/execute-tx node (ops "missing"))
                (xt/execute-tx node (assoc (envelope/bind-transition-envelope! (ops "altered")) 0 ["ASSERT ? = ?" 2 2]))
                (let [unavailable-id (:tx-id (xt/execute-tx node (envelope/bind-transition-envelope! (ops "unavailable"))))
                      extended (with-redefs [source-log/observe-source-message!
                                             (fn [n id & limits]
                                               (if (= id unavailable-id)
                                                 {:status :unavailable :reason :source-message-not-retained}
                                                 (apply read-source n id limits)))]
                                 (history/audit-chunk! handle admission 8 (:checkpoint prefix)))]
                  (is (= :unclassified-steps (:status extended)))
                  (is (= [:historical-envelope-unsupported :historical-envelope-mismatch
                          :historical-envelope-unavailable] (mapv :reason (:steps extended))))
                  (is (= {:verified-mapped-transition 1 :unclassified 3} (:counts extended)))
                  (is (= :historical-envelope-unsupported (get-in extended [:first-unclassified :reason])))
                  (is (= :unclassified-steps (:status (history/audit-chunk! handle admission 8 (:checkpoint extended)))))
                  (is (= :none (:authority extended)))
                  (is (contains? (:open-obligations extended) :historical-intended-envelope-provenance))))))
          (finally (release)))))))

(deftest declared-writer-requires-matched-publication-for-installed-source
  (let [admission (admission! :requests #{} :example/initialization :assignments true)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        observed (atom [])
        config {:admission admission :initializer seed :observe! #(swap! observed conj %)}
        envelope {:api :execute-tx :tx-options {} :tx-ops seed}
        candidate (atom {})
        claim (fn [& _] {:identity {:alternative :example/initialize}
                         :candidate @candidate})]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (reset! observed [])
          ;; Isolate the installed policy from the genuine one-use handoff.
          ;; Claim's physical suite exercises both boundaries together.
          (with-redefs [publication/claim-framed-submission! claim]
            (doseq [bad [{} {:publication-expectation :not-declared}
                         {:publication-expectation :matched}
                         {:publication-expectation :matched :publication-source-id "other"}
                         {:publication-expectation :open :publication-source-id "s"}]]
              (reset! candidate bad)
              (is (false? (writer envelope)))
              (is (= :submission-policy-refused (reason #(xtdb/execute-tx! node seed))))
              (is (empty? @observed)))
            (reset! candidate {:publication-expectation :matched :publication-source-id "s"})
            (is (true? (writer envelope)))
            (is (= [{:kind :mapped-operation :alternative :example/initialize
                     :candidate @candidate}] @observed)))
          (finally (release)))))))

(deftest declared-writer-requires-matched-residual-assertions
  (doseq [[publication? residual?] [[true true] [false true] [true false] [false false]]]
    (let [admission (admission! :requests #{} :example/initialization :assignments publication? residual?)
          seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
          observed (atom [])
          config {:admission admission :initializer seed :observe! #(swap! observed conj %)}
          envelope {:api :execute-tx :tx-options {} :tx-ops seed}
          candidate (atom {})
          claim (fn [& _] {:identity {:alternative :example/initialize}
                           :candidate @candidate})
          valid-publication {:publication-expectation :matched :publication-source-id "s"}
          valid-residual {:residual-assertion-expectation :matched}]
      (with-open [node (xtn/start-node {})]
        (let [writer (policy/build-policy! node config {:source-id "s"})
              release (xtdb/register-submission-policy! node writer)]
          (try
            (policy/activate-policy! writer)
            (reset! observed [])
            ;; Isolate the gate: the real one-use handoff is exercised by
            ;; the physical Claim suite, not manufactured by this test.
            (with-redefs [publication/claim-framed-submission! claim]
              (doseq [pub [{} valid-publication (assoc valid-publication :publication-source-id "other")]
                      residual [{} valid-residual {:residual-assertion-expectation :open}
                                {:residual-assertion-expectation :not-declared}]]
                (let [c (merge pub residual)
                      allowed? (and (or (not publication?) (= pub valid-publication))
                                    (or (not residual?) (= residual valid-residual)))]
                  (reset! observed [])
                  (reset! candidate c)
                  (is (= allowed? (writer envelope)))
                  (if allowed?
                    (is (= [{:kind :mapped-operation :alternative :example/initialize :candidate c}] @observed))
                    (do (is (= :submission-policy-refused (reason #(xtdb/execute-tx! node seed))))
                        (is (empty? @observed)))))))
            (finally (release))))))))

(deftest committed-stutters-require-retained-assertion-only-operations
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                good (:tx-id (xt/execute-tx node [["ASSERT ? = ?" 1 1]]))
                prefix (history/audit-chunk! handle admission 1)]
            (is (= :observed-interval-checked (:status prefix)))
            (is (= [:observed-mapped-stutter] (mapv :classification (:steps prefix))))
            (is (= :assertion-only (get-in prefix [:steps 0 :stutter-operations])))
            (is (= :matched (:status (xtdb/check-recorded-stutter! handle good))))
            (with-redefs [source-log/observe-source-message! (fn [& _] {:status :unavailable :reason :source-message-not-retained})]
              (is (= :historical-stutter-envelope-unavailable
                     (get-in (history/audit! handle admission 8) [:steps 0 :reason]))))
            (let [hidden (:tx-id (xt/execute-tx node [[:put-docs :unmapped {:xt/id "outside" :value 1}]]))
                  extended (history/audit-chunk! handle admission 8 (:checkpoint prefix))]
              (is (= :unclassified-steps (:status extended)))
              (is (= :historical-stutter-contains-write (get-in extended [:first-unclassified :reason])))
              (is (= :stutter-contains-write (:reason (xtdb/check-recorded-stutter! handle hidden))))
              (is (= {:observed-mapped-stutter 1 :unclassified 1} (:counts extended)))
              (xt/execute-tx node [["ASSERT TRUE"]])
              (let [later (history/audit-chunk! handle admission 8 (:checkpoint extended))]
                (is (= {:observed-mapped-stutter 2 :unclassified 1} (:counts later)))
                (is (= (:first-unclassified extended) (:first-unclassified later))))))
          (finally (release)))))))

(deftest aborted-stutters-require-retained-restricted-attempt-and-outcome
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (let [handle (xtdb/adapter-connectable! node)
                aborted (xt/submit-tx node [[:put-docs :unmapped {:xt/id "outside" :value 1}]
                                           ["ASSERT FALSE"]])
                id (:tx-id aborted)
                _ (.awaitTxBlocking (db/primary-db node) id (java.time.Duration/ofSeconds 10))
                prefix (history/audit-chunk! handle admission 1)
                checked (xtdb/check-recorded-abort! handle id)]
            (is (= :observed-interval-checked (:status prefix)))
            (is (= [:aborted-stutter] (mapv :classification (:steps prefix))))
            (is (= :restricted-envelope (get-in prefix [:steps 0 :aborted-operations])))
            (is (= {:status :matched :profile envelope/profile :authority :none :certificate :not-issued
                    :tx-id id :committed? false :aborted-operations :restricted-envelope} checked))
            (is (empty? (xt/q node "SELECT * FROM unmapped")))
            (doseq [[observation expected] [[{:status :unavailable :reason :source-message-not-retained}
                                             :historical-abort-envelope-unavailable]
                                            [{:status :unsupported :reason :unsupported-operation-kind}
                                             :historical-abort-envelope-unsupported]]]
              (with-redefs [source-log/observe-source-message! (fn [& _] observation)]
                (is (= expected (get-in (history/audit! handle admission 8) [:steps 0 :reason])))))
            (let [committed (:tx-id (xt/execute-tx node [["ASSERT TRUE"]]))]
              (is (= :transaction-outcome-mismatch (:reason (xtdb/check-recorded-abort! handle committed)))))
            (let [unsupported (xt/submit-tx node [["ASSERT FALSE"] [:delete-docs :requests "r1"]])
                  _ (.awaitTxBlocking (db/primary-db node) (:tx-id unsupported) (java.time.Duration/ofSeconds 10))
                  extended (history/audit-chunk! handle admission 8 (:checkpoint prefix))]
              (is (= :unclassified-steps (:status extended)))
              (is (= :historical-abort-envelope-unsupported (get-in extended [:first-unclassified :reason])))
              (is (= {:aborted-stutter 1 :observed-mapped-stutter 1 :unclassified 1} (:counts extended)))
              (xt/execute-tx node [["ASSERT TRUE"]])
              (let [later (history/audit-chunk! handle admission 8 (:checkpoint extended))]
                (is (= (:first-unclassified extended) (:first-unclassified later))))))
          (finally (release)))))))

(deftest retained-initialization-envelope-required-at-activation
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        checked (inductive/check-mapped-initialization! admission seed)
        ops (init/transaction "s" seed #{:requests :assignments} checked)]
    (doseq [[mode submitted expected] [[:exact ops nil]
                                       [:unguarded (subvec ops (- (count ops) 2)) :initialization-envelope-mismatch]
                                       [:extra (conj ops [:put-docs :unselected {:xt/id "hidden" :value 1}]) :initialization-envelope-mismatch]
                                       [:duplicate (conj ops (first seed)) :initialization-envelope-mismatch]]]
      (with-open [node (xtn/start-node {})]
        (let [id (:tx-id (xt/execute-tx node submitted))
              writer (policy/build-policy! node config {:source-id "s"})
              release (xtdb/register-submission-policy! node writer)]
          (try
            (is (= expected (reason #(policy/activate-policy! writer))) (name mode))
            (let [observation (correspondence/initialization-observation node admission)]
              (if expected
                (do (is (nil? observation))
                    (is (= :missing-history-initialization (reason #(history/audit! node admission 8)))))
                (do (is (= :matched (:initialization-envelope observation)))
                    (is (= id (:initialization-tx-id observation)))
                    (is (= :observed-interval-checked (:status (history/audit! node admission 8)))))))
            (finally (release))))))))

(deftest retained-manifest-required-at-mapped-activation
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        scope {:source-id "s" :descriptor "profile"}
        ops [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" "s"]
             ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" "s"]
             [:put-docs :gesso_publication_manifests {:xt/id "s" :profile-version 1 :descriptor "profile"}]]]
    (doseq [[mode submitted expected] [[:exact ops nil]
                                       [:unguarded [(peek ops)] :manifest-envelope-mismatch]
                                       [:extra (conj ops [:put-docs :unselected {:xt/id "hidden" :value 1}]) :manifest-envelope-mismatch]
                                       [:duplicate (conj ops (peek ops)) :manifest-envelope-mismatch]
                                       [:missing nil :publication-manifest-mismatch]
                                       [:wrong-descriptor (assoc-in ops [2 2 :descriptor] "other") :publication-manifest-mismatch]
                                       [:after-initialization ops :manifest-initialization-root-mismatch]]]
      (with-open [node (xtn/start-node {})]
        (when (= :after-initialization mode)
          (policy/initialize! node "s" config))
        (let [id (when submitted (:tx-id (xt/execute-tx node submitted)))
              writer (policy/build-policy! node config scope)
              release (xtdb/register-submission-policy! node writer)]
          (try
            (is (= expected (reason #(policy/activate-policy! writer))) (name mode))
            (if expected
              (do (is (nil? (correspondence/initialization-observation node admission)))
                  (is (= :missing-history-initialization (reason #(history/audit! node admission 8))))
                  (when-not (= :after-initialization mode)
                    (is (empty? (xt/q node "SELECT * FROM gesso_initializations")))))
              (let [initial (:initialization (history/audit! node admission 8))]
                (is (= :matched (:manifest-envelope initial)))
                (is (= id (:manifest-tx-id initial)))
                (is (< id (:initialization-tx-id initial)))
                (is (not (contains? initial :manifest-row)))))
            (finally (release))))))))

(deftest aborted-temporal-worker-attempts-have-separate-restricted-profile
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        config {:admission admission :initializer seed :observe! identity}
        id (random-uuid)
        stamp (java.time.Instant/parse "2020-01-01T00:00:00Z")
        deadline (.plusSeconds stamp 60)
        cas "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)"
        reclaim "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP >= ?)"
        ack "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)"
        clock [["ASSERT CURRENT_TIMESTAMP < ?" deadline]
               ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]]
        put [:put-docs :gesso_live_outbox {:xt/id id :value 1}]
        acquire (conj (into [[cas id stamp]] clock) put)
        attempts [[:acquire acquire true]
                  [:reclaim (conj (into [[reclaim id stamp deadline]] clock) put) true]
                  [:ack [[ack id stamp deadline] put] true]
                  [:extra (conj acquire [:put-docs :unmapped {:xt/id "x"}]) false]
                  [:wrong-table (assoc-in acquire [3 1] :unmapped) false]
                  [:reordered (assoc acquire 1 (nth acquire 2) 2 (nth acquire 1)) false]
                  [:interval (assoc-in acquire [2 0] "ASSERT CURRENT_TIMESTAMP + INTERVAL '6' MINUTE >= ?") false]
                  [:assertion-only [["ASSERT CURRENT_TIMESTAMP < ?" deadline]] false]]]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config {:source-id "s"})
            release (xtdb/register-submission-policy! node writer)]
        (try
          (policy/activate-policy! writer)
          (doseq [[mode ops allowed?] attempts]
            (let [tx-id (:tx-id (xt/submit-tx node ops))
                  _ (.awaitTxBlocking (db/primary-db node) tx-id (java.time.Duration/ofSeconds 10))
                  result (xtdb/check-recorded-abort! node tx-id)
                  step (last (:steps (history/audit! node admission 16)))]
              (is (= [{:committed false}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" tx-id])) (name mode))
              (is (= (if allowed? :matched :unsupported) (:status result)) (name mode))
              (is (= (when allowed? :restricted-worker-envelope) (:aborted-operations result)))
              (is (= (if allowed? :aborted-stutter :unclassified) (:classification step)))
              (is (= (when allowed? :restricted-worker-envelope) (:aborted-operations step)))
              (is (= (when-not allowed? :historical-abort-envelope-unsupported) (:reason step)))
              (is (= :unsupported (:status (envelope/check-envelope! node tx-id ops false))))))
          (is (empty? (xt/q node "SELECT * FROM gesso_live_outbox")))
          (finally (release)))))))


(deftest worker-maintenance-requires-current-activated-generation
  (let [admission (admission!)
        seed [[:put-docs :requests {:xt/id "r1" :item-count 0}]]
        events (atom [])
        config {:admission admission :initializer seed :observe! #(swap! events conj (:kind %))}
        scope {:source-id "s" :worker-id "w"}
        id (random-uuid)
        pending {:xt/id id :gesso.live.outbox/type outbox/intent-type
                 :gesso.live.outbox/version outbox/intent-version
                 :gesso.live.outbox/status :pending :gesso.live.outbox/source-id "s"
                 :gesso.live.outbox/operation :example/update :gesso.live.outbox/purpose :gesso.live/refresh
                 :gesso.live.outbox/topic :example :gesso.live.outbox/subject-id "r1"
                 :gesso.live.outbox/command-id "c1"}
        claim (fn [] (let [now (java.time.Instant/now)]
                      {:intent-id id :source-id "s" :worker-id "w" :lease-id (random-uuid)
                       :now now :lease-until (.plusSeconds now 60)}))]
    (with-open [node (xtn/start-node {})]
      (let [writer (policy/build-policy! node config scope)
            release (xtdb/register-submission-policy! node writer)
            acquire! #(acquisition/acquire-pending! node (claim))
            read-row #(first (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" id]))]
        (try
          (policy/activate-policy! writer)
          ;; Fixture-only raw write: create existing work after initialization.
          (xt/execute-tx node [[:put-docs :gesso_live_outbox pending]])
          (finally (release)))
        (let [release (xtdb/register-submission-policy! node writer)]
          (try
            (is (= :submission-policy-refused (reason acquire!)))
            (is (= pending (read-row)))
            (is (= [:initializer] @events))
            (finally (release))))
        (let [fresh (policy/build-policy! node config scope)
              release (xtdb/register-submission-policy! node fresh)]
          (try
            (is (= :submission-policy-refused (reason acquire!)))
            (is (= pending (read-row)))
            (is (= [:initializer] @events))
            (policy/activate-policy! fresh)
            (is (= :acquired (:status (acquire!))))
            (is (= :leased (:gesso.live.outbox/status (read-row))))
            (is (= [:initializer :worker] @events))
            (finally (release))))))))
