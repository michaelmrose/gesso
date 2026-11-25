(ns gesso.live.outbox.initialization-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.initialization :as init]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.writer-policy :as policy]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.inductive-plan-test]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- admission!
  ([] (admission! :requests #{}))
  ([table auxiliary] (admission! table auxiliary :example/initialization :assignments))
  ([table auxiliary catalog-id nullable-table]
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
     {:inductive-source ((deref (ns-resolve 'gesso.model.inductive-plan-test 'proof-package)) catalog)
      :target-map {:example/count [:document table "r1" :item_count]
                   :example/assignee [:nullable-document nullable-table "a1" :helper]}
      :decode-outcome! identity
      :transaction-footprint! (fn [_] (throw (ex-info "Unexpected mapped operation" {})))
      :auxiliary-tables auxiliary :document-effects {id {[:document table "r1"] {}}}}))))

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
