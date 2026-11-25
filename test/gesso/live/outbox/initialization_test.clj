(ns gesso.live.outbox.initialization-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.initialization :as init]
            [gesso.live.outbox.writer-policy :as policy]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.inductive-plan-test]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- admission! []
  (let [id :example/initialize
        source {:subject {:kind :operation :id id :interface {:operation id}}
                :state-types {:example/count {:type :integer}}
                :input-types {} :guard {:readers #{} :expression [:literal true]}
                :reads #{} :writes #{:example/count}
                :replacements {:example/count [:literal 3]} :frame #{}}
        invariant {:readers #{:example/count}
                   :expression [:>= [:read :example/count] [:literal 0]]}
        catalog {:id :example/initialization :initial-condition invariant
                 :invariants {:example/nonnegative invariant}
                 :declared-operation-ids #{id} :operations [{:id id :construction source}]}]
    (inductive/build-inductive-mapped-admission!
     {:inductive-source ((deref (ns-resolve 'gesso.model.inductive-plan-test 'proof-package)) catalog)
      :target-map {:example/count [:document :requests "r1" :item_count]}
      :decode-outcome! identity
      :transaction-footprint! (fn [_] (throw (ex-info "Unexpected mapped operation" {})))
      :auxiliary-tables #{} :document-effects {id {[:document :requests "r1"] {}}}})))

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
        marker {:xt/id source :profile-version 1}
        observed (atom [])
        scope {:source-id source :worker-id "worker" :descriptor nil}
        admission (admission!)
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
            (is (false? (init/initialized? node source)))
            (is (= :submission-policy-refused (reason #(xtdb/execute-tx! node seed))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/execute-tx! node (pop (init/transaction source seed))))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/submit-tx! node (init/transaction source seed)))))
            (is (nil? (init/initialize! node source seed)))
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
            (is (true? (init/initialized? node source)))
            (is (nil? (init/initialize! node source changed)))
            (is (= :submission-policy-refused
                   (reason #(xtdb/execute-tx! node (init/transaction source changed)))))
            (is (= :submission-policy-refused
                   (reason #(xtdb/execute-tx! node (init/transaction source seed)))))
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
          (is (= :assert-failed (assert-result #(init/initialize! node source seed))))
          (is (= [{:xt/id "other" :item-count 42}] (read-requests node)))
          (is (empty? (xt/q node ["SELECT * FROM memberships"])))
          (is (empty? (read-marker node)))
          (finally (release))))
      (xt/execute-tx node [[:put-docs :gesso_initializations (assoc marker :profile-version 2)]])
      (is (= :invalid-initialization-marker (reason #(init/initialize! node source seed))))
      (is (= :invalid-mapped-writer-policy
             (reason #(policy/validate-config! {:admission admission :initializer [[:put-docs init/table marker]]
                                                :observe! identity})))))))
