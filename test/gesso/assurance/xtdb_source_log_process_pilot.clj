(ns gesso.assurance.xtdb-source-log-process-pilot
  "Two-process persistent source-log qualification. Run seed and verify
   in separate JVMs against the same initially empty directory. The
   manifest is test evidence, not a trusted production receipt."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :as t :refer [deftest is]]
            [gesso.assurance.xtdb-source-log-pilot :as log]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.lang.management ManagementFactory)
           (java.time Duration)
           (xtdb.database Database)))

(def ^:dynamic *root* nil)

(def ^:private doc
  {:xt/id "probe" :value :open
   :nested {:items [1 "open" :open true #uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"]}})

(def ^:private cases
  [{:kind :guarded :committed? true
    :ops [["ASSERT ? = ?" 1 1] ["ASSERT ? = ?" :open :open] [:put-docs :probe doc]]}
   {:kind :unguarded :committed? true :ops [[:put-docs :probe doc]]}
   {:kind :aborted :committed? false
    :ops [["ASSERT FALSE"] [:put-docs :probe (assoc doc :value :aborted)]]}])

(defn- process-identity []
  (let [runtime (ManagementFactory/getRuntimeMXBean)]
    {:pid (.getPid runtime) :started-ms (.getStartTime runtime)}))

(defn- config []
  {:log [:local {:path (str (io/file *root* "log"))}]
   :storage [:local {:path (str (io/file *root* "storage"))}]})

(defn- manifest [] (io/file *root* "manifest.edn"))

(defn- check! [node {:keys [ops committed?]} {:keys [tx-id observation]}]
  (is (= observation (log/observe-source-message! node tx-id)))
  (is (= {:status :checked-decoded-claim-envelope :tx-id tx-id
          :committed? committed? :operation-count (count ops)
          :authority :none :certificate :not-issued}
         (log/check-claim-envelope! node tx-id ops committed?))))

(deftest seed-process
  (let [root (io/file *root*)]
    (when-not (and (.isDirectory root) (empty? (.listFiles root)))
      (throw (ex-info "Seed requires an existing empty directory." {})))
    (let [entries
          (with-open [node (xtn/start-node (config))]
            (let [entries (mapv (fn [{:keys [kind ops committed?] :as scenario}]
                                  (let [tx (if committed? (xt/execute-tx node ops) (xt/submit-tx node ops))
                                        id (:tx-id tx)]
                                    (.awaitTxBlocking ^Database (db/primary-db node) id (Duration/ofSeconds 10))
                                    (let [entry {:kind kind :tx-id id
                                                 :observation (log/observe-source-message! node id)}]
                                      (check! node scenario entry)
                                      entry))) cases)]
              (is (= 3 (count (set (map :tx-id entries)))))
              (is (= [doc] (xt/q node "SELECT * FROM probe")))
              entries))]
      ;; Only serialized observations and identifiers cross the process
      ;; boundary. No live node, Arrow relation or cached provider object.
      (spit (manifest) (pr-str {:profile :local-log-process-pilot-v1
                                :process (process-identity) :entries entries}))
      (is (.isFile (manifest))))))

(deftest verify-process
  (let [{:keys [profile process entries]} (edn/read-string (slurp (manifest)))]
    (is (= :local-log-process-pilot-v1 profile))
    (is (not= process (process-identity)))
    (is (= (mapv :kind cases) (mapv :kind entries)))
    (is (= 3 (count (set (map :tx-id entries)))))
    (with-open [node (xtn/start-node (config))]
      (.awaitTxBlocking ^Database (db/primary-db node)
                        (apply max (map :tx-id entries)) (Duration/ofSeconds 10))
      (is (= [doc] (xt/q node "SELECT * FROM probe")))
      (doseq [[scenario entry] (map vector cases entries)]
        (check! node scenario entry))
      (let [later (assoc doc :value :later)
            ops [[:put-docs :probe later]]
            id (:tx-id (xt/execute-tx node ops))]
        (is (> id (apply max (map :tx-id entries))))
        (is (= :checked-decoded-claim-envelope
               (:status (log/check-claim-envelope! node id ops true))))
        (is (= [later] (xt/q node "SELECT * FROM probe")))
        (doseq [[scenario entry] (map vector cases entries)]
          (check! node scenario entry)))
      (is (= :unavailable (:status (log/observe-source-message!
                                    node (+ 1000000 (apply max (map :tx-id entries))))))))))

(defn -main [& [phase root :as args]]
  (when-not (and (= 2 (count args)) (#{"seed" "verify"} phase))
    (throw (ex-info "Usage: xtdb-source-log-process-pilot seed|verify DIRECTORY" {})))
  (let [counters (ref t/*initial-report-counters*)]
    (binding [*root* root t/*report-counters* counters]
      (t/test-vars [(case phase "seed" #'seed-process "verify" #'verify-process)]))
    (println phase @counters)
    (shutdown-agents)
    (System/exit (if (zero? (+ (:fail @counters) (:error @counters))) 0 1))))
