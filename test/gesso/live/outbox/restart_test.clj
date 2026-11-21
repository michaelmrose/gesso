(ns gesso.live.outbox.restart-test
  "v2100-v2101: actual JVM process termination and XTDB restart.

   Phase 1 runs a separate JVM which commits an authoritative mutation and
   outbox records, acquires one lease, then calls Runtime.halt WITHOUT closing
   XTDB. Phase 2 starts an independent JVM on the same physical log/storage,
   discovers/reclaims work, emits Live invalidations and records acceptance.
   Phase 3 opens the same database in another JVM and confirms there is no
   replay of terminal records. This is a single-node durability test, not a
   claim of crash consistency for every environment or browser delivery."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]
           [java.util.concurrent TimeUnit]))

(def ^:private child-timeout-seconds 85)

(defn- child!
  [^Path root mode]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        classpath (System/getProperty "java.class.path")
        output (.resolve root (str mode ".edn"))
        log (.resolve root (str mode ".log"))
        command [java "-Xmx1024m"
                 "--add-opens=java.base/java.nio=ALL-UNNAMED"
                 "--enable-native-access=ALL-UNNAMED"
                 "-Dio.netty.tryReflectionSetAccessible=true"
                 "-Dio.netty.noUnsafe=false"
                 "-cp" classpath
                 "clojure.main" "-m" "gesso.live.outbox.restart-child"
                 mode (str root) (str output)]
        builder (ProcessBuilder. (into-array String command))]
    (.redirectErrorStream builder true)
    (.redirectOutput builder (.toFile log))
    (let [child (.start builder)
          finished? (.waitFor child child-timeout-seconds TimeUnit/SECONDS)
          _ (when-not finished?
              (.destroyForcibly child)
              (.waitFor child 5 TimeUnit/SECONDS))
          exit (if finished? (.exitValue child) :timed-out)
          report (when (Files/exists output (make-array java.nio.file.LinkOption 0))
                   (edn/read-string (slurp (.toFile output))))]
      {:exit exit
       :report report
       :log (when-not (= 0 exit)
              (let [text (slurp (.toFile log))]
                (subs text (max 0 (- (count text) 6000)))))})))

(defn- clean! [^Path root]
  (doseq [f (reverse (file-seq (.toFile root)))]
    (io/delete-file f true)))

(deftest abrupt-jvm-termination-retains-outbox-and-recovers-in-new-processes-test
  ;; Not a worker thread restart: the seed, recovery and verification nodes
  ;; occupy three different Java process IDs, and only one opens the local
  ;; XTDB files at a time. No process-global memory can cross this boundary.
  (let [root (Files/createTempDirectory
              "gesso-live-v2100-"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (let [seed (child! root "seed")]
        (is (= 0 (:exit seed)) (:log seed))
        (is (true? (get-in seed [:report :seeded])) (pr-str seed))
        (is (integer? (get-in seed [:report :pid])))
        (when (and (= 0 (:exit seed)) (:report seed))
          (let [recovery (child! root "recover")]
            (is (= 0 (:exit recovery)) (:log recovery))
            (is (true? (get-in recovery [:report :recovered]))
                (pr-str recovery))
            (is (integer? (get-in recovery [:report :pid])))
            (is (not= (get-in seed [:report :pid])
                      (get-in recovery [:report :pid])))
            (is (= [:pending :leased :pending]
                   (get-in recovery [:report :before-statuses])))
            (is (= [:dispatch-accepted :dispatch-accepted :pending]
                   (get-in recovery [:report :after-statuses])))
            (is (true? (get-in recovery [:report :all-have-progression?])))
            (is (= 2 (get-in recovery [:report :accepted-count])))
            (is (= 2 (count (get-in recovery [:report :events]))))
            (is (true? (get-in recovery [:report :foreign-remains-pending?])))
            (when (and (= 0 (:exit recovery)) (:report recovery))
              (let [verification (child! root "verify")]
                (is (= 0 (:exit verification)) (:log verification))
                (is (true? (get-in verification [:report :verified]))
                    (pr-str verification))
                (is (integer? (get-in verification [:report :pid])))
                (is (= 3 (count (set (map #(get-in % [:report :pid])
                                          [seed recovery verification])))))
                (is (= [:dispatch-accepted :dispatch-accepted :pending]
                       (get-in verification [:report :statuses])))
                (is (zero? (get-in verification [:report :resends] -1))))))))
      (finally (clean! root)))))


(deftest crash-after-source-acceptance-before-ack-replays-after-restart-test
  ;; First JVM confirms actual Live source acceptance then HALTS in the
  ;; acknowledgment boundary, leaving a durable leased intent. Second JVM
  ;; must re-emit after expiry and physically record acceptance; third JVM
  ;; must observe a terminal record without re-emission. Thus the same
  ;; logical invalidation was accepted twice, NOT delivered exactly once.
  (let [root (Files/createTempDirectory
              "gesso-live-v2101-"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (let [crash (child! root "post-dispatch-crash")]
        (is (= 0 (:exit crash)) (:log crash))
        (is (true? (get-in crash [:report :source-accepted-before-crash?]))
            (pr-str crash))
        (is (= :leased (get-in crash [:report :pre-ack-status])))
        (is (nil? (get-in crash [:report :receipt-before-crash])))
        (is (true? (get-in crash [:report :progression-present?])))
        (is (uuid? (get-in crash [:report :lease-id])))
        (when (and (= 0 (:exit crash)) (:report crash))
          (let [recovery (child! root "post-dispatch-recover")]
            (is (= 0 (:exit recovery)) (:log recovery))
            (is (true?
                 (get-in recovery [:report :recovered-after-source-acceptance?]))
                (pr-str recovery))
            (is (= :leased (get-in recovery [:report :before-status])))
            (is (= :dispatch-accepted
                   (get-in recovery [:report :after-status])))
            (is (= 1 (get-in recovery [:report :reclaimed-count])))
            (is (= 1 (get-in recovery [:report :accepted-count])))
            (is (true? (get-in recovery [:report :new-lease-token?])))
            (is (true? (get-in recovery [:report :receipt-recorded?])))
            (is (true? (get-in recovery [:report :progression-present?])))
            ;; Same logical refresh has been accepted by TWO distinct JVMs.
            ;; This is safe because invalidations demand authoritative refetch,
            ;; not execution of a second domain mutation.
            (is (= (get-in crash [:report :event])
                   (get-in recovery [:report :event])))
            (is (not= (get-in crash [:report :pid])
                      (get-in recovery [:report :pid])))
            (when (and (= 0 (:exit recovery)) (:report recovery))
              (let [verification (child! root "post-dispatch-verify")]
                (is (= 0 (:exit verification)) (:log verification))
                (is (true? (get-in verification
                                   [:report :terminal-state-survived?]))
                    (pr-str verification))
                (is (= :dispatch-accepted
                       (get-in verification [:report :status])))
                (is (zero? (get-in verification [:report :resends] -1)))
                (is (= 3
                       (count
                        (set (map #(get-in % [:report :pid])
                                  [crash recovery verification]))))))))))
      (finally (clean! root)))))
