(ns gesso.live.outbox.host-restart-test
  "v2107: the concrete canonical Gesso/Biff host survives three distinct JVMs.

   Unlike the earlier node-only restart fixture, each phase independently
   assembles a current Gesso application and starts the fixed host inventory
   with real persistent XTDB/Live providers and managed outbox lifecycle.
   The first host hard-halts AFTER source acceptance, BEFORE acknowledgment.
   The second reclaims and re-emits; the third verifies terminal durability.
   This demonstrates at-least-once-compatible source acceptance, NOT browser
   receipt, exactly-once execution, or arbitrary storage-failure tolerance."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files Path]
           [java.util.concurrent TimeUnit]))

(def ^:private child-timeout-seconds 100)

(defn- child! [^Path root phase]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        output (.resolve root (str phase ".edn"))
        log (.resolve root (str phase ".log"))
        cmd [java "-Xmx1024m"
             "--add-opens=java.base/java.nio=ALL-UNNAMED"
             "--enable-native-access=ALL-UNNAMED"
             "-Dio.netty.tryReflectionSetAccessible=true"
             "-Dio.netty.noUnsafe=false"
             "-cp" (System/getProperty "java.class.path")
             "clojure.main" "-m" "gesso.live.outbox.host-restart-child"
             phase (str root) (str output)]
        builder (ProcessBuilder. (into-array String cmd))]
    (.redirectErrorStream builder true)
    (.redirectOutput builder (.toFile log))
    (let [process (.start builder)
          finished? (.waitFor process child-timeout-seconds TimeUnit/SECONDS)
          _ (when-not finished?
              (.destroyForcibly process)
              (.waitFor process 5 TimeUnit/SECONDS))
          exit (if finished? (.exitValue process) :timed-out)
          report (when (Files/exists output
                                     (make-array java.nio.file.LinkOption 0))
                   (edn/read-string (slurp (.toFile output))))]
      {:exit exit
       :report report
       :log (when-not (= 0 exit)
              (let [contents (slurp (.toFile log))]
                (subs contents (max 0 (- (count contents) 6000)))))})))

(defn- cleanup! [^Path root]
  (doseq [entry (reverse (file-seq (.toFile root)))]
    (io/delete-file entry true)))

(deftest concrete-host-crash-reclaim-and-terminal-reopen-across-jvms-test
  (let [root (Files/createTempDirectory
              "gesso-v2107-host-restart-"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (let [crash (child! root "crash")]
        (is (= 0 (:exit crash)) (:log crash))
        (is (= :crash (get-in crash [:report :phase])))
        (is (true? (get-in crash [:report :source-accepted?])))
        (is (= :leased (get-in crash [:report :pre-ack-status])))
        (is (nil? (get-in crash [:report :pre-ack-receipt])))
        (is (uuid? (get-in crash [:report :lease-id])))
        (is (true? (get-in crash [:report :progression?])))
        (is (= :pending (get-in crash [:report :foreign-status])))
        (is (integer? (get-in crash [:report :pid])))
        (is (= 4 (count (get-in crash [:report :module-ids]))))
        (when (and (= 0 (:exit crash)) (:report crash))
          (let [recovery (child! root "recover")]
            (is (= 0 (:exit recovery)) (:log recovery))
            (is (= :recover (get-in recovery [:report :phase])))
            (is (true? (get-in recovery [:report :recovered?])))
            (is (= :leased (get-in recovery [:report :before-status])))
            (is (= :dispatch-accepted
                   (get-in recovery [:report :after-status])))
            (is (true? (get-in recovery [:report :reclaimed?])))
            (is (true? (get-in recovery [:report :new-lease?])))
            (is (true? (get-in recovery [:report :receipt?])))
            (is (true? (get-in recovery [:report :progression?])))
            (is (= :pending (get-in recovery [:report :foreign-status])))
            (is (= (get-in crash [:report :logical-event])
                   (get-in recovery [:report :logical-event])))
            (is (= (get-in crash [:report :module-ids])
                   (get-in recovery [:report :module-ids])))
            (is (not= (get-in crash [:report :pid])
                      (get-in recovery [:report :pid])))
            (when (and (= 0 (:exit recovery)) (:report recovery))
              (let [verification (child! root "verify")]
                (is (= 0 (:exit verification)) (:log verification))
                (is (= :verify (get-in verification [:report :phase])))
                (is (true? (get-in verification [:report :verified?])))
                (is (= :dispatch-accepted
                       (get-in verification [:report :status])))
                (is (= :pending
                       (get-in verification [:report :foreign-status])))
                (is (zero? (get-in verification [:report :resends] -1)))
                (is (= (get-in crash [:report :module-ids])
                       (get-in verification [:report :module-ids])))
                (is (= 3
                       (count (set (map #(get-in % [:report :pid])
                                        [crash recovery verification]))))))))))
      (finally (cleanup! root)))))
