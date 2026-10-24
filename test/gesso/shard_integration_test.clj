(ns gesso.shard-integration-test
  "Regression tests for the Gesso <-> Shard boundary.

  Checks the actual Babashka inventory and the actual bb.edn dispatcher without
  running the expensive JVM/Node/Chromium suite or requiring ZFS. A temporary
  PATH intercepts *only child commands* when testing bb.edn dispatch. The
  normal sequential JVM gate must remain runnable without Shard installed.

  These tests establish job-inventory / command correspondence, not that the
  gates produce equivalent outcomes under concurrent execution."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io PushbackReader]
           [java.nio.file Files LinkOption Path]
           [java.nio.file.attribute FileAttribute]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [java.util.concurrent TimeUnit]))

(def ^:private attrs (make-array FileAttribute 0))
(def ^:private no-follow (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- root []
  (.getCanonicalFile (io/file (System/getProperty "user.dir"))))

(defn- expected-source! [relative]
  (let [path (io/file (root) relative)]
    (when-not (.isFile path)
      (throw (ex-info "Run Gesso integration tests from the Gesso repository root"
                      {:expected (str path)})))
    path))

(defn- bb-executable []
  ;; Ordinarily installed on PATH. Use an absolute override only in a
  ;; controlled local test environment where PATH does not contain Babashka.
  (or (System/getenv "GESSO_SHARD_TEST_BB") "bb"))

(defn- delete-tree! [path]
  (when (Files/exists (.toPath (io/file path)) no-follow)
    (with-open [walk (Files/walk (.toPath (io/file path))
                                  (make-array java.nio.file.FileVisitOption 0))]
      (doseq [entry (reverse (sort-by #(.getNameCount ^Path %)
                                     (iterator-seq (.iterator walk))))]
        (Files/deleteIfExists entry)))))

(defn- with-temp-repo [f]
  (let [base (.toFile (Files/createTempDirectory "gesso-shard-adapter-" attrs))]
    (try (f base)
         (finally (delete-tree! base)))))

(defn- stop-tree! [^Process process]
  (with-open [descendants (.descendants (.toHandle process))]
    (doseq [handle (iterator-seq (.iterator descendants))]
      (when (.isAlive handle) (.destroyForcibly handle))))
  (when (.isAlive process) (.destroyForcibly process))
  (.waitFor process 5 TimeUnit/SECONDS))

(defn- exec!
  "Run a bounded command, capturing both streams without pipe deadlocks."
  [cwd argv env]
  (let [out (Files/createTempFile "gesso-shard-exec-" ".log" attrs)
        builder (ProcessBuilder. ^java.util.List (vec argv))]
    (.directory builder (io/file cwd))
    (.redirectErrorStream builder true)
    (.redirectOutput builder (ProcessBuilder$Redirect/to (.toFile out)))
    (doseq [[k v] env]
      (.put (.environment builder) (name k) (str v)))
    (try
      (let [p (.start builder)
            done? (.waitFor p 25000 TimeUnit/MILLISECONDS)]
        (when-not done? (stop-tree! p))
        {:exit (if done? (.exitValue p) ::timeout)
         :out (slurp (.toFile out))})
      (finally (Files/deleteIfExists out)))))

(defn- inventory! []
  (exec! (root)
         [(bb-executable) "script/gesso_test.clj" "inventory"]
         {}))

(defn- forms-in [relative]
  (with-open [rdr (PushbackReader. (io/reader (expected-source! relative)))]
    (binding [*read-eval* false]
      (loop [acc []]
        (let [form (read {:eof ::eof} rdr)]
          (if (= ::eof form) acc (recur (conj acc form))))))))

(defn- defined-function! [forms symbol]
  (or (first (filter #(and (seq? %)
                           (= 'defn (first %))
                           (= symbol (second %))) forms))
      (throw (ex-info "Required Gesso test-runner function missing"
                      {:symbol symbol}))))

(defn- nested-forms [form]
  (tree-seq coll? seq form))

(defn- declared-full-suite! [forms]
  (let [runner (defined-function! forms 'run-all!)
        calls (filter #(and (seq? %)
                            (= 'run-gates! (first %))
                            (vector? (second %)))
                      (nested-forms runner))]
    (when-not (= 1 (count calls))
      (throw (ex-info "Ambiguous run-all! gate definition"
                      {:calls calls})))
    (second (first calls))))

(defn- cli-full-suite! [forms]
  (let [fn-form (defined-function! forms 'command-gates)
        cases (filter #(and (seq? %)
                            (= 'case (first %))
                            (= 'command (second %)))
                      (nested-forms fn-form))]
    (when-not (= 1 (count cases))
      (throw (ex-info "Ambiguous CLI gate selection" {:cases cases})))
    (let [branches (vec (first cases))
          index (.indexOf branches "all")]
      (when-not (and (pos? index)
                     (< (inc index) (count branches))
                     (vector? (nth branches (inc index))))
        (throw (ex-info "CLI all selection is absent or nonliteral" {})))
      (into [:integrity] (nth branches (inc index))))))

(defn- command-stub! [bin name]
  (let [f (io/file bin name)]
    (spit f (str "#!/bin/sh\n"
                 "printf '%s\\n' '" name "' \"$@\" > \"$GESSO_SHARD_TEST_TRACE\"\n"
                 "exit \"${GESSO_SHARD_TEST_EXIT:-0}\"\n"))
    (when-not (.setExecutable f true false)
      (throw (ex-info "Cannot make fake command executable" {:file (str f)})))
    f))

(defn- prepare-dispatch-fixture! [temp]
  (let [bin (io/file temp "bin")]
    (.mkdirs bin)
    (io/copy (expected-source! "bb.edn") (io/file temp "bb.edn"))
    ;; The parent bb invocation is an absolute path when overridden and is
    ;; executed directly. Only its child shell commands resolve through PATH.
    (command-stub! bin "bb")
    (command-stub! bin "shard")
    {:root temp
     :bin bin
     :trace (io/file temp "invoked.txt")}))

(defn- dispatch! [fx args exit-code]
  (let [trace (:trace fx)]
    (Files/deleteIfExists (.toPath trace))
    (let [path (str (:bin fx) java.io.File/pathSeparator
                    (or (System/getenv "PATH") "/usr/bin:/bin"))
          result (exec! (:root fx)
                        (into [(bb-executable) "test"] args)
                        {"PATH" path
                         "GESSO_SHARD_TEST_TRACE" (.getPath trace)
                         "GESSO_SHARD_TEST_EXIT" exit-code})]
      (assoc result :invoked (when (.isFile trace)
                               (str/split-lines (slurp trace)))))))

(deftest shard-inventory-corresponds-to-full-sequential-gates
  (let [result (inventory!)
        forms (forms-in "script/gesso_test.clj")
        sequential (declared-full-suite! forms)
        cli (cli-full-suite! forms)]
    (is (= 0 (:exit result)) (:out result))
    (when (= 0 (:exit result))
      (let [inventory (edn/read-string (:out result))
            jobs (:jobs inventory)
            ids (mapv :id jobs)
            gates (mapv :gate jobs)]
        (is (= #{:jobs} (set (keys inventory))))
        (is (vector? jobs))
        (is (= 8 (count jobs))
            "Eight known full-suite gates must not silently disappear")
        (is (= sequential cli)
            "The actual full-suite and CLI 'all' declarations must agree")
        (is (= sequential gates)
            "Shard inventory must represent the exact sequential full-suite gates")
        (is (= :integrity (first gates)))
        (is (= (count ids) (count (set ids))))
        (is (= (count gates) (count (set gates))))
        (is (every? #(and (string? %) (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" %)) ids))
        (doseq [{:keys [id gate runtime unit command depends-on requires timeout-ms]} jobs]
          (is (= id (str "gesso-" (str/replace (name gate) ":" "-"))))
          (is (= runtime :gesso))
          (is (= unit {:type :gesso-gate :name (name gate)}))
          (is (= command ["bb" "script/gesso_test.clj" (name gate)]))
          (is (= depends-on []))
          (is (= requires #{}))
          (is (and (integer? timeout-ms) (pos? timeout-ms))))))))

(deftest shard-inventory-is-deterministic-and-does-not-execute-gates
  (let [first-run (inventory!)
        second-run (inventory!)]
    (is (= 0 (:exit first-run)) (:out first-run))
    (is (= 0 (:exit second-run)) (:out second-run))
    (when (and (= 0 (:exit first-run)) (= 0 (:exit second-run)))
      (is (= (:out first-run) (:out second-run)))
      (is (= (:jobs (edn/read-string (:out first-run)))
             (:jobs (edn/read-string (:out second-run))))))))

(deftest plain-bb-test-preserves-sequential-gate-command
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)
            result (dispatch! fx [] 0)]
        (is (= 0 (:exit result)) (:out result))
        (is (= ["bb" "script/gesso_test.clj" "all"] (:invoked result)))))))

(deftest parallel-bb-test-delegates-exact-worker-count
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [n [1 4 8 256]]
          (testing (str n " workers")
            (let [result (dispatch! fx ["-n" (str n)] 0)]
              (is (= 0 (:exit result)) (:out result))
              (is (= ["shard" "run" "--workers" (str n)] (:invoked result))))))))))

(deftest sequential-and-parallel-failures-propagate
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [[args called] [[[] ["bb" "script/gesso_test.clj" "all"]]
                               [["-n" "8"] ["shard" "run" "--workers" "8"]]]]
          (let [result (dispatch! fx args 23)]
            (is (not= 0 (:exit result)) (:out result))
            (is (= called (:invoked result)))))))))

(deftest invalid-parallel-arguments-never-invoke-a-worker
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [args [["-n" "0"] ["-n" "257"] ["-n" "abc"]
                      ["-n"] ["-n" "8" "extra"] ["--workers" "8"]]]
          (testing (pr-str args)
            (let [result (dispatch! fx args 0)]
              (is (not= 0 (:exit result)) (:out result))
              (is (nil? (:invoked result))
                  (str "Invalid invocation unexpectedly launched " (:invoked result))))))))))

(deftest inventory-cli-rejects-extra-arguments
  (let [result (exec! (root)
                      [(bb-executable) "script/gesso_test.clj" "inventory" "unexpected"]
                      {})]
    (is (= 2 (:exit result)) (:out result))))

(deftest optional-real-shard-inventory-adapter-agrees-with-gesso
  ;; Set GESSO_SHARD_TEST_SHARD to an installed executable to test both tools
  ;; together. Do not make the ordinary sequential JVM suite require Shard.
  (if-let [shard (System/getenv "GESSO_SHARD_TEST_SHARD")]
    (let [direct (inventory!)
          adapted (exec! (root) [shard "gesso-inventory" (str (root))] {})]
      (is (= 0 (:exit direct)) (:out direct))
      (is (= 0 (:exit adapted)) (:out adapted))
      (when (and (= 0 (:exit direct)) (= 0 (:exit adapted)))
        (is (= (edn/read-string (:out direct))
               (edn/read-string (:out adapted))))))
    (is true "Shard adapter is optional for ordinary sequential Gesso tests")))


;; v1085: A separate, opt-in JVM namespace inventory is published by Gesso.
;; This suite checks the correspondence between paths, declared namespaces,
;; Shard job descriptors and exact-namespace dispatch without actually running
;; XTDB/Recife/JVM tests. The normal eight-gate inventory stays unchanged.

(def ^:private declared-jvm-ns-pattern
  #"(?m)^[ \t]*\(ns[ \t\r\n]+([A-Za-z_][A-Za-z0-9_.-]*)")

(defn- jvm-inventory-in! [dir]
  (exec! dir [(bb-executable) "script/gesso_test.clj" "jvm:inventory"] {}))

(defn- declared-jvm-namespace-files! [dir]
  ;; Compute the expected namespace set independently of Gesso's enumerator.
  ;; This explicitly includes .cljc but excludes .cljs and helper sources.
  (let [base (.toPath (io/file dir "test"))]
    (with-open [walk (Files/walk base (make-array java.nio.file.FileVisitOption 0))]
      (->> (iterator-seq (.iterator walk))
           (filter #(Files/isRegularFile ^Path % no-follow))
           (filter #(re-find #"_test\.clj(?:c)?$" (str (.getFileName ^Path %))))
           (map (fn [^Path path]
                  (let [rel (str (.relativize base path))
                        name (-> rel
                                 (str/replace #"\.clj(?:c)?$" "")
                                 (str/replace #"[/\\]" ".")
                                 (str/replace "_" "-"))
                        actual (second (re-find declared-jvm-ns-pattern
                                                (slurp (.toFile path))))]
                    {:path rel :name name :declared actual})))
           (sort-by :path)
           vec))))

(defn- strict-jvm-inventory! [dir]
  (let [result (jvm-inventory-in! dir)]
    (when-not (zero? (:exit result))
      (throw (ex-info "JVM namespace inventory command failed" result)))
    (edn/read-string (:out result))))

(defn- write-jvm-test-file! [repo basename namespace-name]
  (let [f (io/file repo "test" "gesso" basename)]
    (.mkdirs (.getParentFile f))
    (spit f (str "(ns " namespace-name ")\n"))
    f))

(defn- prepare-jvm-dispatch-fixture! [temp]
  (let [script (io/file temp "script")
        bin (io/file temp "fake-bin")]
    (.mkdirs script)
    (.mkdirs bin)
    (spit (io/file temp "deps.edn") "{:paths [\"src\"]}\n")
    (io/copy (expected-source! "script/gesso_test.clj")
             (io/file script "gesso_test.clj"))
    (spit (io/file script "repo_integrity.fish") "# harmless fixture\n")
    (write-jvm-test-file! temp "sample_test.clj" "gesso.sample-test")
    (write-jvm-test-file! temp "portable_test.cljc" "gesso.portable-test")
    ;; The parent bb command is NOT intercepted. The project test runner's
    ;; fish + clojure children are controlled and logged independently.
    (command-stub! bin "fish")
    ;; Keep repository integrity successful so failures target the child JVM.
    (spit (io/file bin "fish")
          (str "#!/bin/sh" (System/lineSeparator)
               "exit 0" (System/lineSeparator)))
    (command-stub! bin "clojure")
    {:root temp
     :bin bin
     :trace (io/file temp "invoked.txt")}))

(defn- jvm-dispatch! [fixture args exit-code]
  (let [trace (:trace fixture)]
    (Files/deleteIfExists (.toPath trace))
    (let [env {"PATH" (str (:bin fixture) java.io.File/pathSeparator
                            (or (System/getenv "PATH") "/usr/bin:/bin"))
               "GESSO_SHARD_TEST_TRACE" (.getPath trace)
               "GESSO_SHARD_TEST_EXIT" (str exit-code)}
          result (exec! (:root fixture)
                        (into [(bb-executable) "script/gesso_test.clj"] args)
                        env)]
      (assoc result :invoked (when (.isFile trace)
                               (str/split-lines (slurp trace)))))))

(deftest jvm-namespace-inventory-has-exact-source-correspondence
  (let [files (declared-jvm-namespace-files! (root))
        result (jvm-inventory-in! (root))]
    (is (= 0 (:exit result)) (:out result))
    (is (seq files) "The repository must have JVM test namespaces")
    (is (= (count files) (count (set (map :path files)))))
    (doseq [{:keys [path name declared]} files]
      (is (= name declared)
          (str "Test file namespace/path mismatch: " path)))
    (when (= 0 (:exit result))
      (let [inventory (edn/read-string (:out result))
            jobs (:jobs inventory)
            source-names (mapv :name files)
            listed-names (mapv #(get-in % [:unit :name]) jobs)
            ids (mapv :id jobs)]
        (is (= #{:jobs} (set (keys inventory))))
        (is (vector? jobs))
        (is (= (count files) (count jobs)))
        (is (= (set source-names) (set listed-names)))
        (is (= listed-names (vec (sort listed-names))))
        (is (= (count ids) (count (set ids))))
        (is (= (count listed-names) (count (set listed-names))))
        (is (some #(str/ends-with? (:path %) ".cljc") files))
        (is (some #(str/ends-with? (:path %) ".clj") files))
        (doseq [{:keys [id gate runtime unit command depends-on timeout-ms requires]} jobs]
          (let [n (:name unit)]
            (is (= id (str "gesso-jvm-" n)))
            (is (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" id))
            (is (= :jvm gate))
            (is (= :gesso runtime))
            (is (= unit {:type :gesso-jvm-namespace :name n}))
            (is (= ["bb" "script/gesso_test.clj" "jvm:namespace" n] command))
            (is (= [] depends-on))
            (is (= #{} requires))
            (is (and (integer? timeout-ms) (pos? timeout-ms)))))))))

(deftest jvm-namespace-inventory-is-stable-and-opt-in
  (let [gate-before (inventory!)
        first-run (jvm-inventory-in! (root))
        second-run (jvm-inventory-in! (root))
        gate-after (inventory!)]
    (doseq [result [gate-before first-run second-run gate-after]]
      (is (= 0 (:exit result)) (:out result)))
    (when (every? #(zero? (:exit %))
                  [gate-before first-run second-run gate-after])
      (is (= (:out gate-before) (:out gate-after))
          "The authoritative parallel full-suite inventory must be unchanged")
      (is (= (:out first-run) (:out second-run)))
      (let [old (edn/read-string (:out gate-after))
            new (edn/read-string (:out first-run))]
        (is (= 8 (count (:jobs old))))
        (is (= 1 (count (filter #(= :jvm (:gate %)) (:jobs old)))))
        (is (= :gesso-gate (get-in (first (filter #(= :jvm (:gate %))
                                                    (:jobs old))) [:unit :type])))
        (is (every? #(= :gesso-jvm-namespace (get-in % [:unit :type]))
                    (:jobs new)))))))

(deftest namespace-selection-dispatches-exactly-one-registered-jvm-test
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)
            listed (strict-jvm-inventory! temp)]
        (is (= #{"gesso.sample-test" "gesso.portable-test"}
               (set (map #(get-in % [:unit :name]) (:jobs listed)))))
        (doseq [n ["gesso.sample-test" "gesso.portable-test"]]
          (let [r (jvm-dispatch! fixture ["jvm:namespace" n] 0)]
            (is (= 0 (:exit r)) (:out r))
            (is (= ["clojure" "-M:test:formal" "-n" n]
                   (:invoked r))
                "Every namespace must run exactly the requested -n selector")))))))

(deftest namespace-selection-rejects-unknown-and-malformed-names
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)]
        (doseq [name ["gesso.missing-test" "gesso.sample-test,other"
                      "(do (println :unsafe))" ""]]
          (let [r (jvm-dispatch! fixture ["jvm:namespace" name] 0)]
            (is (not= 0 (:exit r)) (:out r))
            (is (nil? (:invoked r))
                "Unknown namespace must not launch a child JVM")))))))

(deftest namespace-selection-propagates-jvm-process-failure
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)
            r (jvm-dispatch! fixture ["jvm:namespace" "gesso.sample-test"] 17)]
        (is (not= 0 (:exit r)) (:out r))
        (is (= ["clojure" "-M:test:formal" "-n" "gesso.sample-test"]
               (:invoked r)))))))

(deftest namespace-inventory-rejects-mismatched-declarations
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)
            file (io/file temp "test" "gesso" "sample_test.clj")]
        (spit file "(ns gesso.wrong-test)\n")
        (let [r (jvm-dispatch! fixture ["jvm:inventory"] 0)]
          (is (not= 0 (:exit r)) (:out r))
          (is (str/includes? (:out r) "disagrees with its path"))
          (is (nil? (:invoked r))))))))

(deftest namespace-inventory-ignores-cljs-and-non-test-helper-files
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)
            ignored (io/file temp "test" "gesso" "browser_test.cljs")
            helper (io/file temp "test" "gesso" "helper.clj")]
        (spit ignored "(ns gesso.browser-test)\n")
        (spit helper "(ns gesso.helper)\n")
        (let [result (jvm-dispatch! fixture ["jvm:inventory"] 0)]
          (is (= 0 (:exit result)) (:out result))
          (when (= 0 (:exit result))
            (is (= #{"gesso.sample-test" "gesso.portable-test"}
                   (set (map #(get-in % [:unit :name])
                             (:jobs (edn/read-string (:out result)))))))
            (is (nil? (:invoked result)))))))))

(deftest namespace-inventory-cli-enforces-arity-without-running-tests
  (with-temp-repo
    (fn [temp]
      (let [fixture (prepare-jvm-dispatch-fixture! temp)]
        (doseq [args [["jvm:inventory" "extra"]
                      ["jvm:namespace"]
                      ["jvm:namespace" "gesso.sample-test" "extra"]]]
          (let [r (jvm-dispatch! fixture args 0)]
            (is (= 2 (:exit r)) (:out r))
            (is (nil? (:invoked r)))))))))


(deftest optional-real-shard-accepts-fine-grained-jvm-manifest
  ;; This is deliberately optional: normal sequential Gesso tests must pass
  ;; without Ygg, Shard, ZFS or a separate integration installation.
  (if-let [shard (System/getenv "GESSO_SHARD_TEST_SHARD")]
    (with-temp-repo
      (fn [temp]
        (let [config (io/file temp "jvm-jobs.edn")
              run (io/file temp "run")
              config-data {:version 1
                           :project-root (str (root))
                           :runtimes {:gesso {:identity "gesso-jvm-test"
                                              :capabilities #{}}}
                           :inventory-command [(bb-executable)
                                               "script/gesso_test.clj"
                                               "jvm:inventory"]}
              direct (strict-jvm-inventory! (root))]
          (spit config (pr-str config-data))
          (let [result (exec! (root)
                              [shard "enumerate" (.getPath config) (.getPath run)]
                              {})]
            (is (= 0 (:exit result)) (:out result))
            (when (= 0 (:exit result))
              (let [manifest (edn/read-string
                              (slurp (io/file run "jobs.edn")))]
                (is (= :shard/jobs (:format manifest)))
                (is (= 1 (:version manifest)))
                (is (= (set (map :id (:jobs direct)))
                       (set (map :id (:jobs manifest)))))
                (is (= (count (:jobs direct)) (count (:jobs manifest))))))))))
    (is true "Fine-grained Shard manifest validation is optional")))
