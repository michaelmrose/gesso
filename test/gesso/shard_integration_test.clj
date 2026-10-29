(ns gesso.shard-integration-test
  "Regression tests for the Gesso <-> Shard boundary.

  Checks the actual Babashka inventory and the actual bb.edn dispatcher without
  running the expensive JVM/Node/Chromium suite or requiring ZFS. A temporary
  PATH intercepts *only child commands* when testing bb.edn dispatch. The
  normal sequential JVM gate must remain runnable without Shard installed.

  Prepared JVM namespace jobs must declare persistent execution, while the
  six standalone gates remain subprocesses. Direct legacy inventories retain
  their subprocess compatibility contract. These tests establish job-inventory
  and command correspondence, not equivalent outcomes under concurrency or
  successful native Bubblewrap admission."
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

(defn- dispatch!
  ([fx args exit-code]
   (dispatch! fx "test" args exit-code))
  ([fx task args exit-code]
  (let [trace (:trace fx)]
    (Files/deleteIfExists (.toPath trace))
    (let [path (str (:bin fx) java.io.File/pathSeparator
                    (or (System/getenv "PATH") "/usr/bin:/bin"))
          result (exec! (:root fx)
                        (into [(bb-executable) task] args)
                        {"PATH" path
                         "GESSO_SHARD_TEST_TRACE" (.getPath trace)
                         "GESSO_SHARD_TEST_EXIT" exit-code})]
      (assoc result :invoked (when (.isFile trace)
                               (str/split-lines (slurp trace))))))))

(deftest shard-inventory-expands-only-jvm-in-declared-sequential-order
  (let [result (inventory!)
        jvm-result (exec! (root)
                          [(bb-executable) "script/gesso_test.clj" "jvm:inventory"]
                          {})
        forms (forms-in "script/gesso_test.clj")
        sequential (declared-full-suite! forms)
        cli (cli-full-suite! forms)]
    (is (= 0 (:exit result)) (:out result))
    (is (= 0 (:exit jvm-result)) (:out jvm-result))
    (is (= sequential cli)
        "Sequential all and CLI all must name the same authoritative gates")
    (is (= 1 (count (filter #{:jvm} sequential)))
        "The original JVM gate must have exactly one expansion point")
    (is (= (count sequential) (count (distinct sequential)))
        "The sequential suite must not repeat a gate")
    (when (and (zero? (:exit result)) (zero? (:exit jvm-result)))
      (let [inventory (edn/read-string (:out result))
            namespace-inventory (edn/read-string (:out jvm-result))
            jobs (:jobs inventory)
            jvm-jobs (:jobs namespace-inventory)
            non-jvm-gates (vec (remove #{:jvm} sequential))
            non-jvm-jobs (filterv #(not= :jvm (:gate %)) jobs)
            expected (vec (mapcat (fn [gate]
                                    (if (= :jvm gate)
                                      jvm-jobs
                                      [{:id (str "gesso-" (str/replace (name gate) ":" "-"))
                                        :gate gate
                                        :runtime :gesso
                                        :unit {:type :gesso-gate :name (name gate)}
                                        :command ["bb" "script/gesso_test.clj" (name gate)]
                                        :depends-on []
                                        :timeout-ms 3600000
                                        :requires #{}}]))
                                  sequential))
            ids (mapv :id jobs)]
        (is (= #{:jobs} (set (keys inventory))))
        (is (= #{:jobs} (set (keys namespace-inventory))))
        (is (vector? jobs))
        (is (seq jvm-jobs) "The JVM namespace expansion must be nonempty")
        (is (= (+ (count non-jvm-gates) (count jvm-jobs))
               (count jobs))
            "Replacing the JVM gate must neither omit nor add other work")
        (is (= expected jobs)
            "Expanded parallel jobs must exactly match sequential gate order")
        (is (= non-jvm-gates (mapv :gate non-jvm-jobs)))
        (is (= jvm-jobs (filterv #(= :jvm (:gate %)) jobs))
            "The full-suite JVM jobs must exactly equal jvm:inventory")
        (is (= (count ids) (count (set ids))))
        (is (every? #(and (string? %)
                         (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]*" %))
                    ids))
        (is (= (set sequential) (set (map :gate jobs))))
        (is (not-any? #(= :gesso-gate (get-in % [:unit :type]))
                      (filter #(= :jvm (:gate %)) jobs))
            "The coarse JVM gate must not execute as well as its namespaces")
        (doseq [{:keys [runtime command depends-on timeout-ms requires]} jobs]
          (is (= :gesso runtime))
          (is (vector? command))
          (is (= [] depends-on))
          (is (= #{} requires))
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


;; v1087: The JVM namespace inventory is an active expansion of the full
;; parallel suite, not an opt-in substitute. These tests independently check
;; source paths, declared namespaces, job descriptors and exact dispatch while
;; preserving all seven other gates. They do not run XTDB/Recife/JVM tests.
;; Repository integrity remains a gate in v1086; a later revision will move it
;; to Shard's exactly-once invocation-level preflight barrier.

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
    ;; The real v1094 namespace runner requires Cognitect/clojure.test execution
    ;; evidence; a bare exit 0 is deliberately insufficient. All variants below
    ;; are deterministic stdout fixtures, not assertions about JVM internals.
    (spit (io/file bin "clojure")
          (str "#!/bin/sh\n"
               "printf '%s\\n' 'clojure' \"$@\" > \"$GESSO_SHARD_TEST_TRACE\"\n"
               "case \"${GESSO_SHARD_JVM_EVIDENCE_CASE:-valid}\" in\n"
               "  valid) printf 'Testing %s\\n\\nRan 3 tests containing 6 assertions.\\n0 failures, 0 errors.\\n' \"$3\" ;;\n"
               "  timed) sleep 0.12; printf 'Testing %s\n' \"$3\"; sleep 0.18; printf 'Ran 3 tests containing 6 assertions.\n0 failures, 0 errors.\n'; sleep 0.12 ;;\n"
               "  empty) printf 'Testing %s\\n\\nRan 0 tests containing 0 assertions.\\n0 failures, 0 errors.\\n' \"$3\" ;;\n"
               "  no-assertions) printf 'Testing %s\\n\\nRan 1 test containing 0 assertions.\\n0 failures, 0 errors.\\n' \"$3\" ;;\n"
               "  wrong-namespace) printf 'Testing gesso.other-test\\n\\nRan 3 tests containing 6 assertions.\\n0 failures, 0 errors.\\n' ;;\n"
               "  extra-namespace) printf 'Testing %s\\nTesting gesso.other-test\\nRan 3 tests containing 6 assertions.\\n0 failures, 0 errors.\\n' \"$3\" ;;\n"
               "  missing-testing) printf 'Ran 3 tests containing 6 assertions.\\n0 failures, 0 errors.\\n' ;;\n"
               "  no-summary) printf 'Testing %s\\n' \"$3\" ;;\n"
               "  missing-outcome) printf 'Testing %s\\nRan 3 tests containing 6 assertions.\\n' \"$3\" ;;\n"
               "  reported-failures) printf 'Testing %s\\nRan 3 tests containing 6 assertions.\\n1 failures, 0 errors.\\n' \"$3\" ;;\n"
               "  reported-errors) printf 'Testing %s\\nRan 3 tests containing 6 assertions.\\n0 failures, 1 errors.\\n' \"$3\" ;;\n"
               "  mismatched-counts) printf 'Testing %s\\nRan 3 tests containing 6 assertions.\\n0 failures, 0 errors.\\nRan 1 test containing 2 assertions.\\n' \"$3\" ;;\n"
               "  no-output) : ;;\n"
               "  *) echo 'Unrecognized JVM fixture scenario' >&2; exit 90 ;;\n"
               "esac\n"
               "exit \"${GESSO_SHARD_TEST_EXIT:-0}\"\n"))
    {:root temp
     :bin bin
     :trace (io/file temp "invoked.txt")}))

(defn- jvm-dispatch!
  ([fixture args exit-code] (jvm-dispatch! fixture args exit-code "valid"))
  ([fixture args exit-code evidence-case]
  (let [trace (:trace fixture)]
    (Files/deleteIfExists (.toPath trace))
    (let [env {"PATH" (str (:bin fixture) java.io.File/pathSeparator
                            (or (System/getenv "PATH") "/usr/bin:/bin"))
               "GESSO_SHARD_TEST_TRACE" (.getPath trace)
               "GESSO_SHARD_TEST_EXIT" (str exit-code)
               "GESSO_SHARD_JVM_EVIDENCE_CASE" evidence-case}
          result (exec! (:root fixture)
                        (into [(bb-executable) "script/gesso_test.clj"] args)
                        env)]
      (assoc result :invoked (when (.isFile trace)
                               (str/split-lines (slurp trace))))))))

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

(deftest jvm-namespace-inventory-is-stable-and-active-in-full-suite
  (let [full-before (inventory!)
        jvm-before (jvm-inventory-in! (root))
        jvm-after (jvm-inventory-in! (root))
        full-after (inventory!)]
    (doseq [result [full-before jvm-before jvm-after full-after]]
      (is (= 0 (:exit result)) (:out result)))
    (when (every? #(zero? (:exit %))
                  [full-before jvm-before jvm-after full-after])
      (is (= (:out full-before) (:out full-after))
          "The complete manifest must be deterministic")
      (is (= (:out jvm-before) (:out jvm-after))
          "The namespace inventory must be deterministic")
      (let [full (edn/read-string (:out full-after))
            jvm (edn/read-string (:out jvm-before))
            jobs (:jobs full)
            jvm-jobs (:jobs jvm)
            non-jvm (remove #(= :jvm (:gate %)) jobs)
            expected-gates (vec (remove #{:jvm}
                                        (declared-full-suite!
                                         (forms-in "script/gesso_test.clj"))))]
        (is (seq jvm-jobs))
        (is (= jvm-jobs (filterv #(= :jvm (:gate %)) jobs)))
        (is (= expected-gates (mapv :gate non-jvm)))
        (is (= (+ (count expected-gates) (count jvm-jobs))
               (count jobs)))
        (is (every? #(= :gesso-jvm-namespace (get-in % [:unit :type]))
                    jvm-jobs))
        (is (= (set (map :id jvm-jobs))
               (set (map :id (filter #(= :jvm (:gate %)) jobs)))))))))

(defn- jvm-evidence!
  "Return the single machine-readable JVM record, or nil if absent.
  Never mistake a child-process success exit for execution evidence."
  [result]
  (let [records (->> (str/split-lines (:out result))
                     (filter #(str/starts-with? % "{:gesso/jvm-namespace-evidence "))
                     vec)]
    (when (= 1 (count records))
      (edn/read-string (first records)))))

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
            (is (= {:gesso/jvm-namespace-evidence 1
                    :namespace n :tests 3 :assertions 6
                    :failures 0 :errors 0}
                   (jvm-evidence! r))
                "Success must include verifiable namespace execution evidence")
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


;; v1092: The selected scopes are projections of Gesso's authoritative suite,
;; with repository integrity moved outside the basket into Shard's single
;; invocation-level preflight. These checks exercise the actual BB commands;
;; they do not create ZFS clones or run expensive production tests.

(def ^:private selected-test-tasks
  {"test" "all"
   "test:jvm" "jvm"
   "test:formal" "formal"
   "test:cljs" "cljs"
   "test:node" "node"
   "test:node:choreo" "node:choreo"
   "test:node:browser" "node:browser"
   "test:browser" "browser"
   "test:browser:integration" "browser:integration"
   "test:advanced" "advanced"
   "test:themes" "themes"})

(def ^:private expected-prepared-gates
  {"all" [:advanced :jvm :node:choreo :node:browser :browser
          :browser:integration :themes]
   "jvm" [:jvm]
   "formal" [:formal]
   "cljs" [:advanced :node:choreo :node:browser :browser
           :browser:integration]
   "node" [:node:choreo :node:browser]
   "node:choreo" [:node:choreo]
   "node:browser" [:node:browser]
   "browser" [:browser]
   ;; :advanced must run IN the same clone as browser integration; the
   ;; integration job itself handles that prerequisite.
   "browser:integration" [:browser:integration]
   "advanced" [:advanced]
   "themes" [:themes]})

(defn- prepared-inventory! [scope]
  (exec! (root)
         [(bb-executable) "script/gesso_test.clj" "inventory:prepared" scope]
         {}))

(deftest scoped-bb-tasks-preserve-sequential-and-parallel-commands
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [[task scope] (sort-by key selected-test-tasks)]
          (testing (str task " sequential")
            (let [r (dispatch! fx task [] 0)]
              (is (= 0 (:exit r)) (:out r))
              (is (= ["bb" "script/gesso_test.clj" scope]
                     (:invoked r)))))
          (testing (str task " parallel")
            (doseq [n [1 4 8 256]]
              (let [r (dispatch! fx task ["-n" (str n)] 0)
                    expected (cond-> ["shard" "run" "--workers" (str n)]
                               (not= "all" scope)
                               (into ["--scope" scope]))]
                (is (= 0 (:exit r)) (:out r))
                (is (= expected (:invoked r)))))))))))

(deftest scoped-bb-tasks-fail-closed-for-invalid-worker-arguments
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [task (sort (keys selected-test-tasks))
                args [["-n" "0"] ["-n" "257"] ["-n" "abc"]
                      ["-n"] ["-n" "4" "extra"] ["--workers" "8"]]]
          (let [r (dispatch! fx task args 0)]
            (is (not= 0 (:exit r)) (str task " " args " " (:out r)))
            (is (nil? (:invoked r))
                (str "Unexpected child command: " task " " args))))))))

(deftest scoped-bb-tasks-propagate-command-failures
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-dispatch-fixture! temp)]
        (doseq [[task scope] (sort-by key selected-test-tasks)
                args [[] ["-n" "3"]]]
          (let [r (dispatch! fx task args 27)
                expected (if (seq args)
                           (cond-> ["shard" "run" "--workers" "3"]
                             (not= "all" scope)
                             (into ["--scope" scope]))
                           ["bb" "script/gesso_test.clj" scope])]
            (is (not= 0 (:exit r)) (str task " " args))
            (is (= expected (:invoked r)))))))))

(deftest every-prepared-inventory-exactly-represents-its-selected-scope
  (let [namespaces (->> (jvm-inventory-in! (root))
                        :out edn/read-string :jobs
                        (mapv #(get-in % [:unit :name])))
        all (prepared-inventory! "all")]
    (is (= 0 (:exit all)) (:out all))
    (is (seq namespaces))
    (doseq [[scope gates] (sort-by key expected-prepared-gates)]
      (testing scope
        (let [result (prepared-inventory! scope)]
          (is (= 0 (:exit result)) (:out result))
          (when (= 0 (:exit result))
            (let [{:keys [jobs runtimes]} (edn/read-string (:out result))
                  actual (mapv :gate jobs)
                  expected (vec (mapcat (fn [gate]
                                          (if (= gate :jvm)
                                            (repeat (count namespaces) :jvm)
                                            (if (= gate :formal)
                                              (repeat (count jobs) :formal)
                                              [gate])))
                                        gates))
                  ids (mapv :id jobs)]
              (is (seq jobs))
              (is (= expected actual)
                  (str "Unexpected gate expansion for " scope))
              (is (= (count ids) (count (distinct ids))))
              (is (not-any? #(= :integrity (:gate %)) jobs))
              (doseq [{:keys [command runtime depends-on requires unit gate]} jobs]
                (let [jvm? (contains? #{:jvm :formal} gate)
                      expected-runtime (if jvm? :gesso-jvm :gesso)]
                  (is (= expected-runtime runtime)
                      "Prepared JVM namespaces must not fall back to per-job subprocesses")
                  (is (= (if jvm? :persistent-jvm :subprocess)
                         (get-in runtimes [runtime :execution])))
                  (is (= [] depends-on))
                  (is (= #{} requires))
                  (is (= 4 (count command)))
                  (is (= ["bb" "script/gesso_test.clj"] (subvec command 0 2)))
                  (is (= (if jvm?
                           "jvm:namespace:prepared"
                           "gate:prepared")
                         (nth command 2)))
                  (is (= (if jvm? :gesso-jvm-namespace :gesso-gate)
                         (:type unit))))))))))))

;; v1124: Prepared parallel jobs have an execution CONTRACT, not merely
;; a different runtime keyword. The legacy direct `inventory` and
;; `jvm:inventory` intentionally retain their historical subprocess schemas.
;; Assert the complete descriptor, selected-scope projection and fail-closed
;; generation boundary. Neither these tests nor the legacy fallback command
;; are evidence that native Bubblewrap admission or classpath provisioning works.
(def ^:private expected-prepared-runtime-descriptors
  {:gesso-jvm {:identity "gesso-clojure-1.12.5-test-formal-shard-protocol-v1"
               :capabilities #{}
               :execution :persistent-jvm
               :session-command ["clojure" "-M:test:formal" "-m" "shard.worker.jvm"]}
   :gesso {:identity "gesso-standalone-prepared-gates-v1"
           :capabilities #{}
           :execution :subprocess}})

(defn- prepared-projection! [scope]
  (let [r (prepared-inventory! scope)]
    (when-not (zero? (:exit r))
      (throw (ex-info "Prepared inventory failed" {:scope scope :result r})))
    (edn/read-string (:out r))))

(deftest prepared-runtime-descriptors-are-exact-and-scope-specific
  ;; A subprocess-only scope must never request a JVM, and the inverse also
  ;; holds. Mixed scope must retain both executors, not coerce one into the
  ;; other. No platform-dependent claim about whether the binary can start.
  (doseq [scope (sort (keys expected-prepared-gates))]
    (testing scope
      (let [{:keys [jobs runtimes]} (prepared-projection! scope)
            used (set (map :runtime jobs))
            jvm-jobs (filterv #(contains? #{:jvm :formal} (:gate %)) jobs)
            other-jobs (filterv #(not (contains? #{:jvm :formal} (:gate %))) jobs)]
        (is (= used (set (keys runtimes)))
            "No unused or undeclared prepared runtime")
        (is (= (select-keys expected-prepared-runtime-descriptors used) runtimes)
            "Exact persistent JVM entry point and subprocess contract")
        (is (every? #(= :gesso-jvm (:runtime %)) jvm-jobs))
        (is (every? #(= :gesso (:runtime %)) other-jobs))
        (is (every? #(= :gesso-jvm-namespace (get-in % [:unit :type])) jvm-jobs))
        (is (every? #(= :gesso-gate (get-in % [:unit :type])) other-jobs))
        (is (every? #(= "jvm:namespace:prepared" (get-in % [:command 2])) jvm-jobs))
        (is (every? #(= "gate:prepared" (get-in % [:command 2])) other-jobs))
        (is (= (count jobs) (+ (count jvm-jobs) (count other-jobs))))))))

(deftest prepared-full-inventory-preserves-exact-legacy-work
  ;; Compare against an *independently enumerated* legacy inventory. The
  ;; changed runtime and prepared command are the only permitted job fields.
  ;; This detects lost jobs, duplicated IDs, changed timeouts and coverage
  ;; drift across advanced, JVM, Node, Chromium and theme gates.
  (let [legacy-result (inventory!)
        prepared-result (prepared-inventory! "all")]
    (is (= 0 (:exit legacy-result)) (:out legacy-result))
    (is (= 0 (:exit prepared-result)) (:out prepared-result))
    (when (and (zero? (:exit legacy-result))
               (zero? (:exit prepared-result)))
      (let [legacy (->> legacy-result :out edn/read-string :jobs
                        (remove #(= :integrity (:gate %))) vec)
            {:keys [jobs runtimes]} (edn/read-string (:out prepared-result))
            stable-fields [:id :gate :unit :depends-on :timeout-ms :requires]
            keep-fields #(select-keys % stable-fields)
            jvm-jobs (filterv #(= :jvm (:gate %)) jobs)
            others (filterv #(not= :jvm (:gate %)) jobs)]
        (is (= #{:gesso :gesso-jvm} (set (keys runtimes))))
        (is (= (mapv keep-fields legacy) (mapv keep-fields jobs))
            "Only runtime and prepared command may differ from the original inventory")
        (is (= (mapv :id legacy) (mapv :id jobs)))
        (is (= (count jobs) (count (set (map :id jobs)))))
        (is (= (mapv #(get-in % [:unit :name]) jvm-jobs)
               (mapv #(get-in % [:unit :name])
                     (filterv #(= :jvm (:gate %)) legacy))))
        (is (= 6 (count others))
            "All six non-JVM gates must remain separate subprocess jobs")
        (doseq [[a b] (map vector legacy jobs)]
          (let [jvm? (= :jvm (:gate b))]
            (is (= (vec (take 2 (:command a)))
                   (vec (take 2 (:command b)))))
            (is (= (if jvm? "jvm:namespace:prepared" "gate:prepared")
                   (nth (:command b) 2)))
            (is (= (last (:command a)) (last (:command b))))
            (is (= (if jvm? :gesso-jvm :gesso) (:runtime b)))))))))

(defn- corrupt-prepared-runner! [temp from to]
  (let [path (io/file temp "script" "gesso_test.clj")
        body (slurp path)]
    (when-not (str/includes? body from)
      (throw (ex-info "Expected prepared code to mutate is absent" {:from from})))
    (spit path (str/replace body from to))))

(deftest prepared-inventory-rejects-subprocess-routed-jvm-namespace
  ;; Exercise the REAL runner's admission condition by mutating a disposable
  ;; script, not a copy of the validation predicate inside this test.
  (with-temp-repo
    (fn [temp]
      (prepare-jvm-dispatch-fixture! temp)
      (corrupt-prepared-runner! temp
                                ":runtime prepared-jvm-runtime"
                                ":runtime prepared-subprocess-runtime")
      (let [r (exec! temp [(bb-executable) "script/gesso_test.clj"
                           "inventory:prepared" "jvm"] {})]
        (is (not= 0 (:exit r)) (:out r))
        (is (str/includes? (:out r) "Invalid preflight-ready inventory")
            (:out r))
        (is (not (str/includes? (:out r) ":jobs ["))
            "No corrupted manifest may be emitted as a successful inventory")))))

(deftest prepared-inventory-rejects-persistent-routed-standalone-gate
  (with-temp-repo
    (fn [temp]
      (prepare-jvm-dispatch-fixture! temp)
      (corrupt-prepared-runner!
       temp
       "(assoc (shard-gate-job gate)\n         :command"
       "(assoc (shard-gate-job gate)\n         :runtime prepared-jvm-runtime\n         :command")
      (let [r (exec! temp [(bb-executable) "script/gesso_test.clj"
                           "inventory:prepared" "themes"] {})]
        (is (not= 0 (:exit r)) (:out r))
        (is (str/includes? (:out r) "Invalid preflight-ready inventory")
            (:out r))
        (is (not (str/includes? (:out r) ":jobs ["))
            "A standalone gate may not masquerade as a JVM namespace")))))

(deftest prepared-jvm-scope-has-one-job-per-authoritative-namespace
  (let [normal (jvm-inventory-in! (root))
        prepared (prepared-inventory! "jvm")]
    (is (= 0 (:exit normal)) (:out normal))
    (is (= 0 (:exit prepared)) (:out prepared))
    (when (and (zero? (:exit normal)) (zero? (:exit prepared)))
      (let [regular (:jobs (edn/read-string (:out normal)))
            selected (:jobs (edn/read-string (:out prepared)))]
        (is (= (count regular) (count selected)))
        (is (= (mapv #(get-in % [:unit :name]) regular)
               (mapv #(get-in % [:unit :name]) selected)))
        (is (= (mapv :id regular) (mapv :id selected)))
        (is (= (mapv #(subvec (:command %) 0 2) regular)
               (mapv #(subvec (:command %) 0 2) selected)))
        (is (every? #(= "jvm:namespace:prepared" (nth (:command %) 2))
                    selected))))))

(deftest prepared-formal-scope-is-a-subset-of-jvm-with-no-duplicates
  (let [jvm (prepared-inventory! "jvm")
        formal (prepared-inventory! "formal")]
    (is (= 0 (:exit jvm)) (:out jvm))
    (is (= 0 (:exit formal)) (:out formal))
    (when (and (zero? (:exit jvm)) (zero? (:exit formal)))
      (let [jvm-names (set (map #(get-in % [:unit :name])
                                (:jobs (edn/read-string (:out jvm)))))
            selected (:jobs (edn/read-string (:out formal)))
            names (mapv #(get-in % [:unit :name]) selected)]
        (is (seq names))
        (is (= (count names) (count (set names))))
        (is (every? jvm-names names))
        (is (every? #(= :formal (:gate %)) selected))))))

(deftest prepared-inventory-commands-are-deterministic-and-reject-invalid-scopes
  (doseq [scope ["all" "jvm" "formal" "browser:integration"]]
    (let [before (prepared-inventory! scope)
          after (prepared-inventory! scope)]
      (is (= 0 (:exit before)) (:out before))
      (is (= 0 (:exit after)) (:out after))
      (is (= (:out before) (:out after)))))
  (doseq [args [["inventory:prepared"]
                ["inventory:prepared" "unknown"]
                ["inventory:prepared" "all" "extra"]
                ["inventory" "unknown"]
                ["inventory" "jvm" "extra"]]]
    (let [r (exec! (root) (into [(bb-executable) "script/gesso_test.clj"] args) {})]
      (is (= 2 (:exit r)) (str args " " (:out r))))))

(defn- git-head-in! [directory]
  (let [r (exec! directory ["git" "rev-parse" "HEAD"] {})]
    (when-not (= 0 (:exit r))
      (throw (ex-info "Git HEAD fixture is missing" r)))
    (str/trim (:out r))))

(defn- init-fixture-git! [directory]
  (doseq [cmd [["git" "init" "-q"]
               ["git" "add" "."]
               ["git" "-c" "user.name=Gesso Test"
                "-c" "user.email=gesso-test@example.invalid"
                "commit" "-qm" "Fixture"]]]
    (let [r (exec! directory cmd {})]
      (when-not (= 0 (:exit r))
        (throw (ex-info "Cannot initialize git test fixture" r)))))
  (git-head-in! directory))

(deftest prepared-jvm-command-requires-current-commit-and-never-repeats-integrity
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)
            head (init-fixture-git! temp)
            fish (io/file (:bin fx) "fish")
            integrity-log (io/file temp "integrity-invocations.txt")
            name "gesso.sample-test"
            env-base {"PATH" (str (:bin fx) java.io.File/pathSeparator
                                   (or (System/getenv "PATH") "/usr/bin:/bin"))
                      "GESSO_SHARD_TEST_TRACE" (str (:trace fx))
                      "GESSO_SHARD_INTEGRITY_LOG" (str integrity-log)
                      "GESSO_SHARD_TEST_EXIT" "0"
                      "GESSO_SHARD_JVM_EVIDENCE_CASE" "valid"}
            run (fn [args environment]
                  (Files/deleteIfExists (.toPath (:trace fx)))
                  (let [r (exec! temp
                                 (into [(bb-executable) "script/gesso_test.clj"] args)
                                 (merge env-base environment))]
                    (assoc r :invoked (when (.isFile (:trace fx))
                                        (str/split-lines (slurp (:trace fx)))))))]
        (spit fish "#!/bin/sh\nprintf 'fish\n' > \"$GESSO_SHARD_TEST_TRACE\"\nprintf 'integrity\n' >> \"$GESSO_SHARD_INTEGRITY_LOG\"\nexit 0\n")
        (.setExecutable fish true false)
        (let [preflight (run ["preflight"] {})
              missing (run ["jvm:namespace:prepared" name]
                           {"GESSO_SHARD_PREFLIGHT_COMMIT" ""})
              wrong (run ["jvm:namespace:prepared" name]
                         {"GESSO_SHARD_PREFLIGHT_COMMIT" (apply str (repeat 40 "0"))})
              allowed (run ["jvm:namespace:prepared" name]
                           {"GESSO_SHARD_PREFLIGHT_COMMIT" head})]
          (is (= 0 (:exit preflight)) (:out preflight))
          (is (= ["fish"] (:invoked preflight)))
          (is (not= 0 (:exit missing)))
          (is (nil? (:invoked missing)))
          (is (not= 0 (:exit wrong)))
          (is (nil? (:invoked wrong)))
          (is (= 0 (:exit allowed)) (:out allowed))
          (is (= {:gesso/jvm-namespace-evidence 1
                  :namespace name :tests 3 :assertions 6
                  :failures 0 :errors 0}
                 (jvm-evidence! allowed)))
          (is (= ["clojure" "-M:test:formal" "-n" name]
                 (:invoked allowed))
              "One admitted JVM job must not invoke fish/repository integrity")
          (is (= ["integrity"] (str/split-lines (slurp integrity-log)))
              "Preflight must execute once across admission and test execution"))))))


;; v1095: v1094 requires positive, namespace-specific Cognitect/clojure.test
;; evidence, not merely a successful exit code. Keep these cases permanent;
;; accepting an empty suite would silently weaken Gesso's assurance gates.
(deftest namespace-success-requires-nonempty-matching-execution-evidence
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)]
        (doseq [n ["gesso.sample-test" "gesso.portable-test"]]
          (let [r (jvm-dispatch! fx ["jvm:namespace" n] 0)]
            (is (= 0 (:exit r)) (:out r))
            (is (= {:gesso/jvm-namespace-evidence 1
                    :namespace n :tests 3 :assertions 6
                    :failures 0 :errors 0}
                   (jvm-evidence! r)))
            (is (= ["clojure" "-M:test:formal" "-n" n]
                   (:invoked r)))))))))

(deftest legacy-jvm-namespace-rejects-empty-and-incomplete-execution
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)
            namespace-name "gesso.sample-test"]
        (doseq [scenario ["empty" "no-assertions" "wrong-namespace"
                          "extra-namespace" "missing-testing" "no-summary"
                          "missing-outcome" "reported-failures"
                          "reported-errors" "mismatched-counts" "no-output"]]
          (testing scenario
            (let [r (jvm-dispatch! fx ["jvm:namespace" namespace-name] 0 scenario)]
              (is (not= 0 (:exit r)) (:out r))
              (is (= ["clojure" "-M:test:formal" "-n" namespace-name]
                     (:invoked r)))
              (is (nil? (jvm-evidence! r))
                  "Rejected JVM execution must not emit an accepted evidence record"))))))))

(deftest prepared-namespace-evidence-failures-never-repeat-integrity
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)
            head (init-fixture-git! temp)
            fish (io/file (:bin fx) "fish")
            trace (:trace fx)
            integrity-log (io/file temp "integrity-invocations.txt")
            namespace-name "gesso.sample-test"
            execute (fn [scenario child-exit]
                      (Files/deleteIfExists (.toPath trace))
                      (let [r (exec! temp
                                     [(bb-executable) "script/gesso_test.clj"
                                      "jvm:namespace:prepared" namespace-name]
                                     {"PATH" (str (:bin fx) java.io.File/pathSeparator
                                                  (or (System/getenv "PATH") "/usr/bin:/bin"))
                                      "GESSO_SHARD_TEST_TRACE" (str trace)
                                      "GESSO_SHARD_JVM_EVIDENCE_CASE" scenario
                                      "GESSO_SHARD_TEST_EXIT" (str child-exit)
                                      "GESSO_SHARD_PREFLIGHT_COMMIT" head
                                      "GESSO_SHARD_INTEGRITY_LOG" (str integrity-log)})]
                        (assoc r :invoked (when (.isFile trace)
                                            (str/split-lines (slurp trace))))))]
        (spit fish
              "#!/bin/sh\nprintf 'unexpected-integrity\n' >> \"$GESSO_SHARD_INTEGRITY_LOG\"\nexit 0\n")
        (.setExecutable fish true false)
        (doseq [[scenario child-exit] [["empty" 0] ["wrong-namespace" 0]
                                     ["no-summary" 0] ["reported-failures" 0]
                                     ["valid" 17]]]
          (testing (str scenario " / JVM exit " child-exit)
            (let [r (execute scenario child-exit)]
              (is (not= 0 (:exit r)) (:out r))
              (is (= ["clojure" "-M:test:formal" "-n" namespace-name]
                     (:invoked r)))
              (is (nil? (jvm-evidence! r))))))
        (let [success (execute "valid" 0)]
          (is (= 0 (:exit success)) (:out success))
          (is (= {:gesso/jvm-namespace-evidence 1
                  :namespace namespace-name :tests 3 :assertions 6
                  :failures 0 :errors 0}
                 (jvm-evidence! success))))
        (is (not (.exists integrity-log))
            "Prepared jobs must not call the repository integrity script")))))


;; v1097: make v1096's timing observations a permanent regression contract.
;; Intervals describe stdout milestones, not pure JVM startup time.
(def ^:private timing-keys
  #{:gesso/jvm-timing :namespace :wall-ms :launch-to-testing-ms
    :testing-to-summary-ms :summary-to-exit-ms :method})

(defn- timing-receipts [result]
  (->> (str/split-lines (:out result))
       (filter #(str/starts-with? % "{:gesso/jvm-timing "))
       (mapv edn/read-string)))

(defn- assert-timing-contract! [receipt namespace-name]
  (is (= timing-keys (set (keys receipt))))
  (is (= 1 (:gesso/jvm-timing receipt)))
  (is (= namespace-name (:namespace receipt)))
  (is (= :stdout-milestones (:method receipt)))
  (doseq [k [:wall-ms :launch-to-testing-ms
             :testing-to-summary-ms :summary-to-exit-ms]]
    (is (and (integer? (get receipt k)) (<= 0 (get receipt k)))
        (str "Bad timing interval " k ": " (pr-str receipt))))
  (let [total (+ (:launch-to-testing-ms receipt)
                 (:testing-to-summary-ms receipt)
                 (:summary-to-exit-ms receipt))]
    (is (<= total (:wall-ms receipt)))
    (is (< (- (:wall-ms receipt) total) 3)
        "Integer truncation must account for the whole process lifetime")))

(deftest successful-jvm-jobs-emit-one-ordered-timing-receipt
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)]
        (doseq [n ["gesso.sample-test" "gesso.portable-test"]]
          (let [r (jvm-dispatch! fx ["jvm:namespace" n] 0)
                lines (str/split-lines (:out r))
                evidence-index (first (keep-indexed
                                        (fn [i s] (when (str/starts-with? s "{:gesso/jvm-namespace-evidence ") i))
                                        lines))
                timing-index (first (keep-indexed
                                      (fn [i s] (when (str/starts-with? s "{:gesso/jvm-timing ") i))
                                      lines))
                receipts (timing-receipts r)]
            (is (= 0 (:exit r)) (:out r))
            (is (= 1 (count receipts)))
            (is (and (some? evidence-index) (some? timing-index)
                     (< evidence-index timing-index))
                "Execution evidence must precede timing evidence")
            (when (= 1 (count receipts))
              (assert-timing-contract! (first receipts) n))))))))

(deftest timing-is-divided-by-observed-stdout-milestones
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)
            r (jvm-dispatch! fx ["jvm:namespace" "gesso.sample-test"] 0 "timed")
            receipts (timing-receipts r)]
        (is (= 0 (:exit r)) (:out r))
        (is (= 1 (count receipts)))
        (when (= 1 (count receipts))
          (let [t (first receipts)]
            (assert-timing-contract! t "gesso.sample-test")
            ;; The child sleeps 120/180/120 ms around its stdout markers.
            ;; Keep generous slack for process and CI scheduling jitter.
            (is (<= 70 (:launch-to-testing-ms t)) (pr-str t))
            (is (<= 125 (:testing-to-summary-ms t)) (pr-str t))
            (is (<= 70 (:summary-to-exit-ms t)) (pr-str t))
            (is (<= 300 (:wall-ms t)) (pr-str t))))))))

(deftest rejected-jvm-jobs-never-emit-accepted-timing
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)]
        (doseq [[scenario child-exit] [["empty" 0] ["no-assertions" 0]
                                     ["wrong-namespace" 0] ["extra-namespace" 0]
                                     ["missing-testing" 0] ["no-summary" 0]
                                     ["missing-outcome" 0] ["reported-failures" 0]
                                     ["reported-errors" 0] ["mismatched-counts" 0]
                                     ["no-output" 0] ["valid" 17]]]
          (testing scenario
            (let [r (jvm-dispatch! fx ["jvm:namespace" "gesso.sample-test"]
                                   child-exit scenario)]
              (is (not= 0 (:exit r)) (:out r))
              (is (nil? (jvm-evidence! r)))
              (is (empty? (timing-receipts r))
                  "Rejected execution cannot publish accepted measurements"))))))))

(deftest prepared-jvm-timing-does-not-rerun-integrity
  (with-temp-repo
    (fn [temp]
      (let [fx (prepare-jvm-dispatch-fixture! temp)
            head (init-fixture-git! temp)
            fish (io/file (:bin fx) "fish")
            integrity-log (io/file temp "integrity-called.log")
            env {"PATH" (str (:bin fx) java.io.File/pathSeparator
                              (or (System/getenv "PATH") "/usr/bin:/bin"))
                 "GESSO_SHARD_TEST_TRACE" (str (:trace fx))
                 "GESSO_SHARD_INTEGRITY_LOG" (str integrity-log)
                 "GESSO_SHARD_JVM_EVIDENCE_CASE" "valid"}
            execute (fn [commit]
                      (exec! temp [(bb-executable) "script/gesso_test.clj"
                                   "jvm:namespace:prepared" "gesso.sample-test"]
                             (assoc env "GESSO_SHARD_PREFLIGHT_COMMIT" commit)))]
        (spit fish "#!/bin/sh\nprintf 'unexpected\n' >> \"$GESSO_SHARD_INTEGRITY_LOG\"\nexit 0\n")
        (.setExecutable fish true false)
        (doseq [r [(execute "") (execute (apply str (repeat 40 "0")))]]
          (is (not= 0 (:exit r)) (:out r))
          (is (empty? (timing-receipts r))))
        (let [good (execute head)
              receipts (timing-receipts good)]
          (is (= 0 (:exit good)) (:out good))
          (is (= 1 (count receipts)))
          (when (= 1 (count receipts))
            (assert-timing-contract! (first receipts) "gesso.sample-test"))
          (is (not (.exists integrity-log))
              "Prepared timing cannot trigger a second preflight"))))))
