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
