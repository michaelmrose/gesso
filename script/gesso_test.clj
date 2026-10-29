(ns gesso.script.gesso-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def test-work-dir
  "target/gesso-test")

(def generated-test-src
  (str test-work-dir
       "/generated-src"))

(def production-runtime-entry
  "gesso.live.browser.runtime")

(def generated-runtime-artifact
  "resources/public/js/gesso-live.js")

(def browser-wall-clock-timeout-ms
  1800000)

(def real-browser-integration-wall-clock-timeout-ms
  300000)

;; The JVM gate includes embedded XTDB and Recife/TLC model checking. Both may
;; leave threads alive after the test runner prints its result. Limit the whole
;; child JVM rather than accepting a printed summary as a completed gate.
;; This bound must comfortably accommodate the full JVM corpus on slower hosts.
(def jvm-wall-clock-timeout-ms
  1800000)

(def real-browser-integration-namespace
  "gesso.live.browser.chromium-integration")

(def browser-test-namespaces-env
  "GESSO_BROWSER_TEST_NAMESPACES")

(def browser-diagnostic-env
  "GESSO_BROWSER_DIAGNOSTIC")

(def repo-integrity-script
  "script/repo_integrity.fish")

(defn fail!
  [message data]
  (throw
   (ex-info
    message
    data)))

(def gate-failure-key
  ::gate-failure)

(defn gate-failure!
  [message data]
  (throw
   (ex-info
    message
    (assoc data gate-failure-key true))))

(defn gate-failure?
  [error]
  (boolean
   (and (instance? clojure.lang.ExceptionInfo error)
        (get (ex-data error) gate-failure-key))))

(defn ensure-project-root!
  []
  (when-not
   (fs/exists?
    "deps.edn")
    (fail!
     "Run the Gesso test harness from the project root containing deps.edn."
     {:cwd
      (str
       (fs/cwd))})))

(defn delete-tree-if-exists!
  [path]
  (when
   (fs/exists?
    path)
    (fs/delete-tree
     path))
  nil)

(defn monotonic-nanos
  []
  (System/nanoTime))

(defn elapsed-seconds
  [started-at]
  (/ (double
      (-
       (monotonic-nanos)
       started-at))
     1000000000.0))

(defn print-stage-pass!
  [label started-at]
  (println
   (format
    "%s: PASS (%.2fs)"
    label
    (elapsed-seconds
     started-at))))

(defn run-command!
  [command]
  (let [result
        @(process/process
          command
          {:in :inherit
           :out :inherit
           :err :inherit})]

    (when-not
     (zero?
      (:exit result))
      (gate-failure!
       "External test command failed."
       {:command command
        :exit (:exit result)}))

    result))

(defn process-descendants
  [java-process]
  (with-open
   [stream
    (.descendants
     (.toHandle
      java-process))]
    (vec
     (iterator-seq
      (.iterator
       stream)))))

(defn terminate-process-tree!
  [java-process]
  ;; Chromium is multi-process. Killing only its top-level process can leave a
  ;; renderer holding stdout/stderr open, which in turn can leave the harness
  ;; blocked even after the nominal timeout. Retire descendants first, then the
  ;; parent, and wait for the parent to become reaped.
  (doseq [process-handle
          (process-descendants
           java-process)]
    (when
     (.isAlive
      process-handle)
      (.destroyForcibly
       process-handle)))

  (when
   (.isAlive
    java-process)
    (.destroyForcibly
     java-process))

  (.waitFor
   java-process)

  nil)

(defn run-command-streaming-with-timeout!
  [command timeout-ms]
  ;; Keep test output live: the full JVM suite can be large and long-running.
  ;; A captured result would hide progress and buffer its entire output.
  ;; Inherit streams, but wait on the child Process with a wall-clock bound.
  (let [process-result
        (process/process
         command
         {:in :inherit
          :out :inherit
          :err :inherit})
        java-process
        (:proc process-result)
        completed?
        (.waitFor
         java-process
         timeout-ms
         java.util.concurrent.TimeUnit/MILLISECONDS)]
    (if completed?
      (let [result @process-result]
        (when-not
         (zero? (:exit result))
          (gate-failure!
           "External test command failed."
           {:command command
            :exit (:exit result)}))
        result)
      (do
        ;; Cleanup is the same process-tree teardown used for Chromium tests.
        ;; A checker that hangs, or one that leaves a non-daemon worker alive,
        ;; must not indefinitely stall the normal Gesso test gate.
        (terminate-process-tree! java-process)
        (let [result @process-result]
          (gate-failure!
           "External test command exceeded its wall-clock timeout."
           {:command command
            :exit (:exit result)
            :timeout-ms timeout-ms}))))))

(defn print-captured-output!
  [result]
  (when
   (seq
    (:out result))
    (print
     (:out result)))

  (when
   (seq
    (:err result))
    (binding
     [*out* *err*]
      (print
       (:err result))))

  nil)

(defn run-command-captured-with-timeout!
  [command timeout-ms]
  (let [process-result
        (process/process
         command
         {:in :inherit
          :out :string
          :err :string})

        java-process
        (:proc
         process-result)

        completed?
        (.waitFor
         java-process
         timeout-ms
         java.util.concurrent.TimeUnit/MILLISECONDS)]

    (if
     completed?

      (let [result
            @process-result]

        (when-not
         (zero?
          (:exit result))
          (print-captured-output!
           result)

          (gate-failure!
           "External test command failed."
           {:command command
            :exit (:exit result)}))

        result)

      (do
        (terminate-process-tree!
         java-process)

        (let [result
              @process-result]

          (print-captured-output!
           result)

          (gate-failure!
           "External test command exceeded its wall-clock timeout."
           {:command command
            :exit (:exit result)
            :timeout-ms timeout-ms}))))))

(defn test-namespace
  [path]
  (-> (fs/relativize
       "test"
       path)
      str
      (str/replace
       #"\.clj[sc]?$"
       "")
      (str/replace
       #"[\\/]"
       ".")
      (str/replace
       "_"
       "-")
      symbol))

;; Formal-backend tests are still ordinary JVM tests, and so are always
;; included in the full `clojure -M:test:formal` discovery. This narrower selector
;; exists for rapid proof work without introducing a second, optional suite.
;; Keep the convention engine-neutral: test names containing a separate
;; `recife` or `formal` segment are eligible, not just one pilot namespace.
(def formal-test-file-pattern
  #"(?:^|[_-])(?:recife|formal)(?:[_-]|\.)")

;; Sequential JVM execution remains a full-suite gate. Parallel JVM execution
;; expands it into individual namespaces. Only *_test.clj and *_test.cljc files are
;; candidates, matching the test corpus included by cognitect.test-runner.
;; Reject a source/path mismatch rather than publishing a runnable-looking job
;; that would silently miss the intended namespace.
(def jvm-test-file-pattern #".*_test\.clj(?:c)?$")
(def jvm-namespace-declaration-pattern
  #"(?m)^[ \t]*\(ns[ \t\r\n]+([A-Za-z_][A-Za-z0-9_.-]*)")

(defn jvm-test-namespaces
  []
  (let [paths (->> (concat (fs/glob "test" "**/*_test.clj")
                           (fs/glob "test" "**/*_test.cljc"))
                   (filter #(re-matches jvm-test-file-pattern
                                         (str (fs/file-name %))))
                   (sort-by str)
                   vec)
        entries (mapv (fn [path]
                        (let [expected (test-namespace path)
                              content (slurp (str path))
                              declared (some-> (re-find jvm-namespace-declaration-pattern
                                                       content)
                                               second
                                               symbol)]
                          (when-not (= expected declared)
                            (fail! "JVM test namespace declaration disagrees with its path."
                                   {:file (str path)
                                    :expected expected
                                    :declared declared}))
                          expected))
                      paths)
        unique-namespaces (vec (sort (distinct entries)))]
    (when (or (empty? entries)
              (not= (count entries) (count unique-namespaces)))
      (fail! "JVM namespace inventory is empty or contains duplicate namespaces."
             {:files (count entries)
              :namespaces (count unique-namespaces)}))
    unique-namespaces))

;; Intentionally fixed, version-controlled regression smoke selection.
;; Keep the list in this single place so the direct and prepared parallel
;; paths always run the same namespaces. This is NOT adaptive selection.
(def quick-test-namespace-names
  ["gesso.choreo.semantics-test"
   "gesso.choreo.type-test"
   "gesso.model.command-test"
   "gesso.model.invariant-check-test"
   "gesso.model.resolved-operation-test"
   "gesso.model.tx-refinement-test"
   "gesso.live.application-preflight.semantic-audit-test"
   "gesso.live.browser.adapter-proof-test"
   "gesso.live.optimistic.resolved-operation-test"])

(defn quick-test-namespaces []
  ;; Validate against the strict authoritative JVM test corpus, not just file
  ;; existence. A renamed/deleted or incorrectly declared namespace must fail
  ;; instead of making the smoke gate silently smaller.
  (let [available (set (map str (jvm-test-namespaces)))
        requested quick-test-namespace-names
        missing (vec (remove available requested))]
    (when (or (seq missing)
              (not= (count requested) (count (distinct requested))))
      (fail! "Quick JVM namespace selection is missing or duplicated."
             {:missing missing :requested requested}))
    (mapv symbol requested)))

(defn shard-jvm-namespace-inventory
  []
  ;; This is also the authoritative :jvm portion of the normal parallel
  ;; inventory; it remains separately inspectable as jvm:inventory.
  {:jobs
   (mapv (fn [test-ns]
           (let [name (str test-ns)]
             {:id (str "gesso-jvm-" name)
              :gate :jvm
              :runtime :gesso
              :unit {:type :gesso-jvm-namespace :name name}
              :command ["bb" "script/gesso_test.clj" "jvm:namespace" name]
              :depends-on []
              :timeout-ms jvm-wall-clock-timeout-ms
              :requires #{}}))
         (jvm-test-namespaces))})

(declare run-repo-integrity!)

;; Prepared jobs are private to Shard's invocation-level preflight barrier. The
;; commit binding prevents accidental reuse against a different clone/source.
;; It is not an authentication or proof mechanism: the coordinator MUST run
;; repository integrity successfully before setting the environment binding.
(def prepared-head-environment "GESSO_SHARD_PREFLIGHT_COMMIT")

(defn require-preflight-admission!
  []
  (let [expected (System/getenv prepared-head-environment)]
    (when-not (and (string? expected)
                   (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" expected))
      (fail! "Prepared test jobs require an invocation-level preflight binding."
             {:required-environment prepared-head-environment}))
    (let [result @(process/process ["git" "rev-parse" "HEAD"]
                                   {:out :string :err :string})]
      (when-not (and (zero? (:exit result))
                     (= expected (str/trim (:out result))))
        (fail! "Prepared test job source differs from the preflighted commit."
               {:expected expected
                :actual (when (zero? (:exit result))
                          (str/trim (:out result)))})))))

;; A runner exit code by itself is not evidence that the selected namespace
;; actually executed. In particular, a missing/empty test namespace can make a
;; JVM subprocess exit normally while silently reducing the covered corpus.
;; Observe Cognitect's clojure.test reports as they stream; do not accumulate
;; the whole output or hide diagnostics until the JVM finishes.
(def jvm-testing-line-pattern
  #"^Testing ([A-Za-z_][A-Za-z0-9_.-]*)$")
(def jvm-count-line-pattern
  #"^Ran ([0-9]+) tests? containing ([0-9]+) assertions?\.$")
(def jvm-outcome-line-pattern
  #"^([0-9]+) failures?, ([0-9]+) errors?\.$")

;; Timing is observation, not attribution.  The first "Testing" line follows
;; JVM startup, classpath setup, namespace loading, and test discovery; it is
;; NOT a measurement of JVM startup alone.  The final clojure.test outcome
;; precedes any shutdown work or lingering background threads.
(defn jvm-timing-receipt
  [namespace-name started-at evidence finished-at]
  (let [{:keys [first-testing-at last-outcome-at]} evidence]
    (when-not (and (integer? first-testing-at)
                   (integer? last-outcome-at)
                   (<= started-at first-testing-at last-outcome-at finished-at))
      (gate-failure!
       "JVM namespace timing markers were missing or out of order."
       {:namespace namespace-name
        :markers (select-keys evidence [:first-testing-at :last-outcome-at])}))
    (let [ms (fn [start end] (long (quot (- end start) 1000000)))]
      {:gesso/jvm-timing 1
       :namespace namespace-name
       :wall-ms (ms started-at finished-at)
       :launch-to-testing-ms (ms started-at first-testing-at)
       :testing-to-summary-ms (ms first-testing-at last-outcome-at)
       :summary-to-exit-ms (ms last-outcome-at finished-at)
       :method :stdout-milestones})))

(defn record-jvm-evidence-line!
  [evidence line]
  (let [line (str/trim line)
        observed-at (monotonic-nanos)]
    (when-let [[_ name] (re-matches jvm-testing-line-pattern line)]
      (swap! evidence
             (fn [e]
               (-> e
                   (update :testing conj name)
                   (cond-> (nil? (:first-testing-at e))
                     (assoc :first-testing-at observed-at))))))
    (when-let [[_ tests assertions] (re-matches jvm-count-line-pattern line)]
      (swap! evidence update :counts conj
             [(parse-long tests) (parse-long assertions)]))
    (when-let [[_ failures errors] (re-matches jvm-outcome-line-pattern line)]
      (swap! evidence (fn [e]
                        (-> e
                            (update :outcomes conj
                                    [(parse-long failures) (parse-long errors)])
                            (assoc :last-outcome-at observed-at)))))))

(defn valid-jvm-namespace-evidence?
  [namespace-name {:keys [testing counts outcomes]}]
  ;; The Cognitect runner invokes clojure.test for precisely the requested -n
  ;; selector. Nested clojure.test runs may generate additional reports; in
  ;; that case require all named testing runs to refer to this namespace and
  ;; trust only the last (outermost) summary. A printed summary is execution
  ;; evidence, not a formal guarantee of test/fixture correspondence.
  (let [[tests assertions] (last counts)
        [failures errors] (last outcomes)]
    (and (seq testing)
         (every? #{namespace-name} testing)
         (= (count counts) (count outcomes))
         (pos-int? tests)
         (pos-int? assertions)
         (zero? failures)
         (zero? errors))))

(defn run-jvm-namespace-with-evidence!
  [namespace-name]
  (let [command ["clojure" "-M:test:formal" "-n" namespace-name]
        evidence (atom {:testing [] :counts [] :outcomes []})
        started-at (monotonic-nanos)
        child (process/process command
                               {:in :inherit :out :pipe :err :inherit})
        java-process (:proc child)
        ;; Drain stdout concurrently with the bounded JVM wait. Reading only
        ;; after waiting risks a pipe-buffer deadlock for verbose test suites.
        output-reader (future
                        (with-open [reader (io/reader (.getInputStream java-process))]
                          (doseq [line (line-seq reader)]
                            (println line)
                            (flush)
                            (record-jvm-evidence-line! evidence line))))
        completed? (.waitFor java-process
                             jvm-wall-clock-timeout-ms
                             java.util.concurrent.TimeUnit/MILLISECONDS)]
    (when-not completed?
      (terminate-process-tree! java-process))
    ;; A background descendant can inherit the stdout pipe even after the JVM
    ;; parent exits. Do not indefinitely block on its output stream; report a
    ;; failed job so Shard retains the workspace on unsafe completion.
    (let [reader-result (deref output-reader 5000 ::unclosed-output)]
      (when (= ::unclosed-output reader-result)
        (future-cancel output-reader)
        (gate-failure!
         "JVM namespace child left an unclosed output stream."
         {:namespace namespace-name :command command}))
      (when-not completed?
        (gate-failure!
         "JVM namespace exceeded its wall-clock timeout."
         {:namespace namespace-name :command command
          :timeout-ms jvm-wall-clock-timeout-ms}))
      (let [result @child]
        (when-not (zero? (:exit result))
          (gate-failure!
           "JVM namespace process failed."
           {:namespace namespace-name :command command :exit (:exit result)}))
        (when-not (valid-jvm-namespace-evidence? namespace-name @evidence)
          (gate-failure!
           "JVM namespace returned without verifiable nonempty test execution."
           {:namespace namespace-name :command command :evidence @evidence}))
        (let [finished-at (monotonic-nanos)
              [tests assertions] (last (:counts @evidence))
              timing (jvm-timing-receipt namespace-name
                                         started-at @evidence finished-at)]
          ;; Preserve the v1094 evidence record byte-for-byte at the EDN level:
          ;; Gesso's existing correspondence checks consume precisely this map.
          (println (pr-str {:gesso/jvm-namespace-evidence 1
                            :namespace namespace-name
                            :tests tests
                            :assertions assertions
                            :failures 0
                            :errors 0}))
          ;; Separate one-line EDN for empirical cost analysis by namespace.
          ;; The timings do not gate test success or claim pure JVM startup.
          (println (pr-str timing)))
        result))))

(defn run-one-jvm-test-namespace!
  ([namespace-name] (run-one-jvm-test-namespace! namespace-name false))
  ([namespace-name prepared?]
   (let [available (set (map str (jvm-test-namespaces)))]
     (when-not (contains? available namespace-name)
       (fail! "Unknown JVM test namespace; refusing to execute arbitrary code."
              {:requested namespace-name
               :available-count (count available)}))
     (if prepared?
       (require-preflight-admission!)
       (run-repo-integrity!))
     (println)
     (println "== JVM test namespace ==" namespace-name)
     (run-jvm-namespace-with-evidence! namespace-name))))

(defn formal-test-namespaces
  []
  (let [namespaces
        (->> (concat
              (fs/glob "test" "**/*_test.clj")
              (fs/glob "test" "**/*_test.cljc"))
             (filter
              (fn [path]
                (re-find formal-test-file-pattern
                         (str (fs/file-name path)))))
             (map test-namespace)
             distinct
             sort
             vec)]
    (when (empty? namespaces)
      (gate-failure!
       "No formal-verification test namespaces were discovered."
       {:expected "test/**/*recife*_test.clj or test/**/*formal*_test.clj (also .cljc)"}))
    namespaces))

(defn cljs-test-namespaces
  []
  (->> (concat
        (fs/glob
         "test"
         "**/*_test.cljs")
        (fs/glob
         "test"
         "**/*_test.cljc"))
       (map
        test-namespace)
       distinct
       sort
       vec))

(defn requested-browser-test-namespaces
  []
  (let [raw
        (System/getenv
         browser-test-namespaces-env)]

    (when-not
     (str/blank?
      raw)
      (->> (str/split
            raw
            #",")
           (map
            str/trim)
           (remove
            str/blank?)
           (map
            symbol)
           distinct
           vec))))

(defn browser-diagnostic?
  []
  (contains?
   #{"1" "true" "yes" "on"}
   (some->
    (System/getenv browser-diagnostic-env)
    str/lower-case
    str/trim)))

(defn browser-test-namespaces
  []
  (let [all-tests
        (cljs-test-namespaces)

        requested
        (requested-browser-test-namespaces)]

    (if-not
     (seq
      requested)
      all-tests

      (let [all-test-set
            (set
             all-tests)

            unknown
            (->> requested
                 (remove
                  all-test-set)
                 vec)]

        (when
         (seq
          unknown)
          (fail!
           "Requested Chromium test namespaces were not discovered."
           {:environment-variable
            browser-test-namespaces-env

            :unknown-namespaces
            unknown

            :available-namespaces
            all-tests}))

        (let [requested-set
              (set
               requested)]

          (->> all-tests
               (filter
                requested-set)
               vec))))))

;; Every CLJS/CLJC test is run in Chromium. The browser-side suites below
;; are additionally required to run under Node so accidental DOM dependencies
;; in the semantic/browser core are caught early. Every portable gesso.choreo
;; CLJS/CLJC test is discovered automatically and also run under Node so the
;; portable choreography corpus is exercised on JVM Clojure, Node CLJS, and
;; Chromium CLJS as required by the choreography design.
(def required-node-browser-test-namespaces
  '#{gesso.live.browser.adapter-test
     gesso.live.browser.adapter-property-test
     gesso.live.browser.bootstrap-test
     gesso.live.browser.choreo-test
     gesso.live.browser.core-test
     gesso.live.browser.fx-conformance-test
     gesso.live.browser.fx-test
     gesso.live.browser.optimistic-test
     gesso.live.browser.optimistic-htmx-test
     gesso.live.browser.runtime-test
     gesso.live.browser.shell-test})

(defn portable-choreo-test-namespace?
  [test-ns]
  (str/starts-with?
   (str
    test-ns)
   "gesso.choreo."))


(defn deps-edn
  []
  (edn/read-string
   (slurp
    "deps.edn")))

(defn test-alias-without-main
  []
  (let [test-alias
        (get-in
         (deps-edn)
         [:aliases
          :test])]

    (when-not
     (map?
      test-alias)
      (fail!
       "deps.edn must define :aliases/:test."
       {}))

    ;; cljs.main must be the launched program, not Cognitect's JVM runner.
    ;; Reuse every other path/dependency/JVM option from the actual :test alias
    ;; so this harness cannot silently drift from deps.edn.
    (-> test-alias
        (dissoc
         :main-opts)
        (update
         :extra-paths
         (fnil
          conj
          [])
         generated-test-src))))

(defn cljs-test-sdeps
  []
  (pr-str
   {:aliases
    {:gesso-cljs-test
     (test-alias-without-main)}}))

(defn cljs-main-command
  [& args]
  (into
   ["clojure"
    "-Sdeps"
    (cljs-test-sdeps)
    "-M:dev:gesso-cljs-test"
    "-m"
    "cljs.main"]
   args))

(defn require-lines
  [namespaces]
  (apply
   str
   (map
    (fn [test-ns]
      (str
       "   ["
       test-ns
       "]\n"))
    namespaces)))

(defn quoted-test-args
  [namespaces]
  (apply
   str
   (map
    (fn [test-ns]
      (str
       "\n   '"
       test-ns))
    namespaces)))

(defn node-runner-name
  [group-id]
  (str
   "test_node_"
   (name
    group-id)
   "_runner"))

(defn node-runner-namespace
  [group-id]
  (symbol
   (str
    "gesso."
    (str/replace
     (node-runner-name
      group-id)
     "_"
     "-"))))

(defn write-node-runner!
  [group-id namespaces]
  (let [runner-name
        (node-runner-name
         group-id)

        runner-ns
        (node-runner-namespace
         group-id)

        path
        (fs/path
         generated-test-src
         "gesso"
         (str
          runner-name
          ".cljs"))]

    (fs/create-dirs
     (fs/parent
      path))

    (spit
     (str
      path)
     (str
      "(ns " runner-ns "\n"
      "  (:require\n"
      "   [cljs.test :as t]\n"
      (require-lines
       namespaces)
      "   ))\n\n"
      "(derive ::reporter ::t/default)\n\n"
      "(defmethod t/report [::reporter :end-run-tests]\n"
      "  [summary]\n"
      "  (let [failed (+ (or (:fail summary) 0)\n"
      "                  (or (:error summary) 0))]\n"
      "    (.exit js/process (if (zero? failed) 0 1))))\n\n"
      "(defn main\n"
      "  []\n"
      "  (t/run-tests\n"
      "   (t/empty-env ::reporter)"
      (quoted-test-args
       namespaces)
      "))\n\n"
      "(main)\n"))

    {:namespace
     runner-ns

     :path
     path}))

(defn write-browser-runner!
  [namespaces]
  (let [path
        (fs/path
         generated-test-src
         "gesso"
         "test_all_browser_runner.cljs")

        selected-cases
        (apply
         str
         (map
          (fn [test-ns]
            (str
             "    "
             (pr-str
              (str
               test-ns))
             "\n"
             "    (t/run-tests\n"
             "     (t/empty-env ::reporter)\n"
             "     '"
             test-ns
             ")\n"))
          namespaces))]

    (fs/create-dirs
     (fs/parent
      path))

    (spit
     (str
      path)
     (str
      "(ns gesso.test-all-browser-runner\n"
      "  (:require\n"
      "   [cljs.test :as t]\n"
      (require-lines
       namespaces)
      "   ))\n\n"
      "(derive ::reporter ::t/default)\n\n"
      "(def failures (atom []))\n\n"
      "(defn default-report!\n"
      "  [event-type message]\n"
      "  (when-let [reporter (get-method t/report [::t/default event-type])]\n"
      "    (reporter message)))\n\n"
      "(defmethod t/report [::reporter :fail]\n"
      "  [message]\n"
      "  (default-report! :fail message)\n"
      "  (swap! failures conj\n"
      "         (select-keys message\n"
      "                      [:file :line :column :message :expected :actual])))\n\n"
      "(defmethod t/report [::reporter :error]\n"
      "  [message]\n"
      "  (default-report! :error message)\n"
      "  (swap! failures conj\n"
      "         (select-keys message\n"
      "                      [:file :line :column :message :expected :actual])))\n\n"
      "(defn complete!\n"
      "  [status payload]\n"
      "  (let [body (.-body js/document)]\n"
      "    (.setAttribute body \"data-gesso-test-status\" status)\n"
      "    (set! (.-textContent body) (pr-str payload))\n"
      "    (set! (.-title js/document) (str \"gesso-tests-\" status))))\n\n"
      "(set! (.-onerror js/window)\n"
      "      (fn [message source line column error]\n"
      "        (complete!\n"
      "         \"runtime-error\"\n"
      "         {:message message\n"
      "          :source source\n"
      "          :line line\n"
      "          :column column\n"
      "          :error (str error)})\n"
      "        false))\n\n"
      "(set! (.-onunhandledrejection js/window)\n"
      "      (fn [event]\n"
      "        (complete!\n"
      "         \"runtime-error\"\n"
      "         {:message \"Unhandled promise rejection\"\n"
      "          :reason (str (.-reason event))})\n"
      "        false))\n\n"
      "(defmethod t/report [::reporter :end-run-tests]\n"
      "  [summary]\n"
      "  (let [failed (+ (or (:fail summary) 0)\n"
      "                  (or (:error summary) 0))]\n"
      "    (complete!\n"
      "     (if (zero? failed) \"pass\" \"fail\")\n"
      "     {:test (:test summary)\n"
      "      :pass (:pass summary)\n"
      "      :fail (:fail summary)\n"
      "      :error (:error summary)\n"
      "      :failures @failures})))\n\n"
      "(defn selected-namespace\n"
      "  []\n"
      "  (.get (js/URLSearchParams. (.-search js/location)) \"ns\"))\n\n"
      "(defn run-selected!\n"
      "  [selected]\n"
      "  (case selected\n"
      selected-cases
      "    (complete!\n"
      "     \"runtime-error\"\n"
      "     {:message \"Unknown Chromium test namespace\"\n"
      "      :namespace selected})))\n\n"
      "(defn main\n"
      "  []\n"
      "  (if-let [selected (selected-namespace)]\n"
      "    (run-selected! selected)\n"
      "    (t/run-tests\n"
      "     (t/empty-env ::reporter)"
      (quoted-test-args
       namespaces)
      ")))\n\n"
      "(main)\n"))

    path))

(defn chromium-command
  []
  (or
   (some
    (fn [candidate]
      (when-let [path
                 (fs/which
                  candidate)]
        (str
         path)))
    ["chromium"
     "chromium-browser"
     "google-chrome"
     "google-chrome-stable"])
   (fail!
    "No Chromium/Chrome executable was found on PATH."
    {})))

(defn run-repo-integrity!
  []
  (println)
  (println
   "== Repository integrity ==")

  (when-not
   (fs/regular-file?
    repo-integrity-script)
    (fail!
     "Repository integrity script is missing."
     {:script repo-integrity-script}))

  (let [fish
        (or
         (some->
          (fs/which
           "fish")
          str)
         (fail!
          "Repository integrity checking requires fish on PATH."
          {:script repo-integrity-script}))]
    (run-command!
     [fish
      repo-integrity-script])))

(defn run-jvm-tests!
  []
  (println)
  (println
   "== JVM tests ==")
  (println
   "JVM suite wall-clock limit:"
   (/ jvm-wall-clock-timeout-ms 60000)
   "minutes")

  (run-command-streaming-with-timeout!
   ["clojure"
    "-M:test:formal"]
   jvm-wall-clock-timeout-ms))

(defn run-quick-tests! []
  (let [namespaces (quick-test-namespaces)
        command (into ["clojure" "-M:test:formal"]
                      (mapcat (fn [namespace] ["-n" (str namespace)])
                              namespaces))]
    (println "== Quick JVM tests (fixed selection) ==")
    (println "Selected namespaces:" (pr-str namespaces))
    ;; Use the same integrity boundary as the sequential JVM gate.
    (run-repo-integrity!)
    (run-command-streaming-with-timeout! command jvm-wall-clock-timeout-ms)))

(defn run-formal-tests!
  []
  (let [namespaces (formal-test-namespaces)
        command (into ["clojure" "-M:test:formal"]
                      (mapcat (fn [namespace]
                                ["-n" (str namespace)])
                              namespaces))]
    (println)
    (println "== Formal JVM tests (Recife/TLC) ==")
    (println "Selected namespaces:" (pr-str namespaces))
    (println "Wall-clock limit:" (/ jvm-wall-clock-timeout-ms 60000)
             "minutes")
    (run-command-streaming-with-timeout!
     command
     jvm-wall-clock-timeout-ms)))

(defn node-test-plan
  []
  (let [all-tests
        (cljs-test-namespaces)

        all-test-set
        (set
         all-tests)

        missing-required
        (->> required-node-browser-test-namespaces
             (remove
              all-test-set)
             sort
             vec)

        portable-choreo-tests
        (->> all-tests
             (filter
              portable-choreo-test-namespace?)
             sort
             vec)]

    (when
     (seq
      missing-required)
      (fail!
       "Required Node-compatible browser tests are missing."
       {:missing-namespaces
        missing-required}))

    (when
     (empty?
      portable-choreo-tests)
      (fail!
       "No portable gesso.choreo CLJS/CLJC tests were discovered for Node."
       {}))

    {:browser
     (->> required-node-browser-test-namespaces
          sort
          vec)

     :choreo
     portable-choreo-tests}))

(defn run-node-test-groups!
  [group-ids]
  (println)
  (println
   "== CLJS / Node tests ==")

  (let [plan
        (node-test-plan)

        node-dir
        (str
         test-work-dir
         "/node")

        output-dir
        (str
         node-dir
         "/out")]

    (delete-tree-if-exists!
     node-dir)

    (fs/create-dirs
     node-dir)

    (doseq [group-id
            group-ids]
      (let [namespaces
            (get
             plan
             group-id)

            _
            (when-not
             (seq
              namespaces)
              (fail!
               "Unknown or empty Node test group."
               {:group-id
                group-id}))

            {:keys [namespace]}
            (write-node-runner!
             group-id
             namespaces)

            output-to
            (str
             node-dir
             "/"
             (name
              group-id)
             "-tests.js")]

        (println)
        (println
         (case
          group-id

          :browser
          "Node browser-semantic suites"

          :choreo
          "Portable Choreo suites"

          (str
           "Node group "
           group-id)))
        (println
         "Namespaces:"
         (count
          namespaces))

        ;; Node is a semantic host-conformance gate, not an optimization gate.
        ;; :none keeps the portable corpus fast enough for routine execution;
        ;; the production advanced bundle has already been generated by the
        ;; first gate of the full test run.
        (run-command!
         (cljs-main-command
          "-co"
          (pr-str
           {:target :nodejs
            :optimizations :none
            :output-dir output-dir
            :output-to output-to})
          "-c"
          (str
           namespace)))

        (run-command!
         ["node"
          output-to])))))

(defn run-node-browser-tests!
  []
  (run-node-test-groups!
   [:browser]))

(defn run-node-choreo-tests!
  []
  (run-node-test-groups!
   [:choreo]))

(defn browser-html
  [output-to]
  (str
   "<!doctype html>\n"
   "<html>\n"
   "<head>\n"
   "  <meta charset=\"utf-8\">\n"
   "  <title>gesso-tests-running</title>\n"
   "</head>\n"
   "<body data-gesso-test-status=\"running\">running</body>\n"
   "<script src=\"./"
   (fs/file-name
    output-to)
   "\"></script>\n"
   "</html>\n"))

(defn chromium-result-status
  [dumped-dom]
  (cond
    (str/includes?
     dumped-dom
     "data-gesso-test-status=\"pass\"")
    :pass

    (str/includes?
     dumped-dom
     "data-gesso-test-status=\"fail\"")
    :fail

    (str/includes?
     dumped-dom
     "data-gesso-test-status=\"runtime-error\"")
    :runtime-error

    :else
    :incomplete))

(defn require-successful-chromium-result!
  [result dump-file context]
  (let [dumped-dom
        (:out result)

        status
        (chromium-result-status
         dumped-dom)]

    (spit
     dump-file
     dumped-dom)

    (case status
      :pass
      result

      :fail
      (do
        (println
         dumped-dom)
        (gate-failure!
         "CLJS / Chromium assertions failed."
         (merge
          {:dump dump-file}
          context)))

      :runtime-error
      (do
        (println
         dumped-dom)
        (gate-failure!
         "CLJS / Chromium encountered an uncaught runtime error."
         (merge
          {:dump dump-file}
          context)))

      :incomplete
      (do
        (println
         dumped-dom)

        (when
         (seq
          (:err result))
          (binding
           [*out* *err*]
            (print
             (:err result))))

        (gate-failure!
         "Chromium exited without a completed cljs.test run."
         (merge
          {:dump dump-file}
          context))))))

(defn chromium-url
  [html test-ns]
  (str
   "file://"
   (fs/absolutize
    html)
   (when
    test-ns
    (str
     "?ns="
     (java.net.URLEncoder/encode
      (str
       test-ns)
      "UTF-8")))))

(defn chromium-base-command
  []
  (cond->
   [(chromium-command)
    "--headless=new"
    "--disable-gpu"
    "--disable-dev-shm-usage"
    "--disable-background-timer-throttling"
    "--disable-renderer-backgrounding"
    "--run-all-compositor-stages-before-draw"
    "--allow-file-access-from-files"
    "--virtual-time-budget=120000"
    "--dump-dom"]

   (= "root"
      (System/getProperty
       "user.name"))
   (conj
    "--no-sandbox")))

(defn run-one-chromium!
  [html dump-file test-ns]
  (let [label
        (if
         test-ns
         (str
          "Chromium namespace "
          test-ns)
         "Chromium execution")

        command
        (conj
         (chromium-base-command)
         (chromium-url
          html
          test-ns))

        started-at
        (monotonic-nanos)]

    (println
     (str
      label
      ": START"
      " (wall timeout "
      (/ browser-wall-clock-timeout-ms
         1000)
      "s)"))

    (let [result
          (run-command-captured-with-timeout!
           command
           browser-wall-clock-timeout-ms)]

      (require-successful-chromium-result!
       result
       dump-file
       (cond->
        {}
        test-ns
        (assoc
         :namespace
         test-ns)))

      (print-stage-pass!
       label
       started-at)

      result)))

(defn run-browser-tests!
  []
  (println)
  (println
   "== CLJS / Chromium tests ==")

  (let [namespaces
        (browser-test-namespaces)

        diagnostic?
        (browser-diagnostic?)

        browser-dir
        (str
         test-work-dir
         "/browser")

        output-dir
        (str
         browser-dir
         "/out")

        output-to
        (str
         browser-dir
         "/tests.js")

        html
        (str
         browser-dir
         "/index.html")

        dump-file
        (str
         browser-dir
         "/dump.html")]

    (when
     (empty?
      namespaces)
      (fail!
       "No CLJS/CLJC tests were discovered."
       {}))

    (println
     "Namespaces:"
     (count
      namespaces))

    (when
     (requested-browser-test-namespaces)
      (println
       "Chromium namespace selection:"
       (str/join
        ", "
        (map
         str
         namespaces))))

    (when
     diagnostic?
      (println
       "Chromium diagnostic mode: one namespace per browser process"))

    (delete-tree-if-exists!
     browser-dir)

    (fs/create-dirs
     browser-dir)

    (write-browser-runner!
     namespaces)

    ;; :simple deliberately produces a self-contained browser program. This
    ;; avoids depending on the Closure development loader over file:// URLs.
    (let [started-at
          (monotonic-nanos)]

      (println
       "Chromium bundle compile: START")

      (run-command!
       (cljs-main-command
        "-co"
        (pr-str
         {:target :browser
          :optimizations :simple
          :output-dir output-dir
          :output-to output-to})
        "-c"
        "gesso.test-all-browser-runner"))

      (print-stage-pass!
       "Chromium bundle compile"
       started-at))

    (spit
     html
     (browser-html
      output-to))

    (if
     diagnostic?

      (doseq [[index test-ns]
              (map-indexed
               vector
               namespaces)]
        (println
         (format
          "Chromium diagnostic [%d/%d]: %s"
          (inc
           index)
          (count
           namespaces)
          test-ns))

        (run-one-chromium!
         html
         (str
          browser-dir
          "/dump-"
          (format
           "%03d"
           (inc
            index))
          ".html")
         test-ns))

      (run-one-chromium!
       html
       dump-file
       nil))

    (println
     "CLJS / Chromium: PASS")))

(defn run-real-browser-integration!
  []
  (println)
  (println
   "== Production runtime / real Chromium integration ==")

  (when-not
   (fs/regular-file?
    generated-runtime-artifact)
    (fail!
     "Real-browser integration requires the generated production Gesso Live runtime."
     {:artifact generated-runtime-artifact
      :hint "Run the advanced/runtime gate before this integration gate."}))

  (when-not
   (pos?
    (fs/size
     generated-runtime-artifact))
    (fail!
     "Real-browser integration found an empty production Gesso Live runtime."
     {:artifact generated-runtime-artifact}))

  (let [started-at
        (monotonic-nanos)

        ;; The :test alias normally launches Cognitect's JVM test runner through
        ;; :main-opts. Reuse the exact test paths/dependencies/JVM options but
        ;; remove those main opts so this explicit browser integration program
        ;; owns the process entry point.
        command
        ["clojure"
         "-Sdeps"
         (cljs-test-sdeps)
         "-M:gesso-cljs-test"
         "-m"
         real-browser-integration-namespace]

        result
        (run-command-captured-with-timeout!
         command
         real-browser-integration-wall-clock-timeout-ms)]

    ;; This gate is intentionally small and its cljs.test summary is valuable
    ;; in successful full-gate logs too, not only when the subprocess fails.
    (print-captured-output!
     result)

    (print-stage-pass!
     "Production runtime / real Chromium integration"
     started-at)

    result))

(defn run-runtime-build!
  []
  (println)
  (println
   "== Production runtime build ==")

  (let [runtime-dir
        (str
         test-work-dir
         "/runtime")

        output-dir
        (str
         runtime-dir
         "/out")]

    (delete-tree-if-exists!
     runtime-dir)

    (fs/create-dirs
     runtime-dir)

    (when-let [parent
               (fs/parent
                (fs/path
                 generated-runtime-artifact))]
      (fs/create-dirs
       parent))

    ;; Remove any previous bundle so the post-compile checks prove that this
    ;; invocation actually produced the artifact under test.
    (when
     (fs/exists?
      generated-runtime-artifact)
      (fs/delete
       generated-runtime-artifact))

    ;; This is the production advanced compile and is deliberately the first
    ;; gate in `bb test`. It writes the real generated artifact once so every
    ;; later consumer sees the bundle produced from the source tree currently
    ;; under test. There is no freshness precondition and no second advanced
    ;; compile later in the run.
    (run-command!
     ["clojure"
      "-M:dev"
      "-m"
      "cljs.main"
      "-O"
      "advanced"
      "-co"
      (pr-str
       {:output-dir
        output-dir})
      "-o"
      generated-runtime-artifact
      "-c"
      production-runtime-entry])

    (when-not
     (fs/exists?
      generated-runtime-artifact)
      (gate-failure!
       "Production runtime compilation completed without creating its expected output."
       {:artifact
        generated-runtime-artifact
        :entry-point
        production-runtime-entry}))

    (when-not
     (pos?
      (fs/size
       generated-runtime-artifact))
      (gate-failure!
       "Production runtime compilation created an empty artifact."
       {:artifact
        generated-runtime-artifact
        :entry-point
        production-runtime-entry}))

    (println
     "Production runtime advanced compile: PASS")
    (println
     "Generated runtime:"
     generated-runtime-artifact)))

(defn run-theme-build!
  []
  (println)
  (println
   "== Theme build ==")

  (let [theme-dir
        (str
         test-work-dir
         "/themes")

        output-file
        (str
         theme-dir
         "/gesso-themes.css")]

    (delete-tree-if-exists!
     theme-dir)

    (fs/create-dirs
     theme-dir)

    ((requiring-resolve
      'gesso.build.themes/build!)
     {:input-dir
      "resources/themes"

      :output-file
      output-file})

    (when-not
     (fs/exists?
      output-file)
      (gate-failure!
       "Theme build completed without creating its expected output."
       {:output-file
        output-file}))

    (println
     "Theme build: PASS")))

(def gate-specs
  {:integrity
   {:label "Repo integrity"
    :run run-repo-integrity!}

   :jvm
   {:label "JVM"
    :run run-jvm-tests!}

   :formal
   {:label "Formal JVM"
    :run run-formal-tests!}

   :node:choreo
   {:label "Node Choreo"
    :run run-node-choreo-tests!}

   :node:browser
   {:label "Node Browser"
    :run run-node-browser-tests!}

   :browser
   {:label "Chromium"
    :run run-browser-tests!}

   :browser:integration
   {:label "Chromium integration"
    :run run-real-browser-integration!}

   :advanced
   {:label "Runtime build"
    :run run-runtime-build!}

   :themes
   {:label "Themes"
    :run run-theme-build!}})

(defn run-gate
  [gate-id]
  (let [{:keys [label run]}
        (or (get gate-specs gate-id)
            (fail!
             "Unknown Gesso test gate."
             {:gate-id gate-id
              :known-gates (set (keys gate-specs))}))]
    (try
      (run)
      {:gate gate-id
       :label label
       :status :pass}
      (catch Throwable error
        (if (gate-failure? error)
          (let [data (dissoc (ex-data error) gate-failure-key)]
            (println)
            (println (str label ": FAIL"))
            (println (ex-message error))
            (when-let [dump (:dump data)]
              (println "Diagnostic dump:" dump))
            {:gate gate-id
             :label label
             :status :fail
             :message (ex-message error)
             :data data})
          (throw error))))))

(defn print-gate-summary!
  [results]
  (println)
  (println "== Gate summary ==")
  (doseq [{:keys [label status]} results]
    (println
     (format "%-18s %s"
             label
             (if (= :pass status)
               "PASS"
               "FAIL"))))
  (let [failed (filter #(= :fail (:status %)) results)]
    (println)
    (if (seq failed)
      (println (count failed) "gate(s) failed.")
      (println "All selected Gesso gates passed.")))
  results)

(defn run-gates!
  [gate-ids]
  (let [results (mapv run-gate gate-ids)]
    (print-gate-summary! results)))

(defn failed-gates?
  [results]
  (boolean (some #(= :fail (:status %)) results)))

(defn run-cljs-tests!
  []
  (run-gates!
   [:integrity
    :advanced
    :node:choreo
    :node:browser
    :browser
    :browser:integration]))

(defn run-node-tests!
  []
  (run-gates!
   [:integrity
    :node:choreo
    :node:browser]))

(defn run-all!
  []
  (run-gates!
   [:integrity
    :advanced
    :jvm
    :node:choreo
    :node:browser
    :browser
    :browser:integration
    :themes]))

(defn usage!
  []
  (println
   "Usage: bb script/gesso_test.clj [all|integrity|quick|jvm|formal|cljs|node|node:choreo|node:browser|browser|browser:integration|advanced|themes|inventory [SCOPE]|inventory:prepared SCOPE|preflight|gate:prepared GATE|jvm:inventory|jvm:namespace NAMESPACE|jvm:namespace:prepared NAMESPACE]")
  (System/exit
   2))

(defn command-gates
  [command]
  (let [requested
        (case command
          "all"
          [:advanced
           :jvm
           :node:choreo
           :node:browser
           :browser
           :browser:integration
           :themes]

          "integrity"
          []

          "quick"
          [:jvm]

          "jvm"
          [:jvm]

          "formal"
          [:formal]

          "cljs"
          [:advanced
           :node:choreo
           :node:browser
           :browser
           :browser:integration]

          "node"
          [:node:choreo
           :node:browser]

          "node:choreo"
          [:node:choreo]

          "node:browser"
          [:node:browser]

          "browser"
          [:browser]

          "browser:integration"
          [:advanced
           :browser:integration]

          "advanced"
          [:advanced]

          "themes"
          [:themes]

          nil)]
    (when
     (some? requested)
      ;; Repository structure is a prerequisite for every test/build gate. In
      ;; particular, browser namespace discovery walks the filesystem, so a
      ;; misplaced or misnamed namespace must fail before any compiler starts.
      (into
       [:integrity]
       requested))))

;; Shard consumes an executable description of the authoritative full suite.
;; The sequential `bb test` entry point still uses `run-all!` and runs the
;; complete JVM test corpus in one process. Parallel execution replaces only
;; the :jvm gate with the individually discovered JVM namespaces. Every other
;; gate is unchanged, including Chromium's workspace-local build requirement.
;;
;; No cross-clone prerequisites imply shared generated files: each Gesso gate
;; executes its existing standalone command in its own clone. Namespace jobs
;; likewise validate repository integrity before starting a JVM. A namespace
;; must be in the exact discovered JVM test inventory to be executable.
(defn shard-gate-job
  [gate]
  (let [gate-name (name gate)]
    {:id (str "gesso-" (str/replace gate-name ":" "-"))
     :gate gate
     :runtime :gesso
     :unit {:type :gesso-gate :name gate-name}
     :command ["bb" "script/gesso_test.clj" gate-name]
     :depends-on []
     :timeout-ms 3600000
     :requires #{}}))

(defn shard-job-inventory
  []
  (let [gates (command-gates "all")]
    (when-not
     (and (vector? gates)
          (seq gates)
          (= :integrity (first gates))
          (= 1 (count (filter #{:jvm} gates)))
          (= (count gates) (count (distinct gates)))
          (every? (fn [gate]
                    (and (keyword? gate)
                         (fn? (get-in gate-specs [gate :run]))
                         (string? (get-in gate-specs [gate :label]))))
                  gates))
      (fail!
       "Cannot enumerate an invalid or incomplete full-suite gate definition."
       {:gates gates :known-gates (set (keys gate-specs))}))

    ;; Construct the JVM inventory from exactly the same strict source scan
    ;; exposed by jvm:inventory, not from a second manually maintained list.
    (let [jvm-jobs (:jobs (shard-jvm-namespace-inventory))
          jobs (vec (mapcat (fn [gate]
                              (if (= :jvm gate)
                                jvm-jobs
                                [(shard-gate-job gate)]))
                            gates))
          ids (mapv :id jobs)
          represented-gates (set (map :gate jobs))]
      (when-not
       (and (seq jvm-jobs)
            (= (count jvm-jobs)
               (count (set (map :id jvm-jobs))))
            (= (set gates) represented-gates)
            (= (count jobs) (count (distinct ids)))
            (= (count jobs) (+ (dec (count gates)) (count jvm-jobs)))
            (every? (fn [job]
                      (and (= :jvm (:gate job))
                           (= :gesso-jvm-namespace (get-in job [:unit :type]))))
                    jvm-jobs))
        (fail!
         "Fine-grained JVM inventory does not cover the complete parallel suite."
         {:gates gates
          :represented-gates represented-gates
          :jvm-job-count (count jvm-jobs)
          :job-count (count jobs)
          :distinct-id-count (count (distinct ids))}))
      {:jobs jobs})))

;; Staged v1088 interface for Shard v1089. The existing zero-argument
;; `inventory` remains unchanged until Shard enforces invocation-level preflight.
;; This prepared inventory contains NO :integrity job and NO job command that
;; invokes the ordinary integrity-bearing public gate. Only Shard may admit
;; these commands after one successful preflight for this exact Git HEAD.
(def prepared-standalone-gates
  #{:advanced :node:choreo :node:browser :browser
    :browser:integration :themes})

;; v1123: The prepared parallel inventory has TWO fundamentally different
;; execution types. JVM namespace jobs are independently claimed *commands*
;; addressed to an already-running, persistent JVM; the six other gates retain
;; their isolated subprocess behavior. Keep the legacy unprepared inventory
;; unchanged for compatibility with direct `jvm:namespace` execution.
;;
;; The session command requires shard.worker.jvm on the launched Clojure
;; classpath. Gesso does not own Shard's source and must not copy or silently
;; replace its worker. The Shard host must supply that source on the JVM
;; classpath before enabling native execution (a subsequent integration gate).
(def prepared-jvm-runtime :gesso-jvm)
(def prepared-subprocess-runtime :gesso)
(def prepared-jvm-session-command
  ["clojure" "-M:test:formal" "-m" "shard.worker.jvm"])

(defn prepared-runtime-descriptors [jobs]
  (let [used (set (map :runtime jobs))]
    (when-not (every? #{prepared-jvm-runtime prepared-subprocess-runtime} used)
      (fail! "Prepared inventory contains an unknown runtime."
             {:runtimes used}))
    (cond-> {}
      (contains? used prepared-jvm-runtime)
      (assoc prepared-jvm-runtime
             {:identity "gesso-clojure-1.12.5-test-formal-shard-protocol-v1"
              :capabilities #{}
              :execution :persistent-jvm
              :session-command prepared-jvm-session-command})
      (contains? used prepared-subprocess-runtime)
      (assoc prepared-subprocess-runtime
             {:identity "gesso-standalone-prepared-gates-v1"
              :capabilities #{}
              :execution :subprocess}))))

(defn prepared-jvm-job
  [test-ns gate]
  (let [n (str test-ns)
        family (if (= gate :formal) "formal" "jvm")]
    {:id (str "gesso-" family "-" n)
     :gate gate
     :runtime prepared-jvm-runtime
     :unit {:type :gesso-jvm-namespace :name n}
     ;; A deliberately nonempty compatibility/audit command. Shard's
     ;; :persistent-jvm executor NEVER invokes this per-namespace subprocess.
     ;; Retain it until the scheduler's prepared-inventory admission and
     ;; historical command contracts have been migrated together.
     :command ["bb" "script/gesso_test.clj" "jvm:namespace:prepared" n]
     :depends-on []
     :timeout-ms jvm-wall-clock-timeout-ms
     :requires #{}}))

(defn prepared-gate-job
  [gate]
  (when-not (contains? prepared-standalone-gates gate)
    (fail! "Unsupported prepared gate." {:gate gate}))
  (assoc (shard-gate-job gate)
         :command ["bb" "script/gesso_test.clj" "gate:prepared" (name gate)]))

(def prepared-inventory-scopes
  #{"all" "quick" "jvm" "formal" "cljs" "node" "node:choreo" "node:browser"
    "browser" "browser:integration" "advanced" "themes"})

(defn shard-prepared-inventory
  [scope]
  (let [gates (command-gates scope)]
    (when-not (and (vector? gates)
                   (= :integrity (first gates))
                   (seq (rest gates)))
      (fail! "Unknown or empty Shard inventory scope."
             {:scope scope}))
    ;; `formal` is a subset of the authoritative JVM namespace corpus; no
    ;; separate manually maintained namespace list or repeated integrity gate.
    (let [selected-gates (if (= scope "browser:integration")
                           [:browser:integration]
                           (rest gates))
          selected (cond
                     (= scope "formal")
                     (mapv #(prepared-jvm-job % :formal)
                           (formal-test-namespaces))
                     (= scope "quick")
                     (mapv #(prepared-jvm-job % :jvm)
                           (quick-test-namespaces))
                     :else
                     (vec (mapcat (fn [gate]
                                    (if (= :jvm gate)
                                      (mapv #(prepared-jvm-job % :jvm)
                                            (jvm-test-namespaces))
                                      [(prepared-gate-job gate)]))
                                  selected-gates)))
          ids (mapv :id selected)]
      (when-not (and (seq selected)
                     (= (count ids) (count (distinct ids)))
                     (every? #(not= :integrity (:gate %)) selected)
                     (every? (fn [{:keys [command gate runtime unit]}]
                               (and (contains? #{"jvm:namespace:prepared"
                                                 "gate:prepared"}
                                               (nth command 2 nil))
                                    (if (#{:jvm :formal} gate)
                                      (and (= prepared-jvm-runtime runtime)
                                           (= :gesso-jvm-namespace (:type unit))
                                           (= "jvm:namespace:prepared" (nth command 2)))
                                      (and (= prepared-subprocess-runtime runtime)
                                           (= :gesso-gate (:type unit))
                                           (= "gate:prepared" (nth command 2))))))
                             selected))
        (fail! "Invalid preflight-ready inventory."
               {:scope scope :job-count (count selected)}))
      {:runtimes (prepared-runtime-descriptors selected)
       :jobs selected})))

(defn run-prepared-gate!
  [name]
  (let [gate (keyword name)]
    (when-not (contains? prepared-standalone-gates gate)
      (fail! "Unknown prepared gate; refusing arbitrary gate execution."
             {:requested name}))
    (require-preflight-admission!)
    ;; A browser integration command needs its own workspace-local advanced
    ;; build; an advanced job on another clone cannot provide that artifact.
    ;; Neither path repeats invocation-level repository integrity.
    (let [gates (if (= gate :browser:integration)
                  [:advanced :browser:integration]
                  [gate])
          results (run-gates! gates)]
      (when (failed-gates? results)
        (System/exit 1)))))

(defn -main
  []
  (ensure-project-root!)
  (let [args (vec *command-line-args*)
        command (or (first args) "all")]
    (case command
      "inventory"
      (case (count args)
        1 ;; Legacy inventory keeps the old per-job integrity checks until
          ;; Shard's v1089 preflight barrier becomes the execution owner.
        (prn (shard-job-inventory))
        2 (do
            (when-not (contains? prepared-inventory-scopes (second args))
              (usage!))
            (prn (shard-prepared-inventory (second args))))
        (usage!))

      "inventory:prepared"
      (do
        (when-not (and (= 2 (count args))
                       (contains? prepared-inventory-scopes (second args)))
          (usage!))
        (prn (shard-prepared-inventory (second args))))

      "quick"
      (do
        (when-not (= 1 (count args)) (usage!))
        (run-quick-tests!))

      "preflight"
      (do
        (when-not (= 1 (count args)) (usage!))
        (run-repo-integrity!))

      "gate:prepared"
      (do
        (when-not (= 2 (count args)) (usage!))
        (run-prepared-gate! (second args)))

      "jvm:inventory"
      (do
        (when-not (= 1 (count args)) (usage!))
        ;; Strict EDN only; namespace discovery never launches a test.
        (prn (shard-jvm-namespace-inventory)))

      "jvm:namespace"
      (do
        (when-not (= 2 (count args)) (usage!))
        (try
          (run-one-jvm-test-namespace! (second args))
          (catch Throwable error
            (if (gate-failure? error)
              (do
                (binding [*out* *err*]
                  (println (ex-message error)))
                (System/exit 1))
              (throw error)))))

      "jvm:namespace:prepared"
      (do
        (when-not (= 2 (count args)) (usage!))
        (try
          (run-one-jvm-test-namespace! (second args) true)
          (catch Throwable error
            (if (gate-failure? error)
              (do
                (binding [*out* *err*]
                  (println (ex-message error)))
                (System/exit 1))
              (throw error)))))

      (do
        (when (> (count args) 1) (usage!))
        (let [gates (command-gates command)]
          (when-not gates (usage!))
          (let [results (run-gates! gates)]
            (when (failed-gates? results)
              ;; Ordinary gate failures exit nonzero without Babashka traces.
              ;; Unexpected harness/configuration exceptions still escape.
              (System/exit 1))))))))

(-main)
