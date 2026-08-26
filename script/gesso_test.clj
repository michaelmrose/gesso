(ns gesso.script.gesso-test
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [clojure.edn :as edn]
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
  180000)

(def browser-test-namespaces-env
  "GESSO_BROWSER_TEST_NAMESPACES")

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
         "test_all_browser_runner.cljs")]

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
      "(defn main\n"
      "  []\n"
      "  (t/run-tests\n"
      "   (t/empty-env ::reporter)"
      (quoted-test-args
       namespaces)
      "))\n\n"
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

(defn run-jvm-tests!
  []
  (println)
  (println
   "== JVM tests ==")

  (run-command!
   ["clojure"
    "-M:test"]))

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

(defn run-browser-tests!
  []
  (println)
  (println
   "== CLJS / Chromium tests ==")

  (let [namespaces
        (browser-test-namespaces)

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

    (let [url
          (str
           "file://"
           (fs/absolutize
            html))

          base-command
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

          command
          (cond->
           base-command

           (= "root"
              (System/getProperty
               "user.name"))
           (conj
            "--no-sandbox")

           true
           (conj
            url))

          started-at
          (monotonic-nanos)

          _
          (println
           "Chromium execution: START"
           (str
            "(wall timeout "
            (/ browser-wall-clock-timeout-ms
               1000)
            "s)"))

          result
          (run-command-captured-with-timeout!
           command
           browser-wall-clock-timeout-ms)

          _
          (print-stage-pass!
           "Chromium execution"
           started-at)

          dumped-dom
          (:out result)]

      (spit
       dump-file
       dumped-dom)

      (cond
        (str/includes?
         dumped-dom
         "data-gesso-test-status=\"pass\"")
        (println
         "CLJS / Chromium: PASS")

        (str/includes?
         dumped-dom
         "data-gesso-test-status=\"fail\"")
        (do
          (println
           dumped-dom)
          (gate-failure!
           "CLJS / Chromium assertions failed."
           {:dump
            dump-file}))

        (str/includes?
         dumped-dom
         "data-gesso-test-status=\"runtime-error\"")
        (do
          (println
           dumped-dom)
          (gate-failure!
           "CLJS / Chromium encountered an uncaught runtime error."
           {:dump
            dump-file}))

        :else
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
           {:dump
            dump-file}))))))

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
  {:jvm
   {:label "JVM"
    :run run-jvm-tests!}

   :node:choreo
   {:label "Node Choreo"
    :run run-node-choreo-tests!}

   :node:browser
   {:label "Node Browser"
    :run run-node-browser-tests!}

   :browser
   {:label "Chromium"
    :run run-browser-tests!}

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
   [:node:choreo
    :node:browser
    :browser]))

(defn run-node-tests!
  []
  (run-gates!
   [:node:choreo
    :node:browser]))

(defn run-all!
  []
  (run-gates!
   [:advanced
    :jvm
    :node:choreo
    :node:browser
    :browser
    :themes]))

(defn usage!
  []
  (println
   "Usage: bb script/gesso_test.clj [all|jvm|cljs|node|node:choreo|node:browser|browser|advanced|themes]")
  (System/exit
   2))

(defn command-gates
  [command]
  (case command
    "all"
    [:advanced
     :jvm
     :node:choreo
     :node:browser
     :browser
     :themes]

    "jvm"
    [:jvm]

    "cljs"
    [:node:choreo
     :node:browser
     :browser]

    "node"
    [:node:choreo
     :node:browser]

    "node:choreo"
    [:node:choreo]

    "node:browser"
    [:node:browser]

    "browser"
    [:browser]

    "advanced"
    [:advanced]

    "themes"
    [:themes]

    nil))

(defn -main
  []
  (ensure-project-root!)
  (let [command (or (first *command-line-args*) "all")
        gates (command-gates command)]
    (when-not gates
      (usage!))
    (let [results (run-gates! gates)]
      (when (failed-gates? results)
        ;; Ordinary test/build gate failures are expected process outcomes, not
        ;; harness exceptions. Exit nonzero without printing a Babashka stack
        ;; trace. Unexpected harness/configuration exceptions still escape.
        (System/exit 1)))))

(-main)