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

(defn fail!
  [message data]
  (throw
   (ex-info
    message
    data)))

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
      (fail!
       "External test command failed."
       {:command command
        :exit (:exit result)}))

    result))

(defn run-command-captured!
  [command]
  (let [result
        @(process/process
          command
          {:in :inherit
           :out :string
           :err :string})]

    (when-not
     (zero?
      (:exit result))
      (do
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

        (fail!
         "External test command failed."
         {:command command
          :exit (:exit result)})))

    result))

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
        ;; production Closure optimization is verified separately by
        ;; run-advanced-compile!.
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

(defn run-node-tests!
  []
  (run-node-test-groups!
   [:choreo
    :browser]))

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
        (cljs-test-namespaces)

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

    (delete-tree-if-exists!
     browser-dir)

    (fs/create-dirs
     browser-dir)

    (write-browser-runner!
     namespaces)

    ;; :simple deliberately produces a self-contained browser program. This
    ;; avoids depending on the Closure development loader over file:// URLs.
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

          result
          (run-command-captured!
           command)

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
          (fail!
           "CLJS / Chromium assertions failed."
           {:dump
            dump-file}))

        (str/includes?
         dumped-dom
         "data-gesso-test-status=\"runtime-error\"")
        (do
          (println
           dumped-dom)
          (fail!
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

          (fail!
           "Chromium exited without a completed cljs.test run."
           {:dump
            dump-file}))))))

(defn run-advanced-compile!
  []
  (println)
  (println
   "== Production advanced compile ==")

  (let [advanced-dir
        (str
         test-work-dir
         "/advanced")]

    (delete-tree-if-exists!
     advanced-dir)

    (fs/create-dirs
     advanced-dir)

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
        (str
         advanced-dir
         "/out")})
      "-o"
      (str
       advanced-dir
       "/gesso-live.js")
      "-c"
      "gesso.live.browser.runtime"])))

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
      (fail!
       "Theme build completed without creating its expected output."
       {:output-file
        output-file}))

    (println
     "Theme build: PASS")))

(defn run-cljs-tests!
  []
  (run-node-tests!)
  (run-browser-tests!))

(defn run-all!
  []
  (run-jvm-tests!)
  (run-cljs-tests!)
  (run-advanced-compile!)
  (run-theme-build!)

  (println)
  (println
   "All Gesso JVM tests, CLJS tests, browser tests, production compilation, and theme build passed."))

(defn usage!
  []
  (println
   "Usage: bb script/gesso_test.clj [all|jvm|cljs|node|node:choreo|node:browser|browser|advanced|themes]")
  (System/exit
   2))

(defn -main
  []
  (ensure-project-root!)

  (case
   (or
    (first
     *command-line-args*)
    "all")

    "all"
    (run-all!)

    "jvm"
    (run-jvm-tests!)

    "cljs"
    (run-cljs-tests!)

    "node"
    (run-node-tests!)

    "node:choreo"
    (run-node-choreo-tests!)

    "node:browser"
    (run-node-browser-tests!)

    "browser"
    (run-browser-tests!)

    "advanced"
    (run-advanced-compile!)

    "themes"
    (run-theme-build!)

    (usage!)))

(-main)
