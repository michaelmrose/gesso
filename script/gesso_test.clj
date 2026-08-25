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

;; Every CLJS/CLJC test is run in Chromium. These host-independent suites
;; additionally run under Node so accidental browser dependencies and
;; host-specific semantic drift are caught early.
(def node-test-namespaces
  '#{gesso.live.browser.adapter-test
     gesso.live.browser.adapter-property-test
     gesso.live.browser.choreo-test
     gesso.live.browser.core-test
     gesso.live.browser.fx-conformance-test
     gesso.live.browser.fx-test
     gesso.live.browser.shell-test})

(defn existing-node-test-namespaces
  []
  (let [all-tests
        (set
         (cljs-test-namespaces))]

    (->> node-test-namespaces
         (filter
          all-tests)
         sort
         vec)))

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

(defn write-node-runner!
  [namespaces]
  (let [path
        (fs/path
         generated-test-src
         "gesso"
         "test_all_node_runner.cljs")]

    (fs/create-dirs
     (fs/parent
      path))

    (spit
     (str
      path)
     (str
      "(ns gesso.test-all-node-runner\n"
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

    path))

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

(defn run-node-tests!
  []
  (println)
  (println
   "== CLJS / Node tests ==")

  (let [namespaces
        (existing-node-test-namespaces)

        node-dir
        (str
         test-work-dir
         "/node")

        output-dir
        (str
         node-dir
         "/out")

        output-to
        (str
         node-dir
         "/tests.js")]

    (when
     (empty?
      namespaces)
      (fail!
       "No Node-compatible CLJS tests were discovered."
       {}))

    (println
     "Namespaces:"
     (count
      namespaces))

    (delete-tree-if-exists!
     node-dir)

    (fs/create-dirs
     node-dir)

    (write-node-runner!
     namespaces)

    (run-command!
     (cljs-main-command
      "-co"
      (pr-str
       {:target :nodejs
        :optimizations :simple
        :output-dir output-dir
        :output-to output-to})
      "-c"
      "gesso.test-all-node-runner"))

    (run-command!
     ["node"
      output-to])))

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
      "gesso.live.browser.core"])))

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
   "Usage: bb script/gesso_test.clj [all|jvm|cljs|node|browser|advanced|themes]")
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

    "browser"
    (run-browser-tests!)

    "advanced"
    (run-advanced-compile!)

    "themes"
    (run-theme-build!)

    (usage!)))

(-main)
