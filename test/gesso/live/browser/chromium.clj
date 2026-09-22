(ns gesso.live.browser.chromium
  "Playwright-backed control for Gesso real-browser integration tests.

   This is test infrastructure only. It owns Chromium process/context/page
   control and browser diagnostics. Gesso semantics stay in the production
   browser runtime, while deterministic HTTP/SSE ordering stays in
   gesso.live.browser.fixture-server."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (com.microsoft.playwright
    Browser
    BrowserContext
    BrowserType$LaunchOptions
    ConsoleMessage
    Page
    Playwright
    Playwright$CreateOptions
    Request
    TimeoutError
    Tracing$StartOptions
    Tracing$StopOptions
    WebError)
   (com.microsoft.playwright.options
    WebErrorLocation)
   (java.io File)
   (java.util Collections WeakHashMap)
   (java.util.function Consumer)))

(def ^:private error-type
  :gesso.live.browser.chromium/error)

(def ^:private default-browser-commands
  ["chromium"
   "chromium-browser"
   "google-chrome"
   "google-chrome-stable"])

(def ^:private default-timeout-ms 10000)
(def ^:private default-launch-timeout-ms 20000)
(def ^:private fatal-console-types #{"assert" "error"})

(def ^:private unhandled-rejection-prefix
  "[gesso-browser-harness:unhandled-rejection] ")

(defonce ^:private page-diagnostic-states
  ;; Page objects are physical browser resources. Keep only weak keys so test
  ;; diagnostics never become another owner that can prolong a page/context
  ;; lifetime after Playwright cleanup.
  (Collections/synchronizedMap (WeakHashMap.)))

(def ^:private browser-init-script
  (str
   "(() => {\n"
   "  const key = '__gessoBrowserHarnessUnhandledRejectionInstalled';\n"
   "  if (globalThis[key]) return;\n"
   "  Object.defineProperty(globalThis, key, {value: true});\n"
   "  globalThis.addEventListener('unhandledrejection', (event) => {\n"
   "    const reason = event.reason;\n"
   "    let detail;\n"
   "    try {\n"
   "      detail = reason && reason.stack ? String(reason.stack) : String(reason);\n"
   "    } catch (_) {\n"
   "      detail = '<unprintable rejection reason>';\n"
   "    }\n"
   "    console.error("
   (pr-str unhandled-rejection-prefix)
   " + detail);\n"
   "  });\n"
   "})();\n"))

(defn- fail!
  ([kind message]
   (fail! kind message {}))
  ([kind message data]
   (throw
    (ex-info
     message
     (merge {:error/type error-type
             :error/kind kind}
            data)))))

(defn- executable-path
  [value]
  (let [file (io/file (str value))]
    (when (and (.isFile ^File file)
               (.canExecute ^File file))
      (.toAbsolutePath (.toPath ^File file)))))

(defn- path-directories
  []
  (let [value (or (System/getenv "PATH") "")
        separator (re-pattern
                   (java.util.regex.Pattern/quote File/pathSeparator))]
    (if (str/blank? value)
      []
      (str/split value separator -1))))

(defn- command-path
  [command]
  (some
   (fn [directory]
     (executable-path
      (io/file (if (str/blank? directory) "." directory)
               command)))
   (path-directories)))

(defn- resolve-executable
  [value]
  (or (executable-path value)
      (when-not (str/includes? (str value) File/separator)
        (command-path value))))

(defn chromium-executable
  "Returns the Chromium-family executable used by browser tests.

   Resolution order is explicit :override, GESSO_CHROMIUM, then the standard
   Chromium/Chrome command names on PATH. An explicit override is authoritative:
   a bad override fails instead of silently selecting a different browser."
  ([]
   (chromium-executable {}))
  ([{:keys [override candidates]
     :or {candidates default-browser-commands}}]
   (let [override (or override
                      (some-> (System/getenv "GESSO_CHROMIUM")
                              str/trim
                              not-empty))]
     (if override
       (or (resolve-executable override)
           (fail!
            :invalid-chromium-override
            (str "Chromium override is not executable: " override
                 ". Set GESSO_CHROMIUM to an executable path or command on PATH.")
            {:override (str override)}))
       (or (some resolve-executable candidates)
           (fail!
            :chromium-not-found
            (str "No Chromium-family executable was found for Gesso browser tests. "
                 "Install Chromium/Chrome or set GESSO_CHROMIUM. Checked: "
                 (str/join ", " candidates) ".")
            {:checked-commands (vec candidates)}))))))

(defn start!
  "Starts Playwright and launches system Chromium.

   Options:
   - :chromium-executable  explicit executable path/command
   - :headless?            defaults true
   - :launch-timeout-ms    defaults 20 seconds
   - :args                 additional Chromium arguments"
  ([]
   (start! {}))
  ([opts]
   (let [requested-executable (:chromium-executable opts)
         headless? (get opts :headless? true)
         launch-timeout-ms (get opts :launch-timeout-ms
                                default-launch-timeout-ms)
         args (vec (get opts :args []))
         chromium-path
         (if requested-executable
           (or (resolve-executable requested-executable)
               (fail!
                :invalid-chromium-executable
                (str "Requested Chromium executable is unavailable: "
                     requested-executable)
                {:chromium-executable (str requested-executable)}))
           (chromium-executable))
         playwright-env
         (assoc
          (into {} (System/getenv))
          "PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD"
          "1")
         create-options
         (doto (Playwright$CreateOptions.)
           (.setEnv playwright-env))
         playwright
         (try
           (Playwright/create create-options)
           (catch Throwable cause
             (throw
              (ex-info
               (str "Playwright could not start for Gesso browser tests. "
                    "Gesso suppresses Playwright-managed browser downloads because "
                    "these tests launch the resolved system Chromium executable. "
                    "Ensure the Playwright test dependency and driver bundle are available.")
               {:error/type error-type
                :error/kind :playwright-start-failed}
               cause))))]
     (try
       (let [launch-options
             (doto (BrowserType$LaunchOptions.)
               (.setExecutablePath chromium-path)
               (.setHeadless (boolean headless?))
               (.setTimeout (double launch-timeout-ms))
               (.setArgs args))
             browser (.launch (.chromium ^Playwright playwright)
                              launch-options)]
         {:playwright playwright
          :browser browser
          :chromium-executable chromium-path
          :headless? (boolean headless?)})
       (catch Throwable cause
         (try
           (.close ^Playwright playwright)
           (catch Throwable _))
         (throw
          (ex-info
           (str "Playwright failed to launch Chromium at " chromium-path ". "
                "If the installed browser is incompatible with Playwright, "
                "set GESSO_CHROMIUM to the intended executable explicitly.")
           {:error/type error-type
            :error/kind :chromium-launch-failed
            :chromium-executable (str chromium-path)
            :headless? (boolean headless?)}
           cause)))))))

(defn stop!
  "Closes Chromium and Playwright. Cleanup failures are reported because leaked
   browser processes can make later integration tests misleading."
  [{:keys [browser playwright]}]
  (let [errors (atom [])]
    (when browser
      (try
        (.close ^Browser browser)
        (catch Throwable cause
          (swap! errors conj [:browser cause]))))
    (when playwright
      (try
        (.close ^Playwright playwright)
        (catch Throwable cause
          (swap! errors conj [:playwright cause]))))
    (when (seq @errors)
      (let [[resource cause] (first @errors)]
        (throw
         (ex-info
          (str "Gesso Chromium cleanup failed for " (count @errors)
               " resource(s); first failure was " (name resource) ".")
          {:error/type error-type
           :error/kind :cleanup-failed
           :resources (mapv first @errors)}
          cause))))
    nil))

(defn- consumer
  [f]
  (reify Consumer
    (accept [_ value]
      (f value))))

(defn- safe-page-url
  [page]
  (when page
    (try
      (.url ^Page page)
      (catch Throwable _
        nil))))

(defn- record!
  [state key value]
  (swap! state update key conj value)
  nil)

(defn- console-entry
  [^ConsoleMessage message]
  (let [text (.text message)
        unhandled-rejection?
        (and (string? text)
             (str/starts-with? text unhandled-rejection-prefix))]
    (cond->
     {:type (.type message)
      :text text
      :location (.location message)
      :timestamp (.timestamp message)
      :page-url (safe-page-url (.page message))}
      unhandled-rejection?
      (assoc
       :kind :unhandled-promise-rejection
       :reason
       (subs text (count unhandled-rejection-prefix))))))

(defn- web-error-location
  [^WebErrorLocation location]
  (when location
    {:url (.-url location)
     :line (.-line location)
     :column (.-column location)}))

(defn- web-error-entry
  [^WebError error]
  {:error (.error error)
   :location (web-error-location (.location error))
   :page-url (safe-page-url (.page error))})

(defn- request-failure-entry
  [^Request request]
  {:method (.method request)
   :url (.url request)
   :resource-type (.resourceType request)
   :failure (.failure request)})

(defn- instrument-page!
  [state ^Page page]
  ;; Associate the page with its context diagnostic atom for bounded-wait
  ;; failure reporting. Weak keys prevent this registry from owning pages.
  (.put page-diagnostic-states page state)
  (let [identity (System/identityHashCode page)
        install? (atom false)]
    (swap!
     state
     (fn [current]
       (if (contains? (:instrumented-pages current) identity)
         current
         (do
           (reset! install? true)
           (update current :instrumented-pages conj identity)))))
    (when @install?
      (.onCrash
       page
       (consumer
        (fn [crashed-page]
          (record! state
                   :page-crashes
                   {:page-url (safe-page-url crashed-page)})))))
    page))

(defn new-context!
  "Creates an isolated browser context with diagnostics installed.

   Separate contexts have separate cookies/storage and are the unit used later
   for independent HumanHelp/Gesso browser actors."
  ([harness]
   (new-context! harness {}))
  ([{:keys [browser]} opts]
   (when-not browser
     (fail! :missing-browser
            "Cannot create a Chromium context from a harness with no browser."))
   (let [timeout-ms (get opts :timeout-ms default-timeout-ms)
         navigation-timeout-ms (get opts :navigation-timeout-ms timeout-ms)
         context (.newContext ^Browser browser)
         state (atom {:console []
                      :web-errors []
                      :request-failures []
                      :page-crashes []
                      :instrumented-pages #{}
                      :trace-active? false})]
     (.setDefaultTimeout ^BrowserContext context (double timeout-ms))
     (.setDefaultNavigationTimeout ^BrowserContext context
                                   (double navigation-timeout-ms))

     ;; Playwright's WebError event gives us unhandled synchronous page
     ;; exceptions. Install one independent browser-side listener as well so
     ;; an unhandled Promise rejection cannot disappear merely because the
     ;; browser/Playwright version reports it only through console machinery.
     ;; The listener observes and reports; it does not preventDefault or alter
     ;; application semantics.
     (.addInitScript ^BrowserContext context browser-init-script)

     (.onConsoleMessage
      ^BrowserContext context
      (consumer #(record! state :console (console-entry %))))
     (.onWebError
      ^BrowserContext context
      (consumer #(record! state :web-errors (web-error-entry %))))
     (.onRequestFailed
      ^BrowserContext context
      (consumer #(record! state :request-failures
                          (request-failure-entry %))))
     (.onPage
      ^BrowserContext context
      (consumer #(instrument-page! state %)))
     {:context context
      :state state
      :timeout-ms timeout-ms
      :navigation-timeout-ms navigation-timeout-ms})))

(defn close-context!
  "Closes a context. Active tracing is stopped and discarded unless stop-trace!
   was called first with an output path."
  [{:keys [context state]}]
  (when (and context state (:trace-active? @state))
    (try
      (.stop (.tracing ^BrowserContext context))
      (finally
        (swap! state assoc :trace-active? false))))
  (when context
    (.close ^BrowserContext context))
  nil)

(defn new-page!
  "Creates and instruments a page in an isolated context."
  [{:keys [context state]}]
  (when-not context
    (fail! :missing-context
           "Cannot create a Chromium page without a browser context."))
  (instrument-page! state (.newPage ^BrowserContext context)))

(defn navigate!
  "Navigates to an absolute URL and returns the Page."
  [^Page page url]
  (.navigate page (str url))
  page)

(defn evaluate
  "Evaluates JavaScript and returns the Playwright-decoded value."
  ([^Page page expression]
   (.evaluate page (str expression)))
  ([^Page page expression argument]
   (.evaluate page (str expression) argument)))

(defn- bounded-string
  [value limit]
  (let [value (str value)]
    (if (<= (count value) limit)
      value
      (str (subs value 0 limit) "…"))))

(defn- diagnostics-from-state
  [state]
  (when state
    (dissoc @state :instrumented-pages :trace-active?)))

(defn- page-diagnostic-state
  [^Page page]
  (.get page-diagnostic-states page))

(defn- safe-page-snapshot
  [^Page page]
  (try
    {:url (safe-page-url page)
     :document
     (.evaluate
      page
      (str
       "() => ({"
       "readyState: document.readyState,"
       "title: document.title,"
       "bodyText: document.body ? document.body.innerText.slice(0, 1200) : null"
       "})"))}
    (catch Throwable snapshot-error
      {:url (safe-page-url page)
       :snapshot-error (.getMessage snapshot-error)})))

(defn- wait-timeout!
  [^Page page expression cause]
  (let [state (page-diagnostic-state page)
        captured (diagnostics-from-state state)]
    (throw
     (ex-info
      (str
       "Timed out waiting for a browser predicate. The failure report includes "
       "the predicate, current page snapshot, and diagnostics already observed "
       "by the owning BrowserContext so a root runtime error is not hidden "
       "behind the downstream timeout.")
      {:error/type error-type
       :error/kind :wait-for-js-timeout
       :expression (bounded-string expression 1200)
       :page (safe-page-snapshot page)
       :diagnostics captured}
      cause))))

(defn wait-for-js!
  "Waits for a JavaScript expression/function to become truthy.

   The context timeout bounds the wait; this helper never adds sleeps. If the
   predicate times out, the thrown ExceptionInfo preserves Playwright's timeout
   as its cause and adds the current page snapshot plus every browser diagnostic
   already captured by the page's BrowserContext. This prevents a real runtime
   failure from degrading into an opaque downstream wait timeout."
  ([^Page page expression]
   (try
     (.waitForFunction page (str expression))
     page
     (catch TimeoutError cause
       (wait-timeout! page expression cause))))
  ([^Page page expression argument]
   (try
     (.waitForFunction page (str expression) argument)
     page
     (catch TimeoutError cause
       (wait-timeout! page expression cause)))))

(defn diagnostics
  "Returns observable diagnostics while hiding harness bookkeeping."
  [{:keys [state]}]
  (dissoc @state :instrumented-pages :trace-active?))

(defn clear-diagnostics!
  [{:keys [state]}]
  (swap! state assoc
         :console []
         :web-errors []
         :request-failures []
         :page-crashes [])
  nil)

(defn browser-errors
  "Returns unexpected browser failures. Deliberate network failures are omitted
   because fault scenarios intentionally create them; inspect :request-failures
   in diagnostics when network behavior matters.

   Unhandled Promise rejections are classified separately from ordinary console
   errors so a failure report says what actually happened rather than reducing
   every browser problem to `console.error`."
  [context-harness]
  (let [{:keys [console web-errors page-crashes]}
        (diagnostics context-harness)]
    (into []
          cat
          [(->> console
                (filter #(contains? fatal-console-types (:type %)))
                (map
                 (fn [entry]
                   (if (:kind entry)
                     entry
                     (assoc entry :kind :console-error)))))
           (map #(assoc % :kind :web-error) web-errors)
           (map #(assoc % :kind :page-crash) page-crashes)])))

(defn assert-clean!
  "Fails with a structured explanation if the page logged a console error/assert,
   produced an unhandled Promise rejection, threw an uncaught exception, or
   crashed."
  [context-harness]
  (let [errors (browser-errors context-harness)]
    (when (seq errors)
      (fail!
       :browser-errors
       (str "Chromium produced " (count errors)
            " unexpected browser failure(s). Each entry includes its failure kind "
            "and, when Playwright provides it, source/page location. Inspect "
            ":errors and :diagnostics. Request failures are recorded separately "
            "because adversarial tests may cause them intentionally.")
       {:errors errors
        :diagnostics (diagnostics context-harness)}))
    true))

(defn start-trace!
  "Starts a Playwright trace. Screenshots and DOM/network snapshots default on."
  ([context-harness]
   (start-trace! context-harness {}))
  ([{:keys [context state] :as context-harness} opts]
   (when (:trace-active? @state)
     (fail! :trace-already-active
            "Playwright tracing is already active for this browser context."))
   (let [options (doto (Tracing$StartOptions.)
                   (.setScreenshots (boolean (get opts :screenshots? true)))
                   (.setSnapshots (boolean (get opts :snapshots? true)))
                   (.setSources (boolean (get opts :sources? false))))]
     (when-let [title (:title opts)]
       (.setTitle options (str title)))
     (.start (.tracing ^BrowserContext context) options)
     (swap! state assoc :trace-active? true)
     context-harness)))

(defn stop-trace!
  "Stops tracing and writes a trace zip to output-path."
  [{:keys [context state] :as context-harness} output-path]
  (when-not (:trace-active? @state)
    (fail! :trace-not-active
           "Cannot stop a Playwright trace because tracing is not active."))
  (let [file (io/file (str output-path))]
    (some-> (.getParentFile ^File file) .mkdirs)
    (try
      (.stop (.tracing ^BrowserContext context)
             (doto (Tracing$StopOptions.)
               (.setPath (.toPath ^File file))))
      (finally
        (swap! state assoc :trace-active? false))))
  context-harness)
