(ns gesso.live.browser.chromium-integration
  "Real Chromium integration scenarios for the Gesso Live browser boundary.

   This namespace is intentionally *not* named ...-test. The ordinary JVM test
   gate must remain independent of Chromium. The browser gate invokes -main
   explicitly after building the production gesso-live.js artifact.

   These scenarios exercise the actual browser integration stack:

     system Chromium
       -> Playwright control only
       -> real HTMX 2.0.7
       -> real htmx-ext-sse 2.2.4 / EventSource
       -> production resources/public/js/gesso-live.js
       -> real HTTP/SSE sockets
       -> deterministic gesso.live.browser.fixture-server

   Playwright does not mock network behavior here. fixture-server owns network
   ordering so the physical HTTP/SSE boundary remains real."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is run-tests testing]]
   [gesso.live.browser.chromium :as chromium]
   [gesso.live.browser.fixture-server :as fixture]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]
   [gesso.live.ui :as live.ui]
   [rum.core :as rum]))

(def ^:private client-id
  "progression-browser")

(def ^:private fragment-id
  "fixture-fragment")

(def ^:private page-path
  "/")

(def ^:private fragment-path
  "/fragment")

(def ^:private htmx-path
  "/assets/htmx-2.0.7.js")

(def ^:private sse-extension-path
  "/assets/htmx-ext-sse-2.2.4.js")

(def ^:private gesso-runtime-path
  "/assets/gesso-live.js")

(def ^:private javascript-headers
  {"content-type" "text/javascript; charset=utf-8"
   "cache-control" "no-store"})

(def ^:private html-headers
  {"content-type" "text/html; charset=utf-8"
   "cache-control" "no-store"})

(def ^:private htmx-resource-candidates
  ["META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.min.js"
   "META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.js"])

(def ^:private sse-resource-candidates
  ["META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.min.js"
   "META-INF/resources/webjars/htmx-ext-sse/2.2.4/sse.js"])

(defn- integration-error
  ([kind message data]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.chromium-integration/error
      :error/kind kind}
     data)))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.chromium-integration/error
      :error/kind kind}
     data)
    cause)))

(defn- resource-text!
  [label candidates]
  (if-let [resource
           (some io/resource candidates)]
    (slurp resource)
    (throw
     (integration-error
      :missing-browser-asset
      (str label " is unavailable on the test classpath. "
           "The real-browser gate deliberately refuses CDN fallback because "
           "its HTMX/SSE implementation bytes must be pinned and reproducible.")
      {:label label
       :candidates candidates}))))

(defn- runtime-text!
  []
  (let [file (io/file "resources/public/js/gesso-live.js")]
    (when-not (.isFile file)
      (throw
       (integration-error
        :missing-production-runtime
        (str "Production Gesso Live runtime is missing at "
             (.getPath file)
             ". Build the advanced runtime before running real-browser integration.")
        {:path (.getPath file)})))
    (let [text (slurp file)]
      (when (str/blank? text)
        (throw
         (integration-error
          :empty-production-runtime
          "Production Gesso Live runtime exists but is empty."
          {:path (.getPath file)})))
      text)))

(defn- js-response
  [body]
  (fixture/response 200 javascript-headers body))

(defn- html-response
  [body]
  (fixture/response 200 html-headers body))

(defn- fragment-response
  [label]
  (fixture/response
   200
   html-headers
   (str "<div id=\"" fragment-id "\" data-fixture-version=\"" label "\">"
        label
        "</div>")))

(defn- fragment-panel-html
  ([server]
   (fragment-panel-html server nil))
  ([server {:keys [client-continuity]}]
   (let [panel
         (live.ui/fragment-panel
          (cond-> {:id fragment-id
                   :src fragment-path
                   :stream-url (fixture/sse-url server client-id)
                   :event "live-update"
                   :swap "outerHTML"}
            (some? client-continuity)
            (assoc :client-continuity client-continuity)))
         ;; fragment-panel intentionally renders an empty canonical target.
         ;; Give this scenario a visible initial marker without changing any
         ;; behavior-owning markup.
         panel'
         (update-in panel [3] conj "initial")]
     (rum/render-static-markup panel'))))

(def ^:private browser-observer-script
  (str
   "window.__gessoFixture = {\n"
   "  sseMessages: 0,\n"
   "  sseOpens: 0,\n"
   "  sseErrors: 0,\n"
   "  sendErrors: 0,\n"
   "  responseErrors: 0,\n"
   "  fragmentBeforeRequests: 0,\n"
   "  fragmentAfterRequests: 0,\n"
   "  continuityCaptured: 0,\n"
   "  continuityRestored: 0,\n"
   "  continuityErrors: 0,\n"
   "  windowErrors: [],\n"
   "  runtimeStarted: false\n"
   "};\n"
   "document.addEventListener('htmx:sseBeforeMessage', function (event) {\n"
   "  var elt = event.detail && event.detail.elt;\n"
   "  if (elt && elt.hasAttribute('data-gesso-live-invalidation')) {\n"
   "    window.__gessoFixture.sseMessages += 1;\n"
   "  }\n"
   "});\n"
   "document.addEventListener('htmx:sseOpen', function () {\n"
   "  window.__gessoFixture.sseOpens += 1;\n"
   "});\n"
   "document.addEventListener('htmx:sseError', function () {\n"
   "  window.__gessoFixture.sseErrors += 1;\n"
   "});\n"
   "document.addEventListener('htmx:sendError', function () {\n"
   "  window.__gessoFixture.sendErrors += 1;\n"
   "});\n"
   "document.addEventListener('htmx:responseError', function () {\n"
   "  window.__gessoFixture.responseErrors += 1;\n"
   "});\n"
   "document.addEventListener('htmx:beforeRequest', function (event) {\n"
   "  var elt = event.detail && event.detail.elt;\n"
   "  if (elt && elt.hasAttribute('data-gesso-live-fragment')) {\n"
   "    window.__gessoFixture.fragmentBeforeRequests += 1;\n"
   "  }\n"
   "});\n"
   "document.addEventListener('htmx:afterRequest', function (event) {\n"
   "  var elt = event.detail && event.detail.elt;\n"
   "  if (elt && elt.hasAttribute('data-gesso-live-fragment')) {\n"
   "    window.__gessoFixture.fragmentAfterRequests += 1;\n"
   "  }\n"
   "});\n"
   "document.addEventListener('gesso:live-continuity:captured', function () {\n"
   "  window.__gessoFixture.continuityCaptured += 1;\n"
   "});\n"
   "document.addEventListener('gesso:live-continuity:restored', function () {\n"
   "  window.__gessoFixture.continuityRestored += 1;\n"
   "});\n"
   "document.addEventListener('gesso:live-continuity:error', function () {\n"
   "  window.__gessoFixture.continuityErrors += 1;\n"
   "});\n"
   "window.addEventListener('error', function (event) {\n"
   "  window.__gessoFixture.windowErrors.push({\n"
   "    message: event.message || '',\n"
   "    errorMessage: event.error && event.error.message ? event.error.message : ''\n"
   "  });\n"
   "});\n"
   "gesso.live.browser.runtime.init_BANG_();\n"
   "window.__gessoFixture.runtimeStarted = !!window.gessoLive;\n"))

(defn- page-html
  ([server]
   (page-html server nil))
  ([server options]
   (str
    "<!doctype html>\n"
    "<html>\n"
    "<head>\n"
    "  <meta charset=\"utf-8\">\n"
    "  <title>Gesso Live real-browser progression fixture</title>\n"
    "  <link rel=\"icon\" href=\"data:,\">\n"
    "  <script src=\"" htmx-path "\"></script>\n"
    "  <script src=\"" sse-extension-path "\"></script>\n"
    "  <script src=\"" gesso-runtime-path "\"></script>\n"
    "</head>\n"
    "<body>\n"
    (fragment-panel-html server options)
    "\n<script>\n"
    browser-observer-script
    "</script>\n"
    "</body>\n"
    "</html>\n")))

(defn- repeated-action
  [copies action]
  (when-not (and (integer? copies)
                 (pos? copies))
    (throw
     (integration-error
      :invalid-route-copy-count
      "Real-browser static route copy count must be a positive integer."
      {:copies copies})))
  (vec (repeat copies action)))

(defn- script-static-routes!
  ([server]
   (script-static-routes! server 1 (page-html server)))
  ([server copies]
   (script-static-routes! server copies (page-html server)))
  ([server copies page-body]
   (fixture/script!
    server
    :get
    htmx-path
    (repeated-action
     copies
     (fixture/respond
      (js-response
       (resource-text!
        "HTMX 2.0.7 WebJar asset"
        htmx-resource-candidates)))))

   (fixture/script!
    server
    :get
    sse-extension-path
    (repeated-action
     copies
     (fixture/respond
      (js-response
       (resource-text!
        "htmx-ext-sse 2.2.4 WebJar asset"
        sse-resource-candidates)))))

   (fixture/script!
    server
    :get
    gesso-runtime-path
    (repeated-action
     copies
     (fixture/respond
      (js-response (runtime-text!)))))

   (fixture/script!
    server
    :get
    page-path
    (repeated-action
     copies
     (fixture/respond
      (html-response page-body))))

   server))

(defn- fragment-requests
  [server]
  (->> (fixture/requests server)
       (filter
        #(and (= :get (:method %))
              (= fragment-path (:path %))))
       vec))

(defn- request-header
  [request header-name]
  (some-> request
          :headers
          (get (str/lower-case header-name))
          first))

(defn- progression-payload
  [requirement]
  (let [wire (progression/requirement->wire requirement)]
    (pr-str
     {:progression wire
      :invalidation
      {:progression wire}})))

(defn- emit-progression!
  [server requirement]
  (fixture/emit-sse!
   server
   client-id
   {:event "live-update"
    :data (progression-payload requirement)}))

(defn- await-successor-request!
  [server first-request-id]
  (fixture/await-request!
   server
   #(and (= :get (:method %))
         (= fragment-path (:path %))
         (> (:request-id %) first-request-id))
   5000))


(defn- console-error?
  [entry]
  (= :console-error (:kind entry)))

(defn- htmx-console-error?
  [entry pred]
  (let [{:keys [type text location]} entry]
    (and
     (console-error? entry)
     (= "error" type)
     (string? text)
     (pred text)
     (string? location)
     (str/includes? location htmx-path))))

(defn- fragment-resource-console-error?
  [entry pred]
  (let [{:keys [type text location]} entry]
    (and
     (console-error? entry)
     (= "error" type)
     (string? text)
     (str/includes? text "Failed to load resource")
     (pred text)
     (string? location)
     (str/includes? location fragment-path))))

(defn- consume-expected-send-error-diagnostics!
  [context]
  (let [errors
        (chromium/browser-errors context)

        diagnostics
        (chromium/diagnostics context)

        request-failures
        (:request-failures diagnostics)

        after-request-errors
        (filterv
         #(htmx-console-error?
           %
           (fn [text]
             (= "htmx:afterRequest" text)))
         errors)

        send-errors
        (filterv
         #(htmx-console-error?
           %
           (fn [text]
             (= "htmx:sendError" text)))
         errors)

        resource-errors
        (filterv
         #(fragment-resource-console-error?
           %
           (constantly true))
         errors)

        recognized-errors
        (set
         (concat
          after-request-errors
          send-errors
          resource-errors))

        unexpected-errors
        (vec (remove recognized-errors errors))

        request-failure
        (first request-failures)

        expected-request-failure?
        (and
         (= 1 (count request-failures))
         (= "GET" (:method request-failure))
         (string? (:url request-failure))
         (str/includes? (:url request-failure) fragment-path)
         (string? (:failure request-failure))
         (not (str/blank? (:failure request-failure)))
         ;; The exact Chromium net error for a deliberately truncated fixed-
         ;; length response is browser-version presentation detail. What matters
         ;; is that Playwright observed a genuine network failure for /fragment.
         (str/includes? (:failure request-failure) "ERR_"))

        expected?
        (and
         (= 1 (count after-request-errors))
         (= 1 (count send-errors))
         ;; Chromium normally emits one failed-resource console line for the
         ;; truncated XHR, but that console presentation is not part of HTMX's
         ;; contract and may change across system-Chromium revisions.
         (<= (count resource-errors) 1)
         (empty? unexpected-errors)
         expected-request-failure?)]

    (when-not expected?
      (throw
       (integration-error
        :unexpected-send-error-diagnostics
        (str "The deliberate truncated-response transport failure did not "
             "produce the expected semantic diagnostics: exactly one HTMX "
             "afterRequest error, exactly one HTMX sendError, exactly one "
             "Playwright network failure for GET /fragment, at most one "
             "Chromium failed-resource console entry for that request, and no "
             "other browser failures. The exact Chromium net error string is "
             "deliberately not part of the contract.")
        {:errors errors
         :unexpected-errors unexpected-errors
         :diagnostics diagnostics})))

    (chromium/clear-diagnostics! context)
    true))

(defn- consume-expected-sse-reconnect-diagnostics!
  [context]
  (let [errors
        (chromium/browser-errors context)

        diagnostics
        (chromium/diagnostics context)

        request-failures
        (:request-failures diagnostics)

        sse-console-errors
        (filterv
         #(htmx-console-error?
           %
           (constantly true))
         errors)

        expected?
        (and
         ;; htmx-ext-sse deliberately reports EventSource.onerror through
         ;; HTMX triggerErrorEvent. HTMX therefore console.errors the native
         ;; Event object. Playwright's textual rendering of that Event is not a
         ;; stable semantic API, so require its source/category rather than the
         ;; literal string "Event".
         (= 1 (count errors))
         (= 1 (count sse-console-errors))
         (empty? request-failures))]

    (when-not expected?
      (throw
       (integration-error
        :unexpected-sse-reconnect-diagnostics
        (str "The deliberate EventSource disconnect did not produce exactly "
             "one HTMX-sourced console error and no HTTP request failure. "
             "Unexpected browser failures must remain visible rather than "
             "being globally whitelisted.")
        {:errors errors
         :diagnostics diagnostics})))

    (chromium/clear-diagnostics! context)
    true))

(defn- consume-expected-managed-sse-progression-rejection!
  [context]
  (let [errors
        (chromium/browser-errors context)

        diagnostics
        (chromium/diagnostics context)

        web-errors
        (filterv #(= :web-error (:kind %)) errors)

        runtime-web-errors
        (filterv
         (fn [{:keys [location]}]
           (let [url
                 (when (map? location)
                   (:url location))]
             (and
              (string? url)
              (str/includes? url gesso-runtime-path))))
         web-errors)

        recognized-errors
        (set runtime-web-errors)

        unexpected-errors
        (vec (remove recognized-errors errors))

        expected?
        (and
         ;; The semantic message is asserted separately from the browser's
         ;; window.error event. Under Closure :advanced, Playwright's Java
         ;; WebError.error() may stringify the same exception as an internal
         ;; minified constructor such as "Error: ol". That presentation is not
         ;; a Gesso API. Here we require one uncaught exception sourced from the
         ;; production runtime and no unrelated browser/network failure.
         (= 1 (count web-errors))
         (= 1 (count runtime-web-errors))
         (empty? unexpected-errors)
         (empty? (:request-failures diagnostics))
         (empty? (:page-crashes diagnostics)))]

    (when-not expected?
      (throw
       (integration-error
        :unexpected-managed-sse-progression-diagnostic
        (str "A deliberately inconsistent managed SSE progression payload did "
             "not produce exactly one uncaught WebError sourced from the "
             "production Gesso runtime with no unrelated browser/network "
             "failure. The semantic invariant text is asserted independently "
             "through window.error because Playwright's advanced-compiled "
             "exception string is presentation detail.")
        {:errors errors
         :unexpected-errors unexpected-errors
         :diagnostics diagnostics})))

    (chromium/clear-diagnostics! context)
    true))

(defn- consume-expected-managed-fragment-abort-diagnostics!
  [context]
  (let [errors
        (chromium/browser-errors context)

        diagnostics
        (chromium/diagnostics context)

        request-failures
        (:request-failures diagnostics)

        after-request-errors
        (filterv
         #(htmx-console-error?
           %
           (fn [text]
             (= "htmx:afterRequest" text)))
         errors)

        send-abort-errors
        (filterv
         #(htmx-console-error?
           %
           (fn [text]
             (= "htmx:sendAbort" text)))
         errors)

        resource-errors
        (filterv
         #(fragment-resource-console-error?
           %
           (constantly true))
         errors)

        recognized-errors
        (set
         (concat
          after-request-errors
          send-abort-errors
          resource-errors))

        unexpected-errors
        (vec (remove recognized-errors errors))

        request-failure
        (first request-failures)

        expected-request-failure?
        (and
         (= 1 (count request-failures))
         (= "GET" (:method request-failure))
         (string? (:url request-failure))
         (str/includes? (:url request-failure) fragment-path)
         (string? (:failure request-failure))
         (str/includes? (:failure request-failure) "ERR_ABORTED"))

        expected?
        (and
         ;; HTMX 2.0.7 aborts an in-flight XHR when its owning element is
         ;; cleaned up. triggerErrorEvent reports both lifecycle events through
         ;; console.error, while Playwright observes the XHR as ERR_ABORTED.
         (= 1 (count after-request-errors))
         (= 1 (count send-abort-errors))
         ;; Chromium may or may not additionally render a failed-resource
         ;; console line for an aborted XHR. That rendering is not semantic.
         (<= (count resource-errors) 1)
         (empty? unexpected-errors)
         expected-request-failure?
         (empty? (:page-crashes diagnostics)))]

    (when-not expected?
      (throw
       (integration-error
        :unexpected-managed-fragment-abort-diagnostics
        (str "Deleting a managed fragment with an in-flight HTMX request did "
             "not produce the expected physical-abort contract: exactly one "
             "HTMX afterRequest error, exactly one HTMX sendAbort error, "
             "exactly one Playwright GET /fragment failure containing "
             "ERR_ABORTED, at most one Chromium failed-resource console "
             "entry, and no unrelated browser failure.")
        {:errors errors
         :unexpected-errors unexpected-errors
         :diagnostics diagnostics})))

    (chromium/clear-diagnostics! context)
    true))

(defn- consume-expected-response-error-diagnostics!
  [context status]
  (let [errors
        (chromium/browser-errors context)

        diagnostics
        (chromium/diagnostics context)

        request-failures
        (:request-failures diagnostics)

        status-token
        (str status)

        htmx-response-errors
        (filterv
         #(htmx-console-error?
           %
           (fn [text]
             (and
              (str/includes? text "Response Status Error Code")
              (str/includes? text status-token)
              (str/includes? text fragment-path))))
         errors)

        resource-errors
        (filterv
         #(fragment-resource-console-error?
           %
           (fn [text]
             (str/includes? text status-token)))
         errors)

        recognized-errors
        (set
         (concat
          htmx-response-errors
          resource-errors))

        unexpected-errors
        (vec (remove recognized-errors errors))

        expected?
        (and
         (= 1 (count htmx-response-errors))
         ;; Chromium currently logs one failed-resource line for a 5xx XHR.
         ;; Keep it shape-checked when present, but don't make browser console
         ;; presentation part of Gesso's semantic contract.
         (<= (count resource-errors) 1)
         (empty? unexpected-errors)
         ;; A completed HTTP 5xx is not a Playwright transport failure.
         (empty? request-failures))]

    (when-not expected?
      (throw
       (integration-error
        :unexpected-response-error-diagnostics
        (str "The deliberate HTTP " status
             " response did not produce the expected semantic diagnostics: "
             "exactly one HTMX responseError, no Playwright transport failure, "
             "at most one Chromium failed-resource console entry for /fragment, "
             "and no other browser failures.")
        {:status status
         :errors errors
         :unexpected-errors unexpected-errors
         :diagnostics diagnostics})))

    (chromium/clear-diagnostics! context)
    true))

(defn- safe-cleanup!
  [label f]
  (try
    (f)
    nil
    (catch Throwable error
      {:resource label
       :class (str (class error))
       :message (.getMessage error)})))

(defn- with-real-browser
  [f]
  (let [server (fixture/start!)
        harness* (atom nil)
        context* (atom nil)
        result* (atom nil)
        primary-error* (atom nil)]
    (try
      (script-static-routes! server)
      (let [harness (chromium/start!)
            _ (reset! harness* harness)
            context (chromium/new-context! harness)
            _ (reset! context* context)
            page (chromium/new-page! context)]
        (reset!
         result*
         (f {:server server
             :harness harness
             :context context
             :page page})))
      (catch Throwable error
        (reset! primary-error* error))
      (finally
        ;; Browser first: closing the EventSource before stopping the fixture
        ;; prevents cleanup from racing an automatic SSE reconnect.
        (let [cleanup-errors
              (keep
               identity
               [(when-let [context @context*]
                  (safe-cleanup!
                   :browser-context
                   #(chromium/close-context! context)))
                (safe-cleanup!
                 :fixture-server
                 #(fixture/stop! server))
                (when-let [harness @harness*]
                  (safe-cleanup!
                   :chromium
                   #(chromium/stop! harness)))])]
          (cond
            @primary-error*
            (when (seq cleanup-errors)
              (binding [*out* *err*]
                (println
                 "Gesso real-browser cleanup also failed after the primary error:"
                 (pr-str cleanup-errors))))

            (seq cleanup-errors)
            (reset!
             primary-error*
             (integration-error
              :cleanup-failed
              "Gesso real-browser integration did not clean up all browser/network resources."
              {:cleanup-errors cleanup-errors}))))))
    (if-let [error @primary-error*]
      (throw error)
      @result*)))

(deftest queued-progression-survives-real-htmx-sse-single-flight-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "real HTMX/SSE cannot bypass adapter single-flight and queued progression"
        (let [basis-b
              {:tx-id 102
               :system-time "2026-08-28T00:00:02Z"}

              basis-c
              {:tx-id 103
               :system-time "2026-08-28T00:00:03Z"}

              requirement-b
              (progression/requirement basis-b)

              requirement-c
              (progression/requirement basis-c)

              expected
              (progression/compose requirement-b requirement-c)]

          ;; The EventSource open itself is an advisory invalidation. Hold that
          ;; first adapter-admitted GET so later canonical invalidations arrive
          ;; while one physical refresh generation owns the fragment.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A"))
            (fixture/respond (fragment-response "B+C"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [first-request
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                first-request-id
                (:request-id first-request)]

            (is
             (nil?
              (request-header
               first-request
               progression.http/request-header-name))
             "The reconnect/open refresh is advisory and must not invent authority.")

            (is (= 1 (count (fragment-requests server)))
                "Exactly one physical fragment GET owns the active generation.")

            ;; Deliver two distinct opaque authoritative requirements while A
            ;; remains physically in flight.
            (emit-progression! server requirement-b)
            (emit-progression! server requirement-c)

            ;; Synchronize on actual browser receipt of both named SSE messages;
            ;; this avoids sleeps and makes the following request-count assertion
            ;; about semantics rather than scheduler luck.
            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 2")

            (is (= 1 (count (fragment-requests server)))
                "B and C must queue behind A instead of starting parallel GETs.")

            ;; Completing A promotes the full queued requirement set into one
            ;; successor generation.
            (fixture/release! server first-request-id)

            (let [successor
                  (await-successor-request!
                   server
                   first-request-id)

                  encoded
                  (request-header
                   successor
                   progression.http/request-header-name)

                  decoded
                  (some-> encoded
                          progression.http/decode-request-progression)]

              (is (some? encoded)
                  "The successor GET must carry its adapter-approved minimum-read requirement.")

              (is (= expected decoded)
                  "The real configRequest header must conservatively compose B and C.")

              (is (= #{basis-b basis-c}
                     (:bases decoded))
                  "Distinct opaque bases must survive real browser/HTTP transport.")

              (is (= 2 (count (fragment-requests server)))
                  "A plus one composed successor is the complete physical request set.")

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B+C'"))

              (is (= "B+C"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "The successor authoritative fragment must complete the real HTMX swap."))))

        (is (true? (chromium/assert-clean! context))
            "The real browser must finish without console errors, uncaught exceptions, or crashes.")))))


(deftest failed-refresh-advances-queued-progression-through-real-send-error-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "a real transport failure cannot discard progression queued behind the failed generation"
        (let [basis-b
              {:tx-id 202
               :system-time "2026-08-28T00:10:02Z"}

              requirement-b
              (progression/requirement basis-b)]

          ;; As in the normal-completion scenario, the EventSource open starts
          ;; advisory request A. Hold A, then establish a canonical requirement
          ;; while that generation owns the fragment. This time A receives a
          ;; deliberately truncated HTTP response: headers commit the response,
          ;; but the body ends early so HTMX must take its real sendError path.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-never-arrives"))
            (fixture/respond (fragment-response "B-after-failure"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [first-request
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                first-request-id
                (:request-id first-request)]

            (is
             (nil?
              (request-header
               first-request
               progression.http/request-header-name))
             "The SSE-open request is advisory and must not invent authority.")

            (emit-progression! server requirement-b)

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "Canonical B must queue behind the still-owned request A.")

            (is (true? (chromium/assert-clean! context))
                "The browser must be clean before the deliberate truncated response.")
            (chromium/clear-diagnostics! context)

            ;; Commit a real HTTP response with a full Content-Length, send only
            ;; a strict body prefix, then close. Unlike a pre-header connection
            ;; loss, this cannot be transparently replayed by Chromium as an
            ;; idempotent GET; HTMX must observe the resulting sendError.
            (fixture/release! server first-request-id :truncate)

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sendErrors === 1")

            (let [successor
                  (await-successor-request!
                   server
                   first-request-id)

                  encoded
                  (request-header
                   successor
                   progression.http/request-header-name)

                  decoded
                  (some-> encoded
                          progression.http/decode-request-progression)]

              (is (= requirement-b decoded)
                  "The successor after sendError must carry the queued canonical requirement B.")

              (is (= #{basis-b} (:bases decoded))
                  "The authoritative basis must survive failure/retry through the real HTTP header.")

              (is (= 2 (count (fragment-requests server)))
                  "The failed A plus one successor B is the complete physical request set.")

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-after-failure'"))

              (is (= "B-after-failure"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "The successor authoritative response must still complete the real HTMX swap.")

              (is (= 0
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.responseErrors"))
                  "A deliberate truncated transport failure should exercise sendError, not HTTP responseError."))))

        (is (true? (consume-expected-send-error-diagnostics! context))
            "The deliberate truncated response must produce exactly the expected semantic HTMX/browser diagnostics.")
        (is (true? (chromium/assert-clean! context))
            "After accounting for the deliberate truncated response, the browser must be clean.")))))


(deftest http-response-error-advances-queued-progression-without-swapping-error-body-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "a real HTTP error retires its generation and preserves queued canonical progression"
        (let [basis-b
              {:tx-id 302
               :system-time "2026-08-28T00:20:02Z"}

              requirement-b
              (progression/requirement basis-b)

              error-response
              (fixture/response
               503
               html-headers
               (str "<div id=\"" fragment-id
                    "\" data-fixture-version=\"error\">"
                    "ERROR-BODY-MUST-NOT-SWAP"
                    "</div>"))]

          ;; Both generations are held. A will be released with a real HTTP 503.
          ;; The successor is then observable as an in-flight physical request
          ;; before we allow its canonical response to complete.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-never-authoritative"))
            (fixture/hold (fragment-response "B-after-503"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [first-request
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                first-request-id
                (:request-id first-request)]

            (emit-progression! server requirement-b)

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "Canonical B must remain queued while A owns the fragment.")

            ;; Establish that setup is clean before deliberately creating the
            ;; HTTP error. Any diagnostics after this point belong to the
            ;; response-error experiment itself.
            (is (true? (chromium/assert-clean! context))
                "The browser must be clean before the deliberate 503 response.")
            (chromium/clear-diagnostics! context)

            (fixture/release! server first-request-id error-response)

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.responseErrors === 1")

            (is (= 0
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.sendErrors"))
                "A real HTTP 503 must exercise responseError, not sendError.")

            (let [successor
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) first-request-id))
                   5000)

                  successor-id
                  (:request-id successor)

                  encoded
                  (request-header
                   successor
                   progression.http/request-header-name)

                  decoded
                  (some-> encoded
                          progression.http/decode-request-progression)]

              (is (= requirement-b decoded)
                  "The successor after responseError must carry queued canonical requirement B.")

              (is (= #{basis-b} (:bases decoded))
                  "The authoritative basis must survive the real HTTP-error lifecycle.")

              (is (= 2 (count (fragment-requests server)))
                  "The errored A plus one held successor B is the complete physical request set.")

              (is (= "initial"
                     (str/trim
                      (chromium/evaluate
                       page
                       (str "() => document.getElementById('"
                            fragment-id
                            "').textContent"))))
                  "HTMX must not install the 503 response body as canonical fragment content.")

              (is (true? (consume-expected-response-error-diagnostics!
                           context
                           503))
                  "The deliberate 503 must produce exactly HTMX's expected responseError diagnostic.")

              (fixture/release! server successor-id)

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-after-503'"))

              (is (= "B-after-503"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "Canonical successor B must complete the real HTMX swap after A's HTTP error.")

              (is (true? (chromium/assert-clean! context))
                  "After accounting for the deliberate 503, the browser must be clean."))))))))


(deftest sse-reconnect-queues-one-advisory-successor-behind-in-flight-refresh-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "reconnecting a managed EventSource queues one advisory refresh without inventing authority"
        ;; Initial SSE open starts advisory request A. Keep A in flight across a
        ;; forced EventSource disconnect/reconnect. The second sseOpen is another
        ;; advisory invalidation: it must be coordinated behind A rather than
        ;; creating a parallel physical GET.
        (fixture/script!
         server
         :get
         fragment-path
         [(fixture/hold (fragment-response "A-before-reconnect"))
          (fixture/hold (fragment-response "B-after-reconnect"))])

        (chromium/navigate!
         page
         (fixture/url server page-path))

        (chromium/wait-for-js!
         page
         "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

        (let [initial-sse
              (fixture/await-sse-client!
               server
               client-id
               5000)

              initial-connection-id
              (:connection-id initial-sse)

              first-request
              (fixture/await-pending!
               server
               #(and (= :get (:method %))
                     (= fragment-path (:path %)))
               5000)

              first-request-id
              (:request-id first-request)]

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture.sseOpens === 1")

          (is
           (nil?
            (request-header
             first-request
             progression.http/request-header-name))
           "Initial sseOpen refresh A is advisory and must not invent progression authority.")

          (is (= 1 (count (fragment-requests server)))
              "Exactly one physical fragment request must own the fragment before reconnect.")

          ;; Set the native EventSource retry delay through the SSE protocol
          ;; itself. A retry-only frame dispatches no application message, so
          ;; this controls timing without creating another semantic invalidation.
          (is (= 1
                 (fixture/emit-sse!
                  server
                  client-id
                  {:retry 50}))
              "The reconnect timing control must reach exactly the open EventSource.")

          (is (= 1 (fixture/close-sse! server client-id))
              "The fixture must close exactly the currently open managed EventSource.")

          ;; Synchronize on the fixture's new real connection rather than
          ;; sleeping for the native/extension reconnect path.
          (let [reconnected
                (fixture/await-sse-client!
                 server
                 client-id
                 5000)]

            (is (not= initial-connection-id
                      (:connection-id reconnected))
                "The observed SSE client must be a genuinely new connection.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseOpens === 2")

            (is (= 1 (count (fragment-requests server)))
                "Reconnect while A is in flight must queue an advisory wakeup, not start a parallel GET.")

            ;; Completion of A promotes the queued reconnect wakeup into exactly
            ;; one successor generation. Because reconnect carries no known
            ;; progression fact, the successor must remain headerless.
            (fixture/release! server first-request-id)

            (let [successor
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) first-request-id))
                   5000)

                  successor-id
                  (:request-id successor)]

              (is
               (nil?
                (request-header
                 successor
                 progression.http/request-header-name))
               "Reconnect successor B is advisory and must not fabricate a minimum-read progression.")

              (is (= 2 (count (fragment-requests server)))
                  "A plus exactly one reconnect successor B is the complete physical request set.")

              (is (= "A-before-reconnect"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "A may complete normally; reconnect still requires one subsequent authoritative refresh.")

              (fixture/release! server successor-id)

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-after-reconnect'"))

              (is (= "B-after-reconnect"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "The single reconnect successor must complete the real HTMX swap.")

              (is (= 2
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.sseOpens"))
                  "One forced disconnect must produce exactly one observed reconnect/open.")

              (is (= 1
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.sseErrors"))
                  "Exactly one deliberate EventSource disconnect must produce exactly one htmx:sseError.")

              (is (true? (consume-expected-sse-reconnect-diagnostics! context))
                  "The forced disconnect must produce exactly htmx-ext-sse's expected browser diagnostic.")

              (is (true? (chromium/assert-clean! context))
                  "After accounting for the deliberate SSE disconnect, the browser must be clean."))))))))


(deftest canonical-progression-after-reconnect-advisory-refresh-queues-authoritative-successor-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "canonical progression arriving during a reconnect advisory refresh becomes one authoritative successor"
        (let [basis-c
              {:tx-id 403
               :system-time "2026-08-28T00:30:03Z"}

              requirement-c
              (progression/requirement basis-c)]

          ;; A is the advisory refresh caused by the initial EventSource open.
          ;; Disconnect/reconnect while A is held so the second sseOpen queues an
          ;; advisory refresh B behind it. Hold B as well. Only after B owns the
          ;; fragment do we deliver canonical progression C on the reconnected
          ;; EventSource. C must wait behind B, then become exactly one successor
          ;; carrying the authoritative progression header.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-initial-open"))
            (fixture/hold (fragment-response "B-reconnect-advisory"))
            (fixture/hold (fragment-response "C-canonical"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (let [initial-sse
                (fixture/await-sse-client!
                 server
                 client-id
                 5000)

                initial-connection-id
                (:connection-id initial-sse)

                request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseOpens === 1")

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial-open request A must be advisory and headerless.")

            (is (= 1
                   (fixture/emit-sse!
                    server
                    client-id
                    {:retry 50}))
                "The reconnect timing control must reach exactly the initial EventSource.")

            (is (= 1 (fixture/close-sse! server client-id))
                "The fixture must close exactly the initial EventSource.")

            (let [reconnected
                  (fixture/await-sse-client!
                   server
                   client-id
                   5000)]

              (is (not= initial-connection-id
                        (:connection-id reconnected))
                  "The browser must establish a genuinely new SSE connection.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 2")

              (is (= 1 (count (fragment-requests server)))
                  "Reconnect must queue advisory B behind still-running A rather than overlap it.")

              ;; Completing A admits reconnect advisory B. B is deliberately
              ;; held so canonical progression C can arrive while the advisory
              ;; generation owns the physical refresh slot.
              (fixture/release! server request-a-id)

              (let [request-b
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-a-id))
                     5000)

                    request-b-id
                    (:request-id request-b)]

                (is
                 (nil?
                  (request-header
                   request-b
                   progression.http/request-header-name))
                 "Reconnect advisory request B must remain headerless.")

                (is (= 2 (count (fragment-requests server)))
                    "A plus reconnect advisory B are the only physical requests before canonical C arrives.")

                (emit-progression! server requirement-c)

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.sseMessages === 1")

                (is (= 2 (count (fragment-requests server)))
                    "Canonical C must queue behind in-flight advisory B, not bypass fragment single-flight.")

                ;; B may complete and swap because it was legitimately admitted.
                ;; Its completion must then promote queued canonical C with the
                ;; exact authoritative requirement recovered from SSE.
                (fixture/release! server request-b-id)

                (let [request-c
                      (fixture/await-pending!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (> (:request-id %) request-b-id))
                       5000)

                      request-c-id
                      (:request-id request-c)

                      encoded
                      (request-header
                       request-c
                       progression.http/request-header-name)

                      decoded
                      (some-> encoded
                              progression.http/decode-request-progression)]

                  (is (= requirement-c decoded)
                      "Successor C must carry the canonical requirement that arrived during advisory B.")

                  (is (= #{basis-c} (:bases decoded))
                      "Reconnect advisory state must not add or replace C's authoritative basis.")

                  (is (= 3 (count (fragment-requests server)))
                      "A, reconnect advisory B, and one canonical successor C are the complete physical request set.")

                  (is (= "B-reconnect-advisory"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "B may settle, but queued canonical C still owns the required follow-up refresh.")

                  (fixture/release! server request-c-id)

                  (chromium/wait-for-js!
                   page
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'C-canonical'"))

                  (is (= "C-canonical"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "Canonical C must complete the final real HTMX swap.")

                  (is (= 2
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseOpens"))
                      "The canonical follow-up must not require or create another SSE reconnect.")

                  (is (= 1
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseErrors"))
                      "The scenario's single forced disconnect must produce exactly one htmx:sseError.")

                  (is (true? (consume-expected-sse-reconnect-diagnostics! context))
                      "Reconnect must produce only htmx-ext-sse's expected disconnect diagnostic.")

                  (is (true? (chromium/assert-clean! context))
                      "After accounting for the deliberate SSE disconnect, reconnect plus canonical progression must leave the browser clean."))))))))))


(deftest multiple-canonical-progressions-compose-behind-reconnect-advisory-refresh-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "multiple canonical requirements arriving during reconnect advisory work compose into one successor"
        (let [basis-d
              {:tx-id 504
               :system-time "2026-08-28T00:40:04Z"}

              basis-e
              {:tx-id 505
               :system-time "2026-08-28T00:40:05Z"}

              requirement-d
              (progression/requirement basis-d)

              requirement-e
              (progression/requirement basis-e)

              expected
              (progression/compose requirement-d requirement-e)]

          ;; Initial SSE open admits advisory A. A reconnect while A is held
          ;; queues advisory B. Once B owns the fragment, deliver two distinct
          ;; canonical progression requirements over the reconnected EventSource.
          ;; They must be accumulated by set union behind B and admitted as one
          ;; successor C; neither arrival may overwrite the other or create its
          ;; own physical refresh.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-initial-open"))
            (fixture/hold (fragment-response "B-reconnect-advisory"))
            (fixture/hold (fragment-response "C-composed-authority"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (let [initial-sse
                (fixture/await-sse-client!
                 server
                 client-id
                 5000)

                initial-connection-id
                (:connection-id initial-sse)

                request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseOpens === 1")

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial-open request A must be advisory and headerless.")

            (is (= 1
                   (fixture/emit-sse!
                    server
                    client-id
                    {:retry 50}))
                "The reconnect timing control must reach exactly the initial EventSource.")

            (is (= 1 (fixture/close-sse! server client-id))
                "The fixture must close exactly the initial EventSource.")

            (let [reconnected
                  (fixture/await-sse-client!
                   server
                   client-id
                   5000)]

              (is (not= initial-connection-id
                        (:connection-id reconnected))
                  "The browser must establish a genuinely new SSE connection.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 2")

              (is (= 1 (count (fragment-requests server)))
                  "Reconnect must queue advisory B behind A rather than overlap it.")

              (fixture/release! server request-a-id)

              (let [request-b
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-a-id))
                     5000)

                    request-b-id
                    (:request-id request-b)]

                (is
                 (nil?
                  (request-header
                   request-b
                   progression.http/request-header-name))
                 "Reconnect advisory request B must remain headerless.")

                (is (= 2 (count (fragment-requests server)))
                    "A plus advisory B are the only physical requests before canonical requirements arrive.")

                ;; Emit E before D deliberately. The expected requirement is
                ;; composed in the opposite textual order to prove that browser
                ;; admission is set-union semantics, not last-arrival authority.
                (is (= 1 (emit-progression! server requirement-e))
                    "Canonical requirement E must reach the reconnected EventSource.")

                (is (= 1 (emit-progression! server requirement-d))
                    "Canonical requirement D must reach the reconnected EventSource.")

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.sseMessages === 2")

                (is (= 2 (count (fragment-requests server)))
                    "D and E must both queue behind B without creating parallel or per-event GETs.")

                (fixture/release! server request-b-id)

                (let [request-c
                      (fixture/await-pending!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (> (:request-id %) request-b-id))
                       5000)

                      request-c-id
                      (:request-id request-c)

                      encoded
                      (request-header
                       request-c
                       progression.http/request-header-name)

                      decoded
                      (some-> encoded
                              progression.http/decode-request-progression)]

                  (is (= expected decoded)
                      "The single successor must carry composition D∪E, independent of arrival order.")

                  (is (= #{basis-d basis-e} (:bases decoded))
                      "Neither canonical basis may be discarded as advisory B settles.")

                  (is (= 3 (count (fragment-requests server)))
                      "A, advisory B, and one composed canonical successor C are the complete physical request set.")

                  (is (= "B-reconnect-advisory"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "B may settle, but composed D∪E still requires exactly one follow-up refresh.")

                  (fixture/release! server request-c-id)

                  (chromium/wait-for-js!
                   page
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'C-composed-authority'"))

                  (is (= "C-composed-authority"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "The composed canonical successor must complete the final real HTMX swap.")

                  (is (= 2
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseOpens"))
                      "Composing canonical requirements must not create another SSE reconnect.")

                  (is (= 1
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseErrors"))
                      "The scenario's single forced disconnect must produce exactly one htmx:sseError.")

                  (is (true? (consume-expected-sse-reconnect-diagnostics! context))
                      "Reconnect must produce only htmx-ext-sse's expected disconnect diagnostic.")

                  (is (true? (chromium/assert-clean! context))
                      "After accounting for the deliberate SSE disconnect, reconnect plus multiple canonical invalidations must leave the browser clean."))))))))))


(deftest duplicate-canonical-progression-is-idempotent-behind-reconnect-advisory-refresh-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "duplicate delivery of one canonical requirement produces one basis and one physical successor"
        (let [basis-d
              {:tx-id 604
               :system-time "2026-08-28T00:50:04Z"}

              requirement-d
              (progression/requirement basis-d)]

          ;; Initial SSE open admits advisory A. Reconnect queues advisory B
          ;; behind A. Once B owns the fragment, deliver the exact same
          ;; canonical progression requirement twice. At-least-once delivery is
          ;; normal for distributed notification paths; duplicate observation
          ;; must therefore be idempotent at the progression and physical-work
          ;; boundaries.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-initial-open"))
            (fixture/hold (fragment-response "B-reconnect-advisory"))
            (fixture/hold (fragment-response "C-single-authority"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (let [initial-sse
                (fixture/await-sse-client!
                 server
                 client-id
                 5000)

                initial-connection-id
                (:connection-id initial-sse)

                request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseOpens === 1")

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial-open request A must be advisory and headerless.")

            (is (= 1
                   (fixture/emit-sse!
                    server
                    client-id
                    {:retry 50}))
                "The reconnect timing control must reach exactly the initial EventSource.")

            (is (= 1 (fixture/close-sse! server client-id))
                "The fixture must close exactly the initial EventSource.")

            (let [reconnected
                  (fixture/await-sse-client!
                   server
                   client-id
                   5000)]

              (is (not= initial-connection-id
                        (:connection-id reconnected))
                  "The browser must establish a genuinely new SSE connection.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 2")

              (is (= 1 (count (fragment-requests server)))
                  "Reconnect must queue advisory B behind A rather than overlap it.")

              (fixture/release! server request-a-id)

              (let [request-b
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-a-id))
                     5000)

                    request-b-id
                    (:request-id request-b)]

                (is
                 (nil?
                  (request-header
                   request-b
                   progression.http/request-header-name))
                 "Reconnect advisory request B must remain headerless.")

                (is (= 2 (count (fragment-requests server)))
                    "A plus advisory B are the only requests before duplicate canonical delivery.")

                (is (= 1 (emit-progression! server requirement-d))
                    "First delivery of canonical requirement D must reach the reconnected EventSource.")

                (is (= 1 (emit-progression! server requirement-d))
                    "Duplicate delivery of the identical canonical requirement D must also reach the browser.")

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.sseMessages === 2")

                (is (= 2 (count (fragment-requests server)))
                    "Two deliveries of D must both queue behind B without creating parallel or per-delivery GETs.")

                (fixture/release! server request-b-id)

                (let [request-c
                      (fixture/await-pending!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (> (:request-id %) request-b-id))
                       5000)

                      request-c-id
                      (:request-id request-c)

                      encoded
                      (request-header
                       request-c
                       progression.http/request-header-name)

                      decoded
                      (some-> encoded
                              progression.http/decode-request-progression)]

                  (is (= requirement-d decoded)
                      "The successor must carry D exactly once despite duplicate notification delivery.")

                  (is (= #{basis-d} (:bases decoded))
                      "Progression set semantics must collapse duplicate basis D rather than represent it twice.")

                  (is (= 1 (count (:bases decoded)))
                      "The canonical progression header must contain exactly one distinct basis.")

                  (is (= 3 (count (fragment-requests server)))
                      "A, advisory B, and one canonical successor C are the complete physical request set.")

                  (is (= "B-reconnect-advisory"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "B may settle, but duplicate D delivery still requires only one follow-up refresh.")

                  (fixture/release! server request-c-id)

                  (chromium/wait-for-js!
                   page
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'C-single-authority'"))

                  (is (= "C-single-authority"
                         (chromium/evaluate
                          page
                          (str "() => document.getElementById('"
                               fragment-id
                               "').textContent")))
                      "The single canonical successor must complete the final real HTMX swap.")

                  (is (= 3 (count (fragment-requests server)))
                      "Completing the duplicate-derived successor must not reveal a hidden fourth refresh.")

                  (is (= 2
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseOpens"))
                      "Duplicate canonical delivery must not cause an SSE reconnect.")

                  (is (= 1
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.sseErrors"))
                      "The scenario's single forced disconnect must produce exactly one htmx:sseError.")

                  (is (true? (consume-expected-sse-reconnect-diagnostics! context))
                      "Reconnect must produce only htmx-ext-sse's expected disconnect diagnostic.")

                  (is (true? (chromium/assert-clean! context))
                      "After accounting for the deliberate SSE disconnect, reconnect plus duplicate canonical delivery must leave the browser clean."))))))))))


(deftest duplicate-authority-already-carried-by-bound-request-does-not-schedule-successor-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "a duplicate canonical requirement already carried by the bound request is absorbed"
        (let [basis-d
              {:tx-id 704
               :system-time "2026-08-28T01:00:04Z"}

              requirement-d
              (progression/requirement basis-d)]

          ;; Initial SSE open starts advisory A. Canonical D queues behind it.
          ;; Once successor B is physically bound, configRequest has attached D
          ;; to B's real HTTP request. Re-observing D while B is held is then
          ;; already-covered authority, not evidence that another generation is
          ;; needed after B.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-advisory"))
            (fixture/hold (fragment-response "B-authoritative-D"))
            ;; If the adapter regresses and schedules a redundant successor,
            ;; keep it held so the test can diagnose that physical request
            ;; deterministically rather than letting a default response race.
            (fixture/hold (fragment-response "UNEXPECTED-duplicate-D-successor"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial SSE-open request A must remain advisory and headerless.")

            (is (= 1 (emit-progression! server requirement-d))
                "Canonical requirement D must reach the managed EventSource.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "D must queue behind advisory A rather than overlap it.")

            (fixture/release! server request-a-id)

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded
                  (some-> encoded
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded)
                  "Bound successor B must carry canonical requirement D on the real HTTP request.")

              (is (= #{basis-d} (:bases decoded))
                  "B's minimum-read header must contain exactly D's basis.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.fragmentBeforeRequests === 2")

              (is (= 2 (count (fragment-requests server)))
                  "Only advisory A and canonical B exist before duplicate D is delivered.")

              ;; This is the boundary v7.346 changed: B is not merely a logical
              ;; generation anymore; it is a physically bound HTMX request that
              ;; demonstrably carries D. The duplicate must therefore be
              ;; recognized as already covered.
              (is (= 1 (emit-progression! server requirement-d))
                  "Duplicate canonical D must genuinely reach the browser while B is in flight.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseMessages === 2")

              (is (= 2 (count (fragment-requests server)))
                  "Duplicate D must not start a parallel request while B owns the fragment.")

              (fixture/release! server request-b-id)

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-authoritative-D'"))

              ;; afterRequest is the coordinator's completion boundary. Waiting
              ;; for the second managed afterRequest means all synchronous Gesso
              ;; handlers for B's completion have run before these assertions.
              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.fragmentAfterRequests === 2")

              (is (= 2
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.fragmentBeforeRequests"))
                  "Completing B must not synchronously admit a third HTMX fragment request.")

              (is (= 2 (count (fragment-requests server)))
                  "Duplicate authority already carried by B must not create a post-B physical refresh.")

              (is (empty? (fixture/pending-request-ids server))
                  "No hidden held successor may remain after authoritative B completes.")

              (is (= "B-authoritative-D"
                     (chromium/evaluate
                      page
                      (str "() => document.getElementById('"
                           fragment-id
                           "').textContent")))
                  "The authoritative D response remains the final canonical DOM.")

              (is (true? (chromium/assert-clean! context))
                  "Covered duplicate authority must leave the real browser clean."))))))))


(deftest duplicate-covered-authority-plus-new-authority-queues-only-new-successor-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "duplicate authority covered by the bound request is discarded while genuinely new authority survives"
        (let [basis-d
              {:tx-id 804
               :system-time "2026-08-28T01:10:04Z"}

              basis-e
              {:tx-id 805
               :system-time "2026-08-28T01:10:05Z"}

              requirement-d
              (progression/requirement basis-d)

              requirement-e
              (progression/requirement basis-e)]

          ;; Advisory A owns the fragment first. D arrives and becomes bound to
          ;; successor B's actual HTTP request. While B is held, deliver D again
          ;; followed by genuinely new E. The duplicate D is already covered by
          ;; B; only E may survive into the queued successor.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-advisory"))
            (fixture/hold (fragment-response "B-authoritative-D"))
            (fixture/hold (fragment-response "C-authoritative-E"))
            ;; A regression that turns duplicate D into additional queued work
            ;; would eventually reveal itself as a fourth request. Keep it held
            ;; so that mistake remains deterministic and inspectable.
            (fixture/hold (fragment-response "UNEXPECTED-fourth-refresh"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial SSE-open request A must remain advisory and headerless.")

            (is (= 1 (emit-progression! server requirement-d))
                "Canonical requirement D must reach the managed EventSource.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "D must queue behind advisory A rather than overlap it.")

            (fixture/release! server request-a-id)

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded-b
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded-b
                  (some-> encoded-b
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded-b)
                  "Bound request B must carry canonical D on its real HTTP request.")

              (is (= #{basis-d} (:bases decoded-b))
                  "B's minimum-read header must contain exactly D's basis.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.fragmentBeforeRequests === 2")

              (is (= 2 (count (fragment-requests server)))
                  "Only advisory A and canonical B exist before the mixed observations.")

              (is (= 1 (emit-progression! server requirement-d))
                  "Duplicate D must genuinely reach the browser while B is physically bound.")

              (is (= 1 (emit-progression! server requirement-e))
                  "New canonical E must genuinely reach the browser while B is physically bound.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseMessages === 3")

              (is (= 2 (count (fragment-requests server)))
                  "Duplicate D plus new E must remain queued behind B without parallel fragment work.")

              (fixture/release! server request-b-id)

              (let [request-c
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-b-id))
                     5000)

                    request-c-id
                    (:request-id request-c)

                    encoded-c
                    (request-header
                     request-c
                     progression.http/request-header-name)

                    decoded-c
                    (some-> encoded-c
                            progression.http/decode-request-progression)]

                (is (= requirement-e decoded-c)
                    "Successor C must carry only genuinely new authority E.")

                (is (= #{basis-e} (:bases decoded-c))
                    "D is already covered by completed B and must not be redundantly retained in C's header.")

                (is (= 1 (count (:bases decoded-c)))
                    "Successor C must contain exactly one distinct canonical basis.")

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.fragmentBeforeRequests === 3")

                (is (= 3 (count (fragment-requests server)))
                    "A, B, and exactly one E successor C are the complete physical request set.")

                (is (= "B-authoritative-D"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "B may settle normally before E's successor becomes canonical.")

                (fixture/release! server request-c-id)

                (chromium/wait-for-js!
                 page
                 (str "() => document.getElementById('"
                      fragment-id
                      "') && document.getElementById('"
                      fragment-id
                      "').textContent === 'C-authoritative-E'"))

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.fragmentAfterRequests === 3")

                (is (= 3
                       (chromium/evaluate
                        page
                        "() => window.__gessoFixture.fragmentBeforeRequests"))
                    "Completing C must not admit a redundant fourth request for duplicate D.")

                (is (= 3 (count (fragment-requests server)))
                    "No hidden post-C refresh may be scheduled from authority already covered by B.")

                (is (empty? (fixture/pending-request-ids server))
                    "No unexpected fourth held refresh may remain after canonical E completes.")

                (is (= "C-authoritative-E"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "The genuinely new E response must remain the final canonical DOM.")

                (is (true? (chromium/assert-clean! context))
                    "Covered duplicate D plus new E must leave the real browser clean.")))))))))


(deftest failed-bound-canonical-refresh-preserves-active-and-queued-authority-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "a failed bound canonical generation preserves its own unsatisfied authority plus queued authority"
        (let [basis-d
              {:tx-id 904
               :system-time "2026-08-29T00:20:04Z"}

              basis-e
              {:tx-id 905
               :system-time "2026-08-29T00:20:05Z"}

              requirement-d
              (progression/requirement basis-d)

              requirement-e
              (progression/requirement basis-e)

              requirement-d+e
              (progression/compose requirement-d requirement-e)]

          ;; Initial SSE open creates advisory A. Canonical D queues behind A
          ;; and becomes physically bound to B. While B is held, genuinely new E
          ;; queues. B then loses its transport before satisfying D. The already
          ;; required successor must therefore carry both still-unsatisfied D
          ;; and queued E; dropping D would weaken the minimum-read contract.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-advisory"))
            (fixture/hold (fragment-response "B-MUST-NOT-BECOME-CANONICAL"))
            (fixture/hold (fragment-response "C-authoritative-D+E"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial SSE-open request A must remain advisory and headerless.")

            (is (= 1 (emit-progression! server requirement-d))
                "Canonical D must reach the browser while advisory A owns the fragment.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "D must queue behind A rather than create parallel physical work.")

            (fixture/release! server request-a-id)

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded-b
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded-b
                  (some-> encoded-b
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded-b)
                  "Physical request B must be bound carrying canonical D.")

              (is (= #{basis-d} (:bases decoded-b))
                  "B's real minimum-read header must contain exactly D's basis.")

              (is (= 2 (count (fragment-requests server)))
                  "Only advisory A and canonical B exist before E arrives.")

              (is (= 1 (emit-progression! server requirement-e))
                  "Genuinely new E must reach the browser while B is physically bound.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseMessages === 2")

              (is (= 2 (count (fragment-requests server)))
                  "E must queue behind B without admitting parallel fragment work.")

              (is (true? (chromium/assert-clean! context))
                  "The browser must be clean before B's deliberate truncated response.")
              (chromium/clear-diagnostics! context)

              ;; B carries D, receives response headers declaring a complete
              ;; body, then the fixture closes after only a strict body prefix.
              ;; Chromium cannot transparently replay that partially observed
              ;; response, so HTMX deterministically takes the sendError path.
              ;; Since D was not satisfied, the successor must retain D as well
              ;; as queued E.
              (fixture/release! server request-b-id :truncate)

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sendErrors === 1")

              (let [request-c
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-b-id))
                     5000)

                    request-c-id
                    (:request-id request-c)

                    encoded-c
                    (request-header
                     request-c
                     progression.http/request-header-name)

                    decoded-c
                    (some-> encoded-c
                            progression.http/decode-request-progression)]

                (is (= requirement-d+e decoded-c)
                    "Successor C after B's transport failure must carry D ∪ E.")

                (is (= #{basis-d basis-e} (:bases decoded-c))
                    "No unsatisfied active or queued canonical basis may be lost across sendError.")

                (is (= 2 (count (:bases decoded-c)))
                    "The successor progression header must contain exactly the two distinct required bases.")

                (is (= 3 (count (fragment-requests server)))
                    "A, failed B, and one composed successor C are the complete physical request set.")

                (is (= "A-advisory"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "Failed B must never install its unsatisfied response as canonical DOM.")

                (is (= 0
                       (chromium/evaluate
                        page
                        "() => window.__gessoFixture.responseErrors"))
                    "The deliberate truncated transport failure must exercise sendError, not HTTP responseError.")

                ;; Keep C held until the semantic assertions above are complete.
                ;; Consume B's browser/network diagnostics only after C settles;
                ;; this avoids racing Playwright's console/request-failure
                ;; callbacks against HTMX's synchronous sendError event.
                (fixture/release! server request-c-id)

                (chromium/wait-for-js!
                 page
                 (str "() => document.getElementById('"
                      fragment-id
                      "') && document.getElementById('"
                      fragment-id
                      "').textContent === 'C-authoritative-D+E'"))

                (is (= "C-authoritative-D+E"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "The composed D ∪ E successor must complete the final real HTMX swap.")

                (is (= 3 (count (fragment-requests server)))
                    "Completing C must not reveal a hidden fourth refresh.")

                (is (true? (consume-expected-send-error-diagnostics! context))
                    "B's deliberate truncated response must produce exactly the expected semantic HTMX/browser diagnostics.")

                (is (true? (chromium/assert-clean! context))
                    "After accounting for B's deliberate transport failure, the completed recovery scenario must leave the browser clean.")))))))))


(deftest failed-bound-canonical-refresh-via-http-error-preserves-active-and-queued-authority-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "a failed bound canonical generation preserves D ∪ E across real HTTP responseError"
        (let [basis-d
              {:tx-id 1004
               :system-time "2026-08-29T00:30:04Z"}

              basis-e
              {:tx-id 1005
               :system-time "2026-08-29T00:30:05Z"}

              requirement-d
              (progression/requirement basis-d)

              requirement-e
              (progression/requirement basis-e)

              requirement-d+e
              (progression/compose requirement-d requirement-e)

              error-response
              (fixture/response
               503
               html-headers
               (str "<div id=\"" fragment-id
                    "\" data-fixture-version=\"error\">"
                    "B-503-MUST-NOT-BECOME-CANONICAL"
                    "</div>"))]

          ;; Initial SSE open creates advisory A. Canonical D queues behind A
          ;; and becomes physically bound to B. While B is held, genuinely new E
          ;; queues. B then receives a completed HTTP 503 response. Since neither
          ;; D nor E has been satisfied by a successful canonical generation,
          ;; the already-required successor must retain both.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-advisory"))
            (fixture/hold (fragment-response "B-unused-success-body"))
            (fixture/hold (fragment-response "C-authoritative-D+E"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial SSE-open request A must remain advisory and headerless.")

            (is (= 1 (emit-progression! server requirement-d))
                "Canonical D must reach the browser while advisory A owns the fragment.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "D must queue behind A rather than create parallel physical work.")

            (fixture/release! server request-a-id)

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded-b
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded-b
                  (some-> encoded-b
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded-b)
                  "Physical request B must be bound carrying canonical D.")

              (is (= #{basis-d} (:bases decoded-b))
                  "B's real minimum-read header must contain exactly D's basis.")

              (is (= 2 (count (fragment-requests server)))
                  "Only advisory A and canonical B exist before E arrives.")

              (is (= 1 (emit-progression! server requirement-e))
                  "Genuinely new E must reach the browser while B is physically bound.")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseMessages === 2")

              (is (= 2 (count (fragment-requests server)))
                  "E must queue behind B without admitting parallel fragment work.")

              ;; Establish a clean diagnostic baseline immediately before the
              ;; deliberate HTTP failure so every subsequent console entry is
              ;; attributable to this exact 503.
              (is (true? (chromium/assert-clean! context))
                  "The browser must be clean before the deliberate 503 response.")
              (chromium/clear-diagnostics! context)

              (fixture/release! server request-b-id error-response)

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.responseErrors === 1")

              (is (= 0
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.sendErrors"))
                  "A completed HTTP 503 must exercise responseError, not sendError.")

              (let [request-c
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %))
                           (> (:request-id %) request-b-id))
                     5000)

                    request-c-id
                    (:request-id request-c)

                    encoded-c
                    (request-header
                     request-c
                     progression.http/request-header-name)

                    decoded-c
                    (some-> encoded-c
                            progression.http/decode-request-progression)]

                (is (= requirement-d+e decoded-c)
                    "Successor C after B's HTTP failure must carry D ∪ E.")

                (is (= #{basis-d basis-e} (:bases decoded-c))
                    "No unsatisfied active or queued canonical basis may be lost across responseError.")

                (is (= 2 (count (:bases decoded-c)))
                    "The successor progression header must contain exactly D and E.")

                (is (= 3 (count (fragment-requests server)))
                    "A, errored B, and one composed successor C are the complete physical request set.")

                (is (= "A-advisory"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "The 503 response body for failed B must never become canonical DOM.")

                (is (true? (consume-expected-response-error-diagnostics!
                            context
                            503))
                    "B's deliberate 503 must produce exactly the expected Chromium/HTMX diagnostics.")

                (is (true? (chromium/assert-clean! context))
                    "After accounting for B's deliberate 503, no unrelated browser failure may remain.")

                (fixture/release! server request-c-id)

                (chromium/wait-for-js!
                 page
                 (str "() => document.getElementById('"
                      fragment-id
                      "') && document.getElementById('"
                      fragment-id
                      "').textContent === 'C-authoritative-D+E'"))

                (is (= "C-authoritative-D+E"
                       (chromium/evaluate
                        page
                        (str "() => document.getElementById('"
                             fragment-id
                             "').textContent")))
                    "The composed D ∪ E successor must complete the final real HTMX swap.")

                (is (= 3 (count (fragment-requests server)))
                    "Completing C must not reveal a hidden fourth refresh.")

                (is (true? (chromium/assert-clean! context))
                    "The completed HTTP-error recovery scenario must leave the browser clean.")))))))))


(deftest two-isolated-browser-contexts-preserve-independent-single-flight-and-shared-authority-test
  (with-real-browser
    (fn [{:keys [server harness context page]}]
      (testing
       "two isolated Chromium contexts independently coordinate one shared authoritative invalidation"
        (let [basis-b
              {:tx-id 1102
               :system-time "2026-08-29T00:40:02Z"}

              requirement-b
              (progression/requirement basis-b)

              context-b
              (chromium/new-context! harness)]
          (try
            (let [page-b
                  (chromium/new-page! context-b)]

              ;; with-real-browser initially scripts one page load. Replace the
              ;; static queues before navigation so both isolated contexts load
              ;; the exact same pinned HTMX/SSE/runtime bytes from real HTTP.
              (script-static-routes! server 2)

              ;; Each context receives an independent advisory request from its
              ;; own EventSource open and, after the shared canonical B event,
              ;; one independently coordinated successor. Keep all four
              ;; physical GETs held so their headers/counts are inspectable.
              (fixture/script!
               server
               :get
               fragment-path
               [(fixture/hold (fragment-response "A-context-1"))
                (fixture/hold (fragment-response "A-context-2"))
                (fixture/hold (fragment-response "B-authoritative"))
                (fixture/hold (fragment-response "B-authoritative"))])

              (chromium/navigate!
               page
               (fixture/url server page-path))

              (chromium/navigate!
               page-b
               (fixture/url server page-path))

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

              (chromium/wait-for-js!
               page-b
               "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 1")

              (chromium/wait-for-js!
               page-b
               "() => window.__gessoFixture.sseOpens === 1")

              (is (= 2
                     (count
                      (filter
                       #(= client-id (:client-id %))
                       (fixture/sse-connections server))))
                  "Each isolated BrowserContext must own a distinct real EventSource connection.")

              (is (= 2 (count (fragment-requests server)))
                  "Exactly one advisory fragment request per browser context may exist before canonical delivery.")

              (let [initial-requests
                    (vec (fragment-requests server))

                    initial-ids
                    (mapv :request-id initial-requests)

                    max-initial-id
                    (apply max initial-ids)]

                (is (= 2 (count initial-requests))
                    "Each browser context must independently admit one advisory initial refresh.")

                (is (every?
                     #(nil?
                       (request-header
                        %
                        progression.http/request-header-name))
                     initial-requests)
                    "Both initial requests are advisory and must remain headerless.")

                (is (= 2 (emit-progression! server requirement-b))
                    "One canonical SSE invalidation must be delivered to both live browser contexts.")

                (chromium/wait-for-js!
                 page
                 "() => window.__gessoFixture.sseMessages === 1")

                (chromium/wait-for-js!
                 page-b
                 "() => window.__gessoFixture.sseMessages === 1")

                (is (= 2 (count (fragment-requests server)))
                    "Shared B must queue independently behind each context's own active A; no parallel successor may start yet.")

                (doseq [request-id initial-ids]
                  (fixture/release! server request-id))

                (let [successor-1
                      (fixture/await-request!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (> (:request-id %) max-initial-id))
                       5000)

                      successor-2
                      (fixture/await-request!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (> (:request-id %) (:request-id successor-1)))
                       5000)

                      successors
                      [successor-1 successor-2]

                      decoded
                      (mapv
                       (fn [request]
                         (some->
                          (request-header
                           request
                           progression.http/request-header-name)
                          progression.http/decode-request-progression))
                       successors)]

                  (is (= [requirement-b requirement-b]
                         decoded)
                      "Each isolated browser must independently carry the same canonical B minimum-read requirement.")

                  (is (every? #(= #{basis-b} (:bases %)) decoded)
                      "Neither context may invent, drop, or merge authority through the other context's coordinator state.")

                  (is (= 4 (count (fragment-requests server)))
                      "Two initial A requests plus one B successor per context are the complete physical request set.")

                  (is (= 2
                         (chromium/evaluate
                          page
                          "() => window.__gessoFixture.fragmentBeforeRequests"))
                      "Context A must observe exactly its own initial request and successor.")

                  (is (= 2
                         (chromium/evaluate
                          page-b
                          "() => window.__gessoFixture.fragmentBeforeRequests"))
                      "Context B must observe exactly its own initial request and successor.")

                  (doseq [{:keys [request-id]} successors]
                    (fixture/release! server request-id))

                  (chromium/wait-for-js!
                   page
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'B-authoritative'"))

                  (chromium/wait-for-js!
                   page-b
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'B-authoritative'"))

                  (chromium/wait-for-js!
                   page
                   "() => window.__gessoFixture.fragmentAfterRequests === 2")

                  (chromium/wait-for-js!
                   page-b
                   "() => window.__gessoFixture.fragmentAfterRequests === 2")

                  (is (= 4 (count (fragment-requests server)))
                      "Settling both successors must not expose cross-context duplicate work.")

                  (is (true? (chromium/assert-clean! context))
                      "Context A must remain browser-clean throughout shared-authority delivery.")

                  (is (true? (chromium/assert-clean! context-b))
                      "Context B must remain browser-clean throughout shared-authority delivery."))))
            (finally
              (chromium/close-context! context-b))))))))


(deftest targeted-canonical-progression-does-not-leak-across-browser-contexts-test
  (with-real-browser
    (fn [{:keys [server harness context page]}]
      (testing
       "canonical progression delivered to one physical SSE connection cannot leak into another browser context"
        (let [basis-b
              {:tx-id 1202
               :system-time "2026-08-29T01:00:02Z"}

              requirement-b
              (progression/requirement basis-b)

              context-b
              (chromium/new-context! harness)]
          (try
            (let [page-b
                  (chromium/new-page! context-b)]

              (script-static-routes! server 2)

              (fixture/script!
               server
               :get
               fragment-path
               [(fixture/hold (fragment-response "A-initial"))
                (fixture/hold (fragment-response "B-initial"))
                (fixture/hold (fragment-response "A-authoritative"))
                (fixture/hold (fragment-response "UNEXPECTED-B-successor"))])

              (chromium/navigate!
               page
               (fixture/url server page-path))

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 1")

              (let [connection-a
                    (fixture/await-sse-client! server client-id 5000)
                    connection-a-id
                    (:connection-id connection-a)
                    request-a
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %)))
                     5000)
                    request-a-id
                    (:request-id request-a)]

                (is (= client-id (:client-id connection-a))
                    "Context A must establish the shared logical client-id on its physical SSE connection.")

                (is
                 (nil?
                  (request-header
                   request-a
                   progression.http/request-header-name))
                 "Context A's initial SSE-open request must be advisory and headerless.")

                (chromium/navigate!
                 page-b
                 (fixture/url server page-path))

                (chromium/wait-for-js!
                 page-b
                 "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

                (chromium/wait-for-js!
                 page-b
                 "() => window.__gessoFixture.sseOpens === 1")

                (let [connections
                      (vec
                       (filter
                        #(= client-id (:client-id %))
                        (fixture/sse-connections server)))
                      connection-b
                      (first
                       (remove
                        #(= connection-a-id (:connection-id %))
                        connections))
                      request-b
                      (fixture/await-pending!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (not= request-a-id (:request-id %)))
                       5000)
                      request-b-id
                      (:request-id request-b)]

                  (is (= 2 (count connections))
                      "Exactly two physical SSE connections must exist for the shared logical client.")

                  (is (some? connection-b)
                      "Context B must own a physical SSE connection distinct from context A.")

                  (is (= client-id (:client-id connection-b))
                      "Context B must share A's logical client-id without sharing its physical connection.")

                  (is (not= connection-a-id (:connection-id connection-b))
                      "The two BrowserContexts must remain physically distinct SSE observers.")

                  (is
                   (nil?
                    (request-header
                     request-b
                     progression.http/request-header-name))
                   "Context B's initial SSE-open request must also be advisory and headerless.")

                  (is (= 2 (count (fragment-requests server)))
                      "Exactly one advisory fragment request per context may exist before targeted authority arrives.")

                  (is (= 1
                         (fixture/emit-sse-connection!
                          server
                          connection-a-id
                          {:event "live-update"
                           :data (progression-payload requirement-b)}))
                      "Canonical B must be written to exactly context A's physical SSE stream.")

                  (chromium/wait-for-js!
                   page
                   "() => window.__gessoFixture.sseMessages === 1")

                  (is (= 0
                         (chromium/evaluate
                          page-b
                          "() => window.__gessoFixture.sseMessages"))
                      "Untargeted context B must not observe context A's canonical progression event.")

                  (is (= 2 (count (fragment-requests server)))
                      "Targeted B must queue behind A's active request without creating parallel work in either context.")

                  (fixture/release! server request-b-id)

                  (chromium/wait-for-js!
                   page-b
                   (str "() => document.getElementById('"
                        fragment-id
                        "') && document.getElementById('"
                        fragment-id
                        "').textContent === 'B-initial'"))

                  (chromium/wait-for-js!
                   page-b
                   "() => window.__gessoFixture.fragmentAfterRequests === 1")

                  (is (= 2 (count (fragment-requests server)))
                      "Settling untargeted B must not create any canonical successor.")

                  (fixture/release! server request-a-id)

                  (let [successor
                        (fixture/await-pending!
                         server
                         #(and (= :get (:method %))
                               (= fragment-path (:path %))
                               (> (:request-id %)
                                  (max request-a-id request-b-id)))
                         5000)
                        successor-id
                        (:request-id successor)
                        decoded
                        (some->
                         (request-header
                          successor
                          progression.http/request-header-name)
                         progression.http/decode-request-progression)]

                    (is (= requirement-b decoded)
                        "Only context A's successor may carry targeted canonical B.")

                    (is (= #{basis-b} (:bases decoded))
                        "The targeted successor must carry exactly basis B and no cross-context authority.")

                    (is (= 3 (count (fragment-requests server)))
                        "Two advisory requests plus one targeted canonical successor are the complete physical request set.")

                    (fixture/release! server successor-id)

                    (chromium/wait-for-js!
                     page
                     (str "() => document.getElementById('"
                          fragment-id
                          "') && document.getElementById('"
                          fragment-id
                          "').textContent === 'A-authoritative'"))

                    (chromium/wait-for-js!
                     page
                     "() => window.__gessoFixture.fragmentAfterRequests === 2")

                    (is (= "B-initial"
                           (chromium/evaluate
                            page-b
                            (str "() => document.getElementById('"
                                 fragment-id
                                 "').textContent")))
                        "Untargeted context B must remain on its own advisory result after A becomes authoritative.")

                    (is (= 0
                           (chromium/evaluate
                            page-b
                            "() => window.__gessoFixture.sseMessages"))
                        "Context B must still have observed zero canonical messages after A settles.")

                    (is (= 3 (count (fragment-requests server)))
                        "No hidden fourth refresh may appear in the untargeted context.")

                    (is (empty? (fixture/pending-request-ids server))
                        "No leaked cross-context successor may remain held in the fixture.")

                    (is (true? (chromium/assert-clean! context))
                        "Targeted context A must remain browser-clean throughout authority delivery.")

                    (is (true? (chromium/assert-clean! context-b))
                        "Untargeted context B must remain browser-clean and authority-free.")))))
            (finally
              (chromium/close-context! context-b))))))))


(deftest targeted-and-shared-canonical-progression-compose-per-browser-context-test
  (with-real-browser
    (fn [{:keys [server harness context page]}]
      (testing
       "private authority followed by shared authority composes independently in each browser context"
        (let [basis-b
              {:tx-id 1302
               :system-time "2026-08-29T01:10:02Z"}

              basis-c
              {:tx-id 1303
               :system-time "2026-08-29T01:10:03Z"}

              requirement-b
              (progression/requirement basis-b)

              requirement-c
              (progression/requirement basis-c)

              requirement-b+c
              (progression/compose requirement-b requirement-c)

              context-b
              (chromium/new-context! harness)]
          (try
            (let [page-b
                  (chromium/new-page! context-b)]

              (script-static-routes! server 2)

              ;; A and B each begin with one advisory refresh. Their successors
              ;; are held generically; after observing the actual progression
              ;; headers we release each with a response chosen by authority,
              ;; so response order cannot accidentally identify the context.
              (fixture/script!
               server
               :get
               fragment-path
               [(fixture/hold (fragment-response "A-initial"))
                (fixture/hold (fragment-response "B-initial"))
                (fixture/hold (fragment-response "held-successor-1"))
                (fixture/hold (fragment-response "held-successor-2"))])

              ;; Establish context A first so its physical SSE connection and
              ;; initial request are identified without relying on collection
              ;; iteration order.
              (chromium/navigate!
               page
               (fixture/url server page-path))

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.sseOpens === 1")

              (let [connection-a
                    (fixture/await-sse-client! server client-id 5000)

                    connection-a-id
                    (:connection-id connection-a)

                    request-a
                    (fixture/await-pending!
                     server
                     #(and (= :get (:method %))
                           (= fragment-path (:path %)))
                     5000)

                    request-a-id
                    (:request-id request-a)]

                (chromium/navigate!
                 page-b
                 (fixture/url server page-path))

                (chromium/wait-for-js!
                 page-b
                 "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

                (chromium/wait-for-js!
                 page-b
                 "() => window.__gessoFixture.sseOpens === 1")

                (let [connections
                      (vec
                       (filter
                        #(= client-id (:client-id %))
                        (fixture/sse-connections server)))

                      connection-b
                      (first
                       (remove
                        #(= connection-a-id (:connection-id %))
                        connections))

                      request-b
                      (fixture/await-pending!
                       server
                       #(and (= :get (:method %))
                             (= fragment-path (:path %))
                             (not= request-a-id (:request-id %)))
                       5000)

                      request-b-id
                      (:request-id request-b)]

                  (is (= 2 (count connections))
                      "Two isolated BrowserContexts must own two physical SSE connections.")

                  (is (some? connection-b)
                      "Context B must have a physical SSE connection distinct from context A.")

                  (is
                   (every?
                    nil?
                    [(request-header
                      request-a
                      progression.http/request-header-name)
                     (request-header
                      request-b
                      progression.http/request-header-name)])
                   "Both initial SSE-open fragment requests must remain advisory and headerless.")

                  ;; First give only A canonical B.
                  (is (= 1
                         (fixture/emit-sse-connection!
                          server
                          connection-a-id
                          {:event "live-update"
                           :data (progression-payload requirement-b)}))
                      "Private canonical B must reach exactly context A's physical connection.")

                  (chromium/wait-for-js!
                   page
                   "() => window.__gessoFixture.sseMessages === 1")

                  (is (= 0
                         (chromium/evaluate
                          page-b
                          "() => window.__gessoFixture.sseMessages"))
                      "Context B must not observe A's private canonical B.")

                  ;; Then broadcast canonical C to both physical connections.
                  ;; A should compose B ∪ C; B should know only C.
                  (is (= 2
                         (fixture/emit-sse!
                          server
                          client-id
                          {:event "live-update"
                           :data (progression-payload requirement-c)}))
                      "Shared canonical C must reach both physical SSE connections.")

                  (chromium/wait-for-js!
                   page
                   "() => window.__gessoFixture.sseMessages === 2")

                  (chromium/wait-for-js!
                   page-b
                   "() => window.__gessoFixture.sseMessages === 1")

                  (is (= 2 (count (fragment-requests server)))
                      "All canonical observations must queue behind the two active advisory requests without parallel work.")

                  ;; Release both advisory requests. Each browser must promote
                  ;; one successor, but with a different minimum-read requirement.
                  (fixture/release! server request-a-id)
                  (fixture/release! server request-b-id)

                  (let [successor-1
                        (fixture/await-pending!
                         server
                         #(and (= :get (:method %))
                               (= fragment-path (:path %))
                               (> (:request-id %)
                                  (max request-a-id request-b-id)))
                         5000)

                        successor-2
                        (fixture/await-pending!
                         server
                         #(and (= :get (:method %))
                               (= fragment-path (:path %))
                               (> (:request-id %)
                                  (max request-a-id request-b-id))
                               (not= (:request-id %)
                                     (:request-id successor-1)))
                         5000)

                        successors
                        [successor-1 successor-2]

                        decoded-by-id
                        (into
                         {}
                         (map
                          (fn [request]
                            [(:request-id request)
                             (some->
                              (request-header
                               request
                               progression.http/request-header-name)
                              progression.http/decode-request-progression)])
                          successors))

                        decoded-requirements
                        (set (vals decoded-by-id))]

                    (is (= #{requirement-b+c requirement-c}
                           decoded-requirements)
                        "A must request B ∪ C while B requests C only; shared authority must not erase or leak private authority.")

                    (is (= #{#{basis-b basis-c}
                             #{basis-c}}
                           (set (map :bases decoded-requirements)))
                        "Successor basis sets must be exactly {B,C} for A and {C} for B.")

                    (is (= 4 (count (fragment-requests server)))
                        "Two advisory requests plus exactly one successor per context are the complete physical request set.")

                    ;; Release according to the actual progression each request
                    ;; carries, not according to request arrival order.
                    (doseq [request successors]
                      (let [decoded
                            (get decoded-by-id (:request-id request))

                            response-text
                            (cond
                              (= requirement-b+c decoded)
                              "A-private-B-plus-shared-C"

                              (= requirement-c decoded)
                              "B-shared-C-only"

                              :else
                              (throw
                               (integration-error
                                :unexpected-multi-context-progression
                                "A multi-context successor carried an unexpected progression requirement."
                                {:request request
                                 :decoded decoded
                                 :expected
                                 #{requirement-b+c requirement-c}})))]
                        (fixture/release!
                         server
                         (:request-id request)
                         (fragment-response response-text))))

                    ;; Whichever physical request was admitted first, the DOM
                    ;; results reveal whether the authority remained attached to
                    ;; the correct browser coordinator.
                    (chromium/wait-for-js!
                     page
                     (str "() => document.getElementById('"
                          fragment-id
                          "') && document.getElementById('"
                          fragment-id
                          "').textContent === 'A-private-B-plus-shared-C'"))

                    (chromium/wait-for-js!
                     page-b
                     (str "() => document.getElementById('"
                          fragment-id
                          "') && document.getElementById('"
                          fragment-id
                          "').textContent === 'B-shared-C-only'"))

                    (is (= 4 (count (fragment-requests server)))
                        "Settling both asymmetric successors must not expose hidden cross-context work.")

                    (is (empty? (fixture/pending-request-ids server))
                        "No cross-context successor may remain pending after both browsers settle.")

                    (is (true? (chromium/assert-clean! context))
                        "Context A must remain browser-clean after composing private B with shared C.")

                    (is (true? (chromium/assert-clean! context-b))
                        "Context B must remain browser-clean with shared C only.")))))
            (finally
              (chromium/close-context! context-b))))))))


(deftest inconsistent-sse-progression-fails-closed-without-advisory-downgrade-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "conflicting canonical progression copies are rejected loudly and cannot become advisory refresh work"
        (let [basis-b
              {:tx-id 1202
               :system-time "2026-08-29T01:20:02Z"}

              basis-c
              {:tx-id 1203
               :system-time "2026-08-29T01:20:03Z"}

              basis-d
              {:tx-id 1204
               :system-time "2026-08-29T01:20:04Z"}

              requirement-b
              (progression/requirement basis-b)

              requirement-c
              (progression/requirement basis-c)

              requirement-d
              (progression/requirement basis-d)

              wire-b
              (progression/requirement->wire requirement-b)

              wire-c
              (progression/requirement->wire requirement-c)

              forged-payload
              (pr-str
               {:progression wire-b
                :invalidation
                {:progression wire-c}})]

          ;; Initial SSE open creates one advisory A. While A is held, send a
          ;; payload whose two protocol-mandated progression copies are both
          ;; individually valid but disagree. Core prevents the SSE swap before
          ;; decoding and must then fail closed rather than downgrade the bad
          ;; authority into an advisory invalidation.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-after-forged-event"))
            (fixture/hold (fragment-response "B-valid-after-rejection"))
            (fixture/hold (fragment-response "UNEXPECTED-extra-refresh"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial request A must remain advisory and headerless.")

            (is (= 1
                   (fixture/emit-sse!
                    server
                    client-id
                    {:event "live-update"
                     :data forged-payload}))
                "The forged managed progression payload must reach exactly one EventSource.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.windowErrors.length === 1")

            (let [message
                  (chromium/evaluate
                   page
                   "() => window.__gessoFixture.windowErrors[0].errorMessage || window.__gessoFixture.windowErrors[0].message")]
              (is (and (string? message)
                       (str/includes?
                        message
                        "Managed Gesso Live SSE progression copies disagree."))
                  (str "The browser must surface the actual rejected progression invariant, got: "
                       (pr-str message))))

            (is (= 1 (count (fragment-requests server)))
                "Rejected canonical data must not create a parallel or queued fragment GET while A is active.")

            (is (true? (consume-expected-managed-sse-progression-rejection!
                        context))
                "The malformed authority must produce exactly one explicit Gesso progression diagnostic.")

            (fixture/release! server request-a-id)

            (chromium/wait-for-js!
             page
             (str "() => document.getElementById('"
                  fragment-id
                  "') && document.getElementById('"
                  fragment-id
                  "').textContent === 'A-after-forged-event'"))

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.fragmentAfterRequests === 1")

            (is (= 1
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.fragmentBeforeRequests"))
                "Finishing A must not reveal an advisory successor derived from the rejected payload.")

            (is (= 1 (count (fragment-requests server)))
                "The forged event must contribute zero physical refresh work after A settles.")

            ;; Rejection of one malformed message must not poison the managed
            ;; fragment. A later valid canonical progression remains admissible
            ;; and must produce the normal authoritative successor.
            (is (= 1 (emit-progression! server requirement-d))
                "A later valid canonical D must still reach the EventSource.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 2")

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded
                  (some-> encoded
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded)
                  "The first refresh after rejection must carry only the later valid D authority.")

              (is (= #{basis-d} (:bases decoded))
                  "Rejected B/C authority must not leak into the later valid request header.")

              (is (= 2 (count (fragment-requests server)))
                  "A plus one later valid-D successor are the complete physical request set.")

              (fixture/release! server request-b-id)

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-valid-after-rejection'"))

              (is (= 2 (count (fragment-requests server)))
                  "No hidden refresh may appear after valid recovery from the rejected payload.")

              (is (empty? (fixture/pending-request-ids server))
                  "All fixture work must settle after the valid recovery request.")

              (is (true? (chromium/assert-clean! context))
                  "After consuming the deliberate rejection diagnostic, valid subsequent work must finish clean."))))))))


(deftest retired-fragment-aborts-in-flight-request-and-cannot-resurrect-replacement-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "real HTMX cleanup retires queued authority, aborts physical work, and cannot corrupt replacement DOM"
        (let [basis-b
              {:tx-id 1302
               :system-time "2026-08-29T02:00:02Z"}

              requirement-b
              (progression/requirement basis-b)

              remove-path
              "/remove-managed-fragment"]

          ;; Advisory request A owns the managed fragment while canonical B is
          ;; queued behind it. A separate real HTMX request deletes the stable
          ;; behavior-owning root with hx-swap=delete. HTMX 2.0.7 cleans the
          ;; root, emits beforeCleanupElement, and aborts A's XHR. Gesso must
          ;; retire the logical fragment and discard queued B before the
          ;; physical abort lifecycle is reported.
          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold (fragment-response "A-never-delivered-after-retirement"))
            ;; If retirement accidentally preserves B, keep the illicit
            ;; successor deterministic and observable instead of allowing an
            ;; unplanned fixture response.
            (fixture/hold (fragment-response "UNEXPECTED-successor-B"))])

          (fixture/script!
           server
           :get
           remove-path
           [(fixture/respond (html-response ""))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial request A must be advisory and headerless.")

            (is (= 1 (emit-progression! server requirement-b))
                "Canonical B must reach the managed EventSource while A is physically held.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (is (= 1 (count (fragment-requests server)))
                "B must queue behind A rather than creating parallel fragment work.")

            ;; Observe the actual HTMX cleanup/abort lifecycle directly on the
            ;; behavior-owning root before removal. Once detached, its events no
            ;; longer need to bubble to document, but listeners on the detached
            ;; object remain a deterministic barrier.
            (is
             (true?
              (chromium/evaluate
               page
               (str
                "() => {"
                "  const root = document.querySelector('[data-gesso-live-fragment]');"
                "  if (!root) return false;"
                "  window.__gessoFixture.managedCleanupEvents = 0;"
                "  window.__gessoFixture.detachedAfterRequests = 0;"
                "  window.__gessoFixture.detachedSendAborts = 0;"
                "  root.addEventListener('htmx:beforeCleanupElement', function (event) {"
                "    if (event.target === root) {"
                "      window.__gessoFixture.managedCleanupEvents += 1;"
                "    }"
                "  });"
                "  root.addEventListener('htmx:afterRequest', function (event) {"
                "    if (event.target === root) {"
                "      window.__gessoFixture.detachedAfterRequests += 1;"
                "    }"
                "  });"
                "  root.addEventListener('htmx:sendAbort', function (event) {"
                "    if (event.target === root) {"
                "      window.__gessoFixture.detachedSendAborts += 1;"
                "    }"
                "  });"
                "  const button = document.createElement('button');"
                "  button.id = 'remove-managed-fragment';"
                "  button.setAttribute('hx-get', '" remove-path "');"
                "  button.setAttribute('hx-target', '[data-gesso-live-fragment]');"
                "  button.setAttribute('hx-swap', 'delete');"
                "  document.body.appendChild(button);"
                "  window.htmx.process(button);"
                "  button.click();"
                "  return true;"
                "}")))
             "The real HTMX deletion control must be installed and activated.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.managedCleanupEvents === 1
                    && window.__gessoFixture.detachedAfterRequests === 1
                    && window.__gessoFixture.detachedSendAborts === 1
                    && document.querySelector('[data-gesso-live-fragment]') === null")

            (is (= 1
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.managedCleanupEvents"))
                "HTMX must emit exactly one cleanup event for the removed managed fragment root.")

            (is (= 1
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.detachedAfterRequests"))
                "The aborted in-flight XHR must report exactly one afterRequest lifecycle event.")

            (is (= 1
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.detachedSendAborts"))
                "HTMX cleanup must report exactly one sendAbort for request A.")

            (is (= 1 (count (fragment-requests server)))
                "Retirement itself must not promote queued B into a successor.")

            ;; Reuse the old inner target's DOM id deliberately. Even though
            ;; HTMX has already physically aborted A, this still verifies that
            ;; cleanup did not leave behavior ownership capable of resurrecting
            ;; or mutating a fresh unrelated node with the same id.
            (is
             (true?
              (chromium/evaluate
               page
               (str
                "() => {"
                "  const replacement = document.createElement('div');"
                "  replacement.id = '" fragment-id "';"
                "  replacement.setAttribute('data-fixture-replacement', 'true');"
                "  replacement.textContent = 'replacement-survivor';"
                "  document.body.appendChild(replacement);"
                "  return true;"
                "}")))
             "A fresh replacement node reusing the old target id must be installed.")

            (is (= "replacement-survivor"
                   (chromium/evaluate
                    page
                    (str "() => document.getElementById('"
                         fragment-id
                         "').textContent")))
                "The replacement must survive the real HTMX cleanup/abort lifecycle.")

            ;; The client has aborted A, but the deterministic fixture handler
            ;; remains held until explicitly released. Release it only to retire
            ;; server-side fixture ownership; the response is no longer a stale
            ;; browser completion and must not be treated as one.
            (fixture/release! server request-a-id)

            (is (= "replacement-survivor"
                   (chromium/evaluate
                    page
                    (str "() => document.getElementById('"
                         fragment-id
                         "').textContent")))
                "Draining the server-side held request after client abort must not mutate the replacement.")

            (is
             (true?
              (chromium/evaluate
               page
               (str
                "() => {"
                "  const replacement = document.getElementById('" fragment-id "');"
                "  return !!replacement"
                "    && replacement.getAttribute('data-fixture-replacement') === 'true'"
                "    && document.querySelector('[data-gesso-live-fragment]') === null;"
                "}")))
             "No managed fragment root may be resurrected after retirement.")

            (is (= 1 (count (fragment-requests server)))
                "Queued canonical B must have been discarded with retirement; no successor GET may appear.")

            (is (empty? (fixture/pending-request-ids server))
                "Draining aborted A must leave no fixture ownership or illicit successor.")

            (is (= 0
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.sendErrors"))
                "HTMX sendAbort is distinct from the transport sendError contract.")

            (is (= 0
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.responseErrors"))
                "HTMX sendAbort is distinct from an HTTP responseError.")

            (is
             (true?
              (consume-expected-managed-fragment-abort-diagnostics!
               context))
             "The deliberate physical abort must be consumed through an exact, local diagnostic contract.")

            (is (true? (chromium/assert-clean! context))
                "After consuming the expected abort diagnostics, fragment retirement must leave Chromium clean.")))))))

(deftest continuity-restore-failure-is-local-and-later-refresh-recovers-test
  (with-real-browser
    (fn [{:keys [server context page]}]
      (testing
       "continuity restore failure cannot roll back canonical DOM or poison later fragment refreshes"
        (let [basis-d
              {:tx-id 1404
               :system-time "2026-08-29T02:20:04Z"}

              requirement-d
              (progression/requirement basis-d)]

          ;; Enable the ordinary server-authored client-continuity metadata.
          ;; The first advisory refresh will be swapped normally, but the test
          ;; makes the browser's next requestAnimationFrame invocation throw at
          ;; continuity's post-layout restore boundary. This exercises the real
          ;; continuity Promise rejection -> shell continuity/failed -> adapter
          ;; release path without replacing any Gesso handler.
          (script-static-routes!
           server
           1
           (page-html
            server
            {:client-continuity true}))

          (fixture/script!
           server
           :get
           fragment-path
           [(fixture/hold
             (fragment-response "A-canonical-despite-continuity-failure"))
            (fixture/hold
             (fragment-response "B-recovered-continuity"))])

          (chromium/navigate!
           page
           (fixture/url server page-path))

          (chromium/wait-for-js!
           page
           "() => window.__gessoFixture && window.__gessoFixture.runtimeStarted === true")

          (fixture/await-sse-client! server client-id 5000)

          (let [request-a
                (fixture/await-pending!
                 server
                 #(and (= :get (:method %))
                       (= fragment-path (:path %)))
                 5000)

                request-a-id
                (:request-id request-a)]

            (is
             (nil?
              (request-header
               request-a
               progression.http/request-header-name))
             "Initial SSE-open request A must remain advisory and headerless.")

            (is
             (true?
              (chromium/evaluate
               page
               "() => {
                  const root = document.querySelector('[data-gesso-live-fragment]');
                  return !!root
                    && root.getAttribute('data-gesso-live-continuity') === 'true';
                }"))
             "The production page must genuinely enable Gesso client continuity.")

            ;; Save and sabotage the physical RAF primitive only for the first
            ;; restore. after-layout! catches this synchronous browser failure
            ;; and returns a rejected Promise; the shell must translate that
            ;; rejection into the current continuity slot's failed lifecycle.
            (is
             (true?
              (chromium/evaluate
               page
               "() => {
                  window.__gessoFixture.originalRequestAnimationFrame =
                    window.requestAnimationFrame;
                  window.__gessoFixture.rafFaults = 0;
                  window.requestAnimationFrame = function () {
                    window.__gessoFixture.rafFaults += 1;
                    throw new Error('fixture continuity requestAnimationFrame failure');
                  };
                  return true;
                }"))
             "The continuity-only browser fault must be installed.")

            (fixture/release! server request-a-id)

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.continuityCaptured === 1")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.rafFaults === 1")

            ;; Remove the physical fault immediately after continuity has hit it
            ;; once, so no unrelated browser behavior is affected and the next
            ;; refresh can demonstrate recovery.
            (is
             (true?
              (chromium/evaluate
               page
               "() => {
                  window.requestAnimationFrame =
                    window.__gessoFixture.originalRequestAnimationFrame;
                  return typeof window.requestAnimationFrame === 'function';
                }"))
             "The browser RAF primitive must be restored after the deliberate continuity failure.")

            (chromium/wait-for-js!
             page
             (str "() => document.getElementById('"
                  fragment-id
                  "') && document.getElementById('"
                  fragment-id
                  "').textContent === 'A-canonical-despite-continuity-failure'"))

            ;; The canonical swap already happened before continuity restoration
            ;; failed. Continuity is explicitly not choreography truth, so its
            ;; physical failure cannot roll back, reinterpret, or hide that DOM.
            (is (= "A-canonical-despite-continuity-failure"
                   (chromium/evaluate
                    page
                    (str "() => document.getElementById('"
                         fragment-id
                         "').textContent")))
                "Continuity failure must not roll back the successfully installed canonical fragment.")

            (chromium/wait-for-js!
             page
             "() => {
                const d = window.gessoLive.diagnostics();
                const shell = d.core && d.core.shell;
                const resources = shell && shell['resource-counts'];
                return !!shell
                  && shell['active-continuity-slots'] === 0
                  && !!resources
                  && resources.continuity === 0;
              }")

            (is (= 1
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.continuityCaptured"))
                "Exactly one continuity resource must have been captured for failed restore A.")

            (is (= 0
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.continuityRestored"))
                "A failed restore must not emit the successful restored event.")

            (is (= 0
                   (chromium/evaluate
                    page
                    "() => window.__gessoFixture.continuityErrors"))
                "RAF-level restore rejection is shell-managed and must not masquerade as a malformed continuity-config DOM error.")

            (is (true? (chromium/assert-clean! context))
                "Handled continuity failure must not escape as an uncaught browser error.")

            ;; A later canonical invalidation proves the failed physical
            ;; continuity resource did not poison the fragment coordinator.
            (is (= 1 (emit-progression! server requirement-d))
                "Canonical D must reach the browser after continuity failure.")

            (chromium/wait-for-js!
             page
             "() => window.__gessoFixture.sseMessages === 1")

            (let [request-b
                  (fixture/await-pending!
                   server
                   #(and (= :get (:method %))
                         (= fragment-path (:path %))
                         (> (:request-id %) request-a-id))
                   5000)

                  request-b-id
                  (:request-id request-b)

                  encoded-b
                  (request-header
                   request-b
                   progression.http/request-header-name)

                  decoded-b
                  (some-> encoded-b
                          progression.http/decode-request-progression)]

              (is (= requirement-d decoded-b)
                  "Recovery request B must carry the later canonical requirement D.")

              (is (= #{basis-d} (:bases decoded-b))
                  "The failed continuity slot must contribute no authority to later progression.")

              (fixture/release! server request-b-id)

              (chromium/wait-for-js!
               page
               (str "() => document.getElementById('"
                    fragment-id
                    "') && document.getElementById('"
                    fragment-id
                    "').textContent === 'B-recovered-continuity'"))

              (chromium/wait-for-js!
               page
               "() => window.__gessoFixture.continuityRestored === 1")

              (chromium/wait-for-js!
               page
               "() => {
                  const d = window.gessoLive.diagnostics();
                  const shell = d.core && d.core.shell;
                  const resources = shell && shell['resource-counts'];
                  return !!shell
                    && shell['active-continuity-slots'] === 0
                    && !!resources
                    && resources.continuity === 0;
                }")

              (is (= 2
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.continuityCaptured"))
                  "Both swaps must have captured independent continuity resources.")

              (is (= 1
                     (chromium/evaluate
                      page
                      "() => window.__gessoFixture.continuityRestored"))
                  "Only the second, post-recovery continuity generation may report successful restoration.")

              (is (= 2 (count (fragment-requests server)))
                  "Continuity failure must not create retry or hidden fragment refresh work.")

              (is (empty? (fixture/pending-request-ids server))
                  "All physical HTTP ownership must be retired after continuity recovery.")

              (is (true? (chromium/assert-clean! context))
                  "The recovered continuity scenario must finish browser-clean."))))))))

(defn -main
  [& _]
  (let [{:keys [fail error] :as summary}
        (run-tests 'gesso.live.browser.chromium-integration)]
    (when (pos? (+ fail error))
      (throw
       (integration-error
        :real-browser-tests-failed
        (str "Gesso real-browser integration failed with "
             fail " assertion failure(s) and "
             error " error(s).")
        {:summary summary})))
    summary))
