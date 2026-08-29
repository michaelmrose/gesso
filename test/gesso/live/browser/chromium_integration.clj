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
  [server]
  (let [panel
        (live.ui/fragment-panel
         {:id fragment-id
          :src fragment-path
          :stream-url (fixture/sse-url server client-id)
          :event "live-update"
          :swap "outerHTML"})
        ;; fragment-panel intentionally renders an empty canonical target.
        ;; Give this scenario a visible initial marker without changing any
        ;; behavior-owning markup.
        panel'
        (update-in panel [3] conj "initial")]
    (rum/render-static-markup panel')))

(def ^:private browser-observer-script
  (str
   "window.__gessoFixture = {\n"
   "  sseMessages: 0,\n"
   "  sseOpens: 0,\n"
   "  sendErrors: 0,\n"
   "  responseErrors: 0,\n"
   "  fragmentBeforeRequests: 0,\n"
   "  fragmentAfterRequests: 0,\n"
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
   "gesso.live.browser.runtime.init_BANG_();\n"
   "window.__gessoFixture.runtimeStarted = !!window.gessoLive;\n"))

(defn- page-html
  [server]
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
   (fragment-panel-html server)
   "\n<script>\n"
   browser-observer-script
   "</script>\n"
   "</body>\n"
   "</html>\n"))

(defn- script-static-routes!
  [server]
  (fixture/script!
   server
   :get
   htmx-path
   (fixture/respond
    (js-response
     (resource-text!
      "HTMX 2.0.7 WebJar asset"
      htmx-resource-candidates))))

  (fixture/script!
   server
   :get
   sse-extension-path
   (fixture/respond
    (js-response
     (resource-text!
      "htmx-ext-sse 2.2.4 WebJar asset"
      sse-resource-candidates))))

  (fixture/script!
   server
   :get
   gesso-runtime-path
   (fixture/respond
    (js-response (runtime-text!))))

  (fixture/script!
   server
   :get
   page-path
   (fixture/respond
    (html-response
     (page-html server))))

  server)

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
          ;; while that generation owns the fragment. This time A never receives
          ;; an HTTP response: the fixture closes the exchange to force HTMX's
          ;; real sendError lifecycle.
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

            ;; Close the real socket without response headers/body. The runtime
            ;; must treat the resulting HTMX sendError as completion of exactly
            ;; A's physical correlation and promote B into a new generation.
            (fixture/release! server first-request-id :close)

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
                  "A deliberate socket loss should exercise sendError, not HTTP responseError."))))

        (is (true? (chromium/assert-clean! context))
            "Deliberate request failure must not leave console errors, uncaught exceptions, or crashes.")))))


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

              ;; HTMX 2 reports an HTTP response error through responseError and
              ;; may also log that status as console.error. Permit only console
              ;; diagnostics that explicitly identify this deliberate 503; all
              ;; uncaught exceptions, promise rejections, crashes, or unrelated
              ;; console failures remain test failures.
              (let [browser-errors (chromium/browser-errors context)
                    unexpected
                    (remove
                     (fn [{:keys [kind text]}]
                       (and (= :console-error kind)
                            (string? text)
                            (str/includes? text "503")))
                     browser-errors)]
                (is (empty? unexpected)
                    (str "The deliberate 503 produced unexpected browser diagnostics: "
                         (pr-str unexpected)
                         ". Full diagnostics: "
                         (pr-str (chromium/diagnostics context)))))

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
                  "Canonical successor B must complete the real HTMX swap after A's HTTP error."))))))))


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

              (is (true? (chromium/assert-clean! context))
                  "Forced SSE reconnect must not leave console errors, uncaught exceptions, or crashes."))))))))


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

                  (is (true? (chromium/assert-clean! context))
                      "Reconnect followed by canonical progression must leave the browser clean."))))))))))


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

                  (is (true? (chromium/assert-clean! context))
                      "Reconnect plus multiple canonical invalidations must leave the browser clean."))))))))))


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

                  (is (true? (chromium/assert-clean! context))
                      "Reconnect plus duplicate canonical delivery must leave the browser clean."))))))))))


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
