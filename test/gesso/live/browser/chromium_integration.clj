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
