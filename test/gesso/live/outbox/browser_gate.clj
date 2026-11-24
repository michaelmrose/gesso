(ns gesso.live.outbox.browser-gate
  "v2113: explicit, fail-closed real-Chromium admission and convergence gate.

   A real navigation probe exercises the SAME Playwright/system-Chromium path
   that the concrete-host convergence scenario uses. Browser policy blocks,
   missing executables and navigation failures are reported distinctly. A
   successful probe is ONLY environment readiness; the default command must
   subsequently pass the actual EventSource/HTMX/DOM scenario. Neither a
   simulated DOM nor the HTTP-only readiness tests can satisfy that gate.

   Entry points from the Gesso root:
     clojure -M:test -m gesso.live.outbox.browser-gate --preflight
     clojure -M:test -m gesso.live.outbox.browser-gate --readiness
     clojure -M:test -m gesso.live.outbox.browser-gate

   The Chromium selected by the existing harness (including an explicit
   GESSO_CHROMIUM override) is always subject to its installed policies; this
   gate never changes or bypasses those policies."
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [gesso.assurance.example.physical-single-node-durable-live-test :as durable]
            [gesso.assurance.example.physical-single-node-session-test :as session]
            [gesso.live.browser.chromium :as chromium]
            [gesso.live.outbox.browser-integration :as integration]
            [gesso.live.outbox.browser-restart-test :as restart]
            [gesso.live.outbox.command-race-test :as claim]
            [gesso.live.outbox.frontier-recovery-test :as frontier]
            [gesso.live.outbox.http-fragment-test :as installed])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(def ^:private error-type :gesso.live.outbox/browser-gate-error)
(def ^:private probe-path "/__gesso_outbox_browser_probe")
(def ^:private probe-html
  "<!doctype html><html><body><p id=\"gesso-browser-probe\">ready</p></body></html>")

(defn- reject! [reason data]
  (throw (ex-info "Gesso outbox Chromium gate is not satisfied."
                  (assoc data :error/type error-type :reason reason))))

(defn navigation-failure-reason
  "Classify a real Playwright navigation failure; never infer app success.

   Inspect the cause chain because Playwright and Gesso's Chromium wrapper
   may add contextual exceptions. Only the browser's explicit policy error
   is classified as an administrative navigation block; all other failures
   remain independent errors that must be diagnosed, not skipped."
  [error]
  (let [messages (map #(.getMessage ^Throwable %)
                      (take-while some? (iterate #(.getCause ^Throwable %) error)))]
    (if (some #(and (string? %)
                   (str/includes? % "ERR_BLOCKED_BY_ADMINISTRATOR"))
              messages)
      :managed-navigation-block
      :navigation-failed)))

(defn- start-probe-server! []
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        response (.getBytes probe-html StandardCharsets/UTF_8)]
    (.createContext
     server probe-path
     (reify HttpHandler
       (handle [_ exchange]
         (try
           (.set (.getResponseHeaders exchange) "content-type"
                 "text/html; charset=utf-8")
           (.sendResponseHeaders exchange 200 (alength response))
           (with-open [body (.getResponseBody exchange)]
             (.write body response))
           (finally (.close exchange))))))
    (.start server)
    {:url (str "http://127.0.0.1:" (.getPort (.getAddress server)) probe-path)
     :stop! #(.stop server 0)}))

(defn preflight!
  "Navigate real Chromium to an isolated loopback page and read its DOM.

   The probe does not use Gesso application code or pretend to establish
   convergence. A managed browser block, failure to launch, and unexpected
   probe content all fail closed. The caller retains no browser resources."
  []
  (let [server (start-probe-server!)
        browser* (atom nil)
        context* (atom nil)]
    (try
      (let [harness (chromium/start! {:args ["--no-sandbox"
                                             "--disable-dev-shm-usage"]})
            _ (reset! browser* harness)
            context (chromium/new-context! harness)
            _ (reset! context* context)
            page (chromium/new-page! context)]
        (try
          (chromium/navigate! page (:url server))
          (catch Throwable cause
            (throw (ex-info
                    "Chromium could not open the local Gesso test origin."
                    {:error/type error-type
                     :reason (navigation-failure-reason cause)}
                    cause))))
        (when-not (= "ready"
                     (chromium/evaluate
                      page "() => document.getElementById('gesso-browser-probe')?.textContent"))
          (reject! :probe-document-mismatch {}))
        {:status :environment-ready
         :browser-executable (str (:chromium-executable harness))
         :browser-convergence-verified? false})
      (finally
        (try
          (when-let [context @context*]
            (chromium/close-context! context))
          (finally
            (try
              (when-let [harness @browser*]
                (chromium/stop! harness))
              (finally
                ((:stop! server))))))))))

(defn- test-selection! [vars]
  (when-not (and (seq vars) (= (count vars) (count (distinct vars)))
                 (every? #(and (var? %) (fn? (:test (meta %)))) vars))
    (reject! :invalid-test-selection {:selected (count vars)}))
  ;; Keep individual evidence: an assertion-free test must not borrow
  ;; passing assertions from other selected scenarios.
  (let [cases (mapv (fn [v]
                      (let [identity (symbol (str (ns-name (:ns (meta v))))
                                             (str (:name (meta v))))
                            result (binding [test/*report-counters*
                                             (ref test/*initial-report-counters*)]
                                     (test/test-vars [v])
                                     @test/*report-counters*)]
                        (when-not (and (= 1 (:test result)) (pos? (:pass result))
                                       (zero? (:fail result)) (zero? (:error result)))
                          (reject! :assertion-failure {:case identity :results result}))
                        (assoc result :case identity)))
                    vars)]
    (assoc (apply merge-with + (map #(dissoc % :case) cases)) :cases cases)))

(defn- verified-selection! [results]
  ;; Be defensive even if a custom test runner returns a summary instead of
  ;; throwing. A zero-test or failing summary can NEVER authorize the gate.
  (when-not (and (map? results)
                 (integer? (:test results)) (pos? (:test results))
                 (integer? (:pass results)) (pos? (:pass results))
                 (integer? (:fail results)) (zero? (:fail results))
                 (integer? (:error results)) (zero? (:error results)))
    (reject! :assertion-failure {:results results}))
  results)

(defn run-readiness!
  "HTTP/XTDB tests only. This is explicitly NOT browser convergence."
  []
  (binding [claim/*claim-browser-observer?* false]
   (test-selection!
   [#'integration/canonical-browser-page-and-authoritative-fragment-over-loopback-test
    #'integration/actual-http-fragment-read-honors-source-progression-test
    #'integration/physical-sse-frame-progresses-exact-authoritative-fragment-http-test
    #'integration/two-source-events-refresh-one-http-sse-connection-test
    #'installed/assembled-streams-share-capacity-and-use-original-model-policy
    #'installed/installed-model-command-recovers-submitted-transaction-without-replay-write
    #'frontier/submitted-receipt-survives-cycle-interruption-and-rescan-test
    #'session/actual-jetty-requests-require-server-issued-session-and-csrf
    #'durable/managed-worker-delivers-protected-http-writes-without-manual-processing
    #'durable/managed-worker-scope-retirement-precedes-provider-close-on-exception
    #'durable/closed-application-reopens-persisted-reservation-and-recovers-pending-intent
    #'claim/distinct-claim-commands-contend-for-one-primary-assignment])))

(defn run-browser!
  "The STRICT real-browser scenario, not the HTTP-only readiness selection."
  []
  (binding [claim/*claim-browser-observer?* true]
   (test-selection!
   [#'integration/concrete-host-eventsource-htmx-converges-to-authoritative-xtdb-test
    #'integration/recovered-biff-worker-redelivers-to-unchanged-chromium-fragment-test
    #'integration/late-independent-browser-interests-acquire-without-replayed-notice-test
    #'restart/browser-survives-physical-host-jvm-kill-and-reopens-persistent-xtdb-test
    #'claim/distinct-claim-commands-contend-for-one-primary-assignment])))

(defn execute!
  "Run a selected gate. The default browser gate requires preflight AND
   the strict scenario. An unusable environment must not run that scenario,
   and --readiness must never be reported as browser convergence."
  [mode]
  (case mode
    :preflight
    (preflight!)

    :readiness
    {:status :http-readiness-passed
     :browser-convergence-verified? false
     :results (verified-selection! (run-readiness!))}

    :browser
    (let [environment (preflight!)]
      (when-not (= :environment-ready (:status environment))
        (reject! :browser-preflight-not-ready
                 {:status (:status environment)}))
      (let [results (verified-selection! (run-browser!))]
        {:status :browser-convergence-passed
         :browser-convergence-verified? true
         :environment environment
         :results results}))

    (reject! :invalid-mode {:mode mode})))

(defn -main [& args]
  (let [mode (case (vec args)
               [] :browser
               ["--preflight"] :preflight
               ["--readiness"] :readiness
               (reject! :invalid-arguments {:arguments (vec args)}))]
    (try
      (let [result (execute! mode)]
        (println "GESSO OUTBOX CHROMIUM GATE" (pr-str result))
        (shutdown-agents)
        (System/exit 0))
      (catch Throwable failure
        (binding [*out* *err*]
          (println "GESSO OUTBOX CHROMIUM GATE FAILED"
                   (pr-str (or (ex-data failure)
                               {:error-class (.getName (class failure))})))
          (.printStackTrace failure))
        (shutdown-agents)
        (System/exit 1)))))
