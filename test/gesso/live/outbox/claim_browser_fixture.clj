(ns gesso.live.outbox.claim-browser-fixture
  "Opt-in Chromium observer of the assembled multi-record Claim fixture.
   Uses the production Live runtime and pinned HTMX/SSE assets; no synthetic
   invalidation, direct refresh trigger, or manual fragment fetch."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [gesso.core :as g]
            [gesso.live.core :as live]
            [gesso.live.ui :as ui]
            [gesso.live.browser.chromium :as chromium])
  (:import [com.microsoft.playwright BrowserContext Page]
           [com.microsoft.playwright.options Cookie SameSiteAttribute]
           [java.util.function BooleanSupplier]))

(defn shell-handler [compiled authorized? initial-node]
  (let [assets {"/claim-assets/htmx.js" (slurp (io/resource "META-INF/resources/webjars/htmx.org/2.0.7/dist/htmx.min.js"))
                "/claim-assets/sse.js" (slurp (io/resource "META-INF/resources/webjars/htmx-ext-sse/2.2.4/dist/sse.js"))
                "/claim-assets/runtime.js" (slurp (or (System/getProperty "gesso.claim.browser.runtime")
                                                      "resources/public/js/gesso-live.js"))
                "/claim-assets/bootstrap.js" "gesso.live.browser.runtime.init_BANG_();"}]
    (fn [ctx]
      (cond
        (and (= :get (:request-method ctx)) (contains? assets (:uri ctx)))
        {:status 200 :headers {"content-type" "text/javascript; charset=utf-8"
                               "cache-control" "no-store"} :body (get assets (:uri ctx))}
        (and (= :get (:request-method ctx)) (= "/claim-observer" (:uri ctx)))
        (if (authorized? ctx)
          (let [panel (live/model-fragment-panel compiled :request-list "l1"
                         {:fragment-url "/fragments/request-list?scope=l1"
                          :stream-url "/streams/request-list?scope=l1"
                          :event "live-update"})]
            (g/html-response [:html [:head [:meta {:charset "utf-8"}]
                                     [:title "Claim observer"] [:link {:rel "icon" :href "data:,"}]]
                              [:body (assoc panel 3 (initial-node ctx))
                               [:script {:src "/claim-assets/htmx.js"}]
                               [:script {:src "/claim-assets/sse.js"}]
                               (ui/live-script {:src "/claim-assets/runtime.js"})
                               [:script {:src "/claim-assets/bootstrap.js" :defer true}]]]))
          {:status 403 :headers {"content-type" "text/plain"} :body "Denied"})
        :else {:status 404 :headers {"content-type" "text/plain"} :body "Not found"}))))

(defn open! [base issued subscribed?]
  (let [harness (chromium/start! {:args ["--no-sandbox" "--disable-dev-shm-usage"]})]
    (try
      (let [context (chromium/new-context! harness)]
        (.addCookies ^BrowserContext (:context context)
                     [(doto (Cookie. "gesso-proving-session" (:session-id issued))
                        (.setUrl (str base "/")) (.setHttpOnly true)
                        (.setSameSite SameSiteAttribute/STRICT))])
        (let [page (chromium/new-page! context)]
          (chromium/navigate! page (str base "/claim-observer"))
          (chromium/wait-for-js! page "() => !!window.htmx && !!window.gessoLive && document.querySelector('#request-list-l1 [data-field=status]')?.textContent === 'open'")
          (.waitForCondition ^Page page (reify BooleanSupplier (getAsBoolean [_] (boolean (subscribed?)))))
          (is (= "unassigned" (chromium/evaluate page "() => document.querySelector('#request-list-l1 [data-field=helper]').textContent")))
          (is (true? (chromium/evaluate page "() => !document.cookie.includes('gesso-proving-session')")))
          (chromium/evaluate page "() => { window.claimRoot = document.querySelector('[data-gesso-live-fragment]'); window.claimTarget = document.getElementById('request-list-l1'); }")
          {:harness harness :context context :page page}))
      (catch Throwable e (chromium/stop! harness) (throw e)))))

(defn verify! [{:keys [page context]} helper]
  (chromium/wait-for-js! page "helper => document.querySelector('#request-list-l1 [data-field=status]')?.textContent === 'claimed' && document.querySelector('#request-list-l1 [data-field=helper]')?.textContent === helper" helper)
  (is (true? (chromium/evaluate page "() => window.claimRoot === document.querySelector('[data-gesso-live-fragment]') && window.claimTarget !== document.getElementById('request-list-l1')")))
  (is (true? (chromium/assert-clean! context))))

(defn close! [{:keys [context harness]}]
  (try (chromium/close-context! context) (finally (chromium/stop! harness))))
