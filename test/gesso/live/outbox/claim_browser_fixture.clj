(ns gesso.live.outbox.claim-browser-fixture
  "Opt-in Chromium observer of the assembled multi-record Claim fixture.
   Uses the production Live runtime and pinned HTMX/SSE assets; no synthetic
   invalidation, direct refresh trigger, or manual fragment fetch."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [clojure.test :refer [is]]
            [gesso.core :as g]
            [gesso.live.core :as live]
            [gesso.live.ui :as ui]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.choreo.identity :as identity]
            [gesso.live.browser.chromium :as chromium])
  (:import [com.microsoft.playwright BrowserContext Page Response]
           [com.microsoft.playwright.options Cookie SameSiteAttribute]
           [java.util.function BooleanSupplier Consumer]
           [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]))

(defn claim-action [ctx basis csrf]
  (ui/post-button ctx
                  {:to "/operations/request/claim" :label "Claim"
                   :swap "none" :choreo/op :request/claim
                   :optimistic-binding {:arguments {:request-id "r1"}
                                        :observed-basis basis
                                        :target-id "request-list-l1"}
                   :button-attrs {:id "claim-request"
                                  :hx-headers (json/write-str {"x-gesso-proving-csrf" csrf})}}))

(defn shell-handler [compiled capabilities authorized? csrf-for initial-state]
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
          (let [{:keys [node basis open?]} (initial-state ctx)
                csrf (when open? (csrf-for ctx))
                node (cond-> node csrf
                       (conj (claim-action (ui/with-optimistic-operation-capabilities ctx capabilities)
                                           basis csrf)))
                panel (live/model-fragment-panel compiled :request-list "l1"
                         {:fragment-url "/fragments/request-list?scope=l1"
                          :stream-url "/streams/request-list?scope=l1"
                          :event "live-update"})]
            (assoc-in (g/html-response [:html [:head [:meta {:charset "utf-8"}]
                                     [:title "Claim observer"] [:link {:rel "icon" :href "data:,"}]]
                              [:body (assoc panel 3 node)
                               [:script {:src "/claim-assets/htmx.js"}]
                               [:script {:src "/claim-assets/sse.js"}]
                               (ui/live-script {:src "/claim-assets/runtime.js"})
                               [:script {:src "/claim-assets/bootstrap.js" :defer true}]]])
                      [:headers "cache-control"] "no-store"))
          {:status 403 :headers {"content-type" "text/plain"} :body "Denied"})
        :else {:status 404 :headers {"content-type" "text/plain"} :body "Not found"}))))

(defn- rendered-action [body]
  (when-let [button (re-find #"<button\b[^>]*id=\"claim-request\"[^>]*>.*?</button>" body)]
    (:attrs (xml/parse (ByteArrayInputStream. (.getBytes ^String button StandardCharsets/UTF_8))))))

(defn verify-helper-pages!
  "Exercise the exact serialized page for each session; no browser claim here."
  [fragment! headers-for credentials observed-basis]
  (doseq [who ["a" "b"]]
    (let [response (fragment! "/claim-observer" (headers-for who))
          body (.body response)
          attrs (rendered-action body)
          action (some-> (:data-gesso-live-optimistic attrs) edn/read-string)]
      (is (= 200 (.statusCode response)))
      (is (= "no-store" (.orElse (.firstValue (.headers response) "cache-control") "")))
      (is (= "/operations/request/claim" (:hx-post attrs)))
      (is (= "none" (:hx-swap attrs)))
      (is (= {"x-gesso-proving-csrf" (get-in credentials [who :csrf-token])}
             (some-> (:hx-headers attrs) json/read-str)))
      (is (= {:operation :request/claim :plan-key :request/claim
              :arguments {:request-id "r1"} :target-id "request-list-l1"
              :observed-basis @observed-basis} action))
      (is (not (str/includes? body (get-in credentials [who :session-id]))))))
  (let [reader (fragment! "/claim-observer" (headers-for "reader"))]
    (is (= 200 (.statusCode reader)))
    (is (nil? (rendered-action (.body reader))))
    (is (not (str/includes? (.body reader) (get-in credentials ["reader" :csrf-token]))))))

(defn verify-no-claim-action! [fragment! headers]
  (let [response (fragment! "/claim-observer" headers)]
    (is (= 200 (.statusCode response)))
    (is (nil? (rendered-action (.body response))))))

(defn- open-context! [harness base issued subscribed?]
  (let [context (chromium/new-context! harness)]
    (try
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
        {:context context :page page})
      (catch Throwable e (chromium/close-context! context) (throw e)))))

(defn open! [base issued subscribed?]
  (let [harness (chromium/start! {:args ["--no-sandbox" "--disable-dev-shm-usage"]})]
    (try (assoc (open-context! harness base issued subscribed?) :harness harness)
         (catch Throwable e (chromium/stop! harness) (throw e)))))

(defn verify! [{:keys [page context]} helper]
  (chromium/wait-for-js! page "helper => document.querySelector('#request-list-l1 [data-field=status]')?.textContent === 'claimed' && document.querySelector('#request-list-l1 [data-field=helper]')?.textContent === helper" helper)
  (is (true? (chromium/evaluate page "() => window.claimRoot === document.querySelector('[data-gesso-live-fragment]') && window.claimTarget !== document.getElementById('request-list-l1')")))
  (is (true? (chromium/assert-clean! context))))

(defn- capture-claim! [{:keys [page]}]
  (let [responses (atom [])]
    (.onResponse ^Page page
                 (reify Consumer
                   (accept [_ response]
                     (when (and (str/ends-with? (.url ^Response response) "/operations/request/claim")
                                (= "POST" (.method (.request ^Response response))))
                       (swap! responses conj response)))))
    responses))

(defn- response-result [^Response response]
  (let [marker (xml/parse (ByteArrayInputStream. (.getBytes (.text response) StandardCharsets/UTF_8)))]
    {:status (.status response)
     :settlement (protocol/wire->settlement
                  (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))}))

(defn compete!
  "Click two genuine managed actions on the Playwright owner thread. The server
   fixture's construction barrier forces contention. No test-built command,
   programmatic form submission or synthetic settlement is used."
  [{:keys [harness]} base credentials subscribed-count?]
  (let [opened (atom [])]
    (try
      (doseq [[who n] [["a" 4] ["b" 5]]]
        (swap! opened conj (open-context! harness base (get credentials who)
                                          #(subscribed-count? n))))
      (let [[a b] @opened
            responses (mapv capture-claim! [a b])]
        (doseq [{:keys [page]} [a b]]
          (.click (.locator ^Page page "#claim-request")))
        (.waitForCondition ^Page (:page a)
                           (reify BooleanSupplier
                             (getAsBoolean [_] (every? seq (map deref responses)))))
        (let [results (mapv #(response-result (first @%)) responses)
              settlements (mapv :settlement results)
              helper (some #(when (= :confirmed (:resolution %))
                              (get-in % [:authoritative :projection :helper])) settlements)]
          (is (= [200 200] (mapv :status results)))
          (is (= #{:confirmed :rejected} (set (map :resolution settlements))))
          (is (= 2 (count (set (map :command-id settlements)))))
          (is (= 2 (count (set (map :execution-id settlements)))))
          (when-not helper (throw (ex-info "No confirmed browser Claim" {})))
          (doseq [{:keys [page] :as context} [a b]]
            (verify! context helper)
            (chromium/wait-for-js! page "() => !document.getElementById('claim-request')")
            (chromium/wait-for-js! page "() => window.gessoLive.diagnostics().optimistic['active-optimistic-executions'].length === 0"))
          (is (= [1 1] (mapv #(count @%) responses)))
          results))
      (finally (doseq [{:keys [context]} (reverse @opened)] (chromium/close-context! context))))))

(defn http-race! [invoke]
  (let [a (future (invoke "a" "claim-a" "attempt-a"))
        b (future (invoke "b" "claim-b" "attempt-b"))
        results [(deref a 40000 ::timeout) (deref b 40000 ::timeout)]]
    (when (some #{::timeout} results)
      (future-cancel a) (future-cancel b)
      (throw (ex-info "Claim race timed out" {})))
    results))

(defn race-actors [browser? results]
  (mapv (fn [[who fixed-id helper] result]
          [who (if browser? (identity/raw-value (get-in result [:settlement :command-id])) fixed-id) helper])
        [["a" "claim-a" "helper-1"] ["b" "claim-b" "helper-2"]] results))

(defn close! [{:keys [context harness]}]
  (try (chromium/close-context! context) (finally (chromium/stop! harness))))
