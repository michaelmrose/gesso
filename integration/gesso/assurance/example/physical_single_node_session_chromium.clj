(ns gesso.assurance.example.physical-single-node-session-chromium
  "v2340: Strict real Chromium/Aleph/session/CSRF/XTDB/Live gate.
   Credentials are provisioned out of band, not via a production login."
  (:require [clojure.test :as t :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.live.browser.chromium :as chromium])
  (:import (com.microsoft.playwright BrowserContext Page Response)
           (com.microsoft.playwright.options Cookie SameSiteAttribute)
           (java.util.function BooleanSupplier Predicate)))

(def ^:private policy
  {:catalog-id :slot/reservation-lifecycle
   :policy-id :proving/session-browser
   :grants {:actor/writer #{:slot/reserve :slot/release}}
   :read-principals #{:actor/writer :actor/reader}})
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")

(defn- open-session-page! [harness contexts base issued]
  (let [ctx (chromium/new-context! harness)]
    (swap! contexts conj ctx)
    ;; Install only the opaque cookie, never a default CSRF header.
    (.addCookies ^BrowserContext (:context ctx)
                 [(doto (Cookie. "gesso-proving-session" (:session-id issued))
                    (.setUrl (str base "/"))
                    (.setHttpOnly true)
                    (.setSameSite SameSiteAttribute/STRICT))])
    (let [page (chromium/new-page! ctx)]
      (chromium/navigate! page (str base fixture/panel-uri))
      (chromium/wait-for-js!
       page "() => !!window.htmx && !!window.gessoLive && !!document.querySelector('[data-gesso-proving-live-target]')")
      page)))

(defn- wait-state! [page status revision]
  (chromium/wait-for-js!
   page "([status, revision]) => { const f = document.querySelector('[data-gesso-proving-live-target]'); return !!f && f.dataset.slotStatus === status && Number(f.dataset.slotRevision) === revision; }"
   [status revision]))

(defn- own-token? [page uri issued]
  ;; Return only a boolean, not the credential itself.
  (true? (chromium/evaluate
          page "([uri, token]) => { const b = Array.from(document.querySelectorAll('button[hx-post]')).find(e => e.getAttribute('hx-post') === uri); return !!b && JSON.parse(b.getAttribute('hx-headers'))['x-gesso-proving-csrf'] === token; }"
          [uri (:csrf-token issued)])))

(defn- click-command! [^Page page base uri issued]
  (let [^Response response
        (.waitForResponse
         page
         ^Predicate (reify Predicate
                      (test [_ response]
                        (and (= (str base uri) (.url ^Response response))
                             (= "POST" (.method (.request ^Response response))))))
         ^Runnable (fn [] (.click (.locator page (str "button[hx-post='" uri "']")))))]
    (is (= 200 (.status response)))
    (is (true? (= (:csrf-token issued)
                  (.headerValue (.request response) "x-gesso-proving-csrf")))
        "Actual HTMX POST must carry the rendered session token")))

(defn- exercise-browser! [app a b reader]
  (let [{:keys [system handler loopback-base-url http-report read!
                outstanding-sse-count]} app
        base loopback-base-url
        harness (chromium/start! {:args ["--no-sandbox" "--disable-dev-shm-usage"]})
        contexts (atom [])]
    (try
      (is (identical? handler (:biff.ring/handler system)))
      (is (= :aleph-http1-test-adapter (:biff-owned-loopback-listener http-report)))
      (let [pa (open-session-page! harness contexts base a)
            pb (open-session-page! harness contexts base b)
            pr (open-session-page! harness contexts base reader)]
        (.waitForCondition ^Page pa
                           (reify BooleanSupplier
                             (getAsBoolean [_] (= 3 (outstanding-sse-count)))))
        (doseq [page [pa pb pr]] (wait-state! page "available" 0))
        (is (zero? (:revision (read!))))
        (is (true? (own-token? pa reserve a)))
        (is (true? (own-token? pb reserve b)))
        (is (false? (own-token? pa reserve b)))
        (is (true? (chromium/evaluate pr "() => !document.querySelector('[hx-post], [hx-headers]')")))
        (is (true? (chromium/evaluate pa "() => !document.cookie.includes('gesso-proving-session')")))
        ;; No manual reload or simulated DOM: each existing page must receive
        ;; the SSE wakeup and acquire the authoritative server fragment.
        (click-command! pa base reserve a)
        (doseq [page [pa pb pr]] (wait-state! page "reserved" 1))
        (is (= 1 (:revision (read!))))
        (is (true? (own-token? pb release b)))
        (is (false? (own-token? pb release a)))
        (is (true? (chromium/evaluate pr "() => !document.querySelector('[hx-post], [hx-headers]')")))
        (click-command! pb base release b)
        (doseq [page [pa pb pr]] (wait-state! page "available" 2))
        (is (= 2 (:revision (read!))))
        (doseq [ctx @contexts]
          ;; The fixture has no favicon route; do not mask other errors.
          (is (empty? (remove #(and (= :console-error (:kind %))
                                  (= (str base "/favicon.ico")
                                     (get-in % [:location "url"])))
                              (chromium/browser-errors ctx))))))
      (finally
        (try
          (doseq [ctx (reverse @contexts)] (chromium/close-context! ctx))
          (finally (chromium/stop! harness)))))))

(deftest session-protected-chromium-converges-through-actual-aleph
  (session/with-session-policy!
   policy
   (fn [{:keys [source issue!]}]
     (let [a (issue! :actor/writer)
           b (issue! :actor/writer)
           reader (issue! :actor/reader)]
       (fixture/with-biff-aleph-granted-live-proving-application!
        source #(exercise-browser! % a b reader))))))

(defn -main [& args]
  (try
    (when (seq args) (throw (ex-info "Unexpected browser gate arguments" {})))
    (let [{:keys [test fail error] :as result} (t/run-tests 'gesso.assurance.example.physical-single-node-session-chromium)]
      (println "GESSO SESSION CHROMIUM GATE" (pr-str result))
      (when-not (and (= 1 test) (zero? fail) (zero? error))
        (throw (ex-info "Strict session Chromium gate did not pass" result))))
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable failure
      (.printStackTrace failure)
      (shutdown-agents)
      (System/exit 1))))
