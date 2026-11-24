(ns gesso.assurance.example.physical-single-node-session-chromium
  "v2353: Strict real Chromium/Aleph/session/CSRF/XTDB/Live gate.
   Both direct publication and automatic durable publication must converge.
   Credentials are provisioned out of band, not via a production login."
  (:require [clojure.test :as t :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.assurance.example.physical-single-node-live :as fixture]
            [gesso.live.browser.chromium :as chromium]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.live.outbox.biff :as managed]
            [xtdb.api :as xt])
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
      ;; Distinguish missing assets, missing runtime initialization, and
      ;; absent managed markup instead of timing out on a composite predicate.
      (chromium/wait-for-js! page "() => !!window.htmx")
      (chromium/wait-for-js! page "() => !!window.gessoLive")
      (chromium/wait-for-js! page "() => !!document.querySelector('[data-gesso-proving-live-target]')")
      page)))

(defn- wait-state! [page status revision]
  (chromium/wait-for-js!
   page "([status, revision]) => { const f = document.querySelector('[data-gesso-proving-live-target]'); return !!f && f.dataset.slotStatus === status && f.dataset.slotRevision === revision; }"
   ;; Playwright Java 1.61 rejects boxed Clojure Long arguments. The dataset
   ;; is already a string; compare the exact decimal revision without coercion.
   [status (str revision)]))

(defn- own-token? [page uri issued]
  ;; Return only a boolean, not the credential itself.
  (true? (chromium/evaluate
          page "([uri, token]) => { const b = Array.from(document.querySelectorAll('button[hx-post]')).find(e => e.getAttribute('hx-post') === uri); return !!b && JSON.parse(b.getAttribute('hx-headers'))['x-gesso-proving-csrf'] === token; }"
          [uri (:csrf-token issued)])))

(defn- click-command! [^Page page base uri issued expected-publication]
  (let [^Response response
        (.waitForResponse
         page
         ^Predicate (reify Predicate
                      (test [_ response]
                        (and (= (str base uri) (.url ^Response response))
                             (= "POST" (.method (.request ^Response response))))))
         ^Runnable (fn [] (.click (.locator page (str "button[hx-post='" uri "']")))))]
    (is (= 200 (.status response)))
    (is (= expected-publication
           (.headerValue response "x-gesso-proving-live-publication"))
        "The command must use the selected publication boundary")
    (is (true? (= (:csrf-token issued)
                  (.headerValue (.request response) "x-gesso-proving-csrf")))
        "Actual HTMX POST must carry the rendered session token")))

(defn- outbox-observed? [app operations]
  ;; The durable gate requires both authoritative persisted intent outcomes
  ;; and worker accounting. A browser refresh alone is not evidence of either.
  (let [rows (try (xt/q (:node app) ["SELECT * FROM gesso_live_outbox"])
                  (catch Exception e
                    ;; Lease/ack fields can change the prepared result type.
                    ;; The enclosing Playwright wait remains bounded. Never
                    ;; treat an unavailable observation as a successful check.
                    (if (= :prepared-query-out-of-date (:xtdb.error/code (ex-data e)))
                      ::replan
                      (throw e))))
        status ((:outbox-worker-status app))]
    (and (not= ::replan rows)
         (not= :failed (:status status))
         (= (count operations) (:total-dispatch-accepted status) (count rows))
         (= (frequencies operations) (frequencies (map :gesso.live.outbox/operation rows)))
         (every? #(and (= :dispatch-accepted (:gesso.live.outbox/status %))
                       (= physical/outbox-source-id (:gesso.live.outbox/source-id %))
                       (= (:slot-id app) (:gesso.live.outbox/subject-id %)))
                 rows))))

(defn- wait-publication! [^Page page app operations durable?]
  (if durable?
    (.waitForCondition page
                       (reify BooleanSupplier
                         (getAsBoolean [_] (outbox-observed? app operations))))
    (is (empty? (xt/q (:node app) ["SELECT * FROM gesso_live_outbox"]))
        "Direct publication must not silently select the durable path")))

(defn- exercise-browser! [app a b reader durable?]
  (let [{:keys [system handler loopback-base-url http-report read!
                outstanding-sse-count]} app
        base loopback-base-url
        expected-publication (if durable? "intent-recorded" "source-accepted")
        harness (chromium/start! {:args ["--no-sandbox" "--disable-dev-shm-usage"]})
        contexts (atom [])]
    (try
      (is (identical? handler (:biff.ring/handler system)))
      (is (= :aleph-http1-test-adapter (:biff-owned-loopback-listener http-report)))
      (is (= (if durable? :durable-outbox :direct) (:publication-selection app)))
      (is (= (if durable? :atomic-intent-then-scoped-managed-worker
                 :after-delegated-http-success)
             (get-in app [:live-report :notification-timing])))
      (is (not (contains? app :process-outbox!)))
      (is (= durable? (fn? (:outbox-worker-status app))))
      (is (= (if durable? :started-last :not-selected)
             (:native-worker-module http-report)))
      (is (= durable? (contains? system managed/handle-key)))
      (let [pa (open-session-page! harness contexts base a)
            pb (open-session-page! harness contexts base b)
            pr (open-session-page! harness contexts base reader)]
        (.waitForCondition ^Page pa
                           (reify BooleanSupplier
                             (getAsBoolean [_] (= 3 (outstanding-sse-count)))))
        (doseq [page [pa pb pr]] (wait-state! page "available" 0))
        (is (zero? (:revision (read!))))
        (wait-publication! pa app [] durable?)
        (is (true? (own-token? pa reserve a)))
        (is (true? (own-token? pb reserve b)))
        (is (false? (own-token? pa reserve b)))
        (is (true? (chromium/evaluate pr "() => !document.querySelector('[hx-post], [hx-headers]')")))
        (is (true? (chromium/evaluate pa "() => !document.cookie.includes('gesso-proving-session')")))
        ;; No manual reload or simulated DOM: each existing page must receive
        ;; the SSE wakeup and acquire the authoritative server fragment.
        (click-command! pa base reserve a expected-publication)
        (doseq [page [pa pb pr]] (wait-state! page "reserved" 1))
        (is (= 1 (:revision (read!))))
        (wait-publication! pa app [:slot/reserve] durable?)
        (is (true? (own-token? pb release b)))
        (is (false? (own-token? pb release a)))
        (is (true? (chromium/evaluate pr "() => !document.querySelector('[hx-post], [hx-headers]')")))
        (click-command! pb base release b expected-publication)
        (doseq [page [pa pb pr]] (wait-state! page "available" 2))
        (is (= 2 (:revision (read!))))
        (wait-publication! pb app [:slot/reserve :slot/release] durable?)
        (doseq [ctx @contexts]
          ;; The document declares an icon. No unexpected console errors,
          ;; including resource failures, are suppressed by this gate.
          (is (true? (chromium/assert-clean! ctx)))))
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
        source #(exercise-browser! % a b reader false))))))

(deftest session-protected-chromium-converges-through-managed-durable-outbox
  (let [worker-status (atom nil)]
    (session/with-session-policy!
     policy
     (fn [{:keys [source issue!]}]
       (let [a (issue! :actor/writer)
             b (issue! :actor/writer)
             reader (issue! :actor/reader)]
         (fixture/with-managed-durable-granted-live-proving-application!
          (str "browser-durable-" (random-uuid)) source :biff-aleph-http1
          {:worker-id "browser-durable-worker" :poll-ms 20 :max-items 8}
          (fn [app]
            (reset! worker-status (:outbox-worker-status app))
            (exercise-browser! app a b reader true))))))
    (is (= :stopped (:status (@worker-status))))
    (is (= 2 (:total-dispatch-accepted (@worker-status))))))

(defn -main [& args]
  (try
    (when (seq args) (throw (ex-info "Unexpected browser gate arguments" {})))
    (let [{:keys [test fail error] :as result} (t/run-tests 'gesso.assurance.example.physical-single-node-session-chromium)]
      (println "GESSO SESSION CHROMIUM GATE" (pr-str result))
      (when-not (and (= 2 test) (zero? fail) (zero? error))
        (throw (ex-info "Strict session Chromium gate did not pass" result))))
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable failure
      (.printStackTrace failure)
      (shutdown-agents)
      (System/exit 1))))
