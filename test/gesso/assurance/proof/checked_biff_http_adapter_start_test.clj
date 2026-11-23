(ns gesso.assurance.proof.checked-biff-http-adapter-start-test
  "v2317: An independently selected Biff HTTP adapter cannot finish startup
   until its original final-handler probe passes over an actual HTTP socket.
   On failure its server stop callback runs exactly once.

   v2317: A stop that arrives during original adapter startup cannot steal
   the one-use physical cleanup attempt; only the startup owner can roll back
   a partially opened listener during :failed-cleaning-up. Both adversarial
   interleavings run with the original real Biff/XTDB listener."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe-test]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.resolved-operation-test]
            [gesso.model.command-resolution :as resolution]
            [com.biffweb.core :as biff])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.util.concurrent CountDownLatch ExecutorService TimeUnit]))

(def ^:private prior-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private adapter-ns
  'gesso.assurance.proof.checked-biff-http-consumption-probe-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [ns-name s]
  (var-get (or (ns-resolve ns-name s)
               (throw (ex-info "Missing original helper" {:ns ns-name :symbol s})))))
(defn- model [s] (helper model-ns s))
(defn- prior [s] (helper prior-ns s))
(defn- adapter [mode server-state]
  ((helper adapter-ns 'adapter-module) mode server-state))
(def ^:dynamic *modules* [])
(use-fixtures :once
  (fn [run]
    ((model 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly] (run))))))))

(defn- fixture [] ((prior 'case-env)))
(defn- seed! [& xs] (apply (model 'seed!) xs))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- scenario
  ([mode] (scenario mode nil))
  ([mode typed?]
   (let [env (fixture)
         contract (:contract env)
         adopted (host/issue-checked-biff-host-module! contract)
         config (when typed?
                  {:catalog-source (:catalog-source env)
                   :operation :request/claim
                   :inputs {:input/owner "helper-a"}})
         original (probe/issue-http-consumption-probe-module!
                   (:handle adopted) contract config)
         server-state (atom nil)
         raw (adapter mode server-state)
         guarded-module (guarded/issue-checked-http-adapter-start!
                         original raw
                         (fn [_]
                           (str "http://127.0.0.1:" (:port @server-state))))]
     {:env env :probe original :adopted adopted
      :raw raw :server-state server-state :guarded guarded-module
      :modules [app/application-handler-module (:module adopted)
                (:module original) (:module guarded-module)]})))

(defn- start! [s & [order]]
  (let [ids (or order [app/application-handler-module-id
                       host/module-id probe/module-id guarded/module-id])]
    (binding [*modules* (:modules s)]
      (app/start-biff-application!
       (get-in s [:env :contract :application-assembly]) {} #'*modules* ids))))

(defn- close! [s system]
  (when system (try (biff/stop system) (catch Throwable _ nil)))
  ;; Safety cleanup even when Biff startup threw before returning a system.
  ((:biff.core/stop (:raw s)) (or system {})))

(defn- post! [s]
  (let [port (:port @(:server-state s))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port
                                  "/operations/request/claim")))
                (.timeout (Duration/ofSeconds 6))
                (.POST (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send (HttpClient/newHttpClient) req
                        (HttpResponse$BodyHandlers/ofString)))))

(deftest valid-adapter-is-verified-before-biff-start-returns
  (let [s (scenario :honest)
        {:keys [request binding]} (get-in s [:env :data])
        started (atom nil)]
    (seed! [:requests request])
    (try
      (reset! started (start! s))
      (let [r ((:summary! (:guarded s)))
            pr ((:summary! (:probe s)))]
        (is (= guarded/report-type (:type r)))
        (is (= :verified-selected-adapter-start (:status r)))
        (is (= :one-real-loopback-challenge-observed-before-start-return
               (:final-checked-handler-http-consumption r)))
        (is (= :one-final-checked-wrapper-http-challenge-returned (:status pr)))
        (is (= 1 (:challenge-handler-observations pr)))
        (is (= :not-established (:external-server-deployment r)))
        (is (= :not-issued (:certificate r)))
        (is (= :none (:authority r) (:retry-authority r)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        ;; The challenge is read-only. A real route remains functional.
        (is (= 200 (post! s)))
        (is (some? (read-command (resolution/record-id binding)))))
      (finally (close! s @started)))))

(deftest ignored-handler-is-rejected-and-stopped-before-startup-return
  (let [s (scenario :ignored)]
    (is (= :adapter-start-http-consumption-unverified
           (reason #(start! s))))
    (is (= :failed-and-cleaned-up
           (:status ((:summary! (:guarded s))))))
    (is (= :http-consumption-challenge-unconfirmed
           (:status ((:summary! (:probe s))))))
    (is (some? @(:server-state s)))
    (is (.isShutdown (:executor @(:server-state s))))
    (is (= :adapter-start-already-attempted
           (reason #((:biff.core/start (:module (:guarded s))) {}))))
    (close! s nil)))

(deftest startup-before-probe-never-invokes-the-raw-adapter
  (let [s (scenario :honest)]
    (is (= :original-consumption-probe-not-started
           (reason #(start! s [app/application-handler-module-id
                                host/module-id guarded/module-id probe/module-id]))))
    (is (nil? @(:server-state s)))
    (is (= :failed-and-cleaned-up (:status ((:summary! (:guarded s))))))
    (close! s nil)))

(deftest changed-returned-handler-blocks-startup-and-closes-port
  (let [s (scenario :honest)
        raw (:raw s)
        corrupted (assoc raw :biff.core/start
                         (fn [system]
                           ((:biff.core/start raw) system)
                           (assoc system :biff.ring/handler
                                  (fn [_] {:status 200 :body "forged"}))))
        replaced (guarded/issue-checked-http-adapter-start!
                  (:probe s) corrupted
                  (fn [_] (str "http://127.0.0.1:"
                               (:port @(:server-state s)))))
        cfg (assoc s :guarded replaced
                   :modules [app/application-handler-module
                             (:module (:adopted s))
                             (:module (:probe s)) (:module replaced)])]
    (is (= :adapter-changed-final-checked-handler
           (reason #(start! cfg))))
    (is (.isShutdown (:executor @(:server-state s))))
    (close! s nil)))

(deftest adapter-throwing-after-opening-listener-still-runs-stop
  (let [s (scenario :honest)
        raw (:raw s)
        broken (assoc raw :biff.core/start
                      (fn [system]
                        ((:biff.core/start raw) system)
                        (throw (ex-info "startup lost" {:reason :adapter-start-threw}))))
        guarded-module (guarded/issue-checked-http-adapter-start!
                        (:probe s) broken
                        (fn [_] "http://127.0.0.1:1"))
        cfg (assoc s :guarded guarded-module
                   :modules [app/application-handler-module
                             (:module (:adopted s))
                             (:module (:probe s)) (:module guarded-module)])]
    (is (= :adapter-start-threw (reason #(start! cfg))))
    (is (= :failed-and-cleaned-up
           (:status ((:summary! guarded-module)))))
    (is (.isShutdown (:executor @(:server-state s))))
    (close! s nil)))

(deftest unissued-or-substituted-probe-is-rejected-at-issuance
  (let [s (scenario :honest)
        p (:probe s)]
    (is (= :unissued-or-substituted-http-consumption-probe
           (reason #(guarded/issue-checked-http-adapter-start!
                     (assoc p :verify-once! (fn [& _]
                                              {:status :one-final-checked-wrapper-http-challenge-returned}))
                     (:raw s) (fn [_] "http://127.0.0.1:1")))))
    (is (= :invalid-original-probe-capability-shape
           (reason #(guarded/issue-checked-http-adapter-start!
                     (assoc p :certificate :fabricated)
                     (:raw s) (fn [_] "http://127.0.0.1:1")))))
    (is (= :invalid-selected-http-adapter-module
           (reason #(guarded/issue-checked-http-adapter-start!
                     p (dissoc (:raw s) :biff.core/stop)
                     (fn [_] "http://127.0.0.1:1")))))))

(deftest invalid-target-and-fake-http-200-leave-no-started-adapter
  (let [s (scenario :honest)
        bad (guarded/issue-checked-http-adapter-start!
             (:probe s) (:raw s) (fn [_] "http://example.org:80"))
        cfg (assoc s :guarded bad
                   :modules [app/application-handler-module
                             (:module (:adopted s)) (:module (:probe s))
                             (:module bad)])]
    (is (= :nonloopback-or-unsafe-probe-target (reason #(start! cfg))))
    (is (= :failed-and-cleaned-up (:status ((:summary! bad)))))
    (is (.isShutdown (:executor @(:server-state s))))
    (close! s nil)))

(deftest checked-start-preserves-the-selected-typed-atomic-trial
  (let [s (scenario :honest true)
        {:keys [request binding]} (get-in s [:env :data])
        started (atom nil)]
    (seed! [:requests request])
    (try
      (reset! started (start! s))
      (is (= :verified-selected-adapter-start
             (:status ((:summary! (:guarded s))))))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= 200 (post! s)))
      (is (= :bounded-challenge-and-one-typed-canonical-execution
             (:selected-adapter-typed-atomic-correspondence
              ((:summary! (:probe s))))))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= 500 (post! s)))
      (finally (close! s @started)))))

(deftest stop-occurs-at-most-once-after-successful-start
  (let [s (scenario :honest)
        actual (:biff.core/stop (:raw s))
        calls (atom 0)
        counted (assoc (:raw s) :biff.core/stop
                       (fn [system]
                         (swap! calls inc)
                         (actual system)))
        checked (guarded/issue-checked-http-adapter-start!
                 (:probe s) counted
                 (fn [_] (str "http://127.0.0.1:"
                              (:port @(:server-state s)))))
        cfg (assoc s :guarded checked
                   :modules [app/application-handler-module
                             (:module (:adopted s)) (:module (:probe s))
                             (:module checked)])
        started (start! cfg)]
    (try
      ((:biff.core/stop (:module checked)) started)
      ((:biff.core/stop (:module checked)) started)
      (is (= 1 @calls))
      (is (= :stopped (:status ((:summary! checked)))))
      (finally (close! cfg started)))))

(deftest throwing-stop-callback-never-produces-a-cleanup-guarantee
  (let [s (scenario :ignored)
        original-stop (:biff.core/stop (:raw s))
        broken (assoc (:raw s) :biff.core/stop
                      (fn [system]
                        (original-stop system)
                        (throw (ex-info "stop reported error" {}))))
        guarded-module (guarded/issue-checked-http-adapter-start!
                        (:probe s) broken
                        (fn [_] (str "http://127.0.0.1:"
                                     (:port @(:server-state s)))))
        cfg (assoc s :guarded guarded-module
                   :modules [app/application-handler-module
                             (:module (:adopted s)) (:module (:probe s))
                             (:module guarded-module)])]
    (is (= :adapter-start-http-consumption-unverified
           (reason #(start! cfg))))
    (is (= :failed-cleanup-not-established
           (:status ((:summary! guarded-module)))))
    (is (= :not-established
           (:selected-adapter-cleanup ((:summary! guarded-module)))))
    (close! s nil)))

(deftest racing-external-stop-cannot-consume-provisional-startup-cleanup
  ;; The original adapter's Biff start callback is paused BEFORE it opens its
  ;; real listener. An external stop on a foreign target used to consume the
  ;; only physical cleanup attempt and overwrite :starting with :stopped.
  ;; That let startup open a listener it could no longer close on failure.
  (let [s (scenario :honest)
        raw (:raw s)
        physical-calls (atom 0)
        start-entered (CountDownLatch. 1)
        start-release (CountDownLatch. 1)
        wrapped (assoc raw
                       :biff.core/start
                       (fn [system]
                         (.countDown start-entered)
                         (when-not (.await start-release 8 TimeUnit/SECONDS)
                           (throw (ex-info "Start latch never released" {})))
                         ((:biff.core/start raw) system))
                       :biff.core/stop
                       (fn [system]
                         (swap! physical-calls inc)
                         ((:biff.core/stop raw) system)))
        issued (guarded/issue-checked-http-adapter-start!
                (:probe s) wrapped
                (fn [_] (str "http://127.0.0.1:"
                             (:port @(:server-state s)))))
        cfg (assoc s :guarded issued
                   :modules [app/application-handler-module
                             (:module (:adopted s))
                             (:module (:probe s))
                             (:module issued)])
        starter (atom nil)]
    (try
      (reset! starter (future (try (start! cfg)
                                   (catch Throwable error error))))
      (is (.await start-entered 4000 TimeUnit/MILLISECONDS)
          "Original start must claim lifecycle before opening listener")
      (is (= :starting (:status ((:summary! issued)))))
      (is (= :adapter-start-in-progress
             (deref (future (reason #((:biff.core/stop (:module issued))
                                     {:foreign :shutdown-target})))
                    1500 ::competing-stop-timeout))
          "Concurrent stop rejects rather than consuming provisional startup")
      (is (zero? @physical-calls))
      (is (zero? (:original-stop-attempts ((:summary! issued)))))
      (is (= :not-attempted
             (:selected-adapter-cleanup ((:summary! issued)))))
      (is (not (realized? @starter)))
      (.countDown start-release)
      (let [out (deref @starter 9000 ::start-timeout)]
        (is (map? out)
            "Original real start must finish with the stop capability intact")
        (is (= :verified-selected-adapter-start
               (:status ((:summary! issued)))))
        (is (some? @(:server-state s)))
        ((:biff.core/stop (:module issued)) out)
        (is (= 1 @physical-calls))
        (is (.isShutdown ^ExecutorService
                          (:executor @(:server-state s)))
            "Only the legitimate stop should terminate the real executor")
        (is (= :stopped (:status ((:summary! issued))))))
      (finally
        (.countDown start-release)
        (when @starter (deref @starter 10000 nil))
        (try ((:biff.core/stop raw) {}) (catch Throwable _ nil))))))

(deftest racing-external-stop-cannot-steal-failed-start-rollback
  ;; The original start opens a real listener and then throws. Its own
  ;; cleanup callback pauses before closing the listener, while an external
  ;; stop attempts to replace the startup-owned :failed-cleaning-up state.
  (let [s (scenario :honest)
        raw (:raw s)
        physical-calls (atom 0)
        cleanup-entered (CountDownLatch. 1)
        cleanup-release (CountDownLatch. 1)
        wrapped (assoc raw
                       :biff.core/start
                       (fn [system]
                         ((:biff.core/start raw) system)
                         (throw (ex-info "Original Biff start failed after listening"
                                         {:reason :original-start-failed-after-listening})))
                       :biff.core/stop
                       (fn [system]
                         (swap! physical-calls inc)
                         (.countDown cleanup-entered)
                         (when-not (.await cleanup-release 8 TimeUnit/SECONDS)
                           (throw (ex-info "Stop cleanup never released" {})))
                         ((:biff.core/stop raw) system)))
        issued (guarded/issue-checked-http-adapter-start!
                (:probe s) wrapped
                (fn [_] (str "http://127.0.0.1:"
                             (:port @(:server-state s)))))
        cfg (assoc s :guarded issued
                   :modules [app/application-handler-module
                             (:module (:adopted s))
                             (:module (:probe s))
                             (:module issued)])
        starter (atom nil)]
    (try
      (reset! starter (future (try (start! cfg)
                                   (catch Throwable error error))))
      (is (.await cleanup-entered 5000 TimeUnit/MILLISECONDS)
          "Startup owner begins physical rollback after opening listener")
      (is (= :failed-cleaning-up (:status ((:summary! issued)))))
      (is (= :in-progress (:selected-adapter-cleanup ((:summary! issued)))))
      (is (= 1 @physical-calls))
      (is (= :adapter-start-cleanup-in-progress
             (deref (future (reason #((:biff.core/stop (:module issued))
                                     {:foreign :caller-target})))
                    1500 ::competing-stop-timeout))
          "Concurrent stop must not steal failed startup's rollback claim")
      (is (= 1 @physical-calls))
      (is (= :failed-cleaning-up (:status ((:summary! issued)))))
      (.countDown cleanup-release)
      (let [result (deref @starter 9000 ::failed-start-timeout)]
        (is (= :original-start-failed-after-listening
               (when (instance? clojure.lang.ExceptionInfo result)
                 (:reason (ex-data result)))))
        (is (= :failed-and-cleaned-up
               (:status ((:summary! issued)))))
        (is (= 1 @physical-calls))
        (is (.isShutdown ^ExecutorService
                         (:executor @(:server-state s)))
            "Startup owner actually shut down the original Biff executor"))
      (finally
        (.countDown cleanup-release)
        (when @starter (deref @starter 10000 nil))
        (try ((:biff.core/stop raw) {}) (catch Throwable _ nil))))))
