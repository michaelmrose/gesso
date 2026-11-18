(ns gesso.assurance.proof.checked-biff-http-adapter-start-test
  "v1782: An independently selected Biff HTTP adapter cannot finish startup
   until its original final-handler probe passes over an actual HTTP socket.
   On failure its server stop callback runs exactly once."
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
           [java.time Duration]))

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
