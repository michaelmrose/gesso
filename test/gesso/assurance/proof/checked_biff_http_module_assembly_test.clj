(ns gesso.assurance.proof.checked-biff-http-module-assembly-test
  "v1783: Exact original module inventory and order are checked before Biff
   starts a selected HTTP adapter. Actual successful startup still requires
   the real loopback challenge, not just an ID or a copied module map."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-adapter-start-test]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
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

(def ^:private previous-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private scenario-ns
  'gesso.assurance.proof.checked-biff-http-adapter-start-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [n sym]
  (var-get (or (ns-resolve n sym)
               (throw (ex-info "Missing test fixture" {:ns n :sym sym})))))
(defn- model [sym] (helper model-ns sym))
(defn- scenario [mode typed?] ((helper scenario-ns 'scenario) mode typed?))
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

(defn- start!
  ([s] (start! s (:modules s)
               [app/application-handler-module-id
                host/module-id probe/module-id guarded/module-id]))
  ([s modules order]
   (binding [*modules* modules]
     (sealed/start-closed-checked-http-application!
      (get-in s [:env :contract :application-assembly]) {}
      (:adopted s) (:probe s) (:guarded s) #'*modules* order))))

(defn- cleanup! [s started]
  (when started (try (biff/stop (:system started)) (catch Throwable _ nil)))
  ;; Cleanup may already have been performed by the guarded module.
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (stop (or (:system started) {}))))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

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

(deftest sealed-start-verifies-original-four-modules-and-real-http-before-return
  (let [s (scenario :honest false)
        d (get-in s [:env :data])
        request (:request d)
        binding (:binding d)
        started (atom nil)]
    ((model 'seed!) [:requests request])
    (try
      (reset! started (start! s))
      (let [r (:report @started)]
        (is (= sealed/report-type (:type r)))
        (is (= :exact-original-four-modules-by-identity (:module-inventory r)))
        (is (= :complete-unique-and-canonical (:start-order r)))
        (is (= :rejected-in-selected-var (:additional-modules r)))
        (is (= :one-selected-verified-adapter-start (:physical-server-start r)))
        (is (= :not-established (:independent-biff-instances r)))
        (is (= :not-established (:external-listeners r)))
        (is (= :none (:authority r) (:transaction-authority r)))
        (is (= :verified-selected-adapter-start
               (:status ((:summary! (:guarded s))))))
        (is (= request ((model 'read-request) (:xt/id request))))
        (is (nil? ((model 'read-command) (resolution/record-id binding))))
        (is (= 200 (post! s)))
        (is (some? ((model 'read-command) (resolution/record-id binding)))))
      (finally (cleanup! s @started)))))

(deftest sealed-start-preserves-one-typed-atomic-post-through-verified-adapter
  (let [s (scenario :honest true)
        {:keys [request binding]} (get-in s [:env :data])
        started (atom nil)]
    ((model 'seed!) [:requests request])
    (try
      (reset! started (start! s))
      (is (= 200 (post! s)))
      (is (= :bounded-challenge-and-one-typed-canonical-execution
             (:selected-adapter-typed-atomic-correspondence
              ((:summary! (:probe s))))))
      (is (some? ((model 'read-command) (resolution/record-id binding))))
      (is (= 500 (post! s)))
      (finally (cleanup! s @started)))))

(deftest raw-server-module-cannot-be-added-to-the-sealed-inventory
  (let [s (scenario :honest false)
        modules (conj (:modules s) (:raw s))]
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s modules
                            [app/application-handler-module-id
                             host/module-id probe/module-id guarded/module-id
                             (:biff.core/id (:raw s))]))))
    (is (nil? @(:server-state s)))
    (is (= :issued (:status ((:summary! (:guarded s))))))
    (cleanup! s nil)))

(deftest duplicate-guarded-adapter-and-duplicate-start-id-reject-before-init
  (let [s (scenario :honest false)]
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s (conj (:modules s) (:module (:guarded s)))
                            [app/application-handler-module-id host/module-id
                             probe/module-id guarded/module-id guarded/module-id]))))
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s (:modules s)
                            [app/application-handler-module-id host/module-id
                             probe/module-id probe/module-id]))))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest omitted-probe-or-reordered-start-list-cannot-open-listener
  (let [s (scenario :honest false)]
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s (vec (remove
                                     (fn [m] (= probe/module-id (:biff.core/id m)))
                                     (:modules s)))
                            [app/application-handler-module-id host/module-id
                             guarded/module-id]))))
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s (:modules s)
                            [app/application-handler-module-id host/module-id
                             guarded/module-id probe/module-id]))))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest copied-or-substituted-module-map-cannot-pass-original-identity
  (let [s (scenario :honest false)
        extra (fn [system] (assoc system :corrupt true))
        forged (assoc-in (:modules s) [3 :biff.core/start] extra)]
    (is (= :issued-module-object-identity-mismatch
           (reason #(start! s forged
                            [app/application-handler-module-id host/module-id
                             probe/module-id guarded/module-id]))))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest forged-issued-guard-or-host-is-rejected-before-biff-init
  (let [s (scenario :honest false)
        fake-guard (assoc (:guarded s) :summary! (fn [] {:status :verified-selected-adapter-start}))
        fake-host (assoc (:adopted s) :module {:biff.core/id host/module-id})]
    (is (= :original-guarded-module-not-established
           (reason #(binding [*modules* (:modules s)]
                      (sealed/start-closed-checked-http-application!
                       (get-in s [:env :contract :application-assembly]) {}
                       (:adopted s) (:probe s) fake-guard #'*modules*
                       [app/application-handler-module-id host/module-id
                        probe/module-id guarded/module-id])))))
    (is (= :original-host-module-not-established
           (reason #(binding [*modules* (:modules s)]
                      (sealed/start-closed-checked-http-application!
                       (get-in s [:env :contract :application-assembly]) {}
                       fake-host (:probe s) (:guarded s) #'*modules*
                       [app/application-handler-module-id host/module-id
                        probe/module-id guarded/module-id])))))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest inserted-unrecognized-start-callback-never-runs
  (let [s (scenario :honest false)
        calls (atom 0)
        injected {:biff.core/id :untrusted/raw-server
                  :biff.core/start (fn [system] (swap! calls inc) system)}]
    (is (= :missing-extra-duplicate-or-reordered-http-modules
           (reason #(start! s (conj (:modules s) injected)
                            [app/application-handler-module-id host/module-id
                             probe/module-id guarded/module-id
                             :untrusted/raw-server]))))
    (is (zero? @calls))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest nonvar-module-source-and-map-shaped-module-collection-are-not-accepted
  (let [s (scenario :honest false)]
    (is (= :unsealed-or-nonvector-module-inventory
           (reason #(sealed/start-closed-checked-http-application!
                     (get-in s [:env :contract :application-assembly]) {}
                     (:adopted s) (:probe s) (:guarded s)
                     (:modules s)
                     [app/application-handler-module-id host/module-id
                      probe/module-id guarded/module-id]))))
    (is (= :unsealed-or-nonvector-module-inventory
           (reason #(start! s (list* (:modules s))
                            [app/application-handler-module-id host/module-id
                             probe/module-id guarded/module-id]))))
    (is (nil? @(:server-state s)))
    (cleanup! s nil)))

(deftest original-issued-host-and-adapter-report-no-remote-deployment
  (let [s (scenario :honest false)
        h (host/require-original-issued-biff-host-module! (:adopted s))
        g (guarded/require-original-issued-http-adapter-start! (:guarded s))]
    (is (= :exact-identity (:original-host-module h)))
    (is (= :not-established (:host-deployment h)))
    (is (= :exact-identity (:original-guarded-module g)))
    (is (= :not-established (:external-server-deployment g)))
    (is (= :none (:authority h) (:authority g)))
    (cleanup! s nil)))
