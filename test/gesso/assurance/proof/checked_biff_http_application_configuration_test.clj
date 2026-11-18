(ns gesso.assurance.proof.checked-biff-http-application-configuration-test
  "v1784: The actual application startup entrypoint owns module issuance,
   inventory and order; users configure only contract and a selected adapter.
   All passing claims remain bounded to a real local Biff HTTP adapter."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-http-application-configuration :as config]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.assurance.proof.checked-biff-http-adapter-start-test]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def ^:private scenario-ns
  'gesso.assurance.proof.checked-biff-http-adapter-start-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- helper [ns-name s]
  (var-get (or (ns-resolve ns-name s)
               (throw (ex-info "Missing original test fixture"
                               {:ns ns-name :name s})))))
(defn- model [s] (helper model-ns s))
(defn- scenario [mode] ((helper scenario-ns 'scenario) mode false))
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

(defn- original-source [s & [typed?]]
  (cond-> {:contract-source (get-in s [:env :contract])
           :adapter-module (:raw s)
           :base-url! (fn [_] (str "http://127.0.0.1:"
                                   (:port @(:server-state s))))}
    typed?
    (assoc :typed-trial
           {:catalog-source (get-in s [:env :catalog-source])
            :operation :request/claim
            :inputs {:input/owner "helper-a"}})))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
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
(defn- cleanup! [s issued]
  ((:stop! issued))
  ;; The raw adapter's stop is also idempotent in this retained test fixture.
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (stop {})))

(deftest application-config-start-derives-sealed-inventory-and-real-http-adoption
  (let [s (scenario :honest)
        {:keys [request binding]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    ((model 'seed!) [:requests request])
    (try
      (is (= :issued (:status ((:summary! issued)))))
      (let [result ((:start-once! issued) {})
            report (:report result)]
        (is (= config/report-type (:type report)))
        (is (= :verified-configured-start (:status report)))
        (is (= :internally-derived-original-four-modules
               (:module-inventory report)))
        (is (= :internally-derived-canonical (:module-order report)))
        (is (= :selected-adapter-challenge-passed-during-start
               (:real-http-consumption report)))
        (is (= :not-established (:external-deployment report)))
        (is (= :none (:authority report) (:retry-authority report)))
        (is (not-any? #(contains? report %)
                      [:modules :host-handle :probe :adapter :handler :source
                       :contract :system :tx-ops :receipt]))
        (is (= request ((model 'read-request) (:xt/id request))))
        (is (nil? ((model 'read-command) (resolution/record-id binding))))
        (is (= 200 (post! s)))
        (is (some? ((model 'read-command) (resolution/record-id binding)))))
      (finally (cleanup! s issued)))
    (is (= :stopped (:status ((:summary! issued)))))))

(deftest configured-start-preserves-typed-atomic-post-through-actual-adapter
  (let [s (scenario :honest)
        {:keys [request binding]} (get-in s [:env :data])
        issued (config/issue-configured-checked-http-application!
                (original-source s true))]
    ((model 'seed!) [:requests request])
    (try
      (is (= :verified-configured-start
             (get-in ((:start-once! issued) {}) [:report :status])))
      (is (= 200 (post! s)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             ((model 'read-request) (:xt/id request))))
      (is (some? ((model 'read-command) (resolution/record-id binding))))
      (is (= 500 (post! s)))
      (is (= 1 @(get-in s [:env :environment :constructs])))
      (finally (cleanup! s issued)))))

(deftest invalid-application-configuration-cannot-inject-module-var-or-order
  (let [s (scenario :honest)
        basic (original-source s)]
    (doseq [extra [{:modules-var #'config/issue-configured-checked-http-application!}
                   {:start-order [:untrusted/raw-adapter]}
                   {:server-module (:raw s)}
                   {:certificate :synthetic}]]
      (is (= :invalid-application-owned-http-configuration
             (reason #(config/issue-configured-checked-http-application!
                       (merge basic extra))))))
    (is (= :invalid-application-owned-http-configuration
           (reason #(config/issue-configured-checked-http-application!
                     (assoc basic :typed-trial
                            {:operation :request/claim
                             :inputs {:input/owner "helper-a"}
                             :submit! (fn [] :forged)})))))
    (is (nil? @(:server-state s)))))

(deftest unexpected-preinstalled-biff-handler-rejected-before-server-start
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :invalid-or-preinstalled-biff-initial-system
           (reason #((:start-once! issued)
                     {:biff.ring/handler (fn [_] {:status 200})}))))
    (is (nil? @(:server-state s)))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (is (= :configured-start-already-attempted
           (reason #((:start-once! issued) {}))))
    (cleanup! s issued)))

(deftest configured-start-rejects-selected-adapter-ignoring-checked-handler
  (let [s (scenario :ignored)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :adapter-start-http-consumption-unverified
           (reason #((:start-once! issued) {}))))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (is (.isShutdown (:executor @(:server-state s))))
    (is (= :configured-start-already-attempted
           (reason #((:start-once! issued) {}))))
    (cleanup! s issued)))

(deftest invalid-local-consumption-target-blocks-selected-server-start
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (assoc (original-source s)
                       :base-url! (fn [_] "http://example.com:80")))]
    (is (= :nonloopback-or-unsafe-probe-target
           (reason #((:start-once! issued) {}))))
    (is (.isShutdown (:executor @(:server-state s))))
    (is (= :start-failed-physical-state-unknown
           (:status ((:summary! issued)))))
    (cleanup! s issued)))

(deftest configured-start-is-once-only-and-stop-is-idempotent
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (try
      ((:start-once! issued) {})
      (is (= :configured-start-already-attempted
             (reason #((:start-once! issued) {}))))
      (is (= :stopped (:status ((:stop! issued)))))
      (is (= :stopped (:status ((:stop! issued)))))
      (finally (cleanup! s issued)))))

(deftest stop-before-start-refuses-to-resurrect-adapter
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (is (= :stopped-without-successful-start (:status ((:stop! issued)))))
    (is (= :configured-start-stopped-before-use
           (reason #((:start-once! issued) {}))))
    (is (nil? @(:server-state s)))
    (cleanup! s issued)))

(deftest substituted-sealed-start-boundary-rejects-before-server-initialization
  (let [s (scenario :honest)
        issued (config/issue-configured-checked-http-application!
                (original-source s))]
    (with-redefs [sealed/start-closed-checked-http-application!
                  (fn [& _] {:system {} :report {:physical-server-start
                                                  :one-selected-verified-adapter-start}})]
      (is (= :substituted-original-configured-start-boundary
             (reason #((:start-once! issued) {})))))
    (is (nil? @(:server-state s)))
    (cleanup! s issued)))

(deftest stopping-an-in-progress-start-cannot-claim-successful-cleanup
  (let [s (scenario :honest)
        entered (promise)
        continue (promise)
        source (assoc (original-source s)
                      :base-url! (fn [system]
                                   (deliver entered true)
                                   (when-not (deref continue 8000 false)
                                     (throw (ex-info "unreleased adapter" {})))
                                   ((:base-url! (original-source s)) system)))
        issued (config/issue-configured-checked-http-application! source)
        started (future
                  (try ((:start-once! issued) {})
                       (catch Throwable e e)))]
    (try
      (is (= true (deref entered 10000 false)))
      (is (= :configured-start-in-progress (reason (:stop! issued))))
      (is (= :starting (:status ((:summary! issued)))))
      (deliver continue true)
      (let [result (deref started 10000 :timeout)]
        (is (map? result))
        (is (= :verified-configured-start (get-in result [:report :status])))
        (is (= :stopped (:status ((:stop! issued))))))
      (finally
        (deliver continue true)
        (future-cancel started)
        (cleanup! s issued)))))
