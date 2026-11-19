(ns gesso.assurance.proof.checked-biff-http-application-configuration-fixture
  "Test-only source of shared real-XTDB and loopback HTTP setup for v1814's
   configured Biff startup suites; not an application/runtime facility."
  (:require [gesso.assurance.proof.checked-biff-http-adapter-start-test]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test
             :as previous]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.model.resolved-operation-test])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(def ^:private scenario-ns
  'gesso.assurance.proof.checked-biff-http-adapter-start-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn helper [ns-name s]
  (var-get (or (ns-resolve ns-name s)
               (throw (ex-info "Missing original test fixture"
                               {:ns ns-name :name s})))))
(defn model [s] (helper model-ns s))
(defn scenario [mode] ((helper scenario-ns 'scenario) mode false))
(defn with-real-application-fixture [run]
    ((model 'with-real-xtdb)
     (fn []
       (app-fixture/with-closed-application
        {:operations {:request/claim
                      (app-fixture/trusted-operation
                       :request/claim {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [previous/*assembly* assembly] (run)))))))

(defn original-source [s & [typed?]]
  (cond-> {:contract-source (get-in s [:env :contract])
           :adapter-module (:raw s)
           :base-url! (fn [_] (str "http://127.0.0.1:"
                                   (:port @(:server-state s))))}
    typed?
    (assoc :typed-trial
           {:catalog-source (get-in s [:env :catalog-source])
            :operation :request/claim
            :inputs {:input/owner "helper-a"}})))

(defn reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn post! [s]
  (let [port (:port @(:server-state s))
        req (-> (HttpRequest/newBuilder
                 (URI/create (str "http://127.0.0.1:" port
                                  "/operations/request/claim")))
                (.timeout (Duration/ofSeconds 6))
                (.POST (HttpRequest$BodyPublishers/noBody))
                (.build))]
    (.statusCode (.send (HttpClient/newHttpClient) req
                        (HttpResponse$BodyHandlers/ofString)))))
(defn cleanup! [s issued]
  ((:stop! issued))
  ;; The raw adapter's stop is also idempotent in this retained test fixture.
  (when-let [stop (get-in s [:raw :biff.core/stop])]
    (stop {})))

