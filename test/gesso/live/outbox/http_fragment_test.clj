(ns gesso.live.outbox.http-fragment-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.core :as g]
            [gesso.live.acquisition-preflight :as acquisition]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.core :as live]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-fragment :as fragment]
            [gesso.live.progression.http :as progression-http])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- with-assembly
  ([f] (with-assembly "/fragments/request-list/:id" f))
  ([list-path f]
  ;; Preserve the original acquisition products while selecting one concrete
  ;; parameterized fragment route, in addition to the ordinary literal route.
  (let [realization-var (ns-resolve 'gesso.live.application-preflight.fixture 'realization)]
    (with-redefs-fn
      {realization-var (fn [app fragment]
                         (acquisition/require-acquisition-realization!
                          {:name (keyword "fixture" (str (name fragment) "-acquisition"))
                           :live-app app :fragment fragment
                           :fragment-route (acquisition/fragment-route
                                            {:fragment fragment
                                             :path (if (= :request-list fragment) list-path
                                                       (str "/fragments/" (name fragment)))})
                           :stream-route (acquisition/stream-route
                                          {:fragment fragment :path (str "/streams/" (name fragment))})}))}
      (fn []
        (let [ops (fixture/standard-operations)]
          (fixture/with-surfaced-application
           {:operations ops :route-capabilities (fixture/route-capabilities ops)
            :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                              (fixture/render-context ops) :request/claim
                                              "/operations/request/claim")]}}
           (fn [{:keys [assembly report]}]
             (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
             (f assembly)))))))))

(defn- config []
  {:xtdb-config {} :source-id "v2364-fragment-host"
   :rules-context {} :worker-options {:worker-id "v2364" :poll-ms 20}
   :live-options {:rules []}})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest canonical-fragment-admission-and-physical-progression-read
  (let [resolutions (atom []) authorizations (atom []) queries (atom []) fallback (atom 0)
        resolve-id (fn [ctx fragment]
                     (swap! resolutions conj [fragment ctx])
                     (or (get-in ctx [:path-params :id]) (get-in ctx [:headers "fragment-id"])))]
    (with-redefs [fixture/allow? (fn [ctx id]
                                   (swap! authorizations conj [id ctx])
                                   (and (= "allowed" (get-in ctx [:headers "authorization"]))
                                        (= "r1" id)))
                  fixture/generic-query (fn [ctx id]
                                          (swap! queries conj [id ctx])
                                          {:fragment/id (str "actual-" id)
                                           :status (:status (first (live/q ctx ["SELECT status FROM requests WHERE _id = ?" id])))})
                  fixture/generic-render (fn [{:keys [fragment/id status]}]
                                           [:section {:id id} (name (or status :open))])]
      (with-assembly
        (fn [assembly]
          (is (= :invalid-fragment-options (reason #(fragment/wrap-handler assembly identity {}))))
          (let [started (host/start! assembly
                                     (fn [_] (swap! fallback inc) (g/html-response [:div "fallback"]))
                                     (assoc (config) :http-jdk {}
                                            :assembled-fragments {:resolve-id resolve-id}
                                            :assembled-operations {:authorize-request? (constantly false)}))
                listener (get started host/listener-key)
                handler (:biff.ring/handler started)
                node (:xtdb/connectable started)
                client (HttpClient/newHttpClient)
                send! (fn [path headers]
                        (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))]
                          (.timeout builder (Duration/ofSeconds 10))
                          (doseq [[k v] headers] (.header builder k v))
                          (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))
                path "/fragments/request-list/r1"
                header progression-http/request-header-name]
            (try
              (is (= 405 (:status (handler {:request-method :post :uri path}))))
              (is (= 400 (:status (handler {:request-method :get :uri "/fragments/request-list/.."}))))
              (is (= 400 (.statusCode (send! path {header "%not-valid"}))))
              (is (empty? @resolutions))
              (is (= 400 (.statusCode (send! "/fragments/request-toolbar" {}))))
              (is (= 403 (.statusCode (send! path {}))))
              (is (= 403 (.statusCode (send! "/fragments/request-list/r2" {"authorization" "allowed"}))))
              (is (empty? @queries))
              (is (zero? @fallback))
              (let [receipt (xtdb/execute-tx! node [[:put-docs :requests {:xt/id "r1" :status :claimed}]])
                    requirement (:progression receipt)
                    headers {"authorization" "allowed"
                             header (progression-http/encode-request-progression requirement)}
                    response (send! path headers)]
                (is (some? requirement))
                (is (= 200 (.statusCode response)))
                (is (str/includes? (.body response) "<section id=\"actual-r1\">claimed</section>"))
                (is (= "no-store" (.orElse (.firstValue (.headers response) "cache-control") "")))
                (is (= requirement (live/progression (second (peek @authorizations)))))
                (is (= requirement (live/progression (second (peek @queries)))))
                (is (identical? node (xtdb/read-connectable-from (second (peek @queries)))))
                (is (= 200 (:status (handler {:request-method :get :uri path :headers headers
                                              :xtdb/read-connectable :forged
                                              :path-params {:id "forged"} :params {:id "forged"}}))))
                (is (= {:id "r1"} (:path-params (second (peek @resolutions)))))
                (is (not (contains? (second (peek @resolutions)) :params)))
                (is (identical? node (xtdb/read-connectable-from (second (peek @queries)))))
                (is (= 200 (.statusCode (send! "/fragments/request-toolbar" (assoc headers "fragment-id" "r1")))))
                (is (= :request-toolbar (first (peek @resolutions)))))
              (is (= 200 (.statusCode (send! "/other" {}))))
              (is (= 1 @fallback))
              (finally (host/stop! started) (.close client)))))))))

(deftest canonical-fragment-route-rejects-overlapping-or-unsupported-templates
  (doseq [[path expected] [["/fragments/:id" :ambiguous-fragment-route]
                           ["/streams/:id" :ambiguous-fragment-route]
                           ["/operations/request/:id" :ambiguous-fragment-route]
                           ["/fragments/:id/:id" :unsupported-fragment-route]]]
    (with-assembly path
      (fn [assembly]
        (is (= expected (reason #(fragment/wrap-handler assembly identity
                                                       {:resolve-id (constantly "r1")}))))))))
