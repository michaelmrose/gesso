(ns gesso.live.outbox.http-operation-test
  (:require [clojure.edn :as edn]
            [clojure.xml :as xml]
            [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.optimistic.protocol :as protocol]
            [ring.util.codec :as codec]
            [xtdb.api :as xt])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- with-assembly [execute! f]
  (let [ops (assoc (fixture/standard-operations) :request/claim
                   (fixture/trusted-operation :request/claim
                    {:published-change-topics #{:request} :execute! execute!}))]
    (fixture/with-surfaced-application
     {:operations ops :route-capabilities (fixture/route-capabilities ops)
      :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                        (fixture/render-context ops) :request/claim
                                        "/operations/request/claim")]}}
     (fn [{:keys [assembly]}] (f assembly)))))

(defn- command [operation]
  (protocol/command {:command-id (identity/command-id "v2362-command")
                     :execution-id (identity/execution-id "v2362-execution")
                     :operation operation :arguments {:request-id "r1"}
                     :observed-basis fixture/rendered-basis}))

(defn- form [operation token]
  (codec/form-encode {transport/command-parameter (pr-str (protocol/command->wire (command operation)))
                      "csrf" token}))

(defn- request [body]
  {:request-method :post :uri "/operations/request/claim"
   :headers {"content-type" "application/x-www-form-urlencoded"} :body body})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest assembled-transport-rejects-bypasses-before-authority
  (let [calls (atom []) gate (atom []) fallback (atom 0)
        boom (ex-info "unknown executor result" {})]
    (with-assembly (fn [ctx] (swap! calls conj ctx) (throw boom))
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly
                                             (fn [_] (swap! fallback inc) {:status 200})
                                             {:authorize-request? (fn [r]
                                                                    (swap! gate conj r)
                                                                    (= "valid" (get-in r [:form-params "csrf"])))})
              valid (form :request/claim "valid")]
          (is (= :invalid-command-options (reason #(transport/wrap-handler assembly identity {}))))
          (doseq [r [(request "") (request (form :request/cancel "valid"))
                     (request (codec/form-encode {transport/command-parameter
                                                  (pr-str (dissoc (protocol/command->wire (command :request/claim)) :observed-basis))}))
                     (request (str valid "&" transport/command-parameter "=%ZZ"))
                     (request (str valid "&csrf=valid"))
                     (request (codec/form-encode {transport/command-parameter
                                                  (str (pr-str (protocol/command->wire (command :request/claim))) " nil")}))
                     (request (apply str (repeat 65537 "x")))
                     (assoc (request valid) :query-string "csrf=valid")
                     (assoc (request "") :params {transport/command-parameter valid})]]
            (is (= 400 (:status (handler r)))))
          (is (= 405 (:status (handler (assoc (request valid) :request-method :get)))))
          (is (empty? @gate))
          (is (= 403 (:status (handler (request (form :request/claim "wrong"))))))
          (is (empty? @calls))
          (is (zero? @fallback))
          (is (identical? boom (try (handler (request valid)) nil (catch Exception e e))))
          (is (= 1 (count @calls)))
          (is (= fixture/trusted-principal (:principal (first @calls))))
          (is (= 200 (:status (handler {:uri "/other"}))))
          (is (= 1 @fallback)))))))

(deftest assembled-operation-over-owned-http-commits-and-returns-correlated-settlement
  (let [intent-id (random-uuid) calls (atom [])
        result* (atom nil)]
    (with-assembly
      (fn [{:keys [ctx arguments principal] :as invocation}]
        (swap! calls conj invocation)
        (let [node (:xtdb/connectable ctx)
              id (:request-id arguments)
              plan (outbox/prepare! {:intent-ids [intent-id]
                                     :source-id (get ctx managed/source-id-key)
                                     :operation :request/claim :purpose :gesso.live/refresh}
                                    [{:topic :request :id id :change/kind :updated}])
              tx (xt/execute-tx node (into (vec (:guard-ops plan))
                                           [[:put-docs :requests {:xt/id id :status :claimed}]
                                            (:tx-op plan)]))
              result {:resolution :confirmed :outcome :request/claimed
                      :authoritative (protocol/authoritative
                                      {:presence :present
                                       :basis {:tx-id (:tx-id tx) :system-time (str (:system-time tx))}
                                       :projection {:request/id id :request/status :claimed}})}]
          (reset! result* result)
          result))
      (fn [assembly]
        (let [started (host/start! assembly (fn [_] (g/html-response [:div "read"]))
                                   {:xtdb-config {} :source-id "v2362-operation-host"
                                    :rules-context {} :worker-options {:worker-id "v2362" :poll-ms 20}
                                    :live-options {:rules [{:when-topic :request
                                                           :expand (fn [_ change] [(select-keys change [:topic :id])])}]}
                                    :http-jdk {} :assembled-operations
                                    {:authorize-request? #(= "valid" (get-in % [:form-params "csrf"]))}})
              listener (get started host/listener-key)
              node (:xtdb/connectable started)
              client (HttpClient/newHttpClient)
              send! (fn [body]
                      (.send client
                             (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/operations/request/claim")))
                                 (.timeout (Duration/ofSeconds 10))
                                 (.header "content-type" "application/x-www-form-urlencoded")
                                 (.POST (HttpRequest$BodyPublishers/ofString body)) .build)
                             (HttpResponse$BodyHandlers/ofString)))]
          (try
            (is (= 403 (.statusCode (send! (form :request/claim "wrong")))))
            (is (= 400 (.statusCode (send! (form :request/reassign "valid")))))
            (is (empty? @calls))
            (is (empty? (xt/q node ["SELECT _id FROM requests"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
            (let [response (send! (form :request/claim "valid"))
                  settlement (protocol/settlement (merge @result* (select-keys (command :request/claim)
                                                                            [:command-id :execution-id])))]
              (is (= 200 (.statusCode response)))
              (is (= "none" (.orElse (.firstValue (.headers response) "HX-Reswap") "")))
              (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))]
                (is (= :template (:tag marker)))
                (is (= settlement (protocol/wire->settlement
                                   (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))))))
            (is (= [{:status :claimed}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"])))
            (is (= [{:xt/id intent-id}] (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id])))
            (is (= 1 (count @calls)))
            (is (identical? node (get-in (first @calls) [:ctx :xtdb/connectable])))
            (is (= fixture/trusted-principal (:principal (first @calls))))
            (finally (host/stop! started) (.close client))))))))
