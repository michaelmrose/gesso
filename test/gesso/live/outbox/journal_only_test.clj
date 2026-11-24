(ns gesso.live.outbox.journal-only-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [clojure.xml :as xml]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.outbox.model-publication :as publication]
            [ring.util.codec :as codec]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.io ByteArrayInputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]))

(def ^:private mapping
  {:subject-keys {:request :request/location-id
                  :request-assignment :request/location-id
                  :audit :request/location-id}})

(defn- with-assembly [executor f]
  (let [ops (assoc (fixture/standard-operations) :request/claim
                   (fixture/trusted-operation :request/claim
                    {:published-change-topics #{} :execute! executor}))]
    (fixture/with-surfaced-application
     {:operations ops :route-capabilities (fixture/route-capabilities ops)
      :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                        (fixture/render-context ops) :request/claim
                                        "/operations/request/claim")]}}
     (fn [{:keys [assembly report]}]
       (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
       (f assembly)))))

(defn- command [execution]
  (protocol/command {:command-id (identity/command-id "journal-2378")
                     :execution-id (identity/execution-id execution)
                     :operation :request/claim :arguments {:request-id "r1"}
                     :observed-basis fixture/rendered-basis}))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(def ^:private outcome {:reason :not-available})
(def ^:private now "2026-11-24T00:00:00Z")
(def ^:private no-request ["ASSERT 0 = (SELECT COUNT(*) FROM requests WHERE _id = ?)" "r1"])

(deftest journal-only-rejection-is-durable-without-domain-or-publication-writes
  (let [constructed (atom 0)
        projected (atom [])
        executor (publication/build-executor
                  {:operation :request/claim
                   :submission-mode! (constantly :submit)
                   :may-disclose? (fn [binding record]
                                    (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
                   :construct! (fn [_]
                                 (swap! constructed inc)
                                 {:spec nil :changes [] :domain-ops [no-request]
                                  :outcome outcome :now now})
                   :project! (fn [_ result]
                               (swap! projected conj (:status result))
                               {:resolution :rejected :reason (get-in result [:outcome :reason])})})]
    (with-assembly executor
      (fn [assembly]
        (let [started (host/start! assembly (fn [_] (g/html-response [:div "read"]))
                                   {:xtdb-config {} :source-id "v2378-journal"
                                    :rules-context {} :worker-options {:worker-id "v2378" :poll-ms 20}
                                    :live-options {} :assembled-publication mapping
                                    :http-jdk {} :assembled-operations
                                    {:authorize-request? (constantly true)}})
              listener (get started host/listener-key)
              node (:xtdb/connectable started)
              client (HttpClient/newHttpClient)
              post! (fn [cmd]
                      (.send client
                             (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/operations/request/claim")))
                                 (.timeout (Duration/ofSeconds 10))
                                 (.header "content-type" "application/x-www-form-urlencoded")
                                 (.POST (HttpRequest$BodyPublishers/ofString
                                         (codec/form-encode {transport/command-parameter
                                                             (pr-str (protocol/command->wire cmd))})))
                                 .build)
                             (HttpResponse$BodyHandlers/ofString)))]
          (try
            (doseq [execution ["first" "replacement"]]
              ;; State changes after the original rejection. Replay must not
              ;; re-evaluate or turn that prior decision into a fresh mutation.
              (when (= execution "replacement")
                (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :status :open}]]))
              (let [cmd (command execution)
                    response (post! cmd)
                    marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                    settlement (protocol/wire->settlement
                                (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                (is (= 200 (.statusCode response)))
                (is (= :rejected (:resolution settlement)))
                (is (= "not-available" (:reason settlement)))
                (is (= (select-keys cmd [:command-id :execution-id])
                       (select-keys settlement [:command-id :execution-id])))
                (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
              (when (= execution "first")
                (is (empty? (xt/q node ["SELECT _id FROM requests"])))))
            (is (= 1 @constructed))
            (is (= [:incorporated :replay] @projected))
            (is (= [{:xt/id "journal-2378"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
            (is (= [{:status :open}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"])))
            (finally (host/stop! started) (.close client))))))))

(deftest journal-only-outcome-rejects-writes-and-failed-assertions
  (with-assembly identity
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly mapping))
              ctx {:xtdb/connectable node publication/preparation-key
                   (bind "source" (assoc (command "first") :principal fixture/trusted-principal))}
              run! #(publication/transact-resolved! ctx %1 %2 %3 outcome now)
              inspect #(publication/reconcile-command! ctx (constantly true))]
          (doseq [ops [[[:put-docs :requests {:xt/id "r1"}]]
                       [["DELETE FROM requests"]]
                       [["ASSERT 1 = 1; DELETE FROM requests"]]
                       nil]]
            (is (= :invalid-domain-ops (reason #(run! nil [] ops)))))
          (is (= :journal-only-requires-no-changes
                 (reason #(run! nil [{:topic :request :request/location-id "r1"}] []))))
          (is (= :missing-model-changes (reason #(run! {:purpose :gesso.live/refresh} [] [no-request]))))
          (is (= :xtdb/assert-failed
                 (try (run! nil [] [["ASSERT 1 = 0"]]) nil
                      (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= :unresolved (:status (inspect))))
          (let [receipt (run! nil [] [no-request])]
            (is (empty? (:outbox/intents receipt)))
            (is (some? (:progression receipt))))
          (is (= {:status :replay :outcome outcome} (select-keys (inspect) [:status :outcome])))
          (is (= :not-observed (:outcome (publication/observe-command! ctx))))
          (is (= :xtdb/assert-failed
                 (try (run! nil [] []) nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (empty? (xt/q node ["SELECT _id FROM requests"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))))))
