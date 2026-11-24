(ns gesso.live.outbox.command-race-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [gesso.live.consistency.xtdb :as xtdb]
            [clojure.xml :as xml]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.outbox.model-publication :as publication]
            [ring.util.codec :as codec]
            [xtdb.api :as xt])
  (:import [java.io ByteArrayInputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private mapping
  {:subject-keys {:request :request/location-id
                  :request-assignment :request/location-id
                  :audit :request/location-id}})

(defn- with-assembly [executor f]
  (let [ops (assoc (fixture/standard-operations) :request/claim
                   (fixture/trusted-operation :request/claim
                    {:published-change-topics #{:request} :execute! executor}))]
    (fixture/with-surfaced-application
     {:operations ops :route-capabilities (fixture/route-capabilities ops)
      :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                        (fixture/render-context ops) :request/claim
                                        "/operations/request/claim")]}}
     (fn [{:keys [assembly report]}]
       (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
       (f assembly)))))

(defn- command [execution]
  (protocol/command {:command-id (identity/command-id "race-2379")
                     :execution-id (identity/execution-id execution)
                     :operation :request/claim :arguments {:request-id "r1"}
                     :observed-basis fixture/rendered-basis}))

(defn- send-command! [client started execution]
  (let [listener (get started host/listener-key)
        cmd (command execution)]
    (.sendAsync client
                (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/operations/request/claim")))
                    (.timeout (Duration/ofSeconds 30))
                    (.header "content-type" "application/x-www-form-urlencoded")
                    (.POST (HttpRequest$BodyPublishers/ofString
                            (codec/form-encode {transport/command-parameter
                                                (pr-str (protocol/command->wire cmd))})))
                    .build)
                (HttpResponse$BodyHandlers/ofString))))

(defn- settlement! [pending]
  (let [response (.get pending 35 TimeUnit/SECONDS)]
    (is (= 200 (.statusCode response)))
    (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))]
      (protocol/wire->settlement
       (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement]))))))

(deftest concurrent-physical-command-and-persistent-restart-recover-one-outcome
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2379-command-race-" (make-array java.nio.file.attribute.FileAttribute 0))
        both-constructing (CountDownLatch. 2)
        constructions (atom [])
        projections (atom [])
        executor (publication/build-executor
                  {:operation :request/claim
                   :submission-mode! (constantly :submit)
                   :may-disclose? (fn [binding record]
                                    (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
                   :construct! (fn [{:keys [execution-id]}]
                                 (let [attempt (identity/raw-value execution-id)]
                                   (swap! constructions conj attempt)
                                   ;; Both requests have already observed absence. Force
                                   ;; the identity guards, not request timing, to decide.
                                   (.countDown both-constructing)
                                   (when-not (.await both-constructing 15 TimeUnit/SECONDS)
                                     (throw (ex-info "Concurrent construction barrier timed out" {})))
                                   {:spec {:purpose :gesso.live/refresh}
                                    :changes [{:topic :request :request/location-id "r1"}]
                                    :domain-ops [[:put-docs :requests {:xt/id "r1" :status :claimed :winner attempt}]]
                                    :outcome {:outcome :request/claimed :winner attempt}
                                    :now "2026-11-24T00:00:00Z"}))
                   :project! (fn [{:keys [ctx execution-id]} result]
                               (let [basis (xtdb/latest-completed-basis ctx)
                                     row (first (xtdb/q-from ctx ["SELECT status, winner FROM requests WHERE _id = ?" "r1"]
                                                              {:snapshot-token (xtdb/basis-snapshot-token basis)}))]
                                 (swap! projections conj {:execution (identity/raw-value execution-id)
                                                         :status (:status result) :outcome (:outcome result)})
                                 {:resolution (if (= :incorporated (:status result)) :confirmed :already-incorporated)
                                  :outcome (get-in result [:outcome :outcome])
                                  :authoritative (protocol/authoritative
                                                  {:presence :present :basis basis
                                                   :projection {:request/id "r1" :request/status (:status row)}})}))})
        config {:xtdb-config {:log [:local {:path (str root "/log")}]
                              :storage [:local {:path (str root "/storage")}]}
                :source-id "v2379-race" :rules-context {}
                :worker-options {:worker-id "v2379" :poll-ms 20}
                :live-options {} :assembled-publication mapping
                :http-jdk {} :assembled-operations {:authorize-request? (constantly true)}}
        handler (fn [_] (g/html-response [:div "ready"]))]
    (try
      (with-assembly executor
        (fn [assembly]
          (with-open [client (HttpClient/newHttpClient)]
            (let [started (host/start! assembly handler config)
                  winner (try
                           (let [a (send-command! client started "first")
                                 b (send-command! client started "second")
                                 results [(settlement! a) (settlement! b)]
                                 node (:xtdb/connectable started)
                                 row (first (xt/q node ["SELECT winner FROM requests WHERE _id = ?" "r1"]))
                                 winner (:winner row)]
                             (is (= #{"first" "second"} (set @constructions)))
                             (is (= 2 (count @constructions)))
                             (is (= #{:confirmed :already-incorporated} (set (map :resolution results))))
                             (is (= (mapv #(select-keys (command %) [:command-id :execution-id]) ["first" "second"])
                                    (mapv #(select-keys % [:command-id :execution-id]) results)))
                             (is (= #{:incorporated :replay} (set (map :status @projections))))
                             (is (= #{{:outcome :request/claimed :winner winner}} (set (map :outcome @projections))))
                             (is (= winner (:execution (first (filter #(= :incorporated (:status %)) @projections)))))
                             (is (= [{:xt/id "race-2379"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
                             (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                             winner)
                           (finally (host/stop! started)))]
              ;; Reopen actual local log and storage, not the in-memory node.
              (let [reopened (host/start! assembly handler config)]
                (try
                  (let [result (settlement! (send-command! client reopened "restart"))
                        node (:xtdb/connectable reopened)]
                    (is (= :already-incorporated (:resolution result)))
                    (is (= (identity/execution-id "restart") (:execution-id result)))
                    (is (= 2 (count @constructions)))
                    (is (= {:execution "restart" :status :replay
                            :outcome {:outcome :request/claimed :winner winner}} (peek @projections)))
                    (is (= [{:winner winner}] (xt/q node ["SELECT winner FROM requests WHERE _id = ?" "r1"])))
                    (is (= [{:xt/id "race-2379"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
                    (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))
                  (finally (host/stop! reopened))))))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))]
          (io/delete-file entry true))))))
