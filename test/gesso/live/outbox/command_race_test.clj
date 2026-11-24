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
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.command :as model-command]
            [xtdb.node :as xtn]
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

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})

(defn- with-operations [ops f]
  (fixture/with-surfaced-application
   {:operations ops :route-capabilities (fixture/route-capabilities ops)
    :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                      (fixture/render-context ops) :request/claim
                                      "/operations/request/claim")]}}
   (fn [{:keys [assembly report]}]
     (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
     (f assembly))))

(defn- claim-operation [executor]
  (fixture/trusted-operation :request/claim
                             {:published-change-topics #{:request} :execute! executor}))

(defn- with-assembly [executor f]
  (with-operations (assoc (fixture/standard-operations) :request/claim (claim-operation executor)) f))

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
                                    :model-plan {:commands [(model-command/create
                                                            :requests {:xt/id "r1" :status :claimed :winner attempt
                                                                       :revision 0 :created-at "t0" :updated-at "t0"
                                                                       :request/location-id "r1"}
                                                            version)]}
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
                :live-options {} :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request}}
                                                        :require-derived-model? true)
                :http-jdk {} :assembled-operations {:authorize-request? (constantly true)}}
        handler (fn [_] (g/html-response [:div "ready"]))]
    (try
      (with-operations {:request/claim (claim-operation executor)}
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

(deftest canonical-model-publication-preserves-version-guards-and-plan-admission
  (with-assembly identity
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly mapping))
              ctx (fn [id]
                    {:xtdb/connectable node
                     publication/preparation-key
                     (bind "model-2380" (assoc (command "attempt")
                                               :command-id (identity/command-id id)
                                               :principal fixture/trusted-principal))})
              spec {:purpose :gesso.live/refresh}
              before {:xt/id "r1" :revision 0 :created-at "t0" :updated-at "t0" :status :open}
              after (assoc before :revision 1 :updated-at "t1" :status :claimed)
              plan {:commands [(model-command/update-command :requests :claim before after version)]
                    :changes [{:topic :request :request/location-id "r1"}]}
              run! (fn [id p] (publication/transact-model-resolved!
                               (ctx id) spec p {:outcome :request/claimed} "t1"))
              error (fn [f] (try (f) nil (catch Exception e (ex-data e))))]
          (xt/execute-tx node [[:put-docs :requests before]])
          (doseq [p [(assoc plan :emit :sync) (assoc plan :entry-fn identity)
                     (assoc plan :tx-options {:database "other"})]]
            (is (= :invalid-durable-model-plan (:reason (error #(run! "bad" p))))))
          (doseq [table [:gesso_commands :public/gesso-live-outbox :gesso_publication_manifests]]
            (is (= :reserved-durable-model-table
                   (:reason (error #(run! "reserved" (assoc plan :commands
                                                            [(model-command/create table before version)])))))))
          (let [receipt (run! "winner" plan)]
            (is (= 1 (count (:outbox/intents receipt))))
            (is (some? (:progression receipt))))
          (is (= :xtdb/assert-failed (:xtdb.error/code (error #(run! "stale" plan)))))
          (is (= [{:revision 1 :status :claimed}] (xt/q node ["SELECT revision, status FROM requests"])))
          (is (= [{:xt/id "winner"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))))))

(deftest derived-model-publication-covers-both-subjects-and-checks-old-scope
  (with-assembly identity
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [selected (assoc mapping :model-entity-topics {:requests #{:request}})
              bind (:bind-command (publication/profile! assembly selected))
              context (fn [id]
                        {:xtdb/connectable node publication/preparation-key
                         (bind "derived-2382" (assoc (command "attempt")
                                                    :command-id (identity/command-id id)
                                                    :principal fixture/trusted-principal))})
              before {:xt/id "r1" :revision 0 :created-at "t0" :updated-at "t0"
                      :request/location-id :location/old}
              after (assoc before :revision 1 :updated-at "t1" :request/location-id :location/new)
              plan (fn [b a] {:commands [(model-command/update-command :requests :move b a version)]})
              run! (fn [id p] (publication/transact-model-resolved!
                               (context id) {:purpose :gesso.live/refresh} p {:outcome :moved} "t1"))
              error (fn [f] (try (f) nil (catch Exception e (ex-data e))))]
          (doseq [bad [nil {} {:requests #{}} {:requests [:request]} {:requests #{:unknown}}]]
            (is (= :invalid-model-entity-topics
                   (:reason (error #(publication/profile! assembly (assoc mapping :model-entity-topics bad)))))))
          (xt/execute-tx node [[:put-docs :requests before]])
          (is (= :explicit-changes-with-derived-model
                 (:reason (error #(run! "explicit" (assoc (plan before after) :changes [{:topic :request :request/location-id "wrong"}]))))))
          (is (= :unmapped-model-entity
                 (:reason (error #(run! "unmapped" {:commands [(model-command/create :other before version)]})))))
          (is (= :invalid-model-document-subject
                 (:reason (error #(run! "missing" (plan before (dissoc after :request/location-id)))))))
          (is (= :xtdb/assert-failed
                 (:xtdb.error/code (error #(run! "wrong-before" (plan (assoc before :request/location-id :location/wrong) after))))))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
          (let [receipt (run! "move" (plan before after))]
            (is (= [:location/old :location/new]
                   (mapv :gesso.live.outbox/subject-id (:outbox/intents receipt)))))
          (let [next-doc (assoc after :revision 2 :updated-at "t2")
                receipt (run! "same" (plan after next-doc))]
            (is (= [:location/new] (mapv :gesso.live.outbox/subject-id (:outbox/intents receipt)))))
          (is (= [{:revision 2}] (xt/q node ["SELECT revision FROM requests"])))
          (is (= 3 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))))))

(deftest strict-derived-model-refuses-raw-command-paths
  (with-assembly identity
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [selected (assoc mapping :model-entity-topics {:requests #{:request}}
                                      :require-derived-model? true)
              bind (:bind-command (publication/profile! assembly selected))
              invocation (fn [id]
                           (let [cmd (assoc (command "attempt") :command-id (identity/command-id id))]
                             (assoc (select-keys cmd [:operation :command-id :execution-id :arguments])
                                    :command cmd :principal fixture/trusted-principal
                                    :ctx {:xtdb/connectable node publication/preparation-key
                                          (bind "strict-2383" (assoc cmd :principal fixture/trusted-principal))})))
              call (invocation "strict-command")
              ctx (:ctx call)
              spec {:purpose :gesso.live/refresh}
              changes [{:topic :request :request/location-id "wrong"}]
              doc {:xt/id "r1" :revision 0 :created-at "t0" :updated-at "t0"
                   :request/location-id "actual"}
              ops [[:put-docs :requests doc]]
              outcome {:outcome :request/claimed}
              reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
              options {:mode :submit :may-disclose? (constantly true)
                       :construct! (constantly {:spec spec :changes changes :domain-ops ops
                                                :outcome outcome :now "t0"})
                       :project! (fn [& _] (throw (ex-info "unexpected projection" {})))}]
          (is (= :invalid-subject-mapping
                 (reason #(publication/profile! assembly (assoc mapping :require-derived-model? true)))))
          (is (= :invalid-derived-model-requirement
                 (reason #(publication/profile! assembly (assoc selected :require-derived-model? false)))))
          (doseq [f [#(publication/prepare! ctx spec changes)
                     #((get ctx publication/preparation-key) spec changes)
                     #(publication/transact! ctx spec changes ops)
                     #(publication/transact-resolved! ctx spec changes ops outcome "t0")
                     #(publication/transact-resolved! ctx nil [] [] outcome "t0")
                     #(publication/execute-resolved! call options)]]
            (is (= :canonical-model-plan-required (reason f))))
          (is (empty? (xt/q node ["SELECT _id FROM requests"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
          (let [receipt (publication/transact-model-resolved!
                         ctx spec {:commands [(model-command/create :requests doc version)]} outcome "t0")]
            (is (= ["actual"] (mapv :gesso.live.outbox/subject-id (:outbox/intents receipt)))))
          (let [receipt (publication/transact-model-resolved!
                         (:ctx (invocation "strict-noop")) nil {} {:reason :unavailable} "t0")]
            (is (empty? (:outbox/intents receipt))))
          (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))))))

(deftest strict-host-requires-complete-recognized-executor-inventory
  (let [calls (atom 0)
        build (fn [operation]
                (publication/build-executor
                 {:operation operation :submission-mode! (constantly :submit)
                  :may-disclose? (constantly false)
                  :construct! (fn [_] (throw (ex-info "unexpected construction" {})))
                  :project! (fn [& _] (throw (ex-info "unexpected projection" {})))}))
        claim (claim-operation (build :request/claim))
        cancel (fn [executor topics]
                 (fixture/trusted-operation :request/cancel
                                            {:published-change-topics topics :execute! executor}))
        selected {:xtdb-config {} :source-id "strict-2384" :rules-context {}
                  :worker-options {:worker-id "strict-2384" :poll-ms 20}
                  :live-options {} :assembled-publication
                  (assoc mapping :model-entity-topics {:requests #{:request}}
                                 :require-derived-model? true)
                  :assembled-operations {:authorize-request? (constantly true)}}
        error (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (doseq [[entry expected]
            [[(cancel identity #{}) :unrecognized-strict-publication-executor]
             [(cancel (build :request/cancel) #{:audit}) :unmapped-publication-executor-topics]
             [(cancel (build :request/claim) #{}) :publication-executor-operation-mismatch]]]
      (with-operations {:request/claim claim :request/cancel entry}
        (fn [assembly]
          (with-redefs [managed/start-biff-application! (fn [& _] (swap! calls inc))]
            (let [failure (error #(host/start! assembly identity selected))]
              (is (= expected (:reason failure)))
              (is (= :request/cancel (:operation failure))))))))
    (with-operations {:request/claim claim :request/cancel (cancel (build :request/cancel) #{})}
      (fn [assembly]
        (is (nil? (publication/require-host-wiring! assembly selected)))
        (with-redefs [managed/start-biff-application! (fn [& _] (swap! calls inc))]
          (is (= :missing-publication-executor-profile
                 (:reason (error #(host/start! assembly identity (dissoc selected :assembled-operations)))))))))
    (is (zero? @calls))))
