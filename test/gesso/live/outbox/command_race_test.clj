(ns gesso.live.outbox.command-race-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [gesso.live.source :as live-source]
            [gesso.live.core :as live]
            [gesso.live.progression :as progression]
            [gesso.live.progression.http :as progression-http]
            [clojure.test :as t :refer [deftest is]]
            [clojure.java.io :as io]
            [gesso.live.consistency.xtdb :as xtdb]
            [clojure.xml :as xml]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.claim-browser-fixture :as browser]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.model.command :as model-command]
            [xtdb.node :as xtn]
            [ring.util.codec :as codec]
            [xtdb.api :as xt])
  (:import [java.io ByteArrayInputStream BufferedReader InputStreamReader]
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
                               (let [{:keys [basis rows]} (frontier/q-with-basis!
                                                           ctx ["SELECT status, winner FROM requests WHERE _id = ?" "r1"])
                                     row (first rows)]
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
                                                        :require-derived-model? true
                                                        :operation-model-entities {:request/claim #{:requests}})
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

(deftest strict-operation-entity-inventory-distinguishes-shared-topics
  (let [build (fn [operation]
                (publication/build-executor
                 {:operation operation :submission-mode! (constantly :submit)
                  :may-disclose? (constantly true)
                  :construct! (fn [_] (throw (ex-info "unexpected construction" {})))
                  :project! (fn [& _] (throw (ex-info "unexpected projection" {})))}))
        ops {:request/claim (claim-operation (build :request/claim))
             :request/cancel (fixture/trusted-operation :request/cancel
                              {:published-change-topics #{} :execute! (build :request/cancel)})}
        selected (assoc mapping :model-entity-topics {:requests #{:request}
                                                      :other-requests #{:request}
                                                      :audits #{:audit}}
                                :require-derived-model? true
                                :operation-model-entities {:request/claim #{:requests}
                                                           :request/cancel #{}})
        config {:xtdb-config {} :source-id "strict-2385" :rules-context {}
                :worker-options {:worker-id "strict-2385" :poll-ms 20}
                :live-options {} :assembled-publication selected
                :assembled-operations {:authorize-request? (constantly true)}}
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (with-operations ops
      (fn [assembly]
        (let [starts (atom 0)]
          (with-redefs [managed/start-biff-application! (fn [& _] (swap! starts inc))]
            (doseq [[inventory expected]
                    [[nil :incomplete-operation-model-entities]
                     [{:request/claim #{:requests}} :incomplete-operation-model-entities]
                     [(assoc (:operation-model-entities selected) :request/unknown #{}) :incomplete-operation-model-entities]
                     [{:request/claim [:requests] :request/cancel #{}} :invalid-operation-model-entities]
                     [{:request/claim #{:unknown} :request/cancel #{}} :invalid-operation-model-entities]
                     [{:request/claim #{:audits} :request/cancel #{}} :operation-model-topic-mismatch]
                     [{:request/claim #{:requests} :request/cancel #{:other-requests}} :operation-model-topic-mismatch]]]
              (is (= expected (reason #(host/start! assembly identity
                                       (assoc-in config [:assembled-publication :operation-model-entities] inventory))))))
            (is (zero? @starts))))
        (with-open [node (xtn/start-node {})]
          (let [bind (:bind-command (publication/profile! assembly selected))
                ctx (fn [operation id]
                      {:xtdb/connectable node publication/preparation-key
                       (bind "strict-2385" (assoc (command "attempt") :operation operation
                                             :command-id (identity/command-id id)
                                             :principal fixture/trusted-principal))})
                doc {:xt/id "r1" :revision 0 :created-at "t0" :updated-at "t0"
                     :request/location-id "actual"}
                run! (fn [operation id entity]
                       (publication/transact-model-resolved! (ctx operation id)
                         {:purpose :gesso.live/refresh}
                         {:commands [(model-command/create entity doc version)]}
                         {:outcome :request/claimed} "t0"))]
            (is (= :unmapped-model-entity (reason #(run! :request/claim "wrong" :other-requests))))
            (is (= :unmapped-model-entity (reason #(run! :request/cancel "wrong-cancel" :requests))))
            (is (empty? (xt/q node ["SELECT _id FROM requests"])))
            (is (empty? (xt/q node ["SELECT _id FROM other_requests"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
            (is (= ["actual"] (mapv :gesso.live.outbox/subject-id
                                    (:outbox/intents (run! :request/claim "right" :requests)))))
            (is (empty? (:outbox/intents (publication/transact-model-resolved!
                                         (ctx :request/cancel "noop") nil {} {:reason :unavailable} "t0"))))
            (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
            (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))))))))

(deftest derived-publication-inventory-rejects-physical-table-aliases
  (let [executor (publication/build-executor
                  {:operation :request/claim :submission-mode! (constantly :submit)
                   :may-disclose? (constantly true)
                   :construct! (fn [_] (throw (ex-info "unexpected construction" {})))
                   :project! (fn [& _] (throw (ex-info "unexpected projection" {})))})
        selected (assoc mapping :model-entity-topics {:request-items #{:request}}
                                :require-derived-model? true
                                :operation-model-entities {:request/claim #{:request-items}})
        config {:xtdb-config {} :source-id "strict-2386" :rules-context {}
                :worker-options {:worker-id "strict-2386" :poll-ms 20}
                :live-options {} :assembled-publication selected
                :assembled-operations {:authorize-request? (constantly true)}}
        error (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    ;; Establish the actual provider alias rather than relying only on
    ;; a string normalization assumption in the admission checker.
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :request-items {:xt/id "r1" :status :open}]])
      (xt/execute-tx node [[:put-docs :public/request_items {:xt/id "r1" :status :claimed}]])
      (is (= [{:xt/id "r1" :status :claimed}]
             (xt/q node ["SELECT _id, status FROM request_items"])))
      (xt/execute-tx node [[:put-docs :other/request_items {:xt/id "r1" :status :other}]])
      (is (= [{:status :other}] (xt/q node ["SELECT status FROM other.request_items"])))
      (is (= [{:status :claimed}] (xt/q node ["SELECT status FROM request_items"]))))
    (with-operations {:request/claim (claim-operation executor)}
      (fn [assembly]
        (let [starts (atom 0)]
          (with-redefs [managed/start-biff-application! (fn [& _] (swap! starts inc))]
            (doseq [alias [:request_items :public/request-items :public/request_items]]
              (let [failure (error #(host/start! assembly identity
                                       (assoc-in config [:assembled-publication :model-entity-topics alias]
                                                 #{:request})))]
                (is (= :aliased-model-entities (:reason failure)))
                (is (= #{:request-items alias} (:entities failure)))
                (is (= ["public" "request_items"] (:physical-table failure)))))
            (doseq [entity [:RequestItems :public/request.items :xt/requests :pg_catalog/requests]]
              (is (= :unsupported-model-entity-name
                     (:reason (error #(host/start! assembly identity
                                      (assoc-in config [:assembled-publication :model-entity-topics entity]
                                                #{:request})))))))
            (doseq [entity [:gesso-commands :public/gesso_live_outbox :gesso-publication-manifests]]
              (is (= :reserved-durable-model-table
                     (:reason (error #(host/start! assembly identity
                                      (assoc-in config [:assembled-publication :model-entity-topics entity]
                                                #{:request})))))))
            (is (zero? @starts))))
        (is (map? (publication/profile! assembly selected)))
        (is (map? (publication/profile! assembly
                    (assoc-in selected [:model-entity-topics :other/request_items] #{:request}))))))))

(deftest derived-model-rejects-ambiguous-physical-document-fields
  (with-assembly identity
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile!
                                  assembly (assoc mapping :model-entity-topics {:requests #{:request}}
                                                          :require-derived-model? true)))
              ctx (fn [id]
                    {:xtdb/connectable node publication/preparation-key
                     (bind "fields-2387" (assoc (command "attempt")
                                               :command-id (identity/command-id id)
                                               :principal fixture/trusted-principal))})
              before {:xt/id "r1" :revision 0 :created-at "t0" :updated-at "t0"
                      :request/location-id "old"}
              after (assoc before :revision 1 :updated-at "t1" :request/location-id "new")
              run! (fn [id cmd]
                     (publication/transact-model-resolved!
                      (ctx id) {:purpose :gesso.live/refresh} {:commands [cmd]}
                      {:outcome :moved} "t1"))
              error (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))]
          ;; A different Clojure key really writes the same physical column.
          (xt/execute-tx node [[:put-docs :requests (-> before
                                                      (dissoc :request/location-id)
                                                      (assoc :request/location_id "old"))]])
          (is (= [{:subject "old"}]
                 (xt/q node ["SELECT request$location_id AS subject FROM requests WHERE _id = ?" "r1"])))
          (doseq [[alias field physical value] [[:request/location_id :request/location-id :request$location_id "other"]
                                                [:request$location_id :request/location-id :request$location_id "other"]
                                                [:_id :xt/id :_id "other"]
                                                [:updated_at :updated-at :updated_at "other"]]
                  phase [:before :after]]
            (let [b (cond-> before (= phase :before) (assoc alias value))
                  a (cond-> after (= phase :after) (assoc alias value))
                  failure (error #(run! "alias" (model-command/update-command :requests :move b a version)))]
              (is (= :gesso.model.tx/aliased-document-fields (:error/type failure)))
              (is (= phase (get-in failure [:error/details :phase])))
              (is (= #{field alias} (get-in failure [:error/details :fields])))
              (is (= physical (get-in failure [:error/details :physical-field])))))
          (is (= :gesso.model.tx/unsupported-document-field
                 (:error/type (error #(run! "string-key" (model-command/create
                                                       :requests (assoc before "location" "wrong") version))))))
          (is (= :gesso.model.tx/aliased-document-fields
                 (:error/type (error #(run! "create-alias" (model-command/create
                                                         :requests (assoc before :_id "wrong") version))))))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
          (is (= [{:revision 0 :subject "old"}]
                 (xt/q node ["SELECT revision, request$location_id AS subject FROM requests"])))
          (let [receipt (run! "move" (model-command/update-command :requests :move before after version))]
            (is (= ["old" "new"] (mapv :gesso.live.outbox/subject-id (:outbox/intents receipt)))))
          (is (= [{:revision 1 :subject "new"}]
                 (xt/q node ["SELECT revision, request$location_id AS subject FROM requests"])))
          (is (= [{:xt/id "move"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
          (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))))))

(deftest installed-executor-enforces-independent-document-targets
  (let [mode (atom :wrong)
        trace (atom [])
        submissions (atom 0)
        member {:xt/id "m1" :revision 0 :created-at "t0" :updated-at "t0" :active true}
        tables {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                :required-write-tables #{:requests :gesso_commands}
                :allowed-write-tables #{:requests :gesso_commands}}
        doc (fn [id] {:xt/id id :status :claimed :revision 0
                      :created-at "t0" :updated-at "t0"
                      :request/location-id "r1"})
        opts {:operation :request/claim
              :submission-mode! (constantly :submit)
              :may-disclose? (constantly true)
              :construction-contract tables
              :target-contract! (fn [_ binding]
                                  (swap! trace conj :declare)
                                  (let [target [:requests (get-in binding [:arguments :request-id])]]
                                    {:required-guard-targets #{[:memberships "m1"]}
                                     :allowed-guard-targets #{[:memberships "m1"]}
                                     :required-write-targets (if (= :invalid @mode) #{} #{target})
                                     :allowed-write-targets #{target}}))
              :predicate-contract! (fn [_ _]
                                     (swap! trace conj :declare-predicate)
                                     {:required-guard-checks (if (= :invalid-predicate @mode) #{} #{[:memberships "m1" :active true]})
                                      :allowed-guard-checks #{[:memberships "m1" :active true]
                                                              [:memberships "m1" :revision 0]
                                                              [:memberships "m1" :updated-at "t0"]}
                                      :required-update-checks #{} :allowed-update-checks #{}})
              :command-operation-contract! (fn [_ binding]
                                             (swap! trace conj :declare-operation)
                                             (let [entry [:requests (get-in binding [:arguments :request-id])
                                                          (if (= :wrong-operation @mode) :claim :create)]]
                                               {:required-command-operations (if (= :invalid-operation @mode) #{} #{entry})
                                                :allowed-command-operations #{entry}}))
              :post-state-contract! (fn [_ binding]
                                      (swap! trace conj :declare-post-state)
                                      (let [id (get-in binding [:arguments :request-id])]
                                        {:required-post-state-values (if (= :invalid-post-state @mode) #{} #{[:requests id :status :claimed]})
                                         :allowed-post-state-values #{[:requests id :xt/id id]
                                                                      [:requests id :status :claimed]
                                                                      [:requests id :revision 0]
                                                                      [:requests id :created-at "t0"]
                                                                      [:requests id :updated-at "t0"]
                                                                      [:requests id :request/location-id "r1"]}}))
              :outcome-contract! (fn [_ _]
                                   (swap! trace conj :declare-outcome)
                                   {:expected-outcome (if (= :invalid-outcome @mode)
                                                        {} {:outcome :request/claimed})})
              :construct! (fn [_]
                            (swap! trace conj :construct)
                            {:spec {:purpose :gesso.live/refresh}
                             :model-plan {:guards [(model-command/guard
                                                    :memberships (cond-> member (= :wrong-predicate @mode) (assoc :active false))
                                                    (if (= :missing-predicate @mode) version
                                                        (assoc version :compare-keys [:revision :updated-at :active])))]
                                          :commands (mapv #(model-command/create :requests
                                                                                 (cond-> (doc %)
                                                                                   (= :wrong-post-state @mode) (assoc :status :open)
                                                                                   (= :extra-post-state @mode) (assoc :admin true)
                                                                                   (= :missing-post-state @mode) (dissoc :status))
                                                                                 version)
                                                          (case @mode
                                                            :wrong ["r2"]
                                                            :extra ["r1" "r2"]
                                                            ["r1"]))}
                             :outcome (case @mode
                                        :wrong-outcome {:outcome :request/cancelled}
                                        :extra-outcome {:outcome :request/claimed :extra true}
                                        {:outcome :request/claimed})
                             :now "t0"})
              :project! (fn [{:keys [ctx]} result]
                          (swap! trace conj :project)
                          (let [{:keys [basis rows]} (frontier/q-with-basis!
                                                    ctx ["SELECT status FROM requests WHERE _id = ?" "r1"])]
                            {:resolution (if (= :incorporated (:status result)) :confirmed :already-incorporated)
                             :outcome (get-in result [:outcome :outcome])
                             :authoritative (protocol/authoritative
                                             {:presence :present :basis basis
                                              :projection {:request/id "r1" :request/status (:status (first rows))}})}))}
        executor (publication/build-executor opts)
        config {:xtdb-config {} :source-id "v2474-targets" :rules-context {}
                :worker-options {:worker-id "v2474" :poll-ms 20}
                :live-options {} :assembled-publication
                (assoc mapping :model-entity-topics {:requests #{:request}}
                               :require-derived-model? true
                               :operation-model-entities {:request/claim #{:requests}})
                :http-jdk {} :assembled-operations {:authorize-request? (constantly true)}}
        error (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (doseq [incomplete [(dissoc opts :construction-contract) (dissoc opts :target-contract!)]]
      (is (= :incomplete-document-target-contract
             (:reason (error #(publication/build-executor incomplete))))))
    (is (= :invalid-document-target-callback
           (:reason (error #(publication/build-executor (assoc opts :target-contract! nil))))))
    (is (= :invalid-construction-contract
           (:error/kind (error #(publication/build-executor (assoc opts :construction-contract {}))))))
    (is (= :command-operation-contract-requires-document-targets
           (:reason (error #(publication/build-executor
                             (dissoc opts :construction-contract :target-contract!))))))
    (is (= :invalid-command-operation-callback
           (:reason (error #(publication/build-executor
                             (assoc opts :command-operation-contract! nil))))))
    (is (= :predicate-contract-requires-document-targets
           (:reason (error #(publication/build-executor
                             (dissoc opts :construction-contract :target-contract! :command-operation-contract!))))))
    (is (= :invalid-predicate-callback
           (:reason (error #(publication/build-executor (assoc opts :predicate-contract! nil))))))
    (is (= :post-state-contract-requires-document-targets
           (:reason (error #(publication/build-executor
                             (dissoc opts :construction-contract :target-contract! :command-operation-contract! :predicate-contract!))))))
    (is (= :invalid-post-state-callback
           (:reason (error #(publication/build-executor (assoc opts :post-state-contract! nil))))))
    (doseq [missing [:command-operation-contract! :post-state-contract!]]
      (is (= :outcome-contract-requires-operation-and-post-state
             (:reason (error #(publication/build-executor (dissoc opts missing)))))))
    (is (= :invalid-outcome-callback
           (:reason (error #(publication/build-executor (assoc opts :outcome-contract! nil))))))
    (with-operations {:request/claim (claim-operation executor)}
      (fn [assembly]
        (with-open [client (HttpClient/newHttpClient)]
          (let [started (host/start! assembly (fn [_] (g/html-response [:div "fallback"])) config)
                node (:xtdb/connectable started)
                execute xtdb/execute-tx-from!]
            (try
              (xt/execute-tx node [[:put-docs :memberships member]])
              (with-redefs [xtdb/execute-tx-from! (fn [ctx ops & [options]]
                                                   ;; Count one call at the actual submission boundary,
                                                   ;; not the delegating arity or outbox-worker writes.
                                                   (when (contains? ctx publication/preparation-key)
                                                     (swap! submissions inc))
                                                   (execute ctx ops options))]
                (doseq [[scenario events] [[:wrong [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:extra [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:invalid [:declare]]
                                           [:invalid-operation [:declare :declare-predicate :declare-operation]]
                                           [:wrong-operation [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:invalid-predicate [:declare :declare-predicate]]
                                           [:missing-predicate [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:wrong-predicate [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:invalid-post-state [:declare :declare-predicate :declare-operation :declare-post-state]]
                                           [:wrong-post-state [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:extra-post-state [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:missing-post-state [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:invalid-outcome [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome]]
                                           [:wrong-outcome [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]
                                           [:extra-outcome [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct]]]]
                  (reset! mode scenario)
                  (reset! trace [])
                  (let [failure (error #((:biff.ring/handler started)
                                           {:uri "/operations/request/claim"
                                            :request-method :post
                                            :headers {"content-type" "application/x-www-form-urlencoded"}
                                            :body (codec/form-encode
                                                   {transport/command-parameter
                                                    (pr-str (protocol/command->wire (command (name scenario))))})}))]
                    (is (= (case scenario
                             :invalid :invalid-target-contract
                             :invalid-operation :invalid-command-operation-contract
                             :wrong-operation :construction-command-operation-mismatch
                             :invalid-predicate :invalid-predicate-contract
                             :missing-predicate :construction-predicate-mismatch
                             :wrong-predicate :construction-predicate-mismatch
                             :invalid-post-state :invalid-post-state-contract
                             :wrong-post-state :construction-post-state-mismatch
                             :extra-post-state :construction-post-state-mismatch
                             :missing-post-state :construction-post-state-mismatch
                             :invalid-outcome :invalid-outcome-contract
                             :wrong-outcome :construction-outcome-mismatch
                             :extra-outcome :construction-outcome-mismatch
                             :construction-target-mismatch)
                           (:error/kind failure)))
                    (is (= events @trace))
                    (is (zero? @submissions))
                    (is (empty? (xt/q node ["SELECT _id FROM requests"])))
                    (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
                    (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))
                (reset! mode :right)
                (reset! trace [])
                (let [settlement (settlement! (send-command! client started "right"))]
                  (is (= :confirmed (:resolution settlement)))
                  (is (= :request/claimed (:outcome settlement))))
                (is (= [:declare :declare-predicate :declare-operation :declare-post-state :declare-outcome :construct :project] @trace))
                (is (= 1 @submissions))
                (is (= [{:xt/id "r1" :status :claimed}]
                       (xt/q node ["SELECT _id, status FROM requests"])))
                (is (= [{:gesso.command/outcome {:outcome :request/claimed}}]
                       (mapv #(select-keys % [:gesso.command/outcome])
                             (xt/q node ["SELECT * FROM gesso_commands"]))))
                (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                ;; Replays neither redeclare targets nor construct a mutation.
                (reset! mode :invalid)
                (reset! trace [])
                (let [settlement (settlement! (send-command! client started "replay"))]
                  (is (= :already-incorporated (:resolution settlement)))
                  (is (= :request/claimed (:outcome settlement))))
                (is (= [:project] @trace))
                (is (= 1 @submissions)))
              (finally (host/stop! started)))))))))
(deftest installed-multi-record-claim-correspondence
  (let [mode (atom :missing-assignment)
        calls (atom 0)
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0"
                :updated-at "t0" :request/location-id "l1"}
        member {:xt/id "m1" :active true :revision 0 :created-at "t0" :updated-at "t0"}
        assignment {:xt/id "primary-r1" :request-id "r1" :helper "helper-1"
                    :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"}
        ;; Independent closed fixture policy, not extracted from the constructed plan.
        tables #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        guards #{[:memberships "m1" :active true] [:memberships "m1" :revision 0]
                 [:memberships "m1" :updated-at "t0"]}
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0]
                  [:requests "r1" :updated-at "t0"]}
        values (into #{} (mapcat (fn [[table id fields]]
                                  (map (fn [[k v]] [table id k v]) fields)))
                     [[:requests "r1" {:xt/id "r1" :status :claimed :revision 1 :created-at "t0"
                                       :updated-at "t1" :request/location-id "l1"}]
                      [:request_assignments "primary-r1" {:xt/id "primary-r1" :request-id "r1"
                                                         :helper "helper-1" :revision 0 :created-at "t1"
                                                         :updated-at "t1" :request/location-id "l1"}]])
        executor (publication/build-executor
                  {:operation :request/claim :submission-mode! (constantly :submit)
                   :may-disclose? (constantly true)
                   :construction-contract {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                                           :required-write-tables tables :allowed-write-tables tables}
                   :target-contract! (fn [_ binding]
                                       (when-not (= "r1" (get-in binding [:arguments :request-id]))
                                         (throw (ex-info "Outside fixture request" {})))
                                       {:required-guard-targets #{[:memberships "m1"]} :allowed-guard-targets #{[:memberships "m1"]}
                                        :required-write-targets targets :allowed-write-targets targets})
                   :predicate-contract! (fn [_ _] {:required-guard-checks guards :allowed-guard-checks guards
                                                   :required-update-checks updates :allowed-update-checks updates})
                   :command-operation-contract! (fn [_ _] {:required-command-operations operations :allowed-command-operations operations})
                   :post-state-contract! (fn [_ _] {:required-post-state-values values :allowed-post-state-values values})
                   :outcome-contract! (fn [_ _] {:expected-outcome {:outcome :request/claimed}})
                   :construct! (fn [_]
                                 (swap! calls inc)
                                 {:spec {:purpose :gesso.live/refresh}
                                  :model-plan {:guards [(model-command/guard :memberships member
                                                                             (assoc version :compare-keys [:revision :updated-at :active]))]
                                               :commands (cond-> [(model-command/update-command
                                                                   :requests :claim before
                                                                   (assoc before :status :claimed :revision 1 :updated-at "t1")
                                                                   (assoc version :compare-keys [:revision :updated-at :status]))]
                                                           (not= :missing-assignment @mode)
                                                           (conj (model-command/create :request_assignments
                                                                                       (cond-> assignment (= :wrong-helper @mode) (assoc :helper "other"))
                                                                                       version)))}
                                  :outcome {:outcome :request/claimed} :now "t1"})
                   :project! (fn [{:keys [ctx]} result]
                               (let [{:keys [basis rows]} (frontier/q-with-basis!
                                                           ctx ["SELECT r.status, a.helper FROM requests r JOIN request_assignments a ON r._id = a.request_id WHERE r._id = ? AND a._id = ?" "r1" "primary-r1"])]
                                 {:resolution (if (= :incorporated (:status result)) :confirmed :already-incorporated)
                                  :outcome (get-in result [:outcome :outcome])
                                  :authoritative (protocol/authoritative {:presence :present :basis basis :projection (first rows)})}))})
        operation (fixture/trusted-operation :request/claim {:published-change-topics #{:request :request-assignment} :execute! executor})
        config {:xtdb-config {} :source-id "v2479-claim" :rules-context {}
                :worker-options {:worker-id "v2479" :poll-ms 20} :live-options {}
                :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request} :request_assignments #{:request-assignment}}
                                              :require-derived-model? true
                                              :operation-model-entities {:request/claim #{:requests :request_assignments}})
                :http-jdk {} :assembled-operations {:authorize-request? (constantly true)}}]
    (with-operations {:request/claim operation}
      (fn [assembly]
        (with-open [client (HttpClient/newHttpClient)]
          (let [started (host/start! assembly (fn [_] (g/html-response [:div "fallback"])) config)
                node (:xtdb/connectable started)
                failure (fn [attempt]
                          (try ((:biff.ring/handler started)
                                {:uri "/operations/request/claim" :request-method :post
                                 :headers {"content-type" "application/x-www-form-urlencoded"}
                                 :body (codec/form-encode {transport/command-parameter (pr-str (protocol/command->wire (command attempt)))})})
                               nil (catch Exception e (ex-data e))))
                unchanged! (fn []
                             (is (= [{:status :open :revision 0}] (xt/q node ["SELECT status, revision FROM requests"])))
                             (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
                             (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))]
            (try
              (xt/execute-tx node [[:put-docs :requests before] [:put-docs :memberships member]])
              (doseq [[scenario kind] [[:missing-assignment :construction-contract-mismatch]
                                       [:wrong-helper :construction-post-state-mismatch]]]
                (reset! mode scenario)
                (is (= kind (:error/kind (failure (name scenario)))))
                (unchanged!)
                (is (empty? (xt/q node ["SELECT _id FROM request_assignments"]))))
              ;; A real database identity conflict must roll back all writes.
              (reset! mode :right)
              (xt/execute-tx node [[:put-docs :request_assignments (assoc assignment :helper "other")]])
              (is (= :xtdb/assert-failed (:xtdb.error/code (failure "occupied"))))
              (unchanged!)
              (is (= [{:helper "other"}] (xt/q node ["SELECT helper FROM request_assignments"])))
              (xt/execute-tx node [[:delete-docs :request_assignments "primary-r1"]])
              (let [result (settlement! (send-command! client started "claim"))]
                (is (= :confirmed (:resolution result)))
                (is (= :request/claimed (:outcome result)))
                (is (= {:status :claimed :helper "helper-1"} (get-in result [:authoritative :projection]))))
              (is (= [{:status :claimed :revision 1}] (xt/q node ["SELECT status, revision FROM requests"])))
              (is (= [{:xt/id "primary-r1" :request-id "r1" :helper "helper-1"}]
                     (xt/q node ["SELECT _id, request_id, helper FROM request_assignments"])))
              (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
              (is (= #{{:topic :request :subject "l1"} {:topic :request-assignment :subject "l1"}}
                     (set (xt/q node ["SELECT gesso$live$outbox$topic AS topic, gesso$live$outbox$subject_id AS subject FROM gesso_live_outbox"]))))
              (let [n @calls replay (settlement! (send-command! client started "claim-replay"))]
                (is (= :already-incorporated (:resolution replay)))
                (is (= :request/claimed (:outcome replay)))
                (is (= n @calls))
                (is (= 1 (count (xt/q node ["SELECT _id FROM request_assignments"]))))
                (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
                (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))
              (finally (host/stop! started)))))))))

(defn- read-claim-invalidation [input]
  (let [reader (BufferedReader. (InputStreamReader. input StandardCharsets/UTF_8))]
    (loop []
      (when-let [line (.readLine reader)]
        (if (= "event: live-update" line)
          (let [data (.readLine reader)]
            (when (and data (str/starts-with? data "data: "))
              (edn/read-string (subs data 6))))
          (recur))))))

(def ^:dynamic *claim-browser-observer?* false)

(deftest distinct-claim-commands-contend-for-one-primary-assignment
  (doseq [recovery-mode [:valid :wrong-outcome :missing-guard :domain-write]]
   (session/with-session-authority!
    #{:fixture/helper-1 :fixture/helper-2 :fixture/reader}
    (fn [{:keys [source issue! revoke!]}]
     (let [ready (CountDownLatch. 2)
        recovery-trace (atom [])
        observed (atom [])
        projected (atom [])
        streams (atom [])
        observer (atom nil)
        page-basis (atom nil)
        fragment-reads (atom [])
        credentials {"a" (issue! :fixture/helper-1) "b" (issue! :fixture/helper-2)
                     "reader" (issue! :fixture/reader)
                     "revoked" (issue! :fixture/reader)}
        headers-for (fn [who] (let [{:keys [cookie csrf-token]} (get credentials who)]
                               {"cookie" cookie "x-gesso-proving-csrf" csrf-token}))
        read-authorized? (fn [ctx] (= :authenticated (:status ((:acquire-principal! source) ctx))))
        principal-fn (fn [ctx]
                       (let [who (:principal-id ((:acquire-principal! source) ctx))]
                         (when (contains? #{:fixture/helper-1 :fixture/helper-2} who)
                           (identity/principal (name who)))))
        actor (comp identity/raw-value :principal)
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0"
                :updated-at "t0" :request/location-id "l1"}
        member {:xt/id "m1" :active true :revision 0 :created-at "t0" :updated-at "t0"}
        assignment {:xt/id "primary-r1" :request-id "r1" :helper "helper-1"
                    :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"}
        ;; Independent closed fixture policy, not extracted from the constructed plan.
        tables #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        guards (fn [id] #{[:memberships id :active true] [:memberships id :revision 0]
                          [:memberships id :updated-at "t0"]})
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0]
                  [:requests "r1" :updated-at "t0"]}
        values (fn [helper] (into #{} (mapcat (fn [[table id fields]]
                                  (map (fn [[k v]] [table id k v]) fields)))
                     [[:requests "r1" {:xt/id "r1" :status :claimed :revision 1 :created-at "t0"
                                       :updated-at "t1" :request/location-id "l1"}]
                      [:request_assignments "primary-r1" {:xt/id "primary-r1" :request-id "r1"
                                                         :helper helper :revision 0 :created-at "t1"
                                                         :updated-at "t1" :request/location-id "l1"}]]))
        refusal {:outcome :request/not-claimable :reason :already-claimed}
        refusal-guards #{[:requests "r1" :status :claimed]
                         [:requests "r1" :revision 1]
                         [:requests "r1" :updated-at "t1"]}
        opts {:operation :request/claim :submission-mode! (constantly :submit)
                   :may-disclose? (fn [binding record] (= binding (:gesso.command/binding record)))
                   :construction-contract {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                                           :required-write-tables tables :allowed-write-tables tables}
                   :target-contract! (fn [_ binding]
                                       (when-not (= "r1" (get-in binding [:arguments :request-id]))
                                         (throw (ex-info "Outside fixture request" {})))
                                       {:required-guard-targets #{[:memberships (actor binding)]} :allowed-guard-targets #{[:memberships (actor binding)]}
                                        :required-write-targets targets :allowed-write-targets targets})
                   :predicate-contract! (fn [_ binding] {:required-guard-checks (guards (actor binding)) :allowed-guard-checks (guards (actor binding))
                                                   :required-update-checks updates :allowed-update-checks updates})
                   :command-operation-contract! (fn [_ _] {:required-command-operations operations :allowed-command-operations operations})
                   :post-state-contract! (fn [_ binding] {:required-post-state-values (values (actor binding)) :allowed-post-state-values (values (actor binding))})
                   :outcome-contract! (fn [_ binding] {:expected-outcome {:outcome :request/claimed :helper (actor binding)}})
                   :construct! (fn [{:keys [ctx command-id] :as invocation}]
                                 (let [helper (actor invocation)
                                       row (first (frontier/q-from! ctx ["SELECT * FROM requests WHERE _id = ?" "r1"]))
                                       membership (first (frontier/q-from! ctx ["SELECT * FROM memberships WHERE _id = ?" helper]))]
                                   (swap! observed conj [(identity/raw-value command-id) helper row])
                                   (.countDown ready)
                                   (when-not (.await ready 15 TimeUnit/SECONDS)
                                     (throw (ex-info "Claim construction barrier timed out" {})))
                                   {:spec {:purpose :gesso.live/refresh}
                                    :model-plan {:guards [(model-command/guard :memberships membership
                                                                               (assoc version :compare-keys [:revision :updated-at :active]))]
                                                 :commands [(model-command/update-command
                                                             :requests :claim row
                                                             (assoc row :status :claimed :revision 1 :updated-at "t1")
                                                             (assoc version :compare-keys [:revision :updated-at :status]))
                                                            (model-command/create :request_assignments (assoc assignment :helper helper) version)]}
                                    :outcome {:outcome :request/claimed :helper helper} :now "t1"}))
                   :abort-contract
                   {:construction-contract {:required-guard-tables #{:requests} :allowed-guard-tables #{:requests}
                                            :required-write-tables #{:gesso_commands} :allowed-write-tables #{:gesso_commands}}
                    :target-contract! (fn [_ binding]
                                        (swap! recovery-trace conj [:declare (:command-id binding)])
                                        {:required-guard-targets #{[:requests "r1"]} :allowed-guard-targets #{[:requests "r1"]}
                                         :required-write-targets #{} :allowed-write-targets #{}})
                    :predicate-contract! (fn [_ _] {:required-guard-checks refusal-guards :allowed-guard-checks refusal-guards
                                                    :required-update-checks #{} :allowed-update-checks #{}})
                    :command-operation-contract! (fn [_ _] {:required-command-operations #{} :allowed-command-operations #{}})
                    :post-state-contract! (fn [_ _] {:required-post-state-values #{} :allowed-post-state-values #{}})
                    :outcome-contract! (fn [_ _] {:expected-outcome refusal})}
                   :construct-after-abort!
                   (fn [{:keys [ctx command-id]}]
                     (swap! recovery-trace conj [:construct command-id])
                     (let [row (first (frontier/q-from! ctx ["SELECT * FROM requests WHERE _id = ?" "r1"]))]
                       (when (= :claimed (:status row))
                         {:spec nil
                          :model-plan (cond-> {:guards [(model-command/guard :requests row
                                                    (if (= :missing-guard recovery-mode) version
                                                        (assoc version :compare-keys [:revision :updated-at :status])))]}
                                        (= :domain-write recovery-mode)
                                        (assoc :commands [(model-command/create :request_assignments (assoc assignment :xt/id "illegal") version)]))
                          :outcome (if (= :wrong-outcome recovery-mode) {:outcome :request/claimed} refusal)
                          :now "t2"})))
                   :project! (fn [{:keys [ctx command-id]} result]
                               (let [{:keys [basis rows]} (frontier/q-with-basis!
                                                           ctx ["SELECT r.status, a.helper FROM requests r JOIN request_assignments a ON r._id = a.request_id WHERE r._id = ? AND a._id = ?" "r1" "primary-r1"])]
                                 (swap! projected conj [(identity/raw-value command-id) (:status result)])
                                 {:resolution (if (= refusal (:outcome result)) :rejected
                                                  (if (= :incorporated (:status result)) :confirmed :already-incorporated))
                                  :outcome (get-in result [:outcome :outcome])
                                  :authoritative (protocol/authoritative {:presence :present :basis basis :projection (first rows)})}))}
        executor (publication/build-executor opts)
        operation (fixture/trusted-operation :request/claim {:published-change-topics #{:request :request-assignment} :execute! executor})
        resolve-id (fn [ctx _] (or (get-in ctx [:headers "fragment-id"])
                                  (when (= "scope=l1" (:query-string ctx)) "l1")))
        config {:xtdb-config {} :source-id "v2480-claim" :rules-context {}
                :worker-options {:worker-id "v2480" :poll-ms 20} :live-options {}
                :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request} :request_assignments #{:request-assignment}}
                                              :require-derived-model? true
                                              :operation-model-entities {:request/claim #{:requests :request_assignments}})
                :http-jdk {} :assembled-operations {:authorize-request? (comp some? principal-fn)}
                :assembled-fragments {:resolve-id resolve-id}
                :assembled-sse {:resolve-id resolve-id
                                :session-authorized? read-authorized?
                                :sse-options {:keepalive-ms 50} :max-connections 4}}]
    (let [reason (fn [config] (try (publication/build-executor config) nil
                                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
      (is (= :invalid-abort-contract (reason (dissoc opts :construct-after-abort!))))
      (is (= :invalid-abort-contract (reason (update opts :abort-contract dissoc :outcome-contract!))))
      (is (= :abort-contract-requires-guarded-journal-only
             (reason (assoc-in opts [:abort-contract :construction-contract :allowed-write-tables]
                               #{:requests :gesso_commands})))))
    (with-redefs [fixture/allow? (fn [ctx id] (and (= "l1" id) (read-authorized? ctx)))
                  fixture/generic-query
                  (fn [ctx id]
                    (let [row (first (live/q ctx ["SELECT r.status, a.helper FROM requests r LEFT JOIN request_assignments a ON r._id = a.request_id WHERE r.request$location_id = ? AND r._id = ?" id "r1"]))]
                      (swap! fragment-reads conj {:uri (:uri ctx) :id id
                                                  :progression (live/progression ctx) :row row
                                                  :wire (get-in ctx [:headers progression-http/request-header-name])})
                      {:fragment/id (str (last (str/split (:uri ctx) #"/")) "-" id)
                       :row row}))
                  fixture/generic-render
                  (fn [{:keys [fragment/id row]}]
                    [:section {:id id}
                     [:span {:data-field "status"} (name (:status row))]
                     [:span {:data-field "helper"} (or (:helper row) "unassigned")]])]
     (fixture/with-surfaced-application
     {:operations {:request/claim operation}
      :route-capabilities (fixture/route-capabilities {:request/claim operation})
      :principal-fn principal-fn
      :rendered-surfaces {:board [:main (browser/claim-action (fixture/render-context {:request/claim operation})
                                                 fixture/rendered-basis "fixture-csrf")]}}
     (fn [{:keys [assembly report]}]
       (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
       (with-open [client (HttpClient/newHttpClient)]
        (let [delegate (if (= :valid recovery-mode)
                         (browser/shell-handler
                          (get-in assembly [:operation-acquisition-assembly :acquisition-assembly :realizations :request-list :live-app])
                          (get-in assembly [:operation-acquisition-assembly :execution-assembly :route-assembly :operation-assembly :operation-capabilities])
                          read-authorized?
                          (fn [ctx] (let [who (:principal-id ((:acquire-principal! source) ctx))]
                                      (when (contains? #{:fixture/helper-1 :fixture/helper-2} who)
                                        ((:browser-csrf-token! source) ctx who))))
                          (fn [ctx] (let [{:keys [basis rows]} (frontier/q-with-basis! ctx ["SELECT r.status, a.helper FROM requests r LEFT JOIN request_assignments a ON r._id = a.request_id WHERE r.request$location_id = ? AND r._id = ?" "l1" "r1"])
                                          row (first rows)]
                                      (reset! page-basis basis)
                                      {:node (fixture/generic-render {:fragment/id "request-list-l1" :row row})
                                       :basis basis :open? (= :open (:status row))})))
                         (fn [_] (g/html-response [:div "fallback"])))
              started (host/start! assembly delegate config)
             node (:xtdb/connectable started)
             fragment! (fn [path headers]
                         (let [builder (HttpRequest/newBuilder
                                        (URI/create (str (get-in started [host/listener-key :base-url]) path)))]
                           (.timeout builder (Duration/ofSeconds 10))
                           (doseq [[k v] headers] (.header builder k v))
                           (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))
             stream! (fn [path headers]
                       (let [builder (HttpRequest/newBuilder
                                      (URI/create (str (get-in started [host/listener-key :base-url]) path)))]
                         (.timeout builder (Duration/ofSeconds 10))
                         (doseq [[k v] headers] (.header builder k v))
                         (let [response (.send client (.build builder) (HttpResponse$BodyHandlers/ofInputStream))]
                           (swap! streams conj (.body response))
                           response)))
             request! (fn [headers id execution network?]
                        (let [cmd (assoc (command execution) :command-id (identity/command-id id))
                              headers (assoc headers "content-type" "application/x-www-form-urlencoded")
                              body (codec/form-encode {transport/command-parameter (pr-str (protocol/command->wire cmd))})]
                          (if network?
                            (let [builder (HttpRequest/newBuilder
                                           (URI/create (str (get-in started [host/listener-key :base-url]) "/operations/request/claim")))]
                              (doseq [[k v] headers] (.header builder k v))
                              (let [response (.send client (-> builder (.timeout (Duration/ofSeconds 35))
                                                               (.POST (HttpRequest$BodyPublishers/ofString body)) .build)
                                                    (HttpResponse$BodyHandlers/ofString))]
                                {:status (.statusCode response) :body (.body response)}))
                            ((:biff.ring/handler started) {:uri "/operations/request/claim" :request-method :post
                                                          :headers headers :body body}))))
             invoke (fn [session id execution]
                      (try
                        (let [response (request! (headers-for session) id execution (= :valid recovery-mode))]
                          {:status (:status response)
                           :settlement (when (= 200 (:status response))
                                         (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (:body response) StandardCharsets/UTF_8)))]
                                           (protocol/wire->settlement (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))))})
                        (catch Exception e {:failure (ex-data e)})))]
         (try
           (xt/execute-tx node [[:put-docs :requests before]
                                [:put-docs :memberships (assoc member :xt/id "helper-1") (assoc member :xt/id "helper-2")]])
           (when (= :valid recovery-mode)
             ;; Actual network refusals must not reach either constructor.
             (doseq [headers [{"x-fixture-session" "a"}
                              (dissoc (headers-for "a") "x-gesso-proving-csrf")
                              (assoc (headers-for "a") "x-gesso-proving-csrf" (get-in credentials ["b" :csrf-token]))
                              (headers-for "reader")]]
               (is (= 403 (:status (request! headers "denied" "denied" true)))))
             (is (empty? @observed))
             (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
             (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
             (let [response (fragment! "/fragments/request-list" (assoc (headers-for "reader") "fragment-id" "l1"))]
               (is (= 200 (.statusCode response)))
               (is (str/includes? (.body response) ">open<"))
               (is (str/includes? (.body response) ">unassigned<")))
             (browser/verify-helper-pages! fragment! headers-for credentials page-basis)
             (let [page (fragment! "/claim-observer" (headers-for "reader"))
                   body (.body page)]
               (is (= 403 (.statusCode (fragment! "/claim-observer" {}))))
               (is (= 200 (.statusCode page)))
               (is (str/includes? body "data-gesso-live-fragment=\"request-list-l1\""))
               (is (str/includes? body "sse-connect=\"/streams/request-list?scope=l1\""))
               (is (str/includes? body "hx-get=\"/fragments/request-list?scope=l1\""))
               (is (str/includes? body ">open<"))
               (is (str/includes? body ">unassigned<"))
               (doseq [path ["/claim-assets/htmx.js" "/claim-assets/sse.js"
                             "/claim-assets/runtime.js" "/claim-assets/bootstrap.js"]]
                 (let [asset (fragment! path {})]
                   (is (= 200 (.statusCode asset)))
                   (is (not (str/blank? (.body asset))))))))
           (let [stream-headers (fn [who] (assoc (headers-for who) "fragment-id" "l1"))
                 pending (when (= :valid recovery-mode)
                           (let [path "/streams/request-list"
                                 denied (stream! path {"fragment-id" "l1"})
                                 wrong-scope (stream! path (assoc (stream-headers "reader") "fragment-id" "l2"))
                                 list-response (stream! (str path "?scope=l1") (headers-for "reader"))
                                 toolbar-response (stream! "/streams/request-toolbar" (stream-headers "a"))
                                 revoked-response (stream! path (stream-headers "revoked"))]
                             (is (= [403 403 200 200 200]
                                    (mapv #(.statusCode %) [denied wrong-scope list-response toolbar-response revoked-response])))
                             (is (= 3 (:subscription-count (live-source/stats (get-in started [:gesso.live/system :source])))))
                             (let [revoked (future (read-claim-invalidation (.body revoked-response)))]
                               (revoke! (get-in credentials ["revoked" :session-id]))
                               (is (= nil (deref revoked 10000 ::timeout))))
                             [(future (read-claim-invalidation (.body list-response)))
                              (future (read-claim-invalidation (.body toolbar-response)))]))
                 _ (when (and *claim-browser-observer?* (= :valid recovery-mode))
                     (reset! observer (browser/open! (get-in started [host/listener-key :base-url])
                                                    (get credentials "reader")
                                                    #(= 3 (:subscription-count (live-source/stats (get-in started [:gesso.live/system :source])))))))
                 a (future (invoke "a" "claim-a" "attempt-a"))
                 b (future (invoke "b" "claim-b" "attempt-b"))
                 results [(deref a 40000 ::timeout) (deref b 40000 ::timeout)]]
             (when (some #{::timeout} results)
               (future-cancel a) (future-cancel b)
               (throw (ex-info "Claim race timed out" {})))
             (let [winner-index (if (= :confirmed (get-in (first results) [:settlement :resolution])) 0 1)
                   winner (nth results winner-index) loser (nth results (- 1 winner-index))
                   actors [["a" "claim-a" "helper-1"] ["b" "claim-b" "helper-2"]]
                   [session id helper] (nth actors winner-index)
                   [loser-session loser-id loser-helper] (nth actors (- 1 winner-index))
                   valid? (= :valid recovery-mode)]
               (is (= #{["claim-a" "helper-1" before] ["claim-b" "helper-2" before]} (set @observed)))
               (is (= 2 (count @observed)))
               (is (= 1 (count (filter #(= :confirmed (get-in % [:settlement :resolution])) results))))
               (is (= :confirmed (get-in winner [:settlement :resolution])))
               (is (= :request/claimed (get-in winner [:settlement :outcome])))
               (is (= {:status :claimed :helper helper} (get-in winner [:settlement :authoritative :projection])))
               (is (= [[:declare (identity/command-id loser-id)] [:construct (identity/command-id loser-id)]] @recovery-trace))
               (if valid?
                 (do (is (= :rejected (get-in loser [:settlement :resolution])))
                     (is (= :request/not-claimable (get-in loser [:settlement :outcome])))
                     (is (= {:status :claimed :helper helper} (get-in loser [:settlement :authoritative :projection]))))
                 (case recovery-mode
                   :wrong-outcome (is (= :construction-outcome-mismatch (get-in loser [:failure :error/kind])))
                   :missing-guard (is (= :construction-predicate-mismatch (get-in loser [:failure :error/kind])))
                   :domain-write (is (= :journal-only-requires-no-model-commands (get-in loser [:failure :reason])))))
               (is (= [{:status :claimed :revision 1}] (xt/q node ["SELECT status, revision FROM requests"])))
               (is (= [{:xt/id "primary-r1" :request-id "r1" :helper helper}] (xt/q node ["SELECT _id, request_id, helper FROM request_assignments"])))
               (let [records (into {} (map (juxt :xt/id clojure.core/identity)) (xt/q node ["SELECT * FROM gesso_commands"]))]
                 (is (= (cond-> #{id} valid? (conj loser-id)) (set (keys records))))
                 (is (= (identity/principal helper) (get-in records [id :gesso.command/binding :principal])))
                 (is (= {:outcome :request/claimed :helper helper} (get-in records [id :gesso.command/outcome])))
                 (when valid?
                   (is (= (identity/principal loser-helper) (get-in records [loser-id :gesso.command/binding :principal])))
                   (is (= refusal (get-in records [loser-id :gesso.command/outcome])))))
               (is (= #{{:topic :request :command-id id} {:topic :request-assignment :command-id id}}
                      (set (xt/q node ["SELECT gesso$live$outbox$topic AS topic, gesso$live$outbox$command_id AS command_id FROM gesso_live_outbox"]))))
               (is (= (cond-> #{[id :incorporated]} valid? (conj [loser-id :incorporated])) (set @projected)))
               (when valid? (browser/verify-no-claim-action! fragment! (headers-for session)))
               (when-let [opened @observer]
                 (browser/verify! opened helper)
                 (is (some #(and (= "/fragments/request-list" (:uri %))
                                 (= {:status :claimed :helper helper} (:row %))
                                 (some? (:progression %))
                                 (string? (:wire %))) @fragment-reads))
                 (browser/close! opened)
                 (reset! observer nil))
               (when valid?
                 (doseq [[task topic who] (map vector pending [:fixture/request-list :fixture/request-toolbar] ["reader" "a"])]
                   (let [payload (deref task 10000 ::timeout)]
                     (is (= {:topic topic :id "l1"} (select-keys (:invalidation payload) [:topic :id])))
                     (is (some? (:progression payload)))
                     (when-let [wire (:progression payload)]
                       (let [requirement (progression/wire->requirement wire)
                             path (str "/fragments/" (name topic))
                             headers (assoc (stream-headers who) progression-http/request-header-name
                                            (progression-http/encode-request-progression requirement))
                             before-reads @fragment-reads]
                         ;; A delivered minimum-read token never confers read authority.
                         (doseq [denied [(dissoc headers "cookie")
                                         (assoc headers "fragment-id" "l2")
                                         (merge headers (headers-for "revoked"))]]
                           (is (= 403 (.statusCode (fragment! path denied)))))
                         (is (= 400 (.statusCode (fragment! path (assoc headers progression-http/request-header-name "%ZZ")))))
                         (is (= before-reads @fragment-reads))
                         (let [response (if (= topic :fixture/request-list)
                                          (fragment! (str path "?scope=l1") (dissoc headers "fragment-id"))
                                          (fragment! path headers))
                               read (peek @fragment-reads)]
                           (is (= 200 (.statusCode response)))
                           (is (= "no-store" (.orElse (.firstValue (.headers response) "cache-control") "")))
                           (is (str/includes? (.body response) (str "id=\"" (name topic) "-l1\"")))
                           (is (str/includes? (.body response) ">claimed<"))
                           (is (str/includes? (.body response) (str ">" helper "<")))
                           (is (= (inc (count before-reads)) (count @fragment-reads)))
                           (is (= {:uri path :id "l1" :row {:status :claimed :helper helper}}
                                  (select-keys read [:uri :id :row])))
                           (is (progression/covers? (:progression read) requirement))))))))
               (let [replay (invoke session id "winner-replay")]
                 (is (= :already-incorporated (get-in replay [:settlement :resolution])))
                 (is (= {:status :claimed :helper helper} (get-in replay [:settlement :authoritative :projection])))
                 (is (= 2 (count @observed)))
                 (is (= [id :replay] (last @projected)))
                 (when valid?
                   (let [replay (invoke loser-session loser-id "loser-replay")]
                     (is (= :rejected (get-in replay [:settlement :resolution])))
                     (is (= :request/not-claimable (get-in replay [:settlement :outcome])))
                     (is (= {:status :claimed :helper helper} (get-in replay [:settlement :authoritative :projection])))
                     (is (= [loser-id :replay] (last @projected)))
                     (is (= 2 (count @recovery-trace)))
                     (is (= 2 (count @observed)))))
                 (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                 (when valid?
                   (revoke! (get-in credentials [session :session-id]))
                   (is (= 403 (:status (request! (headers-for session) id "revoked-replay" true))))
                   (is (= 2 (count @observed)))))))
           (finally
             (when-let [opened @observer] (browser/close! opened))
             (doseq [input @streams] (.close input))
             (host/stop! started)))))))))))))

(defn -main [& args]
  (when (seq args) (throw (ex-info "Unexpected Claim browser gate arguments" {})))
  (let [result (binding [*claim-browser-observer?* true
                         t/*report-counters* (ref t/*initial-report-counters*)]
                 (t/test-vars [#'distinct-claim-commands-contend-for-one-primary-assignment])
                 @t/*report-counters*)]
    (println "GESSO CLAIM CHROMIUM OBSERVER GATE" (pr-str result))
    (shutdown-agents)
    (System/exit (if (and (= 1 (:test result)) (zero? (:fail result)) (zero? (:error result))) 0 1))))
