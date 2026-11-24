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
            [gesso.live.outbox.read-frontier :as frontier]
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
        tables {:required-guard-tables #{} :allowed-guard-tables #{}
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
                                    {:required-guard-targets #{} :allowed-guard-targets #{}
                                     :required-write-targets (if (= :invalid @mode) #{} #{target})
                                     :allowed-write-targets #{target}}))
              :command-operation-contract! (fn [_ binding]
                                             (swap! trace conj :declare-operation)
                                             (let [entry [:requests (get-in binding [:arguments :request-id])
                                                          (if (= :wrong-operation @mode) :claim :create)]]
                                               {:required-command-operations (if (= :invalid-operation @mode) #{} #{entry})
                                                :allowed-command-operations #{entry}}))
              :construct! (fn [_]
                            (swap! trace conj :construct)
                            {:spec {:purpose :gesso.live/refresh}
                             :model-plan {:commands (mapv #(model-command/create :requests (doc %) version)
                                                          (case @mode
                                                            :wrong ["r2"]
                                                            :extra ["r1" "r2"]
                                                            ["r1"]))}
                             :outcome {:outcome :request/claimed} :now "t0"})
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
    (with-operations {:request/claim (claim-operation executor)}
      (fn [assembly]
        (with-open [client (HttpClient/newHttpClient)]
          (let [started (host/start! assembly (fn [_] (g/html-response [:div "fallback"])) config)
                node (:xtdb/connectable started)
                execute xtdb/execute-tx-from!]
            (try
              (with-redefs [xtdb/execute-tx-from! (fn [ctx ops & [options]]
                                                   ;; Count one call at the actual submission boundary,
                                                   ;; not the delegating arity or outbox-worker writes.
                                                   (when (contains? ctx publication/preparation-key)
                                                     (swap! submissions inc))
                                                   (execute ctx ops options))]
                (doseq [[scenario events] [[:wrong [:declare :declare-operation :construct]]
                                           [:extra [:declare :declare-operation :construct]]
                                           [:invalid [:declare]]
                                           [:invalid-operation [:declare :declare-operation]]
                                           [:wrong-operation [:declare :declare-operation :construct]]]]
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
                             :construction-target-mismatch)
                           (:error/kind failure)))
                    (is (= events @trace))
                    (is (zero? @submissions))
                    (is (empty? (xt/q node ["SELECT _id FROM requests"])))
                    (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
                    (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))))
                (reset! mode :right)
                (reset! trace [])
                (is (= :confirmed (:resolution (settlement! (send-command! client started "right")))))
                (is (= [:declare :declare-operation :construct :project] @trace))
                (is (= 1 @submissions))
                (is (= [{:xt/id "r1"}] (xt/q node ["SELECT _id FROM requests"])))
                (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
                (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                ;; Replays neither redeclare targets nor construct a mutation.
                (reset! mode :invalid)
                (reset! trace [])
                (is (= :already-incorporated (:resolution (settlement! (send-command! client started "replay")))))
                (is (= [:project] @trace))
                (is (= 1 @submissions)))
              (finally (host/stop! started)))))))))
