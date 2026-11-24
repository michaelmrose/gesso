(ns gesso.live.outbox.http-fragment-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [gesso.choreo.identity :as identity]
            [clojure.test :refer [deftest is]]
            [gesso.core :as g]
            [gesso.live.acquisition-preflight :as acquisition]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.core :as live]
            [gesso.live.model :as model]
            [gesso.model.command :as model-command]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.invalidation :as invalidation]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-sse :as sse]
            [gesso.live.outbox.http-operation :as operation-http]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.progression :as progression]
            [ring.util.codec :as codec]
            [gesso.live.source :as source]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn]
            [gesso.live.outbox.http-fragment :as fragment]
            [gesso.live.progression.http :as progression-http])
  (:import [java.io BufferedReader InputStreamReader ByteArrayInputStream]
           [java.net URI]
           [java.nio.charset StandardCharsets]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- with-assembly
  ([f] (with-assembly "/fragments/request-list/:id" f))
  ([list-path f] (with-assembly list-path "/streams/request-list/:id" f))
  ([list-path stream-path f] (with-assembly list-path stream-path nil f))
  ([list-path stream-path execute! f]
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
                                          {:fragment fragment :path (if (= :request-list fragment) stream-path
                                                                      (str "/streams/" (name fragment)))})}))}
      (fn []
        (let [ops (if (map? execute!)
                    execute!
                    (cond-> (fixture/standard-operations)
                      execute! (assoc :request/claim
                                      (fixture/trusted-operation :request/claim
                                       {:published-change-topics #{:request} :execute! execute!}))))]
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

(def ^:private subject-mapping
  {:subject-keys {:request :request/location-id
                  :request-assignment :request/location-id
                  :audit :request/location-id}})

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

(defn- read-invalidation [input]
  (let [reader (BufferedReader. (InputStreamReader. input StandardCharsets/UTF_8))]
    (loop []
      (when-let [line (.readLine reader)]
        (if (= "event: live-update" line)
          (let [data (.readLine reader)]
            (when (and data (str/starts-with? data "data: "))
              (edn/read-string (subs data 6))))
          (recur))))))

(defn- await-dispatch [worker]
  (loop [remaining 750]
    (let [snapshot (lifecycle/status worker)]
      (if (or (zero? remaining) (= :failed (:status snapshot))
              (= 1 (:total-dispatch-accepted snapshot)))
        snapshot
        (do (Thread/sleep 20) (recur (dec remaining)))))))

(defn- exercise-installed-claim! [lose-reply?]
  (let [seen (atom []) queries (atom []) invocations (atom []) fallback (atom 0)
        prepared-intents (atom nil)
        submitted (atom 0)
        execute xtdb/execute-tx-from!
        lost (ex-info "submitted command reply lost" {:test/lost-reply true})
        constructions (atom 0)
        current-policy? (atom false)
        abort-policy? (atom false)
        abort-constructions (atom 0)
        projections (atom [])
        version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0"
                :request/location-id "r1"}
        after (assoc before :status :claimed :revision 1 :updated-at "t1")
        command (protocol/command {:command-id (identity/command-id "v2366-command")
                                   :execution-id (identity/execution-id "v2366-execution")
                                   :operation :request/claim :arguments {:request-id "r1"}
                                   :observed-basis fixture/rendered-basis})
        form (fn [command]
               (codec/form-encode {operation-http/command-parameter
                                   (pr-str (protocol/command->wire command))}))
        execute! (publication/build-executor
                    {:operation :request/claim
                     :submission-mode! (fn [invocation]
                                         (swap! invocations conj invocation)
                                         :submit)
                     :may-disclose? (fn [binding record]
                                      (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
                     :construct! (fn [{:keys [ctx arguments]}]
                                   (swap! constructions inc)
                                   (when-not (= (:request-id arguments) (:xt/id before))
                                     (throw (ex-info "Unexpected claim subject" {:arguments arguments})))
                                   (if @current-policy?
                                     ;; Explicit trusted fixture policy after a known abort.
                                     ;; A fresh dependency guard prevents a stale refusal.
                                     (let [row (first (frontier/q-from! ctx
                                                        ["SELECT * FROM requests WHERE _id = ?"
                                                         (:request-id arguments)]))]
                                       (when-not (= :done (:status row))
                                         (throw (ex-info "Unexpected rejection state" {})))
                                       {:spec nil
                                        :model-plan {:guards [(model-command/guard :requests row version)]}
                                        :outcome {:outcome :request/not-claimable :reason :request-closed}
                                        :now "2026-11-24T00:00:01Z"})
                                     {:spec {:purpose :gesso.live/refresh}
                                      :model-plan {:commands [(model-command/update-command
                                                                :requests :claim before after version)]}
                                      :outcome {:outcome :request/claimed}
                                      :now "2026-11-24T00:00:00Z"}))
                     :construct-after-abort!
                     (fn [{:keys [ctx arguments]}]
                       (when @abort-policy?
                         (swap! abort-constructions inc)
                         (let [row (first (frontier/q-from! ctx
                                           ["SELECT * FROM requests WHERE _id = ?" (:request-id arguments)]))]
                           (when (= :done (:status row))
                             {:spec nil
                              :model-plan {:guards [(model-command/guard :requests row version)]}
                              :outcome {:outcome :request/not-claimable :reason :request-closed}
                              :now "2026-11-24T00:00:02Z"}))))
                     :project! (fn [{:keys [ctx arguments]} result]
                                 (swap! projections conj (:status result))
                                 (when (= :incorporated (:status result))
                                   (reset! prepared-intents (get-in result [:transaction :outbox/intents])))
                                 (let [id (:request-id arguments)
                                       {:keys [basis rows]} (frontier/q-with-basis!
                                                             ctx ["SELECT status FROM requests WHERE _id = ?" id])
                                       row (first rows)]
                                   {:resolution (cond
                                                  (= :request/not-claimable (get-in result [:outcome :outcome])) :rejected
                                                  (= :incorporated (:status result)) :confirmed
                                                  :else :already-incorporated)
                                    :reason (get-in result [:outcome :reason])
                                    :outcome (get-in result [:outcome :outcome])
                                    :authoritative (protocol/authoritative
                                                    {:presence :present :basis basis
                                                     :projection {:request/id id :request/status (:status row)}})}))})
        resolve-id (fn [ctx _]
                     (or (get-in ctx [:path-params :id]) (get-in ctx [:headers "fragment-id"])))]
    (with-redefs [fixture/allow? (fn [ctx id]
                                   (swap! seen conj [id ctx])
                                   (and (= "allowed" (get-in ctx [:headers "authorization"]))
                                        (= "r1" id)))
                  fixture/generic-query (fn [ctx id]
                                          (swap! queries conj [id (live/progression ctx)])
                                          {:fragment/id (str "actual-" id)
                                           :status (:status (first (live/q ctx ["SELECT status FROM requests WHERE _id = ?" id])))})
                  fixture/generic-render (fn [{:keys [fragment/id status]}]
                                           [:section {:id id} (name (or status :open))])]
      (with-assembly "/fragments/request-list/:id" "/streams/request-list/:id"
        {:request/claim (fixture/trusted-operation :request/claim
                         {:published-change-topics #{:request} :execute! execute!})}
        (fn [assembly]
          (let [started (host/start! assembly
                                     (fn [_] (swap! fallback inc) (g/html-response [:div "fallback"]))
                                     (assoc (config) :http-jdk {}
                                            :live-options {}
                                            :assembled-publication (assoc subject-mapping
                                                                          :model-entity-topics {:requests #{:request}}
                                                                          :require-derived-model? true
                                                                          :operation-model-entities {:request/claim #{:requests}})
                                            :assembled-operations {:authorize-request? #(= "allowed" (get-in % [:headers "authorization"]))}
                                            :assembled-fragments {:resolve-id resolve-id}
                                            :assembled-sse {:resolve-id resolve-id :max-connections 2
                                                            :sse-options {:keepalive-ms 50}}))
                listener (get started host/listener-key)
                status (get started host/sse-status-key)
                node (:xtdb/connectable started)
                system (:gesso.live/system started)
                worker (get started managed/handle-key)
                handler (:biff.ring/handler started)
                client (HttpClient/newHttpClient)
                send! (fn [path headers body-handler]
                        (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))]
                          (.timeout builder (Duration/ofSeconds 10))
                          (doseq [[k v] headers] (.header builder k v))
                          (.send client (.build builder) body-handler)))
                post-command! (fn [headers command]
                        (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/operations/request/claim")))]
                          (.timeout builder (Duration/ofSeconds 10))
                          (.header builder "content-type" "application/x-www-form-urlencoded")
                          (doseq [[k v] headers] (.header builder k v))
                          (.POST builder (HttpRequest$BodyPublishers/ofString (form command)))
                          (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))
                post! #(post-command! % command)
                headers {"authorization" "allowed" "fragment-id" "r1"}
                list-path "/streams/request-list/r1"
                toolbar-path "/streams/request-toolbar"
                fragments ["/fragments/request-list/r1" "/fragments/request-toolbar"]]
            (try
              (is (= 405 (:status (handler {:uri list-path :request-method :post}))))
              (is (= 403 (:status (handler {:uri "/streams/request-list/.." :request-method :get}))))
              (is (empty? @seen))
              (is (= 403 (.statusCode (send! list-path {} (HttpResponse$BodyHandlers/ofString)))))
              (is (= 403 (.statusCode (send! "/streams/request-list/r2" headers (HttpResponse$BodyHandlers/ofString)))))
              (is (zero? (:subscription-count (source/stats (:source system)))))
              (is (= 403 (.statusCode (post! {}))))
              (is (empty? @invocations))
              (is (empty? (xt/q node ["SELECT _id FROM requests"])))
              (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
              ;; Explicit fixture initialization, not part of the admitted command.
              (xt/execute-tx node [[:put-docs :requests before]])
              (doseq [path fragments]
                (is (str/includes? (.body (send! path headers (HttpResponse$BodyHandlers/ofString))) ">open<")))
              (let [list-response (send! list-path headers (HttpResponse$BodyHandlers/ofInputStream))
                    toolbar-response (send! toolbar-path headers (HttpResponse$BodyHandlers/ofInputStream))]
                (with-open [list-body (.body list-response) toolbar-body (.body toolbar-response)]
                  (is (= 200 (.statusCode list-response) (.statusCode toolbar-response)))
                  (is (= 2 ((:occupied-count status))))
                  (is (= 2 (:subscription-count (source/stats (:source system)))))
                  (is (= 503 (.statusCode (send! list-path headers (HttpResponse$BodyHandlers/ofString)))))
                  (is (identical? node (:xtdb/connectable (second (peek @seen)))))
                  (is (= {:id "r1"} (:path-params (second (peek @seen)))))
                  ;; Start both readers before mutation. One bounded observation
                  ;; per stream; timeout is a failed assertion, not a retry.
                  (let [a (future (read-invalidation list-body))
                        b (future (read-invalidation toolbar-body))]
                    (try
                      (let [response (if lose-reply?
                                       (with-redefs [xtdb/execute-tx-from!
                                                     (fn [ctx ops & options]
                                                       (if (some #(and (vector? %)
                                                                       (= [:put-docs :gesso_commands]
                                                                          (subvec % 0 (min 2 (count %))))) ops)
                                                         (do (swap! submitted inc)
                                                             (xt/submit-tx (:xtdb/connectable ctx) ops)
                                                             (throw lost))
                                                         (apply execute ctx ops options)))]
                                         (post! headers))
                                       (post! headers))]
                        (is (= (if lose-reply? 1 0) @submitted))
                        (is (= 200 (.statusCode response)))
                        (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                              settlement (protocol/wire->settlement
                                          (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                          (is (= (if lose-reply? :already-incorporated :confirmed)
                                 (:resolution settlement)))
                          (is (= :request/claimed (:outcome settlement)))
                          (is (= :claimed (get-in settlement [:authoritative :projection :request/status])))
                          (is (= (select-keys command [:command-id :execution-id])
                                 (select-keys settlement [:command-id :execution-id])))))
                      (is (= 1 (count @invocations)))
                      (is (identical? node (get-in (first @invocations) [:ctx :xtdb/connectable])))
                      (let [worker-state (await-dispatch worker)]
                        (is (= 1 (:total-dispatch-accepted worker-state)) (pr-str worker-state)))
                      (doseq [[pending expected-topic path] [[a :fixture/request-list (first fragments)]
                                                            [b :fixture/request-toolbar (second fragments)]]]
                        (let [payload (deref pending 10000 nil)
                              diagnostics (pr-str {:worker (lifecycle/status worker)
                                                   :source (source/stats (:source system))})]
                          (is (= {:topic expected-topic :id "r1"}
                                 (select-keys (:invalidation payload) [:topic :id])) diagnostics)
                          (is (some? (:progression payload)) diagnostics)
                          (when-let [wire (:progression payload)]
                            (let [requirement (progression/wire->requirement wire)
                                  response (send! path (assoc headers progression-http/request-header-name
                                                              (progression-http/encode-request-progression requirement))
                                                  (HttpResponse$BodyHandlers/ofString))]
                              (is (= 200 (.statusCode response)))
                              (is (str/includes? (.body response) ">claimed<"))
                              (is (= ["r1" requirement] (peek @queries)))))))
                      (finally (future-cancel a) (future-cancel b))))
                  (is (= [{:status :claimed :revision 1}]
                         (xt/q node ["SELECT status, revision FROM requests WHERE _id = ?" "r1"])))
                  ;; Recovery cannot expose an execution receipt that was lost.
                  ;; Observe the durable intent independently for that branch.
                  (when lose-reply?
                    (is (nil? @prepared-intents))
                    (reset! prepared-intents (xt/q node ["SELECT * FROM gesso_live_outbox"])))
                  (let [intent-id (:xt/id (first @prepared-intents))]
                    (is (uuid? intent-id))
                    (is (= [{:xt/id intent-id}] (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id]))))
                  (is (= {:status :replay :outcome {:outcome :request/claimed}}
                         (select-keys (publication/reconcile-command!
                                       (:ctx (first @invocations)) (constantly true))
                                      [:status :outcome])))
                  (is (= {:outcome :prior-intent-present
                          :intent-id (:xt/id (first @prepared-intents))
                          :publication-status :dispatch-accepted}
                         (publication/observe-command! (:ctx (first @invocations)))))
                  (is (= {:gesso.live.outbox/source-id (:source-id (config))
                          :gesso.live.outbox/operation :request/claim
                          :gesso.live.outbox/command-id "v2366-command"
                          :gesso.live.outbox/subject-id "r1"}
                         (select-keys (first @prepared-intents)
                                      [:gesso.live.outbox/source-id :gesso.live.outbox/operation
                                       :gesso.live.outbox/command-id :gesso.live.outbox/subject-id])))
                  (let [invocation (first @invocations)
                        error-kind (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e
                                                        (:error/kind (ex-data e)))))
                        forbidden (fn [& _] (throw (ex-info "projector must not run" {})))]
                    (is (= :resolution-not-settleable
                           (error-kind #(publication/reconcile-settlement! invocation (constantly false) forbidden))))
                    (is (= :mismatched-command-binding
                           (reason #(publication/reconcile-settlement!
                                     (-> invocation (assoc :arguments {:request-id "other"})
                                         (assoc-in [:command :arguments] {:request-id "other"}))
                                     (constantly true) forbidden))))
                    (is (= :invalid-resolution
                           (error-kind #(publication/reconcile-settlement!
                                         invocation (constantly true) (constantly {:resolution :failed}))))))
                  ;; A later domain mutation changes the authoritative state.
                  ;; Replay must report that state, not the original projection.
                  (xt/execute-tx node [[:put-docs :requests (assoc after :status :done :revision 2 :updated-at "t2")]])
                  ;; Even an authorized :submit checks the durable record before planning.
                  (let [retry (assoc command :execution-id (identity/execution-id "v2374-retry"))
                        response (post-command! headers retry)
                        marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                        settlement (protocol/wire->settlement
                                    (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                    (is (= 200 (.statusCode response)))
                    (is (= :already-incorporated (:resolution settlement)))
                    (is (= (select-keys retry [:command-id :execution-id])
                           (select-keys settlement [:command-id :execution-id])))
                    (is (= :request/claimed (:outcome settlement)))
                    (is (= :done (get-in settlement [:authoritative :projection :request/status])))
                    (is (= 2 (count @invocations)))
                    (is (= [(if lose-reply? :replay :incorporated) :replay] @projections))
                    (is (= (if lose-reply? 1 0) @submitted))
                    (is (= 1 @constructions))
                    (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
                    (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                    (is (= [{:status :done}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"]))))
                  ;; A distinct command is not a replay. The constructor still
                  ;; describes revision zero; the actual revision-two row must
                  ;; defeat its physical guard, atomically excluding journal
                  ;; and publication writes. Exercise the real listener too:
                  ;; a failed transaction must not fabricate a settlement.
                  (let [stale (assoc command
                                     :command-id (identity/command-id "v2411-stale-command")
                                     :execution-id (identity/execution-id "v2411-stale-execution"))
                        failures (atom [])
                        project-before @projections
                        durable-before (xt/q node ["SELECT * FROM gesso_commands"])
                        intents-before (xt/q node ["SELECT * FROM gesso_live_outbox"])
                        result (with-redefs [xtdb/execute-tx-from!
                                             (fn [ctx ops & [opts]]
                                               ;; Call the terminal arity directly, avoiding
                                               ;; re-entry through the redefined Var.
                                               (try (execute ctx ops opts)
                                                    (catch Exception e
                                                      (swap! failures conj e)
                                                      (throw e))))]
                                 (try (post-command! headers stale)
                                      (catch java.io.IOException e e)))]
                    (is (instance? java.io.IOException result))
                    (is (= [:xtdb/assert-failed]
                           (mapv #(-> % ex-data :xtdb.error/code) @failures)))
                    (is (= 3 (count @invocations)))
                    (is (= 2 @constructions))
                    (is (= project-before @projections))
                    (is (= durable-before (xt/q node ["SELECT * FROM gesso_commands"])))
                    (is (= intents-before (xt/q node ["SELECT * FROM gesso_live_outbox"])))
                    (is (= [{:status :done :revision 2}]
                           (xt/q node ["SELECT status, revision FROM requests WHERE _id = ?" "r1"])))
                    ;; The trusted fixture has observed the definitive abort.
                    ;; A new authorized execution re-evaluates current policy,
                    ;; recording a guarded refusal, not retrying the old write.
                    (reset! current-policy? true)
                    (doseq [[index execution] (map-indexed vector ["v2412-refusal" "v2412-refusal-replay"])]
                      (let [cmd (assoc stale :execution-id (identity/execution-id execution))
                            response (post-command! headers cmd)
                            marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                            settlement (protocol/wire->settlement
                                        (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                        (is (= 200 (.statusCode response)))
                        (is (= :rejected (:resolution settlement)))
                        (is (= :request/not-claimable (:outcome settlement)))
                        (is (= "request-closed" (:reason settlement)))
                        (is (= (select-keys cmd [:command-id :execution-id])
                               (select-keys settlement [:command-id :execution-id])))
                        (is (= :done (get-in settlement [:authoritative :projection :request/status])))
                        (is (= 3 @constructions))
                        (is (= (conj project-before :incorporated)
                               (if (zero? index) @projections (pop @projections))))
                        (is (= (if (zero? index) :incorporated :replay) (peek @projections)))
                        (is (= intents-before (xt/q node ["SELECT * FROM gesso_live_outbox"])))
                        (is (= [{:status :done :revision 2}]
                               (xt/q node ["SELECT status, revision FROM requests WHERE _id = ?" "r1"])))
                        (is (= #{"v2366-command" "v2411-stale-command"}
                               (set (map :xt/id (xt/q node ["SELECT _id FROM gesso_commands"])))))))
                    (is (= 5 (count @invocations)))
                    (is (= {:status :replay :outcome {:outcome :request/not-claimable :reason :request-closed}}
                           (select-keys (publication/reconcile-command! (:ctx (peek @invocations)) (constantly true))
                                        [:status :outcome]))))
                  (reset! current-policy? false)
                  (reset! abort-policy? true)
                  (let [cmd (assoc command
                                   :command-id (identity/command-id "v2413-guarded-refusal")
                                   :execution-id (identity/execution-id "v2413-original-execution"))
                        intents-before (xt/q node ["SELECT * FROM gesso_live_outbox"])
                        response (post-command! headers cmd)
                        marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                        settlement (protocol/wire->settlement
                                    (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                    (is (= 200 (.statusCode response)))
                    (is (= :rejected (:resolution settlement)))
                    (is (= :request/not-claimable (:outcome settlement)))
                    (is (= (select-keys cmd [:command-id :execution-id])
                           (select-keys settlement [:command-id :execution-id])))
                    (is (= :done (get-in settlement [:authoritative :projection :request/status])))
                    (is (= 1 @abort-constructions))
                    (is (= 4 @constructions))
                    (is (= 3 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
                    (is (= intents-before (xt/q node ["SELECT * FROM gesso_live_outbox"])))
                    (is (= [{:status :done :revision 2}]
                           (xt/q node ["SELECT status, revision FROM requests WHERE _id = ?" "r1"]))))
                  (is (zero? @fallback))
                  (host/stop! started)
                  (is ((:drained? status)))
                  (is ((:drained? listener)))
                  (is ((:drained? (get started host/recovery-status-key))))))
              (finally (host/stop! started) (.close client)))))))))

(deftest assembled-streams-share-capacity-and-use-original-model-policy
  (exercise-installed-claim! false))

(deftest installed-model-command-recovers-submitted-transaction-without-replay-write
  (exercise-installed-claim! true))

(deftest assembled-stream-route-rejects-overlap-and-incompatible-profile
  (doseq [[path expected] [["/streams/:id" :ambiguous-stream-route]
                           ["/fragments/:id" :ambiguous-stream-route]
                           ["/operations/request/:id" :ambiguous-stream-route]
                           ["/streams/:id/:id" :unsupported-stream-route]]]
    (with-assembly "/fragments/request-list/:id" path
      (fn [assembly]
        (is (= expected (reason #(sse/prepare! assembly {:resolve-id (constantly "r1")})))))))
  (with-assembly
    (fn [assembly]
      (is (= :invalid-stream-options (reason #(sse/prepare! assembly {:resolve-id identity :path "/forged"}))))
      (is (= :conflicting-sse-profiles
             (reason #(host/start! assembly identity
                                   (assoc (config) :assembled-sse {:resolve-id identity}
                                          :authorized-sse {}))))))))

(deftest assembled-publication-uses-original-graph-and-rejects-incomplete-mappings
  (with-assembly
    (fn [assembly]
      (let [rules (publication/rules! assembly subject-mapping)
            compiled (get-in assembly [:operation-acquisition-assembly :acquisition-assembly :live-app])
            scope-keys #(set (map (juxt :topic :id) %))]
        (doseq [topic (keys (:subject-keys subject-mapping))
                id ["location-1" :location/one 42 (random-uuid)]]
          (is (= (scope-keys (model/expand-change compiled {:topic topic :request/location-id id}))
                 (scope-keys (invalidation/expand rules {} {:topic topic :id id})))))
        (doseq [change [{:topic :unknown :id "r1"}
                        {:topic :request :id "r1" :request/location-id "r2"}
                        {:topic :request :id ""}]]
          (is (thrown? clojure.lang.ExceptionInfo (invalidation/expand rules {} change))))
        (doseq [[options expected]
                [[{} :invalid-subject-mapping]
                 [(update subject-mapping :subject-keys dissoc :audit) :incomplete-topic-mapping]
                 [(assoc-in subject-mapping [:subject-keys :unknown] :id) :incomplete-topic-mapping]
                 [(assoc-in subject-mapping [:subject-keys :request] :id) :unsupported-subject-graph]]]
          (is (= expected (reason #(publication/rules! assembly options)))))
        (let [starts (atom 0)]
          (with-redefs [managed/start-biff-application! (fn [& _] (swap! starts inc))]
            (is (= :conflicting-publication-rules
                   (reason #(host/start! assembly identity
                                         (assoc (config) :assembled-publication subject-mapping)))))
            (is (= :incomplete-topic-mapping
                   (reason #(host/start! assembly identity
                                         (assoc (config) :live-options {}
                                                :assembled-publication {:subject-keys {}})))))
            (is (zero? @starts))))))))

(deftest assembled-publication-refuses-graphs-that-need-more-than-a-durable-subject
  (let [compiled-var (ns-resolve 'gesso.live.application-preflight.fixture 'compiled-live)
        original @compiled-var]
    (doseq [transform [(fn [app] (assoc-in app [:graph :request 0 :when] (constantly true)))
                       (fn [app] (assoc-in app [:graph :request 0 :id-key] :request/other-id))]]
      (with-redefs-fn
        {compiled-var #(model/compile-live-app (transform (original)))}
        (fn []
          (with-assembly
            (fn [assembly]
              (is (= :unsupported-subject-graph
                     (reason #(publication/rules! assembly subject-mapping)))))))))))

(deftest assembled-publication-prepares-only-command-declared-model-changes
  (with-assembly
    (fn [assembly]
      (let [profile (publication/profile! assembly subject-mapping)
            bind (:bind-command profile)
            command {:operation :request/claim :command-id (identity/command-id "command-2369")}
            ctx {publication/preparation-key (bind "owned-source" command)}
            spec {:intent-ids [(random-uuid)] :purpose :gesso.live/refresh}
            change {:topic :request :request/location-id "location-1" :private "not persisted"}
            result (publication/prepare! ctx spec [change])
            intent (first (:intents result))]
        (is (= "location-1" (:gesso.live.outbox/subject-id intent)))
        (is (= "owned-source" (:gesso.live.outbox/source-id intent)))
        (is (= :request/claim (:gesso.live.outbox/operation intent)))
        (is (= "command-2369" (:gesso.live.outbox/command-id intent)))
        (is (not (contains? intent :private)))
        (is (= 1 (count (:guard-ops result))))
        (doseq [[ctx' spec' changes expected]
                [[{} spec [change] :missing-command-preparation]
                 [ctx (assoc spec :source-id "forged") [change] :invalid-command-intent-spec]
                 [ctx (assoc spec :operation :request/cancel) [change] :invalid-command-intent-spec]
                 [ctx (assoc spec :command-id "forged") [change] :invalid-command-intent-spec]
                 [ctx spec [] :missing-model-changes]
                 [ctx spec [(dissoc change :request/location-id)] :invalid-subject]
                 [ctx spec [(assoc change :id "another-location")] :conflicting-subject-field]
                 [ctx spec [(assoc change :topic :audit)] :undeclared-operation-topic]
                 [{publication/preparation-key (bind "owned-source" (assoc command :operation :request/cancel))}
                  spec [change] :undeclared-operation-topic]
                 [{publication/preparation-key (bind "owned-source" (assoc command :operation :request/reassign))}
                  spec [(assoc change :topic :external/audit)] :unknown-change-topic]]]
          (is (= expected (reason #(publication/prepare! ctx' spec' changes)))))))))

(deftest command-publication-transact-preserves-atomicity-and-provider-receipt
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              ctx {:xtdb/connectable node
                   publication/preparation-key
                   (bind "owned-source" {:operation :request/claim
                                         :command-id (identity/command-id "command-2370")})}
              spec {:intent-ids [(random-uuid)] :purpose :gesso.live/refresh}
              change {:topic :request :request/location-id "r1"}
              write (fn [id] [:put-docs :requests {:xt/id id :status :claimed}])
              domain-ids #(set (map :xt/id (xt/q node ["SELECT _id FROM requests"])))
              outbox-ids #(set (map :xt/id (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
              conflict? (fn [f]
                          (try (f) false
                               (catch Exception e
                                 (= :xtdb/assert-failed (:xtdb.error/code (ex-data e))))))]
          (is (= :invalid-domain-ops (reason #(publication/transact! ctx spec [change] []))))
          (is (= :invalid-subject (reason #(publication/transact! ctx spec [{:topic :request}] [(write "bad")]))))
          (is (empty? (domain-ids)))
          (is (empty? (outbox-ids)))
          (let [receipt (publication/transact! ctx spec [change] [(write "r1")])]
            (is (some? (:progression receipt)))
            (is (= (:tx-id (:tx-result receipt)) (get-in receipt [:consistency :tx-id])))
            (is (= (:intent-ids spec) (mapv :xt/id (:outbox/intents receipt)))))
          (is (conflict? #(publication/transact! ctx spec [change] [(write "duplicate")])))
          (is (conflict? #(publication/transact!
                           ctx (assoc spec :intent-ids [(random-uuid)]) [change]
                           [["ASSERT 1 = 0"] (write "guard-failed")])))
          (is (= #{"r1"} (domain-ids)))
          (is (= (set (:intent-ids spec)) (outbox-ids)))
          (let [failure (ex-info "provider result failed" {:commit/status :committed})]
            (with-redefs [xtdb/execute-tx-from! (fn [& _] (throw failure))]
              (is (identical? failure
                              (try (publication/transact! ctx spec [change] [(write "r2")])
                                   nil (catch Exception e e)))))))))))

(deftest derived-publication-identities-fence-replacement-executions
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              command {:operation :request/claim
                       :principal (identity/principal "helper-1")
                       :command-id (identity/command-id "command-2371")
                       :execution-id (identity/execution-id "first")}
              context (fn [source command]
                        {:xtdb/connectable node publication/preparation-key (bind source command)})
              ctx (context "owned-source" command)
              retry (context "owned-source" (assoc command :execution-id (identity/execution-id "retry")))
              spec {:purpose :gesso.live/refresh}
              change {:topic :request :request/location-id "r1"}
              ids (fn [ctx spec changes]
                    (mapv :xt/id (:intents (publication/prepare! ctx spec changes))))
              expected (ids ctx spec [change change])
              write (fn [id] [:put-docs :requests {:xt/id id :status :claimed}])]
          ;; Frozen independent v1 identity vectors protect durable retries
          ;; across deployment changes, not merely within one process.
          (is (= [(java.util.UUID/fromString "2f5346bd-0e1e-85b4-a1d1-2c86817a06e6")
                  (java.util.UUID/fromString "c29521ea-cef4-8c5a-95c5-917462f40ca1")] expected))
          (is (= 2 (count (distinct expected))))
          (is (every? #(and (= 8 (.version ^java.util.UUID %))
                            (= 2 (.variant ^java.util.UUID %))) expected))
          (is (= expected (ids retry spec [change change])))
          (is (= expected (binding [*print-length* 0 *print-level* 0 *print-meta* true]
                            (ids retry spec [change change]))))
          (is (= [(first expected)] (ids retry {:purpose :other/refresh}
                                         [(assoc change :request/location-id "r2")])))
          (doseq [[source other] [["other-source" command]
                                  ["owned-source" (assoc command :principal (identity/principal "helper-2"))]
                                  ["owned-source" (assoc command :command-id (identity/command-id "different"))]
                                  ["owned-source" (assoc command :command-id (identity/command-id :command-2371))]]]
            (is (not= (first expected) (first (ids (context source other) spec [change])))))
          (doseq [bad [(dissoc command :principal)
                       (assoc command :principal (identity/actor "helper-1"))
                       (assoc command :principal (identity/principal {:id "helper-1"}))]]
            (is (= :invalid-derived-command-identity
                   (reason #(ids (context "owned-source" bad) spec [change])))))
          (let [receipt (publication/transact! ctx spec [change] [(write "r1")])]
            (is (= [(first expected)] (mapv :xt/id (:outbox/intents receipt)))))
          (is (= :xtdb/assert-failed
                 (try (publication/transact! retry {:purpose :other/refresh}
                                             [(assoc change :request/location-id "r2") change]
                                             [(write "r2")])
                      nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= [{:xt/id "r1"}] (xt/q node ["SELECT _id FROM requests"])))
          (is (= [{:xt/id (first expected)}] (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))))))

(deftest command-recovery-observes-commit-without-inventing-settlement
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              command {:operation :request/claim
                       :principal (identity/principal "helper-1")
                       :command-id (identity/command-id "command-2372")
                       :execution-id (identity/execution-id "first")}
              context (fn [source command]
                        {:xtdb/connectable node publication/preparation-key (bind source command)})
              ctx (context "owned-source" command)
              retry (context "owned-source" (assoc command :execution-id (identity/execution-id "retry")))
              spec {:purpose :gesso.live/refresh}
              change {:topic :request :request/location-id "r1"}
              id (:xt/id (first (:intents (publication/prepare! ctx spec [change]))))
              write [:put-docs :requests {:xt/id "r1" :status :claimed}]
              lost (ex-info "lost provider receipt" {:commit/status :unknown})
              execute xtdb/execute-tx-from!]
          (is (= {:outcome :not-observed :intent-id id} (publication/observe-command! retry)))
          ;; The real physical transaction commits; simulate losing its
          ;; response before the registered executor can construct settlement.
          (with-redefs [xtdb/execute-tx-from! (fn [& args]
                                               (apply execute args)
                                               (throw lost))]
            (is (identical? lost (try (publication/transact! ctx spec [change] [write])
                                      nil (catch Exception e e)))))
          (is (= {:outcome :prior-intent-present :intent-id id :publication-status :pending}
                 (publication/observe-command! retry)))
          (doseq [[source other] [["other-source" command]
                                  ["owned-source" (assoc command :principal (identity/principal "helper-2"))]
                                  ["owned-source" (assoc command :command-id (identity/command-id "other"))]]]
            (is (= :not-observed (:outcome (publication/observe-command! (context source other))))))
          (is (= :missing-command-preparation (reason #(publication/observe-command! {}))))
          (is (= :invalid-derived-command-identity
                 (reason #(publication/observe-command! (context "owned-source" (dissoc command :principal))))))
          (is (= :xtdb/assert-failed
                 (try (publication/transact! retry spec [change]
                                             [[:put-docs :requests {:xt/id "r2" :status :claimed}]])
                      nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= [{:xt/id "r1"}] (xt/q node ["SELECT _id FROM requests"])))
          (doseq [rows [nil {} [nil] [{:xt/id id}] [{} {}]]]
            (with-redefs [xtdb/q-from (fn [& _] rows)]
              (is (#{:invalid-intent-observation :inconsistent-intent-observation}
                   (reason #(publication/observe-command! retry))))))
          (with-redefs [xtdb/q-from (fn [& _] (throw lost))]
            (is (identical? lost (try (publication/observe-command! retry)
                                      nil (catch Exception e e)))))
          ;; An inconsistent physical row must not certify prior publication.
          (xt/execute-tx node [[:put-docs :gesso_live_outbox
                               (assoc (first (:intents (publication/prepare! ctx spec [change])))
                                      :gesso.live.outbox/source-id "wrong-source")]])
          (is (= :inconsistent-intent-observation (reason #(publication/observe-command! retry)))))))))

(deftest resolved-publication-recovers-exact-durable-command-outcome
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              command {:operation :request/claim :principal (identity/principal "helper-1")
                       :command-id (identity/command-id "command-2373")
                       :execution-id (identity/execution-id "first")
                       :arguments {:request-id "r1"}}
              context (fn [command]
                        {:xtdb/connectable node publication/preparation-key (bind "owned-source" command)})
              ctx (context command)
              retry (context (assoc command :execution-id (identity/execution-id "retry")))
              spec {:purpose :gesso.live/refresh}
              change {:topic :request :request/location-id "r1"}
              outcome {:outcome :request/claimed :request-id "r1"}
              now "2026-11-24T00:00:00Z"
              write (fn [id] [:put-docs :requests {:xt/id id :status :claimed}])
              disclose (fn [binding record]
                         (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
              inspect #(select-keys (publication/reconcile-command! % disclose) [:status :outcome])
              lost (ex-info "lost resolved receipt" {:commit/status :unknown})
              execute xtdb/execute-tx-from!]
          (is (= {:status :unresolved} (inspect retry)))
          (is (= :invalid-resolved-intent-spec
                 (reason #(publication/transact-resolved! ctx (assoc spec :intent-ids [(random-uuid)])
                                                          [change] [(write "bad")] outcome now))))
          (is (= :xtdb/assert-failed
                 (try (publication/transact-resolved! ctx spec [change]
                                                       [["ASSERT 1 = 0"] (write "bad")] outcome now)
                      nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= {:status :unresolved} (inspect retry)))
          (is (empty? (xt/q node ["SELECT _id FROM requests"])))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
          (with-redefs [xtdb/execute-tx-from! (fn [& args] (apply execute args) (throw lost))]
            (is (identical? lost (try (publication/transact-resolved! ctx spec [change]
                                                                     [(write "r1")] outcome now)
                                     nil (catch Exception e e)))))
          (is (= {:status :replay :outcome outcome} (inspect retry)))
          (is (= {:status :incompatible}
                 (inspect (context (assoc command :arguments {:request-id "r2"})))))
          (is (= {:status :unavailable}
                 (inspect (context (assoc command :principal (identity/principal "other"))))))
          (is (= {:status :unavailable}
                 (select-keys (publication/reconcile-command! retry (constantly :truthy)) [:status :outcome])))
          (is (= :missing-command-disclosure (reason #(publication/reconcile-command! retry nil))))
          (is (= :xtdb/assert-failed
                 (try (publication/transact-resolved! (context (assoc command :principal (identity/principal "other")))
                                                       spec [change] [(write "r2")] outcome now)
                      nil (catch Exception e (:xtdb.error/code (ex-data e))))))
          (is (= [{:xt/id "r1"}] (xt/q node ["SELECT _id FROM requests"])))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
          (is (= [{:xt/id "command-2373"}] (xt/q node ["SELECT _id FROM gesso_commands"])))
          (with-redefs [xtdb/q-from (fn [& _] (throw lost))]
            (is (identical? lost (try (inspect retry) nil (catch Exception e e))))))))))

(deftest resolved-execution-orchestrates-recovery-without-resubmitting
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              principal (identity/principal "helper-1")
              invocation (fn [id execution-id arguments principal]
                           (let [command (protocol/command
                                          {:command-id (identity/command-id id)
                                           :execution-id (identity/execution-id execution-id)
                                           :operation :request/claim :arguments arguments
                                           :observed-basis fixture/rendered-basis})]
                             (assoc (select-keys command [:operation :command-id :execution-id :arguments])
                                    :command command :principal principal
                                    :ctx {:xtdb/connectable node
                                          publication/preparation-key
                                          (bind "owned-source" (assoc command :principal principal))})))
              first-call (invocation "command-2375" "first" {:request-id "r1"} principal)
              retry (invocation "command-2375" "retry" {:request-id "r1"} principal)
              absent (invocation "absent-2375" "first" {:request-id "r2"} principal)
              constructed (atom 0)
              projected (atom [])
              calls (atom 0)
              lost (ex-info "receipt lost" {:commit/status :unknown})
              execute xtdb/execute-tx-from!
              read-record xtdb/q-from
              options {:mode :submit
                       :may-disclose? (fn [binding record]
                                        (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
                       :construct! (fn [{:keys [arguments]}]
                                     (swap! constructed inc)
                                     (let [id (:request-id arguments)]
                                       {:spec {:purpose :gesso.live/refresh}
                                        :changes [{:topic :request :request/location-id id}]
                                        :domain-ops [[:put-docs :requests {:xt/id id :status :claimed}]]
                                        :outcome {:outcome :request/claimed}
                                        :now "2026-11-24T00:00:00Z"}))
                       :project! (fn [_ result]
                                   (swap! projected conj (:status result))
                                   {:resolution (if (= :replay (:status result)) :already-incorporated :confirmed)
                                    :outcome (get-in result [:outcome :outcome])
                                    :authoritative (protocol/authoritative
                                                    {:presence :present :basis fixture/rendered-basis
                                                     :projection {:request/id "r1" :request/status :claimed}})})}
              error-kind (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))
              run! #(publication/execute-resolved! %1 %2)]
          (is (= :invalid-resolved-execution-options (reason #(run! first-call (assoc options :mode :retry)))))
          (is (= :invalid-operation-context (error-kind #(run! (assoc first-call :execution-id (identity/execution-id "forged")) options))))
          (is (= :resolution-not-settleable (error-kind #(run! first-call (assoc options :mode :reconcile)))))
          (is (zero? @constructed))
          (is (empty? @projected))
          (with-redefs [xtdb/execute-tx-from! (fn [ctx ops]
                                               (swap! calls inc)
                                               (execute ctx ops nil)
                                               (throw lost))]
            (is (= :already-incorporated (:resolution (run! first-call options))))
            (is (= :already-incorporated (:resolution (run! retry options))))
            (is (= :already-incorporated (:resolution (run! retry (assoc options :mode :reconcile)))))
            (is (= :resolution-not-settleable
                   (error-kind #(run! (invocation "command-2375" "other" {:request-id "r2"} principal) options))))
            (is (= :resolution-not-settleable
                   (error-kind #(run! (invocation "command-2375" "other" {:request-id "r1"}
                                                   (identity/principal "other")) options)))))
          (is (= 1 @calls @constructed))
          (is (= [:replay :replay :replay] @projected))
          ;; Absent record after an execution failure preserves the original
          ;; exception. No retry or projection occurs.
          (with-redefs [xtdb/execute-tx-from! (fn [& _] (swap! calls inc) (throw lost))]
            (is (identical? lost (try (run! absent (assoc options :construct-after-abort!
                                                            (fn [_] (throw (AssertionError. "Unknown outcome retried")))))
                                      nil (catch Exception e e)))))
          (is (= 2 @calls @constructed))
          (let [reads (atom 0)
                failed-read (ex-info "recovery read failed" {})
                failed-execute (ex-info "execute failed" {})]
            (with-redefs [xtdb/q-from (fn [& args]
                                      (if (= 1 (swap! reads inc))
                                        (apply read-record args)
                                        (throw failed-read)))
                          xtdb/execute-tx-from! (fn [& _] (throw failed-execute))]
              (is (identical? failed-execute (try (run! absent options) nil (catch Exception e e))))
              (is (= [failed-read] (vec (.getSuppressed failed-execute))))
              (is (= 2 @reads))))
          (let [reads (atom 0) interrupted (InterruptedException. "execute interrupted")]
            (try
              (with-redefs [xtdb/q-from (fn [& args] (swap! reads inc) (apply read-record args))
                            xtdb/execute-tx-from! (fn [& _] (throw interrupted))]
                (is (= [interrupted true]
                       (try (run! absent options) nil
                            (catch Exception e [e (.isInterrupted (Thread/currentThread))]))))
                (is (= 1 @reads)))
              (finally (Thread/interrupted))))
          (is (= [:replay :replay :replay] @projected))
          (is (= [{:xt/id "r1"}] (xt/q node ["SELECT _id FROM requests"])))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
          (is (= [{:xt/id "command-2375"}] (xt/q node ["SELECT _id FROM gesso_commands"]))))))))

(deftest registered-publication-executor-requires-compatible-host-wiring
  (let [called (atom 0)
        build-options {:operation :request/claim
                       :submission-mode! (fn [_] (swap! called inc) :submit)
                       :construct! (fn [_] (throw (ex-info "unexpected construction" {})))
                       :may-disclose? (constantly false)
                       :project! (fn [& _] (throw (ex-info "unexpected projection" {})))}
        executor (publication/build-executor build-options)
        selected (assoc (config) :live-options {}
                        :assembled-publication subject-mapping
                        :assembled-operations {:authorize-request? (constantly true)})]
    (doseq [options [(dissoc build-options :submission-mode!)
                     (assoc build-options :operation :claim)
                     (assoc build-options :project! nil)
                     (assoc build-options :extra true)]]
      (is (= :invalid-publication-executor-config
             (reason #(publication/build-executor options)))))
    (is (= :missing-command-preparation (reason #(executor {:ctx {}}))))
    (with-assembly "/fragments/request-list/:id" "/streams/request-list/:id" executor
      (fn [assembly]
        (let [starts (atom 0)]
          (with-redefs [managed/start-biff-application! (fn [& _] (swap! starts inc))]
            (doseq [[cfg expected]
                    [[(config) :missing-publication-executor-profile]
                     [(dissoc selected :assembled-operations) :missing-publication-executor-profile]
                     [(-> selected (dissoc :assembled-publication) (assoc :live-options {:rules []}))
                      :missing-publication-executor-profile]
                     [(update-in selected [:assembled-publication :subject-keys] dissoc :request)
                      :unsupported-publication-executor-topics]]]
              (is (= expected (reason #(host/start! assembly identity cfg)))))
            (is (zero? @starts))))
        (let [command (protocol/command {:command-id (identity/command-id "v2376-command")
                                         :execution-id (identity/execution-id "v2376-execution")
                                         :operation :request/claim :arguments {:request-id "r1"}
                                         :observed-basis fixture/rendered-basis})
              bind (:bind-command (publication/profile! assembly subject-mapping))
              invocation (assoc (select-keys command [:operation :command-id :execution-id :arguments])
                                :command command :principal fixture/trusted-principal
                                :ctx {publication/preparation-key
                                      (bind "owned-source" (assoc command :principal (identity/principal "other")))})]
          (is (= :mismatched-command-binding (reason #(executor invocation)))))))
    (with-assembly "/fragments/request-list/:id" "/streams/request-list/:id"
      (publication/build-executor (assoc build-options :operation :request/cancel))
      (fn [assembly]
        (is (= :publication-executor-operation-mismatch
               (reason #(host/start! assembly identity selected))))))
    (is (zero? @called))))

(deftest assembled-publication-manifest-fences-persistent-host-restart
  (let [root (java.nio.file.Files/createTempDirectory
              "gesso-v2377-manifest-" (make-array java.nio.file.attribute.FileAttribute 0))
        compiled-var (ns-resolve 'gesso.live.application-preflight.fixture 'compiled-live)
        original @compiled-var
        selected (assoc (config) :xtdb-config {:log [:local {:path (str root "/log")}]
                                               :storage [:local {:path (str root "/storage")}]}
                        :live-options {} :assembled-publication subject-mapping)
        handler (fn [_] (g/html-response [:div "ready"]))
        start-error (fn [assembly cfg]
                      (try (let [started (host/start! assembly handler cfg)]
                             (host/stop! started) nil)
                           (catch clojure.lang.ExceptionInfo e e)))]
    (try
      (with-assembly
        (fn [assembly]
          (let [profile (publication/profile! assembly subject-mapping)
                compiled (get-in assembly [:operation-acquisition-assembly :acquisition-assembly :live-app])]
            (is (= (:manifest profile)
                   (binding [*print-length* 0 *print-level* 0 *print-meta* true]
                     (manifest/descriptor compiled (:subject-keys subject-mapping)))))
            (dotimes [_ 2]
              (let [started (host/start! assembly handler selected)]
                (try
                  (is (= [{:xt/id (:source-id selected) :profile-version 1
                           :descriptor (:manifest profile)}]
                         (xt/q (:xtdb/connectable started)
                               ["SELECT _id, profile_version, descriptor FROM gesso_publication_manifests"])))
                  (finally (host/stop! started)))))
            (let [starts (atom 0)]
              (with-redefs [live/create (fn [& _] (swap! starts inc)
                                          (throw (ex-info "unexpected Live startup" {})))]
                (let [error (start-error assembly (-> selected (dissoc :assembled-publication)
                                                       (assoc :live-options {:rules []})))]
                  (is (= :drained (:rollback/status (ex-data error))))
                  (is (= :publication-manifest-mismatch (:reason (ex-data (ex-cause error))))))
                (is (zero? @starts)))))))
      (with-redefs-fn
        {compiled-var #(model/compile-live-app
                        (assoc-in (original) [:scopes :request-list :topic] :fixture/revised-list))}
        (fn []
          (with-assembly
            (fn [assembly]
              (let [error (start-error assembly selected)]
                (is (= :drained (:rollback/status (ex-data error))))
                (is (= :publication-manifest-mismatch (:reason (ex-data (ex-cause error))))))))))
      ;; Failed incompatible startup did not overwrite the original manifest.
      (with-assembly
        (fn [assembly]
          (let [legacy "legacy-unclassified"
                id (random-uuid)
                started (host/start! assembly handler selected)]
            (try
              (let [node (:xtdb/connectable started)
                    plan (outbox/prepare! {:intent-ids [id] :source-id legacy
                                           :operation :request/claim :purpose :gesso.live/refresh}
                                          [{:topic :request :id "old-subject"}])]
                (xt/execute-tx node (conj (:guard-ops plan) (:tx-op plan))))
              (finally (host/stop! started)))
            ;; Another source's unclassified intent cannot be reinterpreted
            ;; merely by enabling the selected publication profile.
            (let [error (start-error assembly (assoc selected :source-id legacy))]
              (is (= :drained (:rollback/status (ex-data error))))
              (is (= :xtdb/assert-failed (:xtdb.error/code (ex-data (ex-cause error))))))
            (let [started (host/start! assembly handler selected)]
              (try
                (is (= [{:xt/id (:source-id selected)}]
                       (xt/q (:xtdb/connectable started) ["SELECT _id FROM gesso_publication_manifests"])))
                (is (= [{:xt/id id}]
                       (xt/q (:xtdb/connectable started) ["SELECT _id FROM gesso_live_outbox"])))
                (finally (host/stop! started)))))))
      (finally
        (doseq [entry (reverse (file-seq (.toFile root)))]
          (clojure.java.io/delete-file entry true))))))

(deftest command-recovery-reads-await-the-submitted-frontier
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              command {:operation :request/claim :principal (identity/principal "helper-1")
                       :command-id (identity/command-id "command-2392")
                       :arguments {:request-id "r1"}}
              ctx {:xtdb/connectable node publication/preparation-key (bind "owned-source" command)}
              outcome {:outcome :request/claimed}
              receipt (publication/transact-resolved!
                       ctx {:purpose :gesso.live/refresh}
                       [{:topic :request :request/location-id "r1"}]
                       [[:put-docs :requests {:xt/id "r1" :status :claimed}]] outcome "t0")
              id (:xt/id (first (:outbox/intents receipt)))
              record (first (xt/q node ["SELECT * FROM gesso_commands WHERE _id = ?" "command-2392"]))
              intent {:xt/id id :intent-type outbox/intent-type :intent-version outbox/intent-version
                      :source-id "owned-source" :operation-id :request/claim :command-id "command-2392"
                      :intent-status :pending}
              submitted {"xtdb" [42]}
              opts {:await-token (basis/->tx-basis-str submitted)}
              reads (atom [])
              statuses (atom [])
              inspect #(select-keys (publication/reconcile-command! ctx (constantly true)) [:status :outcome])
              observe #(publication/observe-command! ctx)]
          ;; Default reads are stale in this deterministic provider seam.
          ;; Both actual read paths must receive the submitted await token,
          ;; not merely a token applied to a separate readiness query.
          (with-redefs [xt/status (fn [n] (swap! statuses conj n)
                                    {:latest-submitted-msg-ids submitted :latest-processed-msg-ids {"xtdb" [3]}})
                        xtdb/q-from (fn [n query & [actual]]
                                      (swap! reads conj [n query actual])
                                      (if (= opts actual)
                                        (if (= "command-2392" (second query)) [record] [intent]) []))]
            (is (= {:status :replay :outcome outcome} (inspect)))
            (is (= {:outcome :prior-intent-present :intent-id id :publication-status :pending} (observe)))
            (is (= 2 (count @reads) (count @statuses)))
            (is (every? #(identical? node %) @statuses))
            (is (every? #(and (identical? node (first %)) (= opts (nth % 2))) @reads)))
          (reset! reads [])
          (with-redefs [xt/status (constantly {:latest-submitted-msg-ids nil})
                        xtdb/q-from (fn [& args] (swap! reads conj args) [])]
            (is (= :invalid-submitted-log-frontier (reason inspect)))
            (is (= :invalid-submitted-log-frontier (reason observe)))
            (is (empty? @reads)))
          (let [interrupted (InterruptedException. "frontier interrupted")]
            (try
              (with-redefs [xt/status (fn [_] (throw interrupted))]
                (is (= [interrupted true]
                       (try (inspect) nil (catch Exception e [e (.isInterrupted (Thread/currentThread))])))))
              (finally (Thread/interrupted)))))))))

(deftest abort-policy-is-bounded-by-physical-guards-and-journal-identity
  (with-assembly
    (fn [assembly]
      (with-open [node (xtn/start-node {})]
        (let [bind (:bind-command (publication/profile! assembly subject-mapping))
              principal (identity/principal "helper-1")
              command (protocol/command {:command-id (identity/command-id "v2414-command")
                                         :execution-id (identity/execution-id "v2414-execution")
                                         :operation :request/claim :arguments {:request-id "r1"}
                                         :observed-basis fixture/rendered-basis})
              ctx {:xtdb/connectable node publication/preparation-key
                   (bind "owned-source" (assoc command :principal principal))}
              invocation (assoc (select-keys command [:operation :command-id :execution-id :arguments])
                                :command command :principal principal :ctx ctx)
              version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
              row {:xt/id "r1" :status :done :revision 2 :created-at "t0" :updated-at "t2"}
              stale (assoc row :revision 1)
              refusal {:outcome :request/not-claimable :reason :request-closed}
              construction {:spec nil :model-plan {:guards [(model-command/guard :requests row version)]}
                            :outcome refusal :now "2026-11-24T00:00:02Z"}
              initial (assoc construction :model-plan {:guards [(model-command/guard :requests stale version)]})
              calls (atom 0) policies (atom 0) projected (atom [])
              execute xtdb/execute-tx-from!
              options {:mode :submit :construct! (constantly initial) :may-disclose? (constantly true)
                       :project! (fn [_ result]
                                   (swap! projected conj (:status result))
                                   {:resolution :rejected :outcome (get-in result [:outcome :outcome])
                                    :authoritative (protocol/authoritative
                                                    {:presence :present :basis fixture/rendered-basis
                                                     :projection {:request/id "r1" :request/status :done}})})}
              run! (fn [answer]
                     (publication/execute-resolved!
                      invocation (assoc options :construct-after-abort!
                                        (fn [_] (swap! policies inc) answer))))
              failure (fn [f] (try (f) nil (catch Exception e e)))]
          (xt/execute-tx node [[:put-docs :requests row]])
          (with-redefs [xtdb/execute-tx-from! (fn [ctx ops & [opts]]
                                               (swap! calls inc)
                                               (execute ctx ops opts))]
            ;; Every case begins with a real failed XTDB version guard.
            (doseq [answer [nil (assoc construction :model-plan {})
                            (assoc construction :spec {:purpose :gesso.live/refresh})
                            (assoc-in construction [:model-plan :commands]
                              [(model-command/update-command :requests :claim row
                                (assoc row :status :claimed :revision 3 :updated-at "t3") version)])]]
              (let [n @calls p @policies e (failure #(run! answer))]
                (is (instance? Exception e))
                (is (= (if (nil? answer) :xtdb/assert-failed :gesso.live.outbox/invalid-model-publication)
                       (or (:xtdb.error/code (ex-data e)) (:error/type (ex-data e)))))
                (is (= (inc n) @calls))
                (is (= (inc p) @policies))))
            ;; Capture interruption before assertion reporting can consume it.
            (let [n @calls p @policies
                  observed (try
                             (publication/execute-resolved!
                              invocation (assoc options :construct!
                                                (fn [_]
                                                  (.interrupt (Thread/currentThread))
                                                  construction)))
                             nil
                             (catch Exception e
                               [(instance? InterruptedException e)
                                (.isInterrupted (Thread/currentThread))])
                             (finally (Thread/interrupted)))]
              (is (= [true true] observed))
              (is (= n @calls))
              (is (= p @policies)))
            ;; A real first abort permits policy evaluation, but cancellation
            ;; during that callback must prevent the second submission.
            (let [n @calls p @policies
                  observed (try
                             (publication/execute-resolved!
                              invocation (assoc options :construct-after-abort!
                                                (fn [_]
                                                  (swap! policies inc)
                                                  (.interrupt (Thread/currentThread))
                                                  construction)))
                             nil
                             (catch Exception e
                               [(instance? InterruptedException e)
                                (.isInterrupted (Thread/currentThread))])
                             (finally (Thread/interrupted)))]
              (is (= [true true] observed))
              (is (= (inc n) @calls))
              (is (= (inc p) @policies)))
            ;; A second physical guard abort must not start a third attempt.
            (let [n @calls p @policies e (failure #(run! initial))]
              (is (= :xtdb/assert-failed (:xtdb.error/code (ex-data e))))
              (is (= (+ 2 n) @calls))
              (is (= (inc p) @policies)))
            (is (empty? @projected))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_commands"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
          ;; Simulate another authorized writer winning after the unresolved
          ;; read, before the guarded recovery transaction reaches XTDB.
          (reset! calls 0)
          (with-redefs [xtdb/execute-tx-from!
                        (fn [ctx ops & [opts]]
                          (when (= 2 (swap! calls inc))
                            (with-redefs [xtdb/execute-tx-from! execute]
                              (publication/transact-model-resolved!
                               ctx nil (:model-plan construction) refusal (:now construction))))
                          (execute ctx ops opts))]
            (is (= :rejected (:resolution (run! construction))))
            (is (= 2 @calls))
            (is (= [:replay] @projected)))
          (is (= {:status :replay :outcome refusal}
                 (select-keys (publication/reconcile-command! ctx (constantly true)) [:status :outcome])))
          (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
          (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
          (is (= [{:status :done :revision 2}]
                 (xt/q node ["SELECT status, revision FROM requests WHERE _id = ?" "r1"]))))))))
