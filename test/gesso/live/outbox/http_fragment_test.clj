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
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.invalidation :as invalidation]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-sse :as sse]
            [gesso.live.outbox.http-operation :as operation-http]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.progression :as progression]
            [ring.util.codec :as codec]
            [gesso.live.source :as source]
            [xtdb.api :as xt]
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
        (let [ops (cond-> (fixture/standard-operations)
                    execute! (assoc :request/claim
                                    (fixture/trusted-operation :request/claim
                                     {:published-change-topics #{:request} :execute! execute!})))]
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

(deftest assembled-streams-share-capacity-and-use-original-model-policy
  (let [seen (atom []) queries (atom []) invocations (atom []) fallback (atom 0)
        prepared-intents (atom nil)
        replay? (atom false)
        projections (atom [])
        command (protocol/command {:command-id (identity/command-id "v2366-command")
                                   :execution-id (identity/execution-id "v2366-execution")
                                   :operation :request/claim :arguments {:request-id "r1"}
                                   :observed-basis fixture/rendered-basis})
        form (fn [command]
               (codec/form-encode {operation-http/command-parameter
                                   (pr-str (protocol/command->wire command))}))
        execute! (fn [{:keys [ctx arguments] :as invocation}]
                   (swap! invocations conj invocation)
                   (if @replay?
                     (publication/reconcile-settlement!
                      invocation (fn [binding record]
                                   (= (:principal binding) (get-in record [:gesso.command/binding :principal])))
                      (fn [{:keys [ctx arguments]} result]
                        (swap! projections conj result)
                        (let [id (:request-id arguments)
                              basis (xtdb/latest-completed-basis ctx)
                              row (first (xtdb/q-from ctx ["SELECT status FROM requests WHERE _id = ?" id]
                                                       {:snapshot-token (xtdb/basis-snapshot-token basis)}))]
                          {:resolution :already-incorporated
                           :outcome (get-in result [:outcome :outcome])
                           :authoritative (protocol/authoritative
                                           {:presence :present :basis basis
                                            :projection {:request/id id :request/status (:status row)}})})))
                     (let [id (:request-id arguments)
                           receipt (publication/transact-resolved!
                                    ctx {:purpose :gesso.live/refresh}
                                    [{:topic :request :request/location-id id :change/kind :updated}]
                                    [[:put-docs :requests {:xt/id id :status :claimed}]]
                                    {:outcome :request/claimed} "2026-11-24T00:00:00Z")
                           _ (reset! prepared-intents (:outbox/intents receipt))
                           tx (:tx-result receipt)]
                       {:resolution :confirmed :outcome :request/claimed
                        :authoritative (protocol/authoritative
                                        {:presence :present
                                         :basis {:tx-id (:tx-id tx) :system-time (str (:system-time tx))}
                                         :projection {:request/id id :request/status :claimed}})})))
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
      (with-assembly "/fragments/request-list/:id" "/streams/request-list/:id" execute!
        (fn [assembly]
          (let [started (host/start! assembly
                                     (fn [_] (swap! fallback inc) (g/html-response [:div "fallback"]))
                                     (assoc (config) :http-jdk {}
                                            :live-options {}
                                            :assembled-publication subject-mapping
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
                      (let [response (post! headers)]
                        (is (= 200 (.statusCode response)))
                        (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))
                              settlement (protocol/wire->settlement
                                          (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))]
                          (is (= :confirmed (:resolution settlement)))
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
                  (is (= [{:status :claimed}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"])))
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
                                     (assoc invocation :arguments {:request-id "other"})
                                     (constantly true) forbidden))))
                    (is (= :invalid-resolution
                           (error-kind #(publication/reconcile-settlement!
                                         invocation (constantly true) (constantly {:resolution :failed}))))))
                  ;; A later domain mutation changes the authoritative state.
                  ;; Replay must report that state, not the original projection.
                  (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :status :done}]])
                  (reset! replay? true)
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
                    (is (= 1 (count @projections)))
                    (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_commands"]))))
                    (is (= 1 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                    (is (= [{:status :done}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"]))))
                  (is (zero? @fallback))
                  (host/stop! started)
                  (is ((:drained? status)))
                  (is ((:drained? listener)))
                  (is ((:drained? (get started host/recovery-status-key))))))
              (finally (host/stop! started) (.close client)))))))))

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
