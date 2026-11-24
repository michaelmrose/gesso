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
            [gesso.live.outbox :as outbox]
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
        intent-id (random-uuid)
        command (protocol/command {:command-id (identity/command-id "v2366-command")
                                   :execution-id (identity/execution-id "v2366-execution")
                                   :operation :request/claim :arguments {:request-id "r1"}
                                   :observed-basis fixture/rendered-basis})
        form (codec/form-encode {operation-http/command-parameter
                                 (pr-str (protocol/command->wire command))})
        execute! (fn [{:keys [ctx arguments] :as invocation}]
                   (swap! invocations conj invocation)
                   (let [id (:request-id arguments)
                         plan (outbox/prepare! {:intent-ids [intent-id]
                                                 :source-id (get ctx managed/source-id-key)
                                                 :operation :request/claim :purpose :gesso.live/refresh}
                                                [{:topic :request :id id :change/kind :updated}])
                         tx (xt/execute-tx (:xtdb/connectable ctx)
                                           (into (vec (:guard-ops plan))
                                                 [[:put-docs :requests {:xt/id id :status :claimed}]
                                                  (:tx-op plan)]))]
                     {:resolution :confirmed :outcome :request/claimed
                      :authoritative (protocol/authoritative
                                      {:presence :present
                                       :basis {:tx-id (:tx-id tx) :system-time (str (:system-time tx))}
                                       :projection {:request/id id :request/status :claimed}})}))
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
                post! (fn [headers]
                        (let [builder (HttpRequest/newBuilder (URI/create (str (:base-url listener) "/operations/request/claim")))]
                          (.timeout builder (Duration/ofSeconds 10))
                          (.header builder "content-type" "application/x-www-form-urlencoded")
                          (doseq [[k v] headers] (.header builder k v))
                          (.POST builder (HttpRequest$BodyPublishers/ofString form))
                          (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))
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
                  (is (= [{:xt/id intent-id}] (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id])))
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
