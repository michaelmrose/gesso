(ns gesso.live.outbox.claim-physical-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [gesso.live.source :as live-source]
            [gesso.live.core :as live]
            [gesso.live.progression :as progression]
            [gesso.live.progression.http :as progression-http]
            [clojure.test :as t :refer [deftest is]]
            [gesso.live.consistency.xtdb :as xtdb]
            [clojure.xml :as xml]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.assurance.example.physical-single-node-session :as session]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.claim-race-fixture :refer [mapping version command]]
            [gesso.live.outbox.claim-browser-fixture :as browser]
            [gesso.live.outbox.claim-semantic-fixture :as semantics]
            [gesso.assurance.observed-correspondence :as correspondence]
            [gesso.model.inductive-plan :as inductive-plan]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.tx :as tx]
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
  (doseq [bound [(semantics/bound-inductive-source!)]
          recovery-mode [:valid :wrong-outcome :missing-guard :domain-write]]
   (session/with-session-authority!
    #{:fixture/helper-1 :fixture/helper-2 :fixture/reader}
    (fn [{:keys [source issue! revoke!]}]
     (let [ready (CountDownLatch. 2)
        recovery-trace (atom [])
        initial-state (atom nil)
        recovery-state (atom nil)
        comparisons (atom [])
        record-step! (fn [helper alternative command-id before after replay-counts]
                       (swap! comparisons conj {:helper helper :alternative alternative :command-id command-id
                                                :before before :after after :replay-counts replay-counts}))
        submissions (atom [])
        writers (atom [])
        policy-admissions (atom [])
        preparations (semantics/build-prepared-admission! bound)
        checked-candidates (atom {})
        ;; Independent diagnostic: acceptance writes have the same mapped
        ;; document targets for either helper. Refusal writes only a journal
        ;; row and therefore has no document-frame prefix. Keep the exact
        ;; original footprint checker unchanged.
        base-ops! (fn [ops]
                    (let [prefix (mapv tx/format-assertion
                                       (frame/prestate-assertions!
                                        (plan/check-target-mapping! (semantics/source "helper-1" :accept)
                                                                    semantics/target-map)
                                        (:document-rows @initial-state)))
                          accepting? (some #(and (= :put-docs (first %)) (= :requests (second %))) ops)]
                      (if accepting?
                        (do (when-not (and (<= (count prefix) (count ops))
                                           (= prefix (subvec ops 0 (count prefix))))
                              (throw (ex-info "Incorrect observed Claim frame prefix" {})))
                            (subvec ops (count prefix)))
                        ops)))
        classify! (fn [seed ops]
                    (semantics/writer-kind! seed "v2480-claim" "v2480"
                     (if (some #(and (= :put-docs (first %)) (= :gesso_commands (second %))) ops)
                       (base-ops! ops) ops)))
        execute-adapter xtdb/execute-tx!
        execute xtdb/execute-tx-from!
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
        seed [[:put-docs :requests before]
              [:put-docs :memberships (assoc member :xt/id "helper-1") (assoc member :xt/id "helper-2")]]
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
                   :mapped-admission (:issued preparations)
                   :single-command-per-principal? true
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
                                   (let [snapshot (semantics/snapshot-at-frontier! ctx)]
                                     ((:prepare! preparations) (semantics/alternative-id helper :accept)
                                      (select-keys invocation [:command-id :principal :operation :arguments]) snapshot {}))
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
                   (fn [{:keys [ctx command-id] :as invocation}]
                     (swap! recovery-trace conj [:construct command-id])
                     (let [snapshot (semantics/snapshot-at-frontier! ctx)]
                       (reset! recovery-state snapshot)
                       ((:prepare! preparations) (semantics/alternative-id (actor invocation) :refuse)
                        (select-keys invocation [:command-id :principal :operation :arguments]) snapshot {}))
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
                :mapped-writer-policy
                {:admission (:issued preparations)
                 :initializer seed
                 :observe! (fn [{:keys [candidate] :as kind}]
                             (when candidate
                               (swap! checked-candidates assoc (dissoc kind :kind :candidate) candidate))
                             (swap! policy-admissions conj (cond-> (dissoc kind :candidate)
                                                           candidate (assoc :kind :claim))))}
                :worker-options {:worker-id "v2480" :poll-ms 20} :live-options {}
                :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request} :request_assignments #{:request-assignment}}
                                              :require-derived-model? true
                                              :operation-model-entities {:request/claim #{:requests :request_assignments}})
                :http-jdk {} :assembled-operations {:authorize-request? (comp some? principal-fn)}
                :assembled-fragments {:resolve-id resolve-id}
                :assembled-sse {:resolve-id resolve-id
                                :session-authorized? read-authorized?
                                :sse-options {:keepalive-ms 50} :max-connections (if *claim-browser-observer?* 6 4)}}]
    (let [reason (fn [config] (try (publication/build-executor config) nil
                                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
      (is (= :invalid-abort-contract (reason (dissoc opts :construct-after-abort!))))
      (is (= :invalid-abort-contract (reason (update opts :abort-contract dissoc :outcome-contract!))))
      (is (= :abort-contract-requires-guarded-journal-only
             (reason (assoc-in opts [:abort-contract :construction-contract :allowed-write-tables]
                               #{:requests :gesso_commands})))))
    (with-redefs [xtdb/execute-tx!
                  (fn [node ops & [options]]
                    (let [kind (try (classify! seed ops)
                                    (catch Exception e
                                      (swap! writers conj {:kind :unclassified :ops ops})
                                      (throw e)))]
                      (try
                        (let [result (execute-adapter node ops options)]
                          (swap! writers conj (assoc kind :result :committed :ops ops))
                          result)
                        (catch Exception e
                          (swap! writers conj (assoc kind :result (:xtdb.error/code (ex-data e)) :ops ops))
                          (throw e)))))
                  xtdb/execute-tx-from!
                  (fn [ctx ops & [options]]
                    (if (contains? ctx publication/preparation-key)
                      (let [base (base-ops! ops)
                            entry {:ops base :framed-ops ops :identity (semantics/submitted-alternative! base)}]
                        (try
                          (let [result (execute ctx ops options)]
                            (swap! submissions conj (assoc entry :result :committed
                                                           :commit-basis (xtdb/tx-result-basis (:tx-result result))))
                            result)
                          (catch Exception e
                            (swap! submissions conj (assoc entry :result (:xtdb.error/code (ex-data e))))
                            (throw e))))
                      (execute ctx ops options)))
                  fixture/allow? (fn [ctx id] (and (= "l1" id) (read-authorized? ctx)))
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
           (reset! initial-state (semantics/snapshot-at-frontier! node))
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
                 results (if-let [opened @observer]
                           (browser/compete! opened (get-in started [host/listener-key :base-url]) credentials
                                             #(= % (:subscription-count (live-source/stats (get-in started [:gesso.live/system :source])))))
                           (browser/http-race! invoke))]
             (let [winner-index (if (= :confirmed (get-in (first results) [:settlement :resolution])) 0 1)
                   winner (nth results winner-index) loser (nth results (- 1 winner-index))
                   actors (browser/race-actors (some? @observer) results)
                   [session id helper] (nth actors winner-index)
                   [loser-session loser-id loser-helper] (nth actors (- 1 winner-index))
                   valid? (= :valid recovery-mode)]
               (is (= (set (map (fn [[_ id helper]] [id helper before]) actors)) (set @observed)))
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
               (let [final-state (semantics/snapshot-at-frontier! node)
                     initial (:state @initial-state)
                     recovered (:state @recovery-state)
                     final (:state final-state)
                     accepted (semantics/observe! helper :accept initial recovered)
                     refused (semantics/observe! loser-helper :refuse recovered final)]
                 (record-step! helper :accept id @initial-state @recovery-state nil)
                 (when valid? (record-step! loser-helper :refuse loser-id @recovery-state final-state nil))
                 (is (:matches? accepted))
                 (is (neg? (xtdb/compare-bases (:basis @initial-state) (:basis @recovery-state))))
                 (is (if valid? (neg? (xtdb/compare-bases (:basis @recovery-state) (:basis final-state)))
                         (not (pos? (xtdb/compare-bases (:basis @recovery-state) (:basis final-state))))))
                 (is (= valid? (:matches? refused)))
                 (if valid?
                   (do (is (not (:matches? (semantics/observe! loser-helper :refuse recovered recovered))))
                       (is (not (:matches? (semantics/observe! loser-helper :refuse recovered (assoc final :assignment/helper loser-helper)))))
                       (is (not (:matches? (semantics/observe! helper :accept recovered final)))))
                   (is (= recovered final))))
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
               (let [before-snapshot (semantics/snapshot-at-frontier! node)
                     before-count (count @submissions)
                     replay (invoke session id "winner-replay")
                     after-count (count @submissions)
                     after-snapshot (semantics/snapshot-at-frontier! node)
                     before (:state before-snapshot) after (:state after-snapshot)]
                 (record-step! helper :replay-accepted id before-snapshot after-snapshot [before-count after-count])
                 (is (:matches? (semantics/observe! helper :replay-accepted before after)))
                 (is (not (:matches? (semantics/observe! helper :replay-rejected before after))))
                 (is (not (:matches? (semantics/observe! helper :replay-accepted before (assoc after :request/revision 2)))))
                 (is (= :already-incorporated (get-in replay [:settlement :resolution])))
                 (is (= {:status :claimed :helper helper} (get-in replay [:settlement :authoritative :projection])))
                 (is (= 2 (count @observed)))
                 (is (= [id :replay] (last @projected)))
                 (when valid?
                   (let [before-count (count @submissions)
                         replay (invoke loser-session loser-id "loser-replay")
                         after-count (count @submissions)
                         refused-snapshot (semantics/snapshot-at-frontier! node)
                         refused-after (:state refused-snapshot)]
                     (record-step! loser-helper :replay-rejected loser-id after-snapshot refused-snapshot [before-count after-count])
                     (is (:matches? (semantics/observe! loser-helper :replay-rejected after refused-after)))
                     (is (not (:matches? (semantics/observe! loser-helper :replay-accepted after refused-after))))
                     (is (= :rejected (get-in replay [:settlement :resolution])))
                     (is (= :request/not-claimable (get-in replay [:settlement :outcome])))
                     (is (= {:status :claimed :helper helper} (get-in replay [:settlement :authoritative :projection])))
                     (is (= [loser-id :replay] (last @projected)))
                     (is (= 2 (count @recovery-trace)))
                     (is (= 2 (count @observed)))))
                 (is (= 2 (count (xt/q node ["SELECT _id FROM gesso_live_outbox"]))))
                 ;; Both replays have returned without any new physical submission.
                 (let [expected (cond-> #{{:identity {:alternative (semantics/alternative-id helper :accept) :command-id id}
                                            :result :committed}
                                           {:identity {:alternative (semantics/alternative-id loser-helper :accept) :command-id loser-id}
                                            :result :xtdb/assert-failed}}
                                  valid? (conj {:identity {:alternative (semantics/alternative-id loser-helper :refuse) :command-id loser-id}
                                                :result :committed}))
                       accepted (:ops (first (filter #(= {:alternative (semantics/alternative-id helper :accept) :command-id id}
                                                         (:identity %)) @submissions)))
                       refused? (fn [ops] (try (semantics/submitted-alternative! ops) false
                                               (catch clojure.lang.ExceptionInfo _ true)))]
                   (is (= (count expected) (count @submissions)))
                   (is (= expected (set (map #(dissoc % :ops :framed-ops :commit-basis) @submissions))))
                   (is (= (set (map :identity expected)) (set (keys @checked-candidates))))
                   (is (every? #(= :checked-supplied-complete-rows (:document-frame %)) (vals @checked-candidates)))
                   (is (every? #(= :exact-generated-prefix (:frame-prefix %)) (vals @checked-candidates)))
                   (doseq [{:keys [identity ops framed-ops]} @submissions]
                     (is (= ops (base-ops! framed-ops)))
                     (is (= (if (= :accept (keyword (name (:alternative identity)))) 3 0)
                            (- (count framed-ops) (count ops)))))
                   (doseq [{:keys [identity]} expected]
                     (is (= (:state (if (= :accept (keyword (name (:alternative identity))))
                                     @initial-state @recovery-state))
                            (:before (get @checked-candidates identity)))))
                   (is (refused? (conj accepted [:put-docs :memberships {:xt/id helper :active false}])))
                   (is (refused? (conj accepted ["DELETE FROM requests"])))
                   (is (refused? (filterv #(not= :request_assignments (second %)) accepted)))
                   (is (refused? (conj accepted (first (filter #(= :gesso_commands (second %)) accepted))))))
                 (let [deadline (+ (System/nanoTime) 10000000000)]
                   (loop []
                     (when (and (< (System/nanoTime) deadline)
                                (< (count (filter #(and (= :worker (:kind %))
                                                       (= :dispatch-accepted (:status %))
                                                       (= :committed (:result %))) @writers)) 2))
                       (Thread/sleep 20)
                       (recur))))
                 (let [entries @writers
                       claims (filter #(= :claim (:kind %)) entries)
                       workers (filter #(= :worker (:kind %)) entries)
                       intent-ids (set (map :xt/id (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
                       classify #(semantics/writer-kind! seed "v2480-claim" "v2480" %)
                       refused? (fn [ops] (try (classify ops) false (catch clojure.lang.ExceptionInfo _ true)))
                       worker-ops (:ops (first workers))]
                   (is (= #{:initializer :manifest :claim :worker} (set (map :kind entries))))
                   (is (= 1 (count (filter #(= :initializer (:kind %)) entries))))
                   (is (= 1 (count (filter #(= :manifest (:kind %)) entries))))
                   (is (= (count @submissions) (count claims)))
                   (is (= (frequencies (map #(select-keys % [:alternative :command-id :result]) claims))
                          (frequencies (map #(assoc (:identity %) :result (:result %)) @submissions))))
                   (is (= (set (for [intent-id intent-ids status [:leased :dispatch-accepted]] [intent-id status]))
                          (set (map (juxt :intent-id :status) workers))))
                   (is (= 4 (count workers)))
                   (is (every? #(and (= id (:command-id %)) (= :committed (:result %))) workers))
                   (is (every? #(= :dispatch-accepted (:gesso.live.outbox/status %))
                               (xt/q node ["SELECT * FROM gesso_live_outbox"])))
                   (is (refused? (conj worker-ops [:put-docs :memberships {:xt/id helper :active false}])))
                   (is (refused? (conj worker-ops ["DELETE FROM requests"])))
                   (is (refused? (assoc-in worker-ops [(dec (count worker-ops)) 2 :gesso.live.outbox/source-id] "other")))
                   (is (refused? (assoc-in seed [0 2 :status] :claimed)))
                   (is (= (frequencies @policy-admissions)
                          (frequencies (map #(dissoc % :result :ops) entries))))
                   ;; Direct adapter callers cannot repeat initialization or
                   ;; use the asynchronous API to bypass the installed policy.
                   (let [before (semantics/snapshot-at-frontier! node)
                         denied (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
                     (is (= :submission-policy-refused (denied #(execute-adapter node seed nil))))
                     (is (= :submission-policy-refused (denied #(xtdb/submit-tx! node seed))))
                     (is (= :submission-policy-refused
                            (denied #(execute-adapter node [[:put-docs :requests (assoc (get-in seed [0 2]) :status :claimed)]] nil))))
                     (let [accepted (:ops (first (filter #(and (= :claim (:kind %)) (= :committed (:result %))
                                                                  (= id (:command-id %))) entries)))
                           guards (filterv #(string? (first %)) accepted)
                           puts (filterv #(= :put-docs (first %)) accepted)
                           alternative (semantics/alternative-id helper :accept)
                           binding (:gesso.command/binding (get-in before [:records id]))
                           original-admissions @policy-admissions
                           original-checks @checked-candidates]
                       ;; Even the original valid vector needs a live handoff.
                       (is (= :submission-policy-refused (denied #(execute-adapter node accepted nil))))
                       ;; Direct callers cannot consume even a genuine preparation.
                       ((:prepare! preparations) alternative binding before {})
                       ;; Neither malformed nor exact envelopes may consume it.
                       (is (= :submission-policy-refused (denied #(execute-adapter node (into puts guards) nil))))
                       (is ((:pending? preparations) alternative id))
                       (is (= :submission-policy-refused (denied #(execute-adapter node (pop accepted) nil))))
                       (is ((:pending? preparations) alternative id))
                       (is (= :submission-policy-refused (denied #(execute-adapter node accepted nil))))
                       (is ((:pending? preparations) alternative id))
                       ((:discard! preparations) alternative id)
                       (is (= original-admissions @policy-admissions))
                       (is (= original-checks @checked-candidates))
                       ;; Even the original open preparation cannot grant a direct
                       ;; submission, whether the vector is framed or unframed.
                       ((:prepare! preparations) alternative binding @initial-state {})
                       (is (= :submission-policy-refused (denied #(execute-adapter node accepted nil))))
                       (is (= :submission-policy-refused (denied #(execute-adapter node (base-ops! accepted) nil))))
                       (is (= :submission-policy-refused (denied #(xtdb/submit-tx! node accepted))))
                       (is ((:pending? preparations) alternative id))
                       ((:discard! preparations) alternative id)
                       (doseq [mutate [(fn [doc] (assoc doc :unexpected true))
                                       (fn [doc] (dissoc doc :created-at))]]
                         ((:prepare! preparations) alternative binding @initial-state {})
                         (is (= :submission-policy-refused
                                (denied #(execute-adapter node
                                          (mapv (fn [op] (if (= :requests (second op))
                                                          (update op 2 mutate) op)) accepted) nil))))
                         (is ((:pending? preparations) alternative id))
                         ((:discard! preparations) alternative id))
                       (is (= original-admissions @policy-admissions))
                       ;; A well-shaped write is not enough without every source-derived predicate.
                       (doseq [guard guards]
                         (is (= :submission-policy-refused
                                (denied #(execute-adapter node (filterv (partial not= guard) accepted) nil)))))
                       (is (= :submission-policy-refused
                              (denied #(execute-adapter node (into puts guards) nil))))
                       (is (= :submission-policy-refused
                              (denied #(execute-adapter node (mapv (fn [op] (if (= :requests (second op))
                                                                            (assoc-in op [2 :status] :open) op)) accepted) nil)))))
                     (is (= before (semantics/snapshot-at-frontier! node)))))
                 (when valid?
                   (let [issued (inductive-plan/require-current-mapped-admission!
                                 (:issued preparations) (semantics/mapped-admission-source bound))
                         correlated (semantics/correlate-history! @comparisons @submissions)
                         report (correspondence/check-bound-declared-observations!
                                 {:inductive-source bound
                                  :observations (mapv :observation correlated)})
                         rejected? (fn [steps attempts]
                                     (try (semantics/correlate-history! steps attempts) false
                                          (catch clojure.lang.ExceptionInfo _ true)))
                         committed (set (map (comp :alternative :identity)
                                             (filter #(= :committed (:result %)) @submissions)))
                         replay-ids #{(semantics/alternative-id helper :replay-accepted)
                                      (semantics/alternative-id loser-helper :replay-rejected)}]
                     (is (= :issued-local-preparation-registry (:admission issued)))
                     (is (= (:catalog-fingerprint issued) (get-in report [:induction-ledger :catalog-fingerprint])))
                     (is (= (:induction-fingerprint issued) (get-in report [:induction-ledger :induction-fingerprint])))
                     (is (= :none (:transaction-authority issued)))
                     (is (= [id loser-id id loser-id] (mapv :command-id correlated)))
                     (is (= [:committed-in-window :committed-in-window :no-new-submission :no-new-submission]
                            (mapv :submission correlated)))
                     (is (= (mapv :commit-basis (take 2 correlated)) (mapv :commit-basis (drop 2 correlated))))
                     (is (rejected? (assoc-in @comparisons [0 :command-id] loser-id) @submissions))
                     (is (rejected? (assoc-in @comparisons [0 :after :basis] (get-in @comparisons [0 :before :basis])) @submissions))
                     (is (rejected? (assoc-in @comparisons [2 :replay-counts] [0 1]) @submissions))
                     (is (rejected? (update-in @comparisons [2 :after :records] dissoc id) @submissions))
                     (is (rejected? @comparisons (pop @submissions)))
                     (is (= :both-live-abstract-handles-and-exact-observations-rechecked (:checking report)))
                     (is (= :source-bound-conditional-abstract-safety (get-in report [:bound-inductive-report :verdict])))
                     (is (= 8 (count (get-in report [:bound-inductive-report :declared-operation-ids]))))
                     (is (false? (get-in report [:bound-inductive-report :full-success?])))
                     (is (= :not-issued (:certificate report)))
                     (is (:all-supplied-observations-match? report))
                     (is (empty? (:divergent-operation-ids report)))
                     (is (= (into committed replay-ids) (:observed-operation-ids report)))
                     (is (= 4 (count (:unobserved-operation-ids report))))
                     (is (= 4 (count (:observations report))))
                     (is (get-in report [:induction-ledger :all-declared-invariants-conditionally-inductive?]))
                     (is (= :all-original-obligations-remain-open
                            (get-in report [:induction-ledger :original-obligation-status])))
                     (is (= :not-established (:installed-executor-correspondence report)))
                     (is (= :none (:authority report))))
                   (revoke! (get-in credentials [session :session-id]))
                   (is (= 403 (:status (request! (headers-for session) id "revoked-replay" true))))
                   (is (= 2 (count @observed)))))))
           (finally
             (when-let [opened @observer] (browser/close! opened))
             (doseq [input @streams] (.close input))
             (host/stop! started)))))))))))))

(deftest claim-semantic-snapshot-does-not-mix-completed-bases
  (with-open [node (xtn/start-node {})]
    (xt/execute-tx node [[:put-docs :requests {:xt/id "r1" :status :open :revision 0}]
                         [:put-docs :memberships {:xt/id "helper-1" :active true}
                          {:xt/id "helper-2" :active true}]])
    (let [q xtdb/q-from
          pinned-opts (atom [])
          observed (with-redefs [xtdb/q-from
                                  (fn [n query opts]
                                    (let [rows (q n query opts)]
                                      (when-not (= query ["SELECT 1 AS ready"])
                                        (swap! pinned-opts conj opts))
                                      (when (= query ["SELECT * FROM requests"])
                                        ;; Commit between projection queries, not before basis capture.
                                        (xt/execute-tx node
                                          [[:put-docs :requests {:xt/id "r1" :status :claimed :revision 1}]
                                           [:put-docs :request_assignments {:xt/id "primary-r1" :request-id "r1" :helper "helper-1"}]
                                           [:put-docs :memberships {:xt/id "helper-2" :active false}]]))
                                      rows))]
                     (semantics/snapshot-at-frontier! node))
          latest (semantics/snapshot-at-frontier! node)]
      (is (= 4 (count @pinned-opts)))
      (is (= 1 (count (set @pinned-opts))))
      (is (= (xtdb/basis-snapshot-token (:basis observed)) (:snapshot-token (first @pinned-opts))))
      (is (instance? java.time.Instant (:current-time (first @pinned-opts))))
      (is (= {:request/status :open :request/revision 0 :assignment/helper nil
              :journal/one nil :journal/two nil :membership/one true :membership/two true}
             (:state observed)))
      (is (= {:request/status :claimed :request/revision 1 :assignment/helper "helper-1"
              :journal/one nil :journal/two nil :membership/one true :membership/two false}
             (:state latest)))
      (is (neg? (xtdb/compare-bases (:basis observed) (:basis latest)))))))

(defn -main [& args]
  (when (seq args) (throw (ex-info "Unexpected Claim browser gate arguments" {})))
  (let [result (binding [*claim-browser-observer?* true
                         t/*report-counters* (ref t/*initial-report-counters*)]
                 (t/test-vars [#'distinct-claim-commands-contend-for-one-primary-assignment])
                 @t/*report-counters*)]
    (println "GESSO CLAIM CHROMIUM RACE GATE" (pr-str result))
    (shutdown-agents)
    (System/exit (if (and (= 1 (:test result)) (zero? (:fail result)) (zero? (:error result))) 0 1))))
