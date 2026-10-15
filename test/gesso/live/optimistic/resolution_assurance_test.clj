(ns gesso.live.optimistic.resolution-assurance-test
  "Permanent executor-assurance regressions, including v943 construction-input
   capture at the actual production call site. Exercises registered adapter
   identity, genuine Gesso Live settlement, embedded XTDB incorporation, replay,
   conflict, post-commit diagnostics, private-context redaction and fail-closed
   missing, duplicated and inconsistent construction-input observations.

   These bounded process-local witnesses do not certify authorization policy,
   snapshot consistency, implementation refinement, or downstream applications."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.core :as live]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.resolution-assurance :as assurance]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-event-observation :as events]
   [gesso.model.resolved-operation :as model-resolved]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private operation-id :request/claim)
(def ^:private trusted-principal (identity/principal "helper-v814"))
(def ^:private now "2026-10-12T10:00:00Z")

(defn- row [table id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- server-with [executor]
  (server/server
   {:principal-fn (fn [ctx] (:authenticated-principal ctx))
    :operations
    {operation-id
     (server/operation
      {:name :request/claim-optimistic
       :operation operation-id
       :execute! executor
       :settlement-contract
       {:confirmed {:outcomes #{:request/claimed} :commit/status :committed}
        :already-incorporated {:outcomes #{:request/claimed}
                               :commit/status :committed}}})}}))

(defn- fixture! []
  (let [suffix (str (random-uuid))
        before {:xt/id (str "v814-request-" suffix)
                :revision 0 :created-at "t0" :updated-at "t0"
                :owner "unclaimed"}
        member {:xt/id (str "v814-member-" suffix)
                :revision 0 :created-at "t0" :updated-at "t0"
                :active true}
        claimed (assoc before :revision 1 :updated-at now
                       :owner "helper-v814")
        command (protocol/command
                 {:command-id (identity/command-id (str "v814-command-" suffix))
                  :execution-id (identity/execution-id (str "v814-execution-" suffix))
                  :operation operation-id
                  :arguments {:request-id (:xt/id before)}
                  :observed-basis {:tx-id 1 :system-time now}
                  :scope [:request (:xt/id before)]})
        counts (atom {:construct 0 :project 0 :disclose 0})
        disclose? (atom true)
        model (model-resolved/build-operation
               {:operation operation-id
                :read-record! (fn [_ id] (row :gesso_commands id))
                :may-disclose? (fn [_ _ _]
                                 (swap! counts update :disclose inc)
                                 @disclose?)
                :construct! (fn [_ _]
                              (swap! counts update :construct inc)
                              {:now now
                               :outcome {:status :claimed}
                               :plan {:commands
                                      [(command/update-command
                                        :requests :claim before claimed version)]
                                      :guards [(command/guard :memberships
                                                             member version)]
                                      :emit false}})})
        executor (adapter/build-executor
                  {:operation operation-id
                   :resolved-operation model
                   :submission-mode! (fn [_] :submit)
                   :project! (fn [_ result]
                               (swap! counts update :project inc)
                               {:resolution
                                (if (= :replay (:status result))
                                  :already-incorporated :confirmed)
                                :authoritative
                                (protocol/authoritative
                                 {:presence :present
                                  :basis {:tx-id 2 :system-time now}
                                  :projection
                                  {:request/owner (:owner (row :requests (:xt/id before)))}})
                                :outcome :request/claimed})})]
    (xt/execute-tx @!node [[:put-docs :requests before]
                           [:put-docs :memberships member]])
    {:before before :member member :claimed claimed :command command
     :counts counts :disclose? disclose? :model model :executor executor
     :server (server-with executor)}))

(defn- invocation [fixture]
  {:server (:server fixture)
   :ctx {:biff.xtdb/node @!node :authenticated-principal trusted-principal}
   :command (:command fixture)})

(defn- stored-record [fixture]
  (let [command (:command fixture)]
    (row :gesso_commands
         (resolution/record-id
          {:command-id (:command-id command)
           :principal trusted-principal
           :operation operation-id
           :arguments (:arguments command)}))))

(defn- thrown [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- kind [f]
  (some-> (thrown f) ex-data :error/kind))

(defn- execute! [fixture]
  (assurance/run-and-check! (invocation fixture)))

(deftest diagnostic-scope-does-not-claim-formal-certification-test
  (let [{:keys [checked assumed not-established]}
        assurance/assurance-scope]
    (is (= :gesso.live.optimistic.resolution-assurance/diagnostic-v1
           (:construction assurance/assurance-scope)))
    (is (contains? checked :actual-registered-executor-identity))
    (is (contains? checked :independent-bounded-xtdb-state-correspondence))
    (is (contains? checked :no-evidence-on-unknown-outcome-or-replay))
    (is (contains? assumed :trusted-domain-construct-and-project-callbacks))
    (is (contains? assumed :consistent-unpinned-xtdb-reads))
    (is (contains? not-established :domain-authorization-or-invariant-correctness))
    (is (contains? not-established :authenticated-xtdb-transaction-history))
    (is (contains? not-established :formal-refinement-proof))
    (is (contains? not-established :whole-application-certification))))

(deftest successful-live-execution-yields-physical-evidence-test
  (let [f (fixture!)
        {:keys [prepared-send evidence]} (execute! f)
        report (assurance/inspect-evidence evidence)]
    (is (server/prepared-send? prepared-send))
    (is (= :confirmed (get-in prepared-send [:settlement :resolution])))
    (is (= (get-in f [:command :command-id])
           (get-in prepared-send [:settlement :command-id])))
    (is (= (get-in f [:command :execution-id])
           (get-in prepared-send [:settlement :execution-id])))
    (is (= :bounded-execution-correspondence (:status report)))
    (is (= :consistent (get-in report [:observation :evidence :status])))
    (is (= :committed (get-in report [:observation :physical :status])))
    (is (= [:committed]
           (get-in report [:observation :observations :statuses])))
    (is (= :private-runtime-construction-registry
           (get-in report [:adapter-construction :evidence-source])))
    (is (= trusted-principal (get-in report [:binding :principal])))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (not= (:claimed f) (stored-record f))
        "The durable command record is distinct from the domain request.")
    (is (map? (stored-record f)))
    (is (= {:construct 1 :project 1}
           (select-keys @(:counts f) [:construct :project])))))

(deftest process-local-handle-is-not-an-app-authored-certificate-test
  (let [f (fixture!)
        {:keys [evidence]} (execute! f)
        report (assurance/inspect-evidence evidence)]
    (is (some? report))
    (is (nil? (assurance/inspect-evidence nil)))
    (is (nil? (assurance/inspect-evidence (Object.))))
    (is (nil? (assurance/inspect-evidence report)))
    (is (nil? (assurance/inspect-evidence (assoc report :status :verified))))
    (is (= :bounded-execution-correspondence
           (:status (assurance/inspect-evidence evidence))))
    (is (not (identical? evidence report)))))

(deftest malformed-request-cannot-execute-test
  (let [f (fixture!)
        valid (invocation f)]
    (is (= :invalid-request (kind #(assurance/run-and-check! (assoc valid :extra true)))))
    (is (= :invalid-request (kind #(assurance/run-and-check! (dissoc valid :command)))))
    (is (= :invalid-request (kind #(assurance/run-and-check! nil))))
    (is (= :invalid-server-context
           (kind #(assurance/run-and-check! (assoc valid :server nil)))))
    (is (= :invalid-server-context
           (kind #(assurance/run-and-check! (assoc valid :ctx nil)))))
    (is (zero? (:construct @(:counts f))))
    (is (= (:before f) (row :requests (get-in f [:before :xt/id]))))))

(deftest missing-node-and-existing-observer-fail-before-domain-construction-test
  (let [f (fixture!)
        valid (invocation f)]
    (is (= :invalid-observation-context
           (kind #(assurance/run-and-check!
                   (update valid :ctx dissoc :biff.xtdb/node)))))
    (is (= :invalid-observation-context
           (kind #(assurance/run-and-check!
                   (assoc-in valid [:ctx model-resolved/observation-context-key]
                             (fn [_] nil))))))
    (is (zero? (:construct @(:counts f))))
    (is (nil? (stored-record f)))))

(deftest copied-metadata-cannot-impersonate-registered-adapter-test
  (let [f (fixture!)
        called (atom 0)
        impostor (with-meta (fn [_] (swap! called inc)) (meta (:executor f)))
        request (assoc (invocation f) :server (server-with impostor))]
    (is (nil? (adapter/construction-evidence impostor)))
    (is (= :unrecognized-registered-adapter
           (kind #(assurance/run-and-check! request))))
    (is (zero? @called))
    (is (zero? (:construct @(:counts f))))
    (is (nil? (stored-record f)))))

(deftest wrapped-executor-does-not-inherit-construction-evidence-test
  (let [f (fixture!)
        called (atom 0)
        actual (:executor f)
        wrapped (with-meta (fn [context]
                             (swap! called inc)
                             (actual context))
                  (meta actual))]
    (is (nil? (adapter/construction-evidence wrapped)))
    (is (= :unrecognized-registered-adapter
           (kind #(assurance/run-and-check!
                   (assoc (invocation f) :server (server-with wrapped))))))
    (is (zero? @called))
    (is (zero? (:construct @(:counts f))))))

(deftest copied-function-with-modified-metadata-does-not-inherit-identity-test
  (let [f (fixture!)
        original (:executor f)
        cloned (with-meta original (assoc (meta original) :copied true))]
    (is (not (identical? cloned original)))
    (is (nil? (adapter/construction-evidence cloned)))
    (is (= :unrecognized-registered-adapter
           (kind #(assurance/run-and-check!
                   (assoc (invocation f) :server (server-with cloned))))))
    (is (zero? (:construct @(:counts f))))))

(deftest durable-replay-has-no-new-physical-witness-test
  (let [f (fixture!)
        first-result (execute! f)
        initial (stored-record f)
        first-handle (:evidence first-result)]
    (is (some? (assurance/inspect-evidence first-handle)))
    (is (= :incomplete-event-pair (kind #(execute! f))))
    (is (= 1 (:construct @(:counts f))))
    (is (= 2 (:project @(:counts f))))
    (is (= initial (stored-record f)))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (some? (assurance/inspect-evidence first-handle)))))

(deftest stale-guard-cannot-produce-successful-physical-witness-test
  (let [f (fixture!)
        member (:member f)
        revoked (assoc member :active false :revision 1 :updated-at now)]
    (xt/execute-tx @!node
                   (:tx-ops (tx/prepare {}
                                        {:commands [(command/update-command
                                                    :memberships :revoke member
                                                    revoked version)]
                                         :emit false})))
    (is (= :resolution-not-settleable (kind #(execute! f))))
    (is (nil? (stored-record f)))
    (is (= (:before f) (row :requests (get-in f [:before :xt/id]))))
    (is (= revoked (row :memberships (:xt/id member))))
    (is (= 1 (:construct @(:counts f))))
    (is (zero? (:project @(:counts f))))))

(deftest missing-acknowledgment-does-not-create-a-physical-witness-test
  (let [f (fixture!)
        original tx/transact!]
    (is (= :nondefinitive-outcome
           (kind #(with-redefs [tx/transact!
                               (fn [ctx plan]
                                 (let [_ (original ctx plan)]
                                   (throw (ex-info "simulated lost acknowledgment"
                                                   {:failure :lost-ack}))))]
                    (execute! f)))))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (map? (stored-record f)))
    (is (= 1 (:construct @(:counts f))))
    (is (= 1 (:project @(:counts f))))))

(deftest classified-post-commit-delivery-failure-remains-committed-test
  (let [f (fixture!)
        original tx/transact!
        result (with-redefs [tx/transact!
                            (fn [ctx plan]
                              (let [ack (original ctx plan)]
                                (throw (ex-info "delivery failed after commit"
                                                {:error/type live/post-commit-delivery-failure-type
                                                 :commit/status :committed
                                                 :failure/stage :post-commit-delivery
                                                 :tx-result (:tx-result ack)}))))]
                 (execute! f))
        checked (assurance/inspect-evidence (:evidence result))]
    (is (= :already-incorporated
           (get-in result [:prepared-send :settlement :resolution])))
    (is (= :bounded-execution-correspondence (:status checked)))
    (is (= :consistent (get-in checked [:observation :evidence :status])))
    (is (= :gesso-live-classified-post-commit-failure
           (get-in checked [:observation :physical :source])))
    (is (= :committed (get-in checked [:observation :physical :status])))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (map? (stored-record f)))
    (is (= 1 (:construct @(:counts f))))))

(deftest projector-failure-after-commit-is-never-turned-into-authoritative-rejection-test
  (let [f (fixture!)
        original (:executor f)
        ;; Preserve genuine adapter identity: the domain remains real, but the
        ;; underlying XTDB node's query for the projector fails after commit.
        ;; Rather than mutate the registered function, use a trusted domain
        ;; projector configured to throw through the same adapter builder.
        model (model-resolved/build-operation
               {:operation operation-id
                :read-record! (fn [_ id] (row :gesso_commands id))
                :may-disclose? (fn [& _] true)
                :construct! (fn [_ _]
                              {:now now :outcome {:status :claimed}
                               :plan {:commands [(command/update-command
                                                  :requests :claim (:before f)
                                                  (:claimed f) version)]
                                      :guards [(command/guard :memberships
                                                             (:member f) version)]
                                      :emit false}})})
        bad (adapter/build-executor
             {:operation operation-id
              :resolved-operation model
              :submission-mode! (fn [_] :submit)
              :project! (fn [& _] (throw (ex-info "projection failed after commit"
                                                  {:failure :projection})))} )]
    (is (= :projection
           (some-> (thrown #(assurance/run-and-check!
                            (assoc (invocation f) :server (server-with bad))))
                   ex-data :failure)))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (map? (stored-record f)))
    (is (some? (adapter/construction-evidence original)))))

(deftest inconsistent-finite-verdict-cannot-produce-evidence-handle-test
  (let [f (fixture!)
        original events/start-session!]
    (is (= :execution-counterexample
           (kind #(with-redefs [events/start-session!
                               (fn [request]
                                 (let [s (original request)]
                                   (assoc s :complete!
                                          (fn [] {:evidence
                                                  {:status :counterexample
                                                   :reason :tampered-diagnostic-verdict}}))))]
                    (execute! f)))))
    (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
    (is (map? (stored-record f)))
    (is (= 1 (:construct @(:counts f))))))

(deftest truthful-transaction-witness-does-not-certify-projector-output-test
  (let [f (fixture!)
        liar (adapter/build-executor
              {:operation operation-id
               :resolved-operation (:model f)
               :submission-mode! (fn [_] :submit)
               :project! (fn [_ _]
                           {:resolution :confirmed
                            :authoritative
                            (protocol/authoritative
                             {:presence :present
                              :basis {:tx-id 2 :system-time now}
                              :projection {:request/owner "wrong-helper"}})
                            :outcome :request/claimed})})
        result (assurance/run-and-check!
                (assoc (invocation f) :server (server-with liar)))
        report (assurance/inspect-evidence (:evidence result))
        claimed (row :requests (get-in f [:before :xt/id]))]
    (is (= :consistent (get-in report [:observation :evidence :status])))
    (is (= :bounded-execution-correspondence (:status report)))
    (is (= "helper-v814" (:owner claimed)))
    (is (= "wrong-helper"
           (get-in result [:prepared-send :settlement :authoritative
                           :projection :request/owner])))
    (is (not= (:owner claimed)
              (get-in result [:prepared-send :settlement :authoritative
                              :projection :request/owner])))
    (is (map? (stored-record f)))
    (is (= 1 (:construct @(:counts f))))
    (is (contains? (get-in report [:scope :assumed])
                   :trusted-domain-construct-and-project-callbacks))
    (is (contains? (get-in report [:scope :not-established])
                   :domain-authorization-or-invariant-correctness))))

;; v944: permanent v943 executor-bound construction-input regressions.
(deftest actual-sensitive-construction-input-is-executor-bound-test
  (let [f (fixture!)
        secret "v943-secret-input-witness"
        context (assoc (:ctx (invocation f))
                       :private-policy secret :nullable-restriction nil)
        invocation (assoc (invocation f) :ctx context)
        {:keys [evidence]} (assurance/run-and-check! invocation)
        report (assurance/inspect-evidence evidence)
        witness (assurance/inspect-construction-input-for-executor
                 evidence (:executor f))
        other (fixture!)]
    (is (= :bounded-execution-correspondence (:status report)))
    (is (= :captured-and-correlated
           (get-in report [:construction-input :status])))
    (is (= :withheld-from-report
           (get-in report [:construction-input :context-values])))
    (is (not (.contains (pr-str report) secret)))
    (is (= :executor-bound-construction-input (:status witness)))
    (is (= report
           (assurance/inspect-evidence-for-executor evidence (:executor f))))
    (is (= :not-established
           (get-in report [:construction-input :model-input-coverage])))
    (is (contains? (get-in report [:scope :not-established])
                   :complete-model-input-or-policy-dependencies))
    (is (not (contains? report :context)))
    (is (not (contains? report :construction-input-witness)))
    (is (not (.contains (pr-str report) ":private-policy")))
    (is (= context (:context witness)))
    (is (contains? (:context witness) :nullable-restriction))
    (is (nil? (:nullable-restriction (:context witness))))
    (is (= (:binding report) (:binding witness)))
    (is (= :trusted-local-call-site-capture (:input-origin witness)))
    (is (= :not-established (:application-certification witness)))
    (is (nil? (assurance/inspect-evidence-for-executor
               evidence (:executor other))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               evidence (:executor other))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               nil (:executor f))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               (Object.) (:executor f))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               report (:executor f))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               (assoc report :status :bounded-execution-correspondence)
               (:executor f))))
    (is (nil? (assurance/inspect-construction-input-for-executor
               evidence (with-meta (fn [_] nil) (meta (:executor f))))))))

(deftest preexisting-construction-input-observer-is-rejected-test
  (let [f (fixture!)
        request (assoc-in (invocation f)
                          [:ctx model-resolved/construction-input-context-key]
                          (fn [_] nil))]
    (is (= :invalid-observation-context
           (kind #(assurance/run-and-check! request))))
    (is (zero? (:construct @(:counts f))))
    (is (nil? (stored-record f)))
    (is (= (:before f) (row :requests (get-in f [:before :xt/id]))))))

(deftest construction-input-events-fail-closed-test
  (let [probe (var-get (ns-resolve 'gesso.live.optimistic.resolution-assurance
                                    'construction-input-witness!))
        ctx {:context-private "do-not-disclose" :nullable nil}
        binding (resolution/require-binding
                 {:command-id (identity/command-id "v943-test-command")
                  :operation operation-id
                  :principal trusted-principal
                  :arguments {:request-id "v943-target"}})
        instrumented (assoc ctx
                            model-resolved/construction-input-context-key (fn [_] nil)
                            model-resolved/observation-context-key (fn [_] nil))
        event {:scope model-resolved/construction-input-observation-scope
               :phase :construction-input :operation operation-id
               :context instrumented :binding binding}
        invalids [["no event" [] :construction-input-incomplete]
                  ["duplicated event" [event event] :construction-input-incomplete]
                  ["wrong command" [(assoc event :binding
                                             (assoc binding :command-id
                                                    (identity/command-id "v943-other")))]
                   :construction-input-mismatch]
                  ["wrong operation" [(assoc event :operation :request/cancel)]
                   :construction-input-mismatch]
                  ["wrong principal" [(assoc event :binding
                                                (assoc binding :principal
                                                       (identity/principal "impostor")))]
                   :construction-input-mismatch]
                  ["wrong arguments" [(assoc event :binding
                                                (assoc binding :arguments
                                                       {:request-id "other"}))]
                   :construction-input-mismatch]
                  ["copied context" [(assoc event :context (into {} instrumented))]
                   :construction-input-mismatch]
                  ["extra field" [(assoc event :secret "should-not-leak")]
                   :construction-input-mismatch]]]
    (is (= {:context ctx :binding binding}
           (probe (atom [event]) instrumented ctx binding)))
    (is (not (identical? instrumented (into {} instrumented))))
    (doseq [[label observations expected-kind] invalids]
      (testing label
        (let [err (thrown #(probe (atom observations) instrumented ctx binding))]
          (is (= expected-kind (get-in (ex-data err) [:error/kind])))
          (is (not (.contains (pr-str (ex-data err)) "do-not-disclose"))))))))

(deftest committed-transaction-with-incomplete-construction-input-has-no-handle-test
  (doseq [mode [:missing :duplicate]]
    (testing (name mode)
      (let [f (fixture!)
            original-model (:model f)
            wrapped-model
            (with-meta
              (fn [ctx input]
                (case mode
                  :missing
                  (original-model
                   (dissoc ctx model-resolved/construction-input-context-key)
                   input)
                  :duplicate
                  (do
                    ((get ctx model-resolved/construction-input-context-key)
                     {:scope model-resolved/construction-input-observation-scope
                      :phase :construction-input
                      :operation operation-id
                      :context ctx
                      :binding (:binding input)})
                    (original-model ctx input))))
              (meta original-model))
            executor (adapter/build-executor
                      {:operation operation-id
                       :resolved-operation wrapped-model
                       :submission-mode! (constantly :submit)
                       :project! (fn [_ _]
                                   {:resolution :confirmed
                                    :authoritative
                                    (protocol/authoritative
                                     {:presence :present
                                      :basis {:tx-id 2 :system-time now}
                                      :projection {:request/owner
                                                   (:owner (row :requests
                                                                (get-in f [:before :xt/id])))}})
                                    :outcome :request/claimed})})
            request (assoc (invocation f) :server (server-with executor))]
        (is (= :construction-input-incomplete
               (kind #(assurance/run-and-check! request))))
        (is (= (:claimed f) (row :requests (get-in f [:before :xt/id]))))
        (is (map? (stored-record f)))
        (is (= 1 (:construct @(:counts f))))))))
