(ns gesso.live.optimistic.resolved-operation-test
  "Regression coverage for the v796 adapter between durable model resolution
   and Gesso Live's trusted optimistic server operation boundary.

   These tests exercise the *registered* server/run-command path, real
   protocol-v3 command/settlement correlation, real durable XTDB incorporation,
   replay and lost acknowledgment, as well as fail-closed adapter paths.

   Fake transaction cases establish control-flow properties, not persistence.
   The embedded-XTDB cases establish only the exercised serial commit behavior;
   the fixture projector models an authoritative read but does not certify a
   production application's policy, basis, or notification progression.

   Construction metadata is an untrusted indication, never a proof certificate.
   An unresolved delivery cannot automatically become a new :submit, and a
   post-commit projection error cannot be converted into :failed."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolved-operation :as model-resolved]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private now "2026-10-12T09:00:00Z")
(def ^:private recorded-outcome {:status :claimed})
(def ^:private principal (identity/principal "helper-v797"))
(def ^:private untrusted-principal (identity/principal "browser-spoofed"))
(def ^:private operation-id :request/claim)

(defn- fresh-id [prefix]
  (str prefix "-" (random-uuid)))

(defn- request-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- claimed-request [request]
  (-> request
      (assoc :owner "helper-v797" :updated-at now)
      (update :revision inc)))

(defn- claim-plan [request]
  {:commands [(command/update-command
               :requests :claim request (claimed-request request) version-keys)]
   :guards []
   :emit false})

(defn- browser-command [request-id cmd-id exec-id]
  (protocol/command
   {:command-id cmd-id
    :execution-id exec-id
    :operation operation-id
    :arguments {:request-id request-id}
    :observed-basis {:tx-id 1 :system-time "2026-10-12T08:00:00Z"}
    :scope [:request request-id]}))

(defn- authoritative [request-id]
  (protocol/authoritative
   {:presence :present
    :basis {:tx-id 2 :system-time now}
    :projection {:request/id request-id :request/status :claimed}}))

(defn- binding-from-command [command]
  {:command-id (:command-id command)
   :principal principal
   :operation operation-id
   :arguments (:arguments command)})

(defn- error-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- error-kind [f]
  (:error/kind (error-data f)))

(defn- prepare-server [execute!]
  (server/server
   {:principal-fn (fn [ctx] (:authenticated-principal ctx))
    :operations
    {operation-id
     (server/operation
      {:name :request/claim-optimistic
       :operation operation-id
       :execute! execute!
       :settlement-contract
       {:confirmed {:outcomes #{:request/claimed}
                    :commit/status :committed}
        :reconciled {:outcomes #{:request/claimed}
                     :commit/status :committed}
        :already-incorporated {:outcomes #{:request/claimed}
                               :commit/status :committed}}})}}))

(defn- fake-environment
  ([] (fake-environment {}))
  ([{:keys [project-fn]}]
   (let [request (request-document (fresh-id "request"))
         command (browser-command (:xt/id request)
                                  (identity/command-id (fresh-id "command"))
                                  (identity/execution-id (fresh-id "execution")))
         binding (binding-from-command command)
         record (resolution/command-record binding recorded-outcome now)
         stored (atom nil)
         mode (atom :submit)
         permitted? (atom true)
         calls (atom {:read 0 :disclose 0 :construct 0 :mode 0 :project 0 :commit 0})
         constructed (model-resolved/build-operation
                      {:operation operation-id
                       :read-record! (fn [_ctx id]
                                       (swap! calls update :read inc)
                                       (when (= id (resolution/record-id binding))
                                         @stored))
                       :may-disclose? (fn [_ctx _binding _record]
                                        (swap! calls update :disclose inc)
                                        @permitted?)
                       :construct! (fn [_ctx received-binding]
                                     (swap! calls update :construct inc)
                                     (when-not (= binding received-binding)
                                       (throw (ex-info "Unexpected binding in construction" {})))
                                     {:now now :plan (claim-plan request)
                                      :outcome recorded-outcome})})
         projector (or project-fn
                       (fn [_context result]
                         {:resolution (if (= :replay (:status result))
                                        :already-incorporated :confirmed)
                          :authoritative (authoritative (:xt/id request))
                          :outcome :request/claimed}))
         executor (adapter/build-executor
                   {:operation operation-id
                    :resolved-operation constructed
                    :submission-mode! (fn [_] (swap! calls update :mode inc) @mode)
                    :project! (fn [context result]
                                (swap! calls update :project inc)
                                (projector context result))})]
     {:request request :command command :binding binding :record record
      :stored stored :mode mode :permitted? permitted? :calls calls
      :constructed constructed :executor executor
      :server (prepare-server executor)})))

(defn- fake-commit! [environment]
  (fn [_ctx _plan]
    (swap! (:calls environment) update :commit inc)
    (reset! (:stored environment) (:record environment))
    {:commit/status :committed}))

(defn- invoke
  ([environment] (invoke environment (:command environment)))
  ([environment command]
   (server/run-command (:server environment)
                       {:authenticated-principal principal
                        :browser-principal untrusted-principal}
                       command)))

(deftest v796-adapter-assurance-is-bounded-and-metadata-is-not-a-certificate
  (let [scope adapter/assurance-scope
        env (fake-environment)
        metadata (meta (:executor env))]
    (is (= :gesso.live.optimistic.resolved-operation/adapter-v1
           (:construction scope)))
    (doseq [checked [:registered-operation-binding
                     :typed-principal-and-command-correlation
                     :no-settlement-on-unresolved-command
                     :no-failed-resolution-after-incorporation
                     :committed-result-provenance]]
      (is (contains? (:checked scope) checked)))
    (doseq [unknown [:authenticated-construction-provenance
                     :humanhelp-authorization
                     :complete-domain-invariants
                     :durable-notification-debt
                     :whole-application-correspondence]]
      (is (contains? (:not-established scope) unknown)))
    (is (= operation-id (:gesso.model/operation-id metadata)))
    (is (= :durable-command-v1 (:gesso.model/resolution metadata)))
    (is (= model-resolved/assurance-scope
           (:gesso.model/assurance-scope metadata)))
    (is (= scope
           (:gesso.live.optimistic.resolved-operation/assurance-scope metadata)))
    (is (server/operation?
         (get-in (:server env) [:operations operation-id])))))

(deftest construction-validation-rejects-invalid-function-and-mismatched-metadata
  (let [env (fake-environment)
        config {:operation operation-id
                :resolved-operation (:constructed env)
                :submission-mode! (fn [_] :submit)
                :project! (fn [_ _] {:resolution :confirmed})}]
    (is (= :invalid-configuration
           (error-kind #(adapter/build-executor (assoc config :extra true)))))
    (is (= :invalid-configuration
           (error-kind #(adapter/build-executor (dissoc config :project!)))))
    (is (= :invalid-operation
           (error-kind #(adapter/build-executor (assoc config :operation :unqualified)))))
    (is (= :invalid-resolved-operation
           (error-kind #(adapter/build-executor (assoc config :operation :request/cancel)))))
    (is (= :invalid-resolved-operation
           (error-kind #(adapter/build-executor (assoc config :resolved-operation (fn [& _] nil))))))
    (is (= :invalid-mode-selector
           (error-kind #(adapter/build-executor (assoc config :submission-mode! {:callable true})))))
    (is (= :invalid-projector
           (error-kind #(adapter/build-executor (assoc config :project! nil)))))
    (testing "copyable metadata is only a construction indication, not authenticated evidence"
      (let [forged (with-meta (fn [& _] {:status :unresolved})
                     (meta (:constructed env)))
            adapter' (adapter/build-executor (assoc config :resolved-operation forged))]
        (is (fn? adapter'))
        (is (= :resolution-not-settleable
               (error-kind #(adapter' (server/operation-context
                                      (server/begin-command (:server env)
                                                            {:authenticated-principal principal}
                                                            (:command env)))))))))))

(deftest genuine-optimistic-server-submission-and-replay-keep-identities
  (let [env (fake-environment)]
    (with-redefs [tx/transact! (fake-commit! env)]
      (let [first-send (invoke env)
            next-execution (identity/execution-id (fresh-id "execution"))
            replay-command (browser-command (:xt/id (:request env))
                                            (:command-id (:command env))
                                            next-execution)
            replay-send (invoke env replay-command)]
        (is (server/prepared-send? first-send))
        (is (server/prepared-send? replay-send))
        (is (= :confirmed (get-in first-send [:settlement :resolution])))
        (is (= :already-incorporated (get-in replay-send [:settlement :resolution])))
        (is (= (:command-id (:command env))
               (get-in first-send [:settlement :command-id])
               (get-in replay-send [:settlement :command-id])))
        (is (= (:execution-id (:command env))
               (get-in first-send [:settlement :execution-id])))
        (is (= next-execution (get-in replay-send [:settlement :execution-id])))
        (is (= :request/claimed (get-in first-send [:settlement :outcome])))
        (is (= :request/claimed (get-in replay-send [:settlement :outcome])))
        (is (= (get first-send :settlement)
               (protocol/wire->settlement (:settlement-wire first-send))))
        (is (= (get replay-send :settlement)
               (protocol/wire->settlement (:settlement-wire replay-send))))
        (is (server/completed-send? (server/complete-settlement-send first-send)))
        (is (= 1 (:construct @(:calls env))))
        (is (= 1 (:commit @(:calls env))))
        (is (= 2 (:project @(:calls env))))))))

(deftest browser-principal-does-not-replace-trusted-server-principal
  (let [env (fake-environment)
        boundary (server/begin-command (:server env)
                                       {:authenticated-principal principal
                                        :browser-principal untrusted-principal}
                                       (:command env))
        context (server/operation-context boundary)]
    (is (= principal (:principal context)))
    (is (not= untrusted-principal (:principal context)))
    (is (= (:command-id (:command env)) (:command-id context)))
    (is (= (:execution-id (:command env)) (:execution-id context)))
    (is (= (:arguments (:command env)) (:arguments context)))
    (is (= (dissoc (:command env) :protocol-version)
           (:command context)))))

(deftest corrupted-operation-context-is-rejected-before-resolution-or-projection
  (let [env (fake-environment)
        context (server/operation-context
                 (server/begin-command (:server env)
                                       {:authenticated-principal principal}
                                       (:command env)))
        changes [(assoc context :principal "untyped")
                 (assoc context :command-id (identity/command-id (fresh-id "different")))
                 (assoc context :execution-id (identity/execution-id (fresh-id "other")))
                 (assoc context :operation :request/cancel)
                 (assoc context :arguments {:request-id "swapped"})
                 (assoc context :command (assoc (:command context) :arguments {:request-id "swapped"}))
                 (dissoc context :ctx)
                 (assoc context :principal (assoc principal :extra :forged))]]
    (doseq [altered changes]
      (is (contains? #{:invalid-operation-context :invalid-binding}
                     (error-kind #((:executor env) altered)))))
    (is (zero? (:mode @(:calls env))))
    (is (zero? (:read @(:calls env))))
    (is (zero? (:construct @(:calls env))))
    (is (zero? (:project @(:calls env))))))

(deftest trusted-mode-selector-is-closed-and-reconcile-never-submits
  (let [env (fake-environment)]
    (reset! (:mode env) :reconcile)
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "unexpected commit" {})))]
      (is (= :resolution-not-settleable (error-kind #(invoke env))))
      (is (zero? (:construct @(:calls env))))
      (is (zero? (:project @(:calls env))))
      (reset! (:mode env) :browser-selected)
      (is (= :invalid-submission-mode (error-kind #(invoke env))))
      (is (zero? (:construct @(:calls env)))))))

(deftest incompatible-command-id-reuse-never-reconstructs-or-leaks-result
  (let [env (fake-environment)
        changed (protocol/command
                 (-> (:command env)
                     (dissoc :protocol-version)
                     (assoc :arguments {:request-id (fresh-id "other-request")})))]
    (with-redefs [tx/transact! (fake-commit! env)]
      (is (= :confirmed (get-in (invoke env) [:settlement :resolution])))
      (let [before @(:calls env)]
        (is (= :resolution-not-settleable (error-kind #(invoke env changed))))
        (is (= (:construct before) (:construct @(:calls env))))
        (is (= (:commit before) (:commit @(:calls env))))
        (is (= (:project before) (:project @(:calls env))))))))

(deftest disclosure-denial-never-turns-a-record-into-authorized-replay
  (let [env (fake-environment)]
    (reset! (:stored env) (:record env))
    (reset! (:permitted? env) false)
    (is (= :resolution-not-settleable (error-kind #(invoke env))))
    (is (zero? (:construct @(:calls env))))
    (is (zero? (:project @(:calls env))))
    (is (zero? (:commit @(:calls env))))))

(deftest noncommitted-transaction-without-authoritative-record-does-not-settle
  (let [env (fake-environment)
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 {:commit/status :not-committed})]
      (is (= :resolution-not-settleable (error-kind #(invoke env))))
      (is (= 1 @submissions))
      (is (= 1 (:construct @(:calls env))))
      (is (zero? (:project @(:calls env)))))))

(deftest projection-failure-after-commit-propagates-without-fabricated-failure
  (let [reject-projection? (atom true)
        env (fake-environment
             {:project-fn (fn [_ result]
                            (if @reject-projection?
                              (throw (ex-info "authoritative observation unavailable"
                                              {:stage :after-commit}))
                              {:resolution (if (= :replay (:status result))
                                             :already-incorporated :confirmed)
                               :authoritative (authoritative "request")
                               :outcome :request/claimed}))})]
    (with-redefs [tx/transact! (fake-commit! env)]
      (is (= :after-commit (:stage (error-data #(invoke env)))))
      (is (= 1 (:commit @(:calls env))))
      (is (= 1 (:construct @(:calls env))))
      (reset! reject-projection? false)
      (reset! (:mode env) :reconcile)
      (is (= :already-incorporated (get-in (invoke env) [:settlement :resolution])))
      (is (= 1 (:commit @(:calls env))))
      (is (= 1 (:construct @(:calls env)))))))

(deftest projection-must-not-report-committed-work-as-failed
  (doseq [resolution [:failed :unknown :already-incorporated]]
    (let [env (fake-environment
               {:project-fn (fn [_ _] {:resolution resolution})})]
      (with-redefs [tx/transact! (fake-commit! env)]
        (is (= :invalid-resolution (error-kind #(invoke env))))
        (is (= 1 (:commit @(:calls env))))))))

(deftest projected-settlement-shape-is-closed-and-protocol-checked
  (doseq [[projected expected]
          [[{:resolution :confirmed :authoritative (authoritative "r")
             :outcome :request/claimed :extra :unexamined} :invalid-projection]
           [{:resolution :confirmed :outcome :request/claimed} :missing-settlement-authority]
           [{:resolution :confirmed :authoritative (authoritative "r")
             :outcome "not-a-keyword"} :invalid-settlement-outcome]]]
    (let [env (fake-environment {:project-fn (fn [_ _] projected)})]
      (with-redefs [tx/transact! (fake-commit! env)]
        (is (= expected (error-kind #(invoke env))))
        (is (= 1 (:commit @(:calls env))))))))

(deftest application-settlement-contract-still-checks-trusted-outcome
  (let [env (fake-environment
             {:project-fn (fn [_ _] {:resolution :confirmed
                                    :authoritative (authoritative "r")
                                    :outcome :request/other})})]
    (with-redefs [tx/transact! (fake-commit! env)]
      (is (= :settlement-outcome-not-allowed (error-kind #(invoke env))))
      (is (= 1 (:commit @(:calls env)))))))

(deftest actual-xtdb-submission-replay-and-lost-acknowledgment-through-live
  (with-open [database (xtn/start-node {})]
    (let [request (request-document (fresh-id "xt-request"))
          command (browser-command (:xt/id request)
                                   (identity/command-id (fresh-id "xt-command"))
                                   (identity/execution-id (fresh-id "xt-execution")))
          command-row (resolution/record-id (binding-from-command command))
          mode (atom :submit)
          constructs (atom 0)
          submits (atom 0)
          real-commit tx/transact!
          operation (model-resolved/build-operation
                     {:operation operation-id
                      :read-record! (fn [_ctx id]
                                      (first (xt/q database
                                                   ["SELECT * FROM gesso_commands WHERE _id = ?" id])))
                      :may-disclose? (fn [_ctx _binding _record] true)
                      :construct! (fn [_ctx _binding]
                                    (swap! constructs inc)
                                    {:now now :plan (claim-plan request)
                                     :outcome recorded-outcome})})
          execute! (adapter/build-executor
                    {:operation operation-id
                     :resolved-operation operation
                     :submission-mode! (fn [_] @mode)
                     :project! (fn [_ctx result]
                                 (let [stored (first (xt/q database
                                                           ["SELECT * FROM requests WHERE _id = ?"
                                                            (:xt/id request)]))]
                                   {:resolution (if (= :replay (:status result))
                                                  :already-incorporated :confirmed)
                                    :authoritative
                                    (protocol/authoritative
                                     {:presence :present
                                      :basis {:tx-id 2 :system-time now}
                                      :projection {:request/id (:xt/id stored)
                                                   :request/owner (:owner stored)}})
                                    :outcome :request/claimed}))})
          app (prepare-server execute!)
          ctx {:authenticated-principal principal :biff.xtdb/node database}
          run! (fn [cmd] (server/run-command app ctx cmd))
          read-row (fn [table id]
                     (first (xt/q database
                                  [(str "SELECT * FROM " table " WHERE _id = ?") id])))]
      (xt/execute-tx database [[:put-docs :requests request]])
      (with-redefs [tx/transact!
                    (fn [current-ctx plan]
                      (swap! submits inc)
                      (real-commit current-ctx plan)
                      (throw (ex-info "lost commit acknowledgment" {:phase :after-commit})))]
        (let [prepared (run! command)]
          (is (server/prepared-send? prepared))
          (is (= :already-incorporated (get-in prepared [:settlement :resolution])))
          (is (= (:command-id command) (get-in prepared [:settlement :command-id])))))
      (is (= (claimed-request request) (read-row "requests" (:xt/id request))))
      (is (= (resolution/command-record
              (binding-from-command command) recorded-outcome now)
             (read-row "gesso_commands" command-row)))
      (is (= 1 @constructs))
      (is (= 1 @submits))
      (reset! mode :reconcile)
      (let [another-execution (identity/execution-id (fresh-id "retry-execution"))
            retried (browser-command (:xt/id request)
                                     (:command-id command)
                                     another-execution)
            prepared (run! retried)]
        (is (= :already-incorporated (get-in prepared [:settlement :resolution])))
        (is (= another-execution (get-in prepared [:settlement :execution-id])))
        (is (= 1 @constructs))
        (is (= 1 @submits)))
      (is (= (claimed-request request) (read-row "requests" (:xt/id request)))))))
