(ns gesso.model.resolved-operation-test
  "Permanent regression coverage for the v792 application-facing resolved
   operation wrapper. Distinguish admissible construction, read-before-plan
   replay, explicit disclosure, ambiguous acknowledgments, and real XTDB
   persistence. A mock commit establishes control-flow properties only;
   real-XTDB cases check the persisted domain and command-result documents.

   None of these examples proves application authentication, complete
   business authorization, domain-guard completeness, operation liveness,
   or universal transaction linearizability. :submit is a trusted *initial*
   submission mode, not a browser-selected retry instruction."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolved-operation :as operation]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private outcome {:status :claimed :owner "helper-a"})
(def ^:private now "t1")

(defn- unique-id [prefix]
  (str prefix "-" (random-uuid)))

(defn- request-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership-document [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- next-version [before changes]
  (-> before (merge changes) (update :revision inc)
      (assoc :updated-at now)))

(defn- claim-command [request]
  (command/update-command
   :requests :claim request (next-version request {:owner "helper-a"}) version))

(defn- revoke-command [membership]
  (command/update-command
   :memberships :revoke membership (next-version membership {:active false})
   version))

(defn- binding-for [request-id command-id]
  {:command-id (identity/command-id command-id)
   :principal (identity/principal "helper-a")
   :operation :request/claim
   :arguments {:request-id request-id}})

(defn- domain-plan
  ([request] (domain-plan request nil))
  ([request membership]
   {:commands [(claim-command request)]
    :guards (if membership
              [(command/guard :memberships membership version)]
              [])
    :emit false}))

(defn- invocation [binding mode]
  {:binding binding :mode mode})

(defn- status [result]
  (:status result))

(defn- error-kind [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/kind (ex-data e)))))

(defn- fixture-data []
  (let [request (request-document (unique-id "request"))
        membership (membership-document (unique-id "membership"))
        binding (binding-for (:xt/id request) (unique-id "command"))]
    {:request request :membership membership :binding binding
     :record (resolution/command-record binding outcome now)}))

(defn- fake-environment
  ([] (fake-environment (fixture-data)))
  ([{:keys [request binding record] :as data}]
   (let [stored (atom nil)
         permitted? (atom true)
         calls (atom {:read 0 :disclose 0 :construct 0 :commit 0})
         configuration
         {:operation :request/claim
          :read-record! (fn [_ctx row-id]
                          (swap! calls update :read inc)
                          (when (= row-id (resolution/record-id binding))
                            @stored))
          :may-disclose? (fn [_ctx _binding _record]
                           (swap! calls update :disclose inc)
                           @permitted?)
          :construct! (fn [_ctx _binding]
                        (swap! calls update :construct inc)
                        {:now now :plan (domain-plan request)
                         :outcome outcome})}]
     (assoc data :stored stored :permitted? permitted? :calls calls
            :configuration configuration
            :operation (operation/build-operation configuration)))))

(defn- call [environment mode]
  ((:operation environment) {} (invocation (:binding environment) mode)))

(deftest assurance-scope-and-metadata-are-honest-test
  (let [scope operation/assurance-scope
        built (:operation (fake-environment))]
    (is (= :gesso.model.resolved-operation/operation-v1
           (:construction scope)))
    (is (contains? (:checked scope) :reconcile-before-domain-construction))
    (is (contains? (:checked scope) :no-write-during-reconcile))
    (is (contains? (:checked scope) :no-automatic-resubmission-after-uncertain-commit))
    (doseq [assumption [:trusted-server-owned-invocation-mode
                        :authoritative-command-record-read
                        :correct-domain-construction-and-atomic-guards
                        :trusted-record-disclosure]]
      (is (contains? (:assumed scope) assumption)))
    (doseq [unproved [:humanhelp-authorization
                     :complete-domain-invariants
                     :commit-uncertainty-liveness
                     :whole-application-choreo-preflight
                     :universal-linearizability]]
      (is (contains? (:not-established scope) unproved)))
    (is (= :request/claim (:gesso.model/operation-id (meta built))))
    (is (= :durable-command-v1 (:gesso.model/resolution (meta built))))
    (is (= scope (:gesso.model/assurance-scope (meta built))))))

(deftest closed-operation-configuration-is-validated-before-use-test
  (let [environment (fake-environment)
        configuration (:configuration environment)]
    (is (= :invalid-configuration
           (error-kind #(operation/build-operation (assoc configuration :extra true)))))
    (is (= :invalid-configuration
           (error-kind #(operation/build-operation (dissoc configuration :construct!)))))
    (is (= :invalid-operation-id
           (error-kind #(operation/build-operation (assoc configuration :operation :claim)))))
    (doseq [callback [:read-record! :may-disclose? :construct!]]
      (is (= :missing-function
             (error-kind #(operation/build-operation
                           (assoc configuration callback {:not-a-function true}))))))))

(deftest invocation-validation-happens-before-any-callback-test
  (let [{:keys [operation binding calls]} (fake-environment)]
    (is (= :invalid-invocation
           (error-kind #(operation {} {:binding binding :mode :submit :extra 7}))))
    (is (= :invalid-invocation
           (error-kind #(operation {} {:binding binding}))))
    (is (= :invalid-mode
           (error-kind #(operation {} (invocation binding :retry)))))
    (is (= :operation-mismatch
           (error-kind #(operation {}
                                   (invocation (assoc binding :operation :request/cancel)
                                               :submit)))))
    (is (= :invalid-binding
           (error-kind #(operation {}
                                   (invocation (assoc binding :principal "helper-a")
                                               :reconcile)))))
    (is (= {:read 0 :disclose 0 :construct 0 :commit 0} @calls))))

(deftest reconcile-with-no-authoritative-record-never-constructs-test
  (let [{:keys [calls] :as e} (fake-environment)
        result (call e :reconcile)]
    (is (= :unresolved (status result)))
    (is (not (contains? result :outcome)))
    (is (not (contains? result :retry?)))
    (is (= operation/assurance-scope (:operation-scope result)))
    (is (= 1 (:read @calls)))
    (is (zero? (:disclose @calls)))
    (is (zero? (:construct @calls)))))

(deftest known-record-replays-without-planning-or-committing-test
  (let [{:keys [stored record calls] :as e} (fake-environment)]
    (reset! stored record)
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "must not submit" {})))]
      (doseq [mode [:submit :reconcile]]
        (let [result (call e mode)]
          (is (= :replay (status result)))
          (is (= outcome (:outcome result)))
          (is (= operation/assurance-scope (:operation-scope result))))))
    (is (= 2 (:read @calls)))
    (is (= 2 (:disclose @calls)))
    (is (zero? (:construct @calls)))))

(deftest existing-record-disclosure-denial-must-not-leak-or-build-test
  (let [{:keys [stored permitted? record calls] :as e} (fake-environment)]
    (reset! stored record)
    (reset! permitted? false)
    (let [result (call e :submit)]
      (is (= :unavailable (status result)))
      (is (not (contains? result :outcome)))
      (is (not (contains? result :binding))))
    (is (= 1 (:disclose @calls)))
    (is (zero? (:construct @calls)))))

(deftest incompatible-command-binding-cannot-trigger-a-domain-write-test
  (let [{:keys [stored record binding operation calls] :as e} (fake-environment)]
    (reset! stored record)
    (doseq [different [(assoc binding :arguments {:request-id "different"})
                       (assoc binding :principal (identity/principal "helper-b"))]]
      (let [result (operation {} (invocation different :submit))]
        (is (= :incompatible (status result)))
        (is (not (contains? result :outcome)))))
    (is (zero? (:construct @calls)))))

(deftest invalid-stored-record-rejects-before-disclosure-and-construction-test
  (let [{:keys [stored record calls] :as e} (fake-environment)]
    (reset! stored (assoc record :gesso.command/status :pending))
    (is (= :invalid-record (error-kind #(call e :submit))))
    (is (zero? (:disclose @calls)))
    (is (zero? (:construct @calls)))))

(deftest fresh-submit-uses-one-atomic-plan-and-returns-acknowledged-outcome-test
  (let [{:keys [binding request calls] :as e} (fake-environment)
        delivered (atom nil)
        tx-result {:commit/status :committed :receipt :confirmed}]
    (with-redefs [tx/transact! (fn [_ctx plan]
                                 (reset! delivered plan)
                                 (swap! calls update :commit inc)
                                 tx-result)]
      (let [result (call e :submit)
            plan @delivered
            commands (:commands plan)
            record-command (last commands)]
        (is (= :incorporated (status result)))
        (is (= outcome (:outcome result)))
        (is (= tx-result (:transaction result)))
        (is (= resolution/assurance-scope (:scope result)))
        (is (= operation/assurance-scope (:operation-scope result)))
        (is (= 2 (count commands)))
        (is (= :requests (:model/entity-type (first commands))))
        (is (= :gesso_commands (:model/entity-type record-command)))
        (is (= :create (:model/operation record-command)))
        (is (= (resolution/record-id binding) (:model/id record-command)))
        (is (= [(claim-command request)] (subvec commands 0 1)))
        (is (= [] (:guards plan)))
        (is (false? (:emit plan)))))
    (is (= 1 (:read @calls)))
    (is (= 1 (:construct @calls)))
    (is (= 1 (:commit @calls)))))

(deftest noncommitted-ack-is-unknown-unless-reread-finds-record-test
  (let [{:keys [calls] :as e} (fake-environment)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! calls update :commit inc)
                                 {:commit/status :not-committed})]
      (let [result (call e :submit)]
        (is (= :unresolved (status result)))
        (is (not (contains? result :retry?)))
        (is (not (contains? result :outcome)))))
    (is (= 2 (:read @calls)))
    (is (= 1 (:construct @calls)))
    (is (= 1 (:commit @calls)))))

(deftest lost-acknowledgement-can-only-be-resolved-by-record-read-test
  (let [{:keys [stored record calls] :as e} (fake-environment)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! calls update :commit inc)
                                 (reset! stored record)
                                 (throw (ex-info "acknowledgment lost" {})))]
      (let [result (call e :submit)]
        (is (= :replay (status result)))
        (is (= outcome (:outcome result)))
        (is (= operation/assurance-scope (:operation-scope result)))))
    (is (= 2 (:read @calls)))
    (is (= 1 (:construct @calls)))
    (is (= 1 (:commit @calls)))))

(deftest rejected-commit-is-not-mislabeled-a-business-rejection-test
  (let [{:keys [calls] :as e} (fake-environment)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! calls update :commit inc)
                                 (throw (ex-info "stale dependency" {:xtdb.error/code :xtdb/assert-failed})))]
      (let [result (call e :submit)]
        (is (= :unresolved (status result)))
        (is (not (contains? result :outcome)))
        (is (not (contains? result :retry?)))))
    (is (= 2 (:read @calls)))
    (is (= 1 (:construct @calls)))
    (is (= 1 (:commit @calls)))))

(deftest authoritative-read-failure-must-propagate-test
  (let [{:keys [configuration binding]} (fake-environment)
        first-read-fails (operation/build-operation
                          (assoc configuration
                                 :read-record! (fn [& _]
                                                 (throw (ex-info "storage unavailable" {:source :xtdb})))))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"storage unavailable"
                          (first-read-fails {} (invocation binding :submit))))
    (let [reads (atom 0)
          second-read-fails (operation/build-operation
                             (assoc configuration
                                    :read-record! (fn [& _]
                                                    (when (= 2 (swap! reads inc))
                                                      (throw (ex-info "reread unavailable" {})))
                                                    nil)))]
      (with-redefs [tx/transact! (fn [& _] (throw (ex-info "commit unknown" {})))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reread unavailable"
                              (second-read-fails {} (invocation binding :submit))))
        (is (= 2 @reads))))))

(deftest trusted-callback-failures-are-not-treated-as-committed-test
  (let [{:keys [configuration binding calls]} (fake-environment)
        invalid-construct (operation/build-operation
                           (assoc configuration :construct! (fn [& _] {:plan {}})))
        throws-construct (operation/build-operation
                          (assoc configuration
                                 :construct! (fn [& _] (throw (ex-info "policy failure" {})))))]
    ;; A disclosure failure only matters for an existing authoritative record.
    (is (= :invalid-construction
           (error-kind #(invalid-construct {} (invocation binding :submit)))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"policy failure"
                          (throws-construct {} (invocation binding :submit))))
    (let [stored-record (resolution/command-record binding outcome now)
          with-record (operation/build-operation
                       (assoc configuration :read-record! (fn [_ _] stored-record)))
          disclosure-failure (operation/build-operation
                              (-> configuration
                                  (assoc :read-record! (fn [_ _] stored-record))
                                  (assoc :may-disclose? (fn [& _]
                                                          (throw (ex-info "authorization unavailable" {}))))))]
      (is (= :replay (status (with-record {} (invocation binding :submit)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"authorization unavailable"
                            (disclosure-failure {} (invocation binding :submit)))))
    (is (zero? (:construct @calls)))))

;; Real database cases execute the actual gesso.model.tx/transact! path, not a
;; fake assertion engine. Each uses unique IDs to isolate its persisted rows.
(defonce ^:private !node (atom nil))

(defn- database []
  (or @!node (throw (ex-info "Embedded XTDB fixture not started." {}))))

(defn- with-real-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f) (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- read-one [sql id]
  (let [rows (xt/q (database) [sql id])]
    (when (> (count rows) 1)
      (throw (ex-info "Unexpected duplicate XTDB rows." {:id id :rows rows})))
    (first rows)))

(defn- read-request [id]
  (read-one "SELECT * FROM requests WHERE _id = ?" id))

(defn- read-command [id]
  (read-one "SELECT * FROM gesso_commands WHERE _id = ?" id))

(defn- read-membership [id]
  (read-one "SELECT * FROM memberships WHERE _id = ?" id))

(defn- seed! [& table-doc-pairs]
  (xt/execute-tx (database)
                 (mapv (fn [[table doc]] [:put-docs table doc]) table-doc-pairs)))

(defn- real-environment
  ([data] (real-environment data nil))
  ([{:keys [request membership binding] :as data} opts]
   (let [constructs (atom 0)
         disclosures (atom 0)
         permitted? (atom true)
         configuration
         {:operation :request/claim
          :read-record! (fn [_ctx row-id] (read-command row-id))
          :may-disclose? (fn [_ctx _binding _record]
                           (swap! disclosures inc)
                           @permitted?)
          :construct! (fn [_ctx _binding]
                        (swap! constructs inc)
                        {:now now :plan (domain-plan request (when (:guard-membership? opts)
                                                               membership))
                         :outcome outcome})}]
     (assoc data :constructs constructs :disclosures disclosures
            :permitted? permitted?
            :operation (operation/build-operation configuration)))))

(defn- real-call [e mode]
  ((:operation e) {:biff.xtdb/node (database)}
   (invocation (:binding e) mode)))

(deftest real-xtdb-incorporation-replay-and-reconciliation-test
  (let [{:keys [request binding] :as data} (fixture-data)
        e (real-environment data {:guard-membership? false})]
    (seed! [:requests request])
    (let [result (real-call e :submit)]
      (is (= :incorporated (status result)))
      (is (= :committed (get-in result [:transaction :commit/status])))
      (is (= outcome (:outcome result))))
    (is (= (next-version request {:owner "helper-a"})
           (read-request (:xt/id request))))
    (is (= (resolution/command-record binding outcome now)
           (read-command (resolution/record-id binding))))
    (is (= :replay (status (real-call e :submit))))
    (is (= :replay (status (real-call e :reconcile))))
    (is (= 1 @(:constructs e)))))

(deftest real-xtdb-stale-membership-guard-keeps-command-unincorporated-test
  (let [{:keys [request membership binding] :as data} (fixture-data)
        e (real-environment data {:guard-membership? true})]
    (seed! [:requests request] [:memberships membership])
    ;; The *trusted* construction is deliberately stale. The generic wrapper
    ;; neither invents nor silently removes its application-owned guard.
    (xt/execute-tx (database)
                   (:tx-ops (tx/prepare {}
                                        {:commands [(revoke-command membership)]
                                         :emit false})))
    (is (= false (:active (read-membership (:xt/id membership)))))
    (let [result (real-call e :submit)]
      (is (= :unresolved (status result)))
      (is (not (contains? result :outcome)))
      (is (not (contains? result :retry?))))
    (is (= request (read-request (:xt/id request))))
    (is (nil? (read-command (resolution/record-id binding))))
    (is (= 1 @(:constructs e)))))

(deftest real-xtdb-lost-commit-ack-reconciles-from-stored-record-test
  (let [{:keys [request binding] :as data} (fixture-data)
        e (real-environment data nil)
        commit! tx/transact!
        submissions (atom 0)]
    (seed! [:requests request])
    ;; Execute the REAL commit but simulate loss of its acknowledgment. The
    ;; wrapper must reacquire durable evidence rather than executing again.
    (with-redefs [tx/transact! (fn [ctx plan]
                                 (swap! submissions inc)
                                 (commit! ctx plan)
                                 (throw (ex-info "acknowledgment lost" {})))]
      (let [result (real-call e :submit)]
        (is (= :replay (status result)))
        (is (= outcome (:outcome result)))))
    (is (= 1 @submissions))
    (is (= 1 @(:constructs e)))
    (is (= (next-version request {:owner "helper-a"})
           (read-request (:xt/id request))))
    (is (= (resolution/command-record binding outcome now)
           (read-command (resolution/record-id binding))))
    (is (= :replay (status (real-call e :reconcile))))
    (is (= 1 @(:constructs e)))))

(deftest real-xtdb-existing-record-refuses-reuse-and-disclosure-test
  (let [{:keys [request binding] :as data} (fixture-data)
        e (real-environment data nil)]
    (seed! [:requests request])
    (is (= :incorporated (status (real-call e :submit))))
    (let [another (assoc binding :arguments {:request-id (unique-id "different")})
          alternate-result ((:operation e) {:biff.xtdb/node (database)}
                            (invocation another :submit))]
      (is (= :incompatible (status alternate-result)))
      (is (not (contains? alternate-result :outcome))))
    (reset! (:permitted? e) false)
    (let [result (real-call e :submit)]
      (is (= :unavailable (status result)))
      (is (not (contains? result :outcome))))
    (is (= 1 @(:constructs e)))
    (is (= (next-version request {:owner "helper-a"})
           (read-request (:xt/id request))))))
