(ns gesso.live.optimistic.resolved-operation-correspondence-test
  "Bounded *cross-layer* correspondence for the real Live -> resolved operation
   -> model transaction -> embedded XTDB path.

   The existing model suites compare transaction plans with XTDB, and the
   Live adapter suites check settlements. Here one scenario checks BOTH from
   the same execution: its model-expected atomic document write set, the
   actual persisted state, command-record incorporation and the resulting
   protocol-v3 settlement. The model's serial paths are an oracle only for
   the admitted plans, not an independent specification of business policy.

   The fixture's Claim policy and projector are deliberately test-owned and
   trusted. These tests do not establish production HumanHelp authorization,
   correctness of all projectors, arbitrary schedules, temporal liveness,
   or universal correspondence. In particular, an unresolved failed commit
   must never be counted as a successful Live settlement."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolved-operation :as model-resolved]
   [gesso.model.tx-model :as reference]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private operation-id :request/claim)
(def ^:private now "2026-10-12T10:00:00Z")
(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private outcome {:status :claimed})
(defonce ^:private active-database (atom nil))

(defn- with-database [f]
  (with-open [database (xtn/start-node {})]
    (reset! active-database database)
    (try (f)
         (finally (reset! active-database nil)))))

(use-fixtures :once with-database)

(defn- database []
  (or @active-database
      (throw (ex-info "Real embedded XTDB test fixture not running." {}))))

(defn- row [table id]
  (let [rows (xt/q (database)
                   [(case table
                      :requests "SELECT * FROM requests WHERE _id = ?"
                      :memberships "SELECT * FROM memberships WHERE _id = ?"
                      :gesso_commands "SELECT * FROM gesso_commands WHERE _id = ?")
                    id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Multiple current documents at target." {:table table :id id})))
    (first rows)))

(defn- request [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- change [before attrs]
  (-> before
      (merge attrs)
      (update :revision inc)
      (assoc :updated-at now)))

(defn- claim-plan [request membership owner]
  {:commands [(command/update-command
               :requests :claim request (change request {:owner owner}) version)]
   :guards [(command/guard :memberships membership version)]
   :emit false})

(defn- fixture []
  (let [id (str (random-uuid))
        req (request (str "v801-request-" id))
        member (membership (str "v801-member-" id))]
    (xt/execute-tx (database) [[:put-docs :requests req]
                                [:put-docs :memberships member]])
    {:request req
     :membership member
     :initial {[:requests (:xt/id req)] req
               [:memberships (:xt/id member)] member}}))

(defn- browser-command [request-id command-id execution-id]
  (protocol/command
   {:command-id command-id
    :execution-id execution-id
    :operation operation-id
    :arguments {:request-id request-id}
    :observed-basis {:tx-id 1 :system-time "2026-10-12T09:00:00Z"}
    :scope [:request request-id]}))

(defn- new-command [request-id]
  (browser-command request-id
                   (identity/command-id (str (random-uuid)))
                   (identity/execution-id (str (random-uuid)))))

(defn- bound [principal command]
  {:command-id (:command-id command)
   :principal principal
   :operation operation-id
   :arguments (:arguments command)})

(defn- incorporation [binding plan]
  (resolution/incorporation-plan
   {:binding binding :now now :plan plan :outcome outcome}))

(defn- stored-state [initial bindings]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (row table id)]
                  [target document])))
        (concat (keys initial)
                (map (fn [b] [:gesso_commands (resolution/record-id b)]) bindings))))

(defn- server-for [request snapshot mode calls]
  (let [op (model-resolved/build-operation
            {:operation operation-id
             :read-record! (fn [_ctx id] (row :gesso_commands id))
             :may-disclose? (fn [_ctx _binding _record] true)
             :construct! (fn [_ctx binding]
                           (swap! calls update :construct inc)
                           {:now now
                            :plan (claim-plan request (:membership snapshot)
                                              (identity/raw-value (:principal binding)))
                            :outcome outcome})})
        executor (adapter/build-executor
                  {:operation operation-id
                   :resolved-operation op
                   :submission-mode! (fn [_context] @mode)
                   :project! (fn [context result]
                               (swap! calls update :project inc)
                               (let [stored (row :requests (:xt/id request))]
                                 {:resolution (if (= :replay (:status result))
                                                :already-incorporated :confirmed)
                                  :authoritative
                                  (protocol/authoritative
                                   {:presence :present
                                    :basis {:tx-id 2 :system-time now}
                                    :projection {:request/id (:xt/id stored)
                                                 :request/owner (:owner stored)}})
                                  :outcome :request/claimed}))})]
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
          :reconciled {:outcomes #{:request/claimed} :commit/status :committed}
          :already-incorporated {:outcomes #{:request/claimed} :commit/status :committed}}})}})))

(defn- invoke [app principal cmd]
  (server/run-command app {:authenticated-principal principal
                           :biff.xtdb/node (database)} cmd))

(defn- error-kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))

(defn- committed-settlement! [prepared cmd expected-owner expected-resolution]
  (is (server/prepared-send? prepared))
  (let [settlement (:settlement prepared)]
    (is (= expected-resolution (:resolution settlement)))
    (is (= (:command-id cmd) (:command-id settlement)))
    (is (= (:execution-id cmd) (:execution-id settlement)))
    (is (= :request/claimed (:outcome settlement)))
    (is (= expected-owner
           (get-in settlement [:authoritative :projection :request/owner])))))

(deftest live-incorporation-corresponds-to-model-and-persisted-state
  (let [{:keys [request membership initial] :as f} (fixture)
        principal (identity/principal "helper-v801-a")
        cmd (new-command (:xt/id request))
        binding (bound principal cmd)
        plan (claim-plan request membership (identity/raw-value principal))
        incorporation (incorporation binding plan)
        model (reference/step initial (:plan incorporation))
        mode (atom :submit)
        calls (atom {:construct 0 :project 0})
        app (server-for request f mode calls)]
    (is (= :committed (:status model)))
    (let [prepared (invoke app principal cmd)]
      (committed-settlement! prepared cmd "helper-v801-a" :confirmed))
    (is (= (:after model) (stored-state initial [binding])))
    (is (= (:record incorporation)
           (row :gesso_commands (resolution/record-id binding))))
    (is (= {:construct 1 :project 1} @calls))))

(deftest same-command-replay-changes-execution-id-but-not-durable-state
  (let [{:keys [request membership initial] :as f} (fixture)
        principal (identity/principal "helper-v801-a")
        cmd (new-command (:xt/id request))
        binding (bound principal cmd)
        model (reference/step initial
                              (:plan (incorporation
                                      binding (claim-plan request membership
                                                          "helper-v801-a"))))
        mode (atom :submit)
        calls (atom {:construct 0 :project 0})
        app (server-for request f mode calls)]
    (committed-settlement! (invoke app principal cmd)
                           cmd "helper-v801-a" :confirmed)
    (let [before (stored-state initial [binding])
          retried (browser-command (:xt/id request)
                                   (:command-id cmd)
                                   (identity/execution-id (str (random-uuid))))]
      (reset! mode :reconcile)
      (committed-settlement! (invoke app principal retried)
                             retried "helper-v801-a" :already-incorporated)
      (is (= before (stored-state initial [binding])))
      (is (= (:after model) before))
      (is (= {:construct 1 :project 2} @calls)))))

(deftest two-distinct-live-claims-match-one-reference-serial-order
  (doseq [owners [["helper-v801-a" "helper-v801-b"]
                  ["helper-v801-b" "helper-v801-a"]]]
    (testing (str "first submitted principal " (first owners))
      (let [{:keys [request membership initial] :as f} (fixture)
            principals (mapv identity/principal owners)
            commands (mapv (fn [_] (new-command (:xt/id request))) owners)
            bindings (mapv bound principals commands)
            plans (mapv (fn [binding owner]
                          (:plan (incorporation binding
                                                (claim-plan request membership owner))))
                        bindings owners)
            expected (reference/explore-orders initial plans)
            mode (atom :submit)
            calls (atom {:construct 0 :project 0})
            apps [(server-for request f mode calls)
                  (server-for request f mode calls)]
            first-result (invoke (first apps) (first principals) (first commands))
            second-error (error-kind
                          #(invoke (second apps) (second principals) (second commands)))
            expected-state (:after (reference/step initial (first plans)))
            actual (stored-state initial bindings)]
        (committed-settlement! first-result (first commands) (first owners) :confirmed)
        (is (= :resolution-not-settleable second-error))
        (is (= expected-state actual))
        (is (some (fn [path]
                    (and (= [0 1] (mapv :plan-index (:trace path)))
                         (= [:committed :rejected] (mapv :status (:trace path)))
                         (= actual (:final-state path))))
                  (:paths expected)))
        (is (nil? (row :gesso_commands
                       (resolution/record-id (second bindings)))))
        (is (= {:construct 2 :project 1} @calls))))))

(deftest membership-revocation-makes-live-claim-unresolved-without-command-record
  (let [{:keys [request membership initial] :as f} (fixture)
        principal (identity/principal "helper-v801-a")
        cmd (new-command (:xt/id request))
        binding (bound principal cmd)
        prepared-plan (:plan (incorporation binding
                                           (claim-plan request membership
                                                       "helper-v801-a")))
        changed-member (change membership {:active false})
        revocation {:commands [(command/update-command
                                :memberships :revoke membership changed-member version)]
                    :guards [] :emit false}
        model-after-revoke (:after (reference/step initial revocation))
        model-after-claim (reference/step model-after-revoke prepared-plan)
        mode (atom :submit)
        calls (atom {:construct 0 :project 0})
        app (server-for request f mode calls)]
    (is (= :committed (:status (reference/step initial revocation))))
    (xt/execute-tx (database) (:tx-ops
                             (tx/prepare {} revocation)))
    (is (= :rejected (:status model-after-claim)))
    (is (= :resolution-not-settleable (error-kind #(invoke app principal cmd))))
    (is (= model-after-revoke (stored-state initial [])))
    (is (nil? (row :gesso_commands (resolution/record-id binding))))
    (is (= {:construct 1 :project 0} @calls))))
