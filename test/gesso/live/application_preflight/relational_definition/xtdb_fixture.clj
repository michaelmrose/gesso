(ns gesso.live.application-preflight.relational-definition.xtdb-fixture
  "Genuine XTDB executor-bound fixtures for relational definition regression suites.

   Each invocation starts and closes its own node, preserving original isolation."
  (:require
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as resolved-adapter]
   [gesso.live.optimistic.resolution-assurance :as resolution-assurance]
   [gesso.live.optimistic.server :as optimistic-server]
   [gesso.model.resolved-operation :as resolved-model]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]
   [gesso.live.application-preflight.relational-definition.support :refer [version-keys initial-state relational-model]]))

(defn with-real-xtdb-root-step
  "Use the same actual resolved-operation -> executor -> resolution-assurance
   path as the operation-family-correspondence test, but submit the genuine
   evidence to a relational-definition handle. Each invocation owns its node."
  [f]
  (with-open [node (xtn/start-node {})]
    (let [suffix (str (random-uuid))
          request-id (str "v868-request-" suffix)
          member-id (str "v868-member-" suffix)
          principal-label "v868-private-helper"
          now "2026-10-13T12:00:00Z"
          before {:xt/id request-id :revision 0
                  :created-at "t0" :updated-at "t0" :owner "unclaimed"}
          member {:xt/id member-id :revision 0
                  :created-at "t0" :updated-at "t0" :active true}
          claimed (assoc before :revision 1 :updated-at now
                         :owner principal-label :membership-id member-id)
          principal (identity/principal principal-label)
          command-wire
          (protocol/command
           {:command-id (identity/command-id (str "v868-command-" suffix))
            :execution-id (identity/execution-id (str "v868-execution-" suffix))
            :operation :request/claim
            :arguments {:request-id request-id}
            :observed-basis {:tx-id 1 :system-time now}
            :scope [:request request-id]})
          operation
          (resolved-model/build-operation
           {:operation :request/claim
            :read-record! (fn [_ id]
                            (first (xt/q node
                                         ["SELECT * FROM gesso_commands WHERE _id = ?"
                                          id])))
            :may-disclose? (fn [& _] true)
            :construct! (fn [& _]
                          {:now now
                           :outcome {:status :claimed}
                           :plan {:commands [(command/update-command
                                              :requests :claim before claimed
                                              version-keys)]
                                  :guards [(command/guard :memberships member
                                                          version-keys)]
                                  :emit false}})})
          executor
          (resolved-adapter/build-executor
           {:operation :request/claim
            :resolved-operation operation
            :submission-mode! (fn [_] :submit)
            :project! (fn [_ _]
                        {:resolution :confirmed
                         :authoritative
                         (protocol/authoritative
                          {:presence :present
                           :basis {:tx-id 2 :system-time now}
                           :projection {:request/owner principal-label}})
                         :outcome :request/claimed})})
          server
          (optimistic-server/server
           {:principal-fn (fn [ctx] (:authenticated-principal ctx))
            :operations
            {:request/claim
             (optimistic-server/operation
              {:name :v868-claim-optimistic
               :operation :request/claim
               :execute! executor
               :settlement-contract
               {:confirmed {:outcomes #{:request/claimed}
                            :commit/status :committed}
                :already-incorporated {:outcomes #{:request/claimed}
                                       :commit/status :committed}}})}})
          modeled-state {[:requests request-id] before
                         [:memberships member-id] member}
          families
          [{:id :request/claim
            :principals [principal-label]
            :arguments [{:request-id request-id}]
            :enabled? (fn [state who {:keys [request-id]}]
                        (and (= principal-label who)
                             (= "unclaimed"
                                (:owner (get state [:requests request-id])))
                             (true? (:active (get state [:memberships member-id])))))
            :prepare (fn [state _ {:keys [request-id]}]
                       {:commands [(command/update-command
                                    :requests :claim
                                    (get state [:requests request-id])
                                    claimed version-keys)]
                        :guards [(command/guard
                                  :memberships (get state [:memberships member-id])
                                  version-keys)]
                        :emit false})}
           {:id :request/reassign
            :principals [:manager] :arguments [{:request-id request-id}]
            :enabled? (fn [& _] false)
            :prepare (fn [& _] (throw (ex-info "Disabled" {})))}
           {:id :request/cancel
            :principals [:manager] :arguments [{:request-id request-id}]
            :enabled? (fn [& _] false)
            :prepare (fn [& _] (throw (ex-info "Disabled" {})))}]
          relational-model
          {:initial-state modeled-state
           :families families
           :declarations [{:id :request/owner-member-active
                           :source-kind :requests
                           :source-where {:owner principal-label}
                           :reference-attr :membership-id
                           :allow-unset? false
                           :target-kind :memberships
                           :target-where {:active true}}]
           :options {:max-depth 1 :max-transitions 10}}]
      (xt/execute-tx node [[:put-docs :requests before]
                           [:put-docs :memberships member]])
      (fixture/with-closed-application
        {:operations
         (assoc (fixture/standard-operations)
                :request/claim
                (fixture/trusted-operation
                 :request/claim
                 {:published-change-topics #{:request}
                  :execute! executor}))}
        (fn [{:keys [options]}]
          (let [handle (sut/construct!
                        {:application-options options
                         :relational-model relational-model})
                evidence
                (:evidence
                 (resolution-assurance/run-and-check!
                  {:server server
                   :ctx {:biff.xtdb/node node
                         :authenticated-principal principal}
                   :command command-wire}))]
            (f {:handle handle
                :evidence evidence
                :request-id request-id
                :modeled-state modeled-state})))))))

(defn v892-with-authentic-nonroot-predecessor
  "Keep XTDB's genuine claim and evidence unchanged, but explore a modeled
   predecessor that must first transition into the actual captured prestate.
   Only construction is customized; the physical observation stays authentic."
  [f]
  (let [original-construct sut/construct!]
    (with-redefs
      [sut/construct!
       (fn [declaration]
         (let [state (get-in declaration [:relational-model :initial-state])
               request-key (first (filter #(= :requests (first %)) (keys state)))
               before (get state request-key)
               create-request
               {:id :request/reassign
                :principals [:manager]
                :arguments [{:request-id (second request-key)}]
                :enabled? (fn [state _ _]
                            (not (contains? state request-key)))
                :prepare (fn [_ _ _]
                           {:commands [(command/create
                                        :requests before version-keys)]
                            :guards [] :emit false})}
               with-predecessor
               (-> declaration
                   (update-in [:relational-model :initial-state] dissoc request-key)
                   (assoc-in [:relational-model :families 1] create-request)
                   (assoc-in [:relational-model :options :max-depth] 2)
                   (assoc-in [:relational-model :options :max-transitions] 30))]
           (original-construct with-predecessor)))]
      (with-real-xtdb-root-step f))))

