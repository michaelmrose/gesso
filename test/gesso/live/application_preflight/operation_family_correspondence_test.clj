(ns gesso.live.application-preflight.operation-family-correspondence-test
  "Regressions for registered operation families and semantic-input closure.

   A finite operation family must match the exact executor from a freshly
   checked application assembly. An observed XTDB transaction can demonstrate
   bounded preparation/execution correspondence, but not domain-policy
   completeness, truthful projections, or universal refinement."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.operation-family-correspondence :as correspondence]
   [gesso.live.application-preflight.formal-family :as formal-family]
   [gesso.model.formal-check :as formal-check]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.resolution-assurance :as assurance]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.tx-model :as tx-model]
   [gesso.model.resolved-operation :as resolved]
   [gesso.model.resolved-family :as shared-family]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(def ^:private operation-id :request/claim)
(def ^:private now "2026-10-12T12:00:00Z")
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private principal (identity/principal "v829-principal-private"))

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

(defn- test-operation
  ([] (test-operation {}))
  ([{:keys [false-projection?]}]
   (let [suffix (str (random-uuid))
         request-id (str "v829-request-private-" suffix)
         before {:xt/id request-id :revision 0
                 :created-at "t0" :updated-at "t0" :owner "unclaimed"}
         membership {:xt/id (str "v829-member-" suffix)
                     :revision 0 :created-at "t0" :updated-at "t0"
                     :active true}
         claimed (assoc before :revision 1 :updated-at now
                        :owner "v829-principal-private")
         cmd (protocol/command
              {:command-id (identity/command-id (str "v829-command-" suffix))
               :execution-id (identity/execution-id (str "v829-execution-" suffix))
               :operation operation-id
               :arguments {:request-id request-id}
               :observed-basis {:tx-id 1 :system-time now}
               :scope [:request request-id]})
         calls (atom {:construct 0 :disclose 0 :project 0 :read 0})
         model (resolved/build-operation
                {:operation operation-id
                 :read-record! (fn [_ id]
                                 (swap! calls update :read inc)
                                 (row :gesso_commands id))
                 :may-disclose? (fn [_ _ _]
                                  (swap! calls update :disclose inc)
                                  true)
                 :construct! (fn [_ _]
                               (swap! calls update :construct inc)
                               {:now now
                                :outcome {:status :claimed}
                                :plan {:commands [(command/update-command
                                                   :requests :claim before claimed
                                                   version)]
                                       :guards [(command/guard
                                                 :memberships membership version)]
                                       :emit false}})})
         executor (adapter/build-executor
                   {:operation operation-id
                    :resolved-operation model
                    :submission-mode! (fn [_] :submit)
                    :project! (fn [_ result]
                                (swap! calls update :project inc)
                                {:resolution (if (= :replay (:status result))
                                               :already-incorporated :confirmed)
                                 :authoritative
                                 (protocol/authoritative
                                  {:presence :present
                                   :basis {:tx-id 2 :system-time now}
                                   :projection
                                   {:request/owner
                                    (if false-projection?
                                      "deliberately-false-projection"
                                      (:owner (row :requests request-id)))}})
                                 :outcome :request/claimed})})]
     (xt/execute-tx @!node [[:put-docs :requests before]
                             [:put-docs :memberships membership]])
     {:request-id request-id
      :before before :claimed claimed :membership membership :command cmd
      :model model :executor executor :calls calls
      :server (server-with executor)})))

(defn- invoke! [{:keys [server command]}]
  (assurance/run-and-check!
   {:server server
    :ctx {:biff.xtdb/node @!node :authenticated-principal principal}
    :command command}))

(defn- with-application [executor f]
  (fixture/with-closed-application
   {:operations
    (assoc (fixture/standard-operations)
           operation-id
           (fixture/trusted-operation
            operation-id
            {:published-change-topics #{:request}
             :execute! executor}))}
   f))

(defn- kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))

(defn- modeled-families [{:keys [request-id before claimed membership]}]
  [{:id :request/claim
    :principals ["v829-principal-private"]
    :arguments [{:request-id request-id}]
    :enabled? (fn [state who {:keys [request-id]}]
                (and (= who "v829-principal-private")
                     (= "unclaimed" (:owner (get state [:requests request-id])))
                     (true? (:active (get state [:memberships (:xt/id membership)])))))
    :prepare (fn [state _ {:keys [request-id]}]
               {:commands [(command/update-command
                             :requests :claim
                             (get state [:requests request-id])
                             claimed version)]
                :guards [(command/guard :memberships
                                        (get state [:memberships (:xt/id membership)])
                                        version)]
                :emit false})}
   {:id :request/reassign
    :principals [:manager]
    :arguments [{:request-id request-id}]
    :enabled? (fn [& _] false)
    :prepare (fn [& _] (throw (ex-info "Disabled" {})))}
   {:id :request/cancel
    :principals [:manager]
    :arguments [{:request-id request-id}]
    :enabled? (fn [& _] false)
    :prepare (fn [& _] (throw (ex-info "Disabled" {})))}])

(defn- model-state [{:keys [request-id before membership]}]
  {[:requests request-id] before
   [:memberships (:xt/id membership)] membership})

(defn- model-rules [example]
  [{:id :request/owner-active
    :holds? (fn [state]
              (let [request (get state [:requests (:request-id example)])]
                (or (= "unclaimed" (:owner request))
                    (true? (:active (get state [:memberships (get-in example [:membership :xt/id])]))))))}])

(deftest genuine-live-handle-comparison-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (correspondence/build-comparator
                          assembly (model-state example) (modeled-families example)
                          (model-rules example) {:max-depth 1 :max-transitions 10})
              handle (:evidence (invoke! example))
              match (comparator handle)]
          (is (= :matched (:status match)))
          (is (= :exact-observed-domain-plan-match (:reason match)))
          (is (= :same-current-registered-executor (:executor-correspondence match)))
          (is (= :holds-in-explored-model (:bounded-invariant-model match)))
          (is (= :request/claim (:operation match)))
          (is (= :committed (:physical-status match)))
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(comparator (Object.))))))))))

(deftest rejected-unadmitted-principal-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (assoc-in (modeled-families example)
                                 [0 :principals] ["not-the-executing-principal"])
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) {:max-depth 1 :max-transitions 10})
              handle (:evidence (invoke! example))]
          (is (= :execution-outside-admitted-family-domain
                 (kind #(comparator handle)))))))))

(deftest mismatched-admitted-callback-is-a-counterexample-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (update-in (modeled-families example) [0 :prepare]
                                  (fn [original]
                                    (fn [state principal arguments]
                                      (update (original state principal arguments)
                                              :guards (constantly [])))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) {:max-depth 1 :max-transitions 10})
              handle (:evidence (invoke! example))
              result (comparator handle)]
          (is (= :counterexample (:status result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result)))
          (is (false? (:matching-guards? result))))))))

(deftest invariant-failure-prevents-admission-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (is (= :modeled-invariant-violation
               (kind #(correspondence/build-comparator
                        assembly (model-state example) (modeled-families example)
                        [{:id :test/always-false :holds? (constantly false)}]
                        {:max-depth 1 :max-transitions 10}))))))))

(def ^:private small-options {:max-depth 1 :max-transitions 10})

(defn- build! [assembly example]
  (correspondence/build-comparator
   assembly (model-state example) (modeled-families example)
   (model-rules example) small-options))

(deftest admission-does-not-execute-business-callbacks-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [before @(:calls example)
              comparator (build! assembly example)]
          (is (fn? comparator))
          (is (= before @(:calls example)))
          (is (= "unclaimed" (:owner (row :requests (:request-id example)))))
          (is (nil? (row :gesso_commands
                         (identity/raw-value
                          (get-in example [:command :command-id]))))))))))

(deftest matched-result-is-scoped-and-not-certification-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              result (comparator (:evidence (invoke! example)))
              scope (:assurance-scope result)]
          (is (= :matched (:status result)))
          (is (true? (:matching-guards? result)))
          (is (true? (:matching-commands? result)))
          (is (= :exact (:registered-coverage result)))
          (is (= :holds-in-explored-model (:bounded-invariant-model result)))
          (is (= correspondence/assurance-scope scope))
          (is (contains? (:not-established scope)
                         :correctness-or-completeness-of-business-policy-guards))
          (is (contains? (:not-established scope)
                         :unbounded-execution-refinement))
          (is (contains? (:not-established scope)
                         :equality-of-modeled-and-observed-initial-states))
          (is (not (contains? result :binding)))
          (is (not (contains? result :observation))))))))

(deftest opaque-handle-cannot-be-forged-or-replaced-by-report-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))]
          (is (= :matched (:status (comparator handle))))
          (doseq [not-handle [nil (Object.) {} {:status :bounded-execution-correspondence}
                              (assurance/inspect-evidence handle)]]
            (is (= :unrecognized-executor-bound-evidence
                   (kind #(comparator not-handle)))))
          ;; Merely inspecting a valid witness cannot make a second handle.
          (is (= :matched (:status (comparator handle)))))))))

(deftest second-genuine-adapter-does-not-inherit-first-evidence-test
  (let [first-example (test-operation)
        other-example (test-operation)
        other-handle (:evidence (invoke! other-example))]
    (with-application (:executor first-example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly first-example)]
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(comparator other-handle))))
          (is (= "unclaimed"
                 (:owner (row :requests (:request-id first-example))))))))))

(deftest executor-with-copied-metadata-cannot-impersonate-original-test
  (let [example (test-operation)
        copied (with-meta (fn [_] (throw (ex-info "Should never execute" {})))
                 (meta (:executor example)))
        handle (:evidence (invoke! example))]
    (with-application copied
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)]
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(comparator handle)))))))))

(deftest modified-metadata-clone-does-not-keep-construction-identity-test
  (let [example (test-operation)
        original (:executor example)
        clone (with-meta original
                (assoc (meta original) :v829/copied true))
        handle (:evidence (invoke! example))]
    (is (not (identical? original clone)))
    (with-application clone
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)]
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(comparator handle)))))))))

(deftest argument-not-in-admitted-finite-domain-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [models (assoc-in (modeled-families example) [0 :arguments]
                               [{:request-id "some-other-request"}])
              comparator (correspondence/build-comparator
                          assembly (model-state example) models
                          (model-rules example) small-options)
              handle (:evidence (invoke! example))]
          (is (= :execution-outside-admitted-family-domain
                 (kind #(comparator handle)))))))))

(deftest disabled-modeled-operation-is-observed-counterexample-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [models (assoc-in (modeled-families example) [0 :enabled?]
                               (fn [& _] false))
              comparator (correspondence/build-comparator
                          assembly (model-state example) models
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))]
          (is (= :counterexample (:status result)))
          (is (= :actual-execution-while-model-disabled (:reason result)))
          (is (= :request/claim (:operation result))))))))

(deftest wrong-modeled-ownership-mutation-is-counterexample-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [models (update-in (modeled-families example) [0 :prepare]
                                (fn [original]
                                  (fn [state principal arguments]
                                    (update-in (original state principal arguments)
                                               [:commands 0 :model/after :updated-at]
                                               (constantly "wrong-timestamp")))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) models
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))]
          (is (= :counterexample (:status result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result)))
          (is (false? (:matching-commands? result)))
          (is (true? (:matching-guards? result))))))))

(deftest incomplete-registered-families-never-get-admitted-test
  (let [example (test-operation)
        calls (atom 0)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (-> (modeled-families example)
                           (update-in [0 :prepare]
                                      (fn [_] (fn [& _]
                                                (swap! calls inc)
                                                (throw (ex-info "Should not run" {}))))))
              missing (subvec families 0 2)
              error (try (correspondence/build-comparator
                          assembly (model-state example) missing
                          (model-rules example) small-options)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
          (is (= :operation-identity-coverage-mismatch
                 (get (ex-data error) :error/kind)))
          (is (zero? @calls)))))))

(deftest invented-family-operation-never-gets-admitted-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (conj (modeled-families example)
                             {:id :request/invented
                              :principals [:manager]
                              :arguments [{:request-id (:request-id example)}]
                              :enabled? (constantly false)
                              :prepare (fn [& _] (throw (ex-info "Disabled" {})))})]
          (is (= :operation-identity-coverage-mismatch
                 (kind #(correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)))))))))

(deftest invariant-failure-blocks-construction-before-execution-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (is (= :modeled-invariant-violation
               (kind #(correspondence/build-comparator
                        assembly (model-state example)
                        (modeled-families example)
                        [{:id :test/invalid :holds? (constantly false)}]
                        small-options))))
        (is (= "unclaimed" (:owner (row :requests (:request-id example)))))
        (is (= 0 (:construct @(:calls example))))))))

(deftest unsupported-model-construction-is-not-admitted-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (assoc-in (modeled-families example)
                                 [0 :prepare]
                                 (fn [& _] {:commands []
                                              :guards [] :emit false
                                              :assertions ["SELECT false"]}))]
          (is (some? (try (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)
                          nil
                          (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest browser-artifact-drift-invalidates-admitted-comparator-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly artifact-path]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))]
          (is (= :matched (:status (comparator handle))))
          (spit artifact-path "externally modified: invalid artifact receipt\n")
          (is (some? (try (comparator handle) nil
                          (catch clojure.lang.ExceptionInfo e e))))
          (is (not (application/application-assembly? assembly))))))))

(deftest executor-change-after-admission-invalidates-existing-comparator-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))
              original application/check-application-assembly
              replacement (fn [_] :not-actually-an-adapter)]
          (is (= :matched (:status (comparator handle))))
          ;; Fault-inject a changed freshly checked executor registry. Do not
          ;; mutate production assembly internals or claim this is real reload.
          (with-redefs [application/check-application-assembly
                        (fn [opts]
                          (assoc-in (original opts)
                                    [:analysis :operation-acquisition-assembly
                                     :execution-assembly :server :operations
                                     operation-id :execute!]
                                    replacement))]
            (is (= :registered-executor-replaced
                   (kind #(comparator handle))))))))))

(deftest matching-model-does-not-certify-an-untruthful-projection-test
  (let [example (test-operation {:false-projection? true})]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              result (assurance/run-and-check!
                      {:server (:server example)
                       :ctx {:biff.xtdb/node @!node
                             :authenticated-principal principal}
                       :command (:command example)})
              comparison (comparator (:evidence result))]
          (is (= :matched (:status comparison)))
          (is (= "v829-principal-private"
                 (:owner (row :requests (:request-id example)))))
          (is (not= "v829-principal-private"
                    (get-in result [:prepared-send :settlement
                                    :authoritative :projection :request/owner])))
          (is (contains? (get-in comparison [:assurance-scope :not-established])
                         :correctness-of-projection-callbacks)))))))

;; v866 reconstructs the controlled pre-transaction comparison contract added
;; in v865. These are observations of an actual executor-bound XTDB attempt,
;; not a proof that the modeled root came from a pinned database snapshot.

(def ^:private root-state-key :declared-root-state-comparison)

(defn- root-comparison [result]
  (get result root-state-key))

(deftest declared-root-equals-genuine-pretransaction-dependencies-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              result (comparator (:evidence (invoke! example)))
              root (root-comparison result)]
          (is (= :matched (:status result)))
          (is (= :committed (:physical-status result)))
          (is (= :matches-captured-pretransaction-state (:status root)))
          (is (= :exact-clojure-state-map-equality (:relation root)))
          (is (= :declared-model-root-vs-controlled-dependency-snapshot
                 (:scope root)))
          ;; A successful transaction changed XTDB, but the observation was
          ;; captured *before* the authoritative transaction was submitted.
          (is (= (:claimed example) (row :requests (:request-id example))))
          (is (= "unclaimed"
                 (:owner (get (model-state example)
                              [:requests (:request-id example)]))))
          (is (not (contains? result :observation))))))))

(deftest additional-declared-root-document-breaks-exact-equality-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [extra [:private/unobserved "v866-extra-private"]
              root (assoc (model-state example)
                          extra {:xt/id "v866-extra-private" :private/secret "not-public"})
              comparator (correspondence/build-comparator
                          assembly root (modeled-families example)
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))]
          (is (= :matched (:status result)))
          (is (true? (:matching-guards? result)))
          (is (true? (:matching-commands? result)))
          (is (= :differs-from-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (not (contains? result :initial-state)))
          (is (not (.contains (pr-str result) "v866-extra-private")))
          (is (not (.contains (pr-str result) "not-public"))))))))

(deftest altered-declared-root-document-value-breaks-equality-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [request-key [:requests (:request-id example)]
              root (assoc-in (model-state example)
                             [request-key :updated-at] "v866-modeled-only-timestamp")
              comparator (correspondence/build-comparator
                          assembly root (modeled-families example)
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))]
          (is (= :matched (:status result)))
          (is (= :differs-from-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (not (.contains (pr-str result) "v866-modeled-only-timestamp"))))))))

(deftest root-equality-does-not-turn-plan-counterexample-into-success-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (update-in (modeled-families example) [0 :prepare]
                                  (fn [original]
                                    (fn [state who args]
                                      (assoc (original state who args) :guards []))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))]
          (is (= :counterexample (:status result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result)))
          (is (false? (:matching-guards? result)))
          (is (= :matches-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (= :not-established
                 (get-in result [root-state-key :production-trace-refinement]))))))))

(deftest root-state-report-adds-no-extra-model-or-physical-execution-test
  (let [example (test-operation)
        model-evaluations (atom 0)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (update-in (modeled-families example) [0 :prepare]
                                  (fn [original]
                                    (fn [state who args]
                                      (swap! model-evaluations inc)
                                      (original state who args))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)
              handle (:evidence (invoke! example))
              callbacks-before @(:calls example)
              model-before @model-evaluations
              first-result (comparator handle)
              after-first @model-evaluations
              second-result (comparator handle)
              after-second @model-evaluations]
          (is (pos? model-before))
          ;; The established plan comparator deliberately runs :prepare once
          ;; per observation. Root equality must not cause an *extra* run.
          (is (= 1 (- after-first model-before)))
          (is (= 1 (- after-second after-first)))
          (is (= callbacks-before @(:calls example)))
          (is (= :matches-captured-pretransaction-state
                 (get-in first-result [root-state-key :status])))
          (is (= (root-comparison first-result)
                 (root-comparison second-result)))
          (is (= :matched (:status second-result))))))))

(deftest matching-root-does-not-certify-incorrect-live-projection-test
  (let [example (test-operation {:false-projection? true})]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              result (comparator (:evidence (invoke! example)))]
          (is (= :matched (:status result)))
          (is (= :matches-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (contains? (get-in result [:assurance-scope :not-established])
                         :correctness-of-projection-callbacks))
          (is (= :not-established
                 (get-in result [root-state-key :non-root-state-correspondence]))))))))

(deftest root-state-scope-does-not-promote-global-xtdb-or-trace-claims-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [result ((build! assembly example) (:evidence (invoke! example)))
              scope (:assurance-scope result)]
          (is (= correspondence/assurance-scope scope))
          (is (contains? (:checked scope)
                         :declared-root-vs-observed-pretransaction-state-value-equality))
          (is (contains? (:assumed scope)
                         :controlled-execution-observation-and-unpinned-xtdb-reads))
          (doseq [limit [:authenticated-xtdb-history-or-global-isolation
                         :unbounded-execution-refinement
                         :whole-application-certification]]
            (is (contains? (:not-established scope) limit)))
          (is (= :not-established
                 (get-in result [root-state-key :production-trace-refinement]))))))))

(deftest arbitrary-values-and-detached-reports-cannot-obtain-root-equality-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))
              report (assurance/inspect-evidence handle)]
          (doseq [forged [nil (Object.) {} report
                          {:declared-root-state-comparison
                           {:status :matches-captured-pretransaction-state}}]]
            (is (= :unrecognized-executor-bound-evidence
                   (kind #(comparator forged)))))
          (is (= :matches-captured-pretransaction-state
                 (get-in (comparator handle) [root-state-key :status]))))))))

(deftest stale-application-blocks-root-comparison-before-observation-use-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly artifact-path]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))]
          (is (= :matches-captured-pretransaction-state
                 (get-in (comparator handle) [root-state-key :status])))
          (spit artifact-path "v866-tampered-runtime-artifact\n")
          (is (not (application/application-assembly? assembly)))
          (is (some? (try (comparator handle)
                          nil
                          (catch clojure.lang.ExceptionInfo e e)))))))))

;; v870: root-poststate correspondence is a local value comparison on a
;; controlled observation. Keep a genuine XTDB path and separately exercise
;; the pure status branches that cannot be injected through opaque handles.

(def ^:private poststate-key :declared-root-poststate-comparison)

(defn- poststate-comparison [result]
  (get result poststate-key))

(deftest genuine-xtdb-commit-matches-modeled-poststate-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [handle (:evidence (invoke! example))
              comparison ((build! assembly example) handle)
              post (poststate-comparison comparison)]
          (is (= :matched (:status comparison)))
          (is (= :committed (:physical-status comparison)))
          (is (= :matches-captured-pretransaction-state
                 (get-in comparison [root-state-key :status])))
          (is (= :matches-captured-posttransaction-dependencies (:status post)))
          (is (= :exact-clojure-domain-document-map-equality (:relation post)))
          (is (= :one-controlled-serial-root-step-vs-captured-dependencies
                 (:scope post)))
          (is (= (:claimed example) (row :requests (:request-id example))))
          (is (false? (:poststate-data-exposed? post)))
          (is (not (contains? comparison :observations))))))))

(deftest mismatched-declared-root-withholds-poststate-equality-test
  (let [example (test-operation)
        secret "v870-model-only-private-document"]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [root (assoc (model-state example)
                          [:private/extra secret]
                          {:xt/id secret :secret "v870-do-not-disclose"})
              comparator (correspondence/build-comparator
                          assembly root (modeled-families example)
                          (model-rules example) small-options)
              comparison (comparator (:evidence (invoke! example)))]
          (is (= :matched (:status comparison)))
          (is (= :differs-from-captured-pretransaction-state
                 (get-in comparison [root-state-key :status])))
          (is (= :declared-root-not-established-equal-to-observation
                 (get-in comparison [poststate-key :status])))
          (is (not (.contains (pr-str comparison) secret)))
          (is (not (.contains (pr-str comparison) "v870-do-not-disclose"))))))))

(deftest plan-counterexample-cannot-be-promoted-by-matching-root-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (update-in (modeled-families example) [0 :prepare]
                                  (fn [original]
                                    (fn [state who args]
                                      (assoc (original state who args)
                                             :guards []))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)
              comparison (comparator (:evidence (invoke! example)))]
          (is (= :counterexample (:status comparison)))
          (is (= :modeled-and-submitted-plans-disagree (:reason comparison)))
          (is (= :matches-captured-pretransaction-state
                 (get-in comparison [root-state-key :status])))
          (is (= :submitted-plan-not-matched
                 (get-in comparison [poststate-key :status]))))))))

(deftest matching-poststate-does-not-certify-live-projection-test
  (let [example (test-operation {:false-projection? true})]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparison ((build! assembly example)
                          (:evidence (invoke! example)))]
          (is (= :matched (:status comparison)))
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in comparison [poststate-key :status])))
          (is (contains? (get-in comparison [:assurance-scope :not-established])
                         :correctness-of-projection-callbacks))
          (is (= :not-established
                 (get-in comparison [poststate-key :production-trace-refinement]))))))))

(deftest poststate-check-does-not-reexecute-model-or-domain-test
  (let [example (test-operation)
        !prepares (atom 0)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [families (update-in (modeled-families example) [0 :prepare]
                                  (fn [original]
                                    (fn [state who args]
                                      (swap! !prepares inc)
                                      (original state who args))))
              comparator (correspondence/build-comparator
                          assembly (model-state example) families
                          (model-rules example) small-options)
              handle (:evidence (invoke! example))
              domain-before @(:calls example)
              before @!prepares
              first-result (comparator handle)
              middle @!prepares
              second-result (comparator handle)]
          ;; compare-family-preparation intentionally invokes :prepare once
          ;; per comparison. The new reference step must NOT invoke it again.
          (is (= 1 (- middle before)))
          (is (= 1 (- @!prepares middle)))
          (is (= domain-before @(:calls example)))
          (is (= (poststate-comparison first-result)
                 (poststate-comparison second-result)))
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in second-result [poststate-key :status]))))))))

(deftest poststate-evidence-rejects-forgeries-and-stale-assembly-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly artifact-path]}]
        (let [comparator (build! assembly example)
              handle (:evidence (invoke! example))
              detached (assurance/inspect-evidence handle)]
          (doseq [fake [nil {} (Object.) detached
                        {poststate-key {:status :matches-captured-posttransaction-dependencies}}]]
            (is (= :unrecognized-executor-bound-evidence
                   (kind #(comparator fake)))))
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in (comparator handle) [poststate-key :status])))
          (spit artifact-path "v870-modified-artifact\n")
          (is (not (application/application-assembly? assembly)))
          (is (some? (try (comparator handle)
                          nil
                          (catch clojure.lang.ExceptionInfo e e)))))))))

(defn- pure-poststate-fixture []
  (let [document {:xt/id "v870-private-pure-document" :revision 0
                  :created-at "t0" :updated-at "t0" :secret "v870-secret-value"}
        plan {:commands [(command/create :requests document version)]
              :guards [] :emit false}
        expected (get-in (tx-model/step {} plan) [:after])
        record {:xt/id "v870-private-command"}
        after-with-record (assoc expected [:gesso_commands (:xt/id record)] record)
        observation (fn [state]
                      {:submitted-incorporation {:plan plan}
                       :observations {:state state}})
        compare! (fn [state comparison root-match?]
                   (#'correspondence/compare-root-poststate!
                    {} (observation state) record comparison root-match?))]
    {:expected expected :with-record after-with-record
     :compare! compare!}))

(deftest pure-reference-branch-statuses-distinguish-state-and-outcome-test
  (let [{:keys [expected with-record compare!]} (pure-poststate-fixture)
        matched {:status :matched :physical-status :committed}]
    (is (= :matches-captured-posttransaction-dependencies
           (:status (compare! expected matched true))))
    ;; Ignore only the canonical separate durable command-result target.
    (is (= :matches-captured-posttransaction-dependencies
           (:status (compare! with-record matched true))))
    ;; An unrelated domain document must NOT be silently discarded along
    ;; with the canonical command-result record.
    (is (= :differs-from-captured-posttransaction-dependencies
           (:status (compare! (assoc with-record
                                     [:requests "unexpected"]
                                     {:xt/id "unexpected"})
                              matched true))))
    (is (= :differs-from-captured-posttransaction-dependencies
           (:status (compare! {} matched true))))
    (is (= :modeled-and-physical-outcomes-disagree
           (:status (compare! {} (assoc matched :physical-status :rejected) true))))
    (is (= :physical-outcome-not-reported
           (:status (compare! {} (dissoc matched :physical-status) true))))
    (is (= :submitted-plan-not-matched
           (:status (compare! {} (assoc matched :status :counterexample) true))))
    (is (= :declared-root-not-established-equal-to-observation
           (:status (compare! {} matched false))))))

(deftest pure-reference-rejected-step-and-redaction-test
  (let [document {:xt/id "v870-existing-document" :revision 0
                  :created-at "t0" :updated-at "t0" :secret "v870-private-rejection"}
        target [:requests (:xt/id document)]
        state {target document}
        plan {:commands [(command/create :requests document version)]
              :guards [] :emit false}
        record {:xt/id "v870-command-rejected"}
        observation {:submitted-incorporation {:plan plan}
                     :observations {:state (assoc state
                                                 [:gesso_commands (:xt/id record)]
                                                 record)}}
        result (#'correspondence/compare-root-poststate!
                state observation record
                {:status :matched :physical-status :rejected} true)]
    (is (= :matches-captured-posttransaction-dependencies (:status result)))
    (is (false? (:poststate-data-exposed? result)))
    (is (not (.contains (pr-str result) "v870-private-rejection")))
    (is (= :not-established (:pinned-xtdb-snapshot result)))
    (is (= :not-established (:production-trace-refinement result)))))

(deftest poststate-assurance-scope-remains-finite-and-local-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [comparison ((build! assembly example)
                          (:evidence (invoke! example)))
              scope (:assurance-scope comparison)
              post (poststate-comparison comparison)]
          (is (= correspondence/assurance-scope scope))
          (is (contains? (:checked scope)
                         :single-root-step-vs-observed-posttransaction-dependency-values))
          (doseq [limit [:posttransaction-equality-for-non-root-model-states
                         :authenticated-xtdb-history-or-global-isolation
                         :unbounded-execution-refinement
                         :whole-application-certification]]
            (is (contains? (:not-established scope) limit)))
          (is (= :not-established (:pinned-xtdb-snapshot post)))
          (is (= :not-established (:production-trace-refinement post)))
          (is (= :one-controlled-serial-root-step-vs-captured-dependencies
                 (:scope post))))))))

;; v878 reconstructs the public pair-observation contract at the boundary of
;; executor-issued evidence. The synthetic cases isolate redacted state-map
;; comparison; the last cases exercise genuine, separately issued XTDB handles.

(defn- pair-synthetic-fixture
  "No real execution is claimed by this fixture: only the capture comparison
   classification is isolated with controlled reports."
  [earlier-before earlier-after later-before later-after]
  (let [earlier (Object.)
        later (Object.)
        unknown (Object.)
        executor (fn [_] nil)
        earlier-command-id "v878-earlier-command-private"
        later-command-id "v878-later-command-private"
        state-record (fn [id] {:xt/id id})
        observation (fn [initial final command-id]
                      {:observation
                       {:initial-state initial
                        :submitted-incorporation {:record (state-record command-id)}
                        :observations
                        {:state (assoc final
                                       [resolution/command-entity-type command-id]
                                       (state-record command-id))}}})
        !observations (atom {earlier (observation earlier-before earlier-after earlier-command-id)
                             later (observation later-before later-after later-command-id)})
        !inspections (atom 0)
        !callbacks (atom 0)
        run! (fn [f]
               (with-redefs-fn
                 {#'application/application-assembly? (constantly true)
                  #'application/check-application-assembly
                  (fn [_] {:analysis {:operations {:request/claim {}}}})
                  #'correspondence/current-executors!
                  (fn [_ _] {:request/claim executor})
                  #'assurance/inspect-evidence-for-executor
                  (fn [handle expected]
                    (swap! !inspections inc)
                    (when (identical? executor expected)
                      (get @!observations handle)))
                  #'resolution/require-record identity}
                 f))]
    {:earlier earlier :later later :unknown unknown
     :observations !observations :inspections !inspections :callbacks !callbacks
     :run! run!
     :compare! (fn [a b] (run! #(correspondence/compare-observation-pair! {} a b)))}))

(def ^:private pair-request-key [:requests "v878-request-private"])
(def ^:private pair-auxiliary-key [:memberships "v878-membership-private"])
(def ^:private pair-before {:xt/id "v878-request-private" :value 0})
(def ^:private pair-after {:xt/id "v878-request-private" :value 1})

(deftest pair-matching-captured-domain-documents-test
  (let [initial {pair-request-key pair-before}
        after {pair-request-key pair-after}
        {:keys [earlier later compare!]} (pair-synthetic-fixture initial after after after)
        result (compare! earlier later)]
    (is (= :captured-domain-documents-match (:status result)))
    (is (= :same-captured-keys (:dependency-key-set-comparison result)))
    (is (= :two-caller-ordered-executor-bound-dependency-observations (:scope result)))
    (is (= :exact-clojure-captured-domain-document-map-equality (:relation result)))
    (is (false? (:state-contents-exposed? result)))
    (is (false? (:additional-xtdb-reads? result)))
    (is (false? (:comparison-ran-model-callbacks? result)))))

(deftest pair-differing-captured-document-values-test
  (let [initial {pair-request-key pair-before}
        after {pair-request-key pair-after}
        {:keys [earlier later compare!]} (pair-synthetic-fixture initial after initial initial)
        result (compare! earlier later)]
    (is (= :captured-domain-documents-differ (:status result)))
    (is (= :same-captured-keys (:dependency-key-set-comparison result)))
    (is (false? (:state-contents-exposed? result)))))

(deftest pair-different-dependency-keys-are-not-equal-test
  (let [initial {pair-request-key pair-before}
        after {pair-request-key pair-after}
        later-state {pair-auxiliary-key {:xt/id "v878-membership-private" :active true}}
        {:keys [earlier later compare!]} (pair-synthetic-fixture initial after later-state later-state)
        result (compare! earlier later)]
    (is (= :captured-dependency-key-sets-differ (:status result)))
    (is (= :different-captured-keys (:dependency-key-set-comparison result)))))

(deftest pair-empty-captured-domain-cannot-match-test
  (doseq [[before after] [[{} {}]
                          [{pair-request-key pair-before} {}]
                          [{} {pair-request-key pair-after}]]]
    (let [{:keys [earlier later compare!]}
          (pair-synthetic-fixture {} before after after)
          result (compare! earlier later)]
      (is (= :no-common-nonempty-captured-domain (:status result)))
      (is (false? (:state-contents-exposed? result))))))

(deftest pair-command-record-exclusion-is-narrow-test
  (let [initial {pair-request-key pair-before}
        after {pair-request-key pair-after pair-auxiliary-key
               {:xt/id "v878-membership-private" :active true}}
        {:keys [earlier later compare!]}
        (pair-synthetic-fixture initial after after after)
        result (compare! earlier later)]
    ;; The genuine command-result target is inserted by the fixture; it must
    ;; not force a mismatch. Other domain documents must remain compared.
    (is (= :captured-domain-documents-match (:status result)))
    (is (= :same-captured-keys (:dependency-key-set-comparison result)))
    (let [{:keys [earlier later compare!]}
          (pair-synthetic-fixture initial after
                                  {pair-request-key pair-after}
                                  {pair-request-key pair-after})]
      (is (= :captured-dependency-key-sets-differ
             (:status (compare! earlier later)))))))

(deftest pair-order-is-caller-specified-and-not-proven-chronology-test
  (let [initial {pair-request-key pair-before}
        after {pair-request-key pair-after}
        {:keys [earlier later compare!]} (pair-synthetic-fixture initial after after after)
        forward (compare! earlier later)
        backward (compare! later earlier)]
    (is (= :captured-domain-documents-match (:status forward)))
    (is (= :captured-domain-documents-differ (:status backward)))
    (doseq [r [forward backward]]
      (is (= :not-established (:observed-execution-chronology r)))
      (is (= :not-established (:no-intervening-writes r)))
      (is (= :not-established (:pinned-xtdb-snapshot r)))
      (is (= :not-established (:complete-domain-dependency-coverage r)))
      (is (= :not-established (:production-trace-refinement r)))
      (is (= :not-established (:application-certification r))))))

(deftest pair-requires-two-distinct-evidence-handles-before-inspection-test
  (let [initial {pair-request-key pair-before}
        {:keys [earlier later compare! inspections]}
        (pair-synthetic-fixture initial initial initial initial)]
    (doseq [[a b] [[nil later] [earlier nil] [earlier earlier]]]
      (is (= :invalid-observation-pair (kind #(compare! a b)))))
    (is (zero? @inspections))
    (is (= :captured-domain-documents-match
           (:status (compare! earlier later))))))

(deftest pair-rejects-unknown-and-detached-evidence-test
  (let [initial {pair-request-key pair-before}
        {:keys [earlier later unknown compare! observations]}
        (pair-synthetic-fixture initial initial initial initial)]
    (doseq [fake [unknown {} (Object.) (get @observations earlier)]]
      (is (= :unrecognized-executor-bound-evidence
             (kind #(compare! earlier fake)))))))

(deftest pair-explanation-is-redacted-and-inert-test
  (let [secret "v878-should-never-disclose-private-document"
        before {pair-request-key {:xt/id "v878-request-private"
                                  :secret secret :value 0}}
        after {pair-request-key {:xt/id "v878-request-private"
                                 :secret secret :value 1}}
        {:keys [earlier later compare! callbacks inspections]}
        (pair-synthetic-fixture before after after after)
        first-result (compare! earlier later)
        second-result (compare! earlier later)]
    (is (= first-result second-result))
    (is (= :captured-domain-documents-match (:status first-result)))
    (is (not (.contains (pr-str first-result) secret)))
    (is (not (.contains (pr-str first-result) "v878-request-private")))
    (is (zero? @callbacks))
    (is (= 4 @inspections))
    (doseq [field [:observation :observations :initial-state :submitted-incorporation
                   :record :documents :before :after]]
      (is (not (contains? first-result field))))))

(deftest pair-genuine-xtdb-observations-detect-intervening-write-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [first-handle (:evidence (invoke! example))
              ;; A real, deliberately intervening domain write makes the
              ;; second independently issued Claim observe the prior value.
              _ (xt/execute-tx @!node [[:put-docs :requests (:before example)]])
              second-command (assoc (:command example)
                                    :command-id (identity/command-id
                                                 (str "v878-second-command-" (random-uuid)))
                                    :execution-id (identity/execution-id
                                                   (str "v878-second-execution-" (random-uuid))))
              second-handle (:evidence
                             (assurance/run-and-check!
                              {:server (:server example)
                               :ctx {:biff.xtdb/node @!node
                                     :authenticated-principal principal}
                               :command second-command}))
              result (correspondence/compare-observation-pair!
                      assembly first-handle second-handle)]
          (is (not (identical? first-handle second-handle)))
          (is (= :captured-domain-documents-differ (:status result)))
          (is (= :same-captured-keys (:dependency-key-set-comparison result)))
          (is (= :not-established (:no-intervening-writes result)))
          (is (= :not-established (:observed-execution-chronology result)))
          (is (false? (:state-contents-exposed? result)))
          (is (= :invalid-observation-pair
                 (kind #(correspondence/compare-observation-pair!
                         assembly first-handle first-handle))))
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(correspondence/compare-observation-pair!
                         assembly first-handle (Object.))))))))))

(deftest pair-application-staleness-blocks-genuine-observations-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly artifact-path]}]
        (let [first-handle (:evidence (invoke! example))]
          (spit artifact-path "v878-artifact-tampered\n")
          (is (not (application/application-assembly? assembly)))
          (is (some? (try (correspondence/compare-observation-pair!
                          assembly first-handle (Object.))
                          nil
                          (catch clojure.lang.ExceptionInfo e e)))))))))


;; v884: Compare a PURE single-step transition from the captured before-state,
;; without requiring that before-state to equal the admitted finite model root.
;; These are local diagnostic observations, not reachability, causality, or
;; complete-dependency proof certificates.

(def ^:private local-poststate-key :captured-local-poststate-comparison)

(defn- v884-pure-local-fixture []
  (let [document {:xt/id "v884-private-create" :revision 0
                  :created-at "t0" :updated-at "t0"
                  :secret "v884-must-remain-private"}
        plan {:commands [(command/create :requests document version)]
              :guards [] :emit false}
        expected (:after (tx-model/step {} plan))
        record {:xt/id "v884-private-command"}
        record-key [resolution/command-entity-type (:xt/id record)]
        observe (fn [before final]
                  {:initial-state before
                   :submitted-incorporation {:plan plan}
                   :observations {:state (assoc final record-key record)}})
        compare! (fn [before final comparison]
                   (#'correspondence/compare-captured-local-poststate!
                    (observe before final) record comparison false nil))]
    {:document document
     :expected expected
     :compare! compare!}))

(deftest v884-real-xtdb-root-local-poststate-match-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [result ((build! assembly example) (:evidence (invoke! example)))
              local (get result local-poststate-key)]
          (is (= :matched (:status result)))
          (is (= :matches-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in result [poststate-key :status])))
          (is (= :matches-captured-posttransaction-dependencies (:status local)))
          (is (= :controlled-observation-pretransaction-dependencies
                 (:input-source local)))
          (is (false? (:declared-model-root-equality-required? local)))
          (is (false? (:additional-xtdb-reads? local)))
          (is (false? (:additional-model-callbacks? local)))
          (is (false? (:poststate-data-exposed? local))))))))

(deftest v884-real-xtdb-nonroot-local-poststate-match-test
  (let [example (test-operation)
        secret "v884-extra-model-root-secret"]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [root (assoc (model-state example)
                          [:extra secret] {:xt/id secret :secret secret})
              comparator (correspondence/build-comparator
                          assembly root (modeled-families example)
                          (model-rules example) small-options)
              result (comparator (:evidence (invoke! example)))
              local (get result local-poststate-key)]
          (is (= :matched (:status result)))
          (is (= :differs-from-captured-pretransaction-state
                 (get-in result [root-state-key :status])))
          (is (= :declared-root-not-established-equal-to-observation
                 (get-in result [poststate-key :status])))
          (is (= :matches-captured-posttransaction-dependencies (:status local)))
          (is (= :not-established (:equality-with-reachable-non-root-model-state local)))
          (is (not (.contains (pr-str result) secret))))))))

(deftest v884-pure-local-matching-and-unrelated-documents-test
  (let [{:keys [expected compare!]} (v884-pure-local-fixture)
        matched {:status :matched :physical-status :committed}
        result (compare! {} expected matched)]
    (is (= :matches-captured-posttransaction-dependencies (:status result)))
    (is (= :one-controlled-serial-step-from-captured-pretransaction-dependencies
           (:scope result)))
    (is (= :not-established (:complete-dependency-coverage result)))
    (is (= :not-established (:authenticated-execution-history result)))
    (is (= :differs-from-captured-posttransaction-dependencies
           (:status (compare! {} (assoc expected [:requests "unrelated"]
                                          {:xt/id "unrelated"}) matched))))
    (is (= :differs-from-captured-posttransaction-dependencies
           (:status (compare! {} {} matched))))))

(deftest v884-pure-local-physical-outcome-and-plan-disagreement-test
  (let [{:keys [expected compare!]} (v884-pure-local-fixture)
        matched {:status :matched :physical-status :committed}]
    (is (= :modeled-and-physical-outcomes-disagree
           (:status (compare! {} expected (assoc matched :physical-status :rejected)))))
    (is (= :physical-outcome-not-reported
           (:status (compare! {} expected (dissoc matched :physical-status)))))
    (is (= :submitted-plan-not-matched
           (:status (compare! {} expected (assoc matched :status :counterexample)))))
    (is (= :matches-captured-posttransaction-dependencies
           (:status (compare! {} expected matched))))))

(deftest v884-pure-local-rejection-preserves-captured-state-test
  (let [{:keys [document compare!]} (v884-pure-local-fixture)
        before {[:requests (:xt/id document)] document}
        result (compare! before before {:status :matched :physical-status :rejected})]
    (is (= :matches-captured-posttransaction-dependencies (:status result)))
    (is (= :controlled-observation-pretransaction-dependencies
           (:input-source result)))
    (is (= :not-established (:pinned-xtdb-snapshot result)))
    (is (false? (:poststate-data-exposed? result)))))

(deftest v884-captured-local-report-is-redacted-and-non-authorizing-test
  (let [{:keys [expected compare!]} (v884-pure-local-fixture)
        result (compare! {} expected {:status :matched :physical-status :committed})
        printed (pr-str result)]
    (doseq [secret ["v884-must-remain-private" "v884-private-create"
                    "v884-private-command"]]
      (is (not (.contains printed secret))))
    (doseq [field [:initial-state :observations :record :plan :commands
                   :before :after :binding :admission-handle]]
      (is (not (contains? result field))))
    (is (= :not-established (:production-trace-refinement result)))
    (is (= :not-established (:equality-with-reachable-non-root-model-state result)))
    (is (false? (:declared-model-root-equality-required? result)))))

(deftest v884-comparator-does-not-reexecute-business-operation-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [cmp (build! assembly example)
              evidence (:evidence (invoke! example))
              executed @(:calls example)
              result (cmp evidence)]
          (is (= executed @(:calls example)))
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in result [local-poststate-key :status])))
          (is (false? (get-in result [local-poststate-key :additional-model-callbacks?])))
          (is (false? (get-in result [local-poststate-key :additional-xtdb-reads?]))))))))

(deftest v884-local-result-respects-stale-and-forged-evidence-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly artifact-path]}]
        (let [cmp (build! assembly example)
              evidence (:evidence (invoke! example))]
          (is (= :matches-captured-posttransaction-dependencies
                 (get-in (cmp evidence) [local-poststate-key :status])))
          (is (= :unrecognized-executor-bound-evidence (kind #(cmp (Object.)))))
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(cmp (assurance/inspect-evidence evidence)))))
          (spit artifact-path "v884-stale-browser-artifact\n")
          (is (not (application/application-assembly? assembly)))
          (is (some? (try (cmp evidence)
                          nil
                          (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest v884-assurance-scope-explicitly-bounds-local-check-test
  (let [scope correspondence/assurance-scope]
    (is (contains? (:checked scope)
                   :single-captured-input-step-vs-observed-posttransaction-dependency-values))
    (doseq [limit [:local-step-equality-is-not-reachable-model-node-equality
                   :local-step-equality-does-not-prove-dependency-completeness
                   :authenticated-xtdb-history-or-global-isolation
                   :unbounded-execution-refinement
                   :whole-application-certification]]
      (is (contains? (:not-established scope) limit)))))


;; v937 — Permanent regressions for the v936 executor-bound, frozen prepared
;; competition. These exercise the real registered Live executor and XTDB
;; collector, not detached sample reports. The application-owned domain
;; fixture remains deliberately test-only; these checks do not certify policy.

(def ^:private v937-options {:max-depth 1 :max-transitions 16})

(defn- v937-enabled-competition [example]
  (let [families (modeled-families example)
        original-prepare (:prepare (first families))]
    (-> families
        (assoc-in [1 :enabled?] (fn [& _] true))
        (assoc-in [1 :prepare] original-prepare)
        (assoc-in [2 :enabled?] (fn [& _] true))
        (assoc-in [2 :prepare] original-prepare))))

(defn- v937-bound! [assembly example families]
  (formal-family/bound-prepared-competition
   assembly (model-state example) families (model-rules example)
   v937-options))

(deftest v937-real-registered-claim-has-frozen-finite-tlc-subject-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [before @(:calls example)
              bound (v937-bound! assembly example (v937-enabled-competition example))
              subject (:subject bound)]
          (is (= 3 (:choice-count bound)))
          (is (= 3 (count (:plans subject))))
          ;; Subject construction must not execute the application operation.
          (is (= before @(:calls example)))
          (is (= :not-run (:formal-check bound)))
          (is (= :not-established (:application-certification bound)))
          (let [checked (formal-check/check-prepared! subject)]
            (is (= :checked-finite-model (:status checked)))
            (is (pos? (:distinct-states checked)))
            (is (contains? (:not-established checked)
                           :independent-application-certificate)))
          (let [handle (:evidence (invoke! example))
                verdict ((:compare-execution! bound) handle)
                printed (pr-str verdict)]
            (is (= :matched-selected-root-execution (:status verdict)))
            (is (= :registered-executor-and-frozen-plan-correspondence
                   (:reason verdict)))
            (is (= operation-id (:operation verdict)))
            (is (= :not-run (:tla-check verdict)))
            (is (= :not-established (:application-certification verdict)))
            (is (contains? (get-in verdict [:scope :not-established])
                           :identity-of-authored-callbacks-and-production-executor-logic))
            (is (contains? (get-in verdict [:scope :not-established])
                           :recife-tlc-verdict))
            (doseq [private-field [:subject :plan :commands :observation :binding]]
              (is (not (contains? verdict private-field))))
            (is (not (.contains printed (:request-id example))))))))))

(deftest v937-changed-preparation-cannot-rewrite-frozen-proof-subject-test
  (let [example (test-operation)
        original-prep (:prepare (first (modeled-families example)))
        current-preparation? (atom false)
        unstable-prep
        (fn [state who args]
          (if @current-preparation?
            (original-prep state who args)
            ;; This was admitted as the frozen plan. Its update differs from
            ;; the *actual* executor's update, although the same mutable
            ;; callback will agree with the executor when re-evaluated later.
            (let [plan (original-prep state who args)
                  prior (get state [:requests (:request-id args)])
                  changed (assoc (:claimed example)
                                 :updated-at "v937-frozen-different-update")]
              (assoc plan :commands
                     [(command/update-command :requests :claim
                                              prior changed version)]))))
        families (assoc-in (v937-enabled-competition example)
                           [0 :prepare] unstable-prep)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [bound (v937-bound! assembly example families)
              frozen-first-plan (first (get-in bound [:subject :plans]))
              _ (reset! current-preparation? true)
              handle (:evidence (invoke! example))
              verdict ((:compare-execution! bound) handle)]
          (is (not= frozen-first-plan
                    (gesso.model.tx-model/admitted-plan
                     (original-prep (model-state example)
                                    "v829-principal-private"
                                    {:request-id (:request-id example)}))))
          (is (= :counterexample (:status verdict)))
          (is (= :observed-plan-differs-from-frozen-formal-subject
                 (:reason verdict)))
          (is (= :not-established (:application-certification verdict)))
          (is (= :not-run (:tla-check verdict)))
          (is (not (contains? verdict :plan)))
          (is (not (contains? verdict :observation))))))))

(deftest v937-exact-registered-executor-rejects-detached-evidence-test
  (let [example (test-operation)
        other (test-operation)
        other-handle (:evidence (invoke! other))]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [bound (v937-bound! assembly example
                                 (v937-enabled-competition example))
              compare! (:compare-execution! bound)
              valid (:evidence (invoke! example))]
          (is (= :matched-selected-root-execution
                 (:status (compare! valid))))
          (doseq [forged [nil (Object.) {} {:status :matched-selected-root-execution}
                          (assurance/inspect-evidence valid) other-handle]]
            (is (= :unrecognized-executor-bound-evidence
                   (kind #(compare! forged)))))
          (is (= :matched-selected-root-execution
                 (:status (compare! valid)))))))))

(deftest v937-prepared-competition-refuses-incomplete-choice-coverage-test
  (let [example (test-operation)]
    (with-application (:executor example)
      (fn [{:keys [assembly]}]
        (let [complete (v937-enabled-competition example)
              disabled (assoc-in complete [1 :enabled?] (constantly false))
              missing (subvec complete 0 2)]
          (is (= :disabled-declared-choice
                 (kind #(v937-bound! assembly example disabled))))
          (is (= :operation-identity-coverage-mismatch
                 (kind #(v937-bound! assembly example missing))))
          (is (= "unclaimed" (:owner (row :requests (:request-id example)))))
          (is (= 0 (:construct @(:calls example)))))))))


;; v939 — Permanent shared-construction bridge tests.  The actual production
;; resolved operation and the finite operation family use one identical
;; application-owned :construct! function.  This eliminates a duplicate
;; business-plan implementation, not the trusted model-context mapping or
;; proof of the callback's purity, authorization, or universal correctness.

(defn- v939-pair
  ([example calls] (v939-pair example calls {}))
  ([example calls overrides]
   (let [original (first (modeled-families example))
         initial (model-state example)
         args {:request-id (:request-id example)}
         source (fn [context binding]
                  (swap! calls inc)
                  {:now now
                   :outcome {:status :claimed}
                   :plan ((:prepare original)
                          (or (:model-state context) initial)
                          (identity/raw-value (:principal binding))
                          (:arguments binding))})
         config {:operation operation-id
                 :read-record! (fn [_ id] (row :gesso_commands id))
                 :may-disclose? (fn [_ _ _] true)
                 :construct! source
                 :principals [(identity/raw-value principal)]
                 :arguments [args]
                 :enabled? (:enabled? original)
                 :model-input! (fn [state who arguments]
                                 {:context {:model-state state}
                                  :binding {:command-id (identity/command-id
                                                          "v939-finite-command")
                                            :principal (identity/principal who)
                                            :operation operation-id
                                            :arguments arguments}})}]
     (shared-family/build-pair (merge config overrides)))))

(defn- v939-executor [example paired]
  (adapter/build-executor
   {:operation operation-id
    :resolved-operation (:resolved-operation paired)
    :submission-mode! (constantly :submit)
    :project! (fn [_ result]
                {:resolution (if (= :replay (:status result))
                               :already-incorporated :confirmed)
                 :authoritative
                 (protocol/authoritative
                  {:presence :present
                   :basis {:tx-id 2 :system-time now}
                   :projection {:request/owner
                                (:owner (row :requests (:request-id example)))}})
                 :outcome :request/claimed})}))

(defn- v939-families [example pair]
  (let [families (modeled-families example)]
    (-> families
        (assoc 0 (:family pair))
        (assoc-in [1 :enabled?] (fn [& _] true))
        (assoc-in [1 :prepare] (get-in pair [:family :prepare]))
        (assoc-in [2 :enabled?] (fn [& _] true))
        (assoc-in [2 :prepare] (get-in pair [:family :prepare])))))

(deftest v939-shared-construction-real-executor-and-tlc-test
  (let [example (test-operation)
        calls (atom 0)
        pair (v939-pair example calls)
        executor (v939-executor example pair)]
    (is (= 0 @calls) "Pairing must not execute application construction")
    (is (fn? (:resolved-operation pair)))
    (is (= operation-id (get-in pair [:family :id])))
    (is (= :not-established (:application-certification pair)))
    (with-application executor
      (fn [{:keys [assembly]}]
        (let [bound (formal-family/bound-prepared-competition
                     assembly (model-state example) (v939-families example pair)
                     (model-rules example) {:max-depth 1 :max-transitions 18})
              subject (:subject bound)
              checked (formal-check/check-prepared! subject)]
          (is (= 3 (:choice-count bound)))
          (is (= 3 (count (:plans subject))))
          (is (= :checked-finite-model (:status checked)))
          (is (pos? (:distinct-states checked)))
          (let [handle (:evidence (invoke! (assoc example :server (server-with executor))))
                result ((:compare-execution! bound) handle)]
            (is (= :matched-selected-root-execution (:status result)))
            (is (= :registered-executor-and-frozen-plan-correspondence
                   (:reason result)))
            (is (= :not-established (:application-certification result)))
            (is (= :not-run (:tla-check result)))
            (is (> @calls 0))
            (is (not (contains? result :plan)))
            (is (not (contains? result :binding)))
            (is (not (.contains (pr-str result) (:request-id example))))))))))

(deftest v939-model-input-mismatch-and-shape-rejected-test
  (let [example (test-operation)
        initial (model-state example)
        who (identity/raw-value principal)
        args {:request-id (:request-id example)}
        binding {:command-id (identity/command-id "v939-model-input")
                 :principal principal
                 :operation operation-id
                 :arguments args}]
    (doseq [[label input expected]
            [["wrong principal" {:context {} :binding (assoc binding
                                                     :principal (identity/principal "other"))}
              :model-binding-choice-mismatch]
             ["wrong operation" {:context {} :binding (assoc binding
                                                     :operation :request/cancel)}
              :model-binding-choice-mismatch]
             ["wrong arguments" {:context {} :binding (assoc binding
                                                     :arguments {:request-id "different"})}
              :model-binding-choice-mismatch]
             ["missing binding" {:context {} :binding {}}
              :invalid-model-binding]
             ["missing context" {:binding binding}
              :invalid-model-input]
             ["additional unadmitted model field" {:context {} :binding binding
                                                    :untrusted true}
              :invalid-model-input]
             ["nonmap context" {:context [] :binding binding}
              :invalid-model-input]]]
      (testing label
        (let [calls (atom 0)
              pair (v939-pair example calls
                              {:model-input! (fn [& _] input)})]
          (is (= 0 @calls) "Malformed model input is only evaluated on demand")
          (is (= expected
                 (kind #((get-in pair [:family :prepare]) initial who args))))
          (is (zero? @calls)
              "A rejected model binding must not invoke business construction"))))))

(deftest v939-same-mutable-construction-still-cannot-rewrite-frozen-plan-test
  (let [example (test-operation)
        initial (model-state example)
        source-plan (:prepare (first (modeled-families example)))
        changed? (atom false)
        source (fn [ctx binding]
                 (let [state (or (:model-state ctx) initial)
                       plan (source-plan state (identity/raw-value (:principal binding))
                                         (:arguments binding))]
                   {:now now
                    :outcome {:status :claimed}
                    :plan (if @changed?
                            (assoc plan :commands
                                   [(command/update-command
                                     :requests :claim (:before example)
                                     (assoc (:claimed example)
                                            :updated-at "v939-unfaithful-mutable-construction")
                                     version)])
                            plan)}))
        pair (v939-pair example (atom 0) {:construct! source})
        executor (v939-executor example pair)]
    (with-application executor
      (fn [{:keys [assembly]}]
        (let [bound (formal-family/bound-prepared-competition
                     assembly initial (v939-families example pair)
                     (model-rules example) {:max-depth 1 :max-transitions 18})
              frozen (first (get-in bound [:subject :plans]))]
          (reset! changed? true)
          (is (not= frozen
                    (tx-model/admitted-plan
                     (:plan (source {:model-state initial}
                                    {:principal principal
                                     :operation operation-id
                                     :arguments {:request-id (:request-id example)}})))))
          (let [handle (:evidence (invoke! (assoc example :server (server-with executor))))
                verdict ((:compare-execution! bound) handle)]
            (is (= :counterexample (:status verdict)))
            (is (= :observed-plan-differs-from-frozen-formal-subject
                   (:reason verdict)))
            (is (= :not-established (:application-certification verdict)))
            (is (not (contains? verdict :plan)))))))))

(deftest v939-construction-and-config-fail-closed-test
  (let [example (test-operation)
        who (identity/raw-value principal)
        args {:request-id (:request-id example)}
        initial (model-state example)
        calls (atom 0)
        invalid (v939-pair example calls
                           {:construct! (fn [& _]
                                          (swap! calls inc)
                                          {:now now :outcome {:status :claimed}
                                           :plan {:commands [] :guards []
                                                  :emit false}})})]
    (is (= 0 @calls))
    (is (= :invalid-durable-incorporation
           (kind #((get-in invalid [:family :prepare]) initial who args))))
    (is (= 1 @calls))
    (let [secret "v939-private-construction-failure"
          pair (v939-pair example (atom 0)
                          {:construct! (fn [& _]
                                         (throw (ex-info secret {:secret secret})))})
          error (try ((get-in pair [:family :prepare]) initial who args)
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (= :domain-construction-exception (:error/kind (ex-data error))))
      (is (not (.contains (.getMessage error) secret)))
      (is (nil? (.getCause error)))
      (is (not (.contains (pr-str (ex-data error)) secret))))
    (is (= :invalid-finite-domain
           (kind #(v939-pair example (atom 0) {:principals []}))))
    (is (= :invalid-configuration
           (kind #(v939-pair example (atom 0) {:unexpected-field true}))))
    (is (contains? (get-in shared-family/assurance-scope [:not-established])
                   :identity-of-model-context-and-production-context))
    (is (contains? (get-in shared-family/assurance-scope [:not-established])
                   :current-live-executor-registration))))


;; v941 — Permanent, conditional input-fidelity regressions for v940.
;; This comparison uses supplied production inputs.  It does not acquire
;; authenticated execution context, prove model-input! purity, certify an
;; operation, or establish completeness of application authorization fields.

(defn- v941-inputs [example]
  (let [state (model-state example)
        who (identity/raw-value principal)
        args {:request-id (:request-id example)}
        modeled-binding {:command-id (identity/command-id "v941-finite-private")
                         :principal principal
                         :operation operation-id
                         :arguments args}
        physical-binding (assoc modeled-binding
                                :command-id (identity/command-id "v941-live-private"))
        modeled-context {:model-state state
                         :request/current-owner nil
                         :request/eligible? true}
        supplied-context (assoc modeled-context
                                :authenticated-principal principal
                                :biff.fx/now now)]
    {:state state :who who :args args
     :modeled-binding modeled-binding :physical-binding physical-binding
     :modeled-context modeled-context :supplied-context supplied-context}))

(defn- v941-pair [example calls {:keys [modeled-context modeled-binding] :as inputs}]
  (v939-pair example calls
             {:model-input! (fn [_ _ _]
                              {:context modeled-context
                               :binding modeled-binding})}))

(deftest v941-field-comparison-is-conditional-and-redacted-test
  (let [example (test-operation)
        calls (atom 0)
        inputs (v941-inputs example)
        pair (v941-pair example calls inputs)
        compare! (:compare-production-input! pair)
        {:keys [state who args supplied-context physical-binding]} inputs
        result (compare! state who args supplied-context physical-binding)]
    (is (fn? compare!))
    (is (= :matched-declared-input-fields (:status result)))
    (is (= 3 (:context-fields-compared result)))
    (is (= 2 (:unmodeled-production-context-fields result)))
    (is (= :excluded-independent-execution-identity
           (:command-id-comparison result)))
    (is (= :caller-supplied-not-authenticated (:input-origin result)))
    (is (= :not-established (:application-certification result)))
    (is (zero? @calls) "The comparison must not construct or transact anything")
    (is (not (contains? result :context)))
    (is (not (contains? result :binding)))
    (is (not (.contains (pr-str result) (:request-id example))))
    (is (contains? (get-in pair [:scope :assumed])
                   :trusted-provenance-of-supplied-production-context-and-binding))
    (is (contains? (get-in pair [:scope :not-established])
                   :completeness-of-modeled-production-context-fields))
    (is (contains? (get-in pair [:scope :not-established])
                   :identity-of-model-context-and-production-context))))

(deftest v941-context-field-mismatch-nil-and-absence-are-rejected-test
  (let [example (test-operation)
        calls (atom 0)
        inputs (v941-inputs example)
        compare! (:compare-production-input! (v941-pair example calls inputs))
        {:keys [state who args supplied-context physical-binding]} inputs]
    (doseq [[label context]
            [["changed modeled state" (assoc supplied-context :model-state {:false :state})]
             ["missing modeled state" (dissoc supplied-context :model-state)]
             ["false permission" (assoc supplied-context :request/eligible? false)]
             ["missing nil-valued field" (dissoc supplied-context :request/current-owner)]
             ["non-nil instead of nil" (assoc supplied-context :request/current-owner :claimed)]]]
      (testing label
        (is (= :production-context-differs-from-model-input
               (kind #(compare! state who args context physical-binding))))))
    (is (= :invalid-production-context
           (kind #(compare! state who args [] physical-binding))))
    (is (= :invalid-production-context
           (kind #(compare! state who args nil physical-binding))))
    (is (= :matched-declared-input-fields
           (:status (compare! state who args
                              (assoc supplied-context :another-private-field :value)
                              physical-binding))))
    (is (= 3 (:unmodeled-production-context-fields
              (compare! state who args
                        (assoc supplied-context :another-private-field :value)
                        physical-binding))))
    (is (zero? @calls))))

(deftest v941-production-binding-must-match-except-command-id-test
  (let [example (test-operation)
        calls (atom 0)
        inputs (v941-inputs example)
        compare! (:compare-production-input! (v941-pair example calls inputs))
        {:keys [state who args supplied-context physical-binding]} inputs]
    (doseq [[label altered]
            [["wrong principal" (assoc physical-binding :principal
                                        (identity/principal "v941-wrong-private"))]
             ["wrong operation" (assoc physical-binding :operation :request/cancel)]
             ["wrong arguments" (assoc physical-binding :arguments {:request-id "different"})]]]
      (testing label
        (is (= :production-binding-differs-from-model-input
               (kind #(compare! state who args supplied-context altered))))))
    (is (= :invalid-production-binding
           (kind #(compare! state who args supplied-context {}))))
    (is (= :invalid-production-binding
           (kind #(compare! state who args supplied-context nil))))
    (is (= :matched-declared-input-fields
           (:status (compare! state who args supplied-context
                              (assoc physical-binding :command-id
                                     (identity/command-id "another-live-id"))))))
    (is (zero? @calls))))

(deftest v941-empty-and-invalid-modeled-input-fail-without-construction-test
  (let [example (test-operation)
        calls (atom 0)
        {:keys [state who args supplied-context physical-binding] :as inputs}
        (v941-inputs example)
        empty-pair (v941-pair example calls (assoc inputs :modeled-context {}))
        faulty-pair (v939-pair example calls
                    {:model-input! (fn [& _]
                                     (throw (ex-info "v941-secret-error"
                                                     {:password "v941-secret-value"})))})
        malformed-pair (v939-pair example calls
                        {:model-input! (fn [& _] {:context []
                                                  :binding (:modeled-binding inputs)})})
        error (try ((:compare-production-input! faulty-pair)
                    state who args supplied-context physical-binding)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :empty-modeled-context-comparison
           (kind #((:compare-production-input! empty-pair)
                   state who args supplied-context physical-binding))))
    (is (= :invalid-model-input
           (kind #((:compare-production-input! malformed-pair)
                   state who args supplied-context physical-binding))))
    (is (= :model-input-exception (:error/kind (ex-data error))))
    (is (nil? (.getCause error)))
    (is (not (.contains (.getMessage error) "v941-secret-error")))
    (is (not (.contains (pr-str (ex-data error)) "v941-secret-value")))
    (is (zero? @calls))))

(deftest v941-model-input-is-reevaluated-and-not-proven-stable-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        {:keys [state who args supplied-context physical-binding]} inputs
        calls (atom 0)
        mutated? (atom false)
        pair (v939-pair example calls
              {:model-input! (fn [_ _ _]
                               {:context (if @mutated?
                                           (assoc (:modeled-context inputs)
                                                  :request/eligible? false)
                                           (:modeled-context inputs))
                                :binding (:modeled-binding inputs)})})
        compare! (:compare-production-input! pair)]
    (is (= :matched-declared-input-fields
           (:status (compare! state who args supplied-context physical-binding))))
    (reset! mutated? true)
    (is (= :production-context-differs-from-model-input
           (kind #(compare! state who args supplied-context physical-binding))))
    (is (zero? @calls))
    (is (contains? (get-in pair [:scope :not-established])
                   :purity-of-clojure-callbacks))))

;; v946 — Authenticated production construction-input correspondence.
;; Every comparison uses the original finite admission, frozen plans, a checked
;; application assembly and the exact registered executor's opaque evidence.
;; This is selected-execution evidence, NOT proof of model-input completeness.

(defn- v946-pair [example context!]
  (v939-pair example (atom 0)
             {:model-input!
              (fn [state who args]
                {:context (context! state who args)
                 :binding {:command-id (identity/command-id "v946-finite-command")
                           :principal (identity/principal who)
                           :operation operation-id
                           :arguments args}})}))

(defn- v946-with-bound [pair-fn f]
  (let [example (test-operation)
        pair (pair-fn example)
        executor (v939-executor example pair)]
    (with-application executor
      (fn [{:keys [assembly]}]
        (let [bound (formal-family/bound-prepared-competition
                     assembly (model-state example) (v939-families example pair)
                     (model-rules example) {:max-depth 1 :max-transitions 18})]
          (f example pair executor bound))))))

(defn- v946-handle [example executor]
  (:evidence (invoke! (assoc example :server (server-with executor)))))

(deftest v946-executor-inputs-match-frozen-family-with-real-tlc-test
  (v946-with-bound
   (fn [example]
     (v946-pair example (fn [& _] {:authenticated-principal principal})))
   (fn [example pair executor bound]
     (let [checked (formal-check/check-prepared! (:subject bound))
           handle (v946-handle example executor)
           execution ((:compare-execution! bound) handle)
           comparison ((:compare-construction-input! bound) pair handle)
           printed (pr-str comparison)]
       (is (= 3 (:choice-count bound)))
       (is (= 3 (count (get-in bound [:subject :plans]))))
       (is (= :checked-finite-model (:status checked)))
       (is (pos? (:distinct-states checked)))
       (is (= :matched-selected-root-execution (:status execution)))
       (is (= :matched-executor-bound-declared-input-fields
              (:status comparison)))
       (is (= :exact-registered-executor-call-site (:input-origin comparison)))
       (is (= 1 (:context-fields-compared comparison)))
       (is (pos? (:unmodeled-production-context-fields comparison)))
       (is (= :not-established (:model-input-coverage comparison)))
       (is (= :not-run (:tla-check comparison)))
       (is (= :not-established (:application-certification comparison)))
       (is (not (contains? comparison :context)))
       (is (not (contains? comparison :binding)))
       (is (not (contains? comparison :plan)))
       (is (not (.contains printed (:request-id example))))
       (is (not (.contains printed "v829-principal-private")))
       (doseq [detached [nil (Object.) {} (assurance/inspect-evidence handle)]]
         (is (= :unrecognized-executor-bound-evidence
                (kind #((:compare-construction-input! bound) pair detached)))))
       (is (= :unrecognized-frozen-shared-family
              (kind #((:compare-construction-input! bound)
                      (assoc pair :family (assoc (:family pair) :arguments []))
                      handle))))
       (is (= :not-established (:application-certification bound)))))))

(deftest v946-wrong-modeled-context-rejected-after-real-plan-match-test
  (v946-with-bound
   (fn [example]
     (v946-pair example (fn [& _] {:authenticated-principal
                                    (identity/principal "v946-wrong-principal")})))
   (fn [example pair executor bound]
     (let [handle (v946-handle example executor)
           execution ((:compare-execution! bound) handle)
           comparison ((:compare-construction-input! bound) pair handle)]
       (is (= :matched-selected-root-execution (:status execution)))
       (is (= :counterexample (:status comparison)))
       (is (= :modeled-input-differs-from-executor-bound-construction-input
              (:reason comparison)))
       (is (= :exact-registered-executor-call-site (:input-origin comparison)))
       (is (= :not-run (:tla-check comparison)))
       (is (= :not-established (:application-certification comparison)))
       (is (not (contains? comparison :context)))
       (is (not (.contains (pr-str comparison) "v946-wrong-principal")))))))

(deftest v946-changed-construction-blocks-otherwise-matching-input-test
  (let [changed? (atom false)]
    (v946-with-bound
     (fn [example]
       (let [original (:prepare (first (modeled-families example)))
             initial (model-state example)]
         (v939-pair
          example (atom 0)
          {:construct!
           (fn [ctx binding]
             (let [plan (original (or (:model-state ctx) initial)
                                  (identity/raw-value (:principal binding))
                                  (:arguments binding))]
               {:now now :outcome {:status :claimed}
                :plan (if @changed?
                        plan
                        (assoc plan :commands
                               [(command/update-command
                                 :requests :claim (:before example)
                                 (assoc (:claimed example)
                                        :updated-at "v946-frozen-different")
                                 version)]))}))
           :model-input!
           (fn [_ who args]
             {:context {:authenticated-principal principal}
              :binding {:command-id (identity/command-id "v946-frozen-id")
                        :principal (identity/principal who)
                        :operation operation-id :arguments args}})})))
     (fn [example pair executor bound]
       (reset! changed? true)
       (let [handle (v946-handle example executor)
             execution ((:compare-execution! bound) handle)
             comparison ((:compare-construction-input! bound) pair handle)]
         (is (= :counterexample (:status execution)))
         (is (= :observed-plan-differs-from-frozen-formal-subject
                (:reason execution)))
         (is (= :counterexample (:status comparison)))
         (is (= :frozen-execution-correspondence-not-established
                (:reason comparison)))
         (is (= :not-inspected (:input-origin comparison)))
         (is (= :not-established (:application-certification comparison)))
         (is (not (contains? comparison :context)))
         (is (not (.contains (pr-str comparison) (:request-id example)))))))))

(deftest v946-changed-model-input-mapping-rejected-after-admission-test
  (let [changed? (atom false)]
    (v946-with-bound
     (fn [example]
       (v946-pair example
                  (fn [& _]
                    {:authenticated-principal
                     (if @changed?
                       (identity/principal "v946-unfaithful-model-input")
                       principal)})))
     (fn [example pair executor bound]
       (reset! changed? true)
       (let [handle (v946-handle example executor)
             execution ((:compare-execution! bound) handle)
             comparison ((:compare-construction-input! bound) pair handle)]
         (is (= :matched-selected-root-execution (:status execution)))
         (is (= :counterexample (:status comparison)))
         (is (= :modeled-input-differs-from-executor-bound-construction-input
                (:reason comparison)))
         (is (= :not-established (:model-input-coverage comparison)))
         (is (= :not-established (:application-certification comparison)))
         (is (not (.contains (pr-str comparison)
                             "v946-unfaithful-model-input"))))))))


;; v948 — Permanent closed modeled-context inventory regressions (v947).
;; A closed inventory makes omitted/added production keys visible, but authored
;; :excluded fields remain UNPROVED irrelevant to construction or authorization.
;; These checks are caller-supplied comparisons, not executor authentication.

(defn- v948-coverage [inputs]
  {:modeled (set (keys (:modeled-context inputs)))
   :excluded (set (remove (set (keys (:modeled-context inputs)))
                          (keys (:supplied-context inputs))))})

(defn- v948-pair
  ([example inputs] (v948-pair example inputs (v948-coverage inputs)))
  ([example inputs coverage]
   (v939-pair example (atom 0)
              {:model-input! (fn [_ _ _]
                               {:context (:modeled-context inputs)
                                :binding (:modeled-binding inputs)})
               :context-coverage coverage})))

(deftest v948-closed-context-inventory-reports-limits-and-redacts-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        pair (v948-pair example inputs)
        {:keys [state who args supplied-context physical-binding]} inputs
        actual ((:compare-production-input! pair)
                state who args supplied-context physical-binding)
        printable (pr-str actual)]
    (is (= :matched-declared-input-fields (:status actual)))
    (is (= :closed-against-authored-exclusions (:context-key-inventory actual)))
    (is (= 3 (:context-fields-compared actual)))
    (is (= 2 (:explicitly-excluded-context-fields actual)))
    (is (= 2 (:unmodeled-production-context-fields actual)))
    (is (= :not-established (:exclusion-relevance actual)))
    (is (= :caller-supplied-not-authenticated (:input-origin actual)))
    (is (= :not-established (:application-certification actual)))
    (is (contains? (get-in pair [:scope :assumed])
                   :truthful-declaration-of-excluded-context-field-irrelevance))
    (is (contains? (get-in pair [:scope :not-established])
                   :irrelevance-of-declared-excluded-production-fields))
    (is (contains? (get-in pair [:scope :not-established])
                   :completeness-of-application-policy-dependency-declaration))
    (is (not (contains? actual :context)))
    (is (not (contains? actual :binding)))
    (is (not (.contains printable (:request-id example))))))

(deftest v948-production-context-must-have-exactly-declared-fields-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        compare! (:compare-production-input! (v948-pair example inputs))
        {:keys [state who args supplied-context physical-binding]} inputs]
    (doseq [[label mutated]
            [["added field" (assoc supplied-context :authorization/revoked? true)]
             ["removed excluded field" (dissoc supplied-context :biff.fx/now)]
             ["removed another excluded field" (dissoc supplied-context :authenticated-principal)]]]
      (testing label
        (is (= :production-context-key-inventory-differs
               (kind #(compare! state who args mutated physical-binding))))))
    (doseq [[label mutated]
            [["changed modeled state" (assoc supplied-context :model-state {:corrupted true})]
             ["removed nullable modeled field" (dissoc supplied-context :request/current-owner)]
             ["replaced nullable modeled field" (assoc supplied-context :request/current-owner :claimed)]]]
      (testing label
        (is (= :production-context-differs-from-model-input
               (kind #(compare! state who args mutated physical-binding))))))
    (is (= :matched-declared-input-fields
           (:status (compare! state who args supplied-context physical-binding))))))

(deftest v948-closed-inventory-configuration-must-be-total-and-disjoint-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        coverage (v948-coverage inputs)]
    (doseq [[label invalid]
            [["nil" nil]
             ["empty" {}]
             ["no modeled fields" {:modeled #{} :excluded #{:another}}]
             ["modeled field is not keyword" {:modeled #{1} :excluded #{}}]
             ["excluded field is not keyword" {:modeled #{:a} :excluded #{1}}]
             ["overlap" {:modeled #{:a} :excluded #{:a}}]
             ["excluded not a set" {:modeled #{:a} :excluded [:b]}]
             ["unknown configuration key" (assoc coverage :unexpected true)]]]
      (testing label
        (is (= :invalid-context-coverage
               (kind #(v948-pair example inputs invalid))))))
    (is (= :invalid-configuration
           (kind #(v939-pair example (atom 0)
                              {:context-coverage coverage
                               :unknown :configuration}))))))

(deftest v948-model-key-drift-rejected-before-business-construction-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        initial (:modeled-context inputs)
        changed? (atom false)
        constructed (atom 0)
        pair (v939-pair example constructed
                        {:context-coverage (v948-coverage inputs)
                         :model-input!
                         (fn [_ _ _]
                           {:context (if @changed?
                                       (dissoc initial :request/current-owner)
                                       initial)
                            :binding (:modeled-binding inputs)})})
        {:keys [state who args supplied-context physical-binding]} inputs
        compare! (:compare-production-input! pair)]
    (is (= :matched-declared-input-fields
           (:status (compare! state who args supplied-context physical-binding))))
    (reset! changed? true)
    (is (= :modeled-context-key-inventory-drift
           (kind #(compare! state who args supplied-context physical-binding))))
    (is (= :modeled-context-key-inventory-drift
           (kind #((get-in pair [:family :prepare]) state who args))))
    (is (zero? @constructed))))

(deftest v948-inventory-is-opt-in-and-does-not-certify-extra-fields-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        pair (v941-pair example (atom 0) inputs)
        {:keys [state who args supplied-context physical-binding]} inputs
        actual ((:compare-production-input! pair)
                state who args
                (assoc supplied-context :undeclared/new-authorization-key :private)
                physical-binding)]
    (is (= :matched-declared-input-fields (:status actual)))
    (is (= :not-declared (:context-key-inventory actual)))
    (is (= 3 (:unmodeled-production-context-fields actual)))
    (is (= :not-established (:exclusion-relevance actual)))
    (is (= :not-established (:application-certification actual)))
    (is (not (.contains (pr-str actual) "private")))))


;; v966 — Permanent tests for v965's opt-in recursive closed-data input gate.
;; This gate enforces a bounded representation discipline, not application
;; callback purity, complete authorization dependencies, or authenticated
;; production acquisition. Keep fixtures synthetic and source independent.

(defn- v966-coverage [inputs]
  (assoc (v948-coverage inputs) :modeled-values :closed-data))

(defn- v966-pair
  [example inputs calls modeled-context]
  (v939-pair example calls
             {:context-coverage (v966-coverage inputs)
              :model-input!
              (fn [_ _ _]
                {:context modeled-context
                 :binding (:modeled-binding inputs)})}))

(defn- v966-error [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e e)))

(deftest v966-closed-data-valid-nested-values-and-scope-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        value {:numbers [nil true false 0 12N 3/7 1.5M 4.25]
               :identity [(random-uuid) (java.time.Instant/parse "2026-10-15T00:00:00Z")]
               :collections {:map (array-map :a 1 :b #{:ok :good})
                             :vector (subvec [:a :b :c] 1)
                             :list (list :yes :no)
                             :empty '()}}
        modeled (assoc (:modeled-context inputs) :request/eligible? value)
        physical (assoc (:supplied-context inputs) :request/eligible? value)
        calls (atom 0)
        pair (v966-pair example inputs calls modeled)
        {:keys [state who args physical-binding]} inputs
        match ((:compare-production-input! pair)
               state who args physical physical-binding)
        printed (pr-str match)]
    (is (= :matched-declared-input-fields (:status match)))
    (is (= :checked-closed-data (:modeled-value-closure match)))
    (is (= :not-established (:constructor-purity match)))
    (is (= :closed-against-authored-exclusions (:context-key-inventory match)))
    (is (= :caller-supplied-not-authenticated (:input-origin match)))
    (is (= :not-established (:application-certification match)))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-recursive-modeled-value-closure))
    (is (contains? (get-in pair [:scope :not-established])
                   :absence-of-ambient-constructor-dependencies))
    (is (zero? @calls) "Comparison does not call the business constructor")
    (is (not (contains? match :context)))
    (is (not (contains? match :binding)))
    (is (not (.contains printed (:request-id example))))
    (is (map? ((get-in pair [:family :prepare]) state who args)))
    (is (= 1 @calls) "Admitted finite construction still executes normally")))

(deftest v966-closed-data-rejects-nested-capabilities-before-construction-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        {:keys [state who args]} inputs
        bad-values
        [["atom in map" {:outer {:inner (atom true)}}]
         ["atom in key" {(atom :secret) :value}]
         ["function in vector" [:ok (fn [] :hidden)]]
         ["volatile" [1 (volatile! 2)]]
         ["lazy sequence" (map identity [:a :b])]
         ["delay" (delay :secret)]
         ["java map" (doto (java.util.HashMap.) (.put "key" "value"))]
         ["java list" (java.util.ArrayList. [:a :b])]
         ["java array" (into-array Object [:a :b])]
         ["java date" (java.util.Date.)]
         ["sorted map comparator" (sorted-map :a 1)]
         ["sorted set comparator" (sorted-set :a :b)]
         ["nonfinite double" Double/NaN]
         ["nonfinite float" Float/POSITIVE_INFINITY]
         ["nested metadata" {:path (with-meta [:safe] {:hidden (atom 2)})}]
         ["metadata on context value" (with-meta {:a 1} {:source :untrusted})]]]
    (doseq [[label value] bad-values]
      (testing label
        (let [calls (atom 0)
              modeled (assoc (:modeled-context inputs) :request/eligible? value)
              pair (v966-pair example inputs calls modeled)
              exception (v966-error #((get-in pair [:family :prepare])
                                      state who args))]
          (is (= :non-data-modeled-context (:error/kind (ex-data exception))))
          (is (= :gesso.model.resolved-family/invalid-resolved-family
                 (:error/type (ex-data exception))))
          (is (= #{:error/type :error/kind} (set (keys (ex-data exception)))))
          (is (not (contains? (ex-data exception) :context)))
          (is (zero? @calls) "Construction cannot observe the rejected context"))))))

(deftest v966-closed-data-traversal-bounds-fail-closed-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        {:keys [state who args]} inputs
        depth-excess (nth (iterate vector :leaf) 40)
        node-excess (vec (repeat 8250 :leaf))]
    (doseq [[label value] [["depth" depth-excess]
                           ["node budget" node-excess]]]
      (testing label
        (let [calls (atom 0)
              modeled (assoc (:modeled-context inputs) :request/eligible? value)
              pair (v966-pair example inputs calls modeled)
              exception (v966-error #((get-in pair [:family :prepare])
                                      state who args))]
          (is (= :non-data-modeled-context (:error/kind (ex-data exception))))
          (is (zero? @calls))
          (is (not (.contains (pr-str (ex-data exception)) "leaf"))))))))

(deftest v966-closed-production-values-checked-before-equality-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        calls (atom 0)
        pair (v966-pair example inputs calls (:modeled-context inputs))
        {:keys [state who args supplied-context physical-binding]} inputs
        compare! (:compare-production-input! pair)]
    (doseq [[label value] [["production atom" {:nested (atom true)}]
                           ["production function" (fn [] :hidden)]
                           ["production metadata" (with-meta [:value] {:hidden :yes})]
                           ["production lazy" (map identity [:x])]]]
      (testing label
        (let [exception (v966-error
                         #(compare! state who args
                                    (assoc supplied-context :request/eligible? value)
                                    physical-binding))]
          (is (= :non-data-modeled-context (:error/kind (ex-data exception))))
          (is (zero? @calls))
          (is (not (contains? (ex-data exception) :context))))))
    ;; Excluded inputs intentionally have no content-level closure guarantee.
    (let [physical (assoc supplied-context :biff.fx/now (Object.))
          report (compare! state who args physical physical-binding)]
      (is (= :matched-declared-input-fields (:status report)))
      (is (= :not-established (:exclusion-relevance report)))
      (is (= :checked-closed-data (:modeled-value-closure report)))
      (is (zero? @calls)))))

(deftest v966-closed-data-configuration-and-legacy-mode-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        coverage (v966-coverage inputs)
        invalid [nil :unexpected false :partially-closed]
        {:keys [state who args supplied-context physical-binding]} inputs]
    (doseq [value invalid]
      (testing (str "invalid mode " value)
        (is (= :invalid-context-coverage
               (kind #(v948-pair example inputs
                                 (assoc coverage :modeled-values value)))))))
    (is (= :invalid-context-coverage
           (kind #(v948-pair example inputs
                             (assoc coverage :unsupported true)))))
    (let [calls (atom 0)
          modeled (assoc (:modeled-context inputs) :request/eligible? (atom true))
          old-coverage (v948-coverage inputs)
          pair (v939-pair example calls
                          {:context-coverage old-coverage
                           :model-input!
                           (fn [_ _ _] {:context modeled
                                        :binding (:modeled-binding inputs)})})]
      (is (= :not-checked
             (:modeled-value-closure
              ((:compare-production-input! pair)
               state who args
               (assoc supplied-context :request/eligible? (:request/eligible? modeled))
               physical-binding))))
      (is (zero? @calls))
      ;; Legacy key-inventory validation did not impose value-closure.
      (is (map? ((get-in pair [:family :prepare]) state who args)))
      (is (= 1 @calls)))))


;; v968 — Permanent tests for v967's declared nested semantic paths. The
;; path contract verifies authored *presence* in admitted model inputs and
;; caller-supplied production inputs; it does not infer authorization facts,
;; authenticate acquisition, prove constructor purity, or enforce commit guards.

(defn- v968-coverage [modeled production paths]
  {:modeled (set (keys modeled))
   :excluded (set (remove (set (keys modeled)) (keys production)))
   :modeled-values :closed-data
   :required-paths paths})

(defn- v968-pair
  [example inputs calls modeled production paths]
  (v939-pair
   example calls
   {:context-coverage (v968-coverage modeled production paths)
    :model-input! (fn [_ _ _]
                    {:context modeled :binding (:modeled-binding inputs)})}))

(defn- v968-compare [pair inputs production]
  ((:compare-production-input! pair)
   (:state inputs) (:who inputs) (:args inputs) production
   (:physical-binding inputs)))

(deftest v968-declared-paths-accept-nested-facts-and-present-nil-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        policy {:decision {:active? true :owner nil}
                :evidence {:revision 7 :status :evaluated}}
        modeled (assoc (:modeled-context inputs) :policy policy)
        production (assoc (:supplied-context inputs) :policy policy)
        paths #{[:policy :decision :active?]
                [:policy :decision :owner]
                [:policy :evidence :revision]
                [:request/current-owner]}
        calls (atom 0)
        pair (v968-pair example inputs calls modeled production paths)
        actual (v968-compare pair inputs production)]
    (is (= :matched-declared-input-fields (:status actual)))
    (is (= :checked-declared-paths (:required-semantic-path-presence actual)))
    (is (= 4 (:required-semantic-paths-checked actual)))
    (is (= :not-established (:required-paths-completeness actual)))
    (is (= :checked-closed-data (:modeled-value-closure actual)))
    (is (= :caller-supplied-not-authenticated (:input-origin actual)))
    (is (= :not-established (:constructor-purity actual)))
    (is (= :not-established (:application-certification actual)))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-declared-nested-semantic-path-presence))
    (is (contains? (get-in pair [:scope :assumed])
                   :truthful-complete-authored-required-semantic-paths))
    (is (contains? (get-in pair [:scope :not-established])
                   :commit-time-enforcement-of-declared-semantic-facts))
    (is (zero? @calls) "A production comparison cannot call the constructor")
    (is (not (contains? actual :context)))
    (is (not (contains? actual :binding)))
    (is (not (.contains (pr-str actual) (:request-id example))))
    (is (map? ((get-in pair [:family :prepare])
               (:state inputs) (:who inputs) (:args inputs))))
    (is (= 1 @calls))))

(deftest v968-required-modeled-paths-fail-before-construction-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        original (assoc (:modeled-context inputs)
                        :policy {:decision {:active? false :owner nil}})
        production (assoc (:supplied-context inputs)
                          :policy (:policy original))
        paths #{[:policy :decision :active?] [:policy :decision :owner]}
        invalid
        [["missing nested leaf" (update-in original [:policy :decision] dissoc :active?)]
         ["missing nil leaf" (update-in original [:policy :decision] dissoc :owner)]
         ["missing intermediate" (assoc original :policy {:else :unchanged})]
         ["nonmap intermediate" (assoc original :policy {:decision :not-a-map})]
         ["missing modeled top-level key" (dissoc original :policy)]]]
    (doseq [[label modeled] invalid]
      (testing label
        (let [calls (atom 0)
              pair (v939-pair example calls
                              {:context-coverage
                               (v968-coverage original production paths)
                               :model-input! (fn [_ _ _]
                                               {:context modeled
                                                :binding (:modeled-binding inputs)})})
              error (v966-error #((get-in pair [:family :prepare])
                                   (:state inputs) (:who inputs) (:args inputs)))]
          (is (= (if (= label "missing modeled top-level key")
                   :modeled-context-key-inventory-drift
                   :missing-required-modeled-semantic-input)
                 (:error/kind (ex-data error))))
          (is (= #{:error/type :error/kind}
                 (set (keys (ex-data error)))))
          (is (zero? @calls) "Missing modeled fact must block construction"))))))

(deftest v968-required-production-paths-fail-before-context-comparison-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        modeled (assoc (:modeled-context inputs)
                       :policy {:decision {:active? false :owner nil}})
        production (assoc (:supplied-context inputs) :policy (:policy modeled))
        paths #{[:policy :decision :active?] [:policy :decision :owner]}
        calls (atom 0)
        pair (v968-pair example inputs calls modeled production paths)
        invalid
        [["missing active" (update-in production [:policy :decision] dissoc :active?)]
         ["missing nil owner" (update-in production [:policy :decision] dissoc :owner)]
         ["missing decision" (assoc production :policy {:other :still-present})]
         ["nonmap decision" (assoc production :policy {:decision 42})]
         ["missing policy" (dissoc production :policy)]]]
    (doseq [[label physical] invalid]
      (testing label
        (let [error (v966-error #(v968-compare pair inputs physical))]
          (is (= :missing-required-production-semantic-input
                 (:error/kind (ex-data error))))
          (is (= #{:error/type :error/kind}
                 (set (keys (ex-data error)))))
          (is (zero? @calls)))))
    (is (= :matched-declared-input-fields
           (:status (v968-compare pair inputs production))))
    ;; Values can be changed without disappearing. Existing complete-value
    ;; comparison must still detect such changes, separately from presence.
    (is (= :production-context-differs-from-model-input
           (kind #(v968-compare pair inputs
                                (assoc-in production [:policy :decision :active?] true)))))
    (is (zero? @calls))))

(deftest v968-required-paths-support-composite-document-keys-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        document-key [:resources (:request-id example)]
        nested {:documents {document-key {:revision 9 :owner nil}}}
        modeled (assoc (:modeled-context inputs) :finite nested)
        production (assoc (:supplied-context inputs) :finite nested)
        paths #{[:finite :documents document-key :revision]
                [:finite :documents document-key :owner]}
        calls (atom 0)
        pair (v968-pair example inputs calls modeled production paths)]
    (is (= :matched-declared-input-fields
           (:status (v968-compare pair inputs production))))
    (is (= 2 (:required-semantic-paths-checked
              (v968-compare pair inputs production))))
    (is (= :missing-required-modeled-semantic-input
           (kind #((get-in (v968-pair example inputs calls
                                      (assoc-in modeled [:finite :documents document-key]
                                                {:owner nil})
                                      production paths)
                           [:family :prepare])
                   (:state inputs) (:who inputs) (:args inputs)))))
    (is (= :missing-required-production-semantic-input
           (kind #(v968-compare pair inputs
                                (assoc-in production [:finite :documents document-key]
                                          {:revision 9})))))
    (is (zero? @calls))))

(deftest v968-required-path-configuration-is-bounded-and-fail-closed-test
  (let [example (test-operation)
        inputs (v941-inputs example)
        modeled (:modeled-context inputs)
        production (:supplied-context inputs)
        baseline (v968-coverage modeled production #{[:model-state]})
        too-many (set (for [n (range 257)] [:model-state n]))
        too-deep (vec (cons :model-state (repeat 32 :deeper)))
        invalid
        [["empty path inventory" #{}]
         ["non-set paths" [[:model-state]]]
         ["nil paths" nil]
         ["nonvector path" #{'(:model-state)}]
         ["empty path" #{[]}]
         ["undeclared root" #{[:not-modeled]}]
         ["unsafe segment" #{[:model-state (Object.)]}]
         ["too-long path" #{too-deep}]
         ["too many declared paths" too-many]
         ["wrong composite key arity" #{[:model-state [:resources 1 2]]}]
         ["nonkeyword composite type" #{[:model-state ["resources" 1]]}]
         ["composite key with opaque id" #{[:model-state [:resources (Object.)]]}]]]
    (doseq [[label paths] invalid]
      (testing label
        (is (= :invalid-context-coverage
               (kind #(v948-pair example inputs
                                 (assoc baseline :required-paths paths)))))))
    (is (= :invalid-context-coverage
           (kind #(v948-pair example inputs (dissoc baseline :modeled-values)))))
    (is (= :invalid-context-coverage
           (kind #(v948-pair example inputs
                             (assoc baseline :unknown-configuration true)))))
    (is (= :invalid-context-coverage
           (kind #(v948-pair example inputs
                             (assoc baseline :required-paths #{[:biff.fx/now]})))))
    (is (= :not-declared
           (:required-semantic-path-presence
            (v968-compare (v966-pair example inputs (atom 0) modeled)
                          inputs production))))))


;; v970 — Permanent generic guard-to-transaction correspondence controls.
;; The required guard paths are authored by the application, and are checked
;; against BOTH the finite family and the production construction.  These
;; tests deliberately do not claim completeness of authored policy, proven
;; acquisition provenance, or an XTDB transaction-refinement theorem.

(def ^:private v970-time
  (java.time.Instant/parse "2026-10-15T00:00:00Z"))

(def ^:private v970-version
  {:revision-key :fixture/revision
   :created-at-key :fixture/created-at
   :updated-at-key :fixture/updated-at})

(defn- v970-eligibility [revision]
  {:xt/id "v970-eligibility"
   :fixture/revision revision
   :fixture/created-at v970-time
   :fixture/updated-at v970-time})

(defn- v970-guard
  ([] (v970-guard 4))
  ([revision]
   (command/guard :fixture/eligibility
                  (v970-eligibility revision) v970-version)))

(defn- v970-binding []
  {:command-id (identity/command-id "v970-finite-command")
   :principal (identity/principal "v970-actor")
   :operation :fixture/allocate
   :arguments {:resource "v970-resource"}})

(defn- v970-context
  ([] (v970-context (v970-guard)))
  ([guard]
   {:eligibility {:guard guard}
    :other {:nested {:present? true}}}))

(defn- v970-coverage []
  {:modeled #{:eligibility :other}
   :excluded #{}
   :modeled-values :closed-data
   :required-guard-paths #{[:eligibility :guard]}})

(defn- v970-plan [guards]
  {:commands
   [(command/create
     :fixture/resource
     {:xt/id "v970-resource"
      :fixture/revision 0
      :fixture/created-at v970-time
      :fixture/updated-at v970-time}
     v970-version)]
   :guards guards
   :emit false})

(defn- v970-config
  [builder model-context production-guard-coverage calls]
  {:operation :fixture/allocate
   :read-record! (fn [_ _] nil)
   :may-disclose? (fn [_ _ _] true)
   :construct! (fn [context binding]
                 (swap! calls inc)
                 {:now v970-time
                  :outcome {:status :allocated}
                  :plan (builder context binding)})
   :principals ["v970-actor"]
   :arguments [{:resource "v970-resource"}]
   :enabled? (constantly true)
   :model-input! (fn [_ _ _]
                   {:context model-context
                    :binding (v970-binding)})
   :context-coverage production-guard-coverage})

(defn- v970-pair [builder modeled calls]
  (shared-family/build-pair
   (v970-config builder modeled (v970-coverage) calls)))

(defn- v970-prepare [pair]
  ((get-in pair [:family :prepare])
   {[:fixture/eligibility "v970-eligibility"] (v970-eligibility 4)}
   "v970-actor" {:resource "v970-resource"}))

(defn- v970-submit [pair context]
  ((:resolved-operation pair)
   context {:binding (v970-binding) :mode :submit}))

(deftest v970-required-guards-are-enforced-in-both-constructions-test
  (let [context (v970-context)
        calls (atom 0)
        guard (v970-guard)
        pair (v970-pair (fn [ctx _]
                          (v970-plan [(get-in ctx [:eligibility :guard])]))
                        context calls)
        submitted (atom 0)]
    (is (= [guard] (:guards (v970-prepare pair))))
    (is (= 1 @calls))
    (with-redefs [tx/transact!
                  (fn [& _]
                    (swap! submitted inc)
                    {:commit/status :committed})]
      (is (= :incorporated (:status (v970-submit pair context))))
      (is (= 1 @submitted)))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-construction-guard-to-plan-correspondence))
    (is (contains? (get-in pair [:scope :assumed])
                   :truthful-complete-authored-guard-dependency-paths))
    (is (contains? (get-in pair [:scope :not-established])
                   :completeness-of-authored-policy-and-guards))
    (is (contains? (get-in pair [:scope :not-established])
                   :real-xtdb-enforcement-of-declared-guards))
    (is (= :not-established (:application-certification pair)))))

(deftest v970-omitted-guard-fails-in-finite-and-production-paths-test
  (let [context (v970-context)
        calls (atom 0)
        pair (v970-pair (fn [_ _] (v970-plan [])) context calls)
        submitted (atom 0)]
    (is (= :domain-construction-exception (kind #(v970-prepare pair))))
    (is (= 1 @calls))
    (with-redefs [tx/transact!
                  (fn [& _]
                    (swap! submitted inc)
                    {:commit/status :committed})]
      (let [error (v966-error #(v970-submit pair context))]
        (is (= :missing-required-commit-guard
               (:error/kind (ex-data error))))
        (is (= #{:error/type :error/kind}
               (set (keys (ex-data error)))))))
    (is (zero? @submitted) "A missing dependency guard cannot submit")
    (is (= 2 @calls) "One shared constructor is used by both paths")))

(deftest v970-forged-guard-target-and-version-cannot-stand-in-test
  (let [context (v970-context)
        wrong-version (v970-guard 5)
        wrong-target (command/guard
                      :fixture/other
                      (assoc (v970-eligibility 4)
                             :xt/id "different-dependency")
                      v970-version)]
    (doseq [[label replacement]
            [["wrong version" wrong-version]
             ["wrong target" wrong-target]]]
      (testing label
        (let [calls (atom 0)
              submitted (atom 0)
              pair (v970-pair (fn [_ _] (v970-plan [replacement]))
                              context calls)]
          (is (= :domain-construction-exception
                 (kind #(v970-prepare pair))))
          (with-redefs [tx/transact!
                        (fn [& _]
                          (swap! submitted inc)
                          {:commit/status :committed})]
            (is (= :missing-required-commit-guard
                   (kind #(v970-submit pair context)))))
          (is (zero? @submitted))
          (is (= 2 @calls)))))))

(deftest v970-equivalent-update-precondition-can-protect-dependency-test
  (let [before (v970-eligibility 4)
        after (assoc before
                     :fixture/revision 5
                     :fixture/updated-at (.plusSeconds v970-time 1))
        guarded-update (command/update-command
                        :fixture/eligibility :renew before after v970-version)
        calls (atom 0)
        pair (v970-pair (fn [_ _]
                          {:commands [guarded-update]
                           :guards []
                           :emit false})
                        (v970-context) calls)
        submitted (atom 0)]
    (is (= [guarded-update] (:commands (v970-prepare pair))))
    (with-redefs [tx/transact!
                  (fn [& _]
                    (swap! submitted inc)
                    {:commit/status :committed})]
      (is (= :incorporated
             (:status (v970-submit pair (v970-context))))))
    (is (= 1 @submitted))
    (is (= 2 @calls))))

(deftest v970-invalid-declared-guard-is-rejected-before-construction-test
  (let [baseline (v970-context)
        invalid [["missing path" (update baseline :eligibility dissoc :guard)
                  :missing-required-semantic-guard]
                 ["noncanonical guard" (assoc-in baseline [:eligibility :guard]
                                                  {:arbitrary true})
                  :invalid-required-semantic-guard]
                 ["present nil guard" (assoc-in baseline [:eligibility :guard] nil)
                  :invalid-required-semantic-guard]]]
    (doseq [[label modeled expected] invalid]
      (testing label
        (let [calls (atom 0)
              pair (v970-pair (fn [ctx _]
                                (v970-plan [(get-in ctx [:eligibility :guard])]))
                              modeled calls)]
          (is (= expected
                 (kind #(v970-prepare pair)))
              "The invalid guard must be found before domain construction")
          (is (zero? @calls)))))
    (let [calls (atom 0)
          pair (v970-pair (fn [ctx _]
                            (v970-plan [(get-in ctx [:eligibility :guard])]))
                          baseline calls)]
      (doseq [[label production expected] invalid]
        (testing label
          (let [error (v966-error #(v970-submit pair production))]
            (is (= expected (:error/kind (ex-data error))))
            (is (= #{:error/type :error/kind}
                   (set (keys (ex-data error))))))))
      (is (zero? @calls) "Invalid production dependencies block construction"))))

(deftest v970-guard-path-declarations-and-input-comparison-remain-scoped-test
  (let [context (v970-context)
        calls (atom 0)
        covered (v970-coverage)
        config (v970-config (fn [ctx _]
                              (v970-plan [(get-in ctx [:eligibility :guard])]))
                            context covered calls)
        pair (shared-family/build-pair config)
        compare! (:compare-production-input! pair)
        binding (v970-binding)]
    (is (= :invalid-context-coverage
           (kind #(shared-family/build-pair
                   (assoc config :context-coverage
                          (assoc covered :required-guard-paths
                                 #{[:not-declared :guard]}))))))
    ;; A one-segment path is structurally legal, but it resolves to the
    ;; enclosing map rather than to a canonical guard and must fail on use.
    (let [single-path (shared-family/build-pair
                       (assoc config :context-coverage
                              (assoc covered :required-guard-paths
                                     #{[:eligibility]})))]
      (is (= :invalid-required-semantic-guard
             (kind #(v970-prepare single-path)))))
    (let [report (compare!
                  {} "v970-actor" {:resource "v970-resource"}
                  context binding)]
      (is (= :matched-declared-input-fields (:status report)))
      (is (= 1 (:declared-semantic-guards-checked report)))
      (is (= :not-checked
             (:semantic-guard-commit-enforcement-in-this-comparison report)))
      (is (= :not-established
             (:semantic-guard-dependency-completeness report)))
      (is (= :not-established (:application-certification report)))
      (is (zero? @calls)
          "Pure context comparison may not construct or transact"))
    (is (= :production-context-differs-from-model-input
           (kind #(compare! {} "v970-actor" {:resource "v970-resource"}
                           (assoc-in context [:other :nested :present?] false)
                           binding))))
    (is (zero? @calls))))


;; v972 — A declared guard is checked against its acquired document snapshot
;; on both the finite and production paths. This checks the authored relationship
;; only; snapshot origin and the completeness of domain authorization remain
;; independently assumed. All fixture entities are synthetic Gesso test data.

(defn- v972-context
  ([] (v972-context 4))
  ([revision]
   (assoc-in (v970-context (v970-guard revision))
             [:eligibility :document]
             (v970-eligibility revision))))

(defn- v972-coverage []
  (assoc (v970-coverage)
         :required-guard-snapshots
         {[:eligibility :guard]
          {:document-path [:eligibility :document]
           :entity-type :fixture/eligibility}}))

(defn- v972-pair
  ([modeled calls]
   (v972-pair modeled (v972-coverage) calls))
  ([modeled coverage calls]
   (shared-family/build-pair
    (v970-config (fn [ctx _]
                   (v970-plan [(get-in ctx [:eligibility :guard])]))
                 modeled coverage calls))))

(deftest v972-guard-snapshot-binding-accepts-correct-finite-and-production-inputs-test
  (let [context (v972-context)
        calls (atom 0)
        submissions (atom 0)
        pair (v972-pair context calls)
        planned (v970-prepare pair)]
    (is (= [(get-in context [:eligibility :guard])]
           (:guards planned)))
    (is (= 1 @calls))
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 {:commit/status :committed})]
      (is (= :incorporated (:status (v970-submit pair context)))))
    (is (= 1 @submissions))
    (is (= 2 @calls))
    (let [report ((:compare-production-input! pair)
                  {} "v970-actor" {:resource "v970-resource"}
                  context (v970-binding))]
      (is (= :matched-declared-input-fields (:status report)))
      (is (= 1 (:declared-semantic-guards-checked report)))
      (is (= 1 (:declared-guard-snapshots-checked report)))
      (is (= :caller-supplied-not-authenticated
             (:guard-snapshot-origin report)))
      (is (= :not-established (:application-certification report)))
      (is (= 2 @calls) "Context comparison must not invoke the constructor"))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-guard-to-modeled-snapshot-correspondence))
    (is (contains? (get-in pair [:scope :assumed])
                   :truthful-guard-snapshot-acquisition-and-identity))
    (is (contains? (get-in pair [:scope :not-established])
                   :truth-of-acquired-document-snapshots))
    (is (contains? (get-in pair [:scope :not-established])
                   :completeness-of-declared-guard-snapshot-bindings))))

(deftest v972-missing-snapshots-fail-before-finite-and-production-construction-test
  (let [good (v972-context)
        missing (update good :eligibility dissoc :document)
        calls (atom 0)
        submissions (atom 0)
        pair (v972-pair good calls)
        missing-model-pair (v972-pair missing calls)]
    (is (= :missing-required-guard-snapshot
           (kind #(v970-prepare missing-model-pair))))
    (is (zero? @calls) "A missing model observation cannot reach :construct!")
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! submissions inc)
                                 {:commit/status :committed})]
      (is (= :missing-required-guard-snapshot
             (kind #(v970-submit pair missing)))))
    (is (zero? @calls) "A missing production observation cannot construct")
    (is (zero? @submissions) "No transaction may be submitted")
    (is (= :missing-required-guard-snapshot
           (kind #((:compare-production-input! pair)
                   {} "v970-actor" {:resource "v970-resource"}
                   missing (v970-binding)))))
    (is (zero? @calls) "Pure comparison may not execute domain construction")))

(deftest v972-mismatched-snapshots-fail-on-both-paths-test
  (let [good (v972-context)
        variants
        [["wrong document identity"
          (assoc-in good [:eligibility :document :xt/id] "other-document")]
         ["wrong document revision"
          (assoc-in good [:eligibility :document :fixture/revision] 999)]
         ["wrong update timestamp"
          (assoc-in good [:eligibility :document :fixture/updated-at]
                    (.plusSeconds v970-time 10))]
         ["missing compared field"
          (update-in good [:eligibility :document]
                     dissoc :fixture/revision)]
         ["wrong guard entity type"
          (assoc-in good [:eligibility :guard :model/entity-type]
                    :fixture/unrelated)]
         ["wrong guard expected identity"
          (assoc-in good [:eligibility :guard :model/expected :model/id]
                    "other-document")]
         ["wrong guard expected revision"
          (update-in good [:eligibility :guard :model/expected
                           :model/checks]
                     (fn [checks]
                       (mapv (fn [[field value]]
                               [field (if (= field :fixture/revision)
                                        999 value)])
                             checks)))]]
        calls (atom 0)
        submissions (atom 0)
        production (v972-pair good calls)]
    (doseq [[label bad] variants]
      (testing label
        (let [finite (v972-pair bad calls)
              before @calls]
          (is (= :required-guard-snapshot-mismatch
                 (kind #(v970-prepare finite))))
          (is (= before @calls)
              "Finite validation must fail before :construct!"))
        (let [before @calls]
          (with-redefs [tx/transact! (fn [& _]
                                       (swap! submissions inc)
                                       {:commit/status :committed})]
            (is (= :required-guard-snapshot-mismatch
                   (kind #(v970-submit production bad)))))
          (is (= before @calls)
              "Production validation must fail before :construct!"))))
    (is (zero? @submissions))))

(deftest v972-snapshot-declarations-are-closed-and-bounded-test
  (let [good (v972-context)
        calls (atom 0)
        coverage (v972-coverage)
        config (v970-config (fn [ctx _]
                              (v970-plan [(get-in ctx [:eligibility :guard])]))
                            good coverage calls)
        excessive (into {}
                        (for [n (range 257)]
                          [[:eligibility :guard n]
                           {:document-path [:eligibility :document]
                            :entity-type :fixture/eligibility}]))
        invalid
        [["empty descriptor map" {}]
         ["nil descriptors" nil]
         ["sequential descriptors" [[:eligibility :guard]]]
         ["extra descriptor field"
          {[:eligibility :guard]
           {:document-path [:eligibility :document]
            :entity-type :fixture/eligibility
            :source :untrusted}}]
         ["missing entity type"
          {[:eligibility :guard]
           {:document-path [:eligibility :document]}}]
         ["non-keyword entity type"
          {[:eligibility :guard]
           {:document-path [:eligibility :document] :entity-type "eligibility"}}]
         ["undeclared guard path"
          {[:other :unknown]
           {:document-path [:eligibility :document]
            :entity-type :fixture/eligibility}}]
         ["undeclared document root"
          {[:eligibility :guard]
           {:document-path [:unmodeled :document]
            :entity-type :fixture/eligibility}}]
         ["non-vector document path"
          {[:eligibility :guard]
           {:document-path '(:eligibility :document)
            :entity-type :fixture/eligibility}}]
         ["too deep document path"
          {[:eligibility :guard]
           {:document-path (vec (cons :eligibility (repeat 32 :nested)))
            :entity-type :fixture/eligibility}}]
         ["too many entries" excessive]]]
    (doseq [[label descriptors] invalid]
      (testing label
        (is (= :invalid-context-coverage
               (kind #(shared-family/build-pair
                       (assoc config :context-coverage
                              (assoc coverage :required-guard-snapshots
                                     descriptors))))))))
    (is (zero? @calls) "Invalid declarations may not construct operations")))

(deftest v972-guard-snapshot-coverage-is-opt-in-and-not-a-truth-proof-test
  (let [good (v972-context)
        calls (atom 0)
        without-snapshot-contract (v972-pair good (v970-coverage) calls)
        differing (assoc-in good [:eligibility :document :fixture/revision] 9)
        fully-checked (v972-pair good calls)]
    ;; Legacy guards still protect the plan, but there is no binding to the
    ;; supplied acquisition document unless explicitly declared.
    (is (map? (v970-prepare without-snapshot-contract)))
    (is (= 1 @calls))
    (is (= :required-guard-snapshot-mismatch
           (kind #(v970-prepare
                   (v972-pair differing calls)))))
    (is (= 1 @calls))
    ;; Two internally consistent, caller-supplied values are not evidence that
    ;; they came from an actual database observation.
    (let [fabricated (v972-context 9)
          candidate (v972-pair fabricated calls)]
      (is (map? (v970-prepare candidate)))
      (is (= 2 @calls))
      (let [report ((:compare-production-input! candidate)
                    {} "v970-actor" {:resource "v970-resource"}
                    fabricated (v970-binding))]
        (is (= :caller-supplied-not-authenticated
               (:guard-snapshot-origin report)))
        (is (= :not-established (:application-certification report)))))
    (is (= :required-guard-snapshot-mismatch
           (kind #(v970-submit fully-checked differing))))
    ;; A version guard compares its declared CAS fields, not every document
    ;; attribute. In this fixture :fixture/created-at is not a compare field.
    ;; The unchanged guard therefore still matches that supplied document.
    (let [uncompared (assoc-in good
                               [:eligibility :document :fixture/created-at]
                               (.plusSeconds v970-time 10))
          not-all-document-fields-protected (v972-pair uncompared calls)]
      (is (map? (v970-prepare not-all-document-fields-protected)))
      (is (contains? (get-in not-all-document-fields-protected
                              [:scope :not-established])
                     :commit-time-enforcement-of-undeclared-semantic-facts)))))

;; v974 — The selected finite prestate independently constrains snapshots
;; returned by application-owned model-input!. This is an opt-in assertion
;; about the *finite model*, not evidence that physical XTDB acquired the
;; asserted records or that the application's guard inventory is complete.

(def ^:private v974-document-key
  [:fixture/eligibility "v970-eligibility"])

(defn- v974-state []
  {v974-document-key (v970-eligibility 4)})

(defn- v974-coverage []
  (assoc (v972-coverage)
         :required-model-state-snapshots #{[:eligibility :guard]}))

(defn- v974-pair
  ([modeled calls]
   (v974-pair modeled (v974-coverage) calls))
  ([modeled coverage calls]
   (v972-pair modeled coverage calls)))

(defn- v974-prepare [pair state]
  ((get-in pair [:family :prepare])
   state "v970-actor" {:resource "v970-resource"}))

(defn- v974-compare [pair state production]
  ((:compare-production-input! pair)
   state "v970-actor" {:resource "v970-resource"}
   production (v970-binding)))

(deftest v974-selected-finite-prestate-matches-declared-snapshot-test
  (let [context (v972-context)
        calls (atom 0)
        pair (v974-pair context calls)
        state (v974-state)
        prepared (v974-prepare pair state)]
    (is (= [(get-in context [:eligibility :guard])] (:guards prepared)))
    (is (= 1 @calls) "One finite construction follows successful validation")
    (let [report (v974-compare pair state context)]
      (is (= :matched-declared-input-fields (:status report)))
      (is (= 1 (:declared-semantic-guards-checked report)))
      (is (= 1 (:declared-guard-snapshots-checked report)))
      (is (= 1 (:declared-finite-prestate-snapshots-checked report)))
      (is (= :caller-supplied-not-authenticated (:finite-prestate-origin report)))
      (is (= :caller-supplied-not-authenticated (:input-origin report)))
      (is (= :not-established (:application-certification report)))
      (is (= 1 @calls) "Comparison cannot execute domain construction")
      (is (not (contains? report :context)))
      (is (not (contains? report :binding))))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-snapshot-to-selected-finite-prestate-correspondence))
    (is (contains? (get-in pair [:scope :assumed])
                   :truthful-selection-of-the-finite-prestate))
    (is (contains? (get-in pair [:scope :not-established])
                   :correspondence-of-finite-prestate-to-production-xtdb))))

(deftest v974-fabricated-observation-is-rejected-before-domain-construction-test
  (let [actual (v974-state)
        context (v972-context 5)
        calls (atom 0)
        pair (v974-pair context calls)]
    ;; The guard and acquired snapshot agree with each other; only the
    ;; selected finite PRESTATE exposes their fabrication.
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-prepare pair actual))))
    (is (zero? @calls))
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-compare pair actual context))))
    (is (zero? @calls) "Even the comparison path cannot construct")))

(deftest v974-missing-or-malformed-finite-prestate-fails-closed-test
  (let [context (v972-context)
        calls (atom 0)
        pair (v974-pair context calls)
        missing [[:empty {}]
                 [:wrong-identity {[:fixture/eligibility "unrelated"]
                                   (assoc (v970-eligibility 4) :xt/id "unrelated")}]]
        malformed [[:atom {v974-document-key (atom {:secret "private-prestate"})}]
                   [:lazy-state {v974-document-key (map identity [:invalid])}]
                   [:nonmap [1 2 3]]]]
    (doseq [[label state] missing]
      (testing (name label)
        (is (= :required-snapshot-missing-from-finite-prestate
               (kind #(v974-prepare pair state))))
        (is (= :required-snapshot-missing-from-finite-prestate
               (kind #(v974-compare pair state context))))))
    (doseq [[label state] malformed]
      (testing (name label)
        (let [e (v966-error #(v974-prepare pair state))]
          (is (= :invalid-finite-semantic-prestate
                 (:error/kind (ex-data e))))
          (is (= #{:error/kind :error/type}
                 (set (keys (ex-data e)))))
          (is (not (.contains (pr-str (ex-data e)) "private-prestate"))))))
    (is (zero? @calls) "No invalid prestate reaches :construct!")))

(deftest v974-full-document-equality-catches-unguarded-fields-test
  (let [context (v972-context)
        state (assoc-in (v974-state)
                        [v974-document-key :fixture/unguarded-field]
                        :changed-after-observation)
        calls (atom 0)
        pair (v974-pair context calls)]
    ;; The version guard is unchanged; validating only :model/checks would
    ;; miss the difference in the selected finite document.
    (is (= (get-in context [:eligibility :guard :model/expected :model/checks])
           (get-in (v972-context) [:eligibility :guard :model/expected :model/checks])))
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-prepare pair state))))
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-compare pair state context))))
    (is (zero? @calls))
    ;; No finite prestate is supplied on the physical path. Gesso must not
    ;; mistake this checked finite relation for physical XTDB authentication.
    (let [submitted (atom 0)]
      (with-redefs [tx/transact! (fn [& _]
                                   (swap! submitted inc)
                                   {:commit/status :committed})]
        (is (= :incorporated (:status (v970-submit pair context)))))
      (is (= 1 @submitted))
      (is (= 1 @calls)))))

(deftest v974-prestate-declarations-are-closed-and-opt-in-test
  (let [context (v972-context)
        coverage (v974-coverage)
        calls (atom 0)
        invalid [["empty set" #{}]
                 ["vector rather than set" [[:eligibility :guard]]]
                 ["nil" nil]
                 ["not in snapshot inventory" #{[:other :guard]}]]]
    (doseq [[label paths] invalid]
      (testing label
        (is (= :invalid-context-coverage
               (kind #(v974-pair context
                                 (assoc coverage :required-model-state-snapshots paths)
                                 calls))))))
    (is (= :invalid-context-coverage
           (kind #(v974-pair context
                             (dissoc coverage :required-guard-snapshots)
                             calls))))
    (is (= :invalid-context-coverage
           (kind #(v974-pair context
                             (dissoc coverage :modeled-values)
                             calls))))
    ;; Previous contracts do not independently check the finite prestate.
    (let [without-prestate-contract (v974-pair (v972-context 5)
                                                   (v972-coverage) calls)]
      (is (map? (v974-prepare without-prestate-contract (v974-state))))
      (is (= :not-declared
             (:finite-prestate-origin
              (v974-compare without-prestate-contract
                            (v974-state) (v972-context 5))))))
    (is (= 1 @calls))))

(deftest v974-state-dependent-model-input-is-rechecked-every-time-test
  (let [current (atom (v972-context))
        calls (atom 0)
        ;; The model-input callback may change its observation between
        ;; its observation between separate invocations, as application-owned
        ;; acquisition functions may do.
        dynamic-pair (shared-family/build-pair
                      (assoc (v970-config
                              (fn [ctx _]
                                (v970-plan [(get-in ctx [:eligibility :guard])]))
                              @current (v974-coverage) calls)
                             :model-input!
                             (fn [_ _ _]
                               {:context @current :binding (v970-binding)})))
        state (v974-state)]
    (is (map? (v974-prepare dynamic-pair state)))
    (is (= 1 @calls))
    (reset! current (v972-context 5))
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-prepare dynamic-pair state))))
    (is (= :required-snapshot-finite-prestate-mismatch
           (kind #(v974-compare dynamic-pair state @current))))
    (is (= 1 @calls) "Changed observation cannot reach the constructor")))

;; v976 — Permanent document-identity absence controls. These tests exercise
;; only the opt-in finite-prestate absence / canonical-create correspondence;
;; they do not assert query-wide uniqueness or authenticate physical XTDB reads.

(def ^:private v976-resource-key [:fixture/resource "v970-resource"])

(defn- v976-context []
  (assoc (v970-context) :allocation {:target-id "v970-resource"}))

(defn- v976-coverage []
  (assoc (v970-coverage)
         :modeled #{:eligibility :other :allocation}
         :required-create-absences {[:allocation :target-id] :fixture/resource}))

(defn- v976-state []
  {[:fixture/eligibility "v970-eligibility"] (v970-eligibility 4)})

(defn- v976-builder [context _]
  (v970-plan [(get-in context [:eligibility :guard])]))

(defn- v976-pair
  ([context calls]
   (v976-pair context (v976-coverage) calls v976-builder))
  ([context coverage calls builder]
   (shared-family/build-pair (v970-config builder context coverage calls))))

(defn- v976-prepare [pair state]
  ((get-in pair [:family :prepare])
   state "v970-actor" {:resource "v970-resource"}))

(defn- v976-compare [pair state production]
  ((:compare-production-input! pair)
   state "v970-actor" {:resource "v970-resource"}
   production (v970-binding)))

(deftest v976-declared-document-absence-is-protected-in-both-plans-test
  (let [context (v976-context)
        calls (atom 0)
        pair (v976-pair context calls)
        prepared (v976-prepare pair (v976-state))
        submitted (atom 0)]
    (is (= v976-resource-key (command/target (first (:commands prepared)))))
    (is (= 1 @calls))
    (let [report (v976-compare pair (v976-state) context)]
      (is (= :matched-declared-input-fields (:status report)))
      (is (= 1 (:declared-create-absences-checked report)))
      (is (= :not-authenticated (:create-absence-physical-origin report)))
      (is (= :not-established (:application-certification report)))
      (is (not (contains? report :context)))
      (is (not (contains? report :binding)))
      (is (= 1 @calls) "Input comparison never runs the constructor"))
    (with-redefs [tx/transact! (fn [& _]
                                (swap! submitted inc)
                                {:commit/status :committed})]
      (is (= :incorporated (:status (v970-submit pair context)))))
    (is (= 1 @submitted))
    (is (= 2 @calls) "One constructor call on each construction path")
    (is (contains? (get-in pair [:scope :checked])
                   :optional-declared-create-absence-in-selected-finite-prestate))
    (is (contains? (get-in pair [:scope :checked])
                   :optional-declared-create-absence-to-transaction-correspondence))
    (is (contains? (get-in pair [:scope :not-established])
                   :physical-truth-of-declared-create-absences))
    (is (contains? (get-in pair [:scope :not-established])
                   :completeness-of-declared-create-absence-dependencies))))

(deftest v976-existing-target-is-rejected-before-domain-construction-test
  (let [context (v976-context)
        calls (atom 0)
        pair (v976-pair context calls)
        existing (get-in (v970-plan [(get-in context [:eligibility :guard])])
                         [:commands 0 :model/after])
        occupied (assoc (v976-state) v976-resource-key existing)]
    (is (= :required-create-target-not-absent-in-finite-prestate
           (kind #(v976-prepare pair occupied))))
    (is (= :required-create-target-not-absent-in-finite-prestate
           (kind #(v976-compare pair occupied context))))
    (is (zero? @calls) "An occupied target must not reach :construct!")
    ;; Same document ID in a different entity type is NOT the declared target.
    (is (map? (v976-prepare pair
                            (assoc (v976-state)
                                   [:fixture/unrelated "v970-resource"]
                                   (assoc existing :xt/id "v970-resource")))))
    (is (= 1 @calls))))

(deftest v976-required-create-targets-are-rechecked-and-redacted-test
  (let [context (v976-context)
        calls (atom 0)
        pair (v976-pair context calls)
        missing (assoc context :allocation {})
        nil-id (assoc-in context [:allocation :target-id] nil)
        opaque-id (assoc-in context [:allocation :target-id]
                            (atom "private-create-target"))]
    (doseq [[label invalid wanted]
            [["missing path" missing :missing-required-create-absence-target]
             ["nil ID" nil-id :invalid-required-create-absence-target]
             ["opaque ID" opaque-id :non-data-modeled-context]]]
      (testing label
        (let [bad (v976-pair invalid (atom 0))
              error (v966-error #(v976-prepare bad (v976-state)))]
          (is (= wanted (:error/kind (ex-data error))))
          (is (= #{:error/kind :error/type} (set (keys (ex-data error)))))
          (is (not (.contains (pr-str (ex-data error)) "private-create-target"))))))
    (is (= :missing-required-create-absence-target
           (kind #(v976-compare pair (v976-state) missing))))
    (is (= :invalid-required-create-absence-target
           (kind #(v976-compare pair (v976-state) nil-id))))
    (is (zero? @calls) "Rejected modeled/production facts cannot construct")))

(deftest v976-incorrect-or-conflicting-create-cannot-protect-absence-test
  (let [context (v976-context)
        coverage (v976-coverage)
        base (v970-plan [(get-in context [:eligibility :guard])])
        original (first (:commands base))
        changed-id (assoc base :commands
                          [(command/create :fixture/resource
                                           (assoc (:model/after original)
                                                  :xt/id "other-resource")
                                           v970-version)])
        changed-type (assoc base :commands
                            [(command/create :fixture/unrelated
                                             (:model/after original)
                                             v970-version)])
        duplicate-target (assoc base :commands
                                [original
                                 (command/create :fixture/resource
                                                 (assoc (:model/after original)
                                                        :fixture/variant :different)
                                                 v970-version)])]
    (doseq [[label plan expected]
            [["wrong document identity" changed-id
              :missing-required-absence-protecting-create]
             ["wrong entity type" changed-type
              :missing-required-absence-protecting-create]
             ["duplicate conflicting creates" duplicate-target
              :unsupported-guarded-domain-plan]]]
      (testing label
        (let [calls (atom 0)
              submissions (atom 0)
              pair (v976-pair context coverage calls (fn [_ _] plan))]
          (is (= :domain-construction-exception
                 (kind #(v976-prepare pair (v976-state)))))
          (is (= 1 @calls))
          (with-redefs [tx/transact! (fn [& _]
                                      (swap! submissions inc)
                                      {:commit/status :committed})]
            (let [error (v966-error #(v970-submit pair context))]
              (is (= expected (:error/kind (ex-data error))))
              (is (= #{:error/type :error/kind} (set (keys (ex-data error)))))))
          (is (zero? @submissions) "Conflicting creates never submit")
          (is (= 2 @calls)))))))

(deftest v976-create-absence-declaration-is-strict-and-bounded-test
  (let [context (v976-context)
        coverage (v976-coverage)
        invalid [["empty" {}]
                 ["nil" nil]
                 ["not a map" [[:allocation :target-id]]]
                 ["unqualified entity" {[:allocation :target-id] :resource}]
                 ["string entity" {[:allocation :target-id] "fixture/resource"}]
                 ["empty path" {[] :fixture/resource}]
                 ["path not vector" {'(:allocation :target-id) :fixture/resource}]
                 ["wrong root" {[:excluded :target-id] :fixture/resource}]
                 ["object segment" {[:allocation (Object.)] :fixture/resource}]
                 ["deep path" {(vec (cons :allocation (repeat 32 :field)))
                                 :fixture/resource}]
                 ["too many paths" (into {}
                                          (for [n (range 257)]
                                            [[:allocation n] :fixture/resource]))]]]
    (doseq [[label declaration] invalid]
      (testing label
        (is (= :invalid-context-coverage
               (kind #(v976-pair context
                                 (assoc coverage :required-create-absences declaration)
                                 (atom 0) v976-builder))))))
    (is (= :invalid-context-coverage
           (kind #(v976-pair context
                             (dissoc coverage :modeled-values)
                             (atom 0) v976-builder))))
    (is (= :invalid-context-coverage
           (kind #(v976-pair context
                             (assoc coverage :unrecognized-assurance true)
                             (atom 0) v976-builder))))))

(deftest v976-prestate-absence-is-opt-in-and-state-dependent-test
  (let [context (v976-context)
        coverage (v976-coverage)
        current (atom context)
        calls (atom 0)
        dynamic (shared-family/build-pair
                 (assoc (v970-config v976-builder context coverage calls)
                        :model-input!
                        (fn [_ _ _]
                          {:context @current :binding (v970-binding)})))
        baseline (v976-state)
        old (v976-pair context (dissoc coverage :required-create-absences)
                       (atom 0) v976-builder)]
    (is (map? (v976-prepare dynamic baseline)))
    (is (= 1 @calls))
    (reset! current (assoc-in context [:allocation :target-id] "other-target"))
    (is (= :domain-construction-exception
           (kind #(v976-prepare dynamic baseline)))
        "A changed target cannot reuse the constructor's old create target")
    (is (= 2 @calls))
    ;; Production input comparison is a value correspondence check, not a
    ;; proof that the physical database has no such document.
    (is (= :production-context-differs-from-model-input
           (kind #(v976-compare dynamic baseline context))))
    (is (map? (v976-prepare old
                            (assoc baseline v976-resource-key
                                   (get-in (v970-plan [(get-in context [:eligibility :guard])])
                                           [:commands 0 :model/after])))))
    (is (= 2 @calls))))
