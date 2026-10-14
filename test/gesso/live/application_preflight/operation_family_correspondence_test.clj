(ns gesso.live.application-preflight.operation-family-correspondence-test
  "Permanent v829 and v866 regressions for registered operation-family correspondence.

   A finite operation family must match the exact executor from a freshly
   checked application assembly. An observed XTDB transaction can demonstrate
   bounded preparation/execution correspondence, but not domain-policy
   completeness, truthful projections, or universal refinement."
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.operation-family-correspondence :as correspondence]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.resolution-assurance :as assurance]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.resolved-operation :as resolved]
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
