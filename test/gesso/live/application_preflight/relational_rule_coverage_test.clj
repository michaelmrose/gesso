(ns gesso.live.application-preflight.relational-rule-coverage-test
  "Permanent v843 integration regressions for source-interpreted relational
   rules admitted through a CURRENT Gesso application registry.

   The tests deliberately distinguish: exact registered operation-ID coverage,
   bounded model checking of declared relations, optional correspondence with
   one genuinely observed XTDB transaction, and business-policy completeness.
   A passing finite model is not proof of a correct application or projection."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.operation-family-coverage
    :as registry-coverage]
   [gesso.live.application-preflight.relational-rule-coverage :as coverage]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.resolution-assurance :as assurance]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.resolved-operation :as resolved]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private options {:max-depth 2 :max-transitions 40})
(def ^:private counter-target [:counters :counter-1])
(def ^:private counter-state
  {counter-target {:xt/id :counter-1 :revision 0 :created-at :t0
                   :updated-at :t0 :parent nil :active? true :n 0}})
(def ^:private counter-relation
  {:id :counters/parent-active
   :source-kind :counters :source-where {}
   :reference-attr :parent :allow-unset? true
   :target-kind :counters :target-where {:active? true}})

(defn- counter-family [operation & {:keys [bad-parent? calls]
                                   :or {bad-parent? false}}]
  {:id operation
   :principals [:helper/a]
   :arguments [{:delta 1}]
   :enabled? (fn [_ _ _]
               (when calls (swap! calls inc))
               true)
   :prepare (fn [state _ _]
              (when calls (swap! calls inc))
              (let [before (get state counter-target)]
                {:commands
                 [(command/update-command
                   :counters :increment before
                   (cond-> (-> before
                               (update :revision inc)
                               (update :n inc)
                               (assoc :updated-at :t1))
                     bad-parent? (assoc :parent :nonexistent))
                   version)]
                 :guards [] :emit false}))})

(defn- counter-families
  ([] (counter-families false))
  ([bad?]
   [(counter-family :request/claim :bad-parent? bad?)
    (counter-family :request/reassign)
    (counter-family :request/cancel)]))

(defn- problem [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- kind [f] (:error/kind (problem f)))

(defn- with-standard-application [f]
  (fixture/with-closed-application f))

(defn- check-counter
  ([assembly] (check-counter assembly (counter-families) [counter-relation] options))
  ([assembly families rules opts]
   (coverage/check-registered-relations
    assembly counter-state families rules opts)))

(deftest admitted-relationship-is-inspectable-but-not-certifying-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [result (check-counter assembly)
            report (:relational-coverage result)
            root (first (:initial-state-rule-coverage report))]
        (is (= :holds-in-explored-model (:status result)))
        (is (= :exact-registered-operation-id-coverage
               (get-in result [:registry-coverage :status])))
        (is (= [(:id counter-relation)] (:rule-ids report)))
        (is (= 1 (:initially-matching-source-count root)))
        (is (true? (:initially-exercised? root)))
        (is (true? (:initially-holds? root)))
        (is (empty? (:initially-unexercised-rule-ids report)))
        (is (= [counter-relation] (:relational-declarations result)))
        (is (= :not-established (:declaration-completeness report)))
        (is (= :not-established (:registered-executor-correspondence report)))
        (is (contains? (get-in result [:registered-relational-scope :not-established])
                       :whole-application-certification))))))

(deftest state-dependent-relational-counterexample-identifies-rule-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [result (check-counter assembly (counter-families true)
                                  [counter-relation] options)]
        (is (= :counterexample (:status result)))
        (is (= :counters/parent-active (get-in result [:violations 0 :rule-id])))
        (is (= 1 (get-in result [:violations 0 :depth]))
            "The counterexample must arise from an attempted operation")))))

(deftest missing-operation-rejected-before-business-callbacks-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [calls (atom 0)
            families [(counter-family :request/claim :calls calls)
                      (counter-family :request/reassign :calls calls)]]
        (is (= :operation-identity-coverage-mismatch
               (kind #(check-counter assembly families [counter-relation] options))))
        (is (zero? @calls))))))

(deftest invented-operation-rejected-before-business-callbacks-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [calls (atom 0)
            families (conj (counter-families)
                           (counter-family :other/fabricated :calls calls))]
        (is (= :operation-identity-coverage-mismatch
               (kind #(check-counter assembly families [counter-relation] options))))
        (is (zero? @calls))))))

(deftest duplicate-operation-identities-are-not-counted-as-coverage-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [families (assoc (counter-families) 1
                            (counter-family :request/claim))]
        (is (= :duplicate-family
               (kind #(check-counter assembly families [counter-relation] options))))))))

(deftest oversized-finite-domain-fails-without-sampling-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [families (mapv #(assoc % :principals [:a :b :c])
                           (counter-families))]
        (is (= :instance-bound-exceeded
               (kind #(check-counter assembly families [counter-relation] options))))))))

(deftest budget-exhaustion-is-not-success-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (is (= :exploration-budget-exhausted
             (kind #(check-counter assembly (counter-families)
                                   [counter-relation]
                                   {:max-depth 2 :max-transitions 2})))))))

(deftest opaque-business-predicate-is-not-a-relational-rule-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (is (= :invalid-declaration
             (kind #(check-counter assembly (counter-families)
                                   [(assoc counter-relation
                                           :holds? (constantly true))]
                                   options)))))))

(deftest incomplete-or-duplicated-declarations-fail-closed-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (is (= :invalid-declarations
             (kind #(check-counter assembly (counter-families) [] options))))
      (is (= :invalid-declaration
             (kind #(check-counter assembly (counter-families)
                                   [(dissoc counter-relation :target-where)]
                                   options))))
      (is (= :duplicate-declaration
             (kind #(check-counter assembly (counter-families)
                                   [counter-relation counter-relation]
                                   options)))))))

(deftest initially-empty-source-domain-is-explicitly-vacuous-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [unused (assoc counter-relation
                          :id :other/parent-active
                          :source-kind :other)
            result (check-counter assembly (counter-families) [unused] options)]
        (is (= :holds-in-explored-model (:status result)))
        (is (= [:other/parent-active]
               (get-in result [:relational-coverage
                               :initially-unexercised-rule-ids])))
        (is (= 0 (get-in result [:relational-coverage
                                :initial-state-rule-coverage 0
                                :initially-matching-source-count])))
        (is (= :not-established
               (get-in result [:relational-coverage :declaration-completeness])))))))

(deftest missing-source-selector-attribute-is-a-violation-not-vacuity-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [rule (assoc counter-relation
                        :source-where {:missing-selector :expected})
            result (check-counter assembly (counter-families) [rule] options)]
        (is (= :counterexample (:status result)))
        (is (= :counters/parent-active (get-in result [:violations 0 :rule-id])))))))

(deftest stale-browser-artifact-is-not-current-construction-test
  (with-standard-application
    (fn [{:keys [assembly artifact-path]}]
      (is (= :holds-in-explored-model (:status (check-counter assembly))))
      (spit artifact-path "tampered outside generated receipt\n")
      (is (some? (problem #(check-counter assembly))))
      (is (false? (application/application-assembly? assembly))))))

(deftest detached-report-cannot-substitute-for-application-assembly-test
  (with-standard-application
    (fn [{:keys [assembly options]}]
      (let [report (application/check-application-assembly options)]
        (is (application/valid? report))
        (is (= :application-assembly-required
               (kind #(check-counter report (counter-families)
                                     [counter-relation] options))))))))

(deftest comparator-admission-rejects-a-finite-invariant-violation-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (is (= :modeled-invariant-violation
             (kind #(coverage/build-comparator
                     assembly counter-state (counter-families true)
                     [counter-relation] options)))))))

(deftest comparator-refuses-fabricated-handle-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [compare! (coverage/build-comparator
                      assembly counter-state (counter-families)
                      [counter-relation] options)]
        (is (fn? compare!))
        (is (= :unrecognized-executor-bound-evidence
               (kind #(compare! (Object.)))))
        (is (= :unrecognized-executor-bound-evidence
               (kind #(compare! {:status :matched}))))))))

;; The next tests cross the stronger boundary: an actual registered Live
;; executor and its actual XTDB transaction, not merely an admitted model.
(defonce ^:private !node (atom nil))
(def ^:private now "2026-10-12T12:00:00Z")
(def ^:private operation-id :request/claim)
(def ^:private principal (identity/principal "v833-helper"))

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(defn- db-row [table id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- real-example
  ([] (real-example {}))
  ([{:keys [false-projection?]}]
   (let [suffix (str (random-uuid))
         request-id (str "v833-request-" suffix)
         membership-id (str "v833-membership-" suffix)
         before {:xt/id request-id :revision 0 :created-at "t0"
                 :updated-at "t0" :owner "unclaimed"}
         member {:xt/id membership-id :revision 0 :created-at "t0"
                 :updated-at "t0" :active true}
         after (assoc before :revision 1 :updated-at now
                      :member-id membership-id :owner "v833-helper")
         command (protocol/command
                  {:command-id (identity/command-id (str "v833-command-" suffix))
                   :execution-id (identity/execution-id (str "v833-execution-" suffix))
                   :operation operation-id
                   :arguments {:request-id request-id}
                   :observed-basis {:tx-id 1 :system-time now}
                   :scope [:request request-id]})
         calls (atom {:construct 0 :project 0})
         model (resolved/build-operation
                {:operation operation-id
                 :read-record! (fn [_ id] (db-row :gesso_commands id))
                 :may-disclose? (fn [_ _ _] true)
                 :construct! (fn [_ _]
                               (swap! calls update :construct inc)
                               {:now now :outcome {:status :claimed}
                                :plan {:commands
                                       [(command/update-command
                                         :requests :claim before after version)]
                                       :guards [(command/guard
                                                 :memberships member version)]
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
                                   {:request/owner (if false-projection?
                                                     "wrong-projection"
                                                     (:owner (db-row :requests request-id)))}})
                                 :outcome :request/claimed})})
         runner (server/server
                 {:principal-fn (fn [ctx] (:authenticated-principal ctx))
                  :operations
                  {operation-id
                   (server/operation
                    {:name :request/claim-optimistic
                     :operation operation-id
                     :execute! executor
                     :settlement-contract
                     {:confirmed {:outcomes #{:request/claimed}
                                  :commit/status :committed}
                      :already-incorporated {:outcomes #{:request/claimed}
                                             :commit/status :committed}}})}})]
     (xt/execute-tx @!node [[:put-docs :requests before]
                            [:put-docs :memberships member]])
     {:request-id request-id :membership-id membership-id
      :before before :member member :after after
      :command command :executor executor :runner runner :calls calls})))

(defn- real-families [{:keys [request-id membership-id after]}]
  [{:id operation-id
    :principals ["v833-helper"]
    :arguments [{:request-id request-id}]
    :enabled? (fn [state _ {:keys [request-id]}]
                (and (nil? (get-in state [[:requests request-id] :member-id]))
                     (true? (get-in state [[:memberships membership-id] :active]))))
    :prepare (fn [state _ {:keys [request-id]}]
               {:commands [(command/update-command
                             :requests :claim
                             (get state [:requests request-id]) after version)]
                :guards [(command/guard
                          :memberships
                          (get state [:memberships membership-id]) version)]
                :emit false})}
   {:id :request/reassign
    :principals [:manager] :arguments [{:request-id request-id}]
    :enabled? (fn [& _] false)
    :prepare (fn [& _] (throw (ex-info "disabled" {})))}
   {:id :request/cancel
    :principals [:manager] :arguments [{:request-id request-id}]
    :enabled? (fn [& _] false)
    :prepare (fn [& _] (throw (ex-info "disabled" {})))}])

(def ^:private owner-relation
  {:id :request/assigned-membership-active
   :source-kind :requests :source-where {:owner "v833-helper"}
   :reference-attr :member-id :allow-unset? true
   :target-kind :memberships :target-where {:active true}})

(defn- real-state [{:keys [request-id membership-id before member]}]
  {[:requests request-id] before [:memberships membership-id] member})

(defn- with-real-assembly [executor f]
  (fixture/with-closed-application
    {:operations
     (assoc (fixture/standard-operations) operation-id
            (fixture/trusted-operation
             operation-id
             {:published-change-topics #{:request}
              :execute! executor}))}
    f))

(defn- run-real! [{:keys [runner command]}]
  (assurance/run-and-check!
   {:server runner
    :ctx {:biff.xtdb/node @!node :authenticated-principal principal}
    :command command}))

(deftest genuine-registered-execution-matches-declared-relation-test
  (let [sample (real-example)]
    (with-real-assembly
      (:executor sample)
      (fn [{:keys [assembly]}]
        (let [compare! (coverage/build-comparator
                        assembly (real-state sample) (real-families sample)
                        [owner-relation] {:max-depth 1 :max-transitions 10})
              handle (:evidence (run-real! sample))
              result (compare! handle)]
          (is (= :matched (:status result)))
          (is (= :request/claim (:operation result)))
          (is (= [:request/assigned-membership-active]
                 (:relational-rule-ids result)))
          (is (= :holds-in-explored-model (:relational-admission result)))
          (is (= :same-current-registered-executor
                 (:executor-correspondence result)))
          (is (= :committed (:physical-status result)))
          (is (= (:after sample) (db-row :requests (:request-id sample))))
          (is (some? (db-row :gesso_commands
                             (identity/raw-value
                              (get-in sample [:command :command-id])))))
          (is (= 1 (:construct @(:calls sample))))
          (is (contains? (get-in result [:registered-relational-scope :not-established])
                         :whole-application-certification)))))))

(deftest genuine-execution-does-not-prove-truthfulness-of-projection-test
  (let [sample (real-example {:false-projection? true})]
    (with-real-assembly
      (:executor sample)
      (fn [{:keys [assembly]}]
        (let [compare! (coverage/build-comparator
                        assembly (real-state sample) (real-families sample)
                        [owner-relation] {:max-depth 1 :max-transitions 10})
              result (compare! (:evidence (run-real! sample)))]
          (is (= :matched (:status result)))
          (is (= "v833-helper" (:owner (db-row :requests (:request-id sample)))))
          (is (contains? (get-in result [:registered-relational-scope :not-established])
                         :verified-browser-projection)))))))

(deftest valid-handle-is-not-transferable-to-another-adapter-test
  (let [sample (real-example)
        handle (:evidence (run-real! sample))
        alternate (real-example)]
    (with-real-assembly
      (:executor alternate)
      (fn [{:keys [assembly]}]
        (let [compare! (coverage/build-comparator
                        assembly (real-state alternate) (real-families alternate)
                        [owner-relation] {:max-depth 1 :max-transitions 10})]
          (is (= :unrecognized-executor-bound-evidence
                 (kind #(compare! handle)))))))))

(deftest admitted-family-guard-disagreement-is-a-counterexample-test
  (let [sample (real-example)]
    (with-real-assembly
      (:executor sample)
      (fn [{:keys [assembly]}]
        (let [wrong (update-in (real-families sample) [0 :prepare]
                               (fn [original]
                                 (fn [state who arguments]
                                   (assoc (original state who arguments)
                                          :guards []))))
              compare! (coverage/build-comparator
                        assembly (real-state sample) wrong
                        [owner-relation] {:max-depth 1 :max-transitions 10})
              result (compare! (:evidence (run-real! sample)))]
          (is (= :counterexample (:status result)))
          (is (false? (:matching-guards? result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result))))))))

(deftest admitted-family-can-exclude-real-principal-test
  (let [sample (real-example)]
    (with-real-assembly
      (:executor sample)
      (fn [{:keys [assembly]}]
        (let [families (assoc-in (real-families sample)
                                 [0 :principals] ["different-helper"])
              compare! (coverage/build-comparator
                        assembly (real-state sample) families
                        [owner-relation] {:max-depth 1 :max-transitions 10})]
          (is (= :execution-outside-admitted-family-domain
                 (kind #(compare! (:evidence (run-real! sample)))))))))))

(deftest comparator-revalidates-physical-application-artifact-test
  (let [sample (real-example)]
    (with-real-assembly
      (:executor sample)
      (fn [{:keys [assembly artifact-path]}]
        (let [compare! (coverage/build-comparator
                        assembly (real-state sample) (real-families sample)
                        [owner-relation] {:max-depth 1 :max-transitions 10})
              handle (:evidence (run-real! sample))]
          (spit artifact-path "changed after comparator admission\n")
          (is (some? (problem #(compare! handle)))))))))


;; v842 extends relational checking with source-matching observations gathered
;; while the finite checker actually evaluates the visited state occurrences.
;; These are diagnostic coverage facts, never a proof of complete policies,
;; complete input domains, or actual production execution correspondence.

(defn- reachable-facts [result]
  (get-in result [:relational-coverage :reachable-state-rule-coverage]))

(deftest reachable-coverage-measures-actual-state-occurrences-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [result (check-counter assembly)
            fact (first (reachable-facts result))]
        (is (= :holds-in-explored-model (:status result)))
        (is (= 13 (get-in result [:exploration :visited-state-occurrences])))
        (is (= :counters/parent-active (:rule-id fact)))
        (is (= 13 (:evaluated-state-occurrences fact)))
        (is (= 13 (:state-occurrences-with-matching-sources fact)))
        (is (= 13 (:matching-source-occurrences fact)))
        (is (true? (:ever-matched-source-in-explored-states? fact)))
        (is (empty? (get-in result [:relational-coverage
                                   :never-matched-in-explored-states-rule-ids])))
        (is (= :not-established
               (get-in result [:relational-coverage :declaration-completeness])))
        (is (not (contains? fact :state)))
        (is (not (contains? fact :documents)))))))

(deftest initially-unexercised-rule-can-become-reachable-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [rule (assoc counter-relation
                        :id :counters/parent-after-increment
                        :source-where {:n 1})
            result (check-counter assembly (counter-families) [rule] options)
            fact (first (reachable-facts result))]
        (is (= :holds-in-explored-model (:status result)))
        (is (= [:counters/parent-after-increment]
               (get-in result [:relational-coverage
                               :initially-unexercised-rule-ids])))
        (is (= 0 (get-in result [:relational-coverage
                                :initial-state-rule-coverage 0
                                :initially-matching-source-count])))
        (is (= 13 (:evaluated-state-occurrences fact)))
        (is (= 3 (:state-occurrences-with-matching-sources fact)))
        (is (= 3 (:matching-source-occurrences fact)))
        (is (true? (:ever-matched-source-in-explored-states? fact)))
        (is (empty? (get-in result [:relational-coverage
                                   :never-matched-in-explored-states-rule-ids])))))))

(deftest permanently-vacuous-rules-remain-explicit-in-exploration-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [later (assoc counter-relation :id :counters/after-one
                         :source-where {:n 1})
            never (assoc counter-relation :id :counters/after-ninety-nine
                         :source-where {:n 99})
            result (check-counter assembly (counter-families)
                                  [later never] options)
            [later-fact never-fact] (reachable-facts result)]
        (is (= [:counters/after-one :counters/after-ninety-nine]
               (get-in result [:relational-coverage :rule-ids])))
        (is (= [:counters/after-one :counters/after-ninety-nine]
               (get-in result [:relational-coverage
                               :initially-unexercised-rule-ids])))
        (is (= 13 (:evaluated-state-occurrences later-fact)))
        (is (= 13 (:evaluated-state-occurrences never-fact)))
        (is (= 3 (:matching-source-occurrences later-fact)))
        (is (= 0 (:matching-source-occurrences never-fact)))
        (is (= 0 (:state-occurrences-with-matching-sources never-fact)))
        (is (false? (:ever-matched-source-in-explored-states? never-fact)))
        (is (= [:counters/after-ninety-nine]
               (get-in result [:relational-coverage
                               :never-matched-in-explored-states-rule-ids])))))))

(deftest reachable-coverage-honors-actual-depth-bound-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      (let [rule (assoc counter-relation :id :counters/after-two
                         :source-where {:n 2})
            result (check-counter assembly (counter-families) [rule]
                                  {:max-depth 1 :max-transitions 40})
            fact (first (reachable-facts result))]
        (is (= :holds-in-explored-model (:status result)))
        (is (= 1 (get-in result [:exploration :reached-depth])))
        (is (= 4 (:evaluated-state-occurrences fact)))
        (is (= 0 (:matching-source-occurrences fact)))
        (is (false? (:ever-matched-source-in-explored-states? fact)))
        (is (= [:counters/after-two]
               (get-in result [:relational-coverage
                               :never-matched-in-explored-states-rule-ids])))
        (is (= :not-established
               (get-in result [:relational-coverage
                               :registered-executor-correspondence])))))))


(deftest incomplete-reachable-rule-observation-cannot-pass-as-coverage-test
  (with-standard-application
    (fn [{:keys [assembly]}]
      ;; Simulate an accidentally changed upstream checker that returns a
      ;; superficially successful visit count but does not evaluate rules.
      ;; No partial diagnostic coverage must be admitted as a successful run.
      (with-redefs [registry-coverage/check-registered-families
                    (fn [& _]
                      {:status :holds-in-explored-model
                       :exploration {:visited-state-occurrences 1}})]
        (is (= :incomplete-rule-observation
               (kind #(check-counter assembly))))))))
