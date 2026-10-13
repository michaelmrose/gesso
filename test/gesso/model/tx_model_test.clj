(ns gesso.model.tx-model-test
  "Permanent regressions for the bounded, pure serial-commit reference model.

   These tests establish properties of gesso.model.tx-model's admitted subset;
   they do NOT certify XTDB, real authorization, durable command replay, or
   complete HumanHelp lifecycle invariants. All plans in an exploration are
   prepared against the SAME initial state and are never re-prepared."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]))

(def version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def request
  {:xt/id :request-1
   :revision 0
   :created-at :t0
   :updated-at :t0
   :owner nil})

(def membership
  {:xt/id :membership-1
   :revision 0
   :created-at :t0
   :updated-at :t0
   :active? true})

(def initial-state
  {[:request :request-1] request
   [:membership :membership-1] membership})

(defn next-document
  [document changes]
  (-> document
      (merge changes)
      (update :revision inc)
      (assoc :updated-at :t1)))

(defn update-command
  [type operation before changes]
  (command/update-command
   type operation before (next-document before changes) version))

(def claim-a
  (update-command :request :claim request {:owner :helper-a}))

(def claim-b
  (update-command :request :claim request {:owner :helper-b}))

(def revoke-membership
  (update-command :membership :revoke membership {:active? false}))

(def membership-guard
  (command/guard :membership membership version))

(defn plan
  [commands & {:keys [guards]}]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(def guarded-claim
  (plan [claim-a] :guards [membership-guard]))

(def unguarded-claim
  (plan [claim-a]))

(def revoke-plan
  (plan [revoke-membership]))

(defn error-type
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (:error/type (ex-data ex)))))

(defn path-by-order
  [exploration order]
  (some #(when (= order (:order %)) %) (:paths exploration)))

(defn statuses
  [path]
  (mapv :status (:trace path)))

(deftest reference-scope-is-explicit-and-conservative-test
  (let [scope model/reference-scope
        outcome (model/step initial-state unguarded-claim)
        exploration (model/explore-orders initial-state [unguarded-claim])]
    (is (= :gesso.model.tx/serial-commit-reference-v1 (:model scope)))
    (is (= #{:canonical-command-preconditions
             :canonical-version-guards
             :atomic-document-write-set}
           (:checked scope)))
    (is (contains? (:assumed scope) :serialized-atomic-commit))
    (doseq [unproved [:real-xtdb-correspondence
                     :business-authorization
                     :completeness-of-decision-dependencies
                     :arbitrary-sql-assertions
                     :durable-application-command-replay
                     :delivery-and-live-effects
                     :temporal-validity-and-isolation]]
      (is (contains? (:not-established scope) unproved)))
    (is (= scope (:scope outcome)))
    (is (= scope (:scope exploration)))))

(deftest successful-step-uses-the-prewrite-snapshot-test
  (let [result (model/step initial-state guarded-claim)]
    (is (= :committed (:status result)))
    (is (= initial-state (:before result)))
    (is (= :helper-a (get-in result [:after [:request :request-1] :owner])))
    (is (= membership (get-in result [:after [:membership :membership-1]])))
    (is (= 2 (count (:checks result))))
    (is (every? :satisfied? (:checks result)))
    (is (nil? (:failed-precondition result)))
    (is (= (assoc initial-state
                  [:request :request-1]
                  (command/after claim-a))
           (:after result)))))

(deftest stale-command-is-rejected-without-mutating-any-document-test
  (let [first-result (model/step initial-state unguarded-claim)
        retry (model/step (:after first-result) unguarded-claim)]
    (is (= :committed (:status first-result)))
    (is (= :rejected (:status retry)))
    (is (= (:before retry) (:after retry)))
    (is (= :command-version (get-in retry [:failed-precondition :kind])))
    (is (= [:request :request-1]
           (get-in retry [:failed-precondition :target])))
    (is (false? (get-in retry [:failed-precondition :satisfied?])))))

(deftest competing-claims-have-a-single-committer-in-every-order-test
  (let [exploration (model/explore-orders initial-state
                                           [(plan [claim-a]) (plan [claim-b])])
        paths (:paths exploration)]
    (is (= {:kind :all-serial-orders :prepared-plans 2 :orderings 2}
           (:exploration exploration)))
    (is (= #{[0 1] [1 0]} (set (map :order paths))))
    (is (every? #(= [:committed :rejected] (statuses %)) paths))
    (is (= #{:helper-a :helper-b}
           (set (map #(get-in % [:final-state [:request :request-1] :owner])
                     paths))))
    (is (every? #(= 1 (get-in % [:final-state [:request :request-1] :revision]))
                paths))
    (is (every? #(= :command-version
                    (get-in % [:trace 1 :failed-precondition :kind]))
                paths))))

(deftest version-comparison-rejects-updated-at-drift-test
  (let [drifted (assoc-in initial-state
                          [[:request :request-1] :updated-at]
                          :different-time)
        result (model/step drifted unguarded-claim)]
    (is (= :rejected (:status result)))
    (is (= drifted (:after result)))
    (is (= :command-version (get-in result [:failed-precondition :kind])))
    (is (false? (get-in result [:failed-precondition :satisfied?])))))

(deftest stale-membership-guard-rejects-prepared-claim-test
  (let [exploration (model/explore-orders initial-state
                                           [guarded-claim revoke-plan])
        revoke-first (path-by-order exploration [1 0])
        claim-first (path-by-order exploration [0 1])]
    (is (= [:committed :rejected] (statuses revoke-first)))
    (is (= :dependency-version
           (get-in revoke-first [:trace 1 :failed-precondition :kind])))
    (is (= [:membership :membership-1]
           (get-in revoke-first [:trace 1 :failed-precondition :target])))
    (is (nil? (get-in revoke-first
                      [:final-state [:request :request-1] :owner])))
    ;; A guard constrains the Claim's *admission time*. It does not impose
    ;; a lifelong invariant on a later, independently authorized revocation.
    (is (= [:committed :committed] (statuses claim-first)))
    (is (= :helper-a
           (get-in claim-first [:final-state [:request :request-1] :owner])))))

(deftest omitted-guard-exposes-concrete-counterexample-test
  (let [exploration (model/explore-orders initial-state
                                           [unguarded-claim revoke-plan])
        revoke-first (path-by-order exploration [1 0])]
    (is (= [:committed :committed] (statuses revoke-first)))
    (is (= false
           (get-in revoke-first
                   [:trace 0 :state [:membership :membership-1] :active?])))
    (is (= :helper-a
           (get-in revoke-first
                   [:trace 1 :state [:request :request-1] :owner])))
    (is (empty? (filter #(= :dependency-version (:kind %))
                        (:checks (model/step initial-state unguarded-claim)))))))

(deftest rejection-of-one-precondition-preserves-entire-write-set-test
  (let [fresh {:xt/id :audit-1
               :revision 0
               :created-at :t0
               :updated-at :t0
               :message "claim-attempt"}
        create (command/create :audit fresh version)
        multi-plan (plan [claim-a create] :guards [membership-guard])
        state-after-revoke (:after (model/step initial-state revoke-plan))
        rejection (model/step state-after-revoke multi-plan)]
    (is (= :rejected (:status rejection)))
    (is (= state-after-revoke (:after rejection)))
    (is (not (contains? (:after rejection) [:audit :audit-1])))
    (is (nil? (get-in rejection
                      [:after [:request :request-1] :owner])))
    (is (= 3 (count (:checks rejection))))
    (is (= :dependency-version
           (get-in rejection [:failed-precondition :kind])))))

(deftest atomic-multi-command-success-writes-all-targets-test
  (let [fresh {:xt/id :audit-1
               :revision 0
               :created-at :t0
               :updated-at :t0
               :message "claim-attempt"}
        create (command/create :audit fresh version)
        result (model/step initial-state (plan [claim-a create]))]
    (is (= :committed (:status result)))
    (is (= fresh (get-in result [:after [:audit :audit-1]])))
    (is (= :helper-a
           (get-in result [:after [:request :request-1] :owner])))
    (is (= membership
           (get-in result [:after [:membership :membership-1]])))))

(deftest duplicate-creates-reject-second-in-every-order-test
  (let [fresh {:xt/id :request-2
               :revision 0
               :created-at :t0
               :updated-at :t0
               :owner nil}
        create-plan (plan [(command/create :request fresh version)])
        exploration (model/explore-orders {} [create-plan create-plan])]
    (is (every? #(= [:committed :rejected] (statuses %))
                (:paths exploration)))
    (is (every? #(= :document-absent
                    (get-in % [:trace 1 :failed-precondition :kind]))
                (:paths exploration)))
    (is (every? #(= {[:request :request-2] fresh} (:final-state %))
                (:paths exploration)))))

(deftest normalized-duplicate-guards-are-not-multiplied-test
  (let [result (model/step initial-state
                           (plan [claim-a]
                                 :guards [membership-guard membership-guard]))]
    (is (= :committed (:status result)))
    (is (= 2 (count (:checks result))))
    (is (= [:dependency-version :command-version]
           (mapv :kind (:checks result))))))

(deftest contradictory-guards-and-command-targets-fail-admission-test
  (let [old-membership-guard (command/guard :membership membership version)
        later-membership (next-document membership {:active? false})
        new-membership-guard (command/guard :membership later-membership version)
        request-guard (command/guard :request request version)]
    (is (= ::tx/conflicting-guards
           (error-type #(model/admitted-plan
                         (plan [claim-a] :guards
                               [old-membership-guard new-membership-guard])))))
    (is (= ::tx/duplicate-command-targets
           (error-type #(model/admitted-plan (plan [claim-a claim-b])))))
    (is (= ::tx/guard-conflicts-with-create
           (error-type #(let [fresh (assoc request :xt/id :request-2)
                              creation (command/create :request fresh version)]
                          (model/admitted-plan
                           (plan [creation]
                                 :guards [(command/guard :request fresh version)]))))))
    ;; Identical command-target guard is redundant, not additional evidence.
    (is (= 1 (count (:checks
                     (model/step initial-state
                                 (plan [claim-a] :guards [request-guard]))))))))

(deftest unsupported-transaction-semantics-fail-closed-test
  (doseq [[feature supplement]
          [[:assertions {:assertions [(tx/assert-one :membership
                                                    [:= :xt/id :membership-1])]}]
           [:entry {:entry {:custom true}}]
           [:entry-fn {:entry-fn identity}]
           [:tx-options {:tx-options {}}]]]
    (testing (name feature)
      (let [result (try
                     (model/admitted-plan (merge unguarded-claim supplement))
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (ex-data e)))]
        (is (= ::model/unsupported-plan (:error/type result)))
        (is (some #{feature}
                  (get-in result [:error/details :unsupported]))))))
  (is (= ::tx/missing-commands
         (error-type #(model/admitted-plan {:commands [] :emit false}))))
  (is (= ::tx/unknown-plan-keys
         (error-type #(model/admitted-plan
                       (assoc unguarded-claim :arbitrary-rule :trust-me))))))

(deftest semantic-changes-and-publication-are-not-certified-test
  (let [plan-with-changes (assoc unguarded-claim
                                 :emit :sync
                                 :changes [{:topic :request :id :request-1}])
        admitted (model/admitted-plan plan-with-changes)
        result (model/step initial-state plan-with-changes)]
    (is (= :sync (:emit admitted)))
    (is (= [{:topic :request :id :request-1}] (:changes admitted)))
    (is (= :committed (:status result)))
    (is (contains? (get-in result [:scope :not-established])
                   :delivery-and-live-effects))))

(deftest invalid-states-and-exploration-bounds-are-rejected-test
  (doseq [state [nil []
                 {[:request :wrong-id] request}
                 {[:request nil] request}
                 {[:request :request-1] nil}
                 {[:request] request}
                 {"request-1" request}]]
    (is (= ::model/invalid-state
           (error-type #(model/step state unguarded-claim)))))
  (doseq [plans [nil () []
                 (vec (repeat 7 unguarded-claim))]]
    (is (= ::model/invalid-plans
           (error-type #(model/explore-orders initial-state plans)))))
  (is (= 6 (get-in (model/explore-orders initial-state
                                          (vec (repeat 3 unguarded-claim)))
                   [:exploration :orderings])))
  (is (= 6 (count (:paths (model/explore-orders initial-state
                                                 (vec (repeat 3 unguarded-claim)))))))
  (is (= ::model/unsupported-plan
         (error-type #(model/explore-orders
                       initial-state
                       [unguarded-claim
                        (assoc unguarded-claim :tx-options {})])))))

(deftest model-execution-is-deterministic-and-does-not-mutate-input-test
  (let [state initial-state
        guarded guarded-claim
        exploration-a (model/explore-orders state [guarded revoke-plan])
        exploration-b (model/explore-orders state [guarded revoke-plan])]
    (is (= exploration-a exploration-b))
    (is (= initial-state state))
    (is (= [membership-guard] (:guards guarded)))
    (is (= 2 (count (:paths exploration-a))))))
