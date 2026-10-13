(ns gesso.model.invariant-check-test
  "Permanent, deliberately bounded regressions for the application-authored
   invariant checker. The rules here are independent, ordinary Clojure
   predicates over a finite document map; they are not certified policies.

   The tests distinguish (1) guards on an individual prepared operation from
   (2) preservation of an invariant by *every* admitted operation. They also
   document the limited assurance returned by the checker: finite serial
   exploration only, not a proof about XTDB, arbitrary future commands,
   authorization, predicate purity, or complete policy dependencies."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.invariant-check :as invariant]
            [gesso.model.tx-model :as reference]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request
  {:xt/id :request-1
   :revision 0
   :created-at :t0
   :updated-at :t0
   :owner nil})

(def ^:private membership
  {:xt/id :membership-1
   :revision 0
   :created-at :t0
   :updated-at :t0
   :active? true})

(def ^:private initial-state
  {[:request :request-1] request
   [:membership :membership-1] membership})

(defn- next-document [doc changes]
  (-> doc
      (merge changes)
      (update :revision inc)
      (assoc :updated-at :t1)))

(defn- update-command [kind operation before changes]
  (command/update-command kind operation before
                          (next-document before changes) version))

(def ^:private claim-a
  (update-command :request :claim request {:owner :helper-a}))

(def ^:private claim-b
  (update-command :request :claim request {:owner :helper-b}))

(def ^:private revoke
  (update-command :membership :revoke membership {:active? false}))

(def ^:private membership-guard
  (command/guard :membership membership version))

(defn- plan [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(def ^:private guarded-claim (plan [claim-a] membership-guard))
(def ^:private guarded-claim-b (plan [claim-b] membership-guard))
(def ^:private unguarded-claim (plan [claim-a]))
(def ^:private unsafe-revocation (plan [revoke]))

;; A single transaction that both revokes membership and clears ownership is
;; an illustrative *finite-model* repair, not a prescribed HumanHelp policy.
(def ^:private revocation-with-release
  (plan [revoke (update-command :request :release request {:owner nil})]))

(def ^:private ownership-rule
  {:id :request/owner-requires-active-membership
   :holds? (fn [state]
             (or (nil? (get-in state [[:request :request-1] :owner]))
                 (true? (get-in state [[:membership :membership-1] :active?]))))})

(def ^:private no-owner-b-rule
  {:id :request/no-helper-b
   :holds? (fn [state]
             (not= :helper-b
                   (get-in state [[:request :request-1] :owner])))})

(def ^:private membership-active-rule
  {:id :membership/remains-active
   :holds? (fn [state]
             (true? (get-in state [[:membership :membership-1] :active?])))})

(defn- check
  ([state plans] (check state plans [ownership-rule]))
  ([state plans rules]
   (invariant/check-prepared state plans rules)))

(defn- captured-error [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e e)))

(defn- kind [f]
  (some-> (captured-error f) ex-data :error/kind))

(defn- violation [result rule-id]
  (some #(when (= rule-id (:rule-id %)) %) (:violations result)))

(deftest scope-explicitly-limits-finite-assurance-test
  (let [scope invariant/assurance-scope
        checked (:checked scope)
        assumed (:assumed scope)
        excluded (:not-established scope)
        result (check initial-state [guarded-claim])]
    (is (= :gesso.model.invariant-check/finite-prepared-serial-v1
           (:checker scope)))
    (is (contains? checked :rule-preservation-at-every-explored-step))
    (is (contains? checked :all-serial-orderings-of-admitted-prepared-plans))
    (is (contains? assumed :pure-total-truthful-rule-predicates))
    (is (contains? assumed :complete-finite-state-universe))
    (is (contains? assumed :serial-atomic-commit-semantics))
    (doseq [not-proven [:truth-of-authored-business-rules
                       :source-derived-completeness-of-policy-dependencies
                       :unbounded-invariant-preservation
                       :concurrent-execution-linearizability
                       :real-xtdb-execution-correspondence
                       :application-certification]]
      (is (contains? excluded not-proven) (str not-proven)))
    (is (= scope (:scope result)))))

(deftest single-guarded-claim-preserves-the-modeled-rule-test
  (let [result (check initial-state [guarded-claim])]
    (is (= :holds-in-explored-model (:status result)))
    (is (= [] (:violations result)))
    (is (= [:request/owner-requires-active-membership] (:rule-ids result)))
    (is (= {:kind :all-serial-orders :prepared-plans 1 :orderings 1}
           (:exploration result)))
    (is (= 2 (:evaluated-state-visits result)))))

(deftest stale-membership-guard-rejects-a-claim-test
  (let [revoked-state
        (assoc initial-state [:membership :membership-1]
               (next-document membership {:active? false}))
        result (check revoked-state [guarded-claim])
        step (reference/step revoked-state guarded-claim)]
    (is (= :rejected (:status step)))
    (is (= :dependency-version
           (get-in step [:failed-precondition :kind])))
    (is (= revoked-state (:after step)))
    (is (= :holds-in-explored-model (:status result)))
    (is (empty? (:violations result)))))

(deftest omitted-guard-exposes-a-committed-counterexample-test
  (let [revoked-state
        (assoc initial-state [:membership :membership-1]
               (next-document membership {:active? false}))
        result (check revoked-state [unguarded-claim])
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= :request/owner-requires-active-membership (:rule-id witness)))
    (is (= :after-plan (:at witness)))
    (is (= [0] (:order witness)))
    (is (= 0 (:plan-index witness)))
    (is (= :committed (:status witness)))
    (is (= [{:plan-index 0
             :status :committed
             :state (:state witness)
             :failed-precondition nil}]
           (:trace witness)))
    (is (= :helper-a
           (get-in witness [:state [:request :request-1] :owner])))
    (is (false? (get-in witness [:state [:membership :membership-1] :active?])))))

(deftest guarded-claim-does-not-protect-a-later-revocation-test
  (let [result (check initial-state [guarded-claim unsafe-revocation])
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= 2 (get-in result [:exploration :orderings])))
    (is (= 5 (:evaluated-state-visits result)))
    (is (= [0 1] (:order witness)))
    (is (= :after-plan (:at witness)))
    (is (= 1 (:plan-index witness)))
    (is (= :committed (:status witness)))
    (is (= [0 1] (mapv :plan-index (:trace witness))))
    (is (= [:committed :committed]
           (mapv :status (:trace witness))))
    (is (= :helper-a
           (get-in witness [:state [:request :request-1] :owner])))
    (is (false? (get-in witness [:state [:membership :membership-1] :active?])))))

(deftest atomic-revocation-with-release-preserves-the-finite-invariant-test
  (let [result (check initial-state [guarded-claim revocation-with-release])
        exploration (reference/explore-orders
                     initial-state [guarded-claim revocation-with-release])
        paths (:paths exploration)]
    (is (= :holds-in-explored-model (:status result)))
    (is (empty? (:violations result)))
    (is (= #{[0 1] [1 0]} (set (map :order paths))))
    (is (= #{[:committed :rejected]}
           (set (map #(mapv :status (:trace %)) paths))))
    (is (= #{:helper-a nil}
           (set (map #(get-in % [:final-state [:request :request-1] :owner])
                     paths))))))

(deftest competing-claims-do-not-by-themselves-violate-ownership-test
  (let [result (check initial-state [guarded-claim guarded-claim-b])
        paths (:paths (reference/explore-orders initial-state
                                                 [guarded-claim guarded-claim-b]))]
    (is (= :holds-in-explored-model (:status result)))
    (is (= 2 (get-in result [:exploration :orderings])))
    (is (every? #(= [:committed :rejected]
                    (mapv :status (:trace %))) paths))
    (is (= #{:helper-a :helper-b}
           (set (map #(get-in % [:final-state [:request :request-1] :owner])
                     paths))))))

(deftest every-permutation-and-every-attempted-step-are-counted-test
  (let [result (check initial-state
                      [guarded-claim guarded-claim-b unsafe-revocation])]
    (is (= :counterexample (:status result)))
    (is (= {:kind :all-serial-orders :prepared-plans 3 :orderings 6}
           (:exploration result)))
    (is (= 19 (:evaluated-state-visits result)))
    (is (= [0 1 2] (get-in result [:violations 0 :order])))
    (is (= [0 1 2] (mapv :plan-index
                         (get-in result [:violations 0 :trace]))))
    (is (= [:committed :rejected :committed]
           (mapv :status (get-in result [:violations 0 :trace]))))))

(deftest initial-failure-is-distinct-from-transition-failure-test
  (let [invalid-initial
        (assoc-in initial-state [[:request :request-1] :owner] :helper-a)
        invalid-initial
        (assoc-in invalid-initial [[:membership :membership-1] :active?]
                  false)
        result (check invalid-initial [guarded-claim])
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= :initial-state (:at witness)))
    (is (= :request/owner-requires-active-membership (:rule-id witness)))
    (is (nil? (:order witness)))
    (is (nil? (:plan-index witness)))
    (is (= [] (:trace witness)))
    (is (= invalid-initial (:state witness)))
    (is (= 2 (:evaluated-state-visits result)))))

(deftest violations-are-in-declared-rule-order-not-discovery-order-test
  (let [result (check initial-state
                      [guarded-claim unsafe-revocation]
                      [membership-active-rule ownership-rule no-owner-b-rule])]
    (is (= :counterexample (:status result)))
    (is (= [:membership/remains-active
            :request/owner-requires-active-membership
            :request/no-helper-b]
           (:rule-ids result)))
    (is (= [:membership/remains-active
            :request/owner-requires-active-membership]
           (mapv :rule-id (:violations result))))
    (is (= 1 (get-in result [:violations 0 :plan-index])))
    (is (= 1 (get-in result [:violations 1 :plan-index])))
    (is (= 5 (:evaluated-state-visits result)))))

(deftest different-rules-can-fail-on-different-steps-test
  (let [result (check initial-state
                      [guarded-claim-b unsafe-revocation]
                      [membership-active-rule no-owner-b-rule ownership-rule])]
    (is (= :counterexample (:status result)))
    (is (= [:membership/remains-active :request/no-helper-b
            :request/owner-requires-active-membership]
           (mapv :rule-id (:violations result))))
    (is (= [1 0 1]
           (mapv :plan-index (:violations result))))
    (is (= [2 1 2]
           (mapv (comp count :trace) (:violations result))))))

(deftest rejected-transactions-are-still-visited-test
  (let [revoked-state
        (assoc initial-state [:membership :membership-1]
               (next-document membership {:active? false}))
        result (check revoked-state [guarded-claim guarded-claim-b])]
    (is (= :holds-in-explored-model (:status result)))
    (is (= 2 (get-in result [:exploration :orderings])))
    (is (= 5 (:evaluated-state-visits result)))
    (is (every? (fn [path]
                  (every? #(= :rejected (:status %)) (:trace path)))
                (:paths (reference/explore-orders
                         revoked-state [guarded-claim guarded-claim-b]))))))

(deftest input-state-and-plans-are-not-mutated-test
  (let [state initial-state
        plans [guarded-claim unsafe-revocation]
        state-before (pr-str state)
        plans-before (pr-str plans)]
    (check state plans)
    (is (= state-before (pr-str state)))
    (is (= plans-before (pr-str plans)))))

(deftest invalid-rule-collections-fail-closed-test
  (doseq [rules [nil [] {} '() (list ownership-rule) #{ownership-rule}]]
    (is (= :invalid-rules
           (kind #(check initial-state [guarded-claim] rules)))
        (str "rules = " (pr-str rules)))))

(deftest malformed-rule-entries-fail-closed-test
  (doseq [rule [nil
                true
                {:id :request/ownership}
                {:holds? (constantly true)}
                {:id :ownership :holds? (constantly true)}
                {:id "request/ownership" :holds? (constantly true)}
                {:id :request/ownership :holds? true}
                {:id :request/ownership :holds? (constantly true) :name "extra"}]]
    (is (= :invalid-rule
           (kind #(check initial-state [guarded-claim] [rule])))
        (str "rule = " (pr-str rule)))))

(deftest duplicate-rule-ids-fail-closed-test
  (is (= :duplicate-rule
         (kind #(check initial-state [guarded-claim]
                       [ownership-rule
                        (assoc ownership-rule :holds? (constantly true))])))))

(deftest predicates-must-return-real-booleans-test
  (doseq [answer [nil :yes 0 1 [] {} "true"]]
    (let [failure (captured-error
                   #(check initial-state [guarded-claim]
                           [{:id :request/strict-bool
                             :holds? (constantly answer)}]))]
      (is (= :non-boolean-rule-result
             (get-in (ex-data failure) [:error/kind]))
          (pr-str answer))
      (is (= :request/strict-bool
             (get-in (ex-data failure) [:error/details :rule-id]))))))

(deftest nonboolean-after-commit-aborts-the-entire-check-test
  (let [failure
        (captured-error
         #(check initial-state [guarded-claim]
                 [{:id :request/strict-after
                   :holds? (fn [state]
                             (if (nil? (get-in state [[:request :request-1] :owner]))
                               true :not-a-boolean))}]))]
    (is (= :non-boolean-rule-result (:error/kind (ex-data failure))))
    (is (= :request/strict-after
           (get-in (ex-data failure) [:error/details :rule-id])))))

(deftest exceptions-from-rules-propagate-with-identity-and-cause-test
  (let [failure
        (captured-error
         #(check initial-state [guarded-claim]
                 [{:id :request/throwing-after
                   :holds? (fn [state]
                             (if (nil? (get-in state [[:request :request-1] :owner]))
                               true
                               (throw (ex-info "business predicate crashed"
                                               {:origin :rule}))))}]))]
    (is (= :gesso.model.invariant-check/rule-evaluation-failed
           (:error/type (ex-data failure))))
    (is (= :request/throwing-after (:rule-id (ex-data failure))))
    (is (= {:origin :rule} (ex-data (.getCause failure))))))

(deftest invalid-state-and-plan-inputs-fail-closed-test
  (is (= :gesso.model.tx-model/invalid-state
         (:error/type
          (ex-data (captured-error #(check nil [guarded-claim]))))))
  (is (= :gesso.model.tx-model/invalid-state
         (:error/type
          (ex-data (captured-error
                    #(check (assoc initial-state [:request :request-1] nil)
                            [guarded-claim]))))))
  (doseq [plans [nil [] '() [guarded-claim guarded-claim guarded-claim
                             guarded-claim guarded-claim guarded-claim
                             guarded-claim]]]
    (is (= :gesso.model.tx-model/invalid-plans
           (:error/type (ex-data (captured-error
                                   #(check initial-state plans)))))))
  (is (= :gesso.model.tx-model/unsupported-plan
         (:error/type (ex-data (captured-error
                                 #(check initial-state
                                         [{:commands [claim-a]
                                           :assertions [{:assert true}]
                                           :emit false}]))))))
  (is (= :gesso.model.tx-model/unsupported-plan
         (:error/type (ex-data (captured-error
                                 #(check initial-state
                                         [(assoc guarded-claim
                                                 :tx-options {:some-option true})])))))))

(deftest invariant-checking-is-not-business-policy-inference-test
  (let [revoked-state
        (assoc initial-state [:membership :membership-1]
               (next-document membership {:active? false}))
        result (check revoked-state [unguarded-claim]
                      [{:id :request/document-has-id
                        :holds? (fn [state]
                                  (= :request-1
                                     (get-in state [[:request :request-1] :xt/id])))}])]
    ;; This weak but truthful rule passes. Gesso cannot infer the missing
    ;; ownership/member relationship from the transaction structure alone.
    (is (= :holds-in-explored-model (:status result)))
    (is (empty? (:violations result)))
    (is (contains? (get-in result [:scope :not-established])
                   :source-derived-completeness-of-policy-dependencies))))
