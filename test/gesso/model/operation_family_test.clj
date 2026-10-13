(ns gesso.model.operation-family-test
  "Permanent regressions for the finite operation-family expander and its
   state-dependent reachable invariant checker.

   These fixtures describe small *abstract* applications. They are not
   extracted from HumanHelp or its authorization policy. Expansion checks
   the complete Cartesian product of explicitly supplied finite domains;
   it cannot establish that those domains exhaust production principals,
   argument values, operations, or states. Callback correctness, real XTDB
   correspondence, and unbounded preservation remain outside this suite."
  (:require [clojure.test :refer [deftest is]]
            [gesso.model.command :as command]
            [gesso.model.operation-family :as family]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private counter-target [:counter :counter-1])
(def ^:private request-target [:request :request-1])
(def ^:private membership-target [:membership :membership-1])

(def ^:private counter-state
  {counter-target {:xt/id :counter-1 :revision 0
                   :created-at :t0 :updated-at :t0 :n 0}})

(def ^:private request-state
  {request-target {:xt/id :request-1 :revision 0
                   :created-at :t0 :updated-at :t0 :owner nil}
   membership-target {:xt/id :membership-1 :revision 0
                      :created-at :t0 :updated-at :t0 :active? true}})

(defn- next-document [before changes]
  (-> before (merge changes) (update :revision inc) (assoc :updated-at :t1)))

(defn- change [kind operation before attributes]
  (command/update-command kind operation before
                          (next-document before attributes) version))

(defn- counter-plan [state amount]
  (let [before (get state counter-target)]
    {:commands [(change :counter :increase before
                        {:n (+ (:n before) amount)})]
     :guards []
     :emit false}))

(defn- increment-family
  ([] (increment-family [:helper/a :helper/b] [{:amount 1} {:amount 2}]))
  ([principals arguments]
   {:id :counter/increment
    :principals principals
    :arguments arguments
    :enabled? (fn [state principal args]
                (and (contains? state counter-target)
                     (keyword? principal)
                     (pos? (:amount args))))
    :prepare (fn [state _principal args]
               (counter-plan state (:amount args)))}))

(defn- maximum-rule [maximum]
  {:id :counter/within-bound
   :holds? (fn [state]
             (<= (get-in state [counter-target :n]) maximum))})

(defn- counter-check
  ([families rule depth budget]
   (family/check-families counter-state families [rule]
                          {:max-depth depth :max-transitions budget}))
  ([families rule]
   (counter-check families rule 2 100)))

(defn- error-of [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo exception exception)))

(defn- error-kind [thunk]
  (some-> (error-of thunk) ex-data :error/kind))

(defn- owner-active? [state]
  (or (nil? (get-in state [request-target :owner]))
      (true? (get-in state [membership-target :active?]))))

(def ^:private owner-rule
  {:id :request/owner-has-active-membership
   :holds? owner-active?})

(defn- claim-family
  ([] (claim-family [:helper/a :helper/b]))
  ([principals]
   {:id :request/claim
    :principals principals
    :arguments [{:request-id :request-1}]
    :enabled? (fn [state _principal args]
                (and (= :request-1 (:request-id args))
                     (nil? (get-in state [request-target :owner]))
                     (true? (get-in state [membership-target :active?]))))
    :prepare (fn [state principal _args]
               {:commands [(change :request :claim (get state request-target)
                                   {:owner principal})]
                :guards [(command/guard :membership
                                        (get state membership-target) version)]
                :emit false})}))

(def ^:private unsafe-revoke
  {:id :membership/revoke
   :principals [:manager/a]
   :arguments [{:membership-id :membership-1}]
   :enabled? (fn [state _principal args]
               (and (= :membership-1 (:membership-id args))
                    (true? (get-in state [membership-target :active?]))))
   :prepare (fn [state _principal _args]
              {:commands [(change :membership :revoke
                                  (get state membership-target)
                                  {:active? false})]
               :guards []
               :emit false})})

(def ^:private atomic-revoke-and-release
  (assoc unsafe-revoke
         :prepare (fn [state _principal _args]
                    {:commands [(change :membership :revoke
                                        (get state membership-target)
                                        {:active? false})
                                (change :request :release
                                        (get state request-target)
                                        {:owner nil})]
                     :guards []
                     :emit false})))

(defn- request-check [families]
  (family/check-families request-state families [owner-rule]
                         {:max-depth 2 :max-transitions 25}))

(deftest finite-cartesian-product-is-complete-and-deterministic-test
  (let [{:keys [choices operations]} (family/expand-families [(increment-family)])]
    (is (= 4 (count choices)))
    (is (= 4 (count operations)))
    (is (= [:helper/a :helper/a :helper/b :helper/b]
           (mapv :principal choices)))
    (is (= [1 2 1 2]
           (mapv #(get-in % [:arguments :amount]) choices)))
    (is (= [:counter/increment :counter/increment
            :counter/increment :counter/increment]
           (mapv :family-id choices)))
    (is (= [0 0 1 1] (mapv :principal-index choices)))
    (is (= [0 1 0 1] (mapv :argument-index choices)))
    (is (= (mapv :instance-id choices) (mapv :id operations)))
    (is (= (mapv :instance-id choices)
           (mapv :instance-id (:choices (family/expand-families [(increment-family)])))))))

(deftest multiple-families-expand-in-declaration-order-test
  (let [other (assoc (increment-family [:manager/a] [{:amount 3}])
                     :id :counter/manager-increment)
        {:keys [choices operations]}
        (family/expand-families [(increment-family [:helper/a] [{:amount 1} {:amount 2}])
                                 other])]
    (is (= 3 (count choices)))
    (is (= [:counter/increment :counter/increment :counter/manager-increment]
           (mapv :family-id choices)))
    (is (= [0 0 1] (mapv :family-index choices)))
    (is (= [:gesso.model.operation-family/choice-0-0-0
            :gesso.model.operation-family/choice-0-0-1
            :gesso.model.operation-family/choice-1-0-0]
           (mapv :instance-id choices)))
    (is (= 3 (count (distinct (map :id operations)))))))

(deftest expansion-never-evaluates-application-callbacks-test
  (let [calls (atom [])
        stub (assoc (increment-family [:helper/a] [{:amount 1}])
                    :enabled? (fn [& args] (swap! calls conj [:enabled args]) true)
                    :prepare (fn [& args] (swap! calls conj [:prepare args])
                               (throw (ex-info "not called by expansion" {}))))]
    (is (= 1 (count (:choices (family/expand-families [stub])))))
    (is (empty? @calls))))

(deftest callback-closures-bind-each-principal-and-argument-test
  (let [seen (atom [])
        f {:id :counter/captured
           :principals [:helper/a :helper/b]
           :arguments [{:amount 1} {:amount 2}]
           :enabled? (fn [state principal args]
                       (swap! seen conj [:enabled (:n (get state counter-target))
                                         principal (:amount args)])
                       true)
           :prepare (fn [state principal args]
                      (swap! seen conj [:prepare (:n (get state counter-target))
                                        principal (:amount args)])
                      (counter-plan state (:amount args)))}
        {:keys [operations]} (family/expand-families [f])]
    (doseq [operation operations]
      (is (true? ((:enabled? operation) counter-state)))
      (is (= 1 (count (:commands ((:prepare operation) counter-state))))))
    (is (= [[:enabled 0 :helper/a 1] [:prepare 0 :helper/a 1]
            [:enabled 0 :helper/a 2] [:prepare 0 :helper/a 2]
            [:enabled 0 :helper/b 1] [:prepare 0 :helper/b 1]
            [:enabled 0 :helper/b 2] [:prepare 0 :helper/b 2]] @seen))))

(deftest expansion-is-not-dependent-on-callback-object-identity-test
  (let [first-expansion (family/expand-families [(increment-family)])
        second-expansion (family/expand-families [(increment-family)])]
    (is (= (:choices first-expansion) (:choices second-expansion)))
    (is (= (mapv :id (:operations first-expansion))
           (mapv :id (:operations second-expansion))))))

(deftest six-instances-are-admitted-without-truncation-test
  (let [{:keys [choices]} (family/expand-families
                           [(increment-family [:helper/a :helper/b :helper/c]
                                              [{:amount 1} {:amount 2}])])]
    (is (= family/maximum-instances (count choices)))
    (is (= 6 (count (distinct (map :instance-id choices)))))))

(deftest larger-cartesian-product-fails-before-expansion-test
  (let [exception (error-of #(family/expand-families
                              [(increment-family [:a :b :c :d]
                                                 [{:amount 1} {:amount 2}])]))]
    (is (= :instance-bound-exceeded (:error/kind (ex-data exception))))
    (is (= 8 (get-in (ex-data exception) [:error/details :expanded-instances])))
    (is (= 6 (get-in (ex-data exception) [:error/details :maximum])))))

(deftest multi-family-total-bound-is-not-per-family-test
  (let [a (increment-family [:a :b] [{:amount 1} {:amount 2}])
        b (assoc (increment-family [:manager/a] [{:amount 1} {:amount 2} {:amount 3}])
                 :id :counter/manager)]
    (is (= 7 (-> (error-of #(family/expand-families [a b]))
                 ex-data :error/details :expanded-instances)))
    (is (= :instance-bound-exceeded
           (error-kind #(family/expand-families [a b]))))))

(deftest invalid-family-collection-fails-closed-test
  (doseq [families [nil [] '() {} (vec (repeat 7 (increment-family)))]]
    (is (= :invalid-families
           (error-kind #(family/expand-families families))) (pr-str families))))

(deftest duplicate-family-identity-fails-closed-test
  (is (= :duplicate-family
         (error-kind #(family/expand-families
                       [(increment-family [:a] [{:amount 1}])
                        (increment-family [:b] [{:amount 2}])])))))

(deftest invalid-family-shape-fails-closed-test
  (doseq [bad [(dissoc (increment-family) :enabled?)
               (assoc (increment-family) :unexpected true)
               (assoc (increment-family) :id :unqualified)
               (assoc (increment-family) :enabled? {:callable :map})
               (assoc (increment-family) :prepare [])
               nil]]
    (is (= :invalid-family (error-kind #(family/expand-families [bad])))
        (pr-str bad))))

(deftest invalid-domains-fail-closed-test
  (doseq [bad [(assoc (increment-family) :principals nil)
               (assoc (increment-family) :principals [])
               (assoc (increment-family) :principals '(:helper/a))
               (assoc (increment-family) :arguments [])
               (assoc (increment-family) :arguments [{} nil])
               (assoc (increment-family) :arguments [:not-a-map])
               (assoc (increment-family) :arguments '({:amount 1}))]]
    (is (= :invalid-domain (error-kind #(family/expand-families [bad])))
        (pr-str bad))))

(deftest duplicate-domain-choices-fail-closed-test
  (doseq [bad [(assoc (increment-family) :principals [:a :a])
               (assoc (increment-family) :arguments [{:amount 1} {:amount 1}])]]
    (is (= :duplicate-domain-choice
           (error-kind #(family/expand-families [bad]))))))

(deftest nonportable-choice-values-are-not-admitted-test
  (doseq [principal [(Object.) 0.5 #{:helper/a} '(:helper/a)
                     (fn [] :helper/a) (java.util.Date.)]]
    (is (= :invalid-domain
           (error-kind #(family/expand-families
                          [(assoc (increment-family) :principals [principal])])))))
  (doseq [arguments [{:amount 1.5} {:amount (Object.)}
                     {:amount {:unqualified? 1 3 :illegal-key}}
                     {:amount [[:nested (Object.)]]}]]
    (is (= :invalid-domain
           (error-kind #(family/expand-families
                          [(assoc (increment-family) :arguments [arguments])]))))))

(deftest portable-nested-data-and-uuid-are-admitted-test
  (let [id (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")
        f (assoc (increment-family [:helper/a] [{:amount 1}])
                 :principals [id]
                 :arguments [{:nested [true nil 123 {:id id}]}])
        expansion (family/expand-families [f])]
    (is (= id (get-in expansion [:choices 0 :principal])))
    (is (= {:nested [true nil 123 {:id id}]}
           (get-in expansion [:choices 0 :arguments])))))

(deftest passing-finite-check-reports-coverage-and-assumptions-test
  (let [result (counter-check [(increment-family [:helper/a] [{:amount 1}])]
                              (maximum-rule 2) 2 2)]
    (is (= :holds-in-explored-model (:status result)))
    (is (= 1 (get-in result [:family-expansion :instance-count])))
    (is (= [:counter/increment]
           (get-in result [:family-expansion :family-ids])))
    (is (= 2 (get-in result [:exploration :attempted-transitions])))
    (is (= :gesso.model.operation-family/finite-cross-product-v1
           (get-in result [:family-scope :checker])))
    (is (contains? (get-in result [:family-scope :not-established])
                   :completeness-of-real-principals-or-arguments))
    (is (contains? (get-in result [:family-scope :not-established])
                   :application-certification))
    (is (contains? (get-in result [:scope :not-established])
                   :unbounded-invariant-preservation))))

(deftest state-dependent-preparation-revises-every-step-test
  (let [result (counter-check [(increment-family [:helper/a] [{:amount 1}])]
                              (maximum-rule 1) 2 2)
        witness (first (:violations result))
        revisions (mapv #(get-in % [:plan :commands 0 :model/before :revision])
                        (:trace witness))]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= [0 1] revisions))
    (is (= [:committed :committed]
           (mapv :status (:trace witness))))
    (is (= 2 (get-in witness [:state counter-target :n])))
    (is (= 2 (get-in witness [:state counter-target :revision])))))

(deftest family-trace-matches-each-breadth-first-transition-test
  (let [result (counter-check [(increment-family)] (maximum-rule 2))
        witness (first (:violations result))
        trace (:trace witness)
        family-trace (:family-trace witness)]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= 2 (count trace) (count family-trace)))
    (is (= (mapv :operation-id trace)
           (mapv :instance-id family-trace)))
    (is (= [:counter/increment :counter/increment]
           (mapv :family-id family-trace)))
    (is (= [1 2] (mapv #(get-in % [:arguments :amount]) family-trace)))
    (is (= [:helper/a :helper/a]
           (mapv :principal family-trace)))))

(deftest cross-family-counterexample-identifies-caller-and-arguments-test
  (let [result (request-check [(claim-family) unsafe-revoke])
        witness (first (:violations result))
        choices (:family-trace witness)]
    (is (= :counterexample (:status result)))
    (is (= 3 (get-in result [:family-expansion :instance-count])))
    (is (= 2 (:depth witness)))
    (is (= [:request/claim :membership/revoke]
           (mapv :family-id choices)))
    (is (= [:helper/a :manager/a] (mapv :principal choices)))
    (is (= [{:request-id :request-1} {:membership-id :membership-1}]
           (mapv :arguments choices)))
    (is (= [:committed :committed]
           (mapv :status (:trace witness))))
    (is (= :helper/a (get-in witness [:state request-target :owner])))
    (is (false? (get-in witness [:state membership-target :active?])))))

(deftest atomic-revocation-and-release-preserves-modeled-rule-test
  (let [result (request-check [(claim-family) atomic-revoke-and-release])]
    (is (= :holds-in-explored-model (:status result)))
    (is (empty? (:violations result)))
    (is (= 3 (get-in result [:family-expansion :instance-count])))))

(deftest missing-operation-family-can-hide-violations-test
  (let [incomplete (request-check [(claim-family)])
        complete (request-check [(claim-family) unsafe-revoke])]
    (is (= :holds-in-explored-model (:status incomplete)))
    (is (= :counterexample (:status complete)))
    (is (contains? (get-in incomplete [:family-scope :assumed])
                   :truthful-complete-declared-operation-families))))

(deftest initially-broken-rule-has-no-attributed-family-steps-test
  (let [broken (assoc-in request-state [membership-target :active?] false)
        broken (assoc-in broken [request-target :owner] :helper/a)
        result (family/check-families broken [(claim-family)] [owner-rule]
                                      {:max-depth 1 :max-transitions 10})
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= :initial-state (:at witness)))
    (is (= 0 (:depth witness)))
    (is (= [] (:trace witness) (:family-trace witness)))))

(deftest invalid-reachability-options-and-exhausted-budget-fail-closed-test
  (let [f [(increment-family [:helper/a :helper/b] [{:amount 1}])]]
    (is (= :exploration-budget-exhausted
           (error-kind #(counter-check f (maximum-rule 999) 2 2))))
    (doseq [options [nil {} {:max-depth 0 :max-transitions 5}
                     {:max-depth 7 :max-transitions 5}
                     {:max-depth 2 :max-transitions 0}
                     {:max-depth 2 :max-transitions 10 :truncate? true}]]
      (is (= :invalid-reachability-bound
             (error-kind #(family/check-families counter-state f
                                                 [(maximum-rule 999)] options)))))))

(deftest downstream-checker-errors-do-not-produce-a-verdict-test
  (let [f (increment-family [:helper/a] [{:amount 1}])]
    (is (= :invalid-rule
           (error-kind #(counter-check [f] {:id :bad-rule} 1 1))))
    (is (= :non-boolean-operation-enablement
           (error-kind #(counter-check [(assoc f :enabled? (constantly :yes))]
                                      (maximum-rule 999) 1 1))))
    (is (= :non-boolean-rule-result
           (error-kind #(counter-check [f] (assoc (maximum-rule 999)
                                                 :holds? (constantly nil)) 1 1))))
    (is (= :invalid-operation-plan
           (error-kind #(counter-check [(assoc f :prepare (constantly nil))]
                                      (maximum-rule 999) 1 1))))
    (is (= :gesso.model.invariant-check/operation-evaluation-failed
           (-> (error-of #(counter-check [(assoc f :prepare
                                               (fn [& _] (throw (ex-info "bad builder" {}))))]
                                         (maximum-rule 999) 1 1))
               ex-data :error/type)))))

(deftest callbacks-can-remain-incomplete-without-the-checker-knowing-test
  (let [weak {:id :counter/always-fine :holds? (constantly true)}
        result (counter-check [(increment-family)] weak 2 100)]
    (is (= :holds-in-explored-model (:status result)))
    (is (contains? (get-in result [:family-scope :not-established])
                   :truth-of-authored-business-rules))
    (is (contains? (get-in result [:family-scope :not-established])
                   :correctness-of-arbitrary-clojure-callbacks))))
