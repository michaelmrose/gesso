(ns gesso.model.invariant-reachable-test
  "Permanent bounded-reachability regressions for gesso.model.invariant-check.

   The operations here are independently authored finite Clojure fixtures,
   NOT production HumanHelp policy. These tests exercise state-dependent
   preparation, breadth-first counterexamples, all enabled branches, strict
   exploration limits, and fail-closed admission. A passing result concerns
   the enumerated abstract serial machine only; it does not establish
   authorization, operation completeness, actual XTDB behavior, or proof of
   the host predicates themselves. v858 checks the inert, redacted transition tree; v888 adds permanent
   checks for the v887 private modeled-occurrence state witnesses."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.command :as command]
            [gesso.model.invariant-check :as invariant]
            [gesso.model.tx-model :as reference]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private request
  {:xt/id :request-1 :revision 0 :created-at :t0 :updated-at :t0
   :owner nil})

(def ^:private membership
  {:xt/id :membership-1 :revision 0 :created-at :t0 :updated-at :t0
   :active? true})

(def ^:private counter
  {:xt/id :counter-1 :revision 0 :created-at :t0 :updated-at :t0
   :n 0})

(def ^:private initial-state
  {[:request :request-1] request
   [:membership :membership-1] membership
   [:counter :counter-1] counter})

(def ^:private request-target [:request :request-1])
(def ^:private member-target [:membership :membership-1])
(def ^:private counter-target [:counter :counter-1])

(defn- after-document [before changes]
  (-> before (merge changes) (update :revision inc) (assoc :updated-at :t1)))

(defn- update-command [kind operation before changes]
  (command/update-command kind operation before
                          (after-document before changes) version))

(defn- tx-plan
  ([commands] (tx-plan commands []))
  ([commands guards]
   {:commands (vec commands) :guards (vec guards) :emit false}))

(def ^:private owner-active
  {:id :request/owner-requires-membership
   :holds? (fn [state]
             (or (nil? (get-in state [request-target :owner]))
                 (true? (get-in state [member-target :active?]))))})

(def ^:private membership-active
  {:id :membership/remains-active
   :holds? (fn [state]
             (true? (get-in state [member-target :active?])))})

(defn- max-counter [limit]
  {:id :counter/within-limit
   :holds? (fn [state]
             (<= (get-in state [counter-target :n]) limit))})

(def ^:private claim
  {:id :request/claim
   :enabled? (fn [state]
               (and (nil? (get-in state [request-target :owner]))
                    (true? (get-in state [member-target :active?]))))
   :prepare (fn [state]
              (tx-plan [(update-command :request :claim
                                        (get state request-target)
                                        {:owner :helper-a})]
                       [(command/guard :membership
                                       (get state member-target) version)]))})

(defn- revocation [release?]
  {:id :membership/revoke
   :enabled? (fn [state]
               (true? (get-in state [member-target :active?])))
   :prepare (fn [state]
              (tx-plan
               (cond-> [(update-command :membership :revoke
                                        (get state member-target)
                                        {:active? false})]
                 release?
                 (conj (update-command :request :release
                                       (get state request-target)
                                       {:owner nil})))))})

(defn- increment-operation [id amount]
  {:id id
   :enabled? (constantly true)
   :prepare (fn [state]
              (let [current (get state counter-target)]
                (tx-plan [(update-command :counter :increment current
                                          {:n (+ amount (:n current))})])))})

(def ^:private increment-one (increment-operation :counter/one 1))
(def ^:private increment-two (increment-operation :counter/two 2))

(def ^:private opts {:max-depth 3 :max-transitions 100})

(defn- check
  ([operations rules] (check initial-state operations rules opts))
  ([state operations rules options]
   (invariant/check-reachable state operations rules options)))

(defn- error-of [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- kind-of [f]
  (some-> (error-of f) ex-data :error/kind))

(defn- violation-for [result id]
  (some #(when (= id (:rule-id %)) %) (:violations result)))

(defn- command-revision [model-command]
  (get (into {} (get-in model-command [:model/expected :model/checks]))
       :revision))

(defn- request-revision-in [plan]
  (some->> (:commands plan)
           (filter #(= :request (:model/entity-type %)))
           first
           command-revision))

(deftest reachable-assurance-scope-is-explicit-test
  (let [{:keys [checked assumed not-established] :as scope}
        invariant/reachable-assurance-scope
        result (check [claim] [owner-active])]
    (is (= :gesso.model.invariant-check/finite-reachable-serial-v1
           (:checker scope)))
    (doseq [property [:initial-state-rule-evaluation
                     :per-declared-operation-exercise-counts
                     :rule-preservation-after-every-attempted-operation
                     :all-enabled-choices-through-declared-depth
                     :state-dependent-plan-reconstruction
                     :shortest-named-counterexamples]]
      (is (contains? checked property) (str property)))
    (doseq [premise [:pure-total-truthful-operation-builders
                    :truthful-operation-enablement
                    :complete-modeled-operation-and-argument-instances
                    :serial-atomic-commit-semantics]]
      (is (contains? assumed premise) (str premise)))
    (doseq [limitation [:truth-of-authored-business-rules
                       :completeness-of-available-operations-or-arguments
                       :source-derived-completeness-of-policy-dependencies
                       :unbounded-invariant-preservation
                       :real-xtdb-execution-correspondence
                       :concurrent-execution-linearizability
                       :durable-application-command-replay
                       :application-certification]]
      (is (contains? not-established limitation) (str limitation)))
    (is (= scope (:scope result)))
    (is (= :holds-in-explored-model (:status result)))))

(deftest unsafe-claim-then-revocation-produces-two-step-counterexample-test
  (let [result (check [claim (revocation false)] [owner-active])
        witness (violation-for result (:id owner-active))]
    (is (= :counterexample (:status result)))
    (is (= :after-operation (:at witness)))
    (is (= 2 (:depth witness)))
    (is (= :membership/revoke (:operation-id witness)))
    (is (= :committed (:status witness)))
    (is (= [:request/claim :membership/revoke]
           (mapv :operation-id (:trace witness))))
    (is (= [:committed :committed]
           (mapv :status (:trace witness))))
    (is (= :helper-a (get-in (:state witness) [request-target :owner])))
    (is (false? (get-in (:state witness) [member-target :active?])))
    (is (= (:state witness) (:state (peek (:trace witness)))))))

(deftest atomic-revoke-and-release-preserves-finite-ownership-test
  (let [result (check [claim (revocation true)] [owner-active])]
    (is (= :holds-in-explored-model (:status result)))
    (is (= [] (:violations result)))
    (is (= 3 (get-in result [:exploration :max-depth])))
    (is (pos? (get-in result [:exploration :attempted-transitions])))
    (is (= (inc (get-in result [:exploration :attempted-transitions]))
           (get-in result [:exploration :visited-state-occurrences])))))

(deftest later-transaction-uses-reached-version-and-state-test
  (let [after-claim (assoc (revocation true)
                           :enabled? (fn [state]
                                       (= :helper-a (get-in state [request-target :owner]))))
        result (check [claim after-claim] [membership-active])
        witness (violation-for result (:id membership-active))
        second-plan (:plan (peek (:trace witness)))]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= [:request/claim :membership/revoke]
           (mapv :operation-id (:trace witness))))
    (is (= :committed (:status (peek (:trace witness)))))
    (is (= 1 (request-revision-in second-plan)))
    (is (= 2 (get-in witness [:state request-target :revision])))
    (is (nil? (get-in witness [:state request-target :owner])))
    (is (false? (get-in witness [:state member-target :active?])))))

(deftest preparation-is-recomputed-after-each-successful-step-test
  (let [result (check [increment-one] [(max-counter 1)] )
        witness (first (:violations result))
        versions (mapv (fn [step]
                         (command-revision (first (get-in step [:plan :commands]))))
                       (:trace witness))]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= [:counter/one :counter/one]
           (mapv :operation-id (:trace witness))))
    (is (= [0 1] versions))
    (is (= [:committed :committed]
           (mapv :status (:trace witness))))
    (is (= 2 (get-in witness [:state counter-target :n])))
    (is (= 2 (get-in witness [:state counter-target :revision])))))

(deftest a-third-transition-is-not-checked-at-depth-two-test
  (let [rule (max-counter 2)
        shallow (check initial-state [increment-one] [rule]
                       {:max-depth 2 :max-transitions 2})
        deeper (check initial-state [increment-one] [rule]
                      {:max-depth 3 :max-transitions 3})]
    (is (= :holds-in-explored-model (:status shallow)))
    (is (= 2 (get-in shallow [:exploration :reached-depth])))
    (is (= 2 (get-in shallow [:exploration :attempted-transitions])))
    (is (= :counterexample (:status deeper)))
    (is (= 3 (get-in deeper [:violations 0 :depth])))
    (is (= 3 (get-in deeper [:exploration :attempted-transitions])))
    (is (= [1 2 3]
           (mapv #(get-in % [:state counter-target :n])
                 (get-in deeper [:violations 0 :trace]))))))

(deftest branching-exploration-checks-every-state-occurrence-test
  (let [result (check initial-state [increment-one increment-two]
                      [(max-counter 999)] opts)
        stats (:exploration result)]
    (is (= :holds-in-explored-model (:status result)))
    (is (= {:kind :bounded-reachable-serial-operations
            :operation-ids [:counter/one :counter/two]
            :max-depth 3
            :max-transitions 100
            :reached-depth 3
            :attempted-transitions 14
            :visited-state-occurrences 15
            :frontier-exhausted? false}
           stats))
    ;; No state deduplication: both paths one→two and two→one reach n=3.
    (is (= 14 (:attempted-transitions stats)))))

(deftest counterexample-is-shortest-and-deterministically-tie-broken-test
  (let [result (check [increment-one increment-two] [(max-counter 2)])
        witness (first (:violations result))
        second-result (check [increment-one increment-two] [(max-counter 2)])]
    (is (= :counterexample (:status result)))
    (is (= 2 (:depth witness)))
    (is (= [:counter/one :counter/two]
           (mapv :operation-id (:trace witness))))
    (is (= 3 (get-in witness [:state counter-target :n])))
    (is (= (:violations result) (:violations second-result)))
    (is (= (:exploration result) (:exploration second-result)))))

(deftest violations-are-reported-in-rule-order-test
  (let [result (check [claim (revocation false)]
                      [owner-active membership-active])]
    (is (= [:request/owner-requires-membership :membership/remains-active]
           (:rule-ids result)))
    (is (= (:rule-ids result) (mapv :rule-id (:violations result))))
    (is (= [2 1] (mapv :depth (:violations result))))
    (is (= [:membership/revoke :membership/revoke]
           (mapv :operation-id (:violations result))))))

(deftest initial-invariant-violation-has-empty-trace-test
  (let [bad-state (-> initial-state
                      (assoc-in [request-target :owner] :helper-a)
                      (assoc-in [member-target :active?] false))
        result (check bad-state [increment-one] [owner-active] opts)
        witness (first (:violations result))]
    (is (= :counterexample (:status result)))
    (is (= :initial-state (:at witness)))
    (is (zero? (:depth witness)))
    (is (nil? (:operation-id witness)))
    (is (nil? (:status witness)))
    (is (= [] (:trace witness)))
    (is (= bad-state (:state witness)))
    (is (= 3 (get-in result [:exploration :attempted-transitions])))))

(deftest disabled-operations-are-never-prepared-test
  (let [invoked (atom 0)
        disabled {:id :counter/disabled
                  :enabled? (constantly false)
                  :prepare (fn [_]
                             (swap! invoked inc)
                             (throw (ex-info "Not enabled" {})))}
        result (check [disabled] [(max-counter 0)])]
    (is (= :holds-in-explored-model (:status result)))
    (is (zero? @invoked))
    (is (= 0 (get-in result [:exploration :attempted-transitions])))
    (is (= 1 (get-in result [:exploration :visited-state-occurrences])))
    (is (= 1 (get-in result [:exploration :reached-depth])))
    (is (true? (get-in result [:exploration :frontier-exhausted?])))))

(deftest a-rejected-operation-remains-in-the-explored-tree-test
  (let [revoked-member (after-document membership {:active? false})
        state (assoc initial-state member-target revoked-member)
        stale-guard (command/guard :membership membership version)
        op {:id :request/stale-claim
            :enabled? (constantly true)
            :prepare (fn [current]
                       (tx-plan [(update-command :request :claim
                                                 (get current request-target)
                                                 {:owner :helper-a})]
                                [stale-guard]))}
        result (check state [op] [owner-active]
                      {:max-depth 1 :max-transitions 1})
        prepared ((:prepare op) state)
        outcome (reference/step state prepared)]
    (is (= :rejected (:status outcome)))
    (is (= :dependency-version (get-in outcome [:failed-precondition :kind])))
    (is (= state (:after outcome)))
    (is (= :holds-in-explored-model (:status result)))
    (is (= 1 (get-in result [:exploration :attempted-transitions])))
    (is (= 2 (get-in result [:exploration :visited-state-occurrences])))))

(deftest exhausted-budget-aborts-rather-than-returns-partial-verdict-test
  (doseq [[operations options expected-attempts]
          [[[increment-one] {:max-depth 3 :max-transitions 2} 2]
           [[increment-one increment-two] {:max-depth 3 :max-transitions 1} 1]]]
    (let [error (error-of #(check initial-state operations [(max-counter 999)] options))]
      (is (= :exploration-budget-exhausted (:error/kind (ex-data error))))
      (is (= expected-attempts
             (get-in (ex-data error) [:error/details :attempted-transitions]))))))

(deftest exact-budget-can-succeed-test
  (let [result (check initial-state [increment-one] [(max-counter 999)]
                      {:max-depth 3 :max-transitions 3})]
    (is (= :holds-in-explored-model (:status result)))
    (is (= 3 (get-in result [:exploration :attempted-transitions])))
    (is (= 4 (get-in result [:exploration :visited-state-occurrences])))))

(deftest invalid-operation-sets-fail-closed-test
  (doseq [operations [nil [] '() [claim claim]
                      (vec (repeat 7 increment-one))
                      [(dissoc claim :prepare)]
                      [(assoc claim :enabled? {})]
                      [(assoc claim :prepare [])]
                      [(assoc claim :id :unqualified)]
                      [(assoc claim :extra true)]]]
    (is (= :gesso.model.invariant-check/invalid-invariant-check
           (:error/type (ex-data (error-of #(check operations [owner-active])))))
        (pr-str operations)))
  (is (= :invalid-operations (kind-of #(check [] [owner-active]))))
  (is (= :duplicate-operation (kind-of #(check [claim claim] [owner-active]))))
  (is (= :invalid-operation (kind-of #(check [(assoc claim :extra true)]
                                           [owner-active])))))

(deftest invalid-rules-are-rejected-before-exploration-test
  (doseq [rules [nil [] '() [(dissoc owner-active :holds?)]
                 [(assoc owner-active :holds? {})]
                 [(assoc owner-active :extra true)]
                 [(assoc owner-active :id :unqualified)]
                 [owner-active owner-active]]]
    (is (= :gesso.model.invariant-check/invalid-invariant-check
           (:error/type (ex-data (error-of #(check [claim] rules)))))
        (pr-str rules)))
  (is (= :duplicate-rule (kind-of #(check [claim]
                                         [owner-active owner-active])))))

(deftest invalid-exploration-options-fail-closed-test
  (doseq [options [nil {} {:max-depth 0 :max-transitions 10}
                   {:max-depth 7 :max-transitions 10}
                   {:max-depth 1.5 :max-transitions 10}
                   {:max-depth 2 :max-transitions 0}
                   {:max-depth 2 :max-transitions 10001}
                   {:max-depth 2 :max-transitions 3.5}
                   {:max-depth 2 :max-transitions 10 :ignore-budget true}]]
    (is (= :invalid-reachability-bound
           (kind-of #(check initial-state [claim] [owner-active] options)))
        (pr-str options))))

(deftest nonboolean-enablement-and-predicate-results-are-rejected-test
  (doseq [result [nil :yes 1 "true"]]
    (is (= :non-boolean-operation-enablement
           (kind-of #(check [(assoc claim :enabled? (constantly result))]
                            [owner-active]))))
    (is (= :non-boolean-rule-result
           (kind-of #(check [claim]
                            [(assoc owner-active :holds? (constantly result))]))))))

(deftest callback-errors-do-not-become-check-results-test
  (let [enablement-error
        (error-of #(check [(assoc claim :enabled?
                                 (fn [_] (throw (ex-info "enabled" {}))))]
                          [owner-active]))
        prepare-error
        (error-of #(check [(assoc claim :prepare
                                 (fn [_] (throw (ex-info "prepare" {}))))]
                          [owner-active]))
        rule-error
        (error-of #(check [claim]
                          [(assoc owner-active :holds?
                                  (fn [_] (throw (ex-info "rule" {}))))]))]
    (is (= :gesso.model.invariant-check/operation-evaluation-failed
           (:error/type (ex-data enablement-error))))
    (is (= :enabled? (:phase (ex-data enablement-error))))
    (is (= :request/claim (:operation-id (ex-data enablement-error))))
    (is (instance? clojure.lang.ExceptionInfo (ex-cause enablement-error)))
    (is (= :gesso.model.invariant-check/operation-evaluation-failed
           (:error/type (ex-data prepare-error))))
    (is (= :prepare (:phase (ex-data prepare-error))))
    (is (instance? clojure.lang.ExceptionInfo (ex-cause prepare-error)))
    (is (= :gesso.model.invariant-check/rule-evaluation-failed
           (:error/type (ex-data rule-error))))
    (is (= :request/owner-requires-membership
           (:rule-id (ex-data rule-error))))))

(deftest missing-and-uninterpretable-plans-fail-closed-test
  (let [empty-plan (assoc claim :prepare (constantly (tx-plan [])))
        raw-assertions (assoc claim :prepare
                              (fn [state]
                                (assoc ((:prepare claim) state)
                                       :assertions [{:assert true}])))
        wrong-return (assoc claim :prepare (constantly :not-a-plan))]
    (is (= :invalid-operation-plan (kind-of #(check [empty-plan] [owner-active]))))
    (is (= :invalid-operation-plan (kind-of #(check [wrong-return] [owner-active]))))
    (is (= :gesso.model.tx-model/unsupported-plan
           (:error/type (ex-data (error-of #(check [raw-assertions]
                                                  [owner-active]))))))))

(deftest malformed-initial-document-state-fails-before-operation-test
  (let [bad-state (assoc initial-state request-target
                         (assoc request :xt/id :wrong-id))]
    (is (= :gesso.model.tx-model/invalid-state
           (:error/type (ex-data
                         (error-of #(check bad-state [claim] [owner-active] opts))))))))

(deftest missing-argument-instances-can-produce-misleading-pass-test
  ;; Checking only the safe release operation does not establish that an
  ;; unsafe revocation (or a different principal/argument) is impossible.
  (let [partial (check [claim (revocation true)] [owner-active])
        expanded (check [claim (revocation true) (assoc (revocation false)
                                                     :id :membership/unsafe-revoke)]
                        [owner-active])]
    (is (= :holds-in-explored-model (:status partial)))
    (is (= :counterexample (:status expanded)))
    (is (contains? (get-in partial [:scope :not-established])
                   :completeness-of-available-operations-or-arguments))))

;; v849: permanent evidence tests for the bounded per-operation exercise
;; summary. Counters describe only choices in this finite search tree; they do
;; not certify that every production operation or argument was enumerated.

(defn- exercise-for [result id]
  (some #(when (= id (:operation-id %)) %) (:operation-exercise result)))

(deftest operation-exercise-counts-every-branch-occurrence-test
  (let [result (check initial-state [increment-one increment-two]
                      [(max-counter 999)] opts)
        counts (:operation-exercise result)]
    ;; At depth three, 1 + 2 + 4 state occurrences have outgoing choices;
    ;; an operation is evaluated once per occurrence, NOT once per unique state.
    (is (= [{:operation-id :counter/one
             :enablement-checks 7 :disabled-checks 0
             :enabled-attempts 7 :committed-attempts 7
             :rejected-attempts 0}
            {:operation-id :counter/two
             :enablement-checks 7 :disabled-checks 0
             :enabled-attempts 7 :committed-attempts 7
             :rejected-attempts 0}]
           counts))
    (is (= 14 (get-in result [:exploration :attempted-transitions])))
    (is (= 14 (reduce + (map :enabled-attempts counts))))
    (is (= 15 (get-in result [:exploration :visited-state-occurrences])))
    (is (= (set [:enablement-checks :disabled-checks :enabled-attempts
                 :committed-attempts :rejected-attempts :operation-id])
           (set (keys (first counts)))))))

(deftest operation-exercise-preserves-declaration-order-and-disabled-choices-test
  (let [prepare-invocations (atom 0)
        disabled {:id :counter/never
                  :enabled? (constantly false)
                  :prepare (fn [_]
                             (swap! prepare-invocations inc)
                             (throw (ex-info "Disabled operation was prepared" {})))}
        result (check initial-state [disabled increment-one]
                      [(max-counter 999)]
                      {:max-depth 2 :max-transitions 2})]
    (is (= [:counter/never :counter/one]
           (mapv :operation-id (:operation-exercise result))))
    (is (= {:operation-id :counter/never
            :enablement-checks 2 :disabled-checks 2
            :enabled-attempts 0 :committed-attempts 0 :rejected-attempts 0}
           (exercise-for result :counter/never)))
    (is (= {:operation-id :counter/one
            :enablement-checks 2 :disabled-checks 0
            :enabled-attempts 2 :committed-attempts 2 :rejected-attempts 0}
           (exercise-for result :counter/one)))
    (is (zero? @prepare-invocations))
    (is (= :holds-in-explored-model (:status result)))))

(deftest operation-exercise-differentiates-disabled-after-transition-test
  (let [result (check initial-state [claim (revocation false)]
                      [owner-active] {:max-depth 2 :max-transitions 10})]
    ;; Root: both enabled. After claim: claim disabled, revoke enabled.
    ;; After revoke: both disabled. No deduplication or hidden pruning.
    (is (= {:operation-id :request/claim
            :enablement-checks 3 :disabled-checks 2
            :enabled-attempts 1 :committed-attempts 1 :rejected-attempts 0}
           (exercise-for result :request/claim)))
    (is (= {:operation-id :membership/revoke
            :enablement-checks 3 :disabled-checks 1
            :enabled-attempts 2 :committed-attempts 2 :rejected-attempts 0}
           (exercise-for result :membership/revoke)))
    (is (= 3 (get-in result [:exploration :attempted-transitions])))
    (is (= :counterexample (:status result)))
    (is (= :membership/revoke
           (:operation-id (violation-for result (:id owner-active)))))))

(deftest operation-exercise-counts-rejected-attempts-without-calling-them-disabled-test
  (let [revoked (assoc initial-state member-target
                       (after-document membership {:active? false}))
        stale-guard (command/guard :membership membership version)
        rejected {:id :request/stale-claim
                  :enabled? (constantly true)
                  :prepare (fn [state]
                             (tx-plan [(update-command :request :claim
                                                       (get state request-target)
                                                       {:owner :helper-a})]
                                      [stale-guard]))}
        result (check revoked [rejected] [owner-active]
                      {:max-depth 2 :max-transitions 2})]
    (is (= :holds-in-explored-model (:status result)))
    (is (= {:operation-id :request/stale-claim
            :enablement-checks 2 :disabled-checks 0
            :enabled-attempts 2 :committed-attempts 0 :rejected-attempts 2}
           (exercise-for result :request/stale-claim)))
    (is (= 2 (get-in result [:exploration :attempted-transitions])))
    (is (= 3 (get-in result [:exploration :visited-state-occurrences])))))

(deftest operation-exercise-depth-cutoff-does-not-count-unexplored-frontier-test
  (let [shallow (check initial-state [increment-one] [(max-counter 999)]
                       {:max-depth 1 :max-transitions 1})
        deep (check initial-state [increment-one] [(max-counter 999)]
                    {:max-depth 3 :max-transitions 3})]
    (is (= {:operation-id :counter/one
            :enablement-checks 1 :disabled-checks 0
            :enabled-attempts 1 :committed-attempts 1 :rejected-attempts 0}
           (exercise-for shallow :counter/one)))
    (is (= 3 (:enablement-checks (exercise-for deep :counter/one))))
    (is (false? (get-in shallow [:exploration :frontier-exhausted?])))
    (is (= :holds-in-explored-model (:status shallow)))))

(deftest operation-exercise-exhausted-frontier-has-no-imaginary-attempts-test
  (let [invocations (atom 0)
        disabled {:id :counter/never
                  :enabled? (fn [_] (swap! invocations inc) false)
                  :prepare (fn [_] (throw (ex-info "Unexpected prepare" {})))}
        result (check initial-state [disabled] [(max-counter 0)]
                      {:max-depth 6 :max-transitions 1})]
    (is (= {:operation-id :counter/never
            :enablement-checks 1 :disabled-checks 1
            :enabled-attempts 0 :committed-attempts 0 :rejected-attempts 0}
           (exercise-for result :counter/never)))
    (is (= 1 @invocations))
    (is (= 0 (get-in result [:exploration :attempted-transitions])))
    (is (= 1 (get-in result [:exploration :visited-state-occurrences])))
    (is (true? (get-in result [:exploration :frontier-exhausted?])))))

(deftest operation-exercise-reports-evaluation-not-predicate-inference-test
  (let [enabled-calls (atom 0)
        prepare-calls (atom 0)
        secret (Object.)
        operation {:id :counter/observed
                   :enabled? (fn [_]
                               (swap! enabled-calls inc)
                               true)
                   :prepare (fn [state]
                              (swap! prepare-calls inc)
                              ;; Closure captures data that must never enter
                              ;; the public count summary.
                              (let [_ secret]
                                ((:prepare increment-one) state)))}
        result (check initial-state [operation] [(max-counter 999)]
                      {:max-depth 2 :max-transitions 2})
        observation (exercise-for result :counter/observed)]
    (is (= 2 @enabled-calls))
    (is (= 2 @prepare-calls))
    (is (= @enabled-calls (:enablement-checks observation)))
    (is (= @prepare-calls (:enabled-attempts observation)))
    (is (= #{:operation-id :enablement-checks :disabled-checks
             :enabled-attempts :committed-attempts :rejected-attempts}
           (set (keys observation))))
    (is (every? (complement fn?) (vals observation)))
    (is (not-any? #(identical? secret %) (vals observation)))
    ;; Reading or printing the inert summary must not reevaluate callbacks.
    (is (string? (pr-str (:operation-exercise result))))
    (is (= 2 @enabled-calls))
    (is (= 2 @prepare-calls))))


;; v858: the transition tree is a redacted record of attempted *occurrences*
;; in a finite search, not a concrete state graph, trace of production, or
;; evidence of complete application policy. The test cases below assert the
;; actual public tree contract without reconstructing private checker internals.

(defn- tree-edges [result]
  (get-in result [:transition-tree :edges]))

(deftest transition-tree-assurance-scope-and-exact-shape-test
  (let [result (check initial-state [increment-one] [(max-counter 9)]
                      {:max-depth 1 :max-transitions 1})
        tree (:transition-tree result)]
    (is (contains? (:checked (:scope result))
                   :redacted-bounded-transition-prefix-tree))
    (is (contains? (:not-established (:scope result))
                   :replay-correspondence-to-production-executors))
    (is (contains? (:not-established (:scope result))
                   :concrete-state-identity-or-deduplication))
    (is (= #{:kind :root-node-id :node-count :edges :state-identity
             :production-correspondence}
           (set (keys tree))))
    (is (= :bounded-reachable-transition-prefix-tree (:kind tree)))
    (is (= 0 (:root-node-id tree)))
    (is (= :occurrences-not-deduplicated (:state-identity tree)))
    (is (= :not-established (:production-correspondence tree)))
    (is (= 2 (:node-count tree)))
    (is (vector? (:edges tree)))
    (is (= #{:parent-node-id :node-id :depth :operation-id :status}
           (set (keys (first (:edges tree))))))))

(deftest transition-tree-linear-chain-retains-parent-and-depth-test
  (let [result (check initial-state [increment-one] [(max-counter 9)]
                      {:max-depth 3 :max-transitions 3})]
    (is (= [{:parent-node-id 0 :node-id 1 :depth 1
             :operation-id :counter/one :status :committed}
            {:parent-node-id 1 :node-id 2 :depth 2
             :operation-id :counter/one :status :committed}
            {:parent-node-id 2 :node-id 3 :depth 3
             :operation-id :counter/one :status :committed}]
           (tree-edges result)))
    (is (= 4 (get-in result [:transition-tree :node-count])))
    (is (= 3 (get-in result [:exploration :attempted-transitions])))
    (is (false? (get-in result [:exploration :frontier-exhausted?])))))

(deftest transition-tree-branching-follows-breadth-first-source-order-test
  (let [options {:max-depth 2 :max-transitions 6}
        result (check initial-state [increment-one increment-two]
                      [(max-counter 9)] options)
        edges (tree-edges result)
        second-run (check initial-state [increment-one increment-two]
                          [(max-counter 9)] options)]
    (is (= [0 0 1 1 2 2] (mapv :parent-node-id edges)))
    (is (= [1 2 3 4 5 6] (mapv :node-id edges)))
    (is (= [1 1 2 2 2 2] (mapv :depth edges)))
    (is (= [:counter/one :counter/two
            :counter/one :counter/two
            :counter/one :counter/two]
           (mapv :operation-id edges)))
    (is (= (repeat 6 :committed) (mapv :status edges)))
    (is (= (:transition-tree result) (:transition-tree second-run)))
    (is (= 7 (get-in result [:transition-tree :node-count])))
    (is (= (count edges)
           (reduce + (map :enabled-attempts (:operation-exercise result)))))
    ;; Distinct paths one→two and two→one reach the same *entire*
    ;; concrete state, yet both modeled occurrences survive.
    (let [one (:after (reference/step initial-state
                                      ((:prepare increment-one) initial-state)))
          two (:after (reference/step initial-state
                                      ((:prepare increment-two) initial-state)))
          one-two (:after (reference/step one ((:prepare increment-two) one)))
          two-one (:after (reference/step two ((:prepare increment-one) two)))]
      (is (= one-two two-one))
      (is (= [[1 4] [2 5]]
             (mapv (juxt :parent-node-id :node-id)
                   (filter #(contains? #{4 5} (:node-id %)) edges)))))))

(deftest transition-tree-does-not-invent-edges-for-disabled-operations-test
  (let [prepare-count (atom 0)
        disabled {:id :counter/disabled
                  :enabled? (constantly false)
                  :prepare (fn [_] (swap! prepare-count inc)
                             (throw (ex-info "Unexpected preparation" {})))}
        result (check initial-state [disabled] [(max-counter 9)]
                      {:max-depth 6 :max-transitions 1})]
    (is (= [] (tree-edges result)))
    (is (= 1 (get-in result [:transition-tree :node-count])))
    (is (= 1 (get-in result [:exploration :reached-depth])))
    (is (true? (get-in result [:exploration :frontier-exhausted?])))
    (is (= 0 (get-in result [:exploration :attempted-transitions])))
    (is (zero? @prepare-count))))

(deftest transition-tree-rejected-attempts-create-new-occurrences-test
  (let [state (assoc initial-state member-target
                     (after-document membership {:active? false}))
        stale-guard (command/guard :membership membership version)
        op {:id :request/rejected-claim
            :enabled? (constantly true)
            :prepare (fn [current]
                       (tx-plan [(update-command :request :claim
                                                 (get current request-target)
                                                 {:owner :helper-a})]
                                [stale-guard]))}
        result (check state [op] [owner-active]
                      {:max-depth 2 :max-transitions 2})]
    (is (= [{:parent-node-id 0 :node-id 1 :depth 1
             :operation-id :request/rejected-claim :status :rejected}
            {:parent-node-id 1 :node-id 2 :depth 2
             :operation-id :request/rejected-claim :status :rejected}]
           (tree-edges result)))
    (is (= 2 (get-in result [:operation-exercise 0 :rejected-attempts])))
    (is (= 0 (get-in result [:operation-exercise 0 :committed-attempts])))
    (is (= :holds-in-explored-model (:status result)))))

(deftest transition-tree-depth-cutoff-excludes-unevaluated-children-test
  (let [result (check initial-state [increment-one increment-two]
                      [(max-counter 9)]
                      {:max-depth 1 :max-transitions 2})]
    (is (= [0 0] (mapv :parent-node-id (tree-edges result))))
    (is (= [1 2] (mapv :node-id (tree-edges result))))
    (is (= [1 1] (mapv :depth (tree-edges result))))
    (is (= 3 (get-in result [:transition-tree :node-count])))
    (is (= 1 (get-in result [:exploration :reached-depth])))
    (is (false? (get-in result [:exploration :frontier-exhausted?])))))

(deftest transition-tree-preserves-disabled-after-transition-branch-shape-test
  (let [result (check initial-state [claim (revocation false)]
                      [owner-active] {:max-depth 2 :max-transitions 10})]
    (is (= [{:parent-node-id 0 :node-id 1 :depth 1
             :operation-id :request/claim :status :committed}
            {:parent-node-id 0 :node-id 2 :depth 1
             :operation-id :membership/revoke :status :committed}
            {:parent-node-id 1 :node-id 3 :depth 2
             :operation-id :membership/revoke :status :committed}]
           (tree-edges result)))
    (is (= 4 (get-in result [:transition-tree :node-count])))
    (is (= 3 (get-in result [:exploration :attempted-transitions])))
    (is (= :counterexample (:status result)))))

(deftest transition-tree-has-path-for-shortest-counterexample-test
  (let [result (check [claim (revocation false)] [owner-active])
        witness (violation-for result (:id owner-active))
        edges (tree-edges result)
        node-ids (reduce
                  (fn [ids {:keys [operation-id status]}]
                    (let [parent (peek ids)
                          matching (filter #(and (= parent (:parent-node-id %))
                                                 (= operation-id (:operation-id %))
                                                 (= status (:status %)))
                                           edges)]
                      (is (= 1 (count matching)))
                      (conj ids (:node-id (first matching)))))
                  [0] (:trace witness))]
    (is (= [:request/claim :membership/revoke]
           (mapv :operation-id (:trace witness))))
    (is (= 2 (:depth witness)))
    (is (= 3 (count node-ids)))
    (is (= 2 (:depth (first (filter #(= (peek node-ids) (:node-id %)) edges)))))
    (is (= :counterexample (:status result)))))

(deftest transition-tree-budget-exhaustion-never-returns-partial-tree-test
  (let [error (error-of #(check initial-state
                                [increment-one increment-two]
                                [(max-counter 9)]
                                {:max-depth 2 :max-transitions 2}))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= :exploration-budget-exhausted (:error/kind (ex-data error))))
    (is (= 2 (get-in (ex-data error)
                     [:error/details :attempted-transitions])))
    (is (not (contains? (ex-data error) :transition-tree)))))

(deftest transition-tree-leaves-concrete-state-and-callbacks-private-test
  (let [secret "private-transition-tree-marker-9fd802"
        secret-state (assoc-in initial-state [counter-target :secret] secret)
        private-value (Object.)
        op {:id :counter/redacted
            :enabled? (fn [_] (let [_ private-value] true))
            :prepare (fn [state] (let [_ private-value]
                                   ((:prepare increment-one) state)))}
        result (check secret-state [op] [(max-counter 9)]
                      {:max-depth 2 :max-transitions 2})
        tree (:transition-tree result)
        serialized (pr-str tree)]
    (is (= [1 2] (mapv :node-id (:edges tree))))
    (is (not (.contains serialized secret)))
    (is (not (.contains serialized "private-value")))
    (is (every? (fn [edge]
                  (= #{:parent-node-id :node-id :depth :operation-id :status}
                     (set (keys edge))))
                (:edges tree)))
    (is (every? (fn [edge]
                  (every? (complement fn?) (vals edge)))
                (:edges tree)))
    (is (not-any? #(identical? private-value %)
                  (mapcat vals (:edges tree))))))

(deftest reading-transition-tree-does-not-reexecute-model-functions-test
  (let [enabled-count (atom 0)
        prepared-count (atom 0)
        predicate-count (atom 0)
        op {:id :counter/observed-transition
            :enabled? (fn [_] (swap! enabled-count inc) true)
            :prepare (fn [state]
                       (swap! prepared-count inc)
                       ((:prepare increment-one) state))}
        rule {:id :counter/observed-invariant
              :holds? (fn [_] (swap! predicate-count inc) true)}
        result (check initial-state [op] [rule]
                      {:max-depth 2 :max-transitions 2})
        before [@enabled-count @prepared-count @predicate-count]]
    (is (= [2 2 3] before))
    (is (= 2 (count (get-in result [:transition-tree :edges]))))
    (is (string? (pr-str (:transition-tree result))))
    (is (= 2 (count (get-in result [:transition-tree :edges]))))
    (is (= before [@enabled-count @prepared-count @predicate-count]))))

;; v888: The v887 node-state capability is a private, process-local witness
;; for a *modeled occurrence*, not production evidence, an exposed state
;; snapshot, or a proof that the modeled prefix happened in production.
(defn- v888-compare [result node-id state]
  (invariant/compare-reachable-occurrence-state! result node-id state))

(defn- v888-safe-check
  ([operations] (v888-safe-check initial-state operations))
  ([state operations]
   (check state operations [(max-counter 999)]
          {:max-depth 2 :max-transitions 8})))

(defn- v888-modeled-next [operation state]
  (:after (reference/step state ((:prepare operation) state))))

(deftest v888-root-and-distinct-one-step-occurrences-test
  (let [result (v888-safe-check [increment-one increment-two])
        one (v888-modeled-next increment-one initial-state)
        two (v888-modeled-next increment-two initial-state)
        root (v888-compare result 0 initial-state)
        first-step (v888-compare result 1 one)
        second-step (v888-compare result 2 two)]
    (is (= 7 (get-in result [:transition-tree :node-count])))
    (is (= [0 0] (mapv :parent-node-id
                       (take 2 (get-in result [:transition-tree :edges])))))
    (is (= [1 2] (mapv :node-id
                       (take 2 (get-in result [:transition-tree :edges])))))
    (is (= :matches-modeled-occurrence-state (:status root)))
    (is (= :matches-modeled-occurrence-state (:status first-step)))
    (is (= :matches-modeled-occurrence-state (:status second-step)))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 1 two))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 2 one))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 1 initial-state))))))

(deftest v888-nonroot-state-witnesses-follow-exact-tree-node-ids-test
  (let [result (v888-safe-check [increment-one increment-two])
        one (v888-modeled-next increment-one initial-state)
        two (v888-modeled-next increment-two initial-state)
        one-one (v888-modeled-next increment-one one)
        one-two (v888-modeled-next increment-two one)
        two-one (v888-modeled-next increment-one two)
        two-two (v888-modeled-next increment-two two)
        edges (get-in result [:transition-tree :edges])]
    (is (= [[1 3] [1 4] [2 5] [2 6]]
           (mapv (juxt :parent-node-id :node-id) (drop 2 edges))))
    (doseq [[node-id expected] [[3 one-one] [4 one-two]
                                [5 two-one] [6 two-two]]]
      (is (= :matches-modeled-occurrence-state
             (:status (v888-compare result node-id expected)))
          (str "node " node-id)))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 3 two-two))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 6 one-one))))))

(deftest v888-repeated-equal-documents-remain-distinct-occurrences-test
  (let [result (v888-safe-check [increment-one increment-two])
        a (v888-modeled-next increment-one initial-state)
        b (v888-modeled-next increment-two initial-state)
        ab (v888-modeled-next increment-two a)
        ba (v888-modeled-next increment-one b)]
    (is (= ab ba))
    (is (not= 4 5))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 4 ab))))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 5 ba))))
    (is (= 4 (:model-node-id (v888-compare result 4 ba))))
    (is (= 5 (:model-node-id (v888-compare result 5 ab))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 3 ab))))))

(deftest v888-assertion-rejected-attempt-has-unchanged-modeled-state-test
  (let [stale-operation {:id :counter/stale-guard
                         :enabled? (constantly true)
                         :prepare (fn [state]
                                    (assoc ((:prepare increment-one) state)
                                           :guards [(command/guard
                                                     :membership
                                                     (assoc (get state member-target)
                                                            :revision 999)
                                                     version)]))}
        result (v888-safe-check [stale-operation])
        edges (get-in result [:transition-tree :edges])]
    (is (= [:rejected :rejected] (mapv :status edges)))
    (is (= [0 1] (mapv :parent-node-id edges)))
    (is (= 3 (get-in result [:transition-tree :node-count])))
    (doseq [node-id [0 1 2]]
      (is (= :matches-modeled-occurrence-state
             (:status (v888-compare result node-id initial-state)))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 2
                                  (v888-modeled-next increment-one initial-state)))))))

(deftest v888-disabled-operation-provides-root-only-witness-test
  (let [disabled {:id :counter/never-enabled
                  :enabled? (constantly false)
                  :prepare (fn [_] (throw (ex-info "must not run" {})))}
        result (v888-safe-check [disabled])]
    (is (= 1 (get-in result [:transition-tree :node-count])))
    (is (= [] (get-in result [:transition-tree :edges])))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 0 initial-state))))
    (is (= :invalid-occurrence-state-witness
           (kind-of #(v888-compare result 1 initial-state))))))

(deftest v888-counterexample-results-keep-their-occurrence-witness-test
  (let [result (check initial-state [increment-one]
                      [(max-counter 0)]
                      {:max-depth 2 :max-transitions 2})
        one (v888-modeled-next increment-one initial-state)
        two (v888-modeled-next increment-one one)]
    (is (= :counterexample (:status result)))
    (is (= 1 (get-in result [:violations 0 :depth])))
    (is (= 3 (get-in result [:transition-tree :node-count])))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 0 initial-state))))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 1 one))))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare result 2 two))))
    (is (= :differs-from-modeled-occurrence-state
           (:status (v888-compare result 2 one))))))

(deftest v888-comparison-is-inert-after-reachability-exploration-test
  (let [enable-count (atom 0)
        prepare-count (atom 0)
        rule-count (atom 0)
        operation {:id :counter/observed-for-witness
                   :enabled? (fn [_] (swap! enable-count inc) true)
                   :prepare (fn [state]
                              (swap! prepare-count inc)
                              ((:prepare increment-one) state))}
        rule {:id :counter/witness-rule
              :holds? (fn [_] (swap! rule-count inc) true)}
        result (check initial-state [operation] [rule]
                      {:max-depth 2 :max-transitions 2})
        after-check [@enable-count @prepare-count @rule-count]
        state-one (v888-modeled-next increment-one initial-state)]
    (is (= [2 2 3] after-check))
    (dotimes [_ 3]
      (is (= :matches-modeled-occurrence-state
             (:status (v888-compare result 0 initial-state))))
      (is (= :matches-modeled-occurrence-state
             (:status (v888-compare result 1 state-one))))
      (is (= :differs-from-modeled-occurrence-state
             (:status (v888-compare result 1 initial-state)))))
    (is (= after-check [@enable-count @prepare-count @rule-count]))))

(deftest v888-witness-result-redacts-private-model-documents-test
  (let [secret "v888-private-document-marker-4ea6c41d"
        state (assoc-in initial-state [counter-target :secret] secret)
        result (v888-safe-check state [increment-one])
        status (v888-compare result 1 (v888-modeled-next increment-one state))
        mismatched (v888-compare result 1 state)
        surface (pr-str result)]
    (is (= :holds-in-explored-model (:status result)))
    (is (not (.contains surface secret)))
    (is (not (.contains (pr-str (:transition-tree result)) secret)))
    (is (not (.contains (pr-str status) secret)))
    (is (not (.contains (pr-str mismatched) secret)))
    (is (= :matches-modeled-occurrence-state (:status status)))
    (is (= :differs-from-modeled-occurrence-state (:status mismatched)))
    (is (= false (:state-contents-exposed? status)))
    (is (= false (:model-callbacks-reexecuted? status)))
    (is (not-any? #{:state :modeled-state :observed-state :document-map}
                  (keys status)))
    (is (= (:scope status) (:scope mismatched)))))

(deftest v888-comparison-reports-limits-without-asserting-history-test
  (let [result (v888-safe-check [increment-one])
        answer (v888-compare result 1
                             (v888-modeled-next increment-one initial-state))]
    (is (= :matches-modeled-occurrence-state (:status answer)))
    (is (= 1 (:model-node-id answer)))
    (is (= :one-finite-modeled-occurrence-versus-caller-supplied-document-map
           (:scope answer)))
    (is (= :exact-clojure-domain-document-map-equality
           (:relation answer)))
    (doseq [key [:observed-state-authenticity
                 :model-prefix-observed-in-production
                 :execution-chronology
                 :production-trace-refinement]]
      (is (= :not-established (get answer key)) (str key)))
    (is (= false (:state-contents-exposed? answer)))
    (is (= false (:model-callbacks-reexecuted? answer)))
    (is (contains? (:checked invariant/reachable-assurance-scope)
                   :private-exact-state-witnesses-for-reachable-occurrences))
    (is (contains? (:not-established invariant/reachable-assurance-scope)
                   :authenticity-of-caller-supplied-observed-states))))

(deftest v888-fail-closed-on-invalid-node-ids-and-detached-results-test
  (let [result (v888-safe-check [increment-one])]
    (doseq [node-id [-10 -1 3 999 nil :node "1" 1/2 1.0]]
      (is (= :invalid-occurrence-state-witness
             (kind-of #(v888-compare result node-id initial-state)))
          (pr-str node-id)))
    (doseq [bad-result [nil {} (dissoc result :occurrence-state-witness)
                        (assoc result :occurrence-state-witness nil)
                        (assoc result :occurrence-state-witness (Object.))]]
      (is (= :invalid-occurrence-state-witness
             (kind-of #(v888-compare bad-result 0 initial-state)))))))

(deftest v888-tampering-with-any-checker-owned-field-rejects-witness-test
  (let [result (v888-safe-check [increment-one])
        altered [(assoc result :status :counterexample)
                 (assoc result :violations [{:rule-id :forged/violation}])
                 (assoc result :rule-ids [:forged/rule])
                 (assoc-in result [:transition-tree :node-count] 99)
                 (assoc-in result [:transition-tree :edges 0 :status] :forged)
                 (assoc-in result [:exploration :attempted-transitions] 42)
                 (assoc result :operation-exercise [])
                 (assoc result :scope {})]]
    (doseq [candidate altered]
      (is (= :invalid-occurrence-state-witness
             (kind-of #(v888-compare candidate 0 initial-state)))
          (pr-str (keys candidate))))
    (let [with-downstream-report (assoc result :downstream/assessment {:pass? false})]
      (is (= :matches-modeled-occurrence-state
             (:status (v888-compare with-downstream-report 0 initial-state)))))))

(deftest v888-witness-handles-reject-incompatible-completed-checks-test
  (let [first-result (v888-safe-check [increment-one])
        second-result (v888-safe-check [increment-two])
        one (v888-modeled-next increment-one initial-state)
        two (v888-modeled-next increment-two initial-state)]
    (is (not (identical? (:occurrence-state-witness first-result)
                         (:occurrence-state-witness second-result))))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare first-result 1 one))))
    (is (= :matches-modeled-occurrence-state
           (:status (v888-compare second-result 1 two))))
    (is (= :invalid-occurrence-state-witness
           (kind-of #(v888-compare
                      (assoc first-result :occurrence-state-witness
                             (:occurrence-state-witness second-result))
                      0 initial-state))))))

(deftest v888-rejects-malformed-caller-state-without-guessing-equality-test
  (let [result (v888-safe-check [increment-one])
        invalid-states [nil [] #{:x} {[:counter :counter-1] nil}
                        {[:counter :counter-1] {:xt/id :someone-else}}
                        {:counter {:xt/id :counter-1}}
                        {[nil :counter-1] {:xt/id :counter-1}}]]
    (doseq [observed invalid-states]
      (is (= :gesso.model.tx-model/invalid-state
             (some-> (error-of #(v888-compare result 1 observed))
                     ex-data :error/type))
          (pr-str observed)))))
