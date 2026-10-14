(ns gesso.live.application-preflight.operation-family-coverage-test
  "Reconstructed v857 regressions for the missing v825 operation-family coverage suite.

   Exercise the surviving v824 registered-operation coverage boundary against
   real, current ApplicationAssemblies. These tests establish exact registry
   identifier coverage and fail-closed bounded MODEL checking; they do not
   establish truthful family callbacks, complete business policy, actual
   executor correspondence, or unbounded invariants."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.operation-family-coverage :as coverage]
   [gesso.model.command :as command]
   [gesso.model.operation-family :as family]))

(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private target [:counters :one])
(def ^:private initial-state
  {target {:xt/id :one :revision 0
           :created-at :t0 :updated-at :t0 :n 0}})

(def ^:private rules
  [{:id :counters/nonnegative
    :holds? (fn [state]
              (<= 0 (get-in state [target :n])))}])

(def ^:private bounds {:max-depth 2 :max-transitions 20})

(def ^:private operation-ids
  #{:request/claim :request/reassign :request/cancel})

(defn- operation-family
  ([id] (operation-family id {}))
  ([id {:keys [calls enabled? prepare? principals arguments]
        :or {enabled? false
             prepare? true
             principals [:helper/a]
             arguments [{:increment 1}]}}]
   {:id id
    :principals principals
    :arguments arguments
    :enabled? (fn [state principal args]
                (when calls (swap! calls update :enabled (fnil inc 0)))
                (if (fn? enabled?)
                  (enabled? state principal args)
                  enabled?))
    :prepare (fn [state _principal {:keys [increment]}]
               (when calls (swap! calls update :prepared (fnil inc 0)))
               (when-not prepare?
                 (throw (ex-info "A disabled family must not prepare" {})))
               (let [before (get state target)
                     after (-> before
                               (update :revision inc)
                               (update :n + increment)
                               (assoc :updated-at :t1))]
                 {:commands [(command/update-command
                              :counters :increment before after version-keys)]
                  :guards []
                  :emit false}))}))

(defn- families
  ([] (families {}))
  ([claim-options]
   [(operation-family :request/claim claim-options)
    (operation-family :request/reassign)
    (operation-family :request/cancel)]))

(defn- check!
  ([assembly] (check! assembly (families) rules bounds))
  ([assembly modeled-families] (check! assembly modeled-families rules bounds))
  ([assembly modeled-families modeled-rules options]
   (coverage/check-registered-families
    assembly initial-state modeled-families modeled-rules options)))

(defn- problem [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo exception
         (ex-data exception))))

(defn- error-kind [f]
  (:error/kind (problem f)))

(defn- with-application [f]
  (fixture/with-closed-application f))

(deftest exact-registered-identities-produce-scoped-bounded-verdict
  (with-application
    (fn [{:keys [assembly]}]
      (let [result (check! assembly)
            report (:registry-coverage result)
            scope (:registry-scope result)]
        (is (= :holds-in-explored-model (:status result)))
        (is (= :exact-registered-operation-id-coverage (:status report)))
        (is (= :current-application-assembly-operation-summary (:source report)))
        (is (= (vec (sort-by str operation-ids))
               (:registered-operation-ids report)
               (:declared-family-ids report)))
        (is (= 3 (:expanded-instance-count report)))
        (is (= :not-established (:family-to-executor-correspondence report)))
        (is (= :not-established (:domain-completeness report)))
        (is (= coverage/assurance-scope scope))
        (is (contains? (:checked scope)
                       :exact-declared-family-to-registry-identity-coverage))
        (is (contains? (:not-established scope)
                       :whole-application-certification))))))

(deftest declared-family-order-does-not-change-registry-identity-coverage
  (with-application
    (fn [{:keys [assembly]}]
      (let [reordered (vec (reverse (families)))
            report (:registry-coverage (check! assembly reordered))]
        (is (= (vec (sort-by str operation-ids))
               (:registered-operation-ids report)))
        (is (= (:registered-operation-ids report)
               (:declared-family-ids report)))
        (is (= (mapv :id reordered)
               (get-in (check! assembly reordered)
                       [:family-expansion :family-ids])))))))

(deftest missing-registered-operations-fail-before-any-business-callback
  (with-application
    (fn [{:keys [assembly]}]
      (let [calls (atom {})
            supplied [(operation-family :request/claim {:calls calls})]
            ex (problem #(check! assembly supplied))]
        (is (= :operation-identity-coverage-mismatch (:error/kind ex)))
        (is (= #{:request/reassign :request/cancel}
               (get-in ex [:error/details :missing-registered-operations])))
        (is (empty? (get-in ex [:error/details :unregistered-family-ids])))
        (is (= {} @calls))))))

(deftest invented-operations-fail-before-any-business-callback
  (with-application
    (fn [{:keys [assembly]}]
      (let [calls (atom {})
            supplied (conj (families {:calls calls})
                           (operation-family :fabricated/operation {:calls calls}))
            ex (problem #(check! assembly supplied))]
        (is (= :operation-identity-coverage-mismatch (:error/kind ex)))
        (is (= #{:fabricated/operation}
               (get-in ex [:error/details :unregistered-family-ids])))
        (is (empty? (get-in ex [:error/details :missing-registered-operations])))
        (is (= {} @calls))))))

(deftest missing-and-invented-identities-are-both-reported
  (with-application
    (fn [{:keys [assembly]}]
      (let [supplied [(operation-family :request/claim)
                      (operation-family :fabricated/operation)]
            details (:error/details
                     (problem #(check! assembly supplied)))]
        (is (= #{:request/reassign :request/cancel}
               (:missing-registered-operations details)))
        (is (= #{:fabricated/operation}
               (:unregistered-family-ids details)))))))

(deftest duplicate-families-are-rejected-not-counted-toward-coverage
  (with-application
    (fn [{:keys [assembly]}]
      (is (= :duplicate-family
             (error-kind #(check! assembly
                                  (conj (families)
                                        (operation-family :request/claim)))))))))

(deftest invalid-and-duplicate-domains-are-rejected-without-callbacks
  (with-application
    (fn [{:keys [assembly]}]
      (let [calls (atom {})
            original (families {:calls calls})]
        (is (= :invalid-domain
               (error-kind #(check! assembly
                                    (assoc-in original [0 :principals] [])))))
        (is (= :invalid-domain
               (error-kind #(check! assembly
                                    (assoc-in original [0 :arguments] [{:x (Object.)}])))))
        (is (= :duplicate-domain-choice
               (error-kind #(check! assembly
                                    (assoc-in original [0 :principals]
                                              [:helper/a :helper/a])))))
        (is (= :duplicate-domain-choice
               (error-kind #(check! assembly
                                    (assoc-in original [0 :arguments]
                                              [{:increment 1} {:increment 1}])))))
        (is (= {} @calls))))))

(deftest finite-instance-ceiling-cannot-silently-sample-or-truncate
  (with-application
    (fn [{:keys [assembly]}]
      (let [calls (atom {})
            oversized (mapv #(assoc % :principals [:helper/a :helper/b :helper/c])
                            (families {:calls calls}))
            ex (problem #(check! assembly oversized))]
        (is (= :instance-bound-exceeded (:error/kind ex)))
        (is (= 9 (get-in ex [:error/details :expanded-instances])))
        (is (= family/maximum-instances (get-in ex [:error/details :maximum])))
        (is (= {} @calls))))))

(deftest exact-coverage-still-expands-every-admitted-principal-argument-pair
  (with-application
    (fn [{:keys [assembly]}]
      (let [expanded (assoc-in (families) [0 :principals]
                               [:helper/a :helper/b])
            result (check! assembly expanded)
            instances (get-in result [:family-expansion :instances])]
        (is (= :holds-in-explored-model (:status result)))
        (is (= 4 (count instances)))
        (is (= 4 (get-in result [:registry-coverage :expanded-instance-count])))
        (is (= #{:helper/a :helper/b}
               (set (map :principal (filter #(= :request/claim (:family-id %))
                                            instances)))))))))

(deftest detached-valid-report-is-not-an-admitted-assembly
  (with-application
    (fn [{:keys [assembly report]}]
      (is (application/application-assembly? assembly))
      (is (application/valid? report))
      (is (= :application-assembly-required
             (error-kind #(check! report))))
      (is (= :application-assembly-required
             (error-kind #(check! {:status :valid})))))))

(deftest typed-but-forged-assembly-is-not-accepted-as-current
  (with-application
    (fn [{:keys [assembly]}]
      (let [forged (assoc assembly :operation-acquisition-assembly
                          {:fabricated/authority true})]
        (is (some? (problem #(check! forged))))
        (is (= :application-assembly-required
               (error-kind #(check! nil))))))))

(deftest changed-physical-browser-artifact-invalidates-coverage
  (with-application
    (fn [{:keys [assembly artifact-path]}]
      (is (= :holds-in-explored-model (:status (check! assembly))))
      (spit artifact-path "tampered since browser receipt\n")
      (is (false? (application/application-assembly? assembly)))
      (is (some? (problem #(check! assembly)))))))

(deftest invalid-model-rules-are-not-accepted-after-registry-match
  (with-application
    (fn [{:keys [assembly]}]
      (is (= :invalid-rules
             (error-kind #(check! assembly (families) [] bounds))))
      (is (= :invalid-rule
             (error-kind #(check! assembly (families)
                                  [{:id :counters/nonnegative
                                    :holds? :not-a-function}]
                                  bounds)))))))

(deftest reachable-business-rule-counterexample-remains-visible
  (with-application
    (fn [{:keys [assembly]}]
      (let [result (check! assembly
                           (families {:enabled? true})
                           [{:id :counters/never-increment
                             :holds? (fn [state]
                                       (zero? (get-in state [target :n])))}]
                           bounds)
            witness (first (:violations result))]
        (is (= :counterexample (:status result)))
        (is (= :counters/never-increment (:rule-id witness)))
        (is (= 1 (:depth witness)))
        (is (= :committed (:status witness)))
        (is (= :request/claim
               (-> witness :family-trace first :family-id)))))))

(deftest exploration-budget-exhaustion-never-produces-success
  (with-application
    (fn [{:keys [assembly]}]
      (is (= :exploration-budget-exhausted
             (error-kind #(check! assembly
                                  (families {:enabled? true})
                                  rules
                                  {:max-depth 2 :max-transitions 1})))))))

(deftest registry-coverage-does-not-invoke-authoritative-executors
  (let [invocations (atom 0)
        operations (into {}
                         (map (fn [[id operation]]
                                [id (fixture/trusted-operation
                                     id
                                     {:published-change-topics
                                      (:published-change-topics operation)
                                      :execute! (fn [_]
                                                  (swap! invocations inc)
                                                  (throw (ex-info "Must not execute" {})))})]))
                         (fixture/standard-operations))]
    (fixture/with-closed-application
      {:operations operations}
      (fn [{:keys [assembly]}]
        (let [result (check! assembly)]
          (is (= :holds-in-explored-model (:status result)))
          (is (zero? @invocations)))))))

(deftest callback-execution-stays-on-the-modeled-side-of-the-boundary
  (with-application
    (fn [{:keys [assembly]}]
      (let [calls (atom {})
            result (check! assembly (families {:calls calls :enabled? true}))]
        (is (= :holds-in-explored-model (:status result)))
        (is (pos? (:enabled @calls 0)))
        (is (pos? (:prepared @calls 0)))
        (is (= :not-established
               (get-in result [:registry-coverage :family-to-executor-correspondence])))))))
