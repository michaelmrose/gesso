(ns gesso.live.application-preflight.relational-definition-test
  "v837 regressions for the opt-in, source-owned relational application handle.

   These tests check that the v836 one-source construction admits the same
   finite rules and registered operations for startup and later diagnostic
   comparison, without exposing or accepting caller-edited assurance reports.
   A passing bounded model is neither a policy-completeness claim nor proof
   that arbitrary production executions refine its Clojure callbacks."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]))

(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private counter-key [:counters :one])
(def ^:private initial-state
  {counter-key {:xt/id :one
                :revision 0
                :created-at :t0
                :updated-at :t0
                :n 0
                :parent nil
                :active? true}})

(def ^:private parent-rule
  {:id :counter/parent-active
   :source-kind :counters
   :source-where {}
   :reference-attr :parent
   :allow-unset? true
   :target-kind :counters
   :target-where {:active? true}})

(defn- family
  ([id] (family id false))
  ([id invalid-parent?]
   {:id id
    :principals [:helper/a]
    :arguments [{:delta 1}]
    :enabled? (fn [_ _ _] true)
    :prepare (fn [state _ _]
               (let [before (get state counter-key)
                     after (cond-> (-> before
                                       (update :revision inc)
                                       (update :n inc)
                                       (assoc :updated-at :t1))
                             invalid-parent? (assoc :parent :missing))]
                 {:commands [(command/update-command
                              :counters :increment before after version-keys)]
                  :guards []
                  :emit false}))}))

(defn- relational-model
  ([] (relational-model false))
  ([invalid-parent?]
   {:initial-state initial-state
    :families [(family :request/claim invalid-parent?)
               (family :request/reassign)
               (family :request/cancel)]
    :declarations [parent-rule]
    :options {:max-depth 2 :max-transitions 50}}))

(defn- definition
  [application-options]
  {:application-options application-options
   :relational-model (relational-model)})

(defn- error-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn- error-kind [f]
  (:error/kind (error-data f)))

(deftest admitted-construction-has-redacted-inspectable-scope
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (sut/inspect handle)]
        (is (not (map? handle)) "The authority is not a caller-editable map")
        (is (= #{:status :registered-operation-ids
                 :relational-rule-ids :assurance-scope}
               (set (keys report))))
        (is (= :admitted-finite-relational-construction (:status report)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (:registered-operation-ids report))))
        (is (= [:counter/parent-active] (:relational-rule-ids report)))
        (is (= sut/assurance-scope (:assurance-scope report)))
        (is (contains? (get-in report [:assurance-scope :not-established])
                       :all-executions-correspond-to-model))
        (is (not (.contains (pr-str report) ":initial-state")))
        (is (not (.contains (pr-str report) ":prepare")))
        (is (= report (sut/inspect handle)))))))

(deftest edited-reports-and-recreated-objects-have-no-authority
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (sut/inspect handle)
            forged (assoc report :status :admitted-finite-relational-construction)]
        (doseq [candidate [nil (Object.) report forged {}]]
          (is (= :unknown-construction
                 (error-kind #(sut/inspect candidate))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! candidate nil)))))))))

(deftest declaration-must-be-closed-before-building-an-application
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)]
        (with-redefs [application/require-application-assembly!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Unexpected construction" {})))]
          (doseq [bad [nil {} []
                       {:application-options options}
                       {:relational-model (relational-model)}
                       (assoc (definition options) :forged-certificate true)]]
            (is (= :invalid-definition
                   (error-kind #(sut/construct! bad)))))
          (is (zero? @calls)))))))

(deftest model-mismatch-and-invalid-transitions-fail-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)]
        (is (= :operation-identity-coverage-mismatch
               (error-kind
                #(sut/construct!
                  (update-in base [:relational-model :families] pop)))))
        (is (= :invalid-declaration
               (error-kind
                #(sut/construct!
                  (assoc-in base [:relational-model :declarations 0 :holds?]
                            (constantly true))))))
        (is (= :exploration-budget-exhausted
               (error-kind
                #(sut/construct!
                  (assoc-in base [:relational-model :options :max-transitions]
                            1)))))
        (let [bad (assoc base :relational-model (relational-model true))
              error (error-data #(sut/construct! bad))]
          (is (= :finite-relational-invariant-violation (:error/kind error)))
          (is (= :counterexample (get-in error [:error/details :status])))
          (is (= :counter/parent-active
                 (get-in error [:error/details :counterexamples 0 :rule-id])))
          (is (not (contains? (:error/details error) :initial-state))))))))

(deftest comparison-refuses-unissued-physical-evidence
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))]
        (doseq [attempt [nil (Object.)
                         {:operation :request/claim
                          :principal :helper/a
                          :arguments {:delta 1}}]]
          (is (= :unrecognized-executor-bound-evidence
                 (error-kind #(sut/compare! handle attempt)))))))))

(deftest both-startup-arities-reuse-the-exact-admitted-assembly
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [actual-require application/require-application-assembly!
            checked (atom nil)
            started (atom [])
            initial-system {:fixture/system true}
            modules-var #'fixture/standard-operation-acquisition
            order [application/application-handler-module-id]]
        (with-redefs [application/require-application-assembly!
                      (fn [opts]
                        (let [assembly (actual-require opts)]
                          (reset! checked assembly)
                          assembly))
                      application/start-biff-application!
                      (fn [& args]
                        (swap! started conj args)
                        :started)]
          (let [handle (sut/construct! (definition options))]
            (is (application/application-assembly? @checked))
            (is (= :started (sut/start-biff! handle modules-var order)))
            (is (= :started (sut/start-biff!
                             handle initial-system modules-var order)))
            (is (= 2 (count @started)))
            (is (every? #(identical? @checked (first %)) @started))
            (is (= [@checked modules-var order] (first @started)))
            (is (= [@checked initial-system modules-var order]
                   (second @started)))))))))

(deftest stale-artifact-revokes-inspection-comparison-and-startup
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))
            starts (atom 0)]
        (is (= :admitted-finite-relational-construction
               (:status (sut/inspect handle))))
        (spit artifact-path "modified after construction\n")
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! starts inc) :unexpected-start)]
          (is (= :stale-application-assembly
                 (error-kind #(sut/inspect handle))))
          (is (= :stale-application-assembly
                 (error-kind #(sut/compare! handle nil))))
          (is (= :stale-application-assembly
                 (error-kind #(sut/start-biff! handle nil []))))
          (is (zero? @starts)))))))
