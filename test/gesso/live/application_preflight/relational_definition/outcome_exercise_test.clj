(ns gesso.live.application-preflight.relational-definition.outcome-exercise-test
  "Relational definition: outcome exercise.

   Extracted from the 171-test regression suite without changing tests or assertions."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [counter-key initial-state family relational-model definition error-data error-kind relational-audit finite-exploration modeled-operation-exercise outcome-audit stale-guard-definition]]))

(deftest required-outcome-gate-defaults-to-unrequested
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [candidate [(definition options)
                         (assoc (definition options) :require-operation-outcomes {})]]
        (let [handle (sut/construct! candidate)
              audit (outcome-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= {} (:required-operation-outcomes audit)))
          (is (= :not-requested (:operation-outcome-gate audit)))
          (is (= :not-established
                 (get-in (relational-audit handle)
                         [:business-rule-catalog :completeness])))
          (is (contains? (:not-established sut/assurance-scope)
                         :modeled-operation-outcomes-do-not-establish-production-outcomes)))))))

(deftest required-commits-are-reported-in-registered-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [required {:request/cancel #{:committed}
                      :request/claim #{:committed}}
            handle (sut/construct!
                    (assoc (definition options) :require-operation-outcomes required))
            registered (:registered-operation-ids (sut/inspect handle))
            audit (outcome-audit handle)
            expected (into (array-map)
                           (keep (fn [id] (when (required id) [id [:committed]])))
                           registered)]
        (is (= expected (:required-operation-outcomes audit)))
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (= (vec (keys expected))
               (filterv (set (keys required)) registered)))
        (is (every? pos?
                    (map :committed-attempts
                         (filter #(contains? required (:registered-operation-id %))
                                 (get-in audit [:modeled-operation-exercise
                                                :registered-operation-exercise])))))
        (is (contains? (:not-established sut/assurance-scope)
                       :operation-attempts-do-not-establish-successful-commits))
        (is (contains? (:not-established sut/assurance-scope)
                       :modeled-operation-outcomes-do-not-establish-production-outcomes))))))

(deftest missing-rejections-are-reported-without-claiming-commits-suffice
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            requirements {:request/cancel #{:rejected}
                          :request/claim #{:committed :rejected}}
            registered (:registered-operation-ids
                        (sut/inspect (sut/construct! base)))
            failure (error-data
                     #(sut/construct! (assoc base :require-operation-outcomes requirements)))
            missing (get-in failure [:error/details :unobserved-operation-outcomes])]
        (is (= :required-operation-outcomes-unobserved (:error/kind failure)))
        (is (= (filterv (set (keys requirements)) registered)
               (mapv :operation-id missing)))
        (is (= [{:operation-id :request/claim
                 :unobserved-outcomes [:rejected]}]
               (filterv #(= :request/claim (:operation-id %)) missing)))
        (is (= [{:operation-id :request/cancel
                 :unobserved-outcomes [:rejected]}]
               (filterv #(= :request/cancel (:operation-id %)) missing)))
        (is (= :declared-bounded-explored-states
               (get-in failure [:error/details :scope])))
        (is (not (.contains (pr-str failure) ":initial-state")))
        (is (= :unknown-construction (error-kind #(sut/explain failure))))))))

(deftest unknown-outcome-operation-ids-are-rejected-in-stable-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [failure (error-data
                     #(sut/construct!
                       (assoc (definition options) :require-operation-outcomes
                              {:missing/z #{:committed}
                               :request/claim #{:committed}
                               :missing/a #{:rejected}})))]
        (is (= :unknown-required-operation-outcome-ids (:error/kind failure)))
        (is (= [:missing/a :missing/z]
               (get-in failure [:error/details :unknown-operation-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))))))

(deftest malformed-outcome-requirements-fail-before-model-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            calls (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Invalid outcomes reached model admission" {})))]
          (doseq [invalid [nil true false [] #{:committed} :committed
                           {:request/claim nil} {:request/claim #{}}
                           {:request/claim [:committed]}
                           {:request/claim #{:committed :unknown}}
                           {:request/claim #{:unknown}}
                           {:request/claim #{nil}}
                           {:request/claim :committed}
                           {:unqualified #{:committed}}
                           {"request/claim" #{:committed}}]]
            (is (= :invalid-required-operation-outcomes
                   (error-kind #(sut/construct!
                                 (assoc base :require-operation-outcomes invalid))))
                (pr-str invalid)))
          (is (zero? @calls)))))))

(deftest rejected-transaction-can-satisfy-rejection-but-not-commit
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (stale-guard-definition (definition options))
            handle (sut/construct!
                    (assoc base :require-operation-outcomes
                           {:request/claim #{:rejected}}))
            audit (outcome-audit handle)
            claim (first
                   (filter #(= :request/claim (:registered-operation-id %))
                           (get-in audit [:modeled-operation-exercise
                                          :registered-operation-exercise])))
            failure (error-data
                     #(sut/construct! (assoc base :require-operation-outcomes
                                            {:request/claim #{:committed}})))]
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (= {:request/claim [:rejected]}
               (:required-operation-outcomes audit)))
        (is (= 1 (:enabled-attempts claim)))
        (is (zero? (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= :required-operation-outcomes-unobserved (:error/kind failure)))
        (is (= [{:operation-id :request/claim
                 :unobserved-outcomes [:committed]}]
               (get-in failure [:error/details :unobserved-operation-outcomes])))))))

(deftest both-outcomes-must-be-observed-on-reachable-branches
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            rejected (stale-guard-definition base)
            guarded-prepare (get-in rejected [:relational-model :families 0 :prepare])
            plain-prepare (get-in base [:relational-model :families 0 :prepare])
            ;; The stale guard applies only at the initial state; a different
            ;; operation can advance the counter so the claim then commits.
            mixed (-> rejected
                      (assoc-in [:relational-model :options :max-depth] 2)
                      (assoc-in [:relational-model :families 0 :prepare]
                                (fn [state & args]
                                  (if (zero? (get-in state [counter-key :n]))
                                    (apply guarded-prepare state args)
                                    (apply plain-prepare state args))))
                      (assoc :require-operation-outcomes
                             {:request/claim #{:rejected :committed}}))
            handle (sut/construct! mixed)
            audit (outcome-audit handle)
            claim (first
                   (filter #(= :request/claim (:registered-operation-id %))
                           (get-in audit [:modeled-operation-exercise
                                          :registered-operation-exercise])))]
        (is (= {:request/claim [:committed :rejected]}
               (:required-operation-outcomes audit)))
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (pos? (:committed-attempts claim)))
        (is (pos? (:rejected-attempts claim)))
        (is (= 2 (get-in audit [:finite-exploration :max-depth])))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest outcome-exercise-failures-do-not-create-a-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)
            base (assoc (definition options) :require-operation-outcomes
                        {:request/claim #{:rejected}})]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Comparator must not run" {})))]
          (is (= :required-operation-outcomes-unobserved
                 (error-kind #(sut/construct! base))))
          (is (zero? @calls)))))))

(deftest invariant-counterexamples-precede-outcome-exercise-gates
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-operation-outcomes
                           {:request/claim #{:rejected}}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest outcome-audit-is-inert-redacted-and-not-an-authority
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v855-private-model-predicate"
            invocations (atom 0)
            declared (-> (definition options)
                         (update-in [:relational-model :families 0 :enabled?]
                                    (fn [enabled?]
                                      (fn [& args]
                                        (swap! invocations inc)
                                        (let [_ secret] (apply enabled? args)))))
                         (assoc :require-operation-outcomes
                                {:request/claim #{:committed}}))
            handle (sut/construct! declared)
            after-admission @invocations
            explanation (sut/explain handle)
            again (sut/explain handle)
            audit (get-in explanation [:semantic-audit :relational-assurance])
            serialized (pr-str explanation)]
        (is (pos? after-admission))
        (is (= after-admission @invocations))
        (is (= explanation again))
        (is (= after-admission @invocations))
        (is (= {:request/claim [:committed]}
               (:required-operation-outcomes audit)))
        (is (not (.contains serialized secret)))
        (is (not (.contains serialized ":initial-state")))
        (is (not (.contains serialized "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/explain audit))))))))


;; v860: v859 joins the checker-owned bounded transition prefix tree to the
;; registered operations through the admitted finite family expansion. Test
;; the PUBLIC opaque-handle explanation and fail-before-comparator boundary,
;; not a separately crafted explanation or a model passed by the caller.

