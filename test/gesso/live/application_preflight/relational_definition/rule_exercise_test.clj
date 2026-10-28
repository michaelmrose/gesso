(ns gesso.live.application-preflight.relational-definition.rule-exercise-test
  "Relational definition: rule exercise.

   The opt-in gate requires a rule's source to match in explored states;
   a vacuously true invariant alone is insufficient. A bounded exercise
   observation does not establish complete predicate or production coverage.

   Extracted from the original regression suite without changing its assertions."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.live.application-preflight.relational-definition.support :refer [initial-state parent-rule relational-model definition error-data error-kind relational-audit finite-exploration source-at]]))

(deftest exercise-gate-is-optional-and-empty-set-does-not-strengthen-assurance
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [declared [(definition options)
                        (assoc (definition options)
                               :require-exercised-rule-ids #{})]]
        (let [handle (sut/construct! declared)
              audit (relational-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= [] (get-in audit [:relational-assurance
                                  :required-exercised-rule-ids])))
          (is (= :not-requested
                 (get-in audit [:relational-assurance :rule-exercise-gate])))
          (is (= :not-established
                 (get-in audit [:business-rule-catalog :completeness])))
          (is (false? (get-in audit [:inspection-scope
                                    :complete-business-policy-inventory?]))))))))

(deftest required-initially-matching-rule-is-admitted-and-explained
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct!
                    (assoc (definition options)
                           :require-exercised-rule-ids #{:counter/parent-active}))
            audit (relational-audit handle)]
        (is (= [:counter/parent-active]
               (get-in audit [:relational-assurance
                              :required-exercised-rule-ids])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [] (get-in audit [:relational-assurance
                                :initially-unexercised-rule-ids])))
        (is (true? (get-in audit [:business-rule-catalog
                                 :reachable-state-rule-coverage 0
                                 :ever-matched-source-in-explored-states?])))
        (is (contains? (:not-established sut/assurance-scope)
                       :rule-exercise-does-not-establish-all-predicate-branches))
        (is (= :not-established
               (get-in audit [:business-rule-catalog :completeness])))))))

(deftest required-rule-may-first-match-after-a-transition
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [later (source-at :counter/after-one 1)
            definition (-> (definition options)
                           (assoc-in [:relational-model :declarations]
                                     [later parent-rule])
                           (assoc :require-exercised-rule-ids
                                  #{:counter/parent-active :counter/after-one}))
            handle (sut/construct! definition)
            audit (relational-audit handle)]
        ;; Declaration order, not hash-set iteration order, owns the report.
        (is (= [:counter/after-one :counter/parent-active]
               (get-in audit [:relational-assurance
                              :required-exercised-rule-ids])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [:counter/after-one]
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (= [:counter/after-one]
               (get-in audit [:relational-assurance
                              :initially-unexercised-rule-ids])))
        (is (= 3
               (get-in audit [:business-rule-catalog
                              :reachable-state-rule-coverage 0
                              :matching-source-occurrences])))
        (is (= :not-established
               (get-in audit [:business-rule-catalog :completeness])))))))

(deftest required-but-never-matched-rule-rejects-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [never (source-at :counter/never-ninety-nine 99)
            base (assoc-in (definition options)
                           [:relational-model :declarations]
                           [parent-rule never])
            no-requirement (sut/construct! base)
            rejected (error-data
                      #(sut/construct!
                        (assoc base :require-exercised-rule-ids
                               #{:counter/never-ninety-nine})))]
        ;; The relation itself is valid and vacuously holds in this model.
        (is (= :holds-in-explored-model
               (get-in (relational-audit no-requirement)
                       [:relational-assurance :status])))
        (is (= :required-relational-rules-unexercised
               (:error/kind rejected)))
        (is (= [:counter/never-ninety-nine]
               (get-in rejected [:error/details :unexercised-rule-ids])))
        (is (= :declared-bounded-explored-states
               (get-in rejected [:error/details :scope])))
        (is (not (.contains (pr-str rejected) ":initial-state")))
        (is (not (.contains (pr-str rejected) ":prepare")))))))

(deftest bounded-depth-determines-whether-an-exercise-obligation-is-met
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [rule (source-at :counter/after-two 2)
            base (-> (definition options)
                     (assoc-in [:relational-model :declarations] [rule])
                     (assoc :require-exercised-rule-ids #{:counter/after-two}))
            shallow (assoc-in base [:relational-model :options :max-depth] 1)
            failure (error-data #(sut/construct! shallow))
            deep (sut/construct! base)
            audit (relational-audit deep)]
        (is (= :required-relational-rules-unexercised (:error/kind failure)))
        (is (= [:counter/after-two]
               (get-in failure [:error/details :unexercised-rule-ids])))
        (is (= 2 (get-in audit [:relational-assurance
                               :finite-exploration :reached-depth])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [:counter/after-two]
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (pos? (get-in audit [:business-rule-catalog
                                :reachable-state-rule-coverage 0
                                :matching-source-occurrences])))))))

(deftest unknown-required-rule-ids-fail-without-a-usable-handle
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            failure (error-data
                     #(sut/construct!
                       (assoc base :require-exercised-rule-ids
                              #{:counter/missing-b :counter/missing-a
                                :counter/parent-active})))]
        (is (= :unknown-required-rule-ids (:error/kind failure)))
        (is (= [:counter/missing-a :counter/missing-b]
               (get-in failure [:error/details :unknown-rule-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))
        (is (= :unknown-construction
               (error-kind #(sut/inspect failure))))))))

(deftest malformed-exercise-requirements-fail-before-admitting-an-application
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            calls (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Unexpected expensive admission" {})))]
          (doseq [invalid [nil false true [] [:counter/parent-active]
                           '(:counter/parent-active)
                           {:counter/parent-active true}
                           #{:not-qualified} #{nil}
                           #{:counter/parent-active "invalid"}]]
            (is (= :invalid-required-rule-ids
                   (error-kind #(sut/construct!
                                 (assoc base :require-exercised-rule-ids
                                        invalid))))))
          (is (zero? @calls)))))))

(deftest exercise-gate-does-not-override-actual-relational-violations
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-exercised-rule-ids
                           #{:counter/parent-active}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest rule-exercise-gate-precedes-building-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [never (source-at :counter/never-ninety-nine 99)
            base (-> (definition options)
                     (assoc-in [:relational-model :declarations] [never])
                     (assoc :require-exercised-rule-ids
                            #{:counter/never-ninety-nine}))
            comparator-calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "Unexpected comparator build" {})))]
          (is (= :required-relational-rules-unexercised
                 (error-kind #(sut/construct! base))))
          (is (zero? @comparator-calls)))))))
