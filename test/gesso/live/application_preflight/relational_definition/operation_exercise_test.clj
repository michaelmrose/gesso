(ns gesso.live.application-preflight.relational-definition.operation-exercise-test
  "Relational definition: operation exercise.

   The bounded model distinguishes enabled attempts from disabled choices,
   recognizes rejected transactions as attempts, and optionally requires
   specified operations to have been attempted. These observations do not
   establish complete production execution coverage.

   Extracted from the 171-test regression suite without changing assertions."
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]
   [gesso.live.application-preflight.relational-definition.support :refer [version-keys counter-key initial-state family relational-model definition error-data error-kind relational-audit finite-exploration modeled-operation-exercise expected-operation-count-keys operation-attempt-audit]]))

(deftest admitted-explanation-reports-exact-operation-exercise
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)
            ids (get-in (sut/inspect handle) [:registered-operation-ids])]
        (is (= :observed-in-declared-bounded-model (:status report)))
        (is (= :declared-finite-operation-family-reachable-states
               (:scope report)))
        (is (= ids (mapv :registered-operation-id families)))
        (is (= 3 (count families) (count instances)))
        (is (= 12 (get-in (finite-exploration handle)
                          [:attempted-transitions])))
        (is (= 12 (reduce + (map :enabled-attempts families))))
        (is (= 12 (reduce + (map :committed-attempts instances))))
        (is (= 0 (reduce + (map :rejected-attempts instances))))
        (doseq [[i family-entry instance-entry] (map vector (range)
                                                     families instances)]
          (is (= 1 (:modeled-instance-count family-entry)))
          (is (= (select-keys family-entry expected-operation-count-keys)
                 (select-keys instance-entry expected-operation-count-keys)))
          (is (= {:enablement-checks 4 :disabled-checks 0
                  :enabled-attempts 4 :committed-attempts 4
                  :rejected-attempts 0}
                 (select-keys instance-entry expected-operation-count-keys)))
          (is (= (keyword "gesso.model.operation-family"
                          (str "choice-" i "-0-0"))
                 (:model-instance-id instance-entry)))
          (is (= 0 (:principal-choice-index instance-entry)))
          (is (= 0 (:argument-choice-index instance-entry)))
          (is (true? (:ever-attempted-in-explored-states? family-entry)))
          (is (true? (:ever-attempted-in-explored-states? instance-entry))))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= [] (:never-attempted-modeled-instance-ids report)))
        (is (= #{:completeness-of-real-principals-or-arguments
                 :faithfulness-of-family-callbacks-to-registered-executors
                 :exercise-in-production :unbounded-operation-coverage}
               (:not-established report)))
        (is (contains? (:not-established sut/assurance-scope)
                       :modeled-operation-exercise-does-not-establish-production-execution))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest fully-disabled-families-are-not-reported-as-exercised
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (update-in (definition options) [:relational-model :families]
                                (fn [families]
                                  (mapv #(assoc % :enabled? (constantly false)
                                                  :prepare (fn [& _]
                                                             (throw (ex-info "Disabled operation prepared" {}))))
                                        families)))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)]
        (is (= 0 (:attempted-transitions (finite-exploration handle))))
        (is (true? (:frontier-exhausted? (finite-exploration handle))))
        (is (= 3 (count families) (count instances)))
        (is (= (mapv :registered-operation-id families)
               (:never-attempted-registered-operation-ids report)))
        (is (= (mapv :model-instance-id instances)
               (:never-attempted-modeled-instance-ids report)))
        (doseq [entry (concat families instances)]
          (is (= {:enablement-checks 1 :disabled-checks 1
                  :enabled-attempts 0 :committed-attempts 0
                  :rejected-attempts 0}
                 (select-keys entry expected-operation-count-keys)))
          (is (false? (:ever-attempted-in-explored-states? entry))))))))

(deftest mixed-principal-choices-keep-family-totals-without-leaking-inputs
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [private-principal :helper/private-person-token-851
            private-arguments {:delta 1 :opaque-input "private-arg-token-851"}
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :families 0 :principals]
                                   [:helper/a private-principal])
                         (assoc-in [:relational-model :families 0 :arguments]
                                   [private-arguments])
                         (assoc-in [:relational-model :families 0 :enabled?]
                                   (fn [_ principal _]
                                     (not= principal private-principal))))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)
            claim (first (filter #(= :request/claim (:registered-operation-id %)) families))
            claim-choices (filterv #(= :request/claim
                                       (:registered-operation-id %)) instances)
            printed (pr-str (sut/explain handle))]
        (is (= 4 (count instances)))
        (is (= 2 (:modeled-instance-count claim)))
        (is (= 1 (:enabled-attempts claim)))
        (is (= 1 (:disabled-checks claim)))
        (is (= [0 1] (mapv :principal-choice-index claim-choices)))
        (is (= [0 0] (mapv :argument-choice-index claim-choices)))
        (is (= [1 0] (mapv :enabled-attempts claim-choices)))
        (is (= [0 1] (mapv :disabled-checks claim-choices)))
        (is (= [:request/claim] (->> instances
                                  (filter (complement :ever-attempted-in-explored-states?))
                                  (mapv :registered-operation-id))))
        (is (= [(:model-instance-id (second claim-choices))]
               (:never-attempted-modeled-instance-ids report)))
        (is (= [] (:never-attempted-registered-operation-ids report))
            "One disabled choice must not mark its whole family unexercised")
        (is (= 3 (get-in (finite-exploration handle)
                         [:attempted-transitions])))
        (is (= 3 (reduce + (map :enabled-attempts instances))))
        (is (not (.contains printed "private-person-token-851")))
        (is (not (.contains printed "private-arg-token-851")))
        (is (not (contains? (first instances) :principal)))
        (is (not (contains? (first instances) :arguments)))
        (is (not (.contains printed "#object[")))))))

(deftest rejected-model-attempts-are-attempted-not-disabled
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [aux-key [:counters :aux]
            aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
            stale (assoc aux-doc :revision 99 :updated-at :stale)
            guard (command/guard :counters stale version-keys)
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :initial-state aux-key] aux-doc)
                         (update-in [:relational-model :families 0 :prepare]
                                    (fn [prepare]
                                      (fn [& args]
                                        (update (apply prepare args) :guards
                                                conj guard)))))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 (:registered-operation-exercise report)))
            instance (first (filter #(= :request/claim (:registered-operation-id %))
                                    (:modeled-instance-exercise report)))]
        (is (= 3 (:attempted-transitions (finite-exploration handle))))
        (is (= 1 (:enablement-checks claim)))
        (is (= 0 (:disabled-checks claim)))
        (is (= 1 (:enabled-attempts claim)))
        (is (= 0 (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= (select-keys claim expected-operation-count-keys)
               (select-keys instance expected-operation-count-keys)))
        (is (true? (:ever-attempted-in-explored-states? claim)))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest operation-exercise-summary-fails-closed-before-comparator-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (definition options)
            original (relational-app/require-relational-application!
                      (:application-options declared)
                      (:relational-model declared))
            counts (get-in original [:finite-relational-verdict :operation-exercise])
            verdict (:finite-relational-verdict original)
            mutations [(fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise] pop))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :operation-id]
                                    (constantly :other/instance)))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :enabled-attempts] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :exploration :attempted-transitions] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :disabled-checks] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :family-expansion :instances 0 :instance-id]
                                    (constantly :other/choice)))]
            comparator-calls (atom 0)]
        (is (= 3 (count counts)))
        (is (= 12 (get-in verdict [:exploration :attempted-transitions])))
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "Comparator should not run" {})))]
          (doseq [mutate mutations]
            (let [spoofed (mutate original)]
              (with-redefs [relational-app/require-relational-application!
                            (fn [& _] spoofed)]
                (let [failure (error-data #(sut/construct! declared))]
                  (is (= :incomplete-modeled-operation-exercise
                         (:error/kind failure)))
                  (is (not (contains? (:error/details failure) :initial-state)))
                  (is (not (contains? (:error/details failure) :families)))))))
          (is (zero? @comparator-calls)))))))

(deftest reported-operation-exercise-is-inert-and-does-not-reexecute-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [private-marker "v851-private-callback-capture"
            calls (atom {:enabled 0 :prepare 0})
            declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (-> family
                                              (update :enabled?
                                                      (fn [original]
                                                        (fn [& args]
                                                          (swap! calls update :enabled inc)
                                                          (let [_ private-marker]
                                                            (apply original args)))))
                                              (update :prepare
                                                      (fn [original]
                                                        (fn [& args]
                                                          (swap! calls update :prepare inc)
                                                          (let [_ private-marker]
                                                            (apply original args)))))))
                                        families)))
            handle (sut/construct! declared)
            after-admission @calls
            report (modeled-operation-exercise handle)
            printed (pr-str (sut/explain handle))]
        (is (pos? (:enabled after-admission)))
        (is (pos? (:prepare after-admission)))
        (is (= after-admission @calls))
        (is (= report (modeled-operation-exercise handle)))
        (is (= after-admission @calls))
        (is (not (.contains printed private-marker)))
        (is (not (.contains printed ":initial-state")))
        (is (not (.contains printed "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/inspect report))))
        (is (= :unknown-construction
               (error-kind #(sut/explain report))))))))

;; v853: v852's required-operation attempt gate is a bounded nonvacuity
;; requirement. A modeled enabled attempt (including a rejected transaction)
;; satisfies it, not merely registration or checking an enablement predicate.
;; Neither the gate nor its report proves production execution or commits.

(deftest attempted-operation-gate-is-optional-or-explicitly-empty
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [candidate [(definition options)
                         (assoc (definition options)
                                :require-attempted-operation-ids #{})]]
        (let [handle (sut/construct! candidate)
              audit (operation-attempt-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= [] (:required-attempted-operation-ids audit)))
          (is (= :not-requested (:operation-attempt-gate audit)))
          (is (= :not-established
                 (get-in (relational-audit handle)
                         [:business-rule-catalog :completeness])))
          (is (contains? (:not-established sut/assurance-scope)
                         :modeled-operation-exercise-does-not-establish-production-execution)))))))

(deftest required-attempted-operations-are-reported-in-registered-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [required #{:request/reassign :request/claim :request/cancel}
            handle (sut/construct!
                    (assoc (definition options)
                           :require-attempted-operation-ids required))
            registered (:registered-operation-ids (sut/inspect handle))
            audit (operation-attempt-audit handle)
            report (:modeled-operation-exercise audit)]
        (is (= (set registered) required))
        (is (= registered (:required-attempted-operation-ids audit)))
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (= registered (mapv :registered-operation-id
                                (:registered-operation-exercise report))))
        (is (every? pos? (map :enabled-attempts
                              (:registered-operation-exercise report))))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))
        (is (contains? (:not-established sut/assurance-scope)
                       :operation-attempts-do-not-establish-successful-commits))))))

(deftest disabled-registered-operations-fail-required-attempt-gate
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [prepare-count (atom 0)
            disabled (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (assoc family
                                                 :enabled? (constantly false)
                                                 :prepare (fn [& _]
                                                            (swap! prepare-count inc)
                                                            (throw (ex-info "Disabled" {})))))
                                        families)))
            handle (sut/construct! disabled)
            report (:modeled-operation-exercise (operation-attempt-audit handle))
            registered (:registered-operation-ids (sut/inspect handle))
            requested #{:request/cancel :request/claim}
            rejected (error-data
                      #(sut/construct!
                        (assoc disabled :require-attempted-operation-ids requested)))]
        (is (zero? @prepare-count))
        (is (= registered (:never-attempted-registered-operation-ids report)))
        (is (= :required-operations-unattempted (:error/kind rejected)))
        (is (= (filterv requested registered)
               (get-in rejected [:error/details :unattempted-operation-ids])))
        (is (= :declared-bounded-explored-states
               (get-in rejected [:error/details :scope])))
        (is (not (.contains (pr-str rejected) ":initial-state")))
        (is (not (.contains (pr-str rejected) ":prepare")))
        (is (zero? @prepare-count))))))

(deftest required-operation-can-become-enabled-later-in-the-search
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (-> (definition options)
                     (assoc-in [:relational-model :families 0 :enabled?]
                               (fn [state _ _]
                                 (pos? (get-in state [counter-key :n]))))
                     (assoc :require-attempted-operation-ids #{:request/claim}))
            shallow (assoc-in base [:relational-model :options :max-depth] 1)
            deep (assoc-in base [:relational-model :options :max-depth] 2)
            failure (error-data #(sut/construct! shallow))
            handle (sut/construct! deep)
            audit (operation-attempt-audit handle)
            families (get-in audit [:modeled-operation-exercise
                                    :registered-operation-exercise])
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 families))]
        (is (= :required-operations-unattempted (:error/kind failure)))
        (is (= [:request/claim]
               (get-in failure [:error/details :unattempted-operation-ids])))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (pos? (:disabled-checks claim)))
        (is (pos? (:enabled-attempts claim)))
        (is (= 2 (get-in audit [:finite-exploration :reached-depth])))))))

(deftest unknown-required-operation-ids-reject-in-registered-gate
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [failure (error-data
                     #(sut/construct!
                       (assoc (definition options)
                              :require-attempted-operation-ids
                              #{:request/claim :absent/z :absent/a})))]
        (is (= :unknown-required-operation-ids (:error/kind failure)))
        (is (= [:absent/a :absent/z]
               (get-in failure [:error/details :unknown-operation-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))
        (is (= :unknown-construction
               (error-kind #(sut/explain failure))))))))

(deftest invalid-operation-attempt-requirements-fail-before-application-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            attempts (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! attempts inc)
                        (throw (ex-info "Unexpected application admission" {})))]
          (doseq [invalid [nil false true [] [:request/claim]
                           '(:request/claim) {:request/claim true}
                           :request/claim #{:unqualified} #{nil}
                           #{:request/claim "bad"}]]
            (is (= :invalid-required-operation-ids
                   (error-kind
                    #(sut/construct!
                      (assoc base :require-attempted-operation-ids invalid))))))
          (is (zero? @attempts)))))))

(deftest a-rejected-model-transaction-still-counts-as-an-enabled-attempt
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [aux-key [:counters :aux]
            aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
            stale (assoc aux-doc :revision 99 :updated-at :stale)
            guard (command/guard :counters stale version-keys)
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :initial-state aux-key] aux-doc)
                         (update-in [:relational-model :families 0 :prepare]
                                    (fn [prepare]
                                      (fn [& args]
                                        (update (apply prepare args)
                                                :guards conj guard))))
                         (assoc :require-attempted-operation-ids #{:request/claim}))
            handle (sut/construct! declared)
            audit (operation-attempt-audit handle)
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 (get-in audit [:modeled-operation-exercise
                                                :registered-operation-exercise])))]
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (= 1 (:enabled-attempts claim)))
        (is (zero? (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest actual-invariant-violations-take-precedence-over-attempt-gating
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-attempted-operation-ids #{:request/claim}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample
               (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest unattempted-required-operation-rejects-before-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (-> (definition options)
                     (assoc-in [:relational-model :families 0 :enabled?]
                               (constantly false))
                     (assoc :require-attempted-operation-ids #{:request/claim}))
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Comparator must not run" {})))]
          (is (= :required-operations-unattempted
                 (error-kind #(sut/construct! base))))
          (is (zero? @calls)))))))

(deftest operation-attempt-gate-does-not-reexecute-or-leak-modeled-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v853-secret-private-callback"
            invocations (atom 0)
            declared (-> (definition options)
                         (update-in [:relational-model :families 0 :enabled?]
                                    (fn [enabled?]
                                      (fn [& args]
                                        (swap! invocations inc)
                                        (let [_ secret] (apply enabled? args)))))
                         (assoc :require-attempted-operation-ids #{:request/claim}))
            handle (sut/construct! declared)
            after-admission @invocations
            first-report (sut/explain handle)
            second-report (sut/explain handle)
            audit (get-in first-report [:semantic-audit :relational-assurance])
            printed (pr-str first-report)]
        (is (pos? after-admission))
        (is (= after-admission @invocations))
        (is (= first-report second-report))
        (is (= after-admission @invocations))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (not (.contains printed secret)))
        (is (not (.contains printed ":initial-state")))
        (is (not (.contains printed "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/inspect audit))))))))


