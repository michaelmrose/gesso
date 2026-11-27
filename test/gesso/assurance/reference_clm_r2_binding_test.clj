(ns gesso.assurance.reference-clm-r2-binding-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-clm-r2-binding :as binding]
            [gesso.assurance.reference-admitted-step-argument :as admitted]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.model.inductive-plan :as induction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-issued-obligation-binding
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        description (induction/describe-mapped-admission admission)
        abstract (induction/recheck-mapped-induction! admission)
        checked (catalog/check-operation-set! (get-in source [:inductive-source :catalog]))
        fp (:catalog-fingerprint description)
        declarations (claim/outcome-declarations)
        cases (:cases (outcome/derive-cases! source declarations))
        args [admission source declarations fp cases]
        report (apply binding/derive! args)
        argument (:argument report)
        by-op (:original-obligations-by-operation report)]
    (is (= :conditional-original-clm-r2-binding (:status report)) (pr-str (dissoc report :argument)))
    (is (= :CLM-R2 (get-in report [:target :id])))
    (is (= :open (:original-obligation-status report)))
    (is (= :issued-closed-claim-fixture-only (:scope report)))
    (is (= fp (:catalog-fingerprint report)))
    (is (= (:fingerprint abstract) (:abstract-report-fingerprint report)))
    (is (= (:original-open-obligations description) (:original-open-obligation-ids-by-operation report)))
    (is (= (:declared-operation-ids checked) (set (keys by-op))))
    (is (= :related-not-entailing (:condition-link-semantics report)))
    (is (= :not-established (:original-target-coverage report) (:universal-refinement report)))
    (is (= :not-performed (:premise-acceptance report) (:proof-discharge report)))
    (is (= :not-consumed (:physical-evidence report)))
    (is (= (:remaining-conditions argument) (:remaining-conditions report)))
    (is (= (get-in abstract [:classification :open]) (:abstract-open-assumptions report)))
    (doseq [[id row] by-op]
      (let [all (:open-obligations row) links (:related-condition-ids row)]
        (is (= (get-in description [:original-open-obligations id]) (set (keys all))))
        (is (every? #(= :open (:status %)) (vals all)))
        (is (= (set (keys all)) (into (:unlinked-obligation-ids row) (keys links))))
        (is (contains? (:unlinked-obligation-ids row) [:implementation :integer-semantics-correspondence]))
        (is (= :each-real-accepted-step-refines-the-declared-transition
               (get-in all [[:implementation :production-transition-correspondence] :required-property])))
        (is (= (get-in description [:operation-mappings id :fingerprint]) (get-in row [:identity :mapping-fingerprint])))))
    (doseq [bad [(assoc args 0 (Object.)) (assoc args 3 "foreign") (assoc args 4 {})
                 (assoc-in args [1 :target-map :request/revision] [:document :requests "foreign" :revision])
                 (assoc args 2 {})]]
      (is (= :not-established (:status (apply binding/derive! bad)))))
    (let [run (fn [d a c g]
                (try (#'binding/bind-register! source d a c g)
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
          id (first (keys by-op))]
      (is (= :clm-r2-original-register-mismatch
             (run description (update-in abstract [:classification :original-obligations :by-operation id] disj [:implementation :production-transition-correspondence]) checked argument)))
      (is (= :clm-r2-original-register-mismatch
             (run description abstract checked (update argument :remaining-conditions disj :abort-preserves-complete-mapped-image))))
      (is (= :clm-r2-alternative-obligation-mismatch
             (run (assoc-in description [:operation-mappings id :fingerprint] "foreign") abstract checked argument))))
    (with-redefs [admitted/derive! (constantly argument)]
      (is (= :clm-r2-boundary-replaced (:reason (apply binding/derive! args)))))))
