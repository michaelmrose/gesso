(ns gesso.assurance.reference-xtdb-provider-contract-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-xtdb-provider-contract :as contract]
            [gesso.assurance.reference-clm-r2-condition-inventory :as inventory]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.inductive-plan :as induction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest versioned-open-contract-for-issued-catalog
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        description (induction/describe-mapped-admission admission)
        declarations (claim/outcome-declarations)
        cases (:cases (outcome/derive-cases! source declarations))
        args [admission source declarations (:catalog-fingerprint description) cases contract/reviewed-provider]
        report (apply contract/derive! args)
        i (:original-inventory report)
        premises (:premises report)]
    (is (= :open-provider-contract (:status report)) (pr-str (dissoc report :original-inventory :premises)))
    (is (= contract/reviewed-provider (get-in report [:scope :provider])))
    (is (= (:catalog-fingerprint description) (get-in report [:scope :catalog-fingerprint])))
    (is (= (:induction-fingerprint description) (get-in report [:scope :induction-fingerprint])))
    (is (= 8 (count premises)))
    (is (= 8 (count (set (map :premise-fingerprint (vals premises))))))
    (is (every? #(= :open (:discharge-state %)) (vals premises)))
    (is (= 11 (count (:linked-conditions report))))
    (is (= (:linked-conditions report) (set (keys (:condition-links report)))))
    (is (= (:remaining-conditions i) (:remaining-conditions report)))
    (is (= (:original-open-obligations description)
           (get-in i [:original-binding :original-open-obligation-ids-by-operation])))
    (is (= :related-not-entailing (:condition-link-semantics report)))
    (is (= :not-established (:provider-correspondence report) (:loaded-provider-identity report)
           (:universal-refinement report) (:writer-coverage report)))
    (is (= :not-performed (:proof-discharge report) (:premise-acceptance report)))
    (is (contains? (:non-provider-requirements report) :qualified-deployment-isolation))
    (is (= :unsupported-provider-target (:reason (apply contract/derive! (assoc-in args [5 :version] "other")))))
    (is (= :unsupported-provider-target (:reason (apply contract/derive! (assoc-in args [5 :artifacts "com.xtdb/xtdb-core"] "foreign")))))
    (is (= :not-established (:status (apply contract/derive! (assoc args 0 (Object.))))))
    (with-redefs [inventory/derive! (constantly i)]
      (is (= :provider-contract-boundary-replaced (:reason (apply contract/derive! args)))))))

(deftest reject-contract-inventory-drift
  (let [rows @#'contract/clauses links @#'contract/links required (set (keys links))
        run (fn [r c l] (try (#'contract/check-links! r c l)
                             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (nil? (run required rows links)))
    (is (= :provider-contract-inventory-mismatch (run (conj required :foreign) rows links)))
    (is (= :provider-contract-inventory-mismatch (run required rows (dissoc links :abort-preserves-complete-mapped-image))))
    (is (= :provider-contract-inventory-mismatch (run required (dissoc rows ::contract/commit-abort) links)))
    (is (= :provider-contract-inventory-mismatch (run required rows (assoc links :abort-preserves-complete-mapped-image #{:foreign}))))
    (is (= :provider-contract-inventory-mismatch (run required (assoc-in rows [::contract/commit-abort :dependencies] #{:foreign}) links)))
    (is (= :cyclic-provider-contract-dependencies (run required (assoc-in rows [::contract/ordered-transaction-basis :dependencies] #{::contract/commit-abort}) links)))))
