(ns gesso.assurance.reference-clm-r2-condition-inventory-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-clm-r2-condition-inventory :as inventory]
            [gesso.assurance.reference-clm-r2-binding :as binding]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.inductive-plan :as induction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-issued-open-inventory
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        description (induction/describe-mapped-admission admission)
        declarations (claim/outcome-declarations)
        cases (:cases (outcome/derive-cases! source declarations))
        args [admission source declarations (:catalog-fingerprint description) cases]
        report (apply inventory/derive! args)
        groups (:groups report)
        b (:original-binding report)
        conditions (:remaining-conditions report)]
    (is (= :complete-open-condition-review (:status report)) (pr-str (dissoc report :original-binding)))
    (is (= (:remaining-conditions b) conditions))
    (is (= 7 (count groups)))
    (is (= (zipmap conditions (repeat 1)) (frequencies (mapcat :conditions (vals groups)))))
    (is (= (:original-open-obligations description) (:original-open-obligation-ids-by-operation b)))
    (is (= :open (:original-obligation-status report)))
    (is (= :not-established (:universal-refinement report)))
    (is (= :not-performed (:premise-acceptance report) (:proof-discharge report)))
    (is (every? #(= :open (:status %)) (vals groups)))
    (is (every? #(= :not-performed (:trust-acceptance %) (:proof-discharge %)) (vals groups)))
    (is (= [:writer-mediation :provider-effects] (:priority report)))
    (is (contains? (get-in groups [:writer-mediation :requires]) :deployment-isolation))
    (is (contains? (get-in groups [:provider-effects :requires]) :provider-contract))
    (is (= :adapter-policy-does-not-intercept-raw-provider-calls (get-in groups [:writer-mediation :limitation])))
    (is (= :not-established (:status (apply inventory/derive! (assoc args 0 (Object.))))))
    (with-redefs [binding/derive! (constantly b)]
      (is (= :condition-inventory-boundary-replaced (:reason (apply inventory/derive! args)))))))

(deftest reject-condition-inventory-drift
  (let [groups @#'inventory/review-groups
        conditions (into #{} (mapcat :conditions) (vals groups))
        run (fn [c g] (try (#'inventory/classify! c g)
                          (catch clojure.lang.ExceptionInfo e (ex-data e))))
        c :all-physical-writers-mediated-by-exact-installed-policy]
    (is (= #{:foreign} (:unclassified (run (conj conditions :foreign) groups))))
    (is (= #{c} (:extraneous (run (disj conditions c) groups))))
    (is (= #{c} (:overlapping (run conditions (update-in groups [:provider-effects :conditions] conj c)))))
    (is (= :incomplete-or-overlapping-condition-review (:reason (run conditions (assoc-in groups [:writer-mediation :requires] #{})))))
    (is (= (get-in groups [:writer-mediation :conditions]) (:unclassified (run conditions (dissoc groups :writer-mediation)))))))
