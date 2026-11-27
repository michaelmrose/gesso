(ns gesso.assurance.reference-xtdb-provider-trust-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-xtdb-provider-trust :as trust]
            [gesso.assurance.reference-xtdb-artifact-origin :as origin]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.inductive-plan :as induction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [xtdb.api :as xt]))

(deftest exact-issued-provider-trust
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        description (induction/describe-mapped-admission admission)
        declarations (claim/outcome-declarations)
        cases (:cases (outcome/derive-cases! source declarations))
        args [admission source declarations (:catalog-fingerprint description) cases]
        prepared (apply trust/prepare! args)
        handle (:handle prepared)
        offered (mapv :assumption (vals (:premises prepared)))
        declaration {:scope (:scope prepared) :acceptance :trust-open-premises :premises offered}
        run #(trust/declare! admission handle %)
        accepted (run declaration)]
    (is (= :open-provider-trust-requirements (:status prepared)) (pr-str (dissoc prepared :origin-report)))
    (is (= :conditional-declared-provider-trust (:status accepted)) (pr-str (dissoc accepted :origin-report)))
    (is (= (:scope prepared) (:scope accepted)))
    (is (= (:premises prepared) (:premises accepted)))
    (is (= 8 (count (:trusted-assumptions accepted))))
    (is (= (set (vals (get-in prepared [:scope :premise-fingerprints]))) (:trusted-assumptions accepted)))
    (is (= :explicit-declaration-only (:premise-acceptance accepted)))
    (is (= :not-performed (:proof-discharge accepted)))
    (is (every? #(= :open (:discharge-state %)) (vals (:premises accepted))))
    (is (= :not-established (:provider-correspondence accepted) (:loaded-provider-identity accepted)
           (:deployment-isolation accepted) (:writer-coverage accepted) (:universal-refinement accepted)))
    (is (= :none (:authority accepted)))
    (is (= :unissued-provider-trust-scope (:reason (trust/declare! (Object.) handle declaration))))
    (is (= :unissued-provider-trust-scope (:reason (trust/declare! admission (trust/->ProviderScope) declaration))))
    (doseq [bad [(update declaration :premises pop)
                 (update declaration :premises conj (first offered))
                 (assoc declaration :acceptance :proved)
                 (assoc-in declaration [:scope :scope-ref] "foreign")
                 (assoc-in declaration [:scope :contract-scope :provider :version] "foreign")
                 (assoc-in declaration [:scope :artifact-origins] {})
                 (assoc declaration :premises (vec (vals (:premises prepared))))]]
      (is (= :incorrect-provider-premise-selection (:reason (run bad)))))
    (is (= :not-established (:status (run (assoc-in declaration [:premises 0 :proposition] [:foreign/meaning])))))
    (is (= :not-established (:status (run (assoc-in declaration [:premises 0 :dependencies] #{:foreign/dep})))))
    (with-redefs [origin/derive! (constantly (:origin-report prepared))]
      (is (= :provider-trust-boundary-replaced (:reason (run declaration)))))
    (with-redefs [xt/execute-tx (constantly nil)]
      (is (= :unqualified-open-provider-contract (:reason (run declaration)))))))
