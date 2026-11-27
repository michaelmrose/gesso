(ns gesso.assurance.reference-source-transition-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-transition-argument :as composition]
            [gesso.assurance.reference-source-write-argument :as writes]
            [gesso.assurance.reference-transition-argument :as transition]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.transition :as construction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- fixture []
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))]
    [source declarations fp cases]))

(deftest compose-original-journal-writing-alternatives
  (let [[source declarations fp cases :as args] (fixture)
        report (apply composition/derive! args)
        w (apply writes/derive-catalog! args)
        generic (transition/derive!)]
    (is (= :conditional-catalog-commit-composition (:status report)) (pr-str report))
    (is (= fp (:catalog-fingerprint report)))
    (is (= (set (keys declarations)) (set (keys (:alternatives report)))))
    (is (= (:outside-fragment w) (:outside-fragment report)))
    (is (= 4 (count (:outside-fragment report))))
    (is (= :not-established (:universal-refinement report) (:writer-coverage report)))
    (is (= :not-performed (:proof-discharge report)))
    (is (= (:remaining-conditions w) (:source-condition-expansion report)))
    (is (every? (:remaining-conditions report) (:remaining-conditions w)))
    (is (every? (:remaining-conditions report)
                (disj (:remaining-general-conditions generic) :source-derived-put-image-satisfies-original-abstract-relation)))
    (is (not (contains? (:remaining-conditions report) :source-derived-put-image-satisfies-original-abstract-relation)))
    (doseq [[id alternative] (:alternatives report)]
      (let [original (:construction (first (filter #(= id (:id %)) (get-in source [:inductive-source :catalog :operations]))))
            identity (:identity alternative)
            fact (:source-equation-fact alternative) goal (:goal alternative)]
        (is (= id (:alternative-id identity)))
        (is (= fp (:catalog-fingerprint identity)))
        (is (= (:transition-fingerprint (construction/build-guarded-replacement! original)) (:transition-fingerprint identity)))
        (is (= (:mapping-fingerprint (get-in w [:alternatives id])) (:mapping-fingerprint identity)))
        (is (= identity (second fact) (second goal)))
        (is (= [:admitted-inputs id (:input-types original)] (nth fact 2) (nth goal 2)))
        (is (= fact (:normalized-goal alternative)))
        (is (= (:mapping-fingerprint identity) (get-in fact [3 1]) (get-in fact [4 1])
               (get-in goal [3 1]) (get-in goal [4 1])))
        (is (= [:put-image identity :prepared (nth fact 2) (:case-key alternative) :suffix]
               (get-in fact [4 2]) (get-in alternative [:equalities :actual-after])))
        (is (= (:guard original) (:guard alternative)))
        (is (= (:replacements original) (:assignments alternative)))
        (is (= (:frame original) (:frame alternative)))))
    (is (= :not-established (:status (composition/derive! source declarations "foreign" cases))))
    (is (= :not-established (:status (composition/derive! source declarations fp {}))))
    (with-redefs [writes/derive-catalog! (constantly w)]
      (is (= :composition-boundary-replaced (:reason (apply composition/derive! args)))))
    (with-redefs [transition/derive! (constantly generic)]
      (is (= :composition-boundary-replaced (:reason (apply composition/derive! args)))))))

(deftest mismatched-equations-and-identity-reject
  (let [[source declarations fp cases :as args] (fixture)
        checked (apply projection/check-catalog! args)
        generic (transition/derive!)
        id (claim/alternative-id "helper-1" :accept)
        original (:construction (first (filter #(= id (:id %)) (get-in source [:inductive-source :catalog :operations]))))
        report (get-in (apply writes/derive-catalog! args) [:alternatives id])
        case-key (first (keep (fn [[k v]] (when (= id v) k)) (:case-alternatives checked)))
        run (fn [r g k]
              (try (#'composition/compose-alternative! original id checked r g k)
                   (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        address [:document :request_assignments "primary-r1"]]
    (is (= :conditional-committed-transition-derivation (:status (run report generic case-key))))
    (doseq [bad [(assoc report :alternative-id :foreign/op)
                 (assoc report :mapping-fingerprint "foreign")
                 (assoc report :guard {}) (assoc report :frame #{})
                 (assoc report :assignments {})]]
      (is (= :composition-alternative-mismatch (run bad generic case-key))))
    (is (= :incomplete-composition-equations (run (update report :equations dissoc address) generic case-key)))
    (is (= :composition-presence-coverage-mismatch (run (update-in report [:equations address] pop) generic case-key)))
    ;; Mutate both sides of the prior symbolic equation, not just one.
    (let [bad (reduce #(assoc-in %1 [:equations address 0 %2 :assignment/helper] [:literal "foreign"]) report [:expected :projected])]
      (is (= :composition-field-equation-mismatch (run bad generic case-key))))
    (is (= :composition-journal-equation-mismatch (run report generic [:foreign])))
    (is (= :composition-alternative-mismatch
           (run report (assoc-in generic [:cases :committed-abstract-transition :goal 2] :foreign-inputs) case-key)))))
