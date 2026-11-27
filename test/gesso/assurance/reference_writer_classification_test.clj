(ns gesso.assurance.reference-writer-classification-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.set :as set]
            [gesso.assurance.reference-writer-classification :as classification]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-transition-argument :as transition]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- fixture []
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)]
    [source declarations (:fingerprint (catalog/check-operation-set! original))
     (:cases (outcome/derive-cases! source declarations))]))

(deftest declared-partition-and-reviewed-policy
  (let [args (fixture) report (apply classification/derive! args)
        w (apply commit/derive! args) r (apply replay/derive! args)
        wi (:journal-writing-alternatives report) ri (:read-only-alternatives report)
        branches (:reviewed-policy-branches report)]
    (is (= :conditional-declared-writer-classification (:status report)) (pr-str report))
    (is (= :complete (:declared-alternative-coverage report)))
    (is (= (:declared-operation-ids (claim/catalog)) (set/union wi ri)))
    (is (= #{} (set/intersection wi ri)))
    (is (= 4 (count wi) (count ri)))
    (is (= (set (keys (:alternatives w))) wi))
    (is (= (set (keys (:alternatives r))) ri))
    (is (= #{:requests :request_assignments :memberships :gesso_commands} (:protected-tables report)))
    (is (= #{:mapped-operation :initializer :manifest :lease-acquisition :lease-reclaim :dispatch-acknowledgment} (set (keys branches))))
    (is (= :initial-root (get-in branches [:initializer :effect])))
    (is (= :infrastructure-establishment (get-in branches [:manifest :effect])))
    (doseq [k [:lease-acquisition :lease-reclaim :dispatch-acknowledgment]]
      (is (= :conditional-mapped-stutter (get-in branches [k :effect])))
      (is (= :current-activated-generation (get-in branches [k :activation])))
      (is (= :matched-scheduled-maintenance (get-in branches [k :installed-origin])))
      (is (= :current-managed-lifecycle (get-in branches [k :installed-lineage])))
      (is (= #{} (set/intersection (:protected-tables report) (get-in branches [k :write-tables])))))
    (is (= :conditional-stutter (get-in report [:physical-step-classes :aborted-attempt])))
    (is (= #{:read-only-replay :locally-refused-submission} (:nontransaction-observations report)))
    (is (= :not-established (:writer-coverage report) (:universal-refinement report)))
    (is (= :not-performed (:proof-discharge report)))
    (is (= :none (:authority report)))
    (is (every? (:remaining-conditions report) (set/union (:remaining-conditions w) (:remaining-conditions r))))
    (is (contains? (:remaining-conditions report) :all-physical-writers-mediated-by-exact-installed-policy))
    (is (contains? (:remaining-conditions report) :complete-admitted-writer-and-temporal-boundary-coverage))))

(deftest reject-incomplete-or-inconsistent-classification
  (let [args (fixture) p (apply projection/check-catalog! args)
        w (apply commit/derive! args) r (apply replay/derive! args) t (transition/derive!)
        id (first (keys (:alternatives r))) wid (first (keys (:alternatives w)))
        run (fn [p w r] (try (#'classification/classify! p w r t)
                             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :incomplete-declared-alternative-partition (run p w (update r :alternatives dissoc id))))
    (is (= :incomplete-declared-alternative-partition (run p w (assoc-in r [:alternatives wid] (get-in w [:alternatives wid])))))
    (is (= :incomplete-declared-alternative-partition (run p w (assoc r :catalog-fingerprint "foreign"))))
    (is (= :classification-alternative-identity-mismatch (run p w (assoc-in r [:alternatives id :origin :mapping-fingerprint] "foreign"))))
    (is (= :classification-alternative-identity-mismatch (run p w (assoc-in r [:alternatives id :writes] #{:request/status}))))
    (is (= :classification-alternative-identity-mismatch (run p (assoc-in w [:alternatives wid :case-key] [:foreign]) r)))
    (is (= :classification-alternative-identity-mismatch (run (assoc-in p [:operation-mappings id :frame-coordinates] {}) w r)))
    (is (thrown? clojure.lang.ExceptionInfo (#'classification/policy-branches! #{:gesso_live_outbox})))
    (with-redefs [replay/derive! (constantly r)]
      (is (= :classification-boundary-replaced (:reason (apply classification/derive! args)))))
    (with-redefs [classification/source-digest (constantly "foreign")]
      (is (= :unreviewed-writer-policy-source (:reason (apply classification/derive! args)))))))
