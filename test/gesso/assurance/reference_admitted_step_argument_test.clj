(ns gesso.assurance.reference-admitted-step-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-admitted-step-argument :as admitted]
            [gesso.assurance.reference-writer-classification :as classification]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-transition-argument :as generic]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- fixture []
  (let [original (claim/catalog)
        source {:inductive-source {:catalog original} :target-map claim/target-map}
        declarations (claim/outcome-declarations)]
    [source declarations (:fingerprint (catalog/check-operation-set! original))
     (:cases (outcome/derive-cases! source declarations))]))

(deftest general-conditional-schema
  (let [args (fixture) report (apply admitted/derive! args)
        schemas (:schemas report) c (apply classification/derive! args)
        split (:case-split report)]
    (is (= :conditional-general-admitted-step-derivation (:status report)) (pr-str report))
    (is (= 6 (count schemas)))
    (is (= (:journal-writing-alternatives c) (set (keys (get-in schemas [::admitted/committed :alternatives])))))
    (is (= (:read-only-alternatives c) (set (keys (get-in schemas [::admitted/replay :alternatives])))))
    (is (= :single-qualified-read-cut (get-in schemas [::admitted/replay :scope])))
    (is (= :separate-obligation (get-in report [:quantification :initial-root])))
    (is (= :entails (get-in split [:entailment :result])))
    (is (= 128 (get-in split [:entailment :valuations-checked])))
    (is (= 63 (get-in split [:entailment :premise-satisfying-valuations])))
    (is (= :assumed-not-derived (:coverage split)))
    (is (:every-premise-necessary? split))
    (is (= :not-established (:universal-refinement report) (:writer-coverage report) (:temporal-coverage report)))
    (is (= :not-performed (:proof-discharge report)))
    (is (every? (:remaining-conditions report) (:remaining-conditions c)))
    (is (every? (:remaining-conditions report) (:coverage-conditions report)))
    (is (contains? (:remaining-conditions report) :no-local-refusal-attributed-mutations-or-submissions))
    (is (contains? (:remaining-conditions report) :protected-image-constant-across-time-advance))
    (is (= :not-established (:status (apply admitted/derive! (assoc args 2 "foreign")))))
    (with-redefs [classification/derive! (constantly c)]
      (is (= :admitted-step-boundary-replaced (:reason (apply admitted/derive! args)))))))

(deftest reject-incomplete-general-schemas
  (let [args (fixture) c (apply classification/derive! args)
        w (apply commit/derive! args) r (apply replay/derive! args) g (generic/derive!)
        id (first (keys (:alternatives r)))
        run (fn [c w r g] (try (#'admitted/compose! c w r g)
                               (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :admitted-step-schema-mismatch (run c w (update r :alternatives dissoc id) g)))
    (is (= :admitted-step-schema-mismatch (run (update c :reviewed-policy-branches dissoc :lease-reclaim) w r g)))
    (is (= :admitted-step-schema-mismatch (run (assoc-in c [:reviewed-policy-branches :initializer :effect] :conditional-mapped-stutter) w r g)))
    (is (= :admitted-step-schema-mismatch (run c w r (update g :cases dissoc :aborted-stutter))))
    (is (= :admitted-step-schema-mismatch (run c w (assoc-in r [:alternatives id :identity :mapping-fingerprint] "foreign") g)))))
