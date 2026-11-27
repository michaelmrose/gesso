(ns gesso.assurance.reference-xtdb-artifact-origin-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-xtdb-artifact-origin :as origin]
            [gesso.assurance.reference-xtdb-provider-contract :as contract]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.inductive-plan :as induction]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [xtdb.api :as xt]))

(deftest selected-local-artifact-origin-and-open-contract
  (let [source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        description (induction/describe-mapped-admission admission)
        declarations (claim/outcome-declarations)
        cases (:cases (outcome/derive-cases! source declarations))
        report (origin/derive! admission source declarations (:catalog-fingerprint description) cases)]
    (is (= :open-contract-with-selected-artifact-origins (:status report)) (pr-str (dissoc report :contract)))
    (is (= (:artifacts contract/reviewed-provider)
           (into {} (map (fn [[id row]] [id (:sha256 row)])) (:artifacts report))))
    (is (= 8 (count (:open-premise-fingerprints report))))
    (is (every? #(= :open (:discharge-state %)) (vals (get-in report [:contract :premises]))))
    (is (= :not-established (:provider-correspondence report) (:loaded-provider-identity report)))
    (is (= :not-performed (:proof-discharge report) (:premise-acceptance report)))
    (is (contains? (:unverified report) :generated-clojure-function-bytecode))
    (is (= :not-established (:status (origin/derive! (Object.) source declarations (:catalog-fingerprint description) cases))))))

(deftest origin-rejects-replaced-boundaries-and-inconsistent-markers
  (let [reason (fn [f] (try (f) (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        url (first (enumeration-seq (.getResources (clojure.lang.RT/baseLoader) "xtdb/api.clj")))
        f (#'origin/unique-resource! [url])]
    (is (= :ambiguous-provider-resource (reason #(#'origin/unique-resource! []))))
    (is (= :ambiguous-provider-resource (reason #(#'origin/unique-resource! [url url]))))
    (is (= :split-provider-artifact-origin (reason #(#'origin/check-artifact! "foreign" [f (java.io.File. "other.jar")]))))
    (is (= :provider-artifact-digest-mismatch (reason #(#'origin/check-artifact! "foreign" [f]))))
    (with-redefs [xt/execute-tx (constantly nil)]
      (is (= :provider-origin-boundary-replaced (:reason (origin/observe!)))))
    (with-redefs [origin/observe! (constantly {:status :matched-selected-local-artifact-origins})]
      (is (= :provider-origin-boundary-replaced (:reason (origin/derive! nil nil nil nil nil)))))
    (with-redefs [contract/derive! (constantly {})]
      (is (= :provider-origin-boundary-replaced (:reason (origin/observe!)))))))
