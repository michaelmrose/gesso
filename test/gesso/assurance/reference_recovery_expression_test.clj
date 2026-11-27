(ns gesso.assurance.reference-recovery-expression-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-expression :as expression]
            [gesso.assurance.proof.literal-evaluator-refinement :as refinement]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-catalog-expression-refinement-and-premise-binding
  (let [catalog (claim/catalog)
        fingerprint (:fingerprint (operation-set/check-operation-set! catalog))
        report (expression/check-catalog! catalog fingerprint)
        ids (set (keys (:invariants catalog)))
        subject {:catalog-id (:id catalog) :catalog-fingerprint fingerprint
                 :abstract-induction-fingerprint "induction"}
        abstract {:catalog-id (:id catalog) :identity {:catalog-fingerprint fingerprint}
                  :fingerprint "induction" :declared-invariant-ids ids}
        checked {:status :checked :judgment-subject subject :catalog-fingerprint fingerprint
                 :recovery-cut-ref "original-cut" :invariant-ids ids
                 :expression-correspondence {:status :matched :invariant-ids ids}
                 :catalog-expression-refinement report}
        fp "expression-premise"
        transfer {:status :bound :recovery-cut-ref "original-cut" :invariant-ids ids
                  :abstract-report-fingerprint "induction" :proof-discharge :not-performed
                  :condition-premise-fingerprints {:faithful-invariant-expression-semantics fp}
                  :required-open-premise-fingerprints #{fp}}
        inventory {:abstract {:report-fingerprint "induction"}
                   :unaccepted-premise-fingerprints #{fp}
                   :premises-by-fingerprint {fp {:id :reference-physical/invariant-expression-semantics
                                                  :family :physical :dependency-fingerprints #{}
                                                  :acceptance :not-performed :discharge-state :open}}}
        bound (expression/bind-premise checked transfer inventory abstract)]
    (is (= :conditional-catalog-refinement (:status report)) (pr-str report))
    (is (= 819 (:cases-checked report)))
    (is (= (:initial-condition catalog) (get-in report [:initial-condition :rule])))
    (is (= (:invariants catalog) (into {} (map (fn [[id r]] [id (:rule r)])) (:invariants report))))
    (is (= :conditional-premise-support (:status bound)) (pr-str bound))
    (is (= fp (:premise-fingerprint bound)))
    (is (= (:conditions report) (:remaining-conditions bound)))
    (is (= :open (:premise-state bound)))
    (is (= :not-performed (:proof-discharge bound) (:premise-acceptance bound)))
    (is (= :not-established (:universal-evaluator-correspondence bound)))
    (is (= :expression-catalog-mismatch (:reason (expression/check-catalog! catalog "foreign"))))
    (doseq [[c t i a] [[(assoc checked :recovery-cut-ref "foreign") transfer inventory abstract]
                       [(assoc-in checked [:catalog-expression-refinement :catalog-fingerprint] "foreign") transfer inventory abstract]
                       [(assoc-in checked [:catalog-expression-refinement :invariant-ids] #{}) transfer inventory abstract]
                       [(assoc-in checked [:expression-correspondence :status] :mismatch) transfer inventory abstract]
                       [(assoc-in checked [:catalog-expression-refinement :status] :not-established) transfer inventory abstract]
                       [(assoc-in checked [:catalog-expression-refinement :conditions] #{}) transfer inventory abstract]
                       [checked (assoc transfer :status :not-established) inventory abstract]
                       [checked (assoc transfer :required-open-premise-fingerprints #{}) inventory abstract]
                       [checked transfer (assoc inventory :unaccepted-premise-fingerprints #{}) abstract]
                       [checked transfer (assoc-in inventory [:premises-by-fingerprint fp :discharge-state] :discharged) abstract]
                       [checked transfer (assoc-in inventory [:premises-by-fingerprint fp :acceptance] :explicit-declaration-only) abstract]
                       [checked transfer (assoc-in inventory [:premises-by-fingerprint fp :id] :reference-physical/authoritative-projection) abstract]
                       [checked transfer inventory (assoc abstract :fingerprint "foreign")]]]
      (let [bad (expression/bind-premise c t i a)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :premise-fingerprint)))))))

(deftest original-catalog-refinement-boundaries-fail-closed
  (doseq [v [#'operation-set/check-operation-set! #'refinement/derive!]]
    (with-redefs-fn {v (constantly {})}
      #(is (= :expression-refinement-boundary-replaced
              (:reason (expression/check-catalog! nil nil)))))))
