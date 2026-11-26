(ns gesso.assurance.reference-mapped-outcome-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.model.command-resolution :as resolution]))

(def accepted fixture/source)
(def refused (-> accepted
                 (assoc :subject {:kind :operation :id :example/refuse :interface {:operation :example/refuse}})
                 (assoc-in [:replacements :example/outcome] [:literal :refused])))
(def replay (-> accepted
                (assoc :subject {:kind :operation :id :example/replay :interface {:operation :example/replay}}
                       :input-types {} :writes #{} :replacements {} :reads #{:example/owner}
                       :frame (set (keys (:state-types accepted))))))
(def source {:inductive-source {:catalog {:operations (mapv (fn [c] {:id (get-in c [:subject :id]) :construction c})
                                                             [accepted refused replay])}}
             :target-map fixture/targets
             :decode-outcome! (fn [& _] (throw (ex-info "Must not invoke opaque decoder" {})))})
(def declarations {:example/claim {:arguments {:request-id "r1"} :outcome {:status :ok}}
                   :example/refuse {:arguments {:request-id "r1"} :outcome {:status :denied}}})
(def cases {["helper-a" :example/claim {:request-id "r1"} {:status :ok}] :claimed
            ["helper-a" :example/claim {:request-id "r1"} {:status :denied}] :refused})
(def spec {:alternative :example/claim :binding fixture/command-binding :outcome {:status :ok} :now "t1" :outcome-cases cases})
(defn candidate [s] (fixture/tx 1 [[:put-docs :gesso_commands (resolution/command-record (:binding s) (:outcome s) (:now s))]]))
(defn check! [s d original targets tx sp] (outcome/compare-selected! s d original targets tx sp))
(def args [source declarations accepted fixture/targets (candidate spec) spec])

(deftest derives-abstract-values-and-binds-the-selected-alternative
  (let [r (apply check! args)]
    (is (= cases (:cases (outcome/derive-cases! source declarations))))
    (is (= :matched (:status r)) (pr-str r))
    (is (= :matched-declared-journal-assignment (:outcome-derivation r)))
    (is (= :not-established (:outcome-provenance r)))
    (is (= :not-established (:source-provenance r)))
    (is (= :not-established (:provider-refinement r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (let [s (assoc spec :alternative :example/refuse :outcome {:status :denied})]
      (is (= :matched (:status (check! source declarations refused fixture/targets (candidate s) s))))
      (is (= :selected-concrete-outcome-mismatch (:reason (check! source declarations accepted fixture/targets (candidate s) (assoc s :alternative :example/claim))))))))

(deftest rejects-incomplete-ambiguous-or-substituted-interpretations
  (doseq [[a reason] [[(update args 1 dissoc :example/refuse) :incomplete-outcome-declarations]
                      [(assoc-in args [1 :example/replay] (:example/claim declarations)) :incomplete-outcome-declarations]
                      [(assoc-in args [1 :example/refuse] (:example/claim declarations)) :ambiguous-concrete-outcome-case]
                      [(assoc-in args [5 :outcome-cases] (dissoc cases (first (keys cases)))) :selected-outcome-cases-mismatch]
                      [(assoc-in args [5 :outcome-cases] (update-vals cases (constantly :claimed))) :selected-outcome-cases-mismatch]
                      [(assoc-in args [5 :outcome] {:status :denied}) :selected-outcome-record-mismatch]
                      [(assoc-in args [5 :alternative] :example/refuse) :selected-outcome-alternative-mismatch]
                      [(assoc args 3 {}) :selected-outcome-alternative-mismatch]
                      [(update-in args [4 :ops] conj (first (:ops (candidate spec)))) :invalid-selected-outcome-record]]]
    (let [r (apply check! a)]
      (is (= reason (:reason r)) (pr-str r))
      (is (not (contains? r :outcome-derivation))))))
