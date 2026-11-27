(ns gesso.assurance.reference-recovery-invariant-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-mapped-poststate :as poststate])
  (:import [java.time Instant]))

(def ^:private types {:fixture/value {:type :integer}})
(def ^:private coordinates {:fixture/value [:document :fixture_rows "row" :value]})
(defn- construction [id before after]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types types :input-types {}
   :guard {:readers #{:fixture/value} :expression [:= [:read :fixture/value] [:literal before]]}
   :reads #{:fixture/value} :writes #{:fixture/value} :frame #{}
   :replacements {:fixture/value [:literal after]}})
(def ^:private accept (construction :fixture/accept 0 1))
(def ^:private refuse (construction :fixture/refuse 1 2))
(def ^:private catalog
  {:id :fixture/catalog
   :initial-condition {:readers #{:fixture/value} :expression [:= [:read :fixture/value] [:literal 0]]}
   :invariants {:fixture/nonnegative {:readers #{:fixture/value}
                                      :expression [:>= [:read :fixture/value] [:literal 0]]}}
   :operations [{:id :fixture/accept :construction accept}
                {:id :fixture/refuse :construction refuse}]})
(defn- t [i] (.plusSeconds (Instant/parse "2026-11-26T20:00:00Z") i))
(defn- put-value [v] [:put-docs :fixture_rows {:xt/id "row" :value v}])
(def ^:private inputs
  (mapv (fn [i ops] {:tx-id i :system-time (t i) :ops ops}) (range)
        [[[:put-docs :manifest {:xt/id "root" :value 1}]]
         [(put-value 0)] [(put-value 1)] [[:put-docs :maintenance {:xt/id "worker" :value 1}]]
         [(put-value 9) ["ASSERT FALSE"]] [(put-value 2)] [(put-value -1)]]))
(def ^:private comparison
  {:manifest-tx-id 0 :initialization-tx-id 1 :selected-tx-ids [2 4 5]
   :selected-commitments [:committed :aborted :committed]
   :intervening-maintenance [{:tx-id 3}]})
(def ^:private steps
  (mapv (fn [id op] {:expectation (nth inputs id) :original op
                     :spec {:alternative (get-in op [:subject :id]) :inputs {}}})
        [2 4 5] [accept accept refuse]))

(deftest finite-selected-invariant-boundaries-are-actually-checked
  (let [audit #'invariant/audit-replay!
        run #(audit %1 coordinates {} {:fixture/value 0} %2 comparison %3)
        r (run catalog inputs steps)]
    (is (= :checked (:status r)) (pr-str r))
    (is (= :declared-invariants-hold-on-selected-replay-projections (:conclusion r)))
    (is (= [1 2 3 4 5] (mapv :tx-id (:checked-boundaries r))))
    (is (= [:checked-initial-projection :committed-abstract-transition :maintenance-stutter
            :aborted-stutter :committed-abstract-transition] (mapv :relation (:checked-boundaries r))))
    (is (= #{:fixture/nonnegative} (:invariant-ids r)))
    (is (= :not-established (:physical-invariant-theorem r) (:universal-refinement r)))
    (is (= r (run catalog (pop inputs) steps))) ;; suffix deliberately excluded
    (let [bad-refuse (construction :fixture/refuse 1 -1)
          bad (run (assoc-in catalog [:operations 1 :construction] bad-refuse)
                   (assoc-in inputs [5 :ops] [(put-value -1)])
                   (assoc-in steps [2 :original] bad-refuse))]
      (is (= :selected-projection-invariant-violation (:reason bad)))
      (is (not (contains? bad :conclusion))))
    (doseq [[changed reason] [[(assoc-in inputs [1 :ops] [(put-value 1)]) :initial-projection-invariant-mismatch]
                              [(assoc-in inputs [3 :ops] [(put-value 2)]) :selected-invariant-transition-mismatch]
                              [(assoc-in inputs [4 :ops] [(put-value 1)]) :selected-invariant-transition-mismatch]
                              [(assoc-in inputs [5 :ops] [(put-value 3)]) :selected-invariant-transition-mismatch]
                              [(vec (remove #(= 3 (:tx-id %)) inputs)) :invalid-selected-invariant-sequence]
                              [(assoc-in inputs [2 :ops]
                                         [(put-value 1)
                                          [:put-docs {:into :fixture_rows :valid-from (t 3)} {:xt/id "row" :value 3}]])
                               :selected-projection-continuity-mismatch]]]
      (let [bad (run catalog changed steps)]
        (is (= reason (:reason bad)) (pr-str bad))
        (is (not (contains? bad :conclusion)))))
    (with-redefs [poststate/project-visible! (constantly {:fixture/value 0})]
      (is (not (invariant/boundaries-current?)))
      (is (= :invariant-boundary-replaced
             (:reason (invariant/audit-issued-selection! nil nil nil nil nil nil nil nil)))))))

(defn check-judgment! [result scope original]
  (let [checked (:selected-invariant-evidence result)
        ids (:selected-tx-ids original)
        initial (:initialization-tx-id original)
        maintenance (mapv :tx-id (:intervening-maintenance original))
        boundaries (:checked-boundaries checked)]
    (is (= invariant/profile (:profile checked)))
    (is (= :checked (:status checked)) (pr-str checked))
    (is (= :declared-invariants-hold-on-selected-replay-projections (:conclusion checked)))
    (is (= :original-selected-transaction-boundaries-only (:scope checked)))
    (is (= :finite-direct-evaluation-not-inductive-theorem (:derivation checked)))
    (is (= :transaction-system-and-valid-time (:poststate-coordinate checked)))
    (is (= :preceding-system-current-transaction-valid-time (:prestate-coordinate checked)))
    (is (= (:declared-invariant-ids (:abstract-induction result)) (:invariant-ids checked)))
    (is (= (:judgment-subject result) (:judgment-subject checked)))
    (is (= (:catalog-fingerprint (:judgment-subject result)) (:catalog-fingerprint checked)))
    (is (= (:historical-cut scope) (:historical-cut checked)))
    (is (= (get-in scope [:historical-cut :payload-fingerprint]) (:retained-payload-fingerprint checked)))
    (is (= (get-in result [:command-authentication-premises :scope :command-observation-ref]) (:recovery-cut-ref checked)))
    (is (= ids (:selected-tx-ids checked)))
    (is (= (into [initial] (sort (into ids maintenance))) (mapv :tx-id boundaries)))
    (is (= :checked-initial-projection (:relation (first boundaries))))
    (is (= [(second ids)] (mapv :tx-id (filter #(= :aborted-stutter (:relation %)) boundaries))))
    (is (= maintenance (mapv :tx-id (filter #(= :maintenance-stutter (:relation %)) boundaries))))
    (is (= [(first ids) (peek ids)] (mapv :tx-id (filter #(= :committed-abstract-transition (:relation %)) boundaries))))
    (is (= :not-established (:physical-invariant-theorem checked) (:universal-refinement checked)))
    (is (= :not-performed (:proof-discharge checked)))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))))
