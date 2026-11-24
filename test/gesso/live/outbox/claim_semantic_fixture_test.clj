(ns gesso.live.outbox.claim-semantic-fixture-test
  "Same-source bounded preservation and restricted conditional induction for
   the installed Claim witness. Not physical writer closure or certification."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.finite-model-oracle :as oracle]))

(defn- eq [field value] [:= [:read field] [:literal value]])
(defn- conjunction [fields]
  (into [:and] (map (fn [[field value]] (eq field value))) fields))

(def ^:private domain
  {:request/status [:open :claimed] :request/revision [0 1]
   :assignment/helper [nil "helper-1" "helper-2"]
   :journal/one [nil :accepted :rejected] :journal/two [nil :accepted :rejected]
   :membership/one [false true] :membership/two [false true]})

(def ^:private open-state
  {:request/status :open :request/revision 0 :assignment/helper nil
   :journal/one nil :journal/two nil})

(defn- winner-case [helper own other]
  [:and (conjunction {:request/status :claimed :request/revision 1
                      :assignment/helper helper own :accepted})
   [:or (eq other nil) (eq other :rejected)]])

(def ^:private ownership
  {:readers (set (keys open-state))
   :expression [:or (conjunction open-state)
                (winner-case "helper-1" :journal/one :journal/two)
                (winner-case "helper-2" :journal/two :journal/one)]})

(def ^:private revision
  {:readers #{:request/revision}
   :expression [:>= [:read :request/revision] [:literal 0]]})

(def ^:private alternatives [:accept :refuse :replay-accepted :replay-rejected])

(defn- catalog []
  (let [operations (vec (for [helper (sort (keys claim/helpers)) alternative alternatives]
                          {:id (claim/alternative-id helper alternative)
                           :construction (claim/source helper alternative)}))]
    {:id :fixture/claim-epoch
     :declared-operation-ids (set (map :id operations))
     :initial-condition {:readers (set (keys open-state))
                         :expression [:and (conjunction open-state) (:expression revision)]}
     :invariants {:claim/ownership-and-journal ownership :claim/nonnegative-revision revision}
     :operations operations}))

(defn- finite-source [c]
  {:catalog c :state-domains domain
   :input-domains (zipmap (:declared-operation-ids c) (repeat {}))
   :max-cases 4000})

(defn- alter-source [c id f]
  (update c :operations #(mapv (fn [entry] (if (= id (:id entry)) (update entry :construction f) entry)) %)))

(deftest bounded-claim-ownership-and-journal-preservation
  (let [c (catalog)
        report (oracle/check-finite-model! (finite-source c))]
    (is (= 8 (count (:declared-operation-ids c))))
    (is (= :bounded-no-counterexample (:status report)) (pr-str report))
    (is (:exhaustive? report))
    (is (= 432 (get-in report [:universe :states])))
    (is (= 3456 (get-in report [:examined :transition-cases-examined])))
    (is (= 4 (get-in report [:examined :initial-states-satisfying])))
    (is (false? (get-in report [:universe :all-type-domains-complete?])))
    (is (= :none (:authority report)))
    (is (= :not-established (:universal-induction report)))
    (is (= :not-established (:physical-correspondence report)))
    ;; Every original journal-writing alternative requires an empty own slot;
    ;; other alternatives frame it. This is a source shape check, not a theorem.
    (doseq [[helper [slot _]] claim/helpers alternative alternatives]
      (let [source (claim/source helper alternative)]
        (if (#{:accept :refuse} alternative)
          (do (is (= #{slot} (set (filter #{:journal/one :journal/two} (:writes source)))))
              (is (= [:literal (if (= :accept alternative) :accepted :rejected)]
                     (get-in source [:replacements slot])))
              (is (= :and (first (get-in source [:guard :expression]))))
              (is (some #{(eq slot nil)} (rest (get-in source [:guard :expression])))))
          (is (every? (:frame source) [:journal/one :journal/two])))))))

(deftest relational-counterexamples-are-not-hidden-by-passing-domain-invariants
  (let [c (catalog)
        accept (claim/alternative-id "helper-1" :accept)
        refuse (claim/alternative-id "helper-1" :refuse)
        mutations [(alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
                   (alter-source c accept #(assoc-in % [:replacements :journal/one] [:literal nil]))
                   (alter-source c refuse #(-> % (update :reads disj :journal/one)
                                               (update-in [:guard :readers] disj :journal/one)
                                               (update-in [:guard :expression] (fn [expr] (vec (remove #{(eq :journal/one nil)} expr))))))]]
    (doseq [mutant mutations]
      (let [r (oracle/check-finite-model! (finite-source mutant))]
        (is (= :counterexample (:status r)) (pr-str r))
        (is (= :step (get-in r [:counterexample :kind])))
        (is (= :claim/ownership-and-journal (get-in r [:counterexample :invariant-id])))))))

(deftest restricted-induction-does-not-promote-bounded-results
  (let [r (induction/check-induction! (catalog))]
    (is (= #{:claim/nonnegative-revision} (:conditionally-checked-inductive-invariant-ids r)) (pr-str (:induction-outcomes r)))
    (is (= #{:claim/ownership-and-journal} (:unsupported-inductive-invariant-ids r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :none (:authority r)))))
