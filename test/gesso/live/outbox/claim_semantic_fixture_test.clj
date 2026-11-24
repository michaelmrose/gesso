(ns gesso.live.outbox.claim-semantic-fixture-test
  "Same-source bounded preservation and restricted conditional induction for
   the installed Claim witness. Not physical writer closure or certification."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.observed-correspondence :as correspondence]
            [gesso.assurance.proof.guarded-substitution :as guarded]
            [gesso.assurance.proof.unified-preservation :as unified]
            [gesso.assurance.proof.finite-model-oracle :as oracle]))

(defn- eq [field value] [:= [:read field] [:literal value]])
(defn- conjunction [fields]
  (into [:and] (map (fn [[field value]] (eq field value))) fields))

(def ^:private domain
  {:request/status [:open :claimed] :request/revision [0 1]
   :assignment/helper [nil "helper-1" "helper-2"]
   :journal/one [nil :accepted :rejected] :journal/two [nil :accepted :rejected]
   :membership/one [false true] :membership/two [false true]})

(defn- finite-source [c]
  {:catalog c :state-domains domain
   :input-domains (zipmap (:declared-operation-ids c) (repeat {}))
   :max-cases 4000})

(defn- alter-source [c id f]
  (update c :operations #(mapv (fn [entry] (if (= id (:id entry)) (update entry :construction f) entry)) %)))

(deftest bounded-claim-ownership-and-journal-preservation
  (let [c (claim/catalog)
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
    (doseq [[helper [slot _]] claim/helpers alternative claim/alternatives]
      (let [source (claim/source helper alternative)]
        (if (#{:accept :refuse} alternative)
          (do (is (= #{slot} (set (filter #{:journal/one :journal/two} (:writes source)))))
              (is (= [:literal (if (= :accept alternative) :accepted :rejected)]
                     (get-in source [:replacements slot])))
              (is (= :and (first (get-in source [:guard :expression]))))
              (is (some #{(eq slot nil)} (rest (get-in source [:guard :expression])))))
          (is (every? (:frame source) [:journal/one :journal/two])))))))

(deftest relational-counterexamples-are-not-hidden-by-passing-domain-invariants
  (let [c (claim/catalog)
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

(deftest complete-conditional-induction-does-not-promote-to-certification
  (let [r (induction/check-induction! (claim/catalog))]
    (is (= #{:claim/nonnegative-revision :claim/ownership-and-journal} (:conditionally-checked-inductive-invariant-ids r)) (pr-str (:induction-outcomes r)))
    (is (empty? (:unsupported-inductive-invariant-ids r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-established (:physical-write-path-coverage r)))
    (is (= :none (:authority r)))))

(defn- guarded-report [c id]
  (guarded/check-guarded-preservation!
   {:construction (:construction (first (filter #(= id (:id %)) (:operations c))))
    :initial-condition (:initial-condition c) :invariants (:invariants c)}))

(deftest guarded-substitution-checks-every-claim-alternative
  (let [c (claim/catalog)]
    (doseq [id (:declared-operation-ids c)]
      (let [r (guarded-report c id)]
        (is (= #{[:preservation :claim/ownership-and-journal]}
               (:conditionally-checked-preservation-ids r)) (pr-str r))
        (is (= :not-issued (:certificate r)))
        (is (= :none (:authority r)))))
    (let [accept (claim/alternative-id "helper-1" :accept)
          refuse (claim/alternative-id "helper-1" :refuse)]
      (doseq [[id mutant]
              [[accept (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))]
               [accept (alter-source c accept #(assoc-in % [:replacements :journal/one] [:literal nil]))]
               [refuse (alter-source c refuse #(-> % (update :reads disj :journal/one)
                                                   (update-in [:guard :readers] disj :journal/one)
                                                   (update-in [:guard :expression] (fn [expr] (vec (remove #{(eq :journal/one nil)} expr))))))]]]
        (let [r (guarded-report mutant id)]
          (is (empty? (:conditionally-checked-preservation-ids r)))
          (is (= :post-branch-not-established
                 (:reason (first (filter #(= :claim/ownership-and-journal (:invariant %)) (:preservation-outcomes r)))))))))))

(deftest guarded-substitution-leaves-unsupported-cases-open
  (let [c (claim/catalog)
        accept (claim/alternative-id "helper-1" :accept)
        replay (claim/alternative-id "helper-1" :replay-accepted)
        ownership-id :claim/ownership-and-journal
        no-proof? (fn [report]
                    (= :open (:status (first (filter #(= ownership-id (:invariant %))
                                                     (:preservation-outcomes report))))))]
    (is (no-proof? (guarded-report (alter-source c accept #(assoc-in % [:replacements :request/status]
                                                                    [:read :request/status])) accept)))
    (is (no-proof? (guarded-report (alter-source c replay #(assoc % :guard {:readers #{} :expression [:literal false]}
                                                                   :reads #{})) replay)))
    (is (no-proof? (guarded-report (assoc-in c [:invariants ownership-id]
                                            {:readers #{:request/status}
                                             :expression [:not (eq :request/status :claimed)]}) replay)))
    (is (no-proof? (guarded-report (assoc-in c [:invariants ownership-id]
                                            {:readers #{:request/revision}
                                             :expression (into [:or] (map #(eq :request/revision %) (range 129)))}) replay)))))

(deftest unified-checker-selects-whole-guarded-claim-inferences
  (let [c (claim/catalog)
        id :claim/ownership-and-journal
        obligation-id [:preservation id]
        source (fn [catalog operation-id]
                 {:construction (:construction (first (filter #(= operation-id (:id %)) (:operations catalog))))
                  :initial-condition (:initial-condition catalog) :invariants (:invariants catalog)})]
    (doseq [op (:declared-operation-ids c)]
      (let [s (source c op)
            r (unified/check-unified-preservation! s)
            outcome (first (filter #(= id (:invariant %)) (:preservation-outcomes r)))]
        (is (contains? (:conditionally-checked-preservation-ids r) obligation-id))
        (is (some #{:guarded} (:supporting-families outcome)))
        (is (contains? (:open-meta-obligations r) :guarded-literal-substitution-rule-soundness))
        (is (= :none (:authority r)))))
    (let [accept (claim/alternative-id "helper-1" :accept)
          mutant (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
          r (unified/check-unified-preservation! (source mutant accept))
          induction (induction/check-induction! c)
          row (first (filter #(= id (:invariant %)) (:induction-outcomes induction)))]
      (is (contains? (:unsupported-preservation-ids r) obligation-id))
      (is (= (:declared-operation-ids c) (:preserving-operation-ids row)))
      (is (= :checked-conditional-induction (:status row)))
      (is (= :required-conjunct-disjunction-introduction (:initial-rule row)))
      (is (= :not-issued (:certificate induction))))))

(deftest claim-induction-refuses-bad-initial-state-or-transition
  (let [c (claim/catalog)
        id :claim/ownership-and-journal
        bad-init (assoc-in c [:initial-condition :expression]
                           [:and (conjunction (assoc claim/open-state :assignment/helper "helper-1"))
                            (:expression claim/revision)])
        accept (claim/alternative-id "helper-1" :accept)
        bad-step (alter-source c accept #(assoc-in % [:replacements :assignment/helper] [:literal "helper-2"]))
        row (fn [r] (first (filter #(= id (:invariant %)) (:induction-outcomes r))))
        init-result (row (induction/check-induction! bad-init))
        step-result (row (induction/check-induction! bad-step))]
    (is (= :initial-consequence-open (:reason init-result)))
    (is (= (:declared-operation-ids c) (:preserving-operation-ids init-result)))
    (is (= :checked-conditional-inference (:initial-status step-result)))
    (is (= :some-preservation-open (:reason step-result)))
    (is (= #{accept} (:unsupported-operation-ids step-result)))))

(deftest declared-observation-report-rechecks-exact-sources-and-divergence
  (let [before {:request/status :open :request/revision 0 :assignment/helper nil
                :journal/one nil :journal/two nil :membership/one true :membership/two true}
        after (assoc before :request/status :claimed :request/revision 1
                     :assignment/helper "helper-1" :journal/one :accepted)
        step {:construction (claim/source "helper-1" :accept) :before before :inputs {} :after after}
        id (claim/alternative-id "helper-1" :accept)
        source {:catalog (claim/catalog) :observations [step (assoc-in step [:after :membership/two] false)]}
        r (correspondence/check-declared-observations! source)
        reason (fn [s] (try (correspondence/check-declared-observations! s) nil
                            (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= #{id} (:observed-operation-ids r)))
    (is (= #{id} (:divergent-operation-ids r)))
    (is (= 7 (count (:unobserved-operation-ids r))))
    (is (= [true false] (mapv #(get-in % [:checks :matches?]) (:observations r))))
    (is (false? (:all-supplied-observations-match? r)))
    (is (= :caller-supplied-unauthenticated (:physical-observation-provenance r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:transaction-authority r)))
    (is (= :observation-not-bound-to-declared-construction
           (reason (assoc source :observations [(assoc-in step [:construction :replacements :assignment/helper]
                                                          [:literal "helper-2"])]))))
    (is (= :invalid-source-shape (reason (assoc source :certificate :issued))))
    (is (= :invalid-observation-selection (reason (assoc source :observations []))))))
