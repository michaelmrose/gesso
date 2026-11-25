(ns gesso.assurance.ansatz-mapped-prestate-cardinality-pilot
  "Opt-in cardinality lemma for the v2553 decoded-row assertion rules.
   This proves a list-partition lemma at the actual source-derived map sizes.
   A faithful partition into matched field entries and residual entries is a
   premise, NOT a theorem about Clojure maps, SQL structs, or provider reads.
   Run with clojure -M:ansatz:typed:test -m
   gesso.assurance.ansatz-mapped-prestate-cardinality-pilot."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests]]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.proof.mapped-prestate-rules :as rules]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.mapped-projection-test]))

(defn- proof-pilot! [f & args]
  (apply @(ns-resolve 'gesso.assurance.ansatz-reservation-pilot f) args))

(defn- obligation [n sites]
  {:obligation-kind :encoded-map-residual-exclusion
   :expected-cardinality n :sites sites
   :parameters ['known ':- '(List Int) 'actual ':- '(List Int) 'extra ':- '(List Int)
                'hk ':- (list '= 'Nat '(List.length known) n)
                'ha ':- (list '= 'Nat '(List.length actual) n)
                'hp ':- '(= (List Int) actual (List.append known extra))]
   :goal-form '(= (List Int) actual known)
   :proof-script '[(rw hp at ha) (rw List.length_append at ha)
                   (have hz (= Nat (List.length extra) 0)) (omega)
                   (have he (= (List Int) extra (List.nil Int))
                         (List.eq_nil_of_length_eq_zero hz))
                   (rw hp) (rw he) (simp)]})

(defn derive-obligations!
  "Recheck the actual prefix and derive one lemma per distinct map size,
   retaining every target/path at that size. Empty map and scalar equality
   rules are not cardinality obligations. The source rules remain authority."
  [original targets docs journal prefix]
  (let [description (rules/derive! original targets docs journal prefix)
        sites (vec (for [{:keys [target rules]} (:groups description)
                         [kind path n] rules :when (= :map-size kind)]
                     {:target target :path path :cardinality n}))]
    {:rule-description description
     :sites sites
     :obligations (mapv (fn [[n group]] (obligation n (vec group)))
                        (sort-by key (group-by :cardinality sites)))}))

(defn check!
  "Kernel-check the derived list lemmas and apply the existing proof dependency
   policy. List order, unique field encoding, matching child values, and the
   partition of an actual map into known and residual entries remain premises."
  [original targets docs journal prefix]
  (let [{:keys [rule-description sites obligations]} (derive-obligations! original targets docs journal prefix)
        checked (mapv #(proof-pilot! 'check-obligation! %) obligations)]
    {:type ::cardinality-pilot :rule-description rule-description
     :sites sites :checked checked
     :scope :source-derived-sizes-of-encoded-list-partitions
     :proof-status (if (seq checked) :kernel-checked-conditional-cardinality :no-cardinality-obligations)
     :open-obligations (into (:open-obligations rule-description)
                             #{:decoded-map-to-unique-entry-partition
                               :faithful-field-entry-encoding
                               :recursive-child-equality-correspondence
                               :general-structural-soundness})
     :sql-refinement :not-established :authority :none :certificate :not-issued}))

(defn- fixture! []
  ;; Reuse the exact original mapped-projection fixture; do not author a
  ;; separate example merely for the proof translator.
  (let [lookup #(deref (ns-resolve 'gesso.model.mapped-projection-test %))
        original (lookup 'source) targets (lookup 'targets) docs (lookup 'rows)
        journal [((lookup 'record) "old" "p2" {:nested {:value "original" :flags [true false] :empty {}}})]
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)]
    [original targets docs journal prefix]))

(deftest cardinality-lemmas-cover-source-map-sites-under-explicit-premises
  (let [[original targets docs journal prefix :as args] (fixture!)
        {:keys [sites obligations rule-description]} (apply derive-obligations! args)
        report (apply check! args)
        checked (:checked report)
        site-set (set (for [{:keys [target rules]} (:groups rule-description)
                            [kind path n] rules :when (= :map-size kind)]
                        {:target target :path path :cardinality n}))]
    (is (seq sites))
    (is (= site-set (set sites)))
    (is (= (frequencies sites) (frequencies (mapcat :sites obligations))))
    (is (= (set (map :cardinality sites)) (set (map :expected-cardinality obligations))))
    (is (= (count obligations) (count checked)))
    (is (= :kernel-checked-conditional-cardinality (:proof-status report)))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:sql-refinement report)))
    (is (contains? (:open-obligations report) :decoded-map-to-unique-entry-partition))
    (is (contains? (:open-obligations report) :general-structural-soundness))
    (doseq [[ob {:keys [goal proof dependencies description]}] (map vector obligations checked)]
      (is (= (:sites ob) (:sites description)))
      (is (= (proof-pilot! 'elaborate-source-proposition! ob) goal))
      (is (kernel/verifies? (ansatz/env) goal proof))
      (is (empty? (:non-safe dependencies)))
      (is (empty? (:unresolved dependencies)))
      (is (= :ansatz-reservation-pilot/lean-core-axioms-v1 (:policy dependencies)))
      ;; A proof with the partition premise cannot be transplanted to the
      ;; stronger, generally false proposition with that premise removed.
      (let [without-partition (update ob :parameters #(subvec % 0 (- (count %) 3)))]
        (is (false? (kernel/verifies? (ansatz/env)
                                      (proof-pilot! 'elaborate-source-proposition! without-partition) proof)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (derive-obligations! original targets docs journal (pop prefix))))
    (let [changed (assoc-in docs [:requests 0 :additional] true)
          changed-prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) changed journal)
          changed-sites (:sites (derive-obligations! original targets changed journal changed-prefix))]
      (is (not= sites changed-sites))
      (is (some #{ {:target [:document :requests "r1"] :path [] :cardinality 4}} changed-sites)))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-mapped-prestate-cardinality-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
