(ns gesso.assurance.proof.kernel-acceptance-rejection-correspondence-test
  "v1426: independent bounded cross-route regression for the closed Gesso
   propositional kernel. Reconstruct named open-premise dependencies from raw
   source step references without reading checked/audit/induction reports.
   Require direct checking, source correspondence, and compositional induction
   to agree on the precise still-OPEN assumptions. Malformed original sources,
   cached reports, and alleged authority must fail at every entry point.

   These finite source fixtures do not prove universal kernel soundness,
   normalization semantics, or downstream certificate/physical correctness."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.proof.propositional-kernel-correspondence :as correspondence]
            [gesso.assurance.proof.propositional-derivation-soundness :as induction]))

(def ^:private subject
  {:kind :operation :id :example/kernel-provenance
   :interface {:operation :example/kernel-provenance}})

(defn- atom-form [reader]
  [:atom {:readers #{reader}
          :expression [:> [:read reader] [:literal 0]]}])

(def ^:private p (atom-form :result/p))
(def ^:private q (atom-form :result/q))
(def ^:private r (atom-form :result/r))
(def ^:private all-types
  {:result/p {:type :integer}
   :result/q {:type :integer}
   :result/r {:type :integer}})

(defn- readers [value]
  (cond
    (and (vector? value) (= :atom (first value))) (:readers (second value))
    (map? value) (reduce set/union #{} (map readers (vals value)))
    (coll? value) (reduce set/union #{} (map readers value))
    :else #{}))

(defn- source [premises steps goal]
  {:subject subject
   :variables (select-keys all-types (readers [premises steps goal]))
   :premises premises :steps steps :goal goal})

(defn- s [id rule conclusion & args]
  (merge {:id id :rule rule :conclusion conclusion} (apply hash-map args)))

(defn- prem [step-id premise-id formula]
  (s step-id :premise formula :premise premise-id))

(defn- seven-rule-source [slot]
  (let [pq [:and p q]
        offer (into [:or] (assoc [[:false] [:false] [:false]] slot r))
        reflex [:implies p p]
        compound [:and offer [:true] reflex]]
    (source
     {:premise/p p :premise/q q
      :premise/conditional [:implies pq r]}
     [(prem :step/p :premise/p p)
      (prem :step/q :premise/q q)
      (s :step/join :conjunction-introduction pq
         :from [:step/p :step/q])
      (prem :step/conditional :premise/conditional [:implies pq r])
      (s :step/resolve :implication-elimination r
         :from [:step/conditional :step/join])
      (s :step/disj :disjunction-introduction offer
         :from [:step/resolve] :index slot)
      (s :step/truth :truth-introduction [:true])
      (s :step/reflex :implication-reflexivity reflex)
      (s :step/compound :conjunction-introduction compound
         :from [:step/disj :step/truth :step/reflex])
      (s :step/final :conjunction-elimination offer
         :from [:step/compound] :index 0)]
     offer)))

(defn- repeated-premise-source []
  (source {:premise/only p}
          [(prem :step/a :premise/only p)
           (s :step/twice :conjunction-introduction [:and p p]
              :from [:step/a :step/a])]
          [:and p p]))

(defn- separate-aliases-source [left right]
  (source {left p right p}
          [(prem :step/left left p)
           (prem :step/right right p)
           (s :step/both :conjunction-introduction [:and p p]
              :from [:step/left :step/right])]
          [:and p p]))

(defn- false-premise-source []
  (source {:premise/false [:false]}
          [(prem :step/false :premise/false [:false])]
          [:false]))

(defn- independent-dependencies
  "A test-local source-reference walk. Never consults checker/audit results,
   formal rule obligations, semantics interpreters, or the kernel's private
   dependency helpers. Only call on the intentionally well-formed fixtures."
  [src]
  (let [steps (:steps src)
        trace
        (reduce (fn [known step]
                  (let [inherited (case (:rule step)
                                    :premise #{(:premise step)}
                                    (:truth-introduction :implication-reflexivity) #{}
                                    (reduce set/union #{}
                                            (map (fn [id]
                                                   (get-in known [id :open]))
                                                 (:from step))))]
                    (assoc known (:id step)
                           {:open inherited
                            :parents (vec (:from step))})))
                {} steps)
        last-id (:id (last steps))
        ;; This reachability pass walks source references rather than the
        ;; inherited-premise ledger. Dead steps must not hide behind a
        ;; coincidentally correct final premise set.
        reachable
        (loop [todo [last-id] visited #{}]
          (if (empty? todo)
            visited
            (let [id (peek todo)]
              (if (contains? visited id)
                (recur (pop todo) visited)
                (recur (into (pop todo) (get-in trace [id :parents]))
                       (conj visited id))))))]
    {:per-step (into {} (map (fn [[id info]] [id (:open info)]) trace))
     :final-open (get-in trace [last-id :open])
     :reachable reachable
     :all-step-ids (set (map :id steps))
     :all-premise-ids (set (keys (:premises src)))}))

(defn- assert-no-authority! [report]
  (is (= :none (:authority report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority))))

(defn- assert-correspondence! [src]
  (let [independent (independent-dependencies src)
        checked (kernel/check-derivation! src)
        audit (correspondence/audit-derivation! src)
        proof (induction/check-induction! src)
        per-step (:per-step independent)
        step-ids (mapv :id (:steps src))
        expected-open (:final-open independent)]
    (is (= (:all-step-ids independent) (:reachable independent))
        "All test-fixture steps should be referenced by the final step")
    (is (= (:all-premise-ids independent) expected-open)
        "The deliberately complete source must retain every named premise")
    (is (= expected-open (:open-premises checked)))
    (is (= expected-open (:kernel-open-premises audit)))
    (is (= expected-open (:open-premises proof)))
    (is (= :exact-match (:open-premise-correspondence audit)))
    (is (= :all-step-shapes-match (:source-rule-correspondence audit)))
    (is (= (count step-ids) (:rule-count checked)))
    (is (= (count step-ids) (:checked-step-count audit)))
    (is (= (count step-ids) (:checked-step-count proof)))
    (is (= step-ids (mapv :id (:step-reports audit))))
    (is (= step-ids (mapv :id (:step-induction proof))))
    (doseq [step-id step-ids]
      (is (= (get per-step step-id)
             (:premises (first (filter #(= step-id (:id %))
                                       (:step-reports audit)))))
          (str "Source correspondence lost open premises for " step-id))
      (is (= (get per-step step-id)
             (:premises (first (filter #(= step-id (:id %))
                                       (:step-induction proof)))))
          (str "Induction lost open premises for " step-id)))
    (is (= (if (empty? expected-open)
             :checked-without-premises
             :conditional-on-open-premises)
           (:conclusion-status checked)))
    (is (= :gesso.assurance.proof.kernel/unadmitted-checked-derivation
           (:type checked)))
    (doseq [report [checked audit proof]] (assert-no-authority! report))
    {:kernel checked :audit audit :induction proof
     :independent independent}))

(defn- invalid-type [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- rejects-on-all-routes! [candidate]
  (doseq [[label check]
          [[:kernel kernel/check-derivation!]
           [:correspondence correspondence/audit-derivation!]
           [:induction induction/check-induction!]]]
    (testing (name label)
      (is (= :gesso.assurance.proof.kernel/invalid-derivation
             (invalid-type #(check candidate)))))))

(deftest seven-rules-preserve-exact-open-premises-in-all-three-paths
  (doseq [slot (range 3)]
    (testing (str "disjunction child " slot)
      (let [{:keys [independent audit induction]}
            (assert-correspondence! (seven-rule-source slot))]
        (is (= #{:premise/p :premise/q :premise/conditional}
               (:final-open independent)))
        (is (= #{} (get-in independent [:per-step :step/truth])))
        (is (= #{} (get-in independent [:per-step :step/reflex])))
        (is (= #{:premise/p :premise/q}
               (get-in independent [:per-step :step/join])))
        (is (= :complete-for-supplied-source-instances (:status audit)))
        (is (= :conditional-source-derivation-soundness
               (:status induction)))))))

(deftest duplicate-source-edges-do-not-multiply-open-assumptions
  (let [{:keys [independent audit]}
        (assert-correspondence! (repeated-premise-source))]
    (is (= #{:premise/only} (:final-open independent)))
    (is (= #{:premise/only} (get-in independent [:per-step :step/twice])))
    (is (= :complete-for-supplied-source-instances (:status audit)))))

(deftest identical-formulas-with-different-premise-ids-remain-distinct
  (let [first-source (separate-aliases-source :premise/left :premise/right)
        second-source (separate-aliases-source :premise/alpha :premise/beta)
        first-reports (assert-correspondence! first-source)
        second-reports (assert-correspondence! second-source)]
    (is (= #{:premise/left :premise/right}
           (get-in first-reports [:independent :final-open])))
    (is (= #{:premise/alpha :premise/beta}
           (get-in second-reports [:independent :final-open])))
    (is (not= (get-in first-reports [:kernel :fingerprint])
              (get-in second-reports [:kernel :fingerprint])))
    (is (not= (get-in first-reports [:audit :kernel-source-fingerprint])
              (get-in second-reports [:audit :kernel-source-fingerprint])))))

(deftest truly-premise-free-derivations-retain-empty-context
  (doseq [src [(source {} [(s :step/truth :truth-introduction [:true])]
                             [:true])
               (source {} [(s :step/reflex :implication-reflexivity
                              [:implies p p])]
                             [:implies p p])]]
    (let [{:keys [kernel induction]} (assert-correspondence! src)]
      (is (= #{} (:open-premises kernel)))
      (is (= :checked-without-premises (:conclusion-status kernel)))
      (is (= :conditional-source-derivation-soundness (:status induction))))))

(deftest contradictory-premise-is-open-and-not-discharged-by-any-route
  (let [{:keys [kernel audit induction]}
        (assert-correspondence! (false-premise-source))]
    (is (= #{:premise/false} (:open-premises kernel)))
    (is (= :conditional-on-open-premises (:conclusion-status kernel)))
    (is (= :vacuous-instances (:status audit)))
    (is (#{:vacuous-local-hypotheses :vacuous-global-premises}
         (:status induction)))
    (is (= :not-established (:whole-derivation-claim induction)))))

(deftest unused-premise-cannot-enter-via-different-checking-path
  (let [base (repeated-premise-source)
        with-extra (assoc-in base [:premises :premise/unused] [:true])]
    (is (= :gesso.assurance.proof.kernel/invalid-derivation
           (invalid-type #(kernel/check-derivation! with-extra))))
    (rejects-on-all-routes! with-extra)))

(deftest duplicate-alias-cannot-be-silently-collapsed-in-reference-graph
  (let [base (separate-aliases-source :premise/left :premise/right)
        use-left-twice (assoc-in base [:steps 2 :from]
                                 [:step/left :step/left])
        remove-right-step (assoc base :steps
                                 [(first (:steps base))
                                  (assoc (last (:steps base))
                                         :from [:step/left :step/left])])]
    (rejects-on-all-routes! use-left-twice)
    (rejects-on-all-routes! remove-right-step)
    (is (= #{:premise/left}
           (:open-premises
            (:kernel (assert-correspondence!
                      (source {:premise/left p}
                              [(prem :step/left :premise/left p)
                               (s :step/both :conjunction-introduction
                                  [:and p p]
                                  :from [:step/left :step/left])]
                              [:and p p]))))))))

(deftest altered-proof-shapes-and-references-reject-on-every-entry-point
  (let [base (seven-rule-source 1)
        mutations
        [[:wrong-premise (assoc-in base [:steps 0 :premise] :premise/other)]
         [:forward-reference (assoc-in base [:steps 2 :from]
                                       [:step/p :step/resolve])]
         [:unknown-reference (assoc-in base [:steps 9 :from]
                                       [:step/missing])]
         [:unknown-rule (assoc-in base [:steps 8 :rule] :rule/derive-anything)]
         [:wrong-goal (assoc base :goal [:false])]
         [:duplicate-id (assoc-in base [:steps 8 :id] :step/resolve)]
         [:unused-step (update base :steps
                               #(into (vec (butlast %))
                                      [(s :step/dead :truth-introduction [:true])
                                       (last %)]))]
         [:false-conclusion (assoc-in base [:steps 6 :conclusion] [:false])]
         [:changed-implication-premise
          (assoc-in base [:premises :premise/conditional] [:implies p r])]
         [:changed-conjunction-index (assoc-in base [:steps 9 :index] 1)]
         [:extra-source-key (assoc base :certificate :issued)]
         [:extra-step-key (assoc-in base [:steps 4 :valid?] true)]]]
    (is (= 12 (count mutations)))
    (doseq [[label mutant] mutations]
      (testing (name label)
        (rejects-on-all-routes! mutant)))))

(deftest cached-reports-are-not-alternate-proof-sources
  (let [src (seven-rule-source 0)
        {:keys [kernel audit induction]} (assert-correspondence! src)]
    (doseq [old-report [kernel audit induction]
            forged [(assoc old-report :open-premises #{})
                    (assoc old-report :authority :approved)
                    (assoc old-report :certificate :issued)
                    (merge src old-report)
                    (assoc src :previous-check old-report)]]
      (rejects-on-all-routes! forged))))

(deftest changing-source-after-acceptance-rechecks-instead-of-replaying-report
  (let [src (repeated-premise-source)
        checked (kernel/check-derivation! src)
        mutated (assoc-in src [:steps 1 :from] [:step/unknown :step/unknown])]
    (is (= #{:premise/only} (:open-premises checked)))
    (rejects-on-all-routes! mutated)
    (rejects-on-all-routes! (assoc mutated :proof-report checked))
    (is (= checked (kernel/check-derivation! src)))
    (is (= :none (:authority checked)))))

(deftest source-metadata-and-reference-metadata-never-gain-authority
  (let [src (seven-rule-source 0)]
    (doseq [mutant [(with-meta src {:validated? true})
                   (update src :steps with-meta {:certificate :issued})
                   (assoc-in src [:steps 2 :from]
                             (with-meta [:step/p :step/q]
                               {:assumptions-discharge true}))
                   (assoc-in src [:steps 0]
                             (with-meta (first (:steps src))
                               {:authority :certificate}))]]
      (rejects-on-all-routes! mutant))))

(deftest lexical-source-reordering-preserves-premise-ids-but-not-report-identity
  (let [src (seven-rule-source 0)
        ;; Independent, valid topological reorder of two unrelated premise
        ;; introduction steps. Downstream references keep their named IDs.
        reordered (assoc src :steps
                         (-> (:steps src) vec
                             (assoc 0 (get-in src [:steps 1]))
                             (assoc 1 (get-in src [:steps 0]))))
        original (assert-correspondence! src)
        changed (assert-correspondence! reordered)]
    (is (= (get-in original [:independent :final-open])
           (get-in changed [:independent :final-open])))
    (is (not= (get-in original [:kernel :fingerprint])
              (get-in changed [:kernel :fingerprint])))
    (is (= (get-in original [:independent :per-step])
           (get-in changed [:independent :per-step])))))
