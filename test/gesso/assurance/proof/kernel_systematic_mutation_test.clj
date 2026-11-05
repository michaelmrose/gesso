(ns gesso.assurance.proof.kernel-systematic-mutation-test
  "v1425: deterministic bounded mutation exploration of the closed propositional
   proof kernel. A test-local Boolean interpreter independently evaluates every
   accepted source step under the ORIGINAL premises reachable through its
   reference graph. Rejected sources are not counted as proofs of unsoundness.

   This is a finite falsification test, not a metatheorem, an admission gate,
   or evidence about typed predicates, XTDB, or browser execution."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.kernel :as kernel]))

(def ^:private subject
  {:kind :operation :id :example/mutation-corpus
   :interface {:operation :example/mutation-corpus}})

(defn- atom-of [id]
  [:atom {:readers #{id}
          :expression [:> [:read id] [:literal 0]]}])

(def ^:private a (atom-of :result/a))
(def ^:private b (atom-of :result/b))
(def ^:private c (atom-of :result/c))

(def ^:private types
  {:result/a {:type :integer}
   :result/b {:type :integer}
   :result/c {:type :integer}})

;; Written separately from the production proposition evaluator and the v1424
;; reference interpreter; atoms here are free, uninterpreted Boolean inputs.
(defn- symbols-in [form]
  (if (= :atom (first form))
    #{(second form)}
    (reduce set/union #{} (map symbols-in (rest form)))))

(defn- truth [form env]
  (case (first form)
    :atom (boolean (get env (second form)))
    :true true
    :false false
    :not (not (truth (second form) env))
    :and (every? #(truth % env) (rest form))
    :or (boolean (some #(truth % env) (rest form)))
    :implies (or (not (truth (second form) env))
                 (truth (nth form 2) env))
    (throw (ex-info "Unknown test formula" {:form form}))))

(defn- reader-ids [node]
  (cond
    (and (vector? node) (= :atom (first node))) (:readers (second node))
    (map? node) (reduce set/union #{} (map reader-ids (vals node)))
    (coll? node) (reduce set/union #{} (map reader-ids node))
    :else #{}))

(defn- prepare-source [premises steps goal]
  {:subject subject
   :variables (select-keys types (reader-ids [premises steps goal]))
   :premises premises :steps steps :goal goal})

(defn- s [id rule conclusion & attrs]
  (merge {:id id :rule rule :conclusion conclusion} (apply hash-map attrs)))

(defn- corpus-source
  "Each source exercises all seven rules, every premise, and all ten steps.
   Disjunction and conjunction positions change across the corpus."
  [p q r disjunction-slot conjunction-slot]
  (let [both [:and p q]
        conditional [:implies both r]
        disjunction (into [:or] (assoc [[:false] [:false] [:false]]
                                      disjunction-slot r))
        reflex [:implies p p]
        components (assoc [[:true] [:true] [:true]]
                          conjunction-slot disjunction)
        ;; Positions not holding the disjunction are a truth introduction and
        ;; a reflexivity tautology, respectively.
        others (remove #{conjunction-slot} (range 3))
        components (-> components
                       (assoc (first others) [:true])
                       (assoc (second others) reflex))
        refs (-> [:step/truth :step/truth :step/truth]
                 (assoc conjunction-slot :step/disjunction)
                 (assoc (first others) :step/truth)
                 (assoc (second others) :step/reflex))]
    (prepare-source
     {:premise/p p :premise/q q :premise/conditional conditional}
     [(s :step/p :premise p :premise :premise/p)
      (s :step/q :premise q :premise :premise/q)
      (s :step/both :conjunction-introduction both
         :from [:step/p :step/q])
      (s :step/conditional :premise conditional
         :premise :premise/conditional)
      (s :step/r :implication-elimination r
         :from [:step/conditional :step/both])
      (s :step/disjunction :disjunction-introduction disjunction
         :from [:step/r] :index disjunction-slot)
      (s :step/truth :truth-introduction [:true])
      (s :step/reflex :implication-reflexivity reflex)
      (s :step/combined :conjunction-introduction (into [:and] components)
         :from refs)
      (s :step/final :conjunction-elimination disjunction
         :from [:step/combined] :index conjunction-slot)]
     disjunction)))

(def ^:private templates
  [[a b c]
   [[:not a] b c]
   [[:and a b] [:not c] [:or a b]]
   [[:implies a b] [:not b] c]
   [[:or a b] c [:not b]]
   [[:not [:implies a b]] c [:or a c]]
   [[:and a [:true]] [:implies b c] [:not [:false]]]
   [[:or a c] [:not b] [:implies c a]]])

(defn- corpus []
  (mapv (fn [i [p q r]]
          (corpus-source p q r (mod i 3) (mod (quot i 2) 3)))
        (range) templates))

(defn- assignments [source]
  (let [formulas (concat (vals (:premises source))
                         (map :conclusion (:steps source))
                         [(:goal source)])
        symbols (vec (sort-by pr-str
                              (reduce set/union #{} (map symbols-in formulas))))]
    (when (> (count symbols) 5)
      (throw (ex-info "Test oracle atom cap" {:count (count symbols)})))
    (for [n (range (bit-shift-left 1 (count symbols)))]
      (zipmap symbols (map #(bit-test n %) (range (count symbols)))))))

(defn- violates? [assumptions conclusion valuations]
  (boolean (some (fn [env]
                   (and (every? #(truth % env) assumptions)
                        (not (truth conclusion env))))
                 valuations)))

(defn- independently-audit [source]
  (let [premises (:premises source)
        steps (:steps source)
        ids (map :id steps)
        unique-ids? (= (count ids) (count (set ids)))
        valuations (vec (assignments source))
        trace
        (reduce (fn [{:keys [known unsound invalid] :as state} [index step]]
                  (let [dependencies
                        (case (:rule step)
                          :premise (when (contains? premises (:premise step))
                                     #{(:premise step)})
                          (:truth-introduction :implication-reflexivity) #{}
                          (when (and (vector? (:from step))
                                     (every? #(contains? known %) (:from step)))
                            (reduce set/union #{} (map known (:from step)))))]
                    (if (nil? dependencies)
                      (assoc state :invalid (conj invalid index))
                      (let [assumptions (mapv premises dependencies)]
                        {:known (assoc known (:id step) dependencies)
                         :unsound (cond-> unsound
                                    (violates? assumptions (:conclusion step)
                                               valuations)
                                    (conj index))
                         :invalid invalid}))))
                {:known {} :unsound [] :invalid []}
                (map-indexed vector steps))
        final-premises (get (:known trace) (last ids))
        goal-counterexample? (violates? (vals premises) (:goal source)
                                        valuations)]
    {:auditable? (and unique-ids? (empty? (:invalid trace))
                      (some? final-premises))
     :bad-steps (:unsound trace)
     :bad-goal? goal-counterexample?
     :final-premises final-premises
     :premise-satisfying-count
     (count (filter (fn [env]
                      (every? #(truth % env) (vals premises)))
                    valuations))
     :valuation-count (count valuations)}))

(defn- attempt-check [source]
  (try {:accepted (kernel/check-derivation! source)}
       (catch clojure.lang.ExceptionInfo e
         {:rejected (:error/type (ex-data e))})))

(defn- assert-no-authority! [report]
  (is (= kernel/report-type (:type report)))
  (is (= :none (:authority report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (not (contains? report :transaction-authority)))
  (is (not (contains? report :valid?))))

(def ^:private formula-bank
  [[:false] [:true] a b c [:not a] [:or b c] [:implies a c]])

(defn- step-change [src index path new-value]
  (assoc-in src (into [:steps index] path) new-value))

(defn- generated-mutations [src]
  (let [steps (:steps src)
        conclusion-mutations
        (for [index (range (count steps))
              replacement formula-bank
              :when (not= replacement (get-in src [:steps index :conclusion]))]
          (let [candidate (step-change src index [:conclusion] replacement)
                candidate (cond-> candidate
                            (= index (dec (count steps)))
                            (assoc :goal replacement))]
            [(keyword (str "conclusion-" index "-" (.indexOf formula-bank replacement)))
             candidate]))
        ref-mutations
        (for [[index substitutions]
              [[2 [[:step/q :step/p] [:step/p :step/p]]]
               [4 [[:step/both :step/conditional]
                   [:step/p :step/both]]]
               [5 [[:step/p] [:step/q]]]
               [8 [[:step/disjunction :step/truth :step/truth]
                   [:step/truth :step/truth :step/reflex]]]
               [9 [[:step/both] [:step/disjunction]]]]
              new-from substitutions
              :when (not= new-from (get-in src [:steps index :from]))]
          [(keyword (str "references-" index "-" (pr-str new-from)))
           (step-change src index [:from] new-from)])
        premise-mutations
        (for [index [0 1 3]
              new-premise [:premise/p :premise/q :premise/conditional]
              :when (not= new-premise (get-in src [:steps index :premise]))]
          [(keyword (str "premise-" index "-" (name new-premise)))
           (step-change src index [:premise] new-premise)])
        index-mutations
        (for [index [5 9]
              slot [0 1 2]
              :when (not= slot (get-in src [:steps index :index]))]
          [(keyword (str "index-" index "-" slot))
           (step-change src index [:index] slot)])
        shape-mutations
        [[:duplicate-id (step-change src 8 [:id] :step/r)]
         [:future-reference (step-change src 2 [:from] [:step/p :step/r])]
         [:unknown-reference (step-change src 9 [:from] [:step/absent])]
         [:missing-reference (update-in src [:steps 4] dissoc :from)]
         [:invalid-reference-count (step-change src 4 [:from] [:step/both])]
         [:invalid-index (step-change src 9 [:index] 3)]
         [:unknown-rule (step-change src 8 [:rule] :oracle/magic)]
         [:extra-step (update src :steps #(into (vec (butlast %))
                                               [(s :step/extra :truth-introduction [:true])
                                                (last %)]))]
         [:unused-premise (assoc-in src [:premises :premise/extra] [:true])]
         [:unused-variable (assoc-in src [:variables :result/extra]
                                     {:type :integer})]
         [:forged-certificate (assoc src :certificate :verified)]
         [:goal-mismatch (assoc src :goal [:false])]]]
    (vec (concat conclusion-mutations ref-mutations premise-mutations
                 index-mutations shape-mutations))))

(deftest test-local-oracle-has-real-counterexamples
  (let [source (prepare-source {:premise/p a}
                               [(s :step/fake :premise b :premise :premise/p)]
                               b)
        audit (independently-audit source)]
    (is (:auditable? audit))
    (is (= [0] (:bad-steps audit)))
    (is (:bad-goal? audit))
    (is (= 4 (:valuation-count audit)))
    (is (= 2 (:premise-satisfying-count audit)))
    (is (= :gesso.assurance.proof.kernel/invalid-derivation
           (:rejected (attempt-check source))))))

(deftest generated-unmutated-corpus-is-nonvacuous-and-sound
  (is (= 8 (count (corpus))))
  (doseq [[n src] (map-indexed vector (corpus))]
    (testing (str "base-" n)
      (let [audit (independently-audit src)
            result (attempt-check src)]
        (is (:auditable? audit))
        (is (pos? (:premise-satisfying-count audit)))
        (is (empty? (:bad-steps audit)))
        (is (false? (:bad-goal? audit)))
        (is (= (set (keys (:premises src))) (:final-premises audit)))
        (is (map? (:accepted result)) (pr-str result))
        (when-let [report (:accepted result)]
          (is (= (:final-premises audit) (:open-premises report)))
          (assert-no-authority! report))))))

(deftest systematic-mutations-cannot-smuggle-unsound-derivations
  (let [outcomes
        (for [[corpus-index src] (map-indexed vector (corpus))
              [label mutant] (generated-mutations src)]
          (let [audit (independently-audit mutant)
                result (attempt-check mutant)
                bad? (or (not (:auditable? audit))
                         (seq (:bad-steps audit))
                         (:bad-goal? audit))]
            (testing (str "base-" corpus-index " " label)
              (is (or (contains? result :accepted)
                      (= :gesso.assurance.proof.kernel/invalid-derivation
                         (:rejected result))))
              (when bad?
                (is (nil? (:accepted result))
                    (str "Accepted semantically false or uncheckable mutant "
                         corpus-index " " label)))
              (when-let [report (:accepted result)]
                (is (empty? (:bad-steps audit)))
                (is (false? (:bad-goal? audit)))
                (is (:auditable? audit))
                (is (= (set (keys (:premises mutant)))
                       (:open-premises report)))
                (is (= (:final-premises audit) (:open-premises report)))
                (assert-no-authority! report)))
            {:mutated-label label
             :rejected? (contains? result :rejected)
             :falsifiable? (boolean (or (seq (:bad-steps audit))
                                       (:bad-goal? audit)))
             :invalid-graph? (not (:auditable? audit))}))
        summary (frequencies (map :rejected? outcomes))]
    (is (> (count outcomes) 600) (str "Mutation breadth: " (count outcomes)))
    (is (> (count (filter :falsifiable? outcomes)) 100))
    (is (> (count (filter :invalid-graph? outcomes)) 10))
    (is (> (get summary true 0) 500))
    (is (= (mapv generated-mutations (corpus))
           (mapv generated-mutations (corpus)))
        "Corpus generation must not depend on randomness or the clock")))

(deftest mutation-operators-cover-all-seven-rules-and-all-step-positions
  (let [src (first (corpus))
        mutations (generated-mutations src)
        changed-step-indices
        (set (for [[_ candidate] mutations
                   i (range (count (:steps src)))
                   :when (not= (get-in src [:steps i :conclusion])
                               (get-in candidate [:steps i :conclusion]))]
               i))]
    (is (= (set (range 10)) changed-step-indices))
    (is (= #{:premise :truth-introduction :implication-reflexivity
             :conjunction-introduction :conjunction-elimination
             :implication-elimination :disjunction-introduction}
           (set (map :rule (:steps src)))))
    (is (= (count mutations) (count (distinct (map first mutations)))))
    (is (every? (fn [[_ candidate]] (not= candidate src)) mutations))))
