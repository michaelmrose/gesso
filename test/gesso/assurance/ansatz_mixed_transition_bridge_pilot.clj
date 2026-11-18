(ns gesso.assurance.ansatz-mixed-transition-bridge-pilot
  "Opt-in v1747: one-way abstract transition to mixed Ansatz trace bridge.

   From the ORIGINAL typed Gesso guarded-replacement constructors, the v1744
   preservation catalog, and the v1746 checked finite-trace induction theorem,
   derive a quantified, kernel-checked implication for EACH declared operation
   and one projected stutter. The conclusion is the EXACT source-generated
   step formula used by the induction theorem, not a parallel specification.

   The mathematical premises assert the original normalized assignments and
   framing for every INVARIANT-TRACKED field. Guard success, all untracked
   field framing, and the actual interpreter's correctness are not proved by
   these implications. Guarded observer acceptance entails those premises
   only under the original interpreter semantics. An independent observer is
   compared on finite witnesses, not universally proved correct.

   A projected step may hold even if the corresponding Gesso guard fails.
   Neither this bridge nor the overapproximating trace theorem is authority
   to execute a transition, admit an app, or certify an XTDB write.
   Downstream applications remain ordinary, unannotated Clojure."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.walk :as walk]
            [gesso.assurance.ansatz-mixed-induction-pilot]
            [gesso.assurance.ansatz-mixed-initialization-pilot]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private trace-ns 'gesso.assurance.ansatz-mixed-induction-pilot)
(def ^:private init-ns 'gesso.assurance.ansatz-mixed-initialization-pilot)
(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)

(defn- refuse! [reason detail]
  (throw (ex-info "Unqualified mixed source-transition proof bridge"
                  {:reason reason :details detail})))

(defn- pilot! [ns-name function & args]
  (let [v (ns-resolve ns-name function)]
    (when-not v (refuse! :missing-pilot-contract {:ns ns-name :function function}))
    (apply @v args)))

(defn- eq-int [a b] (list '= 'Int a b))
(defn- field-values [fields prefix]
  (zipmap fields (map-indexed (fn [index _]
                               (symbol (str prefix index))) fields)))

(defn- trace-step-projection! [catalog]
  ;; Enforce all v1746 initial/preservation/induction checks FIRST, including
  ;; source fingerprints and the complete operation-by-invariant proof product.
  (let [induction (pilot! trace-ns 'check-induction! catalog)
        ingredients (pilot! trace-ns 'checked-ingredients! catalog)
        {:keys [source]} ingredients
        trace (pilot! trace-ns 'original-trace-obligation! catalog ingredients)
        moves (pilot! trace-ns 'projected-source-steps! catalog ingredients)
        fields (:field-names source)
        trace-fns (zipmap fields (map-indexed (fn [i _] (symbol (str "trace" i))) fields))
        replacements (into {}
                           (mapcat (fn [field]
                                     (let [f (get trace-fns field)
                                           i (.indexOf fields field)]
                                       [[(list f 'k) (symbol (str "pre" i))]
                                        [(list f '(Nat.succ k)) (symbol (str "post" i))]]))
                                   fields))
        step-quantifier (last (:parameters trace))
        declared-step (when (and (seq? step-quantifier)
                                 (= 'forall (first step-quantifier))
                                 (= '[k :- Nat] (second step-quantifier)))
                        (nth step-quantifier 2 nil))
        stutter (into {} (map (fn [field] [field {:kind :framed}]) fields))
        recomputed (pilot! trace-ns 'disjunction
                           (mapv #(pilot! trace-ns 'one-step
                                          fields trace-fns (:effects %) 'k)
                                 (conj moves {:effects stutter})))
        projected (walk/postwalk-replace replacements declared-step)
        projected-branches (mapv #(walk/postwalk-replace replacements
                                                          (pilot! trace-ns 'one-step
                                                                  fields trace-fns (:effects %) 'k))
                                 (conj moves {:effects stutter}))]
    (when-not (and (= :source-derived-conditional-mixed-inductive-safety
                      (:kind induction))
                   (= 1 (:induction-proofs induction))
                   (= (:catalog-fingerprint induction) (:catalog-fingerprint source)
                      (:source-fingerprint trace))
                   (= (:preservation-fingerprint induction)
                      (:preservation-fingerprint trace))
                   (= (:operation-ids trace) (mapv :id (:operations catalog))
                      (mapv :operation-id moves))
                   (= (:source-transitions trace)
                      (mapv #(select-keys % [:operation-id :transition-fingerprint]) moves))
                   (= fields (:tracked-fields trace))
                   (= declared-step recomputed)
                   (= projected (pilot! trace-ns 'disjunction projected-branches))
                   (= (count projected-branches) (inc (count moves)))
                   (not-any? (set (vals trace-fns))
                             (tree-seq coll? seq projected)))
      (refuse! :trace-step-not-source-equivalent
               {:declared declared-step :recomputed recomputed
                :projected projected :operations (:operation-ids trace)}))
    {:source source :ingredients ingredients :trace trace :moves moves
     :fields fields :before (field-values fields "pre")
     :after (field-values fields "post")
     :projected projected :branches projected-branches
     :induction (:proof-checked induction)}))

(defn- projected-equalities! [model effects]
  (let [{:keys [fields before after source]} model]
    (mapv (fn [field]
            (let [{:keys [kind original-value]} (get effects field)
                  value (case kind
                          :literal-write
                          (pilot! init-ns 'witness-proposition-value
                                  source field original-value)
                          :framed (get before field)
                          (refuse! :unsupported-source-effect {:field field :kind kind}))]
              (eq-int (get after field) value)))
          fields)))

(defn- branch-tactics [index count-fields n-operations]
  (vec (concat (repeat index '(right))
               (when (< index n-operations) ['(left)])
               (mapcat (fn [j]
                         (concat (when (< j (dec count-fields)) ['(constructor)])
                                 [(list 'exact (symbol (str "h" j)))]))
                       (range count-fields)))))

(defn- bridge-obligations! [catalog]
  (let [model (trace-step-projection! catalog)
        fields (:fields model)
        moves (:moves model)
        stutter {:effects (into {} (map (fn [field] [field {:kind :framed}]) fields))}
        sources (mapv #(pilot! preservation-ns 'original-transition! %)
                      (:operations catalog))
        branches (conj moves stutter)
        obligations
        (mapv (fn [index {:keys [effects] :as move}]
                (let [source (get sources index)
                      stutter? (= index (count moves))
                      computed (projected-equalities! model effects)
                      known (nth (:branches model) index)
                      branch-form (pilot! trace-ns 'field-conjunction computed)
                      relation (when-not stutter? (:semantics source))]
                  ;; The theorem's mathematical effect premises must come from
                  ;; the normalized ORIGINAL guarded operation, not just from
                  ;; a separately generated weaker trace branch.
                  (when-not (and (= known branch-form)
                                 (or stutter?
                                     (and (= (:operation-id move) (:id source))
                                          (= (:transition-fingerprint move)
                                             (:transition-fingerprint source))
                                          (every? (fn [field]
                                                    (let [effect (get effects field)
                                                          written? (contains? (:writes relation) field)]
                                                      (if written?
                                                        (and (= :literal-write (:kind effect))
                                                             (= [:literal (:original-value effect)]
                                                                (get-in relation [:assignments field])))
                                                        (and (= :framed (:kind effect))
                                                             (contains? (:frame-fields relation) field)))))
                                                  fields))))
                    (refuse! :source-write-frame-not-represented
                             {:operation-id (:operation-id move) :index index}))
                  {:obligation-kind (if stutter? :projected-abstract-stutter-bridge
                                        :source-abstract-operation-bridge)
                   :catalog-fingerprint (:catalog-fingerprint (:source model))
                   :preservation-fingerprint (:preservation-fingerprint (:source model))
                   :operation-id (when-not stutter? (:operation-id move))
                   :transition-fingerprint (when-not stutter? (:transition-fingerprint move))
                   :effect-premises :normalized-original-assignments-and-frames
                   :guard-required-for-observer-acceptance :not-proved-by-this-theorem
                   :parameters (vec (concat (mapcat (fn [field]
                                                     [(get (:before model) field) ':- 'Int
                                                      (get (:after model) field) ':- 'Int])
                                                   fields)
                                            (mapcat (fn [i equality]
                                                      [(symbol (str "h" i)) ':- equality])
                                                    (range) computed)))
                   :goal-form (:projected model)
                   :proof-script (branch-tactics index (count fields) (count moves))}))
              (range (count branches)) branches)]
    (when-not (and (= (inc (count moves)) (count obligations))
                   (= (mapv :operation-id moves)
                      (mapv :operation-id (butlast obligations)))
                   (every? #(= (:projected model) (:goal-form %)) obligations))
      (refuse! :incomplete-bridge-coverage {:count (count obligations)}))
    {:model model :obligations obligations}))

(defn- check-bridge! [catalog]
  (let [{:keys [model obligations]} (bridge-obligations! catalog)
        checked (mapv #(pilot! proof-ns 'check-obligation! %) obligations)]
    (when-not (and (= (count obligations) (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (kernel/verifies? (ansatz/env) goal proof)
                                  (= (:catalog-fingerprint (:source model))
                                     (:catalog-fingerprint description))
                                  (= (:preservation-fingerprint (:source model))
                                     (:preservation-fingerprint description))
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) checked))
      (refuse! :unqualified-abstract-bridge {:checked (count checked)}))
    {:kind :conditional-source-abstract-to-trace-step-bridge
     :catalog-fingerprint (:catalog-fingerprint (:source model))
     :induction-proof (:induction model)
     :abstract-operation-count (count (:moves model))
     :kernel-checked-bridge-count (count checked)
     :checked checked
     :observer-correspondence :bounded-tests-not-proved
     :interpreter-correspondence :not-universally-proved
     :physical-correspondence :not-established
     :step-exhaustiveness :explicit-premise-not-proved
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- encoded [source field value]
  (let [desc (get-in source [:state-types field])]
    (if (= :enum (:type desc))
      (pilot! init-ns 'witness-proposition-value source field value)
      value)))

(defn- evaluate-projected! [model before after]
  ;; Tiny closed evaluator for the EXACT kernel-checked generated step FORM.
  ;; Used only in independent bounded tests, not as a universal interpreter.
  (let [vars (into {} (mapcat (fn [field]
                               [[(get (:before model) field) (encoded (:source model) field (get before field))]
                                [(get (:after model) field) (encoded (:source model) field (get after field))]])
                             (:fields model)))]
    (letfn [(ev [x]
              (cond
                (contains? vars x) (get vars x)
                (integer? x) x
                (and (seq? x) (= 'Or (first x)) (= 3 (count x)))
                (or (ev (second x)) (ev (nth x 2)))
                (and (seq? x) (= 'And (first x)) (= 3 (count x)))
                (and (ev (second x)) (ev (nth x 2)))
                (and (seq? x) (= '= (first x)) (= 'Int (second x)) (= 4 (count x)))
                (= (ev (nth x 2)) (ev (nth x 3)))
                :else (refuse! :unsupported-checked-step-expression {:expression x})))]
      (boolean (ev (:projected model))))))

(defn- closed-sample-states [catalog]
  (let [types (get-in catalog [:operations 0 :construction :state-types])
        fields (sort (keys types))
        candidates (fn [descriptor]
                     (case (:type descriptor)
                       :integer [-1 0 1 4]
                       :enum (vec (sort-by str (:values descriptor)))
                       :keyword (if (:nullable? descriptor) [nil :audit/sample]
                                    [:audit/sample])
                       (refuse! :unsupported-observer-sample-type descriptor)))]
    (reduce (fn [acc field]
              (vec (for [m acc value (candidates (get types field))]
                     (assoc m field value))))
            [{}] fields)))

(defn- check-bounded-observers! [catalog]
  (let [{:keys [model]} (bridge-obligations! catalog)
        states (closed-sample-states catalog)
        original-operations (mapv #(pilot! preservation-ns 'original-transition! %)
                                 (:operations catalog))
        observations (atom 0)
        accepted (atom 0)]
    (doseq [{:keys [source]} original-operations
            before states]
      (let [assignment (:assignments source)
            post (reduce (fn [m [field expr]]
                           (assoc m field (case (first expr)
                                            :literal (second expr)
                                            (refuse! :unsupported-observer-assignment expr))))
                         before assignment)
            ;; A second candidate deliberately violates a tracked write (or
            ;; guard when the candidate is otherwise identical).
            candidates [post before]]
        (doseq [after candidates]
          (let [primary (abstract/observe-transition! source before {} after)
                secondary (independent/observe-transition! source before {} after)]
            (swap! observations inc)
            (when-not (= (:matches? primary) (:matches? secondary))
              (refuse! :abstract-observer-disagreement
                       {:before before :after after :source (:subject source)}))
            (when (:matches? primary)
              (swap! accepted inc)
              (when-not (evaluate-projected! model before after)
                (refuse! :observer-accepted-step-not-in-ansatz-relation
                         {:before before :after after})))))))
    (when-not (pos? @accepted)
      (refuse! :vacuous-bounded-correspondence {:observations @observations}))
    {:observations @observations :accepted @accepted
     :scope :finite-witnesses-not-universal-interpreter-proof}))

(defn- mixed-catalog []
  (pilot! init-ns 'initialized (pilot! preservation-ns 'mixed-catalog)))

(deftest mixed-catalog-has-operation-and-stutter-bridge-theorems
  (let [catalog (mixed-catalog)
        result (check-bridge! catalog)]
    (is (= 4 (:abstract-operation-count result)))
    (is (= 5 (:kernel-checked-bridge-count result)))
    (is (= (set (:declared-operation-ids catalog))
           (set (keep (comp :operation-id :description) (:checked result)))))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                (:checked result)))
    (is (kernel/verifies? (ansatz/env)
                          (get-in result [:induction-proof :goal])
                          (get-in result [:induction-proof :proof])))
    (is (= :explicit-premise-not-proved (:step-exhaustiveness result)))
    (is (= :not-established (:physical-correspondence result)))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest original-reservation-and-signed-catalog-use-same-bridge
  (doseq [[catalog expected]
          [[(example/reservation-catalog) 3]
           [(pilot! init-ns 'initialized
                    (pilot! preservation-ns 'example-catalog
                            :queue/temperature -20 30 [-5 30])) 4]]]
    (let [result (check-bridge! catalog)]
      (is (= expected (:kernel-checked-bridge-count result)))
      (is (= (dec expected) (:abstract-operation-count result)))
      (is (= :none (:authority result))))))

(deftest two-abstract-observers-and-checked-step-agree-on-bounded-witnesses
  (doseq [catalog [(mixed-catalog) (example/reservation-catalog)]]
    (let [report (check-bounded-observers! catalog)]
      (is (pos? (:accepted report)))
      (is (> (:observations report) (:accepted report)))
      (is (= :finite-witnesses-not-universal-interpreter-proof
             (:scope report))))))

(deftest source-coverage-and-exact-step-mutants-fail-closed
  (let [catalog (mixed-catalog)
        mut (fn [bad]
              (try (bridge-obligations! bad) nil
                   (catch clojure.lang.ExceptionInfo ex
                     (:reason (ex-data ex)))))
        trace-var (ns-resolve trace-ns 'original-trace-obligation!)
        original @trace-var]
    (is (= :incomplete-catalog-coverage
           (mut (update catalog :operations pop))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (bridge-obligations!
                  (assoc-in catalog [:operations 0 :construction
                                     :replacements :queue/count] [:literal 5]))))
    (let [calls (atom 0)]
      ;; The first call is part of check-induction!; change only the subsequent
      ;; source step presented for correspondence, not the proved theorem.
      (with-redefs-fn {trace-var (fn [& args]
                                  (let [result (apply original args)]
                                    (if (= 2 (swap! calls inc))
                                      (update result :parameters
                                              (fn [ps]
                                                (conj (pop ps)
                                                      '(forall [k :- Nat] (= Int 0 0)))))
                                      result)))}
        #(is (= :trace-step-not-source-equivalent
                (mut catalog)))))))

(deftest projected-step-is-not-operation-authority
  (let [catalog (mixed-catalog)
        {:keys [model]} (bridge-obligations! catalog)
        from {:queue/status :busy :queue/count 0 :queue/audit nil}
        after {:queue/status :busy :queue/count 4 :queue/audit nil}
        operation (first (:operations catalog))
        source (:source (pilot! preservation-ns 'original-transition! operation))
        observation (abstract/observe-transition! source from {} after)]
    (is (evaluate-projected! model from after))
    (is (false? (:matches? observation)))
    (is (= :not-admitted (get-in (check-bridge! catalog) [:admission])))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests
                             'gesso.assurance.ansatz-mixed-transition-bridge-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
