(ns gesso.assurance.ansatz-mixed-induction-pilot
  "Opt-in v1746: source-derived finite-trace induction for mixed Gesso catalogs.

   Reuse the ORIGINAL guarded replacement constructors, the v1744 mixed
   integer/enum preservation proofs and the v1745 initial-predicate proofs.
   Project each SOURCE transition to exact writes/framing of all tracked
   invariant fields. Kernel-check one quantified Nat-indexed theorem that
   the declared invariants hold after any finite number of such steps (or
   stutters), conditional on a valid initial predicate and universal step
   exhaustiveness. The latter premise is ASSUMED, not independently proved.

   Guard success, audit framing on other fields, real XTDB writes, actual
   database initialization, writer closure, translator correctness and JVM
   correspondence remain OPEN assumptions. This is opt-in test-only proof
   construction, NOT an application certificate or production gate. Ordinary
   downstream Clojure needs neither Typed Clojure nor Ansatz annotations."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-mixed-initialization-pilot]
            [gesso.assurance.example.single-node-proving :as example]))

(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private initial-ns 'gesso.assurance.ansatz-mixed-initialization-pilot)
(def ^:private outstanding
  #{:actual-database-initialization
    :actual-commits-refine-the-abstract-source-steps
    :complete-installed-writer-catalog
    :source-translation-and-enum-encoding-correspondence
    :runtime-interpreter-and-transaction-correspondence
    :source-deployment-currentness
    :kernel-and-permitted-axioms})

(defn- refuse! [reason detail]
  (throw (ex-info "Unqualified mixed inductive assurance"
                  {:reason reason :details detail})))

(defn- call! [n f & args]
  (let [v (ns-resolve n f)]
    (when-not v (refuse! :missing-pilot-contract {:namespace n :function f}))
    (apply @v args)))

(defn- eq-int [a b] (list '= 'Int a b))
(defn- field-at [name time] (list name time))
(defn- disjunction [forms]
  (when-not (<= 2 (count forms) 13)
    (refuse! :unsupported-step-disjunction-size {:size (count forms)}))
  (reduce (fn [right left] (list 'Or left right))
          (last forms) (reverse (butlast forms))))
(defn- field-conjunction [forms]
  (when-not (<= 1 (count forms) 12)
    (refuse! :unsupported-projected-field-count {:size (count forms)}))
  (if (= 1 (count forms)) (first forms)
      (call! initial-ns 'conjunction (vec forms))))

(defn- checked-ingredients! [catalog]
  (let [initial (call! initial-ns 'check-initialization! catalog)
        preservation (call! preservation-ns 'check-catalog!
                            (dissoc catalog :initial-condition))
        source (call! initial-ns 'initial-source! catalog)
        descriptions (mapv :description (:kernel-checked-proofs preservation))
        expected-pairs (set (for [o (:declared-operation-ids catalog)
                                  i (keys (:invariants catalog))] [o i]))
        pairs (mapv (juxt :operation-id :invariant-id) descriptions)]
    (when-not (and (= (:preservation-fingerprint initial)
                      (:catalog-fingerprint preservation)
                      (:preservation-fingerprint source))
                   (= (:source-fingerprint initial)
                      (:catalog-fingerprint source))
                   (= expected-pairs (set pairs))
                   (= (count expected-pairs) (count pairs))
                   (= (inc (count (:source-ordered-ids source)))
                      (:proof-count initial))
                   (= (count expected-pairs) (:proof-count preservation))
                   (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                           (concat (:kernel-checked-proofs preservation)
                                   (:kernel-checked-proofs initial))))
      (refuse! :incomplete-or-unrelated-component-proofs
               {:actual-pairs pairs :expected expected-pairs}))
    {:source source :initial initial :preservation preservation
     :proof-descriptions descriptions}))

(defn- projected-source-steps! [catalog {:keys [source preservation proof-descriptions]}]
  (let [fields (:field-names source)
        by-pair (into {} (map (juxt (juxt :operation-id :invariant-id) identity)
                              proof-descriptions))
        operations (mapv #(call! preservation-ns 'original-transition! %)
                         (:operations catalog))
        invariant-ids (:source-ordered-ids source)
        result
        (mapv (fn [{:keys [id semantics transition-fingerprint]}]
                (let [effects
                      (into {}
                            (for [field fields]
                              (let [writes (:writes semantics)
                                    framed (:frame-fields semantics)
                                    written? (contains? writes field)
                                    framed? (contains? framed field)
                                    assignment (get-in semantics [:assignments field])
                                    invariant-descriptions
                                    (mapv #(get by-pair [id %])
                                          (filter (fn [inv-id]
                                                    (= field (get-in source
                                                                       [:invariants inv-id :field])))
                                                  invariant-ids))
                                    value (when written? (second assignment))
                                    expected-effect (if written? :source-literal-write
                                                          :source-framed-field)
                                    translated (when written?
                                                 (call! initial-ns
                                                        'witness-proposition-value source field value))]
                                (when-not (and (not= written? framed?)
                                               (seq invariant-descriptions)
                                               (every? some? invariant-descriptions)
                                               (or (not written?)
                                                   (and (vector? assignment)
                                                        (= 2 (count assignment))
                                                        (= :literal (first assignment))))
                                               (every? (fn [d]
                                                         (and (= expected-effect (:effect d))
                                                              (= transition-fingerprint
                                                                 (:transition-fingerprint d))
                                                              (= (:catalog-fingerprint preservation)
                                                                 (:catalog-fingerprint d))
                                                              (= value (:write-value d))))
                                                       invariant-descriptions))
                                  (refuse! :unrepresented-source-field-effect
                                           {:operation id :field field
                                            :assigned assignment
                                            :effect expected-effect}))
                                [field {:kind (if written? :literal-write :framed)
                                        :value translated
                                        :original-value value}])))]
                  {:operation-id id
                   :transition-fingerprint transition-fingerprint
                   :effects effects}))
              operations)]
    (when-not (and (= (:declared-operation-ids catalog)
                      (set (map :operation-id result)))
                   (= (count result) (count (:declared-operation-ids catalog)))
                   (every? #(= (set fields) (set (keys (:effects %)))) result))
      (refuse! :incomplete-source-step-product result))
    result))

(defn- one-step [fields functions effects k]
  (let [next-k (list 'Nat.succ k)]
    (field-conjunction
     (mapv (fn [field]
             (let [function (get functions field)
                   {:keys [kind value]} (get effects field)]
               (eq-int (field-at function next-k)
                       (case kind
                         :literal-write value
                         :framed (field-at function k)
                         (refuse! :unknown-step-effect {:field field :kind kind})))))
           fields))))

(defn- branch-tactics! [source fields invariant-ids effects index]
  ;; One proof branch per ORIGINAL declared operation plus projection stutter.
  ;; Integer leaves are Presburger goals; enum leaves use the same explicit
  ;; membership constructors as v1744, avoiding unjustified omega shortcuts
  ;; for disjunctions. Framed enum fields are rewritten to their prior value.
  (vec
   (mapcat
    (fn [i invariant-id]
      (let [{:keys [field members]} (get-in source [:invariants invariant-id])
            field-index (.indexOf fields field)
            hyp (symbol (str "h" index "_" field-index))
            prior (symbol (str "hi" i))
            {:keys [kind original-value]} (get effects field)]
        (when (neg? field-index)
          (refuse! :missing-invariant-trace-field {:invariant invariant-id}))
        (concat (when (< i (dec (count invariant-ids))) ['(constructor)])
                (if (= :finite-enum-membership
                       (get-in source [:invariants invariant-id :kind]))
                  (case kind
                    :framed [(list 'rw [hyp]) (list 'exact prior)]
                    :literal-write
                    (mapv #(if (= '(exact heffect) %)
                             (list 'exact hyp) %)
                          (call! preservation-ns 'enum-write-script
                                 members original-value))
                    (refuse! :unknown-enum-trace-effect {:kind kind}))
                  ['(omega)]))))
    (range (count invariant-ids)) invariant-ids)))

(defn- original-trace-obligation! [catalog ingredients]
  (let [{:keys [source]} ingredients
        fields (:field-names source)
        functions (zipmap fields (mapv #(symbol (str "trace" %)) (range (count fields))))
        at (fn [k] (into {} (map (fn [field]
                                  [field (field-at (get functions field) k)])
                                fields)))
        invariant-at (fn [k]
                       (call! initial-ns 'conjunction
                              (mapv #(call! initial-ns 'source-proposition!
                                            source % (at k))
                                    (:source-ordered-ids source))))
        moves (projected-source-steps! catalog ingredients)
        stutter-effects (into {} (map (fn [field] [field {:kind :framed}]) fields))
        step (disjunction (mapv #(one-step fields functions (:effects %) 'k)
                                (conj moves {:effects stutter-effects})))
        case-hypotheses
        (vec (for [index (range (inc (count moves)))]
               (let [names (mapv #(symbol (str "h" index "_" %)) (range (count fields)))]
                 (if (= 1 (count fields)) (first names)
                     (call! initial-ns 'pattern names)))))
        case-pattern (reduce (fn [right left] (list 'or left right))
                             (last case-hypotheses)
                             (reverse (butlast case-hypotheses)))
        prior-hypotheses (mapv #(symbol (str "hi" %))
                                (range (count (:source-ordered-ids source))))
        prior-pattern (call! initial-ns 'pattern prior-hypotheses)
        branches (conj moves {:effects stutter-effects})
        _ (when-not (and (= (mapv :id (:operations catalog))
                            (mapv :operation-id moves))
                         (= (count (:declared-operation-ids catalog)) (count moves))
                         (every? #(= (set fields) (set (keys (:effects %)))) moves))
            (refuse! :trace-step-source-coverage-mismatch
                     {:declared (mapv :id (:operations catalog))
                      :projected (mapv :operation-id moves)}))
        params (vec (concat (mapcat (fn [field]
                                    [(get functions field) ':- '(=> Nat Int)]) fields)
                            ['n ':- 'Nat
                             'hinit ':- (invariant-at 0)
                             'hsteps ':- (list 'forall ['k ':- 'Nat] step)]))]
    {:obligation-kind :generic-mixed-conditional-trace-induction
     :source-fingerprint (:catalog-fingerprint source)
     :preservation-fingerprint (:preservation-fingerprint source)
     :tracked-fields fields
     :tracked-invariants (:source-ordered-ids source)
     :operation-ids (mapv :operation-id moves)
     :source-transitions (mapv #(select-keys % [:operation-id
                                                :transition-fingerprint]) moves)
     :step-exhaustiveness :explicit-premise-not-proved
     :step-relation :source-literal-writes-and-framed-fields-or-stutter
     :parameters params
     :goal-form (invariant-at 'n)
     :proof-script (vec (concat '[(induction n) (exact hinit) (rename_i k ih)]
                                [(list 'rcases 'ih prior-pattern)
                                 '(specialize hsteps k)
                                 (list 'rcases 'hsteps case-pattern)]
                                (mapcat (fn [i branch]
                                          (branch-tactics! source fields
                                                           (:source-ordered-ids source)
                                                           (:effects branch) i))
                                        (range (count branches)) branches)))}))

(defn- check-induction! [catalog]
  (let [ingredients (checked-ingredients! catalog)
        obligation (original-trace-obligation! catalog ingredients)
        checked (call! proof-ns 'check-obligation! obligation)]
    (when-not (and (= (:source-fingerprint obligation)
                      (get-in checked [:description :source-fingerprint]))
                   (= (:preservation-fingerprint obligation)
                      (get-in checked [:description :preservation-fingerprint]))
                   (kernel/verifies? (ansatz/env) (:goal checked) (:proof checked))
                   (= :ansatz-reservation-pilot/lean-core-axioms-v1
                      (get-in checked [:dependencies :policy]))
                   (empty? (get-in checked [:dependencies :non-safe]))
                   (empty? (get-in checked [:dependencies :unresolved])))
      (refuse! :unqualified-inductive-trace-proof {}))
    {:kind :source-derived-conditional-mixed-inductive-safety
     :catalog-fingerprint (:source-fingerprint obligation)
     :preservation-fingerprint (:preservation-fingerprint obligation)
     :operation-count (count (:operation-ids obligation))
     :invariant-count (count (:tracked-invariants obligation))
     :initialization-proofs (get-in ingredients [:initial :proof-count])
     :preservation-proofs (get-in ingredients [:preservation :proof-count])
     :induction-proofs 1
     :proof-checked checked
     :step-exhaustiveness :explicit-premise-not-proved
     :open-assumptions outstanding
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- initialized [base] (call! initial-ns 'initialized base))
(defn- mixed [] (initialized (call! preservation-ns 'mixed-catalog)))

(deftest four-operation-mixed-catalog-has-quantified-inductive-proof
  (let [result (check-induction! (mixed))]
    (is (= 4 (:operation-count result)))
    (is (= 3 (:invariant-count result)))
    (is (= 4 (:initialization-proofs result)))
    (is (= 12 (:preservation-proofs result)))
    (is (= 1 (:induction-proofs result)))
    (is (kernel/verifies? (ansatz/env)
                          (get-in result [:proof-checked :goal])
                          (get-in result [:proof-checked :proof])))
    (is (= :explicit-premise-not-proved (:step-exhaustiveness result)))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest original-reservation-and-signed-single-field-catalogs-also-compose
  (doseq [catalog [(example/reservation-catalog)
                   (initialized (call! preservation-ns 'example-catalog
                                       :queue/temperature -20 30 [-5 30]))]]
    (let [result (check-induction! catalog)]
      (is (= 1 (:induction-proofs result)))
      (is (pos? (:preservation-proofs result)))
      (is (kernel/verifies? (ansatz/env)
                            (get-in result [:proof-checked :goal])
                            (get-in result [:proof-checked :proof]))))))

(deftest narrowed-enum-domains-and-reordered-initial-source-compose
  (let [source (mixed)
        enum-rule [:in [:read :queue/status] [:literal #{:ready :busy}]]
        narrowed (-> source
                     (assoc-in [:invariants :queue/status-domain :expression]
                               enum-rule)
                     (assoc-in [:initial-condition :expression 3] enum-rule)
                     (assoc-in [:operations 2 :construction :replacements
                                :queue/status] [:literal :busy]))
        singleton-rule [:in [:read :queue/status] [:literal #{:busy}]]
        singleton (-> narrowed
                      (assoc-in [:invariants :queue/status-domain :expression]
                                singleton-rule)
                      (assoc-in [:initial-condition :expression 3] singleton-rule)
                      (assoc-in [:operations 0 :construction :replacements
                                 :queue/status] [:literal :busy])
                      (assoc-in [:operations 1 :construction :replacements
                                 :queue/status] [:literal :busy]))
        reordered (update-in source [:initial-condition :expression]
                            (fn [expr] (into [:and] (reverse (rest expr)))))
        candidates [narrowed singleton reordered]]
    (doseq [catalog candidates]
      (let [result (check-induction! catalog)]
        (is (= 12 (:preservation-proofs result)))
        (is (= 4 (:initialization-proofs result)))
        (is (= 1 (:induction-proofs result)))
        (is (= :none (:authority result)))))))

(deftest a-dropped-step-branch-cannot-be-certified-by-its-weaker-theorem
  (let [catalog (mixed)
        ingredients (checked-ingredients! catalog)
        v (ns-resolve 'gesso.assurance.ansatz-mixed-induction-pilot
                      'projected-source-steps!)
        original @v]
    ;; A theorem over fewer legal steps might still be mathematically TRUE.
    ;; The required SOURCE coverage must reject that proof as irrelevant.
    (with-redefs-fn {v (fn [& args] (pop (apply original args)))}
      #(is (= :trace-step-source-coverage-mismatch
              (try (original-trace-obligation! catalog ingredients)
                   nil
                   (catch clojure.lang.ExceptionInfo ex
                     (:reason (ex-data ex)))))))))

(deftest missing-step-exhaustiveness-cannot-prove-a-trace-theorem
  (let [catalog (mixed)
        obligation (original-trace-obligation! catalog (checked-ingredients! catalog))
        stripped (update obligation :parameters #(subvec % 0 (- (count %) 3)))]
    (is (= 'hsteps (nth (:parameters obligation) (- (count (:parameters obligation)) 3))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (call! proof-ns 'check-obligation! stripped)))))

(deftest unsupported-mutations-cannot-inherit-a-previous-proof
  (let [catalog (mixed)
        deny (fn [bad]
               (try (check-induction! bad) nil
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e)))))
        original (original-trace-obligation! catalog (checked-ingredients! catalog))
        changed (-> catalog
                    (assoc-in [:operations 0 :construction :replacements :queue/count]
                              [:literal 3]))]
    (is (= :incomplete-catalog-coverage
           (deny (update catalog :operations pop))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check-induction!
                  (assoc-in catalog [:operations 0 :construction :replacements :queue/count]
                            [:literal 5]))))
    (is (= :unsupported-initial-condition
           (deny (update-in catalog [:initial-condition :expression] pop))))
    (is (not= (:source-fingerprint original)
              (:source-fingerprint
               (original-trace-obligation! changed (checked-ingredients! changed)))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-mixed-induction-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
