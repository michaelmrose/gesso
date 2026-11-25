(ns gesso.assurance.ansatz-mapped-prestate-cardinality-pilot
  "Opt-in cardinality lemma for the v2553 decoded-row assertion rules.
   This proves a list-partition lemma at the actual source-derived map sizes.
   A faithful partition into matched field entries and residual entries is a
   premise, NOT a theorem about Clojure maps, SQL structs, or provider reads.
   Run with clojure -M:ansatz:typed:test -m
   gesso.assurance.ansatz-mapped-prestate-cardinality-pilot."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [ansatz.kernel.expr :as expr]
            [clojure.test :refer [deftest is run-tests]]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.proof.mapped-prestate-rules :as rules]
            [gesso.assurance.proof.mapped-prestate-semantics :as semantics]
            [gesso.assurance.proof.mapped-prestate-partition :as partition]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.mapped-projection :as projection]
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

(defn- general-cardinality-obligation []
  ;; Quantify the cardinality itself rather than asking the tactic engine
  ;; to prove a separate arithmetic fact for every map arity.
  (-> (obligation 'n [])
      (assoc :obligation-kind :arbitrary-encoded-map-residual-exclusion)
      (dissoc :expected-cardinality :sites)
      (update :parameters #(into ['n ':- 'Nat] %))))

(defn- specialize-cardinality! [general ob]
  (let [n (:expected-cardinality ob)
        goal (proof-pilot! 'elaborate-source-proposition! ob)
        proof (expr/app* (:proof general) (expr/lit-nat n))
        env (ansatz/env)]
    (when-not (kernel/verifies? env goal proof)
      (throw (ex-info "Kernel rejected cardinality specialization."
                      {:expected-cardinality n})))
    {:description (dissoc ob :parameters :goal-form :proof-script)
     :goal goal :proof proof
     :dependencies (proof-pilot! 'qualify-proof-dependencies!
                                (proof-pilot! 'inspect-proof-dependencies env goal proof))}))

(defn check!
  "Kernel-check one arbitrary-cardinality list lemma and specialize its
   proof term at each source-derived size. Check every specialized goal
   against the original obligation and apply the existing dependency policy.
   List order, unique field encoding, matching child values and the actual
   map entry partition remain premises."
  [original targets docs journal prefix]
  (let [{:keys [rule-description sites obligations]} (derive-obligations! original targets docs journal prefix)
        general (when (seq obligations)
                  (proof-pilot! 'check-obligation! (general-cardinality-obligation)))
        checked (mapv #(specialize-cardinality! general %) obligations)]
    {:type ::cardinality-pilot :rule-description rule-description
     :sites sites :checked checked :general-lemma general
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

(defn- list-form [codes]
  (reduce (fn [tail code] (list 'List.cons 'Int code tail))
          '(List.nil Int) (reverse codes)))

(defn- check-instance! [lemma {:keys [target path cardinality partition]}]
  (let [{:keys [known actual residual]} partition
        known-form (list-form known)
        actual-form (list-form actual)
        residual-form (list-form residual)
        premise! (fn [form]
                   (proof-pilot! 'check-obligation!
                                 {:parameters [] :goal-form form :proof-script '[(rfl)]}))
        hk (premise! (list '= 'Nat (list 'List.length known-form) cardinality))
        ha (premise! (list '= 'Nat (list 'List.length actual-form) cardinality))
        hp (premise! (list '= '(List Int) actual-form
                           (list 'List.append known-form residual-form)))
        goal (proof-pilot! 'elaborate-source-proposition!
                          {:parameters [] :goal-form (list '= '(List Int) actual-form known-form)})
        [_ actual-term known-term] (expr/get-app-args goal)
        residual-term (last (expr/get-app-args (last (expr/get-app-args (:goal hp)))))
        proof (expr/app* (:proof lemma) known-term actual-term residual-term
                         (:proof hk) (:proof ha) (:proof hp))
        env (ansatz/env)]
    (when-not (kernel/verifies? env goal proof)
      (throw (ex-info "Kernel rejected concrete cardinality application."
                      {:target target :path path})))
    {:target target :path path :cardinality cardinality
     :goal goal :proof proof :premises [hk ha hp]
     :dependencies (proof-pilot! 'qualify-proof-dependencies!
                                (proof-pilot! 'inspect-proof-dependencies env goal proof))}))

(defn check-partitions!
  "Re-derive concrete partitions and apply the original checked lemma only
   at sites with established cardinality premises. The kernel checks the
   instantiated equality of encoded lists; the decoded-map bridge remains
   instance evidence and does not imply a general refinement theorem."
  [original targets docs journal prefix actual]
  (let [observation (partition/observe! original targets docs journal prefix actual)
        abstract (check! original targets docs journal prefix)
        lemmas (into {} (map (juxt #(get-in % [:description :expected-cardinality]) identity))
                     (:checked abstract))
        sites (mapv (fn [{:keys [cardinality partition] :as site}]
                      (if (:cardinality-premises-satisfied? partition)
                        (assoc site :application (check-instance! (get lemmas cardinality) site)
                               :status :kernel-checked-encoded-equality)
                        (assoc site :status :premises-open)))
                    (:sites observation))]
    {:type ::partition-cardinality-pilot :sites sites
     :status (cond (empty? sites) :no-map-sites
                   (every? #(= :kernel-checked-encoded-equality (:status %)) sites)
                   :all-map-encoded-equalities-checked
                   :else :map-encoded-equalities-open)
     :open-obligations (:open-obligations observation)
     :scope :source-derived-concrete-encoded-list-equalities
     :sql-refinement :not-established :authority :none :certificate :not-issued}))

(deftest concrete-partitions-instantiate-the-checked-cardinality-lemma
  (let [[_ _ docs journal _ :as args] (fixture!)
        request [:document :requests "r1"]
        bags {request (:requests docs) [:document :assignments "a1"] []
              [:journal-slot "p1" :example/command] []
              [:journal-slot "p2" :example/command] journal}
        report (apply check-partitions! (conj args bags))
        changed (apply check-partitions! (conj args (assoc-in bags [request 0 :extra] true)))
        changed-site (first (filter #(= request (:target %)) (:sites changed)))]
    (is (= :all-map-encoded-equalities-checked (:status report)))
    (is (seq (:sites report)))
    (is (= :map-encoded-equalities-open (:status changed)))
    (is (= :premises-open (:status changed-site)))
    (is (nil? (:application changed-site)))
    (is (= 1 (count (get-in changed-site [:partition :residual]))))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:sql-refinement report)))
    (is (contains? (:open-obligations report) :general-structural-soundness))
    (doseq [{:keys [target path partition application status]} (:sites report)]
      (let [{:keys [goal proof premises dependencies]} application]
        (is (= :kernel-checked-encoded-equality status))
        (is (= [target path] ((juxt :target :path) application)))
        (is (= 3 (count premises)))
        (is (= (:known partition) (:actual partition)))
        (is (kernel/verifies? (ansatz/env) goal proof))
        (is (empty? (:unresolved dependencies)))
        (is (empty? (:non-safe dependencies)))))))

(defn- constructor-obligation []
  {:obligation-kind :encoded-child-cons-congruence
   :parameters '[x :- Int y :- Int xs :- (List Int) ys :- (List Int)
                 hx :- (= Int x y) ht :- (= (List Int) xs ys)]
   :goal-form '(= (List Int) (List.cons Int x xs) (List.cons Int y ys))
   :proof-script '[(rw hx) (rw ht)]})

(defn- transitivity-obligation []
  {:obligation-kind :encoded-parent-equality-composition
   :parameters '[a :- (List Int) k :- (List Int) e :- (List Int)
                 h1 :- (= (List Int) a k) h2 :- (= (List Int) k e)]
   :goal-form '(= (List Int) a e)
   :proof-script '[(rw h1) (exact h2)]})

(defn- checked-application! [goal proof]
  (let [env (ansatz/env)]
    (when-not (kernel/verifies? env goal proof)
      (throw (ex-info "Kernel rejected encoded composition." {})))
    {:goal goal :proof proof
     :dependencies (proof-pilot! 'qualify-proof-dependencies!
                                 (proof-pilot! 'inspect-proof-dependencies env goal proof))}))

(defn- closed-goal [form]
  (proof-pilot! 'elaborate-source-proposition! {:parameters [] :goal-form form}))

(defn- reflexivity! [form]
  (proof-pilot! 'check-obligation!
                {:parameters [] :goal-form form :proof-script '[(rfl)]}))

(defn- compose-child-list! [constructor known expected]
  ;; Work from the empty suffix toward the complete list, composing each
  ;; entry equality with the preceding checked suffix proof. This does
  ;; not prove that a decoded child can faithfully be represented by Int.
  (when-not (= (count known) (count expected))
    (throw (ex-info "Unequal encoded child arities." {})))
  (reduce (fn [{tail-proof :proof :as tail} [k e]]
            (let [head (reflexivity! (list '= 'Int k e))
                  [_ kt et] (expr/get-app-args (:goal head))
                  [_ ks es] (expr/get-app-args (:goal tail))
                  goal (closed-goal (list '= '(List Int)
                                         (list-form (into [k] (:known tail)))
                                         (list-form (into [e] (:expected tail)))))
                  proof (expr/app* (:proof constructor) kt et ks es (:proof head) tail-proof)]
              (assoc (checked-application! goal proof)
                     :known (into [k] (:known tail)) :expected (into [e] (:expected tail)))))
          (assoc (reflexivity! '(= (List Int) (List.nil Int) (List.nil Int)))
                 :known [] :expected [])
          (reverse (map vector known expected))))

(defn- compose-parent! [composition cardinality known expected actual]
  (let [child-list (compose-child-list! (:constructor composition) known expected)
        [_ a k] (expr/get-app-args (:goal cardinality))
        [_ _ e] (expr/get-app-args (:goal child-list))
        goal (closed-goal (list '= '(List Int) (list-form actual) (list-form expected)))
        proof (expr/app* (get-in composition [:transitivity :proof]) a k e
                         (:proof cardinality) (:proof child-list))]
    {:child-list child-list :parent (checked-application! goal proof)}))

(defn- established? [node]
  (#{:decoded-leaf-equality-checked :composed-map-equality-checked
     :empty-bag-checked} (:status node)))

(defn- compose-value! [lemmas composition target path expected actual present?]
  (let [site {:target target :path path}]
    (cond
      (not present?) (assoc site :status :premises-open :reason :missing-field)
      (and (map? expected) (seq expected))
      (if-not (and (map? actual) (every? keyword? (keys actual)))
        (assoc site :status :premises-open :reason :unsupported-map-shape)
        (let [ks (sort-by str (keys expected))
              children (mapv (fn [k] (compose-value! lemmas composition target (conj path k)
                                                     (get expected k) (get actual k)
                                                     (contains? actual k))) ks)
              parent (assoc site :children children :cardinality (count expected))]
          (if-not (every? established? children)
            (assoc parent :status :premises-open :reason :child-equality-open)
            ;; Encode the actual entries only. The known entries are selected
            ;; by the already established child results, not map equality.
            (let [dictionary (mapv (fn [[k v]] [k v]) (sort-by (comp str key) actual))
                  codes (into {} (map-indexed (fn [i [k _]] [k i]) dictionary))
                  expected-codes (mapv codes ks)
                  known (mapv #(codes (peek (:path %))) children)
                  residual (into [] (keep-indexed (fn [i [k _]]
                                                    (when-not (contains? expected k) i))) dictionary)
                  ordered (into known residual)
                  witness {:dictionary dictionary :known known :residual residual :actual ordered}
                  parent (assoc parent :partition witness)]
              (when-not (and (= (count dictionary) (count codes) (count actual) (count ordered))
                             (= (set ordered) (set (range (count dictionary))))
                             (= actual (into {} (mapv dictionary ordered))))
                (throw (ex-info "Invalid recursive entry encoding." site)))
              (if-not (= (count expected) (count actual))
                (assoc parent :status :premises-open :reason :cardinality-mismatch)
                (let [application (check-instance! (get lemmas (count expected)) parent)]
                  (assoc parent :status :composed-map-equality-checked
                         :application application
                         :encoded-composition (compose-parent! composition application
                                                               known expected-codes ordered))))))))
      (= expected actual) (assoc site :status :decoded-leaf-equality-checked)
      :else (assoc site :status :premises-open :reason :leaf-equality-open))))

(defn check-structure!
  "Compose concrete decoded child equalities bottom-up with the checked
   cardinality lemma. Leaf comparisons and the map encoding bridge are
   executable premises; the kernel composes encoded child-list equality
   and cardinality equality into the encoded parent equality.
   No parent comparison against the expected map is used as a premise."
  [original targets docs journal prefix actual]
  (let [abstract (check! original targets docs journal prefix)
        groups (get-in abstract [:rule-description :groups])
        composition {:constructor (proof-pilot! 'check-obligation! (constructor-obligation))
                     :transitivity (proof-pilot! 'check-obligation! (transitivity-obligation))}
        lemmas (into {} (map (juxt #(get-in % [:description :expected-cardinality]) identity))
                     (:checked abstract))]
    (when-not (and (map? actual) (= (set (keys actual)) (set (map :target groups)))
                   (every? #(and (vector? %) (every? map? %)) (vals actual)))
      (throw (ex-info "Expected complete selected decoded row bags." {})))
    (let [roots (mapv (fn [{:keys [target predicate]}]
                        (let [bag (get actual target) [kind expected] predicate]
                          (case kind
                            :absent (if (empty? bag)
                                      {:target target :status :empty-bag-checked}
                                      {:target target :status :premises-open :reason :unexpected-row})
                            :exact-row (if (= 1 (count bag))
                                         (compose-value! lemmas composition target [] expected (first bag) true)
                                         {:target target :status :premises-open :reason :row-cardinality-mismatch}))))
                      groups)]
      {:type ::recursive-cardinality-pilot :roots roots :composition-lemmas composition
       :status (if (and (seq roots) (every? established? roots))
                 :all-concrete-structures-composed :structural-premises-open)
       :scope :source-derived-concrete-decoded-structure-composition
       :open-obligations (into (:open-obligations abstract)
                               #{:general-decoded-map-encoding-refinement
                                 :decoded-leaf-equality-refinement
                                 :general-recursive-composition-soundness})
       :authority :none :certificate :not-issued :sql-refinement :not-established})))

(deftest recursive-structure-composes-child-results-before-parent-cardinality
  (let [[_ _ docs journal _ :as args] (fixture!)
        request [:document :requests "r1"]
        slot [:journal-slot "p2" :example/command]
        bags {request (:requests docs) [:document :assignments "a1"] []
              [:journal-slot "p1" :example/command] [] slot journal}
        run #(apply check-structure! (conj args %))
        report (with-redefs [partition/partition-map (fn [& _] (throw (ex-info "Unexpected whole-map partition" {})))]
                 (run bags))
        nodes #(mapcat (fn [root] (tree-seq (comp seq :children) :children root)) (:roots %))
        maps (filter #(= :composed-map-equality-checked (:status %)) (nodes report))
        nested [:gesso.command/binding :arguments :nested]
        leaf (conj nested :value)]
    (is (= :all-concrete-structures-composed (:status report)))
    (is (= 8 (count maps)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:sql-refinement report)))
    (is (contains? (:open-obligations report) :general-recursive-composition-soundness))
    (doseq [{:keys [children application encoded-composition]} maps]
      (is (every? established? children))
      (is (kernel/verifies? (ansatz/env) (:goal application) (:proof application)))
      (doseq [{:keys [goal proof dependencies]} (vals encoded-composition)]
        (is (kernel/verifies? (ansatz/env) goal proof))
        (is (empty? (:unresolved dependencies)))
        (is (empty? (:non-safe dependencies)))))
    (let [bad (run (assoc-in bags (into [slot 0] leaf) "changed"))
          by-path (into {} (map (juxt (juxt :target :path) identity)) (nodes bad))]
      (is (= :structural-premises-open (:status bad)))
      (is (= :leaf-equality-open (get-in by-path [[slot leaf] :reason])))
      (doseq [path [[] nested]]
        (is (= :child-equality-open (get-in by-path [[slot path] :reason])))
        (is (nil? (get-in by-path [[slot path] :application])))))
    (let [bad (run (assoc-in bags (into [slot 0] (conj nested :extra)) true))
          by-path (into {} (map (juxt (juxt :target :path) identity)) (nodes bad))]
      (is (= :structural-premises-open (:status bad)))
      (is (= :cardinality-mismatch (get-in by-path [[slot nested] :reason])))
      (is (= 1 (count (get-in by-path [[slot nested] :partition :residual]))))
      (is (every? established? (get-in by-path [[slot nested] :children])))
      (is (nil? (get-in by-path [[slot nested] :application])))
      (is (= :child-equality-open (get-in by-path [[slot []] :reason]))))
    (is (= :structural-premises-open (:status (run (assoc bags slot [])))))
    (is (thrown? clojure.lang.ExceptionInfo (run (dissoc bags slot))))))

(deftest arbitrary-cardinality-proof-specializes-without-size-specific-tactics
  (let [args (fixture!)
        proof-var (ns-resolve 'gesso.assurance.ansatz-reservation-pilot 'check-obligation!)
        original @proof-var
        calls (atom [])
        report (with-redefs-fn {proof-var (fn [ob] (swap! calls conj ob) (original ob))}
                 #(apply check! args))
        general (:general-lemma report)
        ob (general-cardinality-obligation)
        env (ansatz/env)]
    (is (= [ob] @calls))
    (is (= ['n ':- 'Nat] (subvec (:parameters ob) 0 3)))
    (is (= (:goal general) (proof-pilot! 'elaborate-source-proposition! ob)))
    (is (kernel/verifies? env (:goal general) (:proof general)))
    (is (empty? (get-in general [:dependencies :non-safe])))
    (is (empty? (get-in general [:dependencies :unresolved])))
    (doseq [checked (:checked report)]
      (let [n (get-in checked [:description :expected-cardinality])]
        (is (= (:proof checked) (expr/app* (:proof general) (expr/lit-nat n))))
        (is (kernel/verifies? env (:goal checked) (:proof checked)))))
    ;; Cardinalities outside the fixture (including zero) use exactly the
    ;; same universally quantified proof, with no additional tactic search.
    (doseq [n [0 37 1000]]
      (let [specialized (specialize-cardinality! general (obligation n []))]
        (is (= (:proof specialized) (expr/app* (:proof general) (expr/lit-nat n))))
        (is (kernel/verifies? env (:goal specialized) (:proof specialized)))))
    (let [wrong (obligation 37 [])
          wrong-goal (proof-pilot! 'elaborate-source-proposition! wrong)
          different (:proof (specialize-cardinality! general (obligation 38 [])))]
      (is (false? (kernel/verifies? env wrong-goal different))))
    (is (contains? (:open-obligations report) :general-structural-soundness))
    (is (= :not-established (:sql-refinement report)))
    (is (= :not-issued (:certificate report)))))

(deftest constructor-and-transitivity-compose-encoded-child-lists
  (let [constructor (proof-pilot! 'check-obligation! (constructor-obligation))
        transitivity (proof-pilot! 'check-obligation! (transitivity-obligation))
        env (ansatz/env)]
    (doseq [[ob checked] [[(constructor-obligation) constructor]
                          [(transitivity-obligation) transitivity]]]
      (is (= (:goal checked) (proof-pilot! 'elaborate-source-proposition! ob)))
      (is (kernel/verifies? env (:goal checked) (:proof checked)))
      (is (empty? (get-in checked [:dependencies :non-safe])))
      (is (empty? (get-in checked [:dependencies :unresolved])))
      (is (false? (kernel/verifies? env
                                    (proof-pilot! 'elaborate-source-proposition!
                                                  (update ob :parameters #(subvec % 0 (- (count %) 3))))
                                    (:proof checked)))))
    (doseq [codes [[] [0] [4 0 3] (vec (range 12))]]
      (let [{:keys [goal proof]} (compose-child-list! constructor codes codes)]
        (is (= goal (closed-goal (list '= '(List Int) (list-form codes) (list-form codes)))))
        (is (kernel/verifies? env goal proof))))
    (is (thrown? Exception (compose-child-list! constructor [0 1] [0 2])))
    (is (thrown? clojure.lang.ExceptionInfo (compose-child-list! constructor [0] [])))))

(defn- bridge-reject! [reason site]
  (throw (ex-info "Decoded prestate correspondence failed."
                  (assoc site :error/type ::decoded-correspondence :reason reason))))

(defn- check-decoded-node! [target path expected actual node]
  (let [site {:target target :path path}]
    (when-not (= site (select-keys node [:target :path]))
      (bridge-reject! :node-identity-mismatch site))
    (if (and (map? expected) (seq expected))
      (do
        (when-not (and (= :composed-map-equality-checked (:status node))
                       (map? actual) (every? keyword? (keys actual)))
          (bridge-reject! :map-evidence-open site))
        (let [ks (sort-by str (keys expected))
              children (:children node)
              dictionary (mapv (fn [[k v]] [k v]) (sort-by (comp str key) actual))
              ;; Look up entire entries, not merely their field names. Both
              ;; encodings must be faithful in this finite observation.
              codes (into {} (map-indexed (fn [i entry] [entry i]) dictionary))
              expected-entries (mapv (fn [k] [k (get expected k)]) ks)
              actual-entries (into (mapv (fn [k] [k (get actual k)]) ks)
                                   (remove (fn [[k _]] (contains? expected k))) dictionary)
              encode (fn [entries]
                       (mapv (fn [entry]
                               (if (contains? codes entry) (get codes entry)
                                   (bridge-reject! :entry-not-encoded site))) entries))]
          (when-not (and (vector? children) (= (count ks) (count children))
                         (every? #(contains? actual %) ks))
            (bridge-reject! :child-coverage-mismatch site))
          (let [child-results (mapv (fn [k child]
                                      (check-decoded-node! target (conj path k)
                                                           (get expected k) (get actual k) child))
                                    ks children)
                expected-codes (encode expected-entries)
                actual-codes (encode actual-entries)
                known (encode (subvec actual-entries 0 (count ks)))
                residual (subvec actual-codes (count ks))
                expected-witness {:dictionary dictionary :known known
                                  :residual residual :actual actual-codes}
                {:keys [goal proof]} (get-in node [:encoded-composition :parent])
                required-goal (closed-goal (list '= '(List Int) (list-form actual-codes)
                                                 (list-form expected-codes)))]
            (when-not (and (= (count dictionary) (count codes) (count actual))
                           (= expected-entries (mapv dictionary expected-codes))
                           (= actual-entries (mapv dictionary actual-codes))
                           (= expected (into {} expected-entries))
                           (= actual (into {} actual-entries))
                           (= (count actual-codes) (count (set actual-codes)))
                           (= (set actual-codes) (set (range (count dictionary))))
                           (= (:cardinality node) (count expected))
                           (= expected-witness (:partition node)))
              (bridge-reject! :entry-encoding-mismatch site))
            (when-not (and proof (= required-goal goal))
              (bridge-reject! :parent-goal-mismatch site))
            ;; Recheck the term and dependency policy, not the saved status
            ;; or the saved dependency report. The decoded bridge is checked
            ;; by Clojure, not by this kernel application.
            (checked-application! required-goal proof)
            (into [(assoc site :kind :map :expected-codes expected-codes
                          :actual-codes actual-codes)] cat child-results))))
      (if (and (= :decoded-leaf-equality-checked (:status node))
               (= expected actual) (not (contains? node :children)))
        [(assoc site :kind :leaf)]
        (bridge-reject! :leaf-evidence-mismatch site)))))

(defn check-decoded-correspondence!
  "Independently audit a successful concrete composition against its source,
   actual selected bags and exact encoded proof goals. Reconstruct both
   entry encodings by full entry value, recheck every parent proof and
   compare the resulting closed rows with the independent row predicate.
   This is an executable instance audit, not a universal codec theorem."
  [original targets docs journal prefix actual report]
  (let [description (semantics/describe! original targets docs journal)
        groups (:groups (rules/derive! original targets docs journal prefix))
        selected (set (keys (:predicates description)))
        roots (:roots report)]
    (when-not (and (= :all-concrete-structures-composed (:status report))
                   (map? actual) (= selected (set (keys actual)))
                   (every? #(and (vector? %) (every? map? %)) (vals actual))
                   (vector? roots) (= (mapv :target groups) (mapv :target roots)))
      (bridge-reject! :incomplete-structure {}))
    (let [checked (into [] cat
                        (mapv (fn [{:keys [target predicate]} root]
                                (let [[kind expected] predicate bag (get actual target)]
                                  (case kind
                                    :absent (if (and (empty? bag) (= :empty-bag-checked (:status root)))
                                              [{:target target :kind :absent}]
                                              (bridge-reject! :absence-evidence-mismatch {:target target}))
                                    :exact-row (if (= 1 (count bag))
                                                 (check-decoded-node! target [] expected (first bag) root)
                                                 (bridge-reject! :row-count-mismatch {:target target})))))
                              groups roots))
          actual-docs (reduce (fn [m [[kind table] bag]]
                                (if (= :document kind) (update m table into bag) m))
                              (zipmap (keys docs) (repeat [])) actual)
          actual-journal (into [] (mapcat val) (filter #(= :journal-slot (ffirst %)) actual))
          observed (semantics/observe! original targets docs journal description actual-docs actual-journal)]
      (when-not (:matches? observed)
        (bridge-reject! :independent-predicate-mismatch {}))
      {:type ::decoded-correspondence :status :decoded-instance-correspondence-checked
       :checked-sites checked :predicate-observation observed
       :decoded-observation {:document-rows actual-docs :journal-rows actual-journal}
       :scope :supplied-closed-decoded-rows :authority :none :certificate :not-issued
       :sql-refinement :not-established
       :open-obligations #{:general-decoded-map-encoding-refinement
                           :general-recursive-composition-soundness
                           :faithful-row-acquisition :transaction-internal-read-basis
                           :sql-value-and-struct-semantics :decoder-congruence}})))

(deftest concrete-decoded-correspondence-rechecks-entries-and-proof-goals
  (let [[_ _ docs journal _ :as args] (fixture!)
        request [:document :requests "r1"]
        slot [:journal-slot "p2" :example/command]
        bags {request (:requests docs) [:document :assignments "a1"] []
              [:journal-slot "p1" :example/command] [] slot journal}
        report (apply check-structure! (conj args bags))
        audit #(apply check-decoded-correspondence! (conj args %1 %2))
        checked (audit bags report)
        ri (first (keep-indexed (fn [i root] (when (= request (:target root)) i)) (:roots report)))
        si (first (keep-indexed (fn [i root] (when (= slot (:target root)) i)) (:roots report)))
        root [:roots ri]
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :decoded-instance-correspondence-checked (:status checked)))
    (is (:matches? (:predicate-observation checked)))
    (is (= 8 (count (filter #(= :map (:kind %)) (:checked-sites checked)))))
    (is (= 2 (count (filter #(= :absent (:kind %)) (:checked-sites checked)))))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))
    (is (= :not-established (:sql-refinement checked)))
    (doseq [site (filter #(= :map (:kind %)) (:checked-sites checked))]
      (is (= (:actual-codes site) (:expected-codes site))))
    (is (= :incomplete-structure (reason #(audit bags (update report :roots pop)))))
    (is (= :child-coverage-mismatch (reason #(audit bags (update-in report (conj root :children) pop)))))
    (is (= :entry-encoding-mismatch
           (reason #(audit bags (assoc-in report (into root [:partition :known]) [0 0 0])))))
    (is (= :entry-encoding-mismatch
           (reason #(audit bags (update-in report (into root [:partition :dictionary]) (fn [entries] (vec (reverse entries))))))))
    (is (= :parent-goal-mismatch
           (reason #(audit bags (assoc-in report (into root [:encoded-composition :parent])
                                          (get-in report [:roots si :encoded-composition :parent]))))))
    (is (thrown? Exception
                 (audit bags (assoc-in report (into root [:encoded-composition :parent :proof])
                                       (get-in report [:roots si :encoded-composition :parent :proof])))))
    (is (= :leaf-evidence-mismatch (reason #(audit (assoc-in bags [request 0 :item-count] 999) report))))
    (is (= :row-count-mismatch (reason #(audit (assoc bags slot []) report))))
    (is (= :incomplete-structure (reason #(audit (dissoc bags slot) report))))))

(defn check-projection-correspondence!
  "Recheck concrete structural evidence and independently project both closed
   decoded row sets using the actual typed mapped projection. This establishes
   equality of the two observed projections, not determinism of an arbitrary
   application decoder or correspondence to a transaction's internal basis."
  [original targets docs journal prefix actual report decode-outcome!]
  (let [audit (check-decoded-correspondence! original targets docs journal prefix actual report)
        {:keys [document-rows journal-rows]} (:decoded-observation audit)
        prepared (projection/project-state! original targets docs journal decode-outcome!)
        observed (projection/project-state! original targets document-rows journal-rows decode-outcome!)]
    (when-not (= prepared observed)
      (bridge-reject! :abstract-projection-mismatch {}))
    {:type ::projected-correspondence
     :status :decoded-instance-projection-checked
     :mapping-fingerprint (get-in audit [:predicate-observation :mapping-fingerprint])
     :prepared-state prepared :observed-state observed
     :checked-sites (:checked-sites audit)
     :predicate-observation (:predicate-observation audit)
     :scope :supplied-closed-decoded-rows-and-observed-projections
     :open-obligations (into (:open-obligations audit)
                             #{:shared-formatter-and-identifier-refinement
                               :general-projection-refinement})
     :authority :none :certificate :not-issued :sql-refinement :not-established}))

(deftest audited-structure-preserves-observed-abstract-projection
  (let [[original targets docs journal _] (fixture!)
        record (deref (ns-resolve 'gesso.model.mapped-projection-test 'record))
        docs (assoc docs :assignments [{:xt/id "a1" :owner "p1"}])
        journal (conj journal (record "other" "p1"))
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)
        args [original targets docs journal prefix]
        request [:document :requests "r1"]
        slot [:journal-slot "p2" :example/command]
        bags {request (:requests docs) [:document :assignments "a1"] (:assignments docs)
              [:journal-slot "p1" :example/command] [(second journal)] slot [(first journal)]}
        report (apply check-structure! (conj args bags))
        decoder (fn [row] (if (= "original" (get-in row [:gesso.command/binding :arguments :nested :value]))
                            :nested-accepted (get-in row [:gesso.command/outcome :result])))
        run #(apply check-projection-correspondence! (conj args %1 %2 %3))
        checked (run bags report decoder)
        expected {:example/count 2 :example/enabled false :example/owner "p1"
                  :example/one :accepted :example/two :nested-accepted}
        reason (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (is (= :decoded-instance-projection-checked (:status checked)))
    (is (= expected (:prepared-state checked) (:observed-state checked)))
    (is (= (:fingerprint (plan/check-target-mapping! original targets)) (:mapping-fingerprint checked)))
    (is (:matches? (:predicate-observation checked)))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))
    (is (= :not-established (:sql-refinement checked)))
    (doseq [obligation [:decoder-congruence :transaction-internal-read-basis
                        :shared-formatter-and-identifier-refinement :general-projection-refinement]]
      (is (contains? (:open-obligations checked) obligation)))
    ;; Equal rows do not ensure equal projections for a stateful decoder.
    (let [calls (atom 0)
          unstable (fn [_] (if (<= (swap! calls inc) 2) :accepted :rejected))]
      (is (= :abstract-projection-mismatch (reason #(run bags report unstable))))
      (is (= 4 @calls)))
    (is (= :invalid-occupied-slot-outcome (reason #(run bags report (constantly nil)))))
    (is (= :leaf-evidence-mismatch (reason #(run (assoc-in bags [request 0 :item-count] 999) report decoder))))
    (is (= :row-count-mismatch (reason #(run (assoc bags slot []) report decoder))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-mapped-prestate-cardinality-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
