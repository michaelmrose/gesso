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
            [gesso.assurance.proof.mapped-prestate-partition :as partition]
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

(defn- established? [node]
  (#{:decoded-leaf-equality-checked :composed-map-equality-checked
     :empty-bag-checked} (:status node)))

(defn- compose-value! [lemmas target path expected actual present?]
  (let [site {:target target :path path}]
    (cond
      (not present?) (assoc site :status :premises-open :reason :missing-field)
      (and (map? expected) (seq expected))
      (if-not (and (map? actual) (every? keyword? (keys actual)))
        (assoc site :status :premises-open :reason :unsupported-map-shape)
        (let [ks (sort-by str (keys expected))
              children (mapv (fn [k] (compose-value! lemmas target (conj path k)
                                                     (get expected k) (get actual k)
                                                     (contains? actual k))) ks)
              parent (assoc site :children children :cardinality (count expected))]
          (if-not (every? established? children)
            (assoc parent :status :premises-open :reason :child-equality-open)
            ;; Encode the actual entries only. The known entries are selected
            ;; by the already established child results, not map equality.
            (let [dictionary (mapv (fn [[k v]] [k v]) (sort-by (comp str key) actual))
                  codes (into {} (map-indexed (fn [i [k _]] [k i]) dictionary))
                  known (mapv codes ks)
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
                (assoc parent :status :composed-map-equality-checked
                       :application (check-instance! (get lemmas (count expected)) parent)))))))
      (= expected actual) (assoc site :status :decoded-leaf-equality-checked)
      :else (assoc site :status :premises-open :reason :leaf-equality-open))))

(defn check-structure!
  "Compose concrete decoded child equalities bottom-up with the checked
   cardinality lemma. Leaf comparisons and the map encoding bridge are
   executable premises; the kernel proves the encoded parent equality.
   No parent comparison against the expected map is used as a premise."
  [original targets docs journal prefix actual]
  (let [abstract (check! original targets docs journal prefix)
        groups (get-in abstract [:rule-description :groups])
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
                                         (compose-value! lemmas target [] expected (first bag) true)
                                         {:target target :status :premises-open :reason :row-cardinality-mismatch}))))
                      groups)]
      {:type ::recursive-cardinality-pilot :roots roots
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
    (doseq [{:keys [children application]} maps]
      (is (every? established? children))
      (is (kernel/verifies? (ansatz/env) (:goal application) (:proof application))))
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

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-mapped-prestate-cardinality-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
