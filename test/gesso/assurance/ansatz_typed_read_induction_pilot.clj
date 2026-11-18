(ns gesso.assurance.ansatz-typed-read-induction-pilot
  "Opt-in v1750: source-derived conditional induction with typed :read effects.

   For the ORIGINAL Gesso operation catalog, derive every tracked literal,
   framed and typed :read effect from its guarded replacement constructor.
   Each read-written invariant contributes a SEPARATE source-value predicate
   to that operation's step relation. Type correctness alone NEVER supplies
   range or proper-enum-subset constraints. The universal step-exhaustiveness
   premise is assumed, not established by the proof.

   This test-only pilot checks the source-derived initialization predicate,
   the complete operation/invariant preservation product and quantified
   Nat-indexed induction. It does not establish guard, translation/JVM,
   physical XTDB, writer-closure or actual initial-state correspondence.
   No production authority or downstream Typed Clojure/Ansatz requirement."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.walk :as walk]
            [gesso.assurance.ansatz-typed-read-pilot]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-mixed-initialization-pilot]
            [gesso.assurance.ansatz-mixed-induction-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]))

(def ^:private read-ns 'gesso.assurance.ansatz-typed-read-pilot)
(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private init-ns 'gesso.assurance.ansatz-mixed-initialization-pilot)
(def ^:private induction-ns 'gesso.assurance.ansatz-mixed-induction-pilot)
(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private open-assumptions
  #{:actual-initial-database-state-matches-declaration
    :every-installed-write-refines-declared-input-aware-step-or-stutters
    :actual-input-values-satisfy-declared-read-preconditions
    :prestate-read-values-correspond-to-source-state
    :complete-physical-writer-catalog
    :source-expression-translation-and-enum-encoding-correctness
    :original-interpreter-jvm-and-xtdb-transaction-correspondence
    :ansatz-kernel-and-permitted-axioms})

(defn- reject! [reason detail]
  (throw (ex-info "Unsupported typed-read induction proof" {:reason reason :details detail})))

(defn- pilot! [ns-name name & args]
  (let [v (ns-resolve ns-name name)]
    (when-not v (reject! :missing-pilot-contract {:ns ns-name :name name}))
    (apply @v args)))

(defn- conjunction [expressions]
  (if (= 1 (count expressions))
    (first expressions)
    (pilot! init-ns 'conjunction (vec expressions))))

(defn- pattern [names]
  (if (= 1 (count names)) (first names)
      (pilot! init-ns 'pattern (vec names))))

(defn- eq-int [a b] (list '= 'Int a b))
(defn- at [f k] (list f k))

(defn- catalog-source! [catalog]
  ;; These original constraints must be checked BEFORE any proof compilation.
  ;; The v1749 catalog is exactly the three-key source used by its proof gate;
  ;; add only declared-operation IDs and an initial predicate here.
  (let [base (dissoc catalog :initial-condition :declared-operation-ids)
        {:keys [sources obligations checked unproved-read-targets] :as reads}
        (pilot! read-ns 'check-read-proofs! base)
        ids (mapv :id sources)
        declared (:declared-operation-ids catalog)
        invs (into (sorted-map)
                   (for [[id rule] (:invariants catalog)]
                     [id (pilot! preservation-ns 'invariant! id rule)]))
        fields (vec (sort (set (map :field (vals invs)))))
        types (select-keys (get-in sources [0 :source :state-types]) fields)
        init (:initial-condition catalog)
        expr (:expression init)
        leaves (when (and (vector? expr) (= :and (first expr)))
                 (vec (rest expr)))
        originals (mapv (comp :expression :rule) (vals invs))
        lookup (into {} (map (juxt (comp :expression :rule) :invariant-id) (vals invs)))
        ordered (mapv lookup leaves)
        full-id (:fingerprint (subject/describe-subject
                               {:kind :rule
                                :id :gesso.assurance.ansatz-pilot/read-induction
                                :interface catalog}))]
    (when-not (and (= #{:id :invariants :operations
                        :declared-operation-ids :initial-condition}
                      (set (keys catalog)))
                   (set? declared) (= declared (set ids))
                   (= (count ids) (count declared))
                   (<= 1 (count ids) 8)
                   (<= 2 (count invs) 10)
                   (= (count originals) (count (set originals)))
                   (= (frequencies originals) (frequencies leaves))
                   (= (set fields) (:readers init))
                   (= #{:readers :expression} (set (keys init)))
                   (= (count ordered) (count invs))
                   (= (set ordered) (set (keys invs)))
                   (= (set fields) (set (keys types)))
                   (every? #(= types (select-keys (get-in % [:source :state-types]) fields))
                           sources)
                   (= (count obligations) (count checked)))
      (reject! :incomplete-read-induction-catalog
               {:ids ids :declared declared :ordered ordered
                :leaves leaves :expected originals}))
    (rule-type/check-rule-type! init types)
    (doseq [inv (vals invs) src sources]
      (pilot! preservation-ns 'state-field-type! inv (:construction src)))
    {:fingerprint full-id :source-read-report reads
     :sources sources :invariants invs :ordered ordered
     :fields fields :field-types types :initial-source init
     :unproved-read-targets unproved-read-targets}))

(defn- proposition [source inv-id value]
  (let [inv (get-in source [:invariants inv-id])]
    (case (:kind inv)
      :signed-int-bound (pilot! preservation-ns 'comparison
                                (:op inv) value (:bound inv))
      :finite-enum-membership
      (let [enc (pilot! preservation-ns 'state-field-type!
                        inv {:state-types (:field-types source)})]
        (pilot! preservation-ns 'enum-membership
                (:codes enc) (:members inv) value))
      (reject! :unsupported-invariant-kind {:id inv-id :kind (:kind inv)}))))

(defn- witness! [source]
  (let [tracked (assoc source :field-names (:fields source))
        witness (pilot! init-ns 'tracked-witness! tracked)
        literal (fn [inv-id]
                  (let [field (get-in source [:invariants inv-id :field])
                        value (pilot! init-ns 'witness-proposition-value
                                      (assoc tracked :state-types (:field-types source))
                                      field (get witness field))]
                    (proposition source inv-id value)))
        forms (mapv literal (:ordered source))
        script (vec (mapcat (fn [i id]
                              (concat (when (< i (dec (count (:ordered source))))
                                        ['(constructor)])
                                      (pilot! init-ns 'witness-leaf-tactics
                                              (assoc tracked :state-types (:field-types source))
                                              id witness)))
                            (range (count (:ordered source))) (:ordered source)))]
    {:obligation-kind :typed-read-initial-predicate-inhabited
     :catalog-fingerprint (:fingerprint source)
     :witness-projection witness
     :parameters [] :goal-form (conjunction forms) :proof-script script}))

(defn- initial-proofs! [source]
  (let [names (zipmap (:fields source)
                      (map #(symbol (str "initial" %)) (range)))
        forms (mapv #(proposition source %
                                 (get names (get-in source [:invariants % :field])))
                    (:ordered source))
        hnames (mapv #(symbol (str "hi" %)) (range (count forms)))
        context (vec (concat (mapcat (fn [field]
                                      [(get names field) ':- 'Int]) (:fields source))
                             ['hinit ':- (conjunction forms)]))
        obligations (vec (concat
                          (map-indexed
                           (fn [i id]
                             {:obligation-kind :typed-read-initial-entailment
                              :invariant-id id
                              :catalog-fingerprint (:fingerprint source)
                              :parameters context
                              :goal-form (nth forms i)
                              :proof-script [(list 'rcases 'hinit (pattern hnames))
                                             (list 'exact (nth hnames i))]})
                           (:ordered source))
                          [(witness! source)]))]
    (mapv #(pilot! proof-ns 'check-obligation! %) obligations)))

(defn- described-invariant-pairs! [source]
  ;; Combine all v1749 checked read proofs with newly checked framed-field
  ;; preservation. No operation/invariant pair may disappear silently.
  (let [read-proofs (:checked (:source-read-report source))
        by-pair (into {} (map (fn [proof]
                                (let [d (:description proof)]
                                  [[(:operation-id d) (:invariant-id d)] proof]))
                              read-proofs))
        pairs (for [op (:sources source) inv-id (:ordered source)]
                [op (get-in source [:invariants inv-id])])
        frames (vec (for [[op inv] pairs
                          :let [field (:field inv)]
                          :when (contains? (get-in op [:semantics :frame-fields]) field)]
                      (let [pre (proposition source (:invariant-id inv) 'pre)
                            post (proposition source (:invariant-id inv) 'post)]
                        {:obligation-kind :typed-read-framed-invariant-preservation
                         :catalog-fingerprint (:fingerprint source)
                         :transition-fingerprint (:transition-fingerprint op)
                         :operation-id (:id op) :invariant-id (:invariant-id inv)
                         :parameters ['pre ':- 'Int 'post ':- 'Int
                                      'hpre ':- pre
                                      'heffect ':- (eq-int 'post 'pre)]
                         :goal-form post
                         :proof-script (if (= :finite-enum-membership (:kind inv))
                                         '[(rw [heffect]) (exact hpre)]
                                         '[(omega)])})))
        framed-checked (mapv #(pilot! proof-ns 'check-obligation! %) frames)
        pairs-checked (into by-pair
                            (for [p framed-checked
                                  :let [d (:description p)]]
                              [[(:operation-id d) (:invariant-id d)] p]))
        expected (set (for [op (:sources source) id (:ordered source)]
                        [(:id op) id]))]
    (when-not (and (= (count pairs-checked) (+ (count read-proofs) (count framed-checked)))
                   (= expected (set (keys pairs-checked)))
                   (every? (fn [[op inv]]
                             (let [effect (get-in op [:semantics :assignments (:field inv)])
                                   framed? (contains? (get-in op [:semantics :frame-fields])
                                                      (:field inv))]
                               (or (and framed? (nil? effect))
                                   (= :read (first effect)))))
                           pairs)
                   (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                           (vals pairs-checked)))
      (reject! :incomplete-typed-read-invariant-product
               {:expected expected :observed (set (keys pairs-checked))}))
    {:checked pairs-checked :read-count (count read-proofs)
     :frame-count (count framed-checked)}))

(defn- trace-sources! [source]
  ;; Non-invariant READ writes (such as :ticket/owner) are explicitly reported
  ;; as unproved, never silently counted in the invariant preservation product.
  (let [ops (:sources source)
        tracked (set (:fields source))
        all-readers
        (set (for [op ops field tracked
                   :let [assignment (get-in op [:semantics :assignments field])]
                   :when (= :read (first assignment))]
               (let [reader (second assignment)
                     origin (if (contains? (get-in op [:source :input-types]) reader)
                              :input :prestate)]
                 [origin reader])))
        symbol-map (zipmap (sort all-readers)
                           (map #(symbol (str "readtrace" %)) (range)))
        op-effects
        (mapv
         (fn [op]
           (let [effects
                 (into {} (for [field (:fields source)]
                            (let [assignment (get-in op [:semantics :assignments field])
                                  framed? (contains? (get-in op [:semantics :frame-fields]) field)]
                              [field
                               (cond
                                 (and (nil? assignment) framed?)
                                 {:kind :framed}
                                 (and (= :read (first assignment)) (not framed?))
                                 (let [reader (second assignment)
                                       origin (if (contains? (get-in op [:source :input-types]) reader)
                                                :input :prestate)
                                       read-id [origin reader]
                                       reader-fn (get symbol-map read-id)]
                                   (when-not reader-fn
                                     (reject! :missing-reader-trace read-id))
                                   {:kind :read :reader reader :reader-origin origin
                                    :reader-id read-id :reader-function reader-fn})
                                 :else (reject! :unsupported-tracked-effect
                                                {:operation (:id op) :field field
                                                 :assignment assignment :framed? framed?}))])))]
             {:operation-id (:id op)
              :transition-fingerprint (:transition-fingerprint op)
              :effects effects}))
         ops)]
    (when-not (and (= (mapv :id ops) (mapv :operation-id op-effects))
                   (= (:fields source) (vec (sort tracked)))
                   (every? #(= tracked (set (keys (:effects %)))) op-effects))
      (reject! :source-trace-field-coverage-mismatch {:effects op-effects}))
    {:read-functions symbol-map :operations op-effects}))

(defn- trace-obligation! [source]
  (let [proven (described-invariant-pairs! source)
        traces (trace-sources! source)
        fields (:fields source)
        field-traces (zipmap fields (map #(symbol (str "trace" %)) (range)))
        readers (:read-functions traces)
        at-value (fn [field k] (at (get field-traces field) k))
        inv-at (fn [k]
                 (conjunction (mapv #(proposition source %
                                                   (at-value (get-in source
                                                                     [:invariants % :field]) k))
                                    (:ordered source))))
        branch-source
        (fn [i {:keys [effects] :as op}]
          (let [k 'k
                post (list 'Nat.succ k)
                writes
                (mapv (fn [field]
                        (let [{:keys [kind reader-function]} (get effects field)
                              rhs (case kind
                                    :framed (at-value field k)
                                    :read (at reader-function k)
                                    (reject! :unsupported-trace-effect {:field field :kind kind}))]
                          (eq-int (at-value field post) rhs)))
                      fields)
                source-conditions
                (vec (for [inv-id (:ordered source)
                           :let [inv (get-in source [:invariants inv-id])
                                 {:keys [kind reader reader-origin reader-function]}
                                 (get effects (:field inv))]
                           :when (= :read kind)
                           :let [checked (get-in proven [:checked [(:operation-id op) inv-id]])
                                 description (:description checked)
                                 original (first (filter #(= (:operation-id op) (:id %))
                                                         (:sources source)))
                                 _ (when-not (and checked
                                                  (= :conditional-typed-read-invariant-preservation
                                                     (:obligation-kind description))
                                                  (= inv-id (:invariant-id description))
                                                  (= reader (:reader description))
                                                  (= reader-origin (:reader-origin description))
                                                  (= (:transition-fingerprint original)
                                                     (:transition-fingerprint description))
                                                  (= (get-in source [:source-read-report
                                                                     :catalog-fingerprint])
                                                     (:catalog-fingerprint description)))
                                     (reject! :unbound-read-premise-proof
                                              {:operation (:operation-id op)
                                               :invariant inv-id :reader reader}))]]
                       {:invariant-id inv-id
                        :reader reader :reader-origin reader-origin
                        :operation-id (:operation-id op)
                        :formula (proposition source inv-id (at reader-function k))}))
                forms (vec (concat writes (map :formula source-conditions)))
                names (mapv #(symbol (str "h" i "_" %)) (range (count forms)))
                source-hyp (into {} (map (fn [index {:keys [invariant-id]}]
                                          [invariant-id (nth names (+ (count writes) index))])
                                        (range) source-conditions))]
            {:operation-id (:operation-id op)
             :effects effects :writes writes
             :source-conditions source-conditions
             :branch-form (conjunction forms)
             :pattern (pattern names)
             :write-hyp (zipmap fields (take (count fields) names))
             :source-hyp source-hyp}))
        branches (mapv branch-source (range) (:operations traces))
        stutter-effects (into {} (map (fn [field] [field {:kind :framed}]) fields))
        stutter (branch-source (count branches)
                               {:operation-id nil :effects stutter-effects})
        all-branches (conj branches stutter)
        case-pattern (reduce (fn [right left] (list 'or left right))
                             (:pattern (last all-branches))
                             (reverse (map :pattern (butlast all-branches))))
        prior-names (mapv #(symbol (str "hi" %)) (range (count (:ordered source))))
        induction-script
        (vec
         (concat
          '[(induction n) (exact hinit) (rename_i k ih)]
          [(list 'rcases 'ih (pattern prior-names))
           '(specialize hsteps k)
           (list 'rcases 'hsteps case-pattern)]
          (mapcat
           (fn [{:keys [effects write-hyp source-hyp]}]
             (mapcat (fn [i inv-id]
                       (let [inv (get-in source [:invariants inv-id])
                             field (:field inv)
                             {:keys [kind]} (get effects field)
                             hw (get write-hyp field)
                             hs (get source-hyp inv-id)
                             hp (nth prior-names i)]
                         (concat
                          (when (< i (dec (count (:ordered source)))) ['(constructor)])
                          (case (:kind inv)
                            :signed-int-bound ['(omega)]
                            :finite-enum-membership
                            (case kind
                              :framed [(list 'rw [hw]) (list 'exact hp)]
                              :read [(list 'rw [hw]) (list 'exact hs)]
                              (reject! :unsupported-enum-branch-kind {:kind kind}))
                            (reject! :unsupported-branch-invariant {:id inv-id})))))
                     (range (count (:ordered source))) (:ordered source)))
           all-branches)))
        step (pilot! induction-ns 'disjunction (mapv :branch-form all-branches))
        params (vec (concat
                     (mapcat (fn [field] [(get field-traces field) ':- '(=> Nat Int)]) fields)
                     (mapcat (fn [[_ sym]] [sym ':- '(=> Nat Int)]) (sort-by key readers))
                     ['n ':- 'Nat
                      'hinit ':- (inv-at 0)
                      'hsteps ':- (list 'forall ['k ':- 'Nat] step)]))
        declared (set (map :id (:sources source)))
        source-pairs (set (for [op (:sources source) inv-id (:ordered source)]
                            [(:id op) inv-id]))]
    (when-not (and (= declared (set (map :operation-id branches)))
                   (= (mapv :id (:sources source)) (mapv :operation-id branches))
                   (= (count branches) (count declared))
                   (= source-pairs (set (keys (:checked proven))))
                   (= (count source-pairs) (count (:checked proven)))
                   (every? #(= (set fields) (set (keys (:effects %)))) branches))
      (reject! :incomplete-typed-read-induction-source-coverage {:declared declared}))
    {:obligation-kind :source-derived-conditional-typed-read-induction
     :source-fingerprint (:fingerprint source)
     :operation-ids (mapv :operation-id branches)
     :invariant-ids (:ordered source)
     :read-functions readers
     :read-premises (vec (mapcat :source-conditions branches))
     :unproved-read-targets (:unproved-read-targets source)
     :read-preservation-proofs (:read-count proven)
     :framed-preservation-proofs (:frame-count proven)
     :step-exhaustiveness :explicit-premise-not-proved
     :source-read-preconditions :explicit-step-premises-not-established-physically
     :parameters params
     :goal-form (inv-at 'n)
     :proof-script induction-script}))

(defn- check-induction! [catalog]
  (let [source (catalog-source! catalog)
        init-checked (initial-proofs! source)
        obligation (trace-obligation! source)
        checked (pilot! proof-ns 'check-obligation! obligation)]
    (when-not (and (= (:fingerprint source)
                      (get-in checked [:description :source-fingerprint]))
                   (kernel/verifies? (ansatz/env) (:goal checked) (:proof checked))
                   (every? #(and (kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                                 (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                    (get-in % [:dependencies :policy]))
                                 (empty? (get-in % [:dependencies :non-safe]))
                                 (empty? (get-in % [:dependencies :unresolved]))) init-checked)
                   (= :ansatz-reservation-pilot/lean-core-axioms-v1
                      (get-in checked [:dependencies :policy]))
                   (empty? (get-in checked [:dependencies :non-safe]))
                   (empty? (get-in checked [:dependencies :unresolved])))
      (reject! :unqualified-typed-read-induction {:checked checked}))
    {:kind :conditional-typed-read-trace-induction
     :catalog-fingerprint (:fingerprint source)
     :operation-ids (:operation-ids obligation)
     :read-functions (:read-functions obligation)
     :read-premises (:read-premises obligation)
     :initialization-proofs (count init-checked)
     :read-preservation-proofs (:read-preservation-proofs obligation)
     :framed-preservation-proofs (:framed-preservation-proofs obligation)
     :trace-proof checked
     :unproved-read-targets (:unproved-read-targets obligation)
     :step-exhaustiveness (:step-exhaustiveness obligation)
     :source-read-preconditions (:source-read-preconditions obligation)
     :open-assumptions open-assumptions
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- ticket-catalog []
  (let [base (pilot! read-ns 'example-catalog)
        initial (pilot! init-ns 'initialized base)]
    (assoc initial :declared-operation-ids (set (map :id (:operations base))))))

(deftest ticket-input-and-prestate-reads-have-finite-trace-induction
  (let [result (check-induction! (ticket-catalog))]
    (is (= [:ticket/assign :ticket/restore] (:operation-ids result)))
    (is (= 5 (:read-preservation-proofs result)))
    (is (= 1 (:framed-preservation-proofs result)))
    (is (= 4 (:initialization-proofs result)))
    (is (= 1 (count (filter #(= :ticket/channel-domain (:invariant-id %))
                            (:read-premises result)))))
    (is (= 5 (count (:read-premises result))))
    (is (= #{[:ticket/assign :ticket/owner]} (:unproved-read-targets result)))
    (is (= :explicit-premise-not-proved (:step-exhaustiveness result)))
    (is (= :explicit-step-premises-not-established-physically
           (:source-read-preconditions result)))
    (is (kernel/verifies? (ansatz/env) (get-in result [:trace-proof :goal])
                          (get-in result [:trace-proof :proof])))
    (is (= :none (:authority result)))))

(deftest exact-input-premises-are-part-of-the-source-derived-step-formula
  (let [source (catalog-source! (ticket-catalog))
        obligation (trace-obligation! source)
        universal (last (:parameters obligation))
        step (nth universal 2)
        printed (pr-str step)]
    (is (= 'forall (first universal)))
    (is (= 5 (count (:read-premises obligation))))
    (is (.contains printed "readtrace"))
    (is (.contains printed ">= Int"))
    (is (.contains printed "<= Int"))
    (is (.contains printed "Or"))
    (is (= :explicit-premise-not-proved (:step-exhaustiveness obligation)))))

(deftest unsafe-well-typed-input-or-backup-must-not-be-assumed-safe
  (let [base (ticket-catalog)
        observe (:observe! (pilot! read-ns 'audited-read-boundary!
                                  (dissoc base :declared-operation-ids :initial-condition)))
        [before inputs after] (pilot! read-ns 'assign-witness)
        bad (assoc inputs :input/count 8)
        bad-after (assoc after :ticket/count 8)
        saved (assoc before :ticket/status :claimed :ticket/backup -1)]
    (is (= :source-invariant-precondition-failed
           (try (observe :ticket/assign before bad bad-after) nil
                (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex))))))
    (is (= :source-invariant-precondition-failed
           (try (observe :ticket/restore saved {} (assoc saved :ticket/count -1)) nil
                (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex))))))))

(deftest typed-input-alone-does-not-prove-bounded-inductive-safety
  ;; A type declaration says Int, not that the copied value is <= 5.
  ;; A mathematical implication without the hsource condition is false.
  (let [unsafe {:parameters ['src ':- 'Int 'post ':- 'Int
                             'hwrite ':- '(= Int post src)]
                :goal-form '(<= Int post 5)
                :proof-script '[(omega)]}
        catalog (ticket-catalog)
        source (catalog-source! catalog)
        read-proofs (get-in source [:source-read-report :checked])
        explicit (filter #(= :ticket/count-max
                              (get-in % [:description :invariant-id])) read-proofs)]
    (is (= 2 (count explicit)))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) explicit))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pilot! proof-ns 'check-obligation! unsafe)))
    (is (false? (pilot! read-ns 'source-predicate-holds?
                        (get-in source [:invariants :ticket/count-max]) 8)))
    (is (false? (pilot! read-ns 'source-predicate-holds?
                        (get-in source [:invariants :ticket/channel-domain])
                        :unknown/channel)))))

(deftest narrowed-enum-subsets-and-signed-bounds-compose-conditionally
  (let [base (ticket-catalog)
        subset-rule [:in [:read :ticket/channel] [:literal #{:web}]]
        subset (-> base
                   (assoc-in [:invariants :ticket/channel-domain :expression]
                             subset-rule)
                   (assoc-in [:initial-condition :expression 3]
                             subset-rule))
        lower-rule [:>= [:read :ticket/count] [:literal -2]]
        upper-rule [:<= [:read :ticket/count] [:literal 3]]
        signed (-> base
                   (assoc-in [:invariants :ticket/count-min :expression] lower-rule)
                   (assoc-in [:invariants :ticket/count-max :expression] upper-rule)
                   (assoc-in [:initial-condition :expression 1] lower-rule)
                   (assoc-in [:initial-condition :expression 2] upper-rule))]
    (doseq [catalog [subset signed]]
      (let [report (check-induction! catalog)]
        (is (= 5 (:read-preservation-proofs report)))
        (is (= 5 (count (:read-premises report))))
        (is (kernel/verifies? (ansatz/env)
                              (get-in report [:trace-proof :goal])
                              (get-in report [:trace-proof :proof])))
        (is (= :none (:authority report)))))
    (is (not= (:catalog-fingerprint (check-induction! base))
              (:catalog-fingerprint (check-induction! subset))))))

(deftest modified-read-declarations-are-new-or-unsupported-proof-subjects
  (let [catalog (ticket-catalog)
        initial (check-induction! catalog)
        changed-guard (assoc-in catalog [:operations 0 :construction
                                         :guard :expression 2 1] :claimed)
        changed (check-induction! changed-guard)]
    (is (= 5 (:read-preservation-proofs initial)
           (:read-preservation-proofs changed)))
    (is (not= (:catalog-fingerprint initial) (:catalog-fingerprint changed)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check-induction!
                  (assoc-in catalog [:operations 0 :construction
                                     :input-types :input/count] {:type :keyword}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check-induction!
                  (assoc-in catalog [:operations 0 :construction
                                     :replacements :ticket/count]
                            [:read :input/undefined]))))))

(deftest dropping-an-operation-or-read-premise-fails-closed
  (let [catalog (ticket-catalog)
        source (catalog-source! catalog)
        original (trace-obligation! source)
        denier (fn [x] (try (check-induction! x) nil
                            (catch clojure.lang.ExceptionInfo ex
                              (:reason (ex-data ex)))))]
    (is (= :incomplete-read-induction-catalog
           (denier (update catalog :operations pop))))
    (is (= :incomplete-read-induction-catalog
           (denier (update-in catalog [:initial-condition :expression] pop))))
    (let [f (ns-resolve 'gesso.assurance.ansatz-typed-read-induction-pilot 'trace-sources!)
          real @f]
      (with-redefs-fn {f (fn [s] (update (real s) :operations pop))}
        #(is (= :incomplete-typed-read-induction-source-coverage
                (try (trace-obligation! source) nil
                     (catch clojure.lang.ExceptionInfo ex
                       (:reason (ex-data ex))))))))
    ;; A once-typed integer input can exceed its target upper bound. If
    ;; even ONE source predicate is silently weakened, the same universal
    ;; theorem must cease to be provable. This checks proof meaning rather
    ;; than merely checking that the generated tactic mentions a hypothesis.
    (let [selected (first (filter #(and (= :ticket/assign (:operation-id %))
                                        (= :ticket/count-max (:invariant-id %)))
                                  (:read-premises original)))
          params (:parameters original)
          step (nth (last params) 2)
          weakened-step (walk/postwalk-replace
                         {(:formula selected) '(= Int 0 0)} step)
          weakened (assoc original :parameters
                          (conj (pop params)
                                (list 'forall '[k :- Nat] weakened-step)))]
      (is selected)
      (is (not= step weakened-step))
      (is (thrown? clojure.lang.ExceptionInfo
                   (pilot! proof-ns 'check-obligation! weakened))))
    ;; Deleting universal step exhaustiveness is not an inductive proof.
    (let [without-step (update original :parameters
                               #(subvec % 0 (- (count %) 3)))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (pilot! proof-ns 'check-obligation! without-step))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-typed-read-induction-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
