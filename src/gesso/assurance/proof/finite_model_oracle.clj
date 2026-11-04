(ns gesso.assurance.proof.finite-model-oracle
  "Independent, bounded, DATA-ONLY semantic oracle for the v1239 guarded
   replacement fragment (v1309). This is a *test oracle*, not a proof checker,
   certificate, or admission path.

   Input (EXACT top-level keys):
     {:catalog <original v1278 induction catalog>
      :state-domains {:request/status [:open :claimed]
                      :request/owner [absent nil :person/alice]}
      :input-domains {:request/claim {:input/actor [:person/alice]}}
      :max-cases 10000}

   `absent` below is a non-scalar vector marker; it denotes omission of a
   map key, and is never a permitted field value. Domain vectors explicitly
   enumerate finite *representatives*: integer, keyword and string types are
   not thereby proven exhaustively covered. Enum/Boolean full-domain coverage
   is reported separately. The oracle checks BASE Init(s) => I(s) and STEP
   AllInvariants(s) & Guard_o(s,x) => AllInvariants(update_o(s,x)), for every
   enumerated typed prestate and input. It requires the updated state to stay
   in the declared finite universe before reporting bounded success.

   This implementation does NOT require or invoke gesso.assurance.rule-eval,
   gesso.assurance.rule-type, proof.transition, proof.induction, proof.kernel,
   proof initial/preservation inference, or any admission mechanism. Its
   validation and interpretation are deliberately separate and restricted.
   A positive result is only agreement with an enumerated FINITE model;
   no universal inductive theorem, physical correspondence, or runtime
   authority follows. Reports are freely copyable diagnostic data."
  (:require [clojure.set :as set]))

(def oracle-version 1)
(def report-type :gesso.assurance.proof.finite-model-oracle/bounded-diagnostic)
(def absent [:gesso.assurance.proof.finite-model-oracle/absent])

(def ^:private max-allowed-cases 100000)
(def ^:private max-state-count 10000)
(def ^:private max-fields 16)
(def ^:private max-operations 8)
(def ^:private max-invariants 16)
(def ^:private max-domain-size 32)
(def ^:private max-expression-nodes 512)
(def ^:private max-expression-depth 32)
(def ^:private max-literal-size 2048)

(defn- diagnostic! [status reason at]
  (throw (ex-info "Finite oracle cannot establish a bounded judgment."
                  {:finite-model-oracle/diagnostic true
                   :status status :reason reason :at at})))

(defn- invalid! [reason at] (diagnostic! :invalid-source reason at))
(defn- unsupported! [reason at] (diagnostic! :unsupported-language reason at))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))
(defn- plain-vector? [x] (instance? clojure.lang.APersistentVector x))
(defn- plain-set? [x] (instance? clojure.lang.PersistentHashSet x))
(defn- qualified? [x] (and (keyword? x) (some? (namespace x))))

(defn- no-meta! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (invalid! :metadata-not-allowed at))
  x)

(defn- exact-map! [x expected at]
  (when-not (plain-map? x) (invalid! :expected-plain-map at))
  (no-meta! x at)
  (when-not (= expected (set (keys x)))
    (invalid! :wrong-keys at))
  x)

(defn- keyword-set! [x at]
  (when-not (plain-set? x) (invalid! :expected-keyword-set at))
  (no-meta! x at)
  (when-not (every? qualified? x) (invalid! :invalid-qualified-id at))
  x)

(defn- scalar? [v]
  (or (nil? v) (instance? Boolean v) (integer? v)
      (string? v) (keyword? v)))

(defn- literal! [x at]
  (if (plain-set? x)
    (do (no-meta! x at)
        (when (or (empty? x) (> (count x) 256)
                  (not (every? scalar? x)))
          (unsupported! :unsupported-set-literal at))
        (doseq [v x] (literal! v at)))
    (when (or (not (scalar? x))
              (and (or (string? x) (keyword? x) (integer? x))
                   (> (count (str x)) max-literal-size)))
      (unsupported! :unsupported-literal at)))
  x)

(defn- descriptor! [id d at]
  (when-not (qualified? id) (invalid! :invalid-qualified-id at))
  (when-not (plain-map? d) (invalid! :invalid-type-descriptor at))
  (no-meta! d at)
  (when (seq (set/difference (set (keys d))
                             #{:type :name :values :optional? :nullable?}))
    (invalid! :unknown-type-keys at))
  (when-not (#{:enum :boolean :integer :string :keyword} (:type d))
    (unsupported! :unsupported-field-type at))
  (doseq [flag [:optional? :nullable?]]
    (when (and (contains? d flag) (not (instance? Boolean (get d flag))))
      (invalid! :invalid-type-flag at)))
  (if (= :enum (:type d))
    (when-not (and (qualified? (:name d)) (plain-set? (:values d))
                   (seq (:values d)) (<= (count (:values d)) 256)
                   (every? keyword? (:values d)))
      (invalid! :invalid-enum-descriptor at))
    (when (or (contains? d :name) (contains? d :values))
      (invalid! :unexpected-nonenum-keys at)))
  (when (= :enum (:type d))
    (no-meta! (:values d) (conj at :values))
    (when (or (> (count (str (:name d))) max-literal-size)
              (some #(> (count (str %)) max-literal-size) (:values d)))
      (unsupported! :oversize-enum-identifier at)))
  d)

(defn- types! [x at]
  (when-not (plain-map? x) (invalid! :expected-type-map at))
  (no-meta! x at)
  (when (> (count x) max-fields) (unsupported! :too-many-fields at))
  (doseq [[k d] x] (descriptor! k d (conj at k)))
  x)

(defn- type-valid? [d v]
  (and (not= v absent)
       (if (nil? v)
         (true? (:nullable? d))
         (case (:type d)
           :enum (contains? (:values d) v)
           :boolean (instance? Boolean v)
           :integer (integer? v)
           :string (string? v)
           :keyword (keyword? v)
           false))
       (or (nil? v) (<= (count (str v)) max-literal-size))))

(defn- domain! [d entries at]
  (exact-map! entries (set (keys d)) at)
  (doseq [[id values] entries]
    (let [descriptor (get d id)]
      (when-not (and (plain-vector? values)
                     (<= 1 (count values) max-domain-size))
        (invalid! :invalid-domain-vector (conj at id)))
      (no-meta! values (conj at id))
      (when-not (= (count values) (count (distinct values)))
        (invalid! :duplicate-domain-value (conj at id)))
      (doseq [v values]
        (cond
          (= v absent) (do (no-meta! v (conj at id :absent))
                           (when-not (true? (:optional? descriptor))
                             (invalid! :absence-for-required-field (conj at id))))
          (not (type-valid? descriptor v))
          (invalid! :ill-typed-domain-value (conj at id))))))
  entries)

(defn- complete-type-domain? [descriptor values]
  (let [expected (case (:type descriptor)
                   :enum (:values descriptor)
                   :boolean #{true false}
                   nil)]
    (and (some? expected)
         (= (set values)
            (cond-> expected
              (true? (:optional? descriptor)) (conj absent)
              (true? (:nullable? descriptor)) (conj nil))))))

(defn- expression! [e permitted at budget depth]
  (when (> depth max-expression-depth)
    (unsupported! :expression-too-deep at))
  (when (> (swap! budget inc) max-expression-nodes)
    (unsupported! :expression-too-large at))
  (when-not (and (plain-vector? e) (seq e))
    (invalid! :expected-expression-vector at))
  (no-meta! e at)
  (let [[op a] e
        n (count e)]
    (when (> n 128) (unsupported! :expression-arity-limit at))
    (case op
      :literal (do (when-not (= n 2) (invalid! :expression-arity at))
                   (literal! a (conj at 1)) #{})
      :read (do (when-not (= n 2) (invalid! :expression-arity at))
                (when-not (and (qualified? a) (contains? permitted a))
                  (invalid! :undeclared-reader at))
                #{a})
      :present (do (when-not (and (= n 2) (plain-vector? a)
                                  (= 2 (count a)) (= :read (first a)))
                     (invalid! :present-requires-direct-reader at))
                   (expression! a permitted (conj at 1) budget (inc depth)))
      :not (do (when-not (= n 2) (invalid! :expression-arity at))
               (expression! a permitted (conj at 1) budget (inc depth)))
      (:and :or) (do (when (< n 3) (invalid! :expression-arity at))
                     (apply set/union
                            (map-indexed
                             (fn [i x] (expression! x permitted
                                                    (conj at (inc i)) budget (inc depth)))
                             (rest e))))
      (:= :not= :< :<= :> :>= :in :implies)
      (do (when-not (= n 3) (invalid! :expression-arity at))
          (set/union (expression! a permitted (conj at 1) budget (inc depth))
                     (expression! (nth e 2) permitted (conj at 2)
                                  budget (inc depth))))
      (unsupported! :unsupported-expression-operator at))))

(defn- scalar-kind [v]
  (cond (nil? v) :nil
        (instance? Boolean v) :boolean
        (integer? v) :integer
        (string? v) :string
        (keyword? v) :keyword
        :else :unsupported))

(defn- kind [v]
  (if (plain-set? v)
    (let [kinds (set (map scalar-kind v))]
      (when-not (= 1 (count kinds))
        (unsupported! :heterogeneous-set-type [:expression]))
      {:type :set :member-kind (first kinds) :values v :nullable? false})
    {:type (scalar-kind v) :origin :literal :literal v :nullable? false}))

(defn- refinement [facts expr truth]
  (let [[op a b] expr]
    (case op
      :present (if truth (update facts :present conj (second a)) facts)
      :not (refinement facts a (not truth))
      :and (if truth (reduce #(refinement %1 %2 true) facts (rest expr)) facts)
      :or (if (not truth) (reduce #(refinement %1 %2 false) facts (rest expr)) facts)
      :implies (if (not truth)
                 (-> facts (refinement a true) (refinement b false))
                 facts)
      (:= :not=)
      (if (= truth (= op :not=))
        (let [id (cond
                   (and (= :read (first a)) (= b [:literal nil])) (second a)
                   (and (= :read (first b)) (= a [:literal nil])) (second b))]
          (if id (update facts :nonnull conj id) facts))
        facts)
      facts)))

(defn- base-kind [{:keys [type]}]
  (if (= type :enum) :keyword type))

(defn- equality-compatible? [a b]
  (let [ka (base-kind a) kb (base-kind b)]
    (or (and (= ka kb) (not (#{:set :nil} ka))
             (cond
               (and (= (:type a) :enum) (= (:type b) :enum))
               (= (select-keys a [:name :values])
                  (select-keys b [:name :values]))
               (= (:type a) :enum) (= :literal (:origin b))
               (= (:type b) :enum) (= :literal (:origin a))
               :else true))
        (and (= ka :nil) (:nullable? b))
        (and (= kb :nil) (:nullable? a)))))

(declare infer!)

(defn- boolean-type! [t at]
  (when-not (and (= :boolean (:type t)) (not (:nullable? t)))
    (unsupported! :nonboolean-typed-expression at))
  {:type :boolean :nullable? false})

(defn- infer! [e types facts at]
  (let [[op a b] e]
    (case op
      :literal (kind a)
      :read (let [d (get types a)]
              (when (and (:optional? d) (not (contains? (:present facts) a)))
                (unsupported! :unguarded-optional-read at))
              (assoc d :nullable? (and (:nullable? d)
                                      (not (contains? (:nonnull facts) a)))))
      :present {:type :boolean :nullable? false}
      :not (do (boolean-type! (infer! a types facts (conj at 1)) at)
               {:type :boolean :nullable? false})
      (:and :or)
      (do (loop [items (rest e), i 1, facts facts]
            (when (seq items)
              (boolean-type! (infer! (first items) types facts (conj at i))
                             (conj at i))
              (recur (rest items) (inc i)
                     (refinement facts (first items) (= op :and)))))
          {:type :boolean :nullable? false})
      :implies (do (boolean-type! (infer! a types facts (conj at 1)) at)
                   (boolean-type! (infer! b types (refinement facts a true)
                                          (conj at 2)) at)
                   {:type :boolean :nullable? false})
      (:= :not=)
      (let [left (infer! a types facts (conj at 1))
            right (infer! b types facts (conj at 2))]
        (when-not (equality-compatible? left right)
          (unsupported! :incompatible-equality at))
        (doseq [[enum other] [[left right] [right left]]]
          (when (and (= :enum (:type enum))
                     (= :literal (:origin other))
                     (= :keyword (:type other))
                     (not (contains? (:values enum) (:literal other))))
            (unsupported! :enum-literal-outside-domain at)))
        {:type :boolean :nullable? false})
      (:< :<= :> :>=)
      (let [left (infer! a types facts (conj at 1))
            right (infer! b types facts (conj at 2))]
        (when-not (and (= :integer (:type left)) (= :integer (:type right))
                       (not (:nullable? left)) (not (:nullable? right)))
          (unsupported! :incompatible-ordering at))
        {:type :boolean :nullable? false})
      :in
      (let [item (infer! a types facts (conj at 1))
            coll (infer! b types facts (conj at 2))]
        (when-not (and (= :set (:type coll))
                       (= (base-kind item) (:member-kind coll))
                       (not (:nullable? item)))
          (unsupported! :incompatible-membership at))
        (when (and (= :enum (:type item))
                   (not (set/subset? (:values coll) (:values item))))
          (unsupported! :membership-outside-enum at))
        {:type :boolean :nullable? false})
      (unsupported! :unsupported-expression-operator at))))

(defn- rule! [r types at budget]
  (exact-map! r #{:readers :expression} at)
  (keyword-set! (:readers r) (conj at :readers))
  (when-not (set/subset? (:readers r) (set (keys types)))
    (invalid! :rule-outside-declared-fields at))
  (let [actual (expression! (:expression r) (set (keys types))
                            (conj at :expression) budget 0)]
    (when-not (= actual (:readers r))
      (invalid! :rule-reader-footprint-mismatch at)))
  (boolean-type! (infer! (:expression r) types {:present #{} :nonnull #{}}
                        (conj at :expression)) at)
  r)

(defn- assignment! [field expr all-types at]
  (when-not (and (plain-vector? expr) (= 2 (count expr))
                 (#{:literal :read} (first expr)))
    (unsupported! :unsupported-assignment at))
  (no-meta! expr at)
  (if (= :literal (first expr))
    (when-not (type-valid? (get all-types field) (second expr))
      (invalid! :ill-typed-assignment at))
    (let [id (second expr)]
      (when-not (contains? all-types id)
        (invalid! :undeclared-assignment-reader at))
      (when (true? (:optional? (get all-types id)))
        (unsupported! :optional-assignment-source at))
      ;; The target and source type descriptors may be nominal. Runtime
      ;; validation below additionally checks the actual copied value.
      (when-not (= (:type (get all-types id))
                   (:type (get all-types field)))
        (unsupported! :cross-type-assignment at))
      (when (and (= :enum (:type (get all-types id)))
                 (not= (select-keys (get all-types id) [:name :values])
                       (select-keys (get all-types field) [:name :values])))
        (unsupported! :cross-enum-assignment at))
      (when (and (:nullable? (get all-types id))
                 (not (:nullable? (get all-types field))))
        (unsupported! :nullable-assignment-source at))))
  expr)

(defn- operation! [entry state-types budget at]
  (exact-map! entry #{:id :construction} at)
  (let [{:keys [id construction]} entry]
    (when-not (qualified? id) (invalid! :invalid-operation-id at))
    (exact-map! construction
                #{:subject :state-types :input-types :guard :reads :writes
                  :replacements :frame}
                (conj at :construction))
    (types! (:state-types construction) (conj at :state-types))
    (when-not (= state-types (:state-types construction))
      (invalid! :state-type-disagreement at))
    (let [{:keys [subject input-types guard reads writes replacements frame]}
          construction]
      (exact-map! subject #{:kind :id :interface} (conj at :subject))
      (when-not (and (= :operation (:kind subject)) (= id (:id subject)))
        (invalid! :operation-subject-mismatch at))
      (exact-map! (:interface subject) #{:operation} (conj at :subject :interface))
      (when-not (= id (get-in subject [:interface :operation]))
        (invalid! :operation-interface-mismatch at))
      (types! input-types (conj at :input-types))
      (when (seq (set/intersection (set (keys state-types))
                                  (set (keys input-types))))
        (invalid! :overlapping-state-input-fields at))
      (let [all-types (merge state-types input-types)
            state-keys (set (keys state-types))]
        (doseq [[k v] [[:reads reads] [:writes writes] [:frame frame]]]
          (keyword-set! v (conj at k)))
        (when-not (and (set/subset? reads (set (keys all-types)))
                       (set/subset? writes state-keys)
                       (= frame (set/difference state-keys writes)))
          (invalid! :bad-transition-footprint at))
        (when-not (plain-map? replacements)
          (invalid! :expected-replacements at))
        (no-meta! replacements (conj at :replacements))
        (when-not (= writes (set (keys replacements)))
          (invalid! :write-assignment-mismatch at))
        (rule! guard all-types (conj at :guard) budget)
        (let [rhs-reads
              (into #{} (keep (fn [[field expression]]
                               (assignment! field expression all-types
                                            (conj at :replacements field))
                               (when (= :read (first expression)) (second expression))))
                    replacements)]
          (when-not (= reads (set/union (:readers guard) rhs-reads))
            (invalid! :read-footprint-mismatch at))))
      entry)))

(defn- catalog! [c budget]
  (exact-map! c #{:id :declared-operation-ids :initial-condition
                  :invariants :operations} [:catalog])
  (when-not (qualified? (:id c)) (invalid! :invalid-catalog-id [:catalog :id]))
  (keyword-set! (:declared-operation-ids c) [:catalog :declared-operation-ids])
  (when-not (and (plain-vector? (:operations c))
                 (<= 1 (count (:operations c)) max-operations))
    (unsupported! :unsupported-operation-count [:catalog :operations]))
  (no-meta! (:operations c) [:catalog :operations])
  (when-not (and (plain-map? (:invariants c))
                 (<= 1 (count (:invariants c)) max-invariants))
    (unsupported! :unsupported-invariant-count [:catalog :invariants]))
  (no-meta! (:invariants c) [:catalog :invariants])
  (let [state-types (get-in c [:operations 0 :construction :state-types])]
    (types! state-types [:catalog :state-types])
    (when-not (seq state-types) (invalid! :empty-state-projection [:catalog]))
    (let [operations (mapv (fn [[i op]]
                             (operation! op state-types budget
                                         [:catalog :operations i]))
                           (map-indexed vector (:operations c)))
          ids (mapv :id operations)]
      (when-not (and (= (count ids) (count (distinct ids)))
                     (= (set ids) (:declared-operation-ids c)))
        (invalid! :operation-coverage-mismatch [:catalog :operations]))
      (rule! (:initial-condition c) state-types
             [:catalog :initial-condition] budget)
      (doseq [[id r] (:invariants c)]
        (when-not (qualified? id) (invalid! :invalid-invariant-id [:catalog id]))
        (rule! r state-types [:catalog :invariants id] budget))
      {:state-types state-types
       :operations (vec (sort-by (comp str :id) operations))
       :invariants (into (sorted-map) (:invariants c))})))

(defn- Boolean! [x at]
  (if (instance? Boolean x)
    x
    (unsupported! :nonboolean-expression at)))

(declare eval-expression)

(defn- eval-expression [e env at]
  (let [[op a b] e]
    (case op
      :literal a
      :read (if (contains? env a)
              (get env a)
              (unsupported! :undefined-partial-read at))
      :present (contains? env (second a))
      :not (not (Boolean! (eval-expression a env (conj at 1)) at))
      :and (loop [items (rest e) i 1]
             (if (empty? items) true
                 (if (Boolean! (eval-expression (first items) env (conj at i)) at)
                   (recur (rest items) (inc i)) false)))
      :or (loop [items (rest e) i 1]
            (if (empty? items) false
                (if (Boolean! (eval-expression (first items) env (conj at i)) at)
                  true (recur (rest items) (inc i)))))
      :implies (if (Boolean! (eval-expression a env (conj at 1)) at)
                 (Boolean! (eval-expression b env (conj at 2)) at) true)
      := (= (eval-expression a env (conj at 1))
            (eval-expression b env (conj at 2)))
      :not= (not= (eval-expression a env (conj at 1))
                     (eval-expression b env (conj at 2)))
      (:> :>= :< :<=)
      (let [x (eval-expression a env (conj at 1))
            y (eval-expression b env (conj at 2))]
        (when-not (and (integer? x) (integer? y))
          (unsupported! :noninteger-comparison at))
        (case op :> (> x y) :>= (>= x y) :< (< x y) :<= (<= x y)))
      :in (let [item (eval-expression a env (conj at 1))
                domain (eval-expression b env (conj at 2))]
            (when-not (plain-set? domain)
              (unsupported! :nonset-membership at))
            (contains? domain item))
      (unsupported! :unsupported-expression-operator at))))

(defn- eval-rule [rule env at]
  (Boolean! (eval-expression (:expression rule) env at) at))

(defn- maps-in-domain [domains]
  (reduce (fn [partial [id values]]
            (vec (for [m partial, v values]
                   (if (= v absent) m (assoc m id v)))))
          [{}] (sort-by (comp str key) domains)))

(defn- update-state [construction pre input]
  (reduce-kv
   (fn [post field [op val]]
     (let [value (if (= op :literal) val
                     (let [env (merge pre input)]
                       (if (contains? env val) (get env val)
                           (unsupported! :undefined-assignment-read
                                         [:replacements field]))))]
       (when-not (type-valid? (get (:state-types construction) field) value)
         (unsupported! :ill-typed-poststate [:replacements field]))
       (assoc post field value)))
   pre (:replacements construction)))

(defn- product-count [domains]
  (reduce *' 1 (map count (vals domains))))

(defn- measure-universe [parsed source]
  (let [s (product-count (:state-domains source))
        input-sizes (into (sorted-map)
                          (map (fn [{:keys [id]}]
                                 [id (product-count (get (:input-domains source) id))])
                               (:operations parsed)))
        cases (+' s (reduce +' 0 (map #(*' s %) (vals input-sizes))))]
    {:states s :input-counts-by-operation input-sizes
     :potential-cases cases :budget (:max-cases source)}))

(defn- bounded-base [states init invariants counts]
  (some (fn [s]
          (swap! counts update :initial-states-examined inc)
          (when (eval-rule init s [:initial-condition])
            (swap! counts update :initial-states-satisfying inc)
            (some (fn [[id invariant]]
                    (when-not (eval-rule invariant s [:invariants id])
                      {:kind :base :invariant-id id :state s
                       :antecedent {:initial-condition true}
                       :consequent {:invariant false}}))
                  invariants)))
        states))

(defn- bounded-step [states operations inputs invariants universe counts]
  (some
   (fn [{:keys [id construction]}]
     (some
      (fn [pre]
        (let [pre-holds? (every? (fn [[iid inv]]
                                   (eval-rule inv pre [:invariants iid]))
                                 invariants)]
          (some
           (fn [input]
             (swap! counts update :transition-cases-examined inc)
             (when (eval-rule (:guard construction) (merge pre input) [:guard id])
               (swap! counts update :enabled-transitions-examined inc)
               (let [post (update-state construction pre input)
                     failed (when pre-holds?
                              (some (fn [[iid inv]]
                                      (when-not (eval-rule inv post [:invariants iid])
                                        iid))
                                    invariants))]
                 (cond
                   failed
                   {:kind :step :operation-id id :invariant-id failed
                    :prestate pre :input input :poststate post
                    :antecedent {:all-pre-invariants true :guard true}
                    :consequent {:post-invariant false}}
                   (not (contains? universe post))
                   {:kind :out-of-universe :operation-id id
                    :prestate pre :input input :poststate post}))))
           (get inputs id))))
      states))
   operations))

(defn- classify! [source]
  (exact-map! source #{:catalog :state-domains :input-domains :max-cases} [])
  (when-not (and (integer? (:max-cases source))
                 (<= 1 (:max-cases source) max-allowed-cases))
    (invalid! :invalid-case-budget [:max-cases]))
  (let [budget (atom 0)
        parsed (catalog! (:catalog source) budget)
        types (:state-types parsed)
        operations (:operations parsed)]
    (domain! types (:state-domains source) [:state-domains])
    (exact-map! (:input-domains source) (set (map :id operations))
                [:input-domains])
    (doseq [{:keys [id construction]} operations]
      (domain! (:input-types construction)
               (get (:input-domains source) id) [:input-domains id]))
    (let [universe-size (measure-universe parsed source)]
      (if (or (> (:states universe-size) max-state-count)
              (> (:potential-cases universe-size) (:max-cases source)))
        {:status :budget-exceeded :reason :finite-case-budget
         :universe universe-size :exhaustive? false
         :physical-correspondence :not-established
         :checker-soundness :not-established
         :universal-induction :not-established}
        (let [states (maps-in-domain (:state-domains source))
              inputs (into {} (map (fn [{:keys [id]}]
                                     [id (maps-in-domain (get (:input-domains source) id))])
                                   operations))
              full-type-domain?
              (and (every? (fn [[field d]]
                             (complete-type-domain? d
                                                    (get (:state-domains source) field)))
                           types)
                   (every? (fn [{:keys [id construction]}]
                             (every? (fn [[field d]]
                                       (complete-type-domain?
                                        d (get-in source [:input-domains id field])))
                                     (:input-types construction)))
                           operations))
              counts (atom {:initial-states-examined 0
                            :initial-states-satisfying 0
                            :transition-cases-examined 0
                            :enabled-transitions-examined 0})
              counterexample (or (bounded-base states
                                               (get-in source [:catalog :initial-condition])
                                               (:invariants parsed) counts)
                                 (bounded-step states operations inputs
                                               (:invariants parsed) (set states) counts))
              status (cond
                       (and (nil? counterexample)
                            (zero? (:initial-states-satisfying @counts)))
                       :unsupported-language
                       (nil? counterexample) :bounded-no-counterexample
                       (= :out-of-universe (:kind counterexample)) :unsupported-language
                       :else :counterexample)]
          (cond->
           {:status status
            :scope :enumerated-declared-abstract-machine-only
            :examined @counts
            :universe (assoc universe-size
                             :all-type-domains-complete? full-type-domain?
                             :state-domains (:state-domains source)
                             :input-domains (:input-domains source))
            :exhaustive? (= :bounded-no-counterexample status)
            :physical-correspondence :not-established
            :checker-soundness :not-established
            :universal-induction :not-established}
            counterexample (assoc :counterexample counterexample)
            (and (nil? counterexample) (= :unsupported-language status))
            (assoc :reason :no-initial-states-in-declared-universe)
            (= :out-of-universe (:kind counterexample))
            (assoc :reason :poststate-outside-declared-universe
                   :exhaustive? false)))))))

(defn check-finite-model!
  "Enumerate a tiny original-source abstract catalog under explicit finite
   state/input representatives. Returns :bounded-no-counterexample,
   :counterexample, :unsupported-language, :budget-exceeded or :invalid-source.
   A bounded success is NOT a universal theorem; no reports/certificates or
   in-process live checker handles are accepted as input. This API does not
   perform any database transaction, issue authority, or admit a proof."
  [source]
  (let [common {:type report-type :version oracle-version
                :checking :independent-bounded-enumeration
                :admission :not-admitted :certificate :not-issued
                :authority :none :runtime-enforcement :not-established}]
    (try
      (merge common (classify! source))
      (catch clojure.lang.ExceptionInfo ex
        (if (:finite-model-oracle/diagnostic (ex-data ex))
          (merge common (dissoc (ex-data ex) :finite-model-oracle/diagnostic)
                 {:exhaustive? false
                  :checker-soundness :not-established
                  :universal-induction :not-established
                  :physical-correspondence :not-established})
          (throw ex))))))
