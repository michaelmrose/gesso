(ns gesso.assurance.proof.typed-atom-oracle
  "Independent bounded semantic cross-check of v1219/v1221/v1223 typed rule
   atoms (v1320). This is diagnostic data, NEVER a proof/certificate/admission.

   (audit-typed-atom!
     {:rule {:readers #{:request/score}
             :expression [:> [:read :request/score] [:literal 7]]}
      :reader-types {:request/score {:type :integer}}
      :binding-domains {:request/score [6 7 8]}
      :max-cases 128})

   Optional absence has the explicit value `absent` in the domain vector;
   the corresponding runtime binding MAP omits that key. Present nil is a
   distinct domain entry. An empty reader footprint has one binding: {}.

   The independent type checker, guarded short-circuit interpreter, and
   finite enumerator below do NOT call rule, rule-type, rule-eval, or other
   proof oracles. Only the outer comparison invokes the production rule-eval
   to test agreement against original source. A bound on supplied inputs is
   NOT evidence of universal type semantics, Clojure/CLJS implementation
   equivalence, trust in runtime field snapshots, or checker soundness."
  (:require [clojure.set :as set]
            [gesso.assurance.rule-eval :as production]))

(def oracle-version 1)
(def report-type :gesso.assurance.proof.typed-atom-oracle/bounded-audit)
(def absent [:gesso.assurance.proof.typed-atom-oracle/absent])

(def ^:private max-fields 12)
(def ^:private max-domain 24)
(def ^:private max-cases-cap 8192)
(def ^:private max-depth 32)
(def ^:private max-nodes 1024)
(def ^:private max-literal-elements 256)
(def ^:private max-text 2048)
(def ^:private allowed-types #{:boolean :integer :string :keyword :enum})
(def ^:private binary-ops #{:= :not= :< :<= :> :>= :in :implies})

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentHashMap v)
      (instance? clojure.lang.PersistentArrayMap v)))
(defn- plain-vector? [v] (instance? clojure.lang.APersistentVector v))
(defn- plain-set? [v] (instance? clojure.lang.PersistentHashSet v))
(defn- qualified-id? [v] (and (keyword? v) (some? (namespace v))))

(defn- stop! [status reason at]
  (throw (ex-info "Typed-atom oracle cannot establish agreement."
                  {:typed-atom-oracle/failure true
                   :status status :reason reason :at at})))
(defn- bad! [reason at] (stop! :invalid-source reason at))
(defn- unsupported! [reason at] (stop! :unsupported-language reason at))

(defn- no-meta! [v at]
  (when (and (instance? clojure.lang.IObj v) (some? (meta v)))
    (bad! :metadata-not-allowed at))
  v)

(defn- exact-map! [v keys-expected at]
  (when-not (plain-map? v) (bad! :expected-plain-map at))
  (no-meta! v at)
  (when-not (= keys-expected (set (keys v)))
    (bad! :wrong-keys at))
  v)

(defn- primitive-kind [v]
  (cond (nil? v) :nil
        (instance? Boolean v) :boolean
        (integer? v) :integer
        (string? v) :string
        (keyword? v) :keyword
        :else nil))

(defn- scalar! [v at]
  (when-not (primitive-kind v) (unsupported! :non-scalar-value at))
  (when (and (#{:integer :string :keyword} (primitive-kind v))
             (> (count (str v)) max-text))
    (unsupported! :oversize-value at))
  v)

(defn- literal! [v at]
  (if (plain-set? v)
    (do (no-meta! v at)
        (when (or (empty? v) (> (count v) max-literal-elements))
          (unsupported! :unsupported-literal-set at))
        (doseq [item v] (scalar! item (conj at :member)))
        (when-not (= 1 (count (set (map primitive-kind v))))
          (unsupported! :heterogeneous-literal-set at)))
    (scalar! v at))
  v)

(defn- descriptors! [readers types]
  (exact-map! types readers [:reader-types])
  (doseq [[field d] types]
    (when-not (qualified-id? field)
      (bad! :invalid-reader-id [:reader-types field]))
    (when-not (plain-map? d)
      (bad! :invalid-descriptor [:reader-types field]))
    (no-meta! d [:reader-types field])
    (when-not (contains? allowed-types (:type d))
      (unsupported! :unknown-type [:reader-types field]))
    (when (seq (set/difference (set (keys d))
                               #{:type :optional? :nullable? :name :values}))
      (bad! :unknown-descriptor-keys [:reader-types field]))
    (doseq [flag [:optional? :nullable?]]
      (when (and (contains? d flag)
                 (not (instance? Boolean (get d flag))))
        (bad! :nonboolean-descriptor-flag [:reader-types field flag])))
    (if (= :enum (:type d))
      (do
        (when-not (qualified-id? (:name d))
          (bad! :invalid-enum-name [:reader-types field :name]))
        (scalar! (:name d) [:reader-types field :name])
        (when-not (and (plain-set? (:values d))
                       (<= 1 (count (:values d)) max-literal-elements))
          (bad! :invalid-enum-domain [:reader-types field :values]))
        (no-meta! (:values d) [:reader-types field :values])
        (doseq [member (:values d)]
          (when-not (keyword? member)
            (bad! :invalid-enum-member [:reader-types field :values]))
          (scalar! member [:reader-types field :values])))
      (when (or (contains? d :values) (contains? d :name))
        (bad! :unexpected-enum-keys [:reader-types field]))))
  types)

(defn- valid-binding? [descriptor value]
  (or (and (nil? value) (true? (:nullable? descriptor)))
      (and (some? value)
           (case (:type descriptor)
             :enum (contains? (:values descriptor) value)
             :boolean (instance? Boolean value)
             :integer (integer? value)
             :string (string? value)
             :keyword (keyword? value)
             false))))

(defn- domains! [types domains]
  (exact-map! domains (set (keys types)) [:binding-domains])
  (doseq [[field entries] domains]
    (when-not (and (plain-vector? entries)
                   (<= 1 (count entries) max-domain))
      (bad! :invalid-domain [:binding-domains field]))
    (no-meta! entries [:binding-domains field])
    (when-not (= (count entries) (count (distinct entries)))
      (bad! :duplicate-domain-entry [:binding-domains field]))
    (doseq [value entries]
      (if (= value absent)
        (do (no-meta! value [:binding-domains field])
            (when-not (true? (:optional? (types field)))
              (bad! :absent-required-field [:binding-domains field])))
        (do (scalar! value [:binding-domains field])
            (when-not (valid-binding? (types field) value)
              (bad! :ill-typed-domain-entry [:binding-domains field]))))))
  domains)

(defn- effective-kind [t]
  (if (= :enum (:kind t)) :keyword (:kind t)))

(defn- equality-compatible? [a b]
  (let [ak (effective-kind a) bk (effective-kind b)]
    (or (and (= ak bk) (some? ak) (not= ak :set)
             (cond
               (and (= (:kind a) :enum) (= (:kind b) :enum))
               (and (= (:name a) (:name b)) (= (:values a) (:values b)))
               (= (:kind a) :enum) (= :literal (:origin b))
               (= (:kind b) :enum) (= :literal (:origin a))
               :else true))
        (and (= ak :nil) (:nullable b))
        (and (= bk :nil) (:nullable a)))))

(defn- refine-branch [env expr true-branch?]
  (let [[op a b] expr]
    (cond
      (= op :not) (recur env a (not true-branch?))
      (= op :present) (if true-branch?
                        (update env :present conj (second a)) env)
      (and (= op :and) true-branch?)
      (reduce (fn [e child] (refine-branch e child true)) env (rest expr))
      (and (= op :or) (not true-branch?))
      (reduce (fn [e child] (refine-branch e child false)) env (rest expr))
      (and (= op :implies) (not true-branch?))
      (-> env (refine-branch a true) (refine-branch b false))
      (and (#{:= :not=} op) (= true-branch? (= op :not=)))
      (let [reader (cond
                     (and (= :read (first a)) (= b [:literal nil])) (second a)
                     (and (= :read (first b)) (= a [:literal nil])) (second b))]
        (if reader (update env :nonnull conj reader) env))
      :else env)))

(defn- require-bool! [t at]
  (when (or (not= :boolean (:kind t)) (:nullable t))
    (unsupported! :nonboolean-expression at)))

(declare infer!)
(defn- infer-children [expr types env at]
  [(infer! (second expr) types env (conj at 1))
   (infer! (nth expr 2) types env (conj at 2))])

(defn- infer! [expr types env at]
  (let [[op a] expr]
    (case op
      :literal
      (let [k (if (plain-set? a) :set (primitive-kind a))]
        (if (= k :set)
          {:kind :set :member-kind (primitive-kind (first a)) :set-values a}
          {:kind k :origin :literal :literal a}))

      :read
      (let [d (types a)]
        (when (and (:optional? d) (not (contains? (:present env) a)))
          (unsupported! :unguarded-optional-read at))
        {:kind (:type d)
         :name (:name d) :values (:values d)
         :nullable (and (true? (:nullable? d))
                        (not (contains? (:nonnull env) a)))})

      :present {:kind :boolean}

      :not
      (do (require-bool! (infer! a types env (conj at 1)) (conj at 1))
          {:kind :boolean})

      (:and :or)
      (do (loop [i 1 environment env]
            (when (< i (count expr))
              (require-bool! (infer! (nth expr i) types environment (conj at i))
                             (conj at i))
              (recur (inc i)
                     (refine-branch environment (nth expr i) (= op :and)))))
          {:kind :boolean})

      :implies
      (do (require-bool! (infer! a types env (conj at 1)) (conj at 1))
          (require-bool! (infer! (nth expr 2) types
                                      (refine-branch env a true) (conj at 2))
                         (conj at 2))
          {:kind :boolean})

      (:= :not=)
      (let [[l r] (infer-children expr types env at)]
        (when-not (equality-compatible? l r)
          (unsupported! :incompatible-equality at))
        (doseq [[en other] [[l r] [r l]]]
          (when (and (= :enum (:kind en))
                     (= :literal (:origin other))
                     (= :keyword (:kind other))
                     (not (contains? (:values en) (:literal other))))
            (unsupported! :enum-literal-outside-domain at)))
        {:kind :boolean})

      (:< :<= :> :>=)
      (let [[l r] (infer-children expr types env at)]
        (when-not (and (= :integer (:kind l)) (= :integer (:kind r))
                       (not (:nullable l)) (not (:nullable r)))
          (unsupported! :noninteger-order at))
        {:kind :boolean})

      :in
      (let [[item domain] (infer-children expr types env at)]
        (when-not (and (= :set (:kind domain))
                       (= (effective-kind item) (:member-kind domain))
                       (not (:nullable item)))
          (unsupported! :incompatible-membership at))
        (when (and (= :enum (:kind item))
                   (not (set/subset? (:set-values domain) (:values item))))
          (unsupported! :enum-set-outside-domain at))
        {:kind :boolean})

      (unsupported! :unknown-operator at))))

(defn- syntax! [expr readers budget depth at]
  (when (> depth max-depth) (unsupported! :expression-too-deep at))
  (when (> (swap! budget inc) max-nodes)
    (unsupported! :expression-too-large at))
  (when-not (and (plain-vector? expr) (seq expr) (<= (count expr) 256))
    (bad! :invalid-expression at))
  (no-meta! expr at)
  (let [[op a] expr, n (count expr)]
    (case op
      :literal (do (when-not (= n 2) (bad! :invalid-arity at))
                   (literal! a (conj at 1)) #{})
      :read (do (when-not (= n 2) (bad! :invalid-arity at))
                (when-not (and (qualified-id? a) (contains? readers a))
                  (bad! :undeclared-reader at))
                #{a})
      :present (do (when-not (and (= n 2) (plain-vector? a)
                                  (= 2 (count a)) (= :read (first a)))
                     (bad! :present-requires-direct-read at))
                   (syntax! a readers budget (inc depth) (conj at 1)))
      :not (do (when-not (= n 2) (bad! :invalid-arity at))
               (syntax! a readers budget (inc depth) (conj at 1)))
      (:and :or) (do (when (< n 3) (bad! :invalid-arity at))
                     (reduce set/union #{}
                             (map-indexed
                              (fn [i child]
                                (syntax! child readers budget (inc depth)
                                         (conj at (inc i))))
                              (rest expr))))
      (:= :not= :< :<= :> :>= :in :implies)
      (do (when-not (= n 3) (bad! :invalid-arity at))
          (set/union (syntax! a readers budget (inc depth) (conj at 1))
                     (syntax! (nth expr 2) readers budget (inc depth) (conj at 2))))
      (bad! :unknown-operator at))))

(defn- interpret [expr bindings]
  (let [[op a b] expr]
    (case op
      :literal a
      :read (if (contains? bindings a)
              (get bindings a)
              (stop! :independent-evaluation-error :absent-read [:expression]))
      :present (contains? bindings (second a))
      :not (not (interpret a bindings))
      ;; Explicit loops: a lazy chunked map can evaluate later (unsafe) reads
      ;; even when the first guard is false. The independent interpreter must
      ;; match left-to-right *operational* short-circuit behavior.
      :and (loop [i 1]
             (if (= i (count expr)) true
                 (if (true? (interpret (nth expr i) bindings))
                   (recur (inc i)) false)))
      :or (loop [i 1]
            (if (= i (count expr)) false
                (if (true? (interpret (nth expr i) bindings))
                  true (recur (inc i)))))
      :implies (or (not (interpret a bindings)) (interpret b bindings))
      := (= (interpret a bindings) (interpret b bindings))
      :not= (not= (interpret a bindings) (interpret b bindings))
      :< (< (interpret a bindings) (interpret b bindings))
      :<= (<= (interpret a bindings) (interpret b bindings))
      :> (> (interpret a bindings) (interpret b bindings))
      :>= (>= (interpret a bindings) (interpret b bindings))
      :in (boolean (some #(= % (interpret a bindings)) (interpret b bindings)))
      (stop! :independent-evaluation-error :unrecognized-expression [:expression]))))

(defn- finite-coverage? [types domains]
  (every?
   (fn [[field d]]
     (let [expected (case (:type d)
                      :boolean #{true false}
                      :enum (:values d)
                      nil)]
       (and expected
            (= (set (domains field))
               (cond-> expected
                 (true? (:nullable? d)) (conj nil)
                 (true? (:optional? d)) (conj absent))))))
   types))

(defn- enumerated-bindings [fields domains]
  (reduce (fn [rows field]
            (for [row rows, value (domains field)]
              (if (= value absent) row (assoc row field value))))
          [{}] fields))

(defn- failure-result [data]
  {:type report-type :version oracle-version
   :status (:status data) :reason (:reason data) :at (:at data)
   :proof-status :not-proved :admission :not-admitted
   :authority :none})

(defn audit-typed-atom!
  "Independently static-check and interpret an ORIGINAL typed rule over each
   supplied finite binding combination, then compare with rule-eval's actual
   result. No prior check report or certificate is accepted.

   :bounded-agreement means all enumerated cases agree; :divergence contains
   the first reproducible counterexample; invalid, unsupported, or over-budget
   inputs cannot pass. A copyable report has no enforcement authority."
  [source]
  (try
    (exact-map! source #{:rule :reader-types :binding-domains :max-cases} [])
    (let [{:keys [rule reader-types binding-domains max-cases]} source
          _ (exact-map! rule #{:readers :expression} [:rule])
          readers (:readers rule)
          _ (when-not (and (plain-set? readers) (<= (count readers) max-fields)
                           (every? qualified-id? readers))
              (bad! :invalid-readers [:rule :readers]))
          _ (no-meta! readers [:rule :readers])
          _ (when-not (and (integer? max-cases)
                           (<= 1 max-cases max-cases-cap))
              (bad! :invalid-budget [:max-cases]))
          _ (descriptors! readers reader-types)
          _ (domains! reader-types binding-domains)
          budget (atom 0)
          used (syntax! (:expression rule) readers budget 0 [:rule :expression])
          _ (when-not (= used readers)
              (bad! :unused-reader [:rule :readers]))
          inferred (infer! (:expression rule) reader-types
                           {:present #{} :nonnull #{}} [:rule :expression])
          _ (require-bool! inferred [:rule :expression])
          fields (vec (sort-by str readers))
          size (reduce *' 1 (map #(count (binding-domains %)) fields))
          _ (when (> size max-cases)
              (stop! :budget-exceeded :enumeration-limit [:max-cases]))
          domain-status (if (finite-coverage? reader-types binding-domains)
                          :complete-declared-finite-values :representative-values)
          base {:type report-type :version oracle-version
                :checking :independent-typing-and-finite-evaluation
                :scope :supplied-binding-domains-only
                :domain-coverage domain-status
                :reader-ids (set fields)
                :cases-planned size
                :proof-status :not-proved
                :admission :not-admitted
                :runtime-enforcement :not-established
                :transaction-authority :none
                :independent-checker-soundness :not-established
                :atomic-semantics-universality :not-established
                :jvm-cljs-equivalence :not-established
                :trusted-snapshot-provenance :not-established
                :authority :none}]
      (loop [cases (seq (enumerated-bindings fields binding-domains))
             checked 0 true-count 0 false-count 0]
        (if-let [bindings (first cases)]
          (let [expected (interpret (:expression rule) bindings)
                response (try
                           {:report (production/evaluate-rule! rule reader-types bindings)}
                           (catch Exception e
                             {:exception (class e)
                              :reason (:reason (ex-data e))}))
                report (:report response)
                good-report? (and (plain-map? report)
                                  (= (set (keys report))
                                     #{:type :version :rule-version :type-version
                                       :readers :expression :value :checking
                                       :evaluation :proof-status :admission
                                       :runtime-enforcement :authority})
                                  (= (:type report) production/evaluated-type)
                                  (= (:version report) production/evaluator-version)
                                  (= (:rule-version report) 1)
                                  (= (:type-version report) 1)
                                  (= (:readers report) readers)
                                  (= (:expression report) (:expression rule))
                                  (= (:checking report) :syntax-and-types)
                                  (= (:evaluation report) :pure-interpreted)
                                  (= (:proof-status report) :not-proved)
                                  (= (:admission report) :not-admitted)
                                  (= (:authority report) :none)
                                  (= (:runtime-enforcement report) :not-established)
                                  (instance? Boolean (:value report)))
                actual (:value report)]
            (if (and good-report? (= expected actual))
              (recur (next cases) (inc checked)
                     (if expected (inc true-count) true-count)
                     (if expected false-count (inc false-count)))
              (assoc base
                     :status :divergence
                     :reason (if good-report? :different-truth-value
                                 (if (:exception response)
                                   :production-evaluator-rejected
                                   :invalid-production-report))
                     :counterexample {:bindings bindings
                                      :independent-value expected
                                      :production-value (when good-report? actual)
                                      :production-error (:reason response)}
                     :cases-checked (inc checked))))
          (assoc base
                 :status :bounded-agreement
                 :cases-checked checked
                 :true-cases true-count
                 :false-cases false-count))))
    (catch clojure.lang.ExceptionInfo e
      (if (:typed-atom-oracle/failure (ex-data e))
        (failure-result (ex-data e))
        (throw e)))))
