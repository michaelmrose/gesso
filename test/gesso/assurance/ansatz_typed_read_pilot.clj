(ns gesso.assurance.ansatz-typed-read-pilot
  "Opt-in v1749: typed input/prestate :read assignments, conditional proofs.

   Original Gesso guarded replacement semantics admits :read from a REQUIRED
   typed operation input or prestate field. For each source-declared invariant
   on a :read-written target, generate an Ansatz implication from BOTH an
   explicit invariant premise on the read source and the normalized equality
   post=source, to the target invariant. A typed integer input alone NEVER
   establishes a numeric bound. The pilot boundary checks this premise on
   EACH invocation before accepting any abstract observation.

   This is a restricted proof bridge, NOT the generic v1746 inductive theorem:
   untracked writes, other fields, installation, actual XTDB writes, global
   writer closure, interpreter soundness and source translation correctness
   remain outside the theorem. No downstream annotations or runtime authority."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-finite-enum-partition-pilot :as enum]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.ansatz-observation-gate-pilot]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.subject :as subject]))

(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private gate-ns 'gesso.assurance.ansatz-observation-gate-pilot)
(def ^:private flags [:guard-holds? :assignments-hold? :frame-holds? :matches?])

(defn- reject! [reason detail]
  (throw (ex-info "Unsupported typed-read assurance pilot"
                  {:reason reason :details detail})))

(defn- pilot! [n f & args]
  (let [v (ns-resolve n f)]
    (when-not v (reject! :pilot-contract-missing {:ns n :function f}))
    (apply @v args)))

(defn- plain-map? [m]
  (or (instance? clojure.lang.PersistentArrayMap m)
      (instance? clojure.lang.PersistentHashMap m)))

(defn- typed-value? [desc value]
  (and (not (true? (:optional? desc)))
       (if (nil? value)
         (true? (:nullable? desc))
         (case (:type desc)
           :integer (integer? value)
           :enum (and (keyword? value) (contains? (:values desc) value))
           :keyword (keyword? value)
           false))))

(defn- projection! [types side value]
  (when-not (and (plain-map? value) (nil? (meta value))
                 (= (set (keys types)) (set (keys value)))
                 (every? (fn [[field descriptor]]
                           (typed-value? descriptor (get value field))) types))
    (reject! :invalid-typed-projection {:side side :value value}))
  value)

(defn- original-source! [operation]
  (let [built (pilot! preservation-ns 'original-transition! operation)
        {:keys [source semantics construction]} built
        state-types (:state-types source)
        input-types (:input-types source)
        guard (:guard source)
        expression (:expression guard)
        [_ read-form literal-form] expression
        field (second read-form)
        target (second literal-form)]
    (when-not (and (every? #(not (true? (:optional? %)))
                          (vals (merge state-types input-types)))
                   (every? (fn [[_ desc]]
                             (#{:integer :enum :keyword} (:type desc)))
                           (merge state-types input-types))
                   (= {} (select-keys input-types (keys state-types)))
                   (= #{field} (:readers guard))
                   (vector? expression) (= 3 (count expression))
                   (= := (first expression))
                   (= [:read field] read-form)
                   (= [:literal target] literal-form)
                   (= :enum (get-in state-types [field :type]))
                   (contains? (get-in state-types [field :values]) target)
                   (= (:assignments semantics) (:replacements construction)))
      (reject! :unsupported-typed-read-source {:operation (:id operation)}))
    (assoc built :guard-field field :guard-value target)))

(defn- read-obligations! [catalog]
  (let [{:keys [id invariants operations]} catalog]
    (when-not (and (= #{:id :invariants :operations} (set (keys catalog)))
                   (keyword? id) (namespace id)
                   (map? invariants) (seq invariants) (<= (count invariants) 12)
                   (vector? operations) (seq operations) (<= (count operations) 12)
                   (= (count operations) (count (set (map :id operations)))))
      (reject! :invalid-read-catalog {:catalog id}))
    (let [source-id (:fingerprint
                     (subject/describe-subject
                      {:kind :rule :id :gesso.assurance.ansatz-pilot/typed-read-source
                       :interface catalog}))
          sources (mapv original-source! operations)
          invs (mapv (fn [[invariant-id rule]]
                       (pilot! preservation-ns 'invariant! invariant-id rule))
                     (sort-by key invariants))
          pairs
          (vec (for [{:keys [id semantics transition-fingerprint] :as op} sources
                     inv invs
                     :let [field (:field inv)
                           assignment (get-in semantics [:assignments field])]
                     :when (= :read (first assignment))]
                 (let [reader (second assignment)
                       origin (if (contains? (get-in op [:source :input-types]) reader)
                                :input :prestate)
                       types (merge (get-in op [:source :state-types])
                                    (get-in op [:source :input-types]))
                       source-type (get types reader)
                       target-type (get-in op [:source :state-types field])
                       encoding (pilot! preservation-ns 'state-field-type!
                                        inv (:construction op))
                       enum? (= :finite-enum-membership (:kind inv))]
                   ;; The original type checker also checked this assignment;
                   ;; these exact supported types keep the proof encoding honest.
                   (when-not (and source-type (= source-type target-type)
                                  (not (true? (:optional? source-type)))
                                  (not (true? (:nullable? source-type)))
                                  (= assignment (get-in op [:construction :replacements field]))
                                  (#{:signed-int-bound :finite-enum-membership} (:kind inv)))
                     (reject! :unsupported-typed-read-invariant
                              {:operation id :reader reader :invariant (:invariant-id inv)}))
                   (let [expression (if enum?
                                      (pilot! preservation-ns 'enum-membership
                                              (:codes encoding) (:members inv) 'src)
                                      (pilot! preservation-ns 'comparison
                                              (:op inv) 'src (:bound inv)))
                         goal (if enum?
                                (pilot! preservation-ns 'enum-membership
                                        (:codes encoding) (:members inv) 'post)
                                (pilot! preservation-ns 'comparison
                                        (:op inv) 'post (:bound inv)))]
                     {:obligation-kind :conditional-typed-read-invariant-preservation
                      :catalog-fingerprint source-id
                      :operation-id id :invariant-id (:invariant-id inv)
                      :transition-fingerprint transition-fingerprint
                      :reader reader :field field :reader-origin origin
                      :invariant inv :encoding (when enum? (:codes encoding))
                      :parameters ['src ':- 'Int 'post ':- 'Int
                                   'hsource ':- expression
                                   'hwrite ':- (list '= 'Int 'post 'src)]
                      :goal-form goal
                      :proof-script (if enum?
                                      '[(rw [hwrite]) (exact hsource)]
                                      '[(omega)])}))))
          required (set (for [op sources inv invs
                              :when (= :read (first (get-in op [:semantics :assignments (:field inv)])))]
                          [(:id op) (:invariant-id inv)]))
          all-read-targets (set (for [op sources
                                      [field [operator _]] (get-in op [:semantics :assignments])
                                      :when (= :read operator)]
                                  [(:id op) field]))
          covered-targets (set (map (juxt :operation-id :field) pairs))
          found (mapv (juxt :operation-id :invariant-id) pairs)]
      (when-not (and (seq pairs) (= required (set found))
                     (= (count required) (count found)))
        (reject! :incomplete-typed-read-proof-coverage
                 {:required required :found found}))
      {:catalog-fingerprint source-id :sources sources
       :obligations pairs :invariants invs
       :unproved-read-targets (set/difference all-read-targets covered-targets)})))

(defn- check-read-proofs! [catalog]
  (let [{:keys [catalog-fingerprint obligations] :as derived}
        (read-obligations! catalog)
        results (mapv #(pilot! proof-ns 'check-obligation! %) obligations)]
    (when-not (and (= (count obligations) (count results))
                   (every? (fn [{:keys [goal proof description dependencies]}]
                             (and (kernel/verifies? (ansatz/env) goal proof)
                                  (= catalog-fingerprint (:catalog-fingerprint description))
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) results))
      (reject! :unqualified-typed-read-proof {:count (count results)}))
    (assoc derived :checked results :proof-count (count results)
           :assurance :conditional-typed-read-abstract-preservation
           :source-preconditions :must-be-established-per-invocation
           :full-inductive-safety :not-established
           :coverage :tracked-read-writes-only
           :physical-correspondence :not-established
           :authority :none :admission :not-admitted :certificate :not-issued)))

(defn- source-predicate-holds? [{:keys [kind op bound members]} value]
  (case kind
    :signed-int-bound (case op :>= (>= value bound) :<= (<= value bound))
    :finite-enum-membership (contains? members value)
    false))

(defn- source-observation! [source before inputs after]
  (let [transition (:source source)
        state-types (:state-types transition)
        input-types (:input-types transition)
        _ (projection! state-types :before before)
        _ (projection! input-types :inputs inputs)
        _ (projection! state-types :after after)
        available (merge before inputs)
        expected {:guard-holds? (= (get before (:guard-field source))
                                   (:guard-value source))
                  :assignments-hold?
                  (every? (fn [[field [op operand]]]
                            (= (get after field)
                               (case op
                                 :literal operand
                                 :read (get available operand)
                                 (reject! :unsupported-assignment-operator op))))
                          (:assignments transition))
                  :frame-holds?
                  (every? (fn [field] (= (get before field) (get after field)))
                          (set/difference (set (keys state-types))
                                          (set (keys (:assignments transition)))))}
        expected (assoc expected :matches? (every? true? (vals expected)))
        rule (:guard transition)
        evaluated (rule-eval/evaluate-rule!
                   rule (select-keys (merge state-types input-types) (:readers rule))
                   (select-keys available (:readers rule)))
        a (abstract/observe-transition! transition before inputs after)
        b (independent/observe-transition! transition before inputs after)]
    (when-not (and (= :syntax-and-types (:checking evaluated))
                   (= :pure-interpreted (:evaluation evaluated))
                   (= (:guard-holds? expected) (:value evaluated))
                   (= :none (:authority evaluated)))
      (reject! :actual-guard-evaluator-disagreement {:expected expected}))
    (when-not (and (= abstract/observation-type (:type a))
                   (= independent/report-type (:type b))
                   (= :abstract-relation-observed (:checking a))
                   (= :independent-typed-guarded-relation (:checking b))
                   (= :none (:authority a) (:authority b))
                   (= (:transition-fingerprint source) (:transition-fingerprint a))
                   (= transition (:source-declaration b))
                   (= before (:before b)) (= inputs (:inputs b)) (= after (:after b))
                   (every? (fn [flag]
                             (and (boolean? (get a flag)) (boolean? (get b flag))
                                  (= (get expected flag) (get a flag) (get b flag)))) flags))
      (reject! :abstract-observer-disagreement {:expected expected :a a :b b}))
    expected))

(defn- audited-read-boundary! [catalog]
  (pilot! gate-ns 'current-interpreter-source!)
  (pilot! gate-ns 'current-rule-source-closure!)
  (let [{:keys [sources obligations] :as proved} (check-read-proofs! catalog)
        operations (into {} (map (juxt :id identity) sources))
        by-operation (group-by :operation-id obligations)]
    {:checking :kernel-backed-per-invocation-typed-read-pilot
     :proof-count (:proof-count proved)
     :catalog-fingerprint (:catalog-fingerprint proved)
     :full-inductive-safety :not-established
     :physical-correspondence :not-established
     :authority :none :admission :not-admitted :certificate :not-issued
     :observe!
     (fn [id before inputs after]
       (let [source (get operations id)]
         (when-not source (reject! :undeclared-operation {:id id}))
         ;; All closed projections are verified before relying on reports.
         (projection! (get-in source [:source :state-types]) :before before)
         (projection! (get-in source [:source :input-types]) :inputs inputs)
         (projection! (get-in source [:source :state-types]) :after after)
         (doseq [{:keys [invariant reader reader-origin]} (get by-operation id)]
           (let [source-value (get (if (= reader-origin :input) inputs before) reader)]
             (when-not (source-predicate-holds? invariant source-value)
               (reject! :source-invariant-precondition-failed
                        {:operation id :reader reader
                         :invariant (:invariant-id invariant)}))))
         (let [observed (source-observation! source before inputs after)]
           {:operation-id id :matches? (:matches? observed)
            :checked-read-invariants (count (get by-operation id))
            :assurance :per-invocation-conditional-read-correspondence
            :authority :none :admission :not-admitted :certificate :not-issued})))}))

(defn- example-catalog []
  (let [status {:type :enum :name :ticket/status :values #{:open :claimed}}
        channel {:type :enum :name :ticket/channel :values #{:web :phone}}
        state {:ticket/status status :ticket/channel channel
               :ticket/count {:type :integer} :ticket/backup {:type :integer}
               :ticket/owner {:type :keyword :nullable? true}
               :ticket/audit {:type :keyword :nullable? true}}
        input {:input/channel channel :input/count {:type :integer}
               :input/actor {:type :keyword}}
        op (fn [id input-types guard-status assignments reads]
             {:id id :construction
              {:subject {:kind :operation :id id :interface {:operation id}}
               :state-types state :input-types input-types
               :guard {:readers #{:ticket/status}
                       :expression [:= [:read :ticket/status]
                                    [:literal guard-status]]}
               :reads reads :writes (set (keys assignments))
               :replacements assignments
               :frame (set/difference (set (keys state))
                                      (set (keys assignments)))}})]
    {:id :ticket/typed-read-example
     :invariants
     {:ticket/count-min {:readers #{:ticket/count}
                         :expression [:>= [:read :ticket/count] [:literal 0]]}
      :ticket/count-max {:readers #{:ticket/count}
                         :expression [:<= [:read :ticket/count] [:literal 5]]}
      :ticket/channel-domain {:readers #{:ticket/channel}
                              :expression [:in [:read :ticket/channel]
                                           [:literal #{:web :phone}]]}}
     :operations
     [(op :ticket/assign input :open
          {:ticket/status [:literal :claimed]
           :ticket/channel [:read :input/channel]
           :ticket/count [:read :input/count]
           :ticket/owner [:read :input/actor]}
          #{:ticket/status :input/channel :input/count :input/actor})
      (op :ticket/restore {} :claimed
          {:ticket/count [:read :ticket/backup]}
          #{:ticket/status :ticket/backup})]}))

(defn- assign-witness []
  (let [before {:ticket/status :open :ticket/channel :phone
                :ticket/count 0 :ticket/backup 4
                :ticket/owner nil :ticket/audit nil}
        inputs {:input/channel :web :input/count 3 :input/actor :user/agent}
        after (assoc before :ticket/status :claimed :ticket/channel :web
                     :ticket/count 3 :ticket/owner :user/agent)]
    [before inputs after]))

(deftest typed-input-and-prestate-copy-proofs-are-source-derived
  (let [catalog (example-catalog)
        {:keys [checked proof-count] :as result} (check-read-proofs! catalog)]
    (is (= 5 proof-count))
    (is (= #{[:ticket/assign :ticket/count-min]
             [:ticket/assign :ticket/count-max]
             [:ticket/assign :ticket/channel-domain]
             [:ticket/restore :ticket/count-min]
             [:ticket/restore :ticket/count-max]}
           (set (map (juxt (comp :operation-id :description)
                           (comp :invariant-id :description)) checked))))
    (is (= #{:input :prestate}
           (set (map (comp :reader-origin :description) checked))))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) checked))
    (is (= #{[:ticket/assign :ticket/owner]}
           (:unproved-read-targets result)))
    (is (= :tracked-read-writes-only (:coverage result)))
    (is (= :not-established (:full-inductive-safety result)))
    (is (= :none (:authority result)))))

(deftest typed-integer-input-does-not-imply-a-range-invariant
  (let [catalog (example-catalog)
        gate (:observe! (audited-read-boundary! catalog))
        [before inputs after] (assign-witness)
        unsafe-input (assoc inputs :input/count 8)
        unsafe-after (assoc after :ticket/count 8)
        operation (:source (original-source! (first (:operations catalog))))]
    (is (:matches? (gate :ticket/assign before inputs after)))
    (is (:matches? (abstract/observe-transition! operation before unsafe-input unsafe-after)))
    (is (= :source-invariant-precondition-failed
           (try (gate :ticket/assign before unsafe-input unsafe-after) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (= :invalid-typed-projection
           (try (gate :ticket/assign before (assoc inputs :input/count 3.2) after) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest typed-enum-does-not-establish-a-proper-subset-invariant
  (let [catalog (assoc-in (example-catalog)
                          [:invariants :ticket/channel-domain :expression 2 1]
                          #{:web})
        gate (:observe! (audited-read-boundary! catalog))
        [before inputs after] (assign-witness)
        typed-but-outside (assoc inputs :input/channel :phone)
        after-phone (assoc after :ticket/channel :phone)
        original (:source (original-source! (first (:operations catalog))))]
    (is (:matches? (gate :ticket/assign before inputs after)))
    (is (:matches? (abstract/observe-transition!
                    original before typed-but-outside after-phone)))
    (is (= :source-invariant-precondition-failed
           (try (gate :ticket/assign before typed-but-outside after-phone) nil
                (catch clojure.lang.ExceptionInfo e
                  (:reason (ex-data e))))))))

(deftest state-read-is-checked-and-untracked-input-writes-are-observed
  (let [catalog (example-catalog)
        gate (:observe! (audited-read-boundary! catalog))
        [before inputs after] (assign-witness)
        restored-before (assoc before :ticket/status :claimed)
        restored-after (assoc restored-before :ticket/count 4)]
    (is (:matches? (gate :ticket/restore restored-before {} restored-after)))
    (is (false? (:matches? (gate :ticket/restore restored-before {}
                                (assoc restored-after :ticket/count 5)))))
    (is (= :source-invariant-precondition-failed
           (try (gate :ticket/restore
                      (assoc restored-before :ticket/backup -1) {}
                      (assoc restored-after :ticket/count -1)) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
    (is (false? (:matches? (gate :ticket/assign before inputs
                                (assoc after :ticket/owner :user/incorrect)))))
    (is (false? (:matches? (gate :ticket/assign before inputs
                                (assoc after :ticket/audit :audit/changed)))))))

(deftest corrupt-observers-and-mutated-read-declarations-fail-closed
  (let [catalog (example-catalog)
        gate (:observe! (audited-read-boundary! catalog))
        [before inputs after] (assign-witness)
        original-primary abstract/observe-transition!
        original-other independent/observe-transition!
        bogus (fn [observer] (fn [& args]
                               (assoc (apply observer args)
                                      :guard-holds? true :assignments-hold? true
                                      :frame-holds? true :matches? true)))
        reason (fn [f] (try (f) nil
                            (catch clojure.lang.ExceptionInfo e
                              (:reason (ex-data e)))))]
    (with-redefs [abstract/observe-transition! (bogus original-primary)
                  independent/observe-transition! (bogus original-other)]
      (is (= :abstract-observer-disagreement
             (reason #(gate :ticket/assign before inputs
                            (assoc after :ticket/count 4))))))
    (let [false-negative (fn [observer]
                           (fn [& args]
                             (assoc (apply observer args)
                                    :guard-holds? false :assignments-hold? false
                                    :frame-holds? false :matches? false)))]
      (with-redefs [abstract/observe-transition! (false-negative original-primary)
                    independent/observe-transition! (false-negative original-other)]
        (is (= :abstract-observer-disagreement
               (reason #(gate :ticket/assign before inputs after))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check-read-proofs!
                  (assoc-in catalog [:operations 0 :construction :replacements
                                     :ticket/count] [:read :input/unknown]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check-read-proofs!
                  (assoc-in catalog [:operations 0 :construction :input-types
                                     :input/count] {:type :keyword}))))
    (is (= :undeclared-operation
           (reason #(gate :ticket/missing before inputs after))))
    (is (= :invalid-typed-projection
           (reason #(gate :ticket/assign before (assoc inputs :input/extra :no) after))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-typed-read-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
