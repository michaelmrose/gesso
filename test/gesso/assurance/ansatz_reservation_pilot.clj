(ns gesso.assurance.ansatz-reservation-pilot
  "Opt-in Ansatz 0.2.617, source-derived, signed-integer preservation pilot.

   From the ORIGINAL Gesso reservation catalog, construct quantified
   conditional preservation propositions for BOTH claim-count invariants
   across EVERY declared operation. For pre/post Int states, assume the
   original invariant at pre and the original literal replacement at post;
   prove the invariant at post. The proposition and proof are checked by
   Ansatz's kernel. Neither the goals nor the operations are authored in a
   separate downstream specification.

   The proof's literal-write premise is now checked against Gesso's
   original guarded-replacement constructor and normalized transition source;
   the original and independent abstract observers are also compared over a
   bounded set of concrete witnesses, including false guards and bad writes.

   Scope remains conditional: bounded interpreter observations do NOT prove
   universal interpreter correctness, original-to-Ansatz translation fidelity,
   invariant initialization, status-domain preservation, or any physical/
   installed correspondence. The proof declaration closure is inspected under
   an explicit pilot axiom policy; no authority-bearing evidence is issued.
   This pilot accepts only the exact known reservation source shape.

   Invoke using the optional dependency alias:
     clojure -M:ansatz:test -m gesso.assurance.ansatz-reservation-pilot

   The filename intentionally does not end in _test.clj: the ordinary JVM
   suite must not acquire an implicit Ansatz dependency."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [ansatz.export.non-safe :as non-safe]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as guarded]))

(def ^:private claim-field :slot/active-claims)
(def ^:private claim-invariant-ids
  #{:slot/nonnegative-claims :slot/at-most-one-claim})
(def ^:private expected-invariant-ids
  (conj claim-invariant-ids :slot/status-domain))
(def ^:private supported-construction-keys
  #{:subject :state-types :input-types :guard :reads :writes
    :replacements :frame})

(defn- reject! [reason data]
  (throw (ex-info "Unsupported Ansatz reservation pilot source"
                  (assoc data :error/type ::unsupported-source :reason reason))))

(defn- integer-literal! [form at]
  (when-not (and (vector? form) (= 2 (count form))
                 (= :literal (first form))
                 (integer? (second form)))
    (reject! :expected-integer-literal {:at at :expression form}))
  (second form))

(defn- claim-invariant! [catalog invariant-id]
  (let [invariant (get-in catalog [:invariants invariant-id])
        {:keys [readers expression]} invariant]
    (when-not (and (= #{:readers :expression} (set (keys invariant)))
                   (= #{claim-field} readers)
                   (vector? expression) (= 3 (count expression))
                   (#{:<= :>=} (first expression))
                   (= [:read claim-field] (second expression)))
      (reject! :unsupported-invariant
               {:invariant-id invariant-id :invariant invariant}))
    {:invariant-id invariant-id
     :comparison (first expression)
     :bound (integer-literal! (nth expression 2)
                              [:invariants invariant-id :expression])}))

(defn- source-transition! [operation]
  (let [{:keys [id construction]} operation
        {:keys [subject state-types input-types reads writes replacements frame]}
        construction]
    ;; This is the exact closed reservation pilot, not a generic translator.
    ;; Reject future state/input/effect shapes instead of silently applying a
    ;; proof to a declaration whose semantics have grown beyond this subset.
    (when-not (and (= #{:id :construction} (set (keys operation)))
                   (keyword? id)
                   (= supported-construction-keys (set (keys construction)))
                   (= :operation (:kind subject))
                   (= id (:id subject))
                   (= {:operation id} (:interface subject))
                   (= #{:slot/status claim-field :slot/audit-tag}
                      (set (keys state-types)))
                   (= {:type :integer} (get state-types claim-field))
                   (= {} input-types)
                   (= #{:slot/status} reads)
                   (= #{:slot/status claim-field} writes)
                   (= writes (set (keys replacements)))
                   (= #{:slot/audit-tag} frame))
      (reject! :unsupported-transition {:operation-id id
                                        :construction construction}))
    (let [replacement (integer-literal! (get replacements claim-field)
                                        [:operations id :replacements claim-field])
          ;; IMPORTANT: do not infer an interpreter's write semantics from
          ;; raw syntax alone. Ask Gesso to construct and normalize the
          ;; ORIGINAL guarded transition, and compare the checked RHS.
          built (guarded/build-guarded-replacement! construction)
          transition (:transition built)
          described (abstract/describe-transition! transition)
          semantics (:semantics described)]
      (when-not (and (= replacements (:assignments transition))
                     (= (:reads built) (:reads semantics))
                     (= (:writes built) (:writes semantics))
                     (= (:frame built) (:frame-fields semantics))
                     (= (:transition-fingerprint built) (:fingerprint described))
                     (= [:literal replacement]
                        (get-in semantics [:assignments claim-field])))
        (reject! :transition-premise-mismatch {:operation-id id}))
      {:replacement replacement
       :transition transition
       :transition-fingerprint (:fingerprint described)})))

(defn- comparison-form [comparison value bound]
  ;; This pilot maps Gesso's :integer to Ansatz Int, not Nat: negative
  ;; assignments must remain representable and fail the lower-bound proof.
  (list (case comparison :<= '<= :>= '>=) 'Int value bound))

(defn- source-obligations! [catalog]
  (when-not (and (map? catalog)
                 (= #{:id :declared-operation-ids :initial-condition
                      :invariants :operations}
                    (set (keys catalog)))
                 (= :slot/reservation-lifecycle (:id catalog))
                 (vector? (:operations catalog))
                 (seq (:operations catalog)))
    (reject! :unsupported-catalog {:catalog-id (:id catalog)}))
  (let [ids (mapv :id (:operations catalog))
        declared (:declared-operation-ids catalog)
        invariants (:invariants catalog)]
    (when-not (and (set? declared)
                   (= (count ids) (count (set ids)))
                   (= declared (set ids)))
      (reject! :incomplete-operation-catalog
               {:declared declared :observed ids}))
    ;; Keep the known catalog CLOSED for this pilot. An added/removed
    ;; invariant must be reviewed rather than silently inheriting coverage.
    (when-not (= expected-invariant-ids (set (keys invariants)))
      (reject! :changed-invariant-catalog
               {:expected expected-invariant-ids
                :observed (set (keys invariants))})))
  (let [invariants (mapv (partial claim-invariant! catalog)
                         (sort claim-invariant-ids))
        catalog-fingerprint
        (:fingerprint
         (subject/describe-subject
          {:kind :rule
           :id :gesso.assurance.ansatz-reservation-pilot/catalog
           :interface catalog}))]
    (vec
     (for [operation (:operations catalog)
           :let [{:keys [replacement transition-fingerprint]}
                 (source-transition! operation)]
           {:keys [invariant-id comparison bound]} invariants]
       (let [id (:id operation)
             source-invariant (get-in catalog [:invariants invariant-id])
             exact-subject
             {:kind :operation
              :id id
              :interface
              {:catalog-id (:id catalog)
               :construction (:construction operation)
               :invariant-id invariant-id
               :invariant source-invariant}}]
         {:operation-id id
          :invariant-id invariant-id
          :catalog-fingerprint catalog-fingerprint
          :transition-fingerprint transition-fingerprint
          :source-fingerprint
          (:fingerprint (subject/describe-subject exact-subject))
          :replacement replacement
          :comparison comparison
          :bound bound
          ;; The proof is universally quantified over signed pre/post
          ;; values. We assume ONLY a pre-invariant and the literal-write
          ;; equality, then prove the post-invariant.
          :parameters
          ['pre ':- 'Int
           'post ':- 'Int
           'hpre ':- (comparison-form comparison 'pre bound)
           'hwrite ':- (list '= 'Int 'post replacement)]
          :goal-form (comparison-form comparison 'post bound)})))))

;; These are foundational assumptions of the published Ansatz/Lean arithmetic
;; library, not Gesso business axioms. Naming them is a policy decision, NOT a
;; theorem that these axioms are sound. No `sorryAx` or user axioms are allowed.
(def ^:private permitted-axioms
  #{"propext" "Quot.sound" "Classical.choice"})

(defn- dependency-references [ci]
  ;; Traverse the actual term DAG of each reachable declaration; do not trust
  ;; compiler metadata such as DefinitionVal.all as a substitute for a body.
  (let [tag (kernel/ci-tag ci)
        exprs (cond-> [(kernel/ci-type ci)]
                (#{:def :thm :opaque} tag) (conj (kernel/ci-value ci))
                (= :recursor tag) (into (map #(.rhs %) (.rules ci))))
        refs (reduce (fn [acc expr]
                       (if expr
                         (into acc (non-safe/expression-dependencies expr))
                         acc))
                     #{} exprs)]
    ;; Constructors, mutual inductives, and recursors refer to companion
    ;; declarations through kernel metadata as well as term expressions.
    (cond-> refs
      (= :ctor tag) (conj (.inductName ci))
      (= :induct tag) (into (seq (.ctors ci)))
      (#{:induct :recursor} tag) (into (seq (.all ci))))))

(defn- inspect-proof-dependencies [env goal proof]
  ;; This is an experimental, deliberately conservative closure walk for
  ;; Ansatz 0.2.617. It is not independent kernel soundness certification.
  (loop [pending (vec (into (non-safe/expression-dependencies goal)
                            (non-safe/expression-dependencies proof)))
         visited #{}
         axioms #{}
         unsafe #{}
         unresolved #{}]
    (if (empty? pending)
      {:declarations-scanned (count visited)
       :axioms (vec (sort axioms))
       :non-safe (vec (sort unsafe))
       :unresolved (vec (sort unresolved))}
      (let [name (peek pending)
            remaining (pop pending)]
        (if (contains? visited name)
          (recur remaining visited axioms unsafe unresolved)
          (if-let [ci (kernel/lookup env name)]
            (recur (into remaining (dependency-references ci))
                   (conj visited name)
                   (cond-> axioms (kernel/axiom? ci) (conj (str name)))
                   (cond-> unsafe
                     (or (.isUnsafeDeclaration ci)
                         (non-safe/non-safe-definition? ci))
                     (conj (str name)))
                   unresolved)
            (recur remaining (conj visited name) axioms unsafe
                   (conj unresolved (str name)))))))))

(defn- qualify-proof-dependencies! [report]
  (let [{:keys [axioms non-safe unresolved]} report]
    (when-not (and (map? report)
                   (pos-int? (:declarations-scanned report))
                   (vector? axioms) (every? string? axioms)
                   (vector? non-safe) (every? string? non-safe)
                   (vector? unresolved) (every? string? unresolved)
                   (set/subset? (set axioms) permitted-axioms)
                   (empty? non-safe)
                   (empty? unresolved))
      (throw (ex-info "Ansatz proof fails the explicit dependency policy"
                      {:error/type ::unqualified-proof
                       :reason :unpermitted-proof-dependencies
                       :report report
                       :permitted-axioms permitted-axioms})))
    (assoc report :policy :ansatz-reservation-pilot/lean-core-axioms-v1)))

(defn- check-obligation! [{:keys [parameters goal-form] :as obligation}]
  (let [[goal proof] (binding [ansatz/*verbose* false]
                       (ansatz/prove-law parameters goal-form '[(omega)]))
        env (ansatz/env)]
    (when-not (kernel/verifies? env goal proof)
      (reject! :kernel-rejected
               {:operation-id (:operation-id obligation)
                :invariant-id (:invariant-id obligation)}))
    (let [dependencies (qualify-proof-dependencies!
                        (inspect-proof-dependencies env goal proof))]
      {:description (dissoc obligation :parameters :goal-form)
       :dependencies dependencies
       :goal goal
       :proof proof})))

(defn- check-catalog! [catalog]
  (mapv check-obligation! (source-obligations! catalog)))

(deftest original-catalog-generates-quantified-preservation-proofs
  (let [catalog (example/reservation-catalog)
        results (check-catalog! catalog)
        subjects (set (map (juxt (comp :operation-id :description)
                                 (comp :invariant-id :description)) results))]
    (is (= #{[:slot/reserve :slot/at-most-one-claim]
             [:slot/reserve :slot/nonnegative-claims]
             [:slot/release :slot/at-most-one-claim]
             [:slot/release :slot/nonnegative-claims]}
           subjects))
    (is (= #{[1 :<= 1] [1 :>= 0] [0 :<= 1] [0 :>= 0]}
           (set (map (juxt (comp :replacement :description)
                           (comp :comparison :description)
                           (comp :bound :description)) results))))
    (is (every? (fn [{:keys [goal proof]}]
                  (kernel/verifies? (ansatz/env) goal proof)) results))
    (testing "Every original proof has a closed and explicitly qualified axiom closure"
      (doseq [{:keys [dependencies]} results]
        (is (= :ansatz-reservation-pilot/lean-core-axioms-v1
               (:policy dependencies)))
        (is (pos-int? (:declarations-scanned dependencies)))
        (is (= #{"Classical.choice" "Quot.sound" "propext"}
               (set (:axioms dependencies))))
        (is (empty? (:non-safe dependencies)))
        (is (empty? (:unresolved dependencies)))))
    (testing "an upper-bound proof is not a lower-bound proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (second results))
                                   (:proof (first results))))))
    (testing "an operation's proof is not another operation's proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (first results))
                                   (:proof (nth results 2))))))
    (is (false? (kernel/verifies? (ansatz/env)
                                  (:goal (first results)) nil)))))

(deftest original-transition-observers-support-the-write-premise-on-witnesses
  (let [catalog (example/reservation-catalog)
        ops (:operations catalog)
        invariant-ids claim-invariant-ids]
    ;; Bounded observations are NOT a proof of the interpreters. They test
    ;; whether the original semantic construction supports the equality
    ;; premise used by Ansatz, over enabled and disabled representative
    ;; before/after maps, and whether two existing interpreters agree.
    (doseq [{:keys [id construction] :as op} ops
            status [:available :reserved]
            claims [-1 0 1 2]
            audit [nil :audit/unchanged]]
      (let [{:keys [transition replacement transition-fingerprint]}
            (source-transition! op)
            before {:slot/status status
                    claim-field claims
                    :slot/audit-tag audit}
            after (reduce-kv (fn [st field [form value]]
                               (assert (= :literal form))
                               (assoc st field value))
                             before (:assignments transition))
            primary (abstract/observe-transition! transition before {} after)
            separate (independent/observe-transition! transition before {} after)
            enabled? (= status (get-in construction [:guard :expression 2 1]))]
        (is (= transition-fingerprint (:transition-fingerprint primary)))
        (is (= enabled? (:matches? primary) (:matches? separate)))
        (is (:assignments-hold? primary))
        (is (:assignments-hold? separate))
        (is (= replacement (get after claim-field)))
        (when (and enabled? (<= 0 claims 1))
          (doseq [invariant-id invariant-ids]
            (let [{:keys [comparison bound]}
                  (claim-invariant! catalog invariant-id)]
              (is ((case comparison :<= <= :>= >=)
                   (get after claim-field) bound)))))))
    (testing "real observers reject violating writes and frame mutations"
      (let [op (first ops)
            {:keys [transition]} (source-transition! op)
            before {:slot/status :available claim-field 0
                    :slot/audit-tag :audit/preserved}
            after {:slot/status :reserved claim-field 1
                   :slot/audit-tag :audit/preserved}]
        (is (:matches? (abstract/observe-transition! transition before {} after)))
        (is (:matches? (independent/observe-transition! transition before {} after)))
        (doseq [bad [(assoc after claim-field 2)
                     (assoc after :slot/status :available)
                     (assoc after :slot/audit-tag :audit/changed)]]
          (is (false? (:matches? (abstract/observe-transition!
                                transition before {} bad))))
          (is (false? (:matches? (independent/observe-transition!
                                transition before {} bad)))))))))

(defn- proof-fails? [obligation]
  ;; A malformed source is not evidence that Ansatz refuted a proposition.
  ;; Require successful source translation and an actual proof failure.
  (try (check-obligation! obligation) false
       (catch clojure.lang.ExceptionInfo ex
         (when (= ::unsupported-source (:error/type (ex-data ex)))
           (throw ex))
         true)))

(deftest mutated-source-cannot-silently-inherit-proofs
  (let [source (example/reservation-catalog)
        reserve-index (first (keep-indexed
                              (fn [i op] (when (= :slot/reserve (:id op)) i))
                              (:operations source)))
        replacement-path [:operations reserve-index :construction
                          :replacements claim-field]
        mutated-obligation
        (fn [replacement invariant-id]
          (let [source' (assoc-in source replacement-path
                                  [:literal replacement])]
            (first (filter #(and (= :slot/reserve (:operation-id %))
                                 (= invariant-id (:invariant-id %)))
                           (source-obligations! source')))))]
    (testing "a poststate of two translates but fails the original upper bound"
      (let [obligation (mutated-obligation 2 :slot/at-most-one-claim)]
        (is (= 2 (:replacement obligation)))
        (is (proof-fails? obligation))))
    (testing "a negative Int poststate translates but fails the lower bound"
      (let [obligation (mutated-obligation -1 :slot/nonnegative-claims)]
        (is (= -1 (:replacement obligation)))
        (is (proof-fails? obligation))))
    (testing "arbitrary expressions cannot masquerade as literal writes"
      (is (= :expected-integer-literal
             (-> (try (source-obligations!
                       (assoc-in source replacement-path [:read claim-field]))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "an omitted operation cannot silently be proved"
      (is (= :incomplete-operation-catalog
             (-> (try (source-obligations! (update source :operations pop))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "removed invariants cannot silently inherit coverage"
      (is (= :changed-invariant-catalog
             (-> (try (source-obligations!
                       (update source :invariants dissoc
                               :slot/nonnegative-claims))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "unsupported effects fail closed"
      (is (= :unsupported-transition
             (-> (try (source-obligations!
                       (assoc-in source [:operations reserve-index
                                         :construction :unchecked-effect] :unknown))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "Gesso's original constructor rejects an invalid guard footprint"
      (is (thrown? clojure.lang.ExceptionInfo
                   (source-obligations!
                    (assoc-in source [:operations reserve-index
                                      :construction :guard :readers] #{})))))
    (testing "the pilot rejects a new unproved field in the state projection"
      (is (= :unsupported-transition
             (-> (try (source-obligations!
                       (assoc-in source [:operations reserve-index
                                         :construction :state-types
                                         :slot/extra] {:type :integer}))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "the full catalog fingerprint changes when other invariants change"
      (let [before (first (source-obligations! source))
            mutant (assoc-in source [:invariants :slot/status-domain
                                     :expression 2 1]
                             #{:available})
            after (first (source-obligations! mutant))]
        (is (= (:source-fingerprint before) (:source-fingerprint after)))
        (is (not= (:catalog-fingerprint before)
                  (:catalog-fingerprint after)))))
    (testing "a same-type guard edit changes subject identity"
      (let [mutant (assoc-in source [:operations reserve-index :construction
                                    :guard :expression]
                             [:= [:read :slot/status] [:literal :reserved]])
            before (source-obligations! source)
            after (source-obligations! mutant)]
        (is (not= (:source-fingerprint (first before))
                  (:source-fingerprint (first after))))))))

(deftest proof-dependency-policy-fails-closed
  (let [clean {:declarations-scanned 1 :axioms []
               :non-safe [] :unresolved []}
        denied? (fn [report]
                  (try (qualify-proof-dependencies! report)
                       false
                       (catch clojure.lang.ExceptionInfo e
                         (= :unpermitted-proof-dependencies
                            (:reason (ex-data e))))))]
    (is (= :ansatz-reservation-pilot/lean-core-axioms-v1
           (:policy (qualify-proof-dependencies! clean))))
    (testing "Admitted or new axioms are never silently trusted"
      (is (denied? (assoc clean :axioms ["sorryAx"])))
      (is (denied? (assoc clean :axioms ["Classical.choice" "user.fake"]))))
    (testing "Partial, unsafe, or missing dependencies reject"
      (is (denied? (assoc clean :non-safe ["user.partial"])))
      (is (denied? (assoc clean :unresolved ["missing.theorem"]))))
    (testing "An empty or incomplete closure report is not accepted"
      (is (denied? (assoc clean :declarations-scanned 0)))
      (is (denied? (dissoc clean :unresolved))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false]
    (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-reservation-pilot)]
    (when (pos? (+ fail error))
      (System/exit 1))))
