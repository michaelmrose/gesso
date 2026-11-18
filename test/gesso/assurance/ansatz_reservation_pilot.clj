(ns gesso.assurance.ansatz-reservation-pilot
  "Opt-in Ansatz 0.2.617, source-derived, signed-integer preservation pilot.

   From the ORIGINAL Gesso reservation catalog, construct quantified
   conditional preservation propositions for ALL THREE original invariants
   across EVERY declared operation: two signed-integer claim bounds and one
   closed two-member status enumeration. For pre/post Int states, assume the
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
   actual initial database state, general enum translation correctness, or
   any physical/installed correspondence. The original initial-condition is
   a PREDICATE, not an installed initial state: we additionally kernel-prove
   that it entails each declared invariant and has a nonvacuity witness. The proof declaration closure is inspected under
   an explicit pilot axiom policy; no authority-bearing evidence is issued.
   This pilot accepts only the exact known reservation source shape.

   A nominal proof handle and separately issued expected-subject token now
   identify the exact original operation/invariant/source relationship. A
   Typed Clojure-checked consumer requires BOTH nominal categories; at
   runtime their source-bound subjects must match before proof revalidation.
   Neither the proof nor its expected-subject token is a certificate or an
   application admission/authority credential.

   The exact kernel proposition is now independently re-elaborated from the
   CURRENT Gesso obligation before issuance and consumption. A valid proof
   of a different quantified goal cannot be substituted under unchanged
   source metadata. This checks goal identity, not general translation
   fidelity or interpreter refinement.

   Invoke using the optional dependency alias:
     clojure -M:ansatz:typed:test -m gesso.assurance.ansatz-reservation-pilot

   The filename intentionally does not end in _test.clj: the ordinary JVM
   suite must not acquire an implicit Ansatz dependency."
  (:require [ansatz.core :as ansatz]
            [clojure.java.io :as io]
            [ansatz.kernel.env :as kernel]
            [ansatz.export.non-safe :as non-safe]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as guarded]
            [typed.clojure])
  (:import [java.util WeakHashMap]))

(def ^:private claim-field :slot/active-claims)
(def ^:private status-field :slot/status)
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

(defn- status-invariant! [catalog]
  (let [id :slot/status-domain
        invariant (get-in catalog [:invariants id])
        {:keys [readers expression]} invariant
        raw-values (get-in expression [2 1])]
    ;; The original Gesso enum is an ordinary set of keywords. Encode its
    ;; TWO members deterministically into Int solely for this pilot. Such an
    ;; encoding is an EXPLICIT translation assumption, not a proven general
    ;; theorem about enum interpretation or Clojure equality.
    (when-not (and (= #{:readers :expression} (set (keys invariant)))
                   (= #{status-field} readers)
                   (= [:in [:read status-field] [:literal raw-values]]
                      expression)
                   (set? raw-values)
                   (= #{:available :reserved} raw-values))
      (reject! :unsupported-status-domain
               {:invariant-id id :invariant invariant}))
    {:invariant-id id
     :values raw-values
     :encoding (zipmap (sort-by str raw-values) (range))}))

(defn- status-literal! [form at]
  (when-not (and (vector? form) (= :literal (first form))
                 (= 2 (count form)) (keyword? (second form)))
    (reject! :unsupported-status-literal
             {:at at :expression form}))
  (second form))

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
                   (= {:type :enum :name :slot/status
                       :values #{:available :reserved}}
                      (get state-types status-field))
                   (= {:type :keyword :nullable? true}
                      (get state-types :slot/audit-tag))
                   (= {} input-types)
                   (= #{:slot/status} reads)
                   (= #{:slot/status claim-field} writes)
                   (= writes (set (keys replacements)))
                   (= #{:slot/audit-tag} frame))
      (reject! :unsupported-transition {:operation-id id
                                        :construction construction}))
    (let [replacement (integer-literal! (get replacements claim-field)
                                        [:operations id :replacements claim-field])
          status-replacement
          (status-literal! (get replacements status-field)
                           [:operations id :replacements status-field])
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
                        (get-in semantics [:assignments claim-field]))
                     (= [:literal status-replacement]
                        (get-in semantics [:assignments status-field])))
        (reject! :transition-premise-mismatch {:operation-id id}))
      {:replacement replacement
       :status-replacement status-replacement
       :transition transition
       :transition-fingerprint (:fingerprint described)})))

(defn- comparison-form [comparison value bound]
  ;; This pilot maps Gesso's :integer to Ansatz Int, not Nat: negative
  ;; assignments must remain representable and fail the lower-bound proof.
  (list (case comparison :<= '<= :>= '>=) 'Int value bound))

(defn- status-membership-form [encoding value]
  (let [[first-status second-status] (sort-by str (keys encoding))]
    (list 'Or (list '= 'Int value (get encoding first-status))
              (list '= 'Int value (get encoding second-status)))))

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
    (when-not (= expected-invariant-ids (set (keys invariants)))
      (reject! :changed-invariant-catalog
               {:expected expected-invariant-ids
                :observed (set (keys invariants))})))
  (let [claims (into {} (map (fn [id] [id (claim-invariant! catalog id)])
                             (sort claim-invariant-ids)))
        status (status-invariant! catalog)
        catalog-fingerprint
        (:fingerprint
         (subject/describe-subject
          {:kind :rule
           :id :gesso.assurance.ansatz-reservation-pilot/catalog
           :interface catalog}))
        ;; Source coverage must be a complete Cartesian product; no part of
        ;; the original 2-operation x 3-invariant catalog is allowed to
        ;; disappear behind an unmentioned unsupported case.
        obligations
        (vec
         (for [operation (:operations catalog)
               invariant-id (concat (sort claim-invariant-ids)
                                    [:slot/status-domain])]
           (let [{:keys [replacement status-replacement
                         transition-fingerprint]}
                 (source-transition! operation)
                 id (:id operation)
                 source-invariant (get-in catalog [:invariants invariant-id])
                 subject-data {:kind :operation
                               :id id
                               :interface
                               {:catalog-id (:id catalog)
                                :construction (:construction operation)
                                :invariant-id invariant-id
                                :invariant source-invariant}}
                 common {:operation-id id
                         :invariant-id invariant-id
                         :catalog-fingerprint catalog-fingerprint
                         :transition-fingerprint transition-fingerprint
                         :source-fingerprint
                         (:fingerprint (subject/describe-subject subject-data))}
                 proof-data
                 (if (= invariant-id :slot/status-domain)
                   (let [{:keys [encoding values]} status
                         code (get encoding status-replacement)]
                     (when (nil? code)
                       (reject! :status-assignment-outside-invariant
                                {:operation-id id
                                 :status status-replacement
                                 :allowed values}))
                     {:proof-kind :quantified-conditional-enum-preservation
                      :replacement status-replacement
                      :status-encoding encoding
                      :parameters ['pre ':- 'Int
                                   'post ':- 'Int
                                   'hpre ':- (status-membership-form encoding 'pre)
                                   'hwrite ':- (list '= 'Int 'post code)]
                      :goal-form (status-membership-form encoding 'post)
                      :proof-script (if (= code 0)
                                      '[(subst hwrite) (left) (rfl)]
                                      '[(subst hwrite) (right) (rfl)])})
                   (let [{:keys [comparison bound]} (get claims invariant-id)]
                     {:proof-kind :quantified-conditional-int-preservation
                      :replacement replacement
                      :comparison comparison
                      :bound bound
                      :parameters ['pre ':- 'Int
                                   'post ':- 'Int
                                   'hpre ':- (comparison-form comparison 'pre bound)
                                   'hwrite ':- (list '= 'Int 'post replacement)]
                      :goal-form (comparison-form comparison 'post bound)
                      :proof-script '[(omega)]}))]
             (merge common proof-data))))
        expected (set (for [id (:declared-operation-ids catalog) invariant-id expected-invariant-ids]
                        [id invariant-id]))
        observed (mapv (juxt :operation-id :invariant-id) obligations)]
    (when-not (and (= expected (set observed))
                   (= (count expected) (count observed)))
      (reject! :incomplete-proof-coverage
               {:expected expected :observed observed}))
    obligations))

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

(defn- elaborate-source-proposition! [{:keys [parameters goal-form]}]
  ;; Build the EXACT kernel goal directly from the original source-derived
  ;; signed-Int telescope, without invoking a tactic or using a stored proof.
  ;; This is the same public elaborator Ansatz 0.2.617 uses in prove-law;
  ;; agreement establishes kernel-expression identity, not translator soundness.
  (:type-ansatz
   (ansatz/elab-signature (ansatz/env)
                          (ansatz/parse-params parameters)
                          goal-form)))

(defn- check-obligation! [{:keys [parameters goal-form proof-script] :as obligation}]
  (let [[goal proof] (binding [ansatz/*verbose* false]
                       (ansatz/prove-law parameters goal-form proof-script))
        env (ansatz/env)]
    (when-not (= (elaborate-source-proposition! obligation) goal)
      (reject! :proof-proposition-mismatch
               {:operation-id (:operation-id obligation)
                :invariant-id (:invariant-id obligation)}))
    (when-not (kernel/verifies? env goal proof)
      (reject! :kernel-rejected
               {:operation-id (:operation-id obligation)
                :invariant-id (:invariant-id obligation)}))
    (let [dependencies (qualify-proof-dependencies!
                        (inspect-proof-dependencies env goal proof))]
      {:description (dissoc obligation :parameters :goal-form :proof-script)
       :dependencies dependencies
       :goal goal
       :proof proof})))

(defn- initial-predicate-source! [catalog]
  ;; Gesso declares a predicate selecting acceptable initial states, NOT a
  ;; concrete row or an actual installed database snapshot. No claim of
  ;; physical initial-state validity can follow from these abstract proofs.
  (source-obligations! catalog)
  (let [{:keys [readers expression] :as initial} (:initial-condition catalog)
        invariants (:invariants catalog)
        inv-expressions (into {} (map (fn [[id v]] [(:expression v) id]) invariants))
        leaves (when (and (vector? expression) (= :and (first expression)))
                 (vec (rest expression)))
        expected-expressions (mapv :expression (vals invariants))]
    (when-not (and (= #{:readers :expression} (set (keys initial)))
                   (= #{status-field claim-field} readers)
                   (= 3 (count leaves))
                   (= 3 (count (set expected-expressions)))
                   (= (frequencies expected-expressions)
                      (frequencies leaves)))
      (reject! :unsupported-initial-condition
               {:initial-condition initial
                :expected-invariants (set (keys invariants))}))
    (let [nonnegative (claim-invariant! catalog :slot/nonnegative-claims)
          upper (claim-invariant! catalog :slot/at-most-one-claim)]
      (when-not (and (= :>= (:comparison nonnegative))
                     (= :<= (:comparison upper)))
        (reject! :unsupported-initial-claim-order
                 {:nonnegative nonnegative :upper upper}))
      {:order (mapv inv-expressions leaves)
       :source initial
       :catalog-fingerprint
       (:fingerprint (subject/describe-subject
                      {:kind :rule
                       :id :gesso.assurance.ansatz-reservation-pilot/catalog
                       :interface catalog}))})))

(defn- initial-invariant-proposition! [catalog invariant-id status claims]
  (if (= invariant-id :slot/status-domain)
    (status-membership-form (:encoding (status-invariant! catalog)) status)
    (let [{:keys [comparison bound]} (claim-invariant! catalog invariant-id)]
      (comparison-form comparison claims bound))))

(defn- three-conjunction [forms]
  ;; The known three-leaf Gesso :and is translated to nested Ansatz And.
  ;; A generic conjunction translator is explicitly NOT claimed here.
  (when-not (= 3 (count forms))
    (reject! :unsupported-conjunction-size {:forms forms}))
  (list 'And (nth forms 0)
        (list 'And (nth forms 1) (nth forms 2))))

(defn- witness-script! [catalog order witness-status]
  (let [encoding (:encoding (status-invariant! catalog))
        status-value (get encoding witness-status)
        codes (mapv encoding (sort-by str (keys encoding)))
        step (fn [invariant-id]
               (if (= invariant-id :slot/status-domain)
                 (if (= status-value (first codes))
                   '[(left) (rfl)]
                   '[(right) (rfl)])
                 '[(omega)]))]
    (vec (concat '[(constructor)]
                 (step (nth order 0))
                 '[(constructor)]
                 (step (nth order 1))
                 (step (nth order 2))))))

(defn- source-initialization-obligations! [catalog]
  (let [{:keys [order catalog-fingerprint]} (initial-predicate-source! catalog)
        status (status-invariant! catalog)
        witness-status (first (sort-by str (:values status)))
        witness-claims (:bound (claim-invariant! catalog :slot/nonnegative-claims))
        abstract-forms (mapv #(initial-invariant-proposition!
                              catalog % 'status 'claim) order)
        initial-proposition (three-conjunction abstract-forms)
        witness-forms (mapv #(initial-invariant-proposition!
                             catalog % (get (:encoding status) witness-status)
                             witness-claims) order)
        descriptions (mapv (fn [i invariant-id]
                             {:obligation-kind :initial-predicate-entails-invariant
                              :invariant-id invariant-id
                              :catalog-fingerprint catalog-fingerprint
                              :parameters ['status ':- 'Int
                                           'claim ':- 'Int
                                           'hinit ':- initial-proposition]
                              :goal-form (nth abstract-forms i)
                              ;; Destructure ORIGINAL :and expression order
                              ;; rather than assuming a handwritten index.
                              :proof-script
                              [(list 'rcases 'hinit '[hi0 [hi1 hi2]])
                               (list 'exact (nth '[hi0 hi1 hi2] i))]})
                           (range 3) order)
        witness {:obligation-kind :initial-predicate-nonvacuity-witness
                 :invariant-id nil
                 :catalog-fingerprint catalog-fingerprint
                 :witness {status-field witness-status
                           claim-field witness-claims
                           :slot/audit-tag nil}
                 :parameters []
                 :goal-form (three-conjunction witness-forms)
                 :proof-script (witness-script! catalog order witness-status)}]
    ;; One implication proof per original invariant, plus an actual
    ;; mathematical witness that the conjunction is satisfiable. None of
    ;; these are evidence of an actual initial DB row or installed snapshot.
    (conj descriptions witness)))

(defn- check-initialization! [catalog]
  (mapv check-obligation! (source-initialization-obligations! catalog)))

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
             [:slot/release :slot/nonnegative-claims]
             [:slot/reserve :slot/status-domain]
             [:slot/release :slot/status-domain]}
           subjects))
    (is (= 6 (count results)))
    (is (= #{[1 :<= 1] [1 :>= 0] [0 :<= 1] [0 :>= 0]}
           (set (map (juxt (comp :replacement :description)
                           (comp :comparison :description)
                           (comp :bound :description))
                     (remove #(= :slot/status-domain
                                 (get-in % [:description :invariant-id]))
                             results)))))
    (is (= #{:available :reserved}
           (set (map (comp :replacement :description)
                     (filter #(= :slot/status-domain
                                 (get-in % [:description :invariant-id]))
                             results)))))
    (is (every? (fn [{:keys [goal proof]}]
                  (kernel/verifies? (ansatz/env) goal proof)) results))
    (testing "Every original proof has a closed and explicitly qualified axiom closure"
      (doseq [{:keys [dependencies]} results]
        (is (= :ansatz-reservation-pilot/lean-core-axioms-v1
               (:policy dependencies)))
        (is (pos-int? (:declarations-scanned dependencies)))
        (is (set/subset? (set (:axioms dependencies)) permitted-axioms))
        (is (empty? (:non-safe dependencies)))
        (is (empty? (:unresolved dependencies)))))
    (testing "Arithmetic proofs still depend on their explicit axiom closure"
      (doseq [entry (remove #(= :slot/status-domain
                               (get-in % [:description :invariant-id]))
                           results)]
        (is (= #{"Classical.choice" "Quot.sound" "propext"}
               (set (get-in entry [:dependencies :axioms]))))))
    (testing "an upper-bound proof is not a lower-bound proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (second results))
                                   (:proof (first results))))))
    (testing "an operation's proof is not another operation's proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (first results))
                                   (:proof (nth results 3))))))
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
    (testing "a silently narrowed status invariant is NOT accepted"
      (is (= :unsupported-status-domain
             (-> (try (source-obligations!
                       (assoc-in source [:invariants :slot/status-domain
                                         :expression 2 1]
                                 #{:available}))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "the full catalog fingerprint also binds the initial condition"
      (let [before (first (source-obligations! source))
            mutant (assoc-in source [:initial-condition :expression 2 2]
                             [:literal 2])
            after (first (source-obligations! mutant))]
        (is (= (:source-fingerprint before) (:source-fingerprint after)))
        (is (not= (:catalog-fingerprint before)
                  (:catalog-fingerprint after)))))
    (testing "an assignment to an unknown enum member cannot inherit a proof"
      (is (thrown? clojure.lang.ExceptionInfo
                   (source-obligations!
                    (assoc-in source [:operations reserve-index
                                      :construction :replacements status-field]
                              [:literal :slot/unknown])))))
    (testing "a same-type guard edit changes subject identity"
      (let [mutant (assoc-in source [:operations reserve-index :construction
                                    :guard :expression]
                             [:= [:read :slot/status] [:literal :reserved]])
            before (source-obligations! source)
            after (source-obligations! mutant)]
        (is (not= (:source-fingerprint (first before))
                  (:source-fingerprint (first after))))))))

(deftest original-initial-predicate-entails-every-invariant-and-is-inhabited
  (let [catalog (example/reservation-catalog)
        results (check-initialization! catalog)
        projections (filter #(= :initial-predicate-entails-invariant
                                 (get-in % [:description :obligation-kind])) results)
        witnesses (filter #(= :initial-predicate-nonvacuity-witness
                               (get-in % [:description :obligation-kind])) results)]
    (is (= 4 (count results)))
    (is (= expected-invariant-ids
           (set (map #(get-in % [:description :invariant-id]) projections))))
    (is (= 1 (count witnesses)))
    (is (= {status-field :available
            claim-field 0
            :slot/audit-tag nil}
           (get-in (first witnesses) [:description :witness])))
    (doseq [{:keys [goal proof dependencies]} results]
      (is (kernel/verifies? (ansatz/env) goal proof))
      (is (= :ansatz-reservation-pilot/lean-core-axioms-v1 (:policy dependencies)))
      (is (set/subset? (set (:axioms dependencies)) permitted-axioms))
      (is (empty? (:non-safe dependencies)))
      (is (empty? (:unresolved dependencies))))
    (testing "A different original proposition does not inherit an initialization proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (second results))
                                   (:proof (first results))))))))

(deftest mutated-initial-predicate-fails-closed
  (let [catalog (example/reservation-catalog)
        errors (fn [source]
                 (try (source-initialization-obligations! source)
                      nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))
        broken-branch (assoc-in catalog [:initial-condition :expression 0] :or)
        missing-conjunct (update-in catalog [:initial-condition :expression] pop)
        duplicate-conjunct (assoc-in catalog [:initial-condition :expression 3]
                                     (get-in catalog [:invariants :slot/status-domain :expression]))
        wrong-readers (assoc-in catalog [:initial-condition :readers]
                                #{:slot/status})]
    (doseq [source [broken-branch missing-conjunct
                    duplicate-conjunct wrong-readers]]
      (is (= :unsupported-initial-condition (errors source))))
    (testing "A permutation of the same three declared predicates stays provable"
      (let [reordered (assoc-in catalog [:initial-condition :expression]
                                (into [:and] (reverse (rest (get-in catalog
                                                        [:initial-condition :expression])))))
            results (check-initialization! reordered)]
        (is (= 4 (count results)))
        (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                    results))))
    (testing "An inconsistent initial predicate must not gain vacuous safety"
      ;; Both the invariant declaration and the initial source predicate
      ;; change together; syntactic coverage is still complete. The
      ;; source-derived witness fails because claims >= 0 and claims <= -1
      ;; have no common state. This is a genuine failed proof obligation.
      (let [upper' [:<= [:read claim-field] [:literal -1]]
            mutant (-> catalog
                       (assoc-in [:invariants :slot/at-most-one-claim :expression] upper')
                       (assoc-in [:initial-condition :expression 3] upper'))
            obligations (source-initialization-obligations! mutant)
            witness (last obligations)]
        (is (= :initial-predicate-nonvacuity-witness
               (:obligation-kind witness)))
        (is (proof-fails? witness))))))

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

;; Pilot qualification never upgrades the six conditional theorems to
;; operation certificates. A directly constructed instance is NOT accepted:
;; only handles issued by the kernel/audit/source path enter this JVM registry.
(deftype AuditedAnsatzPilotHandle []
  Object
  (toString [_] "#<gesso.assurance/ansatz-conditional-pilot>"))

;; A subject token is independent of a proof handle. The caller must identify
;; the expected original obligation, not merely supply *some* valid proof.
;; Nominal types alone are not unforgeable, so both categories are validated
;; against JVM-local issuance registries and the current original source.
(deftype ExpectedAnsatzPilotSubject []
  Object
  (toString [_] "#<gesso.assurance/ansatz-expected-subject>"))

(def ^:private issued-pilot-proofs (WeakHashMap.))
(def ^:private issued-pilot-subjects (WeakHashMap.))

(defn- pilot-failure! [reason]
  (throw (ex-info "Invalid or stale Ansatz pilot evidence"
                  {:error/type ::unqualified-pilot-handle
                   :reason reason})))

(defn- original-catalog! [catalog]
  ;; The pilot only speaks for its single original source. A different catalog
  ;; with compatible types cannot receive this pilot's assurance category.
  (when-not (= catalog (example/reservation-catalog))
    (pilot-failure! :not-original-reservation-catalog))
  (source-obligations! catalog))

(defn- original-obligation! [catalog operation-id invariant-id]
  (let [obligations (original-catalog! catalog)
        matching (filter #(and (= operation-id (:operation-id %))
                               (= invariant-id (:invariant-id %))) obligations)]
    (when-not (= 1 (count matching))
      (pilot-failure! :unknown-or-ambiguous-proof-subject))
    (first matching)))

(defn- issue-expected-pilot-subject! [catalog operation-id invariant-id]
  (let [obligation (original-obligation! catalog operation-id invariant-id)
        expected (ExpectedAnsatzPilotSubject.)]
    (locking issued-pilot-subjects
      (.put issued-pilot-subjects expected
            (dissoc obligation :parameters :goal-form :proof-script)))
    expected))

(defn- issue-audited-pilot-handle! [catalog operation-id invariant-id]
  (let [obligation (original-obligation! catalog operation-id invariant-id)]
    ;; This includes real Ansatz elaboration, strict kernel checking, and the
    ;; dependency-closure policy. Caller-provided :proved fields are ignored.
    (let [checked (check-obligation! obligation)
          handle (AuditedAnsatzPilotHandle.)]
      (locking issued-pilot-proofs
        (.put issued-pilot-proofs handle checked))
      handle)))

(defn- validated-pilot-description! [handle expected-subject catalog]
  (when-not (instance? AuditedAnsatzPilotHandle handle)
    (pilot-failure! :wrong-evidence-category))
  (when-not (instance? ExpectedAnsatzPilotSubject expected-subject)
    (pilot-failure! :wrong-proof-subject-category))
  (let [issued (locking issued-pilot-proofs
                 (.get issued-pilot-proofs handle))
        expected (locking issued-pilot-subjects
                   (.get issued-pilot-subjects expected-subject))]
    (when-not issued
      (pilot-failure! :unregistered-pilot-handle))
    (when-not expected
      (pilot-failure! :unregistered-proof-subject))
    (let [description (:description issued)
          current (original-obligation! catalog
                    (:operation-id expected) (:invariant-id expected))
          current-description (dissoc current :parameters :goal-form :proof-script)]
      (when-not (= expected current-description)
        (pilot-failure! :source-subject-changed))
      ;; A valid proof of one source obligation cannot satisfy a consumer
      ;; expecting a DIFFERENT operation/invariant/source proposition.
      (when-not (= description expected)
        (pilot-failure! :proof-subject-mismatch))
      ;; Proof metadata is not the proposition. Re-elaborate the exact
      ;; CURRENT source obligation, including its precondition and write
      ;; binders, and compare kernel expressions before checking the proof.
      ;; A proof valid for the wrong statement must never qualify here.
      (let [env (ansatz/env)
            goal (:goal issued)
            proof (:proof issued)
            expected-goal (elaborate-source-proposition! current)
            _ (when-not (= expected-goal goal)
                (pilot-failure! :proof-proposition-mismatch))
            current-deps (when (kernel/verifies? env expected-goal proof)
                           (qualify-proof-dependencies!
                            (inspect-proof-dependencies env goal proof)))]
        (when-not (= current-deps (:dependencies issued))
          (pilot-failure! :proof-or-dependency-not-current))
        ;; This descriptive map is intentionally copyable and nonauthoritative.
        {:operation-id (:operation-id description)
         :invariant-id (:invariant-id description)
         :source-fingerprint (:source-fingerprint description)
         :catalog-fingerprint (:catalog-fingerprint description)
         :transition-fingerprint (:transition-fingerprint description)
         :proof-kind (:proof-kind description)
         :checking :ansatz-kernel-with-audited-dependency-closure
         :axiom-policy (:policy current-deps)
         :interpreter-correspondence :not-universally-proved
         :physical-correspondence :not-established
         :certificate :not-issued
         :admission :not-admitted
         :authority :none}))))

(defn require-current-audited-pilot-proof!
  "Revalidate a live proof against its separate original-source subject.
   Checked callers must supply two distinct nominal tokens. A valid proof
   for the wrong subject is rejected at runtime, not treated as admissible."
  [handle expected-subject catalog]
  (validated-pilot-description! handle expected-subject catalog))

;; Source-backed Typed Clojure check for the REAL public handoff. This does
;; NOT typecheck Ansatz internals, the registry's implementation, or the proof
;; translator. The latter remain runtime and mathematical proof obligations.
(def ^:private typed-check-config {:max-processors 1
                                  :check-form-eval :never})

(defn- checked-public-source! []
  (let [file (or (io/resource "gesso/assurance/ansatz_reservation_pilot.clj")
                 (pilot-failure! :original-pilot-source-missing))]
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (let [forms (loop [acc []]
                    (let [form (read {:eof ::done} reader)]
                      (if (= ::done form) acc (recur (conj acc form)))))
            found (filter #(and (seq? %)
                                (= 'defn (first %))
                                (= 'require-current-audited-pilot-proof!
                                   (second %))) forms)]
        (when-not (= 1 (count found))
          (pilot-failure! :original-pilot-consumer-missing))
        (let [original (first found)]
          (when-not (= '(validated-pilot-description! handle expected-subject catalog)
                       (last original))
            (pilot-failure! :original-pilot-consumer-changed))
          original)))))

(defn- check-typed-pilot-handoff! []
  (let [handle-type
        'gesso.assurance.ansatz_reservation_pilot.AuditedAnsatzPilotHandle
        subject-type
        'gesso.assurance.ansatz_reservation_pilot.ExpectedAnsatzPilotSubject
        function 'gesso.assurance.ansatz-reservation-pilot/require-current-audited-pilot-proof!]
    (eval (list 'typed.clojure/ann-datatype handle-type '[]))
    (eval (list 'typed.clojure/ann-datatype subject-type '[]))
    (eval (list 'typed.clojure/ann
                'gesso.assurance.ansatz-reservation-pilot/validated-pilot-description!
                (list 'typed.clojure/IFn
                      [handle-type subject-type 'typed.clojure/Any :-> 'typed.clojure/Any])))
    (eval (list 'typed.clojure/ann function
                (list 'typed.clojure/IFn
                      [handle-type subject-type 'typed.clojure/Any :-> 'typed.clojure/Any])))
    (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
          check (fn [form] (checker form :check-config typed-check-config))
          original (checked-public-source!)
          correct (binding [*ns* (the-ns 'gesso.assurance.ansatz-reservation-pilot)]
                    (check original))]
      (when (or (:ex correct) (seq (:type-errors correct)))
        (throw (ex-info "Original Ansatz pilot handoff failed static typing"
                        {:type-errors (mapv str (:type-errors correct))}
                        (:ex correct))))
      ;; The negative cases must fail with actual type errors, not checker
      ;; crashes. This makes the type distinction executable, not decorative.
      (doseq [[label form]
              [[:raw-map '(require-current-audited-pilot-proof!
                            {} (ExpectedAnsatzPilotSubject.) {})]
               [:conditional-map
                '(require-current-audited-pilot-proof!
                  {:proof-status :checked-conditional-inference}
                  (ExpectedAnsatzPilotSubject.) {})]
               [:nil-evidence
                '(require-current-audited-pilot-proof!
                  nil (ExpectedAnsatzPilotSubject.) {})]
               [:raw-subject
                '(require-current-audited-pilot-proof!
                  (AuditedAnsatzPilotHandle.) {} {})]
               [:nil-subject
                '(require-current-audited-pilot-proof!
                  (AuditedAnsatzPilotHandle.) nil {})]]]
        (let [result (binding [*ns* (the-ns 'gesso.assurance.ansatz-reservation-pilot)]
                       (check form))
              kinds (mapv :type-error (:type-errors result))]
          (when-not (and (nil? (:ex result)) (seq kinds)
                         (every? #{:typed.clojure/type-mismatch-error
                                   :typed.clojure/app-type-error} kinds))
            (throw (ex-info "Invalid evidence escaped nominal static typing"
                            {:label label :types kinds}
                            (:ex result))))))
      true)))

(deftest audited-ansatz-proof-is-bound-to-the-consumer-expected-subject
  (let [catalog (example/reservation-catalog)
        handle (issue-audited-pilot-handle!
                catalog :slot/reserve :slot/at-most-one-claim)
        expected (issue-expected-pilot-subject!
                  catalog :slot/reserve :slot/at-most-one-claim)
        wrong-operation (issue-expected-pilot-subject!
                         catalog :slot/release :slot/at-most-one-claim)
        wrong-invariant (issue-expected-pilot-subject!
                         catalog :slot/reserve :slot/nonnegative-claims)
        accepted (require-current-audited-pilot-proof! handle expected catalog)
        forged (AuditedAnsatzPilotHandle.)
        forged-subject (ExpectedAnsatzPilotSubject.)
        rejection (fn [value subject source]
                    (try (require-current-audited-pilot-proof!
                          value subject source)
                         nil
                         (catch clojure.lang.ExceptionInfo ex
                           (:reason (ex-data ex)))))]
    (is (instance? AuditedAnsatzPilotHandle handle))
    (is (instance? ExpectedAnsatzPilotSubject expected))
    (is (= :slot/reserve (:operation-id accepted)))
    (is (= :slot/at-most-one-claim (:invariant-id accepted)))
    (is (= :quantified-conditional-int-preservation (:proof-kind accepted)))
    (is (= :ansatz-kernel-with-audited-dependency-closure (:checking accepted)))
    (is (= :ansatz-reservation-pilot/lean-core-axioms-v1 (:axiom-policy accepted)))
    (is (= :not-issued (:certificate accepted)))
    (is (= :not-admitted (:admission accepted)))
    (is (= :none (:authority accepted)))
    (testing "Valid proof for the WRONG original obligation is refused"
      (is (= :proof-subject-mismatch
             (rejection handle wrong-operation catalog)))
      (is (= :proof-subject-mismatch
             (rejection handle wrong-invariant catalog))))
    (testing "Neither nominal category is interchangeable with a map"
      (is (= :wrong-evidence-category (rejection accepted expected catalog)))
      (is (= :wrong-evidence-category (rejection {} expected catalog)))
      (is (= :wrong-proof-subject-category (rejection handle {} catalog)))
      (is (= :wrong-proof-subject-category (rejection handle nil catalog))))
    (testing "Directly constructed nominal instances are not registered"
      (is (= :unregistered-pilot-handle (rejection forged expected catalog)))
      (is (= :unregistered-proof-subject
             (rejection handle forged-subject catalog))))
    (testing "Source drift invalidates BOTH the proof and expected subject"
      (is (= :not-original-reservation-catalog
             (rejection handle expected (assoc catalog :id :slot/changed))))
      (is (= :not-original-reservation-catalog
             (rejection handle expected
                        (assoc-in catalog
                                  [:operations 0 :construction :replacements claim-field]
                                  [:literal 2])))))))

(deftest status-domain-proof-can-be-consumed-for-each-original-operation
  (let [catalog (example/reservation-catalog)]
    (doseq [id [:slot/reserve :slot/release]]
      (let [handle (issue-audited-pilot-handle!
                    catalog id :slot/status-domain)
            expected (issue-expected-pilot-subject!
                      catalog id :slot/status-domain)
            report (require-current-audited-pilot-proof!
                    handle expected catalog)]
        (is (= id (:operation-id report)))
        (is (= :slot/status-domain (:invariant-id report)))
        (is (= :quantified-conditional-enum-preservation
               (:proof-kind report)))
        (is (= :not-issued (:certificate report)))
        (is (= :none (:authority report)))))))

(deftest proof-issuance-rejects-a-valid-but-unrelated-kernel-proposition
  (let [catalog (example/reservation-catalog)
        original (original-obligation! catalog
                   :slot/reserve :slot/at-most-one-claim)
        other (original-obligation! catalog
                :slot/release :slot/nonnegative-claims)
        other-checked (check-obligation! other)]
    ;; This is genuinely kernel-checked proof evidence, just not for the
    ;; source proposition being requested. The mismatch must reject at
    ;; issuance, not be hidden behind a valid :goal/:proof pair.
    (is (kernel/verifies? (ansatz/env)
                          (:goal other-checked) (:proof other-checked)))
    (is (= :proof-proposition-mismatch
           (-> (try (with-redefs [ansatz/prove-law
                                 (fn [& _]
                                   [(:goal other-checked)
                                    (:proof other-checked)])]
                      (check-obligation! original))
                    nil
                    (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
               :reason)))))

(deftest proof-consumption-rejects-valid-substituted-theorems
  (let [catalog (example/reservation-catalog)
        handle (issue-audited-pilot-handle!
                catalog :slot/reserve :slot/at-most-one-claim)
        expected (issue-expected-pilot-subject!
                  catalog :slot/reserve :slot/at-most-one-claim)
        original (locking issued-pilot-proofs
                   (.get issued-pilot-proofs handle))
        other-operations
        [(original-obligation! catalog :slot/release :slot/at-most-one-claim)
         (original-obligation! catalog :slot/reserve :slot/nonnegative-claims)
         ;; A proof about the same declared operation and invariant, but a
         ;; modified literal assignment, still has a different proposition.
         (first (filter #(and (= :slot/reserve (:operation-id %))
                              (= :slot/at-most-one-claim (:invariant-id %)))
                        (source-obligations!
                         (assoc-in catalog
                                   [:operations 0 :construction :replacements
                                    claim-field]
                                   [:literal 0]))))]]
    (is (= :slot/reserve
           (:operation-id (require-current-audited-pilot-proof!
                           handle expected catalog))))
    (doseq [source other-operations]
      (let [alternate (check-obligation! source)
            ;; Simulate a corrupted issuance entry that retains matching
            ;; nominal handles and ORIGINAL source metadata while holding
            ;; an independently kernel-valid proof for another proposition.
            swapped (assoc original
                           :goal (:goal alternate)
                           :proof (:proof alternate)
                           :dependencies (:dependencies alternate))]
        (is (not= (:goal original) (:goal alternate)))
        (is (kernel/verifies? (ansatz/env)
                              (:goal alternate) (:proof alternate)))
        (try
          (locking issued-pilot-proofs
            (.put issued-pilot-proofs handle swapped))
          (is (= :proof-proposition-mismatch
                 (-> (try (require-current-audited-pilot-proof!
                           handle expected catalog)
                          nil
                          (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                     :reason)))
          (finally
            (locking issued-pilot-proofs
              (.put issued-pilot-proofs handle original))))))
    (is (= :slot/reserve
           (:operation-id (require-current-audited-pilot-proof!
                           handle expected catalog))))))

(deftest original-audited-handoff-is-statically-distinct-from-ordinary-evidence
  (is (true? (check-typed-pilot-handoff!))))

(defn -main [& _]
  (binding [ansatz/*verbose* false]
    (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-reservation-pilot)]
    (when (pos? (+ fail error))
      (System/exit 1))))
