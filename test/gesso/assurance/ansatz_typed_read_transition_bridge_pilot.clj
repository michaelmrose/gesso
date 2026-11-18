(ns gesso.assurance.ansatz-typed-read-transition-bridge-pilot
  "Opt-in v1751/v1753: exact conditional typed-read observation-to-induction step.

   Construct the exact mixed typed-read trace STEP generated in v1750 from
   ORIGINAL guarded replacement operations. Instantiate it over typed before,
   input/read and after values. Kernel-check source-operation and stutter
   implications from every normalized tracked effect and every REQUIRED
   read-source invariant premise to the SAME quantified induction step.

   A current original and independent abstract observer must agree with a
   separately recomputed closed source relation before an observation may be
   associated with the proved trace branch. This is a per-invocation abstract
   correspondence CHECK. v1753 additionally re-evaluates the FULL original
   guarded source relation at the bridge boundary, so even a fabricated
   lower-layer verdict cannot conceal a failing guard or untracked write.
   This is not a general proof that the original interpreter,
   JVM, XTDB, all installed writers or source translation are sound.
   No application certification, admission, physical authority or downstream
   Typed Clojure/Ansatz requirement is conferred."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.walk :as walk]
            [gesso.assurance.ansatz-typed-read-pilot]
            [gesso.assurance.ansatz-typed-read-induction-pilot]
            [gesso.assurance.ansatz-mixed-initialization-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.independent-transition-semantics :as independent]))

(def ^:private read-ns 'gesso.assurance.ansatz-typed-read-pilot)
(def ^:private induction-ns 'gesso.assurance.ansatz-typed-read-induction-pilot)
(def ^:private init-ns 'gesso.assurance.ansatz-mixed-initialization-pilot)
(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private outstanding
  #{:source-expression-translation-and-enum-codes
    :original-interpreter-and-independent-observer-universal-correctness
    :actual-xtdb-prestate-input-and-poststate-correspondence
    :complete-installed-writer-and-initialization-inventory
    :actual-read-source-premise-enforcement
    :universal-installed-step-exhaustiveness
    :ansatz-kernel-and-axiom-trust})

(defn- refuse! [reason detail]
  (throw (ex-info "Unqualified typed-read trace bridge"
                  {:reason reason :details detail})))

(defn- pilot! [ns-name name & args]
  (let [v (ns-resolve ns-name name)]
    (when-not v (refuse! :missing-source-pilot {:ns ns-name :name name}))
    (apply @v args)))

(defn- eq-int [a b] (list '= 'Int a b))
(defn- pair-values [fields label]
  (zipmap fields (map #(symbol (str label %)) (range (count fields)))))

(defn- step-branches! [form]
  (if (and (seq? form) (= 'Or (first form)) (= 3 (count form)))
    (into [(second form)] (step-branches! (nth form 2)))
    [form]))

(defn- one-step! [catalog]
  (let [source (pilot! induction-ns 'catalog-source! catalog)
        checked-induction (pilot! induction-ns 'check-induction! catalog)
        theorem (pilot! induction-ns 'trace-obligation! source)
        traces (pilot! induction-ns 'trace-sources! source)
        fields (:fields source)
        before (pair-values fields "pre")
        after (pair-values fields "post")
        reader-ids (vec (sort (keys (:read-functions traces))))
        reader-symbols (zipmap reader-ids
                               (map #(symbol (str "read" %)) (range)))
        trace-functions (zipmap fields
                                (map #(symbol (str "trace" %)) (range)))
        substitution (into {}
                           (concat
                            (mapcat (fn [field]
                                      (let [f (get trace-functions field)]
                                        [[(list f 'k) (get before field)]
                                         [(list f '(Nat.succ k)) (get after field)]]))
                                    fields)
                            (for [[id f] (:read-functions traces)]
                              [(list f 'k) (get reader-symbols id)])))
        quantifier (last (:parameters theorem))
        raw-step (when (and (seq? quantifier)
                            (= 'forall (first quantifier))
                            (= '[k :- Nat] (second quantifier)))
                   (nth quantifier 2 nil))
        projected (walk/postwalk-replace substitution raw-step)
        actual (step-branches! projected)
        premise-by-op (group-by :operation-id (:read-premises theorem))
        operations (:operations traces)
        expected
        (mapv (fn [{:keys [operation-id effects transition-fingerprint]}]
                (let [original (first (filter #(= operation-id (:id %))
                                              (:sources source)))
                      leaves (vec (concat
                                   (for [field fields
                                         :let [effect (get effects field)
                                               rhs (case (:kind effect)
                                                     :framed (get before field)
                                                     :read (get reader-symbols (:reader-id effect))
                                                     (refuse! :unsupported-trace-effect
                                                              {:field field :effect effect}))]]
                                     (eq-int (get after field) rhs))
                                   (for [{:keys [formula]} (get premise-by-op operation-id)]
                                     (walk/postwalk-replace substitution formula))))
                      source-assignments (get-in original [:semantics :assignments])
                      source-frames (get-in original [:semantics :frame-fields])]
                  (when-not (and original (= transition-fingerprint
                                                 (:transition-fingerprint original))
                                 (= (set fields) (set (keys effects)))
                                 (every? (fn [field]
                                           (let [effect (get effects field)
                                                 assignment (get source-assignments field)]
                                             (case (:kind effect)
                                               :framed (and (nil? assignment)
                                                            (contains? source-frames field))
                                               :read (and (= [:read (:reader effect)] assignment)
                                                          (not (contains? source-frames field))
                                                          (= (:reader-id effect)
                                                             [(:reader-origin effect) (:reader effect)]))
                                               false))) fields))
                    (refuse! :read-source-transition-mismatch
                             {:operation operation-id :effects effects}))
                  {:operation-id operation-id :transition-fingerprint transition-fingerprint
                   :effects effects :leaves leaves
                   :branch (pilot! init-ns 'conjunction leaves)}))
              operations)
        stutter-leaves (mapv #(eq-int (get after %) (get before %)) fields)
        branches (conj expected {:operation-id nil :transition-fingerprint nil
                                 :effects (into {} (map (fn [f] [f {:kind :framed}]) fields))
                                 :leaves stutter-leaves
                                 :branch (pilot! init-ns 'conjunction stutter-leaves)})]
    (when-not (and raw-step
                   (= (:fingerprint source) (:source-fingerprint theorem)
                      (:catalog-fingerprint checked-induction))
                   (= (:operation-ids theorem)
                      (mapv :operation-id operations))
                   (= reader-ids (vec (sort (keys (:read-functions theorem)))))
                   (= (:read-functions theorem) (:read-functions traces))
                   (= (count actual) (count branches))
                   (= actual (mapv :branch branches))
                   (= (set (map :operation-id operations))
                      (set (map :id (:sources source))))
                   (= (set (map :operation-id (:read-premises theorem)))
                      (set (for [op operations
                                 [_ e] (:effects op)
                                 :when (= :read (:kind e))]
                             (:operation-id op))))
                   (not-any? (set (vals trace-functions))
                             (tree-seq coll? seq projected))
                   (not-any? (set (vals (:read-functions traces)))
                             (tree-seq coll? seq projected)))
      (refuse! :source-derived-induction-step-not-identical
               {:actual actual :expected (mapv :branch branches)}))
    {:source source :induction checked-induction :theorem theorem
     :fields fields :before before :after after
     :reader-symbols reader-symbols :trace-functions trace-functions
     :operations operations :branches branches :projected projected
     :catalog-fingerprint (:fingerprint source)}))

(defn- tactics [index leaves operation-count]
  (vec (concat (repeat index '(right))
               (when (< index operation-count) ['(left)])
               (mapcat (fn [i]
                         (concat (when (< i (dec leaves)) ['(constructor)])
                                 [(list 'exact (symbol (str "h" i)))]))
                       (range leaves)))))

(defn- bridge-obligations! [catalog]
  (let [model (one-step! catalog)
        {:keys [fields before after reader-symbols branches projected]} model
        obligations (mapv
                     (fn [index {:keys [operation-id leaves transition-fingerprint]}]
                       {:obligation-kind (if operation-id :typed-read-original-operation-step-bridge
                                             :typed-read-abstract-stutter-step-bridge)
                        :catalog-fingerprint (:catalog-fingerprint model)
                        :induction-fingerprint (get-in model [:theorem :source-fingerprint])
                        :operation-id operation-id
                        :transition-fingerprint transition-fingerprint
                        :parameters
                        (vec (concat
                              (mapcat (fn [field] [(get before field) ':- 'Int
                                                   (get after field) ':- 'Int]) fields)
                              (mapcat (fn [[_ sym]] [sym ':- 'Int])
                                      (sort-by key reader-symbols))
                              (mapcat (fn [i leaf] [(symbol (str "h" i)) ':- leaf])
                                      (range) leaves)))
                        :goal-form projected
                        :proof-script (tactics index (count leaves)
                                               (dec (count branches)))})
                     (range) branches)]
    (when-not (and (= (count obligations) (count branches))
                   (every? #(= projected (:goal-form %)) obligations)
                   (= (mapv :operation-id branches)
                      (mapv :operation-id obligations)))
      (refuse! :incomplete-bridge-obligations {:obligations obligations}))
    {:model model :obligations obligations}))

(defn- check-bridge! [catalog]
  (let [{:keys [model obligations]} (bridge-obligations! catalog)
        checked (mapv #(pilot! proof-ns 'check-obligation! %) obligations)]
    (when-not (and (= (count checked) (count obligations))
                   (every? (fn [{:keys [goal proof description dependencies]}]
                             (and (kernel/verifies? (ansatz/env) goal proof)
                                  (= (:catalog-fingerprint model)
                                     (:catalog-fingerprint description))
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) checked))
      (refuse! :kernel-bridge-proof-not-verified {:count (count checked)}))
    {:model model :checked checked :bridge-count (count checked)
     :induction-proof (get-in model [:induction :trace-proof])
     :correspondence :source-relative-per-invocation-abstract-only
     :universal-observer-correctness :not-proved
     :step-exhaustiveness :not-proved
     :unproved-read-targets (get-in model [:theorem :unproved-read-targets])
     :physical-correspondence :not-established
     :open-assumptions outstanding
     :authority :none :admission :not-admitted :certificate :not-issued}))

(defn- encoded! [model field value]
  (pilot! init-ns 'witness-proposition-value
          {:state-types (get-in model [:source :field-types])} field value))

(defn- reader-field! [model id]
  (let [candidates (set (for [op (:operations model)
                              [field effect] (:effects op)
                              :when (= id (:reader-id effect))]
                          field))
        types (set (map #(get-in model [:source :field-types %]) candidates))]
    (when-not (and (seq candidates) (= 1 (count types)))
      (refuse! :ambiguous-read-source-encoding {:reader id :fields candidates}))
    (first (sort candidates))))

(defn- variables! [model before inputs after]
  (let [fields (:fields model)
        reader-values (for [[id sym] (:reader-symbols model)
                            :let [[origin reader] id
                                  value (get (if (= origin :input) inputs before) reader)
                                  field (reader-field! model id)]]
                        [sym (encoded! model field value)])]
    (into {} (concat
              (mapcat (fn [field]
                        [[(get (:before model) field)
                          (encoded! model field (get before field))]
                         [(get (:after model) field)
                          (encoded! model field (get after field))]]) fields)
              reader-values))))

(defn- evaluate! [formula vars]
  ;; Closed evaluation of the EXACT projected finite-trace step AST. Only
  ;; per-invocation diagnostic evidence; not a proof of the translation.
  (letfn [(ev [x]
            (cond
              (contains? vars x) (get vars x)
              (integer? x) x
              (and (seq? x) (= 'Or (first x)) (= 3 (count x)))
              (or (ev (second x)) (ev (nth x 2)))
              (and (seq? x) (= 'And (first x)) (= 3 (count x)))
              (and (ev (second x)) (ev (nth x 2)))
              (and (seq? x) (#{'= '<= '>=} (first x))
                   (= 'Int (second x)) (= 4 (count x)))
              (let [a (ev (nth x 2)) b (ev (nth x 3))]
                (case (first x) = (= a b) <= (<= a b) >= (>= a b)))
              :else (refuse! :unsupported-projected-step-term {:term x})))]
    (boolean (ev formula))))

(defn- invariant-holds? [invariant v]
  (pilot! read-ns 'source-predicate-holds? invariant v))

(defn- require-state-invariants! [model side state]
  (doseq [[id invariant] (:invariants (:source model))]
    (when-not (invariant-holds? invariant (get state (:field invariant)))
      (refuse! :tracked-invariant-not-established
               {:side side :invariant id :field (:field invariant)}))))

(defn- require-original-source-relation!
  "Independently rederive the FULL guarded replacement relation from the
   original normalized declaration, not the downstream observer's verdict.
   The Ansatz trace projection tracks only selected invariant fields; it does
   not encode the original guard or writes to other fields. Thus a forged or
   stale intermediate gate may never bridge an otherwise-valid trace branch.
   This is a scoped per-observation oracle, not a universal correctness proof."
  [model id before inputs after]
  (let [source (first (filter #(= id (:id %))
                              (get-in model [:source :sources])))
        declaration (:source source)
        semantics (:semantics source)
        state-types (:state-types declaration)
        input-types (:input-types declaration)
        guard (:guard declaration)
        expression (:expression guard)
        [operator lhs rhs] expression
        guard-field (second lhs)
        guard-literal (second rhs)
        assigned (:assignments semantics)
        frames (:frame-fields semantics)
        available (merge before inputs)]
    (when-not (and source
                   (= (:id source) id)
                   (= (:assignments declaration) assigned)
                   (= (:frame-fields semantics)
                      (set/difference (set (keys state-types))
                                      (set (keys assigned))))
                   (vector? expression) (= 3 (count expression))
                   (= := operator)
                   (vector? lhs) (= [:read guard-field] lhs)
                   (vector? rhs) (= [:literal guard-literal] rhs)
                   (= #{guard-field} (:readers guard))
                   (= :enum (get-in state-types [guard-field :type]))
                   (contains? (get-in state-types [guard-field :values])
                              guard-literal))
      (refuse! :unsupported-original-source-relation
               {:operation id :guard guard :writes (keys assigned)}))
    (pilot! read-ns 'projection! state-types :before before)
    (pilot! read-ns 'projection! input-types :inputs inputs)
    (pilot! read-ns 'projection! state-types :after after)
    (let [guard-ok? (= guard-literal (get before guard-field))
          assignments-ok?
          (every? (fn [[target expression]]
                    (let [[assignment-op operand] expression
                          expected (case assignment-op
                                     :literal operand
                                     :read (get available operand)
                                     (refuse! :unsupported-original-assignment
                                              {:target target :expression expression}))]
                      (and (contains? after target)
                           (= expected (get after target)))))
                  assigned)
          frames-ok?
          (every? (fn [field]
                    (and (= (contains? before field) (contains? after field))
                         (= (get before field) (get after field))))
                  frames)]
      (when-not (and guard-ok? assignments-ok? frames-ok?)
        (refuse! :original-source-relation-not-satisfied
                 {:operation id :guard? guard-ok?
                  :assignments? assignments-ok? :frame? frames-ok?}))
      {:guard? guard-ok? :assignments? assignments-ok?
       :frame? frames-ok? :authority :none})))

(defn- audited-boundary! [catalog]
  (let [{:keys [model checked] :as bridge} (check-bridge! catalog)
        read-gate (:observe! (pilot! read-ns 'audited-read-boundary!
                                    (dissoc catalog :initial-condition
                                            :declared-operation-ids)))
        by-id (into {} (map-indexed (fn [i b] [(:operation-id b) i])
                                   (butlast (:branches model))))
        current-ids (set (keys by-id))]
    (when-not (and (= (:catalog-fingerprint model)
                      (get-in bridge [:model :catalog-fingerprint]))
                   (= (set (:operation-ids (:theorem model))) current-ids)
                   (= (inc (count current-ids)) (count checked)))
      (refuse! :bridge-boundary-source-mismatch {:operations current-ids}))
    {:kind :conditional-source-abstract-observation-to-typed-read-trace
     :bridge-count (:bridge-count bridge)
     :catalog-fingerprint (:catalog-fingerprint model)
     :physical-correspondence :not-established
     :step-exhaustiveness :not-proved
     :unproved-read-targets (get-in model [:theorem :unproved-read-targets])
     :open-assumptions outstanding
     :authority :none :admission :not-admitted :certificate :not-issued
     :observe!
     (fn [id before inputs after]
       (let [index (get by-id id)]
         (when-not (some? index)
           (refuse! :unknown-bridged-operation {:operation id}))
         ;; The v1749 gate checks ALL typed fields, guard, original assignment
         ;; and framing, source-value predicates, and TWO abstract observers.
         ;; Its private returned maps are NOT accepted as authority.
         (let [observed (read-gate id before inputs after)]
           (when-not (and (= id (:operation-id observed))
                          (true? (:matches? observed))
                          (= :none (:authority observed)))
             (refuse! :original-observation-not-accepted
                      {:operation id :observation observed})))
         ;; Do not trust even an affirmative v1749 sub-gate as the complete
         ;; source relation. Its output is a copyable diagnostic, not a proof
         ;; handle. Re-evaluate guard, EVERY original write, and the full frame
         ;; before projecting the tracked fields into the checked trace step.
         (require-original-source-relation! model id before inputs after)
         (require-state-invariants! model :before before)
         (require-state-invariants! model :after after)
         (let [variables (variables! model before inputs after)
               branch (nth (:branches model) index)
               declared-step (:projected model)]
           (when-not (and (every? #(evaluate! % variables) (:leaves branch))
                          (evaluate! (:branch branch) variables)
                          (evaluate! declared-step variables))
             (refuse! :observed-values-do-not-refine-checked-step
                      {:operation id :step declared-step}))
           {:operation-id id :branch-index index :matches? true
            :step-holds? true :read-premise-count
            (- (count (:leaves branch)) (count (:fields model)))
            :authority :none :admission :not-admitted :certificate :not-issued})))
     :observe-stutter!
     (fn [before after]
       (let [first-source (first (get-in model [:source :sources]))
             types (get-in first-source [:source :state-types])]
         (pilot! read-ns 'projection! types :before before)
         (pilot! read-ns 'projection! types :after after)
         (when-not (= before after)
           (refuse! :not-an-abstract-stutter {:before before :after after}))
         (require-state-invariants! model :before before)
         (let [variables (variables! model before {} after)
               branch (last (:branches model))]
           (when-not (and (every? #(evaluate! % variables) (:leaves branch))
                          (evaluate! (:branch branch) variables)
                          (evaluate! (:projected model) variables))
             (refuse! :stutter-step-not-covered {:state before}))
           {:stutter? true :step-holds? true :authority :none})))
     :proofs checked}))

(defn- ticket-catalog [] (pilot! induction-ns 'ticket-catalog))
(defn- witness [] (pilot! read-ns 'assign-witness))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest exact-original-input-aware-branches-are-kernel-checked
  (let [{:keys [model checked bridge-count induction-proof]} (check-bridge! (ticket-catalog))]
    (is (= 3 bridge-count))
    (is (= [:ticket/assign :ticket/restore nil]
           (mapv :operation-id (:branches model))))
    (is (= 5 (count (get-in model [:theorem :read-premises]))))
    (is (= 3 (count (get-in model [:reader-symbols]))))
    (is (= #{[:ticket/assign :ticket/owner]}
           (get-in model [:theorem :unproved-read-targets])))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) checked))
    (is (kernel/verifies? (ansatz/env) (:goal induction-proof) (:proof induction-proof)))
    (is (= :none (:authority (check-bridge! (ticket-catalog)))))))

(deftest original-observations-refine-the-exact-declared-step
  (let [gate (audited-boundary! (ticket-catalog))
        [before inputs after] (witness)
        restored (assoc before :ticket/status :claimed)
        restored-after (assoc restored :ticket/count (:ticket/backup restored))]
    (is (:step-holds? ((:observe! gate) :ticket/assign before inputs after)))
    (is (= 3 (:read-premise-count
              ((:observe! gate) :ticket/assign before inputs after))))
    (is (:step-holds? ((:observe! gate) :ticket/restore restored {} restored-after)))
    (is (= 2 (:read-premise-count
              ((:observe! gate) :ticket/restore restored {} restored-after))))
    (is (:step-holds? ((:observe-stutter! gate) before before)))
    (is (= :not-an-abstract-stutter
           (reason #((:observe-stutter! gate) before after))))
    (is (= #{[:ticket/assign :ticket/owner]} (:unproved-read-targets gate)))
    (is (= :not-proved (:step-exhaustiveness gate)))
    (is (= :none (:authority gate)))))

(deftest source-conditions-and-guard-rejection-cannot-turn-into-proof
  (let [gate (audited-boundary! (ticket-catalog))
        [before inputs after] (witness)
        bad-guard (assoc before :ticket/status :claimed)
        bad-input (assoc inputs :input/count 8)
        invalid-type (assoc inputs :input/count 3.5)
        broken-copy (assoc after :ticket/count 4)
        broken-frame (assoc after :ticket/audit :audit/corrupt)]
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/assign bad-guard inputs after))))
    (is (= :source-invariant-precondition-failed
           (reason #((:observe! gate) :ticket/assign before bad-input
                     (assoc after :ticket/count 8)))))
    (is (= :invalid-typed-projection
           (reason #((:observe! gate) :ticket/assign before invalid-type after))))
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/assign before inputs broken-copy))))
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/assign before inputs broken-frame))))
    (is (= :unknown-bridged-operation
           (reason #((:observe! gate) :ticket/unknown before inputs after))))))

(deftest source-state-invariants-and-enum-subsets-cannot-be-ignored
  (let [catalog (ticket-catalog)
        gate (audited-boundary! catalog)
        [before inputs after] (witness)
        bad-before (assoc before :ticket/count -1)
        narrower (-> catalog
                     (assoc-in [:invariants :ticket/channel-domain :expression 2 1]
                               #{:web})
                     (assoc-in [:initial-condition :expression 3 2 1]
                               #{:web}))]
    (is (= :tracked-invariant-not-established
           (reason #((:observe! gate) :ticket/assign bad-before inputs after))))
    (let [narrow-gate (audited-boundary! narrower)]
      (is (= :tracked-invariant-not-established
             (reason #((:observe! narrow-gate) :ticket/assign before inputs after))))
      (is (:step-holds? ((:observe! narrow-gate) :ticket/assign
                         (assoc before :ticket/channel :web) inputs after))))))

(deftest restored-prestate-read-and-untracked-writes-are-not-laundered
  (let [gate (audited-boundary! (ticket-catalog))
        [before inputs after] (witness)
        restored (assoc before :ticket/status :claimed)
        restored-after (assoc restored :ticket/count (:ticket/backup restored))]
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/restore restored {}
                     (assoc restored-after :ticket/count 5)))))
    (is (= :source-invariant-precondition-failed
           (reason #((:observe! gate) :ticket/restore
                     (assoc restored :ticket/backup 9) {}
                     (assoc restored-after :ticket/count 9)))))
    (is (= :original-observation-not-accepted
           (reason #((:observe! gate) :ticket/assign before inputs
                     (assoc after :ticket/owner :user/wrong)))))
    (is (= :invalid-typed-projection
           (reason #((:observe! gate) :ticket/assign before
                     (assoc inputs :input/actor nil) after))))))

(deftest identical-forged-observer-reports-cannot-cross-bridge
  (let [gate (audited-boundary! (ticket-catalog))
        [before inputs after] (witness)
        p abstract/observe-transition!
        q independent/observe-transition!
        forge (fn [original]
                (fn [& args]
                  (assoc (apply original args)
                         :guard-holds? true :assignments-hold? true
                         :frame-holds? true :matches? true)))
        deny (fn [original]
               (fn [& args]
                 (assoc (apply original args)
                        :guard-holds? false :assignments-hold? false
                        :frame-holds? false :matches? false)))]
    (with-redefs [abstract/observe-transition! (forge p)
                  independent/observe-transition! (forge q)]
      (is (= :abstract-observer-disagreement
             (reason #((:observe! gate) :ticket/assign before inputs
                       (assoc after :ticket/count 4))))))
    (with-redefs [abstract/observe-transition! (deny p)
                  independent/observe-transition! (deny q)]
      (is (= :abstract-observer-disagreement
             (reason #((:observe! gate) :ticket/assign before inputs after)))))))

(deftest changed-source-or-dropped-branch-premise-cannot-reuse-proof
  (let [catalog (ticket-catalog)
        source-fn (ns-resolve induction-ns 'trace-obligation!)
        original @source-fn
        changed (assoc-in catalog
                          [:operations 0 :construction :replacements :ticket/count]
                          [:read :input/undefined])]
    (is (thrown? clojure.lang.ExceptionInfo (check-bridge! changed)))
    (with-redefs-fn
      {source-fn (fn [s] (update (original s) :read-premises pop))}
      #(is (= :source-derived-induction-step-not-identical
              (reason (fn [] (bridge-obligations! catalog))))))))

(deftest signed-bounds-and-read-preconditions-retain-exact-step-correspondence
  (let [catalog (-> (ticket-catalog)
                    (assoc-in [:invariants :ticket/count-min :expression]
                              [:>= [:read :ticket/count] [:literal -2]])
                    (assoc-in [:initial-condition :expression 1]
                              [:>= [:read :ticket/count] [:literal -2]])
                    (assoc-in [:invariants :ticket/count-max :expression]
                              [:<= [:read :ticket/count] [:literal 3]])
                    (assoc-in [:initial-condition :expression 2]
                              [:<= [:read :ticket/count] [:literal 3]]))
        gate (audited-boundary! catalog)
        [before inputs after] (witness)
        restored (assoc before :ticket/status :claimed :ticket/backup -1)]
    (is (= 3 (:bridge-count gate)))
    (is (:step-holds? ((:observe! gate) :ticket/assign before inputs after)))
    (is (:step-holds? ((:observe! gate) :ticket/restore restored {}
                        (assoc restored :ticket/count -1))))
    (is (= :source-invariant-precondition-failed
           (reason #((:observe! gate) :ticket/assign before
                     (assoc inputs :input/count 4)
                     (assoc after :ticket/count 4)))))
    (is (= :none (:authority gate)))))

(deftest forged-intermediate-gate-cannot-launder-an-unchecked-original-operation
  (let [catalog (ticket-catalog)
        [before inputs after] (witness)
        lower-var (ns-resolve read-ns 'audited-read-boundary!)
        fake (fn [_]
               {:observe! (fn [id _before _inputs _after]
                            {:operation-id id :matches? true :authority :none})})
        bridge (with-redefs-fn {lower-var fake}
                 #(audited-boundary! catalog))
        observe (:observe! bridge)
        wrong-guard (assoc before :ticket/status :claimed)
        wrong-owner (assoc after :ticket/owner :user/forged)
        wrong-audit (assoc after :ticket/audit :audit/forged)]
    (is (:step-holds? (observe :ticket/assign before inputs after)))
    (doseq [bad [[wrong-guard inputs after]
                 [before inputs wrong-owner]
                 [before inputs wrong-audit]]]
      (is (= :original-source-relation-not-satisfied
             (reason #(apply observe :ticket/assign bad)))))
    (is (= :invalid-typed-projection
           (reason #(observe :ticket/assign before
                             (assoc inputs :input/count 3.5) after))))
    (is (= :tracked-invariant-not-established
           (reason #(observe :ticket/assign before
                             (assoc inputs :input/count 8)
                             (assoc after :ticket/count 8)))))
    (is (= :none (:authority bridge)))))

(deftest forged-intermediate-gate-cannot-launder-prestate-copy-or-enum-subset
  (let [catalog (ticket-catalog)
        [before inputs after] (witness)
        lower-var (ns-resolve read-ns 'audited-read-boundary!)
        fake (fn [_]
               {:observe! (fn [id _before _inputs _after]
                            {:operation-id id :matches? true :authority :none})})
        bridge (with-redefs-fn {lower-var fake}
                 #(audited-boundary! catalog))
        observe (:observe! bridge)
        restored (assoc before :ticket/status :claimed)
        good (assoc restored :ticket/count (:ticket/backup restored))
        unsafe (assoc restored :ticket/backup 9)]
    (is (:step-holds? (observe :ticket/restore restored {} good)))
    (is (= :original-source-relation-not-satisfied
           (reason #(observe :ticket/restore restored {}
                             (assoc good :ticket/count 3)))))
    (is (= :tracked-invariant-not-established
           (reason #(observe :ticket/restore unsafe {}
                             (assoc unsafe :ticket/count 9)))))
    (is (= :none (:authority bridge)))))

(deftest weakened-or-forged-proof-edges-are-rejected
  (let [{:keys [model obligations]} (bridge-obligations! (ticket-catalog))
        checked (check-bridge! (ticket-catalog))
        original (first obligations)
        weakened (update original :parameters
                         #(subvec % 0 (- (count %) 3)))]
    (is (= :none (:authority checked)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (pilot! proof-ns 'check-obligation! weakened)))
    (is (not= (first (:branches model)) (last (:branches model))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-typed-read-transition-bridge-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
