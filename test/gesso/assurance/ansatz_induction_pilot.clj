(ns gesso.assurance.ansatz-induction-pilot
  "Opt-in Ansatz 0.2.617 finite-trace induction for original Gesso reservation.

   From the same unannotated Gesso catalog as ansatz-reservation-pilot,
   construct a SINGLE quantified safety theorem for any Nat-indexed sequence
   whose first state satisfies the declared initial predicate and whose every
   step is exactly a declared write or a stutter. The theorem is kernel checked
   and dependency audited; the ten source-derived component proofs are checked
   again and their complete coverage required before the composition is
   described. The step-exhaustiveness premise is ASSUMED, not certified.

   This is a pilot, not a new downstream authoring API or an installed safety
   certificate. It does not prove correspondence of Gesso's source translator,
   abstract interpreter, XTDB execution, writer closure, or database contents.

   Invoke with the OPTIONAL :ansatz:typed:test dependencies, e.g.
     java -cp <resolved classpath> clojure.main -m gesso.assurance.ansatz-induction-pilot

   We use the sibling pilot's private functions only in this test-only
   experiment. Refactor into a supported assurance boundary before production."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.ansatz-reservation-pilot]))

(def ^:private pilot-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private obligations #{:slot/nonnegative-claims
                             :slot/at-most-one-claim
                             :slot/status-domain})
(def ^:private operations #{:slot/reserve :slot/release})

(def ^:private open-assumptions
  #{:actual-initial-state-satisfies-declared-predicate
    :every-real-commit-refines-a-declared-transition-or-stutters
    :physical-writer-catalog-is-complete
    :atomic-transaction-and-guard-correspondence
    :enum-encoding-preserves-original-meaning
    :original-clojure-to-ansatz-translation-correspondence
    :source-installation-and-deployment-currentness
    :ansatz-kernel-and-explicitly-permitted-axioms})

(defn- from-pilot! [name & args]
  (let [v (ns-resolve pilot-ns name)]
    (when-not v
      (throw (ex-info "Missing pilot proof checking function"
                      {:reason :pilot-contract-changed :name name})))
    (apply @v args)))

(defn- ensure! [pred reason detail]
  (when-not pred
    (throw (ex-info "Incomplete conditional inductive safety evidence"
                    {:reason reason :details detail}))))

(defn- proof-coverage! [catalog preservation initial]
  (let [wanted-pairs (set (for [o (:declared-operation-ids catalog)
                                i (keys (:invariants catalog))] [o i]))
        seen-pairs (mapv (fn [r] (let [d (:description r)]
                                   [(:operation-id d) (:invariant-id d)])) preservation)
        seen-init (mapv (comp :obligation-kind :description) initial)
        initial-invariants (set (map (comp :invariant-id :description)
                                     (filter #(= :initial-predicate-entails-invariant
                                                 (get-in % [:description :obligation-kind]))
                                             initial)))
        fingerprints (set (map #(get-in % [:description :catalog-fingerprint])
                               (concat preservation initial)))]
    (ensure! (and (= operations (:declared-operation-ids catalog))
                  (= obligations (set (keys (:invariants catalog))))
                  (= wanted-pairs (set seen-pairs))
                  (= (count wanted-pairs) (count seen-pairs)))
             :incomplete-preservation-coverage seen-pairs)
    (ensure! (and (= 4 (count seen-init))
                  (= 3 (count (filter #{:initial-predicate-entails-invariant} seen-init)))
                  (= 1 (count (filter #{:initial-predicate-nonvacuity-witness} seen-init)))
                  (= obligations initial-invariants))
             :incomplete-initialization-coverage seen-init)
    (ensure! (= 1 (count fingerprints)) :catalog-identity-disagreement fingerprints)
    (ensure! (every? (fn [{:keys [goal proof dependencies]}]
                      (and (kernel/verifies? (ansatz/env) goal proof)
                           (= :ansatz-reservation-pilot/lean-core-axioms-v1
                              (:policy dependencies))
                           (empty? (:non-safe dependencies))
                           (empty? (:unresolved dependencies))))
                    (concat preservation initial))
             :unqualified-component-proof :proof-verification)
    (first fingerprints)))

(defn- field-at [field n] (list field n))
(defn- equality [a b] (list '= 'Int a b))
(defn- conjunction [forms] (from-pilot! 'three-conjunction forms))

(defn- invariant-at [catalog order status claim]
  (conjunction (mapv #(from-pilot! 'initial-invariant-proposition!
                                  catalog % status claim) order)))

(defn- source-moves! [catalog preservation]
  (let [by-operation (group-by #(get-in % [:description :operation-id]) preservation)]
    (mapv (fn [id]
            (let [proofs (get by-operation id)
                  by-invariant (into {} (map (fn [r] [(get-in r [:description :invariant-id])
                                                  (:description r)]) proofs))
                  status (:slot/status-domain by-invariant)
                  counts (:slot/nonnegative-claims by-invariant)
                  upper (:slot/at-most-one-claim by-invariant)
                  encoding (:status-encoding status)
                  status-code (get encoding (:replacement status))]
              (ensure! (and (= 3 (count proofs))
                            (= obligations (set (keys by-invariant)))
                            (= (:replacement counts) (:replacement upper))
                            (= #{:available :reserved} (set (keys encoding)))
                            (= #{0 1} (set (vals encoding)))
                            (integer? status-code))
                       :inconsistent-source-write-proofs {:operation id})
              {:operation-id id :status-code status-code
               :count-value (:replacement counts)}))
          (mapv :id (:operations catalog)))))

(defn- step-premise [moves k]
  (let [future (list 'Nat.succ k)
        branch (fn [{:keys [status-code count-value]}]
                 (list 'And (equality (field-at 'statuses future) status-code)
                       (equality (field-at 'claims future) count-value)))
        stutter (list 'And (equality (field-at 'statuses future)
                                    (field-at 'statuses k))
                      (equality (field-at 'claims future)
                                (field-at 'claims k)))]
    (list 'Or (branch (first moves))
          (list 'Or (branch (second moves)) stutter))))

(defn- step-tactics [order code]
  (vec (concat '[(constructor)]
               (if (= :slot/status-domain (first order))
                 [(if (= code 0) '(left) '(right)) '(exact hs)]
                 '[(omega)])
               '[(constructor)]
               (mapcat (fn [id]
                         (if (= :slot/status-domain id)
                           [(if (= code 0) '(left) '(right)) '(exact hs)]
                           '[(omega)]))
                       (rest order)))))

(defn- stutter-tactics [order]
  (vec (concat '[(rcases ih [hi0 [hi1 hi2]]) (constructor)]
               (if (= :slot/status-domain (first order))
                 '[(rw [hs]) (exact hi0)]
                 '[(omega)])
               '[(constructor)]
               (mapcat (fn [[i id]]
                         (if (= id :slot/status-domain)
                           [(list 'rw '[hs]) (list 'exact (nth '[hi0 hi1 hi2] i))]
                           '[(omega)]))
                       (map-indexed (fn [i id] [(inc i) id]) (rest order))))))

(defn- trace-obligation! [catalog preservation]
  (let [{:keys [order catalog-fingerprint]}
        (from-pilot! 'initial-predicate-source! catalog)
        moves (source-moves! catalog preservation)
        params ['statuses ':- '(=> Nat Int)
                'claims ':- '(=> Nat Int)
                'n ':- 'Nat
                'hinit ':- (invariant-at catalog order
                                        (field-at 'statuses 0) (field-at 'claims 0))
                'hsteps ':- (list 'forall ['k ':- 'Nat] (step-premise moves 'k))]
        script (vec (concat '[(induction n) (exact hinit)
                              (rename_i k ih) (specialize hsteps k)
                              (rcases hsteps (or [hs hc] (or [hs hc] [hs hc])))]
                            (step-tactics order (:status-code (first moves)))
                            (step-tactics order (:status-code (second moves)))
                            (stutter-tactics order)))]
    {:obligation-kind :conditional-finite-trace-induction
     :catalog-fingerprint catalog-fingerprint
     :operation-ids (mapv :operation-id moves)
     :initial-condition :declared-source-predicate
     :step-relation :each-declared-literal-write-or-stutter
     :step-exhaustiveness :assumed-not-proved
     :parameters params
     :goal-form (invariant-at catalog order
                              (field-at 'statuses 'n) (field-at 'claims 'n))
     :proof-script script}))

(defn- compose-original-reservation! [catalog]
  (from-pilot! 'original-catalog! catalog)
  (let [preservation (from-pilot! 'check-catalog! catalog)
        initialization (from-pilot! 'check-initialization! catalog)
        identity (proof-coverage! catalog preservation initialization)
        obligation (trace-obligation! catalog preservation)
        traced (from-pilot! 'check-obligation! obligation)]
    (ensure! (= identity (get-in traced [:description :catalog-fingerprint]))
             :trace-source-identity-mismatch identity)
    {:assurance :conditional-abstract-inductive-safety-pilot
     :catalog-fingerprint identity
     :preservation-count (count preservation)
     :initialization-count (count initialization)
     :trace-proof {:goal (:goal traced) :proof (:proof traced)
                   :dependencies (:dependencies traced)}
     :unproved-assumptions open-assumptions
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(deftest original-catalog-gets-quantified-induction-with-explicit-open-assumptions
  (let [source (example/reservation-catalog)
        result (compose-original-reservation! source)]
    (is (= :conditional-abstract-inductive-safety-pilot (:assurance result)))
    (is (= 6 (:preservation-count result)))
    (is (= 4 (:initialization-count result)))
    (is (= open-assumptions (:unproved-assumptions result)))
    (is (= :not-issued (:certificate result)))
    (is (= :not-admitted (:admission result)))
    (is (= :none (:authority result)))
    (is (kernel/verifies? (ansatz/env)
                          (get-in result [:trace-proof :goal])
                          (get-in result [:trace-proof :proof])))
    (is (= :ansatz-reservation-pilot/lean-core-axioms-v1
           (get-in result [:trace-proof :dependencies :policy])))))

(deftest incomplete-or-unrelated-component-proofs-are-rejected
  (let [source (example/reservation-catalog)
        preservation (from-pilot! 'check-catalog! source)
        initialization (from-pilot! 'check-initialization! source)
        reason (fn [ps init]
                 (try (proof-coverage! source ps init) nil
                      (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))]
    (is (= :incomplete-preservation-coverage
           (reason (pop preservation) initialization)))
    (is (= :incomplete-initialization-coverage
           (reason preservation (pop initialization))))
    (is (= :catalog-identity-disagreement
           (reason (assoc-in preservation [0 :description :catalog-fingerprint]
                             "wrong-source") initialization)))
    (is (= :unqualified-component-proof
           (reason (assoc-in preservation [0 :proof] nil) initialization)))))

(deftest finite-trace-safety-does-not-follow-without-the-step-premise
  (let [catalog (example/reservation-catalog)
        preservation (from-pilot! 'check-catalog! catalog)
        complete (trace-obligation! catalog preservation)
        ;; The binder telescope has three entries per binder: name, :-, type.
        ;; Delete exactly the universal step-exhaustiveness premise.
        without-step (update complete :parameters #(subvec % 0 (- (count %) 3)))]
    (is (= 'hsteps (nth (:parameters complete) (- (count (:parameters complete)) 3))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (from-pilot! 'check-obligation! without-step)))))

(deftest changed-sources-must-not-inherit-original-induction-result
  (let [source (example/reservation-catalog)
        reject-reason (fn [bad]
                        (try (compose-original-reservation! bad) nil
                             (catch clojure.lang.ExceptionInfo ex
                               (:reason (ex-data ex)))))]
    (is (= :not-original-reservation-catalog
           (reject-reason (assoc-in source [:operations 0 :construction
                                           :replacements :slot/active-claims]
                                    [:literal 2]))))
    (is (= :not-original-reservation-catalog
           (reject-reason (update source :operations pop))))
    (is (= :not-original-reservation-catalog
           (reject-reason (assoc-in source [:initial-condition :expression 0]
                                    :or))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-induction-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
