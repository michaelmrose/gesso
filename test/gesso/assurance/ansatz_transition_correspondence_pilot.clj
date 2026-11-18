(ns gesso.assurance.ansatz-transition-correspondence-pilot
  "Opt-in, SOURCE-DERIVED Ansatz 0.2.617 abstract-step bridge (v1735).

   This test builds a quantified theorem, for each ORIGINAL guarded Gesso
   reservation transition, that its normalized guard and two literal writes
   imply the EXACT weaker step proposition assumed by the v1734 induction
   theorem. A projection-stutter bridge is also checked. All three theorems
   are kernel verified and dependency audited using the original v1733 pilot.

   A separate finite-domain witness test compares BOTH Gesso abstract
   observers and checks that every MATCHING observed transition satisfies
   the projected step relation. This is NOT a universal proof that either
   interpreter implements its source semantics: the premise of the bridge
   is an explicit, symbolic abstraction of the original declaration.

   The weaker trace relation does not include the source guard or audit-tag
   framing, and deliberately admits additional transitions. This is sound
   only in the one-way direction for the two tracked safety fields. It DOES
   NOT establish physical XTDB correspondence, actual writer closure,
   installed-source currentness, or application admission. No downstream
   application needs to use Typed Clojure or Ansatz directly."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [clojure.walk :as walk]
            [gesso.assurance.ansatz-induction-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract]))

(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private induction-ns 'gesso.assurance.ansatz-induction-pilot)
(def ^:private status-field :slot/status)
(def ^:private claims-field :slot/active-claims)
(def ^:private audit-field :slot/audit-tag)

(defn- private-call! [n name & args]
  (let [v (ns-resolve n name)]
    (when-not v
      (throw (ex-info "Missing original proof pilot function"
                      {:reason :pilot-contract-changed :ns n :name name})))
    (apply @v args)))

(defn- reject! [reason detail]
  (throw (ex-info "Unsupported Ansatz abstract-step correspondence"
                  {:reason reason :details detail})))

(defn- eq-int [a b] (list '= 'Int a b))

(defn- original-source! [catalog]
  (private-call! proof-ns 'original-catalog! catalog)
  (let [preservation (private-call! proof-ns 'check-catalog! catalog)
        initialization (private-call! proof-ns 'check-initialization! catalog)
        identity (private-call! induction-ns 'proof-coverage!
                                catalog preservation initialization)
        trace (private-call! induction-ns 'trace-obligation! catalog preservation)
        original-step (-> trace :parameters last last)
        replacements {'(statuses (Nat.succ k)) 'post-status
                      '(claims (Nat.succ k)) 'post-claims
                      '(statuses k) 'pre-status
                      '(claims k) 'pre-claims}
        projected (walk/postwalk-replace replacements original-step)
        moves (private-call! induction-ns 'source-moves! catalog preservation)
        status (private-call! proof-ns 'status-invariant! catalog)]
    (when-not (and (= identity (:catalog-fingerprint trace))
                   (= #{:slot/reserve :slot/release} (set (map :operation-id moves)))
                   (= 2 (count moves))
                   (= 'forall (first (last (:parameters trace))))
                   (= '[k :- Nat] (second (last (:parameters trace))))
                   (not= original-step projected)
                   (not-any? #{'statuses 'claims 'Nat.succ 'k}
                             (tree-seq coll? seq projected)))
      (reject! :step-projection-does-not-match-original-trace
               {:trace (:obligation-kind trace) :projected projected}))
    {:catalog-fingerprint identity :step-form projected
     :preservation preservation :moves moves :status-encoding (:encoding status)}))

(defn- source-operation! [catalog model operation]
  (let [id (:id operation)
        {:keys [construction]} operation
        {:keys [transition transition-fingerprint status-replacement replacement]}
        (private-call! proof-ns 'source-transition! operation)
        guard (:guard construction)
        expr (:expression guard)
        pre-status (get-in expr [2 1])
        encoding (:status-encoding model)
        before-code (get encoding pre-status)
        after-code (get encoding status-replacement)
        known (first (filter #(= id (:operation-id %)) (:moves model)))]
    (when-not (and (= #{:readers :expression} (set (keys guard)))
                   (= #{status-field} (:readers guard))
                   (= [:= [:read status-field] [:literal pre-status]] expr)
                   (some? before-code)
                   (some? after-code)
                   (= #{audit-field} (:frame construction))
                   (= #{status-field claims-field} (:writes construction))
                   (= #{status-field claims-field} (set (keys (:assignments transition))))
                   (= [:literal status-replacement]
                      (get-in transition [:assignments status-field]))
                   (= [:literal replacement]
                      (get-in transition [:assignments claims-field]))
                   (= after-code (:status-code known))
                   (= replacement (:count-value known)))
      (reject! :source-transition-not-covered-by-induction-step
               {:operation-id id :guard guard :move known}))
    {:operation-id id
     :catalog-fingerprint (:catalog-fingerprint model)
     :transition-fingerprint transition-fingerprint
     :before-code before-code
     :after-code after-code
     :claim-value replacement
     :transition transition}))

(defn- operation-obligation! [model source index]
  {:obligation-kind :original-abstract-guard-and-writes-imply-trace-step
   :operation-id (:operation-id source)
   :catalog-fingerprint (:catalog-fingerprint source)
   :transition-fingerprint (:transition-fingerprint source)
   :premises :original-guard-and-two-normalized-literal-writes
   :conclusion :exact-v1734-trace-step-projection
   :parameters ['pre-status ':- 'Int
                'pre-claims ':- 'Int
                'post-status ':- 'Int
                'post-claims ':- 'Int
                'hguard ':- (eq-int 'pre-status (:before-code source))
                'hstatus ':- (eq-int 'post-status (:after-code source))
                'hclaims ':- (eq-int 'post-claims (:claim-value source))]
   :goal-form (:step-form model)
   :proof-script (vec (concat (if (zero? index) '[(left)] '[(right) (left)])
                              '[(constructor) (exact hstatus) (exact hclaims)]))})

(defn- stutter-obligation! [model]
  {:obligation-kind :projected-stutter-implies-trace-step
   :catalog-fingerprint (:catalog-fingerprint model)
   :premises :projected-status-and-claims-unchanged
   :conclusion :exact-v1734-trace-step-projection
   :parameters ['pre-status ':- 'Int
                'pre-claims ':- 'Int
                'post-status ':- 'Int
                'post-claims ':- 'Int
                'hstatus ':- (eq-int 'post-status 'pre-status)
                'hclaims ':- (eq-int 'post-claims 'pre-claims)]
   :goal-form (:step-form model)
   :proof-script '[(right) (right) (constructor)
                   (exact hstatus) (exact hclaims)]})

(defn- source-bridge-obligations! [catalog]
  (let [model (original-source! catalog)
        operations (:operations catalog)
        sources (mapv (partial source-operation! catalog model) operations)]
    (when-not (= (mapv :operation-id sources)
                 (mapv :operation-id (:moves model)))
      (reject! :incomplete-declared-move-coverage
               {:operations (mapv :operation-id sources)}))
    {:model model
     :sources sources
     :obligations (conj (mapv (fn [index src]
                                 (operation-obligation! model src index))
                               (range) sources)
                         (stutter-obligation! model))}))

(defn- check-original-bridge! [catalog]
  (let [{:keys [model sources obligations]} (source-bridge-obligations! catalog)
        checked (mapv #(private-call! proof-ns 'check-obligation! %) obligations)]
    (when-not (and (= 3 (count checked))
                   (every? #(= (:catalog-fingerprint model)
                               (get-in % [:description :catalog-fingerprint])) checked)
                   (every? #(and (kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                                 (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                    (get-in % [:dependencies :policy]))
                                 (empty? (get-in % [:dependencies :non-safe]))
                                 (empty? (get-in % [:dependencies :unresolved]))) checked))
      (reject! :bridge-proof-not-qualified {:checked (count checked)}))
    {:catalog-fingerprint (:catalog-fingerprint model)
     :source-transition-count (count sources)
     :kernel-checked-bridge-count (count checked)
     :checked checked
     :assurance :conditional-abstract-step-bridge
     :correspondence :symbolic-source-write-to-trace-step
     :observer-correspondence :bounded-tests-only
     :physical-correspondence :not-established
     :writer-closure :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- evaluate-step! [form variables]
  ;; Strict tiny interpreter of the EXACT generated formula for bounded
  ;; comparison with the two Gesso abstract observers. This is TEST support,
  ;; NOT the Ansatz kernel and NOT another general Gesso interpreter.
  (cond
    (#{'post-status 'post-claims 'pre-status 'pre-claims} form)
    (get variables form)

    (and (integer? form) (<= 0 form 1)) form

    (and (seq? form) (= 3 (count form)) (= 'Or (first form)))
    (or (evaluate-step! (second form) variables)
        (evaluate-step! (nth form 2) variables))

    (and (seq? form) (= 3 (count form)) (= 'And (first form)))
    (and (evaluate-step! (second form) variables)
         (evaluate-step! (nth form 2) variables))

    (and (seq? form) (= 4 (count form)) (= '= (first form))
         (= 'Int (second form)))
    (= (evaluate-step! (nth form 2) variables)
       (evaluate-step! (nth form 3) variables))

    :else (reject! :unsupported-step-expression form)))

(defn- step-holds? [model before after]
  (let [encoding (:status-encoding model)]
    (boolean
     (evaluate-step! (:step-form model)
                     {'pre-status (get encoding (status-field before))
                      'post-status (get encoding (status-field after))
                      'pre-claims (claims-field before)
                      'post-claims (claims-field after)}))))

(deftest original-abstract-transitions-have-kernel-checked-bridge-proofs
  (let [catalog (example/reservation-catalog)
        result (check-original-bridge! catalog)]
    (is (= :conditional-abstract-step-bridge (:assurance result)))
    (is (= 2 (:source-transition-count result)))
    (is (= 3 (:kernel-checked-bridge-count result)))
    (is (= #{:slot/reserve :slot/release}
           (set (keep #(get-in % [:description :operation-id]) (:checked result)))))
    (is (= :not-established (:physical-correspondence result)))
    (is (= :not-established (:writer-closure result)))
    (is (= :not-issued (:certificate result)))
    (is (= :not-admitted (:admission result)))
    (is (= :none (:authority result)))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                (:checked result)))))

(deftest bounded-original-observers-agree-on-every-matching-projected-step
  (let [catalog (example/reservation-catalog)
        {:keys [model sources]} (source-bridge-obligations! catalog)
        states (vec (for [status [:available :reserved]
                          claims [-1 0 1 2]
                          audit [nil :audit/tag]]
                      {status-field status claims-field claims audit-field audit}))
        match-count (atom 0)]
    (doseq [{:keys [transition operation-id]} sources
            before states
            after states]
      (let [original (abstract/observe-transition! transition before {} after)
            independent (independent/observe-transition! transition before {} after)
            match? (:matches? original)]
        (is (= match? (:matches? independent))
            (str operation-id " observers disagree " [before after]))
        (when match?
          (swap! match-count inc)
          (is (:guard-holds? original))
          (is (:assignments-hold? original))
          (is (:frame-holds? original))
          (is (step-holds? model before after)
              (str operation-id " accepted transition not represented: " [before after])))))
    ;; This exercises enabled transitions, not just empty/vacuous relations.
    (is (= 16 @match-count))
    (testing "The abstract trace step is strictly weaker than a guarded operation"
      (let [before {status-field :reserved claims-field 0 audit-field nil}
            after {status-field :reserved claims-field 1 audit-field nil}
            reserve (first sources)]
        (is (step-holds? model before after))
        (is (false? (:matches? (abstract/observe-transition!
                                (:transition reserve) before {} after))))))))

(deftest bridge-rejects-source-mutations-and-an-unrelated-theorem
  (let [catalog (example/reservation-catalog)
        failure (fn [source]
                  (try (source-bridge-obligations! source) nil
                       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))]
    (is (= :not-original-reservation-catalog
           (failure (assoc-in catalog [:operations 0 :construction :guard :expression]
                              [:= [:read status-field] [:literal :reserved]]))))
    (is (= :not-original-reservation-catalog
           (failure (assoc-in catalog [:operations 0 :construction
                                       :replacements claims-field] [:literal 2]))))
    (is (= :not-original-reservation-catalog
           (failure (update catalog :operations pop))))
    (let [obligation (-> (source-bridge-obligations! catalog) :obligations first)
          wrong (assoc obligation :goal-form (eq-int 'post-claims 999))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (private-call! proof-ns 'check-obligation! wrong))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests
                              'gesso.assurance.ansatz-transition-correspondence-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
