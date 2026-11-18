(ns gesso.assurance.ansatz-rule-semantics-pilot
  "Opt-in v1740 Ansatz pilot: semantics of the ORIGINAL reservation guards.

   Translate only [:read :slot/status], [:literal enum-member], and [:= a b]
   into equality propositions over the exact closed source-derived enum
   encoding. Kernel-check mutual exclusion and COMPLETE coverage of the
   two status guards for arbitrary encoded states satisfying the source
   status-domain predicate. Also exhaustively compare the real Gesso typed
   evaluator against those propositions on ALL allowed enum values.

   This covers the original two-member finite guard domain, NOT arbitrary
   Clojure expressions or a general evaluator-correctness theorem. The enum
   translation and actual JVM execution remain explicit assumptions; nothing
   here verifies XTDB effects, authorizes operations, or admits applications.
   Downstream applications remain ordinary unannotated Clojure."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-transition-correspondence-pilot]
            [gesso.assurance.ansatz-observation-gate-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-type :as rule-type]))

(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private bridge-ns 'gesso.assurance.ansatz-transition-correspondence-pilot)
(def ^:private gate-ns 'gesso.assurance.ansatz-observation-gate-pilot)
(def ^:private status-field :slot/status)

(defn- from-pilot! [ns-name function & args]
  (let [v (ns-resolve ns-name function)]
    (when-not v
      (throw (ex-info "Original Ansatz proof pilot contract unavailable"
                      {:reason :pilot-contract-changed :name function})))
    (apply @v args)))

(defn- reject! [reason details]
  (throw (ex-info "Unsupported original Gesso guard semantics"
                  {:reason reason :details details})))

(defn- strict-guard-expression! [expression encoding]
  ;; A strict structural translator for precisely the expression constructors
  ;; present in the original reservation guards. No host-language eval, no
  ;; unhandled AST nodes, and no invented or implicit enum conversions.
  (letfn [(translate [expr]
            (when-not (vector? expr)
              (reject! :unsupported-guard-expression expr))
            (case (first expr)
              :read (if (= expr [:read status-field])
                      'status
                      (reject! :unsupported-guard-reader expr))
              :literal (let [member (second expr)]
                         (if (and (= 2 (count expr))
                                  (keyword? member)
                                  (contains? encoding member))
                           (get encoding member)
                           (reject! :unsupported-guard-literal expr)))
              := (if (= 3 (count expr))
                   (list '= 'Int (translate (second expr))
                         (translate (nth expr 2)))
                   (reject! :unsupported-guard-arity expr))
              (reject! :unsupported-guard-operator expr)))]
    (let [formula (translate expression)]
      (when-not (and (seq? formula)
                     (= '= (first formula))
                     (= 'Int (second formula))
                     (= 4 (count formula))
                     (#{'status} (nth formula 2))
                     (integer? (nth formula 3)))
        (reject! :unsupported-original-guard-shape expression))
      formula)))

(defn- guard-partition-source! [ordered parsed encoding]
  (let [targets (mapv (comp #(nth % 3) :formula) parsed)]
    (when-not (and (= [:slot/reserve :slot/release] ordered)
                   (= ordered (mapv :operation-id parsed))
                   (= #{:available :reserved} (set (keys encoding)))
                   (= #{0 1} (set (vals encoding)))
                   (= 2 (count parsed))
                   (= (set (vals encoding)) (set targets))
                   (= (count targets) (count (set targets))))
      (reject! :incomplete-or-aliased-guard-domain
               {:targets targets :encoding encoding :operation-ids ordered}))
    parsed))

(defn- checked-guard-model! [catalog]
  ;; Source-closure and all earlier proofs are checked before deriving ANY
  ;; proposition. This is still an opt-in test, not an admission API.
  (let [_ (from-pilot! gate-ns 'current-rule-source-closure!)
        _ (from-pilot! gate-ns 'current-interpreter-source!)
        {:keys [model sources]} (from-pilot! bridge-ns
                                                'source-bridge-obligations! catalog)
        encoding (:status-encoding model)
        inv (get-in catalog [:invariants :slot/status-domain])
        ordered (mapv :id (:operations catalog))
        parsed (mapv (fn [{:keys [operation-id transition before-code]}]
                       (let [guard (:guard transition)
                             type-map (select-keys (:state-types transition)
                                                   #{status-field})
                             checked (rule-type/check-rule-type! guard type-map)
                             formula (strict-guard-expression! (:expression checked)
                                                               encoding)]
                         (when-not (and (= #{status-field} (:readers checked))
                                        (= before-code (nth formula 3)))
                           (reject! :guard-formula-bridge-mismatch
                                    {:operation-id operation-id :formula formula}))
                         {:operation-id operation-id
                          :guard guard :reader-types type-map
                          :formula formula :source-transition transition}))
                     sources)
        _ (guard-partition-source! ordered parsed encoding)]
    (when-not (= [:in [:read status-field]
                  [:literal (set (keys encoding))]] (:expression inv))
      (reject! :unsupported-status-domain inv))
    {:catalog-fingerprint (:catalog-fingerprint model)
     :status-encoding encoding
     :operations parsed
     :closed-domain (list 'Or (list '= 'Int 'status (first (sort (vals encoding))))
                          (list '= 'Int 'status (second (sort (vals encoding)))))}))

(defn- proof-obligations! [{:keys [catalog-fingerprint operations closed-domain]}]
  (let [[a b] (map :formula operations)
        params ['status ':- 'Int 'hin ':- closed-domain]
        pair-goal (list 'Or (list 'And a (list 'Not b))
                        (list 'And b (list 'Not a)))]
    [{:obligation-kind :original-reservation-guard-partition
      :catalog-fingerprint catalog-fingerprint
      :operation-ids (mapv :operation-id operations)
      :parameters params
      :goal-form pair-goal
      :proof-script '[(rcases hin (or h0 h1))
                      (left) (constructor) (exact h0) (omega)
                      (right) (constructor) (exact h1) (omega)]}
     {:obligation-kind :original-reservation-reserve-guard-complement
      :catalog-fingerprint catalog-fingerprint
      :operation-ids (mapv :operation-id operations)
      :parameters params
      :goal-form (list 'Iff a (list 'Not b))
      :proof-script '[(omega)]}
     {:obligation-kind :original-reservation-release-guard-complement
      :catalog-fingerprint catalog-fingerprint
      :operation-ids (mapv :operation-id operations)
      :parameters params
      :goal-form (list 'Iff b (list 'Not a))
      :proof-script '[(omega)]}]))

(defn- check-guard-proofs! [catalog]
  (let [model (checked-guard-model! catalog)
        checked (mapv #(from-pilot! proof-ns 'check-obligation! %)
                      (proof-obligations! model))]
    (when-not (and (= 3 (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (= (:catalog-fingerprint model)
                                     (:catalog-fingerprint description))
                                  (kernel/verifies? (ansatz/env) goal proof)
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) checked))
      (reject! :unqualified-guard-proof checked))
    {:model model
     :checked checked
     :proof-count (count checked)
     :scope :closed-enum-guard-domain
     :evaluator-correspondence :exhaustive-domain-observations-only
     :general-expression-semantics :not-proved
     :physical-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- evaluate-original-guards! [model]
  ;; Since the entire typed input footprint is one REQUIRED member of a
  ;; two-member enum, these calls enumerate all valid rule-eval binding maps
  ;; for the checked source guards. This is exhaustive for this closed
  ;; projection, but not a universal proof of the JVM evaluator algorithm.
  (let [encoding (:status-encoding model)
        reports (vec
                 (for [member (sort-by str (keys encoding))
                       {:keys [operation-id guard reader-types formula]}
                       (:operations model)]
                   (let [result (rule-eval/evaluate-rule! guard reader-types
                                                          {status-field member})
                         symbolic (= (get encoding member) (nth formula 3))]
                     (when-not (and (= :syntax-and-types (:checking result))
                                    (= :pure-interpreted (:evaluation result))
                                    (= :none (:authority result))
                                    (= (:expression guard) (:expression result))
                                    (instance? Boolean (:value result))
                                    (= symbolic (:value result)))
                       (reject! :original-evaluator-formula-mismatch
                                {:member member :operation-id operation-id
                                 :observed result :symbolic symbolic}))
                     {:operation-id operation-id
                      :status member :value (:value result)})))]
    (when-not (and (= 4 (count reports))
                   (every? #(= 1 (count (filter :value (filter
                                                        (fn [r] (= (:status r) %))
                                                        reports))))
                           (keys encoding)))
      (reject! :guard-evaluator-not-a-partition reports))
    reports))

(deftest original-guards-form-kernel-checked-exhaustive-exclusive-partition
  (let [{:keys [model checked] :as result}
        (check-guard-proofs! (example/reservation-catalog))
        reports (evaluate-original-guards! model)]
    (is (= 3 (:proof-count result)))
    (is (= #{:original-reservation-guard-partition
             :original-reservation-reserve-guard-complement
             :original-reservation-release-guard-complement}
           (set (map (comp :obligation-kind :description) checked))))
    (is (= #{[:slot/reserve :available true]
             [:slot/reserve :reserved false]
             [:slot/release :available false]
             [:slot/release :reserved true]}
           (set (map (juxt :operation-id :status :value) reports))))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) checked))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest unsupported-guards-and-aliased-translations-fail-closed
  (let [catalog (example/reservation-catalog)
        model (checked-guard-model! catalog)
        enc (:status-encoding model)
        denial (fn [f]
                 (try (f) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (is (= :unsupported-guard-operator
           (denial #(strict-guard-expression!
                     [:not= [:read status-field] [:literal :available]] enc))))
    (is (= :unsupported-guard-reader
           (denial #(strict-guard-expression!
                     [:= [:read :slot/audit-tag] [:literal :available]] enc))))
    (is (= :unsupported-guard-literal
           (denial #(strict-guard-expression!
                     [:= [:read status-field] [:literal :unknown]] enc))))
    (testing "A duplicated encoded guard target loses exclusivity"
      (is (= :incomplete-or-aliased-guard-domain
             (denial #(guard-partition-source!
                        (mapv :operation-id (:operations model))
                        (assoc-in (:operations model) [1 :formula]
                                  (:formula (first (:operations model))))
                        enc)))))
    (testing "Mutating the actual original guard cannot inherit old proofs"
      (is (= :not-original-reservation-catalog
             (denial #(checked-guard-model!
                        (assoc-in catalog
                                  [:operations 1 :construction :guard :expression]
                                  [:= [:read status-field]
                                   [:literal :available]]))))))
    (is (= :not-original-reservation-catalog
           (denial #(checked-guard-model!
                     (update catalog :operations pop)))))
    (testing "Outside-domain values make exclusivity insufficient for coverage"
      (let [partition (first (proof-obligations! model))
            without-domain (-> partition
                               (assoc :parameters ['status ':- 'Int])
                               (assoc :proof-script '[(omega)]))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (from-pilot! proof-ns 'check-obligation! without-domain)))))))

(deftest evaluator-disagreement-cannot-inherit-the-partition-theorem
  (let [model (checked-guard-model! (example/reservation-catalog))
        original rule-eval/evaluate-rule!
        denial (fn [evaluator]
                 (with-redefs [rule-eval/evaluate-rule! evaluator]
                   (try (evaluate-original-guards! model) nil
                        (catch clojure.lang.ExceptionInfo ex
                          (:reason (ex-data ex))))))]
    (is (= :original-evaluator-formula-mismatch
           (denial (fn [& args]
                     (assoc (apply original args) :value true)))))
    (is (= :original-evaluator-formula-mismatch
           (denial (fn [& args]
                     (assoc (apply original args) :checking :unchecked)))))
    (testing "Outside-domain status is rejected by the ORIGINAL typed runtime"
      (is (thrown? clojure.lang.ExceptionInfo
                   (rule-eval/evaluate-rule!
                    (:guard (first (:operations model)))
                    (:reader-types (first (:operations model)))
                    {status-field :outside-domain}))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-rule-semantics-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))
