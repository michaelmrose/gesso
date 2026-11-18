(ns gesso.assurance.ansatz-rule-semantics-pilot
  "Opt-in v1742: ORIGINAL reservation guards through the SHARED enum compiler.

   The v1741 generic finite-enum compiler now owns equality-guard translation,
   enum encoding, source bijection, pairwise exclusion, and exhaustive Gesso
   evaluator observations. This ORIGINAL reservation pilot binds the shared
   results to the checked guarded transitions and retains its earlier,
   stronger complement and exclusive-or propositions without a second AST
   translator or evaluator loop.

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
            [gesso.assurance.ansatz-finite-enum-partition-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private bridge-ns 'gesso.assurance.ansatz-transition-correspondence-pilot)
(def ^:private gate-ns 'gesso.assurance.ansatz-observation-gate-pilot)
(def ^:private enum-ns 'gesso.assurance.ansatz-finite-enum-partition-pilot)
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

(defn- source-guard-rules [catalog]
  ;; Adapt the ORIGINAL unannotated Gesso operation declarations to the
  ;; shared v1741 compiler's source-rule interface. This is a projection of
  ;; existing rules, NOT a second handwritten guard specification.
  (mapv (fn [{:keys [id construction]}]
          (let [guard (:guard construction)]
            {:id id
             :rule guard
             :reader-types (select-keys (:state-types construction)
                                        (:readers guard))}))
        (:operations catalog)))

(defn- checked-guard-model! [catalog]
  ;; The bridge independently checks the ORIGINAL operations/transition
  ;; construction. The shared compiler owns all guard AST translation and
  ;; finite-domain coverage. Reject a mismatch between these two sources.
  (let [_ (from-pilot! gate-ns 'current-interpreter-source!)
        {:keys [model sources]}
        (from-pilot! bridge-ns 'source-bridge-obligations! catalog)
        source-rules (source-guard-rules catalog)
        shared (from-pilot! enum-ns 'derive-portfolio! source-rules)
        encoding (:status-encoding model)
        inv (get-in catalog [:invariants :slot/status-domain])
        parsed (mapv (fn [{:keys [operation-id transition before-code]}
                          {:keys [id source code formula]}]
                       (let [guard (:guard transition)
                             types (select-keys (:state-types transition)
                                                #{status-field})]
                         (when-not (and (= operation-id id)
                                        (= guard (:rule source))
                                        (= types (:reader-types source))
                                        (= code before-code)
                                        (= formula (list '= 'Int 'status before-code)))
                           (reject! :guard-formula-bridge-mismatch
                                    {:operation-id operation-id
                                     :shared-guard id :formula formula
                                     :source-code before-code}))
                         {:operation-id operation-id
                          :guard guard :reader-types types
                          :formula formula :source-transition transition}))
                     sources (:guards shared))]
    (when-not (and (= (count sources) (count source-rules) (count parsed))
                   (= [:slot/reserve :slot/release]
                      (mapv :id source-rules))
                   (= encoding (get-in shared [:domain :codes]))
                   (= status-field (get-in shared [:domain :field]))
                   (= [:in [:read status-field]
                       [:literal (set (keys encoding))]] (:expression inv))
                   (= (set (keys encoding))
                      (set (get-in shared [:domain :values])))
                   (= (:domain-form shared) (:guard-disjunction shared)))
      (reject! :shared-guard-model-mismatch
               {:source-operations (mapv :operation-id sources)
                :shared-operations (mapv :id (:guards shared))}))
    {:catalog-fingerprint (:catalog-fingerprint model)
     :status-encoding encoding
     :source-rules source-rules
     :shared-portfolio shared
     :operations parsed
     :closed-domain (:domain-form shared)}))

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
        shared (from-pilot! enum-ns 'check-portfolio! (:source-rules model))
        checked (mapv #(from-pilot! proof-ns 'check-obligation! %)
                      (proof-obligations! model))]
    (when-not (and (= 2 (:proposition-count shared))
                   (= :kernel-checked-conditional-finite-enum-partition
                      (:proof-status shared))
                   (= (:shared-portfolio model) (:model shared))
                   (= 2 (count (:checked shared)))
                   (= 3 (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (= (:catalog-fingerprint model)
                                     (:catalog-fingerprint description))
                                  (kernel/verifies? (ansatz/env) goal proof)
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies)))) checked))
      (reject! :unqualified-shared-or-reservation-proof
               {:shared-count (:proposition-count shared)
                :reservation-count (count checked)}))
    {:model model
     :checked checked
     :proof-count (count checked)
     :shared-checked (:checked shared)
     :shared-proof-count (:proposition-count shared)
     :total-proof-count (+ (count checked) (:proposition-count shared))
     :scope :closed-enum-guard-domain
     :translation-path :shared-source-derived-finite-enum-compiler
     :evaluator-correspondence :exhaustive-domain-observations-only
     :general-expression-semantics :not-proved
     :physical-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- evaluate-original-guards! [model]
  ;; One implementation of exhaustive typed evaluator correspondence:
  ;; delegate to the shared finite-enum proof compiler's observation loop.
  (mapv (fn [{:keys [member guard matches?]}]
          {:operation-id guard :status member :value matches?})
        (from-pilot! enum-ns 'observe-all! (:shared-portfolio model))))

(deftest original-guards-form-kernel-checked-exhaustive-exclusive-partition
  (let [{:keys [model checked] :as result}
        (check-guard-proofs! (example/reservation-catalog))
        reports (evaluate-original-guards! model)]
    (is (= 3 (:proof-count result)))
    (is (= 2 (:shared-proof-count result)))
    (is (= 5 (:total-proof-count result)))
    (is (= :shared-source-derived-finite-enum-compiler (:translation-path result)))
    (is (= 2 (count (:shared-checked result))))
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
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %))
                (:shared-checked result)))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest unsupported-guards-and-aliased-translations-fail-closed
  (let [catalog (example/reservation-catalog)
        model (checked-guard-model! catalog)
        source-rules (:source-rules model)
        denial (fn [f]
                 (try (f) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (testing "The SHARED compiler, not a reservation translator, rejects AST changes"
      (is (thrown? clojure.lang.ExceptionInfo
                   (from-pilot! enum-ns 'derive-portfolio!
                                (assoc-in source-rules [0 :rule :expression]
                                          [:not= [:read status-field]
                                           [:literal :available]]))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (from-pilot! enum-ns 'derive-portfolio!
                                (assoc-in source-rules [0 :rule :expression]
                                          [:= [:read :slot/audit-tag]
                                           [:literal :available]]))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (from-pilot! enum-ns 'derive-portfolio!
                                (assoc-in source-rules [0 :rule :expression]
                                          [:= [:read status-field]
                                           [:literal :unknown]])))))
    (testing "A duplicated source guard target loses shared coverage"
      (is (= :incomplete-or-aliased-guard-portfolio
             (denial #(from-pilot! enum-ns 'derive-portfolio!
                                  (assoc-in source-rules [1 :rule :expression]
                                            (get-in source-rules [0 :rule :expression])))))))
    (testing "A corrupted shared translation is rejected by the original bridge"
      (let [v (ns-resolve enum-ns 'derive-portfolio!)
            original @v]
        (with-redefs-fn {v (fn [rules]
                            (update-in (original rules) [:guards 0 :code] inc))}
          #(is (= :guard-formula-bridge-mismatch
                  (denial (fn [] (checked-guard-model! catalog))))))))
    (testing "Shared proof completeness is required by the original consumer"
      (let [v (ns-resolve enum-ns 'check-portfolio!)
            original @v]
        (with-redefs-fn {v (fn [rules]
                            (assoc (original rules) :proposition-count 1))}
          #(is (= :unqualified-shared-or-reservation-proof
                  (denial (fn [] (check-guard-proofs! catalog))))))))
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
    (is (= :typed-evaluator-disagrees
           (denial (fn [& args]
                     (assoc (apply original args) :value true)))))
    (is (= :typed-evaluator-disagrees
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
