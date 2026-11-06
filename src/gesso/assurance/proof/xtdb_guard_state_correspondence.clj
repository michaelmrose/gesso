(ns gesso.assurance.proof.xtdb-guard-state-correspondence
  "v1515: Conditional correspondence between a source-owned reservation rule,
   an admitted XTDB ASSERT/PUT plan and a caller-supplied candidate state.

   This is a RESTRICTED single-document reservation example, not a generic
   XTDB interpreter. The rule is obtained from the original Gesso catalog;
   the concrete field mapping is pinned to the example's known projection.
   The existing v1511 and v1513 checkers inspect the exact candidate SQL and
   optional top-level field-count guard. No transaction is submitted here.

   The candidate statement-time state is explicitly UNAUTHENTICATED. Even a
   positive model result does not identify the actual XTDB transaction basis,
   prove a submitted plan was used, exclude other writers, authenticate an
   outcome, or establish full physical refinement. Reports never return rows,
   SQL parameters, document IDs, or sensitive state values."
  (:require [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.typed-presence-semantics :as typed]
            [gesso.assurance.proof.xtdb-conditional-guard-admission :as admission]
            [gesso.assurance.proof.xtdb-conditional-write-semantics :as semantics]))

(def report-type
  :gesso.assurance.proof.xtdb-guard-state-correspondence/conditional-step)
(def report-version 1)

(def ^:private original-catalog pilot/reservation-catalog)
(def ^:private original-project physical/physical->abstract)
(def ^:private original-evaluate typed/evaluate-expression!)
(def ^:private original-admission admission/inspect-admission!)
(def ^:private original-model-step semantics/model-step!)

(def ^:private physical-keys
  #{:xt/id :revision :created-at :updated-at :status :claims :audit-tag})
(def ^:private accepted-guard-fields
  #{[:status :claims :audit-tag]
    [:status :claims :audit-tag :created-at]})
(def ^:private accepted-frame-fields
  #{#{:audit-tag} #{:audit-tag :created-at}})

(defn- reject! [reason]
  ;; Deliberately omit source, exception cause and data that may contain rows.
  (throw (ex-info "Unsupported XTDB guard/state correspondence input."
                  {:xtdb-guard-state-correspondence/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentHashMap m)
           (instance? clojure.lang.PersistentArrayMap m))
       (nil? (meta m))))

(defn- seams! []
  ;; This only detects ordinary process-local Var replacement. It does NOT
  ;; authenticate compiled code against hostile classloader instrumentation.
  (when-not (and (identical? original-catalog
                             (var-get #'pilot/reservation-catalog))
                 (identical? original-project
                             (var-get #'physical/physical->abstract))
                 (identical? original-evaluate
                             (var-get #'typed/evaluate-expression!))
                 (identical? original-admission
                             (var-get #'admission/inspect-admission!))
                 (identical? original-model-step
                             (var-get #'semantics/model-step!)))
    (reject! :substituted-semantic-source)))

(defn- typed-true! [declaration types state]
  (let [expression (:expression declaration)
        readers (->> expression
                     (tree-seq coll? seq)
                     (filter #(and (vector? %) (= :read (first %))))
                     (map second)
                     set)
        _ (when-not (= readers (:readers declaration))
            (reject! :typed-reader-footprint-diverges))
        result (try (original-evaluate expression
                                       (select-keys types readers)
                                       (select-keys state readers))
                    (catch Throwable _
                      (reject! :unsupported-typed-expression)))]
    (when-not (and (= typed/result-type (:type result))
                   (true? (:value result))
                   (= :not-proved (:proof-status result))
                   (= :not-admitted (:admission result))
                   (= readers (:source-readers result)))
      (reject! :typed-precondition-or-invariant-failed))))

(defn- source-transition! [op before after]
  (let [catalog (original-catalog)
        entries (filter #(= op (:id %)) (:operations catalog))]
    (when-not (and (= :slot/reservation-lifecycle (:id catalog))
                   (= #{:slot/reserve :slot/release}
                      (:declared-operation-ids catalog))
                   (= 2 (count (:operations catalog)))
                   (= 1 (count entries))
                   (= #{:slot/status-domain :slot/nonnegative-claims
                        :slot/at-most-one-claim}
                      (set (keys (:invariants catalog)))))
      (reject! :missing-or-changed-source-operation))
    (let [construction (:construction (first entries))
          types (:state-types construction)
          guard (:guard construction)
          abstract-before (try (original-project before)
                               (catch Throwable _
                                 (reject! :unprojectable-observed-before)))
          abstract-after (try (original-project after)
                              (catch Throwable _
                                (reject! :unprojectable-planned-after)))
          assignments (:replacements construction)
          expected-after (reduce-kv
                          (fn [s k v]
                            (when-not (and (vector? v) (= 2 (count v))
                                           (= :literal (first v)))
                              (reject! :nonliteral-or-unsupported-assignment))
                            (assoc s k (second v)))
                          abstract-before assignments)]
      (when-not (and (= #{:slot/status :slot/active-claims :slot/audit-tag}
                       (set (keys types)))
                     (= #{:slot/status} (:readers guard))
                     (= [:= [:read :slot/status]
                         [:literal (if (= op :slot/reserve)
                                     :available :reserved)]]
                        (:expression guard))
                     (= #{:slot/status :slot/active-claims}
                        (:writes construction))
                     (= #{:slot/audit-tag} (:frame construction))
                     (= #{:slot/status :slot/active-claims}
                        (set (keys assignments)))
                     (= op (get-in construction [:subject :id]))
                     (= expected-after abstract-after)
                     (= (:slot/audit-tag abstract-before)
                        (:slot/audit-tag abstract-after)))
        (reject! :abstract-transition-and-physical-row-diverge))
      (typed-true! guard types abstract-before)
      (doseq [[_ inv] (:invariants catalog)]
        (typed-true! inv types abstract-before)
        (typed-true! inv types abstract-after))
      ;; Keep the concrete version/time mapping explicit and narrow. This is
      ;; a fixture-specific mapping premise, NOT a general refinement theorem.
      (when-not (and (= physical-keys (set (keys before)))
                     (= physical-keys (set (keys after)))
                     (string? (:xt/id before))
                     (= (:xt/id before) (:xt/id after))
                     (integer? (:revision before))
                     (<= 0 (:revision before))
                     (= (inc (:revision before)) (:revision after))
                     (string? (:created-at before))
                     (= (:created-at before) (:created-at after))
                     (string? (:updated-at before))
                     (= (str "v1494-revision-" (inc (:revision before)))
                        (:updated-at after))
                     (= (:audit-tag before) (:audit-tag after)))
        (reject! :unsupported-physical-version-or-frame))
      {:catalog-id (:id catalog)
       :operation-id op
       :abstract-before abstract-before
       :abstract-after abstract-after})))

(defn inspect-conditional-step!
  "Inspect a source-derived typed transition against admitted physical SQL.

   Input keys EXACTLY:
     {:operation-id               :slot/reserve | :slot/release
      :plan-input                 <v1511 restricted plan-input>
      :submitted-tx-ops           <v1513 candidate lowered operations>
      :frame-fields               #{:audit-tag} or
                                  #{:audit-tag :created-at}
      :candidate-transaction-state <caller-supplied physical document or nil>}

   The candidate state is NOT an XTDB-authenticated transaction snapshot.
   The model assumes supported sequential ASSERT semantics. With a canonical
   field-count guard, cardinality changes block the modeled replacement; with
   partial guards, intervening unguarded field changes may be overwritten.
   No actual execution, retry, or state mutation occurs."
  [source]
  (when-not (and (plain-map? source)
                 (= #{:operation-id :plan-input :submitted-tx-ops
                      :frame-fields :candidate-transaction-state}
                    (set (keys source))))
    (reject! :invalid-correspondence-source))
  (seams!)
  (let [{:keys [operation-id plan-input submitted-tx-ops frame-fields
                candidate-transaction-state]} source]
    (when-not (and (#{:slot/reserve :slot/release} operation-id)
                   (plain-map? plan-input)
                   (= physical/table (:table plan-input))
                   (accepted-guard-fields (:guard-fields plan-input))
                   (= [:revision :updated-at] (:cas-fields plan-input))
                   (accepted-frame-fields frame-fields))
      (reject! :unsupported-typed-physical-link))
    (let [before (:before plan-input)
          after (:after plan-input)
          typed-source (source-transition! operation-id before after)
          admission-source {:plan-input plan-input
                            :submitted-tx-ops submitted-tx-ops
                            :frame-fields frame-fields}
          admitted (try (original-admission admission-source)
                        (catch Throwable _
                          (reject! :unadmitted-physical-guard-plan)))
          modeled (try (original-model-step plan-input
                                             candidate-transaction-state)
                       (catch Throwable _
                         (reject! :unsupported-candidate-transaction-state)))
          ;; v1511 models only its two canonical ASSERTs. XTDB's optional
          ;; additional field-count ASSERT is separately simulated here.
          count-matches? (or (not (:field-count-assertion-present? admitted))
                             (and (some? candidate-transaction-state)
                                  (= (count before)
                                     (count candidate-transaction-state))))
          would-commit? (and (= :would-commit (:modeled-outcome modeled))
                             count-matches?)
          expected-after (:abstract-after typed-source)
          replacement (when would-commit?
                        (try (original-project (:next-document modeled))
                             (catch Throwable _
                               (reject! :invalid-modeled-replacement))))]
      (when-not (and (= admission/report-type (:type admitted))
                     (= admission/report-version (:version admitted))
                     (= :admissible-for-bounded-modeling
                        (:plan-shape-admission admitted))
                     (= :not-admitted (:admission admitted))
                     (= :not-proved (:proof-status admitted))
                     (= semantics/model-type (:type modeled))
                     (= 1 (:version modeled))
                     (= :not-proved (:proof-status modeled)))
        (reject! :unexpected-upstream-assurance-status))
      (when (and would-commit? (not= replacement expected-after))
        (reject! :modeled-write-diverges-from-typed-transition))
      {:type report-type
       :version report-version
       :scope :source-linked-reservation-conditional-single-document-step
       :operation-id operation-id
       :catalog-id (:catalog-id typed-source)
       :typed-guard-at-declared-before-and-invariants-checked? true
       :source-transition-matches-planned-physical-replacement? true
       :physical-guard-shape (:guard-shape-class admitted)
       :frame-checkability (:top-level-frame-checkability admitted)
       :field-count-guard-present? (:field-count-assertion-present? admitted)
       :candidate-statement-state :caller-supplied-unattested
       :modeled-assertion-outcome (if would-commit?
                                    :would-commit :assertion-would-fail)
       :conditional-abstract-correspondence
       (if would-commit?
         :modeled-replacement-agrees-with-source-derived-transition
         :no-authoritative-abstract-step-projected)
       :unguarded-observed-fields (:unguarded-observed-fields admitted)
       :modeled-overwritten-unguarded-fields
       (if would-commit? (:overwritten-unguarded-fields modeled) #{})
       :physical-value-mapping :fixture-specific-not-independently-proved
       :submitted-plan-execution-binding :not-established
       :xtdb-transaction-internal-prestate :not-authenticated
       :backend-semantics-proof :not-established
       :authorization-authenticity :not-established
       :complete-writer-coverage :not-established
       :physical-outcome :not-observed
       :physical-frame-preservation :not-established
       :universal-refinement :not-established
       :proof-status :not-proved
       :certificate :not-issued
       :admission :not-admitted
       :authority :none})))
