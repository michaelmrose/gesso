(ns gesso.assurance.proof.xtdb-conditional-guard-admission
  "v1513: Restricted admission of mechanically CHECKABLE XTDB guard shapes.

   Reuse v1511's exact ASSERT/ASSERT/PUT semantics, and optionally check an
   additional canonical XTDB top-level field-count ASSERT. Comparing every
   previously observed field is not enough to protect a replacement document:
   a new field can appear between preparation and commit and be erased.

   This checker admits a PLAN SHAPE for bounded semantic analysis, NOT an
   executor, transaction, application, proof certificate, or authenticated
   observation. All caller-provided rows and SQL operations are untrusted.
   Even a field-complete plan depends on XTDB's tested ASSERT semantics,
   non-null scalar equality, an unchanged submitted plan, and complete
   declarations. No claim is made about nested fields, other documents,
   independent writers, actual transaction prestate, or real commit outcomes."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [gesso.assurance.proof.xtdb-conditional-write-semantics :as semantics]
            [gesso.model.tx :as tx]
            [honey.sql :as hsql]
            [xtdb.util :as xt.util]))

(def report-type
  :gesso.assurance.proof.xtdb-conditional-guard-admission/restricted-shape)
(def report-version 1)

(def ^:private original-inspect semantics/inspect-plan!)
(def ^:private original-field-count tx/assert-document-field-count)
(def ^:private original-format hsql/format)

(defn- reject! [reason]
  ;; Never attach the input: it contains physical rows and bound SQL values.
  (throw (ex-info "Unsupported XTDB guard admission input."
                  {:xtdb-conditional-guard-admission/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- field? [x]
  (and (keyword? x) (nil? (namespace x))
       (<= 1 (count (name x)) 64)
       (boolean (re-matches #"[a-z][a-z0-9-]*" (name x)))))

(defn- check-seams! []
  ;; Detect ordinary in-process accidental redefinition, not hostile JVM
  ;; instrumentation or a cryptographically authenticated source identity.
  (when-not (and (identical? original-inspect
                             (var-get #'semantics/inspect-plan!))
                 (identical? original-field-count
                             (var-get #'tx/assert-document-field-count))
                 (identical? original-format (var-get #'hsql/format)))
    (reject! :substituted-guard-checking-seam)))

(defn- canonical-count-op! [table before]
  (try
    ;; tx/prepare normalizes qualified SQL identifiers before HoneySQL
    ;; formatting. The count constructor's only qualified SQL identifier is
    ;; :xt/id; its runtime ID value is a bound string, not an SQL identifier.
    (original-format
     (walk/postwalk (fn [x]
                      (if (= x :xt/id)
                        (xt.util/kw->normal-form-kw x)
                        x))
                    (original-field-count table (:xt/id before)
                                          (count before))))
    (catch Throwable _
      (reject! :unsupported-field-count-assertion))))

(defn inspect-admission!
  "Classify a restricted XTDB plan without executing it.

   Input is EXACTLY:
     {:plan-input        <v1511 :table/:before/:after/:guard-fields/
                          :cas-fields/:tx-ops (two ASSERTs and one PUT)>
      :submitted-tx-ops <the actual candidate prepared operations vector>
      :frame-fields     #{<physical fields that MUST be unchanged>}}.

   The submission candidate may contain one extra ASSERT produced by
   tx/assert-document-field-count for the acquired document. It must appear
   BEFORE the PUT, without altering the order of the two base ASSERTs.
   A matching count assertion plus comparisons of every observed field
   makes a top-level replacement frame CONDITIONALLY CHECKABLE; it does not
   prove that this plan will be executed or that its prestate is authentic.

   Report contents are redacted: no rows, ids, SQL text, bound parameters,
   timestamps, or transaction keys are returned. The declared field names
   are included to explain missing guard coverage."
  [source]
  (when-not (and (plain-map? source)
                 (= #{:plan-input :submitted-tx-ops :frame-fields}
                    (set (keys source))))
    (reject! :invalid-admission-source))
  (check-seams!)
  (let [{:keys [plan-input submitted-tx-ops frame-fields]} source]
    (when-not (and (plain-map? plan-input)
                   (set? frame-fields)
                   (<= (count frame-fields) 12)
                   (every? field? frame-fields)
                   (vector? submitted-tx-ops)
                   (nil? (meta submitted-tx-ops))
                   (<= 3 (count submitted-tx-ops) 4))
      (reject! :invalid-admission-shape))
    ;; v1511 validates both the restricted row subset and the exact lowered
    ;; SQL parameters for the two canonical ASSERTs and the replacement PUT.
    (let [base (try (original-inspect plan-input)
                    (catch Throwable _
                      (reject! :unsupported-conditional-write-shape)))
          before (:before plan-input)
          after (:after plan-input)
          base-ops (:tx-ops plan-input)
          [first-assert second-assert write] base-ops
          count-op (canonical-count-op! (:table plan-input) before)
          candidates #{base-ops
                       [count-op first-assert second-assert write]
                       [first-assert count-op second-assert write]
                       [first-assert second-assert count-op write]}
          counted? (= 4 (count submitted-tx-ops))
          fields (disj (set (keys before)) :xt/id)
          compared (set (concat (:guard-fields plan-input)
                                (:cas-fields plan-input)))
          missing-comparisons (set/difference fields compared)
          frame-outside (set/difference frame-fields fields)]
      (when-not (and (= semantics/report-type (:type base))
                     (= 1 (:version base))
                     (true? (:canonical-shape-matches? base))
                     (true? (:all-assertions-precede-write? base))
                     (contains? candidates submitted-tx-ops)
                     (empty? frame-outside))
        (reject! :noncanonical-or-unmapped-guard-plan))
      (when-not (every? #(= (get before %) (get after %)) frame-fields)
        (reject! :declared-frame-field-modified))
      (let [all-observed-compared? (empty? missing-comparisons)
            shape-class (cond
                          (and all-observed-compared? counted?)
                          :complete-top-level-guard-shape

                          all-observed-compared?
                          :observed-field-guard-without-field-count

                          :else
                          :partial-observed-field-guard)
            frame-eligibility (cond
                                (empty? frame-fields)
                                :no-explicit-frame-fields

                                (not all-observed-compared?)
                                :missing-observed-field-guards

                                (not counted?)
                                :missing-field-count-guard

                                :else
                                :conditionally-checkable-top-level-frame)]
        {:type report-type
         :version report-version
         :scope :restricted-two-assert-one-put-with-optional-field-count
         :plan-shape-admission :admissible-for-bounded-modeling
         :guard-shape-class shape-class
         :canonical-order? true
         :field-count-assertion-present? counted?
         :all-observed-fields-compared? all-observed-compared?
         :unguarded-observed-fields missing-comparisons
         :declared-frame-fields frame-fields
         :declared-frame-values-preserved-in-plan? true
         :top-level-frame-checkability frame-eligibility
         :typed-business-guard-source-binding :not-established
         :actual-executed-plan-binding :not-established
         :transaction-internal-prestate :not-authenticated
         :complete-writer-coverage :not-established
         :unobserved-and-nested-dependencies :not-established
         :physical-frame-preservation :not-established
         :universal-refinement :not-established
         :proof-status :not-proved
         :certificate :not-issued
         :admission :not-admitted
         :authority :none}))))
