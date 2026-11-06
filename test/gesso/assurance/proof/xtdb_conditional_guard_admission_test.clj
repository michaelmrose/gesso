(ns gesso.assurance.proof.xtdb-conditional-guard-admission-test
  "v1514: Permanent adversarial admission tests for v1513's restricted
   XTDB conditional guard-shape checker.

   The positive result is a CONDITIONAL classification of a caller-supplied
   plan shape, never an issued executor, authenticated XTDB transaction,
   complete-writer claim, proof of frame preservation, or refinement proof.
   Real XTDB cases test the restricted assertion semantics, not universal
   correspondence with actual transaction-internal state."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-conditional-guard-admission :as admission]
            [gesso.assurance.proof.xtdb-conditional-write-semantics :as semantics]
            [gesso.model.tx :as tx]
            [honey.sql :as hsql]
            [xtdb.api :as xt]))

(defn- observation [app operation]
  (let [before ((:read! app))
        plan (#'physical/transition-plan before operation)
        after (get-in plan [:commands 0 :model/after])]
    {:app app :before before :after after :plan plan}))

(defn- fully-guarded-plan [before plan]
  (assoc plan :assertions
         [(tx/assert-one physical/table
                         [:and [:= :xt/id (:xt/id before)]
                          [:= :status (:status before)]
                          [:= :claims (:claims before)]
                          [:= :audit-tag (:audit-tag before)]
                          [:= :created-at (:created-at before)]])]))

(defn- admission-source
  ([sample] (admission-source sample {}))
  ([{:keys [app before after plan]} {:keys [full? counted? frame-fields]
                                     :or {full? false counted? false
                                          frame-fields #{:audit-tag}}}]
   (let [plan (if full? (fully-guarded-plan before plan) plan)
         counted-plan (if counted?
                        (update plan :assertions conj
                                (tx/assert-document-field-count
                                 physical/table (:xt/id before) (count before)))
                        plan)
         base-ops (:tx-ops (tx/prepare (:biff-context app) plan))
         submitted-ops (:tx-ops (tx/prepare (:biff-context app) counted-plan))]
     {:plan-input {:table physical/table
                   :before before
                   :after after
                   :guard-fields (if full?
                                   [:status :claims :audit-tag :created-at]
                                   [:status :claims :audit-tag])
                   :cas-fields [:revision :updated-at]
                   :tx-ops base-ops}
      :submitted-tx-ops submitted-ops
      :frame-fields frame-fields})))

(defn- rejection [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-conditional-guard-admission/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- assert-rejected! [reason source]
  (is (= reason (rejection #(admission/inspect-admission! source)))))

(defn- canonical-parts [source]
  ;; tx/prepare determines the real ASSERT ordering; do not assume the
  ;; optional count assertion occupies a fixed index.
  (let [[business cas write] (get-in source [:plan-input :tx-ops])
        count-assert (first (remove #{business cas write}
                                    (:submitted-tx-ops source)))]
    {:count-assert count-assert :business business :cas cas :write write}))

(defn- real-tx-outcome [app operations]
  (try
    (xt/execute-tx (:node app) operations)
    :committed
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        :assert-failed
        (throw e)))))

(defn- foreign-put! [app document]
  (xt/execute-tx (:node app) [[:put-docs physical/table document]]))

(deftest restricted-partial-guards-must-not-claim-frame-closure
  (physical/with-proving-application!
    (fn [app]
      (let [r (admission/inspect-admission! (admission-source (observation app :slot/reserve)))]
        (is (= admission/report-type (:type r)))
        (is (= admission/report-version (:version r)))
        (is (= :partial-observed-field-guard (:guard-shape-class r)))
        (is (= #{:created-at} (:unguarded-observed-fields r)))
        (is (false? (:field-count-assertion-present? r)))
        (is (false? (:all-observed-fields-compared? r)))
        (is (= :missing-observed-field-guards (:top-level-frame-checkability r)))
        (is (= :admissible-for-bounded-modeling (:plan-shape-admission r)))))))

(deftest comparing-all-observed-fields-still-does-not-exclude-additions
  (physical/with-proving-application!
    (fn [app]
      (let [r (admission/inspect-admission!
               (admission-source (observation app :slot/reserve) {:full? true}))]
        (is (= :observed-field-guard-without-field-count (:guard-shape-class r)))
        (is (true? (:all-observed-fields-compared? r)))
        (is (= #{} (:unguarded-observed-fields r)))
        (is (false? (:field-count-assertion-present? r)))
        (is (= :missing-field-count-guard (:top-level-frame-checkability r)))))))

(deftest field-count-cannot-compensate-for-missing-value-guards
  (physical/with-proving-application!
    (fn [app]
      (let [r (admission/inspect-admission!
               (admission-source (observation app :slot/reserve) {:counted? true}))]
        (is (true? (:field-count-assertion-present? r)))
        (is (false? (:all-observed-fields-compared? r)))
        (is (= #{:created-at} (:unguarded-observed-fields r)))
        (is (= :partial-observed-field-guard (:guard-shape-class r)))
        (is (= :missing-observed-field-guards (:top-level-frame-checkability r)))))))

(deftest fully-compared-and-counted-shape-is-only-conditionally-checkable
  (physical/with-proving-application!
    (fn [app]
      (let [source (admission-source (observation app :slot/reserve)
                                     {:full? true :counted? true
                                      :frame-fields #{:audit-tag :created-at}})
            r (admission/inspect-admission! source)]
        (is (= :complete-top-level-guard-shape (:guard-shape-class r)))
        (is (true? (:canonical-order? r)))
        (is (true? (:field-count-assertion-present? r)))
        (is (true? (:all-observed-fields-compared? r)))
        (is (= :conditionally-checkable-top-level-frame (:top-level-frame-checkability r)))
        (is (= #{:audit-tag :created-at} (:declared-frame-fields r)))
        (is (true? (:declared-frame-values-preserved-in-plan? r)))
        (is (= :not-established (:physical-frame-preservation r)))
        (is (= :not-authenticated (:transaction-internal-prestate r)))
        (is (= :not-established (:actual-executed-plan-binding r)))
        (is (= :not-proved (:proof-status r)))
        (is (= :not-issued (:certificate r)))
        (is (= :not-admitted (:admission r)))
        (is (= :none (:authority r)))))))

(deftest all-three-canonical-count-positions-precede-put
  (physical/with-proving-application!
    (fn [app]
      (let [source (admission-source (observation app :slot/reserve)
                                     {:full? true :counted? true})
            {:keys [count-assert business cas write]} (canonical-parts source)
            candidates [[count-assert business cas write]
                        [business count-assert cas write]
                        [business cas count-assert write]]]
        (doseq [ops candidates]
          (let [r (admission/inspect-admission!
                   (assoc source :submitted-tx-ops ops))]
            (is (= :complete-top-level-guard-shape (:guard-shape-class r)))
            (is (= :conditionally-checkable-top-level-frame
                   (:top-level-frame-checkability r)))))))))

(deftest no-declared-frame-fields-do-not-imply-frame-certification
  (physical/with-proving-application!
    (fn [app]
      (let [r (admission/inspect-admission!
               (admission-source (observation app :slot/reserve)
                                 {:full? true :counted? true :frame-fields #{}}))]
        (is (= :complete-top-level-guard-shape (:guard-shape-class r)))
        (is (= :no-explicit-frame-fields (:top-level-frame-checkability r)))
        (is (= :not-established (:physical-frame-preservation r)))))))

(deftest forgery-of-field-count-sql-or-bindings-is-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve)
                                  {:full? true :counted? true})
            ops (:submitted-tx-ops src)
            count-op (:count-assert (canonical-parts src))
            count-index (.indexOf ops count-op)]
        (is (vector? count-op))
        (doseq [bad-count [(assoc count-op 0 "ASSERT true")
                           (assoc count-op 0 (str (first count-op) " OR true"))
                           (assoc count-op 1 (inc (second count-op)))
                           (assoc count-op 2 "different-id")
                           (conj count-op "extra-param")]]
          (assert-rejected! :noncanonical-or-unmapped-guard-plan
                            (assoc src :submitted-tx-ops (assoc ops count-index bad-count))))))))

(deftest count-assertion-must-precede-write-and-be-unique
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve)
                                  {:full? true :counted? true})
            {:keys [count-assert business cas write]} (canonical-parts src)]
        (doseq [ops [[business cas write count-assert]
                     [business write count-assert cas]
                     [cas business count-assert write]
                     [count-assert business write cas]
                     [count-assert count-assert business write]]]
          (assert-rejected! :noncanonical-or-unmapped-guard-plan
                            (assoc src :submitted-tx-ops ops)))
        ;; A missing optional count assertion is a supported weaker shape;
        ;; it must never be misclassified as a complete top-level guard.
        (let [weakened (admission/inspect-admission!
                        (assoc src :submitted-tx-ops [business cas write]))]
          (is (= :observed-field-guard-without-field-count
                 (:guard-shape-class weakened)))
          (is (= :missing-field-count-guard
                 (:top-level-frame-checkability weakened))))
        (assert-rejected! :invalid-admission-shape
                          (assoc src :submitted-tx-ops
                                 [count-assert business cas write write]))))))

(deftest changing-business-or-cas-assertion-is-not-an-equivalent-plan
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve)
                                  {:full? true :counted? true})
            ops (:submitted-tx-ops src)
            {:keys [business cas]} (canonical-parts src)]
        (doseq [idx [(.indexOf ops business) (.indexOf ops cas)]]
          (assert-rejected! :noncanonical-or-unmapped-guard-plan
                            (assoc src :submitted-tx-ops
                                   (update ops idx assoc 0 "ASSERT 1 = 1")))
          (assert-rejected! :noncanonical-or-unmapped-guard-plan
                            (assoc src :submitted-tx-ops
                                   (update ops idx assoc 1 "forged-id"))))))))

(deftest changing-replacement-write-rejects-the-shape
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve)
                                  {:full? true :counted? true})
            ops (:submitted-tx-ops src)]
        (doseq [write [(assoc (last ops) 1 :different_table)
                       (update-in (last ops) [2 :claims] inc)
                       (update-in (last ops) [2 :audit-tag] str "-altered")]]
          (assert-rejected! :noncanonical-or-unmapped-guard-plan
                            (assoc src :submitted-tx-ops (assoc ops 3 write))))))))

(deftest declared-frame-fields-must-exist-and-remain-equal
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})]
        (assert-rejected! :noncanonical-or-unmapped-guard-plan
                          (assoc src :frame-fields #{:undeclared-field}))
        (assert-rejected! :invalid-admission-shape
                          (assoc src :frame-fields #{:xt/id :audit-tag}))
        (assert-rejected! :invalid-admission-shape
                          (assoc src :frame-fields [:audit-tag]))
        (assert-rejected! :declared-frame-field-modified
                          (assoc src :frame-fields #{:status}))))))

(deftest malformed-or-extended-source-fails-closed
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve))]
        (doseq [bad [(assoc src :claimed-certified? true)
                     (dissoc src :frame-fields)
                     (assoc src :frame-fields nil)
                     (assoc src :submitted-tx-ops (list* (:submitted-tx-ops src)))
                     (with-meta src {:already-verified true})
                     (update src :plan-input with-meta {:certified true})
                     (update src :submitted-tx-ops with-meta {:certified true})]]
          (is (not= :not-rejected
                    (rejection #(admission/inspect-admission! bad)))))))))

(deftest unsupported-row-values-and-base-guard-shapes-fail-closed
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve))]
        (doseq [bad [(assoc-in src [:plan-input :before :created-at] nil)
                     (assoc-in src [:plan-input :before :created-at] {:nested true})
                     (assoc-in src [:plan-input :before :created-at] [1 2])
                     (assoc-in src [:plan-input :guard-fields] [:claims :claims])
                     (assoc-in src [:plan-input :guard-fields] [:missing-field])
                     (assoc-in src [:plan-input :table] :different-table)]]
          (assert-rejected! :unsupported-conditional-write-shape bad))))))

(deftest checking-seams-cannot-be-substituted-for-positive-evidence
  (physical/with-proving-application!
    (fn [app]
      (let [src (admission-source (observation app :slot/reserve))]
        (with-redefs [semantics/inspect-plan! (fn [_] {:canonical-shape-matches? true})]
          (assert-rejected! :substituted-guard-checking-seam src))
        (with-redefs [tx/assert-document-field-count (fn [& _] [:sql "ASSERT true"])]
          (assert-rejected! :substituted-guard-checking-seam src))
        (with-redefs [hsql/format (fn [& _] ["ASSERT true"])]
          (assert-rejected! :substituted-guard-checking-seam src))))))

(deftest reports-redact-database-values-and-physical-identifiers
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})
            r (admission/inspect-admission! src)
            rendered (pr-str r)]
        (is (not (contains? r :plan-input)))
        (is (not (contains? r :submitted-tx-ops)))
        (is (not (contains? r :before)))
        (is (not (contains? r :after)))
        (is (not (contains? r :tx-key)))
        (is (not (.contains rendered (:xt/id (:before sample)))))
        (is (not (.contains rendered (:created-at (:before sample)))))
        (is (not (.contains rendered "ASSERT ")))
        (is (= :not-established (:complete-writer-coverage r)))
        (is (= :not-established (:unobserved-and-nested-dependencies r)))
        (is (= :not-established (:universal-refinement r)))))))

(deftest shape-inspection-does-not-submit-or-mutate-database-state
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})
            before ((:read! app))
            attempts (atom 0)]
        (with-redefs [xt/execute-tx (fn [& _]
                                      (swap! attempts inc)
                                      (throw (ex-info "unexpected write" {})))]
          (is (= :complete-top-level-guard-shape
                 (:guard-shape-class (admission/inspect-admission! src)))))
        (is (zero? @attempts))
        (is (= before ((:read! app))))))))

(deftest real-xtdb-rejects-foreign-added-field-with-full-count-guard
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})
            changed (assoc (:before sample) :foreign-note "external")]
        (is (= :conditionally-checkable-top-level-frame
               (:top-level-frame-checkability (admission/inspect-admission! src))))
        (foreign-put! app changed)
        (is (= :assert-failed (real-tx-outcome app (:submitted-tx-ops src))))
        (is (= changed ((:read! app))))))))

(deftest real-xtdb-rejects-intervening-guarded-value-change
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})
            changed (assoc (:before sample) :created-at "external-new-value")]
        (foreign-put! app changed)
        (is (= :assert-failed (real-tx-outcome app (:submitted-tx-ops src))))
        (is (= changed ((:read! app))))))))

(deftest real-xtdb-rejects-field-removal-with-count-guard
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})
            changed (dissoc (:before sample) :created-at)]
        (foreign-put! app changed)
        (is (= :assert-failed (real-tx-outcome app (:submitted-tx-ops src))))
        ;; The application read helper correctly rejects this malformed
        ;; document, so compare the raw XTDB value instead.
        (is (= changed
               (first (xt/q (:node app)
                            ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                             (:xt/id changed)]))))))))

(deftest real-xtdb-commits-unchanged-prestate-with-full-guard
  (physical/with-proving-application!
    (fn [app]
      (let [sample (observation app :slot/reserve)
            src (admission-source sample {:full? true :counted? true})]
        (is (= :conditionally-checkable-top-level-frame
               (:top-level-frame-checkability (admission/inspect-admission! src))))
        (is (= :committed (real-tx-outcome app (:submitted-tx-ops src))))
        (is (= (:after sample) ((:read! app))))))))
