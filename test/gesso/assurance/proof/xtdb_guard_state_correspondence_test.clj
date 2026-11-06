(ns gesso.assurance.proof.xtdb-guard-state-correspondence-test
  "v1516: Adversarial tests for the v1515 conditional, source-linked
   reservation guard/state model. These assertions distinguish the
   *caller-supplied* candidate state from XTDB's actual transaction prestate.
   No positive modeled step, even with a field-count guard, is a commit
   attestation, a proof of deployment, or a general refinement theorem."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.typed-presence-semantics :as typed]
            [gesso.assurance.proof.xtdb-conditional-guard-admission :as admission]
            [gesso.assurance.proof.xtdb-conditional-write-semantics :as semantics]
            [gesso.assurance.proof.xtdb-guard-state-correspondence :as target]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(defn- source-for
  ([app operation] (source-for app operation {}))
  ([app operation {:keys [full? counted? frame-fields candidate]
                   :or {full? false counted? false
                        frame-fields #{:audit-tag}
                        candidate ::observed}}]
   (let [before ((:read! app))
         base (#'physical/transition-plan before operation)
         full (if full?
                (assoc base :assertions
                       [(tx/assert-one
                         physical/table
                         [:and [:= :xt/id (:xt/id before)]
                          [:= :status (:status before)]
                          [:= :claims (:claims before)]
                          [:= :audit-tag (:audit-tag before)]
                          [:= :created-at (:created-at before)]])])
                base)
         counted (if counted?
                   (update full :assertions conj
                           (tx/assert-document-field-count
                            physical/table (:xt/id before) (count before)))
                   full)
         after (get-in full [:commands 0 :model/after])]
     {:operation-id operation
      :plan-input {:table physical/table
                   :before before
                   :after after
                   :guard-fields (if full?
                                   [:status :claims :audit-tag :created-at]
                                   [:status :claims :audit-tag])
                   :cas-fields [:revision :updated-at]
                   :tx-ops (:tx-ops (tx/prepare (:biff-context app) full))}
      :submitted-tx-ops (:tx-ops (tx/prepare (:biff-context app) counted))
      :frame-fields frame-fields
      :candidate-transaction-state
      (if (= ::observed candidate) before candidate)})))

(defn- rejection [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-guard-state-correspondence/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- assert-rejection! [reason source]
  (is (= reason (rejection #(target/inspect-conditional-step! source)))))

(defn- inspect! [source]
  (target/inspect-conditional-step! source))

(defn- assert-noncertifying! [report]
  (is (= target/report-type (:type report)))
  (is (= target/report-version (:version report)))
  (is (= :caller-supplied-unattested (:candidate-statement-state report)))
  (is (= :fixture-specific-not-independently-proved (:physical-value-mapping report)))
  (is (= :not-established (:submitted-plan-execution-binding report)))
  (is (= :not-authenticated (:xtdb-transaction-internal-prestate report)))
  (is (= :not-established (:backend-semantics-proof report)))
  (is (= :not-established (:authorization-authenticity report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-established (:physical-frame-preservation report)))
  (is (= :not-observed (:physical-outcome report)))
  (is (= :not-established (:universal-refinement report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report))))

(defn- real-tx-outcome [app ops]
  (try
    (xt/execute-tx (:node app) ops)
    :committed
    (catch Throwable e
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
        :assert-failed
        (throw e)))))

(deftest source-linked-reserve-is-bounded-not-certified
  (physical/with-proving-application!
    (fn [app]
      (let [result (inspect! (source-for app :slot/reserve))]
        (is (= :slot/reserve (:operation-id result)))
        (is (= :slot/reservation-lifecycle (:catalog-id result)))
        (is (true? (:typed-guard-at-declared-before-and-invariants-checked? result)))
        (is (= :partial-observed-field-guard (:physical-guard-shape result)))
        (is (= #{:created-at} (:unguarded-observed-fields result)))
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (is (= :modeled-replacement-agrees-with-source-derived-transition
               (:conditional-abstract-correspondence result)))
        (assert-noncertifying! result)))))

(deftest complete-counted-guard-is-still-conditional
  (physical/with-proving-application!
    (fn [app]
      (let [result (inspect! (source-for app :slot/reserve
                                         {:full? true :counted? true
                                          :frame-fields #{:audit-tag :created-at}}))]
        (is (= :complete-top-level-guard-shape (:physical-guard-shape result)))
        (is (= :conditionally-checkable-top-level-frame
               (:frame-checkability result)))
        (is (true? (:field-count-guard-present? result)))
        (is (empty? (:unguarded-observed-fields result)))
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (assert-noncertifying! result)))))

(deftest release-uses-the-same-original-catalog
  (physical/with-proving-application!
    (fn [app]
      (is (= :committed (:commit/status ((:execute! app) :slot/reserve))))
      (let [result (inspect! (source-for app :slot/release
                                         {:full? true :counted? true}))]
        (is (= :slot/release (:operation-id result)))
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (assert-noncertifying! result)))))

(deftest absent-statement-state-makes-no-authoritative-step
  (physical/with-proving-application!
    (fn [app]
      (let [result (inspect! (source-for app :slot/reserve {:candidate nil}))]
        (is (= :assertion-would-fail (:modeled-assertion-outcome result)))
        (is (= :no-authoritative-abstract-step-projected
               (:conditional-abstract-correspondence result)))
        (is (= #{} (:modeled-overwritten-unguarded-fields result)))
        (assert-noncertifying! result)))))

(deftest guarded-value-changes-block-the-modeled-transaction
  (physical/with-proving-application!
    (fn [app]
      (let [src (source-for app :slot/reserve)
            observed (:candidate-transaction-state src)]
        (doseq [[field value] [[:status "reserved"] [:claims 1]
                               [:audit-tag "changed"] [:revision 7]
                               [:updated-at "foreign"]]]
          (testing (name field)
            (let [result (inspect! (assoc src :candidate-transaction-state
                                          (assoc observed field value)))]
              (is (= :assertion-would-fail (:modeled-assertion-outcome result)))
              (is (= :no-authoritative-abstract-step-projected
                     (:conditional-abstract-correspondence result))))))))))

(deftest unguarded-same-cardinality-change-exposes-overwrite
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)
            current (assoc (:candidate-transaction-state base)
                           :created-at "intervening")
            partial (inspect! (assoc base :candidate-transaction-state current))
            full (inspect! (source-for app :slot/reserve
                                       {:full? true :candidate current}))]
        (is (= :would-commit (:modeled-assertion-outcome partial)))
        (is (contains? (:modeled-overwritten-unguarded-fields partial)
                       :created-at))
        (is (= :assertion-would-fail (:modeled-assertion-outcome full)))
        (is (= :not-established (:physical-frame-preservation partial)))))))

(deftest field-count-without-value-guards-is-not-enough
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve {:counted? true})
            current (assoc (:candidate-transaction-state base)
                           :created-at "same-count-change")
            result (inspect! (assoc base :candidate-transaction-state current))]
        (is (= :partial-observed-field-guard (:physical-guard-shape result)))
        (is (true? (:field-count-guard-present? result)))
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (is (contains? (:modeled-overwritten-unguarded-fields result)
                       :created-at))
        (assert-noncertifying! result)))))

(deftest field-addition-exposes-risk-unless-count-guarded
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)
            current (assoc (:candidate-transaction-state base)
                           :foreign-note "outside")
            partial (inspect! (assoc base :candidate-transaction-state current))
            full (inspect! (source-for app :slot/reserve
                                       {:full? true :counted? true
                                        :candidate current}))]
        (is (= :would-commit (:modeled-assertion-outcome partial)))
        (is (contains? (:modeled-overwritten-unguarded-fields partial)
                       :foreign-note))
        (is (= :assertion-would-fail (:modeled-assertion-outcome full)))
        (is (= :no-authoritative-abstract-step-projected
               (:conditional-abstract-correspondence full)))))))

(deftest removal-of-unguarded-field-remains-a-risk-without-count
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)
            current (dissoc (:candidate-transaction-state base) :created-at)
            partial (inspect! (assoc base :candidate-transaction-state current))
            full (inspect! (source-for app :slot/reserve
                                       {:full? true :counted? true
                                        :candidate current}))]
        (is (= :would-commit (:modeled-assertion-outcome partial)))
        (is (contains? (:modeled-overwritten-unguarded-fields partial)
                       :created-at))
        (is (= :assertion-would-fail (:modeled-assertion-outcome full)))))))

(deftest whole-counted-guard-rejects-same-count-value-replacement
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve {:full? true :counted? true})
            current (assoc (:candidate-transaction-state base)
                           :created-at "foreign-same-count")
            result (inspect! (assoc base :candidate-transaction-state current))]
        (is (= :assertion-would-fail (:modeled-assertion-outcome result)))
        (is (true? (:field-count-guard-present? result)))
        (assert-noncertifying! result)))))

(deftest reported-positive-candidate-is-not-an-authenticated-xtdb-snapshot
  (physical/with-proving-application!
    (fn [app]
      (let [source (source-for app :slot/reserve {:full? true :counted? true})
            original (:candidate-transaction-state source)
            changed (assoc original :created-at "a-real-intervening-write")]
        (xt/execute-tx (:node app) [[:put-docs physical/table changed]])
        (is (= changed ((:read! app))))
        ;; The candidate is still the stale caller-provided value. The
        ;; checker must not pretend it observed XTDB's internal prestate.
        (let [result (inspect! source)]
          (is (= :would-commit (:modeled-assertion-outcome result)))
          (is (= :not-authenticated (:xtdb-transaction-internal-prestate result)))
          (is (= :not-observed (:physical-outcome result)))
          (is (= :assert-failed (real-tx-outcome app (:submitted-tx-ops source))))
          (assert-noncertifying! result))))))

(deftest wrong-operation-identity-and-plan-link-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (doseq [operation [:slot/unknown nil "reserve"]]
          (assert-rejection! :unsupported-typed-physical-link
                             (assoc base :operation-id operation)))
        ;; :slot/release is an admitted operation, but the prepared physical
        ;; reserve transition diverges from its source-declared assignment.
        (assert-rejection! :abstract-transition-and-physical-row-diverge
                           (assoc base :operation-id :slot/release))
        (assert-rejection! :unsupported-typed-physical-link
                           (assoc-in base [:plan-input :table] :other_table))
        (assert-rejection! :unsupported-typed-physical-link
                           (assoc-in base [:plan-input :cas-fields] [:revision]))
        (assert-rejection! :unsupported-typed-physical-link
                           (assoc-in base [:plan-input :guard-fields]
                                     [:claims :audit-tag]))
        (assert-rejection! :unsupported-typed-physical-link
                           (assoc base :frame-fields #{}))
        (assert-rejection! :unsupported-typed-physical-link
                           (assoc base :frame-fields #{:missing-field}))))))

(deftest invalid-or-redecorated-inputs-fail-closed
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (doseq [changed [(dissoc base :candidate-transaction-state)
                         (assoc base :certificate :issued)
                         (assoc base :authority :full)
                         (with-meta base {:claimed :verified})
                         [] nil]]
          (assert-rejection! :invalid-correspondence-source changed))))))

(deftest malformed-or-unprojectable-physical-rows-fail
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (assert-rejection! :unprojectable-observed-before
                           (assoc-in base [:plan-input :before :status] "impossible"))
        (assert-rejection! :unprojectable-planned-after
                           (assoc-in base [:plan-input :after :claims] 2))
        (assert-rejection! :unsupported-physical-version-or-frame
                           (update-in base [:plan-input :after :revision] inc))
        (assert-rejection! :unsupported-physical-version-or-frame
                           (assoc-in base [:plan-input :after :created-at] "tampered"))
        (assert-rejection! :unsupported-candidate-transaction-state
                           (assoc base :candidate-transaction-state [:not-a-row]))
        (assert-rejection! :unsupported-candidate-transaction-state
                           (assoc base :candidate-transaction-state
                                  {:xt/id (:xt/id (:candidate-transaction-state base))
                                   :status nil}))))))

(deftest modified-sql-assertions-and-replacement-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update-in base [:submitted-tx-ops 0 0]
                                      str " AND TRUE"))
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update-in base [:submitted-tx-ops 0 2] str "-fake"))
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update base :submitted-tx-ops
                                   #(vec (reverse %))))
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update-in base [:submitted-tx-ops 2 2 :status]
                                      (constantly "available")))
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update base :submitted-tx-ops conj
                                   [:put-docs physical/table
                                    (get-in base [:plan-input :after])]))))))

(deftest altered-plan-input-and-source-derived-replacement-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (assert-rejection! :unprojectable-planned-after
                           (assoc-in base [:plan-input :after :status] "available"))
        (assert-rejection! :abstract-transition-and-physical-row-diverge
                           (assoc-in base [:plan-input :after :audit-tag] "changed"))
        (assert-rejection! :unadmitted-physical-guard-plan
                           (update-in base [:plan-input :tx-ops 0 0]
                                      str " -- altered"))
        (assert-rejection! :unsupported-physical-version-or-frame
                           (update-in base [:plan-input :before :created-at]
                                      (constantly "false-before")))))))

(deftest ordinary-semantic-var-substitution-is-rejected
  (physical/with-proving-application!
    (fn [app]
      (let [base (source-for app :slot/reserve)]
        (is (= :substituted-semantic-source
               (rejection #(with-redefs [pilot/reservation-catalog (fn [] {})]
                             (inspect! base)))))
        (is (= :substituted-semantic-source
               (rejection #(with-redefs [physical/physical->abstract identity]
                             (inspect! base)))))
        (is (= :substituted-semantic-source
               (rejection #(with-redefs [typed/evaluate-expression! (fn [& _] nil)]
                             (inspect! base)))))
        (is (= :substituted-semantic-source
               (rejection #(with-redefs [admission/inspect-admission! (fn [& _] nil)]
                             (inspect! base)))))
        (is (= :substituted-semantic-source
               (rejection #(with-redefs [semantics/model-step! (fn [& _] nil)]
                             (inspect! base)))))))))

(deftest inspection-is-read-only-and-does-not-retry
  (physical/with-proving-application!
    (fn [app]
      (let [source (source-for app :slot/reserve {:full? true :counted? true})
            before ((:read! app))
            attempts (atom 0)
            result (with-redefs [xt/execute-tx (fn [& _]
                                                (swap! attempts inc)
                                                (throw (ex-info "submitted" {})))]
                     (inspect! source))]
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (is (zero? @attempts))
        (is (= before ((:read! app))))
        (assert-noncertifying! result)))))

(deftest real-execution-agrees-on-an-unchanged-supported-case
  (physical/with-proving-application!
    (fn [app]
      (let [source (source-for app :slot/reserve {:full? true :counted? true})
            result (inspect! source)]
        (is (= :would-commit (:modeled-assertion-outcome result)))
        (is (= :not-observed (:physical-outcome result)))
        (is (= :committed (real-tx-outcome app (:submitted-tx-ops source))))
        (is (= :reserved (:slot/status ((:abstract! app)))))
        (is (= 1 (:slot/active-claims ((:abstract! app)))))
        (assert-noncertifying! result)))))

(deftest real-execution-rejects-added-field-and-keeps-foreign-row
  (physical/with-proving-application!
    (fn [app]
      (let [source (source-for app :slot/reserve {:full? true :counted? true})
            current (assoc (:candidate-transaction-state source)
                           :foreign-note "added-after-preparation")
            result (inspect! (assoc source :candidate-transaction-state current))]
        (is (= :assertion-would-fail (:modeled-assertion-outcome result)))
        (xt/execute-tx (:node app) [[:put-docs physical/table current]])
        (is (= :assert-failed (real-tx-outcome app (:submitted-tx-ops source))))
        (is (= current ((:read! app))))
        (assert-noncertifying! result)))))

(deftest report-is-redacted-even-on-positive-model-step
  (physical/with-proving-application!
    (fn [app]
      (let [source (source-for app :slot/reserve {:full? true :counted? true})
            result (inspect! source)
            printed (pr-str result)
            before (:candidate-transaction-state source)]
        (is (not-any? #(contains? result %)
                      [:plan-input :submitted-tx-ops :candidate-transaction-state
                       :before :after :physical-document :tx-key]))
        (is (not (.contains printed (:xt/id before))))
        (is (not (.contains printed (:created-at before))))
        (is (not (.contains printed "ASSERT ?")))
        (is (not (.contains printed "v1494-created")))
        (assert-noncertifying! result)))))
