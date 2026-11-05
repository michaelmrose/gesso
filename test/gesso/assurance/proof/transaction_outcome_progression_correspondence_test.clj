(ns gesso.assurance.proof.transaction-outcome-progression-correspondence-test
  "v1397: Permanent adversarial regression tests for v1396's deliberately
   non-authoritative transaction-outcome/progression classifier.

   Detached observations and application-created diagnostics are copyable.
   Even a perfect reported key/ledger match cannot authenticate payload,
   causality, authorization, transition refinement, or permission to retry.
   Test both the pure classifier and one actual controlled submission path;
   do not depend on the preceding revision's temporary test namespaces."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.classified-physical-acquisition :as acquisition]
            [gesso.assurance.proof.executed-guard-authorization-correspondence-test :as guard-fixture]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as fixture]
            [gesso.assurance.proof.transaction-outcome-progression-correspondence :as outcome]
            [gesso.assurance.proof.xtdb-submission-outcome-correlation :as correlation]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.model.tx :as tx])
  (:import [java.time Instant]
           [xtdb.api TransactionKey]))

;; Preserve the permanent, source-checked application fixture's contract.
(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns
                 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(def ^:private observed-at (Instant/parse "2026-02-14T10:11:12.123456Z"))
(def ^:private later-at (Instant/parse "2026-02-14T10:11:13.123456Z"))
(def ^:private physical-open
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(defn- key-at [id time]
  (reify TransactionKey
    (getTxId [_] id)
    (getSystemTime [_] time)))

(defn- incorporated-tx
  ([] (incorporated-tx (key-at 42 observed-at)))
  ([key]
   {:commit/status :committed
    :tx-result key
    :progression (xtdb/tx-result-progression key)}))

(defn- execution
  ([resolution]
   {:outcome :returned
    :result {:controlled-result {:resolution resolution}}})
  ([resolution extra]
   (merge (execution resolution) extra)))

(defn- acquisition-report
  ([physical] (acquisition-report physical false))
  ([physical agreement?]
   {:type acquisition/report-type
    :version acquisition/report-version
    :execution-outcome :returned
    :proof-status :classified-coacquisition-diagnostic-only
    :certificate :not-issued
    :admission :not-admitted
    :authority :none
    :transaction-authority :none
    :actual-payload-accepted-by-xtdb? :not-established
    :transaction-internal-prestate :not-established
    :causal-attribution-to-executor-attempt :not-established
    :authenticated-request-or-authorization :not-established
    :other-physical-writers-excluded? :not-established
    :deployed-handler-or-route-identity :not-established
    :all-physical-refinement-obligations physical-open
    :all-physical-refinement-obligation-statuses :open
    :bounded-executor-adapter-ledger-agreement? agreement?
    :status (if agreement?
              :bounded-coobserved-metadata-agreement
              :incomplete-or-divergent-submission-correlation)
    :executor-observation {:physical-outcome physical
                           :status :process-local-executor-observations-agree}
    :adapter-api-status :reported-execute-ledger-agrees
    :ledger-observation-status :reported-key-matches-committed-ledger-row}))

(defn- observation
  ([resolution physical agreement?]
   {:execution (execution resolution)
    :diagnostic (acquisition-report physical agreement?)})
  ([resolution physical]
   (observation resolution physical false)))

(defn- classify [resolution physical agreement?]
  (outcome/classify-observation (observation resolution physical agreement?)))

(defn- no-authority! [report]
  (is (= outcome/report-type (:type report)))
  (is (= outcome/report-version (:version report)))
  (is (= :conditional-execution-progression-diagnostic-only
         (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (= :none (:transaction-authority report)))
  (is (= :none (:live-settlement-authority report)))
  (is (false? (:abstract-transition-state-pair-observed? report)))
  (is (false? (:reported-basis-authenticated? report)))
  (is (false? (:physical-outcome-authenticated? report)))
  (is (false? (:commit-unknown-safe-to-retry? report)))
  (is (false? (:business-rejection-established? report)))
  (is (= physical-open (:physical-refinement-obligations report)))
  (is (= :open (:physical-refinement-obligation-statuses report)))
  (is (= :boolean (cond
                    (true? (:bounded-adapter-ledger-agreement? report)) :boolean
                    (false? (:bounded-adapter-ledger-agreement? report)) :boolean
                    :else :wrong-type))))

(defn- rejection [candidate]
  (try
    (outcome/classify-observation candidate)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected! [candidate expected]
  (let [data (rejection candidate)]
    (is (true? (:transaction-outcome-progression/rejection data)))
    (is (= expected (:reason data)))
    (is (not (contains? data :execution)))
    (is (not (contains? data :diagnostic)))))

(deftest acknowledged-incorporation-with-ledger-and-reported-progression
  (let [d (classify {:status :incorporated
                     :transaction (incorporated-tx)} :committed true)]
    (is (= :bounded-incorporation-and-ledger-coobservation (:status d)))
    (is (= :reported-basis-matches-public-transaction-key
           (:reported-authoritative-progression-status d)))
    (is (= :one-durable-command-incorporation-reported
           (:claimed-abstract-effect d)))
    (is (= :committed (:observed-physical-attempt-status d)))
    (is (= :incorporated (:durable-command-resolution-status d)))
    (no-authority! d)))

(deftest claimed-incorporation-without-bounded-ledger-agreement
  (let [d (classify {:status :incorporated
                     :transaction (incorporated-tx)} :committed false)]
    (is (= :incorporation-without-coobserved-ledger-agreement (:status d)))
    (is (false? (:bounded-adapter-ledger-agreement? d)))
    (no-authority! d)))

(deftest incorporated-but-rejected-or-unknown-physical-outcome
  (doseq [physical [:rejected :indeterminate :unobserved]]
    (let [d (classify {:status :incorporated
                       :transaction (incorporated-tx)} physical true)]
      (is (= :inconsistent-reported-incorporation (:status d)))
      (is (= physical (:observed-physical-attempt-status d)))
      (no-authority! d))))

(deftest missing-or-inconsistent-durable-transaction-acknowledgment
  (doseq [transaction [nil
                       {}
                       {:tx-result (key-at 42 observed-at)}
                       {:commit/status :committed}
                       {:commit/status :rejected
                        :tx-result (key-at 42 observed-at)}
                       {:commit/status :indeterminate
                        :tx-result (key-at 42 observed-at)}]]
    (let [d (classify {:status :incorporated :transaction transaction}
                      :committed true)]
      (is (= :inconsistent-reported-incorporation (:status d)))
      (no-authority! d))))

(deftest durable-replay-is-not-a-newly-observed-commit
  (doseq [physical [:committed :rejected :indeterminate :unobserved]
          agreement? [false true]]
    (let [d (classify {:status :replay
                       :outcome {:private-token "do-not-leak"}}
                      physical agreement?)]
      (is (= :durable-replay-no-new-commit-attribution (:status d)))
      (is (= :previous-incorporation-reconciled-no-new-attempt-attribution
             (:claimed-abstract-effect d)))
      (is (not (re-find #"do-not-leak" (pr-str d))))
      (no-authority! d))))

(deftest unresolved-command-remains-unresolved-after-physical-rejection
  (doseq [[physical expected]
          [[:rejected :physical-assertion-rejected-command-unresolved]
           [:committed :physical-commit-observed-command-unresolved]
           [:indeterminate :commit-unknown-reconciliation-required]
           [:unobserved :commit-unknown-no-definitive-observation]]]
    (let [d (classify {:status :unresolved} physical false)]
      (is (= expected (:status d)))
      (is (= :no-resolved-command-incorporation
             (:claimed-abstract-effect d)))
      (no-authority! d))))

(deftest unresolved-with-claimed-ledger-agreement-does-not-become-incorporated
  (doseq [physical [:committed :rejected :indeterminate :unobserved]]
    (let [d (classify {:status :unresolved} physical true)]
      (is (not= :bounded-incorporation-and-ledger-coobservation (:status d)))
      (is (= :no-resolved-command-incorporation (:claimed-abstract-effect d)))
      (no-authority! d))))

(deftest disclosure-unavailable-and-binding-incompatible-are-not-authoritative
  (doseq [status [:unavailable :incompatible]]
    (let [d (classify {:status status} :committed true)]
      (is (= :binding-or-disclosure-blocked (:status d)))
      (is (not= :one-durable-command-incorporation-reported
                (:claimed-abstract-effect d)))
      (no-authority! d))))

(deftest missing-or-unknown-resolution-is-not-interpreted-as-a-rejection
  (doseq [resolution [nil {} {:status :mystery} {:status :unknown}]]
    (let [d (classify resolution :committed true)]
      (is (= :unclassified-or-missing-resolution (:status d)))
      (is (false? (:business-rejection-established? d)))
      (no-authority! d))))

(deftest matching-transaction-id-but-different-clock-diverges
  (let [tx (incorporated-tx)
        wrong-key (key-at 42 later-at)
        d (classify {:status :incorporated
                     :transaction (assoc tx :progression
                                         (xtdb/tx-result-progression wrong-key))}
                    :committed true)]
    (is (= :reported-progression-diverged (:status d)))
    (is (= :reported-basis-diverges-from-public-transaction-key
           (:reported-authoritative-progression-status d)))
    (no-authority! d)))

(deftest matching-clock-but-different-transaction-id-diverges
  (let [tx (incorporated-tx)
        d (classify {:status :incorporated
                     :transaction (assoc tx :progression
                                         (xtdb/tx-result-progression
                                           (key-at 43 observed-at)))}
                    :committed true)]
    (is (= :reported-progression-diverged (:status d)))
    (is (= :reported-basis-diverges-from-public-transaction-key
           (:reported-authoritative-progression-status d)))
    (no-authority! d)))

(deftest malformed-and-empty-reported-progression-diverge
  (doseq [bad [{:bases #{}}
               {:bases nil}
               {:not-a-progression true}
               nil]]
    (let [tx (assoc (incorporated-tx) :progression bad)
          d (classify {:status :incorporated :transaction tx}
                      :committed true)]
      (is (= :reported-progression-diverged (:status d)))
      (is (= :malformed-reported-progression
             (:reported-authoritative-progression-status d)))
      (no-authority! d))))

(deftest unreported-progression-is-not-invented
  (let [tx (dissoc (incorporated-tx) :progression)
        d (classify {:status :incorporated :transaction tx}
                    :committed true)]
    (is (= :not-reported (:reported-authoritative-progression-status d)))
    (is (= :bounded-incorporation-and-ledger-coobservation (:status d)))
    (no-authority! d)))

(deftest copied-agreement-boolean-alone-is-never-sufficient
  (let [base (observation {:status :incorporated
                           :transaction (incorporated-tx)}
                          :committed true)
        variants [[:status :incomplete-or-divergent-submission-correlation]
                  [:adapter-api-status :not-one-complete-api-call]
                  [:ledger-observation-status :not-visible]]]
    (doseq [[field altered] variants]
      (let [d (outcome/classify-observation
                (assoc-in base [:diagnostic field] altered))]
        (is (= :incorporation-without-coobserved-ledger-agreement
               (:status d)))
        (is (false? (:bounded-adapter-ledger-agreement? d)))
        (no-authority! d)))
    (let [d (outcome/classify-observation
              (assoc-in base [:diagnostic :executor-observation :status]
                        :unmatched))]
      (is (= :incorporation-without-coobserved-ledger-agreement
             (:status d)))
      (no-authority! d))))

(deftest physical-rejection-never-promotes-positive-ledger-agreement
  (let [candidate (observation {:status :incorporated
                                :transaction (incorporated-tx)}
                               :rejected true)
        d (outcome/classify-observation candidate)]
    (is (false? (:bounded-adapter-ledger-agreement? d)))
    (is (= :inconsistent-reported-incorporation (:status d)))
    (no-authority! d)))

(deftest thrown-attempt-remains-unresolved-and-private
  (let [failure (ex-info "PRIVATE-thrown-payload" {:secret "hidden"})
        candidate {:execution {:outcome :threw :error failure}
                   :diagnostic (assoc (acquisition-report :committed true)
                                      :execution-outcome :threw)}
        d (outcome/classify-observation candidate)]
    (is (= :execution-threw-outcome-unresolved (:status d)))
    (is (not (re-find #"PRIVATE-thrown-payload|hidden" (pr-str d))))
    (no-authority! d)))

(deftest rejected-envelope-shapes-and-altered-proof-claims
  (let [base (observation {:status :unresolved} :indeterminate)
        altered [(dissoc base :diagnostic)
                 (assoc base :unexpected true)
                 (with-meta base {:forged :origin})
                 (assoc-in base [:execution :outcome] :bogus)
                 (assoc-in base [:diagnostic :type] :other)
                 (assoc-in base [:diagnostic :version] 999)
                 (assoc-in base [:diagnostic :certificate] :issued)
                 (assoc-in base [:diagnostic :admission] :certified)
                 (assoc-in base [:diagnostic :authority] :physical)
                 (assoc-in base [:diagnostic :transaction-authority] :committed)
                 (assoc-in base [:diagnostic :actual-payload-accepted-by-xtdb?] true)
                 (assoc-in base [:diagnostic :transaction-internal-prestate] {})
                 (assoc-in base [:diagnostic :causal-attribution-to-executor-attempt] true)
                 (assoc-in base [:diagnostic :other-physical-writers-excluded?] true)
                 (assoc-in base [:diagnostic :all-physical-refinement-obligations] #{})
                 (assoc-in base [:diagnostic :all-physical-refinement-obligation-statuses]
                           :discharged)
                 (assoc-in base [:diagnostic :bounded-executor-adapter-ledger-agreement?]
                           :yes)
                 (assoc-in base [:diagnostic :executor-observation :physical-outcome]
                           :fiction)]]
    (rejected! (dissoc base :execution) :invalid-acquisition-shape)
    (doseq [candidate altered]
      (let [expected (if (or (not= #{:execution :diagnostic}
                                    (set (keys candidate)))
                             (meta candidate))
                       :invalid-acquisition-shape
                       :invalid-or-unchecked-acquisition-diagnostic)]
        (rejected! candidate expected)))))

(deftest rejected-missing-throwable-or-returned-result
  (let [base (observation {:status :unresolved} :unobserved)]
    (doseq [candidate [(assoc-in base [:execution :outcome] :threw)
                       (update base :execution dissoc :result)
                       (assoc-in base [:diagnostic :execution-outcome] :threw)]]
      (rejected! candidate :invalid-or-unchecked-acquisition-diagnostic))))

(deftest no-secrets-from-application-binding-or-diagnostic-payload
  (let [secret "SUPER-PRIVATE-PAYLOAD"
        candidate (assoc-in
                    (observation {:status :replay :binding secret}
                                 :committed true)
                    [:execution :result :private-binding] secret)
        d (outcome/classify-observation candidate)]
    (is (not (re-find (re-pattern secret) (pr-str d))))
    (is (not (contains? d :execution)))
    (is (not (contains? d :result)))
    (no-authority! d)))

(defn- issued-source []
  ;; Use the permanent issued-source fixture, not v1394's temporary test.
  {:guard-source ((var-get (ns-resolve
                           'gesso.assurance.proof.executed-guard-authorization-correspondence-test
                           'source)))
   :observation-node :fake})

(defn- tx-through-adapter [_ plan]
  (assoc (xtdb/execute-tx! :fake (:tx-ops (tx/prepare {} plan)))
         :commit/status :committed))

(defn- with-adapter-and-ledger [api-calls ledger-row f]
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
     (fn [_ _ _]
       (swap! api-calls inc)
       (key-at 42 observed-at))
     (ns-resolve 'gesso.assurance.proof.xtdb-submission-outcome-correlation
                 'ledger-row)
     ledger-row}
    f))

(deftest controlled-invocation-classifies-one-complete-observed-attempt
  (let [calls (atom 0)]
    (with-redefs [tx/transact! tx-through-adapter]
      (with-adapter-and-ledger
        calls
        (fn [& _] {:status :observed
                   :row {:xt/id 42 :committed true :system-time observed-at}})
        (fn []
          (let [result (outcome/run-outcome-progression!
                         (issued-source) {:app/actor :user/a})
                d (:diagnostic result)]
            (is (= 1 @calls))
            (is (= :returned (get-in result [:execution :outcome])))
            (is (= :incorporated (:durable-command-resolution-status d)))
            (is (= :bounded-incorporation-and-ledger-coobservation (:status d)))
            (is (= :reported-basis-matches-public-transaction-key
                   (:reported-authoritative-progression-status d)))
            (no-authority! d)))))))

(deftest missing-ledger-does-not-become-causal-commit-proof
  (let [calls (atom 0)]
    (with-redefs [tx/transact! tx-through-adapter]
      (with-adapter-and-ledger
        calls
        (fn [& _] {:status :not-visible})
        (fn []
          (let [result (outcome/run-outcome-progression!
                         (issued-source) {:app/actor :user/a})
                d (:diagnostic result)]
            (is (= 1 @calls))
            (is (= :incorporation-without-coobserved-ledger-agreement
                   (:status d)))
            (is (false? (:bounded-adapter-ledger-agreement? d)))
            (no-authority! d)))))))

(deftest substituted-outer-acquisition-entry-point-is-rejected
  (let [calls (atom 0)]
    (with-redefs [acquisition/run-classified-acquisition!
                  (fn [& _] (swap! calls inc) :fake)]
      (let [data (try (outcome/run-outcome-progression!
                       (issued-source) {:app/actor :user/a})
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :substituted-acquisition-entry-point (:reason data)))
        (is (true? (:transaction-outcome-progression/rejection data)))
        (is (zero? @calls))))))
