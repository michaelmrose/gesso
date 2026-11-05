(ns gesso.assurance.proof.committed-incorporation-progression-test
  "v1409: Permanent adversarial regression tests for the v1408 *diagnostic*
   comparison of a reported commit, durable command record, and Live basis.

   These are detached, caller-supplied readbacks. Delayed visibility, lost
   acknowledgement, replays, rejected attempts, and forged progression below
   are bounded in-memory cases, not proofs of physical XTDB read isolation,
   command causality, or permission to retry/settle. No actual crash is run."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.classified-physical-acquisition :as acquisition]
            [gesso.assurance.proof.committed-incorporation-progression :as target]
            [gesso.assurance.proof.transaction-outcome-progression-correspondence
             :as outcome]
            [gesso.choreo.identity :as identity]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.progression :as progression]
            [gesso.model.command-resolution :as resolution])
  (:import [java.time Instant]
           [xtdb.api TransactionKey]))

(def ^:private secret "V1409_PRIVATE_ARGUMENT_DO_NOT_DISCLOSE")
(def ^:private observed-at (Instant/parse "2026-02-14T10:11:12.123456Z"))
(def ^:private later-at (Instant/parse "2026-02-14T10:11:13.123456Z"))

(defn- tx-key [id at]
  (reify TransactionKey
    (getTxId [_] id)
    (getSystemTime [_] at)))

(def ^:private committed-key (tx-key 42 observed-at))
(def ^:private committed-basis (xtdb/tx-result-progression committed-key))
(def ^:private unrelated-basis (xtdb/tx-result-progression (tx-key 43 later-at)))
(def ^:private command-binding
  {:command-id (identity/identity :command "v1409-command-1")
   :principal (identity/identity :principal "v1409-actor")
   :operation :request/claim
   :arguments {:request-id secret}})
(def ^:private private-outcome {:status :claimed :comment secret})
(def ^:private durable-record
  (resolution/command-record command-binding private-outcome observed-at))
(def ^:private physical-obligations
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :actual-inputs-and-authorization-to-issued-executor-binding
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(defn- transaction
  ([] (transaction committed-key))
  ([key]
   (let [basis (xtdb/tx-result-progression key)]
     {:commit/status :committed
      :tx-result key
      :consistency (xtdb/tx-result-consistency key)
      :progression basis
      :ctx {:gesso.live/progression basis}})))

(defn- acquisition-report
  ([physical] (acquisition-report physical (= physical :committed) :returned))
  ([physical agreement? execution-outcome]
   {:type acquisition/report-type
    :version acquisition/report-version
    :status (if agreement?
              :bounded-coobserved-metadata-agreement
              :incomplete-or-divergent-submission-correlation)
    :execution-outcome execution-outcome
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
    :all-physical-refinement-obligations physical-obligations
    :all-physical-refinement-obligation-statuses :open
    :bounded-executor-adapter-ledger-agreement? agreement?
    :executor-observation {:physical-outcome physical
                           :status (if agreement?
                                     :process-local-executor-observations-agree
                                     :unmatched)}
    :adapter-api-status (if agreement?
                          :reported-execute-ledger-agrees
                          :not-one-complete-api-call)
    :ledger-observation-status (if agreement?
                                 :reported-key-matches-committed-ledger-row
                                 :not-verified)}))

(defn- incorporated []
  {:status :incorporated
   :outcome private-outcome
   :transaction (transaction)})

(defn- request
  ([] (request (incorporated) durable-record committed-basis))
  ([resolution-result record read-basis]
   {:observation {:execution {:outcome :returned
                              :result {:controlled-result
                                       {:resolution resolution-result}}}
                  :diagnostic (acquisition-report :committed)}
    :binding command-binding
    :post-readback {:record record :progression read-basis}}))

(defn- inspect [source]
  (target/compare-incorporation! source))

(defn- no-authority! [report]
  (is (= target/report-type (:type report)))
  (is (= target/report-version (:version report)))
  (is (= :bounded-incorporation-progression-diagnostic-only (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (doseq [k [:authority :transaction-authority :live-settlement-authority]]
    (is (= :none (get report k))))
  (doseq [k [:record-read-authenticated?
             :transaction-causality-authenticated?
             :physical-write-footprint-complete?
             :prior-attempt-commit-attributed?
             :business-rejection-established?
             :safe-to-retry?
             :safe-to-settle?]]
    (is (false? (get report k)) (str "No authority from " k)))
  (is (= :open (:physical-refinement-obligation-statuses report)))
  (is (= physical-obligations (:physical-refinement-obligations report)))
  (is (= :process-local-v1394-copied-evidence
         (get-in report [:observation-origins :executor-and-ledger])))
  (is (= :caller-supplied-readback-unattested
         (get-in report [:observation-origins :durable-record])))
  (is (= :caller-supplied-read-requirement-unattested
         (get-in report [:observation-origins :read-progression])))
  (is (not (re-find (re-pattern secret) (pr-str report))))
  (is (not (contains? report :observation)))
  (is (not (contains? report :binding)))
  (is (not (contains? report :record)))
  (is (not (contains? report :transaction))))

(defn- rejection [candidate]
  (try
    (inspect candidate)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected! [candidate reason]
  (let [data (rejection candidate)]
    (is (true? (:committed-incorporation-progression/rejection data)))
    (is (= reason (:reason data)))
    (is (not (re-find (re-pattern secret) (pr-str data))))
    (is (not (contains? data :observation)))
    (is (not (contains? data :post-readback)))))

(deftest exact-match-is-bounded-coobservation-not-a-certificate
  (let [r (inspect (request))]
    (is (= :bounded-incorporation-and-progression-coobservation (:status r)))
    (is (= :incorporated (:reported-resolution-status r)))
    (is (= :bounded-incorporation-and-ledger-coobservation
           (:reported-transaction-status r)))
    (is (= :reported-key-consistency-and-context-agree
           (:transaction-context-progression-status r)))
    (is (= :reported-readback-includes-transaction-basis
           (:post-readback-progression-status r)))
    (is (= :valid (:durable-record-status r)))
    (is (true? (:durable-command-binding-agrees? r)))
    (is (true? (:durable-outcome-agrees? r)))
    (is (true? (:bounded-three-way-agreement? r)))
    (no-authority! r)))

(deftest delayed-command-incorporation-readback-does-not-settle-earlier-check
  (let [earlier (inspect (request (incorporated) nil nil))
        after-record (inspect (request (incorporated) durable-record nil))
        later (inspect (request))]
    (is (= :durable-record-not-visible (:status earlier)))
    (is (= :progression-or-readback-diverged (:status after-record)))
    (is (= :bounded-incorporation-and-progression-coobservation (:status later)))
    (doseq [r [earlier after-record later]]
      (no-authority! r))))

(deftest delayed-progress-requirement-or-stale-readback-never-implies-read-was-pinned
  (doseq [[reported expected]
          [[nil :not-reported]
           [unrelated-basis :reported-readback-omits-transaction-basis]
           [:invalid :invalid-readback-progression]]]
    (let [r (inspect (request (incorporated) durable-record reported))]
      (is (= :progression-or-readback-diverged (:status r)))
      (is (= expected (:post-readback-progression-status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest unrelated-newer-readback-basis-is-not-proof-of-durable-inclusion
  (let [r (inspect (request (incorporated) durable-record unrelated-basis))]
    (is (= :reported-readback-omits-transaction-basis
           (:post-readback-progression-status r)))
    (no-authority! r)))

(deftest durable-record-absent-malformed-and-mutated-fail-closed
  (doseq [[record expected]
          [[nil :durable-record-not-visible]
           [{} :invalid-durable-record]
           [(dissoc durable-record :gesso.command/status) :invalid-durable-record]
           [(assoc durable-record :gesso.command/revision 1) :invalid-durable-record]
           [(assoc durable-record :xt/id "other-id") :invalid-durable-record]
           [(assoc durable-record :gesso.command/created-at later-at) :invalid-durable-record]
           [(with-meta durable-record {:untrusted true}) :invalid-durable-record]]]
    (let [r (inspect (request (incorporated) record committed-basis))]
      (is (= expected (:status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest wrong-but-valid-command-binding-is-divergence-not-incorporation
  (doseq [other-binding [(assoc command-binding
                              :command-id (identity/identity :command "v1409-other"))
                         (assoc command-binding
                                :arguments {:request-id "other-request"})
                         (assoc command-binding :principal
                                (identity/identity :principal "another-actor"))]]
    (let [other (resolution/command-record other-binding private-outcome observed-at)
          r (inspect (request (incorporated) other committed-basis))]
      (is (= :valid (:durable-record-status r)))
      (is (= :durable-record-binding-diverged (:status r)))
      (is (false? (:durable-command-binding-agrees? r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest durable-outcome-substitution-is-detected-even-for-valid-command-record
  (doseq [different [{:status :claimed :comment "other"}
                     {:status :failed :comment secret}
                     {:status :claimed}
                     {:status :claimed :comment secret :extra true}]]
    (let [record (assoc durable-record :gesso.command/outcome different)
          r (inspect (request (incorporated) record committed-basis))]
      (is (= :valid (:durable-record-status r)))
      (is (true? (:durable-command-binding-agrees? r)))
      (is (false? (:durable-outcome-agrees? r)))
      (is (= :durable-record-outcome-diverged (:status r)))
      (no-authority! r))))

(deftest replay-remains-prior-incorporation-even-if-every-readback-matches
  (doseq [physical [:committed :rejected :indeterminate :unobserved]]
    (let [s (request {:status :replay :outcome private-outcome}
                     durable-record committed-basis)
          s (assoc-in s [:observation :diagnostic]
                      (acquisition-report physical))
          r (inspect s)]
      (is (= :previous-incorporation-not-attributed-to-attempt (:status r)))
      (is (= :replay (:reported-resolution-status r)))
      (is (false? (:durable-outcome-agrees? r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest unresolved-command-with-committed-or-indeterminate-attempt-is-not-rejection
  (doseq [physical [:committed :rejected :indeterminate :unobserved]]
    (let [s (request {:status :unresolved} durable-record committed-basis)
          s (assoc-in s [:observation :diagnostic]
                      (acquisition-report physical))
          r (inspect s)]
      (is (= :unresolved-command-no-commit-established (:status r)))
      (is (= :unresolved (:reported-resolution-status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest incompatible-and-undisclosed-command-record-remain-nonauthoritative
  (doseq [[resolution-status expected]
          [[:incompatible :incompatible-command-binding]
           [:unavailable :command-disclosure-unavailable]
           [:not-observed :nonincorporating-or-uncertain-outcome]]]
    (let [r (inspect (request {:status resolution-status}
                              durable-record committed-basis))]
      (is (= expected (:status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest thrown-execution-with-reported-durable-record-cannot-prove-incorporation
  (let [s (request (incorporated) durable-record committed-basis)
        s (assoc s :observation {:execution {:outcome :threw
                                            :error (ex-info secret {:private secret})}
                                 :diagnostic (acquisition-report :indeterminate
                                                                 false :threw)})
        r (inspect s)]
    (is (= :nonincorporating-or-uncertain-outcome (:status r)))
    (is (= :not-observed (:reported-resolution-status r)))
    (no-authority! r)))

(deftest committed-acknowledgement-without-adapter-ledger-agreement-is-insufficient
  (doseq [physical [:committed :rejected :indeterminate :unobserved]]
    (let [s (request)
          s (assoc-in s [:observation :diagnostic]
                      (acquisition-report physical false :returned))
          r (inspect s)]
      (is (= :commit-or-ledger-correlation-incomplete (:status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest missing-inconsistent-or-rejected-transaction-acknowledgement
  (doseq [tx [nil {} (dissoc (transaction) :tx-result)
              (assoc (transaction) :commit/status :rejected)
              (assoc (transaction) :commit/status :indeterminate)]]
    (let [r (inspect (request (assoc (incorporated) :transaction tx)
                              durable-record committed-basis))]
      (is (= :commit-or-ledger-correlation-incomplete (:status r)))
      (is (false? (:bounded-three-way-agreement? r)))
      (no-authority! r))))

(deftest transaction-key-and-reported-consistency-must-agree
  (let [tx (transaction)
        candidates [(assoc-in tx [:consistency :tx-id] 101)
                    (assoc-in tx [:consistency :system-time] later-at)
                    (assoc tx :consistency {})
                    (assoc tx :consistency nil)
                    (assoc tx :tx-result (tx-key 41 observed-at))]]
    (doseq [changed candidates]
      (let [r (inspect (request (assoc (incorporated) :transaction changed)
                                durable-record committed-basis))]
        ;; A substituted public transaction key is rejected by v1396 first;
        ;; consistency/context mutations are caught by the v1408 comparator.
        (is (contains? #{:progression-or-readback-diverged
                         :commit-or-ledger-correlation-incomplete} (:status r)))
        (is (not= :bounded-incorporation-and-progression-coobservation
                  (:status r)))
        (is (not= :reported-key-consistency-and-context-agree
                  (:transaction-context-progression-status r)))
        (no-authority! r)))))

(deftest transaction-and-context-progressions-must-refer-to-the-transaction
  (let [tx (transaction)
        candidates [(assoc tx :progression unrelated-basis)
                    (assoc tx :progression nil)
                    (assoc tx :ctx {})
                    (assoc tx :ctx nil)
                    (assoc-in tx [:ctx :gesso.live/progression] unrelated-basis)
                    (assoc-in tx [:ctx :gesso.live/progression] nil)]]
    (doseq [changed candidates]
      (let [r (inspect (request (assoc (incorporated) :transaction changed)
                                durable-record committed-basis))]
        (is (false? (:bounded-three-way-agreement? r)))
        (is (not= :bounded-incorporation-and-progression-coobservation
                  (:status r)))
        (no-authority! r)))))

(deftest mismatched-tx-time-even-with-equal-id-does-not-match
  (let [same-id-other-time (tx-key 42 later-at)
        changed-tx (assoc (transaction) :progression
                          (xtdb/tx-result-progression same-id-other-time))
        r (inspect (request (assoc (incorporated) :transaction changed-tx)
                            durable-record committed-basis))]
    (is (false? (:bounded-three-way-agreement? r)))
    (no-authority! r)))

(deftest reported-progressions-never-authenticate-the-actual-read
  (let [normal (inspect (request))
        rb2 (progression/requirement-from-bases (into (:bases committed-basis) (:bases unrelated-basis)))
        enlarged (inspect (request (incorporated) durable-record rb2))]
    (is (= :bounded-incorporation-and-progression-coobservation (:status normal)))
    (is (= :bounded-incorporation-and-progression-coobservation (:status enlarged)))
    (is (false? (:record-read-authenticated? enlarged)))
    (is (false? (:transaction-causality-authenticated? enlarged)))
    (no-authority! normal)
    (no-authority! enlarged)))

(deftest invalid-source-envelopes-are-rejected-with-redacted-reasons
  (let [s (request)]
    (doseq [bad [nil [] (dissoc s :binding) (assoc s :extra secret)
                 (with-meta s {:sensitive secret})]]
      (rejected! bad :invalid-source-shape))
    (doseq [bad [(assoc s :observation nil)
                 (update s :observation assoc :extra secret)
                 (assoc s :post-readback nil)
                 (update s :post-readback assoc :extra secret)]]
      (rejected! bad :invalid-observation-or-readback-shape))))

(deftest malformed-binding-and-forged-diagnostic-must-be-rejected
  (let [s (request)]
    (doseq [bad [(assoc-in s [:binding :principal] "pretend-principal")
                 (assoc-in s [:binding :operation] "claim")]]
      (rejected! bad :invalid-command-binding))
    (doseq [bad [(assoc-in s [:observation :diagnostic :certificate] :issued)
                 (assoc-in s [:observation :diagnostic :authority] :all)
                 (assoc-in s [:observation :diagnostic :execution-outcome] :threw)
                 (assoc-in s [:observation :diagnostic :all-physical-refinement-obligations]
                           #{})]]
      (rejected! bad :invalid-outcome-observation))))

(deftest substituted-checkers-are-not-accepted-even-when-returning-positive
  (let [s (request)]
    (with-redefs [outcome/classify-observation
                  (fn [_] {:durable-command-resolution-status :incorporated})]
      (rejected! s :substituted-upstream-checker))
    (with-redefs [resolution/require-record (fn [r] r)]
      (rejected! s :substituted-upstream-checker))
    (with-redefs [resolution/require-binding (fn [b] b)]
      (rejected! s :substituted-upstream-checker))))

(deftest no-private-inputs-or-outcomes-appear-in-any-status
  (let [variants [(request)
                  (request (incorporated) nil nil)
                  (request (incorporated) durable-record unrelated-basis)
                  (request {:status :replay :outcome private-outcome}
                           durable-record committed-basis)
                  (request {:status :unresolved} durable-record committed-basis)]]
    (doseq [source variants]
      (let [r (inspect source)]
        (is (not (re-find (re-pattern secret) (pr-str r))))
        (no-authority! r)))))
