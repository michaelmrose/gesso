(ns gesso.assurance.proof.commit-unknown-reconciliation-test
  "v1399: Permanent adversarial regression coverage for *reported* later
   reconciliation of an earlier commit-unknown controlled execution.

   Lost responses and crash windows below are deliberately represented by
   detached, caller-supplied observation envelopes; no test simulates an OS
   crash, authenticates XTDB records, or grants an automated retry.  In
   particular, a later replay must never be attributed to the earlier attempt,
   and a missing later record must never become proof of noncommit."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.classified-physical-acquisition :as acquisition]
            [gesso.assurance.proof.commit-unknown-reconciliation :as unknown]
            [gesso.assurance.proof.transaction-outcome-progression-correspondence
             :as prior]
            [gesso.live.progression :as progression]
            [gesso.model.command-resolution :as resolution]))

(def ^:private secret "SECRET_V1399_NEVER_DISCLOSE")
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

(def ^:private status-by-physical
  {:indeterminate :commit-unknown-reconciliation-required
   :unobserved :commit-unknown-no-definitive-observation
   :committed :physical-commit-observed-command-unresolved
   :rejected :physical-assertion-rejected-command-unresolved})

(def ^:private status-by-later
  {:replay :later-replay-reported-prior-attempt-unattributed
   :unresolved :later-command-record-not-visible-noncommit-unproved
   :incompatible :later-binding-incompatible-no-disclosure
   :unavailable :later-record-disclosure-unavailable})

(defn- acquisition-diagnostic [physical execution-outcome]
  {:type acquisition/report-type
   :version acquisition/report-version
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
   :all-physical-refinement-obligations physical-open
   :all-physical-refinement-obligation-statuses :open
   :bounded-executor-adapter-ledger-agreement? false
   :status :incomplete-or-divergent-submission-correlation
   :executor-observation {:physical-outcome physical
                          :status :unmatched}
   :adapter-api-status :not-one-complete-api-call
   :ledger-observation-status :not-verified})

(defn- observation
  ([physical] (observation physical :returned))
  ([physical outcome]
   {:execution (if (= :threw outcome)
                 {:outcome :threw
                  :error (ex-info (str "LOST_RESPONSE_" secret)
                                  {:secret secret})}
                 {:outcome :returned
                  :result {:controlled-result
                           {:resolution {:status :unresolved
                                         :private-payload secret}}}})
    :diagnostic (acquisition-diagnostic physical outcome)}))

(defn- later
  ([status] (later status {:business-result secret}))
  ([status result]
   (cond-> {:status status :scope resolution/assurance-scope}
     (= :replay status) (assoc :outcome result))))

(defn- request
  ([physical status]
   (request (observation physical) (later status) nil))
  ([earlier later-result reported-progression]
   {:prior-observation earlier
    :later-resolution later-result
    :later-progression reported-progression}))

(defn- classify [physical later-status]
  (unknown/classify-later-reconciliation (request physical later-status)))

(defn- no-authority! [result]
  (is (= unknown/report-type (:type result)))
  (is (= unknown/report-version (:version result)))
  (is (= :later-reconciliation-correlation-diagnostic-only
         (:proof-status result)))
  (is (= :not-issued (:certificate result)))
  (is (= :not-admitted (:admission result)))
  (is (= :none (:authority result)))
  (is (= :none (:transaction-authority result)))
  (is (= :none (:live-settlement-authority result)))
  (is (false? (:prior-attempt-commit-attributed? result)))
  (is (false? (:later-record-absence-proves-noncommit? result)))
  (is (false? (:same-command-correspondence-authenticated? result)))
  (is (false? (:reported-later-progression-authenticated? result)))
  (is (false? (:business-rejection-established? result)))
  (is (false? (:safe-to-retry? result)))
  (is (false? (:safe-to-settle? result)))
  (is (= :forbidden-by-this-diagnostic (:automatic-retry result)))
  (is (= :caller-supplied-unattested (:later-reconciliation-origin result)))
  (is (= physical-open (:physical-refinement-obligations result)))
  (is (= :open (:physical-refinement-obligation-statuses result)))
  (is (not (re-find #"SECRET_V1399_NEVER_DISCLOSE|LOST_RESPONSE_"
                    (pr-str result))))
  (is (not (contains? result :later-resolution)))
  (is (not (contains? result :prior-observation))))

(defn- rejection [candidate]
  (try (unknown/classify-later-reconciliation candidate)
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected! [candidate]
  (let [r (rejection candidate)]
    (is (true? (:commit-unknown-reconciliation/rejection r)))
    (is (keyword? (:reason r)))
    (is (not (re-find #"SECRET_V1399_NEVER_DISCLOSE|LOST_RESPONSE_"
                      (pr-str r))))
    (is (not (contains? r :prior-observation)))
    (is (not (contains? r :later-resolution)))))

(deftest all-outcome-matrices-preserve-ambiguity
  (testing "Four physical states times four subsequent read results"
    (doseq [[physical earlier-status] status-by-physical
            [later-status report-status] status-by-later]
      (let [r (classify physical later-status)]
        (is (= earlier-status (:prior-uncertain-status r)))
        (is (= report-status (:status r)))
        (is (= physical (:prior-observed-physical-attempt-status r)))
        (is (= later-status (:later-reported-resolution-status r)))
        (is (= (= later-status :replay)
               (:later-command-incorporation-reported? r)))
        (is (= :not-reported (:later-reported-progression-status r)))
        (no-authority! r)))))

(deftest lost-response-even-with-later-durable-replay-is-unattributed
  (let [r (unknown/classify-later-reconciliation
            (request (observation :indeterminate :threw)
                     (later :replay) nil))]
    (is (= :execution-threw-outcome-unresolved (:prior-uncertain-status r)))
    (is (= :later-replay-reported-prior-attempt-unattributed (:status r)))
    (is (true? (:later-command-incorporation-reported? r)))
    (no-authority! r)))

(deftest crashed-before-response-has-no-implied-noncommit
  (testing "Synthetic unobserved and thrown envelopes are not OS crash tests"
    (doseq [physical [:indeterminate :unobserved]
            later-status [:unresolved :unavailable]]
      (let [r (classify physical later-status)]
        (is (= (status-by-later later-status) (:status r)))
        (no-authority! r)))))

(deftest later-missing-command-record-does-not-overrule-observed-commit
  (let [r (classify :committed :unresolved)]
    (is (= :physical-commit-observed-command-unresolved
           (:prior-uncertain-status r)))
    (is (= :later-command-record-not-visible-noncommit-unproved (:status r)))
    (no-authority! r)))

(deftest rejected-physical-attempt-is-not-business-rejection
  (doseq [later-status [:replay :unresolved :incompatible :unavailable]]
    (let [r (classify :rejected later-status)]
      (is (= :physical-assertion-rejected-command-unresolved
             (:prior-uncertain-status r)))
      (is (false? (:business-rejection-established? r)))
      (no-authority! r))))

(deftest later-record-replay-is-not-new-physical-attempt
  (let [r (classify :unobserved :replay)]
    (is (= :later-replay-reported-prior-attempt-unattributed (:status r)))
    (is (= :unobserved (:prior-observed-physical-attempt-status r)))
    (no-authority! r)))

(deftest no-disclosure-and-binding-incompatibility-are-distinct
  (doseq [later-status [:unavailable :incompatible]]
    (let [r (classify :indeterminate later-status)]
      (is (= (status-by-later later-status) (:status r)))
      (is (false? (:later-command-incorporation-reported? r)))
      (no-authority! r))))

(deftest progression-cannot-backdate-or-attribute-commit
  (doseq [basis [:earlier :current :far-future]
          later-status [:replay :unresolved]]
    (let [r (unknown/classify-later-reconciliation
              (request (observation :indeterminate)
                       (later later-status)
                       (progression/requirement basis)))]
      (is (= :reported-unattributed-and-unauthenticated
             (:later-reported-progression-status r)))
      (is (= (status-by-later later-status) (:status r)))
      (no-authority! r))))

(deftest nil-progression-is-different-from-fabricated-basis
  (let [a (classify :indeterminate :replay)
        b (unknown/classify-later-reconciliation
            (request (observation :indeterminate) (later :replay)
                     (progression/requirement :new-basis)))]
    (is (= :not-reported (:later-reported-progression-status a)))
    (is (= :reported-unattributed-and-unauthenticated
           (:later-reported-progression-status b)))
    (is (= (:status a) (:status b)))
    (no-authority! a)
    (no-authority! b)))

(deftest fake-durable-outcome-content-is-not-reflected
  (doseq [payload [{:private secret} {:result {:nested [secret]}}
                   {:private secret :status :apparent-success}]]
    (let [r (unknown/classify-later-reconciliation
              (request (observation :indeterminate)
                       (later :replay payload) nil))]
      (is (true? (:later-command-incorporation-reported? r)))
      (no-authority! r))))

(deftest copied-report-can-be-replayed-but-not-authenticated
  (let [r (request (observation :indeterminate) (later :replay) nil)
        copied (edn/read-string (pr-str (assoc-in r
                                   [:prior-observation :execution :result
                                    :controlled-result :resolution]
                                   {:status :unresolved :source secret})))
        first-report (unknown/classify-later-reconciliation r)
        copied-report (unknown/classify-later-reconciliation copied)]
    (is (= first-report copied-report))
    (is (false? (:same-command-correspondence-authenticated? copied-report)))
    (no-authority! first-report)
    (no-authority! copied-report)))

(deftest caller-supplied-scope-is-not-authoritative-provenance
  (let [r (classify :unobserved :replay)]
    (is (= :caller-supplied-unattested (:later-reconciliation-origin r)))
    (no-authority! r)))

(deftest stable-repeat-classification-never-retries-or-settles
  (let [r (request (observation :indeterminate) (later :replay) nil)
        before r
        results (repeatedly 3 #(unknown/classify-later-reconciliation r))]
    (is (apply = results))
    (is (= before r))
    (doseq [result results]
      (no-authority! result))))

(deftest rejects-open-or-metadata-bearing-envelope
  (let [good (request (observation :indeterminate) (later :replay) nil)]
    (doseq [bad [(dissoc good :later-progression)
                 (assoc good :unrecognized :value)
                 (with-meta good {:untrusted true})
                 (into (sorted-map) good)
                 [] nil 1 "not a map"
                 (assoc good :later-progression :unknown)
                 (assoc good :later-progression {:bases #{:invented}})]]
      (rejected! bad))))

(deftest rejects-malformed-later-reconciliation-results
  (let [good (request (observation :indeterminate) (later :replay) nil)]
    (doseq [bad-later [nil
                      {}
                      {:status :replay :scope resolution/assurance-scope}
                      {:status :replay :scope resolution/assurance-scope
                       :outcome {}}
                      {:status :replay :scope resolution/assurance-scope
                       :outcome nil}
                      {:status :replay :scope resolution/assurance-scope
                       :outcome []}
                      (assoc (later :replay) :forged true)
                      (with-meta (later :replay) {:not-authenticated true})
                      (assoc (later :unresolved) :outcome {:value secret})
                      (assoc (later :unavailable) :outcome {:value secret})
                      (assoc (later :incompatible) :outcome {:value secret})
                      (assoc (later :unresolved) :scope {})
                      (assoc (later :replay) :scope :fake)
                      (assoc (later :replay) :status :incorporated)]]
      (rejected! (assoc good :later-resolution bad-later)))))

(deftest rejects-unverified-prior-observation-and-authority-upgrades
  (let [good (request (observation :indeterminate) (later :replay) nil)]
    (doseq [bad [(assoc good :prior-observation {})
                 (assoc-in good [:prior-observation :diagnostic :certificate]
                           :issued)
                 (assoc-in good [:prior-observation :diagnostic :authority]
                           :retry-granted)
                 (assoc-in good [:prior-observation :diagnostic
                                 :all-physical-refinement-obligation-statuses]
                           :closed)
                 (assoc-in good [:prior-observation :execution :outcome]
                           :threw)
                 (assoc-in good [:prior-observation :execution :result
                                 :controlled-result :resolution :status]
                           :incorporated)
                 (assoc-in good [:prior-observation :diagnostic
                                 :executor-observation :physical-outcome]
                           :invented)
                 (assoc-in good [:prior-observation :diagnostic :type]
                           :fake)]]
      (rejected! bad))))

(deftest rejects-prior-resolution-that-is-already-settled
  (let [good (request (observation :committed) (later :replay) nil)]
    (doseq [status [:replay :incorporated :unavailable :incompatible]]
      (rejected! (assoc-in good
                           [:prior-observation :execution :result
                            :controlled-result :resolution :status]
                           status)))))

(deftest supplemental-caller-flags-do-not-grant-physical-authority
  (let [good (request (observation :indeterminate) (later :replay) nil)
        baseline (unknown/classify-later-reconciliation good)]
    ;; The upstream v1396 report validates its listed required fields but
    ;; deliberately ignores unrelated extra keys in a detached v1394 map.
    ;; This is not provenance authentication. Verify no authority promotion.
    (doseq [[k v] [[:safe-to-retry? true]
                   [:safe-to-settle? true]
                   [:physical-refinement-proved? true]]]
      (let [candidate (assoc-in good [:prior-observation :diagnostic k] v)
            result (unknown/classify-later-reconciliation candidate)]
        (is (= baseline result))
        (no-authority! result)))
    ;; In contrast, changing a REQUIRED upstream certification field fails.
    (rejected! (assoc-in good [:prior-observation :diagnostic :certificate]
                         :issued))))

(deftest substituted-upstream-classifier-refused-before-invocation
  (let [good (request (observation :indeterminate) (later :replay) nil)
        counter (atom 0)]
    (with-redefs [prior/classify-observation
                  (fn [& _] (swap! counter inc) {:status :committed})]
      (let [r (rejection good)]
        (is (= :substituted-reconciliation-checker (:reason r)))
        (is (zero? @counter))))))

(deftest substituted-command-reconcile-refused-before-invocation
  (let [good (request (observation :indeterminate) (later :replay) nil)
        counter (atom 0)]
    (with-redefs [resolution/reconcile
                  (fn [& _] (swap! counter inc) {:status :replay})]
      (let [r (rejection good)]
        (is (= :substituted-reconciliation-checker (:reason r)))
        (is (zero? @counter))))))
