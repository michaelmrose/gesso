(ns gesso.assurance.opaque-test
  "v1228: permanent adversarial regressions for opaque input/result contracts.

   The v1227 observer checks supplied maps against the source-owned, typed
   predicate interpreter. It NEVER executes a referenced opaque computation,
   proves business correctness, contains side effects, guarantees termination,
   admits a proof, or grants authoritative transaction permission. These tests
   protect that boundary along with exact bindings, fingerprint invalidation,
   effect/timeout caveats, and intentionally acceptable-but-wrong results."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.opaque :as opaque]))

(def ^:private input-id :input/id)
(def ^:private status :result/status)
(def ^:private score :result/score)

(def ^:private sample
  {:id :example/compute
   :implementation :opaque-clojure
   :implementation-boundary :example/calculate
   :input
   {:rule {:readers #{input-id}
           :expression [:> [:read input-id] [:literal 0]]}
    :reader-types {input-id {:type :integer}}}
   :output
   {:rule {:readers #{status score}
           :expression [:and
                        [:in [:read status] [:literal #{:new :active}]]
                        [:> [:read score] [:literal 7]]]}
    :reader-types {status {:type :enum :name :example/status
                           :values #{:new :active}}
                   score {:type :integer}}}
   :effects {:mode :unknown :external-effects #{}}
   :termination {:mode :unknown :timeout-ms nil}})

(def ^:private good-input {input-id 1})
(def ^:private good-output {status :active score 8N})

(defn- error-data [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- rejected! [thunk]
  (let [error (error-data thunk)]
    (is (map? error) "Malformed contracts/bindings must throw typed ex-info")
    (is (qualified-keyword? (:error/type error)) (pr-str error))
    (is (keyword? (:reason error)) (pr-str error))
    (is (vector? (:at error)) (pr-str error))
    error))

(defn- observation! [boundary contract bindings]
  (case boundary
    :input (opaque/validate-input! contract bindings)
    :output (opaque/validate-result! contract bindings)))

(defn- fingerprint [contract]
  (:fingerprint (opaque/describe-contract contract)))

(deftest declaration-remains-unadmitted-and-obligation-bearing
  (let [description (opaque/describe-contract sample)]
    (is (= opaque/description-type (:type description)))
    (is (= opaque/contract-version (:version description)))
    (is (= :syntax-and-types (:checking description)))
    (is (= :not-performed (:evaluation description)))
    (is (= :not-proved (:proof-status description)))
    (is (= :not-admitted (:admission description)))
    (is (= :not-established (:runtime-enforcement description)))
    (is (= :not-established (:effect-assurance description)))
    (is (= :not-established (:termination-assurance description)))
    (is (= :none (:authority description)))
    (is (string? (:fingerprint description)))
    (is (string? (:input-rule-fingerprint description)))
    (is (string? (:output-rule-fingerprint description)))
    (is (= #{:opaque/source-provenance
             :opaque/business-correctness
             :opaque/effects-and-isolation
             :opaque/termination-and-cancellation
             :opaque/authoritative-enforcement
             :opaque/runtime-correspondence}
           (:open-obligations description)))
    (doseq [forbidden [:certificate :checker-handle :proved? :valid?
                       :discharged? :commit-status :transaction-id]]
      (is (not (contains? description forbidden))))))

(deftest successful-input-and-result-are-only-interpreted-observations
  (let [description (opaque/describe-contract sample)]
    (doseq [[boundary bindings] [[:input good-input]
                                [:output good-output]]]
      (let [observed (observation! boundary sample bindings)]
        (is (= opaque/observation-type (:type observed)))
        (is (= opaque/contract-version (:version observed)))
        (is (= boundary (:boundary observed)))
        (is (= :example/compute (:computation observed)))
        (is (= :predicate-satisfied-for-supplied-bindings
               (:observation observed)))
        (is (= (:fingerprint description) (:contract-fingerprint observed)))
        (is (= (if (= boundary :input)
                 (:input-rule-fingerprint description)
                 (:output-rule-fingerprint description))
               (:rule-fingerprint observed)))
        (is (= (:open-obligations description) (:open-obligations observed)))
        (is (= :not-proved (:proof-status observed)))
        (is (= :not-admitted (:admission observed)))
        (is (= :not-established (:runtime-enforcement observed)))
        (is (= :not-established (:effect-assurance observed)))
        (is (= :not-established (:termination-assurance observed)))
        (is (= :none (:authority observed)))
        (doseq [forbidden [:certificate :proof :proved? :valid? :discharged?
                           :commit-status :transaction-id :authoritative?]]
          (is (not (contains? observed forbidden))))))))

(deftest correct-output-is-not-a-business-correctness-proof
  ;; Here the application knows the right business answer SHOULD be :new, but
  ;; the opaque callback returned :active. Both statuses satisfy the contract.
  ;; No logical inference from validity to application/business correctness.
  (let [contract-valid-but-wrong {status :active score 12}
        observed (opaque/validate-result! sample contract-valid-but-wrong)]
    (is (= :predicate-satisfied-for-supplied-bindings
           (:observation observed)))
    (is (contains? (:open-obligations observed) :opaque/business-correctness))
    (is (contains? (:open-obligations observed) :opaque/source-provenance))
    (is (= :not-proved (:proof-status observed)))
    (is (= :none (:authority observed)))))

(deftest invalid-result-threshold-is-rejected-at-and-below-seven
  (doseq [n [-999 -1 0 1 5 6 7]
          s [:new :active]]
    (testing (str "status " s ", score " n)
      (let [error (rejected! #(opaque/validate-result!
                              sample {status s score n}))]
        (is (= :gesso.assurance.opaque/invalid-opaque-contract
               (:error/type error)))
        (is (= :output-contract-rejected (:reason error)))
        (is (= [:output] (:at error)))))))

(deftest valid-result-range-and-exact-unbounded-integers
  (doseq [n [8 8N 9 10N 2147483648 9223372036854775808N
             999999999999999999999999999999999999999N]
          s [:new :active]]
    (is (= :predicate-satisfied-for-supplied-bindings
           (:observation (opaque/validate-result! sample
                                                  {status s score n}))))))

(deftest invalid-enum-members-and-wrong-scalar-types-rejected
  (doseq [value [:unknown :done nil "active" 7 false {} []]]
    (rejected! #(opaque/validate-result!
                 sample {status value score 8})))
  (doseq [value [nil "8" :eight false 8.5 8M 17/2 (fn [] 8) [] {}]]
    (rejected! #(opaque/validate-result!
                 sample {status :new score value}))))

(deftest exact-result-binding-footprint-is-required
  (doseq [value [{status :new}
                 {score 8}
                 {}
                 {status :new score 8 :result/other true}
                 {status :new score 8 input-id 7}
                 {:result/other 8}
                 nil [] #{} "not a map"]]
    (rejected! #(opaque/validate-result! sample value))))

(deftest exact-input-binding-footprint-and-predicate-required
  (doseq [value [{input-id 0}
                 {input-id -1}
                 {input-id nil}
                 {input-id 1.5}
                 {input-id "1"}
                 {input-id :one}
                 {input-id 1 :input/extra true}
                 {}
                 nil [] #{}]]
    (rejected! #(opaque/validate-input! sample value)))
  (doseq [n [1 2N 10000000000000000N]]
    (is (= :predicate-satisfied-for-supplied-bindings
           (:observation (opaque/validate-input! sample {input-id n}))))))

(deftest input-rejection-and-output-rejection-are-distinct
  (let [input-error (rejected! #(opaque/validate-input!
                                sample {input-id 0}))
        output-error (rejected! #(opaque/validate-result!
                                 sample {status :active score 7}))]
    (is (= :input-contract-rejected (:reason input-error)))
    (is (= [:input] (:at input-error)))
    (is (= :output-contract-rejected (:reason output-error)))
    (is (= [:output] (:at output-error)))))

(deftest no-opaque-computation-is-ever-invoked
  (let [calls (atom 0)
        opaque-function (fn [_] (swap! calls inc) {status :active score 8})]
    (is (:fingerprint (opaque/describe-contract sample)))
    (is (= :predicate-satisfied-for-supplied-bindings
           (:observation (opaque/validate-result! sample good-output))))
    (is (= 0 @calls))
    ;; A callback object cannot masquerade as a stable symbolic implementation.
    (rejected! #(opaque/describe-contract
                 (assoc sample :implementation-boundary opaque-function)))
    (is (= 0 @calls))
    (rejected! #(opaque/describe-contract
                 (assoc-in sample [:output :rule :expression]
                           [:call opaque-function])))
    (is (= 0 @calls))))

(deftest malformed-description-and-forged-authority-keys-rejected
  (doseq [[field forged] [[:valid? true]
                          [:proved? true]
                          [:proof-status :proved]
                          [:admission :admitted]
                          [:authority :authoritative]
                          [:runtime-enforcement :established]
                          [:certificate {:claimed true}]
                          [:checker-handle :forged]
                          [:discharged? true]]]
    (let [error (rejected! #(opaque/describe-contract
                            (assoc sample field forged)))]
      (is (= :wrong-keys (:reason error)))))
  (doseq [contract [(assoc-in sample [:input :certificate] :forged)
                    (assoc-in sample [:output :valid?] true)
                    (assoc-in sample [:output :rule :proof] :forged)
                    (assoc-in sample [:output :reader-types score :proved?] true)]]
    (rejected! #(opaque/describe-contract contract))))

(deftest invalid-implementation-identities-rejected
  (doseq [contract [(assoc sample :id nil)
                    (assoc sample :id :unqualified)
                    (assoc sample :implementation :untrusted-eval)
                    (assoc sample :implementation-boundary "example/calculate")
                    (assoc sample :implementation-boundary :not-qualified)
                    (dissoc sample :input)
                    (dissoc sample :output)
                    (assoc sample :unknown :field)]]
    (rejected! #(opaque/describe-contract contract))))

(deftest description-fingerprint-stable-and-semantically-sensitive
  (let [root (fingerprint sample)]
    (is (= root (fingerprint sample)))
    (is (= root (fingerprint
                 (-> sample
                     (assoc-in [:output :rule :expression 2 2 1] 7N)
                     (assoc-in [:input :rule :expression 2 1] 0N)))))
    (doseq [changed [(assoc sample :id :other/compute)
                     (assoc sample :implementation :external-service)
                     (assoc sample :implementation-boundary :example/other)
                     (assoc-in sample [:input :rule :expression 2 1] -1)
                     (assoc-in sample [:output :rule :expression 2 2 1] 8)
                     (assoc-in sample [:output :reader-types status :name] :other/status)
                     (assoc-in sample [:output :reader-types status :values]
                               #{:new :active :paused})
                     (assoc sample :effects {:mode :assumed-pure
                                             :external-effects #{}})
                     (assoc sample :termination {:mode :requested-timeout
                                                 :timeout-ms 250})]]
      (is (not= root (fingerprint changed)) (pr-str changed)))))

(deftest mutation-of-result-contract-changes-acceptance
  (let [stricter (assoc-in sample [:output :rule :expression 2 2 1] 9)]
    (is (:fingerprint (opaque/describe-contract stricter)))
    (is (= :predicate-satisfied-for-supplied-bindings
           (:observation (opaque/validate-result! sample good-output))))
    (is (= :output-contract-rejected
           (:reason (rejected! #(opaque/validate-result!
                                 stricter good-output)))))))

(deftest validation-rechecks-declarations-not-cached-reports
  (let [description (opaque/describe-contract sample)
        observed (opaque/validate-result! sample good-output)]
    (rejected! #(opaque/validate-result!
                 (assoc sample :prior-report description) good-output))
    (rejected! #(opaque/validate-result!
                 (assoc-in sample [:output :prior-report] observed)
                 good-output))
    (rejected! #(opaque/validate-result!
                 (assoc-in sample [:output :rule :valid?] true) good-output))
    (rejected! #(opaque/validate-result!
                 (assoc-in sample [:output :reader-types score :proof]
                           :forged) good-output))
    ;; The observed value is copyable; its apparent status has no admission API.
    (is (= :proved (:proof-status (assoc observed :proof-status :proved))))
    (is (= :none (:authority (opaque/validate-result! sample good-output))))))

(deftest contract-effect-modes-remain-explicit-assumptions
  (let [unknown (opaque/describe-contract sample)
        assumed (opaque/describe-contract
                 (assoc sample :effects {:mode :assumed-pure
                                         :external-effects #{}}))
        external (opaque/describe-contract
                  (assoc sample :effects {:mode :declared-external
                                          :external-effects #{:http/post :email/send}}))]
    (doseq [description [unknown assumed external]]
      (is (contains? (:open-obligations description) :opaque/effects-and-isolation))
      (is (= :not-established (:effect-assurance description)))
      (is (= :not-admitted (:admission description))))
    (is (contains? (:open-obligations external) :opaque/external-effect-completeness))
    (is (not (contains? (:open-obligations assumed)
                        :opaque/external-effect-completeness)))
    (is (not= (:fingerprint unknown) (:fingerprint assumed)))
    (is (not= (:fingerprint unknown) (:fingerprint external)))))

(deftest invalid-effect-declarations-fail-closed
  (doseq [effects [{:mode :unknown :external-effects #{:http/post}}
                   {:mode :assumed-pure :external-effects #{:http/post}}
                   {:mode :declared-external :external-effects #{}}
                   {:mode :declared-external :external-effects #{:send}}
                   {:mode :declared-external :external-effects #{:http/post "email/send"}}
                   {:mode :assured-pure :external-effects #{}}
                   {:mode :unknown :external-effects nil}
                   {:mode :unknown :external-effects []}
                   {:mode :unknown :external-effects #{} :proved? true}]]
    (rejected! #(opaque/describe-contract (assoc sample :effects effects))))
  (rejected! #(opaque/describe-contract
               (assoc sample :effects
                      {:mode :declared-external
                       :external-effects (set (map (fn [n] (keyword "effects" (str n)))
                                                   (range 65)))}))))

(deftest timeout-modes-are-assertions-not-termination-or-cancellation-proof
  (doseq [termination [{:mode :unknown :timeout-ms nil}
                       {:mode :assumed-terminates :timeout-ms nil}
                       {:mode :requested-timeout :timeout-ms 1}
                       {:mode :requested-timeout :timeout-ms 1500}
                       {:mode :requested-timeout :timeout-ms 86400000}]]
    (let [contract (assoc sample :termination termination)
          description (opaque/describe-contract contract)
          observation (opaque/validate-result! contract good-output)]
      (is (= :not-established (:termination-assurance description)))
      (is (= :not-established (:termination-assurance observation)))
      (is (contains? (:open-obligations observation)
                     :opaque/termination-and-cancellation))
      (is (= :not-proved (:proof-status observation)))
      (is (= (if (= :requested-timeout (:mode termination)) true false)
             (contains? (:open-obligations description)
                        :opaque/timeout-enforcement))))))

(deftest invalid-timeout-declarations-fail-closed
  (doseq [termination [{:mode :requested-timeout :timeout-ms nil}
                       {:mode :requested-timeout :timeout-ms 0}
                       {:mode :requested-timeout :timeout-ms -1}
                       {:mode :requested-timeout :timeout-ms 86400001}
                       {:mode :requested-timeout :timeout-ms 1.5}
                       {:mode :unknown :timeout-ms 1}
                       {:mode :assumed-terminates :timeout-ms 1}
                       {:mode :unknown :timeout-ms nil :proof true}
                       {:mode :does-not-exist :timeout-ms nil}]]
    (rejected! #(opaque/describe-contract
                 (assoc sample :termination termination)))))

(deftest binding-metadata-and-unsupported-collection-types-rejected
  (rejected! #(opaque/validate-result!
               sample (with-meta good-output {:proved? true})))
  (rejected! #(opaque/validate-input!
               sample (with-meta good-input {:proved? true})))
  (rejected! #(opaque/describe-contract
               (with-meta sample {:admitted true})))
  (rejected! #(opaque/describe-contract
               (assoc-in sample [:effects :external-effects]
                         (with-meta #{} {:trusted true}))))
  (rejected! #(opaque/validate-result!
               sample (sorted-map status :active score 8))))

(deftest runtime-observations-require-supplied-values-not-source-provenance
  (let [result (opaque/validate-result! sample good-output)]
    (is (contains? (:open-obligations result) :opaque/source-provenance))
    (is (contains? (:open-obligations result) :opaque/authoritative-enforcement))
    (is (contains? (:open-obligations result) :opaque/runtime-correspondence))
    (is (= :none (:authority result)))
    ;; Even an identical output map from a completely different process does
    ;; not imply anything about that process or about transactional success.
    (is (= result (opaque/validate-result! sample (into {} good-output))))))

(deftest deterministic-mutation-matrix-around-score-boundary
  (doseq [threshold (range 0 40)
          n (range 0 40)]
    (let [contract (assoc-in sample [:output :rule :expression 2 2 1]
                             threshold)
          bindings {status :new score n}]
      (if (> n threshold)
        (is (= :predicate-satisfied-for-supplied-bindings
               (:observation (opaque/validate-result! contract bindings))))
        (is (= :output-contract-rejected
               (:reason (error-data #(opaque/validate-result! contract bindings)))))))))

(deftest opaque-result-is-inert-not-a-transaction-or-side-effect-controller
  (let [observed (opaque/validate-result! sample good-output)]
    (is (= :none (:authority observed)))
    (is (= :not-established (:runtime-enforcement observed)))
    (is (= :not-established (:effect-assurance observed)))
    (is (= :not-established (:termination-assurance observed)))
    (is (nil? (:transaction-id observed)))
    (is (nil? (:commit-status observed)))
    (is (nil? (:side-effects-performed observed)))))
