(ns gesso.assurance.proof.opaque-test
  "Permanent v1248 adversarial tests for the v1247 opaque-result bridge.

   No opaque Clojure callback is executed by the bridge. The result is a
   SUPPLIED value whose nominal type, predicate and abstract transition are
   checked afresh. An acceptable but WRONG business answer is deliberately
   accepted as a local observation, not proof of functional correctness.

   The v1235 checker establishes conditional guard/assignment/frame facts;
   it does not yet establish output-to-postcondition transfer, an inductive
   invariant, runtime effect isolation, or an XTDB commit. These tests keep
   those missing guarantees visible, including for a leap-year-like oracle."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.opaque :as opaque]))

(def ^:private status-type
  {:type :enum :name :example/classification :values #{:leap :ordinary}})

(def ^:private input-rule
  {:readers #{:input/year}
   :expression [:> [:read :input/year] [:literal 0]]})

(def ^:private output-rule
  {:readers #{:result/status :result/score}
   :expression [:and
                [:in [:read :result/status] [:literal #{:leap :ordinary}]]
                [:> [:read :result/score] [:literal 7]]]})

(def ^:private opaque-contract
  {:id :example/classify-year
   :implementation :opaque-clojure
   :implementation-boundary :example/year-classifier
   :input {:rule input-rule
           :reader-types {:input/year {:type :integer}}}
   :output {:rule output-rule
            :reader-types {:result/status status-type
                           :result/score {:type :integer}}}
   :effects {:mode :unknown :external-effects #{}}
   :termination {:mode :unknown :timeout-ms nil}})

(def ^:private construction
  {:subject {:kind :operation :id :example/classify-year
             :interface {:operation :example/classify-year}}
   :state-types {:entity/status status-type
                 :entity/score {:type :integer}
                 :entity/optional {:type :integer :optional? true :nullable? true}}
   :input-types {:result/status status-type
                 :result/score {:type :integer}}
   :guard output-rule
   :reads #{:result/status :result/score}
   :writes #{:entity/status :entity/score}
   :replacements {:entity/status [:read :result/status]
                  :entity/score [:read :result/score]}
   :frame #{:entity/optional}})

(def ^:private source
  {:opaque-contract opaque-contract
   :construction construction
   :result-writes {:entity/status :result/status
                   :entity/score :result/score}})

(def ^:private witness
  {:opaque-input {:input/year 1900}
   :result {:result/status :leap :result/score 8}
   :before {:entity/status :ordinary :entity/score 19}
   :after {:entity/status :leap :entity/score 8}})

(def ^:private opaque-error :gesso.assurance.proof.opaque/invalid-opaque-proof)
(def ^:private contract-error :gesso.assurance.opaque/invalid-opaque-contract)
(def ^:private evaluation-error :gesso.assurance.rule-eval/invalid-rule-evaluation)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)

(defn- exception-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject-as! [f error-type reason]
  (let [data (exception-data f)]
    (is (map? data) (str "Expected exception " error-type " / " reason))
    (is (= error-type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (is (vector? (:at data)) (pr-str data))
    data))

(defn- reject-source! [s reason]
  (reject-as! #(opaque/describe-opaque-transition! s) opaque-error reason))

(defn- reject-witness! [w reason]
  (reject-as! #(opaque/check-opaque-witness! source w) opaque-error reason))

(defn- assert-open-obligations! [report]
  (is (set? (:open-obligations report)))
  (is (every? (:open-obligations report)
              #{:opaque/source-provenance
                :opaque/business-correctness
                :opaque/effects-and-isolation
                :opaque/termination-and-cancellation
                :opaque/authoritative-enforcement
                :opaque/runtime-correspondence
                :opaque/declared-output-contract-completeness
                :opaque/output-to-postcondition-transfer
                :opaque/actual-result-provenance
                :opaque/production-guard-enforcement
                :opaque/actual-effect-containment
                :opaque/physical-transaction-correspondence
                :production-transition-correspondence
                :current-runtime-authorization
                :atomic-transaction-enforcement
                :certificate-admission}))
  (is (= #{relation-premise} (:open-premises report)))
  (is (= :not-proved (:invariant-preservation report)))
  (is (= :not-proved (:opaque-business-correctness report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :not-established (:implementation-correspondence report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :proved?))))

(defn- expected-after [w]
  (-> (:before w)
      (assoc :entity/status (get-in w [:result :result/status]))
      (assoc :entity/score (get-in w [:result :result/score]))))

(defn- with-result [w status score]
  (let [w2 (assoc w :result {:result/status status :result/score score})]
    (assoc w2 :after (expected-after w2))))

(defn- is-leap-year? [year]
  ;; Deliberately independent BUSINESS oracle; not available to Gesso's
  ;; output contract, and not asserted by the opaque proof bridge.
  (and (zero? (mod year 4))
       (or (not (zero? (mod year 100)))
           (zero? (mod year 400)))))

(deftest source-describes-exact-original-checked-facts
  (let [description (opaque/describe-opaque-transition! source)
        facts (:checked-abstract-facts description)]
    (is (= opaque/description-type (:type description)))
    (is (= opaque/opaque-proof-version (:version description)))
    (is (= source (:source description)))
    (is (= 4 (count facts)))
    (is (= #{[:guard]
             [:eq [:post :entity/status] [:input :result/status]]
             [:eq [:post :entity/score] [:input :result/score]]
             [:same-field :entity/optional]}
           (set (map :conclusion facts))))
    (is (= #{:guard :assignment :frame} (set (map :rule facts))))
    (is (every? #(= :matching-abstract-transition-only (:scope %)) facts))
    (is (every? #(string? (:derivation-fingerprint %)) facts))
    (is (= :independently-checked-conditional-abstract-facts
           (:checking description)))
    (is (= :exact-transition-guard (:opaque-output-semantics description)))
    (is (= :not-proved (:postcondition-transfer description)))
    (is (= :requires-fresh-supplied-witness-check (:result-admission description)))
    (assert-open-obligations! description)))

(deftest successful-witness-has-no-proof-or-transaction-authority
  (let [report (opaque/check-opaque-witness! source witness)
        description (opaque/describe-opaque-transition! source)]
    (is (= opaque/observation-type (:type report)))
    (is (= :opaque-predicates-and-abstract-relation-matched (:observation report)))
    (is (= :passed-for-supplied-witness (:guard-gate report)))
    (is (= :interpreted-witness-only (:proof-status report)))
    (is (= (:fingerprint description) (:construction-fingerprint report)))
    (is (= (:opaque-fingerprint description) (:opaque-fingerprint report)))
    (is (= (:transition-fingerprint description) (:transition-fingerprint report)))
    (is (= (:subject-fingerprint description) (:subject-fingerprint report)))
    (assert-open-obligations! report)))

(deftest acceptable-but-business-wrong-leap-year-classification
  ;; 1900 was NOT a leap year; a well-shaped :leap answer is not correct.
  (is (false? (is-leap-year? 1900)))
  (is (true? (is-leap-year? 2000)))
  (is (true? (is-leap-year? 2024)))
  (is (false? (is-leap-year? 2025)))
  (doseq [[year status] [[1900 :leap] [2000 :ordinary]
                         [2024 :ordinary] [2025 :leap]]]
    (let [w (-> (with-result witness status 8)
                (assoc :opaque-input {:input/year year}))
          observed (opaque/check-opaque-witness! source w)]
      (is (not= (if (is-leap-year? year) :leap :ordinary) status))
      (is (= :opaque-predicates-and-abstract-relation-matched
             (:observation observed)))
      (is (= :not-proved (:opaque-business-correctness observed)))
      (is (= :not-admitted (:admission observed)))
      (is (= :none (:authority observed))))))

(deftest invalid-leap-year-like-output-is-rejected-before-abstract-witness
  (doseq [score [7 0 -1 -100N]]
    (let [w (with-result witness :leap score)]
      (reject-as! #(opaque/check-opaque-witness! source w)
                  contract-error :output-contract-rejected)))
  (doseq [status [:unknown :error :pending]]
    (let [w (with-result witness status 8)]
      (reject-as! #(opaque/check-opaque-witness! source w)
                  evaluation-error :enum-value-outside-domain))))

(deftest smallest-valid-score-and-large-integers
  (doseq [score [8 9 10N 2147483648N 9223372036854775808N]]
    (let [w (with-result witness :ordinary score)
          report (opaque/check-opaque-witness! source w)]
      (is (= :opaque-predicates-and-abstract-relation-matched (:observation report)))
      (is (= :none (:authority report))))))

(deftest optional-absent-nil-and-value-frames-are-exact
  (doseq [optional [::absent nil 0 19 2147483648N]]
    (let [before (if (= ::absent optional)
                   (:before witness)
                   (assoc (:before witness) :entity/optional optional))
          w (assoc witness :before before :after
                   (merge before {:entity/status :leap :entity/score 8}))
          result (opaque/check-opaque-witness! source w)]
      (is (= :opaque-predicates-and-abstract-relation-matched
             (:observation result)))
      (is (= :none (:authority result))))))

(deftest cannot-change-a-framed-value
  (doseq [after [{:entity/status :leap :entity/score 8 :entity/optional 1}
                 {:entity/status :leap :entity/score 8 :entity/optional nil}]]
    (reject-witness! (assoc witness :after after)
                     :nonmatching-abstract-transition)))

(deftest cannot-remove-present-optional-field
  (let [before (assoc (:before witness) :entity/optional 5)]
    (reject-witness! (assoc witness :before before)
                     :nonmatching-abstract-transition)))

(deftest cannot-change-optional-nil-to-absence
  (let [before (assoc (:before witness) :entity/optional nil)]
    (reject-witness! (assoc witness :before before)
                     :nonmatching-abstract-transition)))

(deftest cannot-change-absent-optional-field-to-nil
  (reject-witness! (assoc-in witness [:after :entity/optional] nil)
                   :nonmatching-abstract-transition))

(deftest cannot-substitute-a-valid-but-different-post-state
  (doseq [after [{:entity/status :ordinary :entity/score 8}
                 {:entity/status :leap :entity/score 9}
                 {:entity/status :ordinary :entity/score 9}]]
    (reject-witness! (assoc witness :after after)
                     :nonmatching-abstract-transition)))

(deftest result-predicate-is-not-a-precondition-on-the-old-state
  (let [w (assoc witness :before {:entity/status :ordinary :entity/score 0})
        report (opaque/check-opaque-witness! source w)]
    (is (= :opaque-predicates-and-abstract-relation-matched
           (:observation report)))
    (is (= :not-proved (:invariant-preservation report)))))

(deftest invalid-input-does-not-pass-gate
  (doseq [year [0 -1 -1900]]
    (reject-as! #(opaque/check-opaque-witness!
                  source (assoc witness :opaque-input {:input/year year}))
                contract-error :input-contract-rejected)))

(deftest wrong-witness-envelope-never-gains-evidence
  (doseq [field [:valid? :proved? :certificate :admission :authority
                 :runtime-enforcement :commit? :implementation-correspondence]]
    (reject-witness! (assoc witness field true) :wrong-keys))
  (reject-witness! (dissoc witness :after) :wrong-keys)
  (reject-witness! (dissoc witness :result) :wrong-keys)
  (reject-as! #(opaque/check-opaque-witness! source [])
              opaque-error :expected-plain-map))

(deftest wrong-source-envelope-never-gains-evidence
  (doseq [field [:proof :valid? :proved? :certificate :admission
                 :authority :implementation-correspondence :verified?]]
    (reject-source! (assoc source field true) :wrong-keys))
  (reject-source! (dissoc source :opaque-contract) :wrong-keys)
  (reject-source! (dissoc source :result-writes) :wrong-keys)
  (reject-source! (into (sorted-map) source) :expected-plain-map))

(deftest metadata-on-source-and-witness-rejected
  (reject-source! (with-meta source {:proved? true}) :metadata-not-allowed)
  (reject-witness! (with-meta witness {:authority :transaction})
                   :metadata-not-allowed))

(deftest malformed-result-bindings-fail-with-specific-typed-reasons
  (doseq [[result reason]
          [[{:result/status :leap :result/score "8"} :wrong-binding-type]
           [{:result/status 10 :result/score 8} :wrong-binding-type]
           [{:result/status :leap} :missing-required-bindings]
           [{:result/status :leap :result/score 8 :result/extra true}
            :undeclared-runtime-readers]]]
    (reject-as! #(opaque/check-opaque-witness! source
                    (assoc witness :result result))
                evaluation-error reason))
  (reject-as! #(opaque/check-opaque-witness! source
                (assoc witness :opaque-input {:input/year "1900"}))
              evaluation-error :wrong-binding-type))

(deftest cannot-introduce-executable-payload-as-boundary
  (let [called (atom 0)
        code (fn [& _] (swap! called inc) {:result/status :leap
                                           :result/score 8})]
    (reject-as! #(opaque/describe-opaque-transition!
                  (assoc-in source [:opaque-contract :implementation-boundary] code))
                contract-error :invalid-implementation-boundary)
    (is (= 0 @called))))

(deftest opaque-computation-is-never-called
  (let [called (atom 0)
        code (fn [& _] (swap! called inc) {:result/status :leap
                                           :result/score 8})]
    (is (= opaque/description-type
           (:type (opaque/describe-opaque-transition! source))))
    (is (= opaque/observation-type
           (:type (opaque/check-opaque-witness! source witness))))
    (is (= 0 @called))
    (reject-as! #(opaque/describe-opaque-transition!
                  (assoc-in source [:opaque-contract :implementation-boundary] code))
                contract-error :invalid-implementation-boundary)
    (is (= 0 @called))))

(deftest mismatched-operation-identity-is-rejected
  (reject-source!
   (assoc-in source [:construction :subject :id] :example/other-operation)
   :operation-subject-mismatch)
  (reject-source!
   (assoc-in source [:opaque-contract :id] :example/other-operation)
   :operation-subject-mismatch))

(deftest guard-cannot-be-weakened-relative-to-output-contract
  (let [weaker {:readers #{:result/status}
                :expression [:in [:read :result/status]
                             [:literal #{:leap :ordinary}]]}
        ;; :result/score remains read by its assignment, so the overall
        ;; declared footprint does not change when the GUARD is weakened.
        s (assoc-in source [:construction :guard] weaker)]
    (reject-source! s :opaque-output-guard-mismatch)))

(deftest assignment-cannot-be-unrelated-constant
  (reject-source!
   (assoc-in source [:construction :replacements :entity/status]
             [:literal :ordinary])
   :result-assignment-mismatch))

(deftest assignment-cannot-swap-result-readers
  (let [s (assoc-in source [:construction :replacements :entity/score]
                    [:read :result/status])]
    (reject-as! #(opaque/describe-opaque-transition! s)
                :gesso.assurance.rule-type/invalid-rule-type
                :incompatible-equality-types)))

(deftest output-readers-must-all-be-accounted-for
  (reject-source!
   (assoc source :result-writes {:entity/status :result/status
                                 :entity/score :result/status})
   :result-reader-coverage-mismatch)
  (reject-source!
   (assoc source :result-writes {:entity/status :result/status})
   :result-write-footprint-mismatch))

(deftest no-target-outside-declared-writes
  (reject-source!
   (assoc source :result-writes {:entity/status :result/status
                                 :entity/optional :result/score})
   :result-write-footprint-mismatch))

(deftest missing-frame-declaration-is-rejected
  (reject-as! #(opaque/describe-opaque-transition!
                (assoc-in source [:construction :frame] #{}))
              :gesso.assurance.transition/invalid-construction
              :frame-footprint-mismatch))

(deftest missing-output-reader-is-rejected
  (let [s (assoc-in source [:result-writes :entity/score] :result/absent)]
    (reject-source! s :result-reader-coverage-mismatch)))

(deftest coherent-policy-change-invalidates-identity
  (let [rule {:readers #{:result/status :result/score}
              :expression [:and
                           [:in [:read :result/status] [:literal #{:leap :ordinary}]]
                           [:> [:read :result/score] [:literal 8]]]}
        updated (-> source
                    (assoc-in [:opaque-contract :output :rule] rule)
                    (assoc-in [:construction :guard] rule))
        old (opaque/describe-opaque-transition! source)
        fresh (opaque/describe-opaque-transition! updated)]
    (is (not= (:fingerprint old) (:fingerprint fresh)))
    (is (not= (:opaque-fingerprint old) (:opaque-fingerprint fresh)))
    (is (not= (:transition-fingerprint old) (:transition-fingerprint fresh)))
    (reject-as! #(opaque/check-opaque-witness! updated witness)
                contract-error :output-contract-rejected)))

(deftest one-sided-policy-mutation-rejected
  (let [rule {:readers #{:result/status :result/score}
              :expression [:and
                           [:in [:read :result/status] [:literal #{:leap :ordinary}]]
                           [:> [:read :result/score] [:literal 8]]]}
        updated (assoc-in source [:opaque-contract :output :rule] rule)]
    (reject-source! updated :opaque-output-guard-mismatch)))

(deftest changing-implementation-reference-changes-identity-not-proof-status
  (let [updated (assoc-in source [:opaque-contract :implementation-boundary]
                          :example/another-classifier)
        original (opaque/describe-opaque-transition! source)
        new (opaque/describe-opaque-transition! updated)]
    (is (not= (:fingerprint original) (:fingerprint new)))
    (is (not= (:opaque-fingerprint original) (:opaque-fingerprint new)))
    (is (= (:transition-fingerprint original)
           (:transition-fingerprint new)))
    (assert-open-obligations! new)))

(deftest effect-and-timeout-declarations-do-not-discharge-effects
  (let [updated (-> source
                    (assoc-in [:opaque-contract :effects]
                              {:mode :declared-external
                               :external-effects #{:service/audit-log}})
                    (assoc-in [:opaque-contract :termination]
                              {:mode :requested-timeout :timeout-ms 1000}))
        description (opaque/describe-opaque-transition! updated)]
    (is (= :not-established (:effect-assurance description)))
    (is (= :not-established (:termination-assurance description)))
    (is (contains? (:open-obligations description)
                   :opaque/external-effect-completeness))
    (is (contains? (:open-obligations description)
                   :opaque/timeout-enforcement))
    (assert-open-obligations! description)))

(deftest forged-checker-output-is-not-trusted
  (with-redefs [derivation/check-transition-derivation!
                (fn [_] {:valid? true :proved? true :authority :transaction})]
    (reject-source! source :derivation-checker-contract-changed)))

(deftest fabricated-but-plausible-checker-result-is-not-trusted
  (let [checked (derivation/check-transition-derivation!
                 {:transition
                  {:subject (:subject construction)
                   :state-types (:state-types construction)
                   :input-types (:input-types construction)
                   :guard (:guard construction)
                   :reads (:reads construction)
                   :writes (:writes construction)
                   :assignments (:replacements construction)}
                  :steps [{:id :test/proof :rule :guard :conclusion [:guard]}]
                  :goal [:guard]})]
    (with-redefs [derivation/check-transition-derivation!
                  (fn [_] (assoc checked :authority :transaction))]
      (reject-source! source :derivation-checker-contract-changed))))

(deftest compact-facts-are-not-reusable-certificates
  (let [description (opaque/describe-opaque-transition! source)]
    (is (every? #(= #{:rule :field :conclusion
                      :derivation-fingerprint :scope}
                    (set (keys %)))
                (:checked-abstract-facts description)))
    (is (every? #(not (contains? % :checked-derivation))
                (:checked-abstract-facts description)))
    (is (= :not-issued (:certificate description)))
    (is (= :not-admitted (:admission description)))
    (is (= :none (:authority description)))))

(deftest repeated-checks-recompute-from-original-source
  (let [first-description (opaque/describe-opaque-transition! source)
        second-description (opaque/describe-opaque-transition! source)
        first-witness (opaque/check-opaque-witness! source witness)
        second-witness (opaque/check-opaque-witness! source witness)]
    (is (= first-description second-description))
    (is (= first-witness second-witness))
    (is (= (:fingerprint first-description)
           (:construction-fingerprint first-witness)))))

(deftest reordered-maps-keep-the-same-semantic-identity
  (let [reordered (into {} (reverse (seq source)))
        a (opaque/describe-opaque-transition! source)
        b (opaque/describe-opaque-transition! reordered)]
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:checked-abstract-facts a) (:checked-abstract-facts b)))))

(deftest invalid-result-matrix-never-produces-observation
  (doseq [score [7 6 0 -200]]
    (let [w (with-result witness :ordinary score)]
      (reject-as! #(opaque/check-opaque-witness! source w)
                  contract-error :output-contract-rejected))))

(deftest no-unsound-postcondition-transfer-from-assignment-equalities
  (let [description (opaque/describe-opaque-transition! source)
        report (opaque/check-opaque-witness! source witness)]
    (is (= :not-proved (:postcondition-transfer description)))
    (is (contains? (:open-obligations description)
                   :opaque/output-to-postcondition-transfer))
    (is (not (some #(= :invariant-preservation (:rule %))
                   (:checked-abstract-facts description))))
    (is (= :not-proved (:invariant-preservation description)))
    (is (= :not-proved (:invariant-preservation report)))
    (is (= :none (:authority report)))))
