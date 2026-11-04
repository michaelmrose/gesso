(ns gesso.assurance.proof.assignment-preservation-test
  "Permanent adversarial tests for v1261 conditional literal-assignment
   preservation. A true literal postcondition is not the same thing as an
   initial-state proof, an admitted certificate, an XTDB refinement proof, or
   evidence that arbitrary Clojure functions obey their declarations.

   Test both positive inferences and carefully distinguished open outcomes;
   require typed rejection reasons for invalid sources and forged checker
   results. Keep independent checking of each required field visible."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.assignment-preservation :as preservation]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.transition :as construction]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types {:request/status {:type :enum :name :request/status
                                  :values #{:open :claimed :done}}
                 :request/owner {:type :keyword :nullable? true}
                 :request/version {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})

(def ^:private claim-invariants
  {:request/status-in-domain
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/status-claimed
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :claimed]]}
   :request/status-open
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :open]]}
   :request/status-and-version
   {:readers #{:request/status :request/version}
    :expression [:and
                 [:= [:read :request/status] [:literal :claimed]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/owner-non-nil
   {:readers #{:request/owner}
    :expression [:not= [:read :request/owner] [:literal nil]]}
   :request/version-nonnegative
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/note-present
   {:readers #{:request/note}
    :expression [:present [:read :request/note]]}
   :request/constant-true
   {:readers #{} :expression [:literal true]}
   :request/constant-false
   {:readers #{} :expression [:literal false]}})

(def ^:private claim-source
  {:construction claim
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants claim-invariants})

(def ^:private score-construction
  {:subject {:kind :operation :id :example/score
             :interface {:operation :example/score}}
   :state-types {:request/score {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {}
   :guard {:readers #{} :expression [:literal true]}
   :reads #{}
   :writes #{:request/score}
   :replacements {:request/score [:literal 9]}
   :frame #{:request/note}})

(def ^:private score-source
  {:construction score-construction
   :initial-condition {:readers #{} :expression [:literal true]}
   :invariants
   {:request/score-gt-seven
    {:readers #{:request/score}
     :expression [:> [:read :request/score] [:literal 7]]}
    :request/score-gt-nine
    {:readers #{:request/score}
     :expression [:> [:read :request/score] [:literal 9]]}
    :request/score-is-nine
    {:readers #{:request/score}
     :expression [:= [:read :request/score] [:literal 9]]}
    :request/note-present
    {:readers #{:request/note}
     :expression [:present [:read :request/note]]}}})

(def ^:private nullable-construction
  (-> score-construction
      (assoc-in [:subject :id] :example/nil-note)
      (assoc-in [:subject :interface :operation] :example/nil-note)
      (assoc :writes #{:request/note}
             :replacements {:request/note [:literal nil]}
             :frame #{:request/score})))

(def ^:private nullable-source
  {:construction nullable-construction
   :initial-condition {:readers #{} :expression [:literal true]}
   :invariants
   {:request/note-present
    {:readers #{:request/note}
     :expression [:present [:read :request/note]]}
    :request/note-nil
    {:readers #{:request/note}
     :expression [:and [:present [:read :request/note]]
                  [:= [:read :request/note] [:literal nil]]]}
    :request/note-non-nil
    {:readers #{:request/note}
     :expression [:and [:present [:read :request/note]]
                  [:not= [:read :request/note] [:literal nil]]]}
    :request/score-positive
    {:readers #{:request/score}
     :expression [:> [:read :request/score] [:literal 0]]}}})

(def ^:private two-write-source
  {:construction
   {:subject {:kind :operation :id :example/complete
              :interface {:operation :example/complete}}
    :state-types {:request/status {:type :enum :name :request/status
                                   :values #{:open :claimed :done}}
                  :request/score {:type :integer}
                  :request/version {:type :integer}}
    :input-types {}
    :guard {:readers #{} :expression [:literal true]}
    :reads #{}
    :writes #{:request/status :request/score}
    :replacements {:request/status [:literal :done]
                   :request/score [:literal 12]}
    :frame #{:request/version}}
   :initial-condition {:readers #{} :expression [:literal true]}
   :invariants
   {:request/both-fixed
    {:readers #{:request/status :request/score}
     :expression [:and
                  [:= [:read :request/status] [:literal :done]]
                  [:> [:read :request/score] [:literal 7]]]}
    :request/both-false
    {:readers #{:request/status :request/score}
     :expression [:and
                  [:= [:read :request/status] [:literal :claimed]]
                  [:> [:read :request/score] [:literal 7]]]}
    :request/mixed-with-frame
    {:readers #{:request/score :request/version}
     :expression [:and
                  [:> [:read :request/score] [:literal 7]]
                  [:>= [:read :request/version] [:literal 0]]]}}})

(def ^:private preservation-error
  :gesso.assurance.proof.assignment-preservation/invalid-assignment-preservation)
(def ^:private source-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)
(def ^:private construction-error
  :gesso.assurance.transition/invalid-construction)
(def ^:private premises
  #{:gesso.assurance.proof.derivation/abstract-relation-match})
(def ^:private checker-open
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})

(defn- check! [source]
  (preservation/check-literal-assignment-preservation! source))

(defn- outcome [report id]
  (first (filter #(= id (:invariant %)) (:preservation-outcomes report))))

(defn- thrown-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects! [f error-type reason]
  (let [data (thrown-data f)]
    (is (map? data) (str "Expected typed rejection " error-type "/" reason))
    (is (= error-type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- check-report! [source report]
  (let [original (obligation/derive-obligations! source)
        original-ids (set (map :id (get-in original [:obligations :preservation])))
        returned (set (map :obligation-id (:preservation-outcomes report)))
        covered (set/union (:conditionally-checked-preservation-ids report)
                           (:unsupported-preservation-ids report))]
    (is (= preservation/report-type (:type report)))
    (is (= preservation/assignment-preservation-version (:version report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= (:fingerprint original) (:obligation-fingerprint report)))
    (is (= (:transition-fingerprint original) (:transition-fingerprint report)))
    (is (= (:open-obligation-ids original) (:original-open-obligation-ids report)))
    (is (= original-ids returned covered))
    (is (= #{} (set/intersection (:conditionally-checked-preservation-ids report)
                                 (:unsupported-preservation-ids report))))
    (is (= :declared-abstract-state-projection-only (:scope report)))
    (is (= :all-pre-invariants-and-matching-transition-imply-post-invariant
           (:claim report)))
    (is (= :closed-typed-literal-postcondition (:proof-rule report)))
    (is (= :original-sources-independent-assignment-derivations-and-typed-evaluation
           (:checking report)))
    (is (= :restricted-conditional-preservation-checked (:proof-status report)))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (= #{:proof-checker-and-literal-substitution-rule-soundness
             :certificate-admission} (:open-meta-obligations report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :full-success?)))
    (is (not (contains? report :transaction-authority)))
    report))

(deftest exact-coverage-of-all-declared-preservation-obligations
  (let [result (check-report! claim-source (check! claim-source))]
    (is (= (set (keys claim-invariants))
           (set (map :invariant (:preservation-outcomes result)))))
    (is (= #{[:preservation :request/status-in-domain]
             [:preservation :request/status-claimed]}
           (:conditionally-checked-preservation-ids result)))
    (is (= (set/difference
            (set (map (fn [id] [:preservation id]) (keys claim-invariants)))
            (:conditionally-checked-preservation-ids result))
           (:unsupported-preservation-ids result)))))

(deftest checked-literal-enum-and-identity
  (let [result (check! claim-source)
        matched (outcome result :request/status-claimed)]
    (is (= :true (:literal-postcondition matched)))
    (is (= :checked-conditional-inference (:status matched)))
    (is (= :closed-typed-literal-postcondition (:inference matched)))
    (is (= #{} (:unsupported-readers matched)))
    (is (= #{:request/status} (set (keys (:assignment-derivations matched)))))
    (is (= result (check! claim-source)))))

(deftest written-enum-membership-proof
  (let [result (check! claim-source)
        out (outcome result :request/status-in-domain)]
    (is (= :true (:literal-postcondition out)))
    (is (= #{:request/status} (:readers out)))
    (is (= :checked-conditional-inference (:status out)))))

(deftest false-literal-invariant-keeps-obligation-open
  (let [result (check! claim-source)
        out (outcome result :request/status-open)]
    (is (= :false (:literal-postcondition out)))
    (is (= :not-established-by-literal-assignment (:inference out)))
    (is (= {} (:assignment-derivations out)))
    (is (= :open (:status out)))
    (is (contains? (:original-open-obligation-ids result)
                   [:preservation :request/status-open]))))

(deftest arithmetic-bound-passed-at-typed-literal
  (let [result (check-report! score-source (check! score-source))]
    (is (= :true (:literal-postcondition
                  (outcome result :request/score-gt-seven))))
    (is (= :true (:literal-postcondition
                  (outcome result :request/score-is-nine))))))

(deftest strict-inequality-at-boundary-is-false
  (let [result (check! score-source)
        out (outcome result :request/score-gt-nine)]
    (is (= :false (:literal-postcondition out)))
    (is (= :open (:status out)))))

(deftest a-bad-constant-does-not-create-a-preservation-proof
  (let [source (assoc-in score-source [:construction :replacements :request/score]
                         [:literal 6])
        result (check-report! source (check! source))]
    (is (= :false (:literal-postcondition
                   (outcome result :request/score-gt-seven))))
    (is (not (contains? (:conditionally-checked-preservation-ids result)
                        [:preservation :request/score-gt-seven])))))

(deftest framed-readers-remain-unsupported
  (let [result (check! score-source)
        out (outcome result :request/note-present)]
    (is (= :not-checked (:literal-postcondition out)))
    (is (= #{:request/note} (:unsupported-readers out)))
    (is (= :open (:status out)))
    (is (= {} (:assignment-derivations out)))))

(deftest copy-assignment-remains-unsupported
  (let [result (check! claim-source)
        out (outcome result :request/owner-non-nil)]
    (is (= #{:request/owner} (:unsupported-readers out)))
    (is (= :not-checked (:literal-postcondition out)))
    (is (= :open (:status out)))
    (is (not (contains? (:assignment-derivations result) :request/owner)))))

(deftest mixed-framed-and-literal-readers-remain-open
  (let [result (check! claim-source)
        out (outcome result :request/status-and-version)]
    (is (= #{:request/version} (:unsupported-readers out)))
    (is (= :not-checked (:literal-postcondition out)))
    (is (= :open (:status out)))))

(deftest empty-reader-true-and-false-do-not-get-literal-substitution-proofs
  (let [result (check! claim-source)]
    (doseq [id [:request/constant-true :request/constant-false]]
      (is (= #{} (:readers (outcome result id))))
      (is (= :open (:status (outcome result id))))
      (is (= :not-checked (:literal-postcondition (outcome result id)))))))

(deftest multi-written-field-invariant-requires-both-assignment-proofs
  (let [result (check-report! two-write-source (check! two-write-source))
        out (outcome result :request/both-fixed)]
    (is (= :true (:literal-postcondition out)))
    (is (= #{:request/status :request/score}
           (set (keys (:assignment-derivations out)))))
    (is (= #{:request/status :request/score}
           (set (keys (:assignment-derivations result)))))
    (is (= :checked-conditional-inference (:status out)))))

(deftest multi-written-field-invariant-false-remains-open
  (let [result (check! two-write-source)
        out (outcome result :request/both-false)]
    (is (= :false (:literal-postcondition out)))
    (is (= {} (:assignment-derivations out)))
    (is (= :open (:status out)))))

(deftest multi-written-field-invariant-with-frame-remains-open
  (let [result (check! two-write-source)
        out (outcome result :request/mixed-with-frame)]
    (is (= #{:request/version} (:unsupported-readers out)))
    (is (= {} (:assignment-derivations out)))
    (is (= :open (:status out)))))

(deftest optional-nil-assignment-means-present-not-absent
  (let [result (check-report! nullable-source (check! nullable-source))]
    (is (= :true (:literal-postcondition
                  (outcome result :request/note-present))))
    (is (= :true (:literal-postcondition
                  (outcome result :request/note-nil))))
    (is (= [:eq [:post :request/note] [:literal nil]]
           (get-in result [:assignment-derivations :request/note :conclusion])))))

(deftest optional-nil-does-not-prove-non-nil
  (let [result (check! nullable-source)
        out (outcome result :request/note-non-nil)]
    (is (= :false (:literal-postcondition out)))
    (is (= :open (:status out)))))

(deftest optional-not-written-is-not-treated-as-present
  (let [result (check! score-source)
        out (outcome result :request/note-present)]
    (is (= :open (:status out)))
    (is (= :not-checked (:literal-postcondition out)))))

(deftest each-emitted-assignment-derivation-matches-original-checker
  (let [result (check! two-write-source)
        t (:transition (construction/build-guarded-replacement!
                        (:construction two-write-source)))]
    (doseq [[field proof] (:assignment-derivations result)]
      (let [literal (second (get-in two-write-source
                                    [:construction :replacements field]))
            checked (derivation/check-transition-derivation!
                     {:transition t
                      :steps [{:id :gesso.assurance.proof.assignment-preservation/assignment
                               :rule :assignment :field field
                               :conclusion [:eq [:post field] [:literal literal]]}]
                      :goal [:eq [:post field] [:literal literal]]})]
        (is (= (:fingerprint checked) (:derivation-fingerprint proof)))
        (is (= (:goal checked) (:conclusion proof)))
        (is (= premises (:open-premises checked)))
        (is (= checker-open (:missing-obligations checked)))
        (is (= :none (:authority checked)))))))

(deftest every-original-invariant-obligation-remains-open
  (let [result (check! claim-source)]
    (doseq [id (keys claim-invariants)]
      (is (contains? (:original-open-obligation-ids result) [:preservation id]))
      (is (contains? (:original-open-obligation-ids result) [:initial-state id])))
    (is (contains? (:original-open-obligation-ids result)
                   [:assumption :declared-invariant-set-coverage]))
    (is (contains? (:original-open-obligation-ids result)
                   [:implementation :production-transition-correspondence]))))

(deftest false-initial-invariant-does-not-vanish
  (let [source (assoc score-source :initial-condition
                      {:readers #{:request/score}
                       :expression [:= [:read :request/score] [:literal 0]]})
        result (check! source)]
    (is (contains? (:conditionally-checked-preservation-ids result)
                   [:preservation :request/score-gt-seven]))
    (is (contains? (:original-open-obligation-ids result)
                   [:initial-state :request/score-gt-seven]))
    (is (= :not-issued (:certificate result)))))

(deftest omitted-invariant-still-leaves-coverage-premise
  (let [source (update score-source :invariants dissoc :request/score-gt-nine)
        result (check! source)]
    (is (not (contains? (:conditionally-checked-preservation-ids result)
                        [:preservation :request/score-gt-nine])))
    (is (contains? (:original-open-obligation-ids result)
                   [:assumption :declared-invariant-set-coverage]))
    (is (not= (:fingerprint (check! score-source)) (:fingerprint result)))))

(deftest changed-written-constant-invalidates-fingerprints
  (let [source (assoc-in score-source [:construction :replacements :request/score]
                         [:literal 8])
        before (check! score-source)
        after (check! source)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (not= (:transition-fingerprint before) (:transition-fingerprint after)))
    (is (not= (:obligation-fingerprint before) (:obligation-fingerprint after)))))

(deftest changed-guard-cannot-reuse-old-proof-identity
  (let [source (-> score-source
                   (assoc-in [:construction :guard]
                             {:readers #{:request/score}
                              :expression [:>= [:read :request/score] [:literal 0]]})
                   (assoc-in [:construction :reads] #{:request/score}))
        before (check! score-source)
        after (check! source)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (= (:conditionally-checked-preservation-ids before)
           (:conditionally-checked-preservation-ids after)))))

(deftest changed-initial-condition-preserves-open-initial-obligations
  (let [source (assoc score-source :initial-condition
                      {:readers #{:request/score}
                       :expression [:>= [:read :request/score] [:literal 0]]})
        before (check! score-source)
        after (check! source)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (contains? (:original-open-obligation-ids after)
                   [:initial-state :request/score-gt-seven]))))

(deftest changed-operation-identity-changes-proof-source
  (let [source (-> score-source
                   (assoc-in [:construction :subject :id] :example/other-score)
                   (assoc-in [:construction :subject :interface :operation] :example/other-score))
        before (check! score-source)
        after (check! source)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (not= (:subject-fingerprint before) (:subject-fingerprint after)))))

(deftest invented-write-is-rejected-at-construction-boundary
  (let [source (update-in score-source [:construction :writes] conj :request/note)]
    (rejects! #(check! source) construction-error :write-footprint-mismatch)))

(deftest incomplete-frame-is-rejected-at-construction-boundary
  (let [source (update-in score-source [:construction :frame] disj :request/note)]
    (rejects! #(check! source) construction-error :frame-footprint-mismatch)))

(deftest undeclared-assignment-is-rejected-at-construction-boundary
  (let [source (assoc-in score-source [:construction :replacements :request/note]
                         [:literal "extra"])]
    (rejects! #(check! source) construction-error :write-footprint-mismatch)))

(deftest undeclared-invariant-reader-is-rejected
  (let [source (assoc-in score-source
                         [:invariants :request/score-is-nine :readers]
                         #{:request/score :request/phantom})]
    (rejects! #(check! source) source-error :non-state-reader)))

(deftest forged-report-used-as-source-is-rejected
  (let [result (check! score-source)]
    (rejects! #(check! result) source-error :wrong-keys)
    (rejects! #(check! (assoc score-source :proof-status :proved))
              source-error :wrong-keys)
    (rejects! #(check! (assoc score-source :valid? true))
              source-error :wrong-keys)
    (rejects! #(check! (assoc score-source :authority :xtdb-commit))
              source-error :wrong-keys)))

(deftest copied-derivation-report-cannot-be-used-as-source
  (let [result (check! score-source)]
    (rejects! #(check! {:construction (:construction score-source)
                        :initial-condition (:initial-condition score-source)
                        :invariants (:invariants score-source)
                        :assignment-derivations (:assignment-derivations result)})
              source-error :wrong-keys)))

(deftest metadata-on-original-source-is-rejected
  (rejects! #(check! (with-meta score-source {:certified true}))
            source-error :metadata-not-allowed))

(deftest tampered-proof-checker-results-fail-closed
  (let [real derivation/check-transition-derivation!
        corruptions [[:open-premises #{}]
                     [:missing-obligations #{}]
                     [:authority :xtdb-commit]
                     [:certificate :issued]
                     [:admission :admitted]
                     [:implementation-correspondence :proved]
                     [:runtime-enforcement :proved]
                     [:claim-scope :physical]
                     [:proof-status :proved]
                     [:conclusion-status :unconditional]
                     [:assumption-discharge :performed]
                     [:goal [:eq [:post :request/score] [:literal 0]]]
                     [:transition-fingerprint "sha256:invalid"]
                     [:subject-fingerprint "sha256:invalid"]
                     [:rule-count 100]
                     [:fingerprint "not-a-sha256"]]]
    (doseq [[k value] corruptions]
      (testing (str "corrupted derivation checker result: " k)
        (rejects!
         #(with-redefs [derivation/check-transition-derivation!
                        (fn [source] (assoc (real source) k value))]
            (check! score-source))
         preservation-error :derivation-checker-contract-changed)))))

(deftest corrupted-rule-evaluator-results-fail-closed
  (let [real rule-eval/evaluate-rule!
        corruptions [[:type :fake]
                     [:version 999]
                     [:checking :unchecked]
                     [:evaluation :opaque-callback]
                     [:proof-status :proved]
                     [:admission :admitted]
                     [:runtime-enforcement :proved]
                     [:authority :xtdb-commit]
                     [:readers #{}]
                     [:expression [:literal true]]
                     [:value :yes]]]
    (doseq [[k value] corruptions]
      (testing (str "corrupted typed evaluator result: " k)
        (rejects!
         #(with-redefs [rule-eval/evaluate-rule!
                        (fn [& args] (assoc (apply real args) k value))]
            (check! score-source))
         preservation-error :rule-evaluator-contract-changed)))))

(deftest fabricated-checker-output-is-not-proof
  (rejects!
   #(with-redefs [derivation/check-transition-derivation!
                  (fn [_] {:proof-status :conditional-abstract-inference
                           :authority :none})]
      (check! score-source))
   preservation-error :derivation-checker-contract-changed))

(deftest fabricated-evaluator-output-is-not-proof
  (rejects!
   #(with-redefs [rule-eval/evaluate-rule!
                  (fn [_ _ _] {:value true :proof-status :not-proved})]
      (check! score-source))
   preservation-error :rule-evaluator-contract-changed))

(deftest numerical-boundary-cases-retain-closed-typed-semantics
  (doseq [n [-1 0 1 7 8 9 10 1000 9223372036854775807N]]
    (let [source (assoc-in score-source
                           [:construction :replacements :request/score]
                           [:literal n])
          result (check! source)
          out (outcome result :request/score-gt-seven)]
      (is (= (if (> n 7) :true :false) (:literal-postcondition out))
          (str "Postcondition should agree for literal " n))
      (is (= (if (> n 7) :checked-conditional-inference :open)
             (:status out))))))

(deftest immutable-frame-and-invalid-poststate-samples-are-distinct
  (let [transition-source
        (:transition (construction/build-guarded-replacement!
                      (:construction score-source)))
        pre {:request/score 0}
        matching {:request/score 9}
        altered {:request/score 9 :request/note nil}
        good (transition/observe-transition! transition-source pre {} matching)
        bad (transition/observe-transition! transition-source pre {} altered)]
    (is (true? (:matches? good)))
    (is (false? (:matches? bad)))
    (is (false? (:frame-holds? bad)))))

(deftest source-only-inference-does-not-claim-execution-or-certification
  (doseq [source [claim-source score-source nullable-source two-write-source]]
    (let [result (check-report! source (check! source))]
      (is (= :not-issued (:certificate result)))
      (is (= :not-established (:implementation-correspondence result)))
      (is (not (contains? result :full-success?))))))
