(ns gesso.assurance.proof.mixed-preservation-test
  "v1264: permanent adversarial tests for v1263's RESTRICTED mixed
   frame/literal preservation inference. The inference is conditional on
   original abstract transition semantics and all pre-invariants, NOT an
   inductive proof, admitted certificate, physical transaction, or claim that
   an arbitrary Clojure callback is correct. Corrupt field derivations and
   evaluators deliberately; a successful-looking report is not authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.mixed-preservation :as mixed]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.transition :as construction]))

(def ^:private operation
  {:subject {:kind :operation :id :example/score-claim
             :interface {:operation :example/score-claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/score {:type :integer}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/score :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/score [:literal 9]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})

(def ^:private rules
  {:request/mixed
   {:readers #{:request/version :request/status}
    :expression [:and
                 [:>= [:read :request/version] [:literal 0]]
                 [:in [:read :request/status]
                  [:literal #{:open :claimed :done}]]]}
   :request/mixed-reversed
   {:readers #{:request/version :request/status}
    :expression [:and
                 [:= [:read :request/status] [:literal :claimed]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/three-conjuncts
   {:readers #{:request/version :request/status :request/score}
    :expression [:and
                 [:>= [:read :request/version] [:literal 0]]
                 [:> [:read :request/score] [:literal 7]]
                 [:= [:read :request/status] [:literal :claimed]]]}
   :request/frame-only
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/literal-only
   {:readers #{:request/score}
    :expression [:> [:read :request/score] [:literal 7]]}
   :request/false-postcondition
   {:readers #{:request/version :request/status}
    :expression [:and
                 [:>= [:read :request/version] [:literal 0]]
                 [:= [:read :request/status] [:literal :open]]]}
   :request/copied-owner
   {:readers #{:request/version :request/owner}
    :expression [:and
                 [:>= [:read :request/version] [:literal 0]]
                 [:not= [:read :request/owner] [:literal nil]]]}
   :request/cross-field
   {:readers #{:request/version :request/score}
    :expression [:and
                 [:>= [:read :request/score] [:read :request/version]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/optional-present
   {:readers #{:request/note :request/status}
    :expression [:and
                 [:present [:read :request/note]]
                 [:= [:read :request/status] [:literal :claimed]]]}
   :request/optional-dependent
   {:readers #{:request/note :request/status}
    :expression [:and
                 [:present [:read :request/note]]
                 [:= [:read :request/note] [:literal nil]]
                 [:= [:read :request/status] [:literal :claimed]]]}})

(def ^:private baseline
  {:construction operation
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants rules})

(def ^:private conditional
  #{:request/mixed :request/mixed-reversed :request/three-conjuncts
    :request/frame-only :request/literal-only :request/optional-present})
(def ^:private open-invariants
  #{:request/false-postcondition :request/copied-owner :request/cross-field
    :request/optional-dependent})
(def ^:private mixed-error
  :gesso.assurance.proof.mixed-preservation/invalid-mixed-preservation)

(defn- check! [source] (mixed/check-mixed-preservation! source))
(defn- outcome [report invariant]
  (first (filter #(= invariant (:invariant %))
                 (:preservation-outcomes report))))
(defn- failing-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- rejects! [f error-type reason]
  (let [d (failing-data f)]
    (is (map? d) (str "Expected a typed rejection " error-type "/" reason))
    (is (= error-type (:error/type d)) (pr-str d))
    (is (= reason (:reason d)) (pr-str d))
    d))
(defn- source-with [id declaration]
  (assoc baseline :invariants {id declaration}))
(defn- obligation-ids [xs] (set (map (fn [x] [:preservation x]) xs)))

(defn- assert-unadmitted! [source report]
  (let [original (obligation/derive-obligations! source)
        declared (set (keys (:invariants source)))
        established (:conditionally-checked-preservation-ids report)
        unsupported (:unsupported-preservation-ids report)]
    (is (= mixed/report-type (:type report)))
    (is (= mixed/mixed-preservation-version (:version report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= (:fingerprint original) (:obligation-fingerprint report)))
    (is (= (:transition-fingerprint original) (:transition-fingerprint report)))
    (is (= (:open-obligation-ids original)
           (:original-open-obligation-ids report)))
    (is (= (obligation-ids declared)
           (set (map :obligation-id (:preservation-outcomes report)))))
    (is (= (obligation-ids declared) (set/union established unsupported)))
    (is (empty? (set/intersection established unsupported)))
    (is (= :separable-frame-and-literal-conjunction (:proof-rule report)))
    (is (= :declared-abstract-state-projection-only (:scope report)))
    (is (= :all-pre-invariants-and-matching-transition-imply-post-invariant
           (:claim report)))
    (is (= :original-sources-independent-field-derivations-and-typed-evaluation
           (:checking report)))
    (is (= :restricted-conditional-preservation-checked (:proof-status report)))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (= #{:proof-checker-frame-and-literal-conjunction-rule-soundness
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

(deftest exact-coverage-of-every-declared-invariant
  (let [r (assert-unadmitted! baseline (check! baseline))]
    (is (= (obligation-ids conditional)
           (:conditionally-checked-preservation-ids r)))
    (is (= (obligation-ids open-invariants)
           (:unsupported-preservation-ids r)))
    (is (= (count rules) (count (:preservation-outcomes r))))
    (is (= (count rules) (count (distinct (map :invariant (:preservation-outcomes r))))))))

(deftest mixed-conjunction-proves-frame-and-literal-clauses
  (let [o (outcome (check! baseline) :request/mixed)]
    (is (= :checked-conditional-inference (:status o)))
    (is (= :frame-and-literal-conjunction (:inference o)))
    (is (= #{:request/version} (:frame-readers o)))
    (is (= #{:request/status} (:literal-readers o)))
    (is (= :true (:literal-postcondition o)))
    (is (= [:frame :literal] (mapv :kind (:clause-judgments o))))))

(deftest reversed-mixed-conjunction-keeps-the-original-order
  (let [o (outcome (check! baseline) :request/mixed-reversed)]
    (is (= :checked-conditional-inference (:status o)))
    (is (= [:literal :frame] (mapv :kind (:clause-judgments o))))
    (is (= #{:request/status} (:literal-readers o)))
    (is (= #{:request/version} (:frame-readers o)))))

(deftest three-conjuncts-require-both-assigned-fields
  (let [r (check! baseline)
        o (outcome r :request/three-conjuncts)]
    (is (= :checked-conditional-inference (:status o)))
    (is (= [:frame :literal :literal] (mapv :kind (:clause-judgments o))))
    (is (= #{:request/status :request/score} (:literal-readers o)))
    (is (= #{:request/version} (:frame-readers o)))
    (is (= #{:request/status :request/score}
           (set (keys (:assignment-derivations r)))))))

(deftest frame-only-and-literal-only-rules-share-the-checked-boundary
  (let [r (check! baseline)
        frame (outcome r :request/frame-only)
        literal (outcome r :request/literal-only)]
    (is (= :checked-conditional-inference (:status frame)))
    (is (= :pure-typed-rule-frame-extensionality (:inference frame)))
    (is (= :not-applicable (:literal-postcondition frame)))
    (is (= :checked-conditional-inference (:status literal)))
    (is (= :closed-typed-literal-postcondition (:inference literal)))
    (is (= :true (:literal-postcondition literal)))))

(deftest each-supported-field-has-an-independent-original-derivation
  (let [r (check! baseline)
        transition (:transition (construction/build-guarded-replacement! operation))]
    (is (= #{:request/version :request/note}
           (set (keys (:frame-derivations r)))))
    (is (= #{:request/status :request/score}
           (set (keys (:assignment-derivations r)))))
    (doseq [[field proof] (:frame-derivations r)]
      (let [source {:transition transition
                    :steps [{:id :gesso.assurance.proof.mixed-preservation/fact :rule :frame :field field
                             :conclusion [:same-field field]}]
                    :goal [:same-field field]}
            checked (derivation/check-transition-derivation! source)]
        (is (= (:fingerprint checked) (:derivation-fingerprint proof)))
        (is (= (:goal checked) (:claim proof)))
        (is (= :matching-abstract-transition-only (:scope proof)))))
    (doseq [[field proof] (:assignment-derivations r)]
      (let [value (get-in operation [:replacements field 1])
            claim [:eq [:post field] [:literal value]]
            checked (derivation/check-transition-derivation!
                     {:transition transition
                      :steps [{:id :gesso.assurance.proof.mixed-preservation/fact :rule :assignment
                               :field field :conclusion claim}]
                      :goal claim})]
        (is (= (:fingerprint checked) (:derivation-fingerprint proof)))
        (is (= (:goal checked) (:claim proof)))))))

(deftest false-postcondition-keeps-the-mixed-obligation-open
  (let [o (outcome (check! baseline) :request/false-postcondition)]
    (is (= :open (:status o)))
    (is (= :false (:literal-postcondition o)))
    (is (= :not-established-by-separable-conjunction (:inference o)))
    (is (= #{} (:frame-readers o) (:literal-readers o)))))

(deftest copy-assigned-owner-never-becomes-a-literal-proof
  (let [o (outcome (check! baseline) :request/copied-owner)]
    (is (= :open (:status o)))
    (is (= :not-checked (:literal-postcondition o)))
    (is (= :unsupported (get-in o [:clause-judgments 1 :kind])))
    (is (= :mixed-or-copy-assigned-clause
           (get-in o [:clause-judgments 1 :reason])))))

(deftest cross-field-comparison-in-one-clause-stays-open
  (let [o (outcome (check! baseline) :request/cross-field)]
    (is (= :open (:status o)))
    (is (= :unsupported (get-in o [:clause-judgments 0 :kind])))
    (is (= :mixed-or-copy-assigned-clause
           (get-in o [:clause-judgments 0 :reason])))))

(deftest optional-presence-is-a-valid-independent-frame-clause
  (let [o (outcome (check! baseline) :request/optional-present)]
    (is (= :checked-conditional-inference (:status o)))
    (is (= :frame-and-literal-conjunction (:inference o)))
    (is (= #{:request/note} (:frame-readers o)))
    (is (= #{:request/status} (:literal-readers o)))))

(deftest optional-refinement-dependence-must-remain-open
  (let [o (outcome (check! baseline) :request/optional-dependent)]
    (is (= :open (:status o)))
    (is (= :unsupported (get-in o [:clause-judgments 1 :kind])))
    (is (= :not-independently-well-typed
           (get-in o [:clause-judgments 1 :reason])))))

(deftest nested-conjunction-is-not-flattened-or-reinterpreted
  (let [s (source-with
           :request/nested
           {:readers #{:request/version :request/status :request/score}
            :expression [:and
                         [:and [:>= [:read :request/version] [:literal 0]]
                          [:= [:read :request/status] [:literal :claimed]]]
                         [:> [:read :request/score] [:literal 7]]]})
        o (outcome (check! s) :request/nested)]
    (is (= :open (:status o)))
    (is (= :unsupported (get-in o [:clause-judgments 0 :kind])))
    (is (= :mixed-or-copy-assigned-clause
           (get-in o [:clause-judgments 0 :reason])))))

(deftest a-single-clause-reading-framed-and-literal-fields-is-not-distributed
  (let [s (source-with
           :request/score-exceeds-version
           {:readers #{:request/score :request/version}
            :expression [:>= [:read :request/score] [:read :request/version]]})
        o (outcome (check! s) :request/score-exceeds-version)]
    (is (= :open (:status o)))
    (is (= :unsupported (get-in o [:clause-judgments 0 :kind])))))

(deftest a-purely-closed-false-rule-cannot-prove-initial-state-existence
  (let [s (source-with :request/impossible {:readers #{} :expression [:literal false]})
        r (assert-unadmitted! s (check! s))
        o (outcome r :request/impossible)]
    ;; Frame extensionality preserves the truth value EVEN IF it is false.
    ;; This conditional implication does not show a reachable pre-state.
    (is (= :checked-conditional-inference (:status o)))
    (is (= :pure-typed-rule-frame-extensionality (:inference o)))
    (is (contains? (:original-open-obligation-ids r)
                   [:initial-state :request/impossible]))))

(deftest changed-status-literal-invalidates-old-proof-identity
  (let [changed (assoc-in baseline [:construction :replacements :request/status]
                          [:literal :done])
        old (check! baseline)
        new (check! changed)]
    (is (not= (:fingerprint old) (:fingerprint new)))
    (is (not= (:obligation-fingerprint old) (:obligation-fingerprint new)))
    (is (= :open (:status (outcome new :request/mixed-reversed))))))

(deftest changed-score-literal-removes-true-postcondition
  (let [changed (assoc-in baseline [:construction :replacements :request/score]
                          [:literal 7])
        r (check! changed)]
    (is (not= (:fingerprint (check! baseline)) (:fingerprint r)))
    (is (= :open (:status (outcome r :request/three-conjuncts))))
    (is (= :false (:literal-postcondition (outcome r :request/three-conjuncts))))))

(deftest rewriting-a-framed-field-to-a-literal-write-removes-its-frame-proof
  (let [op (-> operation
               (assoc :writes #{:request/status :request/score :request/owner
                                :request/version}
                      :frame #{:request/note})
               (assoc-in [:replacements :request/version] [:literal -1]))
        r (check! (assoc baseline :construction op))]
    (is (not (contains? (:frame-derivations r) :request/version)))
    (is (= :open (:status (outcome r :request/mixed))))
    (is (= :false (:literal-postcondition (outcome r :request/mixed))))))

(deftest source-change-to-guard-invalidates-proof-identity
  (let [changed (assoc-in baseline [:construction :guard :expression]
                          [:in [:read :request/status] [:literal #{:open :claimed}]])]
    (is (not= (:fingerprint (check! baseline))
              (:fingerprint (check! changed))))))

(deftest changed-invariant-identity-or-expression-invalidates-proof-identity
  (let [renamed (update baseline :invariants
                        #(set/rename-keys % {:request/mixed :request/renamed}))
        rewritten (assoc-in baseline [:invariants :request/mixed :expression]
                            [:and [:= [:read :request/status] [:literal :claimed]]
                             [:> [:read :request/version] [:literal 3]]])]
    (is (not= (:fingerprint (check! baseline)) (:fingerprint (check! renamed))))
    (is (not= (:fingerprint (check! baseline)) (:fingerprint (check! rewritten))))))

(deftest omitting-an-invariant-does-not-erase-the-coverage-assumption
  (let [s (update baseline :invariants dissoc :request/mixed)
        r (assert-unadmitted! s (check! s))]
    (is (not= (:fingerprint r) (:fingerprint (check! baseline))))
    (is (contains? (:original-open-obligation-ids r)
                   [:assumption :declared-invariant-set-coverage]))
    (is (not (contains? (:conditionally-checked-preservation-ids r)
                        [:preservation :request/mixed])))))

(deftest changed-initial-condition-does-not-discharge-any-initial-obligations
  (let [s (assoc baseline :initial-condition
                 {:readers #{:request/score}
                  :expression [:> [:read :request/score] [:literal 3]]})
        r (assert-unadmitted! s (check! s))]
    (is (not= (:fingerprint (check! baseline)) (:fingerprint r)))
    (is (contains? (:original-open-obligation-ids r)
                   [:initial-state :request/mixed]))))

(deftest forged-derivation-result-with-transaction-authority-is-rejected
  (let [original derivation/check-transition-derivation!]
    (rejects! #(with-redefs [derivation/check-transition-derivation!
                            (fn [p] (assoc (original p) :authority :transaction))]
                 (check! baseline))
              mixed-error :derivation-checker-contract-changed)))

(deftest forged-derivation-result-without-relation-premise-is-rejected
  (let [original derivation/check-transition-derivation!]
    (rejects! #(with-redefs [derivation/check-transition-derivation!
                            (fn [p] (assoc (original p) :open-premises #{}))]
                 (check! baseline))
              mixed-error :derivation-checker-contract-changed)))

(deftest forged-derivation-result-with-false-conclusion-is-rejected
  (let [original derivation/check-transition-derivation!]
    (rejects! #(with-redefs [derivation/check-transition-derivation!
                            (fn [p] (assoc (original p) :goal [:literal true]))]
                 (check! baseline))
              mixed-error :derivation-checker-contract-changed)))

(deftest forged-derivation-result-without-open-obligations-is-rejected
  (let [original derivation/check-transition-derivation!]
    (rejects! #(with-redefs [derivation/check-transition-derivation!
                            (fn [p] (assoc (original p) :missing-obligations #{}))]
                 (check! baseline))
              mixed-error :derivation-checker-contract-changed)))

(deftest forged-derivation-result-cannot-claim-admission
  (let [original derivation/check-transition-derivation!]
    (rejects! #(with-redefs [derivation/check-transition-derivation!
                            (fn [p] (assoc (original p) :certificate :issued))]
                 (check! baseline))
              mixed-error :derivation-checker-contract-changed)))

(deftest forged-rule-type-result-cannot-establish-an-optional-clause
  (let [original rule-type/check-rule-type!]
    (rejects! #(with-redefs [rule-type/check-rule-type!
                            (fn [rule types]
                              (assoc (original rule types) :authority :transaction))]
                 (check! baseline))
              mixed-error :rule-type-checker-contract-changed)))

(deftest forged-rule-evaluation-result-cannot-establish-a-literal-conjunct
  (let [original rule-eval/evaluate-rule!]
    (rejects! #(with-redefs [rule-eval/evaluate-rule!
                            (fn [rule types values]
                              (assoc (original rule types values)
                                     :proof-status :proved))]
                 (check! baseline))
              mixed-error :rule-evaluator-contract-changed)))

(deftest forged-rule-evaluation-transaction-authority-is-rejected
  (let [original rule-eval/evaluate-rule!]
    (rejects! #(with-redefs [rule-eval/evaluate-rule!
                            (fn [rule types values]
                              (assoc (original rule types values)
                                     :authority :transaction))]
                 (check! baseline))
              mixed-error :rule-evaluator-contract-changed)))

(deftest upstream-obligation-coverage-tampering-is-rejected
  (let [original obligation/derive-obligations!]
    (rejects! #(with-redefs [obligation/derive-obligations!
                            (fn [source]
                              (update-in (original source)
                                         [:obligations :preservation] pop))]
                 (check! baseline))
              mixed-error :obligation-source-contract-changed)))

(deftest upstream-premature-proof-tampering-is-rejected
  (let [original obligation/derive-obligations!]
    (rejects! #(with-redefs [obligation/derive-obligations!
                            (fn [source]
                              (update-in (original source)
                                         [:obligations :preservation 0]
                                         assoc :status :proved))]
                 (check! baseline))
              mixed-error :obligation-source-contract-changed)))

(deftest altered-caller-report-is-not-a-valid-source
  (let [report (check! baseline)]
    (is (map? (failing-data #(check! report))))
    (is (map? (failing-data #(check! (assoc report :valid? true)))))))

(deftest source-metadata-does-not-become-hidden-assurance-authority
  (let [s (with-meta baseline {:certificate :issued})]
    (is (map? (failing-data #(check! s))))))

(deftest abstract-transition-samples-reject-mutated-frames
  (let [transition (:transition (construction/build-guarded-replacement! operation))
        inputs {:input/actor :user/a}]
    (doseq [version [-100000000000000000N -1 0 2 100000000000000000N]
            note [::absent nil "hello"]]
      (let [pre (cond-> {:request/status :open :request/score 0
                         :request/owner nil :request/version version}
                  (not= note ::absent) (assoc :request/note note))
            post (-> pre
                     (assoc :request/status :claimed :request/score 9
                            :request/owner :user/a))
            good (abstract/observe-transition! transition pre inputs post)
            bad (abstract/observe-transition!
                 transition pre inputs
                 (assoc post :request/version (inc version)))]
        (is (true? (:matches? good)))
        (is (false? (:frame-holds? bad)))
        (is (false? (:matches? bad)))))))

(deftest optional-frame-presence-mutations-break-the-abstract-relation
  (let [transition (:transition (construction/build-guarded-replacement! operation))
        base {:request/status :open :request/score 0
              :request/owner nil :request/version 2}
        input {:input/actor :user/a}
        after (assoc base :request/status :claimed :request/score 9
                     :request/owner :user/a)]
    (doseq [[pre post] [[base (assoc after :request/note nil)]
                        [(assoc base :request/note nil) after]
                        [(assoc base :request/note "x")
                         (assoc after :request/note "y")]]]
      (let [result (abstract/observe-transition! transition pre input post)]
        (is (false? (:frame-holds? result)))
        (is (false? (:matches? result)))))))

(deftest abstract-guard-failure-is-not-an-established-transition
  (let [transition (:transition (construction/build-guarded-replacement! operation))
        pre {:request/status :claimed :request/score 3
             :request/owner nil :request/version 0}
        post (assoc pre :request/score 9 :request/owner :user/a)]
    (let [r (abstract/observe-transition! transition pre {:input/actor :user/a} post)]
      (is (false? (:guard-holds? r)))
      (is (false? (:matches? r))))))

(deftest a-conditional-preservation-is-not-a-production-certificate
  (let [report (assert-unadmitted! baseline (check! baseline))]
    (is (contains? (:original-open-obligation-ids report)
                   [:assumption :checker-meta-soundness]))
    (is (contains? (:original-open-obligation-ids report)
                   [:assumption :declared-invariant-set-coverage]))
    (is (contains? (:original-open-obligation-ids report)
                   [:implementation :production-transition-correspondence]))
    (is (contains? (:original-open-obligation-ids report)
                   [:implementation :atomic-transaction-enforcement]))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (= :none (:authority report)))))
