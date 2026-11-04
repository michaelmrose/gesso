(ns gesso.assurance.proof.copy-preservation-test
  "Permanent adversarial tests for the v1265 copy-assignment preservation inference.
   Equality proofs are conditional on exact abstract transition semantics;
   neither a matching observed state nor a checked derivation is physical
   transaction authority, independently proved kernel soundness, or proof of
   arbitrary business logic."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [gesso.assurance.proof.copy-preservation :as copy]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.transition :as construction]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-type :as rule-type]))

(def ^:private operation
  {:subject {:kind :operation :id :example/copy
             :interface {:operation :example/copy}}
   :state-types
   {:request/status {:type :enum :name :request/status :values #{:open :claimed}}
    :request/original {:type :integer}
    :request/copied {:type :integer}
    :request/copied2 {:type :integer}
    :request/version {:type :integer}
    :request/actor-one {:type :keyword}
    :request/actor-two {:type :keyword}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :request/original :input/actor}
   :writes #{:request/status :request/copied :request/copied2
             :request/actor-one :request/actor-two}
   :replacements {:request/status [:literal :claimed]
                  :request/copied [:read :request/original]
                  :request/copied2 [:read :request/original]
                  :request/actor-one [:read :input/actor]
                  :request/actor-two [:read :input/actor]}
   :frame #{:request/original :request/version :request/note}})

(def ^:private rules
  {:request/target-source
   {:readers #{:request/copied :request/original}
    :expression [:= [:read :request/copied] [:read :request/original]]}
   :request/target-target
   {:readers #{:request/copied :request/copied2}
    :expression [:= [:read :request/copied] [:read :request/copied2]]}
   :request/input-pair
   {:readers #{:request/actor-one :request/actor-two}
    :expression [:= [:read :request/actor-one] [:read :request/actor-two]]}
   :request/mixed
   {:readers #{:request/copied :request/original :request/version}
    :expression [:and [:= [:read :request/copied] [:read :request/original]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/frame
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/optional-frame
   {:readers #{:request/note}
    :expression [:present [:read :request/note]]}
   :request/unsupported-origin
   {:readers #{:request/copied :request/version}
    :expression [:= [:read :request/copied] [:read :request/version]]}
   :request/arithmetic
   {:readers #{:request/copied :request/original}
    :expression [:>= [:read :request/copied] [:read :request/original]]}
   :request/optional-dependent
   {:readers #{:request/note :request/copied :request/original}
    :expression [:and [:present [:read :request/note]]
                 [:= [:read :request/note] [:literal nil]]
                 [:= [:read :request/copied] [:read :request/original]]]}
   :request/literal-only
   {:readers #{:request/status}
    :expression [:= [:read :request/status] [:literal :claimed]]}})
(def ^:private baseline
  {:construction operation
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants rules})

(defn- outcome [r id]
  (first (filter #(= id (:invariant %)) (:preservation-outcomes r))))
(defn- rejected-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; These helpers deliberately check the contract *as returned*, not a caller's
;; willingness to attach a :valid? flag to a copyable EDN report.
(def ^:private copy-error
  :gesso.assurance.proof.copy-preservation/invalid-copy-preservation)
(def ^:private obligation-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)
(def ^:private construction-error
  :gesso.assurance.transition/invalid-guarded-replacement)

(defn- rejects! [thunk error-type reason]
  (let [data (rejected-data thunk)]
    (is (map? data) (str "Expected typed rejection for " reason))
    (is (= error-type (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(defn- source-for [id rule]
  (assoc baseline :invariants {id rule}))

(defn- proof-ids [ids]
  (set (map (fn [id] [:preservation id]) ids)))

(deftest source-contracts
  (let [report (copy/check-copy-preservation! baseline)
        original (obligation/derive-obligations! baseline)
        checked (set (map (fn [id] [:preservation id])
                          [:request/target-source :request/target-target
                           :request/input-pair :request/mixed :request/frame
                           :request/optional-frame]))]
    (is (= copy/report-type (:type report)))
    (is (= (:fingerprint original) (:obligation-fingerprint report)))
    (is (= (:open-obligation-ids original)
           (:original-open-obligation-ids report)))
    (is (= checked (:conditionally-checked-preservation-ids report)))
    (is (= (set/difference (set (map #(vector :preservation %)
                                      (keys rules))) checked)
           (:unsupported-preservation-ids report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))))

(deftest copy-from-framed-prestate
  (let [report (copy/check-copy-preservation! baseline)
        outcome (outcome report :request/target-source)]
    (is (= :checked-conditional-inference (:status outcome)))
    (is (= :shared-origin-cross-field-equality (:inference outcome)))
    (is (= [:pre :request/original]
           (get-in outcome [:clause-judgments 0 :common-origin])))
    (is (= [:eq [:post :request/copied] [:post :request/original]]
           (get-in report [:equality-derivations [:request/target-source 0]
                           :conclusion])))))

(deftest two-copy-assignments-from-same-prestate
  (let [report (copy/check-copy-preservation! baseline)]
    (is (= [:pre :request/original]
           (get-in (outcome report :request/target-target)
                   [:clause-judgments 0 :common-origin])))))

(deftest two-copy-assignments-from-same-input
  (let [report (copy/check-copy-preservation! baseline)]
    (is (= [:input :input/actor]
           (get-in (outcome report :request/input-pair)
                   [:clause-judgments 0 :common-origin])))))

(deftest supported-flat-conjunction-with-frame
  (let [report (copy/check-copy-preservation! baseline)
        o (outcome report :request/mixed)]
    (is (= :frame-and-copy-equality-conjunction (:inference o)))
    (is (= [:copy-equality :frame]
           (mapv :kind (:clause-judgments o))))
    (is (contains? (:frame-derivations report) :request/version))))

(deftest unsupported-clauses-remain-open
  (let [report (copy/check-copy-preservation! baseline)]
    (doseq [id [:request/unsupported-origin :request/arithmetic
                :request/optional-dependent :request/literal-only]]
      (is (= :open (:status (outcome report id))) (str id)))))

(deftest proof-substitution-is-independently-checked
  (let [report (copy/check-copy-preservation! baseline)
        result (get-in report [:equality-derivations [:request/input-pair 0]])]
    (is (re-matches #"sha256:[0-9a-f]{64}"
                    (:derivation-fingerprint result)))
    (is (= :matching-abstract-transition-only (:scope result)))))

(deftest forged-result-rejected
  (let [data (with-redefs [derivation/check-transition-derivation!
                           (fn [_] {:type derivation/report-type
                                    :proof-status :proved :authority :transaction})]
               (rejected-data #(copy/check-copy-preservation! baseline)))]
    (is (= :gesso.assurance.proof.copy-preservation/invalid-copy-preservation
           (:error/type data)))
    (is (= :derivation-checker-contract-changed (:reason data)))))

(deftest changed-assignment-fails-freshness
  (let [changed (assoc-in baseline [:construction :replacements :request/copied2]
                          [:read :request/version])
        changed (update-in changed [:construction :reads] conj :request/version)
        r (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome r :request/target-target))))
    (is (not= (:fingerprint (copy/check-copy-preservation! baseline))
              (:fingerprint r)))))

(deftest finite-observation-sanity
  (let [transition (:transition (construction/build-guarded-replacement! operation))]
    (doseq [i [0 1 7 99 100000000000000000000000N]]
      (let [pre {:request/status :open :request/original i
                 :request/copied 2 :request/copied2 3 :request/version 12
                 :request/actor-one :old :request/actor-two :other}
            post (assoc pre :request/status :claimed
                        :request/copied i :request/copied2 i
                        :request/actor-one :helper :request/actor-two :helper)]
        (is (true? (:matches? (abstract/observe-transition!
                              transition pre {:input/actor :helper} post))))))))

(deftest different-literals-do-not-manufacture-equality
  (let [changed (-> baseline
                    (assoc-in [:construction :replacements :request/copied]
                              [:literal 9])
                    (assoc-in [:construction :replacements :request/copied2]
                              [:literal 10])
                    (assoc-in [:construction :replacements :request/actor-one]
                              [:read :input/actor])
                    (update-in [:construction :reads] disj :request/original))
        report (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome report :request/target-target))))
    (is (= :open (:status (outcome report :request/target-source))))
    (is (= :checked-conditional-inference
           (:status (outcome report :request/input-pair))))))

(deftest identical-literals-admit-equality-but-not-business-correctness
  (let [changed (-> baseline
                    (assoc-in [:construction :replacements :request/copied]
                              [:literal 9])
                    (assoc-in [:construction :replacements :request/copied2]
                              [:literal 9])
                    (update-in [:construction :reads] disj :request/original))
        report (copy/check-copy-preservation! changed)]
    (is (= :checked-conditional-inference
           (:status (outcome report :request/target-target))))
    (is (= :open (:status (outcome report :request/target-source))))
    (is (= :not-established (:implementation-correspondence report)))))

(deftest a-formerly-framed-source-becoming-written-invalidates-equality
  (let [changed (-> baseline
                    (assoc-in [:construction :replacements :request/original]
                              [:literal 7])
                    (update-in [:construction :writes] conj :request/original)
                    (update-in [:construction :frame] disj :request/original))
        report (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome report :request/target-source))))
    (is (= :checked-conditional-inference
           (:status (outcome report :request/target-target))))
    (is (not= (:fingerprint (copy/check-copy-preservation! baseline))
              (:fingerprint report)))))

(deftest invalid-assignment-source-is-rejected-before-inference
  (let [changed (assoc-in baseline
                          [:construction :replacements :request/copied]
                          [:read :request/missing])]
    (rejects! #(copy/check-copy-preservation! changed)
              :gesso.assurance.proof.transition/invalid-transition
              :undeclared-assignment-reader)))

(deftest false-claim-is-not-accepted-as-source
  (rejects! #(copy/check-copy-preservation! (assoc baseline :proved true))
            obligation-error :wrong-keys))

(deftest inconsistent-concrete-post-state-is-rejected
  (let [transition (:transition (construction/build-guarded-replacement! operation))
        pre {:request/status :open :request/original 8
             :request/copied 0 :request/copied2 0 :request/version 2
             :request/actor-one :old :request/actor-two :old}
        good (assoc pre :request/status :claimed
                    :request/copied 8 :request/copied2 8
                    :request/actor-one :helper :request/actor-two :helper)
        input {:input/actor :helper}]
    (is (true? (:matches? (abstract/observe-transition! transition pre input good))))
    (is (false? (:matches? (abstract/observe-transition!
                           transition pre input
                           (assoc good :request/copied2 7)))))))

(deftest exact-report-classification-and-original-obligation-ledger
  (let [report (copy/check-copy-preservation! baseline)
        original (obligation/derive-obligations! baseline)
        checked (proof-ids [:request/target-source :request/target-target
                            :request/input-pair :request/mixed :request/frame
                            :request/optional-frame])
        unsupported (set/difference (proof-ids (keys rules)) checked)]
    (is (= copy/copy-preservation-version (:version report)))
    (is (= :same-total-origin-substitution-and-frame (:proof-rule report)))
    (is (= :declared-abstract-state-projection-only (:scope report)))
    (is (= :all-pre-invariants-and-matching-transition-imply-post-invariant
           (:claim report)))
    (is (= :original-sources-independent-equality-substitution-and-frame
           (:checking report)))
    (is (= :restricted-conditional-preservation-checked
           (:proof-status report)))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (= #{:proof-checker-substitution-and-frame-extensionality-soundness
             :certificate-admission} (:open-meta-obligations report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-issued (:certificate report)))
    (is (= (:fingerprint original) (:obligation-fingerprint report)))
    (is (= (:transition-fingerprint original)
           (:transition-fingerprint report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= checked (:conditionally-checked-preservation-ids report)))
    (is (= unsupported (:unsupported-preservation-ids report)))
    (is (= (proof-ids (keys rules))
           (set (map :obligation-id (:preservation-outcomes report)))))
    (is (= (count rules) (count (:preservation-outcomes report))))
    (is (empty? (set/intersection checked unsupported)))
    (is (= (:open-obligation-ids original)
           (:original-open-obligation-ids report)))
    (doseq [field [:valid? :full-success? :proved? :transaction-authority]]
      (is (not (contains? report field))))))

(deftest equality-substitution-is-a-real-three-rule-derivation
  (let [report (copy/check-copy-preservation! baseline)
        transition (:transition (construction/build-guarded-replacement! operation))
        a :request/copied
        b :request/original
        left :gesso.assurance.proof.copy-preservation/left
        right :gesso.assurance.proof.copy-preservation/right
        proof (get-in report [:equality-derivations [:request/target-source 0]])
        checked (derivation/check-transition-derivation!
                 {:transition transition
                  :steps [{:id left :rule :assignment :field a
                           :conclusion [:eq [:post a] [:pre b]]}
                          {:id right :rule :frame :field b
                           :conclusion [:eq [:post b] [:pre b]]}
                          {:id :gesso.assurance.proof.copy-preservation/equality
                           :rule :substitution :from [right left]
                           :side :right :direction :reverse
                           :conclusion [:eq [:post a] [:post b]]}]
                  :goal [:eq [:post a] [:post b]]})]
    (is (= (:fingerprint checked) (:derivation-fingerprint proof)))
    (is (= (:goal checked) (:conclusion proof)))
    (is (= :matching-abstract-transition-only (:scope proof)))
    (is (= :conditional-abstract-inference (:proof-status checked)))))

(deftest no-opaque-or-extra-authority-is-hidden-in-equality-proof-data
  (let [r (copy/check-copy-preservation! baseline)]
    (doseq [[_ proof] (:equality-derivations r)]
      (is (= :matching-abstract-transition-only (:scope proof)))
      (is (re-matches #"sha256:[0-9a-f]{64}"
                      (:derivation-fingerprint proof)))
      (is (= :eq (first (:conclusion proof))))
      (is (not (contains? proof :authority)))
      (is (not (contains? proof :certificate))))
    (is (= #{:request/version :request/note}
           (set (keys (:frame-derivations r)))))))

(deftest equality-is-symmetric-only-when-both-sides-share-the-same-origin
  (let [forward (:expression (get rules :request/target-source))
        reverse (assoc-in baseline [:invariants :request/target-source :expression]
                          [:= (nth forward 2) (nth forward 1)])
        result (copy/check-copy-preservation! reverse)]
    (is (= :checked-conditional-inference
           (:status (outcome result :request/target-source))))
    (is (= [:eq [:post :request/original] [:post :request/copied]]
           (get-in result [:equality-derivations [:request/target-source 0]
                           :conclusion])))
    (is (not= (:fingerprint (copy/check-copy-preservation! baseline))
              (:fingerprint result)))))

(deftest different-input-origins-must-remain-open
  (let [changed (-> baseline
                    (assoc-in [:construction :input-types :input/other]
                              {:type :keyword})
                    (update-in [:construction :reads] conj :input/other)
                    (assoc-in [:construction :replacements :request/actor-two]
                              [:read :input/other]))
        r (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome r :request/input-pair))))
    (is (= :no-supported-copy-equality-proof
           (get-in (outcome r :request/input-pair)
                   [:clause-judgments 0 :reason])))
    (is (not (contains? (:equality-derivations r) [:request/input-pair 0])))))

(deftest same-origin-does-not-prove-inequality
  (let [r (source-for :request/inequality
                      {:readers #{:request/copied :request/copied2}
                       :expression [:not= [:read :request/copied]
                                    [:read :request/copied2]]})
        o (outcome (copy/check-copy-preservation! r) :request/inequality)]
    (is (= :open (:status o)))
    (is (= :no-supported-copy-equality-proof
           (get-in o [:clause-judgments 0 :reason])))))

(deftest different-source-equality-does-not-become-true-by-sampling
  (let [report (copy/check-copy-preservation! baseline)
        o (outcome report :request/unsupported-origin)]
    (is (= :open (:status o)))
    (is (= :no-supported-copy-equality-proof
           (get-in o [:clause-judgments 0 :reason])))
    (is (nil? (get-in report
                      [:equality-derivations [:request/unsupported-origin 0]])))))

(deftest cross-field-arithmetic-implications-are-not-inferred
  (doseq [expression [[:>= [:read :request/copied] [:read :request/original]]
                      [:> [:read :request/copied] [:read :request/original]]
                      [:<= [:read :request/copied] [:read :request/original]]]]
    (let [r (source-for :request/cross-arithmetic
                        {:readers #{:request/copied :request/original}
                         :expression expression})
          o (outcome (copy/check-copy-preservation! r) :request/cross-arithmetic)]
      (is (= :open (:status o)) (pr-str expression))
      (is (= :no-supported-copy-equality-proof
             (get-in o [:clause-judgments 0 :reason]))))))

(deftest optional-source-reader-cannot-be-treated-as-total
  (let [changed (-> baseline
                    (assoc-in [:construction :replacements :request/copied]
                              [:read :request/note])
                    (assoc-in [:construction :state-types :request/copied]
                              {:type :string :nullable? true})
                    (update-in [:construction :reads] conj :request/note))]
    (rejects! #(copy/check-copy-preservation! changed)
              :gesso.assurance.proof.transition/invalid-transition
              :optional-assignment-source)))

(deftest frame-only-optional-presence-remains-a-scoped-proof
  (let [r (copy/check-copy-preservation! baseline)
        o (outcome r :request/optional-frame)]
    (is (= :checked-conditional-inference (:status o)))
    (is (= :frame-only-preservation (:inference o)))
    (is (= #{:request/note}
           (get-in o [:clause-judgments 0 :readers])))
    (is (= :frame (get-in o [:clause-judgments 0 :kind])))
    (is (= [:same-field :request/note]
           (get-in r [:frame-derivations :request/note :conclusion])))))

(deftest dependent-optional-predicate-is-not-reordered
  (let [r (copy/check-copy-preservation! baseline)
        o (outcome r :request/optional-dependent)]
    (is (= :open (:status o)))
    (is (= :not-independently-well-typed
           (get-in o [:clause-judgments 1 :reason])))
    (is (= [:frame :unsupported :copy-equality]
           (mapv :kind (:clause-judgments o))))))

(deftest nested-and-does-not-become-flat-proof
  (let [expr [:and
              [:= [:read :request/copied] [:read :request/original]]
              [:and [:= [:read :request/copied] [:read :request/copied2]]
               [:>= [:read :request/version] [:literal 0]]]]
        r (source-for :request/nested
                      {:readers #{:request/copied :request/original
                                  :request/version :request/copied2}
                       :expression expr})
        o (outcome (copy/check-copy-preservation! r) :request/nested)]
    (is (= :open (:status o)))
    (is (= [:copy-equality :unsupported]
           (mapv :kind (:clause-judgments o))))))

(deftest a-mixed-field-predicate-is-not-rewritten-into-independent-clauses
  (let [r (source-for :request/score-comparison
                      {:readers #{:request/copied :request/version}
                       :expression [:>= [:read :request/copied]
                                    [:read :request/version]]})
        o (outcome (copy/check-copy-preservation! r) :request/score-comparison)]
    (is (= :open (:status o)))
    (is (nil? (get-in (copy/check-copy-preservation! r)
                      [:equality-derivations [:request/score-comparison 0]])))))

(deftest reassigning-frame-origin-invalidates-copy-to-frame-proof
  (let [changed (-> baseline
                    (update-in [:construction :frame] disj :request/original)
                    (update-in [:construction :writes] conj :request/original)
                    (assoc-in [:construction :replacements :request/original]
                              [:literal 7]))
        r (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome r :request/target-source))))
    (is (not (contains? (:frame-derivations r) :request/original)))
    (is (not= (:fingerprint r)
              (:fingerprint (copy/check-copy-preservation! baseline))))))

(deftest modifying-one-assignment-removes-only-dependent-proof
  (let [changed (assoc-in baseline
                          [:construction :replacements :request/actor-two]
                          [:literal :nobody])
        report (copy/check-copy-preservation! changed)]
    (is (= :open (:status (outcome report :request/input-pair))))
    (is (= :checked-conditional-inference
           (:status (outcome report :request/target-source))))
    (is (= :checked-conditional-inference
           (:status (outcome report :request/target-target))))
    (is (not= (:fingerprint (copy/check-copy-preservation! baseline))
              (:fingerprint report)))))

(deftest changed-guard-changes-source-fingerprint
  (let [changed (assoc-in baseline [:construction :guard :expression]
                          [:in [:read :request/status]
                           [:literal #{:open :claimed}]])
        a (copy/check-copy-preservation! baseline)
        b (copy/check-copy-preservation! changed)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:transition-fingerprint a) (:transition-fingerprint b)))))

(deftest changed-initial-condition-invalidates-identity-without-proving-initial-state
  (let [changed (assoc baseline :initial-condition
                       {:readers #{:request/status}
                        :expression [:in [:read :request/status]
                                     [:literal #{:open :claimed}]]})
        a (copy/check-copy-preservation! baseline)
        b (copy/check-copy-preservation! changed)]
    (is (not= (:fingerprint a) (:fingerprint b)))
    (is (not= (:obligation-fingerprint a) (:obligation-fingerprint b)))
    (is (= (:conditionally-checked-preservation-ids a)
           (:conditionally-checked-preservation-ids b)))
    (is (contains? (:original-open-obligation-ids b) [:initial-state :request/target-source]))))

(deftest omitting-rule-keeps-completeness-assumption-open
  (let [changed (update baseline :invariants dissoc :request/input-pair)
        r (copy/check-copy-preservation! changed)
        a (copy/check-copy-preservation! baseline)]
    (is (not= (:fingerprint r) (:fingerprint a)))
    (is (not (contains? (:conditionally-checked-preservation-ids r)
                        [:preservation :request/input-pair])))
    (is (some #(and (vector? %) (= :assumption (first %)))
              (:original-open-obligation-ids r)))))

(deftest fabricated-checker-derivation-fields-are-rejected
  (let [original derivation/check-transition-derivation!]
    (doseq [[k v] [[:authority :transaction]
                   [:admission :admitted]
                   [:proof-status :proved]
                   [:certificate :issued]
                   [:open-premises #{}]
                   [:missing-obligations #{}]
                   [:conclusion-status :proved]
                   [:implementation-correspondence :established]]]
      (rejects! #(with-redefs [derivation/check-transition-derivation!
                              (fn [source]
                                (assoc (original source) k v))]
                  (copy/check-copy-preservation! baseline))
                copy-error :derivation-checker-contract-changed))))

(deftest fabricated-rule-type-contract-is-rejected
  (let [original rule-type/check-rule-type!]
    (rejects! #(with-redefs [rule-type/check-rule-type!
                            (fn [source types]
                              (assoc (original source types)
                                     :proof-status :proved))]
                (copy/check-copy-preservation! baseline))
              copy-error :rule-type-checker-contract-changed)))

(deftest forged-source-report-does-not-become-an-admitted-proof
  (let [r (copy/check-copy-preservation! baseline)]
    (doseq [claim [:proved :certified :admitted :transaction-authority :valid?]]
      (rejects! #(copy/check-copy-preservation! (assoc baseline claim true))
                obligation-error :wrong-keys))
    (rejects! #(copy/check-copy-preservation! r)
              obligation-error :wrong-keys)))

(deftest finite-samples-distinguish-observations-from-invariant-proofs
  (let [t (:transition (construction/build-guarded-replacement! operation))]
    (doseq [i [-10 0 1 23 9223372036854775807N]
            note [::absent nil "note"]]
      (let [base {:request/status :open
                  :request/original i
                  :request/copied -1
                  :request/copied2 -2
                  :request/version 0
                  :request/actor-one :old
                  :request/actor-two :past}
            pre (if (= note ::absent) base (assoc base :request/note note))
            post (assoc pre :request/status :claimed
                        :request/copied i :request/copied2 i
                        :request/actor-one :helper :request/actor-two :helper)
            inputs {:input/actor :helper}]
        (is (true? (:matches? (abstract/observe-transition! t pre inputs post))))
        (is (false? (:matches? (abstract/observe-transition!
                               t pre inputs (assoc post :request/copied (inc i))))))
        (is (false? (:matches? (abstract/observe-transition!
                               t pre inputs (assoc post :request/actor-two :wrong)))))))))

(deftest mutation-of-optional-frame-presence-rejected
  (let [t (:transition (construction/build-guarded-replacement! operation))
        base {:request/status :open :request/original 3
              :request/copied 1 :request/copied2 1 :request/version 4
              :request/actor-one :old :request/actor-two :old}
        post (assoc base :request/status :claimed
                    :request/copied 3 :request/copied2 3
                    :request/actor-one :helper :request/actor-two :helper)]
    (is (true? (:matches? (abstract/observe-transition!
                           t base {:input/actor :helper} post))))
    (is (false? (:matches? (abstract/observe-transition!
                           t base {:input/actor :helper}
                           (assoc post :request/note nil)))))))

(deftest matched-transition-is-not-authorization-or-xtdb-proof
  (let [r (copy/check-copy-preservation! baseline)]
    (is (= :not-independently-proved (:checker-soundness r)))
    (is (= :not-issued (:certificate r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-established (:implementation-correspondence r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :none (:authority r)))))
