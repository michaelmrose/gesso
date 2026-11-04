(ns gesso.assurance.proof.derivation-test
  "v1236: permanent adversarial tests for the v1235 CLOSED transition checker.

   These tests distinguish checked, conditional abstract derivations from
   arbitrary arithmetic/implication, assumptions, admission, and transaction
   authority. Positive derivations and finite relational witnesses guard
   against a checker that merely rejects all proofs. All proof inputs are
   original transition source data, not cached reports or alleged certificates."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.transition :as transition]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed}}
    :request/owner {:type :keyword :nullable? true}
    :request/version {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status :request/owner}
           :expression [:and
                        [:= [:read :request/status] [:literal :open]]
                        [:= [:read :request/owner] [:literal nil]]]}
   :reads #{:request/status :request/owner :input/actor}
   :writes #{:request/status :request/owner}
   :assignments {:request/status [:literal :claimed]
                 :request/owner [:read :input/actor]}})

(def ^:private guard-claim [:guard])
(def ^:private status-claim
  [:eq [:post :request/status] [:literal :claimed]])
(def ^:private owner-claim
  [:eq [:post :request/owner] [:input :input/actor]])
(def ^:private version-claim
  [:eq [:post :request/version] [:pre :request/version]])
(def ^:private note-claim [:same-field :request/note])

(defn- step [id rule conclusion & kvs]
  (merge {:id id :rule rule :conclusion conclusion} (apply hash-map kvs)))
(def ^:private guard-step (step :step/guard :guard guard-claim))
(def ^:private status-step
  (step :step/status :assignment status-claim :field :request/status))
(def ^:private owner-step
  (step :step/owner :assignment owner-claim :field :request/owner))
(def ^:private version-step
  (step :step/version :frame version-claim :field :request/version))
(def ^:private note-step
  (step :step/note :frame note-claim :field :request/note))

(defn- proof
  ([steps goal] (proof claim steps goal))
  ([spec steps goal] {:transition spec :steps (vec steps) :goal goal}))

(defn- checked! [source]
  (derivation/check-transition-derivation! source))

(defn- error-data [source]
  (try (checked! source) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects-as!
  "Require a specific checker rejection, not any exception or any typed error."
  [source reason]
  (let [error (error-data source)]
    (is (= :gesso.assurance.proof.derivation/invalid-derivation
           (:error/type error)) (pr-str error))
    (is (= reason (:reason error)) (pr-str error))
    (is (vector? (:at error)) (pr-str error))
    error))

(defn- conditional-evidence! [report source]
  (is (= :gesso.assurance.proof.derivation/unadmitted-checked-transition-derivation
         (:type report)))
  (is (= derivation/derivation-version (:version report)))
  (is (= :checked-abstract-transition-derivation (:evidence-class report)))
  (is (= (:fingerprint (transition/describe-transition! (:transition source)))
         (:transition-fingerprint report)))
  (is (= (:goal source) (:goal report)))
  (is (= (count (:steps source)) (:rule-count report)))
  (is (= #{:gesso.assurance.proof.derivation/abstract-relation-match}
         (:open-premises report)))
  (is (= #{} (:trusted-assumptions report)))
  (is (= #{:certificate-admission
           :production-transition-correspondence
           :current-runtime-authorization
           :atomic-transaction-enforcement}
         (:missing-obligations report)))
  (is (= :matching-abstract-transition-only (:claim-scope report)))
  (is (= :conditional-on-matching-abstract-relation
         (:conclusion-status report)))
  (is (= :conditional-abstract-inference (:proof-status report)))
  (is (= :not-performed (:assumption-discharge report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :not-established (:implementation-correspondence report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority)))
  (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
  report)

(deftest every-basic-rule-accepts-its-exact-conclusion
  (doseq [s [guard-step status-step owner-step version-step note-step]]
    (let [source (proof [s] (:conclusion s))
          report (conditional-evidence! (checked! source) source)]
      (is (= report (checked! source)))
      (is (= (:fingerprint report)
             (:fingerprint (checked! source)))))))

(deftest conjunction-can-only-combine-checked-premises
  (let [both [:and guard-claim status-claim owner-claim]
        source (proof [guard-step status-step owner-step
                       (step :step/all :conjunction-introduction both
                             :from [:step/guard :step/status :step/owner])]
                      both)
        report (conditional-evidence! (checked! source) source)]
    (is (= 4 (:rule-count report)))
    (let [select (step :step/select :conjunction-elimination owner-claim
                       :from [:step/all] :index 2)
          source* (proof (conj (:steps source) select) owner-claim)]
      (conditional-evidence! (checked! source*) source*))))

(deftest invented-frame-facts-never-follow-from-assignment-or-guard
  (doseq [s [(assoc version-step :field :request/status)
             (assoc note-step :field :request/owner)
             (assoc version-step :field :request/unknown)]]
    (rejects-as! (proof [s] (:conclusion s)) :not-a-frame-field))
  (rejects-as!
   (proof [(assoc version-step :conclusion
                  [:eq [:pre :request/version] [:literal 0]])]
          [:eq [:pre :request/version] [:literal 0]])
   :wrong-frame-conclusion)
  (rejects-as!
   (proof [(assoc note-step :conclusion
                  [:eq [:post :request/note] [:pre :request/note]])]
          [:eq [:post :request/note] [:pre :request/note]])
   :partial-equality-reader)
  (rejects-as!
   (proof [(assoc note-step :conclusion version-claim)] version-claim)
   :wrong-frame-conclusion))

(deftest forged-guard-and-assignment-conclusions-fail
  (doseq [conclusion [status-claim owner-claim version-claim]]
    (rejects-as! (proof [(assoc guard-step :conclusion conclusion)] conclusion)
                 :wrong-guard-conclusion))
  (doseq [[s bad] [[status-step
                    [:eq [:post :request/status] [:literal :open]]]
                   [owner-step
                    [:eq [:post :request/owner] [:literal :user/a]]]
                   [owner-step
                    [:eq [:post :request/owner] [:pre :request/owner]]]
                   [status-step owner-claim]]]
    (rejects-as! (proof [(assoc s :conclusion bad)] bad)
                 :wrong-assignment-conclusion))
  (rejects-as! (proof [(assoc status-step :field :request/version)] status-claim)
               :not-an-assigned-field))

(deftest arbitrary-arithmetic-and-implications-are-not-inference-rules
  (doseq [fabricated [[:implies guard-claim status-claim]
                     [:> [:post :request/version] [:literal 7]]
                     [:plus [:pre :request/version] [:literal 1]]
                     [:not guard-claim]
                     [:true]
                     [:atom {:expression [:> [:read :request/version]
                                           [:literal 7]]}]]]
    (rejects-as! (proof [guard-step] fabricated) :unknown-claim-kind)
    (rejects-as!
     (proof [(assoc guard-step :conclusion fabricated)] fabricated)
     :unknown-claim-kind))
  (rejects-as!
   (proof [guard-step
           (step :step/fake :implication-introduction
                 [:implies guard-claim status-claim]
                 :from [:step/guard])]
          [:implies guard-claim status-claim])
   :unknown-claim-kind))

(deftest conjunction-is-not-a-premise-factory
  (let [combined [:and guard-claim status-claim]]
    (rejects-as!
     (proof [guard-step
             (step :step/fake :conjunction-introduction combined
                   :from [:step/guard :step/guard])]
            combined)
     :wrong-conjunction-introduction)
    (rejects-as!
     (proof [guard-step
             (step :step/fake :conjunction-introduction combined
                   :from [:step/guard :step/missing])]
            combined)
     :unknown-or-forward-step)
    (rejects-as!
     (proof [guard-step
             (step :step/fake :conjunction-introduction combined
                   :from [:step/fake :step/guard])]
            combined)
     :unknown-or-forward-step)
    (rejects-as!
     (proof [guard-step status-step
             (step :step/fake :conjunction-introduction status-claim
                   :from [:step/guard :step/status])]
            status-claim)
     :wrong-conjunction-introduction)
    (rejects-as!
     (proof [guard-step status-step
             (step :step/all :conjunction-introduction combined
                   :from [:step/guard :step/status])
             (step :step/fake :conjunction-elimination guard-claim
                   :from [:step/all] :index 1)]
            guard-claim)
     :wrong-conjunction-elimination)))

(deftest conjunct-extraction-is-structurally-exact
  (let [combined [:and guard-claim status-claim]
        all (step :step/both :conjunction-introduction combined
                  :from [:step/guard :step/status])]
    (doseq [index [-1 2 100 :zero]]
      (rejects-as!
       (proof [guard-step status-step all
               (step :step/choose :conjunction-elimination guard-claim
                     :from [:step/both] :index index)] guard-claim)
       :invalid-conjunction-index))
    (rejects-as!
     (proof [guard-step
             (step :step/choose :conjunction-elimination guard-claim
                   :from [:step/guard] :index 0)] guard-claim)
     :source-not-conjunction)))

(def ^:private integer-spec
  {:subject {:kind :operation :id :example/copy-counter
             :interface {:operation :example/copy-counter}}
   :state-types {:counter/a {:type :integer}
                 :counter/b {:type :integer}
                 :counter/c {:type :integer}
                 :counter/optional {:type :integer :optional? true}}
   :input-types {}
   :guard {:readers #{} :expression [:literal true]}
   :reads #{:counter/a}
   :writes #{:counter/c}
   :assignments {:counter/c [:read :counter/a]}})

(def ^:private frame-a
  (step :step/a :frame [:eq [:post :counter/a] [:pre :counter/a]]
        :field :counter/a))
(def ^:private assign-c
  (step :step/c :assignment [:eq [:post :counter/c] [:pre :counter/a]]
        :field :counter/c))

(deftest whole-operand-substitution-does-not-invent-values
  (let [substituted [:eq [:post :counter/c] [:post :counter/a]]
        substitute (step :step/subst :substitution substituted
                         :from [:step/a :step/c] :side :right
                         :direction :reverse)
        source (proof integer-spec [frame-a assign-c substitute] substituted)]
    (conditional-evidence! (checked! source) source)
    (doseq [[change reason]
            [[{:side :left} :substitution-source-mismatch]
             [{:direction :forward} :substitution-source-mismatch]
             [{:conclusion [:eq [:post :counter/c] [:literal 99]]}
              :wrong-substitution-conclusion]
             [{:conclusion [:eq [:post :counter/c] [:pre :counter/b]]}
              :wrong-substitution-conclusion]
             [{:side :everywhere} :invalid-substitution-side]
             [{:direction :sideways} :invalid-substitution-direction]
             [{:from [:step/c :step/a]} :wrong-substitution-conclusion]]]
      (let [mutant (merge substitute change)]
        (rejects-as! (proof integer-spec [frame-a assign-c mutant]
                            (:conclusion mutant)) reason)))))

(deftest substitution-requires-real-equality-evidence
  (let [bad (step :step/subst :substitution version-claim
                  :from [:step/guard :step/version] :side :left
                  :direction :forward)]
    (rejects-as! (proof [guard-step version-step bad] version-claim)
                 :substitution-requires-equalities))
  (rejects-as!
   (proof integer-spec [frame-a assign-c
                        (step :step/subst :substitution
                              [:eq [:post :counter/c] [:post :counter/a]]
                              :from [:step/a :step/unknown] :side :right
                              :direction :reverse)]
          [:eq [:post :counter/c] [:post :counter/a]])
   :unknown-or-forward-step))

(deftest optional-readers-never-become-total-equalities
  (let [optional-equality [:eq [:post :counter/optional]
                           [:pre :counter/optional]]]
    (rejects-as! (proof integer-spec
                        [(step :step/optional :frame optional-equality
                               :field :counter/optional)] optional-equality)
                 :partial-equality-reader)
    (let [source (proof integer-spec
                        [(step :step/optional :frame
                               [:same-field :counter/optional]
                               :field :counter/optional)]
                        [:same-field :counter/optional])]
      (conditional-evidence! (checked! source) source)))
  (let [optional-target
        (-> integer-spec
            (assoc :reads #{:counter/a}
                   :writes #{:counter/c :counter/optional}
                   :assignments {:counter/c [:read :counter/a]
                                 :counter/optional [:literal 3]}))
        optional-written [:eq [:post :counter/optional] [:literal 3]]
        s (step :step/write-optional :assignment optional-written
                :field :counter/optional)
        source (proof optional-target [s] optional-written)]
    (conditional-evidence! (checked! source) source)))

(deftest proof-graph-has-no-forward-duplicate-or-unused-steps
  (rejects-as! (proof [guard-step (assoc guard-step :id :step/guard)] guard-claim)
               :duplicate-step-id)
  (rejects-as! (proof [guard-step status-step] status-claim)
               :unused-proof-steps)
  (rejects-as!
   (proof [guard-step
           (step :step/both :conjunction-introduction
                 [:and guard-claim guard-claim]
                 :from [:step/guard :step/future])
           (assoc guard-step :id :step/future)]
          guard-claim)
   :unknown-or-forward-step)
  (rejects-as!
   (proof [guard-step] status-claim)
   :wrong-final-conclusion))

(deftest invented-inference-rules-fail-even-with-a-valid-goal
  ;; A fabricated rule must not be accepted merely because its conclusion
  ;; happens to be syntactically valid (unlike an invalid implication goal).
  (doseq [rule [:arithmetic :implication-introduction :premise
                :assume :truth-introduction :certificate-admission]]
    (rejects-as!
     (proof [(step :step/fake rule guard-claim)] guard-claim)
     :unknown-rule)))

(deftest report-and-proof-inputs-never-become-authority
  (let [base (proof [status-step] status-claim)
        report (conditional-evidence! (checked! base) base)]
    (doseq [[key payload] [[:valid? true]
                           [:certificate {:valid? true}]
                           [:admission :admitted]
                           [:proof-status :proved]
                           [:authority :transaction]
                           [:fingerprint (:fingerprint report)]
                           [:open-premises #{}]]]
      (rejects-as! (assoc base key payload) :wrong-keys)
      (rejects-as! (assoc-in base [:steps 0 key] payload) :wrong-keys))
    (rejects-as! (assoc base :steps report) :invalid-steps)
    (rejects-as! (assoc base :goal report) :expected-claim-vector)))

(deftest malformed-proof-structures-fail-closed
  (let [base (proof [guard-step] guard-claim)]
    (rejects-as! (assoc base :steps []) :invalid-steps)
    (rejects-as! (assoc base :steps (list guard-step)) :invalid-steps)
    (rejects-as! (assoc base :steps (vec (repeat 257 guard-step))) :invalid-steps)
    (rejects-as! (assoc base :goal [:guard :extra]) :invalid-guard-claim)
    (rejects-as! (assoc base :goal [:same-field :request/absent])
                 :invalid-frame-claim)
    (rejects-as! (assoc base :goal [:eq [:post :request/version]
                                   [:pre :request/unknown]])
                 :unknown-equality-reader)
    (rejects-as! (assoc base :goal [:eq [:post :request/version]
                                   [:literal (Object.)]])
                 :invalid-literal)
    (rejects-as! (assoc base :goal [:and guard-claim])
                 :invalid-conjunction-arity)
    (rejects-as! (with-meta base {:claimed-proof true}) :metadata-not-allowed)
    (rejects-as! (assoc base :steps (with-meta [guard-step] {:trusted true}))
                 :metadata-not-allowed)
    (rejects-as! (assoc base :goal (with-meta guard-claim {:trusted true}))
                 :metadata-not-allowed)
    (rejects-as! (proof [(assoc guard-step :id :unqualified)] guard-claim)
                 :invalid-step-id)))

(deftest invalid-original-transitions-cannot-be-laundered-through-proof
  (let [base (proof [guard-step] guard-claim)]
    ;; These should be rejected by the original v1233 checker, not given an
    ;; abstract proof because somebody supplied a plausible-looking step.
    (doseq [[changed error-type reason]
            [[(assoc-in claim [:assignments :request/status]
                        [:literal :nonexistent])
              :gesso.assurance.rule-type/invalid-rule-type
              :enum-literal-outside-domain]
             [(assoc claim :writes #{:request/status})
              :gesso.assurance.proof.transition/invalid-transition
              :write-footprint-mismatch]
             [(assoc claim :reads #{:request/status})
              :gesso.assurance.proof.transition/invalid-transition
              :read-footprint-mismatch]]]
      (let [error (error-data (assoc base :transition changed))]
        (is (= error-type (:error/type error)) (pr-str error))
        (is (= reason (:reason error)) (pr-str error))
        (is (vector? (:at error)) (pr-str error))))))

(deftest derivation-fingerprint-tracks-the-entire-current-proof
  (let [base (proof [version-step] version-claim)
        fingerprint (:fingerprint (checked! base))
        alternate (proof [(step :step/relabel :frame version-claim
                                :field :request/version)] version-claim)
        different-transition (assoc-in base
                                       [:transition :subject :id]
                                       :example/other-operation)]
    (is (not= fingerprint (:fingerprint (checked! alternate))))
    (is (not= fingerprint (:fingerprint (checked! different-transition))))
    (is (not= (:transition-fingerprint (checked! base))
              (:transition-fingerprint (checked! different-transition))))
    (is (= fingerprint (:fingerprint (checked! base))))))

(deftest finite-witnesses-check-the-relation-not-an-unconditional-theorem
  (let [source (proof [guard-step status-step version-step
                       (step :step/all :conjunction-introduction
                             [:and guard-claim status-claim version-claim]
                             :from [:step/guard :step/status :step/version])]
                      [:and guard-claim status-claim version-claim])
        report (checked! source)]
    (conditional-evidence! report source)
    (doseq [status [:open :claimed]
            owner [nil :user/other]
            version [-3 0 9]
            note [::absent nil "note"]
            actor [:user/a :user/b]]
      (let [before (cond-> {:request/status status
                            :request/owner owner
                            :request/version version}
                     (not= note ::absent) (assoc :request/note note))
            after (assoc before :request/status :claimed
                         :request/owner actor)
            observation (transition/observe-transition!
                         claim before {:input/actor actor} after)
            expected (and (= status :open) (nil? owner))]
        (is (= expected (:matches? observation)))
        (when (:matches? observation)
          (is (= :claimed (:request/status after)))
          (is (= (:request/version before) (:request/version after)))
          (is (= (contains? before :request/note)
                 (contains? after :request/note))))))
    ;; Even though a checkable proof exists for all MATCHING transitions,
    ;; the same state update does not match if its prestate violates guard.
    (is (= :conditional-on-matching-abstract-relation
           (:conclusion-status report)))))
