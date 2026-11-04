(ns gesso.assurance.proof.compose-test
  "Permanent v1246 adversarial tests for v1245 conditional composition.

   Test both source-level structural compatibility and ACTUAL two-step
   relational observations. In particular, exact intermediate-state equality
   must not be weakened to equality of declared writes: framed fields and
   optional-field presence matter just as much. A valid two-step observation
   establishes neither an induction theorem nor permission to commit.

   :atomic is only a grouping REQUEST. A v1245 composition never supplies
   physical single-commit atomicity, runtime authorization, an admitted
   certificate, a complete application invariant, or XTDB correspondence."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.compose :as compose]
            [gesso.assurance.proof.elaborate :as elaborate]))

(def ^:private composition-error
  :gesso.assurance.proof.compose/invalid-composition)

(def ^:private types
  {:request/status {:type :enum :name :request/status
                    :values #{:open :claimed :done}}
   :request/owner {:type :keyword :optional? true}
   :request/version {:type :integer}
   :request/note {:type :string :optional? true :nullable? true}})

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types types
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})

(def ^:private complete
  {:subject {:kind :operation :id :example/complete
             :interface {:operation :example/complete}}
   :state-types types
   :input-types {}
   :guard {:readers #{:request/status :request/owner}
           :expression [:and
                        [:= [:read :request/status] [:literal :claimed]]
                        [:present [:read :request/owner]]]}
   :reads #{:request/status :request/owner}
   :writes #{:request/status}
   :replacements {:request/status [:literal :done]}
   :frame #{:request/owner :request/version :request/note}})

(def ^:private initial-claim
  {:readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :open]]})

(def ^:private initial-complete
  {:readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :claimed]]})

(def ^:private invariants
  {:request/valid-status
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/nonnegative-version
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/claimed-has-owner
   {:readers #{:request/status :request/owner}
    :expression [:implies
                 [:= [:read :request/status] [:literal :claimed]]
                 [:present [:read :request/owner]]]}})

(def ^:private assumptions #{:app/shared-request-binding})

(def ^:private source
  {:mode :sequential
   :subject {:kind :choreography :id :example/claim-complete
             :interface {:resource :request/entity
                         :steps [:example/claim :example/complete]}}
   :resource :request/entity
   :assumptions assumptions
   :steps [{:resource :request/entity :assumptions assumptions
            :source {:construction claim
                     :initial-condition initial-claim
                     :invariants invariants}}
           {:resource :request/entity :assumptions assumptions
            :source {:construction complete
                     :initial-condition initial-complete
                     :invariants invariants}}]})

(def ^:private witness
  {:first {:before {:request/status :open :request/version 7}
           :inputs {:input/actor :helper/one}
           :after {:request/status :claimed :request/owner :helper/one
                   :request/version 7}}
   :second {:before {:request/status :claimed :request/owner :helper/one
                     :request/version 7}
            :inputs {}
            :after {:request/status :done :request/owner :helper/one
                    :request/version 7}}})

(defn- error-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject-as! [f reason]
  (let [data (error-data f)]
    (is (map? data) (str "Expected a typed rejection for " reason))
    (is (= composition-error (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    (is (vector? (:at data)) (pr-str data))
    data))

(defn- reject-source! [s reason]
  (reject-as! #(compose/compose-transitions! s) reason))

(defn- reject-witness!
  ([w reason] (reject-witness! source w reason))
  ([s w reason]
   (reject-as! #(compose/check-composed-witness! s w) reason)))

(defn- check-no-authority! [report]
  (is (= :not-established (:invariant-preservation report)))
  (is (= :not-established (:implementation-correspondence report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :valid?)))
  (is (not (contains? report :transaction-authority))))

(deftest two-original-transitions-compose-conditionally
  (let [report (compose/compose-transitions! source)]
    (is (= compose/report-type (:type report)))
    (is (= compose/composition-version (:version report)))
    (is (= source (:source report)))
    (is (= :sequential (:mode report)))
    (is (= :request/entity (:resource report)))
    (is (= assumptions (:declared-assumptions report)))
    (is (= 2 (count (:components report))))
    (is (= #{:request/status :request/owner} (-> report :components first :writes)))
    (is (= #{:request/status :request/version :request/owner :request/note}
           (:state-projection report)))
    (is (= :structurally-compatible-unadmitted (:composition-status report)))
    (is (= :conditional-abstract-composition-only (:proof-status report)))
    (is (= :exists-intermediate-with-two-matching-original-relations
           (:relation report)))
    (is (= :not-claimed (:atomicity report)))
    (is (= (:open-obligation-count report)
           (count (:open-obligation-ids report))))
    (check-no-authority! report)))

(deftest components-are-rechecked-not-trusted-from-copied-reports
  (let [report (compose/compose-transitions! source)]
    (doseq [[index component] (map-indexed vector (:components report))]
      (testing (str "original operation " index)
        (is (= index (:index component)))
        (is (= :request/entity (:resource component)))
        (is (= assumptions (:assumptions component)))
        (is (pos? (:checked-facts component)))
        (is (re-matches #"sha256:[0-9a-f]{64}" (:transition-fingerprint component)))
        (is (re-matches #"sha256:[0-9a-f]{64}" (:elaboration-fingerprint component)))
        (is (re-matches #"sha256:[0-9a-f]{64}" (:obligation-fingerprint component)))
        (is (not (contains? component :certificate)))))
    (is (= :independent-component-derivations-and-structural-compatibility
           (:checking report)))))

(deftest all-composition-and-step-obligations-remain-open
  (let [ids (:open-obligation-ids (compose/compose-transitions! source))]
    (doseq [id [[:composition :resource-aliasing]
                [:composition :assumption-validity]
                [:composition :intermediate-state-refinement]
                [:composition :second-guard-enablement]
                [:composition :composed-invariant-preservation]
                [:composition :production-order-correspondence]
                [:declared-assumption :app/shared-request-binding]
                [:step 0 [:preservation :request/valid-status]]
                [:step 1 [:preservation :request/claimed-has-owner]]
                [:step 0 [:assumption :declared-invariant-set-coverage]]
                [:step 1 [:implementation :atomic-transaction-enforcement]]]]
      (is (contains? ids id) (pr-str id)))
    (is (not (contains? ids [:composition :single-atomic-commit])))))

(deftest exact-valid-witness-is-an-observation-only
  (let [report (compose/check-composed-witness! source witness)]
    (is (= compose/witness-type (:type report)))
    (is (= compose/composition-version (:version report)))
    (is (= true (:matches? report)))
    (is (= :observed-exact (:intermediate-equality report)))
    (is (= :both-original-abstract-relations-observed (:checking report)))
    (is (= :one-abstract-two-step-witness-only (:claim-scope report)))
    (is (= :not-proved (:proof-status report)))
    (is (= :not-established (:atomicity report)))
    (is (= 2 (count (:component-transition-fingerprints report))))
    (is (= (:fingerprint (compose/compose-transitions! source))
           (:composition-fingerprint report)))
    (check-no-authority! report)))

(deftest optional-framed-field-can-be-absent-null-or-present
  (doseq [value [::absent nil "memo"]]
    (let [w (if (= ::absent value) witness
                (-> witness
                    (assoc-in [:first :before :request/note] value)
                    (assoc-in [:first :after :request/note] value)
                    (assoc-in [:second :before :request/note] value)
                    (assoc-in [:second :after :request/note] value)))
          result (compose/check-composed-witness! source w)]
      (is (:matches? result))
      (is (= :observed-exact (:intermediate-equality result)))
      (is (= :not-proved (:proof-status result))))))

(deftest matched-intermediate-values-are-not-enough-with-different-presence
  ;; Both component relations individually preserve their own :request/note.
  ;; Their middle maps differ only by the PRESENCE of an optional nil field.
  (let [w (-> witness
              (assoc-in [:second :before :request/note] nil)
              (assoc-in [:second :after :request/note] nil))]
    (reject-witness! w :intermediate-state-mismatch)))

(deftest matched-intermediate-written-fields-are-not-enough
  ;; Status and owner match; version is framed by BOTH operations, but the
  ;; caller cannot splice two different versions into the middle.
  (let [w (-> witness
              (assoc-in [:second :before :request/version] 8)
              (assoc-in [:second :after :request/version] 8))]
    (reject-witness! w :intermediate-state-mismatch)))

(deftest mismatched-optional-value-rejects-composition
  (let [w (-> witness
              (assoc-in [:first :before :request/note] "A")
              (assoc-in [:first :after :request/note] "A")
              (assoc-in [:second :before :request/note] "B")
              (assoc-in [:second :after :request/note] "B"))]
    (reject-witness! w :intermediate-state-mismatch)))

(deftest first-step-replacement-fabrication-is-rejected
  (let [w (-> witness
              (assoc-in [:first :after :request/owner] :helper/two)
              (assoc-in [:second :before :request/owner] :helper/two)
              (assoc-in [:second :after :request/owner] :helper/two))]
    (let [data (reject-witness! w :nonmatching-abstract-component)]
      (is (= 0 (:index data)))
      (is (false? (:assignments-hold? data))))))

(deftest second-step-replacement-fabrication-is-rejected
  (let [data (reject-witness! (assoc-in witness [:second :after :request/status] :claimed)
                              :nonmatching-abstract-component)]
    (is (= 1 (:index data)))
    (is (false? (:assignments-hold? data)))))

(deftest frame-interference-on-first-step-is-not-authorized
  (let [w (-> witness
              (assoc-in [:first :after :request/version] 8)
              (assoc-in [:second :before :request/version] 8)
              (assoc-in [:second :after :request/version] 8))
        data (reject-witness! w :nonmatching-abstract-component)]
    (is (= 0 (:index data)))
    (is (false? (:frame-holds? data)))))

(deftest frame-interference-on-second-step-is-not-authorized
  (let [data (reject-witness! (assoc-in witness [:second :after :request/version] 8)
                              :nonmatching-abstract-component)]
    (is (= 1 (:index data)))
    (is (false? (:frame-holds? data)))))

(deftest optional-frame-presence-cannot-be-invented-by-second-step
  (let [w (assoc-in witness [:second :after :request/note] nil)
        data (reject-witness! w :nonmatching-abstract-component)]
    (is (= 1 (:index data)))
    (is (false? (:frame-holds? data)))))

(deftest optional-frame-presence-cannot-be-erased-by-first-step
  (let [w (assoc-in witness [:first :before :request/note] nil)
        data (reject-witness! w :nonmatching-abstract-component)]
    (is (= 0 (:index data)))
    (is (false? (:frame-holds? data)))))

(deftest second-guard-is-checked-not-inferred-from-structural-compatibility
  (let [stricter (assoc-in source [:steps 1 :source :construction :guard :expression]
                           [:and [:= [:read :request/status] [:literal :open]]
                            [:present [:read :request/owner]]])
        report (compose/compose-transitions! stricter)
        data (reject-witness! stricter witness :nonmatching-abstract-component)]
    (is (= :not-established (get-in report [:intermediate-contract :guard-enablement])))
    (is (contains? (:open-obligation-ids report)
                   [:composition :second-guard-enablement]))
    (is (= 1 (:index data)))
    (is (false? (:guard-holds? data)))))

(deftest second-initial-condition-is-not-inferred-as-an-intermediate-fact
  ;; Deliberately contradictory to the actual intermediate, but the source
  ;; still defines a compatible two-step RELATION, not a theorem of initiality.
  (let [changed (assoc-in source [:steps 1 :source :initial-condition :expression]
                          [:= [:read :request/status] [:literal :done]])
        report (compose/compose-transitions! changed)
        observed (compose/check-composed-witness! changed witness)]
    (is (= :not-inferred (get-in report [:intermediate-contract :second-initial-condition])))
    (is (contains? (:open-obligation-ids report)
                   [:composition :intermediate-state-refinement]))
    (is (:matches? observed))
    (is (= :not-proved (:proof-status observed)))))

(deftest different-state-projections-must-not-compose
  (doseq [path [[:steps 1 :source :construction :state-types :request/owner :optional?]
                [:steps 1 :source :construction :state-types :request/note :nullable?]]]
    (reject-source! (assoc-in source path false)
                    :intermediate-state-type-mismatch)))

(deftest same-fields-with-different-enum-meaning-do-not-compose
  (let [changed (assoc-in source
                          [:steps 1 :source :construction :state-types
                           :request/status :values]
                          #{:open :claimed :done :archived})]
    (reject-source! changed :intermediate-state-type-mismatch)))

(deftest same-state-types-but-different-invariant-meaning-do-not-compose
  (let [changed (assoc-in source
                          [:steps 1 :source :invariants
                           :request/nonnegative-version :expression]
                          [:>= [:read :request/version] [:literal 1]])]
    (reject-source! changed :invariant-meaning-mismatch)))

(deftest omitted-invariant-cannot-silently-disappear-from-one-step
  (reject-source! (update-in source [:steps 1 :source :invariants]
                             dissoc :request/claimed-has-owner)
                  :invariant-meaning-mismatch))

(deftest mismatched-resource-assumptions-and-subjects-reject
  (doseq [[label modified reason]
          [[:resource (assoc-in source [:steps 0 :resource] :request/other)
            :resource-mismatch]
           [:resource-interface (assoc-in source [:subject :interface :resource]
                                          :request/other)
            :composition-subject-mismatch]
           [:operation-order (assoc-in source [:subject :interface :steps]
                                       [:example/complete :example/claim])
            :composition-subject-mismatch]
           [:step-assumption (assoc-in source [:steps 1 :assumptions] #{})
            :assumptions-mismatch]
           [:invalid-subject-kind (assoc-in source [:subject :kind] :operation)
            :invalid-composition-subject-kind]
           [:wrong-step-subject-kind (assoc-in source
                                              [:steps 0 :source :construction :subject :kind]
                                              :rule)
            :invalid-step-subject-kind]]]
    (testing (name label) (reject-source! modified reason))))

(deftest closed-source-syntax-excludes-forged-atomicity-and-authority
  (doseq [extra [:atomicity :valid? :certificate :admission :proof-status
                 :runtime-enforcement :transaction-authority :committed?]]
    (testing (name extra)
      (reject-source! (assoc source extra true) :wrong-keys)))
  (doseq [extra [:valid? :certificate :atomic :authority :transaction-authority]]
    (testing (name extra)
      (reject-source! (update-in source [:steps 0] assoc extra true) :wrong-keys))))

(deftest malformed-composition-shapes-reject-before-nested-checking
  (doseq [[label modified reason]
          [[:nil nil :expected-plain-map]
           [:empty {} :wrong-keys]
           [:not-a-map [] :expected-plain-map]
           [:not-two-steps (assoc source :steps (subvec (:steps source) 0 1))
            :expected-two-steps]
           [:three-steps (assoc source :steps (conj (:steps source)
                                                   (first (:steps source))))
            :expected-two-steps]
           [:list-steps (assoc source :steps (seq (:steps source)))
            :expected-two-steps]
           [:invalid-mode (assoc source :mode :parallel) :invalid-mode]
           [:unqualified-resource (assoc source :resource :request)
            :invalid-resource]
           [:unqualified-assumption (assoc source :assumptions #{:assumed})
            :invalid-assumption-id]
           [:not-assumption-set (assoc source :assumptions [])
            :invalid-assumption-set]
           [:non-map-step (assoc-in source [:steps 0] [])
            :expected-plain-map]
           [:metadata (with-meta source {:valid? true})
            :metadata-not-allowed]
           [:steps-metadata (assoc source :steps (with-meta (:steps source)
                                                         {:valid? true}))
            :metadata-not-allowed]]]
    (testing (name label) (reject-source! modified reason))))

(deftest witnessed-sources-cannot-contain-admission-claims
  (doseq [extra [:valid? :certificate :transaction-authority :proof-status]]
    (let [bad (assoc-in source [:steps 1 :source :construction extra] true)
          data (error-data #(compose/check-composed-witness! bad witness))]
      (is (= :gesso.assurance.transition/invalid-construction (:error/type data))
          (pr-str data))
      (is (= :wrong-keys (:reason data)) (pr-str data)))))

(deftest witness-input-rejects-extra-claims-and-malformed-shapes
  (doseq [[label malformed reason]
          [[:extra-root (assoc witness :certified true) :wrong-keys]
           [:missing-root (dissoc witness :second) :wrong-keys]
           [:extra-step (assoc-in witness [:first :committed] true) :wrong-keys]
           [:missing-observation (update witness :first dissoc :after) :wrong-keys]
           [:wrong-observation (assoc witness :first []) :expected-plain-map]
           [:root-metadata (with-meta witness {:valid? true})
            :metadata-not-allowed]
           [:step-metadata (assoc witness :second
                                  (with-meta (:second witness) {:valid? true}))
            :metadata-not-allowed]]]
    (testing (name label) (reject-witness! malformed reason))))

(deftest malformed-values-are-validated-by-the-original-abstract-interpreter
  (doseq [bad ["not-a-version" nil :bad-version]]
    (let [modified (assoc-in witness [:second :before :request/version] bad)
          data (error-data #(compose/check-composed-witness! source modified))]
      (is (map? data) (pr-str bad))
      ;; The original v1233 typed interpreter must reject before comparing
      ;; the two intermediate maps. No arbitrary user .equals invocation.
      (is (not= composition-error (:error/type data)) (pr-str data)))))

(deftest modifying-source-facts-changes-structural-identity
  (let [base (:fingerprint (compose/compose-transitions! source))
        modified-invariant
        (-> source
            (assoc-in [:steps 0 :source :invariants
                       :request/nonnegative-version :expression]
                      [:>= [:read :request/version] [:literal 1]])
            (assoc-in [:steps 1 :source :invariants
                       :request/nonnegative-version :expression]
                      [:>= [:read :request/version] [:literal 1]]))
        changed
        [(assoc source :mode :atomic)
         (assoc-in source [:steps 0 :source :construction :guard :expression]
                   [:= [:read :request/status] [:literal :claimed]])
         (assoc-in source [:steps 1 :source :initial-condition :expression]
                   [:= [:read :request/status] [:literal :done]])
         modified-invariant
         (-> source
             (assoc :assumptions #{:app/new-binding})
             (assoc-in [:steps 0 :assumptions] #{:app/new-binding})
             (assoc-in [:steps 1 :assumptions] #{:app/new-binding}))]]
    (is (= base (:fingerprint (compose/compose-transitions! source))))
    (doseq [s changed]
      (is (not= base (:fingerprint (compose/compose-transitions! s)))))))

(deftest atomic-request-increases-obligations-but-does-not-prove-atomicity
  (let [sequential (compose/compose-transitions! source)
        atomic-source (assoc source :mode :atomic)
        atomic (compose/compose-transitions! atomic-source)
        observed (compose/check-composed-witness! atomic-source witness)
        additions #{[:composition :single-atomic-commit]
                    [:composition :intermediate-state-invisibility]}]
    (is (= :requested-not-established (:atomicity atomic)))
    (is (= :not-established (:atomicity observed)))
    (is (= (set/union (:open-obligation-ids sequential) additions)
           (:open-obligation-ids atomic)))
    (is (= (+ 2 (:open-obligation-count sequential))
           (:open-obligation-count atomic)))
    (is (not= (:fingerprint sequential) (:fingerprint atomic)))
    (is (= :not-proved (:proof-status observed)))
    (check-no-authority! atomic)
    (check-no-authority! observed)))

(deftest independently-rejected-proof-output-cannot-be-smuggled-into-composition
  (let [original elaborate/elaborate-transition!]
    ;; A deliberately buggy proof generator mutates the reported goal AFTER
    ;; the independent v1235 checker has checked the original source. The
    ;; composer must reject the disagreement instead of trusting the report.
    (with-redefs [elaborate/elaborate-transition!
                  (fn [s]
                    (update (original s) :fact-proofs
                            (fn [facts]
                              (assoc-in facts [0 :goal]
                                        [:eq [:post :request/version]
                                         [:literal 999]]))))]
      (reject-source! source :independent-proof-contract-changed))))

(deftest changed-proof-checker-contract-must-not-be-silently-ignored
  (let [original elaborate/elaborate-transition!]
    (with-redefs [elaborate/elaborate-transition!
                  (fn [s]
                    (assoc (original s) :proof-status :certified))]
      (reject-source! source :component-contract-changed))))

(deftest successful-atomic-witness-never-becomes-transaction-authority
  (let [s (assoc source :mode :atomic)
        r (compose/check-composed-witness! s witness)]
    (is (= true (:matches? r)))
    (is (= :not-proved (:proof-status r)))
    (is (= :not-established (:atomicity r)))
    (is (= :not-admitted (:admission r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))
    (is (nil? (:tx-ops r)))
    (is (nil? (:committed? r)))))
