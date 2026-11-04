(ns gesso.assurance.proof.preservation-test
  "Permanent adversarial coverage for v1259's RESTRICTED frame-preservation
   inference. This suite checks independently derived :same-field facts, exact
   written/framed reader sets, typed optional-field presence, stale source
   identities, fail-closed checker contracts, and the boundaries of a
   conditional ABSTRACT rule. A reported inference is never an admitted
   certificate or proof of actual XTDB execution (v1260)."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.preservation :as preservation]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.transition :as construction]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
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

(def ^:private initial
  {:readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :open]]})

(def ^:private rules
  {:request/nonnegative-version
   {:readers #{:request/version}
    :expression [:>= [:read :request/version] [:literal 0]]}
   :request/note-present
   {:readers #{:request/note}
    :expression [:present [:read :request/note]]}
   :request/note-or-version
   {:readers #{:request/note :request/version}
    :expression [:or [:present [:read :request/note]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/status-domain
   {:readers #{:request/status}
    :expression [:in [:read :request/status]
                 [:literal #{:open :claimed :done}]]}
   :request/status-and-version
   {:readers #{:request/status :request/version}
    :expression [:and
                 [:in [:read :request/status]
                  [:literal #{:open :claimed :done}]]
                 [:>= [:read :request/version] [:literal 0]]]}
   :request/claimed-needs-owner
   {:readers #{:request/status :request/owner}
    :expression [:implies
                 [:= [:read :request/status] [:literal :claimed]]
                 [:present [:read :request/owner]]]}
   :request/constant-true {:readers #{} :expression [:literal true]}
   :request/constant-false {:readers #{} :expression [:literal false]}})

(def ^:private baseline
  {:construction claim :initial-condition initial :invariants rules})

(def ^:private supported
  #{:request/nonnegative-version :request/note-present
    :request/note-or-version :request/constant-true :request/constant-false})
(def ^:private unsupported
  #{:request/status-domain :request/status-and-version
    :request/claimed-needs-owner})
(def ^:private error-preservation
  :gesso.assurance.proof.preservation/invalid-preservation)
(def ^:private error-obligation
  :gesso.assurance.proof.obligation/invalid-obligation-source)
(def ^:private error-construction
  :gesso.assurance.transition/invalid-construction)
(def ^:private premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private checker-open
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})

(defn- verify! [source]
  (preservation/check-frame-preservation! source))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  [f expected-type expected-reason]
  (let [err (failure f)]
    (is (map? err) (str "Expected " expected-type "/" expected-reason))
    (is (= expected-type (:error/type err)) (pr-str err))
    (is (= expected-reason (:reason err)) (pr-str err))
    err))

(defn- by-invariant [report]
  (into {} (map (juxt :invariant identity)) (:preservation-outcomes report)))

(defn- conditional-ids [ids]
  (set (map (fn [id] [:preservation id]) ids)))

(defn- proof-source [source field]
  {:transition (:transition (construction/build-guarded-replacement!
                             (:construction source)))
   :steps [{:id :gesso.assurance.proof.preservation/frame
            :rule :frame
            :field field
            :conclusion [:same-field field]}]
   :goal [:same-field field]})

(defn- assert-unadmitted! [report source]
  (let [original (obligation/derive-obligations! source)]
    (is (= preservation/report-type (:type report)))
    (is (= preservation/preservation-version (:version report)))
    (is (= (:fingerprint original) (:obligation-fingerprint report)))
    (is (= (:transition-fingerprint original)
           (:transition-fingerprint report)))
    (is (= (:open-obligation-ids original)
           (:original-open-obligation-ids report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint report)))
    (is (= :declared-abstract-state-projection-only (:scope report)))
    (is (= :all-pre-invariants-and-matching-transition-imply-post-invariant
           (:claim report)))
    (is (= :original-sources-and-independent-frame-derivations
           (:checking report)))
    (is (= :restricted-conditional-preservation-checked (:proof-status report)))
    (is (= :not-independently-proved (:checker-soundness report)))
    (is (= #{:proof-checker-and-extensionality-rule-soundness
             :certificate-admission}
           (:open-meta-obligations report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :full-success?)))
    (is (not (contains? report :transaction-authority)))
    report))

(deftest complete-frame-coverage-is-disjoint-and-source-derived
  (let [result (assert-unadmitted! (verify! baseline) baseline)
        actual (by-invariant result)]
    (is (= (set (keys rules)) (set (keys actual))))
    (is (= (count rules) (count (:preservation-outcomes result))))
    (is (= (conditional-ids supported)
           (:conditionally-checked-preservation-ids result)))
    (is (= (conditional-ids unsupported)
           (:unsupported-preservation-ids result)))
    (is (= #{} (set/intersection (:conditionally-checked-preservation-ids result)
                                 (:unsupported-preservation-ids result))))
    (is (= (conditional-ids (keys rules))
           (set/union (:conditionally-checked-preservation-ids result)
                      (:unsupported-preservation-ids result))))
    (is (= #{:request/note :request/version}
           (set (keys (:frame-derivations result)))))
    (is (= result (verify! baseline)))))

(deftest every-framed-reader-requires-independent-proof
  (let [result (verify! baseline)]
    (doseq [[field entry] (:frame-derivations result)]
      (let [checked (derivation/check-transition-derivation!
                     (proof-source baseline field))]
        (is (= [:same-field field] (:claim entry)))
        (is (= (:fingerprint checked) (:derivation-fingerprint entry)))
        (is (= :matching-abstract-transition-only (:scope entry)))
        (is (= #{premise} (:open-premises checked)))
        (is (= checker-open (:missing-obligations checked)))
        (is (= :none (:authority checked)))))))

(deftest supported-invariants-use-every-reader-and-no-others
  (let [result (verify! baseline)
        actual (by-invariant result)]
    (doseq [id supported]
      (let [outcome (get actual id)
            readers (get-in rules [id :readers])]
        (is (= [:preservation id] (:obligation-id outcome)))
        (is (= readers (:readers outcome)))
        (is (= #{} (:written-readers outcome)))
        (is (= :pure-typed-rule-frame-extensionality (:inference outcome)))
        (is (= :checked-conditional-inference (:status outcome)))
        (is (= readers (set (keys (:field-derivations outcome)))))))))

(deftest invariants-touching-writes-stay-open-even-with-framed-readers
  (let [outcomes (by-invariant (verify! baseline))]
    (doseq [id unsupported]
      (let [outcome (get outcomes id)
            readers (get-in rules [id :readers])]
        (is (= readers (:readers outcome)))
        (is (= (set/intersection readers (:writes claim))
               (:written-readers outcome)))
        (is (= :unsupported-written-field-preservation (:inference outcome)))
        (is (= :open (:status outcome)))
        (is (= {} (:field-derivations outcome)))))))

(deftest no-reader-true-and-false-invariants-are-both-conditional
  (let [outcomes (by-invariant (verify! baseline))]
    (doseq [id [:request/constant-true :request/constant-false]]
      (is (= #{} (get-in outcomes [id :readers])))
      (is (= #{} (get-in outcomes [id :written-readers])))
      (is (= {} (get-in outcomes [id :field-derivations])))
      (is (= :checked-conditional-inference (get-in outcomes [id :status]))))
    (is (contains? (:original-open-obligation-ids (verify! baseline))
                   [:initial-state :request/constant-false]))))

(deftest a-false-invariant-never-proves-initial-state-existence
  (let [source (assoc baseline :invariants
                      {:request/impossible {:readers #{:request/version}
                                            :expression [:< [:read :request/version]
                                                         [:literal 0]]}})
        result (assert-unadmitted! (verify! source) source)]
    (is (= #{[:preservation :request/impossible]}
           (:conditionally-checked-preservation-ids result)))
    (is (contains? (:original-open-obligation-ids result)
                   [:initial-state :request/impossible]))
    (is (contains? (:original-open-obligation-ids result)
                   [:assumption :initial-state-existence]))))

(deftest missing-invariant-is-not-magically-recovered
  (let [source (update baseline :invariants dissoc :request/nonnegative-version)
        result (assert-unadmitted! (verify! source) source)]
    (is (not (contains? (:conditionally-checked-preservation-ids result)
                        [:preservation :request/nonnegative-version])))
    (is (not (contains? (:original-open-obligation-ids result)
                        [:preservation :request/nonnegative-version])))
    (is (contains? (:original-open-obligation-ids result)
                   [:assumption :declared-invariant-set-coverage]))
    (is (not= (:fingerprint result) (:fingerprint (verify! baseline))))))

(deftest moving-framed-version-to-write-set-removes-preservation
  (let [changed (-> baseline
                    (update-in [:construction :writes] conj :request/version)
                    (update-in [:construction :frame] disj :request/version)
                    (assoc-in [:construction :replacements :request/version]
                              [:literal 0]))
        result (assert-unadmitted! (verify! changed) changed)
        outcomes (by-invariant result)]
    (is (= :open (get-in outcomes [:request/nonnegative-version :status])))
    (is (= :open (get-in outcomes [:request/note-or-version :status])))
    (is (= #{:request/version}
           (get-in outcomes [:request/nonnegative-version :written-readers])))
    (is (= #{:request/version}
           (get-in outcomes [:request/note-or-version :written-readers])))
    (is (= #{:request/note} (set (keys (:frame-derivations result)))))
    (is (not= (:fingerprint (verify! baseline)) (:fingerprint result)))))

(deftest optional-frame-moved-to-write-set-removes-preservation
  (let [changed (-> baseline
                    (update-in [:construction :writes] conj :request/note)
                    (update-in [:construction :frame] disj :request/note)
                    (assoc-in [:construction :replacements :request/note]
                              [:literal "set-now"]))
        result (assert-unadmitted! (verify! changed) changed)
        outcomes (by-invariant result)]
    (is (= :open (get-in outcomes [:request/note-present :status])))
    (is (= :open (get-in outcomes [:request/note-or-version :status])))
    (is (= #{:request/note}
           (get-in outcomes [:request/note-present :written-readers])))
    (is (= #{:request/version} (set (keys (:frame-derivations result)))))
    (is (not= (:fingerprint (verify! baseline)) (:fingerprint result)))))

(deftest write-to-same-value-is-not-assumed-a-universal-frame-proof
  (let [source (-> baseline
                   (update-in [:construction :writes] conj :request/version)
                   (update-in [:construction :frame] disj :request/version)
                   (assoc-in [:construction :replacements :request/version]
                             [:literal 0]))
        result (verify! source)]
    (is (= :open (get-in (by-invariant result)
                         [:request/nonnegative-version :status])))
    (is (not (contains? (:frame-derivations result) :request/version)))))

(deftest incomplete-frame-declaration-rejected-upstream
  (let [source (update-in baseline [:construction :frame] disj :request/note)]
    (rejects! #(verify! source) error-construction :frame-footprint-mismatch)))

(deftest invented-frame-field-rejected-upstream
  (let [source (update-in baseline [:construction :frame] conj :request/ghost)]
    (rejects! #(verify! source) error-construction :frame-footprint-mismatch)))

(deftest write-without-replacement-rejected-upstream
  (let [source (update-in baseline [:construction :writes] conj :request/version)]
    (rejects! #(verify! source) error-construction :write-footprint-mismatch)))

(deftest extra-replacement-without-write-rejected-upstream
  (let [source (assoc-in baseline [:construction :replacements :request/version]
                         [:literal 42])]
    (rejects! #(verify! source) error-construction :write-footprint-mismatch)))

(deftest added-optional-field-is-framed-not-an-unrestricted-effect
  (let [source (-> baseline
                   (assoc-in [:construction :state-types :request/audit]
                             {:type :string :optional? true :nullable? true})
                   (update-in [:construction :frame] conj :request/audit)
                   (assoc-in [:invariants :request/audit-present]
                             {:readers #{:request/audit}
                              :expression [:present [:read :request/audit]]}))
        result (assert-unadmitted! (verify! source) source)]
    (is (= #{:request/note :request/version :request/audit}
           (set (keys (:frame-derivations result)))))
    (is (contains? (:conditionally-checked-preservation-ids result)
                   [:preservation :request/audit-present]))
    (is (not= (:fingerprint result) (:fingerprint (verify! baseline))))))

(deftest modified-guard-does-not-reuse-stale-fingerprint
  (let [changed (assoc-in baseline [:construction :guard :expression]
                          [:= [:read :request/status] [:literal :claimed]])
        original (verify! baseline)
        after (assert-unadmitted! (verify! changed) changed)]
    (is (not= (:fingerprint original) (:fingerprint after)))
    (is (not= (:transition-fingerprint original)
              (:transition-fingerprint after)))
    (is (= (:conditionally-checked-preservation-ids original)
           (:conditionally-checked-preservation-ids after)))))

(deftest changed-invariant-changes-source-identity-not-verdict-scope
  (let [changed (assoc-in baseline
                          [:invariants :request/nonnegative-version :expression]
                          [:>= [:read :request/version] [:literal 4]])
        before (verify! baseline)
        after (assert-unadmitted! (verify! changed) changed)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (not= (:obligation-fingerprint before)
              (:obligation-fingerprint after)))
    (is (contains? (:conditionally-checked-preservation-ids after)
                   [:preservation :request/nonnegative-version]))))

(deftest changed-initial-condition-remains-a-separate-open-obligation
  (let [changed (assoc-in baseline [:initial-condition :expression]
                          [:= [:read :request/status] [:literal :claimed]])
        before (verify! baseline)
        after (assert-unadmitted! (verify! changed) changed)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (= (:conditionally-checked-preservation-ids before)
           (:conditionally-checked-preservation-ids after)))
    (is (contains? (:original-open-obligation-ids after)
                   [:initial-state :request/nonnegative-version]))))

(deftest changed-operation-subject-cannot-reuse-proof-identity
  (let [changed (assoc-in baseline [:construction :subject :id] :example/reclaim)
        before (verify! baseline)
        after (assert-unadmitted! (verify! changed) changed)]
    (is (not= (:transition-fingerprint before) (:transition-fingerprint after)))
    (is (not= (:fingerprint before) (:fingerprint after)))))

(deftest copied-report-is-not-accepted-as-original-source
  (let [copy (verify! baseline)]
    (rejects! #(verify! copy) error-obligation :wrong-keys)
    (rejects! #(verify! (assoc baseline :proof-status :proved))
              error-obligation :wrong-keys)
    (rejects! #(verify! (assoc baseline :certificate :issued))
              error-obligation :wrong-keys)
    (rejects! #(verify! (assoc baseline :valid? true))
              error-obligation :wrong-keys)))

(deftest original-source-does-not-accept-forged-admission-data
  (rejects! #(verify! (assoc baseline :authority :xtdb-commit))
            error-obligation :wrong-keys)
  (rejects! #(verify! (assoc baseline :open-obligation-ids #{}))
            error-obligation :wrong-keys))

(deftest metadata-bearing-source-is-rejected
  (rejects! #(verify! (with-meta baseline {:verified true}))
            error-obligation :metadata-not-allowed))

(deftest malformed-invariant-reader-footprint-is-rejected
  (let [changed (assoc-in baseline [:invariants :request/nonnegative-version :readers]
                          #{:request/version :request/ghost})]
    (rejects! #(verify! changed) error-obligation :non-state-reader)))

(deftest an-invariant-written-field-is-not-proved-by-a-true-sample
  (let [result (verify! baseline)
        status (get (by-invariant result) :request/status-domain)]
    (is (= :open (:status status)))
    (is (= #{:request/status} (:written-readers status)))
    (is (= {} (:field-derivations status)))))

(deftest a-fake-proof-checker-report-is-rejected
  (rejects!
   #(with-redefs [derivation/check-transition-derivation!
                  (fn [_] {:proof-status :conditional-abstract-inference
                           :goal [:same-field :request/version]})]
      (verify! baseline))
   error-preservation :derivation-checker-contract-changed))

(deftest checker-tampering-with-premises-authority-or-scope-is-rejected
  (let [real derivation/check-transition-derivation!
        corruptions [[:open-premises #{}]
                     [:missing-obligations #{}]
                     [:authority :xtdb-commit]
                     [:certificate :issued]
                     [:admission :admitted]
                     [:runtime-enforcement :established]
                     [:implementation-correspondence :established]
                     [:claim-scope :production]
                     [:proof-status :proved]
                     [:conclusion-status :unconditional]
                     [:assumption-discharge :performed]
                     [:transition-fingerprint "sha256:invalid"]
                     [:goal [:same-field :request/owner]]
                     [:rule-count 42]
                     [:fingerprint "not-a-fingerprint"]]]
    (doseq [[k v] corruptions]
      (testing (str "Checker tampering: " k)
        (rejects!
         #(with-redefs [derivation/check-transition-derivation!
                        (fn [proof] (assoc (real proof) k v))]
            (verify! baseline))
         error-preservation :derivation-checker-contract-changed)))))

(deftest upstream-obligation-contract-tampering-is-rejected
  (let [real obligation/derive-obligations!
        changed (fn [source]
                  (update (real source) :obligations assoc :invented []))]
    (rejects!
     #(with-redefs [obligation/derive-obligations! changed]
        (verify! baseline))
     error-preservation :obligation-source-contract-changed)))

(deftest source-preservation-obligation-omission-is-rejected
  (let [real obligation/derive-obligations!
        changed (fn [source]
                  (update-in (real source) [:obligations :preservation] pop))]
    (rejects!
     #(with-redefs [obligation/derive-obligations! changed]
        (verify! baseline))
     error-preservation :obligation-source-contract-changed)))

(deftest upstream-established-obligation-is-not-silently-inherited
  (let [real obligation/derive-obligations!
        changed (fn [source]
                  (assoc-in (real source) [:obligations :preservation 0 :status]
                            :proved))]
    (rejects!
     #(with-redefs [obligation/derive-obligations! changed]
        (verify! baseline))
     error-preservation :obligation-source-contract-changed)))

(deftest derived-preservation-obligation-ids-remain-complete
  (let [result (verify! baseline)
        obligations (obligation/derive-obligations! baseline)
        declared (set (map :id (get-in obligations [:obligations :preservation])))]
    (is (= declared (set/union (:conditionally-checked-preservation-ids result)
                               (:unsupported-preservation-ids result))))
    (is (= (count declared) (count (:preservation-outcomes result))))))

(deftest exact-abstract-witnesses-preserve-framed-rule-truth-values
  (let [result (verify! baseline)
        eligible (set (map second (:conditionally-checked-preservation-ids result)))
        types (get-in baseline [:construction :state-types])
        transition (:transition (construction/build-guarded-replacement! claim))]
    (doseq [v [-5 0 1 7 99999999999999999999999999999N]
            note [::absent nil "a note"]]
      (let [before (cond-> {:request/status :open :request/owner nil
                            :request/version v}
                     (not= ::absent note) (assoc :request/note note))
            after (assoc before :request/status :claimed
                                :request/owner :helper/alice)
            observation (abstract/observe-transition!
                         transition before {:input/actor :helper/alice} after)]
        (is (true? (:matches? observation)))
        (doseq [id eligible]
          (let [rule (get rules id)
                readers (:readers rule)
                evaluated (fn [state]
                            (:value (rule-eval/evaluate-rule!
                                     rule (select-keys types readers)
                                     (select-keys state readers))))]
            (is (= (evaluated before) (evaluated after))
                (str "Invariant's truth is unchanged: " id))))))))

(deftest mutated-optional-presence-breaks-matching-relation
  (let [transition (:transition (construction/build-guarded-replacement! claim))
        base {:request/status :open :request/owner nil :request/version 10}
        after {:request/status :claimed :request/owner :helper/alice
               :request/version 10 :request/note nil}
        observed (abstract/observe-transition!
                  transition base {:input/actor :helper/alice} after)]
    (is (false? (:matches? observed)))
    (is (false? (:frame-holds? observed)))
    (is (true? (:guard-holds? observed)))
    (is (true? (:assignments-hold? observed)))))

(deftest mutated-frame-value-breaks-matching-relation
  (let [transition (:transition (construction/build-guarded-replacement! claim))
        base {:request/status :open :request/owner nil
              :request/version 10 :request/note "same"}
        after (assoc base :request/status :claimed
                          :request/owner :helper/alice
                          :request/version 11)
        observed (abstract/observe-transition!
                  transition base {:input/actor :helper/alice} after)]
    (is (false? (:matches? observed)))
    (is (false? (:frame-holds? observed)))
    (is (true? (:guard-holds? observed)))
    (is (true? (:assignments-hold? observed)))))

(deftest nonmatching-guard-does-not-satisfy-abstract-relation
  (let [transition (:transition (construction/build-guarded-replacement! claim))
        base {:request/status :claimed :request/owner :helper/bob
              :request/version 10}
        observed (abstract/observe-transition!
                  transition base {:input/actor :helper/alice} base)]
    (is (false? (:matches? observed)))
    (is (false? (:guard-holds? observed)))))
