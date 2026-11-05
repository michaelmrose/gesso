(ns gesso.assurance.proof.field-copy-soundness-test
  "Permanent v1358 adversarial regression suite for restricted same-origin
   field-copy preservation. Explicitly distinguish source-relative conditional
   lemmas, matching abstract witnesses, unsupported constructions, and physical
   or universally verified claims. Neither reports nor witnesses grant authority."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.field-copy-soundness :as copy]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as production]))

(def types {:doc/a {:type :keyword}
            :doc/b {:type :keyword}
            :doc/source {:type :keyword}
            :doc/status {:type :keyword}})
(def base {:subject {:kind :operation :id :example/copy
                     :interface {:operation :example/copy}}
           :state-types types
           :input-types {}
           :guard {:readers #{:doc/status}
                   :expression [:= [:read :doc/status] [:literal :open]]}
           :reads #{:doc/status :doc/source}
           :writes #{:doc/a :doc/b}
           :assignments {:doc/a [:read :doc/source]
                         :doc/b [:read :doc/source]}})
(def before {:doc/a :old-a :doc/b :old-b :doc/source :new
             :doc/status :open})
(def after {:doc/a :new :doc/b :new :doc/source :new
            :doc/status :open})
(def both {:id :example/equal :readers #{:doc/a :doc/b}
           :expression [:= [:read :doc/a] [:read :doc/b]]})
(defn rejection? [f]
  (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(defn- rejection-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- assert-no-authority! [report]
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not= :proved (:proof-status report))))

(deftest same-origin-two-copy-fields
  (let [lemma (copy/derive-copy-lemma! base both)
        result (copy/audit-copy-witness! base both before {} after)]
    (is (= :conditional-same-origin-copy-rule (:status lemma)))
    (is (= [:pre :doc/source] (:shared-total-origin lemma)))
    (is (= #{:doc/a :doc/b} (:copied-fields lemma)))
    (is (= :checked-conditional-copy-instance (:status result)))
    (is (true? (:matches-transition? result)))
    (is (false? (:before-invariant result)))
    (is (true? (:after-invariant result)))
    (is (= :new (:common-prestate-or-input-value result)))
    (is (= :not-issued (:certificate lemma)))
    (is (= :not-admitted (:admission result)))))

(deftest copy-and-required-frame
  (let [source (assoc base
                 :reads #{:doc/status :doc/source}
                 :writes #{:doc/a}
                 :assignments {:doc/a [:read :doc/source]})
        inv {:id :example/copy-source :readers #{:doc/a :doc/source}
             :expression [:= [:read :doc/a] [:read :doc/source]]}
        post (assoc before :doc/a :new)]
    (is (= :conditional-same-origin-copy-rule
           (:status (copy/derive-copy-lemma! source inv))))
    (is (= [:pre :doc/source]
           (:shared-total-origin (copy/derive-copy-lemma! source inv))))
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! source inv before {} post))))))

(deftest same-input-two-copy-fields
  (let [source (-> base
                   (assoc :input-types {:input/new {:type :keyword}}
                          :reads #{:doc/status :input/new}
                          :assignments {:doc/a [:read :input/new]
                                        :doc/b [:read :input/new]}))
        input {:input/new :from-input}
        post (assoc before :doc/a :from-input :doc/b :from-input)]
    (is (= [:input :input/new]
           (:shared-total-origin (copy/derive-copy-lemma! source both))))
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! source both before input post))))))

(deftest different-origins-are-not-established-by-sampled-equality
  (let [source (-> base
                   (assoc :input-types {:input/new {:type :keyword}}
                          :reads #{:doc/status :doc/source :input/new}
                          :assignments {:doc/a [:read :doc/source]
                                        :doc/b [:read :input/new]}))
        observation (copy/audit-copy-witness! source both before
                                              {:input/new :new} after)]
    (is (true? (:matches-transition? observation)))
    (is (true? (:after-invariant observation)))
    (is (= :open-no-shared-total-origin (:status observation)))
    (is (= :none (:authority observation)))))

(deftest simultaneous-prestate-reads
  (let [source (assoc base
                 :reads #{:doc/status :doc/source :doc/a}
                 :writes #{:doc/a :doc/b :doc/source}
                 :assignments {:doc/a [:read :doc/source]
                               :doc/b [:read :doc/source]
                               :doc/source [:read :doc/a]})
        post (assoc before :doc/a :new :doc/b :new :doc/source :old-a)]
    (is (:matches? (production/observe-transition! source before {} post)))
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! source both before {} post))))
    (is (= :nonmatching-transition
           (:status (copy/audit-copy-witness! source both before {} after))))))

(deftest unsupported-well-typed-invariants-stay-open
  (let [predicate {:id :example/test :readers #{:doc/a}
                   :expression [:= [:read :doc/a] [:literal :new]]}
        equal-self {:id :example/self :readers #{:doc/a}
                    :expression [:= [:read :doc/a] [:read :doc/a]]}]
    (is (= :open-unsupported-invariant-shape
           (:status (copy/derive-copy-lemma! base predicate))))
    (is (= :open-unsupported-invariant-shape
           (:status (copy/derive-copy-lemma! base equal-self))))))

(deftest invalid-inputs-and-nonmatching-witness
  (is (rejection? #(copy/derive-copy-lemma! base
                                            (assoc both :readers #{:doc/a}))))
  (is (rejection? #(copy/derive-copy-lemma! base
                                            (assoc both :extra :forged))))
  (is (rejection? #(copy/derive-copy-lemma! base
                                            (assoc both :expression
                                                   [:= [:read :doc/a]
                                                    [:literal 42]]))))
  (is (rejection? #(copy/derive-copy-lemma!
                   (assoc-in base [:assignments :doc/b] [:read :unknown/nope])
                   both)))
  (let [wrong (assoc after :doc/b :wrong)
        result (copy/audit-copy-witness! base both before {} wrong)]
    (is (= :nonmatching-transition (:status result)))
    (is (false? (:matches-transition? result)))))

(deftest optional-frame-cannot-be-an-unguarded-origin
  (let [source (assoc-in base [:state-types :doc/source :optional?] true)]
    (is (rejection? #(copy/derive-copy-lemma! source both)))))

(deftest optional-copy-targets-are-post-present
  (let [source (-> base
                   (assoc-in [:state-types :doc/a :optional?] true)
                   (assoc-in [:state-types :doc/b :optional?] true))
        inv {:id :example/guarded-post-equality
             :readers #{:doc/a :doc/b}
             :expression [:and [:present [:read :doc/a]]
                          [:present [:read :doc/b]]
                          [:= [:read :doc/a] [:read :doc/b]]]}
        pre (select-keys before [:doc/source :doc/status])
        lemma (copy/derive-copy-lemma! source inv)
        check (copy/audit-copy-witness! source inv pre {} after)]
    (is (= :conditional-same-origin-copy-rule (:status lemma)))
    (is (= :presence-guarded-equality (:postcondition-shape lemma)))
    (is (= :checked-conditional-copy-instance (:status check)))
    (is (false? (:before-invariant check)))
    (is (true? (:after-invariant check)))
    (is (true? (:post-fields-follow-common-origin? check)))))

(deftest extra-guards-and-incorrect-guard-order-stay-open-or-reject
  (let [source (-> base
                   (assoc-in [:state-types :doc/a :optional?] true)
                   (assoc-in [:state-types :doc/b :optional?] true))
        guarded {:id :example/guarded-post-equality
                 :readers #{:doc/a :doc/b}
                 :expression [:and [:present [:read :doc/a]]
                              [:present [:read :doc/b]]
                              [:= [:read :doc/a] [:read :doc/b]]]}
        swapped (update guarded :expression
                        (fn [expr] [:and (nth expr 2) (nth expr 1) (nth expr 3)]))
        extra (update guarded :expression conj [:present [:read :doc/a]])]
    ;; A different (but type-correct) guard order is not silently rewritten.
    (is (= :open-unsupported-invariant-shape
           (:status (copy/derive-copy-lemma! source swapped))))
    (is (= :open-unsupported-invariant-shape
           (:status (copy/derive-copy-lemma! source extra))))))

;; v1358: permanent acceptance checks. The older ten smoke cases above form
;; the positive/negative baseline; the following tests make failure modes,
;; source identity, and finite-state correspondence explicit.

(deftest v1358-lemma-contract-is-conditional-not-a-certificate
  (let [l (copy/derive-copy-lemma! base both)]
    (is (= copy/report-type (:type l)))
    (is (= copy/semantics-version (:version l)))
    (is (= :conditional-same-origin-copy-rule (:status l)))
    (is (= :same-total-prestate-or-input-origin-implies-post-equality
           (:schema l)))
    (is (= :matching-transition-establishes-post-equality
           (:conditional-claim l)))
    (is (= :not-independently-proved (:typed-expression-soundness l)))
    (is (= :not-independently-proved (:transition-implementation-soundness l)))
    (is (= :conditional-schema-only (:proof-status l)))
    (is (= both (:source-invariant l)))
    (is (= #{:doc/a :doc/b} (:readers l)))
    (is (= [:doc/a :doc/b] (:post-fields l)))
    (is (= :direct-equality (:postcondition-shape l)))
    (is (= [:pre :doc/source] (:left-origin l) (:right-origin l)))
    (is (string? (:transition-fingerprint l)))
    (is (string? (:typed-invariant-fingerprint l)))
    (assert-no-authority! l)))

(deftest v1358-witness-contract-is-local-not-physical
  (let [l (copy/derive-copy-lemma! base both)
        r (copy/audit-copy-witness! base both before {} after)]
    (is (= copy/witness-type (:type r)))
    (is (= copy/semantics-version (:version r)))
    (is (= (:transition-fingerprint l) (:transition-fingerprint r)))
    (is (= :example/equal (:invariant-id r)))
    (is (= :conditional-same-origin-copy-rule (:lemma-status r)))
    (is (= :independent-abstract-relation-and-typed-expression (:checking r)))
    (is (= :checked-conditional-copy-instance (:status r)))
    (is (= :not-universally-proved (:proof-status r)))
    (is (= :not-established (:physical-observation r)))
    (is (true? (:post-fields-follow-common-origin? r)))
    (is (true? (:preservation-implication-holds? r)))
    (assert-no-authority! r)))

(deftest v1358-lemma-source-fingerprint-change
  (let [original (copy/derive-copy-lemma! base both)
        changed (copy/derive-copy-lemma! (assoc-in base [:subject :interface :route]
                                                   :example/other)
                                          both)
        changed-guard (copy/derive-copy-lemma!
                       (assoc-in base [:guard :expression]
                                 [:= [:read :doc/status] [:literal :closed]]) both)
        changed-invariant (copy/derive-copy-lemma!
                           base (assoc both :id :example/equal-alt))
        changed-predicate (copy/derive-copy-lemma!
                           base (assoc both :expression
                                       [:not= [:read :doc/a] [:read :doc/b]]))]
    (is (not= (:transition-fingerprint original)
              (:transition-fingerprint changed)))
    (is (not= (:transition-fingerprint original)
              (:transition-fingerprint changed-guard)))
    ;; The fingerprint identifies typed predicate semantics, not invariant ID.
    (is (= (:typed-invariant-fingerprint original)
           (:typed-invariant-fingerprint changed-invariant)))
    (is (= :example/equal-alt (:invariant-id changed-invariant)))
    (is (not= (:typed-invariant-fingerprint original)
              (:typed-invariant-fingerprint changed-predicate)))
    (is (= :open-unsupported-invariant-shape (:status changed-predicate)))
    (is (= :conditional-same-origin-copy-rule (:status changed)))
    (is (= :conditional-same-origin-copy-rule (:status changed-guard)))
    (is (= :conditional-same-origin-copy-rule (:status changed-invariant)))))

(deftest v1358-frame-source-can-provide-a-total-prestate-origin
  (let [t (assoc base :writes #{:doc/a}
                      :assignments {:doc/a [:read :doc/source]})
        invariant {:id :example/source-paired
                   :readers #{:doc/a :doc/source}
                   :expression [:= [:read :doc/a] [:read :doc/source]]}
        lemma (copy/derive-copy-lemma! t invariant)
        post (assoc before :doc/a :new)]
    (is (= [:pre :doc/source] (:left-origin lemma)))
    (is (= [:pre :doc/source] (:right-origin lemma)))
    (is (= #{:doc/a} (:copied-fields lemma)))
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! t invariant before {} post))))))

(deftest v1358-written-source-is-never-a-post-frame-origin
  (let [t (assoc base :reads #{:doc/status :doc/source :doc/a}
                      :writes #{:doc/a :doc/b :doc/source}
                      :assignments {:doc/a [:read :doc/source]
                                    :doc/b [:read :doc/source]
                                    :doc/source [:read :doc/a]})
        paired {:id :example/written-source
                :readers #{:doc/a :doc/source}
                :expression [:= [:read :doc/a] [:read :doc/source]]}
        post (assoc after :doc/source :old-a)
        report (copy/derive-copy-lemma! t paired)
        observed (copy/audit-copy-witness! t paired before {} post)]
    (is (= :open-no-shared-total-origin (:status report)))
    (is (= [:pre :doc/source] (:left-origin report)))
    (is (= [:pre :doc/a] (:right-origin report)))
    (is (true? (:matches-transition? observed)))
    (is (= :open-no-shared-total-origin (:status observed)))
    (is (false? (:after-invariant observed)))
    (is (nil? (:conditional-claim report)))))

(deftest v1358-prestate-copy-swap-remains-simultaneous
  (let [t (assoc base :reads #{:doc/status :doc/a :doc/b}
                      :writes #{:doc/a :doc/b}
                      :assignments {:doc/a [:read :doc/b]
                                    :doc/b [:read :doc/a]})
        post (assoc before :doc/a :old-b :doc/b :old-a)
        lemma (copy/derive-copy-lemma! t both)
        observation (copy/audit-copy-witness! t both before {} post)]
    (is (true? (:matches? (production/observe-transition! t before {} post))))
    (is (= :open-no-shared-total-origin (:status lemma)))
    (is (= [:pre :doc/b] (:left-origin lemma)))
    (is (= [:pre :doc/a] (:right-origin lemma)))
    (is (= :open-no-shared-total-origin (:status observation)))
    (is (true? (:matches-transition? observation)))
    (is (false? (:after-invariant observation)))))

(deftest v1358-input-copy-does-not-read-an-identically-named-state-field
  (let [t (assoc base :input-types {:input/actor {:type :keyword}}
                      :reads #{:doc/status :input/actor}
                      :assignments {:doc/a [:read :input/actor]
                                    :doc/b [:read :input/actor]})
        post (assoc before :doc/a :user/one :doc/b :user/one)
        r (copy/audit-copy-witness! t both before {:input/actor :user/one} post)]
    (is (= :checked-conditional-copy-instance (:status r)))
    (is (= :user/one (:common-prestate-or-input-value r)))
    (is (= [:input :input/actor]
           (:shared-total-origin (copy/derive-copy-lemma! t both))))))

(deftest v1358-two-inputs-same-observed-value-do-not-share-an-origin
  (let [t (assoc base :input-types {:input/one {:type :keyword}
                                    :input/two {:type :keyword}}
                      :reads #{:doc/status :input/one :input/two}
                      :assignments {:doc/a [:read :input/one]
                                    :doc/b [:read :input/two]})
        inp {:input/one :user/one :input/two :user/one}
        post (assoc before :doc/a :user/one :doc/b :user/one)
        l (copy/derive-copy-lemma! t both)
        r (copy/audit-copy-witness! t both before inp post)]
    (is (= :open-no-shared-total-origin (:status l)))
    (is (nil? (:shared-total-origin l)))
    (is (nil? (:conditional-claim l)))
    (is (= :open-no-shared-total-origin (:status r)))
    (is (true? (:matches-transition? r)))
    (is (true? (:after-invariant r)))
    (assert-no-authority! r)))

(deftest v1358-state-and-input-equal-values-are-distinct-origins
  (let [t (assoc base :input-types {:input/new {:type :keyword}}
                      :reads #{:doc/status :doc/source :input/new}
                      :assignments {:doc/a [:read :doc/source]
                                    :doc/b [:read :input/new]})
        l (copy/derive-copy-lemma! t both)
        r (copy/audit-copy-witness! t both before {:input/new :new} after)]
    (is (= :open-no-shared-total-origin (:status l)))
    (is (= [:pre :doc/source] (:left-origin l)))
    (is (= [:input :input/new] (:right-origin l)))
    (is (true? (:after-invariant r)))
    (is (= :open-no-shared-total-origin (:status r)))))

(deftest v1358-literal-writes-are-out-of-copy-schema
  (let [t (assoc base :reads #{:doc/status}
                      :assignments {:doc/a [:literal :new]
                                    :doc/b [:literal :new]})
        l (copy/derive-copy-lemma! t both)
        r (copy/audit-copy-witness! t both before {} after)]
    (is (true? (:matches-transition? r)))
    (is (true? (:after-invariant r)))
    (is (= :open-no-shared-total-origin (:status l)))
    (is (nil? (:conditional-claim l)))
    (is (= #{} (:copied-fields l)))
    (is (= :open-no-shared-total-origin (:status r)))))

(deftest v1358-mixed-literal-and-copy-same-value-stays-open
  (let [t (assoc base :reads #{:doc/status :doc/source}
                      :assignments {:doc/a [:read :doc/source]
                                    :doc/b [:literal :new]})
        l (copy/derive-copy-lemma! t both)
        r (copy/audit-copy-witness! t both before {} after)]
    (is (= :open-no-shared-total-origin (:status l)))
    (is (= #{:doc/a} (:copied-fields l)))
    (is (true? (:after-invariant r)))
    (is (= :open-no-shared-total-origin (:status r)))))

(deftest v1358-independent-relation-must-not-invoke-production-observer
  (with-redefs [production/observe-transition!
                (fn [& _] (throw (ex-info "must not be invoked" {:production-called true})))]
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! base both before {} after))))))

(deftest v1358-false-guard-even-with-correct-post-is-not-a-transition
  (let [pre (assoc before :doc/status :closed)
        r (copy/audit-copy-witness! base both pre {} after)]
    (is (= :nonmatching-transition (:status r)))
    (is (false? (:matches-transition? r)))
    (is (false? (:before-invariant r)))
    (is (true? (:after-invariant r)))
    (is (= :new (:common-prestate-or-input-value r)))))

(deftest v1358-invalid-assignments-and-frames-are-not-matching
  (doseq [bad-post [(assoc after :doc/a :changed)
                    (assoc after :doc/b :changed)
                    (assoc after :doc/source :changed)
                    (assoc after :doc/status :closed)]]
    (let [r (copy/audit-copy-witness! base both before {} bad-post)]
      (is (= :nonmatching-transition (:status r)))
      (is (false? (:matches-transition? r)))
      (is (not= :checked-conditional-copy-instance (:status r)))))
  ;; A malformed state is rejected, not converted to a valid nonmatching
  ;; observation. This is deliberately stronger than a failed frame test.
  (doseq [bad-post [(dissoc after :doc/source)
                    (assoc after :extra/outside 7)
                    (assoc after :doc/b 12)]]
    (is (rejection? #(copy/audit-copy-witness! base both before {} bad-post)))))

(deftest v1358-invalid-prestate-input-and-typed-projection-rejected
  (doseq [bad-before [(dissoc before :doc/source)
                      (assoc before :doc/a 2)
                      (assoc before :extra/wrong :value)]]
    (is (rejection? #(copy/audit-copy-witness! base both bad-before {} after))))
  (is (rejection? #(copy/audit-copy-witness! base both before
                                               {:input/unexpected :value} after)))
  (is (rejection? #(copy/audit-copy-witness! base both before nil after))))

(deftest v1358-optional-targets-require-exact-presence-guards
  (let [t (-> base
              (assoc-in [:state-types :doc/a :optional?] true)
              (assoc-in [:state-types :doc/b :optional?] true))
        guarded {:id :example/guarded-equality
                 :readers #{:doc/a :doc/b}
                 :expression [:and [:present [:read :doc/a]]
                              [:present [:read :doc/b]]
                              [:= [:read :doc/a] [:read :doc/b]]]}
        pre (dissoc before :doc/a :doc/b)
        r (copy/audit-copy-witness! t guarded pre {} after)]
    (is (= :checked-conditional-copy-instance (:status r)))
    (is (= :presence-guarded-equality
           (:postcondition-shape (copy/derive-copy-lemma! t guarded))))
    (is (false? (:before-invariant r)))
    (is (true? (:after-invariant r)))
    (is (true? (:post-fields-follow-common-origin? r)))
    (doseq [modified [(assoc guarded :expression
                             [:and [:present [:read :doc/b]]
                              [:present [:read :doc/a]]
                              [:= [:read :doc/a] [:read :doc/b]]])
                      (update guarded :expression conj [:literal true])]]
      (let [result (copy/derive-copy-lemma! t modified)]
        (is (= :open-unsupported-invariant-shape (:status result)))
        (is (nil? (:conditional-claim result)))))
    ;; This rearrangement is not merely out of scope: it attempts an
    ;; optional :doc/b read before proving that the field is present.
    (is (rejection? #(copy/derive-copy-lemma!
                      t (assoc guarded :expression
                               [:and [:present [:read :doc/a]]
                                [:= [:read :doc/a] [:read :doc/b]]
                                [:present [:read :doc/b]]]))))))

(deftest v1358-optional-presence-is-not-overlooked-after-copy
  (let [t (-> base
              (assoc-in [:state-types :doc/a :optional?] true)
              (assoc-in [:state-types :doc/b :optional?] true))
        invariant {:id :example/guarded :readers #{:doc/a :doc/b}
                   :expression [:and [:present [:read :doc/a]]
                                [:present [:read :doc/b]]
                                [:= [:read :doc/a] [:read :doc/b]]]}
        pre (dissoc before :doc/a :doc/b)]
    (doseq [post [(dissoc after :doc/a)
                  (dissoc after :doc/b)
                  (dissoc after :doc/a :doc/b)]]
      (let [r (copy/audit-copy-witness! t invariant pre {} post)]
        (is (false? (:matches-transition? r)))
        (is (= :nonmatching-transition (:status r)))
        (is (false? (:after-invariant r)))))))

(deftest v1358-nullable-prestate-copy-preserves-nil-value
  (let [t (-> base
              (assoc-in [:state-types :doc/source :nullable?] true)
              (assoc-in [:state-types :doc/a :nullable?] true)
              (assoc-in [:state-types :doc/b :nullable?] true))
        pre (assoc before :doc/source nil)
        post (assoc after :doc/a nil :doc/b nil :doc/source nil)
        r (copy/audit-copy-witness! t both pre {} post)]
    (is (= :conditional-same-origin-copy-rule
           (:status (copy/derive-copy-lemma! t both))))
    (is (true? (:matches-transition? r)))
    (is (true? (:post-fields-follow-common-origin? r)))
    (is (true? (:after-invariant r)))
    (is (nil? (:common-prestate-or-input-value r)))
    (is (= :checked-conditional-copy-instance (:status r)))))

(deftest v1358-optional-source-is-not-admitted-as-a-total-origin
  (let [t (assoc-in base [:state-types :doc/source :optional?] true)]
    (is (rejection? #(copy/derive-copy-lemma! t both)))
    (is (rejection? #(copy/audit-copy-witness! t both before {} after)))))

(deftest v1358-nullable-to-nonnullable-copy-rejected
  (let [t (assoc-in base [:state-types :doc/source :nullable?] true)]
    (is (rejection? #(copy/derive-copy-lemma! t both)))))

(deftest v1358-incorrect-field-type-and-nominal-enum-rejected
  (let [wrong-target (assoc-in base [:state-types :doc/b] {:type :integer})
        enum-types {:type :enum :name :doc/phase :values #{:phase/one :phase/two}}
        enum-t (assoc base :state-types {:doc/a enum-types
                                         :doc/b enum-types
                                         :doc/source enum-types
                                         :doc/status {:type :keyword}})
        enum-b (assoc before :doc/a :phase/one :doc/b :phase/two
                             :doc/source :phase/one)
        enum-a (assoc after :doc/a :phase/one :doc/b :phase/one
                           :doc/source :phase/one)]
    (is (rejection? #(copy/derive-copy-lemma! wrong-target both)))
    (is (= :checked-conditional-copy-instance
           (:status (copy/audit-copy-witness! enum-t both enum-b {} enum-a))))
    (is (rejection? #(copy/derive-copy-lemma!
                      (assoc-in enum-t [:state-types :doc/b :name] :doc/other)
                      both)))))

(deftest v1358-invariant-footprint-must-be-exact-and-typed
  (doseq [bad [(assoc both :readers #{:doc/a})
               (assoc both :readers #{:doc/a :doc/b :doc/source})
               (assoc both :readers [:doc/a :doc/b])
               (assoc both :expression [:= [:read :doc/a] [:literal 99]])
               (assoc both :expression [:read :doc/a])
               (assoc both :expression [:= [:read :doc/a] [:read :input/nope]])
               (assoc both :expression [:and [:literal true]])
               (assoc both :id :unqualified)
               (assoc both :unexpected :certificate)
               (with-meta both {:trusted true})
               (assoc both :expression (with-meta (:expression both)
                                        {:trusted true}))]]
    (is (rejection? #(copy/derive-copy-lemma! base bad)) (pr-str bad))))

(deftest v1358-unsupported-but-admitted-invariant-remains-open
  (let [self {:id :example/self :readers #{:doc/a}
              :expression [:= [:read :doc/a] [:read :doc/a]]}
        constant {:id :example/always :readers #{}
                  :expression [:literal true]}
        truth {:id :example/has-value :readers #{:doc/a :doc/b}
               :expression [:and [:= [:read :doc/a] [:read :doc/b]]
                            [:literal true]]}]
    (doseq [invariant [self constant truth]]
      (let [l (copy/derive-copy-lemma! base invariant)]
        (is (= :open-unsupported-invariant-shape (:status l)))
        (is (nil? (:conditional-claim l)))
        (assert-no-authority! l)))))

(deftest v1358-strict-transition-source-rejection
  (doseq [bad [(assoc base :reads #{:doc/status})
               (assoc base :writes #{:doc/a})
               (assoc base :assignments {:doc/a [:read :doc/source]})
               (assoc-in base [:assignments :doc/b] [:read :input/unknown])
               (assoc-in base [:assignments :doc/b] [:apply identity :doc/source])
               (assoc base :extra/proof :approved)
               (with-meta base {:trusted true})
               (assoc base :assignments
                      (with-meta (:assignments base) {:trusted true}))]]
    (is (rejection? #(copy/derive-copy-lemma! bad both)) (pr-str bad))))

(deftest v1358-forged-independent-observation-rejected
  (doseq [tamper [(fn [m] (assoc m :type :forged/report))
                  (fn [m] (assoc m :version 999))
                  (fn [m] (assoc m :source-declaration {:forged true}))
                  (fn [m] (assoc m :before {:doc/a :spoof}))
                  (fn [m] (assoc m :inputs {:input/forged true}))
                  (fn [m] (assoc m :after {:doc/a :spoof}))
                  (fn [m] (assoc m :checking :trusted))
                  (fn [m] (assoc m :guard-holds? :yes))
                  (fn [m] (assoc m :assignments-hold? nil))
                  (fn [m] (assoc m :frame-holds? :yes))
                  (fn [m] (assoc m :matches? true :guard-holds? false))
                  (fn [m] (assoc m :proof-status :proved))
                  (fn [m] (assoc m :admission :admitted))
                  (fn [m] (assoc m :authority :execute))]]
    (let [original (independent/observe-transition! base before {} after)
          mutated (tamper original)]
      (with-redefs [independent/observe-transition!
                    (fn [& _] mutated)]
        (let [rejection (rejection-data
                         #(copy/audit-copy-witness! base both before {} after))]
          (is (= true (:field-copy-soundness/rejection rejection)))
          (is (= :independent-observation-contract-changed (:reason rejection))))))))

(deftest v1358-unexpected-independent-errors-propagate
  (with-redefs [independent/observe-transition!
                (fn [& _] (throw (ex-info "unexpected subsystem failure"
                                         {:unrelated true})))]
    (let [data (rejection-data
                #(copy/audit-copy-witness! base both before {} after))]
      (is (= true (:unrelated data)))
      (is (not (:field-copy-soundness/rejection data))))))

(deftest v1358-diagnostic-reports-are-data-only-and-round-trip
  (let [l (copy/derive-copy-lemma! base both)
        r (copy/audit-copy-witness! base both before {} after)]
    (doseq [report [l r]]
      (is (= report (edn/read-string (pr-str report))))
      (assert-no-authority! report))
    (is (not= (:type l) (:type r)))
    (is (not (contains? r :physical-authority)))))

(deftest v1358-finite-state-independent-production-correspondence
  ;; The oracle derives assignments from each BEFORE state (not from a prior
  ;; post-state). Nonmatching candidates vary written fields and frame values.
  ;; No finite corpus can prove this relation universally; these are
  ;; adversarial diagnostic checks, not theorem or certification results.
  (let [values [:value/a :value/b]
        statuses [:open :closed]
        candidates values
        total (atom 0)
        rows (reduce (fn [acc axis]
                       (for [prefix acc, v axis] (conj prefix v)))
                     [[]]
                     [statuses values values values candidates candidates candidates])]
    (doseq [[status a b source out-a out-b out-source] rows]
      (let [pre {:doc/a a :doc/b b :doc/source source :doc/status status}
            post {:doc/a out-a :doc/b out-b
                  :doc/source out-source :doc/status status}
            p (production/observe-transition! base pre {} post)
            i (independent/observe-transition! base pre {} post)
            r (copy/audit-copy-witness! base both pre {} post)
            expected (and (= :open status)
                          (= out-a source)
                          (= out-b source)
                          (= out-source source))]
        (swap! total inc)
        (is (= expected (:matches? p)))
        (is (= expected (:matches? i)))
        (is (= expected (:matches-transition? r)))
        (is (= (if expected :checked-conditional-copy-instance
                   :nonmatching-transition)
               (:status r)))
        (when expected
          (is (:after-invariant r))
          (is (:post-fields-follow-common-origin? r)))
        (is (= :none (:authority r)))))
    (is (= (* (count statuses) 2 2 2 2 2 2) @total))))
