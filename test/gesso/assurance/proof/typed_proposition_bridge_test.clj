(ns gesso.assurance.proof.typed-proposition-bridge-test
  "v1330: permanent adversarial checks of the source-relative semantic bridge
   between typed rules and propositional formulas. The checked values are
   realizable, *supplied* finite bindings, not arbitrary atom assignments.

   These tests deliberately do not treat :bounded-no-counterexample as an
   admitted theorem. They test that original sources, independent per-atom
   checks, and proof/transaction authority stay separate."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-proposition-bridge :as bridge]
            [gesso.assurance.proof.typed-atom-oracle :as oracle]
            [gesso.assurance.proof.proposition :as proposition]
            [gesso.assurance.rule-eval :as production]))

(def ^:private subject
  {:kind :operation :id :example/check
   :interface {:operation :example/check}})
(def ^:private score :request/score)
(def ^:private enabled :request/enabled)
(def ^:private status :request/status)
(def ^:private status-type
  {:type :enum :name :request/status :values #{:open :claimed :done}})
(def ^:private int-type {:type :integer})
(def ^:private bool-type {:type :boolean})

(defn- atomic [readers expression]
  [:atom {:readers readers :expression expression}])
(defn- gt [n]
  (atomic #{score} [:> [:read score] [:literal n]]))
(defn- ge [n]
  (atomic #{score} [:>= [:read score] [:literal n]]))
(defn- lt [n]
  (atomic #{score} [:< [:read score] [:literal n]]))
(defn- enabled-atom []
  (atomic #{enabled} [:read enabled]))
(defn- status-is [v]
  (atomic #{status} [:= [:read status] [:literal v]]))
(defn- source
  ([types form domains]
   (source types form domains 128))
  ([types form domains max-cases]
   {:proposition-source
    {:subject subject :variables types :proposition form}
    :binding-domains domains :max-cases max-cases}))
(defn- audit [s]
  (bridge/audit-typed-proposition! s))
(defn- no-authority! [r]
  (is (= bridge/report-type (:type r)) (pr-str r))
  (is (= bridge/bridge-version (:version r)))
  (is (= :original-typed-proposition-and-per-atom-finite-cross-check
         (:checking r)))
  (is (= :supplied-concrete-bindings-only (:scope r)))
  (doseq [k [:atomic-semantics-universality
             :propositional-kernel-soundness
             :independent-checker-soundness
             :trusted-binding-provenance
             :jvm-cljs-equivalence
             :production-certification]]
    (is (= :not-established (get r k)) (str k " " (pr-str r))))
  (is (= :not-performed (:premise-discharge r)))
  (is (= :not-performed (:kernel-admission r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (doseq [k [:certificate :admission-handle :proved? :valid? :proof-status]]
    (is (not (contains? r k)) (str "Forged authority key " k)))
  r)
(defn- success! [s cases true-cases coverage atom-count occurrences]
  (let [r (no-authority! (audit s))]
    (is (= :bounded-no-counterexample (:status r)) (pr-str r))
    (is (= cases (:cases-planned r)))
    (is (= cases (:cases-checked r)))
    (is (= true-cases (:true-cases r)))
    (is (= coverage (:domain-coverage r)))
    (is (= atom-count (:unique-atoms r)))
    (is (= occurrences (:atom-occurrences r)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:proposition-fingerprint r)))
    r))
(defn- failure! [s status reason]
  (let [r (no-authority! (audit s))]
    (is (= status (:status r)) (pr-str r))
    (is (= reason (:reason r)) (pr-str r))
    r))
(defn- counterexample! [s expected-binding expected-atom-values checked]
  (let [r (no-authority! (audit s))]
    (is (= :counterexample (:status r)) (pr-str r))
    (is (= checked (:cases-checked r)))
    (is (= expected-binding (get-in r [:counterexample :bindings])))
    (is (= expected-atom-values
           (mapv :value (get-in r [:counterexample :atom-values]))))
    (is (false? (get-in r [:counterexample :proposition-value])))
    r))

(deftest stronger-integer-predicate-implies-weaker-predicate
  (let [r (success! (source {score int-type}
                            [:implies (gt 7) (ge 7)]
                            {score [6 7 8 9]})
                    4 4 :representative-values 2 2)]
    (is (= 3 (:realized-atom-valuations r)))
    (is (= #{score} (:reader-ids r)))))

(deftest reversing-typed-entailment-exposes-a-concrete-counterexample
  (let [r (counterexample!
           (source {score int-type}
                   [:implies (ge 7) (gt 7)]
                   {score [6 7 8]})
           {score 7} [true false] 2)]
    (is (= 1 (:true-cases r)))
    (is (= 2 (:realized-atom-valuations r)))))

(deftest negation-and-implication-are-checked-over-real-bindings
  (let [p (gt 7)
        q (ge 7)]
    (success! (source {score int-type}
                      [:or [:not p] q] {score [6 7 8]})
              3 3 :representative-values 2 2)
    (counterexample! (source {score int-type}
                             [:and p [:not q]] {score [6 7 8]})
                     {score 6} [false false] 1)))

(deftest finite-sample-cannot-claim-integer-universality
  (let [s (source {score int-type}
                  [:implies (gt 7) (lt 10)] {score [6 7 8 9]})
        r (success! s 4 4 :representative-values 2 2)]
    (is (= :not-established (:atomic-semantics-universality r)))
    (counterexample! (assoc-in s [:binding-domains score] [6 7 8 9 10])
                     {score 10} [true false] 5)))

(deftest large-integers-are-not-rounded-by-the-jvm-audit
  (let [big 9007199254740993N]
    (success! (source {score int-type}
                      [:implies (gt 9007199254740992N)
                       (ge 9007199254740993N)]
                      {score [9007199254740992N big]})
              2 2 :representative-values 2 2)
    (counterexample!
     (source {score int-type} [:implies (ge big) (gt big)]
             {score [9007199254740992N big]})
     {score big} [true false] 2)))

(deftest boolean-complements-realize-only-consistent-pairs
  (let [p (enabled-atom)
        q (atomic #{enabled} [:not [:read enabled]])
        r (success! (source {enabled bool-type}
                            [:or p q] {enabled [false true]})
                    2 2 :complete-declared-finite-values 2 2)]
    (is (= 2 (:realized-atom-valuations r)))
    (is (= #{enabled} (:reader-ids r)))))

(deftest independent-boolean-atoms-realize-the-full-cross-product
  (let [other :request/approved
        p (enabled-atom)
        q (atomic #{other} [:read other])
        types {enabled bool-type other bool-type}
        domains {enabled [false true] other [false true]}
        r (counterexample! (source types [:or p q] domains)
                           {enabled false other false} [false false] 1)]
    (is (= :complete-declared-finite-values (:domain-coverage r)))
    (is (= 4 (:cases-planned r)))))

(deftest repeated-atomic-declarations-are-cross-checked-once
  (let [p (gt 7)
        r (success! (source {score int-type}
                            [:or p p [:not [:not p]]]
                            {score [8 9]})
                    2 2 :representative-values 1 3)]
    (is (= 1 (:realized-atom-valuations r)))))

(deftest no-reader-atoms-have-one-empty-projection
  (let [t (atomic #{} [:literal true])
        f (atomic #{} [:literal false])]
    (success! (source {} [:or t [:not f]] {})
              1 1 :complete-declared-finite-values 2 2)
    (counterexample! (source {} [:and t f] {}) {} [true false] 1)))

(deftest constant-propositions-have-one-empty-binding
  (let [yes (success! (source {} [:true] {})
                      1 1 :complete-declared-finite-values 0 0)]
    (is (= 1 (:realized-atom-valuations yes)))
    (counterexample! (source {} [:false] {}) {} [] 1)))

(deftest nested-connectives-preserve-logical-truth-values
  (let [p (enabled-atom)]
    (success! (source {enabled bool-type}
                      [:implies [:and p [:not [:not p]]]
                       [:or p [:false]]]
                      {enabled [false true]})
              2 2 :complete-declared-finite-values 1 3)
    (counterexample!
     (source {enabled bool-type}
             [:not [:or [:not p] [:and p [:true]]]]
             {enabled [false true]})
     {enabled false} [false] 1)))

(deftest enum-coverage-is-declared-finite-not-propositional-proof
  (let [r (success!
           (source {status status-type}
                   [:or (status-is :open) (status-is :claimed) (status-is :done)]
                   {status [:open :claimed :done]})
           3 3 :complete-declared-finite-values 3 3)]
    (is (= 3 (:realized-atom-valuations r)))
    (is (= :not-established (:propositional-kernel-soundness r)))))

(deftest missing-enum-value-must-not-count-as-complete-coverage
  (let [r (success!
           (source {status status-type}
                   [:or (status-is :open) (status-is :claimed)]
                   {status [:open :claimed]})
           2 2 :representative-values 2 2)]
    (is (= 2 (:cases-planned r)))
    (counterexample!
     (source {status status-type}
             [:or (status-is :open) (status-is :claimed)]
             {status [:open :claimed :done]})
     {status :done} [false false] 3)))

(deftest finite-enum-and-boolean-product-is-counted-exactly
  (let [p (enabled-atom)
        s (status-is :open)
        r (success! (source {status status-type enabled bool-type}
                            [:or p [:not p] s]
                            {status [:done :open :claimed]
                             enabled [false true]})
                    6 6 :complete-declared-finite-values 2 3)]
    (is (= 6 (:cases-planned r)))
    (is (= 4 (:realized-atom-valuations r)))))

(deftest nullable-boolean-has-distinct-nil-and-false-bindings
  (let [p (atomic #{enabled} [:present [:read enabled]])
        r (success!
           (source {enabled {:type :boolean :nullable? true}}
                   [:or p [:not p]] {enabled [nil false true]})
           3 3 :complete-declared-finite-values 1 2)]
    (is (= 1 (:realized-atom-valuations r)))))

(deftest optional-boolean-has-distinct-absent-and-present-false
  (let [p (atomic #{enabled} [:present [:read enabled]])
        r (success!
           (source {enabled {:type :boolean :optional? true}}
                   [:or p [:not p]]
                   {enabled [oracle/absent false true]})
           3 3 :complete-declared-finite-values 1 2)]
    (is (= 2 (:realized-atom-valuations r)))))

(deftest optional-nullable-boolean-domain-can-be-complete
  (let [p (atomic #{enabled} [:present [:read enabled]])
        r (success!
           (source {enabled {:type :boolean :optional? true :nullable? true}}
                   [:or p [:not p]]
                   {enabled [oracle/absent nil false true]})
           4 4 :complete-declared-finite-values 1 2)]
    (is (= 2 (:realized-atom-valuations r)))))

(deftest guarded-optional-integer-rule-is-an-atom-not-cross-atom-refinement
  (let [r [:read score]
        present (atomic #{score} [:present r])
        guarded (atomic #{score}
                        [:and [:present r]
                         [:not= r [:literal nil]]
                         [:>= r [:literal 7]]])
        result (success!
                (source {score {:type :integer :optional? true :nullable? true}}
                        [:implies guarded present]
                        {score [oracle/absent nil 6 7 8]})
                5 5 :representative-values 2 2)]
    (is (= 3 (:realized-atom-valuations result)))))

(deftest optional-field-guard-in-another-atom-cannot-refine-a-read
  (let [r [:read score]
        presence (atomic #{score} [:present r])
        s (source {score {:type :integer :optional? true}}
                  [:implies presence (gt 7)] {score [oracle/absent 8]})]
    (failure! s :invalid-source :invalid-original-proposition)))

(deftest nullable-field-guard-in-another-atom-cannot-refine-a-read
  (let [r [:read score]
        nonnil (atomic #{score} [:not= r [:literal nil]])
        s (source {score {:type :integer :nullable? true}}
                  [:and nonnil (gt 7)] {score [nil 8]})]
    (failure! s :invalid-source :invalid-original-proposition)))

(deftest domain-must-have-exact-reader-keys
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (let [missing (failure! (assoc s :binding-domains {})
                            :invalid-source :wrong-keys)
          extra (failure! (assoc s :binding-domains
                                 {score [6 8] enabled [true false]})
                          :invalid-source :wrong-keys)]
      ;; The bridge deliberately exposes the failing path, not internal
      ;; validator details or an authority-bearing normalized source.
      (is (= [:binding-domains] (:at missing)))
      (is (= [:binding-domains] (:at extra))))))

(deftest duplicated-and-empty-domains-fail-before-atom-auditing
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (failure! (assoc-in s [:binding-domains score] [6 6])
              :invalid-source :duplicate-domain-entry)
    (failure! (assoc-in s [:binding-domains score] [])
              :invalid-source :invalid-domain)))

(deftest domain-must-be-a-plain-unannotated-vector
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (doseq [xs ['(6 8) #{6 8} (with-meta [6 8] {:trusted true})
                (range 2)]]
      (failure! (assoc-in s [:binding-domains score] xs)
                :invalid-source :invalid-domain))))

(deftest missing-optional-and-nullable-values-cannot-be-certified-as-complete
  (let [t {enabled {:type :boolean :optional? true :nullable? true}}
        p (atomic #{enabled} [:present [:read enabled]])]
    (doseq [values [[false true] [oracle/absent false true]
                    [nil false true]]]
      (is (= :representative-values
             (:domain-coverage
              (success! (source t [:or p [:not p]] {enabled values})
                        (count values) (count values)
                        :representative-values 1 2)))))))

(deftest missing-required-domain-or-wrong-typed-value-fails-closed
  (let [s (source {score int-type} [:implies (gt 7) (ge 7)]
                  {score [7 8]})]
    (let [r (failure! (assoc-in s [:binding-domains score] [7 nil])
                      :atomic-cross-check-failed :invalid-source)]
      (is (keyword? (:atom-reason r)))
      (is (= 0 (:cases-checked r))))
    (failure! (assoc-in s [:binding-domains score] [7 true])
              :atomic-cross-check-failed :invalid-source)))

(deftest invalid-source-keys-never-supply-authority
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (doseq [extra [:proved? :valid? :certificate :admission
                   :authority :transaction-authority :kernel-admission
                   :fingerprint :proof-status :status]]
      (failure! (assoc s extra :checked) :invalid-source :wrong-keys))
    (doseq [removed [:proposition-source :binding-domains :max-cases]]
      (failure! (dissoc s removed) :invalid-source :wrong-keys))))

(deftest fabricated-proposition-description-is-not-original-source
  (let [s (source {score int-type} (gt 7) {score [6 8]})
        desc (proposition/describe-proposition! (:proposition-source s))]
    (failure! (assoc s :proposition-source desc)
              :invalid-source :invalid-original-proposition)
    (failure! (assoc s :proposition-source
                     (assoc desc :proof-status :proved :authority :commit))
              :invalid-source :invalid-original-proposition)))

(deftest original-subject-and-formula-changes-change-the-identity
  (let [s (source {score int-type}
                  [:or (gt 7) [:not (gt 7)]] {score [6 8]})
        a (success! s 2 2 :representative-values 1 2)
        b (success! (-> s
                        (assoc-in [:proposition-source :subject :id]
                                  :example/revised)
                        (assoc-in [:proposition-source :subject :interface :operation]
                                  :example/revised))
                    2 2 :representative-values 1 2)]
    ;; Subject identity includes its exact operation interface.
    (is (not= (:proposition-fingerprint a)
              (:proposition-fingerprint b)))))

(deftest reported-counterexample-is-deterministic
  (let [s (source {score int-type}
                  [:implies (gt 7) (lt 8)] {score [0 1 8 9]})]
    (is (= (audit s) (audit s)))
    (is (= {score 8} (get-in (audit s) [:counterexample :bindings])))))

(deftest finite-bindings-are-enumerated-in-sorted-reader-order
  (let [other :request/approved
        s (source {score int-type other bool-type}
                  [:or (gt 10) (atomic #{other} [:read other])]
                  {score [6 9] other [false true]})
        r (audit s)]
    (is (= :counterexample (:status r)))
    (is (= {score 6 other false} (get-in r [:counterexample :bindings])))
    (is (= 1 (:cases-checked r)))))

(deftest source-and-domain-map-insertion-order-do-not-change-diagnostics
  (let [p [:implies (gt 7) (enabled-atom)]
        types (array-map score int-type enabled bool-type)
        domains (array-map score [6 8] enabled [false true])
        s (source types p domains)
        t (source (into {} (reverse (seq types))) p
                  (into {} (reverse (seq domains))))]
    (is (= (audit s) (audit t)))))

(deftest budget-exhaustion-never-counts-as-a-clean-audit
  (let [s (source {score int-type enabled bool-type}
                  [:or (gt 7) [:not (gt 7)] (enabled-atom)]
                  {score [6 7 8] enabled [false true]} 5)
        r (failure! s :budget-exceeded :enumeration-limit)]
    (is (= 6 (:cases-planned r)))
    (is (= 0 (:cases-checked r)))
    (success! (assoc s :max-cases 6)
              6 6 :representative-values 2 3)))

(deftest budgets-must-be-positive-and-within-fixed-cap
  (let [s (source {score int-type} (gt 7) {score [8]})]
    (doseq [n [nil 0 -1 8193 5.5 "8" true]]
      (failure! (assoc s :max-cases n) :invalid-source :invalid-budget))))

(deftest oversized-single-reader-domain-is-rejected
  (let [s (source {score int-type} (gt 7)
                  {score (vec (range 25))} 128)]
    (failure! s :invalid-source :invalid-domain)))

(deftest invalid-proposition-shapes-and-reader-footprints-are-rejected
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (doseq [p [[:bogus (gt 7)] [:and (gt 7)]
               [:implies (gt 7)] [:not] [:atom {:readers #{}
                                                :expression [:read score]}]
               [:atom {:readers #{score} :expression [:literal 7]}]]]
      (failure! (assoc-in s [:proposition-source :proposition] p)
                :invalid-source :invalid-original-proposition))))

(deftest changing-a-valid-enum-name-changes-proposition-identity
  (let [s (source {status status-type}
                  [:or (status-is :open) [:not (status-is :open)]]
                  {status [:open :claimed :done]})
        before (success! s 3 3 :complete-declared-finite-values 1 2)
        renamed (success! (assoc-in s [:proposition-source :variables status :name]
                                     :other/status)
                          3 3 :complete-declared-finite-values 1 2)]
    (is (not= (:proposition-fingerprint before)
              (:proposition-fingerprint renamed)))
    (failure! (assoc-in s [:proposition-source :variables status :values]
                        #{:claimed :done})
              :invalid-source :invalid-original-proposition)))

(deftest nested-and-top-level-metadata-cannot-attest-a-proof
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (doseq [x [(with-meta s {:valid? true})
               (update s :binding-domains
                       #(with-meta % {:proof :checked}))
               (assoc-in s [:binding-domains score]
                         (with-meta [6 8] {:admitted true}))
               (assoc-in s [:proposition-source :proposition]
                         (with-meta (gt 7) {:proved true}))]]
      (is (= :invalid-source (:status (audit x))) (pr-str x)))))

(deftest unsupported-executable-atomic-data-cannot-be-submitted
  (let [s (source {} [:true] {})]
    (doseq [p [[:atom {:readers #{} :expression (fn [] true)}]
               [:atom {:readers #{} :expression [:literal (fn [] true)]}]]]
      (failure! (assoc-in s [:proposition-source :proposition] p)
                :invalid-source :invalid-original-proposition))))

(deftest replacing-public-atom-checker-does-not-issue-a-valid-bridge
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (with-redefs [oracle/audit-typed-atom! (fn [_] {:status :bounded-agreement})]
      (failure! s :invalid-source :substituted-checker))))

(deftest replacing-public-interpreter-does-not-issue-a-valid-bridge
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (with-redefs [production/evaluate-rule! (fn [& _] {:value true})]
      (failure! s :invalid-source :substituted-checker))))

(deftest replacing-public-proposition-checker-does-not-issue-a-valid-bridge
  (let [s (source {score int-type} (gt 7) {score [6 8]})]
    (with-redefs [proposition/describe-proposition!
                  (fn [_] {:fingerprint "sha256:forged"})]
      (failure! s :invalid-source :substituted-checker))))
