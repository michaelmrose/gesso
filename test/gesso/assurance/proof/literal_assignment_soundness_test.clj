(ns gesso.assurance.proof.literal-assignment-soundness-test
  "Permanent v1356 regression tests for the independent, conditional S4
   literal-assignment rule. A matching abstract witness, a diagnostic lemma,
   and a universal/physical proof are distinct. Assert strict source and
   typed admissibility, exact reader projections, and open outcomes.
   These tests never confer certificates or runtime authority."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as production]))

(def transition
  {:subject {:kind :operation :id :example/update :interface {:operation :example/update}}
   :state-types {:request/status {:type :enum :name :request/status :values #{:open :claimed}}
                 :request/owner {:type :keyword :nullable? true}
                 :request/version {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/note :request/owner}
   :assignments {:request/status [:literal :claimed]
                 :request/note [:literal "saved"]
                 :request/owner [:read :input/actor]}})
(def before {:request/status :open :request/owner nil :request/version 4})
(def input {:input/actor :user/a})
(def after {:request/status :claimed :request/owner :user/a
            :request/version 4 :request/note "saved"})
(def status-invariant
  {:id :example/status-claimed :readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :claimed]]})
(def note-invariant
  {:id :example/note-saved :readers #{:request/note}
   :expression [:and [:present [:read :request/note]]
                [:not= [:read :request/note] [:literal nil]]
                [:= [:read :request/note] [:literal "saved"]]]})
(def compound-invariant
  {:id :example/compound :readers #{:request/status :request/note}
   :expression [:and [:= [:read :request/status] [:literal :claimed]]
                [:present [:read :request/note]]
                [:= [:read :request/note] [:literal "saved"]]]})
(def false-invariant
  {:id :example/status-open :readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :open]]})
(def frame-invariant
  {:id :example/version :readers #{:request/version}
   :expression [:>= [:read :request/version] [:literal 0]]})
(def copy-invariant
  {:id :example/owner :readers #{:request/owner}
   :expression [:not= [:read :request/owner] [:literal nil]]})
(defn- rejected? [f]
  (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(deftest literal-rule-instances
  (doseq [invariant [status-invariant note-invariant compound-invariant]]
    (let [l (literal/derive-literal-lemma! transition invariant)
          audit (literal/audit-literal-witness! transition invariant before input after)]
      (is (= :conditional-literal-rule-instance (:status l)))
      (is (= true (:fixed-post-invariant l)))
      (is (= :matching-transition-establishes-post-invariant (:conditional-claim l)))
      (is (= :checked-conditional-literal-instance (:status audit)))
      (is (true? (:matches-transition? audit)))
      (is (true? (:fixed-post-reader-projection? audit)))
      (is (true? (:after-invariant audit)))
      (is (= :not-issued (:certificate audit)))
      (is (= :none (:authority audit))))))

(deftest invariant-with-false-fixed-post-stays-open
  (let [l (literal/derive-literal-lemma! transition false-invariant)
        audit (literal/audit-literal-witness! transition false-invariant before input after)]
    (is (= :open-false-postcondition (:status l)))
    (is (false? (:fixed-post-invariant l)))
    (is (nil? (:conditional-claim l)))
    (is (= :open-false-postcondition (:status audit)))
    (is (true? (:before-invariant audit)))
    (is (false? (:after-invariant audit)))
    (is (false? (:preservation-implication-holds? audit)))))

(deftest frame-and-copy-dependencies-stay-open
  (doseq [invariant [frame-invariant copy-invariant]]
    (let [l (literal/derive-literal-lemma! transition invariant)
          audit (literal/audit-literal-witness! transition invariant before input after)]
      (is (= :open-nonliteral-reader (:status l)))
      (is (= :open-nonliteral-reader (:status audit)))
      (is (nil? (:conditional-claim l)))))
  (let [inv {:id :example/closed :readers #{} :expression [:literal true]}]
    (is (= :open-empty-reader-footprint (:status
                                         (literal/derive-literal-lemma! transition inv))))))

(deftest guard-and-assignment-are-prestate-based
  (let [invalid-pre (assoc before :request/status :claimed)
        audit (literal/audit-literal-witness! transition status-invariant invalid-pre input after)]
    (is (= :nonmatching-transition (:status audit)))
    (is (false? (:matches-transition? audit))))
  (doseq [bad-after [(assoc after :request/status :open)
                     (assoc after :request/note "draft")
                     (dissoc after :request/note)
                     (assoc after :request/version 8)
                     (assoc after :request/owner :user/b)]]
    (let [audit (literal/audit-literal-witness! transition status-invariant
                                                before input bad-after)]
      (is (= :nonmatching-transition (:status audit)))
      (is (false? (:matches-transition? audit))))))

(deftest source-integrity-and-typing
  (doseq [bad [(assoc compound-invariant :readers #{:request/status})
               (assoc compound-invariant :readers #{:request/status :request/note :request/version})
               (assoc status-invariant :extra true)
               (assoc status-invariant :id :invalid)
               (with-meta status-invariant {:trusted true})
               (assoc status-invariant :expression [:bogus])
               (assoc status-invariant :expression [:= [:read :request/status] [:literal 1]])]]
    (is (rejected? #(literal/derive-literal-lemma! transition bad)) (pr-str bad)))
  (doseq [bad [(assoc transition :writes #{:request/status})
               (assoc transition :reads #{:request/status})
               (assoc-in transition [:assignments :request/note] [:literal 7])
               (assoc-in transition [:assignments :request/status] [:bogus :claimed])]]
    (is (rejected? #(literal/derive-literal-lemma! bad status-invariant)))))

(deftest changes-invalidate-source-fingerprint
  (let [original (literal/derive-literal-lemma! transition status-invariant)
        modified (literal/derive-literal-lemma!
                  (assoc-in transition [:assignments :request/status] [:literal :open])
                  status-invariant)
        changed-invariant (literal/derive-literal-lemma!
                           transition (assoc status-invariant :expression
                                             [:not= [:read :request/status] [:literal :open]]))]
    (is (not= (:transition-fingerprint original) (:transition-fingerprint modified)))
    (is (= :open-false-postcondition (:status modified)))
    (is (not= (:typed-invariant-fingerprint original)
              (:typed-invariant-fingerprint changed-invariant)))))

(deftest fabricated-observation-cannot-discharge-lemma
  (with-redefs [independent/observe-transition! (fn [& _] {:matches? true})]
    (is (rejected? #(literal/audit-literal-witness!
                     transition status-invariant before input
                     (assoc after :request/status :open)))))
  (let [r (literal/derive-literal-lemma! transition status-invariant)]
    (is (= :not-admitted (:admission r)))
    (is (= :conditional-schema-only (:proof-status r)))
    (is (= r (clojure.edn/read-string (pr-str r))))))

(deftest optional-nullable-literal-is-present
  (let [s (assoc-in transition [:assignments :request/note] [:literal nil])
        inv {:id :example/null-present :readers #{:request/note}
             :expression [:and [:present [:read :request/note]]
                          [:= [:read :request/note] [:literal nil]]]}
        l (literal/derive-literal-lemma! s inv)
        after-nil (assoc after :request/note nil)
        o (literal/audit-literal-witness! s inv before input after-nil)]
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (= {:request/note nil} (:fixed-post-reader-projection l)))
    (is (= :checked-conditional-literal-instance (:status o)))
    (is (true? (:after-invariant o)))))

;; v1356 permanent adversarial coverage. These tests check both the admissible
;; abstract schema and the ways in which it must REFUSE to infer preservation.

(defn- checked-lemma [inv]
  (literal/derive-literal-lemma! transition inv))

(defn- checked-witness
  ([inv] (checked-witness inv before input after))
  ([inv b i a]
   (literal/audit-literal-witness! transition inv b i a)))

(defn- rejection-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest v1356-exact-lemma-contract
  (let [l (checked-lemma status-invariant)
        described (production/describe-transition! transition)]
    (is (= literal/report-type (:type l)))
    (is (= literal/semantics-version (:version l)))
    (is (= (:fingerprint described) (:transition-fingerprint l)))
    (is (= status-invariant (:source-invariant l)))
    (is (= :example/status-claimed (:invariant-id l)))
    (is (= #{:request/status} (:readers l)))
    (is (= {:request/status :claimed :request/note "saved"} (:literal-writes l)))
    (is (= {:request/status :claimed} (:fixed-post-reader-projection l)))
    (is (empty? (:nonliteral-readers l)))
    (is (= :literal-writes-fix-entire-invariant-reader-projection (:schema l)))
    (is (= #{:valid-typed-invariant
             :exact-abstract-literal-assignment-relation
             :pure-typed-expression-extensionality}
           (:premises l)))
    (is (= :conditional-schema-only (:proof-status l)))
    (is (= :not-admitted (:admission l)))
    (is (= :none (:authority l)))))

(deftest v1356-compound-literal-postcondition
  (let [l (checked-lemma compound-invariant)
        w (checked-witness compound-invariant)]
    (is (= #{:request/status :request/note} (:readers l)))
    (is (= {:request/status :claimed :request/note "saved"}
           (:fixed-post-reader-projection l)))
    (is (true? (:fixed-post-invariant l)))
    (is (true? (:fixed-post-reader-projection? w)))
    (is (true? (:matches-transition? w)))
    (is (true? (:after-invariant w)))))

(deftest v1356-fixed-boolean-invariant-can-be-false-before
  (let [l (checked-lemma status-invariant)
        w (checked-witness status-invariant)]
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (false? (:before-invariant w)))
    (is (true? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))))

(deftest v1356-false-fixed-invariant-is-never-a-lemma
  (let [l (checked-lemma false-invariant)
        w (checked-witness false-invariant)]
    (is (= :open-false-postcondition (:status l)))
    (is (false? (:fixed-post-invariant l)))
    (is (nil? (:conditional-claim l)))
    (is (= :open-false-postcondition (:status w)))
    (is (false? (:preservation-implication-holds? w)))
    (is (= :not-universally-proved (:proof-status w)))))

(deftest v1356-false-fixed-invariant-not-proved-by-vacuity
  (let [inv {:id :example/note-is-other
             :readers #{:request/note}
             :expression [:and [:present [:read :request/note]]
                          [:= [:read :request/note] [:literal "other"]]]}
        l (checked-lemma inv)
        w (checked-witness inv)]
    (is (= :open-false-postcondition (:status l)))
    (is (false? (:before-invariant w)))
    (is (false? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))
    (is (= :open-false-postcondition (:status w)))))

(deftest v1356-frame-and-copy-must-remain-open
  (doseq [[inv reader] [[frame-invariant :request/version]
                         [copy-invariant :request/owner]]]
    (let [l (checked-lemma inv)
          w (checked-witness inv)]
      (is (= :open-nonliteral-reader (:status l)))
      (is (= #{reader} (:nonliteral-readers l)))
      (is (nil? (:fixed-post-invariant l)))
      (is (nil? (:conditional-claim l)))
      (is (= :open-nonliteral-reader (:status w))))))

(deftest v1356-mixed-readers-remain-open
  (let [inv {:id :example/mixed
             :readers #{:request/status :request/version}
             :expression [:and
                          [:= [:read :request/status] [:literal :claimed]]
                          [:>= [:read :request/version] [:literal 0]]]}
        l (checked-lemma inv)
        w (checked-witness inv)]
    (is (= :open-nonliteral-reader (:status l)))
    (is (= #{:request/version} (:nonliteral-readers l)))
    (is (nil? (:fixed-post-invariant l)))
    (is (= :open-nonliteral-reader (:status w)))
    (is (true? (:after-invariant w)))))

(deftest v1356-empty-footprint-remains-open-even-if-true
  (doseq [v [true false]]
    (let [inv {:id :example/constant :readers #{} :expression [:literal v]}
          l (checked-lemma inv)
          w (checked-witness inv)]
      (is (= :open-empty-reader-footprint (:status l)))
      (is (= :open-empty-reader-footprint (:status w)))
      (is (nil? (:conditional-claim l)))
      (is (nil? (:fixed-post-invariant l)))
      (is (= v (:before-invariant w)))
      (is (= v (:after-invariant w))))))

(deftest v1356-assigned-optional-field-becomes-present
  (let [l (checked-lemma note-invariant)
        w (checked-witness note-invariant)]
    (is (not (contains? before :request/note)))
    (is (contains? after :request/note))
    (is (= "saved" (get (:fixed-post-reader-projection l) :request/note)))
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (true? (:after-invariant w)))))

(deftest v1356-present-nil-is-not-absent
  (let [tr (assoc-in transition [:assignments :request/note] [:literal nil])
        inv {:id :example/nil-is-present :readers #{:request/note}
             :expression [:and [:present [:read :request/note]]
                          [:= [:read :request/note] [:literal nil]]]}
        l (literal/derive-literal-lemma! tr inv)
        w (literal/audit-literal-witness! tr inv before input
                                           (assoc after :request/note nil))]
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (contains? (:fixed-post-reader-projection l) :request/note))
    (is (nil? (get (:fixed-post-reader-projection l) :request/note)))
    (is (true? (:after-invariant w)))
    (is (true? (:fixed-post-reader-projection? w)))))

(deftest v1356-present-nil-does-not-prove-nonnull
  (let [tr (assoc-in transition [:assignments :request/note] [:literal nil])
        inv {:id :example/note-is-nonnull :readers #{:request/note}
             :expression [:and [:present [:read :request/note]]
                          [:not= [:read :request/note] [:literal nil]]]}
        l (literal/derive-literal-lemma! tr inv)]
    (is (= :open-false-postcondition (:status l)))
    (is (false? (:fixed-post-invariant l)))))

(deftest v1356-enum-postcondition-is-nominal
  (let [inv {:id :example/status-in-domain :readers #{:request/status}
             :expression [:in [:read :request/status]
                          [:literal #{:open :claimed}]]}
        l (checked-lemma inv)]
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (true? (:fixed-post-invariant l)))))

(deftest v1356-guard-always-false-does-not-become-proof-of-execution
  (let [tr (assoc transition :guard
                  {:readers #{} :expression [:literal false]}
                  :reads #{:input/actor})
        l (literal/derive-literal-lemma! tr status-invariant)
        w (literal/audit-literal-witness! tr status-invariant before input after)]
    (is (= :conditional-literal-rule-instance (:status l)))
    (is (= :nonmatching-transition (:status w)))
    (is (false? (:matches-transition? w)))
    (is (= :not-universally-proved (:proof-status w)))))

(deftest v1356-prestate-guard-is-not-poststate-guard
  (let [b (assoc before :request/status :claimed)
        w (checked-witness status-invariant b input after)]
    (is (= :nonmatching-transition (:status w)))
    (is (false? (:matches-transition? w)))
    (is (true? (:after-invariant w)))))

(deftest v1356-wrong-literal-or-missing-target-never-matches
  (doseq [a [(assoc after :request/status :open)
             (assoc after :request/note "changed")
             (dissoc after :request/note)]]
    (let [w (checked-witness status-invariant before input a)]
      (is (= :nonmatching-transition (:status w)) (pr-str a))
      (is (false? (:matches-transition? w))))))

(deftest v1356-copy-output-and-frame-violations-never-match
  (doseq [a [(assoc after :request/owner :user/b)
             (assoc after :request/version 5)]]
    (let [w (checked-witness status-invariant before input a)]
      (is (= :nonmatching-transition (:status w)))
      (is (false? (:matches-transition? w))))))

(deftest v1356-optional-frame-presence-must-be-preserved
  (let [tr (-> transition
               (assoc :writes #{:request/status :request/owner}
                      :assignments {:request/status [:literal :claimed]
                                    :request/owner [:read :input/actor]}))
        l (literal/derive-literal-lemma! tr status-invariant)]
    (is (= :conditional-literal-rule-instance (:status l)))
    (doseq [[b a matches?] [[before (dissoc after :request/note) true]
                            [before (assoc after :request/note nil) false]
                            [(assoc before :request/note "memo")
                             (assoc after :request/note "memo") true]
                            [(assoc before :request/note "memo")
                             (assoc after :request/note "other") false]]]
      (let [w (literal/audit-literal-witness! tr status-invariant b input a)]
        (is (= matches? (:matches-transition? w)))
        (is (= (if matches? :checked-conditional-literal-instance
                   :nonmatching-transition)
               (:status w)))))))

(deftest v1356-required-field-removal-is-a-rejection
  (doseq [a [(dissoc after :request/version)
             (dissoc after :request/status)
             (dissoc after :request/owner)]]
    (is (rejected? #(checked-witness status-invariant before input a)))))

(deftest v1356-typed-state-and-input-rejections
  (doseq [[b i a] [[(assoc before :request/version "wrong") input after]
                   [before {:input/actor 9} after]
                   [before input (assoc after :request/status :unknown)]
                   [(assoc before :request/note 7) input after]
                   [before input (assoc after :request/untracked 1)]]]
    (is (rejected? #(checked-witness status-invariant b i a)))))

(deftest v1356-hidden-reader-footprints-rejected
  (doseq [inv [(assoc compound-invariant :readers #{:request/status})
               (assoc compound-invariant :readers #{:request/status :request/version})
               (assoc status-invariant :readers #{:request/note})
               (assoc status-invariant :readers #{})]]
    (is (rejected? #(checked-lemma inv)) (pr-str inv))))

(deftest v1356-typed-invariant-errors-rejected
  (doseq [inv [(assoc status-invariant :expression [:literal 7])
               (assoc status-invariant :expression
                      [:> [:read :request/status] [:literal 4]])
               (assoc status-invariant :expression [:bogus :claimed])
               (assoc status-invariant :expression
                      [:= [:read :input/actor] [:literal :user/a]])]]
    (is (rejected? #(checked-lemma inv)) (pr-str inv))))

(deftest v1356-invalid-literal-or-assignment-rejected
  (doseq [tr [(assoc-in transition [:assignments :request/status] [:literal :unknown])
              (assoc-in transition [:assignments :request/note] [:literal 1])
              (assoc-in transition [:assignments :request/note] [:effect println])
              (assoc transition :writes #{:request/status})
              (assoc transition :assignments {:request/status [:literal :claimed]})]]
    (is (rejected? #(literal/derive-literal-lemma! tr status-invariant))
        (pr-str tr))))

(deftest v1356-falsified-source-and-metadata-rejected
  (doseq [inv [(assoc status-invariant :id :unqualified)
               (assoc status-invariant :unexpected true)
               (with-meta status-invariant {:certified true})
               (assoc status-invariant :expression
                      (with-meta (:expression status-invariant) {:certified true}))]]
    (is (rejected? #(checked-lemma inv))))
  (doseq [tr [(with-meta transition {:certified true})
              (assoc transition :unexpected true)
              (assoc-in transition [:guard :expression]
                        (with-meta (:expression (:guard transition)) {:trusted true}))]]
    (is (rejected? #(literal/derive-literal-lemma! tr status-invariant)))))

(deftest v1356-source-and-invariant-fingerprints-are-bound
  (let [baseline (checked-lemma status-invariant)
        status-change (literal/derive-literal-lemma!
                       (assoc-in transition [:assignments :request/status]
                                 [:literal :open]) status-invariant)
        rule-change (checked-lemma (assoc status-invariant :expression
                                          [:not= [:read :request/status]
                                           [:literal :open]]))]
    (is (not= (:transition-fingerprint baseline)
              (:transition-fingerprint status-change)))
    (is (not= (:typed-invariant-fingerprint baseline)
              (:typed-invariant-fingerprint rule-change)))
    (is (= :open-false-postcondition (:status status-change)))
    (is (= :conditional-literal-rule-instance (:status rule-change)))))

(deftest v1356-witness-never-issues-authority
  (doseq [inv [status-invariant note-invariant false-invariant
               frame-invariant copy-invariant]]
    (let [l (checked-lemma inv)
          w (checked-witness inv)]
      (is (= :conditional-schema-only (:proof-status l)))
      (is (= :not-issued (:certificate l)))
      (is (= :not-admitted (:admission l)))
      (is (= :none (:authority l)))
      (is (= :not-universally-proved (:proof-status w)))
      (is (= :not-established (:physical-observation w)))
      (is (= :not-issued (:certificate w)))
      (is (= :not-admitted (:admission w)))
      (is (= :none (:authority w))))))

(deftest v1356-plain-edn-reports-are-copyable-not-credentials
  (let [l (checked-lemma status-invariant)
        w (checked-witness status-invariant)]
    (is (= l (edn/read-string (pr-str l))))
    (is (= w (edn/read-string (pr-str w))))
    (is (= :none (:authority (edn/read-string (pr-str w)))))))

(deftest v1356-reject-forged-observation-identity
  (let [base (independent/observe-transition! transition before input after)]
    (doseq [forged [(assoc base :source-declaration (assoc transition :writes #{}))
                   (assoc base :before (assoc before :request/version 99))
                   (assoc base :inputs {:input/actor :user/b})
                   (assoc base :after (assoc after :request/status :open))
                   (assoc base :type :attacker/observation)
                   (assoc base :version -1)
                   (assoc base :checking :attacker/checked)
                   (assoc base :proof-status :proved)
                   (assoc base :admission :admitted)
                   (assoc base :authority :xtdb)]]
      (with-redefs [independent/observe-transition! (fn [& _] forged)]
        (is (rejected? #(checked-witness status-invariant))
            (pr-str (select-keys forged [:type :version :checking :authority])))))))

(deftest v1356-reject-forged-observation-internal-consistency
  (let [base (independent/observe-transition! transition before input after)]
    (doseq [forged [(assoc base :matches? false)
                   (assoc base :guard-holds? false)
                   (assoc base :assignments-hold? false)
                   (assoc base :frame-holds? false)
                   (assoc base :matches? nil)]]
      (with-redefs [independent/observe-transition! (fn [& _] forged)]
        (is (rejected? #(checked-witness status-invariant)))))))

(deftest v1356-independent-and-production-finite-observation-grid
  (let [n (atom 0)]
    (doseq [status [:open :claimed]
            version [-3 0 7]
            actor [:user/a :user/b]
            note [::absent nil "draft"]]
      (let [b (cond-> (assoc before :request/status status
                              :request/version version)
                (not= note ::absent) (assoc :request/note note))
            a (assoc after :request/owner actor :request/version version
                     :request/note "saved")
            i {:input/actor actor}
            ind (independent/observe-transition! transition b i a)
            prod (production/observe-transition! transition b i a)
            witness (checked-witness status-invariant b i a)
            expected (= status :open)]
        (swap! n inc)
        (is (= expected (:matches? ind)))
        (is (= expected (:matches? prod)))
        (is (= expected (:matches-transition? witness)))
        (is (= (if expected :checked-conditional-literal-instance
                   :nonmatching-transition)
               (:status witness)))
        (is (true? (:after-invariant witness)))))
    (is (= 36 @n))))

(deftest v1356-invalid-observation-rejection-is-not-a-counterexample
  (let [bad (dissoc after :request/status)
        r (rejection-data #(checked-witness status-invariant before input bad))]
    (is (map? r))
    (is (contains? r :reason))
    (is (not= :checked-conditional-literal-instance (:status r)))))
