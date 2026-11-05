(ns gesso.assurance.proof.preservation-composition-soundness-test
  "Permanent v1361 adversarial regression for independent S4 conjunction
   composition. All fragments must be independently typed and justified by
   one of the frame, literal, or same-origin-copy conditional schemas.
   Concrete witnesses and finite models are diagnostic, NOT universal proofs
   or authorization for real transactions."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.preservation-composition-soundness :as compose]
            [gesso.assurance.proof.frame-soundness :as frame]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.field-copy-soundness :as copy]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as production]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.literal-assignment-soundness-test :as literal-fixture]
            [gesso.assurance.proof.field-copy-soundness-test :as copy-fixture]))

(def transition literal-fixture/transition)
(def before literal-fixture/before)
(def inputs literal-fixture/input)
(def after literal-fixture/after)
(def copy-transition copy-fixture/base)
(def copy-before copy-fixture/before)
(def copy-after copy-fixture/after)

(def frame-and-literal
  {:id :composition/frame-literal
   :readers #{:request/status :request/version}
   :expression [:and [:>= [:read :request/version] [:literal 0]]
                [:= [:read :request/status] [:literal :claimed]]]})
(def open-third
  {:id :composition/unsupported-third
   :readers #{:request/status :request/version :request/owner}
   :expression [:and [:>= [:read :request/version] [:literal 0]]
                [:= [:read :request/status] [:literal :claimed]]
                [:not= [:read :request/owner] [:literal nil]]]})
(def copy-and-frame
  {:id :composition/copy-frame
   :readers #{:doc/a :doc/b :doc/status}
   :expression [:and [:= [:read :doc/a] [:read :doc/b]]
                [:= [:read :doc/status] [:literal :open]]]})
(def all-transition
  (-> transition
      (assoc-in [:state-types :request/copy1] {:type :keyword})
      (assoc-in [:state-types :request/copy2] {:type :keyword})
      (update :writes into #{:request/copy1 :request/copy2})
      (assoc-in [:assignments :request/copy1] [:read :input/actor])
      (assoc-in [:assignments :request/copy2] [:read :input/actor])))
(def all-invariant
  {:id :composition/frame-literal-copy
   :readers #{:request/version :request/status :request/copy1 :request/copy2}
   :expression [:and [:>= [:read :request/version] [:literal 0]]
                [:= [:read :request/status] [:literal :claimed]]
                [:= [:read :request/copy1] [:read :request/copy2]]]})
(def all-before (assoc before :request/copy1 :old/a :request/copy2 :old/b))
(def all-after (assoc after :request/copy1 :user/a :request/copy2 :user/a))

(defn- failure-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))
(defn- reject? [f] (some? (failure-data f)))
(defn- no-authority! [report]
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not= :proved (:proof-status report))))
(defn- supported! [report families]
  (is (= compose/report-type (:type report)))
  (is (= compose/semantics-version (:version report)))
  (is (= :conditional-composed-rule-instance (:status report)))
  (is (= families (mapv :supporting-family (:clauses report))))
  (is (= (set (range (count families)))
         (:conditionally-supported-clause-indices report)))
  (is (empty? (:open-clause-indices report)))
  (is (= :invariant-and-matching-transition-imply-post-invariant
         (:conditional-claim report)))
  (no-authority! report))
(defn- open! [report indices]
  (is (= :open-unproved-clause (:status report)))
  (is (= indices (:open-clause-indices report)))
  (is (nil? (:conditional-claim report)))
  (no-authority! report))

(deftest mixed-frame-and-literal-clause-ledger
  (let [r (compose/derive-composed-lemma! transition frame-and-literal)]
    (supported! r [:frame :literal])
    (is (= :top-level-conjunction (:composition-shape r)))
    (is (= [:conditionally-supported-clause :conditionally-supported-clause]
           (mapv :status (:clauses r))))
    (is (= [#{:request/version} #{:request/status}]
           (mapv :readers (:clauses r))))
    (is (= [:frame :literal] (mapv #(-> % :tried-families last :family)
                                   (:clauses r))))))

(deftest mixed-frame-literal-matching-witness
  (let [r (compose/audit-composed-witness!
           transition frame-and-literal before inputs after)]
    (is (= compose/witness-type (:type r)))
    (is (= :checked-conditional-composition-instance (:status r)))
    (is (true? (:matches-transition? r)))
    (is (false? (:before-invariant r)))
    (is (true? (:after-invariant r)))
    (is (true? (:preservation-implication-holds? r)))
    (is (= :not-universally-proved (:proof-status r)))
    (is (= :not-established (:physical-observation r)))
    (no-authority! r)))

(deftest copy-with-frame-clause-ledger
  (let [r (compose/derive-composed-lemma! copy-transition copy-and-frame)]
    (supported! r [:copy :frame])
    (is (= #{:doc/a :doc/b} (get-in r [:clauses 0 :readers])))
    (is (= #{:doc/status} (get-in r [:clauses 1 :readers])))))

(deftest copy-with-frame-matching-witness
  (let [r (compose/audit-composed-witness!
           copy-transition copy-and-frame copy-before {} copy-after)]
    (is (= :checked-conditional-composition-instance (:status r)))
    (is (false? (:before-invariant r)))
    (is (true? (:after-invariant r)))
    (is (true? (:matches-transition? r)))
    (no-authority! r)))

(deftest all-three-families-compose
  (let [r (compose/derive-composed-lemma! all-transition all-invariant)]
    (supported! r [:frame :literal :copy])
    (is (= 3 (count (:clauses r))))
    (is (every? :typed-clause-fingerprint (:clauses r)))
    (is (every? #(contains? % :schema) (:clauses r)))))

(deftest all-three-families-matching-witness
  (let [r (compose/audit-composed-witness!
           all-transition all-invariant all-before inputs all-after)]
    (is (= :checked-conditional-composition-instance (:status r)))
    (is (true? (:matches-transition? r)))
    (is (false? (:before-invariant r)))
    (is (true? (:after-invariant r)))
    (is (= #{0 1 2} (:conditionally-supported-clause-indices r)))
    (no-authority! r)))

(deftest one-open-clause-opens-entire-composition
  (let [r (compose/derive-composed-lemma! transition open-third)]
    (open! r #{2})
    (is (= #{0 1} (:conditionally-supported-clause-indices r)))
    (is (= [:frame :literal nil] (mapv :supporting-family (:clauses r))))
    (is (= :open-no-supported-family (get-in r [:clauses 2 :status])))
    (is (= [:frame :literal :copy]
           (mapv :family (get-in r [:clauses 2 :tried-families]))))))

(deftest one-open-clause-cannot-upgrade-successful-witness
  (let [r (compose/audit-composed-witness!
           transition open-third before inputs after)]
    (is (= :open-unproved-clause (:status r)))
    (is (true? (:matches-transition? r)))
    (is (true? (:after-invariant r)))
    (is (= #{2} (:open-clause-indices r)))
    (no-authority! r)))

(deftest witness-where-open-clause-is-false-does-not-prove
  (let [r (compose/audit-composed-witness!
           transition open-third before inputs (assoc after :request/owner nil))]
    (is (= :open-unproved-clause (:status r)))
    (is (false? (:matches-transition? r)))
    (is (false? (:after-invariant r)))
    (no-authority! r)))

(deftest unsupported-middle-clause-does-not-hide-later-supported-one
  (let [inv (assoc open-third :expression
                   [:and [:= [:read :request/status] [:literal :claimed]]
                    [:not= [:read :request/owner] [:literal nil]]
                    [:>= [:read :request/version] [:literal 0]]])
        r (compose/derive-composed-lemma! transition inv)]
    (open! r #{1})
    (is (= [:literal nil :frame]
           (mapv :supporting-family (:clauses r))))
    (is (= #{0 2} (:conditionally-supported-clause-indices r)))))

(deftest single-frame-clause-uses-composition-without-split
  (let [inv {:id :composition/single-frame :readers #{:request/version}
             :expression [:>= [:read :request/version] [:literal 0]]}
        r (compose/derive-composed-lemma! transition inv)]
    (supported! r [:frame])
    (is (= :single-clause (:composition-shape r)))))

(deftest single-literal-clause-uses-composition-without-split
  (let [inv literal-fixture/status-invariant
        r (compose/derive-composed-lemma! transition inv)]
    (supported! r [:literal])
    (is (= :single-clause (:composition-shape r)))))

(deftest single-copy-clause-uses-composition-without-split
  (let [r (compose/derive-composed-lemma! copy-transition copy-fixture/both)]
    (supported! r [:copy])
    (is (= :single-clause (:composition-shape r)))))

(deftest true-constant-falls-to-frame
  (let [inv {:id :composition/true :readers #{}
             :expression [:literal true]}
        r (compose/derive-composed-lemma! transition inv)]
    (supported! r [:frame])))

(deftest false-constant-is-a-conditional-frame-invariant
  ;; A conditionally preserved FALSE predicate is harmless; its initial-state
  ;; premise is unsatisfiable. This does not prove initialization or reachability.
  (let [inv {:id :composition/false :readers #{}
             :expression [:literal false]}
        r (compose/derive-composed-lemma! transition inv)
        w (compose/audit-composed-witness! transition inv before inputs after)]
    (supported! r [:frame])
    (is (false? (:before-invariant w)))
    (is (false? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))
    (no-authority! w)))

(deftest false-fixed-postcondition-stays-open
  (let [inv {:id :composition/false-status :readers #{:request/status}
             :expression [:= [:read :request/status] [:literal :open]]}
        r (compose/derive-composed-lemma! transition inv)]
    (open! r #{0})
    (is (= :open-no-supported-family (get-in r [:clauses 0 :status])))))

(deftest equal-observations-from-distinct-copy-origins-not-enough
  (let [decl (-> copy-transition
                 (assoc :input-types {:input/new {:type :keyword}}
                        :reads #{:doc/status :doc/source :input/new}
                        :assignments {:doc/a [:read :doc/source]
                                      :doc/b [:read :input/new]}))
        r (compose/derive-composed-lemma! decl copy-and-frame)
        w (compose/audit-composed-witness!
           decl copy-and-frame copy-before {:input/new :new} copy-after)]
    (open! r #{0})
    (is (= #{1} (:conditionally-supported-clause-indices r)))
    (is (true? (:matches-transition? w)))
    (is (true? (:after-invariant w)))
    (is (= :open-unproved-clause (:status w)))))

(deftest simultaneous-copies-come-from-prestate
  (let [decl (assoc copy-transition
               :reads #{:doc/status :doc/source :doc/a}
               :writes #{:doc/a :doc/b :doc/source}
               :assignments {:doc/a [:read :doc/source]
                             :doc/b [:read :doc/source]
                             :doc/source [:read :doc/a]})
        post (assoc copy-before :doc/a :new :doc/b :new :doc/source :old-a)
        l (compose/derive-composed-lemma! decl copy-and-frame)
        w (compose/audit-composed-witness!
           decl copy-and-frame copy-before {} post)]
    (supported! l [:copy :frame])
    (is (true? (:matches-transition? w)))
    (is (true? (:after-invariant w)))))

(deftest optional-copy-targets-guarded-inside-one-clause
  (let [decl (-> copy-transition
                 (assoc-in [:state-types :doc/a :optional?] true)
                 (assoc-in [:state-types :doc/b :optional?] true))
        inv {:id :composition/guarded-copy
             :readers #{:doc/a :doc/b}
             :expression [:and [:present [:read :doc/a]]
                          [:present [:read :doc/b]]
                          [:= [:read :doc/a] [:read :doc/b]]]}
        result (compose/derive-composed-lemma! decl inv)]
    ;; These are three independent immediate clauses, so a copy-family
    ;; guard *cannot* be borrowed from a preceding clause.
    (is (= :top-level-conjunction (:composition-shape result)))
    (is (= 3 (count (:clauses result))))
    (is (seq (:open-clause-indices result)))
    (open! result (:open-clause-indices result))))

(deftest nested-presence-guards-are-not-split
  (let [decl (-> copy-transition
                 (assoc-in [:state-types :doc/a :optional?] true)
                 (assoc-in [:state-types :doc/b :optional?] true))
        inv {:id :composition/nested-guarded-copy
             :readers #{:doc/a :doc/b :doc/status}
             :expression [:and
                          [:and [:present [:read :doc/a]]
                           [:present [:read :doc/b]]
                           [:= [:read :doc/a] [:read :doc/b]]]
                          [:= [:read :doc/status] [:literal :open]]]}]
    (let [r (compose/derive-composed-lemma! decl inv)]
      (supported! r [:copy :frame])
      (is (= 2 (count (:clauses r)))))))

(deftest optional-frame-presence-stays-exact
  (let [inv {:id :composition/note-absent
             :readers #{:request/note :request/version}
             :expression [:and
                          [:not [:present [:read :request/note]]]
                          [:>= [:read :request/version] [:literal 0]]]}
        r (compose/derive-composed-lemma! transition inv)]
    ;; Note is WRITTEN by this transition, so its pre-state absence does not
    ;; establish that it remains absent, even though version is framed.
    (open! r #{0})
    (is (= :frame (get-in r [:clauses 1 :supporting-family])))))

(deftest nested-unsupported-clause-is-not-rewritten-into-success
  (let [inv {:id :composition/nested-unsupported
             :readers #{:request/status :request/version :request/owner}
             :expression [:and [:and [:= [:read :request/status] [:literal :claimed]]
                                     [:not= [:read :request/owner] [:literal nil]]]
                              [:>= [:read :request/version] [:literal 0]]]}
        r (compose/derive-composed-lemma! transition inv)]
    (open! r #{0})
    (is (= 2 (count (:clauses r))))
    (is (= :frame (get-in r [:clauses 1 :supporting-family])))))

(deftest false-prestate-does-not-assert-initialization
  (let [b (assoc before :request/version -3)
        a (assoc after :request/version -3)
        w (compose/audit-composed-witness! transition frame-and-literal b inputs a)]
    (is (= :checked-conditional-composition-instance (:status w)))
    (is (false? (:before-invariant w)))
    (is (false? (:after-invariant w)))
    (is (true? (:preservation-implication-holds? w)))
    (no-authority! w)))

(deftest wrong-guard-is-nonmatching
  (let [b (assoc before :request/status :claimed)
        w (compose/audit-composed-witness! transition frame-and-literal b inputs after)]
    (is (= :nonmatching-transition (:status w)))
    (is (false? (:matches-transition? w)))
    (no-authority! w)))

(deftest wrong-literal-assignment-is-nonmatching
  (let [a (assoc after :request/status :open)
        w (compose/audit-composed-witness! transition frame-and-literal before inputs a)]
    (is (= :nonmatching-transition (:status w)))
    (is (false? (:matches-transition? w)))
    (no-authority! w)))

(deftest frame-value-mutation-is-nonmatching
  (let [a (assoc after :request/version -2)
        w (compose/audit-composed-witness! transition frame-and-literal before inputs a)]
    (is (= :nonmatching-transition (:status w)))
    (is (false? (:after-invariant w)))
    (is (false? (:matches-transition? w)))))

(deftest framed-optional-presence-mutation-is-nonmatching
  (let [a (assoc after :request/unused :surprise)]
    (is (reject? #(compose/audit-composed-witness!
                   transition frame-and-literal before inputs a)))
    (let [decl (-> transition
                   (update :writes disj :request/note)
                   (update :assignments dissoc :request/note))
          result (compose/audit-composed-witness!
                  decl frame-and-literal before inputs (assoc after :request/note "saved"))]
      (is (= :nonmatching-transition (:status result)))
      (is (false? (:matches-transition? result))))))

(deftest required-field-absence-is-invalid-not-merely-nonmatching
  (is (reject? #(compose/audit-composed-witness!
                 transition frame-and-literal before inputs
                 (dissoc after :request/version)))))

(deftest invalid-input-value-is-rejected
  (is (reject? #(compose/audit-composed-witness!
                 transition frame-and-literal before
                 {:input/actor 42} after))))

(deftest forged-undeclared-invariant-reader-is-rejected
  (doseq [readers [#{} #{:request/version :request/status :request/owner}]]
    (is (reject? #(compose/derive-composed-lemma!
                   transition (assoc frame-and-literal :readers readers))))))

(deftest extra-invariant-key-or-bad-identifier-is-rejected
  (doseq [inv [(assoc frame-and-literal :extra/authority :proved)
               (assoc frame-and-literal :id :unqualified)
               (with-meta frame-and-literal {:forged true})
               (assoc frame-and-literal :readers [:request/version :request/status])]]
    (is (reject? #(compose/derive-composed-lemma! transition inv)))))

(deftest invalid-expression-and-outside-state-are-rejected
  (doseq [inv [(assoc frame-and-literal :expression [:call :arbitrary/x])
               {:id :composition/bad :readers #{:unknown/key}
                :expression [:present [:read :unknown/key]]}
               (assoc frame-and-literal :expression
                      [:= [:read :request/status] [:literal 17]])]]
    (is (reject? #(compose/derive-composed-lemma! transition inv)))))

(deftest source-change-updates-transition-fingerprint
  (let [r1 (compose/derive-composed-lemma! transition frame-and-literal)
        changed (assoc-in transition [:assignments :request/note]
                          [:literal "different"])
        r2 (compose/derive-composed-lemma! changed frame-and-literal)]
    (is (not= (:transition-fingerprint r1) (:transition-fingerprint r2)))
    (is (= (:source-declaration r1) transition))
    (is (= (:source-declaration r2) changed))
    (is (= (:source-invariant r1) frame-and-literal))
    (no-authority! r2)))

(deftest invariant-id-not-a-typed-fingerprint
  (let [r1 (compose/derive-composed-lemma! transition frame-and-literal)
        inv2 (assoc frame-and-literal :id :composition/other)
        r2 (compose/derive-composed-lemma! transition inv2)]
    (is (= (:typed-invariant-fingerprint r1)
           (:typed-invariant-fingerprint r2)))
    (is (not= (:invariant-id r1) (:invariant-id r2)))
    (is (= inv2 (:source-invariant r2)))))

(deftest clause-reordering-preserves-source-ledger-order
  (let [inv (update frame-and-literal :expression
                    (fn [[op a b]] [op b a]))
        r (compose/derive-composed-lemma! transition inv)]
    (supported! r [:literal :frame])
    (is (= (subvec (:expression inv) 1)
           (mapv :expression (:clauses r))))))

(deftest edn-roundtrip-does-not-grant-authority
  (let [lemma (compose/derive-composed-lemma! all-transition all-invariant)
        witness (compose/audit-composed-witness!
                 all-transition all-invariant all-before inputs all-after)]
    (doseq [report [lemma witness]]
      (let [copy (edn/read-string (pr-str report))]
        (is (= report copy))
        (no-authority! copy)))))

(deftest public-frame-family-forgery-is-refused
  (let [genuine frame/derive-frame-lemma!]
    (with-redefs [frame/derive-frame-lemma!
                  (fn [decl inv] (assoc (genuine decl inv) :authority :all))]
      (let [error (failure-data #(compose/derive-composed-lemma!
                                  transition frame-and-literal))]
        (is (= :substituted-family-report (:reason error)))))))

(deftest public-literal-family-forgery-is-refused
  (let [genuine literal/derive-literal-lemma!]
    (with-redefs [literal/derive-literal-lemma!
                  (fn [decl inv] (assoc (genuine decl inv) :certificate :issued))]
      (let [error (failure-data #(compose/derive-composed-lemma!
                                  transition frame-and-literal))]
        (is (= :substituted-family-report (:reason error)))))))

(deftest public-copy-family-forgery-is-refused
  (let [genuine copy/derive-copy-lemma!]
    (with-redefs [copy/derive-copy-lemma!
                  (fn [decl inv] (assoc (genuine decl inv) :authority :all))]
      (let [error (failure-data #(compose/derive-composed-lemma!
                                  copy-transition copy-and-frame))]
        (is (= :substituted-family-report (:reason error)))))))

(deftest public-independent-observation-forgery-is-refused
  (let [genuine independent/observe-transition!]
    (with-redefs [independent/observe-transition!
                  (fn [decl b in a] (assoc (genuine decl b in a)
                                           :authority :all))]
      (let [error (failure-data
                   #(compose/audit-composed-witness!
                     transition frame-and-literal before inputs after))]
        (is (= :substituted-transition-observation (:reason error)))))))

(deftest unexpected-family-exception-propagates
  (with-redefs [frame/derive-frame-lemma!
                (fn [& _] (throw (ex-info "unexpected family failure"
                                        {:unexpected :frame}))) ]
    (is (= :frame (:unexpected
                    (failure-data #(compose/derive-composed-lemma!
                                    transition frame-and-literal)))))))

(deftest finite-mutation-grid-respects-conditional-lemma
  ;; Covers 4 pre-state versions x 3 note variants x 2 actors; all are valid
  ;; well-typed maps. We derive a poststate independently from the transition
  ;; and compare both abstract interpreters and the composed witness.
  (let [lemma (compose/derive-composed-lemma! all-transition all-invariant)]
    (supported! lemma [:frame :literal :copy])
    (doseq [version [-2 0 4 17]
            note [::absent nil "remember"]
            actor [:user/a :user/b]]
      (let [pre (cond-> (assoc all-before :request/version version)
                  (not= ::absent note) (assoc :request/note note))
            input {:input/actor actor}
            post (-> pre
                     (assoc :request/status :claimed
                            :request/note "saved"
                            :request/owner actor
                            :request/copy1 actor
                            :request/copy2 actor))
            actual (compose/audit-composed-witness!
                    all-transition all-invariant pre input post)
            prod (production/observe-transition!
                  all-transition pre input post)
            independent-result (independent/observe-transition!
                                all-transition pre input post)
            truth (:value (presence/evaluate-expression!
                           (:expression all-invariant)
                           (select-keys (:state-types all-transition)
                                        (:readers all-invariant))
                           (select-keys post (:readers all-invariant))))]
        (is (= (:matches? prod) (:matches? independent-result)))
        (is (true? (:matches? prod)))
        (is (true? (:matches-transition? actual)))
        (is (= truth (:after-invariant actual)))
        (is (= (>= version 0) truth))
        (is (= :checked-conditional-composition-instance (:status actual)))))))
