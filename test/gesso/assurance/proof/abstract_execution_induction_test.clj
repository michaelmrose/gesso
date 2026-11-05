(ns gesso.assurance.proof.abstract-execution-induction-test
  "Permanent v1363 adversarial acceptance for the independent v1362 S1/S4
   abstract-execution induction schema and finite-history witness. Finite
   enumerations, conditional logical premises, actual initializer/operation
   completeness, and physical execution authority remain distinct.
   Deliberately checks negative construction and forgery paths as well as
   valid histories; none of these reports grants certification."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.abstract-execution-induction :as a]
            [gesso.assurance.proof.preservation-composition-soundness :as compose]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.literal-assignment-soundness-test :as fixture]
            [gesso.assurance.proof.preservation-composition-soundness-test :as comp-fixture]))

(def operation (assoc-in fixture/transition [:guard :expression]
                         [:= [:read :request/status] [:literal :claimed]]))
(def invariant comp-fixture/frame-and-literal)
(def state fixture/after)
(defn- operation-entry [id]
  {:id id :declaration (-> operation
                           (assoc-in [:subject :id] id)
                           (assoc-in [:subject :interface :operation] id))})
(def base
  {:id :request/finite-machine
   :state-types (:state-types fixture/transition)
   :invariant invariant
   :declared-initial-states [state]
   :declared-operation-ids #{:request/claim :request/reclaim}
   :operations [(operation-entry :request/reclaim)
                (operation-entry :request/claim)]})
(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- no-authority! [r]
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r))))

(deftest all-cases-covered-conditional-induction
  (let [r (a/derive-induction-schema! base)]
    (is (= a/schema-type (:type r)))
    (is (= :conditional-declared-induction-schema (:status r)))
    (is (= :every-finite-matching-declared-history-preserves-invariant
           (:conditional-claim r)))
    (is (empty? (:failing-initial-indices r)))
    (is (empty? (:unsupported-operation-ids r)))
    (is (= [:request/claim :request/reclaim]
           (mapv :operation-id (:operation-cases r))))
    (is (= [:conditional-composed-rule-instance
            :conditional-composed-rule-instance]
           (mapv :status (:operation-cases r))))
    (is (= [[:frame :literal] [:frame :literal]]
           (mapv (fn [entry]
                   (let [source (get (into {} (map (juxt :id :declaration)
                                                   (:operations base)))
                                     (:operation-id entry))]
                     (mapv :supporting-family
                           (:clauses (compose/derive-composed-lemma!
                                      source invariant)))))
                 (:operation-cases r))))
    (is (= :not-established (:initializer-coverage r)))
    (is (= :conditional-schema-only (:proof-status r)))
    (no-authority! r)
    (is (= r (edn/read-string (pr-str r))))))

(deftest valid-empty-and-multiple-step-histories
  (let [empty-h (a/audit-finite-history! base state [])
        next-state (assoc state :request/owner :user/b)
        steps [{:operation-id :request/claim
                :inputs {:input/actor :user/b} :after next-state}
               {:operation-id :request/reclaim
                :inputs {:input/actor :user/a} :after state}]
        trace (a/audit-finite-history! base state steps)]
    (is (= :checked-finite-declared-history (:status empty-h)))
    (is (zero? (:steps-inspected empty-h)))
    (is (= 2 (:steps-matching trace)))
    (is (= 2 (:steps-inspected trace)))
    (is (= state (:last-matched-state trace)))
    (is (true? (:all-matched-states-satisfy-invariant? trace)))
    (is (every? #(= :matching-abstract-step (:status %))
                (:step-observations trace)))
    (is (= :finite-witness-not-universal-proof (:proof-status trace)))
    (no-authority! trace)))

(deftest partial-guard-and-frame-mismatch-dont-count
  (let [wrong-post (assoc state :request/status :open)
        trace (a/audit-finite-history! base state
                                       [{:operation-id :request/claim
                                         :inputs {:input/actor :user/a}
                                         :after wrong-post}
                                        {:operation-id :request/reclaim
                                         :inputs {:input/actor :user/a}
                                         :after state}])]
    (is (= :nonmatching-abstract-transition (:status trace)))
    (is (= 1 (:steps-inspected trace)))
    (is (= 0 (:steps-matching trace)))
    (is (= state (:last-matched-state trace)))
    (no-authority! trace)))

(deftest rejected-unknown-operation-does-not-skip
  (let [t (a/audit-finite-history! base state
                                   [{:operation-id :request/unknown
                                     :inputs {} :after state}])]
    (is (= :undeclared-operation (:status t)))
    (is (= 0 (:steps-matching t)))
    (is (= 1 (:steps-inspected t)))
    (no-authority! t)))

(deftest non-enumerated-initial-is-not-inductive-base
  (let [s (assoc state :request/version 16)
        trace (a/audit-finite-history! base s [])]
    (is (= :outside-declared-initial-set (:status trace)))
    (is (false? (:initial-in-declared-set? trace)))
    (is (true? (:initial-invariant trace)))
    (no-authority! trace)))

(deftest failed-enumerated-base-keeps-induction-open
  (let [catalog (assoc base :declared-initial-states [state fixture/before])
        r (a/derive-induction-schema! catalog)]
    (is (= :open-base-or-step (:status r)))
    (is (= #{1} (:failing-initial-indices r)))
    (is (empty? (:unsupported-operation-ids r)))
    (is (nil? (:conditional-claim r)))
    (is (= :finite-history-invariant-counterexample
           (:status (a/audit-finite-history! catalog fixture/before []))))))

(deftest one-open-step-keeps-induction-open
  (let [bad-decl (assoc (get-in base [:operations 1 :declaration])
                        :writes #{:request/status :request/note :request/owner :request/version}
                        :assignments {:request/status [:literal :claimed]
                                      :request/note [:literal "saved"]
                                      :request/owner [:read :input/actor]
                                      :request/version [:read :request/version]}
                        :reads #{:request/status :request/version :input/actor})
        c (assoc-in base [:operations 1 :declaration] bad-decl)
        r (a/derive-induction-schema! c)]
    (is (= :open-base-or-step (:status r)))
    (is (= #{:request/claim} (:unsupported-operation-ids r)))
    (is (nil? (:conditional-claim r)))
    (no-authority! r)))

(deftest exact-catalog-membership-validation
  (let [ops (:operations base)]
    (doseq [c [(assoc base :declared-operation-ids #{:request/claim})
               (assoc base :declared-operation-ids #{:request/claim :request/reclaim :request/x})
               (assoc base :operations [(first ops) (first ops)])
               (assoc-in base [:operations 0 :id] :request/wrong)
               (assoc-in base [:operations 0 :declaration :state-types]
                         {:request/status {:type :keyword}})
               (assoc base :declared-initial-states [])
               (assoc base :declared-initial-states [state state])
               (assoc base :id :unqualified)
               (assoc base :extra true)
               (with-meta base {:forged true})]]
      (is (some? (failure #(a/derive-induction-schema! c)))
          (pr-str (select-keys c [:id :declared-operation-ids]))))))

(deftest invalid-initial-and-trace-fields-reject
  (doseq [s [(dissoc state :request/status)
             (assoc state :request/version -1.0)
             (assoc state :request/extra :extra)]]
    (is (some? (failure #(a/audit-finite-history! base s [])))))
  (doseq [step [{:operation-id :request/claim :inputs {} :after state :extra true}
                {:operation-id :request/claim :inputs {} :after state}
                {:operation-id :request/claim :inputs {:input/actor :user/a}
                 :after (dissoc state :request/version)}]]
    (is (some? (failure #(a/audit-finite-history! base state [step]))))))

(deftest substituted-lemma-or-observer-rejected
  (with-redefs [compose/derive-composed-lemma!
                (fn [& _] {:status :conditional-composed-rule-instance})]
    (is (= :substituted-composition-lemma
           (:reason (failure #(a/derive-induction-schema! base))))))
  (with-redefs [independent/observe-transition!
                (fn [& _] {:matches? true})]
    (is (= :substituted-transition-observer
           (:reason (failure #(a/audit-finite-history! base state
                                    [{:operation-id :request/claim
                                      :inputs {:input/actor :user/a}
                                      :after state}]))))))
  (with-redefs [presence/evaluate-expression!
                (fn [& _] {:value true})]
    (is (= :substituted-typed-interpreter
           (:reason (failure #(a/derive-induction-schema! base)))))))

(deftest source-change-rederives-evidence
  (let [changed (assoc-in base [:operations 0 :declaration :assignments :request/note]
                          [:literal "changed"])
        r1 (a/derive-induction-schema! base)
        r2 (a/derive-induction-schema! changed)]
    (is (not= (mapv :transition-fingerprint (:operation-cases r1))
              (mapv :transition-fingerprint (:operation-cases r2))))
    (is (not= (:source-catalog r1) (:source-catalog r2)))
    (is (= :conditional-declared-induction-schema (:status r2)))))

(deftest optional-field-presence-is-part-of-inductive-base
  (let [inv {:id :request/note-present
             :readers #{:request/note}
             :expression [:present [:read :request/note]]}
        c (assoc base :invariant inv)
        supported (a/derive-induction-schema! c)
        absent (assoc c :declared-initial-states [(dissoc state :request/note)])
        unsupported (a/derive-induction-schema! absent)]
    (is (= :conditional-declared-induction-schema (:status supported)))
    (is (= :open-base-or-step (:status unsupported)))
    (is (= #{0} (:failing-initial-indices unsupported)))
    (is (empty? (:unsupported-operation-ids unsupported)))
    (no-authority! unsupported)))

(deftest false-guard-remains-nonmatching-not-an-accepted-step
  (let [decl (-> (get-in base [:operations 1 :declaration])
                 (assoc-in [:guard :expression]
                           [:= [:read :request/status] [:literal :open]]))
        c (assoc-in base [:operations 1 :declaration] decl)
        t (a/audit-finite-history! c state
                                   [{:operation-id :request/claim
                                     :inputs {:input/actor :user/a}
                                     :after state}])]
    (is (= :nonmatching-abstract-transition (:status t)))
    (is (false? (get-in t [:step-observations 0 :guard-holds?])))
    (is (= 0 (:steps-matching t)))
    (no-authority! t)))

(deftest no-finite-enumeration-claim-beyond-bounds
  (let [states (mapv #(assoc state :request/version %)
                     (range 33))]
    (is (= :invalid-bounded-vector
           (:reason (failure #(a/derive-induction-schema!
                               (assoc base :declared-initial-states states))))))
    (is (= :invalid-bounded-vector
           (:reason (failure #(a/audit-finite-history!
                               base state (vec (repeat 129
                                                {:operation-id :request/claim
                                                 :inputs {:input/actor :user/a}
                                                 :after state})))))))))

(deftest open-lemma-does-not-become-proof-on-matching-sample
  (let [operation-map (assoc (get-in base [:operations 1 :declaration])
                             :writes #{:request/status :request/note :request/owner :request/version}
                             :assignments {:request/status [:literal :claimed]
                                           :request/note [:literal "saved"]
                                           :request/owner [:read :input/actor]
                                           :request/version [:read :request/version]}
                             :reads #{:request/status :request/version :input/actor})
        c (assoc-in base [:operations 1 :declaration] operation-map)
        t (a/audit-finite-history! c state
                                   [{:operation-id :request/claim
                                     :inputs {:input/actor :user/a}
                                     :after state}])]
    (is (= :open-base-or-step (:schema-status t)))
    (is (= :checked-finite-declared-history (:status t)))
    (is (= 1 (:steps-matching t)))
    (is (true? (:all-matched-states-satisfy-invariant? t)))
    (is (= :finite-witness-not-universal-proof (:proof-status t)))
    (no-authority! t)))

(deftest malformed-history-operation-id-rejected
  (is (= :invalid-step-operation-id
         (:reason (failure #(a/audit-finite-history!
                             base state [{:operation-id "request/claim"
                                          :inputs {} :after state}]))))))

;; Additional permanent coverage beyond the source revision's smoke suite.
;; A traced outcome is not a proof: assert exact conditional and non-authority
;; fields on the schema AND witness, including EDN round trips.

(defn- claim-step
  [actor]
  {:operation-id :request/claim
   :inputs {:input/actor actor}
   :after (assoc state :request/owner actor)})

(defn- reclaim-step
  [actor]
  {:operation-id :request/reclaim
   :inputs {:input/actor actor}
   :after (assoc state :request/owner actor)})

(defn- fail-reason [thunk]
  (:reason (failure thunk)))

(defn- assert-schema-boundary! [report]
  (is (= a/schema-type (:type report)))
  (is (= a/semantics-version (:version report)))
  (is (= :finite-enumerated-initial-states-and-closed-declared-operations
         (:scope report)))
  (is (= :conditional-schema-only (:proof-status report)))
  (is (= :not-established (:initializer-coverage report)))
  (is (= :not-established (:physical-operation-completeness report)))
  (is (= :not-established (:universal-implementation-soundness report)))
  (is (= :not-established (:xtdb-correspondence report)))
  (is (= :not-established (:runtime-enforcement report)))
  (is (contains? (:induction-premises report)
                 :sound-independent-abstract-transition-semantics))
  (is (contains? (:open-external-obligations report)
                 :actual-operation-set-completeness))
  (is (contains? (:open-external-obligations report)
                 :actual-initializer-set-coverage))
  (is (contains? (:open-external-obligations report)
                 :xtdb-execution-correspondence))
  (no-authority! report))

(defn- assert-history-boundary! [report]
  (is (= a/history-type (:type report)))
  (is (= a/semantics-version (:version report)))
  (is (= :independent-typed-abstract-history (:checking report)))
  (is (= :finite-witness-not-universal-proof (:proof-status report)))
  (is (= :not-established (:physical-observation report)))
  (no-authority! report))

(deftest accepted-schema-retains-exact-source-and-open-premises
  (let [r (a/derive-induction-schema! base)]
    (is (= base (:source-catalog r)))
    (is (= (:id base) (:catalog-id r)))
    (is (= (:id invariant) (:invariant-id r)))
    (is (= (:declared-operation-ids base) (:declared-operation-ids r)))
    (is (= :conditional-declared-induction-schema (:status r)))
    (is (= :every-finite-matching-declared-history-preserves-invariant
           (:conditional-claim r)))
    (assert-schema-boundary! r)
    (is (= r (edn/read-string (pr-str r))))))

(deftest rejected-initial-base-retains-conditional-boundaries
  (let [r (a/derive-induction-schema!
           (assoc base :declared-initial-states [fixture/before state]))]
    (is (= :open-base-or-step (:status r)))
    (is (= #{0} (:failing-initial-indices r)))
    (is (nil? (:conditional-claim r)))
    (assert-schema-boundary! r)))

(deftest rejected-operation-step-retains-conditional-boundaries
  (let [decl (-> (get-in base [:operations 0 :declaration])
                 (update :writes conj :request/version)
                 (assoc-in [:assignments :request/version]
                           [:read :request/version])
                 (update :reads conj :request/version))
        r (a/derive-induction-schema!
           (assoc-in base [:operations 0 :declaration] decl))]
    (is (= :open-base-or-step (:status r)))
    (is (= #{:request/reclaim} (:unsupported-operation-ids r)))
    (is (nil? (:conditional-claim r)))
    (assert-schema-boundary! r)))

(deftest source-order-does-not-alter-operation-ledger
  (let [reordered (update base :operations #(vec (reverse %)))
        r1 (a/derive-induction-schema! base)
        r2 (a/derive-induction-schema! reordered)]
    (is (= [:request/claim :request/reclaim]
           (mapv :operation-id (:operation-cases r1))))
    (is (= (:operation-cases r1) (:operation-cases r2)))
    (is (= (:initial-cases r1) (:initial-cases r2)))
    (is (not= (:source-catalog r1) (:source-catalog r2)))
    (is (= reordered (:source-catalog r2)))))

(deftest multiple-enumerated-valid-bases-each-checked
  (let [s2 (assoc state :request/version 0)
        s3 (assoc state :request/version 105)
        c (assoc base :declared-initial-states [state s2 s3])
        r (a/derive-induction-schema! c)]
    (is (= :conditional-declared-induction-schema (:status r)))
    (is (= (mapv vector (range 3) [state s2 s3])
           (mapv (juxt :index :state) (:initial-cases r))))
    (is (every? :invariant-holds? (:initial-cases r)))
    (is (empty? (:failing-initial-indices r)))
    (no-authority! r)))

(deftest multiple-failed-bases-preserve-all-failing-indices
  (let [s1 (assoc fixture/before :request/version 10)
        s2 (assoc fixture/before :request/version 0)
        c (assoc base :declared-initial-states [s1 state s2])
        r (a/derive-induction-schema! c)]
    (is (= :open-base-or-step (:status r)))
    (is (= #{0 2} (:failing-initial-indices r)))
    (is (= [false true false]
           (mapv :invariant-holds? (:initial-cases r))))
    (is (nil? (:conditional-claim r)))))

(deftest declared-operations-must-not-drop-or-duplicate-identities
  (let [ops (:operations base)]
    (doseq [variant [(assoc base :operations [(first ops)])
                     (assoc base :operations [(first ops) (first ops)])
                     (assoc base :declared-operation-ids #{:request/claim})
                     (assoc base :declared-operation-ids #{:request/claim
                                                          :request/reclaim :request/third})
                     (assoc-in base [:operations 0 :id] :request/claim)]]
      (is (some? (failure #(a/derive-induction-schema! variant)))))))

(deftest catalog-keyset-is-strict-and-not-extensible-by-user
  (doseq [alter [(dissoc base :invariant)
                 (dissoc base :state-types)
                 (dissoc base :declared-operation-ids)
                 (assoc base :cached-proof {:proved true})
                 (assoc base :authority :admitted)
                 (assoc base :certificate :issued)]]
    (is (= :wrong-keys (fail-reason #(a/derive-induction-schema! alter)))))
  (is (= :expected-plain-map
         (fail-reason #(a/derive-induction-schema! [:not-a-catalog])))))

(deftest operation-entry-keyset-is-strict
  (doseq [entry [(assoc (first (:operations base)) :proof-status :proved)
                 (assoc (first (:operations base)) :certificate :issued)
                 (dissoc (first (:operations base)) :declaration)
                 (dissoc (first (:operations base)) :id)]]
    (is (= :wrong-keys
           (fail-reason #(a/derive-induction-schema!
                          (assoc-in base [:operations 0] entry)))))))

(deftest operation-id-must-equal-subject-and-shared-state-projection
  (doseq [entry [(assoc-in (first (:operations base)) [:declaration :subject :id]
                          :request/other)
                 (assoc-in (first (:operations base)) [:declaration :state-types
                                                       :request/version]
                           {:type :string})]]
    (is (= :operation-source-mismatch
           (fail-reason #(a/derive-induction-schema!
                          (assoc-in base [:operations 0] entry)))))))

(deftest exact-initial-state-duplicates-rejected
  (let [c (assoc base :declared-initial-states [state state])]
    (is (= :duplicate-declared-initial-state
           (fail-reason #(a/derive-induction-schema! c))))))

(deftest syntactically-illegal-catalog-shapes-rejected
  (doseq [c [(assoc base :declared-initial-states '())
             (assoc base :operations (list (first (:operations base))))
             (assoc base :declared-operation-ids [:request/claim :request/reclaim])
             (assoc base :declared-operation-ids #{:unqualified})
             (assoc base :id "request/finite-machine")
             (with-meta base {:certificate :issued})]]
    (is (some? (failure #(a/derive-induction-schema! c))))))

(deftest metadata-on-operation-and-initial-vector-rejected
  (let [op (with-meta (first (:operations base)) {:forged true})
        initial (with-meta (:declared-initial-states base) {:forged true})
        operations (with-meta (:operations base) {:forged true})]
    (doseq [c [(assoc-in base [:operations 0] op)
               (assoc base :declared-initial-states initial)
               (assoc base :operations operations)]]
      (is (= :metadata-not-allowed
             (fail-reason #(a/derive-induction-schema! c)))))))

(deftest bounded-initial-enumeration-at-limit
  (let [all-states (mapv #(assoc state :request/version (inc %)) (range 32))
        r (a/derive-induction-schema!
           (assoc base :declared-initial-states all-states))]
    (is (= 32 (count (:initial-cases r))))
    (is (every? :invariant-holds? (:initial-cases r)))
    (is (= :conditional-declared-induction-schema (:status r)))
    (is (= :invalid-bounded-vector
           (fail-reason #(a/derive-induction-schema!
                          (assoc base :declared-initial-states
                                 (conj all-states (assoc state :request/version 100)))))))))

(deftest zero-initial-state-is-not-proof-of-initial-coverage
  (is (= :invalid-bounded-vector
         (fail-reason #(a/derive-induction-schema!
                        (assoc base :declared-initial-states [])))))
  (is (= :not-established (:initializer-coverage
                            (a/derive-induction-schema! base)))))

(deftest zero-operations-and-absent-declared-set-are-invalid
  (doseq [c [(assoc base :operations [])
             (assoc base :declared-operation-ids #{})
             (assoc base :operations [])]]
    (is (some? (failure #(a/derive-induction-schema! c))))))

(deftest history-zero-step-witness-is-not-universal-proof
  (let [r (a/audit-finite-history! base state [])]
    (is (= :checked-finite-declared-history (:status r)))
    (is (true? (:initial-in-declared-set? r)))
    (is (true? (:initial-invariant r)))
    (is (= 0 (:steps-requested r)))
    (is (= 0 (:steps-inspected r)))
    (is (= 0 (:steps-matching r)))
    (is (empty? (:step-observations r)))
    (is (= :conditional-declared-induction-schema (:schema-status r)))
    (assert-history-boundary! r)
    (is (= r (edn/read-string (pr-str r))))))

(deftest history-two-steps-preserves-original-state-sequencing
  (let [steps [(claim-step :user/b) (reclaim-step :user/a)]
        r (a/audit-finite-history! base state steps)]
    (is (= :checked-finite-declared-history (:status r)))
    (is (= 2 (:steps-requested r)))
    (is (= 2 (:steps-matching r)))
    (is (= [:request/claim :request/reclaim]
           (mapv :operation-id (:step-observations r))))
    (is (= [true true]
           (mapv :before-invariant (:step-observations r))))
    (is (= [true true]
           (mapv :after-invariant (:step-observations r))))
    (is (= state (:last-matched-state r)))
    (assert-history-boundary! r)))

(deftest valid-dynamic-actor-inputs-are-not-fixed-literal-values
  (doseq [actor [:user/a :user/b :user/c :user/d]]
    (let [r (a/audit-finite-history! base state [(claim-step actor)])]
      (is (= :checked-finite-declared-history (:status r)))
      (is (= (assoc state :request/owner actor)
             (:last-matched-state r)))
      (is (= 1 (:steps-matching r)))
      (is (true? (:all-matched-states-satisfy-invariant? r)))
      (no-authority! r))))

(deftest transition-is-checked-before-invariant-is-propagated
  (let [wrong (assoc (claim-step :user/b) :after
                     (assoc state :request/owner :user/b :request/version -1))
        ;; This after state is well-typed but violates the frame and invariant.
        r (a/audit-finite-history! base state [wrong])]
    (is (= :nonmatching-abstract-transition (:status r)))
    (is (= 0 (:steps-matching r)))
    (is (= state (:last-matched-state r)))
    (is (false? (get-in r [:step-observations 0 :frame-holds?])))
    (is (false? (get-in r [:step-observations 0 :after-invariant])))
    (is (true? (:all-matched-states-satisfy-invariant? r)))
    (assert-history-boundary! r)))

(deftest broken-second-step-retains-only-first-matched-step
  (let [first-step (claim-step :user/b)
        second-step (assoc (reclaim-step :user/a) :after
                           (assoc state :request/owner :user/z))
        third-step (claim-step :user/c)
        r (a/audit-finite-history! base state
                                   [first-step second-step third-step])]
    (is (= :nonmatching-abstract-transition (:status r)))
    (is (= 3 (:steps-requested r)))
    (is (= 2 (:steps-inspected r)))
    (is (= 1 (:steps-matching r)))
    (is (= (assoc state :request/owner :user/b)
           (:last-matched-state r)))
    (is (= [:matching-abstract-step :nonmatching-abstract-step]
           (mapv :status (:step-observations r))))
    (assert-history-boundary! r)))

(deftest unknown-middle-operation-stops-history
  (let [r (a/audit-finite-history! base state
                                   [(claim-step :user/b)
                                    {:operation-id :request/extra
                                     :inputs {} :after state}
                                    (reclaim-step :user/a)])]
    (is (= :undeclared-operation (:status r)))
    (is (= 2 (:steps-inspected r)))
    (is (= 1 (:steps-matching r)))
    (is (= (assoc state :request/owner :user/b)
           (:last-matched-state r)))
    (is (= :undeclared-operation
           (get-in r [:step-observations 1 :status])))
    (assert-history-boundary! r)))

(deftest out-of-catalog-initial-state-is-not-implicitly-admitted
  (let [unlisted (assoc state :request/version 73)
        r (a/audit-finite-history! base unlisted [(claim-step :user/b)])]
    (is (= :outside-declared-initial-set (:status r)))
    (is (true? (:initial-invariant r)))
    (is (false? (:initial-in-declared-set? r)))
    (is (= 1 (:steps-requested r)))
    (is (= 0 (:steps-inspected r)))
    (is (= unlisted (:last-matched-state r)))
    (assert-history-boundary! r)))

(deftest failed-invariant-in-enumerated-initial-state-is-counterexample
  (let [c (assoc base :declared-initial-states [state fixture/before])
        r (a/audit-finite-history! c fixture/before [])]
    (is (= :finite-history-invariant-counterexample (:status r)))
    (is (= :open-base-or-step (:schema-status r)))
    (is (true? (:initial-in-declared-set? r)))
    (is (false? (:initial-invariant r)))
    (is (false? (:all-matched-states-satisfy-invariant? r)))
    (assert-history-boundary! r)))

(deftest false-guard-cannot-count-as-a-matching-transition
  (let [decl (assoc-in (get-in base [:operations 1 :declaration])
                       [:guard :expression]
                       [:= [:read :request/status] [:literal :open]])
        c (assoc-in base [:operations 1 :declaration] decl)
        r (a/audit-finite-history! c state [(claim-step :user/a)])]
    (is (= :nonmatching-abstract-transition (:status r)))
    (is (false? (get-in r [:step-observations 0 :guard-holds?])))
    (is (= 0 (:steps-matching r)))
    (assert-history-boundary! r)))

(deftest wrong-literal-and-frame-after-values-not-accepted
  (doseq [wrong [(assoc state :request/status :open)
                 (assoc state :request/note "wrong")
                 (assoc state :request/version 11)
                 (assoc state :request/owner :user/z)]]
    (let [r (a/audit-finite-history! base state
                                     [(assoc (claim-step :user/b)
                                             :after wrong)])]
      (is (= :nonmatching-abstract-transition (:status r)))
      (is (= 0 (:steps-matching r)))
      (is (= state (:last-matched-state r)))
      (assert-history-boundary! r))))

(deftest optional-field-presence-checked-in-inductive-histories
  (let [without-note (dissoc state :request/note)
        r (a/audit-finite-history! base state
                                   [(assoc (claim-step :user/a)
                                           :after without-note)])]
    (is (= :nonmatching-abstract-transition (:status r)))
    (is (= 0 (:steps-matching r)))
    (is (= state (:last-matched-state r)))
    (assert-history-boundary! r)))

(deftest nullable-field-presence-distinct-from-absent
  (let [present-nil (assoc state :request/note nil)
        unlisted (a/audit-finite-history! base present-nil [])
        missing (a/audit-finite-history! base (dissoc state :request/note) [])]
    (is (= :outside-declared-initial-set (:status unlisted)))
    (is (= :outside-declared-initial-set (:status missing)))
    (is (not= present-nil (dissoc state :request/note)))
    (is (false? (:initial-in-declared-set? unlisted)))
    (is (false? (:initial-in-declared-set? missing)))
    (assert-history-boundary! unlisted)))

(deftest malformed-step-shapes-rejected-before-counting
  (doseq [step [(assoc (claim-step :user/a) :certificate :issued)
                (dissoc (claim-step :user/a) :inputs)
                (dissoc (claim-step :user/a) :after)
                (dissoc (claim-step :user/a) :operation-id)]]
    (is (= :wrong-keys
           (fail-reason #(a/audit-finite-history! base state [step]))))))

(deftest malformed-step-value-and-id-rejected
  (is (= :invalid-step-operation-id
         (fail-reason #(a/audit-finite-history! base state
                        [(assoc (claim-step :user/a) :operation-id 123)]))))
  (is (= :expected-plain-map
         (fail-reason #(a/audit-finite-history! base state
                        [[:claim :user/a state]])))))

(deftest invalid-projection-and-inputs-are-rejected-not-skipped
  (doseq [step [(assoc (claim-step :user/a) :inputs {})
                (assoc (claim-step :user/a) :inputs {:input/actor 27})
                (assoc (claim-step :user/a) :inputs {:input/actor :user/a
                                                     :input/extra :x})
                (assoc (claim-step :user/a) :after
                       (dissoc state :request/status))
                (assoc (claim-step :user/a) :after
                       (assoc state :request/version 1.0))]]
    (is (some? (failure #(a/audit-finite-history! base state [step]))))))

(deftest step-and-catalog-metadata-are-not-authorization
  (let [bad-step (with-meta (claim-step :user/a) {:certificate true})]
    (is (= :metadata-not-allowed
           (fail-reason #(a/audit-finite-history! base state [bad-step]))))
    (is (= :metadata-not-allowed
           (fail-reason #(a/derive-induction-schema!
                          (with-meta base {:trusted true})))))))

(deftest bounded-history-exact-limit-matches
  (let [steps (vec (repeat 128 (claim-step :user/a)))
        r (a/audit-finite-history! base state steps)]
    (is (= :checked-finite-declared-history (:status r)))
    (is (= 128 (:steps-inspected r)))
    (is (= 128 (:steps-matching r)))
    (is (true? (:all-matched-states-satisfy-invariant? r)))
    (assert-history-boundary! r)
    (is (= :invalid-bounded-vector
           (fail-reason #(a/audit-finite-history! base state
                                                 (conj steps (claim-step :user/a))))))))

(deftest failed-base-and-open-operation-never-promote-conditionally
  (let [decl (-> (get-in base [:operations 0 :declaration])
                 (update :writes conj :request/version)
                 (assoc-in [:assignments :request/version]
                           [:read :request/version])
                 (update :reads conj :request/version))
        c (-> base
              (assoc-in [:operations 0 :declaration] decl)
              (assoc :declared-initial-states [state fixture/before]))
        r (a/derive-induction-schema! c)]
    (is (= :open-base-or-step (:status r)))
    (is (= #{1} (:failing-initial-indices r)))
    (is (= #{:request/reclaim} (:unsupported-operation-ids r)))
    (is (nil? (:conditional-claim r)))
    (assert-schema-boundary! r)))

(deftest open-schema-with-successful-witness-remains-open
  (let [decl (-> (get-in base [:operations 0 :declaration])
                 (update :writes conj :request/version)
                 (assoc-in [:assignments :request/version]
                           [:read :request/version])
                 (update :reads conj :request/version))
        c (assoc-in base [:operations 0 :declaration] decl)
        r (a/audit-finite-history! c state [(claim-step :user/b)])]
    (is (= :open-base-or-step (:schema-status r)))
    (is (= :checked-finite-declared-history (:status r)))
    (is (= 1 (:steps-matching r)))
    (is (true? (:all-matched-states-satisfy-invariant? r)))
    (assert-history-boundary! r)))

(deftest changing-source-recomputes-all-base-and-step-premises
  (let [changed (-> base
                    (assoc-in [:operations 0 :declaration :assignments
                               :request/note]
                              [:literal "another note"])
                    (assoc :declared-initial-states [state
                                                     (assoc state :request/version 23)]))
        r1 (a/derive-induction-schema! base)
        r2 (a/derive-induction-schema! changed)]
    (is (not= (:source-catalog r1) (:source-catalog r2)))
    (is (not= (:initial-cases r1) (:initial-cases r2)))
    (is (not= (:operation-cases r1) (:operation-cases r2)))
    (is (= :conditional-declared-induction-schema (:status r2)))
    (is (= changed (:source-catalog r2)))))

(deftest no-authoritative-proof-from-edn-roundtrip
  (doseq [r [(a/derive-induction-schema! base)
             (a/audit-finite-history! base state [(claim-step :user/a)])]]
    (let [roundtrip (edn/read-string (pr-str r))]
      (is (= r roundtrip))
      (is (= :not-issued (:certificate roundtrip)))
      (is (= :not-admitted (:admission roundtrip)))
      (is (= :none (:authority roundtrip)))
      (is (not= :proved (:proof-status roundtrip))))))

(deftest independent-typed-interpreter-substitution-does-not-upgrade
  (with-redefs [presence/evaluate-expression! (fn [& _] {:value true
                                                         :proof-status :proved})]
    (is (= :substituted-typed-interpreter
           (fail-reason #(a/derive-induction-schema! base))))))

(deftest independent-transition-observer-substitution-does-not-upgrade
  (with-redefs [independent/observe-transition!
                (fn [& _] {:matches? true :authority :admitted})]
    (is (= :substituted-transition-observer
           (fail-reason #(a/audit-finite-history!
                          base state [(claim-step :user/a)]))))))

(deftest composed-preservation-lemma-substitution-does-not-upgrade
  (with-redefs [compose/derive-composed-lemma!
                (fn [& _] {:status :conditional-composed-rule-instance
                           :proof-status :proved :authority :admitted})]
    (is (= :substituted-composition-lemma
           (fail-reason #(a/derive-induction-schema! base))))))

(deftest complete-declared-operation-set-at-size-limit
  (let [ids (mapv #(keyword "request" (str "operation-" %)) (range 32))
        c (assoc base :declared-operation-ids (set ids)
                      :operations (mapv operation-entry ids))
        r (a/derive-induction-schema! c)]
    (is (= 32 (count (:operation-cases r))))
    (is (= (set ids) (set (map :operation-id (:operation-cases r)))))
    (is (every? #(= :conditional-composed-rule-instance (:status %))
                (:operation-cases r)))
    (is (= :conditional-declared-induction-schema (:status r)))
    (is (= :invalid-declared-operation-set
           (fail-reason #(a/derive-induction-schema!
                          (assoc c :declared-operation-ids
                                   (conj (set ids) :request/operation-32)
                                   :operations (conj (:operations c)
                                                     (operation-entry
                                                      :request/operation-32)))))))
    (no-authority! r)))
