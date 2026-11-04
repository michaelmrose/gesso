(ns gesso.assurance.proof.elaborate-test
  "Permanent v1244 tests for the proof-producing abstract transition elaborator.

   Test both sides of the boundary: every correctly generated guard,
   assignment, frame and conjunction is accepted by the INDEPENDENT v1235
   checker; deliberately corrupted generator steps are rejected by that same
   checker. The v1243 report is copyable, unadmitted data. None of these
   tests mistakes checked conditional relation facts for an invariant proof,
   certificate, runtime authorization, or XTDB implementation refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.elaborate :as elaborate]
            [gesso.assurance.proof.obligation :as obligation]))

(def ^:private claim
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed :done}}
    :request/owner {:type :keyword :optional? true}
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

(def ^:private source
  {:construction claim :initial-condition initial :invariants invariants})

(def ^:private derivation-error
  :gesso.assurance.proof.derivation/invalid-derivation)
(def ^:private elaboration-error
  :gesso.assurance.proof.elaborate/invalid-elaboration)
(def ^:private obligation-error
  :gesso.assurance.proof.obligation/invalid-obligation-source)
(def ^:private construction-error
  :gesso.assurance.transition/invalid-construction)
(def ^:private abstract-transition-error
  :gesso.assurance.proof.transition/invalid-transition)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private original-open-derivation-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects-as!
  [f type reason]
  (let [result (failure f)]
    (is (map? result) (str "Expected " type " / " reason))
    (is (= type (:error/type result)) (pr-str result))
    (is (= reason (:reason result)) (pr-str result))
    result))

(defn- elaborate! [s] (elaborate/elaborate-transition! s))
(defn- fact-for [report kind field]
  (first (filter #(and (= kind (:kind %)) (= field (:field %)))
                 (:fact-proofs report))))

(defn- assert-conditional-check! [proof]
  (let [source (:proof-source proof)
        checked (:checked-derivation proof)]
    (is (= checked (derivation/check-transition-derivation! source)))
    (is (= (:goal source) (:goal checked)))
    (is (= #{relation-premise} (:open-premises checked)))
    (is (= original-open-derivation-obligations
           (:missing-obligations checked)))
    (is (= :matching-abstract-transition-only (:claim-scope checked)))
    (is (= :conditional-abstract-inference (:proof-status checked)))
    (is (= :not-admitted (:admission checked)))
    (is (= :none (:authority checked)))))

(defn- assert-open! [report expected-invariants]
  (let [original (obligation/derive-obligations! (:source report))
        open (vec (mapcat identity (vals (:open-obligations report))))]
    (is (= (:source original) (:source report))
        "The original declarations remain attached to the unadmitted report")
    (is (= (:open-obligation-ids original) (:open-obligation-ids report)))
    (is (= (:obligations original) (:open-obligations report)))
    (is (= (set (map (fn [id] [:initial-state id]) expected-invariants))
           (set (filter #(= :initial-state (first %)) (:open-obligation-ids report)))))
    (is (= (set (map (fn [id] [:preservation id]) expected-invariants))
           (set (filter #(= :preservation (first %)) (:open-obligation-ids report)))))
    (is (= (:open-obligation-ids report) (set (map :id open))))
    (is (every? #(= :open (:status %)) open))
    (is (= :matching-abstract-transition-only (:claim-scope report)))
    (is (= :conditional-abstract-inference-only (:proof-status report)))
    (is (= :not-established (:invariant-preservation report)))
    (is (= :not-established (:initial-state-invariant report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :valid?)))))

(defn- private-elaborator-var [symbol]
  (or (get (ns-interns 'gesso.assurance.proof.elaborate) symbol)
      (throw (ex-info "Missing expected v1243 internal seam" {:symbol symbol}))))

(defn- with-buggy-candidates
  "Mutate the real generator's proposed candidates, then invoke the entire
   public compiler. No independent checker is replaced here: only the
   intentionally faulty proof-generator proposal changes."
  [alter]
  (let [generator (private-elaborator-var 'proposed-facts)
        original @generator]
    (with-redefs-fn {generator (fn [semantics] (alter (original semantics)))}
      #(elaborate! source))))

(defn- candidate-index [candidates kind field]
  (first (keep-indexed
          (fn [i entry]
            (when (and (= kind (:kind entry)) (= field (:field entry))) i))
          candidates)))

(deftest complete-source-derived-coverage
  (let [report (elaborate! source)
        coverage (:primitive-coverage report)]
    (is (= elaborate/report-type (:type report)))
    (is (= elaborate/elaboration-version (:version report)))
    (is (= source (:source report)))
    (is (= :independently-checked-abstract-derivations (:checking report)))
    (is (= 5 (:checked-fact-count report)))
    (is (= 5 (count (:fact-proofs report))))
    (is (= 1 (:guard-count coverage)))
    (is (= #{:request/status :request/owner} (:assignment-fields coverage)))
    (is (= #{:request/note :request/version} (:frame-fields coverage)))
    (is (= (set (keys (:state-types claim))) (:state-fields coverage)))
    (is (= (:transition-fingerprint report)
           (get-in report [:fact-proofs 0 :checked-derivation
                           :transition-fingerprint])))
    (is (= 1 (count (:batch-proofs report))))
    (is (= report (elaborate! source)))
    (assert-open! report (keys invariants))))

(deftest exact-guard-assignments-and-frames
  (let [report (elaborate! source)
        proofs (:fact-proofs report)
        goals (set (map :goal proofs))]
    (is (= #{[:guard]
             [:eq [:post :request/status] [:literal :claimed]]
             [:eq [:post :request/owner] [:input :input/actor]]
             [:same-field :request/version]
             [:same-field :request/note]}
           goals))
    (is (= [:guard] (:goal (fact-for report :guard nil))))
    (is (= :assignment (:kind (fact-for report :assignment :request/owner))))
    (is (= :frame (:kind (fact-for report :frame :request/note))))
    (is (= (count proofs) (count (set (map (comp :id first :steps :proof-source) proofs)))))
    (doseq [p proofs] (assert-conditional-check! p))))

(deftest checked-conjunction-matches-exact-facts
  (let [report (elaborate! source)
        batch (first (:batch-proofs report))
        facts (:fact-proofs report)
        steps (get-in batch [:proof-source :steps])]
    (is (= (set (map (comp :id first :steps :proof-source) facts))
           (set (:fact-ids batch))))
    (is (= 6 (count steps)))
    (is (= :conjunction-introduction (:rule (peek steps))))
    (is (= (:fact-ids batch) (:from (peek steps))))
    (is (= (into [:and] (map :goal facts)) (:goal batch)))
    (assert-conditional-check! batch)))

(deftest read-only-transition-derives-all-frame-facts
  (let [s (-> source
              (assoc-in [:construction :reads] #{:request/status})
              (assoc-in [:construction :input-types] {})
              (assoc-in [:construction :writes] #{})
              (assoc-in [:construction :replacements] {})
              (assoc-in [:construction :frame] (set (keys (:state-types claim)))))
        report (elaborate! s)]
    (is (= 5 (:checked-fact-count report)))
    (is (= #{} (get-in report [:primitive-coverage :assignment-fields])))
    (is (= (set (keys (:state-types claim)))
           (get-in report [:primitive-coverage :frame-fields])))
    (is (= #{[:guard]
             [:same-field :request/status]
             [:same-field :request/owner]
             [:same-field :request/version]
             [:same-field :request/note]}
           (set (map :goal (:fact-proofs report)))))
    (doseq [proof (:fact-proofs report)] (assert-conditional-check! proof))))

(deftest optional-target-write-has-total-post-state-equality
  (let [s (-> source
              (update-in [:construction :writes] conj :request/note)
              (update-in [:construction :frame] disj :request/note)
              (assoc-in [:construction :replacements :request/note]
                        [:literal "created"]))
        report (elaborate! s)]
    (is (= [:eq [:post :request/note] [:literal "created"]]
           (:goal (fact-for report :assignment :request/note))))
    (is (= #{:request/status :request/owner :request/note}
           (get-in report [:primitive-coverage :assignment-fields])))
    (is (= #{:request/version}
           (get-in report [:primitive-coverage :frame-fields])))
    (is (= 5 (:checked-fact-count report)))
    (assert-conditional-check! (fact-for report :assignment :request/note))
    (assert-open! report (keys invariants))))

(deftest required-state-reader-produces-pre-state-equality
  (let [s (-> source
              (assoc-in [:construction :state-types :request/counter]
                        {:type :integer})
              (update-in [:construction :reads] conj :request/counter)
              (update-in [:construction :writes] conj :request/version)
              (update-in [:construction :frame] disj :request/version)
              (update-in [:construction :frame] conj :request/counter)
              (assoc-in [:construction :replacements :request/version]
                        [:read :request/counter]))
        report (elaborate! s)]
    (is (= [:eq [:post :request/version] [:pre :request/counter]]
           (:goal (fact-for report :assignment :request/version))))
    (is (= [:same-field :request/counter]
           (:goal (fact-for report :frame :request/counter))))
    (is (= 6 (:checked-fact-count report)))
    (assert-conditional-check! (fact-for report :assignment :request/version))))

(deftest stronger-or-contradictory-invariants-stay-open
  (let [s (assoc-in source [:invariants :request/nonnegative-version]
                    {:readers #{:request/version}
                     :expression [:< [:read :request/version] [:literal -10]]})
        report (elaborate! s)]
    (is (= (:transition-fingerprint (elaborate! source))
           (:transition-fingerprint report)))
    (is (not= (:fingerprint (elaborate! source)) (:fingerprint report)))
    (assert-open! report (keys invariants))))

(deftest omitted-invariant-does-not-become-a-proof-of-completeness
  (let [s (update source :invariants dissoc :request/claimed-has-owner)
        report (elaborate! s)]
    (is (contains? (:open-obligation-ids report)
                   [:assumption :declared-invariant-set-coverage]))
    (is (not (contains? (:open-obligation-ids report)
                        [:preservation :request/claimed-has-owner])))
    (is (not= (:fingerprint report) (:fingerprint (elaborate! source))))
    (assert-open! report (keys (:invariants s)))))

(deftest changing-only-initial-condition-keeps-abstract-transition-identity
  (let [s (assoc-in source [:initial-condition :expression]
                    [:= [:read :request/status] [:literal :claimed]])
        before (elaborate! source)
        after (elaborate! s)]
    (is (= (:transition-fingerprint before) (:transition-fingerprint after)))
    (is (not= (:obligation-fingerprint before) (:obligation-fingerprint after)))
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (= (mapv :goal (:fact-proofs before))
           (mapv :goal (:fact-proofs after))))
    (assert-open! after (keys invariants))))

(deftest semantic-change-invalidates-proof-source-identity
  (let [before (elaborate! source)
        variants {:guard (assoc-in source [:construction :guard :expression]
                                   [:= [:read :request/status] [:literal :claimed]])
                  :assignment (assoc-in source [:construction :replacements :request/status]
                                        [:literal :done])
                  :subject (assoc-in source [:construction :subject :id]
                                     :example/claim-next)
                  :type (assoc-in source
                                  [:construction :state-types :request/status :values]
                                  #{:open :claimed :done :closed})}]
    (doseq [[kind s] variants]
      (testing (name kind)
        (let [after (elaborate! s)]
          (is (not= (:transition-fingerprint before)
                    (:transition-fingerprint after)))
          (is (not= (:fingerprint before) (:fingerprint after)))
          (is (= :none (:authority after))))))))

(deftest two-identical-runs-have-identical-proof-order-and-fingerprint
  (let [a (elaborate! source)
        b (elaborate! source)]
    (is (= (:fingerprint a) (:fingerprint b)))
    (is (= (:fact-proofs a) (:fact-proofs b)))
    (is (= (:batch-proofs a) (:batch-proofs b)))
    (is (= (:open-obligation-ids a) (:open-obligation-ids b)))))

(deftest bounded-conjunction-batching-for-128-state-fields
  (let [fields (into {}
                     (for [n (range 128)]
                       [(keyword "state" (str "f" n)) {:type :integer}]))
        c {:subject {:kind :operation :id :example/read-only
                     :interface {:operation :example/read-only}}
           :state-types fields
           :input-types {}
           :guard {:readers #{} :expression [:literal true]}
           :reads #{} :writes #{} :replacements {}
           :frame (set (keys fields))}
        s {:construction c
           :initial-condition {:readers #{} :expression [:literal true]}
           :invariants {:example/true
                        {:readers #{} :expression [:literal true]}}}
        report (elaborate! s)
        batches (:batch-proofs report)]
    (is (= 129 (:checked-fact-count report)))
    (is (= 2 (count batches)))
    (is (= [128 1] (mapv (comp count :fact-ids) batches)))
    (is (= (set (keys fields))
           (get-in report [:primitive-coverage :frame-fields])))
    (is (= 13 (count (:open-obligation-ids report))))
    (is (= 129 (count (set (map (comp :id first :steps :proof-source)
                            (:fact-proofs report))))))
    (doseq [b batches] (assert-conditional-check! b))
    (assert-open! report (keys (:invariants s)))))

(deftest input-must-be-original-closed-source
  (let [report (elaborate! source)]
    (doseq [bad [(assoc source :valid? true)
                 (assoc source :certificate {:valid? true})
                 (assoc source :authority :production)
                 (assoc source :proof-status :proved)
                 (assoc source :open-obligation-ids #{})
                 report
                 (:construction source)]]
      (testing (str (set (keys bad)))
        (rejects-as! #(elaborate! bad) obligation-error :wrong-keys)))))

(deftest missing-or-malformed-invariant-declarations-fail-closed
  (rejects-as! #(elaborate! (dissoc source :invariants))
               obligation-error :wrong-keys)
  (rejects-as! #(elaborate! (assoc source :invariants {}))
               obligation-error :invariant-count)
  (rejects-as! #(elaborate! (assoc-in source [:invariants :request/invalid]
                                         {:readers #{:request/absent}
                                          :expression [:present [:read :request/absent]]}))
               obligation-error :non-state-reader))

(deftest constructor-rejects-hidden-physical-effects-and-false-frame
  (rejects-as! #(elaborate! (assoc-in source [:construction :effect]
                                         (fn [] :committed)))
               construction-error :wrong-keys)
  (rejects-as! #(elaborate! (update-in source [:construction :frame]
                                        disj :request/note))
               construction-error :frame-footprint-mismatch)
  (rejects-as! #(elaborate! (update-in source [:construction :writes]
                                        conj :request/version))
               construction-error :write-footprint-mismatch))

(deftest unsupported-optional-pre-reader-does-not-generate-proof
  (let [bad (-> source
                (update-in [:construction :reads] conj :request/owner)
                (assoc-in [:construction :replacements :request/owner]
                          [:read :request/owner]))]
    (rejects-as! #(elaborate! bad)
                 abstract-transition-error :optional-assignment-source)))

(deftest optional-frame-cannot-be-forged-as-total-value-equality
  (let [r (elaborate! source)
        original (:proof-source (fact-for r :frame :request/note))
        bogus [:eq [:post :request/note] [:pre :request/note]]
        mutated (-> original
                    (assoc-in [:steps 0 :conclusion] bogus)
                    (assoc :goal bogus))]
    (rejects-as! #(derivation/check-transition-derivation! mutated)
                 derivation-error :partial-equality-reader)))

(deftest mutated-generator-assignment-fails-independent-check
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (let [i (candidate-index entries :assignment :request/status)
              bogus [:eq [:post :request/status] [:literal :open]]]
          (-> entries
              (assoc-in [i :goal] bogus)
              (assoc-in [i :step :conclusion] bogus)))))
   derivation-error :wrong-assignment-conclusion))

(deftest mutated-generator-frame-fails-independent-check
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (let [i (candidate-index entries :frame :request/note)
              bogus [:same-field :request/version]]
          (-> entries
              (assoc-in [i :goal] bogus)
              (assoc-in [i :step :conclusion] bogus)))))
   derivation-error :wrong-frame-conclusion))

(deftest mutated-generator-guard-fails-independent-check
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (let [i (candidate-index entries :guard nil)
              bogus [:eq [:post :request/status] [:literal :claimed]]]
          (-> entries
              (assoc-in [i :goal] bogus)
              (assoc-in [i :step :conclusion] bogus)))))
   derivation-error :wrong-guard-conclusion))

(deftest mutated-generator-unsupported-rule-never-fabricates-arithmetic
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (let [i (candidate-index entries :assignment :request/status)]
          (assoc-in entries [i :step :rule] :arithmetic-inference))))
   derivation-error :unknown-rule))

(deftest mutated-generator-unsupported-implication-never-proves-invariant
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (let [i (candidate-index entries :guard nil)
              bogus [:implies [:guard] [:guard]]]
          (-> entries
              (assoc-in [i :goal] bogus)
              (assoc-in [i :step :conclusion] bogus)))))
   derivation-error :unknown-claim-kind))

(deftest missing-generator-fact-fails-coverage-before-returning-report
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (vec (remove (fn [entry]
                       (and (= :frame (:kind entry))
                            (= :request/note (:field entry)))) entries))))
   elaboration-error :incomplete-primitive-coverage))

(deftest duplicate-generator-fact-fails-coverage-before-returning-report
  (rejects-as!
   #(with-buggy-candidates
      (fn [entries]
        (conj entries (first entries))))
   elaboration-error :incomplete-primitive-coverage))

(deftest buggy-conjunction-generator-cannot-reuse-a-fact-id
  (let [id-var (private-elaborator-var 'conjunction-id)]
    (with-redefs-fn {id-var (fn [] :gesso.assurance.proof.elaborate/fact-0)}
      #(rejects-as! (fn [] (elaborate! source))
                    derivation-error :duplicate-step-id))))

(deftest corrupted-conjunction-reference-fails-original-derivation-checker
  (let [report (elaborate! source)
        batch (first (:batch-proofs report))
        bad (-> (:proof-source batch)
                (assoc-in [:steps 5 :from 0] :bogus/missing))]
    (rejects-as! #(derivation/check-transition-derivation! bad)
                 derivation-error :unknown-or-forward-step)))

(deftest corrupted-conjunction-goal-fails-original-derivation-checker
  (let [report (elaborate! source)
        batch (first (:batch-proofs report))
        bad (assoc-in (:proof-source batch) [:steps 5 :conclusion]
                      [:and [:guard] [:guard]])]
    (rejects-as! #(derivation/check-transition-derivation! bad)
                 derivation-error :wrong-conjunction-introduction)))

(deftest checker-result-cannot-fabricate-a-closed-premise
  (let [original derivation/check-transition-derivation!]
    (with-redefs [derivation/check-transition-derivation!
                  (fn [s] (assoc (original s) :open-premises #{}))]
      (rejects-as! #(elaborate! source)
                   elaboration-error :derivation-contract-changed))))

(deftest checker-result-cannot-fabricate-production-authority
  (let [original derivation/check-transition-derivation!]
    (doseq [field-and-value [[:authority :production]
                             [:admission :admitted]
                             [:proof-status :proved]
                             [:missing-obligations #{}]]]
      (let [[field value] field-and-value]
        (with-redefs [derivation/check-transition-derivation!
                      (fn [s] (assoc (original s) field value))]
          (rejects-as! #(elaborate! source)
                       elaboration-error :derivation-contract-changed))))))

(deftest altered-upstream-obligations-are-not-silently-discarded
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (update (original s) :open-obligation-ids
                            conj [:implementation :new-external-condition]))]
      (rejects-as! #(elaborate! source)
                   elaboration-error :obligation-contract-changed))))

(deftest altered-upstream-obligation-status-fails-closed
  (let [original obligation/derive-obligations!]
    (with-redefs [obligation/derive-obligations!
                  (fn [s]
                    (assoc-in (original s)
                              [:obligations :preservation 0 :status] :proved))]
      (rejects-as! #(elaborate! source)
                   elaboration-error :obligation-contract-changed))))

(deftest stale-proof-source-does-not-recheck-against-changed-transition
  (let [r (elaborate! source)
        p (:proof-source (fact-for r :assignment :request/status))
        changed (assoc-in source [:construction :replacements :request/status]
                          [:literal :done])
        fresh (elaborate! changed)
        stale (assoc p :transition
                     (get-in fresh [:fact-proofs 0 :proof-source :transition]))]
    (rejects-as! #(derivation/check-transition-derivation! stale)
                 derivation-error :wrong-assignment-conclusion)))

(deftest reports-remain-data-even-after-tampering
  (let [r (elaborate! source)
        fake (assoc r :certificate {:valid? true} :authority :production
                    :invariant-preservation :proved)
        fresh (elaborate! source)]
    (is (= r fresh))
    (is (not= fake fresh))
    (is (= :none (:authority fresh)))
    (is (= :not-issued (:certificate fresh)))
    (rejects-as! #(elaborate! fake) obligation-error :wrong-keys)))
