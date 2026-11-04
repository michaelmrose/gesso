(ns gesso.assurance.proof.obligation-test
  "Permanent adversarial tests for v1241 source-derived obligation generation.

   This suite tests COMPLETE ENUMERATION, source freshness and honest assurance
   boundaries. It never interprets an open obligation as a proof, and it never
   treats identity/fingerprint equality as a grant of authority. The generator
   must be checked against the original v1239 construction and typed rules;
   a fabricated report is not an acceptable input."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.obligation :as obligations]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.transition :as construction]))

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

(def ^:private rules
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

(def ^:private baseline
  {:construction claim
   :initial-condition initial
   :invariants rules})

(defn- derive! [source] (obligations/derive-obligations! source))

(defn- failure [source]
  (try (derive! source) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejects!
  [source expected-type expected-reason]
  (let [error (failure source)]
    (is (map? error) (str "Expected " expected-type "/" expected-reason))
    (is (= expected-type (:error/type error)) (pr-str error))
    (is (= expected-reason (:reason error)) (pr-str error))
    error))

(defn- initial-for [report id]
  (first (filter #(= id (:invariant %))
                 (get-in report [:obligations :initial-state]))))

(defn- preservation-for [report id]
  (first (filter #(= id (:invariant %))
                 (get-in report [:obligations :preservation]))))

(defn- all-open!
  [report expected-invariants]
  (let [ob (:obligations report)
        listed (vec (mapcat identity (vals ob)))
        ids (map :id listed)
        expected (set expected-invariants)]
    (is (= obligations/obligation-type (:type report)))
    (is (= obligations/obligation-version (:version report)))
    (is (= #{:initial-state :preservation :assumption :implementation}
           (set (keys ob))))
    (is (= expected (set (map :invariant (:initial-state ob)))))
    (is (= expected (set (map :invariant (:preservation ob)))))
    (is (= (count expected) (count (:initial-state ob))))
    (is (= (count expected) (count (:preservation ob))))
    (is (= 5 (count (:assumption ob))))
    (is (= 6 (count (:implementation ob))))
    (is (= (+ (* 2 (count expected)) 11) (count listed)))
    (is (= (count ids) (count (set ids))) "No duplicate obligation IDs")
    (is (= (set ids) (:open-obligation-ids report)))
    (is (every? #(= :open (:status %)) listed))
    (is (= :all-obligations-open (:status report)))
    (is (= :not-proved (:proof-status report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :not-established (:runtime-enforcement report)))
    (is (= :not-established (:implementation-correspondence report)))
    (is (= :none (:authority report)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :transaction-authority)))
    report))

(deftest all-four-classes-remain-open-and-separately-identifiable
  (let [report (all-open! (derive! baseline) (keys rules))
        ids (:open-obligation-ids report)]
    (is (= baseline (:source report)))
    (is (= :original-sources-and-typed-rules (:checking report)))
    (is (contains? ids [:assumption :initial-state-existence]))
    (is (contains? ids [:assumption :initial-state-coverage]))
    (is (contains? ids [:assumption :declared-state-projection-coverage]))
    (is (contains? ids [:assumption :declared-invariant-set-coverage]))
    (is (contains? ids [:assumption :checker-meta-soundness]))
    (doseq [id [:production-transition-correspondence
                :current-runtime-authorization
                :atomic-transaction-enforcement
                :physical-effect-containment
                :runtime-type-correspondence
                :integer-semantics-correspondence]]
      (is (contains? ids [:implementation id])))))

(deftest all-invariants-occur-in-both-induction-obligations
  (let [report (derive! baseline)
        invariant-ids (set (keys rules))
        transition-fingerprint (:transition-fingerprint report)]
    (doseq [id invariant-ids]
      (let [init (initial-for report id)
            preservation (preservation-for report id)]
        (is (= [:initial-state id] (:id init)))
        (is (= :initial-state (:kind init)))
        (is (= :all-declared-abstract-states (:quantification init)))
        (is (= {:initial-condition-ref :declared-initial-condition}
               (:antecedent init)))
        (is (= {:state :pre :invariant-ref id} (:consequent init)))
        (is (= transition-fingerprint (:transition-fingerprint init)))
        (is (= [:preservation id] (:id preservation)))
        (is (= :invariant-preservation (:kind preservation)))
        (is (= :all-declared-abstract-pre-input-post-triples
               (:quantification preservation)))
        (is (= invariant-ids (get-in preservation [:antecedent :pre-invariant-ids])))
        (is (= :exact-typed-guard-assignments-and-frame
               (get-in preservation [:antecedent :transition-relation])))
        (is (= {:state :post :invariant-ref id}
               (:consequent preservation)))))))

(deftest each-preservation-retains-entire-source-footprint
  (let [report (derive! baseline)
        description (abstract/describe-transition!
                     (:transition (construction/build-guarded-replacement! claim)))]
    (doseq [ob (get-in report [:obligations :preservation])]
      (is (= (:reads claim) (:source-read-fields ob)))
      (is (= (:writes claim) (:source-write-fields ob)))
      (is (= (:frame claim) (:source-frame-fields ob)))
      (is (= (:readers (:guard claim)) (:guard-read-fields ob)))
      (is (= (set (keys (:state-types claim))) (:state-projection ob)))
      (is (= (:fingerprint description)
             (get-in ob [:antecedent :transition-fingerprint]))))))

(deftest written-and-framed-invariant-dependencies-are-distinguished
  (let [report (derive! baseline)
        status (preservation-for report :request/valid-status)
        version (preservation-for report :request/nonnegative-version)
        owner (preservation-for report :request/claimed-has-owner)]
    (is (= #{:request/status} (:invariant-written-fields status)))
    (is (= #{} (:invariant-framed-fields status)))
    (is (= #{} (:invariant-written-fields version)))
    (is (= #{:request/version} (:invariant-framed-fields version)))
    (is (= #{:request/status :request/owner} (:invariant-written-fields owner)))
    (is (= #{} (:invariant-framed-fields owner)))
    (is (= :open (:status version)) "A framed invariant is not silently proved")))

(deftest a-writer-added-to-a-formerly-framed-field-is-never-hidden
  (let [changed (-> baseline
                    (update-in [:construction :writes] conj :request/version)
                    (update-in [:construction :frame] disj :request/version)
                    (assoc-in [:construction :replacements :request/version] [:literal 42]))
        before (derive! baseline)
        after (all-open! (derive! changed) (keys rules))
        version (preservation-for after :request/nonnegative-version)]
    (is (not= (:fingerprint before) (:fingerprint after)))
    (is (not= (:transition-fingerprint before) (:transition-fingerprint after)))
    (is (= #{:request/version} (:invariant-written-fields version)))
    (is (= #{} (:invariant-framed-fields version)))
    (doseq [ob (get-in after [:obligations :preservation])]
      (is (= #{:request/status :request/owner :request/version}
             (:source-write-fields ob)))
      (is (= #{:request/note} (:source-frame-fields ob))))))

(deftest optional-field-frame-is-not-dropped-when-unmentioned-by-invariant
  (let [extra (-> baseline
                  (assoc-in [:construction :state-types :request/audit]
                            {:type :string :optional? true})
                  (update-in [:construction :frame] conj :request/audit))
        report (derive! extra)]
    (is (not= (:fingerprint (derive! baseline)) (:fingerprint report)))
    (doseq [ob (get-in report [:obligations :preservation])]
      (is (contains? (:source-frame-fields ob) :request/audit))
      (is (contains? (:state-projection ob) :request/audit))
      (is (not (contains? (:source-write-fields ob) :request/audit))))
    (all-open! report (keys rules))))

(deftest added-invariant-always-adds-two-open-obligations
  (let [new-id :request/note-is-optional
        new-rule {:readers #{:request/note}
                  :expression [:or
                               [:not [:present [:read :request/note]]]
                               [:present [:read :request/note]]]}
        expanded (assoc-in baseline [:invariants new-id] new-rule)
        old (derive! baseline)
        report (derive! expanded)]
    (all-open! report (keys (:invariants expanded)))
    (is (= (inc (count (get-in old [:obligations :initial-state])))
           (count (get-in report [:obligations :initial-state]))))
    (is (= (inc (count (get-in old [:obligations :preservation])))
           (count (get-in report [:obligations :preservation]))))
    (is (contains? (:open-obligation-ids report) [:initial-state new-id]))
    (is (contains? (:open-obligation-ids report) [:preservation new-id]))
    (is (= (set (keys (:invariants expanded)))
           (get-in (preservation-for report :request/valid-status)
                   [:antecedent :pre-invariant-ids])))
    (is (not= (:fingerprint old) (:fingerprint report)))))

(deftest omitted-invariant-is-not-detectable-as-a-business-error
  (let [omitted (update baseline :invariants dissoc :request/nonnegative-version)
        report (derive! omitted)]
    (all-open! report (keys (:invariants omitted)))
    (is (not (contains? (:open-obligation-ids report)
                        [:preservation :request/nonnegative-version])))
    (is (contains? (:open-obligation-ids report)
                   [:assumption :declared-invariant-set-coverage]))
    (is (= :all-obligations-open (:status report)))
    (is (= :not-proved (:proof-status report)))
    (is (not= (:fingerprint report) (:fingerprint (derive! baseline))))))

(deftest only-invariant-cannot-be-omitted-entirely
  (let [one (assoc baseline :invariants (select-keys rules [:request/valid-status]))]
    (all-open! (derive! one) [:request/valid-status])
    (rejects! (assoc one :invariants {})
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :invariant-count)))

(deftest independent-invariants-retain-distinct-identities
  (let [a (select-keys rules [:request/valid-status])
        copied (assoc a :request/other-status (:request/valid-status rules))
        report (derive! (assoc baseline :invariants copied))]
    (all-open! report (keys copied))
    (is (not= (get-in report [:invariants :request/valid-status]) nil))
    (is (= #{[:initial-state :request/valid-status]
             [:initial-state :request/other-status]}
           (set (map :id (get-in report [:obligations :initial-state])))))
    (is (= #{[:preservation :request/valid-status]
             [:preservation :request/other-status]}
           (set (map :id (get-in report [:obligations :preservation])))))))

(deftest source-fingerprints-respond-to-meaningful-changes
  (let [original (derive! baseline)
        variants
        {:initial (assoc-in baseline [:initial-condition :expression]
                            [:= [:read :request/status] [:literal :claimed]])
         :invariant (assoc-in baseline
                              [:invariants :request/nonnegative-version :expression]
                              [:>= [:read :request/version] [:literal 1]])
         :guard (assoc-in baseline [:construction :guard :expression]
                         [:= [:read :request/status] [:literal :claimed]])
         :replacement (assoc-in baseline [:construction :replacements :request/status]
                               [:literal :done])
         :state-type (assoc-in baseline [:construction :state-types :request/status :values]
                              #{:open :claimed :done :archived})
         :subject (assoc-in baseline [:construction :subject :id] :example/claim-next)}]
    (doseq [[kind altered] variants]
      (testing (name kind)
        (let [report (derive! altered)]
          (is (not= (:fingerprint original) (:fingerprint report)))
          (is (= :all-obligations-open (:status report)))
          (is (= :not-proved (:proof-status report))))))))

(deftest changing-only-initial-predicate-retains-transition-identity
  (let [original (derive! baseline)
        altered (derive! (assoc-in baseline [:initial-condition :expression]
                                  [:= [:read :request/status] [:literal :claimed]]))]
    (is (= (:transition-fingerprint original) (:transition-fingerprint altered)))
    (is (not= (:fingerprint original) (:fingerprint altered)))
    (is (= (get-in original [:obligations :preservation])
           (get-in altered [:obligations :preservation])))
    (is (not= (:initial-condition original) (:initial-condition altered)))))

(deftest malformed-source-envelopes-fail-closed
  (doseq [altered [(dissoc baseline :construction)
                   (dissoc baseline :initial-condition)
                   (dissoc baseline :invariants)
                   (assoc baseline :proof-status :proved)
                   (assoc baseline :open-obligation-ids #{})
                   (assoc baseline :certificate {:valid? true})
                   (assoc baseline :authority :production)
                   (assoc baseline :valid? true)
                   (assoc baseline :obligations {})]]
    (rejects! altered
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys))
  (rejects! (assoc baseline :invariants [])
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :expected-invariant-map))

(deftest invalid-invariant-ids-and-counts-fail-closed
  (rejects! (assoc baseline :invariants {})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :invariant-count)
  (rejects! (assoc baseline :invariants {:invalid (:request/valid-status rules)})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :invalid-invariant-id)
  (rejects! (assoc baseline :invariants {"request/valid" (:request/valid-status rules)})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :invalid-invariant-id)
  (let [too-many (into {}
                       (map (fn [n] [(keyword "invariant" (str "rule-" n))
                                     (:request/valid-status rules)]))
                       (range 65))]
    (rejects! (assoc baseline :invariants too-many)
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :invariant-count)))

(deftest source-and-nested-metadata-are-not-evidence
  (doseq [altered [(with-meta baseline {:proved true})
                   (update baseline :invariants with-meta {:valid? true})
                   (update baseline :initial-condition with-meta {:proved true})
                   (update-in baseline [:invariants :request/valid-status]
                              with-meta {:admission :yes})]]
    (rejects! altered
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :metadata-not-allowed)))

(deftest input-and-nonexistent-state-readers-are-invalid
  (rejects! (assoc baseline :initial-condition
                   {:readers #{:input/actor}
                    :expression [:present [:read :input/actor]]})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :non-state-reader)
  (rejects! (assoc-in baseline [:invariants :request/valid-status]
                      {:readers #{:request/missing}
                       :expression [:present [:read :request/missing]]})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :non-state-reader)
  (rejects! (assoc-in baseline [:invariants :request/valid-status]
                      {:readers #{:input/actor}
                       :expression [:present [:read :input/actor]]})
            :gesso.assurance.proof.obligation/invalid-obligation-source
            :non-state-reader))

(deftest incomplete-or-extra-predicate-keys-are-not-accepted
  (doseq [path [[:initial-condition]
                [:invariants :request/valid-status]]]
    (let [decl (get-in baseline path)]
      (rejects! (assoc-in baseline path (dissoc decl :readers))
                :gesso.assurance.proof.obligation/invalid-obligation-source
                :wrong-keys)
      (rejects! (assoc-in baseline path (assoc decl :proof true))
                :gesso.assurance.proof.obligation/invalid-obligation-source
                :wrong-keys)
      (rejects! (assoc-in baseline path (assoc decl :callback (fn [] true)))
                :gesso.assurance.proof.obligation/invalid-obligation-source
                :wrong-keys))))

(deftest malformed-reader-declaration-is-rejected
  (doseq [path [[:initial-condition :readers]
                [:invariants :request/valid-status :readers]]]
    (rejects! (assoc-in baseline path [:request/status])
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :expected-reader-set)))

(deftest invalid-construction-is-rejected-before-obligations
  (let [no-frame (update-in baseline [:construction :frame] disj :request/version)
        undeclared-write (update-in baseline [:construction :writes] conj :request/note)
        unknown-effect (assoc-in baseline [:construction :side-effect] :network)]
    (doseq [bad [no-frame undeclared-write unknown-effect]]
      (let [err (failure bad)]
        (is (map? err))
        (is (= :gesso.assurance.transition/invalid-construction
               (:error/type err))
            (pr-str err))))))

(deftest a-declaration-does-not-prove-its-own-invariant
  (let [source (-> baseline
                   (assoc :invariants {:request/status-is-open
                                       {:readers #{:request/status}
                                        :expression [:= [:read :request/status]
                                                       [:literal :open]]}})
                   (assoc-in [:construction :replacements :request/status]
                             [:literal :claimed]))
        report (derive! source)]
    ;; This operation writes :claimed; the declared invariant says :open.
    ;; v1241 MUST generate the preservation obligation and NOT certify it.
    (all-open! report [:request/status-is-open])
    (is (= #{:request/status}
           (:invariant-written-fields
            (preservation-for report :request/status-is-open))))))

(deftest an-inconsistent-initial-condition-is-not-disguised-as-proof
  (let [source (-> baseline
                   (assoc :invariants {:request/status-is-claimed
                                       {:readers #{:request/status}
                                        :expression [:= [:read :request/status]
                                                       [:literal :claimed]]}}))
        report (derive! source)]
    (all-open! report [:request/status-is-claimed])
    (is (= :open (get-in (initial-for report :request/status-is-claimed)
                         [:status])))
    (is (contains? (:open-obligation-ids report)
                   [:assumption :initial-state-existence]))))

(deftest reports-are-copyable-and-not-accepted-as-credentials
  (let [report (derive! baseline)]
    (rejects! report
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys)
    (rejects! (assoc baseline :fingerprint (:fingerprint report))
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys)
    (rejects! (assoc baseline :source (:source report))
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys)
    (is (= report (derive! (:source report))))
    (is (= #{} (set/intersection (:open-obligation-ids report)
                                #{[:proof :admitted] [:authority :transaction]})))))

(deftest changing-a-returned-report-does-not-change-source-derivation
  (let [report (derive! baseline)
        forged (-> report
                   (assoc :status :verified
                          :proof-status :proved
                          :certificate {:valid? true}
                          :admission :admitted
                          :authority :commit
                          :open-obligation-ids #{}))]
    (is (= report (derive! baseline)))
    (is (= report (derive! (:source forged))))
    (rejects! forged
              :gesso.assurance.proof.obligation/invalid-obligation-source
              :wrong-keys)))

(deftest upstream-construction-obligation-changes-fail-closed
  (let [real construction/build-guarded-replacement!]
    (with-redefs [construction/build-guarded-replacement!
                  (fn [source]
                    (update (real source) :open-obligations conj :new/upstream-duty))]
      (rejects! baseline
                :gesso.assurance.proof.obligation/invalid-obligation-source
                :construction-obligation-contract-changed))))

(deftest changing-an-existing-upstream-obligation-also-fails-closed
  (let [real construction/build-guarded-replacement!]
    (with-redefs [construction/build-guarded-replacement!
                  (fn [source]
                    (update (real source) :open-obligations disj
                            :physical-effect-containment))]
      (rejects! baseline
                :gesso.assurance.proof.obligation/invalid-obligation-source
                :construction-obligation-contract-changed))))

(deftest a-read-only-transition-still-derives-preservation
  (let [readonly (-> baseline
                     (assoc-in [:construction :input-types] {})
                     (assoc-in [:construction :reads] #{:request/status})
                     (assoc-in [:construction :writes] #{})
                     (assoc-in [:construction :replacements] {})
                     (assoc-in [:construction :frame]
                               (set (keys (:state-types claim)))))
        report (derive! readonly)]
    (all-open! report (keys rules))
    (doseq [ob (get-in report [:obligations :preservation])]
      (is (= #{} (:source-write-fields ob)))
      (is (= (set (keys (:state-types claim))) (:source-frame-fields ob))))))
