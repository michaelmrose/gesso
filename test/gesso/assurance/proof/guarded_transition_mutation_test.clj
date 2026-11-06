(ns gesso.assurance.proof.guarded-transition-mutation-test
  "v1433: adversarial transition witnesses and source mutations checked against
   a small, test-local prestate interpreter. A matching postcondition alone
   must not admit a transition whose guard, assignment, or frame is wrong.

   This is bounded ABSTRACT evidence only; it neither certifies the proof
   kernel nor establishes XTDB writer coverage or physical refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.field-copy-soundness :as copy]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.preservation-composition-soundness :as composition]))

(def ^:private source
  {:subject {:kind :operation :id :test/claim
             :interface {:operation :test/claim :revision 1433}}
   :state-types
   {:case/status {:type :enum :name :case/status :values #{:open :claimed}}
    :case/owner {:type :keyword :nullable? true}
    :case/mirror {:type :keyword :nullable? true}
    :case/previous-owner {:type :keyword :nullable? true}
    :case/revision {:type :integer}
    :case/flag {:type :boolean}
    :case/memo {:type :string :optional? true :nullable? true}
    :case/ack {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:case/status :case/owner}
           :expression [:and [:= [:read :case/status] [:literal :open]]
                        [:= [:read :case/owner] [:literal nil]]]}
   :reads #{:case/status :case/owner :input/actor}
   :writes #{:case/status :case/owner :case/mirror
             :case/previous-owner :case/ack}
   :assignments {:case/status [:literal :claimed]
                 :case/owner [:read :input/actor]
                 :case/mirror [:read :input/actor]
                 :case/previous-owner [:read :case/owner]
                 :case/ack [:literal nil]}})

(def ^:private base
  {:case/status :open :case/owner nil :case/mirror :actor/previous
   :case/previous-owner :actor/older :case/revision 7 :case/flag false})

(def ^:private same-owner-invariant
  {:id :test/same-owner :readers #{:case/owner :case/mirror}
   :expression [:= [:read :case/owner] [:read :case/mirror]]})
(def ^:private claimed-invariant
  {:id :test/claimed :readers #{:case/status}
   :expression [:= [:read :case/status] [:literal :claimed]]})
(def ^:private combined-invariant
  {:id :test/composed :readers #{:case/revision :case/status
                                :case/owner :case/mirror}
   :expression [:and [:>= [:read :case/revision] [:literal 0]]
                [:= [:read :case/status] [:literal :claimed]]
                [:= [:read :case/owner] [:read :case/mirror]]]})

(defn- oracle-poststate
  "Independently apply :literal/:read RHS against one ORIGINAL environment;
   unlike a sequential update loop, all :read values are captured first."
  [declaration before inputs]
  (let [env (merge before inputs)
        computed (into {}
                       (for [[field [op value]] (:assignments declaration)]
                         [field (case op
                                  :literal value
                                  :read (get env value))]))]
    (merge before computed)))

(defn- oracle-guard? [before]
  (and (= :open (:case/status before))
       (contains? before :case/owner)
       (nil? (:case/owner before))))

(defn- oracle-match?
  "An independently explicit predicate for the ORIGINAL source fixture.
   Presence is checked, not just equality of (get state optional-field)."
  [before inputs after]
  (and (oracle-guard? before)
       (= (set (keys before))
          (disj (set (keys after)) :case/ack))
       (contains? after :case/ack)
       (nil? (:case/ack after))
       (= :claimed (:case/status after))
       (= (:input/actor inputs) (:case/owner after) (:case/mirror after))
       (= (:case/owner before) (:case/previous-owner after))
       (every? (fn [field]
                 (and (= (contains? before field) (contains? after field))
                      (= (get before field) (get after field))))
               [:case/revision :case/flag :case/memo])))

(defn- assert-observation! [decl before inputs after expected]
  (let [prod (transition/observe-transition! decl before inputs after)
        ind (independent/observe-transition! decl before inputs after)]
    (is (= expected (:matches? prod)))
    (is (= expected (:matches? ind)))
    (is (= (select-keys prod [:guard-holds? :assignments-hold?
                              :frame-holds? :matches?])
           (select-keys ind [:guard-holds? :assignments-hold?
                             :frame-holds? :matches?])))
    (is (= :none (:authority ind)))
    (is (= :not-admitted (:admission ind)))))

(defn- no-certificate! [report]
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report))))

(deftest nominal-and-optional-baseline
  (let [lemma (copy/derive-copy-lemma! source same-owner-invariant)
        literal-lemma (literal/derive-literal-lemma! source claimed-invariant)
        combined (composition/derive-composed-lemma! source combined-invariant)]
    (is (= :conditional-same-origin-copy-rule (:status lemma)))
    (is (= :conditional-literal-rule-instance (:status literal-lemma)))
    (is (= :conditional-composed-rule-instance (:status combined)))
    (doseq [r [lemma literal-lemma combined]] (no-certificate! r)))
  (doseq [actor [:actor/alice :actor/bob]
          revision [0 7]
          flag [false true]
          memo [::absent nil "memo"]]
    (let [before (cond-> (assoc base :case/revision revision :case/flag flag)
                   (not= ::absent memo) (assoc :case/memo memo))
          inputs {:input/actor actor}
          after (oracle-poststate source before inputs)]
      (is (oracle-match? before inputs after))
      (is (nil? (:case/previous-owner after)))
      (assert-observation! source before inputs after true))))

(deftest altered-written-fields-are-not-matching-transitions
  (let [inputs {:input/actor :actor/alice}
        after (oracle-poststate source base inputs)
        corruptions {:wrong-status (assoc after :case/status :open)
                     :wrong-owner (assoc after :case/owner :actor/bob)
                     :wrong-mirror (assoc after :case/mirror :actor/bob)
                     :wrong-previous-read (assoc after :case/previous-owner :actor/alice)
                     :missing-optional-write (dissoc after :case/ack)
                     :wrong-optional-write (assoc after :case/ack "present")
                     :copy-both-wrong (assoc after :case/owner :actor/bob
                                               :case/mirror :actor/bob)}]
    (doseq [[name candidate] corruptions]
      (testing (str "corruption: " name)
        (is (false? (oracle-match? base inputs candidate)))
        (assert-observation! source base inputs candidate false)))
    ;; Equal owner and mirror is only a postcondition, not execution evidence.
    (let [candidate (:copy-both-wrong corruptions)
          witness (copy/audit-copy-witness!
                   source same-owner-invariant base inputs candidate)]
      (is (= (:case/owner candidate) (:case/mirror candidate)))
      (is (false? (:matches-transition? witness)))
      (no-certificate! witness))))

(deftest frame-mutations-detect-value-and-presence-changes
  (doseq [before [base (assoc base :case/memo nil)
                  (assoc base :case/memo "original")]
          :let [inputs {:input/actor :actor/alice}
                after (oracle-poststate source before inputs)]
          [tag altered] [[:revision (assoc after :case/revision 8)]
                         [:boolean (update after :case/flag not)]
                         [:memo-value (assoc after :case/memo "changed")]
                         [:memo-absence (dissoc after :case/memo)]
                         [:memo-present-nil (assoc after :case/memo nil)]]
          :when (not= (select-keys after [:case/revision :case/flag :case/memo])
                      (select-keys altered [:case/revision :case/flag :case/memo]))]
    (testing (str tag " from " (if (contains? before :case/memo) :present :absent))
      (is (false? (oracle-match? before inputs altered)))
      (let [observation (independent/observe-transition! source before inputs altered)]
        (is (false? (:frame-holds? observation)))
        (is (false? (:matches? observation))))
      (assert-observation! source before inputs altered false))))

(deftest false-guard-is-not-authorized-by-an-otherwise-valid-poststate
  (doseq [before [(assoc base :case/status :claimed)
                  (assoc base :case/owner :actor/bob)]]
    (let [inputs {:input/actor :actor/alice}
          after (oracle-poststate source before inputs)]
      (is (false? (oracle-guard? before)))
      (is (false? (oracle-match? before inputs after)))
      (let [obs (independent/observe-transition! source before inputs after)]
        (is (false? (:guard-holds? obs)))
        (is (false? (:matches? obs))))
      (assert-observation! source before inputs after false))))

(deftest replayed-result-under-a-different-command-input-is-not-a-step
  (let [original-input {:input/actor :actor/alice}
        current-input {:input/actor :actor/bob}
        old-result (oracle-poststate source base original-input)]
    (is (oracle-match? base original-input old-result))
    (assert-observation! source base original-input old-result true)
    (is (false? (oracle-match? base current-input old-result)))
    (assert-observation! source base current-input old-result false)
    ;; The copied fields agree with each other, but both belong to the old
    ;; command input. Their agreement cannot authorize the new command.
    (is (= (:case/owner old-result) (:case/mirror old-result)))
    (let [witness (copy/audit-copy-witness!
                   source same-owner-invariant base current-input old-result)]
      (is (false? (:matches-transition? witness)))
      (no-certificate! witness))))

(deftest valid-mutated-sources-are-not-evidence-for-the-original-source
  (let [inputs {:input/actor :actor/alice}
        ;; Removing a write from BOTH footprints leaves a syntactically valid
        ;; different operation; a sample poststate may violate the old rule.
        missing-mirror (-> source
                           (update :writes disj :case/mirror)
                           (update :assignments dissoc :case/mirror))
        changed-reader (assoc-in source [:assignments :case/mirror]
                                 [:read :case/owner])
        changed-literal (assoc-in source [:assignments :case/status]
                                  [:literal :open])
        mutations [[:removed-write missing-mirror]
                   [:changed-copy-origin changed-reader]
                   [:changed-status-literal changed-literal]]]
    (doseq [[name mutated] mutations]
      (testing (str "valid alternative declaration: " name)
        (let [candidate (oracle-poststate mutated base inputs)]
          (is (some? (transition/describe-transition! mutated)))
          (assert-observation! mutated base inputs candidate true)
          (is (false? (oracle-match? base inputs candidate)))
          (assert-observation! source base inputs candidate false))))
    (is (= :open-no-shared-total-origin
           (:status (copy/derive-copy-lemma!
                     changed-reader same-owner-invariant))))
    (is (= :open-no-shared-total-origin
           (:status (copy/derive-copy-lemma!
                     missing-mirror same-owner-invariant))))
    (is (not= :conditional-literal-rule-instance
              (:status (literal/derive-literal-lemma!
                        changed-literal claimed-invariant))))))

(deftest a-weakened-guard-does-not-substitute-for-original-authorization
  (let [weakened (assoc source
                        :guard {:readers #{} :expression [:literal true]}
                        :reads #{:case/owner :input/actor})
        before (assoc base :case/status :claimed)
        inputs {:input/actor :actor/alice}
        after (oracle-poststate weakened before inputs)]
    (is (some? (transition/describe-transition! weakened)))
    (assert-observation! weakened before inputs after true)
    (is (false? (oracle-match? before inputs after)))
    (assert-observation! source before inputs after false)))

(deftest malformed-source-cannot-be-repaired-by-an-observed-witness
  (let [inputs {:input/actor :actor/alice}
        after (oracle-poststate source base inputs)
        invalid [(update source :reads disj :case/owner)
                 (update source :writes disj :case/mirror)
                 (assoc-in source [:assignments :case/mirror]
                           [:read :input/not-declared])
                 (assoc-in source [:assignments :case/status]
                           [:literal :unknown])
                 (assoc-in source [:assignments :case/mirror]
                           [:read :case/status])
                 (assoc-in source [:assignments :case/owner]
                           [:read :case/memo])]]
    (doseq [mutated invalid]
      (is (thrown? clojure.lang.ExceptionInfo
                   (transition/describe-transition! mutated)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (independent/observe-transition! mutated base inputs after))))))
