(ns gesso.assurance.proof.guarded-preservation-semantics-test
  "v1432: test-local finite semantics for guarded replacement and the
   restricted frame/literal/copy preservation schemas. The poststate generator
   and expected truth values below do not use production transition observers,
   semantic summaries, proof lemmas, or their independent JVM interpreters.

   These are bounded diagnostic correspondences, not a soundness theorem,
   physical XTDB correspondence, or authority to admit an application."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.frame-soundness :as frame]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.field-copy-soundness :as copy]
            [gesso.assurance.proof.preservation-composition-soundness :as compose]
            [gesso.assurance.proof.typed-presence-semantics :as presence]))

(def ^:private declaration
  {:subject {:kind :operation :id :test/claim
             :interface {:operation :test/claim :revision 1432}}
   :state-types {:case/status {:type :enum :name :case/status
                               :values #{:open :claimed}}
                 :case/owner {:type :keyword :nullable? true}
                 :case/mirror {:type :keyword :nullable? true}
                 :case/revision {:type :integer}
                 :case/flag {:type :boolean}
                 :case/memo {:type :string :optional? true :nullable? true}
                 :case/ack {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:case/status :case/owner}
           :expression [:and [:= [:read :case/status] [:literal :open]]
                        [:= [:read :case/owner] [:literal nil]]]}
   :reads #{:case/status :case/owner :input/actor}
   :writes #{:case/status :case/owner :case/mirror :case/ack}
   :assignments {:case/status [:literal :claimed]
                 :case/owner [:read :input/actor]
                 :case/mirror [:read :input/actor]
                 :case/ack [:literal nil]}})

(def ^:private base
  {:case/status :open :case/owner nil :case/mirror :actor/old
   :case/revision 4 :case/flag false})

(def ^:private frame-invariant
  {:id :test/frame-revision :readers #{:case/revision}
   :expression [:>= [:read :case/revision] [:literal 0]]})
(def ^:private optional-frame-invariant
  {:id :test/frame-memo-presence :readers #{:case/memo}
   :expression [:not [:present [:read :case/memo]]]})
(def ^:private literal-invariant
  {:id :test/literal-status :readers #{:case/status}
   :expression [:= [:read :case/status] [:literal :claimed]]})
(def ^:private optional-literal-invariant
  {:id :test/literal-null-present :readers #{:case/ack}
   :expression [:and [:present [:read :case/ack]]
                [:= [:read :case/ack] [:literal nil]]]})
(def ^:private copy-invariant
  {:id :test/shared-origin :readers #{:case/owner :case/mirror}
   :expression [:= [:read :case/owner] [:read :case/mirror]]})
(def ^:private combined-invariant
  {:id :test/combined :readers #{:case/revision :case/status
                                :case/owner :case/mirror}
   :expression [:and [:>= [:read :case/revision] [:literal 0]]
                [:= [:read :case/status] [:literal :claimed]]
                [:= [:read :case/owner] [:read :case/mirror]]]})
(def ^:private unsupported-invariant
  {:id :test/unsupported-nonnull-owner :readers #{:case/owner}
   :expression [:not= [:read :case/owner] [:literal nil]]})

(defn- guard-from-prestate?
  "A separate, intentionally concrete interpretation of the source guard."
  [before]
  (and (= :open (:case/status before))
       (contains? before :case/owner)
       (nil? (:case/owner before))))

(defn- candidate-poststate
  "Interpret only :read and :literal RHS from ORIGINAL source: every read
   consults the same prestate plus inputs. A false guard does NOT authorize
   this candidate as an actual transition."
  [source before inputs]
  (let [environment (merge before inputs)]
    (reduce-kv (fn [post field [operator operand]]
                 (assoc post field (case operator
                                     :literal operand
                                     :read (get environment operand))))
               before (:assignments source))))

(defn- expected-claim-post?
  [before input after]
  (and (guard-from-prestate? before)
       (= :claimed (:case/status after))
       (= (:input/actor input) (:case/owner after) (:case/mirror after))
       (contains? after :case/ack)
       (nil? (:case/ack after))
       (every? (fn [field]
                 (and (= (contains? before field) (contains? after field))
                      (= (get before field) (get after field))))
               [:case/revision :case/flag :case/memo])))

(defn- manual-truth
  "Fixture-specific invariants, calculated without Gesso's evaluator."
  [id state]
  (case id
    :test/frame-revision (<= 0 (:case/revision state))
    :test/frame-memo-presence (not (contains? state :case/memo))
    :test/literal-status (= :claimed (:case/status state))
    :test/literal-null-present (and (contains? state :case/ack)
                                    (nil? (:case/ack state)))
    :test/shared-origin (= (:case/owner state) (:case/mirror state))
    :test/combined (and (<= 0 (:case/revision state))
                        (= :claimed (:case/status state))
                        (= (:case/owner state) (:case/mirror state)))
    :test/unsupported-nonnull-owner (some? (:case/owner state))))

(defn- actual-invariant-value [invariant state]
  (:value (presence/evaluate-expression!
           (:expression invariant)
           (select-keys (:state-types declaration) (:readers invariant))
           (select-keys state (:readers invariant)))))

(defn- no-authority! [report]
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not= :proved (:proof-status report))))

(deftest source-relative-conditional-families
  (testing "proof families must justify supported source, not observed luck"
    (let [f (frame/derive-frame-lemma! declaration frame-invariant)
          opt (frame/derive-frame-lemma! declaration optional-frame-invariant)
          l (literal/derive-literal-lemma! declaration literal-invariant)
          nil-l (literal/derive-literal-lemma! declaration optional-literal-invariant)
          c (copy/derive-copy-lemma! declaration copy-invariant)
          composed (compose/derive-composed-lemma! declaration combined-invariant)
          open (compose/derive-composed-lemma! declaration unsupported-invariant)]
      (is (= :conditional-frame-rule-instance (:status f)))
      (is (= :conditional-frame-rule-instance (:status opt)))
      (is (= :conditional-literal-rule-instance (:status l)))
      (is (= :conditional-literal-rule-instance (:status nil-l)))
      (is (= :conditional-same-origin-copy-rule (:status c)))
      (is (= :conditional-composed-rule-instance (:status composed)))
      (is (= [:frame :literal :copy]
             (mapv :supporting-family (:clauses composed))))
      (is (= :open-unproved-clause (:status open)))
      (is (nil? (:conditional-claim open)))
      (doseq [r [f opt l nil-l c composed open]] (no-authority! r)))))

(deftest bounded-prestate-transformer-and-preservation
  (testing "finite poststates calculated without the production transition machine"
    (doseq [actor [:actor/a :actor/b]
            revision [-1 0 4]
            flag [false true]
            memo [::absent nil "hello"]]
      (let [before (cond-> (assoc base :case/revision revision :case/flag flag)
                     (not= ::absent memo) (assoc :case/memo memo))
            input {:input/actor actor}
            after (candidate-poststate declaration before input)
            prod (transition/observe-transition! declaration before input after)
            ind (independent/observe-transition! declaration before input after)]
        (is (expected-claim-post? before input after))
        (is (true? (:matches? prod)))
        (is (true? (:matches? ind)))
        (is (= (select-keys prod [:guard-holds? :assignments-hold?
                                  :frame-holds? :matches?])
               (select-keys ind [:guard-holds? :assignments-hold?
                                 :frame-holds? :matches?])))
        (doseq [invariant [frame-invariant optional-frame-invariant
                           literal-invariant optional-literal-invariant
                           copy-invariant combined-invariant
                           unsupported-invariant]]
          (let [id (:id invariant)
                b (manual-truth id before)
                a (manual-truth id after)]
            (is (= b (actual-invariant-value invariant before)))
            (is (= a (actual-invariant-value invariant after)))
            ;; Supported families: the relation preserves a true invariant,
            ;; or establishes a postcondition even when its prestate is false.
            (when-not (= id :test/unsupported-nonnull-owner)
              (is (or (not b) a)))))))))

(deftest conditional-witnesses-use-matching-relation
  (let [before (assoc base :case/memo nil)
        input {:input/actor :actor/a}
        after (candidate-poststate declaration before input)
        witnesses [(frame/audit-frame-witness! declaration frame-invariant before input after)
                   (frame/audit-frame-witness! declaration optional-frame-invariant before input after)
                   (literal/audit-literal-witness! declaration literal-invariant before input after)
                   (literal/audit-literal-witness! declaration optional-literal-invariant before input after)
                   (copy/audit-copy-witness! declaration copy-invariant before input after)
                   (compose/audit-composed-witness! declaration combined-invariant before input after)]]
    (doseq [w witnesses]
      (is (true? (:matches-transition? w)))
      (is (= (:after-invariant w)
             (manual-truth (:invariant-id w) after)))
      (is (true? (:preservation-implication-holds? w)))
      (no-authority! w))
    (is (false? (:before-invariant (nth witnesses 1))))
    (is (true? (:before-invariant (first witnesses))))))

(deftest false-guard-does-not-license-preservation
  (doseq [before [(assoc base :case/status :claimed)
                  (assoc base :case/owner :actor/old)]]
    (let [input {:input/actor :actor/a}
          after (candidate-poststate declaration before input)
          obs (independent/observe-transition! declaration before input after)
          witness (compose/audit-composed-witness!
                    declaration combined-invariant before input after)]
      (is (false? (guard-from-prestate? before)))
      (is (false? (expected-claim-post? before input after)))
      (is (false? (:guard-holds? obs)))
      (is (false? (:matches? obs)))
      (is (= :nonmatching-transition (:status witness)))
      (is (false? (:matches-transition? witness)))
      (no-authority! witness))))

(deftest corrupted-assignments-and-frames-do-not-preserve-a-matching-step
  (let [input {:input/actor :actor/b}
        after (candidate-poststate declaration base input)
        broken [(assoc after :case/owner :actor/other)
                (assoc after :case/mirror nil)
                (dissoc after :case/ack)
                (assoc after :case/status :open)
                (assoc after :case/revision 17)
                (assoc after :case/flag true)
                (assoc after :case/memo nil)]]
    (doseq [candidate broken]
      (let [actual (transition/observe-transition! declaration base input candidate)
            independent (independent/observe-transition! declaration base input candidate)]
        (is (false? (expected-claim-post? base input candidate)))
        (is (false? (:matches? actual)))
        (is (false? (:matches? independent)))
        (is (= (:matches? actual) (:matches? independent)))))))

(deftest coincidental-equality-cannot-substitute-for-common-copy-origin
  (let [swapped (-> declaration
                    (assoc :input-types {})
                    (assoc :reads #{:case/status :case/owner :case/mirror})
                    (assoc :writes #{:case/status :case/owner :case/mirror :case/ack})
                    (assoc :assignments {:case/status [:literal :claimed]
                                         :case/owner [:read :case/mirror]
                                         :case/mirror [:read :case/owner]
                                         :case/ack [:literal nil]}))
        before (assoc base :case/mirror nil)
        after (candidate-poststate swapped before {})
        observation (independent/observe-transition! swapped before {} after)
        lemma (copy/derive-copy-lemma! swapped copy-invariant)]
    (is (true? (:matches? observation)))
    (is (= nil (:case/owner after) (:case/mirror after)))
    (is (= :open-no-shared-total-origin (:status lemma)))
    (is (nil? (:conditional-claim lemma)))
    (no-authority! lemma)
    (let [different-before (assoc base :case/mirror :actor/old)
          different-after (candidate-poststate swapped different-before {})]
      (is (true? (:matches? (independent/observe-transition!
                            swapped different-before {} different-after))))
      (is (not= (:case/owner different-after) (:case/mirror different-after))))))

(deftest source-derived-reader-and-writer-closure
  (doseq [bad [(assoc declaration :reads #{:case/status :input/actor})
               (assoc declaration :writes #{:case/status :case/owner :case/mirror})
               (assoc-in declaration [:assignments :case/mirror] [:read :case/revision])
               (assoc-in declaration [:assignments :case/ack] [:literal false])]]
    (is (try (transition/describe-transition! bad) false
             (catch clojure.lang.ExceptionInfo _ true)))
    (is (try (independent/observe-transition!
              bad base {:input/actor :actor/a}
              (candidate-poststate declaration base {:input/actor :actor/a}))
             false (catch clojure.lang.ExceptionInfo _ true)))))
