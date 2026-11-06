(ns gesso.assurance.proof.finite-preservation-rule-correspondence-test
  "v1434: enumerate finite PRE/INPUT states directly from the source guarded
   assignment declaration; check frame, literal, copy and conjunction schemas
   against separate concrete poststate and invariant predicates. Cross-check
   a small closed catalog with the independent finite-model oracle.

   These are finite representative checks and counterexamples, NOT a universal
   proof of S1/S4 soundness, admission, or physical XTDB correspondence."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.frame-soundness :as frame]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.field-copy-soundness :as copy]
            [gesso.assurance.proof.preservation-composition-soundness :as compose]
            [gesso.assurance.proof.finite-model-oracle :as finite]))

(def ^:private source
  {:subject {:kind :operation :id :case/claim
             :interface {:operation :case/claim :revision 1434}}
   :state-types
   {:case/status {:type :enum :name :case/status :values #{:open :claimed}}
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

(def ^:private invariants
  {:revision {:id :case/nonnegative-revision
              :readers #{:case/revision}
              :expression [:>= [:read :case/revision] [:literal 0]]}
   :memo-absent {:id :case/absent-memo
                 :readers #{:case/memo}
                 :expression [:not [:present [:read :case/memo]]]}
   :claimed {:id :case/claimed
             :readers #{:case/status}
             :expression [:= [:read :case/status] [:literal :claimed]]}
   :ack-null {:id :case/ack-null
              :readers #{:case/ack}
              :expression [:and [:present [:read :case/ack]]
                           [:= [:read :case/ack] [:literal nil]]]}
   :shared-owner {:id :case/shared-owner
                  :readers #{:case/owner :case/mirror}
                  :expression [:= [:read :case/owner] [:read :case/mirror]]}
   :combined {:id :case/combined
              :readers #{:case/revision :case/status :case/owner :case/mirror}
              :expression [:and [:>= [:read :case/revision] [:literal 0]]
                           [:= [:read :case/status] [:literal :claimed]]
                           [:= [:read :case/owner] [:read :case/mirror]]]} 
   :open-status {:id :case/still-open
                 :readers #{:case/status}
                 :expression [:= [:read :case/status] [:literal :open]]}})

(def ^:private absent ::absent)

(defn- cartesian-maps
  "Enumerate literal test fixture domains, omitting optional absent fields."
  [domains]
  (reduce (fn [partials [field values]]
            (for [m partials, value values]
              (if (= absent value) m (assoc m field value))))
          [{}] domains))

(def ^:private prestates
  (vec (cartesian-maps
        [[:case/status [:open :claimed]]
         [:case/owner [nil :actor/a :actor/b]]
         [:case/mirror [nil :actor/a :actor/b]]
         [:case/revision [-1 0 2]]
         [:case/flag [false true]]
         [:case/memo [absent nil "memo"]]
         [:case/ack [absent nil]]])))

(def ^:private inputs [{:input/actor :actor/a} {:input/actor :actor/b}])

(defn- guard? [before]
  (and (= :open (:case/status before))
       (contains? before :case/owner)
       (nil? (:case/owner before))))

(defn- apply-source
  "Snapshot ALL :read RHS before any assignment, retaining framed presence."
  [decl before input]
  (let [env (merge before input)
        writes (into {} (for [[field [kind term]] (:assignments decl)]
                          [field (case kind
                                   :literal term
                                   :read (get env term))]))]
    (merge before writes)))

(defn- truth?
  "Source-independent, fixture-specific Clojure predicates; in particular,
   a present nil is different from an absent optional field."
  [which state]
  (case which
    :revision (<= 0 (:case/revision state))
    :memo-absent (not (contains? state :case/memo))
    :claimed (= :claimed (:case/status state))
    :ack-null (and (contains? state :case/ack) (nil? (:case/ack state)))
    :shared-owner (= (:case/owner state) (:case/mirror state))
    :combined (and (<= 0 (:case/revision state))
                   (= :claimed (:case/status state))
                   (= (:case/owner state) (:case/mirror state)))
    :open-status (= :open (:case/status state))))

(defn- not-certified! [report]
  (is (= :none (:authority report)))
  (is (= :not-admitted (:admission report)))
  (is (= :not-issued (:certificate report)))
  (is (not= :proved (:proof-status report))))

(deftest enumerated-conditional-rules-are-sound-over-finite-relations
  (let [cases (for [before prestates, input inputs]
                {:before before :input input :enabled? (guard? before)
                 :after (apply-source source before input)})
        enabled (filter :enabled? cases)
        blocked (remove :enabled? cases)
        reports {:revision (frame/derive-frame-lemma! source (:revision invariants))
                 :memo-absent (frame/derive-frame-lemma! source (:memo-absent invariants))
                 :claimed (literal/derive-literal-lemma! source (:claimed invariants))
                 :ack-null (literal/derive-literal-lemma! source (:ack-null invariants))
                 :shared-owner (copy/derive-copy-lemma! source (:shared-owner invariants))
                 :combined (compose/derive-composed-lemma! source (:combined invariants))}]
    (is (= 648 (count prestates)))
    (is (= 1296 (count cases)))
    (is (= 216 (count enabled)))
    (is (= 1080 (count blocked)))
    (is (= [:conditional-frame-rule-instance :conditional-frame-rule-instance
            :conditional-literal-rule-instance :conditional-literal-rule-instance
            :conditional-same-origin-copy-rule :conditional-composed-rule-instance]
           (mapv #(get-in reports [% :status])
                 [:revision :memo-absent :claimed :ack-null
                  :shared-owner :combined])))
    (is (= [:frame :literal :copy]
           (mapv :supporting-family (:clauses (:combined reports)))))
    (doseq [[_ report] reports] (not-certified! report))
    (doseq [{:keys [before input after]} enabled]
      ;; Invariant preservation is tested as an implication, NOT as an
      ;; assertion that arbitrary prestate representatives satisfy I.
      (doseq [which [:revision :memo-absent :claimed :ack-null
                     :shared-owner :combined]]
        (let [pre? (truth? which before)
              post? (truth? which after)]
          (is (or (not pre?) post?)
              (str "false conditional preservation: " which " "
                   (pr-str [before input after])))))
      ;; Literal and same-origin copy schemas establish the POST-condition
      ;; even when their respective preconditions were false.
      (doseq [which [:claimed :ack-null :shared-owner]]
        (is (true? (truth? which after)) (str which " " (pr-str after))))
      (is (= (contains? before :case/memo)
             (contains? after :case/memo)))
      (is (= (:case/revision before) (:case/revision after)))
      (is (= (:case/flag before) (:case/flag after)))
      (is (nil? (:case/ack after))))
    ;; Spot-check executable observers on representatives spanning both
    ;; actors, optional absence/presence, and true/false revision invariant.
    (doseq [{:keys [before input after]} (take-nth 13 enabled)]
      (is (true? (:matches? (transition/observe-transition!
                             source before input after))))
      (is (true? (:matches? (independent/observe-transition!
                             source before input after)))))
    (doseq [{:keys [before input after]} (take-nth 101 blocked)]
      (is (false? (:matches? (independent/observe-transition!
                              source before input after)))))))

(deftest open-families-have-explicit-finite-counterexamples
  (let [changed-origin (assoc-in source [:assignments :case/mirror]
                                 [:read :case/owner])
        copy-open (copy/derive-copy-lemma!
                   changed-origin (:shared-owner invariants))
        combined-open (compose/derive-composed-lemma!
                       changed-origin (:combined invariants))
        status-open (literal/derive-literal-lemma!
                     source (:open-status invariants))
        frame-open (frame/derive-frame-lemma!
                    source (:open-status invariants))
        witnesses (for [before prestates, input inputs
                        :when (guard? before)
                        :let [after (apply-source changed-origin before input)]
                        :when (and (truth? :shared-owner before)
                                   (not (truth? :shared-owner after)))]
                    {:before before :input input :after after})]
    (is (= :open-no-shared-total-origin (:status copy-open)))
    (is (= :open-unproved-clause (:status combined-open)))
    (is (= :open-false-postcondition (:status status-open)))
    (is (= :open-written-reader (:status frame-open)))
    (doseq [r [copy-open combined-open status-open frame-open]]
      (is (nil? (:conditional-claim r)))
      (not-certified! r))
    (is (seq witnesses))
    ;; Every counterexample has a matching transition of the *mutated*
    ;; declaration, but disproves copy preservation over that declaration.
    (doseq [{:keys [before input after]} (take 12 witnesses)]
      (is (true? (truth? :shared-owner before)))
      (is (false? (truth? :shared-owner after)))
      (is (true? (:matches? (independent/observe-transition!
                             changed-origin before input after)))))
    (let [before (first (filter guard? prestates))
          after (apply-source source before (first inputs))]
      (is (truth? :open-status before))
      (is (false? (truth? :open-status after))))))

(defn- finite-catalog
  "Translate the small fixture to the existing separate, data-only v1309
   finite-model oracle contract. This does NOT feed a lemma into the oracle."
  [decl]
  (let [fields #{:case/status :case/owner :case/mirror}
        subset (-> decl
                   (update :state-types select-keys fields)
                   (update :writes #(set/intersection % fields))
                   (update :assignments select-keys fields))
        reads (set/union (:readers (:guard subset))
                                 (into #{} (keep (fn [[_ [kind term]]]
                                                  (when (= :read kind) term)))
                                       (:assignments subset)))]
    {:catalog
     {:id :case/finite-proof-regression
      :declared-operation-ids #{:case/claim}
      :initial-condition
      {:readers #{:case/status :case/owner :case/mirror}
       :expression [:and [:= [:read :case/status] [:literal :open]]
                    [:= [:read :case/owner] [:literal nil]]
                    [:= [:read :case/mirror] [:literal nil]]]}
      :invariants
      {:case/owner-agreement
       {:readers #{:case/owner :case/mirror}
        :expression [:= [:read :case/owner] [:read :case/mirror]]}}
      :operations
      [{:id :case/claim
        :construction
        {:subject {:kind :operation :id :case/claim
                   :interface {:operation :case/claim}}
         :state-types (:state-types subset)
         :input-types (:input-types subset)
         :guard (:guard subset)
         :reads reads
         :writes (:writes subset)
         :replacements (:assignments subset)
         :frame (set/difference fields (:writes subset))}}]}
     :state-domains
     {:case/status [:open :claimed]
      :case/owner [nil :actor/a :actor/b]
      :case/mirror [nil :actor/a :actor/b]}
     :input-domains {:case/claim {:input/actor [:actor/a :actor/b]}}
     :max-cases 100}))

(deftest finite-model-oracle-checks-a-separate-construction
  (let [good (finite/check-finite-model! (finite-catalog source))
        bad-source (assoc-in source [:assignments :case/mirror]
                             [:read :case/owner])
        bad (finite/check-finite-model! (finite-catalog bad-source))]
    (is (= :bounded-no-counterexample (:status good)) (pr-str good))
    (is (true? (:exhaustive? good)))
    (is (= 18 (get-in good [:universe :states])))
    (is (= 54 (get-in good [:universe :potential-cases])))
    (is (= :counterexample (:status bad)) (pr-str bad))
    (is (= :step (get-in bad [:counterexample :kind])) (pr-str bad))
    (is (= :case/owner-agreement (get-in bad [:counterexample :invariant-id])))
    (doseq [report [good bad]]
      (is (= :not-established (:checker-soundness report)))
      (is (= :not-established (:universal-induction report)))
      (is (= :not-established (:physical-correspondence report)))
      (not-certified! report))))
