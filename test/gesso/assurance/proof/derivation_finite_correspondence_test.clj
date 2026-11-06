(ns gesso.assurance.proof.derivation-finite-correspondence-test
  "v1435: check closed v1235 derivations against an independently interpreted,
   finite collection of original guarded-transition triples.  The test-local
   evaluator does not reuse Gesso's proof interpreter or its proof conclusions
   to compute a transition or a fact.  Counterexamples must defeat false
   accepted claims, not merely change a report fingerprint.

   This is bounded abstract semantic evidence.  It does not establish a
   universal inference-rule soundness theorem, certification, or XTDB
   execution correspondence."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as transition]))

(def ^:private source
  {:subject {:kind :operation :id :case/claim
             :interface {:operation :case/claim :revision 1435}}
   :state-types
   {:case/status {:type :enum :name :case/status :values #{:open :claimed}}
    :case/owner {:type :keyword :nullable? true}
    :case/mirror {:type :keyword :nullable? true}
    :case/revision {:type :integer}
    :case/flag {:type :boolean}
    :case/note {:type :string :optional? true :nullable? true}
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

(def ^:private guard-claim [:guard])
(def ^:private status-claim [:eq [:post :case/status] [:literal :claimed]])
(def ^:private owner-claim [:eq [:post :case/owner] [:input :input/actor]])
(def ^:private mirror-claim [:eq [:post :case/mirror] [:input :input/actor]])
(def ^:private revision-claim [:eq [:post :case/revision] [:pre :case/revision]])
(def ^:private note-claim [:same-field :case/note])
(def ^:private ack-claim [:eq [:post :case/ack] [:literal nil]])
(def ^:private linked-claim [:eq [:post :case/mirror] [:post :case/owner]])
(def ^:private all-claim
  [:and guard-claim status-claim owner-claim mirror-claim
   revision-claim note-claim ack-claim])
(def ^:private goal [:and all-claim linked-claim])

(def ^:private steps
  [{:id :step/guard :rule :guard :conclusion guard-claim}
   {:id :step/status :rule :assignment :field :case/status
    :conclusion status-claim}
   {:id :step/owner :rule :assignment :field :case/owner
    :conclusion owner-claim}
   {:id :step/mirror :rule :assignment :field :case/mirror
    :conclusion mirror-claim}
   {:id :step/revision :rule :frame :field :case/revision
    :conclusion revision-claim}
   {:id :step/note :rule :frame :field :case/note
    :conclusion note-claim}
   {:id :step/ack :rule :assignment :field :case/ack
    :conclusion ack-claim}
   {:id :step/all :rule :conjunction-introduction
    :from [:step/guard :step/status :step/owner :step/mirror
           :step/revision :step/note :step/ack]
    :conclusion all-claim}
   {:id :step/pick :rule :conjunction-elimination
    :from [:step/all] :index 3 :conclusion mirror-claim}
   {:id :step/linked :rule :substitution
    :from [:step/owner :step/pick] :side :right :direction :reverse
    :conclusion linked-claim}
   {:id :step/final :rule :conjunction-introduction
    :from [:step/all :step/linked] :conclusion goal}])

(def ^:private proof-source {:transition source :steps steps :goal goal})
(def ^:private absent ::absent)

(defn- all-states []
  (for [status [:open :claimed]
        owner [nil :actor/a :actor/b]
        mirror [nil :actor/a :actor/b]
        revision [-1 0 7]
        flag [false true]
        note [absent nil "notes"]
        ack [absent nil]]
    (cond-> {:case/status status
             :case/owner owner
             :case/mirror mirror
             :case/revision revision
             :case/flag flag}
      (not= absent note) (assoc :case/note note)
      (not= absent ack) (assoc :case/ack ack))))

(def ^:private states (vec (all-states)))
(def ^:private inputs [{:input/actor :actor/a} {:input/actor :actor/b}])

;; Test-local specification: no production interpreter is used to compute
;; expected poststates or the meanings of any proof-claim forms.
(defn- enabled? [before]
  (and (= :open (:case/status before))
       (nil? (:case/owner before))))

(defn- apply-claim [before input]
  (-> before
      (assoc :case/status :claimed
             :case/owner (:input/actor input)
             :case/mirror (:input/actor input)
             :case/ack nil)))

(defn- unchanged? [before after field]
  (and (= (contains? before field) (contains? after field))
       (or (not (contains? before field))
           (= (get before field) (get after field)))))

(defn- matching? [before input after]
  (and (enabled? before)
       (= after (apply-claim before input))))

(defn- term-value [before input after [kind value]]
  (case kind
    :pre (get before value)
    :input (get input value)
    :post (get after value)
    :literal value
    (throw (ex-info "Unsupported local claim term" {:kind kind}))))

(defn- claim-holds? [before input after claim]
  (case (first claim)
    :guard (enabled? before)
    :same-field (unchanged? before after (second claim))
    :eq (= (term-value before input after (second claim))
           (term-value before input after (nth claim 2)))
    :and (every? #(claim-holds? before input after %) (rest claim))
    (throw (ex-info "Unsupported local claim" {:claim claim}))))

(def ^:private matched
  (vec (for [before states, input inputs :when (enabled? before)]
         [before input (apply-claim before input)])))

(defn- require-conditional! [report source*]
  (is (= derivation/report-type (:type report)))
  (is (= (:goal source*) (:goal report)))
  (is (= (count (:steps source*)) (:rule-count report)))
  (is (= (:fingerprint (transition/describe-transition!
                        (:transition source*)))
         (:transition-fingerprint report)))
  (is (= #{:gesso.assurance.proof.derivation/abstract-relation-match}
         (:open-premises report)))
  (is (= :conditional-abstract-inference (:proof-status report)))
  (is (= :conditional-on-matching-abstract-relation
         (:conclusion-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report))))

(defn- rejection [spec]
  (try (derivation/check-transition-derivation! spec)
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest accepted-closed-derivation-is-true-of-every-finite-matching-step
  (let [report (derivation/check-transition-derivation! proof-source)]
    (require-conditional! report proof-source)
    (is (= 648 (count states)))
    (is (= 216 (count matched)))
    (is (= 11 (count steps)))
    (doseq [[before input after] matched]
      (is (matching? before input after))
      (is (true? (:matches? (independent/observe-transition!
                            source before input after))))
      ;; Each intermediate conclusion must hold on the same triple; proving
      ;; only the final goal might conceal an invalid intermediate step.
      (doseq [{:keys [id conclusion]} steps]
        (is (true? (claim-holds? before input after conclusion))
            (str "false checked claim " id " " (pr-str [before input after])))))))

(deftest nonmatching-steps-do-not-discharge-the-abstract-relation-premise
  (let [[before input after] (first matched)
        bad-status (assoc after :case/status :open)
        bad-owner (assoc after :case/owner :actor/b)
        bad-frame (update after :case/flag not)
        bad-presence (assoc after :case/note nil)
        forbidden (assoc before :case/status :claimed)
        forbidden-post (apply-claim forbidden input)]
    (doseq [[label pre candidate] [[:status before bad-status]
                                   [:owner before bad-owner]
                                   [:frame before bad-frame]
                                   [:presence before bad-presence]
                                   [:guard forbidden forbidden-post]]]
      (testing (str label)
        (is (false? (matching? pre input candidate)))
        (is (false? (:matches? (independent/observe-transition!
                               source pre input candidate))))))
    (is (= #{:gesso.assurance.proof.derivation/abstract-relation-match}
           (:open-premises (derivation/check-transition-derivation!
                            proof-source))))))

(deftest a-false-conclusion-has-a-real-finite-counterexample
  (let [false-claim [:eq [:post :case/status] [:literal :open]]
        bad-proof (-> proof-source
                      (assoc :steps [{:id :step/bad :rule :assignment
                                      :field :case/status :conclusion false-claim}]
                             :goal false-claim))
        counterexamples (filter (fn [[before input after]]
                                  (not (claim-holds? before input after false-claim)))
                                matched)]
    (is (= :wrong-assignment-conclusion (:reason (rejection bad-proof))))
    (is (= (count matched) (count counterexamples)))
    (let [[before input after] (first counterexamples)]
      (is (matching? before input after))
      (is (false? (claim-holds? before input after false-claim)))
      ;; Even if an altered checker blindly accepted the bad derivation,
      ;; the independent finite oracle would still reject its claimed goal.
      (let [mutant-checker (fn [_]
                             (assoc (derivation/check-transition-derivation!
                                     proof-source)
                                    :goal false-claim))
            alleged (mutant-checker bad-proof)]
        (is (= false-claim (:goal alleged)))
        (is (false? (every? (fn [[pre inp post]]
                             (claim-holds? pre inp post (:goal alleged)))
                           matched)))))))

(deftest changing-a-premise-or-proof-edge-must-not-be-accepted
  (let [mutations
        [[:wrong-frame (assoc-in proof-source [:steps 4 :field] :case/status)
          :not-a-frame-field]
         [:forged-write (assoc-in proof-source [:steps 3 :conclusion]
                                  [:eq [:post :case/mirror] [:pre :case/owner]])
          :wrong-assignment-conclusion]
         [:wrong-elimination (assoc-in proof-source [:steps 8 :index] 2)
          :wrong-conjunction-elimination]
         [:wrong-substitution (assoc-in proof-source [:steps 9 :direction]
                                        :forward)
          :substitution-source-mismatch]
         [:missing-premise (assoc-in proof-source [:steps 7 :from]
                                      [:step/guard :step/status :step/owner
                                       :step/mirror :step/revision :step/ack])
          :wrong-conjunction-introduction]
         [:wrong-final (assoc proof-source :goal linked-claim)
          :wrong-final-conclusion]]]
    (doseq [[label mutant expected] mutations]
      (testing (str label)
        (let [err (rejection mutant)]
          (is (= :gesso.assurance.proof.derivation/invalid-derivation
                 (:error/type err)) (pr-str err))
          (is (= expected (:reason err)) (pr-str err)))))))

(deftest valid-alternative-source-is-not-evidence-for-the-original
  (let [new-source (assoc-in source [:assignments :case/ack]
                             [:literal "receipt"])
        new-ack [:eq [:post :case/ack] [:literal "receipt"]]
        new-all (assoc all-claim 7 new-ack)
        new-goal [:and new-all linked-claim]
        alt-proof (-> proof-source
                      (assoc :transition new-source :goal new-goal)
                      (assoc-in [:steps 6 :conclusion] new-ack)
                      (assoc-in [:steps 7 :conclusion] new-all)
                      (assoc-in [:steps 10 :conclusion] new-goal))
        original (derivation/check-transition-derivation! proof-source)
        alternative (derivation/check-transition-derivation! alt-proof)]
    (require-conditional! alternative alt-proof)
    (is (not= (:transition-fingerprint original)
              (:transition-fingerprint alternative)))
    (is (not= (:fingerprint original) (:fingerprint alternative)))
    (doseq [[before input after] (take 30 matched)]
      (is (true? (claim-holds? before input after goal)))
      (is (false? (claim-holds? before input after new-goal))))
    (is (= :wrong-assignment-conclusion
           (:reason (rejection (assoc alt-proof :transition source)))))))
