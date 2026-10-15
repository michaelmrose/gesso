(ns gesso.model.formal-obligation-test
  "Permanent Recife/TLC checks for the five generic safety obligations over
   Gesso's source-derived prepared-transaction state machine.

   Subjects are constructed from ordinary Gesso transaction plans and stepped
   through formal-transition/transition -> tx-model/step. The obligations
   under test are stated independently in formal-obligation; the deliberately
   broken transitions below corrupt outputs AFTER the genuine Gesso step.

   Bounded scope: two selected, already-prepared plans, one attempt per plan,
   both serial orders, no actual XTDB, application policy, retry, network,
   browser effects, or general refinement theorem. The model checker must
   return real TLC results; a direct predicate check is never called a proof."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]
   [gesso.model.formal-obligation :as obligation]
   [gesso.model.formal-transition :as formal]
   [gesso.model.tx-model :as reference]
   [recife.core :as recife]
   [recife.helpers :as helpers]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- document [id]
  {:xt/id id
   :revision 0
   :created-at :time-zero
   :updated-at :time-zero
   :value :initial})

(defn- updated [before value]
  (-> before
      (assoc :value value :updated-at :time-one)
      (update :revision inc)))

(defn- update-plan [before value]
  {:commands [(command/update-command
               :items :replace before (updated before value) version)]
   :guards []
   :emit false})

(defn- create-plan [id value]
  {:commands [(command/create
               :items (assoc (document id) :value value) version)]
   :guards []
   :emit false})

(def ^:private left (document :left))
(def ^:private right (document :right))
(def ^:private left-target [:items :left])
(def ^:private right-target [:items :right])

(def ^:private cases
  {:competing-updates
   {:subject (formal/prepared-subject
              {left-target left}
              [(update-plan left :first) (update-plan left :second)])
    :statuses [:committed :rejected]}

   :independent-updates
   {:subject (formal/prepared-subject
              {left-target left right-target right}
              [(update-plan left :first) (update-plan right :second)])
    :statuses [:committed :committed]}

   :competing-creates
   {:subject (formal/prepared-subject
              {}
              [(create-plan :new :first) (create-plan :new :second)])
    :statuses [:committed :rejected]}})

(def ^:private mutation-obligation
  {:bad-accounting :action-accounting
   :bad-chain :observation-chain
   :bad-rejection :rejected-is-atomic
   :bad-commit :committed-write-set
   :bad-outcome :outcome-check-consistency})

(defn- initial-state [case-id mutation]
  (assoc (formal/initial-state (get-in cases [case-id :subject]))
         ::case case-id
         ::mutation mutation))

(defn- corrupt [state index]
  (let [observation (peek (::formal/observations state))
        status (:status observation)
        mutation (::mutation state)
        last-index (dec (count (::formal/observations state)))]
    (case mutation
      :bad-accounting
      ;; Reinsert an already attempted plan into the pending set.
      (update state ::formal/pending conj index)

      :bad-chain
      ;; Falsify both sides of the second (rejected) observation so its
      ;; local rejection remains atomic, but its link to the actual preceding
      ;; authoritative state and terminal documents is broken.
      (if (= 1 last-index)
        (-> state
            (assoc-in [::formal/observations last-index :before] {})
            (assoc-in [::formal/observations last-index :after] {}))
        state)

      :bad-rejection
      ;; Make a rejection perform an unauthorized document write.
      (if (= :rejected status)
        (-> state
            (assoc-in [::formal/documents left-target :value] :illicit)
            (assoc-in [::formal/observations last-index :after left-target :value]
                      :illicit))
        state)

      :bad-commit
      ;; Keep the observation chain intact while breaking the declared write set.
      (if (= :committed status)
        (let [target left-target]
          (-> state
              (assoc-in [::formal/documents target :value] :illicit)
              (assoc-in [::formal/observations last-index :after target :value]
                        :illicit)))
        state)

      :bad-outcome
      ;; A committed outcome cannot coexist with a failed precondition.
      (if (and (= :committed status) (seq (:checks observation)))
        (assoc-in state
                  [::formal/observations last-index :checks 0 :satisfied?]
                  false)
        state)

      ;; The good machine is always the unchanged Gesso transition.
      :correct state)))

(defn- attempt [state index]
  (let [subject (get-in cases [(::case state) :subject])
        action (nth (:actions subject) index)]
    (if (some #{action} (formal/enabled-actions subject state))
      (corrupt (formal/transition subject state action) index)
      state)))

(recife/defproc attempt-first (fn [state] (attempt state 0)))
(recife/defproc attempt-second (fn [state] (attempt state 1)))

;; TLC evaluates ALL five independent predicates at every reachable state.
;; Each invariant selects the admitted subject encoded by this model's case ID.
(helpers/definvariant action-accounting [state]
  (obligation/holds? (get-in cases [(::case state) :subject])
                     :action-accounting state))
(helpers/definvariant observation-chain [state]
  (obligation/holds? (get-in cases [(::case state) :subject])
                     :observation-chain state))
(helpers/definvariant rejected-is-atomic [state]
  (obligation/holds? (get-in cases [(::case state) :subject])
                     :rejected-is-atomic state))
(helpers/definvariant committed-write-set [state]
  (obligation/holds? (get-in cases [(::case state) :subject])
                     :committed-write-set state))
(helpers/definvariant outcome-check-consistency [state]
  (obligation/holds? (get-in cases [(::case state) :subject])
                     :outcome-check-consistency state))

(def ^:private components
  #{attempt-first attempt-second
    action-accounting observation-chain rejected-is-atomic
    committed-write-set outcome-check-consistency})

(defn- check! [case-id mutation]
  ;; Terminal states are intentional. This is safety checking, not liveness.
  (let [result (recife/run-model
                (initial-state case-id mutation)
                components
                {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(defn- execution [subject order]
  (reductions
   (fn [state index]
     (formal/transition subject state (nth (:actions subject) index)))
   (formal/initial-state subject)
   order))

(deftest admitted-gesso-subjects-expose-different-outcomes-test
  (testing "three real generic plan constructions explore both serial orders"
    (is (= 5 (count obligation/obligation-ids)))
    (doseq [[case-id {:keys [subject statuses]}] cases]
      (let [paths (reference/explore-orders
                   (::formal/documents (formal/initial-state subject))
                   (:plans subject))]
        (is (= 2 (count (:paths paths))) (name case-id))
        (doseq [{:keys [order trace final-state]} (:paths paths)]
          (let [states (vec (execution subject order))
                final (peek states)]
            (is (= statuses (mapv :status trace)) (name case-id))
            (is (= final-state (::formal/documents final)) (name case-id))
            (is (= [] (obligation/violated-obligations subject final)))
            (is (every? #(empty? (obligation/violated-obligations subject %))
                        states))))))))

(deftest tlc-checks-all-five-obligations-for-multiple-subjects-test
  (doseq [case-id [:competing-updates :independent-updates :competing-creates]]
    (testing (str "Recife/TLC checks every reachable state of " (name case-id))
      (let [{:keys [trace trace-info distinct-states] :as result}
            (check! case-id :correct)]
        (is (= :ok trace) (pr-str (select-keys result [:trace :trace-info])))
        (is (nil? trace-info))
        (is (= 5 distinct-states) (pr-str result))))))

(deftest each-obligation-detects-an-intentional-tlc-counterexample-test
  (let [case-id :competing-updates
        subject (get-in cases [case-id :subject])]
    (doseq [[mutation expected-obligation] mutation-obligation]
      (testing (name mutation)
        ;; First confirm that the intended independent predicate detects the
        ;; mutant, so a failure of an unrelated invariant cannot mask a
        ;; test that never exercised the requested obligation.
        (let [first-state (attempt (initial-state case-id mutation) 0)
              witness (if (obligation/holds? subject expected-obligation first-state)
                        (attempt first-state 1)
                        first-state)]
          (is (false? (obligation/holds? subject expected-obligation witness))
              (str mutation " failed to exercise " expected-obligation)))
        (let [{:keys [trace trace-info] :as result} (check! case-id mutation)
              violation (:violation trace-info)]
          (is (and (vector? trace) (seq trace)) (pr-str result))
          (is (= :invariant (:type violation)) (pr-str trace-info))
          ;; :committed-write-set intentionally depends on action accounting,
          ;; so that mutant may be reported under either valid invariant.
          (is (contains?
               (if (= mutation :bad-accounting)
                 #{:action-accounting :committed-write-set}
                 #{expected-obligation})
               (some-> (:name violation) name keyword))
              (pr-str trace-info)))))))

(deftest malformed-obligation-input-fails-closed-test
  (let [subject (get-in cases [:competing-updates :subject])
        state (formal/initial-state subject)]
    (is (false? (obligation/holds? subject :action-accounting {})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Unknown Gesso formal obligation"
         (obligation/holds? subject :not-an-obligation state)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"Expected an admitted"
         (obligation/holds? (dissoc subject :kind) :action-accounting state)))))
