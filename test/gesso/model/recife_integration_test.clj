(ns gesso.model.recife-integration-test
  "Finite Recife/TLC regression against Gesso's admitted prepared-transaction
  machine, gesso.model.formal-transition. Plans are normalized by tx-model;
  the formal transition function delegates every enabled action to
  tx-model/step. This test does not hand-implement commit/rejection semantics.

  The intentionally defective variant corrupts the result *after* the real
  transition, to prove that independent TLC invariants detect an invalid
  rejection. Checked bounds: one document, two prepared competing updates,
  one attempt per plan, all serial orderings. No XTDB, replay, network,
  authorization, or unbounded refinement claim follows from this test."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]
   [gesso.model.formal-transition :as formal]
   [gesso.model.tx-model :as reference]
   [recife.core :as recife]
   [recife.helpers :as helpers]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private initial-document
  {:xt/id :unit-1
   :revision 0
   :created-at :t0
   :updated-at :t0
   :value :initial})

(def ^:private target [:unit :unit-1])
(def ^:private initial-documents {target initial-document})

(defn- prepared-plan [value]
  {:commands
   [(command/update-command
     :unit :replace initial-document
     (assoc initial-document
            :value value
            :revision 1
            :updated-at :t1)
     version)]
   :guards []
   :emit false})

(def ^:private plans
  [(prepared-plan :first)
   (prepared-plan :second)])

;; The formal state and action vocabulary are constructed from the Gesso
;; reference, rather than independently maintained in this Recife test.
(def ^:private subject
  (formal/prepared-subject initial-documents plans))

(defn- initial-state [mode]
  (assoc (formal/initial-state subject) ::mode mode))

(defn- attempt [state index]
  (let [action (nth (:actions subject) index)]
    (if-not (some #(= action %) (formal/enabled-actions subject state))
      state
      (let [next-state (formal/transition subject state action)
            last-observation (peek (::formal/observations next-state))]
        (if (and (= ::corrupt-rejection (::mode state))
                 (= :rejected (:status last-observation)))
          ;; Deliberate *post-transition* defect. Do not replace Gesso's
          ;; interpreter or its admitted plan. Corrupt both document state
          ;; and its observation to model an illegal rejected write.
          (-> next-state
              (assoc-in [::formal/documents target :value] :corrupted)
              (update ::formal/observations
                      (fn [observations]
                        (update observations (dec (count observations))
                                update :after assoc-in [target :value] :corrupted))))
          next-state)))))

(recife/defproc attempt-first (fn [state] (attempt state 0)))
(recife/defproc attempt-second (fn [state] (attempt state 1)))

;; These are independently stated properties of the *observed* machine,
;; not a replay of the transition function or an assertion that :scope
;; carries proof evidence.
(helpers/definvariant rejected-preserves-documents [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= :rejected status)
                (= before after)))
          (::formal/observations state)))

(helpers/definvariant only-one-competing-update-commits [state]
  (<= (count (filter #(= :committed (:status %))
                     (::formal/observations state)))
      1))

(helpers/definvariant documents-have-admitted-values [state]
  (contains? #{:initial :first :second}
             (get-in state [::formal/documents target :value])))

(helpers/definvariant observations-form-one-state-chain [state]
  (let [observations (::formal/observations state)]
    (and
     (or (empty? observations)
         (= initial-documents (:before (first observations))))
     (every? (fn [[earlier later]]
               (= (:after earlier) (:before later)))
             (partition 2 1 observations))
     (= (::formal/documents state)
        (if (seq observations)
          (:after (peek observations))
          initial-documents)))))

(def ^:private components
  #{attempt-first attempt-second
    rejected-preserves-documents
    only-one-competing-update-commits
    documents-have-admitted-values
    observations-form-one-state-chain})

(defn- check! [mode]
  ;; The two-attempt terminal states are intentional. This checks safety,
  ;; not progress, fairness, or application liveness.
  (let [result (recife/run-model (initial-state mode) components
                                 {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(defn- apply-order [actions]
  (reduce (fn [state action]
            (formal/transition subject state action))
          (formal/initial-state subject)
          actions))

(deftest prepared-transition-is-the-model-checked-subject-test
  (testing "both executions use exactly the admitted Gesso action vocabulary"
    (is (= :gesso.model.formal-transition/prepared-serial-v1 (:kind subject)))
    (is (= 2 (count (:actions subject))))
    (is (= (:actions subject)
           (formal/enabled-actions subject (formal/initial-state subject))))
    (let [explored (reference/explore-orders initial-documents plans)]
      (is (= 2 (count (:paths explored))))
      (doseq [{:keys [order trace final-state]} (:paths explored)]
        (let [machine (apply-order (mapv (:actions subject) order))
              observations (::formal/observations machine)]
          (is (= final-state (::formal/documents machine)))
          (is (= (mapv :status trace) (mapv :status observations)))
          (is (= (mapv :failed-precondition trace)
                 (mapv :failed-precondition observations)))
          (is (= [:committed :rejected] (mapv :status observations)))
          (is (empty? (formal/enabled-actions subject machine))))))))

(deftest derived-gesso-machine-passes-recife-invariants-test
  (testing "TLC explores both serial interleavings of formal/transition"
    (let [{:keys [trace trace-info distinct-states generated-states] :as result}
          (check! ::correct)]
      (is (= :ok trace)
          (pr-str (select-keys result [:trace :trace-info :distinct-states])))
      (is (nil? trace-info))
      (is (= 5 distinct-states)
          "One root, two one-attempt states, and two completed orderings")
      (is (pos? (or generated-states 0))))))

(deftest broken-rejection-produces-tlc-counterexample-test
  (testing "a post-transition illegal write fails independent invariants"
    (let [{:keys [trace trace-info] :as result} (check! ::corrupt-rejection)
          violation (:violation trace-info)]
      (is (and (vector? trace) (seq trace))
          (pr-str (select-keys result [:trace :trace-info])))
      (is (= :invariant (:type violation)) (pr-str trace-info))
      (is (contains?
           #{:gesso.model.recife-integration-test/rejected-preserves-documents
             :gesso.model.recife-integration-test/documents-have-admitted-values}
           (:name violation))
          (pr-str trace-info)))))
