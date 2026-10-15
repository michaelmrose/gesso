(ns gesso.model.recife-integration-test
  "Recife/TLC integration against Gesso's *existing* pure serial transaction
  semantics. This is a bounded model-checking regression, not a claim of
  database linearizability, arbitrary application-policy correctness, or
  universal implementation refinement.

  Two independently prepared updates race for one generic document. The
  model invokes gesso.model.tx-model/step, not an independently handwritten
  transaction decision function. A deliberately corrupted rejection branch
  is expected to yield a TLC invariant counterexample."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]
   [gesso.model.tx-model :as tx-model]
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
  {:first (prepared-plan :first)
   :second (prepared-plan :second)})

(defn- initial-state [mode]
  {::documents initial-documents
   ::attempted #{}
   ::observations []
   ::mode mode})

(defn- attempt [state actor]
  (if (contains? (::attempted state) actor)
    state
    (let [before (::documents state)
          result (tx-model/step before (get plans actor))
          rejected? (= :rejected (:status result))
          ;; Deliberately WRONG interpreter: a rejected plan writes a value.
          ;; The correct branch preserves tx-model/step's authoritative result.
          after (if (and rejected? (= :mutated-rejection (::mode state)))
                  (assoc-in (:after result) [target :value] :corrupted)
                  (:after result))]
      (-> state
          (assoc ::documents after)
          (update ::attempted conj actor)
          (update ::observations conj
                  {:actor actor
                   :status (:status result)
                   :before before
                   :after after})))))

(recife/defproc attempt-first (fn [state] (attempt state :first)))
(recife/defproc attempt-second (fn [state] (attempt state :second)))

(defn- rejection-preserves-state? [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= :rejected status)
                (= before after)))
          (::observations state)))

(defn- single-committer? [state]
  (<= (count (filter #(= :committed (:status %))
                     (::observations state)))
      1))

(defn- admitted-values-only? [state]
  (contains? #{:initial :first :second}
             (get-in state [::documents target :value])))

(helpers/definvariant rejected-is-unchanged [state]
  (rejection-preserves-state? state))

(helpers/definvariant one-committer [state]
  (single-committer? state))

(helpers/definvariant values-come-from-commits [state]
  (admitted-values-only? state))

(def ^:private components
  #{attempt-first attempt-second
    rejected-is-unchanged one-committer values-come-from-commits})

(defn- check! [mode]
  ;; Terminal states with both prepared plans attempted are intentional. This
  ;; pilot checks safety, not progress or fairness; disable TLC deadlock checks.
  (let [run (recife/run-model (initial-state mode) components
                              {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref run) @run run)))

(deftest existing-gesso-serial-commit-semantics-are-model-checked-test
  (testing "TLC checks both serial interleavings of actual tx-model/step"
    (let [{:keys [trace distinct-states generated-states trace-info] :as result}
          (check! :correct)]
      (is (= :ok trace) (pr-str (select-keys result
                                             [:trace :trace-info :distinct-states])))
      (is (nil? trace-info))
      (is (= 5 distinct-states)
          "Both prepared-plan execution orders must be reachable")
      (is (pos? (or generated-states 0))))))

(deftest corrupted-rejection-yields-real-tlc-counterexample-test
  (testing "mutating a rejected transaction must violate named invariants"
    (let [{:keys [trace trace-info] :as result} (check! :mutated-rejection)]
      (is (and (vector? trace) (seq trace))
          (pr-str (select-keys result [:trace :trace-info])))
      (is (= :invariant (get-in trace-info [:violation :type])))
      (is (contains?
           #{:gesso.model.recife-integration-test/rejected-is-unchanged
             :gesso.model.recife-integration-test/values-come-from-commits}
           (get-in trace-info [:violation :name]))
          (pr-str trace-info)))))
