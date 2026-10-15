(ns gesso.model.command-resolution-observation-formal-test
  "Bounded Recife/TLC checks of generic, controlled XTDB command incorporation.

   All transition replay, finite checker-value encoding, physical evidence
   completeness, and prefix/terminal comparison now belong to the reusable
   gesso.model.formal-observation implementation. This test owns only the
   generic physical fixture, TLC orchestration, and positive/negative cases.

   A passing check establishes correspondence for the selected serial trace
   under the collector's assumptions. It does not prove universal XTDB
   refinement, concurrent execution, observation provenance, or application
   authorization. Physical I/O takes place before model checking."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.formal-observation :as formal-observation]
   [gesso.model.resolution-observation :as observation]
   [recife.core :as recife]
   [recife.helpers :as helpers]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private table :formal_units)
(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private created-at "2026-10-14T00:00:00Z")
(def ^:private now "2026-10-14T00:01:00Z")

(defonce ^:private !node (atom nil))
(defonce ^:private !case (atom nil))

(defn- with-xtdb [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try
      (f)
      (finally (reset! !node nil)))))

(use-fixtures :once with-xtdb)

(defn- fixture []
  (let [doc {:xt/id (str "formal-unit-" (random-uuid))
             :revision 0
             :created-at created-at
             :updated-at created-at
             :value "initial"}]
    (xt/execute-tx @!node [[:put-docs table doc]])
    doc))

(defn- incorporation [doc actor value]
  (resolution/incorporation-plan
   {:binding {:command-id (identity/command-id
                           (str "formal-command-" (random-uuid)))
              :principal (identity/principal actor)
              :operation :formal-unit/update
              :arguments {:unit-id (:xt/id doc) :value value}}
    :outcome {:status :applied :value value}
    :now now
    :plan {:commands [(command/update-command
                       table :update doc
                       (-> doc (assoc :value value :updated-at now)
                           (update :revision inc))
                       version)]
           :guards []
           :emit false}}))

(defn- collect [order]
  (let [doc (fixture)
        incorporations [(incorporation doc :actor-a "alpha")
                        (incorporation doc :actor-b "beta")]]
    (assoc (observation/observe-and-check!
            {:node @!node
             :incorporations incorporations
             :submission-order order})
           :incorporations incorporations)))

(defn- prepared-case [physical]
  (formal-observation/prepared-case
   physical
   (mapv :plan (:incorporations physical))))

(recife/defproc attempt-first
  (fn [state] (formal-observation/advance @!case state 0)))
(recife/defproc attempt-second
  (fn [state] (formal-observation/advance @!case state 1)))

(helpers/definvariant reference-rejection-is-atomic [state]
  (formal-observation/reference-rejection-atomic? state))
(helpers/definvariant xtdb-submission-trace-is-complete [_state]
  (formal-observation/evidence-complete? @!case))
(helpers/definvariant xtdb-prefix-matches-modeled-transition [state]
  (formal-observation/prefix-corresponds? @!case state))
(helpers/definvariant xtdb-observation-matches-modeled-order [state]
  (formal-observation/terminal-corresponds? @!case state))

(def ^:private components
  #{attempt-first attempt-second
    reference-rejection-is-atomic
    xtdb-submission-trace-is-complete
    xtdb-prefix-matches-modeled-transition
    xtdb-observation-matches-modeled-order})

(defn- check! [case]
  (reset! !case case)
  (try
    (let [result (recife/run-model
                  (formal-observation/initial-state case)
                  components
                  {:no-deadlock true :workers 1})]
      (if (instance? clojure.lang.IDeref result) @result result))
    (finally (reset! !case nil))))

(defn- invariant-counterexample? [result expected-name]
  (and (not= :ok (:trace result))
       (= :invariant (get-in result [:trace-info :violation :type]))
       (= expected-name (get-in result [:trace-info :violation :name]))))

(deftest controlled-xtdb-execution-refines-selected-serial-order-test
  (doseq [order [[0 1] [1 0]]]
    (testing (str "Physical transaction execution ordered " order)
      (let [physical (collect order)
            case (prepared-case physical)
            codec (:codec case)
            path (formal-observation/observed-path-report case)
            result (check! case)]
        (is (= :consistent (get-in physical [:evidence :status])))
        (is (= [order] (get-in physical [:evidence :matching-orders])))
        (is (= :corresponds (:status path)) (pr-str path))
        (is (= order (:checked-order path)))
        (is (true? (formal-observation/evidence-complete? case)))
        (doseq [value [(:initial-state physical)
                       (get-in physical [:observations :state])
                       :formal_units
                       "initial"]]
          (is (= value ((:decode codec) ((:encode codec) value)))))
        (is (= :ok (:trace result))
            (pr-str (select-keys result [:trace :trace-info])))
        (is (nil? (:trace-info result)))
        (is (= 5 (:distinct-states result)))))))

(deftest tampered-physical-outcome-must-fail-tlc-test
  (let [physical (collect [0 1])
        ;; The authoritative documents and submission trace are untouched.
        altered (assoc-in physical [:observations :statuses]
                          [:rejected :rejected])
        case (prepared-case altered)
        path (formal-observation/observed-path-report case)
        result (check! case)]
    (is (= :consistent (get-in physical [:evidence :status])))
    (is (= :evidence-incomplete (:status path)))
    (is (invariant-counterexample?
         result
         :gesso.model.command-resolution-observation-formal-test/xtdb-submission-trace-is-complete)
        (pr-str (:trace-info result)))))

(deftest tampered-intermediate-physical-prefix-must-fail-tlc-test
  (let [physical (collect [0 1])
        ;; Each variant keeps the terminal physical observation unchanged.
        ;; The first mutant keeps the physical evidence chain internally
        ;; consistent: only the Gesso reference transition can reject it.
        false-but-contiguous
        (-> physical
            (assoc-in [:submission-trace 0 :after] (:initial-state physical))
            (assoc-in [:submission-trace 1 :before] (:initial-state physical)))
        cases [[:false-contiguous-prefix false-but-contiguous
                :xtdb-prefix-matches-modeled-transition]
               [:false-first-poststate
                (assoc-in physical [:submission-trace 0 :after]
                          (:initial-state physical))
                :xtdb-submission-trace-is-complete]
               [:false-second-prestate
                (assoc-in physical [:submission-trace 1 :before]
                          (:initial-state physical))
                :xtdb-submission-trace-is-complete]
               [:false-first-status
                (assoc-in physical [:submission-trace 0 :status] :rejected)
                :xtdb-submission-trace-is-complete]
               [:wrong-plan-index
                (assoc-in physical [:submission-trace 1 :plan-index] 0)
                :xtdb-submission-trace-is-complete]
               [:missing-last-submission
                (update physical :submission-trace pop)
                :xtdb-submission-trace-is-complete]]]
    (is (= :consistent (get-in physical [:evidence :status])))
    (doseq [[label altered invariant] cases]
      (testing (name label)
        (is (= (:observations physical) (:observations altered)))
        (let [case (prepared-case altered)
              path (formal-observation/observed-path-report case)
              result (check! case)]
          (is (not= :corresponds (:status path)))
          (is (invariant-counterexample?
               result
               (keyword "gesso.model.command-resolution-observation-formal-test"
                        (name invariant)))
              (pr-str (:trace-info result))))))))
