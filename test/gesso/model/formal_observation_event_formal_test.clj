(ns gesso.model.formal-observation-event-formal-test
  "Real-executor, finite correspondence regressions for the generic Gesso
   authoritative-operation path.  Unlike controlled direct-XTDB fixtures,
   evidence originates from a registered Live executor invoked through its
   normal server boundary.  The opaque execution-evidence handle must still
   be checked against that exact executor before using any detached report.

   A passing TLC run checks the selected source-derived reference transition
   against one observed execution.  It is not a durable certificate, proof of
   policy correctness, authenticated XTDB history, or universal refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.choreo.identity :as identity]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.live.optimistic.resolution-assurance :as assurance]
            [gesso.live.optimistic.server :as server]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.formal-observation :as formal]
            [gesso.model.resolved-operation :as resolved]
            [recife.core :as recife]
            [recife.helpers :as helpers]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private table :formal_event_units)
(def ^:private operation :formal-event/advance)
(def ^:private principal (identity/principal "formal-actor"))
(def ^:private now "2026-10-14T00:01:00Z")
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))
(defonce ^:private !formal-case (atom nil))

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(defn- read-row [table-name id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table-name) " WHERE _id = ?") id])))

(defn- fixture! []
  (let [suffix (str (random-uuid))
        before {:xt/id (str "formal-event-unit-" suffix)
                :revision 0 :created-at "t0" :updated-at "t0" :value "before"}
        after (assoc before :revision 1 :updated-at now :value "after")
        wire-command (protocol/command
                      {:command-id (identity/command-id (str "formal-event-command-" suffix))
                       :execution-id (identity/execution-id (str "formal-event-execution-" suffix))
                       :operation operation
                       :arguments {:unit-id (:xt/id before)}
                       :observed-basis {:tx-id 1 :system-time now}
                       :scope [:formal-event-unit (:xt/id before)]})
        model (resolved/build-operation
               {:operation operation
                :read-record! (fn [_ id] (read-row :gesso_commands id))
                :may-disclose? (fn [& _] true)
                :construct! (fn [_ _]
                              {:now now
                               :outcome {:status :applied}
                               :plan {:commands [(command/update-command
                                                  table :advance before after version)]
                                      :guards []
                                      :emit false}})})
        executor (adapter/build-executor
                  {:operation operation
                   :resolved-operation model
                   :submission-mode! (fn [_] :submit)
                   :project! (fn [_ result]
                               {:resolution (if (= :replay (:status result))
                                              :already-incorporated :confirmed)
                                :authoritative
                                (protocol/authoritative
                                 {:presence :present
                                  :basis {:tx-id 2 :system-time now}
                                  :projection {:formal-event/value
                                               (:value (read-row table (:xt/id before)))}})
                                :outcome :formal-event/applied})})
        live-server (server/server
                     {:principal-fn (fn [ctx] (:authenticated-principal ctx))
                      :operations
                      {operation
                       (server/operation
                        {:name :formal-event-optimistic
                         :operation operation
                         :execute! executor
                         :settlement-contract
                         {:confirmed {:outcomes #{:formal-event/applied}
                                      :commit/status :committed}
                          :already-incorporated
                          {:outcomes #{:formal-event/applied}
                           :commit/status :committed}}})}})]
    (xt/execute-tx @!node [[:put-docs table before]])
    {:before before :after after :executor executor
     :command wire-command :server live-server}))

(defn- observe! [f]
  (let [{:keys [evidence prepared-send]}
        (assurance/run-and-check!
         {:server (:server f)
          :ctx {:biff.xtdb/node @!node :authenticated-principal principal}
          :command (:command f)})
        report (assurance/inspect-evidence-for-executor evidence (:executor f))]
    {:handle evidence :report report :prepared-send prepared-send}))

(defn- error-kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))

(recife/defproc observed-real-executor-attempt
  (fn [state] (formal/advance @!formal-case state 0)))

(helpers/definvariant executor-observation-is-complete [_state]
  (formal/evidence-complete? @!formal-case))

(helpers/definvariant executor-reference-rejection-is-atomic [state]
  (formal/reference-rejection-atomic? state))

(helpers/definvariant executor-prefix-corresponds [state]
  (formal/prefix-corresponds? @!formal-case state))

(helpers/definvariant executor-terminal-corresponds [state]
  (formal/terminal-corresponds? @!formal-case state))

(def ^:private model-components
  #{observed-real-executor-attempt executor-observation-is-complete
    executor-reference-rejection-is-atomic executor-prefix-corresponds
    executor-terminal-corresponds})

(defn- run-check! [case]
  (reset! !formal-case case)
  (try
    (let [result (recife/run-model (formal/initial-state case)
                                   model-components
                                   {:no-deadlock true :workers 1})]
      (if (instance? clojure.lang.IDeref result) @result result))
    (finally (reset! !formal-case nil))))

(defn- invariant-failure? [result expected]
  (and (not= :ok (:trace result))
       (= :invariant (get-in result [:trace-info :violation :type]))
       (= expected (get-in result [:trace-info :violation :name]))))

(deftest real-registered-executor-observation-refines-selected-reference-step-test
  (let [f (fixture!)
        {:keys [handle report prepared-send]} (observe! f)
        observation (:observation report)
        case (formal/prepared-event-case observation)
        observed (formal/observed-path-report case)
        checked (run-check! case)]
    (is (some? prepared-send))
    (is (= :bounded-execution-correspondence (:status report)))
    (is (identical? report
                    (assurance/inspect-evidence-for-executor handle (:executor f))))
    (is (= (:after f) (read-row table (get-in f [:before :xt/id]))))
    (is (= :consistent (get-in observation [:evidence :status])))
    (is (= :single-executor-attempt-diagnostic (:observation-origin case)))
    (is (= :corresponds (:status observed)))
    (is (= [0] (:checked-order observed)))
    (is (true? (formal/evidence-complete? case)))
    (is (= :ok (:trace checked)) (pr-str (:trace-info checked)))
    (is (nil? (:trace-info checked)))
    (is (nil? (assurance/inspect-evidence observation))
        "Detached physical observations are not registry handles")
    (is (nil? (assurance/inspect-evidence report))
        "A copied evidence report cannot impersonate the opaque handle")
    (is (nil? (assurance/inspect-evidence (Object.))))
    (is (nil? (assurance/inspect-evidence-for-executor
               handle (fn [context] ((:executor f) context))))
        "A wrapper around the actual executor is a different executor")
    (is (nil? (assurance/inspect-evidence-for-executor
               handle (:executor (fixture!))))
        "Another registered executor for the same operation is not the one observed")))

(deftest tampered-detached-executor-observation-must-not-verify-test
  (let [f (fixture!)
        {:keys [report]} (observe! f)
        observation (:observation report)
        altered [[:changed-binding
                  (assoc-in observation
                            [:submitted-incorporation :record
                             :gesso.command/binding :operation]
                            :formal-event/forged)]
                 [:false-definitive-status
                  (assoc-in observation [:physical :status] :rejected)]
                 [:indeterminate-outcome
                  (assoc-in observation [:physical :status] :unknown)]
                 [:lost-inspected-target
                  (update-in observation [:observations :inspected-targets]
                             disj (first (sort-by pr-str
                                                  (get-in observation
                                                          [:observations :inspected-targets]))))]
                 [:false-terminal-poststate
                  (assoc-in observation [:observations :state]
                            (:initial-state observation))]]]
    (doseq [[label mutated] altered]
      (testing (name label)
        (is (some? (error-kind #(formal/prepared-event-case mutated)))
            "A detached malformed observation must not produce a formal subject")))
    (is (nil? (assurance/inspect-evidence
               (assoc report :status :verified)))
        "A fabricated report never becomes registry evidence")))

(deftest checker-catches-false-but-complete-executor-poststate-test
  (let [f (fixture!)
        {:keys [report]} (observe! f)
        case (formal/prepared-event-case (:observation report))
        ;; Keep the physical trace structurally complete; change the only
        ;; observed after-state.  The reference transition must disagree.
        mutated (-> case
                    (assoc-in [:physical :submission-trace 0 :after]
                              (:initial-state (:physical case)))
                    (assoc-in [:physical :observations :state]
                              (:initial-state (:physical case))))
        result (run-check! mutated)]
    (is (= :corresponds (:status (formal/observed-path-report case))))
    (is (not= :corresponds
              (:status (formal/observed-path-report mutated))))
    (is (some #(invariant-failure? result %)
              [:gesso.model.formal-observation-event-formal-test/executor-prefix-corresponds
               :gesso.model.formal-observation-event-formal-test/executor-terminal-corresponds])
        (pr-str (:trace-info result)))))
