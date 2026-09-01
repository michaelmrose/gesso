(ns gesso.live.browser.diagnostic-noninterference-test
  "Browser-host diagnostic noninterference for one canonical ExecutablePlan.

   Artifact tooling owns DiagnosticProofSidecar validation and exact
   executable-digest binding. The browser runtime must not receive a sidecar as
   semantic input at all.

   This namespace exercises the complementary runtime property required by the
   v4.5 plan: the exact same projected ExecutablePlan, given the exact same
   normalized event sequence and physical handlers, must produce the same
   semantic trace, abstract effects, final AdapterState, resource ownership, and
   completion output whether or not a read-only diagnostic observer is attached.

   The sequence deliberately produces one stale diagnostic between two ordinary
   executions so the observer is genuinely exercised rather than merely
   configured.

   As an ordinary *_test.cljs namespace it runs under the Node browser-semantic
   group and again under the normal real-Chromium cljs.test corpus."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.machine :as machine]
   [gesso.choreo.project :as project]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.shell :as shell]))

(def fixed-now
  :gesso.test/fixed-now)

(defn- canonical-browser-plan
  []
  (project/project
   (choreo/->choreography
    {:name :gesso.test/diagnostic-noninterference
     :initial :first
     :states
     {:first
      (choreo/local
       :browser
       :browser/first
       :second
       {:outputs #{:first/value}})

      :second
      (choreo/local
       :browser
       :browser/second
       :done
       {:requires #{:first/value}
        :outputs #{:second/value}})

      :done
      (choreo/return :complete)}})
   :browser))

(defn- start-event
  [execution-id plan]
  {:event :execution/start
   :execution-id execution-id
   :execution
   (machine/start
    plan
    {:now-fn (constantly fixed-now)})
   :target-id :diagnostic/noninterference})

(defn- stale-retire-event
  []
  {:event :execution/retire
   :execution-id "already-retired"
   :generation 999999
   :reason :stale-diagnostic-probe})

(defn- event-sequence
  [plan]
  ;; Construct this vector once and feed the exact same immutable event values to
  ;; both runtimes. The fixed machine clock removes incidental trace-time
  ;; differences from the comparison.
  [(start-event "execution-1" plan)
   (stale-retire-event)
   (start-event "execution-2" plan)])

(defn- semantic-transition
  [{:keys [event effects]}]
  {:event event
   :effects (adapter/semantic-effects effects)})

(defn- transition-effects
  [transitions]
  (mapv :effects transitions))

(defn- run-sequence
  [events diagnostic-observer]
  (let [transitions (atom [])
        diagnostics (atom [])
        local-effects (atom [])
        completions (atom [])
        dispatch-statuses (atom [])
        options
        (cond->
         {:handlers
          {:machine/local
           (fn [{:keys [effect]}]
             (let [action
                   (get-in effect [:action :action])
                   outputs
                   (case action
                     :browser/first
                     {:first/value 7}

                     :browser/second
                     {:second/value 8}

                     {})]
               (swap! local-effects
                      conj
                      {:action action
                       :outputs outputs})
               outputs))

           :execution/completed
           (fn [{:keys [effect]}]
             (swap! completions conj effect))}

          :on-transition
          #(swap! transitions conj %)}

          diagnostic-observer
          (assoc
           :on-diagnostic
           (fn [diagnostic]
             (swap! diagnostics conj diagnostic)
             (diagnostic-observer diagnostic))))
        runtime (shell/create options)]
    (try
      (doseq [event events]
        (swap! dispatch-statuses
               conj
               (:status
                (shell/dispatch! runtime event))))

      {:dispatch-statuses @dispatch-statuses
       :semantic-transitions
       (mapv semantic-transition @transitions)
       :abstract-effect-trace
       (transition-effects @transitions)
       :state (shell/state runtime)
       :resource-counts (shell/resource-counts runtime)
       :local-effects @local-effects
       :completions @completions
       :diagnostics @diagnostics
       :invariant-errors
       (adapter/invariant-errors
        (shell/state runtime))}
      (finally
        (shell/shutdown! runtime)))))

(deftest same-executable-plan-is-semantically-identical-with-or-without-diagnostic-observer-test
  (let [plan (canonical-browser-plan)
        events (event-sequence plan)
        without-observer
        (run-sequence events nil)
        with-observer
        (run-sequence events (constantly nil))]

    (testing "both runs consume one canonical projected ExecutablePlan and the exact same event values"
      (is (project/executable-plan? plan))
      (is (= :browser (:role plan)))
      (is (= 3 (count events))))

    (testing "the stale probe genuinely exercises the diagnostic observer"
      (is (empty? (:diagnostics without-observer)))
      (is (= 1 (count (:diagnostics with-observer))))
      (is (= :execution/retire
             (:event
              (first (:diagnostics with-observer))))))

    (testing "observer attachment cannot alter semantic state or resource ownership"
      (is (= (:state without-observer)
             (:state with-observer)))
      (is (= (:resource-counts without-observer)
             (:resource-counts with-observer)))
      (is (= {:timers 0
              :transports 0
              :continuity 0
              :optimistic 0
              :optimistic-timeouts 0}
             (:resource-counts with-observer)))
      (is (= []
             (:invariant-errors without-observer)
             (:invariant-errors with-observer))))

    (testing "semantic transitions and abstract effects are identical"
      (is (= (:semantic-transitions without-observer)
             (:semantic-transitions with-observer)))
      ;; Diagnostic effects are adapter output too. Merely observing them must
      ;; not add, remove, reorder, or rewrite the effect trace.
      (is (= (:abstract-effect-trace without-observer)
             (:abstract-effect-trace with-observer))))

    (testing "physical interpretation observes the same semantic work"
      (is (= (:dispatch-statuses without-observer)
             (:dispatch-statuses with-observer)))
      (is (= (:local-effects without-observer)
             (:local-effects with-observer)))
      (is (= (:completions without-observer)
             (:completions with-observer))))

    (testing "both ordinary executions complete and retire cleanly"
      (is (= 4 (count (:local-effects with-observer))))
      (is (= 2 (count (:completions with-observer))))
      (is (nil?
           (adapter/execution
            (:state with-observer)
            "execution-1")))
      (is (nil?
           (adapter/execution
            (:state with-observer)
            "execution-2"))))))

(deftest throwing-diagnostic-observer-is-also-semantically-powerless-test
  (let [plan (canonical-browser-plan)
        events (event-sequence plan)
        baseline
        (run-sequence events nil)
        throwing
        (run-sequence
         events
         (fn [_]
           (throw
            (js/Error.
             "diagnostic observer must remain powerless"))))]

    (testing "observer failure is swallowed at the diagnostic boundary"
      (is (= 1 (count (:diagnostics throwing))))
      (is (= :execution/retire
             (:event (first (:diagnostics throwing))))))

    (testing "even a throwing observer cannot perturb the canonical run"
      (is (= (:dispatch-statuses baseline)
             (:dispatch-statuses throwing)))
      (is (= (:semantic-transitions baseline)
             (:semantic-transitions throwing)))
      (is (= (:abstract-effect-trace baseline)
             (:abstract-effect-trace throwing)))
      (is (= (:state baseline)
             (:state throwing)))
      (is (= (:resource-counts baseline)
             (:resource-counts throwing)))
      (is (= (:local-effects baseline)
             (:local-effects throwing)))
      (is (= (:completions baseline)
             (:completions throwing)))
      (is (= []
             (:invariant-errors throwing))))))
