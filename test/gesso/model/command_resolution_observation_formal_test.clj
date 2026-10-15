(ns gesso.model.command-resolution-observation-formal-test
  "Finite Recife/TLC correspondence checks for Gesso's controlled XTDB
   command-incorporation collector and source-derived serial transitions.

   The collector physically submits canonical plans and independently reads
   tracked XTDB documents after EVERY definitive submission. Recife explores
   both orders of the same prepared plans via formal-transition, delegating
   every modeled operation to tx-model/step. On each reached prefix whose
   order agrees with the physical submission, TLC checks the exact indexed
   status, prestate and poststate. The terminal check remains independent.
   Altered intermediate snapshots, missing trace steps and terminal status
   corruption must produce genuine TLC counterexamples.

   This is bounded correspondence evidence for selected serial executions in
   an exclusively controlled database, not universal XTDB refinement, proof
   of collector provenance, or proof of concurrent production operations.
   All physical I/O happens before model checking, never in an invariant."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure.walk :as walk]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.formal-transition :as formal]
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
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-xtdb)

(defn- fixture []
  (let [doc {:xt/id (str "formal-unit-" (random-uuid))
             :revision 0 :created-at created-at :updated-at created-at
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
        incs [(incorporation doc :actor-a "alpha")
              (incorporation doc :actor-b "beta")]]
    (assoc (observation/observe-and-check!
            {:node @!node :incorporations incs :submission-order order})
           :incorporations incs)))

(defn- all-strings [value]
  (let [found (atom #{})]
    (walk/postwalk (fn [x] (when (string? x) (swap! found conj x)) x) value)
    (sort @found)))

(defn- make-codec [values]
  ;; Recife 0.22.0 does not round-trip arbitrary strings, and its keyword
  ;; bridge canonicalizes underscores to hyphens. Neither is a Gesso semantic
  ;; change. Build a closed bijection for BOTH data kinds, fail on new strings,
  ;; and translate only at the model-checker boundary.
  (let [strings (all-strings values)
        string-forward (into {} (map-indexed
                                 (fn [i s] [s (keyword "gesso-model-token" (str "value" i))])
                                 strings))
        keyword-forward {:formal_units :formalunits
                         :gesso_commands :gessocommands}
        forward (merge string-forward keyword-forward)
        backward (into {} (map (fn [[original token]] [token original])) forward)]
    {:encode (fn [value]
               (walk/postwalk (fn [x]
                                (cond
                                  (string? x)
                                  (or (get forward x)
                                      (throw (ex-info "Unmapped checker string."
                                                      {:string x})))
                                  (keyword? x) (get forward x x)
                                  :else x)) value))
     :decode (fn [value]
               (walk/postwalk (fn [x] (get backward x x)) value))}))

(defn- map-model [state transform]
  (-> state
      (update ::formal/documents transform)
      (update ::formal/observations transform)))

(defn- case-data [physical]
  (let [subject (formal/prepared-subject
                 (:initial-state physical)
                 (mapv :plan (:incorporations physical)))
        codec (make-codec [(:initial-state physical)
                           (:incorporations physical)
                           (:observations physical)
                           (:submission-trace physical)])]
    {:subject subject
     :physical physical
     :codec codec}))

(defn- start-state [case]
  (-> (formal/initial-state (:subject case))
      (map-model (get-in case [:codec :encode]))
      (assoc ::order [])))

(defn- run-attempt [state index]
  (let [{:keys [subject codec]} @!case
        action (nth (:actions subject) index)]
    (if (some #{action} (formal/enabled-actions subject state))
      (-> (formal/transition subject
                              (map-model state (:decode codec)) action)
          (map-model (:encode codec))
          (update ::order conj index))
      state)))

(recife/defproc attempt-first (fn [state] (run-attempt state 0)))
(recife/defproc attempt-second (fn [state] (run-attempt state 1)))

(helpers/definvariant reference-rejection-is-atomic [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= status :rejected) (= before after)))
          (::formal/observations state)))

(helpers/definvariant xtdb-submission-trace-has-declared-order [state]
  ;; Unconditional evidence-completeness check. Without it, a missing or
  ;; mislabeled physical step could avoid every matching-prefix branch.
  (let [{:keys [physical]} @!case
        trace (:submission-trace physical)]
    (and (vector? trace)
         (= (count (:submission-order physical)) (count trace))
         (= (:submission-order physical) (mapv :plan-index trace))
         (= (:initial-state physical) (:before (first trace)))
         (= (get-in physical [:observations :state]) (:after (last trace))))))

(helpers/definvariant xtdb-prefix-matches-modeled-transition [state]
  (let [{:keys [physical codec]} @!case
        modeled-order (::order state)
        modeled-steps (::formal/observations state)
        physical-steps (vec (take (count modeled-order)
                                  (:submission-trace physical)))]
    ;; Every prefix of the observed order is reachable by the corresponding
    ;; Recife action choices. Alternate paths are checked for generic safety,
    ;; but must not be compared with a different physical submission order.
    (if (= modeled-order (mapv :plan-index physical-steps))
      (and (= (count modeled-order) (count modeled-steps))
           (every? true?
                   (map (fn [modeled observed]
                          (and (= (:plan-index modeled) (:plan-index observed))
                               (= (:status modeled) (:status observed))
                               (= (:before modeled) ((:encode codec) (:before observed)))
                               (= (:after modeled) ((:encode codec) (:after observed)))))
                        modeled-steps physical-steps))
           (= (::formal/documents state)
              ((:encode codec)
               (if (seq physical-steps)
                 (:after (peek physical-steps))
                 (:initial-state physical)))))
      true)))

(helpers/definvariant xtdb-observation-matches-modeled-order [state]
  (let [{:keys [subject physical codec]} @!case
        actual-order (:submission-order physical)]
    (if (and (empty? (::formal/pending state))
             (= actual-order (::order state)))
      (let [trace (::formal/observations state)
            indexed-statuses (reduce (fn [result {:keys [plan-index status]}]
                                       (assoc result plan-index status))
                                     (vec (repeat (count (:actions subject)) nil))
                                     trace)]
        (and (= indexed-statuses
                (get-in physical [:observations :statuses]))
             (= (::formal/documents state)
                ((:encode codec) (get-in physical [:observations :state])))))
      true)))

(def ^:private components
  #{attempt-first attempt-second
    reference-rejection-is-atomic
    xtdb-submission-trace-has-declared-order
    xtdb-prefix-matches-modeled-transition
    xtdb-observation-matches-modeled-order})

(defn- check! [case]
  (reset! !case case)
  (try
    (let [result (recife/run-model (start-state case) components
                                   {:no-deadlock true :workers 1})]
      (if (instance? clojure.lang.IDeref result) @result result))
    (finally (reset! !case nil))))

(deftest controlled-xtdb-execution-refines-selected-serial-order-test
  (doseq [order [[0 1] [1 0]]]
    (testing (str "Physical transaction execution ordered " order)
      (let [physical (collect order)
            {:keys [codec] :as case} (case-data physical)
            result (check! case)]
        (is (= :consistent (get-in physical [:evidence :status])))
        (is (= [order] (get-in physical [:evidence :matching-orders])))
        (is (= (:initial-state physical)
               ((:decode codec) ((:encode codec) (:initial-state physical)))))
        (is (= order (mapv :plan-index (:submission-trace physical))))
        (is (= (:initial-state physical)
               (get-in physical [:submission-trace 0 :before])))
        (is (= (get-in physical [:submission-trace 0 :after])
               (get-in physical [:submission-trace 1 :before])))
        (is (= (get-in physical [:observations :state])
               (get-in physical [:submission-trace 1 :after])))
        (is (= (get-in physical [:observations :state])
               ((:decode codec)
                ((:encode codec) (get-in physical [:observations :state])))))
        (is (= :ok (:trace result))
            (pr-str (select-keys result [:trace :trace-info])))
        (is (nil? (:trace-info result)))
        (is (pos? (:distinct-states result)))))))

(deftest tampered-physical-outcome-must-fail-tlc-test
  (let [physical (collect [0 1])
        ;; Only the externally observed outcome is corrupted. The stored
        ;; physical documents and the canonical Gesso plans are unchanged.
        altered (assoc-in physical [:observations :statuses]
                          [:rejected :rejected])
        result (check! (case-data altered))]
    (is (= :consistent (get-in physical [:evidence :status])))
    (is (not= :ok (:trace result)))
    (is (some? (:trace-info result)))
    (is (re-find #"xtdb-observation-matches-modeled-order"
                 (pr-str (:trace-info result))))))

(deftest tampered-intermediate-physical-prefix-must-fail-tlc-test
  (let [physical (collect [0 1])
        ;; The independent terminal :observations and real XTDB data are
        ;; untouched. These mutations must be found by prefix checking,
        ;; even though v912's terminal-only comparison would accept them.
        cases [[:false-first-poststate
                (assoc-in physical [:submission-trace 0 :after]
                          (:initial-state physical))]
               [:false-second-prestate
                (assoc-in physical [:submission-trace 1 :before]
                          (:initial-state physical))]
               [:false-first-status
                (assoc-in physical [:submission-trace 0 :status] :rejected)]
               [:wrong-plan-index
                (assoc-in physical [:submission-trace 1 :plan-index] 0)]
               [:missing-last-submission
                (update physical :submission-trace pop)]]]
    (is (= :consistent (get-in physical [:evidence :status])))
    (doseq [[mutant altered] cases]
      (testing (name mutant)
        (is (= (:observations physical) (:observations altered)))
        (let [result (check! (case-data altered))
              error (pr-str (:trace-info result))]
          (is (not= :ok (:trace result)))
          (is (some? (:trace-info result)))
          (is (re-find (if (#{:missing-last-submission :wrong-plan-index} mutant)
                        #"xtdb-submission-trace-has-declared-order"
                        #"xtdb-prefix-matches-modeled-transition") error)
              error))))))
