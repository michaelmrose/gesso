(ns gesso.assurance.proof.recife-finite-correspondence-test
  "v1438: independent bounded finite-model / Recife-TLC agreement for a
   two-operation Gesso guarded-transition construction.

   The source catalog is checked by Gesso's independent finite-model oracle.
   A separate test-local interpreter enumerates both operation orderings.
   Recife explores the same source-derived abstract guarded transitions and
   uses independent safety invariants. A deliberate post-transition mutant
   must yield a TLC counterexample; an independently mutated source must
   yield a finite-oracle step counterexample to the same business property.

   This checks a finite enum/Boolean universe and two one-shot processes. It
   does not prove universal checker soundness, deployment coverage, XTDB
   refinement, fairness, or certification authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.finite-model-oracle :as oracle]
            [gesso.assurance.proof.independent-transition-semantics :as transition]
            [recife.core :as recife]
            [recife.helpers :as helpers]))

(def ^:private status-type
  {:type :enum :name :request/status :values #{:open :claimed}})
(def ^:private state-types
  {:request/status status-type
   :request/flag {:type :boolean}})
(def ^:private initial-state
  {:request/status :open :request/flag false})
(def ^:private open-status
  {:readers #{:request/status :request/flag}
   :expression [:and [:= [:read :request/status] [:literal :open]]
                [:= [:read :request/flag] [:literal false]]]})
(def ^:private invariant-status
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed}]]})
(def ^:private invariant-flag
  {:readers #{:request/flag}
   :expression [:= [:read :request/flag] [:literal false]]})

(defn- operation [id from to]
  {:id id
   :construction
   {:subject {:kind :operation :id id :interface {:operation id}}
    :state-types state-types
    :input-types {}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal from]]}
    :reads #{:request/status}
    :writes #{:request/status}
    :replacements {:request/status [:literal to]}
    :frame #{:request/flag}}})

(def ^:private catalog
  {:id :request/finite-recife-correspondence
   :declared-operation-ids #{:request/claim :request/release}
   :initial-condition open-status
   :invariants {:request/status-domain invariant-status
                :request/flag-false invariant-flag}
   :operations [(operation :request/claim :open :claimed)
                (operation :request/release :claimed :open)]})

(def ^:private source
  {:catalog catalog
   :state-domains {:request/status [:open :claimed]
                   :request/flag [false true]}
   :input-domains {:request/claim {} :request/release {}}
   :max-cases 12})

(def ^:private operations-by-id
  (into {} (map (juxt :id identity)) (:operations catalog)))

(defn- as-guarded-declaration [{:keys [construction]}]
  ;; Lossless adapter for this restricted, literal-only source fragment.
  ;; The finite oracle itself does NOT use this adapter.
  (-> construction
      (assoc :assignments (:replacements construction))
      (dissoc :replacements :frame)))

(defn- independently-enabled? [state id]
  (case id
    :request/claim (= :open (:request/status state))
    :request/release (= :claimed (:request/status state))
    false))

(defn- independently-execute [state id]
  (if (independently-enabled? state id)
    (assoc state :request/status
           (case id :request/claim :claimed :request/release :open))
    state))

(def ^:private orderings
  [[:request/claim :request/release]
   [:request/release :request/claim]])

(defn- expected-path [order]
  (reductions independently-execute initial-state order))

(defn- expected-projections []
  ;; Only successfully executed operations appear in the trace. Disabled
  ;; actions stutter in the abstract state; the Recife process PC is internal.
  (set
   (mapcat (fn [order]
             (rest (reductions
                    (fn [{:keys [model trace]} id]
                      (if (independently-enabled? model id)
                        {:model (independently-execute model id)
                         :trace (conj trace id)}
                        {:model model :trace trace}))
                    {:model initial-state :trace []}
                    order)))
           orderings)))

(def ^:private admitted-projections
  (conj (expected-projections) {:model initial-state :trace []}))

(defn- production-constrained-next [state id]
  (if-not (independently-enabled? state id)
    nil
    (let [after (independently-execute state id)
          report (transition/observe-transition!
                  (as-guarded-declaration (get operations-by-id id))
                  state {} after)]
      (when-not (:matches? report)
        (throw (ex-info "Source-derived transition disagrees with witness."
                        {:operation id :before state :after after
                         :observation report})))
      after)))

(defn- advance [state id]
  (let [before (::model state)
        after (production-constrained-next before id)]
    (if-not after
      state
      (let [after (if (and (= ::corrupt-release (::mode state))
                           (= id :request/release))
                    ;; Corrupt the *result* after checking the original
                    ;; source-derived transition. The oracle sees a separate
                    ;; mutation of the actual source declaration below.
                    (assoc after :request/flag true)
                    after)]
        (-> state
            (assoc ::model after)
            (update ::trace conj id))))))

(recife/defproc attempt-claim (fn [state] (advance state :request/claim)))
(recife/defproc attempt-release (fn [state] (advance state :request/release)))

;; These are independent assertions on TLC-observed states, not reports
;; supplied by the transition interpreter.
(helpers/definvariant status-is-in-declared-enum [state]
  (contains? #{:open :claimed} (get-in state [::model :request/status])))

(helpers/definvariant false-flag-is-preserved [state]
  (false? (get-in state [::model :request/flag])))

(helpers/definvariant paths-belong-to-independent-enumeration [state]
  (contains? admitted-projections
             {:model (::model state) :trace (::trace state)}))

(def ^:private components
  #{attempt-claim attempt-release
    status-is-in-declared-enum
    false-flag-is-preserved
    paths-belong-to-independent-enumeration})

(defn- run-recife! [mode]
  (let [result (recife/run-model
                {::model initial-state ::trace [] ::mode mode}
                components
                {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(defn- incorrect-release-source []
  ;; Still well-typed: the corrupted operation declares its additional write.
  ;; This is a real change to the catalog, not forged oracle output.
  (let [replacement (-> (get operations-by-id :request/release)
                        (update-in [:construction :writes] conj :request/flag)
                        (update-in [:construction :frame] disj :request/flag)
                        (assoc-in [:construction :replacements :request/flag]
                                  [:literal true]))]
    (assoc-in source [:catalog :operations 1] replacement)))

(deftest source-normalization-and-local-reachability-agree
  (is (= #{:request/claim :request/release}
         (set (keys operations-by-id))))
  (is (= #{initial-state
           {:request/status :claimed :request/flag false}}
         (set (mapcat expected-path orderings))))
  (is (= #{{:model initial-state :trace []}
           {:model {:request/status :claimed :request/flag false}
            :trace [:request/claim]}
           {:model initial-state
            :trace [:request/claim :request/release]}}
         admitted-projections))
  (doseq [[id from to] [[:request/claim :open :claimed]
                        [:request/release :claimed :open]]]
    (let [construction (:construction (get operations-by-id id))
          declaration (as-guarded-declaration (get operations-by-id id))]
      (is (= [:= [:read :request/status] [:literal from]]
             (get-in construction [:guard :expression])))
      (is (= {:request/status [:literal to]}
             (:replacements construction)))
      (is (= #{:request/flag} (:frame construction)))
      (is (= #{:request/status} (:writes construction)))
      (is (= (:replacements construction) (:assignments declaration)))
      (is (= (:state-types construction) (:state-types declaration)))
      (let [before (assoc initial-state :request/status from)
            after (independently-execute before id)]
        (is (= after (production-constrained-next before id)))
        (is (nil? (production-constrained-next
                   (assoc before :request/status to) id)))))))

(deftest finite-oracle-establishes-only-declared-bounded-judgment
  (let [report (oracle/check-finite-model! source)]
    (is (= :bounded-no-counterexample (:status report)) (pr-str report))
    (is (:exhaustive? report))
    (is (= 4 (get-in report [:universe :states])))
    (is (= 12 (get-in report [:universe :potential-cases])))
    (is (true? (get-in report [:universe :all-type-domains-complete?])))
    (is (= :none (:authority report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-established (:universal-induction report)))
    (is (= :not-established (:physical-correspondence report)))))

(deftest finite-universe-budget-remains-an-explicit-bound
  (let [report (oracle/check-finite-model! (assoc source :max-cases 11))]
    (is (= :budget-exceeded (:status report)))
    (is (= :finite-case-budget (:reason report)))
    (is (= 12 (get-in report [:universe :potential-cases])))
    (is (false? (:exhaustive? report)))
    (is (= :none (:authority report)))))

(deftest real-transition-observer-rejects-post-transition-mutant
  (let [before {:request/status :claimed :request/flag false}
        good {:request/status :open :request/flag false}
        bad {:request/status :open :request/flag true}
        original (as-guarded-declaration
                  (get operations-by-id :request/release))
        changed (as-guarded-declaration
                 (get-in (incorrect-release-source)
                         [:catalog :operations 1]))]
    (is (true? (:matches?
                (transition/observe-transition! original before {} good))))
    (is (false? (:matches?
                 (transition/observe-transition! original before {} bad))))
    ;; The corrupted result *does* match a different, well-typed source.
    ;; Preserving the independent invariant is therefore essential: checking
    ;; relation membership alone cannot establish that a business rule holds.
    (is (true? (:matches?
                (transition/observe-transition! changed before {} bad))))))

(deftest recife-and-local-oracle-agree-on-safe-two-operation-machine
  (let [{:keys [trace trace-info distinct-states generated-states] :as result}
        (run-recife! ::correct)]
    (is (= :ok trace)
        (pr-str (select-keys result [:trace :trace-info :distinct-states])))
    (is (nil? trace-info))
    ;; Recife also models per-process control locations; abstract projections
    ;; cannot be inferred from a raw TLC distinct-state count alone.
    (is (>= (or distinct-states 0) (count admitted-projections)))
    (is (pos? (or generated-states 0)))))

(deftest independent-source-mutant-produces-finite-oracle-step-counterexample
  (let [report (oracle/check-finite-model! (incorrect-release-source))
        c (:counterexample report)]
    (is (= :counterexample (:status report)) (pr-str report))
    (is (= :step (:kind c)) (pr-str c))
    (is (= :request/release (:operation-id c)))
    (is (= :request/flag-false (:invariant-id c)))
    (is (= false (get-in c [:prestate :request/flag])))
    (is (= true (get-in c [:poststate :request/flag])))
    (is (= :none (:authority report)))
    (is (= :not-established (:universal-induction report)))))

(deftest post-transition-mutant-produces-independent-tlc-counterexample
  (let [{:keys [trace trace-info] :as result}
        (run-recife! ::corrupt-release)
        violation (:violation trace-info)]
    (is (and (vector? trace) (seq trace))
        (pr-str (select-keys result [:trace :trace-info])))
    (is (= :invariant (:type violation)) (pr-str trace-info))
    (is (contains?
         #{:gesso.assurance.proof.recife-finite-correspondence-test/false-flag-is-preserved
           :gesso.assurance.proof.recife-finite-correspondence-test/paths-belong-to-independent-enumeration}
         (:name violation))
        (pr-str violation))))
