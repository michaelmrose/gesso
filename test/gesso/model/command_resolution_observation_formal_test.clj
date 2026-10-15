(ns gesso.model.command-resolution-observation-formal-test
  "Finite Recife/TLC comparison of a real, controlled XTDB command-incorporation
   observation with Gesso's existing serial reference transition semantics.

   The physical observations come from resolution-observation/observe-and-check!,
   which submits canonical prepared plans through gesso.model.tx/prepare and
   xtdb.api/execute-tx and reads actual persisted documents. Recife explores
   both serial orders of the SAME prepared plans using formal-transition,
   which delegates to tx-model/step. On the terminal path matching the actual
   submission order, TLC requires matching indexed commit/reject results and
   exact persisted documents, not merely a plausible outcome label.

   The controlled collector assumes exclusive access to relevant database
   documents and consistent unpinned XTDB reads; this test is NOT evidence of
   production execution provenance, general concurrency linearizability,
   application policy correctness or universal implementation refinement.
   The corrupted-observation variant must fail model checking. No physical
   transaction runs inside a TLC transition or invariant."
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
                           (:observations physical)])]
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
