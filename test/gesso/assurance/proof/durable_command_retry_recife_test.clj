(ns gesso.assurance.proof.durable-command-retry-recife-test
  "v1401: Bounded Recife/TLC interleavings of v1400's PURE durable-command
   retry semantics. The transition under test is sem/step, not an independent
   implementation of atomic incorporation. Invariants below additionally
   inspect the resulting model ledger, attempts, and causal redaction.

   Two attempt IDs, at most one beginning for each, at most one physical
   resolution and delivery loss for each, plus bounded acknowledgement and
   reconciliation. The three fixtures explore equal command bindings,
   incompatible reuse of one command ID, and independent command IDs.
   Inadmissible events stutter; they are not recorded as valid transitions.

   A deliberate post-transition duplicate-incorporation mutant must yield a
   TLC counterexample. This is finite abstract safety checking, NOT proof of
   XTDB atomicity, complete writer mediation, fairness, retry liveness, or
   permission to resubmit or settle an ambiguous browser command."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [gesso.assurance.proof.durable-command-retry-semantics :as sem]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [recife.core :as recife]
            [recife.helpers :as helpers]))

(def ^:private shared-id "recife-shared-command")
(def ^:private other-id "recife-other-command")
(def ^:private string->token
  {shared-id ::shared-id other-id ::other-id})
(def ^:private token->string
  (into {} (map (fn [[s k]] [k s]) string->token)))

(defn- encode-history [history]
  ;; The TLC codec in this installed Recife version does not accept arbitrary
  ;; string IDs in the model state. Restrict conversion to these two fixture
  ;; values; an unexpected string cannot silently become a different ID.
  (walk/postwalk
   (fn [v]
     (if (string? v)
       (or (get string->token v)
           (throw (ex-info "Unexpected string in Recife history." {:value v})))
       v))
   history))

(defn- decode-history [history]
  (walk/postwalk (fn [v] (get token->string v v)) history))

(defn- command-binding [id arguments]
  {:command-id (identity/command-id id)
   :principal (identity/principal :actor-a)
   :operation :request/claim
   :arguments arguments})

(def ^:private first-binding (command-binding shared-id {:request :r1}))
(def ^:private same-binding first-binding)
(def ^:private incompatible-binding (command-binding shared-id {:request :r2}))
(def ^:private independent-binding (command-binding other-id {:request :r2}))

(defn- second-binding [scenario]
  (case scenario
    :same same-binding
    :incompatible incompatible-binding
    :independent independent-binding))

(defn- initial-state [scenario mode]
  {::history (encode-history (sem/initial-state))
   ::scenario scenario
   ::mode mode})

(defn- transition [state event]
  (let [history (decode-history (::history state))]
    (try
      (let [next-history (sem/step history event)
            ;; Deliberate *after sem/step* corruption: the real interpreter
            ;; remains the subject of the successful transition.
            next-history (if (and (= :duplicate-incorporation (::mode state))
                                  (= :physical-commit (:kind event))
                                  (> (count (:incorporations next-history))
                                     (count (:incorporations history))))
                           (update next-history :incorporations
                                   conj (peek (:incorporations next-history)))
                           next-history)]
        (assoc state ::history (encode-history next-history)))
      (catch clojure.lang.ExceptionInfo e
        ;; Invalid preconditions mean the event is disabled; never swallow an
        ;; unexpected error in the model or the codec as an ordinary stutter.
        (if (:durable-command-retry/rejection (ex-data e))
          state
          (throw e))))))

(defn- begin [state id]
  (transition state
              {:kind :begin :attempt-id id
               :binding (if (= id :first)
                          first-binding
                          (second-binding (::scenario state)))}))

(defn- event [state kind id]
  (transition state
              (cond-> {:kind kind :attempt-id id}
                (= kind :physical-commit)
                (assoc :outcome {:status :applied} :now :t1))))

(recife/defproc begin-first (fn [state] (begin state :first)))
(recife/defproc begin-second (fn [state] (begin state :second)))
(recife/defproc commit-first (fn [state] (event state :physical-commit :first)))
(recife/defproc commit-second (fn [state] (event state :physical-commit :second)))
(recife/defproc reject-first (fn [state] (event state :physical-reject :first)))
(recife/defproc reject-second (fn [state] (event state :physical-reject :second)))
(recife/defproc lose-first-response (fn [state] (event state :response-lost :first)))
(recife/defproc lose-second-response (fn [state] (event state :response-lost :second)))
(recife/defproc acknowledge-first (fn [state] (event state :acknowledge :first)))
(recife/defproc acknowledge-second (fn [state] (event state :acknowledge :second)))
(recife/defproc reconcile-first
  (fn [state]
    (let [history (decode-history (::history state))]
      ;; A single abstract read per attempt. Repeated authority reads are a
      ;; separate unbounded protocol, not an implicit model-checker promise.
      (if (= :not-checked (get-in history [:attempts :first :reconciliation]))
        (event state :reconcile :first)
        state))))
(recife/defproc reconcile-second
  (fn [state]
    (let [history (decode-history (::history state))]
      (if (= :not-checked (get-in history [:attempts :second :reconciliation]))
        (event state :reconcile :second)
        state))))

(helpers/definvariant one-incorporation-per-command-id [state]
  (let [ids (map :command-id (get-in state [::history :incorporations]))]
    (= (count ids) (count (set ids)))))

(helpers/definvariant incorporated-records-match-attempts [state]
  (let [{:keys [ledger attempts incorporations]}
        (decode-history (::history state))]
    (and (= (set (keys ledger))
            (set (map :command-id incorporations)))
         (every?
          (fn [{:keys [attempt-id command-id record]}]
            (let [attempt (get attempts attempt-id)]
              (and (some? attempt)
                   (= :committed (:physical attempt))
                   (= command-id (resolution/record-id (:binding attempt)))
                   (= record (get ledger command-id))
                   (= (:binding attempt) (:gesso.command/binding record)))))
          incorporations)
         (every?
          (fn [[id {:keys [physical]}]]
            (= (= :committed physical)
               (boolean (some #(= id (:attempt-id %)) incorporations))))
          attempts))))

(helpers/definvariant unknown-delivery-never-grants-authority [state]
  (let [history (decode-history (::history state))]
    ;; Mutated histories are checked by independent ledger invariants above.
    ;; Do not turn a rejected model state into a checker exception here.
    (or (not (sem/invariant? history))
        (every?
         (fn [[id _]]
           (let [diagnostic (sem/retry-diagnostic history id)]
             (and (false? (:safe-to-retry? diagnostic))
                  (false? (:safe-to-settle? diagnostic))
                  (false? (:same-attempt-caused-commit-authenticated? diagnostic))
                  (= :none (:authority diagnostic))
                  (= :not-issued (:certificate diagnostic)))))
         (:attempts history)))))

(helpers/definvariant delivery-cannot-rewrite-physical-commit [state]
  (let [attempts (get-in state [::history :attempts])
        incorporations (get-in state [::history :incorporations])]
    (every?
     (fn [[id attempt]]
       (and (or (not= :acknowledged (:delivery attempt))
                (= :committed (:physical attempt)))
            (or (not= :committed (:physical attempt))
                (some #(= id (:attempt-id %)) incorporations))))
     attempts)))

(def ^:private model-components
  #{begin-first begin-second commit-first commit-second reject-first reject-second
    lose-first-response lose-second-response acknowledge-first acknowledge-second
    reconcile-first reconcile-second one-incorporation-per-command-id
    incorporated-records-match-attempts unknown-delivery-never-grants-authority
    delivery-cannot-rewrite-physical-commit})

(defn- check! [scenario mode]
  (let [result (recife/run-model
                (initial-state scenario mode) model-components
                {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(deftest model-vocabulary-and-codec-are-closed
  (is (= 16 (count model-components)))
  (doseq [scenario [:same :incompatible :independent]]
    (let [s (-> (initial-state scenario :correct)
                (begin :first)
                (begin :second))
          decoded (decode-history (::history s))]
      (is (sem/invariant? decoded))
      (is (= 2 (count (:attempts decoded))))
      (is (= s (update s ::history
                       (comp encode-history decode-history))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (encode-history {:new-id "unexpected"}))))

(deftest lost-response-does-not-undo-incorporation
  (let [s0 (-> (initial-state :same :correct)
               (begin :first) (begin :second))
        s1 (-> s0 (event :response-lost :first)
               (event :physical-commit :first)
               (event :physical-commit :second)
               (event :reconcile :second))
        h (decode-history (::history s1))]
    (is (sem/invariant? h))
    (is (= 1 (count (:incorporations h))))
    (is (= :committed (get-in h [:attempts :first :physical])))
    (is (= :unknown (get-in h [:attempts :first :delivery])))
    (is (= :conflict (get-in h [:attempts :second :physical])))
    (is (= :compatible-durable-record-attempt-unattributed
           (get-in h [:attempts :second :reconciliation])))
    (is (false? (:safe-to-retry?
                 (sem/retry-diagnostic h :first))))))

(deftest incompatible-binding-remains-incompatible
  (let [s (-> (initial-state :incompatible :correct)
              (begin :first) (begin :second)
              (event :physical-commit :first)
              (event :reconcile :second))
        h (decode-history (::history s))]
    (is (= :incompatible-binding-no-disclosure
           (get-in h [:attempts :second :reconciliation])))
    (is (nil? (get-in (sem/retry-diagnostic h :second) [:outcome])))))

(deftest bounded-correct-retry-interleavings-pass-tlc
  (doseq [scenario [:same :incompatible :independent]]
    (testing (name scenario)
      (let [{:keys [trace trace-info distinct-states generated-states] :as result}
            (check! scenario :correct)]
        (is (= :ok trace)
            (pr-str (select-keys result [:trace :trace-info :distinct-states])))
        (is (nil? trace-info))
        (is (pos? (or distinct-states 0)))
        (is (pos? (or generated-states 0)))))))

(deftest deliberately-duplicated-incorporation-has-tlc-counterexample
  (let [{:keys [trace trace-info] :as result}
        (check! :same :duplicate-incorporation)
        violation (:violation trace-info)]
    (is (and (vector? trace) (seq trace))
        (pr-str (select-keys result [:trace :trace-info])))
    (is (= :invariant (:type violation)) (pr-str trace-info))
    (is (contains?
         #{:gesso.assurance.proof.durable-command-retry-recife-test/one-incorporation-per-command-id
           :gesso.assurance.proof.durable-command-retry-recife-test/incorporated-records-match-attempts}
         (:name violation))
        (pr-str trace-info))))
