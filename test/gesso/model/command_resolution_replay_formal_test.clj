(ns gesso.model.command-resolution-replay-formal-test
  "Bounded TLC exploration of Gesso command resolution after ambiguous delivery.

   The two competing incorporation plans are built by the actual
   command-resolution/incorporation-plan. Attempt transitions invoke
   formal-transition/transition -> tx-model/step. Read transitions invoke
   command-resolution/reconcile against a record taken from the resulting
   authoritative document map, NOT a proof-only approximation of reconcile.

   Reads are deliberately allowed before and after incorporation. A simulated
   lost response changes only the transport observation, not server truth.
   Two reads and two attempts are bounded; no network fairness or retry
   permission is inferred from an absent record. Disclosure is represented by
   an explicit test predicate, not by trusting a structurally typed principal.

   The backend cannot encode fixture string IDs, so a closed, reversible
   string<->keyword codec is used solely at the model-checker boundary.

   This checks a finite reference model, not universal XTDB linearizability,
   client/browser correspondence, real storage persistence, or liveness."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.formal-transition :as formal]
   [recife.core :as recife]
   [recife.helpers :as helpers]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private table :formal-units)
(def ^:private id "shared-command")
(def ^:private token ::shared-command)

(defn- document [key]
  {:xt/id key :revision 0 :created-at :t0 :updated-at :t0 :value :initial})

(defn- command-binding [principal key value]
  {:command-id (identity/command-id id)
   :principal (identity/principal principal)
   :operation :unit/update
   :arguments {:document-id key :value value}})

(defn- plan [doc principal value]
  (resolution/incorporation-plan
   {:binding (command-binding principal (:xt/id doc) value)
    :outcome {:status :applied :value value}
    :now :t1
    :plan {:commands [(command/update-command
                       table :update doc
                       (-> doc (assoc :value value :updated-at :t1)
                           (update :revision inc)) version)]
           :guards [] :emit false}}))

(def ^:private fixture
  (let [left (document :unit-a)
        right (document :unit-b)
        attempts [(plan left :principal-a :alpha)
                  (plan right :principal-b :beta)]
        initial {[table (:xt/id left)] left
                 [table (:xt/id right)] right}]
    {:attempts attempts
     :initial initial
     :subject (formal/prepared-subject initial (mapv :plan attempts))}))

(defn- encode [x]
  (walk/postwalk (fn [v]
                   (if (string? v)
                     (if (= id v) token
                       (throw (ex-info "Unexpected string in bounded proof input."
                                       {:value v})))
                     v)) x))

(defn- decode [x]
  (walk/postwalk (fn [v] (if (= v token) id v)) x))

(defn- transform-model [state f]
  (-> state
      (update ::formal/documents f)
      (update ::formal/observations f)))

(defn- initial-state [variant]
  (-> (formal/initial-state (:subject fixture))
      (transform-model encode)
      (assoc ::variant variant
             ::response :not-sent
             ::read-count 0
             ::readings []
             ::delivery-lost? false)))

(defn- stored-record [state]
  (get (::formal/documents state)
       [resolution/command-entity-type token]))

(defn- attempt [state index]
  (let [subject (:subject fixture)
        action (nth (:actions subject) index)]
    (if (some #{action} (formal/enabled-actions subject state))
      (let [next-state (-> (formal/transition
                            subject (transform-model state decode) action)
                           (transform-model encode))
            status (:status (peek (::formal/observations next-state)))]
        (if (= :committed status)
          (assoc next-state ::response :in-flight)
          next-state))
      state)))

(recife/defproc attempt-first (fn [state] (attempt state 0)))
(recife/defproc attempt-second (fn [state] (attempt state 1)))

(defn- drop-response [state]
  (if (and (= :in-flight (::response state))
           (not (::delivery-lost? state)))
    (assoc state ::response :unknown ::delivery-lost? true)
    state))

(recife/defproc lose-delivery (fn [state] (drop-response state)))

(defn- decide [request stored may-disclose?]
  (select-keys (resolution/reconcile request stored may-disclose?)
               [:status :outcome]))

(defn- read-authority [state]
  (if (>= (::read-count state) 2)
    state
    (let [record (some-> (stored-record state) decode)
          admitted (if record
                     (:gesso.command/binding record)
                     (:gesso.command/binding (:record (first (:attempts fixture)))))
          altered-principal (assoc admitted :principal
                                   (identity/principal :other-principal))
          altered-arguments (assoc admitted :arguments
                                   {:document-id :other-unit :value :other})
          allowed (fn [_ _] true)
          denied (fn [_ _] false)
          normal {:matching (decide admitted record allowed)
                  :changed-principal (decide altered-principal record allowed)
                  :changed-arguments (decide altered-arguments record allowed)
                  :denied (decide admitted record denied)}
          replies (if (and (= :broken (::variant state)) record)
                    ;; Deliberately defective disclosure of an incompatible reuse.
                    (assoc normal :changed-arguments (:matching normal))
                    normal)]
      (-> state
          (update ::read-count inc)
          (update ::readings conj
                  {:record (some-> record encode)
                   :documents (::formal/documents state)
                   :response (::response state)
                   :replies replies})))))

(recife/defproc read-first (fn [state]
                             (if (= 0 (::read-count state))
                               (read-authority state)
                               state)))
(recife/defproc read-second (fn [state]
                              (if (= 1 (::read-count state))
                                (read-authority state)
                                state)))

(helpers/definvariant one-incorporation-per-command-id [state]
  (<= (count (filter #(= :committed (:status %))
                     (::formal/observations state))) 1))

(helpers/definvariant record-preserved-on-rejection [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= :rejected status) (= before after)))
          (::formal/observations state)))

(helpers/definvariant authoritative-record-correspondence [state]
  (let [commits (filter #(= :committed (:status %))
                        (::formal/observations state))
        actual (stored-record state)]
    (if (empty? commits)
      (nil? actual)
      (and (= 1 (count commits))
           (= (encode (:record (nth (:attempts fixture)
                                    (:plan-index (first commits)))))
              actual)))))

(helpers/definvariant reconciliation-disclosure-contract [state]
  (every?
   (fn [{:keys [record replies documents]}]
     (let [expected-status (if record :replay :unresolved)
           expected-incompatibility (if record :incompatible :unresolved)
           expected-denied (if record :unavailable :unresolved)]
       (and (= record
               (get documents [resolution/command-entity-type token]))
            (= expected-status (get-in replies [:matching :status]))
            (= expected-incompatibility
               (get-in replies [:changed-principal :status]))
            (= expected-incompatibility
               (get-in replies [:changed-arguments :status]))
            (= expected-denied (get-in replies [:denied :status]))
            (= (when record (:gesso.command/outcome record))
               (get-in replies [:matching :outcome]))
            (every? (fn [kind] (not (contains? (get replies kind) :outcome)))
                    [:changed-principal :changed-arguments :denied]))))
   (::readings state)))

(helpers/definvariant readings-do-not-write-authority [state]
  (and (<= 0 (::read-count state) 2)
       (= (::read-count state) (count (::readings state)))
       (every?
        (fn [{:keys [record documents]}]
          (= record (get documents [resolution/command-entity-type token])))
        (::readings state))))

(helpers/definvariant lost-response-does-not-undo-commit [state]
  (or (not (::delivery-lost? state))
      (and (= :unknown (::response state))
           (some #(= :committed (:status %))
                 (::formal/observations state))
           (some? (stored-record state)))))

(def ^:private components
  #{attempt-first attempt-second lose-delivery read-first read-second
    one-incorporation-per-command-id record-preserved-on-rejection
    authoritative-record-correspondence reconciliation-disclosure-contract
    readings-do-not-write-authority lost-response-does-not-undo-commit})

(defn- check! [variant]
  (let [result (recife/run-model (initial-state variant)
                                 components
                                 {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(deftest prepared-resolution-and-backend-codec-test
  (testing "all plans are canonical and codec does not rewrite admitted records"
    (is (= 2 (count (:actions (:subject fixture)))))
    (doseq [attempt (:attempts fixture)]
      (is (= (:record attempt) (decode (encode (:record attempt))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (encode {:command-id "unexpected"})))))

(deftest unknown-delivery-and-absence-are-distinct-test
  (let [initial (initial-state :correct)
        before (read-authority initial)
        after (attempt before 0)
        unknown (drop-response after)
        observed (read-authority unknown)]
    (is (= :unresolved (get-in before [::readings 0 :replies :matching :status])))
    (is (not (contains? (get-in before [::readings 0 :replies :matching]) :outcome)))
    (is (= :unknown (::response unknown)))
    (is (some? (stored-record unknown)))
    (is (= :replay (get-in observed [::readings 1 :replies :matching :status])))
    (is (= {:status :applied :value :alpha}
           (get-in observed [::readings 1 :replies :matching :outcome])))
    (is (= (::formal/documents unknown) (::formal/documents observed)))
    (is (= 1 (count (filter #(= :committed (:status %))
                            (::formal/observations observed)))))))

(deftest canonical-reconciliation-machine-passes-tlc-test
  (let [{:keys [trace trace-info distinct-states] :as result} (check! :correct)]
    (is (= :ok trace) (pr-str (select-keys result [:trace :trace-info])))
    (is (nil? trace-info))
    (is (pos? distinct-states))))

(deftest incompatible-disclosure-mutation-produces-tlc-counterexample-test
  (let [{:keys [trace trace-info] :as result} (check! :broken)
        violation (:violation trace-info)]
    (is (and (vector? trace) (seq trace))
        (pr-str (select-keys result [:trace :trace-info])))
    (is (= :invariant (:type violation)) (pr-str trace-info))
    (is (= :gesso.model.command-resolution-replay-formal-test/reconciliation-disclosure-contract
           (:name violation)) (pr-str trace-info))))
