(ns gesso.model.command-resolution-formal-test
  "Finite TLC checks for Gesso's generic atomic command-incorporation plans.

   A canonical command-resolution/incorporation-plan appends the reserved
   command-result CREATE to the SAME ordinary transaction plan as its domain
   update. Each model transition is executed by formal-transition/transition,
   which delegates to the existing tx-model/step reference semantics.

   The tested situations are intentionally domain-neutral:
   * two different updates to disjoint documents reuse one command ID;
   * two different command IDs compete over one versioned document.

   A negative control intentionally removes the required command-record CREATE
   from one normalized plan before admission. TLC must find a counterexample.
   This mutation is NOT a supported way to construct an application operation.

   Scope: finite, serial, two already-prepared plan attempts, no retries,
   observation delivery, arbitrary SQL, policy verification, browser effects,
   or claimed XTDB linearizability. The formal invariants are independent of
   the transition implementation; checking the reference implementation is
   not a proof of its correspondence to real XTDB execution."
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.formal-transition :as formal]
   [gesso.model.tx-model :as reference]
   [recife.core :as recife]
   [recife.helpers :as helpers]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private table :formal-units)
(def ^:private now :t1)

(defn- initial-document [id]
  {:xt/id id
   :revision 0
   :created-at :t0
   :updated-at :t0
   :value :unmodified})

(defn- command-binding [command-id principal document-id value]
  {:command-id (identity/command-id command-id)
   :principal (identity/principal principal)
   :operation :unit/update
   :arguments {:document-id document-id :value value}})

(defn- incorporation [document binding value]
  (resolution/incorporation-plan
   {:binding binding
    :outcome {:status :applied :value value}
    :now now
    :plan {:commands [(command/update-command
                       table :update document
                       (-> document
                           (assoc :value value :updated-at now)
                           (update :revision inc))
                       version)]
           :guards []
           :emit false}}))

(defn- scenario [same-command-id?]
  (let [left (initial-document :unit-a)
        right (if same-command-id?
                (initial-document :unit-b)
                left)
        left-binding (command-binding "command-a" :principal-a (:xt/id left) :alpha)
        right-binding (command-binding (if same-command-id? "command-a" "command-b")
                               :principal-b (:xt/id right) :beta)
        attempts [(incorporation left left-binding :alpha)
                  (incorporation right right-binding :beta)]
        documents (cond-> {[table (:xt/id left)] left}
                    same-command-id?
                    (assoc [table (:xt/id right)] right))]
    {:documents documents
     :attempts attempts
     :subject (formal/prepared-subject documents (mapv :plan attempts))}))

(def ^:private shared-id (scenario true))
(def ^:private shared-document (scenario false))

;; Deliberately broken assembly, formed from a Gesso-normalized plan. Dropping
;; the reserved record CREATE must make an otherwise independent domain update
;; eligible to incorporate despite reusing the same command identity.
(def ^:private missing-record
  (let [attempts (:attempts shared-id)
        broken (update-in (second attempts) [:plan :commands] pop)
        attempts (assoc attempts 1 broken)]
    (assoc shared-id
           :attempts attempts
           :subject (formal/prepared-subject (:documents shared-id)
                                             (mapv :plan attempts)))))

;; This Recife version cannot serialize Clojure strings into TLC values.
;; The normal Gesso command ID is nevertheless REQUIRED to be a string or UUID.
;; Only these fixture-owned string command IDs cross the backend boundary;
;; reversible keyword tokens are *not* admitted as application command IDs.
;; Decode before each real Gesso transition and encode only afterwards.
(def ^:private to-token
  {"command-a" ::command-a
   "command-b" ::command-b})
(def ^:private from-token
  (into {} (map (fn [[raw token]] [token raw])) to-token))

(defn- encode-strings [value]
  (walk/postwalk (fn [node]
                   (if (string? node)
                     (or (get to-token node)
                         (throw (ex-info "Unknown string in bounded Recife fixture."
                                         {:value node})))
                     node))
                 value))

(defn- decode-strings [value]
  (walk/postwalk (fn [node] (get from-token node node)) value))

(defn- map-model-state [state transform]
  (-> state
      (update ::formal/documents transform)
      (update ::formal/observations transform)))

(def ^:private cases
  {::shared-id shared-id
   ::shared-document shared-document
   ::missing-record missing-record})

(defn- case-for [state]
  (get cases (::case state)))

(defn- initial-state [case-key]
  (-> (formal/initial-state (:subject (get cases case-key)))
      (map-model-state encode-strings)
      (assoc ::case case-key)))

(defn- attempt [state index]
  (let [subject (:subject (case-for state))
        action (nth (:actions subject) index)]
    (if (some #(= action %) (formal/enabled-actions subject state))
      (-> (formal/transition subject
                             (map-model-state state decode-strings) action)
          (map-model-state encode-strings))
      state)))

(recife/defproc attempt-first (fn [state] (attempt state 0)))
(recife/defproc attempt-second (fn [state] (attempt state 1)))

(defn- record-target [record]
  [resolution/command-entity-type
   (resolution/record-id (:gesso.command/binding record))])

(defn- encoded-record-target [record]
  (encode-strings (record-target record)))

(defn- documented-record-for [case-data index]
  (:record (nth (:attempts case-data) index)))

(helpers/definvariant at-most-one-incorporation [state]
  (<= (count (filter #(= :committed (:status %))
                     (::formal/observations state)))
      1))

(helpers/definvariant rejected-attempt-is-atomic [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= status :rejected) (= before after)))
          (::formal/observations state)))

(helpers/definvariant incorporated-record-is-coupled [state]
  (let [case-data (case-for state)]
    (every?
     (fn [{:keys [plan-index status after]}]
       (or (not= status :committed)
           (let [expected (documented-record-for case-data plan-index)]
             (= (encode-strings expected)
                (get after (encoded-record-target expected))))))
     (::formal/observations state))))

(helpers/definvariant stored-records-match-committed-attempts [state]
  (let [case-data (case-for state)
        commits (filter #(= :committed (:status %))
                        (::formal/observations state))
        expected (into {}
                       (map (fn [{:keys [plan-index]}]
                              (let [record (documented-record-for case-data plan-index)]
                                [(encoded-record-target record) (encode-strings record)])))
                       commits)
        actual (into {}
                     (filter (fn [[target _]]
                               (= resolution/command-entity-type (first target))))
                     (::formal/documents state))]
    (= expected actual)))

(helpers/definvariant observations-are-a-state-chain [state]
  (let [observations (::formal/observations state)]
    (and (or (empty? observations)
             (= (encode-strings (:documents (case-for state)))
                (:before (first observations))))
         (every? (fn [[before after]]
                   (= (:after before) (:before after)))
                 (partition 2 1 observations))
         (= (::formal/documents state)
            (if (seq observations)
              (:after (peek observations))
              (encode-strings (:documents (case-for state))))))))

(def ^:private components
  #{attempt-first attempt-second
    at-most-one-incorporation
    rejected-attempt-is-atomic
    incorporated-record-is-coupled
    stored-records-match-committed-attempts
    observations-are-a-state-chain})

(defn- check! [case-key]
  ;; Two completed attempts are terminal: do not require infinite progress.
  (let [result (recife/run-model (initial-state case-key)
                                 components
                                 {:no-deadlock true :workers 1})]
    (if (instance? clojure.lang.IDeref result) @result result)))

(defn- apply-order [case-data order]
  (reduce (fn [state index]
            (formal/transition (:subject case-data)
                               state
                               (nth (:actions (:subject case-data)) index)))
          (formal/initial-state (:subject case-data))
          order))

(deftest bounded-backend-codec-round-trips-test
  (testing "checker-facing records round-trip to exactly the admitted Gesso input"
    (doseq [case-data [shared-id shared-document missing-record]
            attempt (:attempts case-data)]
      (let [original (:record attempt)]
        (is (= original (decode-strings (encode-strings original))))
        (is (= (record-target original)
               (decode-strings (encoded-record-target original))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (encode-strings {:unknown "not-in-fixture"})))))

(deftest canonical-command-record-plans-are-admitted-test
  (doseq [[description case-data]
          [["shared command ID / different domain documents" shared-id]
           ["different IDs / same domain document" shared-document]]]
    (testing description
      (let [subject (:subject case-data)
            expected (reference/explore-orders (:documents case-data)
                                               (mapv :plan (:attempts case-data)))]
        (is (= 2 (count (:actions subject))))
        (is (= 2 (count (:paths expected))))
        (doseq [{:keys [order trace final-state]} (:paths expected)]
          (let [state (apply-order case-data order)
                observed (::formal/observations state)]
            (is (= [:committed :rejected] (mapv :status observed)))
            (is (= (mapv :status trace) (mapv :status observed)))
            (is (= final-state (::formal/documents state)))
            (is (empty? (formal/enabled-actions subject state)))
            (let [committed (first observed)
                  winning-record (documented-record-for case-data
                                                         (:plan-index committed))
                  record (get (::formal/documents state)
                              (record-target winning-record))]
              (is (= winning-record record))
              (is (= :replay
                     (:status (resolution/reconcile
                               (:gesso.command/binding winning-record)
                               record (fn [_ _] true)))))
              (is (= :unavailable
                     (:status (resolution/reconcile
                               (:gesso.command/binding winning-record)
                               record (fn [_ _] false)))))
              (is (= :incompatible
                     (:status (resolution/reconcile
                               (:gesso.command/binding
                                (documented-record-for case-data
                                                       (:plan-index (second observed))))
                               record (fn [_ _] true))))))))))))

(deftest actual-command-incorporation-passes-tlc-test
  (doseq [[label case-key]
          [["shared command ID" ::shared-id]
           ["shared document" ::shared-document]]]
    (testing label
      (let [{:keys [trace trace-info distinct-states] :as result} (check! case-key)]
        (is (= :ok trace) (pr-str (select-keys result [:trace :trace-info])))
        (is (nil? trace-info))
        (is (= 5 distinct-states))))))

(deftest missing-command-record-yields-tlc-counterexample-test
  (let [{:keys [trace trace-info] :as result} (check! ::missing-record)
        violation (:violation trace-info)]
    (is (and (vector? trace) (seq trace))
        (pr-str (select-keys result [:trace :trace-info])))
    (is (= :invariant (:type violation)) (pr-str trace-info))
    (is (contains?
         #{:gesso.model.command-resolution-formal-test/incorporated-record-is-coupled
           :gesso.model.command-resolution-formal-test/stored-records-match-committed-attempts
           :gesso.model.command-resolution-formal-test/at-most-one-incorporation}
         (:name violation))
        (pr-str trace-info))))
