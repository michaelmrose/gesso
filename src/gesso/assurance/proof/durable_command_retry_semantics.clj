(ns gesso.assurance.proof.durable-command-retry-semantics
  "v1400: Small pure *abstract* command-history machine for future Recife
   exploration of atomic command identity, competing attempts, and lost replies.

   This is NOT a retry driver, production transaction executor, authenticated
   record reader, or certificate. The :physical-commit step explicitly ASSUMES
   that a single atomic command-ledger create and its domain mutation occur
   together. Tests and finite exploration of that model do not prove XTDB's
   physical refinement, request identity, or authorization.

   Distinguish a command's durable identity from each submission attempt.
   A lost response can precede or follow the abstract commit, and a later
   compatible ledger observation NEVER attributes incorporation to that
   particular attempt or grants retry/settlement permission.

   Histories contain private binding, outcome, and record data. Do not render
   histories to clients or treat caller-created histories as authenticated."
  (:require [clojure.string :as str]
            [gesso.model.command-resolution :as resolution]))

(def model-type :gesso.assurance.proof.durable-command-retry-semantics/model)
(def model-version 1)

(def assurance-scope
  {:construction model-type
   :checked #{:one-durable-incorporation-per-command-id-in-abstract-history
              :same-id-different-binding-does-not-replay
              :competing-attempts-conflict-at-abstract-create
              :response-loss-does-not-determine-physical-outcome
              :reconciliation-does-not-attribute-or-authorize-retry}
   :assumed #{:atomic-ledger-create-and-domain-mutation
              :all-writers-use-the-same-command-ledger
              :trusted-command-binding-and-authorization
              :correct-and-complete-physical-prestate-and-guard-evaluation}
   :not-established #{:xtdb-refinement-or-serializability
                      :authenticated-physical-commit
                      :complete-writer-mediation
                      :recovery-liveness
                      :retry-or-browser-settlement-authority
                      :whole-application-certification}})

(def ^:private event-keys
  {:begin #{:kind :attempt-id :binding}
   :physical-commit #{:kind :attempt-id :outcome :now}
   :physical-reject #{:kind :attempt-id}
   :response-lost #{:kind :attempt-id}
   :acknowledge #{:kind :attempt-id}
   :reconcile #{:kind :attempt-id}})

(def ^:private physical-statuses #{:undecided :committed :rejected :conflict})
(def ^:private delivery-statuses #{:awaiting :unknown :acknowledged})
(def ^:private reconciliation-statuses
  #{:not-checked :record-not-visible-noncommit-unproved
    :compatible-durable-record-attempt-unattributed
    :incompatible-binding-no-disclosure})

(defn- fail! [reason]
  (throw (ex-info "Invalid abstract durable-command history transition."
                  {:durable-command-retry/rejection true
                   :reason reason})))

(defn- attempt-id? [value]
  (or (and (string? value) (not (str/blank? value)))
      (keyword? value)
      (uuid? value)
      (and (integer? value) (pos? value))))

(defn initial-state
  "An empty logical history. Its ledger and incorporation order are MODEL
   artifacts, not a persisted state read or an XTDB transaction ledger."
  []
  {:type model-type
   :version model-version
   :ledger {}
   :attempts {}
   :incorporations []})

(defn- record-valid? [record]
  (try
    (resolution/require-record record)
    true
    (catch Exception _ false)))

(defn- binding-valid? [binding]
  (try
    (resolution/require-binding binding)
    true
    (catch Exception _ false)))

(defn invariant-violations
  "Structural and safety invariants on ONE provided abstract history.
   Returns keywords rather than payloads or raw command bindings. This
   detects broken model states; it cannot establish their physical origin."
  [state]
  (if-not (and (map? state)
               (= #{:type :version :ledger :attempts :incorporations}
                  (set (keys state)))
               (= model-type (:type state))
               (= model-version (:version state))
               (map? (:ledger state))
               (map? (:attempts state))
               (vector? (:incorporations state)))
    [:invalid-history-shape]
    (let [{:keys [ledger attempts incorporations]} state
          valid-inc? (fn [entry]
                       (and (map? entry)
                            (= #{:attempt-id :command-id :record}
                               (set (keys entry)))
                            (attempt-id? (:attempt-id entry))
                            (record-valid? (:record entry))
                            (= (:command-id entry)
                               (:xt/id (:record entry)))))
          valid-attempt? (fn [[id attempt]]
                           (and (attempt-id? id)
                                (map? attempt)
                                (= #{:binding :physical :delivery :reconciliation}
                                   (set (keys attempt)))
                                (binding-valid? (:binding attempt))
                                (physical-statuses (:physical attempt))
                                (delivery-statuses (:delivery attempt))
                                (reconciliation-statuses (:reconciliation attempt))
                                (or (not= :acknowledged (:delivery attempt))
                                    (= :committed (:physical attempt)))))
          valid-records? (every? (fn [[id rec]]
                                   (and (record-valid? rec) (= id (:xt/id rec))))
                                 ledger)
          valid-incorporations? (every? valid-inc? incorporations)
          inc-ids (mapv :command-id incorporations)
          inc-attempt-ids (mapv :attempt-id incorporations)
          current-commits (set inc-attempt-ids)
          shape? (and valid-records?
                      valid-incorporations?
                      (every? valid-attempt? attempts))]
      (cond-> []
        (not shape?) (conj :invalid-record-or-attempt)
        (and valid-incorporations?
             (or (not= (count inc-ids) (count (set inc-ids)))
                 (not= (count inc-attempt-ids) (count current-commits))))
        (conj :duplicate-incorporation)
        (and shape?
             (or (not= (set (keys ledger)) (set inc-ids))
                 (some (fn [{:keys [command-id record]}]
                         (not= record (get ledger command-id)))
                       incorporations)))
        (conj :ledger-incorporation-divergence)
        (and shape?
             (some (fn [{:keys [attempt-id command-id record]}]
                     (let [a (get attempts attempt-id)]
                       (or (not a)
                           (not= :committed (:physical a))
                           (not= command-id
                                 (resolution/record-id (:binding a)))
                           (not= (:binding a)
                                 (:gesso.command/binding record)))))
                   incorporations))
        (conj :incorporation-attempt-divergence)
        (and shape?
             (some (fn [[id a]]
                     (not= (= :committed (:physical a))
                           (contains? current-commits id)))
                   attempts))
        (conj :physical-status-incorporation-divergence)))))

(defn invariant? [state]
  (empty? (invariant-violations state)))

(defn- require-state! [state]
  (when-not (invariant? state)
    (fail! :invalid-prior-history))
  state)

(defn- require-event! [event]
  (when-not (and (map? event)
                 (contains? event-keys (:kind event))
                 (= (event-keys (:kind event)) (set (keys event)))
                 (attempt-id? (:attempt-id event)))
    (fail! :invalid-event))
  event)

(defn- require-attempt! [state id]
  (or (get-in state [:attempts id])
      (fail! :missing-attempt)))

(defn- require-undecided! [attempt]
  (when-not (= :undecided (:physical attempt))
    (fail! :physical-attempt-already-resolved))
  attempt)

(defn- record-for-commit! [binding outcome now]
  (try
    (resolution/command-record binding outcome now)
    (catch clojure.lang.ExceptionInfo _
      (fail! :invalid-incorporation-record))))

(defn step
  "Apply exactly ONE abstract event, returning a new immutable history.

   :begin            a distinct attempt, before any visible command record
   :physical-commit  choose one winner at the assumed atomic-create boundary;
                     competing attempts become :conflict and never rewrite
   :physical-reject  reject an undecided physical attempt with no write
   :response-lost    make delivery unknown, without changing physical state
   :acknowledge      observe an already committed attempt's positive result
   :reconcile        inspect the model ledger; no submission or disclosure

   No :retry action exists. A later :begin models ANOTHER *independently
   admitted* initial attempt; it is not permission for automated retry.
   Every call checks the input and resulting history invariants."
  [state event]
  (require-state! state)
  (require-event! event)
  (let [{:keys [kind attempt-id]} event
        existing (get-in state [:attempts attempt-id])
        next-state
        (case kind
          :begin
          (let [binding (:binding event)]
            (when existing (fail! :duplicate-attempt-id))
            (when-not (binding-valid? binding)
              (fail! :invalid-command-binding))
            (when (contains? (:ledger state) (resolution/record-id binding))
              (fail! :already-durable-reconcile-instead))
            (assoc-in state [:attempts attempt-id]
                      {:binding binding
                       :physical :undecided
                       :delivery :awaiting
                       :reconciliation :not-checked}))

          :physical-commit
          (let [attempt (require-undecided!
                         (require-attempt! state attempt-id))
                binding (:binding attempt)
                cmd-id (resolution/record-id binding)
                record (record-for-commit! binding (:outcome event) (:now event))]
            (if (contains? (:ledger state) cmd-id)
              (assoc-in state [:attempts attempt-id :physical] :conflict)
              (-> state
                  (assoc-in [:ledger cmd-id] record)
                  (assoc-in [:attempts attempt-id :physical] :committed)
                  (update :incorporations conj
                          {:attempt-id attempt-id :command-id cmd-id
                           :record record}))))

          :physical-reject
          (do (require-undecided! (require-attempt! state attempt-id))
              (assoc-in state [:attempts attempt-id :physical] :rejected))

          :response-lost
          (let [attempt (require-attempt! state attempt-id)]
            (when-not (= :awaiting (:delivery attempt))
              (fail! :response-already-classified))
            (assoc-in state [:attempts attempt-id :delivery] :unknown))

          :acknowledge
          (let [attempt (require-attempt! state attempt-id)]
            (when-not (and (= :committed (:physical attempt))
                           (= :awaiting (:delivery attempt)))
              (fail! :no-acknowledgeable-commit))
            (assoc-in state [:attempts attempt-id :delivery] :acknowledged))

          :reconcile
          (let [attempt (require-attempt! state attempt-id)
                binding (:binding attempt)
                record (get-in state [:ledger (resolution/record-id binding)])
                classification (cond
                                 (nil? record)
                                 :record-not-visible-noncommit-unproved
                                 (= binding (:gesso.command/binding record))
                                 :compatible-durable-record-attempt-unattributed
                                 :else :incompatible-binding-no-disclosure)]
            (assoc-in state [:attempts attempt-id :reconciliation]
                      classification)))]
    (when-not (invariant? next-state)
      (fail! :internal-transition-broke-invariant))
    next-state))

(defn retry-diagnostic
  "A REDACTED summary of one model attempt. Matching ledger state is NOT an
   authenticated storage read and does not establish which attempt committed."
  [state attempt-id]
  (require-state! state)
  (let [attempt (require-attempt! state attempt-id)]
    {:type :gesso.assurance.proof.durable-command-retry-semantics/diagnostic
     :version model-version
     :physical-model-status (:physical attempt)
     :delivery-model-status (:delivery attempt)
     :reconciliation-model-status (:reconciliation attempt)
     :safe-to-retry? false
     :safe-to-settle? false
     :same-attempt-caused-commit-authenticated? false
     :physical-refinement :not-established
     :proof-status :abstract-model-diagnostic-only
     :certificate :not-issued
     :authority :none}))
