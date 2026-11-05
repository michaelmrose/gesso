(ns gesso.assurance.proof.transaction-bound-evidence-acquisition
  "v1373: One-attempt process-local acquisition from the ORIGINAL
   gesso.model.resolved-operation transaction observation hook, bound to
   the current v1368 source-checked writer catalog.

   Install :observe! through resolved/observation-context-key into a TRUSTED
   resolved-operation :submit call. After that call settles, invoke :complete!
   exactly once. No transaction is submitted or modified here. The hook runs
   immediately before tx/transact! and when its physical outcome is classified.

   Missing, duplicate, malformed, mismatched, failed, or nondefinitive events
   never yield a completed report. The original executor ISOLATES observer
   exceptions from authoritative transaction outcomes; this session retains
   those errors and refuses completion. Captured events remain COPYABLE,
   application-injectable observations: they cannot authenticate XTDB's actual
   submission, transaction-internal reads, real writer completeness, or
   deployed handler identity. No certificate or authority is issued.

   Reports may contain sensitive plan/command data; retain them only within
   trusted server-side diagnostics, never in client responses or logs."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.executable-writer-correspondence-obligations
             :as writers]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx-model :as model]))

(def acquisition-version 1)
(def acquisition-type
  :gesso.assurance.proof.transaction-bound-evidence-acquisition/attempt)
(def ^:private source-keys #{:writer-source :operation-id :binding})
(def ^:private prepared-keys
  #{:scope :phase :attempt-id :operation :binding :record :plan})
(def ^:private outcome-keys
  (conj prepared-keys :physical/status :physical/source))
(def ^:private allowed-physical
  #{[:committed :gesso-model-transaction-ack]
    [:committed :gesso-live-classified-post-commit-failure]
    [:rejected :xtdb-explicit-assertion-failure]
    [:indeterminate :unclassified-transaction-exception]
    [:indeterminate :unrecognized-transaction-response]})
(def ^:private original-writer-check
  writers/derive-executable-writer-obligations!)
(def ^:private immutable-open
  #{:authenticated-submitted-payload
    :authenticated-transaction-internal-prestate
    :complete-executed-physical-write-footprint
    :transaction-commit-and-conflict-outcome-authenticity
    :deployed-handler-and-route-adoption
    :all-physical-writer-paths-covered
    :independent-abstract-to-physical-semantics-equivalence
    :universal-physical-execution-refinement})

(defn- reject! [reason at]
  ;; Deliberately exclude bindings, plans, receipts, and domain documents.
  (throw (ex-info "Invalid transaction-bound acquisition event."
                  {:transaction-bound-evidence-acquisition/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x expected at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= expected (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- checked-writer! [{:keys [writer-source operation-id binding]}]
  (when-not (identical? original-writer-check
                        (var-get #'writers/derive-executable-writer-obligations!))
    (reject! :substituted-writer-checker []))
  (resolution/require-binding binding)
  (when-not (and (keyword? operation-id)
                 (= operation-id (:operation binding)))
    (reject! :operation-binding-mismatch [:binding]))
  (let [r (original-writer-check writer-source)
        row (first (filter #(= operation-id (:operation-id %))
                           (:operation-rows r)))]
    (when-not (and (= writers/report-type (:type r))
                   (= writers/report-version (:version r))
                   (= :fresh-v1366-source-rechecked-and-obligations-derived
                      (:checking r))
                   (= (set (map :operation-id (:operation-rows r)))
                      (:operation-ids r))
                   (= 9 (count (:required-obligation-ids r)))
                   (= :obligations-only (:proof-status r))
                   (= :none (:authority r))
                   (= :not-issued (:certificate r))
                   (= :not-established (:physical-snapshot-authenticity r))
                   (= :not-established (:physical-write-path-completeness r))
                   (some? row)
                   (:declared-abstract-operation? row)
                   (:checked-issued-executor? row)
                   (= :not-established (:cross-source-transition-equivalence row))
                   (= :not-established (:executable-refinement row))
                   (= (:required-obligation-ids r)
                      (set (map :id (:obligations row))))
                   (every? #(= :open (:status %)) (:obligations row)))
      (reject! :unbound-or-unsupported-issued-writer [:operation-id]))
    {:ledger r :row row}))

(defn- checked-event! [event binding operation]
  (when-not (plain-map? event)
    (reject! :invalid-event [:event]))
  (let [phase (:phase event)
        expected (case phase
                   :prepared prepared-keys
                   :physical-outcome (if (contains? event :physical/receipt)
                                       (conj outcome-keys :physical/receipt)
                                       outcome-keys)
                   (reject! :unexpected-event-phase [:event :phase]))]
    (exact! event expected [:event])
    (when-not (and (= resolved/execution-observation-scope (:scope event))
                   (uuid? (:attempt-id event))
                   (= operation (:operation event))
                   (= binding (:binding event)))
      (reject! :event-identity-mismatch [:event]))
    (when-not (= binding (:gesso.command/binding
                          (resolution/require-record (:record event))))
      (reject! :event-record-binding-mismatch [:event :record]))
    (let [plan (model/admitted-plan (:plan event))
          record-target [resolution/command-entity-type
                         (:xt/id (:record event))]
          record-writes (filterv #(= resolution/command-entity-type
                                    (first (command/target %))) (:commands plan))]
      (when-not (and (= 1 (count record-writes))
                     (command/create? (first record-writes))
                     (= record-target (command/target (first record-writes)))
                     (= (:record event) (command/after (first record-writes))))
        (reject! :mismatched-command-plan [:event :plan])))
    (when (= phase :physical-outcome)
      (when-not (contains? allowed-physical
                           [(:physical/status event) (:physical/source event)])
        (reject! :unclassified-physical-outcome [:event :physical/status]))
      (when (and (= :rejected (:physical/status event))
                 (contains? event :physical/receipt))
        (reject! :rejected-attempt-with-receipt [:event :physical/receipt])))
    event))

(defn start-acquisition!
  "Recheck current source and prepare one process-local observation session.

   Source: {:writer-source <original v1366 source>
            :operation-id <checked operation keyword>
            :binding <canonical Gesso command binding>}.

   Returns {:observe! f :complete! f}. Attach observe! to a trusted invocation
   context under resolved/observation-context-key, then call complete! after
   the physical attempt settles. Observe! never submits a transaction. A
   nonphysical replay has no prepared event and cannot yield evidence.
   Report copies are diagnostics, not authentications of their origin."
  [source]
  (exact! source source-keys [])
  (let [{:keys [operation-id binding]} source
        {:keys [ledger row]} (checked-writer! source)
        state (atom {:phase :awaiting-prepared})
        observe! (fn [event]
                   (try
                     (locking state
                       (let [{:keys [phase prepared failure]} @state]
                         (when failure
                           (reject! :session-poisoned [:event]))
                         (when (#{:closed :ready} phase)
                           (reject! :unexpected-extra-event [:event]))
                         (checked-event! event binding operation-id)
                         (case (:phase event)
                           :prepared
                           (do
                             (when-not (= :awaiting-prepared phase)
                               (reject! :duplicate-prepared-event [:event]))
                             (reset! state {:phase :awaiting-outcome
                                            :prepared event}))
                           :physical-outcome
                           (do
                             (when-not (= :awaiting-outcome phase)
                               (reject! :outcome-without-prepared [:event]))
                             (when-not (= (dissoc prepared :phase)
                                          (apply dissoc event
                                                 [:phase :physical/status
                                                  :physical/source :physical/receipt]))
                               (reject! :event-pair-mismatch [:event]))
                             (reset! state {:phase :ready
                                            :prepared prepared :outcome event})))))
                     (catch Throwable error
                       ;; The real Gesso executor intentionally swallows
                       ;; observer exceptions; poison the independent session
                       ;; so swallowed failures cannot yield evidence.
                       (locking state (swap! state assoc :failure true))
                       (throw error))))
        complete! (fn []
                    (locking state
                      (let [{:keys [phase failure prepared outcome]} @state]
                        (swap! state assoc :phase :closed)
                        (when failure
                          (reject! :observer-failure-poisoned-session []))
                        (when-not (= :ready phase)
                          (reject! :incomplete-physical-attempt []))
                        (when (= :indeterminate (:physical/status outcome))
                          (reject! :nondefinitive-physical-outcome []))
                        (let [status (:physical/status outcome)]
                          {:type acquisition-type
                           :version acquisition-version
                           :checking :process-local-correlated-resolved-operation-events
                           :scope :one-injected-hook-attempt
                           :operation-id operation-id
                           :attempt-id (:attempt-id prepared)
                           :command-binding binding
                           :captured-prepared-plan (:plan prepared)
                           :captured-command-record (:record prepared)
                           :captured-physical-status status
                           :captured-physical-source (:physical/source outcome)
                           :captured-physical-receipt
                           (:physical/receipt outcome)
                           :reported-receipt-present?
                           (contains? outcome :physical/receipt)
                           :issued-application-fingerprint
                           (:application-catalog-fingerprint ledger)
                           :issued-plan-transition-fingerprint
                           (:issued-plan-transition-fingerprint row)
                           :independent-transition-fingerprint
                           (:abstract-transition-fingerprint row)
                           :cross-source-transition-equivalence :not-established
                           :captured-plan-to-issued-plan-correspondence :not-established
                           :submitted-xtdb-ops-equivalence :not-established
                           :host-snapshot-fresh-at-completion :not-established
                           :observation-origin :injected-process-local-hook
                           :hook-origin-authenticated? false
                           :actual-submitted-payload-provenance :not-established
                           :transaction-internal-state-provenance :not-established
                           :transaction-id-authenticated? false
                           :complete-write-footprint-provenance :not-established
                           :all-physical-writer-paths-enumerated :not-established
                           :universal-execution-refinement :not-established
                           :operation-obligations (:obligations row)
                           :open-obligations (set/union immutable-open
                                                        (:open-obligations ledger))
                           :proof-status :hook-observation-diagnostic-only
                           :certificate :not-issued
                           :admission :not-admitted
                           :transaction-authority :none
                           :authority :none}))))]
    {:observe! observe! :complete! complete!}))
