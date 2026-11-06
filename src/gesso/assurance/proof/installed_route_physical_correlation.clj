(ns gesso.assurance.proof.installed-route-physical-correlation
  "v1480: One-shot correlation of the v1478 adopted Ring handler, its
   source-checked issued executor and an XTDB-adapter call-site observation.

   Unlike the v1471 fresh-handler diagnostic, this invokes the exact handler
   currently recorded in the v1478 Biff system slot. It checks the original
   closed application / issued route / plan source, selects the certified
   literal route by the actual request coordinate, and invokes the installed
   guarded handler ONCE inside both the resolved-operation attempt observer
   and the XTDB adapter's actual call-site capture. The canonical prepared
   plan is independently translated by tx/prepare and compared to the actual
   adapter arguments. A definitive execute-tx TransactionKey and a matching
   committed resolved-operation result are required for positive local
   correlation. No second transaction is performed to establish the result.

   This is a controlled, privileged IN-PROCESS DIAGNOSTIC, not a transparent
   production Ring middleware. The special observer is added to a new request
   map supplied to the installed handler. Application callbacks can inspect
   that map and observer; their purity, honesty and authorization are OPEN.
   An installed handler can be captured earlier by an HTTP server or replaced
   later, and a copied Biff system map does not authenticate deployment.
   The bound XTDB API, receipt authenticity, transaction-internal prestate,
   all physical writers and universal refinement remain NOT ESTABLISHED.
   Returned execution values and exceptions may contain private information;
   neither the report nor a live witness is transaction authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as contracts]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx])
  (:import (java.util WeakHashMap)
           (java.time Instant)
           (xtdb.api TransactionKey)))

(def report-type
  :gesso.assurance.proof.installed-route-physical-correlation/local-single-dispatch)
(def report-version 1)

(deftype InstalledPhysicalHandle []
  Object
  (toString [_] "#<gesso.assurance/installed-route-physical-observation>"))

(def ^:private ^WeakHashMap observations (WeakHashMap.))
(def ^:dynamic ^:private *observing* false)
(def ^:private original-host-check host/require-current-biff-host-adoption!)
(def ^:private original-contract-check contracts/check-certified-contracts!)
(def ^:private original-operation-summary app/operation-summary)
(def ^:private original-capture adapter/capture-tx-api-calls!)
(def ^:private original-prepare tx/prepare)

(def ^:private open-obligations
  #{:actual-http-server-consumption-not-authenticated
    :server-may-have-captured-earlier-handler
    :later-handler-or-route-replacement-outside-checked-system
    :request-observer-visible-to-application-callbacks
    :callback-purity-and-input-authorization-not-proved
    :observer-and-adapter-hook-integrity-not-authenticated
    :actual-xtdb-api-and-transaction-receipt-not-authenticated
    :atomic-transaction-internal-prestate-and-guards
    :complete-physical-writer-coverage
    :universal-execution-refinement})

(defn- reject! [reason]
  ;; Never include source, private request data, operations or response.
  (throw (ex-info "Invalid installed-route physical correlation."
                  {:installed-route-physical-correlation/rejection true
                   :reason reason})))

(defn- pinned-boundaries! []
  (when-not (and (identical? original-host-check
                             (var-get #'host/require-current-biff-host-adoption!))
                 (identical? original-contract-check
                             (var-get #'contracts/check-certified-contracts!))
                 (identical? original-operation-summary
                             (var-get #'app/operation-summary))
                 (identical? original-capture
                             (var-get #'adapter/capture-tx-api-calls!))
                 (identical? original-prepare
                             (var-get #'tx/prepare)))
    (reject! :substituted-correlation-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- current-route! [host-handle source system request]
  (pinned-boundaries!)
  (when-not (and (plain-map? source)
                 (= #{:application-assembly :semantic-route-handlers
                      :certified-operations}
                    (set (keys source))))
    (reject! :invalid-contract-source))
  (when-not (and (plain-map? request)
                 (keyword? (:request-method request))
                 (string? (:uri request))
                 (not (contains? request resolved/observation-context-key))
                 (not (contains? request
                                 resolved/construction-input-context-key)))
    (reject! :invalid-request-or-preexisting-observer))
  (original-host-check host-handle source system)
  (let [checked (original-contract-check source)
        summary (original-operation-summary
                 (:operation-acquisition-assembly (:application-assembly source)))
        matches (for [[operation entry] summary
                      route (:routes entry)
                      :when (= [(:method route) (:path route)]
                               [(:request-method request) (:uri request)])]
                  {:operation operation :route-id (:route-id route)})
        match (when (= 1 (count matches)) (first matches))]
    (when-not match (reject! :unmatched-or-ambiguous-route))
    (when-not (and (contains? (:matched-certified-route-ids checked)
                              (:route-id match))
                   (contains? (:certified-operations source) (:operation match))
                   (fn? (get-in source [:semantic-route-handlers (:route-id match)])))
      (reject! :uncertified-or-missing-issued-route))
    {:operation (:operation match)
     :route-id (:route-id match)
     :contract checked
     :host-handler (get system app/biff-ring-handler-key)}))

(defn- resolved-pair [events operation]
  (when (and (vector? events) (= 2 (count events)))
    (let [[prepared outcome] events]
      (when (and (plain-map? prepared) (plain-map? outcome)
                 (= resolved/execution-observation-scope (:scope prepared)
                    (:scope outcome))
                 (= operation (:operation prepared) (:operation outcome))
                 (uuid? (:attempt-id prepared))
                 (= (:attempt-id prepared) (:attempt-id outcome))
                 (= :prepared (:phase prepared))
                 (= :physical-outcome (:phase outcome))
                 (= (select-keys prepared [:binding :record :plan])
                    (select-keys outcome [:binding :record :plan]))
                 (map? (:plan prepared))
                 (#{:committed :rejected :indeterminate}
                  (:physical/status outcome)))
        [prepared outcome]))))

(defn- adapter-pair [capture]
  (when (and (= adapter/submission-capture-type (:type capture))
             (= adapter/submission-capture-version (:version capture))
             (= :instrumented-xtdb-adapter-call-site
                (:observation-origin capture))
             (false? (:actual-xtdb-api-authenticated? capture))
             (= 2 (count (:events capture)))
             (vector? (:events capture)))
    (let [[begin end] (:events capture)]
      (when (and (plain-map? begin) (plain-map? end)
                 (= :invoked (:phase begin))
                 (#{:returned :threw} (:phase end))
                 (#{:submit-tx :execute-tx} (:api begin))
                 (= (:api begin) (:api end))
                 (uuid? (:call-id begin))
                 (= (:call-id begin) (:call-id end))
                 (vector? (:tx-ops begin))
                 (plain-map? (:tx-options begin)))
        [begin end]))))

(defn- definitive-execute-key? [begin end]
  (and (= :execute-tx (:api begin))
       (= :returned (:phase end))
       (instance? TransactionKey (:tx-result end))
       (let [^TransactionKey key (:tx-result end)]
         (and (integer? (.getTxId key))
              (<= 0 (.getTxId key))
              (instance? Instant (.getSystemTime key))))))

(defn- classify [selected events poisoned capture observed-request postcheck?]
  (let [pair (when-not poisoned
               (resolved-pair events (:operation selected)))
        adapter-events (adapter-pair capture)
        [prepared outcome] pair
        [begin end] adapter-events
        prepared-transaction (when pair
                               (try (original-prepare observed-request
                                                      (:plan prepared))
                                    (catch Throwable _ nil)))
        operations-agree? (and (vector? (:tx-ops prepared-transaction))
                               adapter-events
                               (= (:tx-ops prepared-transaction)
                                  (:tx-ops begin)))
        options-agree? (and prepared-transaction adapter-events
                            (= (or (get-in prepared-transaction
                                           [:plan :tx-options]) {})
                               (:tx-options begin)))
        definitive? (and adapter-events
                         (definitive-execute-key? begin end))
        complete? (and (not poisoned) postcheck? pair adapter-events
                       operations-agree? options-agree? definitive?
                       (= :committed (:physical/status outcome))
                       (= :returned (:outcome capture)))
        status (cond
                 poisoned :observer-poisoned
                 (not postcheck?) :source-changed-after-execution
                 (not pair) :missing-or-invalid-executor-attempt
                 (not adapter-events) :missing-or-invalid-adapter-call
                 (not operations-agree?) :prepared-plan-diverges-from-adapter
                 (not options-agree?) :prepared-options-diverge-from-adapter
                 (= :submit-tx (:api begin)) :submit-only-not-definitive
                 (not definitive?) :unconfirmed-execute-tx
                 (not complete?) :unconfirmed-or-uncertain-outcome
                 :else :bounded-installed-route-to-adapter-agreement)]
    {:type report-type
     :version report-version
     :scope :one-controlled-in-process-adopted-handler-invocation
     :route-id (:route-id selected)
     :operation-id (:operation selected)
     :status status
     :host-handler-identity :rechecked-against-issued-host-adoption
     :issued-executor-binding :rechecked-from-original-certified-route-source
     :actual-issued-executor-attempt-observed? (boolean pair)
     :one-adapter-call-observed? (boolean adapter-events)
     :prepared-operations-agree? (boolean operations-agree?)
     :prepared-options-agree? (boolean options-agree?)
     :definitive-execute-key-observed? (boolean definitive?)
     :post-execution-source-rechecked? (boolean postcheck?)
     :handler-invocation-count 1
     :actual-server-deployment :not-established
     :xtdb-receipt-authenticated? false
     :complete-writer-coverage :not-established
     :open-obligations (set/union open-obligations
                                  (:open-obligations (:contract selected)))
     :proof-status :bounded-conditional-process-local-observation-only
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn- entry [handle]
  (when (instance? InstalledPhysicalHandle handle)
    (locking observations (.get observations handle))))

(defn installed-physical-witness? [handle]
  "Recognize an issued handle. Not a physical receipt or authority token."
  (boolean (entry handle)))

(defn observe-installed-route-physical!
  "Execute the adopted Biff system's exact Ring handler ONCE, never a fresh
   diagnostic handler. Source, system slot and declared route are checked
   before invocation; they are rechecked afterward without executing again.

   The returned :execution includes the actual response or Throwable and may
   contain sensitive application data. The :report is redacted and copyable.
   Only a definitive, matching, source-rechecked adapter observation issues a
   process-local :handle. A failed or uncertain call MUST NOT be retried."
  [{:keys [host-handle contract-source system request] :as options}]
  (when *observing* (reject! :nested-route-physical-observation))
  (when-not (and (plain-map? options)
                 (= #{:host-handle :contract-source :system :request}
                    (set (keys options))))
    (reject! :invalid-observation-options))
  (let [selected (current-route! host-handle contract-source system request)
        events (atom [])
        poisoned (atom false)
        observer (fn [event]
                   (try (swap! events conj event)
                        (catch Throwable _ (reset! poisoned true))))
        observed-request (assoc request resolved/observation-context-key observer)
        captured (binding [*observing* true]
                   (original-capture
                    #((:host-handler selected) observed-request)))
        postcheck? (try (current-route! host-handle contract-source system request)
                        true
                        (catch Throwable _ false))
        report (classify selected @events @poisoned captured observed-request
                         postcheck?)
        positive? (= :bounded-installed-route-to-adapter-agreement
                     (:status report))
        handle (when positive? (InstalledPhysicalHandle.))]
    (when handle
      (locking observations
        (.put observations handle
              {:source contract-source :host-handle host-handle
               :handler (:host-handler selected) :request request
               :route-id (:route-id selected)
               :operation (:operation selected) :report report})))
    {:execution (select-keys captured [:outcome :result :error])
     :handle handle :report report}))

(defn require-current-installed-physical!
  "Recheck one genuine local witness against ORIGINAL host/source/request and
   a copy of its report, without invoking Ring or XTDB again. A copied map,
   forged handle, altered request, or changed Biff handler cannot authenticate
   another observation. Deployment and transaction authority remain OPEN."
  [handle options prior-report]
  (let [record (or (entry handle) (reject! :unknown-or-forged-witness))]
    (when-not (and (plain-map? options)
                   (= #{:host-handle :contract-source :system :request}
                      (set (keys options)))
                   (identical? (:host-handle record) (:host-handle options))
                   (identical? (:source record) (:contract-source options))
                   (identical? (:request record) (:request options)))
      (reject! :changed-observation-source))
    (let [selected (current-route! (:host-handle options)
                                   (:contract-source options)
                                   (:system options) (:request options))]
      (when-not (and (identical? (:handler record) (:host-handler selected))
                     (= (:route-id record) (:route-id selected))
                     (= (:operation record) (:operation selected))
                     (plain-map? prior-report)
                     (= (:report record) prior-report))
        (reject! :stale-or-altered-observation))
      (:report record))))
