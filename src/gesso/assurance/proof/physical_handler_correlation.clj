(ns gesso.assurance.proof.physical-handler-correlation
  "v1469: Bounded process-local correlation between one synchronous handler
   invocation and ONE v1467 controlled XTDB observation performed INSIDE it.

   The handler explicitly calls observe-in-handler! from its invocation thread.
   The observer delegates to the ORIGINAL v1467 collector boundary, which in
   turn calls the controlled XTDB collector once. A completed scope produces
   an identity-registered handle bound to the ACTUAL callable and request
   objects invoked and the ORIGINAL physical witness; copied reports and fabricated handles cannot
   replace that binding. Neither inspection nor recheck re-executes the handler
   or resubmits the transaction.

   A handler that does not call the observer fails closed; a second call is
   rejected BEFORE another submission. Unknown outcomes and handler exceptions
   propagate without retry or success-report issuance. This is a synchronous,
   opt-in TEST instrumentation boundary; async callbacks are not correlated.

   It does NOT establish that the function is installed in a deployed router,
   that a registered certified handler's issued executor performed this write,
   or that the collected plan equals the executor's submitted payload. Even a
   registered v1253 handler identity remains a local observation, not a
   production certificate, complete writer inventory, or XTDB receipt."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-xtdb-transition-provenance :as physical]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util WeakHashMap)))

(def correlation-version 1)
(def correlation-type
  :gesso.assurance.proof.physical-handler-correlation/local-handler-step)

(deftype HandlerCorrelationHandle []
  Object
  (toString [_] "#<gesso.assurance/handler-physical-correlation>"))

(def ^:private ^WeakHashMap issued-correlations (WeakHashMap.))
(def ^:dynamic ^:private *invocation-scope* nil)
(def ^:private original-observe physical/observe-provenanced-transition!)
(def ^:private original-physical-recheck physical/require-current-process-local-witness!)
(def ^:private original-certified? certified/certified-handler?)

(def ^:private additional-open
  #{:actual-deployed-route-and-handler-adoption
    :current-issued-executor-plan-and-collector-plan-equivalence
    :physical-transaction-receipt-authenticity
    :complete-physical-writer-inventory
    :actual-authorization-and-request-input-binding
    :asynchronous-and-cross-thread-handler-work
    :universal-physical-execution-refinement})

(defn- reject! [reason]
  ;; Input, request, handler response and physical transaction data are private.
  (throw (ex-info "Invalid controlled handler/physical correlation."
                  {:physical-handler-correlation/rejection true
                   :reason reason})))

(defn- pinned-boundaries! []
  (when-not (and (identical? original-observe
                             (var-get #'physical/observe-provenanced-transition!))
                 (identical? original-physical-recheck
                             (var-get #'physical/require-current-process-local-witness!))
                 (identical? original-certified?
                             (var-get #'certified/certified-handler?)))
    (reject! :substituted-correlation-boundary)))

(defn- current-scope! []
  (let [scope *invocation-scope*]
    (when-not (and scope (identical? (:thread scope) (Thread/currentThread)))
      (reject! :no-current-synchronous-handler-scope))
    scope))

(defn observe-in-handler!
  "Instrument one controlled physical transaction FROM INSIDE the invocation
   of invoke-correlated-handler!. Returns the redacted v1465 report to the
   handler, never an authentication credential. Unknown execution results
   throw unchanged; this function never retries."
  [controlled-input]
  (pinned-boundaries!)
  (let [{:keys [capture]} (current-scope!)]
    (when-not (compare-and-set! capture :not-started :started)
      ;; Poison the scope even if the handler catches this rejection.
      (reset! capture :multiple-observation-attempts)
      (reject! :second-or-reentrant-controlled-transition))
    (try
      (let [observed (original-observe controlled-input)]
        (when-not (compare-and-set! capture :started
                                   {:status :completed
                                    :controlled-input controlled-input
                                    :physical-handle (:handle observed)
                                    :phase-report (:report observed)})
          (reject! :compromised-observation-scope))
        (:report observed))
      (catch Throwable t
        ;; Even if the physical write committed, no successful correlation
        ;; witness is issued. Propagate the ORIGINAL failure without retry.
        (reset! capture :outcome-unknown)
        (throw t)))))

(defn- describe-entry [witness]
  (let [physical-report (original-physical-recheck
                         (:physical-handle witness)
                         (:controlled-input witness)
                         (:phase-report witness))]
    {:type correlation-type
     :version correlation-version
     :status :one-controlled-step-inside-synchronous-handler
     :capture-origin :one-handler-invocation-on-original-thread
     :handler-binding :identical-in-process-callable
     :request-binding :identical-in-process-argument
     :registered-issued-handler-at-invocation?
     (:registered-handler? witness)
     :issued-handler-current-source :not-established
     :issued-executor-to-physical-plan :not-established
     :physical-observation (:phase-report witness)
     :collector-observation (:status physical-report)
     :controlled-observations 1
     :physical-receipt-authenticated? false
     :open-obligations (set/union additional-open
                                  (:open-obligations physical-report))
     :scope :single-process-single-thread-one-controlled-handler-step
     :proof-status :process-local-handler-extent-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn- entry [handle]
  (when (instance? HandlerCorrelationHandle handle)
    (locking issued-correlations (.get issued-correlations handle))))

(defn handler-correlation? [handle]
  "Only a previously registered local invocation handle; NOT authorization."
  (boolean (entry handle)))

(defn invoke-correlated-handler!
  "Invoke the exact caller-supplied synchronous Ring-style callable ONCE.

   The handler must call observe-in-handler! exactly once before returning.
   Returns {:response <original handler return> :handle <opaque handle>
            :report <redacted diagnostic>}. The response itself may be private
   application data and must not be logged as provenance. If handler throws,
   or does not instrument exactly one step, no handle is issued; no retry.

   :registered-issued-handler-at-invocation? reflects only v1253's process
   registry check, not a fresh proof of the handler's plan or deployment."
  [handler request]
  (pinned-boundaries!)
  (when (some? *invocation-scope*)
    (reject! :nested-handler-correlation-scope))
  (when-not (fn? handler)
    (reject! :invalid-handler))
  (let [scope {:thread (Thread/currentThread)
               :capture (atom :not-started)}
        registered? (boolean (original-certified? handler))
        ;; Never catch and reinterpret application or physical exceptions.
        response (binding [*invocation-scope* scope]
                   (handler request))
        captured @(:capture scope)]
    (when (= :outcome-unknown captured)
      (reject! :caught-unknown-physical-outcome))
    (when (= :multiple-observation-attempts captured)
      (reject! :multiple-observation-attempts))
    (when-not (and (map? captured) (= :completed (:status captured)))
      (reject! :missing-completed-controlled-transition))
    (let [witness (assoc captured :handler handler :request request
                                 :registered-handler? registered?)
          ;; Validate the v1467 live witness BEFORE issuing a new handle.
          report (describe-entry witness)
          handle (HandlerCorrelationHandle.)]
      (locking issued-correlations (.put issued-correlations handle witness))
      {:response response :handle handle :report report})))

(defn describe-handler-correlation
  "Return a copyable, redacted diagnostic for a live local handle."
  [handle]
  (pinned-boundaries!)
  (if-let [witness (entry handle)]
    (describe-entry witness)
    (reject! :unknown-or-forged-handler-correlation)))

(defn require-current-handler-correlation!
  "Recheck the ORIGINAL handler and request object identities, current v1467
   live witness and exact prior diagnostic. Never calls the handler, collector, or XTDB again.
   Matching handler identity is not a deployed route/issuer attestation."
  [handle handler request prior-report]
  (pinned-boundaries!)
  (let [witness (or (entry handle)
                    (reject! :unknown-or-forged-handler-correlation))]
    (when-not (identical? handler (:handler witness))
      (reject! :handler-identity-substitution))
    (when-not (identical? request (:request witness))
      (reject! :request-identity-substitution))
    (when-not (= (boolean (original-certified? handler))
                 (:registered-handler? witness))
      (reject! :changed-issued-handler-registration))
    (let [now (describe-entry witness)]
      (when-not (and (or (instance? clojure.lang.PersistentArrayMap prior-report)
                         (instance? clojure.lang.PersistentHashMap prior-report))
                     (nil? (meta prior-report))
                     (= prior-report now))
        (reject! :stale-or-forged-handler-report))
      now)))
