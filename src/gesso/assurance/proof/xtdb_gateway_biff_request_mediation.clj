(ns gesso.assurance.proof.xtdb-gateway-biff-request-mediation
  "v1563/v1565: optional Biff module that installs one host-owned Ring request
   boundary around the v1561 fresh-writer lifecycle. The previously issued
   checked Biff adoption module MUST start first; this module captures that
   intermediate checked system and installs its own EXACT final handler at
   :biff.core/start. Each invocation through that final handler requests a
   newly issued, scoped, guarded writer, dispatches through the intermediate
   checked Biff handler, and retires the writer in a CAS cleanup.

   This is a local module-start and final-handler-slot guarantee, not proof
   that a deployed HTTP server uses the final returned map. Direct access to
   earlier handlers, independent controllers, direct XTDB writes, host
   enumerator omissions, source-builder authorization, and physical refinement
   remain outside this boundary. The original issuer rejects overlapping
   calls. v1565 adds a separate opt-in bounded, fair admission mode which
   accepts overlapping callers but serializes their entire checked dispatch
   under a single local lease; it is NOT parallel writer execution."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle :as lifecycle]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)
           (java.util.concurrent Semaphore TimeUnit)))

(def module-id :gesso.assurance.proof.xtdb-gateway-biff-request-mediation/install)
(def report-type :gesso.assurance.proof.xtdb-gateway-biff-request-mediation/final-handler)
(def report-version 1)

(deftype MediationHandle []
  Object
  (toString [_] "#<gesso.assurance/biff-request-mediation>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-adoption adoption/require-current-biff-host-adoption!)
(def ^:private original-lifecycle lifecycle/issue-checked-biff-request-writer-lifecycle!)
(def ^:private handler-slot application/biff-ring-handler-key)
(def ^:private open-obligations
  #{:server-must-consume-the-final-returned-biff-system
    :direct-access-to-pre-mediation-handlers-and-other-entry-points
    :later-biff-modules-or-server-may-replace-handler
    :concurrent-requests-through-this-controller-are-rejected
    :parallel-request-writer-execution-not-established
    :bounded-queue-timeout-and-admission-overflow
    :independent-controllers-and-host-registry-mutators
    :host-enumerator-exhaustiveness-and-external-xtdb-clients
    :request-source-builder-and-principal-authorization
    :caller-supplied-xtdb-node-deployment-identity
    :declared-operation-physical-write-correspondence
    :transaction-commit-causality-and-application-refinement})

(defn- reject! [reason]
  ;; No original source, request, handler, XTDB node, registry or response.
  (throw (ex-info "Invalid Gesso Biff request mediation boundary."
                  {:xtdb-gateway-biff-request-mediation/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-adoption
                             (var-get #'adoption/require-current-biff-host-adoption!))
                 (identical? original-lifecycle
                             (var-get #'lifecycle/issue-checked-biff-request-writer-lifecycle!))
                 (= handler-slot application/biff-ring-handler-key))
    (reject! :substituted-biff-adoption-or-lifecycle)))

(defn- entry [handle]
  (when (instance? MediationHandle handle)
    (locking issued (.get issued handle))))

(defn- bounded-concurrency-policy! [policy]
  ;; Only primitive, finite bounds: no caller callbacks, executors or thread
  ;; handles can become another unreviewed submission authority.
  (when-not (and (map? policy)
                 (= #{:max-pending-requests :wait-timeout-ms} (set (keys policy)))
                 (integer? (:max-pending-requests policy))
                 (<= 2 (:max-pending-requests policy) 64)
                 (integer? (:wait-timeout-ms policy))
                 (<= 1 (:wait-timeout-ms policy) 60000))
    (reject! :invalid-bounded-mediation-policy))
  {:capacity (int (:max-pending-requests policy))
   :wait-ms (long (:wait-timeout-ms policy))})

(defn- bounded-serial-dispatch [dispatch! {:keys [capacity wait-ms]}]
  ;; A single stable registry map is used by the v1555/v1557 identity checks.
  ;; It is unsafe to overlap two dispatches against that registry: one would
  ;; observe the other's installation/retirement. Accept overlapping callers
  ;; but execute the entire issuance -> observation -> CAS cleanup sequence
  ;; behind one fair lease, without reusing a callback.
  (let [^Semaphore gate (Semaphore. 1 true)
        pending (atom 0)
        owner (atom nil)]
    (fn [request]
      (when (identical? (Thread/currentThread) @owner)
        (reject! :recursive-biff-mediation-dispatch))
      (when-not (loop []
                  (let [n @pending]
                    (cond
                      (>= n capacity) false
                      (compare-and-set! pending n (inc n)) true
                      :else (recur))))
        (reject! :bounded-mediation-capacity-exhausted))
      (try
        (let [acquired?
              (try (.tryAcquire gate wait-ms TimeUnit/MILLISECONDS)
                   (catch InterruptedException _
                     (.interrupt (Thread/currentThread))
                     (reject! :bounded-mediation-interrupted)))]
          (when-not acquired?
            (reject! :bounded-mediation-wait-timeout))
          (try
            (reset! owner (Thread/currentThread))
            (dispatch! request)
            (finally
              (reset! owner nil)
              (.release gate))))
        (finally
          (swap! pending dec))))))

(defn- issue-mediation-module*
  "Issue {:module Biff-module :handle opaque-handle}, for one Biff startup.

   Args: [checked-adoption-handle original-contract-source inventory-source
          writer-registry-atom baseline-issued-handles route-writer-paths
          node request->admitted-source! revision-field].

   Install this Biff module AFTER checked-biff-host-adoption and BEFORE any
   HTTP server module. Biff's :biff.core/start callback captures the exact
   intermediate checked handler, creates ONE v1561 lifecycle controller and
   installs an exact final Ring wrapper in the returned system map. Each
   request to this wrapper automatically invokes :run-request! and returns
   ONLY the original Ring response. The host source builder is caller-owned.

   The module is single-start. The original API rejects overlapping requests;
   the opt-in bounded issuer admits a finite queue and serializes dispatch.
   Neither mode shares one-shot callbacks. It does not intercept
   direct dispatch through a retained old handler or authenticate a server."
  [adoption-handle contract-source inventory-source registry
   baseline-handles route-writer-paths node source! revision-field policy]
  (pinned!)
  (when-not (and (fn? source!)
                 (keyword? revision-field)
                 (instance? clojure.lang.Atom registry)
                 (map? inventory-source)
                 (map? baseline-handles)
                 (map? route-writer-paths)
                 (some? node))
    (reject! :invalid-mediation-issuance))
  (let [handle (MediationHandle.)
        init-state (atom :new)
        start-state (atom :not-started)
        module
        {:biff.core/id module-id
         :biff.core/init
         (fn [_modules-var]
           (pinned!)
           (when-not (compare-and-set! init-state :new :initialized)
             (reject! :repeated-mediation-module-init))
           {})
         :biff.core/start
         (fn [checked-system]
           ;; Poison before the first downstream check; startup retries
           ;; after partial installation must not silently succeed.
           (when-not (compare-and-set! start-state :not-started :starting)
             (reject! :repeated-mediation-module-start))
           (pinned!)
           (when-not (= :initialized @init-state)
             (reject! :mediation-module-not-initialized))
           (let [host (original-adoption adoption-handle contract-source checked-system)
                 intermediate (get checked-system handler-slot)]
             (when-not (and (= adoption/report-type (:type host))
                            (= :identical-to-guarded-checked-router-wrapper
                               (:handler-slot host))
                            (fn? intermediate))
               (reject! :missing-checked-intermediate-biff-handler))
             (let [controller (original-lifecycle
                               adoption-handle contract-source checked-system
                               inventory-source registry baseline-handles
                               route-writer-paths)
                   dispatch!
                   (fn [request]
                     (pinned!)
                     ;; Returns Ring response only; never exposes issued
                     ;; handles, original transaction source or diagnostics.
                     (:response ((:run-request! controller)
                                 request node source! revision-field)))
                   final-handler (if policy
                                   (bounded-serial-dispatch dispatch! policy)
                                   dispatch!)
                   final-system (assoc checked-system handler-slot final-handler)
                   started {:source contract-source
                            :intermediate-system checked-system
                            :intermediate-handler intermediate
                            :final-system final-system
                            :handler final-handler
                            :concurrency-policy policy
                            :controller controller}]
               (when-not (compare-and-set! start-state :starting started)
                 (reject! :compromised-mediation-module-start))
               final-system)))}]
    (locking issued
      (.put issued handle {:source contract-source
                           :module module
                           :init-state init-state
                           :start-state start-state
                           :adoption-handle adoption-handle}))
    {:module module :handle handle}))

(defn issue-checked-biff-request-mediation-module!
  "Issue a module with the original fail-closed, reject-overlap policy."
  [adoption-handle contract-source inventory-source registry
   baseline-handles route-writer-paths node source! revision-field]
  (issue-mediation-module*
   adoption-handle contract-source inventory-source registry
   baseline-handles route-writer-paths node source! revision-field nil))

(defn issue-bounded-queued-biff-request-mediation-module!
  "Issue a module allowing overlapping callers under a finite FAIR queue.

   Extra policy {:max-pending-requests N :wait-timeout-ms M} requires
   2 <= N <= 64 and 1 <= M <= 60000. N counts both running and waiting
   requests. Admission is bounded, thread interruption is preserved, and
   nested dispatch is rejected immediately. Each admitted request executes
   its complete fresh-callback lifecycle EXCLUSIVELY and in turn. This is
   concurrent *arrival*, not simultaneous handler execution or independent
   registries. Retained old handlers and unrelated XTDB clients are open."
  [adoption-handle contract-source inventory-source registry
   baseline-handles route-writer-paths node source! revision-field policy]
  (issue-mediation-module*
   adoption-handle contract-source inventory-source registry
   baseline-handles route-writer-paths node source! revision-field
   (bounded-concurrency-policy! policy)))

(defn require-current-biff-request-mediation!
  "Inspect the EXACT final system map returned by this issued module start.

   Unlike the intermediate v1478 adoption check, the final handler is a
   *different* wrapper. Both the original intermediate handler and the final
   module-owned handler must still match their original objects. A copied
   map is rejected, even when it contains the same callback. Inspection
   does not issue, invoke, retire or submit a transaction."
  [handle contract-source final-system]
  (pinned!)
  (let [{:keys [source init-state start-state adoption-handle]}
        (or (entry handle) (reject! :unissued-mediation-module))
        started @start-state]
    (when-not (identical? source contract-source)
      (reject! :changed-mediation-contract-source))
    (when-not (= :initialized @init-state)
      (reject! :mediation-module-not-initialized))
    (when-not (map? started)
      (reject! :mediation-module-not-started))
    (when-not (and (identical? final-system (:final-system started))
                   (identical? (:handler started) (get final-system handler-slot))
                   (not (identical? (:handler started)
                                    (:intermediate-handler started)))
                   (identical? (:intermediate-handler started)
                               (get (:intermediate-system started) handler-slot)))
      (reject! :final-biff-handler-system-identity-mismatch))
    (let [adopted (original-adoption adoption-handle contract-source
                                     (:intermediate-system started))]
      (when-not (= adoption/report-type (:type adopted))
        (reject! :original-biff-adoption-no-longer-checked))
      (pinned!)
      (when-not (and (identical? final-system (:final-system @start-state))
                     (identical? (:handler started) (get final-system handler-slot)))
        (reject! :mediation-handler-changed-during-check))
      {:type report-type
       :version report-version
       :status :conditional-final-biff-handler-installed-from-checked-host
       :scope :one-issued-process-local-final-biff-system-map
       :biff-module-init :observed-once
       :biff-module-start :observed-once
       :final-handler-slot :exact-identity-checked
       :intermediate-handler :source-checked-biff-adoption
       :fresh-request-scoped-writer-issuance :at-dispatch-only
       :concurrent-arrival-policy
       (if (:concurrency-policy started)
         :bounded-fair-serialized-dispatch
         :reject-overlapping-dispatch)
       :simultaneous-guarded-handler-execution :not-permitted
       :max-pending-requests (when-let [policy (:concurrency-policy started)]
                               (:capacity policy))
       :writer-registration-and-retirement :delegated-to-v1561
       :request-invocation :not-observed-by-this-inspection
       :deployed-server-use :not-established
       :complete-writer-mediation :not-established
       :physical-refinement :not-established
       :open-obligations
       (set/union (if (:concurrency-policy started)
                    (disj open-obligations
                          :concurrent-requests-through-this-controller-are-rejected)
                    open-obligations)
                  (:open-obligations adopted))
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))
