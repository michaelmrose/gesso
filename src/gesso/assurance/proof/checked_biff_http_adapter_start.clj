(ns gesso.assurance.proof.checked-biff-http-adapter-start
  "v2314: Fail-closed Biff HTTP adapter startup for ONE originally issued
   final-handler consumption probe. Unlike post-start verification, the
   selected adapter is not returned as started until an actual read-only
   loopback challenge reaches the precise final checked Ring wrapper.

   This module embeds the selected adapter's Biff start/stop callbacks; the
   raw adapter must NOT also be included in the modules Var. It cleans up a
   listener that fails verification, including a stale captured handler,
   synthetic 200, substituted returned Biff map, or connection failure.

   A guarded stop failure is not automatically replayed. A fourth, exact
   opt-in policy {:idempotent-stop-retry? true} (source-trusted host promise)
   permits ONE additional explicit original stop attempt through the issued
   :retry-cleanup! callback, only after an unresolved first attempt. It
   retains the original system identity, serializes stop and retry, rejects
   reentrant retries, and NEVER returns a release or activation credential.

   v2314: Atomically claim each original physical stop under the private
   monitor, but run host adapter stop callbacks WITHOUT holding the monitor.
   A host callback may synchronously wait on a different thread to request
   this issuer's retry or redundant stop; those operations must observe the
   already-claimed in-progress attempt rather than deadlocking behind the
   physical callback. One frozen target and at most two attempts remain.
   A returned stop callback alone does not prove external physical closure;
   callers must still verify the ORIGINAL managed lifecycle and listener.

   Only a selected, trusted adapter with a loopback URL locator participates.
   It does NOT prevent independent unwrapped listeners or prove that an
   external production deployment uses this module. The original callback
   can perform effects before failure; no auth, retry, or proof certificate."
  (:require [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)))

(def module-id
  :gesso.assurance.proof.checked-biff-http-adapter-start/install)
(def report-type
  :gesso.assurance.proof.checked-biff-http-adapter-start/verified-start)
(def report-version 1)

(deftype GuardedAdapterHandle []
  Object
  (toString [_] "#<gesso.assurance/guarded-biff-http-adapter>"))
(def ^:private ^WeakHashMap issued (WeakHashMap.))

(defn- issued-entry [handle]
  (when (instance? GuardedAdapterHandle handle)
    (locking issued (.get issued handle))))

(def ^:private original-probe-check
  probe/require-current-issued-http-consumption-probe!)

(defn- reject! [reason]
  (throw (ex-info "Selected Biff HTTP adapter did not pass startup consumption."
                  {:checked-biff-http-adapter-start/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (identical? original-probe-check
                         (var-get #'probe/require-current-issued-http-consumption-probe!))
    (reject! :substituted-original-probe-boundary)))

(defn- clean-module? [adapter]
  (and (or (instance? clojure.lang.PersistentArrayMap adapter)
           (instance? clojure.lang.PersistentHashMap adapter))
       (nil? (meta adapter))
       (= #{:biff.core/id :biff.core/start :biff.core/stop}
          (set (keys adapter)))
       (qualified-keyword? (:biff.core/id adapter))
       (not (contains? #{application/application-handler-module-id
                         probe/module-id module-id}
                       (:biff.core/id adapter)))
       (fn? (:biff.core/start adapter))
       (fn? (:biff.core/stop adapter))))

(defn- report [status]
  {:type report-type :version report-version
   :status status
   :scope :one-original-selected-biff-http-adapter-lifecycle
   :final-checked-handler-http-consumption
   (if (= status :verified-selected-adapter-start)
     :one-real-loopback-challenge-observed-before-start-return
     :not-established)
   :normal-http-route-coverage :not-established
   :same-request-typed-canonical-execution :not-established
   :deployed-server-identity :not-established
   :external-server-deployment :not-established
   :authenticated-principal :not-established
   :business-policy-authenticity :not-established
   :complete-physical-writer-coverage :not-established
   :actual-live-delivery :not-established
   :retry-authority :none :transaction-authority :none
   :certificate :not-issued :admission :not-admitted :authority :none})

(defn issue-checked-http-adapter-start!
  "Issue a composite Biff module in place of the raw adapter. Startup order:
   application handler -> checked Biff host -> original consumption probe ->
   THIS module. The wrapper calls the actual adapter :biff.core/start, then
   checks its returned Biff handler and the original probe's real HTTP
   challenge BEFORE publishing the started system. The adapter's locator
   receives its returned system and must describe its running LOOPBACK URL.

   By default the selected adapter stop callback is attempted AT MOST ONCE.
   An explicit fourth argument {:idempotent-stop-retry? true} opts into one
   bounded, manual retry after failed cleanup. This opt-in is a trusted host
   declaration, not a proof of idempotence or a release authorization.
   A failed startup cannot promise rollback of any earlier callback effects."
  ([issued-probe adapter-module base-url!]
   (issue-checked-http-adapter-start!
    issued-probe adapter-module base-url! {}))
  ([issued-probe adapter-module base-url! retry-policy]
   (pinned!)
   (when-not (and (clean-module? adapter-module) (fn? base-url!))
     (reject! :invalid-selected-http-adapter-module))
   (when-not (and (or (instance? clojure.lang.PersistentArrayMap retry-policy)
                      (instance? clojure.lang.PersistentHashMap retry-policy))
                  (nil? (meta retry-policy))
                  (or (empty? retry-policy)
                      (= {:idempotent-stop-retry? true} retry-policy)))
     (reject! :invalid-cleanup-retry-policy))
   (original-probe-check issued-probe)
   (let [handle (GuardedAdapterHandle.)
        original-start (:biff.core/start adapter-module)
        original-stop (:biff.core/stop adapter-module)
        started-state (atom :issued)
        cleanup (atom :not-attempted)
        final-system (atom nil)
        stop-target (atom nil)
        stop-attempts (atom 0)
        stop-monitor (Object.)
        retry-enabled? (= {:idempotent-stop-retry? true} retry-policy)
        ;; Always preserve the exact target selected on FIRST stop; never
        ;; let a later caller swap the original managed Biff system.
        ;; Each stop claim is serialized and publishes :in-progress before
        ;; any original callback executes. The callback itself MUST run
        ;; outside stop-monitor: it can synchronously call back into this
        ;; issuer from another thread during physical listener cleanup.
        claim-attempt! (fn []
                         (swap! stop-attempts inc)
                         (reset! cleanup :in-progress))
        run-claimed-stop! (fn []
                            (try
                              (original-stop @stop-target)
                              (reset! cleanup :completed)
                              (catch Throwable error
                                (when (instance? InterruptedException error)
                                  (.interrupt (Thread/currentThread)))
                                ;; The callback may have closed the listener
                                ;; before throwing. No clean claim is possible.
                                (reset! cleanup :not-established))))
        stop-original! (fn [fallback]
                         (let [claimed?
                               (locking stop-monitor
                                 (when (zero? @stop-attempts)
                                   (reset! stop-target (or @final-system fallback))
                                   (claim-attempt!)
                                   true))]
                           (when claimed?
                             (run-claimed-stop!))))
        retry-cleanup! (fn []
                         (let [claimed?
                               (locking stop-monitor
                                 (when-not retry-enabled?
                                   (reject! :unissued-cleanup-retry))
                                 (when (zero? @stop-attempts)
                                   (reject! :original-stop-not-attempted))
                                 (when (= :in-progress @cleanup)
                                   (reject! :original-stop-in-progress))
                                 (when-not (contains? #{:stopped :failed}
                                                      @started-state)
                                   (reject! :original-adapter-not-terminal))
                                 (when (= :not-established @cleanup)
                                   (when (>= @stop-attempts 2)
                                     (reject! :original-stop-retry-budget-exhausted))
                                   (claim-attempt!)
                                   true))]
                           ;; A second thread may call this during the
                           ;; original physical stop. It observes the claimed
                           ;; :in-progress attempt without waiting for the
                           ;; application callback to return.
                           (when claimed?
                             (run-claimed-stop!))
                           {:status (if (= :completed @cleanup)
                                      :callback-returned :cleanup-unconfirmed)
                            :attempts @stop-attempts
                            :scope :original-selected-stop-callback-only
                            :physical-listener-closure :not-established
                            :release-authority :none
                            :activation-eligibility :not-issued}))
        check-original! (fn []
                          (pinned!)
                          (let [inspection (original-probe-check issued-probe)]
                            (when-not (and (= probe/report-type (:type inspection))
                                           (= probe/report-version (:version inspection))
                                           (= :issued-original-module-and-callbacks
                                              (:original-probe inspection))
                                           (= :none (:authority inspection)))
                              (reject! :unqualified-original-http-probe))
                            true))
        module {:biff.core/id module-id
                :biff.core/start
                (fn [system]
                  (when-not (compare-and-set! started-state :issued :starting)
                    (reject! :adapter-start-already-attempted))
                  ;; A later probe in the module start order must reject
                  ;; BEFORE the adapter's start callback binds any listener.
                  (try
                    (check-original!)
                    (let [current ((:summary! issued-probe))]
                      (when-not (and (= :started (:module-state current))
                                     (= probe/report-type (:type current))
                                     (= :not-issued (:certificate current))
                                     (= :none (:authority current)))
                        (reject! :original-consumption-probe-not-started)))
                    (let [out (original-start system)]
                      (reset! final-system out)
                      (when-not (and (map? out)
                                     (identical? (get system application/biff-ring-handler-key)
                                                 (get out application/biff-ring-handler-key)))
                        (reject! :adapter-changed-final-checked-handler))
                      (check-original!)
                      (let [base (base-url! out)
                            proof ((:verify-once! issued-probe) out base)]
                        (when-not (and (= probe/report-type (:type proof))
                                       (= probe/report-version (:version proof))
                                       (= :one-final-checked-wrapper-http-challenge-returned
                                          (:status proof))
                                       (= :observed-on-selected-loopback-adapter
                                          (:final-checked-handler-consumed-over-http proof))
                                       (= 1 (:challenge-handler-observations proof))
                                       (= :not-established (:external-server-deployment proof))
                                       (= :not-issued (:certificate proof))
                                       (= :none (:authority proof)
                                          (:transaction-authority proof)))
                          (reject! :adapter-start-http-consumption-unverified))
                        (when-not (compare-and-set! started-state
                                                    :starting :verified)
                          (reject! :adapter-start-state-diverged))
                        out))
                    (catch Throwable error
                      (reset! started-state :failed)
                      (stop-original! system)
                      (throw error))))
                :biff.core/stop
                (fn [system]
                  (reset! started-state :stopped)
                  (stop-original! system))}
        summary! (fn []
                   (check-original!)
                   (let [status (case @started-state
                                  :verified :verified-selected-adapter-start
                                  :stopped (if (= :completed @cleanup)
                                             :stopped
                                             :stop-cleanup-not-established)
                                  :failed (if (= :completed @cleanup)
                                            :failed-and-cleaned-up
                                            :failed-cleanup-not-established)
                                  :starting :starting
                                  :issued :issued)]
                     (assoc (report status)
                            :selected-adapter-cleanup @cleanup
                            :original-stop-attempts @stop-attempts
                            :stop-retry-policy (if retry-enabled?
                                                 :explicit-host-idempotence
                                                 :not-enabled))))]
     (locking issued
       (.put issued handle {:module module :summary! summary!
                            :retry-cleanup! retry-cleanup!}))
     {:handle handle :module module :summary! summary!
      :retry-cleanup! retry-cleanup!})))

(defn require-original-issued-http-adapter-start!
  "Recognize one ORIGINAL issued guarded HTTP adapter module, not a copied
   module map or matching ID. No physical startup or deployment is proved."
  [candidate]
  (pinned!)
  (when-not (and (or (instance? clojure.lang.PersistentArrayMap candidate)
                     (instance? clojure.lang.PersistentHashMap candidate))
                 (nil? (meta candidate))
                 (= #{:handle :module :summary! :retry-cleanup!}
                    (set (keys candidate))))
    (reject! :invalid-original-guarded-adapter-shape))
  (let [record (issued-entry (:handle candidate))]
    (when-not (and record
                   (identical? (:module candidate) (:module record))
                   (identical? (:summary! candidate) (:summary! record))
                   (identical? (:retry-cleanup! candidate)
                               (:retry-cleanup! record))
                   (= module-id (get-in candidate [:module :biff.core/id])))
      (reject! :unissued-or-substituted-guarded-adapter))
    {:type report-type :version report-version
     :scope :original-issued-guarded-http-adapter
     :original-guarded-module :exact-identity
     :selected-adapter-start :not-established
     :external-server-deployment :not-established
     :certificate :not-issued :authority :none}))
