(ns gesso.assurance.proof.checked-biff-http-adapter-start
  "v1782: Fail-closed Biff HTTP adapter startup for ONE originally issued
   final-handler consumption probe. Unlike post-start verification, the
   selected adapter is not returned as started until an actual read-only
   loopback challenge reaches the precise final checked Ring wrapper.

   This module embeds the selected adapter's Biff start/stop callbacks; the
   raw adapter must NOT also be included in the modules Var. It cleans up a
   listener that fails verification, including a stale captured handler,
   synthetic 200, substituted returned Biff map, or connection failure.

   Only a selected, trusted adapter with a loopback URL locator participates.
   It does NOT prevent independent unwrapped listeners or prove that an
   external production deployment uses this module. The original callback
   can perform effects before failure; no auth, retry, or proof certificate."
  (:require [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.live.application-preflight :as application])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def module-id
  :gesso.assurance.proof.checked-biff-http-adapter-start/install)
(def report-type
  :gesso.assurance.proof.checked-biff-http-adapter-start/verified-start)
(def report-version 1)

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

   The adapter is stopped at most once on failed verification or Biff stop.
   An adapter that throws AFTER binding a port must implement idempotent
   :biff.core/stop cleanup. A failed startup is not a proof that no external
   or earlier side effects took place."
  [issued-probe adapter-module base-url!]
  (pinned!)
  (when-not (and (clean-module? adapter-module) (fn? base-url!))
    (reject! :invalid-selected-http-adapter-module))
  (original-probe-check issued-probe)
  (let [original-start (:biff.core/start adapter-module)
        original-stop (:biff.core/stop adapter-module)
        started-state (atom :issued)
        stop-called (AtomicBoolean. false)
        cleanup (atom :not-attempted)
        final-system (atom nil)
        stop-original! (fn [fallback]
                         (when (.compareAndSet stop-called false true)
                           (try
                             (original-stop (or @final-system fallback))
                             (reset! cleanup :completed)
                             (catch Throwable _
                               ;; A throwing stop callback may leave a real
                               ;; listener running. Never report cleanup as
                               ;; established in that situation.
                               (reset! cleanup :not-established)))))
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
                            :selected-adapter-cleanup @cleanup)))]
    {:module module :summary! summary!}))
