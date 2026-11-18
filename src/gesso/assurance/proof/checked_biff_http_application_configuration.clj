(ns gesso.assurance.proof.checked-biff-http-application-configuration
  "v1800: Application-owned entry point for one selected CLOSED Biff HTTP
   startup. One declared contract source and selected adapter are enough:
   issue the ORIGINAL host, consumption probe and guarded adapter internally,
   derive the canonical module Var/order, and execute v1783's pre-init sealed
   startup. Callers cannot accidentally supply a different module inventory,
   an unissued proof record, or an earlier start order through this API.

   This is a consciously selected application STARTUP CONFIGURATION, not a
   replacement for Biff's unrestricted start API. This code does not control
   independent Biff instances, raw socket listeners, a remote deployment,
   authentic request principals or physical transaction writers. It issues
   no proof certificate, admission, retry or transaction authority.

   v1793: If a typed trial selects a catalog, its ENTIRE declared certified
   route set must match the current original application assembly before
   the sealed Biff startup touches an HTTP listener. The coverage checker
   does not establish installed/deployed handler identity or prove that the
   selected catalog includes every business rule.

   v1794: AFTER the actual sealed adapter startup, require the exact issued
   FINAL consumption probe wrapper to remain in the returned Biff system.
   Recheck the ORIGINAL host's captured pre-probe system and its full certified
   route inventory beneath the wrapper. Only one real loopback challenge is
   observed; other routes are not claimed to be individually exercised.

   v1798: Require that the original issued probe has confirmed EACH route
   selected in the source-checked catalog through the original checked router
   on the actual returned adapter BEFORE declaring startup successful.
   Parameterized routes have one observed witness, not universal coverage.
   v1799 qualifies route inventory and dispatch from ONE original issued
   probe and the SAME returned Biff system, not a separate success summary.

   v1800: Recheck the SAME managed in-process Biff system and original
   guarded adapter/probe lifecycle before reporting current instance status.
   Managed stops and uncertain cleanup cannot be requalified by a saved
   startup report. Out-of-band raw adapter stops or external deployment
   liveness are outside this local observation."
  (:require [clojure.set :as set]
            [com.biffweb.core :as biff]
            [gesso.assurance.proof.checked-authorization-assembly :as coverage]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.live.application-preflight :as app])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.checked-biff-http-application-configuration/selected-start)
(def report-version 1)
(def running-report-type
  :gesso.assurance.proof.checked-biff-http-application-configuration/current-instance)

;; The ONLY Biff module Var used by this configured entry point. Its dynamic
;; binding is scoped around BOTH the v1783 preflight and Biff startup.
(def ^:dynamic ^:private *configured-modules* [])

(def ^:private original-host-issuer host/issue-checked-biff-host-module!)
(def ^:private original-probe-issuer probe/issue-http-consumption-probe-module!)
(def ^:private original-guarded-issuer guarded/issue-checked-http-adapter-start!)
(def ^:private original-sealed-start sealed/start-closed-checked-http-application!)
(def ^:private original-biff-stop biff/stop)
(def ^:private original-route-coverage coverage/check-declared-route-coverage!)
(def ^:private original-probe-dispatch probe/require-current-final-catalog-dispatch!)
(def ^:private original-probe-inventory probe/require-current-final-probe-route-inventory!)

(defn- reject! [reason]
  (throw (ex-info "Configured closed Biff HTTP startup not established."
                  {:checked-biff-http-application-configuration/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-issuer
                             (var-get #'host/issue-checked-biff-host-module!))
                 (identical? original-probe-issuer
                             (var-get #'probe/issue-http-consumption-probe-module!))
                 (identical? original-guarded-issuer
                             (var-get #'guarded/issue-checked-http-adapter-start!))
                 (identical? original-sealed-start
                             (var-get #'sealed/start-closed-checked-http-application!))
                 (identical? original-biff-stop
                             (var-get #'biff/stop))
                 (identical? original-route-coverage
                             (var-get #'coverage/check-declared-route-coverage!))
                 (identical? original-probe-dispatch
                             (var-get #'probe/require-current-final-catalog-dispatch!))
                 (identical? original-probe-inventory
                             (var-get #'probe/require-current-final-probe-route-inventory!)))
    (reject! :substituted-original-configured-start-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- checked-source! [config]
  (pinned!)
  (when-not (and (plain-map? config)
                 (contains? #{#{:contract-source :adapter-module :base-url!}
                              #{:contract-source :adapter-module :base-url!
                                :typed-trial}}
                            (set (keys config)))
                 (plain-map? (:contract-source config))
                 (fn? (:base-url! config))
                 (or (not (contains? config :typed-trial))
                     (nil? (:typed-trial config))
                     (and (plain-map? (:typed-trial config))
                          (contains? #{#{:catalog-source :operation :inputs}
                                       #{:catalog-source :operation :inputs
                                         :principal-policy :authorization-source}}
                                     (set (keys (:typed-trial config))))
                          (plain-map? (get-in config [:typed-trial :catalog-source]))
                          (plain-map? (get-in config [:typed-trial :inputs]))
                          (qualified-keyword? (get-in config [:typed-trial :operation])))))
    (reject! :invalid-application-owned-http-configuration))
  config)

(defn- require-selected-route-coverage! [contract-source typed-trial]
  ;; There is deliberately no caller-supplied checked report to trust. Only
  ;; selected typed execution requires this check. Run it BEFORE Biff init,
  ;; since an invalid selected catalog must never start an HTTP listener.
  (when typed-trial
    (pinned!)
    (let [checked (try
                    (original-route-coverage
                     {:contract-source contract-source
                      :catalog-source (:catalog-source typed-trial)})
                    (catch Exception _
                      (reject! :selected-catalog-route-coverage-not-checked)))]
      (when-not (and (= coverage/coverage-report-type (:type checked))
                     (= coverage/report-version (:version checked))
                     (= :all-declared-catalog-operations-and-issued-routes
                        (:checking checked))
                     (contains? (:catalog-operations checked)
                                (:operation typed-trial))
                     (= :source-relative-checked
                        (:current-assembly-closure checked))
                     (set? (:matched-certified-route-ids checked))
                     (seq (:matched-certified-route-ids checked))
                     (= :not-established (:installed-handler-adoption checked))
                     (= :none (:transaction-authority checked)
                        (:authority checked))
                     (= :not-issued (:certificate checked))
                     (= :not-admitted (:admission checked)))
        (reject! :selected-catalog-route-coverage-not-checked))
      checked)))

(defn- require-matching-selected-host-routes!
  "Check a genuine host-adoption report against the previously checked
   selected catalog. Only the caller's original issued host observation can
   establish the report itself; this helper cannot authenticate reports."
  [selected host-report]
  (when-not (and (= coverage/coverage-report-type (:type selected))
                 (= coverage/report-version (:version selected))
                 (= :source-relative-checked
                    (:current-assembly-closure selected))
                 (set? (:matched-certified-route-ids selected))
                 (seq (:matched-certified-route-ids selected))
                 (= probe/final-catalog-dispatch-report-type (:type host-report))
                 (= probe/report-version (:version host-report))
                 (= probe/module-id (:module-id host-report))
                 (= host/module-id (:host-module-id host-report))
                 (= :one-original-probe-one-returned-system-and-host
                    (:scope host-report))
                 (= :exact-issued-wrapper
                    (:final-probe-handler-slot host-report))
                 (= :one-returned-in-process-probe-handler-slot
                    (:installed-handler-adoption host-report))
                 (= :checked-and-selected-dispatch-witnessed
                    (:normal-http-route-inventory host-report))
                 (set? (:checked-certified-route-ids host-report))
                 (set? (:checked-route-ids host-report))
                 (= (:matched-certified-route-ids selected)
                    (:checked-certified-route-ids host-report))
                 (set/subset? (:checked-certified-route-ids host-report)
                              (:checked-route-ids host-report))
                 (= :not-established (:external-deployment host-report))
                 (= :not-issued (:certificate host-report))
                 (= :not-admitted (:admission host-report))
                 (= :none (:authority host-report)
                    (:transaction-authority host-report)))
    (reject! :selected-catalog-route-inventory-not-adopted))
  true)

(defn- require-selected-catalog-dispatch!
  "Qualify one ORIGINAL probe inspection already bound to the real final
   Biff system, not a separate callback-supplied success summary."
  [selected observed]
  (let [expected (:matched-certified-route-ids selected)]
    (when-not (and (set? expected)
                   (seq expected)
                   (= probe/final-catalog-dispatch-report-type (:type observed))
                   (= probe/report-version (:version observed))
                   (= :one-original-probe-one-returned-system-and-host
                      (:scope observed))
                   (= :all-selected-certified-routes-observed-through-original-router
                      (:catalog-read-only-route-dispatch observed))
                   (= expected (:catalog-expected-certified-route-ids observed)
                      (:catalog-observed-certified-route-ids observed))
                   (= (count expected) (:selected-route-challenge-observations observed))
                   (= :not-invoked-by-read-only-probe
                      (:selected-route-business-handler-execution observed))
                   (set? (:catalog-parameterized-witness-route-ids observed))
                   (set/subset? (:catalog-parameterized-witness-route-ids observed)
                                expected)
                   (= :not-established (:parameterized-route-generalization observed))
                   (zero? (:typed-trial-attempts observed))
                   (= :none (:authority observed)
                      (:transaction-authority observed)
                      (:retry-authority observed))
                   (= :not-issued (:certificate observed))
                   (= :not-admitted (:admission observed)))
      (reject! :selected-catalog-http-dispatch-not-observed))
    true))

(defn- report [state typed-selected?]
  {:type report-type :version report-version
   :scope :one-application-owned-closed-biff-http-start
   :status state
   :startup-configuration :one-contract-and-selected-adapter
   :module-inventory :internally-derived-original-four-modules
   :module-order :internally-derived-canonical
   :real-http-consumption
   (if (= state :verified-configured-start)
     :selected-adapter-challenge-passed-during-start
     :not-established)
   :typed-catalog-execution :optional-separately-observed
   :declared-route-coverage-at-start
   (if (and typed-selected? (= state :verified-configured-start))
     :selected-certified-catalog-routes-source-checked-before-start
     :not-established)
   :returned-host-selected-route-inventory
   (if (and typed-selected? (= state :verified-configured-start))
     :exact-selected-certified-route-set-observed-beneath-final-probe-wrapper
     :not-established)
   :selected-catalog-read-only-http-dispatch
   (if (and typed-selected? (= state :verified-configured-start))
     :all-selected-certified-routes-observed-before-start-return
     :not-established)
   :unprobed-routes-over-real-http :not-established
   :parameterized-route-generalization :not-established
   :managed-instance-liveness :requires-current-instance-inspection
   :selected-catalog-business-policy-completeness :not-established
   :external-deployment :not-established
   :independent-biff-starts :not-established
   :uncontrolled-socket-listeners :not-established
   :authenticated-principal :not-established
   :complete-physical-writer-coverage :not-established
   :xtdb-physical-receipt :not-established
   :actual-live-delivery :not-established
   :retry-authority :none :transaction-authority :none
   :certificate :not-issued :admission :not-admitted :authority :none})

(defn issue-configured-checked-http-application!
  "Issue a one-use, application-owned startup from exactly a checked
   contract, selected raw adapter module, and local adapter URL locator.
   Optional :typed-trial chooses a catalog-derived one-use execution observer.

   The result does NOT expose a modules Var or customizable start-order.
   (:start-once! issued) takes only an initial Biff system; startup modules
   are derived from the captured contract and adapter, checked by v1783,
   and run through the original canonical Gesso/Biff lifecycle. The optional
   (:stop! issued) stops the selected Biff system at most once. A failed start
   consumes issuance; it cannot promise rollback of earlier callback effects."
  [source]
  (let [{:keys [contract-source adapter-module base-url! typed-trial]}
        (checked-source! source)
        ;; Issuance itself validates original router, original proof
        ;; construction and the selected adapter before any server startup.
        issued-host (original-host-issuer contract-source)
        issued-probe (original-probe-issuer (:handle issued-host)
                                            contract-source typed-trial)
        issued-guarded (original-guarded-issuer issued-probe
                                               adapter-module base-url!)
        modules [app/application-handler-module (:module issued-host)
                 (:module issued-probe) (:module issued-guarded)]
        order [app/application-handler-module-id host/module-id
               probe/module-id guarded/module-id]
        consumed (AtomicBoolean. false)
        state (atom :issued)
        started (atom nil)
        summary! (fn [] (pinned!) (report @state (some? typed-trial)))
        inspect-running! (fn [candidate-system]
                           ;; Historical startup success cannot be reused
                           ;; as present-tense local instance evidence.
                           (pinned!)
                           (when-not (and (= :verified-configured-start @state)
                                          (map? candidate-system)
                                          (identical? candidate-system @started))
                             (reject! :configured-instance-not-current))
                           (let [guard-status (try ((:summary! issued-guarded))
                                                   (catch Throwable _ nil))
                                 inventory (try
                                             (original-probe-inventory
                                              issued-probe contract-source
                                              candidate-system)
                                             (catch Throwable _ nil))]
                             (when-not (and (= :verified-selected-adapter-start
                                                (:status guard-status))
                                            (= :not-attempted
                                               (:selected-adapter-cleanup guard-status))
                                            (= probe/final-route-inventory-report-type
                                               (:type inventory))
                                            (= :exact-issued-wrapper
                                               (:final-probe-handler-slot inventory))
                                            (= :verified-on-selected-loopback-adapter
                                               (:one-real-http-consumption-challenge inventory))
                                            (set? (:checked-certified-route-ids inventory))
                                            (= :none (:authority inventory)
                                               (:transaction-authority inventory))
                                            (= :verified-configured-start @state)
                                            (identical? candidate-system @started))
                               (reject! :configured-instance-not-current))
                             {:type running-report-type :version report-version
                              :scope :one-original-managed-in-process-biff-instance
                              :status :current-at-inspection
                              :returned-system :exact-identity
                              :original-guarded-adapter :verified-not-stopped
                              :original-probe-handler-slot :exact-issued-wrapper
                              :selected-catalog-source :checked-at-startup-only
                              :external-listener-liveness :not-established
                              :out-of-band-adapter-stops :not-established
                              :certificate :not-issued :admission :not-admitted
                              :retry-authority :none :transaction-authority :none
                              :authority :none}))
        start! (fn [initial-system]
                 (when-not (.compareAndSet consumed false true)
                   (reject! :configured-start-already-attempted))
                 (when-not (compare-and-set! state :issued :starting)
                   (reject! :configured-start-stopped-before-use))
                 (try
                   (pinned!)
                   (when-not (and (plain-map? initial-system)
                                  (not (contains? initial-system
                                                  app/biff-ring-handler-key)))
                     (reject! :invalid-or-preinstalled-biff-initial-system))
                   ;; The route and exact issued handler coverage must be
                   ;; checked against CURRENT original sources before Biff
                   ;; initializes modules or installs the real HTTP listener.
                   (let [selected-coverage
                         (require-selected-route-coverage! contract-source typed-trial)
                         result (binding [*configured-modules* modules]
                                  (original-sealed-start
                                   (:application-assembly contract-source)
                                   initial-system issued-host issued-probe
                                   issued-guarded #'*configured-modules* order))]
                     (try
                       (when-not (and (= sealed/report-type
                                         (get-in result [:report :type]))
                                      (= :one-selected-verified-adapter-start
                                         (get-in result [:report :physical-server-start]))
                                      (= :none (get-in result [:report :authority])))
                         (reject! :unqualified-closed-http-start))
                       (when selected-coverage
                         ;; The probe runs AFTER the host, and intentionally
                         ;; wraps its checked handler. Verify that exact FINAL
                         ;; probe wrapper and the original host's pre-probe
                         ;; route inventory under a single issued handle.
                         (let [observed (try
                                          (pinned!)
                                          (original-probe-dispatch
                                           issued-probe contract-source
                                           (:system result)
                                           (:matched-certified-route-ids selected-coverage))
                                          (catch Throwable _
                                            (reject! :selected-catalog-route-inventory-not-adopted)))]
                           (require-matching-selected-host-routes!
                            selected-coverage observed)
                           ;; The v1797 probe's original closure owns the
                           ;; per-route HTTP response and exact router match.
                           ;; Missing even one route prevents startup success.
                           (require-selected-catalog-dispatch!
                            selected-coverage observed)))
                       (when-not (compare-and-set! state :starting
                                                    :verified-configured-start)
                         (reject! :configured-start-state-diverged))
                       (reset! started (:system result))
                       {:system (:system result)
                        :report (report @state (some? typed-trial))}
                       (catch Throwable error
                         ;; The adapter has already started. Any failed
                         ;; post-start coverage check MUST stop it before
                         ;; propagating failure; cleanup can still fail.
                         (when-let [system (:system result)]
                           (try (original-biff-stop system)
                                (catch Throwable _ nil)))
                         (throw error))))
                   (catch Throwable error
                     ;; A selected adapter that opened a port must implement
                     ;; its own stop cleanup on failed Biff startup.
                     (reset! state :start-failed-physical-state-unknown)
                     (throw error))))
        stop! (fn []
                (loop []
                  (let [current @state]
                    (case current
                      :starting (reject! :configured-start-in-progress)
                      :stopping (reject! :configured-stop-in-progress)
                      :issued
                      (if (compare-and-set! state :issued
                                            :stopped-without-successful-start)
                        (summary!) (recur))
                      :verified-configured-start
                      (if (compare-and-set! state :verified-configured-start
                                            :stopping)
                        (do
                          (try
                            (pinned!)
                            (original-biff-stop @started)
                            ;; A returned stop call is not enough: require
                            ;; original guarded adapter and probe cleanup.
                            (let [guard-status (try ((:summary! issued-guarded))
                                                    (catch Throwable _ nil))
                                  probe-status (try ((:summary! issued-probe))
                                                    (catch Throwable _ nil))]
                              (reset! state
                                      (if (and (= :stopped (:status guard-status))
                                               (= :completed
                                                  (:selected-adapter-cleanup guard-status))
                                               (= :stopped (:module-state probe-status)))
                                        :stopped
                                        :stop-cleanup-not-established)))
                            (catch Throwable _
                              (reset! state :stop-cleanup-not-established)))
                          (summary!))
                        (recur))
                      ;; A failed startup may have unrolled partially and may
                      ;; still have an external listener: retain uncertainty.
                      (summary!)))))]
    {:start-once! start! :stop! stop! :summary! summary!
     :inspect-running! inspect-running!}))
