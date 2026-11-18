(ns gesso.assurance.proof.checked-biff-http-application-configuration
  "v1784: Application-owned entry point for one selected CLOSED Biff HTTP
   startup. One declared contract source and selected adapter are enough:
   issue the ORIGINAL host, consumption probe and guarded adapter internally,
   derive the canonical module Var/order, and execute v1783's pre-init sealed
   startup. Callers cannot accidentally supply a different module inventory,
   an unissued proof record, or an earlier start order through this API.

   This is a consciously selected application STARTUP CONFIGURATION, not a
   replacement for Biff's unrestricted start API. This code does not control
   independent Biff instances, raw socket listeners, a remote deployment,
   authentic request principals or physical transaction writers. It issues
   no proof certificate, admission, retry or transaction authority."
  (:require [com.biffweb.core :as biff]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.live.application-preflight :as app])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.checked-biff-http-application-configuration/selected-start)
(def report-version 1)

;; The ONLY Biff module Var used by this configured entry point. Its dynamic
;; binding is scoped around BOTH the v1783 preflight and Biff startup.
(def ^:dynamic ^:private *configured-modules* [])

(def ^:private original-host-issuer host/issue-checked-biff-host-module!)
(def ^:private original-probe-issuer probe/issue-http-consumption-probe-module!)
(def ^:private original-guarded-issuer guarded/issue-checked-http-adapter-start!)
(def ^:private original-sealed-start sealed/start-closed-checked-http-application!)
(def ^:private original-biff-stop biff/stop)

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
                             (var-get #'biff/stop)))
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

(defn- report [state]
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
        summary! (fn [] (pinned!) (report @state))
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
                   (let [result (binding [*configured-modules* modules]
                                  (original-sealed-start
                                   (:application-assembly contract-source)
                                   initial-system issued-host issued-probe
                                   issued-guarded #'*configured-modules* order))]
                     (when-not (and (= sealed/report-type
                                       (get-in result [:report :type]))
                                    (= :one-selected-verified-adapter-start
                                       (get-in result [:report :physical-server-start]))
                                    (= :none (get-in result [:report :authority])))
                       (when-let [system (:system result)]
                         (try (original-biff-stop system)
                              (catch Throwable _ nil)))
                       (reject! :unqualified-closed-http-start))
                     (when-not (compare-and-set! state :starting
                                                  :verified-configured-start)
                       (try (original-biff-stop (:system result))
                            (catch Throwable _ nil))
                       (reject! :configured-start-state-diverged))
                     (reset! started (:system result))
                     {:system (:system result)
                      :report (report @state)})
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
                            (reset! state :stopped)
                            (catch Throwable _
                              (reset! state :stop-cleanup-not-established)))
                          (summary!))
                        (recur))
                      ;; A failed startup may have unrolled partially and may
                      ;; still have an external listener: retain uncertainty.
                      (summary!)))))]
    {:start-once! start! :stop! stop! :summary! summary!}))
