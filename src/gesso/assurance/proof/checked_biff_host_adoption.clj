(ns gesso.assurance.proof.checked-biff-host-adoption
  "v1478: Source-checked adoption of a v1476 Ring router in a Biff system slot.

   Issue one Biff 2 module which, when started, installs the EXACT newly
   created checked router behind Gesso's canonical HTML-response preflight
   wrapper. The module owns the :biff.ring/handler assignment; it never
   merely inspects a caller-selected table or invokes a route to test it.

   Use this module after application/application-handler-module and before any
   HTTP server start module. Its :biff.core/init supplies the checked router
   for the canonical Gesso module's earlier start. At its own start the module
   deliberately replaces that intermediate wrapper with a fresh canonical
   wrapper around the SAME checked router, so the final adopted handler has
   an exact, locally known delegate and HTML enforcement boundary.

   Rechecking requires an opaque issued handle, the ORIGINAL contract source,
   and the actual returned Biff system map. It detects changes to that map's
   handler slot; it does not authenticate a deployed server's handler slot,
   the caller's asserted Biff start order, later replacement, independent
   routes or writers, transaction authority, or universal refinement.
   The initial source and application's declared dependencies remain subject
   to the existing v1476 and application preflight checks. No transaction or
   handler invocation is made by this module or its inspection functions."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.installed-route-provenance :as routes]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)))

(def report-type
  :gesso.assurance.proof.checked-biff-host-adoption/local-handler-slot)
(def report-version 1)
(def module-id :gesso.assurance.proof.checked-biff-host-adoption/install)

(deftype HostAdoptionHandle []
  Object
  (toString [_] "#<gesso.assurance/biff-host-adoption>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-wrap application/wrap-application-handler)
(def ^:private original-describe routes/describe-checked-ring-router)
(def ^:private original-install routes/install-checked-ring-router!)
(def ^:private original-assembly? application/application-assembly?)

(def ^:private open-obligations
  #{:biff-start-order-not-independently-attested
    :server-consumes-the-returned-biff-handler-not-established
    :server-may-capture-an-earlier-handler
    :later-handler-replacement-outside-returned-system
    :no-independent-router-or-raw-writer-bypass
    :trusted-ring-request-method-uri-and-principal
    :issued-executor-to-physical-transaction-correspondence
    :complete-physical-writer-coverage
    :universal-xtdb-refinement})

(defn- reject! [reason]
  ;; Never expose handlers, contracts, requests, transaction plans or responses.
  (throw (ex-info "Invalid checked Biff host adoption."
                  {:checked-biff-host-adoption/rejection true
                   :reason reason})))

(defn- pinned-boundaries! []
  (when-not (and (identical? original-wrap
                             (var-get #'application/wrap-application-handler))
                 (identical? original-describe
                             (var-get #'routes/describe-checked-ring-router))
                 (identical? original-install
                             (var-get #'routes/install-checked-ring-router!))
                 (identical? original-assembly?
                             (var-get #'application/application-assembly?)))
    (reject! :substituted-biff-or-route-boundary)))

(defn- current-router-report! [router source]
  (pinned-boundaries!)
  (original-describe router source))

(defn- entry [handle]
  (when (instance? HostAdoptionHandle handle)
    (locking issued (.get issued handle))))

(defn checked-biff-host-adoption? [handle]
  "Process-local issuance recognition; not host or deployment attestation."
  (boolean (entry handle)))

(defn issue-checked-biff-host-module!
  "Issue {:module <Biff 2 module> :handle <opaque handle>} for ONE startup.

   The module's :biff.core/init makes the checked Ring router available as the
   original handler during Biff initialization. The canonical Gesso application
   module should be first in the Biff start order, followed by this module and
   then server modules. At its start the issued module installs a canonical
   HTML-guarded wrapper around its exact checked router. Do not put this
   module in multiple simultaneous or repeated starts; issue a fresh one.

   Issuance alone proves no adoption. Only a completed module-start callback
   followed by require-current-biff-host-adoption! can produce a local report."
  [contract-source]
  (pinned-boundaries!)
  (when-not (and (map? contract-source)
                 (original-assembly? (:application-assembly contract-source)))
    (reject! :invalid-application-source))
  (let [router (original-install contract-source)
        handle (HostAdoptionHandle.)
        init-state (atom :new)
        start-state (atom :not-started)
        module
        {:biff.core/id module-id
         :biff.core/init
         (fn [_modules-var]
           (when-not (compare-and-set! init-state :new :initialized)
             (reject! :repeated-biff-module-init))
           (current-router-report! router contract-source)
           {application/biff-ring-handler-key router})
         :biff.core/start
         (fn [system]
           ;; Poison the issuance before any callback or wrapping can fail.
           ;; A retry after an uncertain startup cannot silently issue a new
           ;; successful observation of the same module.
           (when-not (compare-and-set! start-state :not-started :started)
             (reject! :repeated-biff-module-start))
           (when-not (= :initialized @init-state)
             (reject! :biff-module-init-not-observed))
           (when-not (and (map? system)
                          (fn? (get system application/biff-ring-handler-key)))
             (reject! :missing-or-invalid-preexisting-biff-handler))
           (current-router-report! router contract-source)
           (when-not (original-assembly?
                      (:application-assembly contract-source))
             (reject! :stale-application-assembly))
           ;; This wrapper retains Gesso's actual HTML-response validation.
           ;; Do not install `router` naked after canonical startup: doing so
           ;; would remove the existing pre-serialization preflight guard.
           (let [wrapper (original-wrap
                          (:application-assembly contract-source) router)
                 result (assoc system application/biff-ring-handler-key wrapper)]
             (when-not (compare-and-set! start-state :started
                                         {:handler wrapper
                                          :system result})
               (reject! :compromised-biff-module-start))
             result))}]
    (locking issued
      (.put issued handle {:source contract-source
                           :router router
                           :module module
                           :init-state init-state
                           :start-state start-state}))
    {:module module :handle handle}))

(defn require-current-biff-host-adoption!
  "Check the completed Biff system's actual handler slot by exact identity.

   The caller supplies the Biff system returned after host startup. A copied
   report, a substituted handler, an unissued module, a swapped contract, or a
   source that fails current route checks cannot pass. No handler is invoked.
   A caller-created map can still mimic this local system slot; no deployed
   web server or external process is authenticated by this interface."
  [handle contract-source system]
  (pinned-boundaries!)
  (let [record (or (entry handle) (reject! :unissued-host-adoption))
        {:keys [source router start-state init-state]} record
        started @start-state]
    (when-not (identical? source contract-source)
      (reject! :changed-host-adoption-source))
    (when-not (= :initialized @init-state)
      (reject! :biff-module-init-not-observed))
    (when-not (and (map? started) (:handler started))
      (reject! :host-adoption-start-not-completed))
    (when-not (and (map? system)
                   (identical? (:handler started)
                               (get system application/biff-ring-handler-key)))
      (reject! :host-handler-slot-mismatch))
    (let [route-report (current-router-report! router source)]
      {:type report-type
       :version report-version
       :scope :one-in-process-biff-system-handler-slot
       :module-id module-id
       :module-start :observed-once
       :biff-module-init :observed-once
       :handler-slot :identical-to-guarded-checked-router-wrapper
       :canonical-html-preflight :wrapped-at-module-start
       :checked-route-ids (:route-ids route-report)
       :checked-certified-route-ids (:certified-route-ids route-report)
       :host-deployment :not-established
       :physical-correspondence :not-established
       :open-obligations (set/union open-obligations
                                    (:open-obligations route-report))
       :certification :not-issued
       :admission :not-admitted
       :authority :none})))
