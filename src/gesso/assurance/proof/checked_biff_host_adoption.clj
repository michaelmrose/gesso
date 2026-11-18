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
            [gesso.assurance.proof.xtdb-gateway-assembled-typed-transition-seam
             :as assembled]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)
           (java.util.concurrent.atomic AtomicBoolean)))

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


;; v1759: Join the v1758 source-relative typed XTDB preparer to the EXACT
;; router already captured by the v1478 Biff host-adoption issuer. The caller
;; must not be allowed to substitute a separately registered checked router.
(def host-typed-binding-type
  :gesso.assurance.proof.checked-biff-host-adoption/host-typed-binding)
(def host-typed-binding-version 1)
(def ^:private original-assembled-issuer
  assembled/issue-assembled-source-bound-typed-transition-gateway!)
(def ^:private original-host-recheck require-current-biff-host-adoption!)
(def ^:private original-operation-summary application/operation-summary)

(defn issue-host-adopted-assembled-typed-transition-gateway!
  "Bind ONE declared typed operation to the exact checked Ring router captured
   by an issued and locally started Biff host-adoption module. Requires the
   original adoption handle, contract source and returned Biff system map;
   the caller cannot supply a replacement router. Rechecks the original host
   slot, assembled source, and certified route coverage at issuance, prepare,
   summary and the sole physical XTDB submission attempt.

   Returned :prepare! accepts the v1758 arguments
   [node resolver-issuance request candidate inputs field-map]. The separate
   one-use :observe-handler! directly invokes the current Biff system handler
   on one exact, selected, literal route and returns only a redacted report.
   Observation is NOT coupled to :prepare! or :submit!: even successful
   invocation gives no evidence that this request caused an XTDB write.
   This is a CURRENT IN-PROCESS system-slot join, not evidence of a deployed
   HTTP server's handler, a complete original business policy, authorized
   actor, independent writer completeness, or universal refinement."
  [host-handle contract-source biff-system catalog selected-operation]
  (pinned-boundaries!)
  (when-not (and (identical? original-assembled-issuer
                             (var-get #'assembled/issue-assembled-source-bound-typed-transition-gateway!))
                 (identical? original-host-recheck
                             (var-get #'require-current-biff-host-adoption!)))
    (reject! :substituted-host-or-assembled-boundary))
  (let [host-report (original-host-recheck host-handle contract-source
                                           biff-system)
        record (or (entry host-handle)
                   (reject! :unissued-host-adoption))
        router (:router record)
        bound (original-assembled-issuer catalog contract-source router
                                          selected-operation)
        initial ((:summary! bound))
        selected-routes!
        (fn []
          (when-not (identical? original-operation-summary
                                (var-get #'application/operation-summary))
            (reject! :substituted-operation-summary))
          (let [operations (original-operation-summary
                            (:operation-acquisition-assembly
                             (:application-assembly contract-source)))
                routes (get-in operations [selected-operation :routes])]
            (when-not (and (seq routes)
                           (every? (fn [{:keys [method path]}]
                                     (and (keyword? method)
                                          (string? path)
                                          (.startsWith ^String path "/")
                                          (not (re-find #"[{}:*]" path))))
                                   routes))
              (reject! :invalid-selected-literal-routes))
            (set (map (juxt :method :path) routes))))
        selected-routes (selected-routes!)
        host-routes (:checked-certified-route-ids host-report)
        _ (when-not (and (= :identical-to-guarded-checked-router-wrapper
                            (:handler-slot host-report))
                         (= :not-established (:host-deployment host-report))
                         (= assembled/binding-type (:type initial))
                         (= assembled/binding-version (:version initial))
                         (= :closed-declared-set-equals-assembly
                            (:source-operation-coverage initial))
                         (= :all-declared-literal-routes-rechecked
                            (:route-coverage initial))
                         (= host-routes (:checked-route-ids host-report))
                         (seq host-routes))
            (reject! :host-and-assembled-route-coverage-diverged))
        recheck! (fn []
                   (pinned-boundaries!)
                   (when-not (and (identical? original-assembled-issuer
                                              (var-get #'assembled/issue-assembled-source-bound-typed-transition-gateway!))
                                  (identical? original-host-recheck
                                              (var-get #'require-current-biff-host-adoption!)))
                     (reject! :substituted-host-or-assembled-boundary))
                   (when-not (= host-report
                                (original-host-recheck host-handle
                                                       contract-source biff-system))
                     (reject! :host-handler-or-source-changed))
                   (when-not (= initial ((:summary! bound)))
                     (reject! :assembled-typed-source-changed))
                   (when-not (= selected-routes (selected-routes!))
                     (reject! :selected-literal-routes-changed))
                   true)
        observe-attempt (AtomicBoolean. false)
        summary! (fn []
                   (recheck!)
                   {:type host-typed-binding-type
                    :version host-typed-binding-version
                    :operation-id selected-operation
                    :host-slot-binding :exact-issued-checked-router-under-canonical-wrapper
                    :host-module-start :observed-once
                    :route-coverage :complete-current-certified-literal-routes
                    :handler-dispatch (if (.get observe-attempt)
                                        :one-shot-attempt-consumed
                                        :available-one-shot-not-yet-attempted)
                    :handler-dispatch-to-xtdb-causality :not-established
                    :original-business-policy-authenticity :not-established
                    :deployed-http-handler-adoption :not-established
                    :complete-deployed-writer-coverage :not-established
                    :formal-to-physical-proof :not-established
                    :certificate :not-issued :admission :not-admitted
                    :transaction-authority :none :authority :none})
        prepare! (fn [node resolver-issuance request candidate inputs field-map]
                   (recheck!)
                   (let [prepared ((:prepare! bound) node resolver-issuance
                                   request candidate inputs field-map)
                         attempt (AtomicBoolean. false)
                         submit! (fn []
                                   (when-not (.compareAndSet attempt false true)
                                     (reject! :host-typed-one-shot-already-attempted))
                                   (recheck!)
                                   ((:submit! prepared)))
                         prepared-summary! (fn []
                                             (recheck!)
                                             (assoc ((:summary! prepared))
                                                    :host-slot-binding
                                                    :exact-issued-checked-router-under-canonical-wrapper
                                                    :deployed-http-handler-adoption :not-established
                                                    :original-business-policy-authenticity :not-established
                                                    :complete-deployed-writer-coverage :not-established
                                                    :production-certification :not-issued))]
                     (recheck!)
                     {:type (:type prepared)
                      :version (:version prepared)
                      :submit! submit!
                      :summary! prepared-summary!}))]
    (recheck!)
    {:type host-typed-binding-type
     :version host-typed-binding-version
     :prepare! prepare!
     :observe-handler!
     (fn [request]
         ;; Consume the local observation before checking any user-supplied
         ;; request. Neither a rejection nor an uncertain handler execution
         ;; may become a second invocation by retrying the same capability.
         (when-not (.compareAndSet observe-attempt false true)
           (reject! :host-handler-observation-already-attempted))
         (recheck!)
         (when-not (and (map? request)
                        (contains? selected-routes
                                   [(:request-method request) (:uri request)]))
           (reject! :request-is-not-a-bound-operation-route))
         ;; Invoke the EXACT Biff slot, including its canonical HTML guard,
         ;; not the separately registered bare Ring router. No handler
         ;; substitution, simulated response or additional XTDB submission.
         (let [response ((get biff-system application/biff-ring-handler-key)
                         request)]
           (recheck!)
           (when-not (and (map? response)
                          (integer? (:status response))
                          (<= 100 (:status response) 599))
             (reject! :invalid-observed-ring-response))
           {:type host-typed-binding-type
            :version host-typed-binding-version
            :operation-id selected-operation
            :dispatch :one-in-process-biff-handler-slot-invocation
            :ring-response-status (:status response)
            :request-principal-authenticity :not-established
            :handler-dispatch-to-xtdb-causality :not-established
            :deployed-http-handler-adoption :not-established
            :original-business-policy-authenticity :not-established
            :complete-deployed-writer-coverage :not-established
            :certificate :not-issued :admission :not-admitted
            :authority :none}))
     :summary! summary!}))
