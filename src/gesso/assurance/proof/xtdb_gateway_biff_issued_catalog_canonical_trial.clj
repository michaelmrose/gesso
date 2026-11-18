(ns gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial
  "v1777: Execute the exact v1776 catalog/assembly-bound issued executor
   THROUGH the current checked, locally adopted Biff Ring handler. Join the
   original v1775 source-derived full-atomic pre-submit trial to the actual
   certified executor interval, not to an independent second invocation.

   The handler is called once via the original Biff host-bound observation;
   inside the original certified executor only, the original catalog-bound
   trial observes the SAME canonical domain+command-ledger transaction.
   No caller-provided executor, physical report or separate writer is used.

   This is opt-in, process-local, cooperative JVM observation. A caller still
   supplies the catalog, request, context and host system; none proves the
   deployed HTTP server consumed that handler, authentic principal/business
   policy, all alternative writers, durable XTDB receipt, or Live delivery.
   Never infer authority, certification or permission to retry a transaction."
  (:require [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial
             :as issued]
            [gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial
             :as catalog-trial]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-biff-issued-catalog-canonical-trial/local-biff-executor)
(def report-version 1)

(def ^:private original-host-issuer
  host/issue-host-adopted-assembled-typed-transition-gateway!)
(def ^:private original-issued-issuer
  issued/issue-issued-catalog-canonical-trial!)
(def ^:private original-catalog-issuer
  catalog-trial/issue-catalog-bound-canonical-trial!)
(def ^:private original-executor-scope certified/observe-issued-executor-scope!)
(def ^:private original-operation-summary application/operation-summary)

(defn- reject! [reason]
  (throw (ex-info "Biff-adopted catalog-bound canonical execution not established."
                  {:biff-issued-catalog-canonical-trial/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-issuer
                             (var-get #'host/issue-host-adopted-assembled-typed-transition-gateway!))
                 (identical? original-issued-issuer
                             (var-get #'issued/issue-issued-catalog-canonical-trial!))
                 (identical? original-catalog-issuer
                             (var-get #'catalog-trial/issue-catalog-bound-canonical-trial!))
                 (identical? original-executor-scope
                             (var-get #'certified/observe-issued-executor-scope!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-host-catalog-or-executor-scope-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- selected-routes! [source operation]
  (pinned!)
  (let [operations (original-operation-summary
                    (get-in source [:application-assembly
                                    :operation-acquisition-assembly]))
        routes (get-in operations [operation :routes])]
    (when-not (and (seq routes)
                   (every? (fn [{:keys [method path route-id]}]
                             (and (keyword? method)
                                  (string? path)
                                  (.startsWith ^String path "/")
                                  (not (re-find #"[{}:*]" path))
                                  (qualified-keyword? route-id)
                                  (certified/certified-handler?
                                   (get-in source
                                           [:semantic-route-handlers route-id]))))
                           routes)
                   (= (count routes)
                      (count (set (map (juxt :method :path) routes)))))
      (reject! :invalid-current-selected-certified-routes))
    (into {} (map (fn [{:keys [method path route-id]}]
                    [[method path] route-id]) routes))))

(defn- report [operation status host? catalog-status]
  {:type report-type :version report-version
   :operation-id operation :status status
   :scope :one-local-biff-handler-and-original-executor-invocation
   :host-handler-return (if host?
                          :one-original-adopted-biff-handler-returned
                          :not-established)
   :certified-executor-return (if host?
                                :one-original-issued-executor-returned
                                :not-established)
   :catalog-derived-atomic-outcome catalog-status
   :canonical-physical-execution
   (if (= status :bounded-biff-issued-catalog-atomic-acknowledgment)
     :one-source-relative-model-transaction-acknowledged
     :not-established)
   :host-slot-provenance :checked-locally-adopted-system-map
   :catalog-executor-provenance :current-assembled-issued-plan-source
   :deployed-http-handler-adoption :not-established
   :authenticated-request-principal :not-established
   :business-policy-authenticity :not-established
   :complete-physical-writer-coverage :not-established
   :authenticated-xtdb-receipt :not-established
   :typed-complete-cas-writer-adoption :not-established
   :actual-live-delivery :not-established
   :universal-formal-refinement :not-established
   :retry-authority :none
   :certificate :not-issued :admission :not-admitted
   :transaction-authority :none :authority :none})

(defn issue-biff-issued-catalog-canonical-trial!
  "Issue a one-use process-local execution observation for a currently started
   checked Biff host. The v1776 ORIGINAL issued-plan binding is checked at
   issuance and before/after the handler invocation; v1775's original
   catalog-derived typed full-atomic trial executes ONLY around the actual
   issued executor called by that handler. The original host :observe-handler!
   invokes the exact adopted Ring slot and confirms executor return.

   One actual XTDB command may have been committed even if validation or
   observation later fails; repeating the request is NEVER allowed as a way
   to obtain more evidence. This does not bind the authenticated user or an
   HTTP server in another process and does not issue a certificate."
  [host-handle contract-source biff-system catalog-source operation inputs]
  (pinned!)
  (when-not (and (plain-map? catalog-source)
                 (plain-map? (:catalog catalog-source))
                 (plain-map? inputs))
    (reject! :invalid-original-catalog-or-inputs))
  (let [host-bound (original-host-issuer
                    host-handle contract-source biff-system
                    (:catalog catalog-source) operation)
        issued-bound (original-issued-issuer contract-source catalog-source
                                             operation inputs)
        physical (original-catalog-issuer catalog-source operation inputs)
        host-initial ((:summary! host-bound))
        issued-initial ((:summary! issued-bound))
        routes (selected-routes! contract-source operation)
        _ (when-not (and (= host/host-typed-binding-type (:type host-initial))
                         (= host/host-typed-binding-version (:version host-initial))
                         (= operation (:operation-id host-initial)
                            (:operation-id issued-initial))
                         (= :complete-current-certified-literal-routes
                            (:route-coverage host-initial))
                         (= issued/report-type (:type issued-initial))
                         (= issued/report-version (:version issued-initial))
                         (= :not-attempted (:status issued-initial))
                         (= :not-established
                            (:deployed-biff-handler-adoption issued-initial)))
            (reject! :unqualified-assembled-host-or-catalog-executor))
        permit (AtomicBoolean. false)
        state (atom :not-attempted)
        host-observed? (atom false)
        physical-status (atom :not-established)
        recheck! (fn []
                   (pinned!)
                   (when-not (= issued-initial ((:summary! issued-bound)))
                     (reject! :issued-catalog-executor-changed))
                   (let [now ((:summary! host-bound))]
                     (when-not (and (= (:operation-id host-initial)
                                       (:operation-id now))
                                    (= (:host-slot-binding host-initial)
                                       (:host-slot-binding now))
                                    (= (:route-coverage host-initial)
                                       (:route-coverage now))
                                    (= :not-established
                                       (:deployed-http-handler-adoption now)))
                       (reject! :adopted-biff-slot-or-source-changed)))
                   (when-not (= routes (selected-routes! contract-source operation))
                     (reject! :certified-literal-routes-changed))
                   true)
        summary! (fn []
                   (recheck!)
                   (report operation @state @host-observed? @physical-status))
        observe! (fn [request]
                   (when-not (.compareAndSet permit false true)
                     (reject! :biff-issued-catalog-trial-already-attempted))
                   (reset! state :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (and (plain-map? request)
                                  (contains? routes
                                             [(:request-method request)
                                              (:uri request)]))
                     (reject! :request-does-not-select-issued-certified-route))
                   (let [route-id (get routes
                                       [(:request-method request) (:uri request)])
                         handler (get-in contract-source
                                         [:semantic-route-handlers route-id])
                         trial-result (atom nil)
                         returned (volatile! false)
                         {:keys [result executor-scope]}
                         (original-executor-scope
                          handler request
                          (fn [invoke-original-executor!]
                            ;; No second call or caller-provided executor:
                            ;; invoke-original-executor! comes only from the
                            ;; exact registered certified handler's executor
                            ;; scope, and may itself be called only once.
                            (let [original-result (volatile! nil)
                                  trial ((:observe-once! physical)
                                         (fn []
                                           (let [r (invoke-original-executor!)]
                                             (vreset! original-result r)
                                             (vreset! returned true)
                                             r)))]
                              (reset! trial-result trial)
                              @original-result))
                          (fn [] ((:observe-handler! host-bound) request)))
                         host-report result
                         original-report @trial-result
                         host-qualified?
                         (and (= :one-original-issued-executor-returned
                                 executor-scope)
                              @returned
                              (map? host-report)
                              (= host/host-typed-binding-type (:type host-report))
                              (= host/host-typed-binding-version
                                 (:version host-report))
                              (= operation (:operation-id host-report))
                              (= :one-in-process-biff-handler-slot-invocation
                                 (:dispatch host-report))
                              (= :one-exact-issued-handler-executor-returned
                                 (:issued-executor-invocation host-report))
                              (= 200 (:ring-response-status host-report))
                              (= :not-established
                                 (:deployed-http-handler-adoption host-report)))
                         physical-qualified?
                         (and (= catalog-trial/report-type
                                 (:type original-report))
                              (= catalog-trial/report-version
                                 (:version original-report))
                              (= operation (:operation-id original-report))
                              (= :not-issued (:certificate original-report))
                              (= :none (:authority original-report)))
                         outcome (cond
                                   (not (and host-qualified? physical-qualified?))
                                   :unqualified-issued-handler-or-physical-result
                                   (= :bounded-catalog-bound-atomic-acknowledgment
                                      (:status original-report))
                                   :bounded-biff-issued-catalog-atomic-acknowledgment
                                   (= :catalog-bound-canonical-rejected
                                      (:status original-report))
                                   :biff-issued-catalog-canonical-rejected
                                   (= :catalog-bound-typed-check-not-established
                                      (:status original-report))
                                   :biff-issued-catalog-typed-check-rejected
                                   :else :not-qualified-or-uncertain)]
                     (recheck!)
                     (reset! host-observed? host-qualified?)
                     (reset! physical-status
                             (if physical-qualified?
                               (:status original-report)
                               :not-established))
                     (reset! state outcome)
                     (report operation outcome host-qualified? @physical-status)))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :observe-once! observe!}))
