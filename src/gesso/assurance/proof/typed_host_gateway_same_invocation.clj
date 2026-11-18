(ns gesso.assurance.proof.typed-host-gateway-same-invocation
  "v1766: A one-use, process-local observation joining an actual v1759
   host-adopted certified Biff handler return with the v1765 private
   source-bound typed gateway return trace, in ONE synchronous invocation.

   This observer invokes the *existing* checked Biff handler once under the
   original typed gateway's dynamic observation. It never calls :submit!
   itself, accepts no caller-supplied witness, or correlates independent
   reports after execution. A positive result requires both the v1761 exact
   certified executor return and one matching same-thread v1757 gateway
   submission return during that handler invocation.

   This is NOT evidence that the certified executor (as opposed to a trusted
   request/response callback) performed that gateway submission, that the
   gateway caused the handler's reported result, that the application policy
   is authentic, that XTDB committed, or that deployed HTTP uses this Biff
   handler. No credential, transaction capability, or certificate is issued."
  (:require [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.typed-host-gateway-same-invocation/local-joined-observation)
(def report-version 1)

(def ^:private original-host-issuer
  host/issue-host-adopted-assembled-typed-transition-gateway!)
(def ^:private original-typed-observer
  typed/observe-source-bound-typed-submission!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported same-invocation typed host/gateway observation."
                  {:typed-host-gateway-same-invocation/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-issuer
                             (var-get #'host/issue-host-adopted-assembled-typed-transition-gateway!))
                 (identical? original-typed-observer
                             (var-get #'typed/observe-source-bound-typed-submission!)))
    (reject! :substituted-host-or-typed-trace-observer)))

(defn issue-same-invocation-observer!
  "Issue a one-attempt observer for the selected operation in a CURRENT,
   locally started Biff host. The caller provides NO gateway submission
   closure or independent physical result; the actual handler may invoke
   a genuine v1757 source-bound gateway during its own synchronous call.
   Results are redacted and nonauthoritative. An uncertain observation may
   have performed a physical write and MUST NOT be retried by this observer."
  [host-handle contract-source biff-system catalog operation]
  (pinned!)
  (let [bound (original-host-issuer host-handle contract-source biff-system
                                    catalog operation)
        initial ((:summary! bound))
        _ (when-not (and (= host/host-typed-binding-type (:type initial))
                         (= host/host-typed-binding-version (:version initial))
                         (= operation (:operation-id initial))
                         (= :complete-current-certified-literal-routes
                            (:route-coverage initial))
                         (= :not-established
                            (:original-business-policy-authenticity initial)))
            (reject! :invalid-checked-host-operation))
        attempted (AtomicBoolean. false)
        state (atom :not-attempted)
        recheck! (fn []
                   (pinned!)
                   ;; The original host-bound summary rechecks the current
                   ;; Biff slot, original catalog, assembly and route issuer.
                   (let [now ((:summary! bound))]
                     (when-not (and (= operation (:operation-id now))
                                    (= (:host-slot-binding initial)
                                       (:host-slot-binding now))
                                    (= (:route-coverage initial)
                                       (:route-coverage now))
                                    (= :not-established
                                       (:deployed-http-handler-adoption now)))
                       (reject! :host-bound-source-or-slot-changed)))
                   true)
        summary! (fn []
                   (recheck!)
                   {:type report-type :version report-version
                    :operation-id operation
                    :status @state
                    :host-handler :exact-current-in-process-biff-handler
                    :issued-executor-return :not-established
                    :source-bound-gateway-return :not-established
                    :gateway-in-certified-executor :not-established
                    :gateway-to-reported-command-causality :not-established
                    :actual-xtdb-commit :not-established
                    :deployed-http-handler-adoption :not-established
                    :business-policy-authenticity :not-established
                    :complete-deployed-writer-coverage :not-established
                    :certificate :not-issued :admission :not-admitted
                    :authority :none})
        observe! (fn [request]
                   (when-not (.compareAndSet attempted false true)
                     (reject! :same-invocation-already-attempted))
                   (reset! state :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (let [{:keys [execution report]}
                         (original-typed-observer
                          operation (fn [] ((:observe-handler! bound) request)))
                         handler (:result execution)
                         handler-qualified?
                         (and (= :returned (:outcome execution))
                              (map? handler)
                              (= host/host-typed-binding-type (:type handler))
                              (= operation (:operation-id handler))
                              (= :one-in-process-biff-handler-slot-invocation
                                 (:dispatch handler))
                              (= :one-exact-issued-handler-executor-returned
                                 (:issued-executor-invocation handler)))
                         gateway-qualified?
                         (and (= typed/source-bound-trace-type (:type report))
                              (= typed/source-bound-trace-version (:version report))
                              (= operation (:operation-id report))
                              (= :one-synchronous-process-local-invocation
                                 (:scope report))
                              (= :one-source-bound-typed-submit-return-observed
                                 (:status report))
                              (= :observed (:source-bound-gateway-return report))
                              (= :not-established (:xtdb-transaction-committed report))
                              (= :not-issued (:certificate report))
                              (= :none (:authority report)))
                         result (if (and handler-qualified? gateway-qualified?)
                                  :bounded-biff-handler-and-typed-gateway-return
                                  :no-qualified-same-invocation-gateway-return)]
                     (recheck!)
                     (when-not handler-qualified?
                       (reject! :checked-biff-handler-did-not-return))
                     (reset! state result)
                     {:type report-type :version report-version
                      :operation-id operation
                      :status result
                      :host-handler :one-checked-biff-handler-invocation
                      :issued-executor-return
                      :one-exact-issued-handler-executor-returned
                      :source-bound-gateway-return
                      (if gateway-qualified? :one-same-thread-return-observed
                          :not-established)
                      :gateway-in-certified-executor :not-established
                      :gateway-to-reported-command-causality :not-established
                      :actual-xtdb-commit :not-established
                      :deployed-http-handler-adoption :not-established
                      :business-policy-authenticity :not-established
                      :complete-deployed-writer-coverage :not-established
                      :certificate :not-issued :admission :not-admitted
                      :authority :none}))]
    (recheck!)
    {:type report-type :version report-version
     :observe-once! observe! :summary! summary!}))

;; v1767: This is the stronger, executor-scoped companion to v1766's
;; whole-handler diagnostic. The latter intentionally remains unchanged.
(def executor-scope-report-type
  :gesso.assurance.proof.typed-host-gateway-same-invocation/executor-scoped-observation)
(def executor-scope-report-version 1)
(def ^:private original-executor-scope
  certified/observe-issued-executor-scope!)
(def ^:private original-operation-summary application/operation-summary)

(defn- pinned-executor-scope! []
  (pinned!)
  (when-not (and (identical? original-executor-scope
                             (var-get #'certified/observe-issued-executor-scope!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-executor-scope-boundary)))

(defn- current-executor-routes! [contract-source operation]
  (pinned-executor-scope!)
  (let [assembly (:application-assembly contract-source)
        operations (original-operation-summary
                    (:operation-acquisition-assembly assembly))
        rows (get-in operations [operation :routes])]
    (when-not (and (seq rows)
                   (every? (fn [{:keys [route-id method path]}]
                             (and (keyword? route-id) (namespace route-id)
                                  (keyword? method) (string? path)
                                  (.startsWith ^String path "/")
                                  (not (re-find #"[{}:*]" path))
                                  (certified/certified-handler?
                                   (get-in contract-source
                                           [:semantic-route-handlers route-id]))))
                           rows)
                   (= (count rows)
                      (count (set (map (juxt :method :path) rows)))))
      (reject! :invalid-current-issued-executor-route))
    (into {} (map (fn [{:keys [method path route-id]}]
                    [[method path] route-id]) rows))))

(defn issue-executor-scoped-gateway-observer!
  "Observe one selected source-bound typed gateway return exclusively during
   the exact issued certified-plan executor's *dynamic call interval* in the
   adopted in-process Biff handler. The original input callback completes
   before tracing begins and the response callback runs after tracing ends.
   Only one actual, identical-request issued executor and one same-thread
   typed source-bound gateway return can qualify. No submitted transaction
   is invented or replayed to obtain evidence.

   This does not establish that the canonical executor itself directed the
   gateway rather than a TRUSTED callback it invoked; nor does it establish
   a causal relation to the returned command, authenticated XTDB durability,
   business-rule correctness, writer completeness or deployed HTTP adoption."
  [host-handle contract-source biff-system catalog operation]
  (pinned-executor-scope!)
  (let [bound (original-host-issuer host-handle contract-source biff-system
                                    catalog operation)
        initial ((:summary! bound))
        routes (current-executor-routes! contract-source operation)
        _ (when-not (and (= host/host-typed-binding-type (:type initial))
                         (= host/host-typed-binding-version (:version initial))
                         (= operation (:operation-id initial))
                         (= :complete-current-certified-literal-routes
                            (:route-coverage initial))
                         (= :not-established
                            (:original-business-policy-authenticity initial)))
            (reject! :invalid-checked-host-operation))
        attempted (AtomicBoolean. false)
        state (atom :not-attempted)
        recheck! (fn []
                   (pinned-executor-scope!)
                   (let [now ((:summary! bound))]
                     (when-not (and (= operation (:operation-id now))
                                    (= (:host-slot-binding initial)
                                       (:host-slot-binding now))
                                    (= (:route-coverage initial)
                                       (:route-coverage now))
                                    (= routes
                                       (current-executor-routes! contract-source
                                                                 operation)))
                       (reject! :host-or-issued-executor-routes-changed)))
                   true)
        summary! (fn []
                   (recheck!)
                   {:type executor-scope-report-type
                    :version executor-scope-report-version
                    :operation-id operation
                    :status @state
                    :certified-executor-scope :not-established
                    :source-bound-gateway-inside-executor :not-established
                    :gateway-directed-by-canonical-executor :not-established
                    :gateway-to-command-result-causality :not-established
                    :xtdb-commit-authenticity :not-established
                    :original-business-policy-authenticity :not-established
                    :deployed-http-handler-adoption :not-established
                    :complete-deployed-writer-coverage :not-established
                    :certificate :not-issued :admission :not-admitted
                    :authority :none})
        observe! (fn [request]
                   (when-not (.compareAndSet attempted false true)
                     (reject! :executor-scoped-observation-already-attempted))
                   (reset! state :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (and (map? request) (nil? (meta request))
                                  (contains? routes
                                             [(:request-method request)
                                              (:uri request)]))
                     (reject! :request-is-not-selected-executor-route))
                   (let [route-id (get routes
                                       [(:request-method request) (:uri request)])
                         handler (get-in contract-source
                                         [:semantic-route-handlers route-id])
                         trace (atom nil)
                         {:keys [result executor-scope]}
                         (original-executor-scope
                          handler request
                          (fn [invoke-executor!]
                            ;; The typed observer exists only inside this
                            ;; callback, and its invoke! is the original
                            ;; issued executor, not a route input callback.
                            (let [{:keys [execution report]}
                                  (original-typed-observer operation
                                                           invoke-executor!)]
                              (reset! trace report)
                              (if (= :returned (:outcome execution))
                                (:result execution)
                                (throw (:error execution)))))
                          (fn [] ((:observe-handler! bound) request)))
                         gateway @trace
                         handler-qualified?
                         (and (= :one-original-issued-executor-returned
                                 executor-scope)
                              (map? result)
                              (= host/host-typed-binding-type (:type result))
                              (= operation (:operation-id result))
                              (= :one-in-process-biff-handler-slot-invocation
                                 (:dispatch result))
                              (= :one-exact-issued-handler-executor-returned
                                 (:issued-executor-invocation result)))
                         gateway-qualified?
                         (and (= typed/source-bound-trace-type (:type gateway))
                              (= typed/source-bound-trace-version
                                 (:version gateway))
                              (= operation (:operation-id gateway))
                              (= :one-synchronous-process-local-invocation
                                 (:scope gateway))
                              (= :one-source-bound-typed-submit-return-observed
                                 (:status gateway))
                              (= :observed (:source-bound-gateway-return gateway))
                              (= :not-established (:xtdb-transaction-committed gateway))
                              (= :not-issued (:certificate gateway))
                              (= :none (:authority gateway)))
                         status (if (and handler-qualified? gateway-qualified?)
                                  :bounded-gateway-return-in-issued-executor-scope
                                  :no-gateway-return-in-issued-executor-scope)]
                     (recheck!)
                     (when-not handler-qualified?
                       (reject! :issued-executor-scope-did-not-return))
                     (reset! state status)
                     {:type executor-scope-report-type
                      :version executor-scope-report-version
                      :operation-id operation
                      :status status
                      :certified-executor-scope
                      :one-original-issued-executor-returned
                      :source-bound-gateway-inside-executor
                      (if gateway-qualified? :one-same-thread-return-observed
                          :not-established)
                      :gateway-directed-by-canonical-executor :not-established
                      :gateway-to-command-result-causality :not-established
                      :xtdb-commit-authenticity :not-established
                      :original-business-policy-authenticity :not-established
                      :deployed-http-handler-adoption :not-established
                      :complete-deployed-writer-coverage :not-established
                      :certificate :not-issued :admission :not-admitted
                      :authority :none}))]
    (recheck!)
    {:type executor-scope-report-type
     :version executor-scope-report-version
     :observe-once! observe! :summary! summary!}))
