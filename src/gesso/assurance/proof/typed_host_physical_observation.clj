(ns gesso.assurance.proof.typed-host-physical-observation
  "v1762: Bind a source-relative typed operation selection to the existing
   v1480 *same-handler-invocation* resolved-operation/XTDB-adapter diagnostic.

   A positive result means that the current, issued Biff handler's selected
   operation and one process-local adapter call agreed on the canonical
   resolved-operation plan during ONE synchronous invocation. It does NOT
   establish that the separately issued typed complete-CAS gateway was used,
   that the caller's typed catalog is the installed business policy, that
   an XTDB API/receipt is authentic, or that deployed HTTP adopted the slot.
   No opaque physical witness, transaction capability, request or response
   escapes this redacted observer. A failed or ambiguous attempt is consumed."
  (:require [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.installed-route-physical-correlation :as physical]
            [gesso.live.application-preflight :as application]
            [gesso.model.resolved-operation :as resolved])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type :gesso.assurance.proof.typed-host-physical-observation/local-bound-observation)
(def report-version 1)

(def ^:private original-host-issuer
  host/issue-host-adopted-assembled-typed-transition-gateway!)
(def ^:private original-observe physical/observe-installed-route-physical!)
(def ^:private original-require physical/require-current-installed-physical!)
(def ^:private original-operation-summary application/operation-summary)

(defn- reject! [reason]
  (throw (ex-info "Unsupported typed host physical observation."
                  {:typed-host-physical-observation/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-issuer
                             (var-get #'host/issue-host-adopted-assembled-typed-transition-gateway!))
                 (identical? original-observe
                             (var-get #'physical/observe-installed-route-physical!))
                 (identical? original-require
                             (var-get #'physical/require-current-installed-physical!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-host-or-correlation-boundary)))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- operation-routes! [source operation]
  (pinned!)
  (let [assembly (:application-assembly source)
        operations (original-operation-summary
                    (:operation-acquisition-assembly assembly))
        routes (get-in operations [operation :routes])]
    (when-not (and (seq routes)
                   (every? (fn [{:keys [route-id method path]}]
                             (and (keyword? route-id) (namespace route-id)
                                  (keyword? method) (string? path)
                                  (.startsWith ^String path "/")
                                  (not (re-find #"[{}:*]" path))))
                           routes)
                   (= (count routes)
                      (count (set (map (juxt :method :path) routes)))))
      (reject! :invalid-selected-typed-route))
    (into {} (map (fn [{:keys [method path route-id]}]
                    [[method path] route-id]) routes))))

(defn issue-typed-host-physical-observer!
  "Issue a one-attempt observer using one original catalog, current assembly,
   issued Biff module, and selected typed operation. Observe the EXACT adopted
   handler through v1480, not by calling the independent typed gateway's
   :submit! or by replaying a saved transaction. Any diagnostic failure
   consumes the attempt, even when XTDB execution may already have occurred.

   No caller-supplied physical report, router, transaction or witness accepted.
   A qualified return is only conditional local adapter correspondence, NOT
   proof of typed transition refinement or production installation."
  [host-handle contract-source system original-catalog selected-operation]
  (pinned!)
  (let [bound (original-host-issuer host-handle contract-source system
                                    original-catalog selected-operation)
        initial ((:summary! bound))
        routes (operation-routes! contract-source selected-operation)
        _ (when-not (and (= host/host-typed-binding-type (:type initial))
                         (= host/host-typed-binding-version (:version initial))
                         (= selected-operation (:operation-id initial))
                         (= :complete-current-certified-literal-routes
                            (:route-coverage initial))
                         (= :not-established
                            (:original-business-policy-authenticity initial)))
            (reject! :invalid-source-bound-host-gateway))
        recheck! (fn []
                   (pinned!)
                   (when-not (and (= initial ((:summary! bound)))
                                  (= routes (operation-routes! contract-source
                                                               selected-operation)))
                     (reject! :typed-host-or-declared-routes-changed))
                   true)
        consumed (AtomicBoolean. false)
        observed (atom :not-observed)
        summary! (fn []
                   (recheck!)
                   {:type report-type :version report-version
                    :operation-id selected-operation
                    :observation-status (if (.get consumed)
                                          (if (= :not-observed @observed)
                                            :attempt-consumed-without-qualified-result
                                            @observed)
                                          :not-attempted)
                    :typed-operation-route :original-catalog-and-current-assembly-checked
                    :typed-to-certified-executor-transition-equivalence :not-established
                    :typed-gateway-used-by-handler :not-established
                    :original-business-policy-authenticity :not-established
                    :deployed-http-handler-adoption :not-established
                    :xtdb-api-and-receipt-authenticity :not-established
                    :complete-deployed-writer-coverage :not-established
                    :formal-to-physical-proof :not-established
                    :certificate :not-issued :admission :not-admitted
                    :transaction-authority :none :authority :none})
        observe! (fn [request]
                   (when-not (.compareAndSet consumed false true)
                     (reject! :one-shot-observation-already-attempted))
                   (recheck!)
                   (when-not (and (plain-map? request)
                                  (not (contains? request
                                                  resolved/observation-context-key))
                                  (not (contains? request
                                                  resolved/construction-input-context-key))
                                  (contains? routes
                                             [(:request-method request) (:uri request)]))
                     (reject! :request-does-not-select-bound-typed-operation))
                   (let [route-id (get routes [(:request-method request) (:uri request)])
                         options {:host-handle host-handle
                                  :contract-source contract-source
                                  :system system :request request}
                         {:keys [report handle]} (original-observe options)
                         positive? (= :bounded-installed-route-to-adapter-agreement
                                      (:status report))]
                     (when-not (and (plain-map? report)
                                    (= physical/report-type (:type report))
                                    (= physical/report-version (:version report))
                                    (= selected-operation (:operation-id report))
                                    (= route-id (:route-id report))
                                    (= :not-established (:actual-server-deployment report))
                                    (= false (:xtdb-receipt-authenticated? report))
                                    (= :not-issued (:certificate report))
                                    (= :none (:authority report))
                                    (if positive?
                                      (and handle
                                           (= report
                                              (original-require handle options report)))
                                      (nil? handle)))
                       (reject! :invalid-or-divergent-physical-correlation))
                     (recheck!)
                     (let [status (if positive?
                                    :bounded-same-invocation-adapter-correlation
                                    :no-qualified-same-invocation-adapter-correlation)]
                       (reset! observed status)
                       {:type report-type :version report-version
                        :operation-id selected-operation
                        :status status
                        :physical-observation-status (:status report)
                        :resolved-executor-attempt-observed?
                        (:actual-issued-executor-attempt-observed? report)
                        :one-adapter-call-observed?
                        (:one-adapter-call-observed? report)
                        :typed-to-certified-executor-transition-equivalence
                        :not-established
                        :typed-gateway-used-by-handler :not-established
                        :original-business-policy-authenticity :not-established
                        :deployed-http-handler-adoption :not-established
                        :xtdb-api-and-receipt-authenticity :not-established
                        :complete-deployed-writer-coverage :not-established
                        :formal-to-physical-proof :not-established
                        :certificate :not-issued :admission :not-admitted
                        :transaction-authority :none :authority :none})))]
    (recheck!)
    {:type report-type :version report-version
     :observe-once! observe! :summary! summary!}))
