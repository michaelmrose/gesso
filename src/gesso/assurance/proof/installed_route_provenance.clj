(ns gesso.assurance.proof.installed-route-provenance
  "v1476: Checked, in-process Ring route dispatcher for v1253-issued handlers.

   v1253 checks a caller-supplied table, but does not install that table into
   a router. This module constructs the ACTUAL synchronous Ring dispatcher
   from that freshly checked table and registers the exact returned callable.
   A local dispatch witness can only arise by invoking that callable once.

   This is deliberately limited to exact (literal) method/path coordinates.
   Parameterized or wildcard routes require a separately checked host matcher
   and fail closed here. Unmatched requests receive 404, never a raw fallback
   that could be mistaken for a certified writer.

   The returned Ring function must still be installed into the host's active
   routing system. No test of this function can authenticate Biff startup,
   a deployed server's handler slot, another router, independent raw writers,
   physical XTDB commits, or request authorization. This is NOT admission,
   proof of deployment, or a transaction credential."
  (:require [clojure.set :as set]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as contract])
  (:import (java.util WeakHashMap)))

(def report-type :gesso.assurance.proof.installed-route-provenance/local-dispatch)
(def report-version 1)

(deftype RouteDispatchWitness []
  Object
  (toString [_] "#<gesso.assurance/route-dispatch-witness>"))

(def ^:private ^WeakHashMap registered-routers (WeakHashMap.))
(def ^:private ^WeakHashMap registered-observations (WeakHashMap.))
(def ^:dynamic ^:private *observing* false)
(def ^:private original-check contract/check-certified-contracts!)
(def ^:private original-summary application/operation-summary)

(def ^:private open-obligations
  #{:checked-ring-dispatcher-not-attested-as-deployed-biff-handler
    :no-later-handler-replacement-in-host
    :no-independent-router-or-raw-writer-bypass
    :literal-route-scope-does-not-cover-parameterized-routes
    :trusted-ring-request-method-and-uri
    :input-binding-authorization-and-response-purity
    :issued-executor-to-physical-transaction-correspondence
    :complete-physical-writer-coverage
    :universal-xtdb-refinement})

(defn- reject! [reason]
  ;; Do not include request, handler, source, response, or executor in errors.
  (throw (ex-info "Invalid checked Ring route installation."
                  {:installed-route-provenance/rejection true
                   :reason reason})))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentHashMap v)
           (instance? clojure.lang.PersistentArrayMap v))
       (nil? (meta v))))

(defn- literal-path? [p]
  (and (string? p) (.startsWith ^String p "/")
       (not (re-find #"[:*{}]" p))
       (not (.contains ^String p "?"))))

(defn- require-unmodified-upstream! []
  (when-not (and (identical? original-check
                             (var-get #'contract/check-certified-contracts!))
                 (identical? original-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-application-or-route-checker)))

(defn- checked-route-index! [source]
  (require-unmodified-upstream!)
  (when-not (and (plain-map? source)
                 (= #{:application-assembly :semantic-route-handlers
                      :certified-operations}
                    (set (keys source))))
    (reject! :invalid-contract-source))
  (let [checked (original-check source)
        ops (original-summary
             (:operation-acquisition-assembly (:application-assembly source)))
        declared (for [[operation entry] ops
                       {:keys [route-id method path]} (:routes entry)]
                   {:operation operation :route-id route-id
                    :method method :path path})]
    (when-not (= (set (map :route-id declared)) (:route-ids checked))
      (reject! :declared-route-source-mismatch))
    (when-not (every? (fn [{:keys [method path]}]
                        (and (keyword? method) (literal-path? path)))
                      declared)
      (reject! :unsupported-parameterized-or-invalid-route))
    (when-not (= (count declared)
                 (count (set (map (juxt :method :path) declared))))
      (reject! :ambiguous-ring-method-path))
    (let [index (into {}
                      (for [{:keys [method path route-id operation]} declared]
                        [[method path]
                         {:route-id route-id :operation operation
                          :handler (get-in source [:semantic-route-handlers
                                                   route-id])}]))]
      (when-not (= (:route-ids checked)
                   (set (map :route-id (vals index))))
        (reject! :missing-checked-ring-route))
      {:index index :contract checked})))

(defn- registered-router [router]
  (when (fn? router)
    (locking registered-routers (.get registered-routers router))))

(defn checked-ring-router? [router]
  "Identity-based process-local recognition; NOT proof of deployment."
  (boolean (registered-router router)))

(defn- require-current-router! [router source]
  (let [entry (or (registered-router router)
                  (reject! :unregistered-ring-router))]
    (when-not (identical? source (:source entry))
      (reject! :changed-ring-router-source-object))
    (let [{:keys [index contract]} (checked-route-index! source)]
      (when-not (and (= (:contract entry) contract)
                     (= (set (keys (:index entry))) (set (keys index)))
                     (every? (fn [[coordinate row]]
                               (let [current (get index coordinate)]
                                 (and (= (:route-id row) (:route-id current))
                                      (= (:operation row) (:operation current))
                                      (identical? (:handler row)
                                                  (:handler current)))))
                             (:index entry)))
        (reject! :stale-or-swapped-installed-route))
      entry)))

(defn install-checked-ring-router!
  "Build the exact synchronous Ring dispatcher for a freshly checked v1253
   source. This does NOT put it into Biff or a production HTTP server. The
   caller must arrange actual host adoption separately. Only literal routes
   are supported; any collision or dynamic path fails before installation."
  [contract-source]
  (let [{:keys [index contract]} (checked-route-index! contract-source)
        router (fn checked-ring-dispatch [request]
                 (when-not (and (map? request)
                                (keyword? (:request-method request))
                                (string? (:uri request)))
                   (reject! :invalid-ring-request))
                 (let [entry (require-current-router! checked-ring-dispatch
                                                      contract-source)]
                   (if-let [{:keys [handler]}
                            (get (:index entry)
                                 [(:request-method request) (:uri request)])]
                     (handler request)
                     {:status 404 :headers {} :body "Not found"})))
        entry {:source contract-source :index index :contract contract}]
    (locking registered-routers (.put registered-routers router entry))
    router))

(defn describe-checked-ring-router
  "Non-authoritative redacted description of the current in-process router."
  [router contract-source]
  (let [{:keys [index contract]} (require-current-router! router contract-source)]
    {:type report-type
     :version report-version
     :scope :one-owned-synchronous-ring-router-object
     :route-ids (set (map :route-id (vals index)))
     :certified-route-ids (:matched-certified-route-ids contract)
     :uncertified-operation-ids (:uncertified-operations contract)
     :route-count (count index)
     :router-callable :registered-in-current-jvm
     :host-adoption :not-established
     :physical-correspondence :not-established
     :open-obligations (set/union open-obligations
                                  (:open-obligations contract))
     :certification :not-issued
     :authority :none}))

(defn observe-checked-ring-route!
  "Invoke the registered router ONCE with this very request object and issue
   an opaque process-local witness for a matching declared route only. A 404,
   thrown handler, or failed source recheck cannot issue a witness. This does
   not authenticate deployment, response correctness or an XTDB commit."
  [router contract-source request]
  (when *observing* (reject! :nested-route-observation))
  (let [{:keys [index contract]}
        (require-current-router! router contract-source)
        coordinate [(:request-method request) (:uri request)]
        row (get index coordinate)]
    (when-not (and (map? request) row)
      (reject! :unmatched-or-invalid-ring-request))
    (when-not (contains? (:matched-certified-route-ids contract)
                         (:route-id row))
      (reject! :uncertified-route-has-no-issued-provenance))
    (binding [*observing* true]
      (let [response (router request)
            witness (RouteDispatchWitness.)
            record {:router router :source contract-source
                    :request request :response response
                    :coordinate coordinate :route-id (:route-id row)
                    :handler (:handler row)}]
        (locking registered-observations
          (.put registered-observations witness record))
        {:witness witness :response response
         :report (assoc (describe-checked-ring-router router contract-source)
                        :observed-route-id (:route-id row)
                        :dispatch :one-actual-in-process-invocation)}))))

(defn require-current-route-dispatch!
  "Verify exact process-local handle and source identities without executing
   Ring or performing database I/O. Reports alone never authenticate a route."
  [witness router contract-source request]
  (let [record (when (instance? RouteDispatchWitness witness)
                 (locking registered-observations
                   (.get registered-observations witness)))
        entry (require-current-router! router contract-source)]
    (when-not (and record
                   (identical? router (:router record))
                   (identical? contract-source (:source record))
                   (identical? request (:request record))
                   (identical? (:handler record)
                               (get-in entry [:index (:coordinate record)
                                              :handler]))
                   (= (:route-id record)
                      (get-in entry [:index (:coordinate record) :route-id])))
      (reject! :stale-or-forged-route-dispatch-witness))
    (describe-checked-ring-router router contract-source)))
