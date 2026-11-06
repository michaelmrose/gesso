(ns gesso.assurance.proof.installed-route-provenance
  "v1476/v1487: Checked in-process Ring router for v1253-issued handlers.

   v1487 additionally admits a deliberately narrow route-template vocabulary:
   complete :name segments, uniquely named, each matching one nonempty raw
   URI segment made from RFC3986 unreserved characters. All ambiguous route
   intersections, including literal/dynamic intersections, are rejected at
   installation. Parameter values are inserted as :path-params only AFTER a
   successful match; no implicit decoding/coercion or client-supplied bindings
   are trusted. Literal routes retain the original request object.

   The exact registered callable and its one-shot local dispatch witness remain
   the evidence boundary. The module does not authenticate host URI rewriting,
   deployed server adoption, caller identity/authorization, physical XTDB
   effects or dynamic-route physical correlation. This is NOT certification."
  (:require [clojure.set :as set]
            [clojure.string :as str]
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
    :route-template-matcher-excludes-wildcards-and-encoded-parameters
    :host-uri-normalization-and-rewriting-not-authenticated
    :dynamic-route-physical-correlation-not-established
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

(def ^:private parameter-name-pattern #"^:([A-Za-z][A-Za-z0-9_-]*)$")
(def ^:private parameter-value-pattern #"^[A-Za-z0-9._~-]+$")

(defn- safe-parameter-value? [value]
  (and (string? value)
       (boolean (re-matches parameter-value-pattern value))
       (not (#{"." ".."} value))))

(defn- path-segments [path]
  ;; -1 retains trailing empty segments: /a/:id and /a/:id/ differ.
  (vec (rest (str/split path #"/" -1))))

(defn- compile-route-template [path]
  (when (and (string? path)
             (str/starts-with? path "/")
             (not (re-find #"[?*{}]" path)))
    (let [raw (path-segments path)
          parts (mapv (fn [segment]
                        (if-let [[_ name] (re-matches parameter-name-pattern
                                                        segment)]
                          {:parameter (keyword name)}
                          {:literal segment}))
                      raw)
          parameters (keep :parameter parts)
          dynamic? (boolean (seq parameters))]
      (when (and (every? (fn [segment]
                           (or (re-matches parameter-name-pattern segment)
                               (not (str/includes? segment ":"))))
                         raw)
                 (= (count parameters) (count (set parameters)))
                 ;; For templated routes, do not treat empty, ambiguous, or
                 ;; noncanonical segments as parts of a matched URL.
                 (or (not dynamic?)
                     (every? (fn [segment]
                               (or (re-matches parameter-name-pattern segment)
                                   (safe-parameter-value? segment)))
                             raw)))
        {:segments parts :parameterized? dynamic?}))))

(defn- segments-overlap? [left right]
  (cond
    (and (:literal left) (:literal right))
    (= (:literal left) (:literal right))

    (and (:parameter left) (:parameter right))
    true

    (:parameter left)
    (safe-parameter-value? (:literal right))

    :else
    (safe-parameter-value? (:literal left))))

(defn- route-overlap? [left right]
  (and (= (:method left) (:method right))
       (= (count (get-in left [:template :segments]))
          (count (get-in right [:template :segments])))
       (every? true?
               (map segments-overlap?
                    (get-in left [:template :segments])
                    (get-in right [:template :segments])))))

(defn- match-template [template uri]
  (when (and (string? uri) (str/starts-with? uri "/"))
    (let [segments (path-segments uri)
          pattern (:segments template)]
      (when (= (count segments) (count pattern))
        (reduce (fn [bindings [part actual]]
                  (when bindings
                    (if-let [name (:parameter part)]
                      (when (safe-parameter-value? actual)
                        (assoc bindings name actual))
                      (when (= (:literal part) actual)
                        bindings))))
                {}
                (map vector pattern segments))))))

(defn- matched-route! [index request]
  (let [method (:request-method request)
        uri (:uri request)
        row (get index [method uri])
        exact (when-not (get-in row [:template :parameterized?]) row)]
    (if exact
      {:row exact :path-params {}}
      (let [matches
            (keep (fn [[[route-method _] row]]
                    (when (= method route-method)
                      (when-let [parameters (match-template (:template row) uri)]
                        {:row row :path-params parameters})))
                  index)]
        (when (> (count matches) 1)
          (reject! :ambiguous-ring-route-match))
        (first matches)))))

(defn- bound-request! [request path-params]
  (if (seq path-params)
    (do
      (when (or (contains? request :path-params)
                (contains? request :route-params))
        (reject! :preexisting-dynamic-route-parameters))
      (assoc request :path-params path-params))
    request))

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
                        (and (keyword? method)
                             (compile-route-template path)))
                      declared)
      (reject! :unsupported-parameterized-or-invalid-route))
    (when-not (= (count declared)
                 (count (set (map (juxt :method :path) declared))))
      (reject! :ambiguous-ring-method-path))
    (let [routes (mapv (fn [{:keys [method path] :as route}]
                         (assoc route :template (compile-route-template path)))
                       declared)]
      (doseq [i (range (count routes))
              j (range (inc i) (count routes))]
        (when (route-overlap? (nth routes i) (nth routes j))
          (reject! :ambiguous-ring-route-templates)))
      (let [index (into {}
                        (for [{:keys [method path route-id operation template]}
                              routes]
                          [[method path]
                           {:route-id route-id :operation operation
                            :template template :route-template path
                            :handler (get-in source [:semantic-route-handlers
                                                     route-id])}]))]
        (when-not (= (:route-ids checked)
                     (set (map :route-id (vals index))))
          (reject! :missing-checked-ring-route))
        {:index index :contract checked}))))

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
                                      (= (:template row) (:template current))
                                      (identical? (:handler row)
                                                  (:handler current)))))
                             (:index entry)))
        (reject! :stale-or-swapped-installed-route))
      entry)))

(defn install-checked-ring-router!
  "Build the exact synchronous Ring dispatcher for a freshly checked v1253
   source. This does NOT put it into Biff or a production HTTP server. The
   caller must arrange actual host adoption separately. Only plain literal
   routes and unique whole-segment :name placeholders are supported; any
   overlapping method/path languages fail at installation."
  [contract-source]
  (let [{:keys [index contract]} (checked-route-index! contract-source)
        router (fn checked-ring-dispatch [request]
                 (when-not (and (map? request)
                                (keyword? (:request-method request))
                                (string? (:uri request)))
                   (reject! :invalid-ring-request))
                 (let [entry (require-current-router! checked-ring-dispatch
                                                      contract-source)]
                   (if-let [{:keys [row path-params]}
                            (matched-route! (:index entry) request)]
                     ((:handler row) (bound-request! request path-params))
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
        match (when (map? request) (matched-route! index request))
        row (:row match)
        coordinate (when row [(:request-method request) (:route-template row)])]
    (when-not row
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
                    :path-params (:path-params match)
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
                      (get-in entry [:index (:coordinate record) :route-id]))
                   (= (:path-params record)
                      (:path-params
                       (matched-route! (:index entry) request))))
      (reject! :stale-or-forged-route-dispatch-witness))
    (describe-checked-ring-router router contract-source)))
