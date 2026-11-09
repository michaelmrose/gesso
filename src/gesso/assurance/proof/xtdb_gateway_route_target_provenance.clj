(ns gesso.assurance.proof.xtdb-gateway-route-target-provenance
  "v1578: issue an identity-recognizable, source-linked request target resolver
   from a COMPLETE static route/physical-target declaration.

   Unlike a freely supplied request->expected-target callback, this resolver
   takes operation identity and literal route selection from the current
   certified application contract, and takes the physical document target
   from an immutable, independently declared startup table. The candidate
   XTDB command is never an input to the resolver. Each lookup rechecks the
   certified contract and exact route index before returning an expectation.

   This is a deliberately NARROW first provenance step: literal routes with
   fixed document targets only. The application still owns the correctness
   and authorization of its static target declarations and the host request.
   It does not establish operation semantics, deployed installation, XTDB
   causality, or exclusion of raw writers. v1576's mediator does not yet
   require an issued resolver; it accepts an arbitrary host callback. That
   integration obligation remains open until separately enforced."
  (:require [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util UUID WeakHashMap)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-route-target-provenance/issued-resolver)
(def report-version 1)

(def ^:private original-check certified/check-certified-contracts!)
(def ^:private original-summary application/operation-summary)
(def ^:private ^WeakHashMap issued (WeakHashMap.))

(defn- reject! [reason]
  (throw (ex-info "Invalid source-linked XTDB route target provenance."
                  {:xtdb-gateway-route-target-provenance/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- physical-target? [target]
  ;; Intentionally the same narrow physical-target vocabulary as v1574.
  (and (plain-vector? target)
       (= 2 (count target))
       (let [[table id] target]
         (and (keyword? table)
              (nil? (namespace table))
              (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
              (or (and (string? id) (not (empty? id)) (<= (count id) 512))
                  (instance? UUID id)
                  (and (integer? id)
                       (not (instance? clojure.lang.BigInt id))
                       (not (instance? java.math.BigInteger id))))))))

(defn- pinned! []
  ;; JVM Var substitution guard; not a sandbox or bytecode attestation.
  (when-not (and (identical? original-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-certification-or-route-checker)))

(defn- current-literal-route-index! [contract-source]
  (pinned!)
  (when-not (and (plain-map? contract-source)
                 (= #{:application-assembly :semantic-route-handlers
                      :certified-operations}
                    (set (keys contract-source))))
    (reject! :invalid-certified-route-source))
  ;; Recheck the real certified handlers/executors, not a caller-fabricated
  ;; report or a redundant list of claimed operation names.
  (let [verified (original-check contract-source)
        summary (original-summary
                 (get-in contract-source
                         [:application-assembly :operation-acquisition-assembly]))
        rows (vec (for [[operation entry] summary
                        route (:routes entry)]
                    {:id (:route-id route)
                     :operation operation
                     :method (:method route)
                     :uri (:path route)}))
        ids (mapv :id rows)
        selectors (mapv (juxt :method :uri) rows)]
    (when-not (and (seq rows)
                   (<= (count rows) 512)
                   (= (count ids) (count (set ids)))
                   (= (count selectors) (count (set selectors)))
                   (= (set ids) (:route-ids verified))
                   (= (:route-ids verified)
                      (:matched-certified-route-ids verified))
                   (every? (fn [{:keys [id operation method uri]}]
                             (and (qualified-id? id)
                                  (qualified-id? operation)
                                  (keyword? method)
                                  (string? uri)
                                  (.startsWith ^String uri "/")
                                  (not (re-find #"[{}:*]" uri))))
                           rows))
      (reject! :requires-complete-unique-certified-literal-routes))
    (pinned!)
    (into {} (map (fn [row] [[(:method row) (:uri row)]
                             {:route-id (:id row)
                              :operation (:operation row)}])
                  rows))))

(defn- checked-static-target-declarations! [route-index declarations]
  (let [route-ids (set (map :route-id (vals route-index)))]
    (when-not (and (plain-map? declarations)
                   (= route-ids (set (keys declarations)))
                   (every? qualified-id? (keys declarations))
                   (every? (fn [entry]
                             (and (plain-map? entry)
                                  (= #{:command-operation :target}
                                     (set (keys entry)))
                                  (keyword? (:command-operation entry))
                                  (physical-target? (:target entry))))
                           (vals declarations)))
      (reject! :invalid-or-incomplete-static-route-target-declarations))
    ;; The resulting closed declaration cannot derive a new target from any
    ;; candidate command, user-supplied :params, or mutable host callback.
    declarations))

(defn- entry [resolver]
  (when (fn? resolver)
    (locking issued (.get issued resolver))))

(defn issue-certified-literal-route-target-resolver!
  "Issue a process-local resolver from one CURRENT certified contract and a
   complete startup map of route ID -> {:command-operation :target}.

   Physical target IDs are fixed per literal route. This does not support
   routes with per-request document identifiers; those need a separately
   specified and checked request-binding model. The target map must come from
   application construction, not the candidate XTDB source builder.

   Returns {:expected-target! callable}. The callable is registered by
   identity. Its argument is a Ring request; it selects only by exact method
   and URI and returns {:operation-id :command-operation :target}, suitable
   for v1574/v1576 structural comparison. No writer or certificate is issued.

   This is NOT an authentication of the startup declarations, request, user,
   physical document, or deployment; the caller must discharge those premises."
  [contract-source route-target-declarations]
  (let [routes (current-literal-route-index! contract-source)
        declarations (checked-static-target-declarations!
                      routes route-target-declarations)
        resolver (fn [request]
                   (pinned!)
                   (when-not (plain-map? request)
                     (reject! :unmatched-certified-literal-request))
                   (let [current (current-literal-route-index! contract-source)]
                     (when-not (= routes current)
                       (reject! :certified-route-index-changed))
                     (let [{:keys [route-id operation]}
                           (get current [(:request-method request) (:uri request)])]
                       (when-not route-id
                         (reject! :unmatched-certified-literal-request))
                       (let [{:keys [command-operation target]}
                             (get declarations route-id)]
                         {:operation-id operation
                          :command-operation command-operation
                          :target target}))))]
    (pinned!)
    (locking issued (.put issued resolver {:source contract-source
                                          :routes routes
                                          :declarations declarations}))
    {:expected-target! resolver}))

(defn require-current-certified-literal-route-target-resolver!
  "Inspect an EXACT issued resolver against its original contract source.
   The returned redacted report is not a certificate, writer capability, or
   proof of the host's physical target declaration. The v1576 mediator does
   NOT currently require this report; integration remains a separate step."
  [resolver contract-source]
  (pinned!)
  (let [{:keys [source routes declarations] :as issued-entry}
        (or (entry resolver) (reject! :unissued-target-resolver))]
    (when-not (and (identical? source contract-source)
                   issued-entry)
      (reject! :changed-target-resolver-source))
    (let [current (current-literal-route-index! contract-source)]
      (when-not (= routes current)
        (reject! :certified-route-index-changed))
      (checked-static-target-declarations! current declarations))
    (pinned!)
    {:type report-type
     :version report-version
     :status :conditional-certified-literal-route-static-target-resolver
     :certified-route-count (count routes)
     :every-declared-route-certified? true
     :operation-identity :rederived-from-current-certified-contract
     :target-identity :closed-startup-route-declaration
     :candidate-command-used-to-derive-target? false
     :request-document-parameters-used? false
     :request-match :exact-ring-method-and-literal-uri
     :static-declaration-authenticity :application-premise-not-proved
     :physical-target-authorization :not-established
     :target-bound-biff-mediator-adoption :not-established
     :abstract-operation-to-command-refinement :not-established
     :physical-transaction-causality :not-established
     :open-obligations
     #{:startup-target-declarations-are-independently-correct-and-authorized
       :ring-method-and-uri-provenance
       :no-dynamic-document-target-binding-supported
       :target-bound-biff-mediator-must-require-issued-resolver
       :transaction-commit-causality-and-physical-refinement
       :external-writer-coverage-not-established}
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))
