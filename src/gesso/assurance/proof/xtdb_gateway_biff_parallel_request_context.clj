(ns gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context
  "v1567: opt-in Biff handler mediation with independent request-local writer
   registrations. Unlike v1565's bounded queue, different caller threads may
   execute checked Biff handlers simultaneously. Each request gets its OWN
   fresh registered monotonic one-shot, host inventory enumerator, and issued
   handle. The checked handler must invoke the writer using
   `invoke-current-request-writer!`, not an old shared host registry.

   A request-local registry is a genuine local inspection surface ONLY for
   handlers that cooperate with this accessor. It is not an exhaustive host
   inventory. Direct XTDB writes, retained old Biff handlers, arbitrary
   instrumentation, deployed-server adoption, and abstract operation
   refinement are NOT proved. For this first revision a single certified
   literal route is required; multi-route per-request coverage is future work."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)))

(def module-id :gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context/install)
(def report-type :gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context/installed)
(def report-version 1)

(deftype ParallelContextHandle []
  Object
  (toString [_] "#<gesso.assurance/parallel-biff-request-context>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-adoption adoption/require-current-biff-host-adoption!)
(def ^:private original-issue writers/issue-request-scoped-monotonic-writer!)
(def ^:private original-observe assembly/observe-current-biff-registered-writer-assembly!)
(def ^:private original-summary application/operation-summary)
(def ^:private handler-slot application/biff-ring-handler-key)
(def ^:dynamic ^:private *current-request-writer* nil)
(def ^:private open-obligations
  #{:checked-handler-must-use-the-request-local-writer-accessor
    :only-one-source-certified-literal-route-supported
    :the-source-builder-must-produce-fresh-authorized-transaction-input
    :shared-target-concurrent-transactions-may-conflict
    :host-enumerator-exhaustiveness-not-established
    :retained-biff-handlers-and-direct-xtdb-write-bypasses
    :deployment-server-uses-final-handler-not-established
    :external-writers-and-xtdb-node-identity-not-established
    :abstract-operation-to-command-correspondence-not-established
    :transaction-commit-causality-and-refinement-not-established})

(defn- reject! [reason]
  (throw (ex-info "Invalid parallel Biff request writer boundary."
                  {:xtdb-gateway-biff-parallel-request-context/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- pinned! []
  ;; Ordinary Clojure Var substitution, not JVM instrumentation or reflection.
  (when-not (and (identical? original-adoption
                             (var-get #'adoption/require-current-biff-host-adoption!))
                 (identical? original-issue
                             (var-get #'writers/issue-request-scoped-monotonic-writer!))
                 (identical? original-observe
                             (var-get #'assembly/observe-current-biff-registered-writer-assembly!))
                 (identical? original-summary
                             (var-get #'application/operation-summary))
                 (= handler-slot application/biff-ring-handler-key))
    (reject! :substituted-parallel-mediation-boundary)))

(defn invoke-current-request-writer!
  "Invoke the EXACT current request's Gesso-issued guarded writer, no args.
   Only callable synchronously on the owning handler-dispatch thread, while
   inside this module's one-use request scope. Never returns a capability.
   Does not authorize unrelated XTDB operations or asynchronous writes."
  [path-id]
  (let [{:keys [owner path writer]} *current-request-writer*]
    (when-not (and (qualified-id? path-id)
                   (identical? owner (Thread/currentThread))
                   (= path path-id)
                   (fn? writer))
      (reject! :no-matching-active-thread-bound-request-writer))
    (writer)))

(defn- single-literal-binding! [source route-writer-paths checked-route-ids]
  (let [summary (original-summary
                 (:operation-acquisition-assembly (:application-assembly source)))
        routes (vec (for [[operation entry] summary
                          route (:routes entry)]
                      {:operation operation :id (:route-id route)
                       :method (:method route) :uri (:path route)}))
        selected (first routes)]
    (when-not (and (= 1 (count routes))
                   (= 1 (count checked-route-ids))
                   (= (set (map :id routes)) checked-route-ids)
                   (qualified-id? (:operation selected))
                   (qualified-id? (:id selected))
                   (keyword? (:method selected))
                   (string? (:uri selected))
                   (.startsWith ^String (:uri selected) "/")
                   (not (re-find #"[{}:*]" (:uri selected)))
                   (plain-map? route-writer-paths)
                   (= #{(:id selected)} (set (keys route-writer-paths)))
                   (qualified-id? (get route-writer-paths (:id selected))))
      (reject! :requires-one-certified-literal-route-and-writer-binding))
    (assoc selected :path (get route-writer-paths (:id selected)))))

(defn- entry [handle]
  (when (instance? ParallelContextHandle handle)
    (locking issued (.get issued handle))))

(defn issue-isolated-parallel-biff-request-module!
  "Issue a single-start Biff module providing parallel, request-ISOLATED
   guarded execution for ONE certified literal route.

   [adoption-handle contract-source operation-writer-source
    route-writer-paths node request->admitted-source! revision-field]

   Install AFTER checked Biff host adoption and BEFORE server start. A
   participating certified handler must call `invoke-current-request-writer!`
   with its declared writer path. It must NOT dereference a shared registry.
   The host supplies the source builder; it is not authenticated as business
   logic by this module. Two requests operating on the same XTDB document may
   legitimately conflict at the transaction-time guard.

   Every dispatch issues a separate identity-authenticated one-shot, constructs
   a private registry and source-derived inventory, verifies the complete
   source/handler/writer assembly, observes one actual synchronous invocation,
   retires the private registration, and returns ONLY the Ring response.
   There is no global writer table to mutate and no queue serializing handlers."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field]
  (pinned!)
  (when-not (and (plain-map? contract-source)
                 (plain-map? operation-writer-source)
                 (plain-map? route-writer-paths)
                 (fn? source!)
                 (keyword? revision-field)
                 (some? node))
    (reject! :invalid-parallel-mediation-issuance))
  (let [handle (ParallelContextHandle.)
        initialized (atom false)
        started (atom nil)
        module
        {:biff.core/id module-id
         :biff.core/init
         (fn [_modules]
           (pinned!)
           (when-not (compare-and-set! initialized false true)
             (reject! :repeated-parallel-module-init))
           {})
         :biff.core/start
         (fn [intermediate-system]
           ;; Poison a first startup attempt before doing any host work.
           (when-not (compare-and-set! started nil :starting)
             (reject! :repeated-parallel-module-start))
           (pinned!)
           (when-not @initialized
             (reject! :parallel-module-not-initialized))
           (let [host (original-adoption adoption-handle contract-source
                                         intermediate-system)
                 intermediate-handler (get intermediate-system handler-slot)]
             (when-not (and (= adoption/report-type (:type host))
                            (= :identical-to-guarded-checked-router-wrapper
                               (:handler-slot host))
                            (fn? intermediate-handler))
               (reject! :unverified-intermediate-biff-handler))
             (let [{:keys [operation id method uri path] :as route}
                   (single-literal-binding!
                    contract-source route-writer-paths
                    (:checked-certified-route-ids host))
                   final-handler
                   (fn [request]
                     (pinned!)
                     (when *current-request-writer*
                       (reject! :nested-parallel-request-dispatch))
                     (when-not (and (plain-map? request)
                                    (= method (:request-method request))
                                    (= uri (:uri request)))
                       (reject! :unmatched-parallel-request-route))
                     (original-adoption adoption-handle contract-source
                                        intermediate-system)
                     (let [source (source! request)]
                       (pinned!)
                       (original-adoption adoption-handle contract-source
                                          intermediate-system)
                       (let [issuance (original-issue path operation node
                                                      source revision-field)
                             writer (:writer issuance)
                             registry (atom {path {:kind :issued-operation
                                                   :operation-id operation
                                                   :writer writer}})
                             inventory {:operation-writer-source operation-writer-source
                                        :enumerate-write-paths! (fn [] @registry)}
                             handles {path (:handle issuance)}
                             installed @registry
                             outcome
                             (try
                               {:result
                                ((:with-request! issuance)
                                 (fn []
                                   (binding [*current-request-writer*
                                             {:owner (Thread/currentThread)
                                              :path path :writer writer}]
                                     (original-observe
                                      adoption-handle contract-source
                                      intermediate-system inventory handles
                                      route-writer-paths request))))}
                               (catch Throwable t {:thrown t})
                               (finally
                                 ;; Private registry cannot be accidentally
                                 ;; replaced by a competing request. Still
                                 ;; reject in-process mutation; never clobber.
                                 (when-not (compare-and-set! registry installed {})
                                   (reject! :request-local-registry-tampered))))]
                         (when-let [thrown (:thrown outcome)] (throw thrown))
                         (when-not (and (= :closed ((:request-scope-status! issuance)))
                                        (= {} @registry)
                                        (= assembly/invocation-report-type
                                           (get-in outcome [:result :report :type]))
                                        (= :conditional-observed-route-invoked-joined-one-shot-writer
                                           (get-in outcome [:result :report :status])))
                           (reject! :unexpected-parallel-invocation-evidence))
                         (get-in outcome [:result :response]))))
                   final-system (assoc intermediate-system handler-slot final-handler)]
               (reset! started {:source contract-source
                                :intermediate-system intermediate-system
                                :intermediate-handler intermediate-handler
                                :final-system final-system
                                :final-handler final-handler
                                :route route})
               final-system)))}]
    (locking issued (.put issued handle {:source contract-source
                                       :initialized initialized
                                       :started started
                                       :adoption-handle adoption-handle}))
    {:module module :handle handle}))

(defn require-current-isolated-parallel-biff-request-module!
  "Read-only inspection of exact locally installed final Biff handler.
   Does not execute requests, create writers, or claim that HTTP server uses
   this system map. Parallel invocation evidence is per-request only."
  [handle contract-source final-system]
  (pinned!)
  (let [{:keys [source initialized started adoption-handle]}
        (or (entry handle) (reject! :unissued-parallel-module))
        state @started]
    (when-not (identical? source contract-source)
      (reject! :changed-parallel-contract-source))
    (when-not @initialized
      (reject! :parallel-module-not-initialized))
    (when-not (map? state)
      (reject! :parallel-module-not-started))
    (when-not (and (identical? final-system (:final-system state))
                   (identical? (get final-system handler-slot)
                               (:final-handler state))
                   (not (identical? (:final-handler state)
                                    (:intermediate-handler state))))
      (reject! :parallel-final-handler-identity-mismatch))
    (original-adoption adoption-handle contract-source
                       (:intermediate-system state))
    (pinned!)
    (when-not (identical? final-system (:final-system @started))
      (reject! :parallel-final-system-changed-during-inspection))
    {:type report-type
     :version report-version
     :status :conditional-request-local-writer-mediation-installed
     :scope :one-process-local-final-biff-system
     :registered-writer-context :independent-per-request
     :parallel-handler-dispatch :permitted
     :request-scoped-one-shot :fresh-per-request
     :in-process-callback-trace :one-checked-invocation-per-request
     :biff-handler-slot :exact-local-identity-checked
     :actual-http-server-adoption :not-established
     :other-writer-closure :not-established
     :operation-refinement :not-established
     :open-obligations open-obligations
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))
