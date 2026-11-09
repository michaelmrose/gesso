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
   refinement are NOT proved. v1569 supports multiple certified literal routes
   with total STATIC route-to-writer declarations, but issues and checks an
   actual registered writer only for the selected route on each request.
   Unselected routes are NOT represented as dynamically registered or invoked.
   v1571 optionally bounds concurrent guarded handler execution while retaining
   independent per-request registries and actual parallel execution.
   v1576 adds explicit callback-based target-bound issuance variants, which
   compare the candidate source before writer issuance. v1580 adds distinct
   source-backed target-bound issuers that REQUIRE a v1578 issued resolver
   linked by identity to the SAME certified contract, revalidated at issuance,
   startup, and before EACH request's candidate source and writer issuance.
   v1588 adds a separate, opt-in certified-PLAN-bound prestate gate BEFORE
   the existing request-scoped monotonic writer is issued. This first bridge
   prepares an independent read-and-CAS capability but NEVER exposes or invokes
   that capability: the actual registered writer is still the original
   monotonic, independently prepared one-shot. This does not yet unify the
   two preparations or certify the installed target table. Prior issuers
   retain their behavior and assurance scope."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as targets]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate :as guarded]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)
           (java.util.concurrent Semaphore TimeUnit)))

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
(def ^:private original-biff-observe writers/observe-checked-biff-host-registered-writer!)
(def ^:private original-summary application/operation-summary)
(def ^:private original-target-check targets/require-declared-operation-target!)
(def ^:private original-provenance-check
  provenance/require-current-certified-literal-route-target-resolver!)
(def ^:private original-plan-bound-prestate
  guarded/prepare-plan-bound-acquired-one-shot!)
(def ^:private handler-slot application/biff-ring-handler-key)
(def ^:dynamic ^:private *current-request-writer* nil)
(def ^:dynamic ^:private *bounded-parallel-dispatch* false)
(def ^:private open-obligations
  #{:checked-handler-must-use-the-request-local-writer-accessor
    :unselected-routes-are-declared-not-dynamically-registered-or-observed
    :the-source-builder-must-produce-fresh-authorized-transaction-input
    :shared-target-concurrent-transactions-may-conflict
    :host-enumerator-exhaustiveness-not-established
    :retained-biff-handlers-and-direct-xtdb-write-bypasses
    :deployment-server-uses-final-handler-not-established
    :external-writers-and-xtdb-node-identity-not-established
    :abstract-operation-to-command-correspondence-not-established
    :transaction-commit-causality-and-refinement-not-established
    :admission-limits-apply-only-to-this-issued-mediator-instance})

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
                 (identical? original-biff-observe
                             (var-get #'writers/observe-checked-biff-host-registered-writer!))
                 (identical? original-summary
                             (var-get #'application/operation-summary))
                 (identical? original-target-check
                             (var-get #'targets/require-declared-operation-target!))
                 (identical? original-provenance-check
                             (var-get #'provenance/require-current-certified-literal-route-target-resolver!))
                 (identical? original-plan-bound-prestate
                             (var-get #'guarded/prepare-plan-bound-acquired-one-shot!))
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

(defn- checked-literal-bindings!
  "Independently rederive ALL certified literal routes. Reject ambiguous
   method/URI selection, duplicate IDs, reused writer paths, or partial
   bindings. This is static route coverage, not issuance for unselected routes."
  [source route-writer-paths checked-route-ids]
  (let [summary (original-summary
                 (:operation-acquisition-assembly (:application-assembly source)))
        routes (vec (for [[operation entry] summary
                          route (:routes entry)]
                      {:operation operation :id (:route-id route)
                       :method (:method route) :uri (:path route)}))
        ids (mapv :id routes)
        selectors (mapv (juxt :method :uri) routes)
        paths (vals route-writer-paths)]
    (when-not (and (seq routes) (<= (count routes) 512)
                   (= (count ids) (count (set ids)))
                   (= (count selectors) (count (set selectors)))
                   (= (set ids) checked-route-ids)
                   (every? (fn [{:keys [operation id method uri]}]
                             (and (qualified-id? operation)
                                  (qualified-id? id)
                                  (keyword? method)
                                  (string? uri)
                                  (.startsWith ^String uri "/")
                                  (not (re-find #"[{}:*]" uri))))
                           routes)
                   (plain-map? route-writer-paths)
                   (= (set ids) (set (keys route-writer-paths)))
                   (= (count paths) (count (set paths)))
                   (every? qualified-id? paths))
      ;; Preserve the existing rejection contract for incomplete singleton
      ;; route tables as well as invalid multi-route declarations.
      (reject! :requires-one-certified-literal-route-and-writer-binding))
    (mapv (fn [route] (assoc route :path (get route-writer-paths (:id route))))
          routes)))

(defn- per-operation-inventory-sources!
  "A writer-coverage source certifies its independently enumerated operation
   IDs. With multiple distinct operations it MUST be supplied independently
   for each operation; a source for one operation cannot attest another.
   The upstream writer-coverage checker rechecks the selected source at each
   issuance/dispatch. No unchecked filtering or fabricated inventory is used."
  [inventory-source routes]
  (let [operations (set (map :operation routes))
        grouped? (contains? inventory-source :per-operation-writer-sources)
        sources (if grouped?
                  (:per-operation-writer-sources inventory-source)
                  (when (= 1 (count operations))
                    {(first operations) inventory-source}))]
    (when-not (and (plain-map? sources)
                   (= operations (set (keys sources)))
                   (every? plain-map? (vals sources))
                   (or (not grouped?)
                       (= #{:per-operation-writer-sources}
                          (set (keys inventory-source)))))
      (reject! :requires-independent-operation-writer-coverage-sources))
    sources))

(defn- selected-literal-route! [routes request]
  (when-not (plain-map? request)
    (reject! :unmatched-parallel-request-route))
  (let [matches (filterv (fn [route]
                           (= [(:method route) (:uri route)]
                              [(:request-method request) (:uri request)]))
                         routes)]
    (when-not (= 1 (count matches))
      (reject! :unmatched-parallel-request-route))
    (first matches)))

(defn- entry [handle]
  (when (instance? ParallelContextHandle handle)
    (locking issued (.get issued handle))))

(defn- bounded-parallel-policy!
  "Validate a finite limit on running plus waiting requests and a separate
   genuinely parallel execution limit. Clojure integers are required; no
   coercion, decimals or caller-supplied semaphore instances are accepted."
  [policy]
  (when-not (and (plain-map? policy)
                 (= #{:max-active-requests :max-pending-requests
                      :wait-timeout-ms}
                    (set (keys policy)))
                 (every? integer? (vals policy))
                 (<= 2 (:max-active-requests policy) 64)
                 (<= (:max-active-requests policy)
                     (:max-pending-requests policy) 256)
                 (<= 1 (:wait-timeout-ms policy) 60000))
    (reject! :invalid-bounded-parallel-policy))
  policy)

(defn- bounded-parallel-dispatch
  "An independent fair permit pool bounds ACTIVE handler executions without
   serializing them. `pending` counts ACTIVE + WAITING entrants. All entrances
   and exits, including interruption and handler exceptions, release their own
   accounting. Semaphore fairness applies to waiting permit acquisition, not
   a proof of strict scheduling order or backend transaction fairness."
  [dispatch! {:keys [max-active-requests max-pending-requests wait-timeout-ms]}]
  (let [^Semaphore permits (Semaphore. (int max-active-requests) true)
        pending (atom 0)]
    (fn [request]
      (when *bounded-parallel-dispatch*
        (reject! :recursive-bounded-parallel-request-dispatch))
      (when-not (loop []
                  (let [n @pending]
                    (cond
                      (>= n max-pending-requests) false
                      (compare-and-set! pending n (inc n)) true
                      :else (recur))))
        (reject! :bounded-parallel-admission-full))
      (try
        (let [acquired?
              (try
                (.tryAcquire permits (long wait-timeout-ms)
                             TimeUnit/MILLISECONDS)
                (catch InterruptedException _
                  (.interrupt (Thread/currentThread))
                  (reject! :bounded-parallel-admission-interrupted)))]
          (when-not acquired?
            (reject! :bounded-parallel-admission-timeout))
          (try
            (binding [*bounded-parallel-dispatch* true]
              (dispatch! request))
            (finally
              (.release permits))))
        (finally
          (swap! pending dec))))))

(defn- require-issued-source-backed-target-resolver!
  "Only the exact v1578 issued function for this *same* certified contract is
   admissible. Re-evaluate the source-backed declaration and certified route
   inventory at every boundary; do not rely on a caller-generated report.
   Foreign resolvers, ordinary callbacks, checker substitutions, and errors
   from the independent verifier are rejected with redacted diagnostics."
  [expected-target! contract-source plan-bound?]
  (pinned!)
  (when-not (fn? expected-target!)
    (reject! :requires-exact-issued-route-target-resolver))
  (let [report (try
                 (original-provenance-check expected-target! contract-source)
                 (catch Throwable _
                   (reject! :unissued-or-incompatible-route-target-resolver)))]
    (when-not (and (= provenance/report-type (:type report))
                   (= provenance/report-version (:version report))
                   (= :conditional-certified-literal-route-static-target-resolver
                      (:status report))
                   (true? (:every-declared-route-certified? report))
                   (= :rederived-from-current-certified-contract
                      (:operation-identity report))
                   (= (if plan-bound?
                        :closed-issued-certified-plan-target-callback
                        :closed-startup-route-declaration)
                      (:target-identity report))
                   (if plan-bound?
                     (= :exact-issued-source-callback-checked
                        (:certified-plan-target-callback-identity report))
                     true)
                   (false? (:candidate-command-used-to-derive-target? report))
                   (false? (:request-document-parameters-used? report))
                   (= :exact-ring-method-and-literal-uri (:request-match report))
                   (= :application-premise-not-proved
                      (:static-declaration-authenticity report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:transaction-authority report) (:authority report)))
      (reject! :unexpected-issued-route-target-resolver-report)))
  (pinned!)
  nil)

(defn- require-resolver-issuance!
  "Accept only the exact one-field v1578 issuance shape. Its callable must
   already be identity-registered for the selected contract; copying or
   wrapping the function does not create an issued provenance capability."
  [resolver-issuance contract-source plan-bound?]
  (when-not (and (plain-map? resolver-issuance)
                 (= #{:expected-target!} (set (keys resolver-issuance))))
    (reject! :requires-exact-issued-route-target-resolver))
  (let [resolver (:expected-target! resolver-issuance)]
    (require-issued-source-backed-target-resolver! resolver contract-source plan-bound?)
    resolver))

(defn- require-target-binding-before-issuance!
  "Run v1574 against the precise candidate that will be handed to the writer
   issuer. The expected target comes from a separate callback receiving ONLY
   the original Ring request, never the candidate source. No authorization is
   inferred from the callback's result; independent provenance is a premise.
   Exceptions from an external expectation builder are redacted."
  [selected-operation request source expected-target!]
  (let [expectation (try
                      (expected-target! request)
                      (catch Throwable _
                        (reject! :independent-target-derivation-failed)))]
    (pinned!)
    (let [result (original-target-check selected-operation expectation source)]
      (when-not (and (= targets/report-type (:type result))
                     (= targets/report-version (:version result))
                     (= :conditional-declared-command-target-match
                        (:status result))
                     (= :local-structural-comparison-only
                        (:proof-status result))
                     (true? (:selected-route-operation-checked? result))
                     (true? (:declared-command-and-protected-document-match-expected-target?
                             result)))
        (reject! :unexpected-target-binding-result)))
    (pinned!)
    nil))

(defn- require-plan-bound-prestate-before-issuance!
  "v1588: prepare (without invoking!) the current certified-plan-bound,
   independently acquired complete-CAS gateway BEFORE issuing a separately
   monotonic request writer. No capability from this extra preparation escapes.
   This deliberately does not claim that the later writer uses the *same*
   prepared submission: both gateways independently check the supplied node
   and source, and the actual registered monotonic gateway owns its own CAS.
   Any errors from the physical prestate read are reported without raw data."
  [node contract-source expected-target! operation request source]
  (pinned!)
  (let [prepared (try
                   (original-plan-bound-prestate
                    node contract-source {:expected-target! expected-target!}
                    operation request source)
                   (catch Throwable _
                     (reject! :plan-bound-guarded-prestate-preparation-failed)))
        report (try
                 ((:summary! prepared))
                 (catch Throwable _
                   (reject! :invalid-plan-bound-guarded-prestate-report)))]
    (when-not (and (= guarded/report-type (:type prepared))
                   (= guarded/report-version (:version prepared))
                   (fn? (:submit! prepared))
                   (fn? (:summary! prepared))
                   (= guarded/report-type (:type report))
                   (= guarded/report-version (:version report))
                   (= :prepared (:status report))
                   (= :same-node-pinned-read-matched-at-preparation
                      (:external-before report))
                   (= :canonical-complete-top-level-cas
                      (:transaction-preconditions report))
                   (= :conditional-plan-bound-acquired-prestate
                      (:proof-status report))
                   (= :one-shot-submission-capability
                      (:transaction-authority report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report)))
      (reject! :unexpected-plan-bound-guarded-prestate-report))
    (pinned!)
    ;; Deliberately discard the prepared submission. The only writer that may
    ;; enter the inspected Biff registry is the separately issued monotonic
    ;; request-scoped writer below. Consolidation is a future revision.
    nil))

(defn- issue-parallel-request-module*
  "Issue a single-start Biff module providing parallel, request-ISOLATED
   guarded execution for one of several certified literal routes.

   [adoption-handle contract-source operation-writer-source
    route-writer-paths node request->admitted-source! revision-field]

   `operation-writer-source` is the existing independent writer-coverage
   source when all routes name the same operation. For routes naming multiple
   distinct operations, pass exactly {:per-operation-writer-sources
   {operation-id independent-source ...}}. Every operation must have its own
   fully checked source; this module does not narrow or forge one. The host
   still owns request->admitted-source! and its business-rule correspondence.

   Install AFTER checked Biff host adoption and BEFORE server start. A
   participating certified handler must call `invoke-current-request-writer!`
   with its declared writer path. It must NOT dereference a shared registry.
   The host supplies the source builder; it is not authenticated as business
   logic by this module. Two requests operating on the same XTDB document may
   legitimately conflict at the transaction-time guard.

   Every dispatch issues a separate identity-authenticated one-shot ONLY for
   its matched route, constructs a private registry and source-derived inventory,
   verifies the actual Biff handler's selected route/writer invocation, retires
   the private registration, and returns ONLY the Ring response. At startup
   ALL source routes must have distinct, unambiguous, certified literal routes
   and complete writer-path declarations. For multiple routes the other paths
   are not claimed as dynamically installed or observed by this request.
   There is no global writer table to mutate and no queue serializing handlers.

   `expected-target!`, when provided by the explicit target-bound issuers,
   accepts ONLY the Ring request and supplies {:operation-id
   :command-operation :target}. It is evaluated independently of `source!`
   and the selected route's operation is compared before any writer issuance.
   Ordinary (legacy) issuers pass nil and remain WITHOUT this guarantee.
   When source-backed?, expected-target! MUST be an issued resolver for
   this exact contract, rechecked at issuance, startup and every dispatch.
   `plan-bound-prestate?` additionally requires the *stronger* issued plan
   target callback and a successful pre-issuance acquired-prestate gateway.
   The actual writer remains the independent v1547 monotonic gateway."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field policy expected-target!
   source-backed? plan-bound-prestate?]
  (pinned!)
  (when-not (and (boolean? source-backed?)
                 (boolean? plan-bound-prestate?)
                 (or (not plan-bound-prestate?) source-backed?)
                 (or (not source-backed?) (fn? expected-target!))
                 (plain-map? contract-source)
                 (plain-map? operation-writer-source)
                 (plain-map? route-writer-paths)
                 (fn? source!)
                 (or (nil? expected-target!) (fn? expected-target!))
                 (keyword? revision-field)
                 (some? node))
    (reject! :invalid-parallel-mediation-issuance))
  (when source-backed?
    (require-issued-source-backed-target-resolver!
     expected-target! contract-source plan-bound-prestate?))
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
           (when source-backed?
             (require-issued-source-backed-target-resolver!
              expected-target! contract-source plan-bound-prestate?))
           (let [host (original-adoption adoption-handle contract-source
                                         intermediate-system)
                 intermediate-handler (get intermediate-system handler-slot)]
             (when-not (and (= adoption/report-type (:type host))
                            (= :identical-to-guarded-checked-router-wrapper
                               (:handler-slot host))
                            (fn? intermediate-handler))
               (reject! :unverified-intermediate-biff-handler))
             (let [routes (checked-literal-bindings!
                           contract-source route-writer-paths
                           (:checked-certified-route-ids host))
                   singleton? (= 1 (count routes))
                   writer-sources (per-operation-inventory-sources!
                                   operation-writer-source routes)
                   final-handler
                   (fn [request]
                     (pinned!)
                     (when *current-request-writer*
                       (reject! :nested-parallel-request-dispatch))
                     (let [{:keys [operation id path] :as selected}
                           (selected-literal-route! routes request)]
                     ;; Provenance revalidation precedes source construction,
                     ;; so an unissued or foreign resolver never obtains a
                     ;; writer, irrespective of the supplied candidate.
                     (when source-backed?
                       (require-issued-source-backed-target-resolver!
                        expected-target! contract-source plan-bound-prestate?))
                     (original-adoption adoption-handle contract-source
                                        intermediate-system)
                     (let [source (source! request)]
                       (pinned!)
                       (original-adoption adoption-handle contract-source
                                          intermediate-system)
                       ;; All target checks precede the ONLY issuance call.
                       ;; This checks source identity, NOT the independent
                       ;; business meaning or authority of expected-target!.
                       (when expected-target!
                         (require-target-binding-before-issuance!
                          operation request source expected-target!))
                       (when plan-bound-prestate?
                         (require-plan-bound-prestate-before-issuance!
                          node contract-source expected-target! operation
                          request source))
                       (let [issuance (original-issue path operation node
                                                      source revision-field)
                             writer (:writer issuance)
                             registry (atom {path {:kind :issued-operation
                                                   :operation-id operation
                                                   :writer writer}})
                             inventory {:operation-writer-source (get writer-sources operation)
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
                                     (if singleton?
                                       (original-observe
                                        adoption-handle contract-source
                                        intermediate-system inventory handles
                                        route-writer-paths request)
                                       (original-biff-observe
                                        adoption-handle contract-source
                                        intermediate-system request inventory
                                        handles path))))) }
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
                                        (= (if singleton?
                                             assembly/invocation-report-type
                                             writers/biff-observation-type)
                                           (get-in outcome [:result :report :type]))
                                        (= (if singleton?
                                             :conditional-observed-route-invoked-joined-one-shot-writer
                                             :observed-checked-biff-handler-invoked-exact-registered-writer)
                                           (get-in outcome [:result :report :status]))
                                        (= 1 (get-in outcome [:result :report :observed-registered-callback-invocations]))
                                        (if singleton?
                                          (and (= 1 (get-in outcome [:result :report :checked-certified-route-count]))
                                               (true? (get-in outcome [:result :report :all-source-checked-routes-locally-joined?])))
                                          (and (= id (get-in outcome [:result :report :observed-route-id]))
                                               (= operation (get-in outcome [:result :report :observed-operation-id]))
                                               (= path (get-in outcome [:result :report :observed-writer-path-id]))
                                               (true? (get-in outcome [:result :report :registered-callback-returned?])))))
                           (reject! :unexpected-parallel-invocation-evidence))
                         (get-in outcome [:result :response])))))
                   final-handler (if policy
                                   (bounded-parallel-dispatch final-handler policy)
                                   final-handler)
                   final-system (assoc intermediate-system handler-slot final-handler)]
               (reset! started {:source contract-source
                                :intermediate-system intermediate-system
                                :intermediate-handler intermediate-handler
                                :final-system final-system
                                :final-handler final-handler
                                :route-count (count routes)
                                :admission-policy policy
                                :target-binding-required? (boolean expected-target!)
                                :source-backed-target-resolver-required?
                                source-backed?
                                :plan-bound-prestate-required?
                                plan-bound-prestate?})
               final-system)))}]
    (locking issued (.put issued handle {:source contract-source
                                       :initialized initialized
                                       :started started
                                       :adoption-handle adoption-handle}))
    {:module module :handle handle}))

(defn issue-isolated-parallel-biff-request-module!
  "Original v1567/v1569 unbounded parallel request-local mediation API.
   This issuer retains its existing registration and handler semantics."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field]
  (issue-parallel-request-module*
   adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field nil nil false false))

(defn issue-bounded-isolated-parallel-biff-request-module!
  "Issue the same independent request-local Biff mediator with bounded active
   parallelism and bounded WAITING+ACTIVE admission. The finite policy is
   {:max-active-requests N :max-pending-requests M :wait-timeout-ms T},
   2 <= N <= 64, N <= M <= 256, 1 <= T <= 60000. It never serializes all
   handlers: up to N real handlers may run simultaneously. It does not issue
   writer capabilities to waiting or rejected requests, nor authorize writes
   by alternate handlers or unregistered XTDB clients. A caller may be rejected
   on timeout, overload or interruption; ordinary handler failure propagates."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field policy]
  (issue-parallel-request-module*
   adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field
   (bounded-parallel-policy! policy) nil false false))

(defn issue-target-bound-isolated-parallel-biff-request-module!
  "Explicit unbounded request-local mediator that requires a separate
   request->expected-target! callback. For EVERY matched request, check the
   selected certified operation and the exact candidate source's UPDATE and
   protected document against that expectation BEFORE writer issuance.
   Wrong-document substitutions fail without submitting an XTDB transaction.

   The callback must derive a trustworthy expected target independently of the
   transaction source. Gesso cannot authenticate that external premise, nor
   prove a matching command's business semantics. Direct writers remain open.
   The legacy 7-argument issuer deliberately retains its prior behavior."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field expected-target!]
  (when-not (fn? expected-target!)
    (reject! :requires-independent-request-target-builder))
  (issue-parallel-request-module*
   adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field nil expected-target! false false))

(defn issue-target-bound-bounded-isolated-parallel-biff-request-module!
  "Bounded parallel variant requiring an independent request target callback.
   A rejected target uses no writer permit or transaction authority beyond
   the already admitted request slot. The caller must independently ground
   its expectation in the declared operation and the request, not in source!."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field policy expected-target!]
  (when-not (fn? expected-target!)
    (reject! :requires-independent-request-target-builder))
  (issue-parallel-request-module*
   adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field
   (bounded-parallel-policy! policy) expected-target! false false))

(defn issue-source-backed-target-bound-isolated-parallel-biff-request-module!
  "v1580 unbounded request-local mediator that REQUIRES a v1578 issuance map
   {:expected-target! exact-issued-resolver} for the very same certified
   contract. Unlike v1576's host-callback issuer, arbitrary target callbacks,
   forged resolver wrappers, and resolvers from another contract are rejected.
   Issued provenance is rechecked at startup and on every matched request,
   and the separately declared document is checked against the actual command
   before any guarded XTDB writer is issued.

   These checks do NOT authenticate the application-owned target declaration,
   prove business-rule refinement, or close bypasses by external XTDB writers."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field resolver-issuance]
  (let [resolver (require-resolver-issuance!
                  resolver-issuance contract-source false)]
    (issue-parallel-request-module*
     adoption-handle contract-source operation-writer-source
     route-writer-paths node source! revision-field nil resolver true false)))

(defn issue-source-backed-target-bound-bounded-isolated-parallel-biff-request-module!
  "Bounded-parallel version of the v1580 issued source-backed target
   mediator. Uses the same exact independently declared and certified literal
   routes, with finite request concurrency and no fall back to host callbacks.
   Rejected provenance or mismatched physical targets issue no XTDB writer."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field policy resolver-issuance]
  (let [resolver (require-resolver-issuance!
                  resolver-issuance contract-source false)]
    (issue-parallel-request-module*
     adoption-handle contract-source operation-writer-source
     route-writer-paths node source! revision-field
     (bounded-parallel-policy! policy) resolver true false)))

(defn issue-plan-bound-guarded-prestate-isolated-parallel-biff-request-module!
  "v1588 opt-in unbounded mediator. Require an EXACT Gesso-issued resolver
   whose sealed static targets equal the :target! callbacks in current
   certified plans. Before issuing each request's registered monotonic writer,
   independently acquire a full guarded XTDB prestate through v1586.
   The acquired gateway's submit! NEVER enters the Biff handler registry and
   is never invoked. The real registered writer is still prepared separately
   through v1547, with its original monotonic-revision guard intact.

   This is pre-issuance enforcement and observational correspondence, NOT a
   unified submission-capability refinement or authenticated business target."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field resolver-issuance]
  (let [resolver (require-resolver-issuance!
                  resolver-issuance contract-source true)]
    (issue-parallel-request-module*
     adoption-handle contract-source operation-writer-source
     route-writer-paths node source! revision-field nil resolver true true)))

(defn issue-plan-bound-guarded-prestate-bounded-isolated-parallel-biff-request-module!
  "Bounded parallel variant of the v1588 plan-bound, pre-issuance guarded
   prestate mediator. Both real handler concurrency limits and the original
   monotonic one-shot writer identity remain intact."
  [adoption-handle contract-source operation-writer-source
   route-writer-paths node source! revision-field policy resolver-issuance]
  (let [resolver (require-resolver-issuance!
                  resolver-issuance contract-source true)]
    (issue-parallel-request-module*
     adoption-handle contract-source operation-writer-source
     route-writer-paths node source! revision-field
     (bounded-parallel-policy! policy) resolver true true)))

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
     :certified-literal-route-count (:route-count state)
     :unselected-route-invocations :not-observed
     :parallel-handler-dispatch :permitted
     :parallel-admission-policy
     (if (:admission-policy state)
       :bounded-fair-permits-with-request-local-registries
       :unbounded-per-issued-mediator)
     :max-active-requests (get-in state [:admission-policy :max-active-requests])
     :max-pending-requests (get-in state [:admission-policy :max-pending-requests])
     :wait-timeout-ms (get-in state [:admission-policy :wait-timeout-ms])
     :request-scoped-one-shot :fresh-per-request
     :source-backed-target-resolver-required?
     (boolean (:source-backed-target-resolver-required? state))
     :plan-bound-guarded-prestate-required?
     (boolean (:plan-bound-prestate-required? state))
     :prestate-submission-correspondence
     (if (:plan-bound-prestate-required? state)
       :independent-preparation-before-monotonic-writer-not-same-capability
       :not-established)
     :operation-target-binding-policy
     (if (:target-binding-required? state)
       :required-before-each-request-writer-issuance
       :not-installed-for-legacy-issuer)
     :expected-target-provenance
     (cond
       (:plan-bound-prestate-required? state)
       :issued-certified-plan-callback-identity-checked-table-not-authenticated
       (:source-backed-target-resolver-required? state)
       :issued-source-linked-static-route-declaration-not-authenticated
       (:target-binding-required? state)
       :independent-host-callback-not-authenticated
       :else :not-established)
     :in-process-callback-trace :one-checked-invocation-per-request
     :biff-handler-slot :exact-local-identity-checked
     :actual-http-server-adoption :not-established
     :other-writer-closure :not-established
     :operation-refinement :not-established
     :open-obligations
     (cond-> open-obligations
       (:plan-bound-prestate-required? state)
       (conj :acquired-prestate-probe-and-real-monotonic-writer-are-distinct-preparations))
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))
