(ns gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle
  "v1561: opt-in request-time installation and guaranteed retirement of an
   EXACT, freshly issued, request-scoped monotonic XTDB writer callback.

   The host supplies its actual mutable writer registry and an enumerator that
   returns that registry's map by identity. The controller leases the registry
   for one synchronous dispatch, checks the genuine checked Biff host, derives
   the literal source route, prepares the admitted transaction via a caller-
   supplied source builder, atomically installs the issued callback, then runs
   v1557's full assembly + handler invocation check inside v1559's one-use
   thread-bound scope. A CAS removes the callback even when dispatch throws.

   The chosen writer path MUST be vacant outside the leased dispatch; other
   paths may be present if their exact issued handles are supplied. A concurrent
   lifecycle dispatch through this controller is rejected, not queued. A
   changed registry is NEVER overwritten during cleanup. On registry drift the
   callback's scope is still closed, but external registry cleanup is an OPEN
   host obligation. This is NOT transparent middleware for all deployed HTTP
   requests or a lock against other independent host writers/controllers.
   Direct XTDB clients, incomplete enumeration, source-builder behavior,
   route authorization, deployed-server identity and physical refinement are
   NOT established. Post-dispatch rejection cannot undo writes."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.live.application-preflight :as application]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-biff-request-lifecycle/request)
(def report-version 1)
(def ^:private original-host host/require-current-biff-host-adoption!)
(def ^:private original-summary application/operation-summary)
(def ^:private original-issue writers/issue-request-scoped-monotonic-writer!)
(def ^:private original-observe assembly/observe-current-biff-registered-writer-assembly!)
(def ^:private open-obligations
  #{:other-request-entry-points-must-use-this-controller
    :host-enumerator-and-actual-server-dispatch-exhaustiveness
    :independent-controllers-and-other-threads-can-change-registry
    :uncaught-registry-drift-must-be-reconciled-by-host
    :source-builder-operation-and-principal-authorization
    :direct-and-external-xtdb-writer-bypasses
    :callback-and-declared-operation-semantic-correspondence
    :xtdb-transaction-commit-and-physical-causality
    :unobserved-requests-and-unissued-writer-paths
    :application-wide-refinement-and-certification})

(defn- reject! [reason]
  ;; No data, document IDs, request, source, callback, or handler in errors.
  (throw (ex-info "Invalid Gesso request-scoped Biff writer lifecycle."
                  {:xtdb-gateway-biff-request-lifecycle/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- pinned! []
  ;; Against ordinary with-redefs substitution, not malicious JVM reflection.
  (when-not (and (identical? original-host
                             (var-get #'host/require-current-biff-host-adoption!))
                 (identical? original-summary
                             (var-get #'application/operation-summary))
                 (identical? original-issue
                             (var-get #'writers/issue-request-scoped-monotonic-writer!))
                 (identical? original-observe
                             (var-get #'assembly/observe-current-biff-registered-writer-assembly!)))
    (reject! :substituted-host-issuer-or-observer)))

(defn- live-registry! [registry inventory-source]
  ;; Intentional strict identity requirement: a copy or a fabricated host
  ;; enumeration cannot be silently substituted for the supplied registry.
  ;; A cooperating but dishonest enumerator is STILL an untrusted host premise.
  (let [enumerate! (:enumerate-write-paths! inventory-source)
        live (try
               (when-not (fn? enumerate!) (reject! :invalid-host-enumerator))
               (enumerate!)
               (catch clojure.lang.ExceptionInfo e
                 (if (:xtdb-gateway-biff-request-lifecycle/rejection (ex-data e))
                   (throw e)
                   (reject! :host-enumeration-failed)))
               (catch Throwable _ (reject! :host-enumeration-failed)))]
    (when-not (and (plain-map? @registry)
                   (identical? @registry live))
      (reject! :enumerator-not-current-registry-object))
    live))

(defn- literal-request! [source route-writer-paths request]
  (when-not (and (plain-map? request)
                 (keyword? (:request-method request))
                 (string? (:uri request)))
    (reject! :invalid-ring-request))
  (let [summary (original-summary
                 (:operation-acquisition-assembly (:application-assembly source)))
        matches (vec (for [[operation row] summary
                           route (:routes row)
                           :when (= [(:request-method request) (:uri request)]
                                    [(:method route) (:path route)])]
                       {:route-id (:route-id route)
                        :operation-id operation
                        :path (:path route)}))]
    (when-not (and (= 1 (count matches))
                   (qualified-id? (:route-id (first matches)))
                   (qualified-id? (:operation-id (first matches)))
                   (string? (:path (first matches)))
                   (not (re-find #"[{}:*]" (:path (first matches))))
                   (qualified-id?
                    (get route-writer-paths (:route-id (first matches)))))
      (reject! :request-not-one-bound-certified-literal-route))
    (assoc (first matches)
           :writer-path-id (get route-writer-paths (:route-id (first matches))))))

(defn issue-checked-biff-request-writer-lifecycle!
  "Construct a local request-time controller without submitting any XTDB tx.

   [adoption-handle original-contract-source biff-system inventory-source
    registry-atom baseline-issued-handles route-writer-paths]

   The REAL Biff handler must look up its issued callback in registry-atom at
   dispatch time. The supplied inventory's :enumerate-write-paths! MUST return
   that same persistent map object, not a copied/simulated registration map.
   `baseline-issued-handles` describes other already registered writer paths;
   a selected request path must be absent until its fresh callback is issued.

   Returns {:run-request! fn :busy? fn}; call :run-request! with
   [request node request->gateway-source! revision-field]. The builder is
   caller-owned, takes one Ring request and MUST supply a fresh admitted
   command source. Its output is checked by the existing monotonic gateway;
   this API cannot establish the builder's business-rule correspondence.

   A controller rejects overlapping dispatches. It does not globally lock
   other controllers, direct handlers, or the host's atom. Installation and
   retirement are independent CAS operations; cleanup will never clobber an
   outside registry change. Results are redacted; no issuance handle escapes."
  [adoption-handle contract-source system inventory-source
   registry baseline-handles route-writer-paths]
  (pinned!)
  (when-not (and (instance? clojure.lang.Atom registry)
                 (plain-map? contract-source)
                 (plain-map? inventory-source)
                 (plain-map? baseline-handles)
                 (plain-map? route-writer-paths)
                 (plain-map? @registry)
                 (seq route-writer-paths)
                 (<= (count route-writer-paths) 512)
                 (every? qualified-id? (keys route-writer-paths))
                 (every? qualified-id? (vals route-writer-paths))
                 (= (set (keys baseline-handles)) (set (keys @registry)))
                 (every? qualified-id? (keys baseline-handles)))
    (reject! :invalid-host-lifecycle-source))
  (let [host-report (original-host adoption-handle contract-source system)]
    (when-not (and (set? (:checked-certified-route-ids host-report))
                   (= (:checked-route-ids host-report)
                      (:checked-certified-route-ids host-report))
                   (= (set (keys route-writer-paths))
                      (:checked-certified-route-ids host-report)))
      (reject! :incomplete-or-uncertified-source-route-bindings)))
  (live-registry! registry inventory-source)
  (let [active (atom false)]
    {:busy? (fn [] @active)
     :run-request!
     (fn [request node source! revision-field]
       (pinned!)
       (when-not (and (fn? source!) (keyword? revision-field))
         (reject! :invalid-request-source-builder-or-revision))
       (when-not (compare-and-set! active false true)
         (reject! :concurrent-host-request-lifecycle))
       (try
         (original-host adoption-handle contract-source system)
         (let [{:keys [operation-id writer-path-id]} 
               (literal-request! contract-source route-writer-paths request)
               before (live-registry! registry inventory-source)]
           (when-not (and (not (contains? before writer-path-id))
                          (= (set (keys before)) (set (keys baseline-handles))))
             (reject! :selected-writer-not-vacant-or-unexpected-host-path))
           (let [source (source! request)]
             (pinned!)
             (when-not (and (identical? before @registry)
                            (identical? before
                                        (live-registry! registry inventory-source)))
               (reject! :host-registry-changed-during-source-acquisition))
             (let [issuance (original-issue writer-path-id operation-id
                                            node source revision-field)
                   installed (assoc before writer-path-id
                                    {:kind :issued-operation
                                     :operation-id operation-id
                                     :writer (:writer issuance)})
                   handles (assoc baseline-handles writer-path-id
                                  (:handle issuance))]
               ;; Never install a callback if source acquisition or another
               ;; host writer changed the registry. Neither source nor the
               ;; callback escapes in the result.
               (when-not (compare-and-set! registry before installed)
                 (reject! :host-registry-changed-before-install))
               (let [cleanup-ok (volatile! false)
                     outcome
                     (try
                       {:result
                        (do
                          (when-not (identical? installed
                                                (live-registry! registry inventory-source))
                            (reject! :host-registry-changed-at-install))
                          ((:with-request! issuance)
                           (fn []
                             (original-observe
                              adoption-handle contract-source system
                              inventory-source handles route-writer-paths request))))}
                       (catch Throwable t
                         {:thrown t})
                       (finally
                         ;; Do not lose a cleanup conflict behind a handler
                         ;; exception, and NEVER clobber a changed registry.
                         (vreset! cleanup-ok
                                  (compare-and-set! registry installed before))))]
                 (when-not @cleanup-ok
                   (reject! :host-registration-changed-cleanup-not-overwritten))
                 (when-let [thrown (:thrown outcome)]
                   (throw thrown))
                 (let [result (:result outcome)]
                 (pinned!)
                 (when-not (and (= :closed ((:request-scope-status! issuance)))
                                (identical? before
                                            (live-registry! registry inventory-source))
                                (= assembly/invocation-report-type
                                   (get-in result [:report :type]))
                                (= :conditional-observed-route-invoked-joined-one-shot-writer
                                   (get-in result [:report :status])))
                   (reject! :unexpected-request-lifecycle-observation))
                 {:response (:response result)
                  :report
                  {:type report-type
                   :version report-version
                   :status :conditional-one-request-issued-observed-and-retired
                   :scope :one-synchronous-process-local-biff-request
                   :fresh-issued-writer? true
                   :one-shot-request-scope-closed? true
                   :exact-callback-installed-and-invoked? true
                   :registered-callback-retired? true
                   :host-registry-restored-by-compare-and-set? true
                   :concurrent-requests-through-this-controller :rejected
                   :route-invocation-coverage :this-request-only
                   :deployed-server-mediation :not-established
                   :external-writer-closure :not-established
                   :abstract-operation-correspondence :not-established
                   :xtdb-commit-and-poststate-causality :not-established
                   :open-obligations (set/union open-obligations
                                                (get-in result [:report :open-obligations]))
                   :certificate :not-issued
                   :admission :not-admitted
                   :transaction-authority :none
                   :authority :none}})))))
         (finally (reset! active false))))}))
