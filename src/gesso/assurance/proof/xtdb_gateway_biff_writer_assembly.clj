(ns gesso.assurance.proof.xtdb-gateway-biff-writer-assembly
  "v1555: one fail-closed, READ-ONLY check of the joined local Biff handler,
   certified literal route, and issued monotonic XTDB writer registrations.

   Unlike an independent Biff adoption report and a writer inventory report,
   this check requires a TOTAL, source-matched route-to-registered-writer join:
   every checked route is certified and explicitly bound to one host path of
   the same independently derived operation; every surveyed writer path is
   used by at least one route. The same original in-process Biff handler and
   host writer registrations are rechecked at the end. No handler, writer, or
   transaction is invoked.

   This is NOT a deployment certificate. The route-to-writer binding is an
   assembly declaration, not observed invocation. The enumerator is host-
   supplied and may omit writers; direct XTDB clients, later mutations,
   actual command-to-operation correspondence, and physical effects remain
   open. Call the separate Biff route-to-writer observer for evidence about
   an actual request; its post-dispatch rejection cannot undo writes.

   v1557: an additional composed, post-dispatch observer checks the TOTAL
   local assembly both before and after one exact Biff handler invocation.
   This does not convert a single observed request into route-wide coverage."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as biff]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.live.application-preflight :as application]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-biff-writer-assembly/local-join)
(def report-version 1)
(def invocation-report-type
  :gesso.assurance.proof.xtdb-gateway-biff-writer-assembly/observed-invocation)
(def invocation-report-version 1)
(def ^:private max-bindings 512)
(def ^:private original-host biff/require-current-biff-host-adoption!)
(def ^:private original-writers writers/require-registered-writer-coverage!)
(def ^:private original-summary application/operation-summary)
(def ^:private original-biff-invocation
  writers/observe-checked-biff-host-registered-writer!)
(def ^:private open-obligations
  #{:host-enumerator-exhaustiveness-and-after-check-stability
    :deployed-server-uses-this-biff-system-handler
    :route-invokes-its-declared-registered-writer
    :actual-request-and-principal-authentication
    :unregistered-direct-and-external-xtdb-writes
    :writer-command-refines-declared-abstract-operation
    :writer-history-monotonicity-outside-issued-gateways
    :one-shot-callback-lifetime-versus-repeated-http-requests
    :transaction-commit-and-poststate-causality
    :physical-refinement-and-deployment-certification
    :untested-routes-and-requests-still-need-invocation-observations
    :one-shot-callback-reissuance-for-subsequent-requests})

(defn- reject! [reason]
  ;; Deliberately omit source values, operation IDs, writer handles, paths,
  ;; physical rows and handlers from exceptions and reports.
  (throw (ex-info "Invalid Gesso Biff/writer assembly correspondence."
                  {:xtdb-gateway-biff-writer-assembly/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- pinned! []
  ;; This protects against ordinary with-redefs substitution, not arbitrary
  ;; JVM reflection, instrumentation, or replacement of loaded bytecode.
  (when-not (and (identical? original-host
                             (var-get #'biff/require-current-biff-host-adoption!))
                 (identical? original-writers
                             (var-get #'writers/require-registered-writer-coverage!))
                 (identical? original-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-host-writer-or-assembly-checker)))

(defn- literal-route? [route]
  ;; Only the exact static method/path subset can be joined here. Parameter
  ;; templates, splats and future routing languages must get an explicit
  ;; separately checked correspondence, not a silent approximation.
  (let [path (:path route)]
    (and (qualified-id? (:route-id route))
         (keyword? (:method route))
         (string? path)
         (.startsWith ^String path "/")
         (not (re-find #"[{}:*]" path)))))

(defn- checked-routes! [source checked-route-ids]
  (when-not (and (plain-map? source)
                 (contains? source :application-assembly))
    (reject! :invalid-original-biff-contract-source))
  (let [summary (original-summary
                 (:operation-acquisition-assembly
                  (:application-assembly source)))
        entries (vec (for [[operation row] summary
                           route (:routes row)]
                       {:operation-id operation
                        :route-id (:route-id route)
                        :route route}))
        route-ids (mapv :route-id entries)]
    (when-not (and (seq entries)
                   (<= (count entries) max-bindings)
                   (every? (fn [{:keys [operation-id route]}]
                             (and (qualified-id? operation-id)
                                  (literal-route? route)))
                           entries)
                   (= (count route-ids) (count (set route-ids)))
                   (= (set route-ids) checked-route-ids))
      (reject! :uncovered-dynamic-uncertified-or-ambiguous-routes))
    (into {} (map (juxt :route-id :operation-id) entries))))

(defn- observed-registrations! [inventory-source]
  (let [enumerate! (:enumerate-write-paths! inventory-source)
        observed (try
                   (when-not (fn? enumerate!)
                     (reject! :invalid-host-enumerator))
                   (enumerate!)
                   (catch clojure.lang.ExceptionInfo e
                     (if (:xtdb-gateway-biff-writer-assembly/rejection
                          (ex-data e))
                       (throw e)
                       (reject! :host-enumeration-failed)))
                   (catch Throwable _ (reject! :host-enumeration-failed)))]
    (when-not (and (plain-map? observed)
                   (seq observed)
                   (<= (count observed) max-bindings)
                   (every? qualified-id? (keys observed))
                   (every? (fn [[_ entry]]
                             (and (plain-map? entry)
                                  (= #{:kind :operation-id :writer}
                                     (set (keys entry)))
                                  (= :issued-operation (:kind entry))
                                  (qualified-id? (:operation-id entry))
                                  (fn? (:writer entry))))
                           observed))
      (reject! :invalid-or-unaccounted-host-registrations))
    observed))

(defn- same-registrations? [a b]
  (and (= (set (keys a)) (set (keys b)))
       (every? (fn [path]
                 (let [x (get a path) y (get b path)]
                   (and (= (:kind x) (:kind y))
                        (= (:operation-id x) (:operation-id y))
                        (identical? (:writer x) (:writer y)))))
               (keys a))))

(defn require-current-biff-registered-writer-assembly!
  "Fail closed on an incomplete *local* route-to-writer join, without dispatch.

   [host-handle original-contract-source returned-biff-system
    inventory-source issued-writer-handles route-writer-paths]

   `route-writer-paths` is the host's explicit {certified-route-id path-id}
   binding. It must cover EVERY source-derived checked route. Every currently
   surveyed host writer must be bound to at least one such route, and each
   route's operation ID must equal its registered writer's observed operation
   ID. Writer identity/issuance and Biff handler provenance are independently
   reverified by their ORIGINAL checkers before and after this join.

   Returns only a redacted, non-authoritative report. Never calls a handler,
   writer, execute-tx, or changes host state. The host's supplied enumeration
   and the returned Biff system are not deployment attestations."
  [host-handle original-contract-source returned-biff-system
   inventory-source issued-writer-handles route-writer-paths]
  (pinned!)
  (when-not (and (plain-map? route-writer-paths)
                 (seq route-writer-paths)
                 (<= (count route-writer-paths) max-bindings)
                 (every? qualified-id? (keys route-writer-paths))
                 (every? qualified-id? (vals route-writer-paths)))
    (reject! :invalid-route-writer-bindings))
  ;; Recheck the genuine Biff module and fresh host writer inventory, not
  ;; copies of either previously issued diagnostic map.
  (let [host (original-host host-handle original-contract-source
                            returned-biff-system)
        registered (original-writers inventory-source issued-writer-handles)
        checked (:checked-route-ids host)
        certified (:checked-certified-route-ids host)]
    (when-not (and (= biff/report-type (:type host))
                   (= biff/report-version (:version host))
                   (= :not-issued (:certification host))
                   (= :not-admitted (:admission host))
                   (= :none (:authority host))
                   (= checked certified)
                   (set? checked)
                   (= writers/report-type (:type registered))
                   (= writers/report-version (:version registered))
                   (true? (:all-currently-surveyed-writers-locally-bound?
                           registered)))
      (reject! :unexpected-host-or-writer-evidence-contract))
    (let [route-operations (checked-routes! original-contract-source checked)
          first-snapshot (observed-registrations! inventory-source)
          paths (set (keys first-snapshot))]
      (when-not (and (= (set (keys route-operations))
                        (set (keys route-writer-paths)))
                     (= paths (set (vals route-writer-paths)))
                     (= paths (set (keys issued-writer-handles)))
                     (every? (fn [[route-id path-id]]
                               (= (get route-operations route-id)
                                  (get-in first-snapshot [path-id :operation-id])))
                             route-writer-paths))
        (reject! :incomplete-or-mismatched-route-writer-join))
      (pinned!)
      (let [host-after (original-host host-handle original-contract-source
                                       returned-biff-system)
            registered-after
            (original-writers inventory-source issued-writer-handles)
            last-snapshot (observed-registrations! inventory-source)]
        (when-not (and (= host host-after)
                       (= registered registered-after)
                       (same-registrations? first-snapshot last-snapshot))
          (reject! :host-or-writer-registration-changed-during-check))
        {:type report-type
         :version report-version
         :status :conditional-current-biff-handler-and-issued-writers-joined
         :scope :one-returned-process-local-biff-system-and-host-writer-survey
         :checked-certified-route-count (count route-operations)
         :matched-route-writer-binding-count (count route-writer-paths)
         :covered-surveyed-writer-count (count paths)
         :all-source-checked-routes-locally-joined? true
         :all-surveyed-writer-paths-locally-joined? true
         :same-biff-handler-slot-rechecked? true
         :same-host-registrations-rechecked? true
         :route-writer-invocation :not-established
         :deployed-server-handler-identity :not-established
         :host-writer-enumerator-exhaustiveness :not-established
         :physical-execution-and-refinement :not-established
         :open-obligations (set/union open-obligations
                                      (:open-obligations host)
                                      (:open-obligations registered))
         :certificate :not-issued
         :admission :not-admitted
         :transaction-authority :none
         :authority :none}))))


(defn- invocation-pinned! []
  ;; Keep this additional seam specific to the composed observer. The v1555
  ;; no-dispatch assembly check must not depend on optional route observation.
  (pinned!)
  (when-not (identical? original-biff-invocation
                        (var-get #'writers/observe-checked-biff-host-registered-writer!))
    (reject! :substituted-biff-invocation-checker)))

(defn- requested-literal-route!
  [source known-route-operations route-writer-paths request]
  ;; The assembly check has already checked EVERY source route. Re-derive the
  ;; request match rather than trust caller-supplied operation/route/path IDs.
  (when-not (and (plain-map? request)
                 (keyword? (:request-method request))
                 (string? (:uri request)))
    (reject! :invalid-observed-biff-request))
  (let [summary (original-summary
                 (:operation-acquisition-assembly
                  (:application-assembly source)))
        matches (vec (for [[operation row] summary
                           route (:routes row)
                           :when (= [(:request-method request) (:uri request)]
                                    [(:method route) (:path route)])]
                       {:operation operation :route-id (:route-id route)
                        :route route}))]
    (when-not (and (= 1 (count matches))
                   (literal-route? (:route (first matches)))
                   (= (:operation (first matches))
                      (get known-route-operations (:route-id (first matches))))
                   (contains? route-writer-paths (:route-id (first matches))))
      (reject! :request-not-one-source-checked-literal-bound-route))
    (let [matched (first matches)]
      {:route-id (:route-id matched)
       :operation-id (:operation matched)
       :path-id (get route-writer-paths (:route-id matched))})))

(defn observe-current-biff-registered-writer-assembly!
  "Observe ONE request against a complete, freshly joined Biff/writer assembly.

   [host-handle contract-source returned-system inventory-source
    issued-writer-handles route-writer-paths request]

   The total route/writer assembly is checked BEFORE dispatch. The selected
   literal route is independently matched to the supplied request and its
   bound writer. The ORIGINAL Biff-installed-handler observer must witness
   exactly one synchronous invocation/normal return of that exact issued
   callback. The TOTAL assembly is then rechecked after dispatch. A positive
   report is evidence only for THIS one local request, NOT all routes, future
   requests, writer-history closure, deployed-server identity, or a committed
   physical effect. This function cannot undo writes on post-dispatch failure.

   Each issued callback is a ONE-SHOT. A successful dispatch consumes it;
   a later HTTP request requires a newly prepared and issued capability.
   This observer does NOT silently reissue the callback or certify its reuse."
  [host-handle contract-source returned-system inventory-source
   issued-writer-handles route-writer-paths request]
  (invocation-pinned!)
  (let [pre (require-current-biff-registered-writer-assembly!
             host-handle contract-source returned-system inventory-source
             issued-writer-handles route-writer-paths)
        known (checked-routes!
               contract-source
               (:checked-route-ids
                (original-host host-handle contract-source returned-system)))
        {:keys [route-id operation-id path-id]}
        (requested-literal-route!
         contract-source known route-writer-paths request)
        observed (original-biff-invocation
                  host-handle contract-source returned-system request
                  inventory-source issued-writer-handles path-id)
        evidence (:report observed)]
    ;; The original observer traces one invocation. Fail closed if a future
    ;; implementation changes the shape or weakens its declared conclusion.
    (when-not (and (= writers/biff-observation-type (:type evidence))
                   (= writers/biff-observation-version (:version evidence))
                   (= :observed-checked-biff-handler-invoked-exact-registered-writer
                      (:status evidence))
                   (= :one-synchronous-in-process-biff-handler-dispatch
                      (:scope evidence))
                   (= route-id (:observed-route-id evidence))
                   (= operation-id (:observed-operation-id evidence))
                   (= path-id (:observed-writer-path-id evidence))
                   (= 1 (:observed-registered-callback-invocations evidence))
                   (true? (:registered-callback-returned? evidence))
                   (true? (:biff-handler-and-writer-registrations-rechecked?
                           evidence))
                   (= :not-issued (:certificate evidence))
                   (= :not-admitted (:admission evidence))
                   (= :none (:authority evidence))
                   (= :none (:transaction-authority evidence)))
      (reject! :unexpected-or-mismatched-biff-invocation-evidence))
    ;; Fresh inspection after actual handler dispatch is essential: the host
    ;; can replace the handler or writer table while servicing this request.
    (invocation-pinned!)
    (let [post (require-current-biff-registered-writer-assembly!
                host-handle contract-source returned-system inventory-source
                issued-writer-handles route-writer-paths)]
      (when-not (= pre post)
        (reject! :assembly-changed-during-observed-dispatch))
      {:response (:response observed)
       :report
       {:type invocation-report-type
        :version invocation-report-version
        :status :conditional-observed-route-invoked-joined-one-shot-writer
        :scope :one-synchronous-process-local-biff-handler-dispatch
        :all-source-checked-routes-locally-joined? true
        :all-surveyed-writer-paths-locally-joined? true
        :observed-route-bound-to-exact-invoked-registered-writer? true
        :observed-registered-callback-invocations 1
        :biff-handler-and-full-writer-assembly-rechecked-after-dispatch? true
        :checked-certified-route-count (:checked-certified-route-count pre)
        :covered-surveyed-writer-count (:covered-surveyed-writer-count pre)
        :invocation-coverage :only-this-observed-request
        :other-routes-and-future-requests :not-established
        :issued-callback-for-next-request :requires-new-one-shot
        :host-enumerator-exhaustiveness :not-established
        :deployed-server-handler-identity :not-established
        :direct-and-external-xtdb-writer-closure :not-established
        :abstract-operation-refinement :not-established
        :transaction-commit-and-physical-causality :not-established
        :open-obligations (set/union open-obligations
                                     (:open-obligations pre)
                                     (:open-obligations evidence))
        :certificate :not-issued
        :admission :not-admitted
        :transaction-authority :none
        :authority :none}})))
