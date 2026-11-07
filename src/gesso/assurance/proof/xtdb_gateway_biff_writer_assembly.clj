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
   an actual request; its post-dispatch rejection cannot undo writes."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.checked-biff-host-adoption :as biff]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as writers]
            [gesso.live.application-preflight :as application]))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-biff-writer-assembly/local-join)
(def report-version 1)
(def ^:private max-bindings 512)
(def ^:private original-host biff/require-current-biff-host-adoption!)
(def ^:private original-writers writers/require-registered-writer-coverage!)
(def ^:private original-summary application/operation-summary)
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
    :physical-refinement-and-deployment-certification})

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
