(ns gesso.live.application-preflight.certified-contract
  "Source-derived assembly adapter for certified-plan operation routes (v1253).

   Gesso's ordinary ApplicationAssembly proves semantic route/Live/browser
   closure relative to declared capabilities; it intentionally does not inspect
   arbitrary Ring handler bodies.  This module adds a small *in-process* link:
   an issued Ring handler calls exactly one v1251 certified-plan executor, and
   each selected semantic route ID in the supplied current route-handler table
   must refer to such an issued handler for its declared semantic operation.

   (issue-certified-handler!
    {:executor          <exact v1251 issued callable>
     :current-source    <fresh, assembly-owned v1251 source>
     :request->input!   (fn [ctx] {:mode :submit :binding ...})
     :result->response! (fn [ctx executor-result] ...)})

   (check-certified-contracts!
    {:application-assembly <current ApplicationAssembly>
     :semantic-route-handlers <route-id -> ACTUAL handler callable>
     :certified-operations <operation -> {:executor callable
                                         :current-source original-source}>})

   NOTE: :semantic-route-handlers MUST be projected from the active routing
   installation, not a second manually maintained claimed route registry.
   This checker cannot observe the physical deployed router or detect a later
   handler swap when the caller provides an old snapshot.  It rechecks the
   existing application semantic/physical-artifact assembly, the original
   certificate and plan source, the issued executor/handler object identities,
   and all route IDs currently declared in that assembly.  A report or copied
   route map is never a credential or transaction authority.

   An issued handler always invokes the certified executor after the trusted
   request conversion succeeds, but input extraction, response rendering,
   authorization, actual deployment adoption, dynamic rendering, atomicity,
   invariant preservation and XTDB correspondence remain explicit assumptions.
   This namespace neither installs a Biff route nor claims to prove such facts."
  (:require [clojure.set :as set]
            [gesso.live.application-preflight :as application]
            [gesso.model.certified-plan :as plan]
            [gesso.model.certified-operation :as certified])
  (:import (java.util WeakHashMap)))

(def adapter-version 1)
(def report-type
  :gesso.live.application-preflight.certified-contract/source-relative-report)
(def handler-type
  :gesso.live.application-preflight.certified-contract/issued-ring-handler)

(def ^:private issue-keys
  #{:executor :current-source :request->input! :result->response!})
(def ^:private issue-with-status-keys
  (conj issue-keys :response-status-contract))
(def ^:private check-keys
  #{:application-assembly :semantic-route-handlers :certified-operations})
(def ^:private binding-keys #{:executor :current-source})
(def ^:private binding-with-status-keys
  (conj binding-keys :response-status-contract))
(def ^:private open-assumptions
  #{:installed-route-table-is-current-and-complete
    :actual-deployment-uses-this-checked-route-table
    :no-later-router-or-handler-replacement
    :no-independent-raw-route-or-direct-executor-bypass
    :trusted-request-binding-and-response-callbacks
    :current-runtime-authorization-and-callback-purity
    :atomic-xtdb-state-guard-and-commit-enforcement
    :application-invariant-preservation
    :source-to-production-execution-correspondence
    :canonical-html-response-preflight-adoption
    ;; This is an application-owned assembly/handler agreement, not yet an
    ;; allowance independently derived from canonical operation semantics.
    :response-status-allowance-not-derived-from-canonical-model-semantics})

;; No report/map/function metadata can substitute for the exact issued callable.
;; Weak references do not retain a handler solely because of registration.
(def ^:private ^WeakHashMap issued-handlers (WeakHashMap.))

(defn- fail! [reason at details]
  (throw (ex-info "Invalid Gesso certified application route binding."
                  (merge {:error/type ::invalid-certified-contract
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [x wanted at]
  (when-not (plain-map? x)
    (fail! :expected-plain-map at {}))
  (when (some? (meta x))
    (fail! :metadata-not-allowed at {}))
  (let [got (set (keys x))]
    (when-not (= got wanted)
      (fail! :wrong-keys at
             {:missing (set/difference wanted got)
              :unexpected (set/difference got wanted)})))
  x)

(defn- require-response-status-contract! [declaration at]
  (let [status (when (map? declaration) (:status declaration))]
    (when-not (and (plain-map? declaration)
                   (nil? (meta declaration))
                   (= #{:status} (set (keys declaration)))
                   (integer? status) (<= 200 status 299))
      (fail! :invalid-response-status-contract at {}))
    declaration))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- registered-handler [handler]
  (when (fn? handler)
    (locking issued-handlers (.get issued-handlers handler))))

;; v1761: Observation is dynamically scoped to a SYNCHRONOUS local Ring call.
;; This reports that the original issued handler returned from its certified
;; plan executor. It does not trace any database write or prove authority.
(def ^:dynamic ^:private *issued-executor-observer* nil)

;; v2078: The original issued handler's result->response! callback produces
;; the response occurrence that a selected physical route must actually
;; return. This is an observation, not response-effect or content admission.
(def ^:dynamic ^:private *issued-response-observer* nil)

;; v1767: Narrower than the v1761 whole-handler observer. This hook runs
;; exclusively around the *original certified executor call*, after the
;; request->input! callback and before result->response!. Any trusted callback
;; invoked BY that executor remains inside the scope: no causal proof follows.
(def ^:dynamic ^:private *issued-executor-scope* nil)

(defn certified-handler?
  "Identity-based recognition only; does not itself establish freshness."
  [handler]
  (boolean (registered-handler handler)))

;; v1791: A reusable, non-authoritative selected-route identity check.
;; Useful to scoped application/source binding checks that do not need to
;; repeat the expensive full application-assembly derivation. The caller
;; must still run check-certified-contracts! on its CURRENT installed table.
(defn issued-handler-binding?
  "Does this EXACT issued handler hold this executor, operation and original
   source? This checks only the in-process registration relation. It does
   NOT check executor freshness, route-table completeness, actual adapter
   adoption, policy authorization, or transaction authority."
  [handler executor operation current-source]
  (let [entry (registered-handler handler)]
    (boolean (and entry
                  (identical? executor (:executor entry))
                  (= operation (:operation entry))
                  (= current-source (:original-source entry))))))

(defn- require-current-binding! [executor current-source]
  (when-not (plan/checked-plan-operation? executor)
    (fail! :unregistered-certified-plan-executor [:executor] {}))
  ;; The only accepted evidence is the original v1251 issued callable. Recheck
  ;; the live certificate, its source, dependency DAG, and callback identities.
  (plan/require-current-plan-operation! executor current-source)
  (let [description (plan/describe-plan-operation executor)
        operation (:operation description)]
    (when-not (qualified-id? operation)
      (fail! :invalid-certified-operation [:executor] {}))
    description))

(defn issue-certified-handler!
  "Issue a Ring-compatible handler which actually delegates to the exact
   current certified-plan executor. Trusted conversion callbacks are opaque.
   The returned function must be inserted into the real router by the app.
   Optional :response-status-contract {:status 200} is a closed, static
   2xx status allowance pinned at issuance. This does not bind browser
   effects, response bodies or operation-owned semantic outcome contracts."
  [source]
  (exact-map! source
              (if (and (map? source) (contains? source :response-status-contract))
                issue-with-status-keys
                issue-keys)
              [])
  ;; An optional immutable, issuance-pinned allowance for ONE HTTP status.
  ;; This is application-owned input to the issued handler, not evidence
  ;; that the canonical operation independently declared response semantics.
  ;; The original result->response! callback must not choose its allowance
  ;; dynamically from its own result or the received request.
  (when (contains? source :response-status-contract)
    (require-response-status-contract! (:response-status-contract source)
                                       [:response-status-contract]))
  (let [{:keys [executor current-source request->input! result->response!]} source]
    (when-not (and (fn? request->input!) (fn? result->response!))
      (fail! :invalid-handler-callbacks [:callbacks] {}))
    (let [proof-description (require-current-binding! executor current-source)]
      (letfn [(certified-route-handler [ctx]
                ;; Revalidation remains in the actual handler invocation.
                (require-current-binding! executor current-source)
                (let [input (request->input! ctx)
                      result (if-let [scope! *issued-executor-scope*]
                               (scope! certified-route-handler ctx
                                       (fn [] (executor ctx input)))
                               (executor ctx input))]
                  ;; Only the authentic issued handler emits this process-local
                  ;; event, after the selected certified executor has returned.
                  ;; A callback-driven nested or duplicate execution is not
                  ;; accepted by the scoped observer below.
                  (when-let [observe! *issued-executor-observer*]
                    (observe! certified-route-handler ctx))
                  (let [response (result->response! ctx result)]
                    (when-let [observe! *issued-response-observer*]
                      (observe! certified-route-handler ctx response))
                    response)))]
        (let [issued certified-route-handler
              entry (cond-> {:executor executor
                             :original-source current-source
                             :operation (:operation proof-description)
                             :request->input! request->input!
                             :result->response! result->response!}
                      (contains? source :response-status-contract)
                      (assoc :response-status-contract
                             (:response-status-contract source)))]
          (locking issued-handlers (.put issued-handlers issued entry))
          issued)))))

(defn observe-issued-response-correlation!
  "Run capture! once while observing the original issued handler's exact
   response callback. capture! returns an observation with :outcome and
   :result (the *final* actual Ring response). A positive same-occurrence
   relation requires exactly one callback on the same thread, exact handler
   and request object, and object identity of the callback's returned value
   with the final Ring response. This is a bounded in-process observation:
   it does not verify the contents, HTML, headers, browser effects, or the
   callback's honesty, and does not authenticate a deployed server. When
   the handler carries a static issuance-bound :response-status-contract,
   its actual returned status is checked independently of the callback's
   chosen result. Such a match is only issued-source-relative, NOT a
   canonical operation response proof."
  [handler request capture!]
  (when-not (and (fn? capture!) (map? request)
                 (registered-handler handler))
    (fail! :invalid-issued-response-observation [:response] {}))
  (when *issued-response-observer*
    (fail! :nested-issued-response-observation [:response] {}))
  (let [{:keys [executor original-source] :as issued}
        (registered-handler handler)
        _ (require-current-binding! executor original-source)
        calling-thread (Thread/currentThread)
        events (volatile! [])
        capture (binding [*issued-response-observer*
                         (fn [actual-handler actual-request response]
                           (vswap! events conj
                                   {:right-handler? (identical? handler actual-handler)
                                    :right-request? (identical? request actual-request)
                                    :same-thread? (identical? calling-thread
                                                              (Thread/currentThread))
                                    :response response}))]
                  (capture!))
        event (first @events)
        same-response?
        (boolean (and (= 1 (count @events))
                      (:right-handler? event)
                      (:right-request? event)
                      (:same-thread? event)
                      (= :returned (:outcome capture))
                      (identical? (:response event) (:result capture))))
        declared-status
        (cond
          (not (contains? issued :response-status-contract)) :not-declared
          (not same-response?) :unconfirmed
          (= (get-in issued [:response-status-contract :status])
             (get-in capture [:result :status])) :matched
          :else :mismatched)]
    {:capture capture
     :same-issued-response? same-response?
     ;; A redacted, source-relative result: never expose callback response
     ;; or claimed expected status in this copyable diagnostic evidence.
     :issued-response-status-declaration declared-status}))

(defn observe-issued-executor-invocation!
  "Run invoke! synchronously, accepting exactly one callback from the exact
   registered issued handler, with the identical Ring request object, after
   its certified-plan executor returned. A synthetic 200 response alone is
   insufficient. Reject missing, nested, substituted-request, or duplicate
   executions. Caller must independently prove that invoke! is the selected
   Biff handler slot; this helper cannot certify host adoption or XTDB writes.
   Output contains only the invocation count and actual return, never source
   capabilities or captured requests."
  [handler request invoke!]
  (when-not (and (fn? invoke!) (map? request)
                 (registered-handler handler))
    (fail! :invalid-issued-executor-observation [:observation] {}))
  (when *issued-executor-observer*
    (fail! :nested-issued-executor-observation [:observation] {}))
  (let [{:keys [executor original-source]} (registered-handler handler)
        _ (require-current-binding! executor original-source)
        events (volatile! [])]
    (binding [*issued-executor-observer*
              (fn [called-handler called-request]
                ;; Never retain the request or handler in an observer report.
                (vswap! events conj (and (identical? called-handler handler)
                                         (identical? called-request request))))]
      (let [result (invoke!)]
        (when-not (= [true] @events)
          (fail! :missing-or-ambiguous-issued-executor-invocation
                 [:observation] {}))
        {:result result
         :executor-calls :one-exact-issued-handler-executor-returned
         :physical-causality :not-established}))))

;; v1767: Capture the original certified executor's invocation interval,
;; never merely the presence of a gateway trace in an input or output callback.
(defn observe-issued-executor-scope!
  "Invoke the actual issued handler via invoke-handler!, wrapping ONLY its
   original certified executor call with around-executor!. The latter receives
   a single-use function that runs the real registered executor. A qualifying
   return requires one matching handler and identical request, one completed
   executor call, and an unchanged result. Nested capture, callback-only
   invocations and manufactured results fail closed.

   This is local cooperative JVM instrumentation: callbacks EXECUTED WITHIN
   the original executor remain trusted, and neither physical effects nor
   deployed-server routing are independently authenticated."
  [handler request around-executor! invoke-handler!]
  (when-not (and (fn? around-executor!) (fn? invoke-handler!) (map? request)
                 (registered-handler handler))
    (fail! :invalid-executor-scope-observation [:scope] {}))
  (when *issued-executor-scope*
    (fail! :nested-executor-scope-observation [:scope] {}))
  (let [{:keys [executor original-source]} (registered-handler handler)
        _ (require-current-binding! executor original-source)
        calls (volatile! 0)
        returned (volatile! false)
        observer-thread (Thread/currentThread)
        scope! (fn [actual-handler actual-request invoke-executor!]
                 (when-not (and (identical? observer-thread
                                            (Thread/currentThread))
                                (identical? handler actual-handler)
                                (identical? request actual-request)
                                (zero? @calls))
                   (fail! :wrong-or-repeated-executor-scope [:scope] {}))
                 (vswap! calls inc)
                 (let [attempted (volatile! false)
                       completed (volatile! false)
                       original-result (volatile! nil)
                       invoke-once! (fn []
                                      (when @attempted
                                        (fail! :executor-scope-double-invocation
                                               [:scope] {}))
                                      (vreset! attempted true)
                                      (let [r (invoke-executor!)]
                                        (vreset! original-result r)
                                        (vreset! completed true)
                                        r))
                       result (around-executor! invoke-once!)]
                   (when-not (and @completed (= result @original-result))
                     (fail! :executor-not-returned-or-result-substituted
                            [:scope] {}))
                   (vreset! returned true)
                   result))]
    (binding [*issued-executor-scope* scope!]
      (let [result (invoke-handler!)]
        (when-not (and (= 1 @calls) @returned)
          (fail! :missing-or-incomplete-executor-scope [:scope] {}))
        {:result result
         :executor-scope :one-original-issued-executor-returned
         :physical-causality :not-established}))))

(defn- require-issued-handler! [handler executor operation source at]
  (let [entry (or (registered-handler handler)
                  (fail! :unissued-route-handler at {}))]
    (when-not (identical? executor (:executor entry))
      (fail! :swapped-route-executor at {}))
    (when-not (= operation (:operation entry))
      (fail! :wrong-route-operation at {}))
    ;; A caller may present a fresh source object with the same value, but
    ;; v1251 additionally enforces exact trusted callback/certificate identity.
    (when-not (= source (:original-source entry))
      (fail! :changed-handler-source at {}))
    (require-current-binding! executor source)
    entry))

(defn- declared-routes! [operations]
  (when-not (map? operations)
    (fail! :invalid-application-operation-summary [:application-assembly] {}))
  (reduce-kv
   (fn [acc operation entry]
     (when-not (qualified-id? operation)
       (fail! :invalid-application-operation [:application-assembly operation] {}))
     (let [routes (:routes entry)]
       (when-not (vector? routes)
         (fail! :invalid-declared-operation-routes
                [:application-assembly operation] {}))
       (reduce (fn [m route]
                 (let [{:keys [route-id method path]} route]
                   (when-not (and (qualified-id? route-id)
                                  (keyword? method)
                                  (string? path) (seq path))
                     (fail! :invalid-route-coordinate
                            [:application-assembly operation :routes] {}))
                   (when (contains? m route-id)
                     (fail! :duplicate-route-id
                            [:application-assembly operation route-id] {}))
                   (assoc m route-id
                          {:operation operation
                           :route-id route-id :method method :path path})))
               acc routes)))
   (sorted-map) operations))

(defn check-certified-contracts!
  "Freshly verify one application-backed semantic routing *snapshot*.

   Every semantic route in the existing ApplicationAssembly must be present in
   :semantic-route-handlers, and every operation that is to be called certified
   must use a v1253-issued handler invoking its exact v1251 plan executor at
   ALL of its declared routes. If the application-owned certified-operation
   binding declares an HTTP response status, every issued handler for that
   operation must carry precisely the same issuance-pinned allowance. Neither
   declaration alone can authenticate or override the other. This is NOT yet
   a canonical operation-derived response contract. Other operations remain
   explicitly uncertified.

   Success means conditional binding correspondence within the supplied table;
   only the actual host can establish that this is the active installed table."
  [options]
  (exact-map! options check-keys [])
  (let [{:keys [application-assembly semantic-route-handlers
                certified-operations]} options]
    (when-not (application/application-assembly? application-assembly)
      (fail! :invalid-or-stale-application-assembly [:application-assembly] {}))
    (when-not (and (plain-map? semantic-route-handlers)
                   (nil? (meta semantic-route-handlers))
                   (every? qualified-id? (keys semantic-route-handlers))
                   (every? fn? (vals semantic-route-handlers)))
      (fail! :invalid-route-handler-table [:semantic-route-handlers] {}))
    (when-not (and (plain-map? certified-operations)
                   (nil? (meta certified-operations))
                   (seq certified-operations)
                   (every? qualified-id? (keys certified-operations)))
      (fail! :invalid-certified-operation-table [:certified-operations] {}))
    (let [summary (application/operation-summary
                   (:operation-acquisition-assembly application-assembly))
          routes (declared-routes! summary)
          declared-ids (set (keys routes))
          installed-ids (set (keys semantic-route-handlers))
          declared-operations (set (keys summary))
          supplied-operations (set (keys certified-operations))]
      (when-not (= declared-ids installed-ids)
        (fail! :semantic-route-table-mismatch [:semantic-route-handlers]
               {:missing (set/difference declared-ids installed-ids)
                :unexpected (set/difference installed-ids declared-ids)}))
      (when-not (set/subset? supplied-operations declared-operations)
        (fail! :unknown-certified-operation [:certified-operations]
               {:unknown (set/difference supplied-operations declared-operations)}))
      (let [certified-descriptions
            (into (sorted-map)
                  (for [[operation binding] certified-operations]
                    (do
                      (exact-map! binding
                                  (if (and (map? binding)
                                           (contains? binding :response-status-contract))
                                    binding-with-status-keys
                                    binding-keys)
                                  [:certified-operations operation])
                      (when (contains? binding :response-status-contract)
                        (require-response-status-contract!
                         (:response-status-contract binding)
                         [:certified-operations operation :response-status-contract]))
                      ;; The original trusted operation (not the checked host
                      ;; table or the response callback) owns the allowance.
                      ;; Presence-sensitive equality prevents both a forged
                      ;; extra allowance and silent loss of a declared one.
                      (when-not (= (select-keys (get summary operation)
                                                [:response-status-contract])
                                   (select-keys binding
                                                [:response-status-contract]))
                        (fail! :operation-response-status-contract-mismatch
                               [:certified-operations operation] {}))
                      (let [{:keys [executor current-source]} binding
                            proof-description
                            (require-current-binding! executor current-source)
                            source-description
                            (certified/describe-operation executor)]
                        (when-not (= operation (:operation proof-description))
                          (fail! :certified-operation-identity-mismatch
                                 [:certified-operations operation] {}))
                        (let [operation-routes
                              (into (sorted-set)
                                    (for [[route-id route] routes
                                          :when (= operation (:operation route))]
                                      route-id))]
                          (when-not (seq operation-routes)
                            (fail! :missing-certified-operation-route
                                   [:certified-operations operation] {}))
                          (doseq [route-id operation-routes]
                            (let [issued (require-issued-handler!
                                          (get semantic-route-handlers route-id)
                                          executor operation current-source
                                          [:semantic-route-handlers route-id])]
                              ;; The operation table and each installed route
                              ;; must independently bind the exact allowance.
                              ;; Checking select-keys preserves the semantic
                              ;; difference between absence and declared nil.
                              (when-not (= (select-keys binding
                                                        [:response-status-contract])
                                           (select-keys issued
                                                        [:response-status-contract]))
                                (fail! :issued-response-status-contract-mismatch
                                       [:semantic-route-handlers route-id] {}))))
                          [operation
                           {:operation operation
                            :route-ids operation-routes
                            :proof-fingerprint (:proof-fingerprint proof-description)
                            :proof-source-fingerprint
                            (:proof-source-fingerprint source-description)
                            :proof-subject-fingerprint
                            (:proof-subject-fingerprint source-description)
                            :dependency-graph-fingerprint
                            (:dependency-graph-fingerprint source-description)
                            :transition-fingerprint
                            (:transition-fingerprint proof-description)
                            :open-premises (:open-premises proof-description)
                            :open-obligations (:open-obligations proof-description)
                            :binding :issued-handler-to-current-plan-executor
                            :response-status-binding
                            (if (contains? binding :response-status-contract)
                              :matched-issued-source
                              :not-declared)}])))))]
        {:type report-type
         :version adapter-version
         :checking :current-source-relative-semantic-route-bindings
         :certified-operations certified-descriptions
         :uncertified-operations
         (set/difference declared-operations supplied-operations)
         :route-ids declared-ids
         :matched-certified-route-ids
         (into #{} (mapcat :route-ids (vals certified-descriptions)))
         :checked #{:current-application-assembly-rechecked
                    :declared-semantic-routes-enumerated
                    :supplied-semantic-route-table-closed
                    :original-plan-and-proof-source-rechecked
                    :exact-issued-handler-to-executor-identities
                    :operation-table-to-issued-response-status-agreement
                    :original-trusted-operation-to-response-status-agreement}
         :open-assumptions open-assumptions
         :open-premises
         (into #{} (mapcat :open-premises (vals certified-descriptions)))
         :open-obligations
         (set/union open-assumptions
                    (into #{}
                          (mapcat (fn [x] (:open-obligations x))
                                  (vals certified-descriptions))))
         :deployment-adoption :not-established
         :physical-operation-correspondence :not-established
         :invariant-preservation :not-proved
         :transaction-authority :none
         :certificate :none}))))
