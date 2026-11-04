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
(def ^:private check-keys
  #{:application-assembly :semantic-route-handlers :certified-operations})
(def ^:private binding-keys #{:executor :current-source})
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
    :canonical-html-response-preflight-adoption})

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

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- registered-handler [handler]
  (when (fn? handler)
    (locking issued-handlers (.get issued-handlers handler))))

(defn certified-handler?
  "Identity-based recognition only; does not itself establish freshness."
  [handler]
  (boolean (registered-handler handler)))

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
   The returned function must be inserted into the real router by the app."
  [source]
  (exact-map! source issue-keys [])
  (let [{:keys [executor current-source request->input! result->response!]} source]
    (when-not (and (fn? request->input!) (fn? result->response!))
      (fail! :invalid-handler-callbacks [:callbacks] {}))
    (let [proof-description (require-current-binding! executor current-source)
          issued (fn certified-route-handler [ctx]
                   ;; Revalidation is deliberately part of the invocation path;
                   ;; this alone cannot detect an independently changed router.
                   (require-current-binding! executor current-source)
                   (let [input (request->input! ctx)
                         result (executor ctx input)]
                     (result->response! ctx result)))
          entry {:executor executor
                 :original-source current-source
                 :operation (:operation proof-description)
                 :request->input! request->input!
                 :result->response! result->response!}]
      (locking issued-handlers (.put issued-handlers issued entry))
      issued)))

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
   ALL of its declared routes. Other operations remain explicitly uncertified.

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
                      (exact-map! binding binding-keys
                                  [:certified-operations operation])
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
                            (require-issued-handler!
                             (get semantic-route-handlers route-id)
                             executor operation current-source
                             [:semantic-route-handlers route-id]))
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
                            :binding :issued-handler-to-current-plan-executor}])))))]
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
                    :exact-issued-handler-to-executor-identities}
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
