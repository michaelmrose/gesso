(ns gesso.assurance.proof.checked-authorization-assembly
  "v1838: Reusable operation/route-scoped ORIGINAL source-to-handler relation.

   For any selected qualified operation and route ID, relate its declared
   catalog construction to the SAME current-source construction of an issued
   certified-plan executor, and identify the EXACT issued Ring handler bound
   to that executor, operation and source. Other application operations and
   routes are allowed. This does not install or certify those routes.

   IMPORTANT: This is a conditional SOURCE-RELATIVE binding, not a checker
   for current ApplicationAssembly closure, original catalog admission,
   authentication, authorization-policy provenance, or physical deployment.
   Its caller MUST separately perform those full original checks before
   startup/adoption. A report is never a certificate or transaction authority.

   v1836: The catalog-wide checker also pins its own original selected-binding
   function. Replacing that public Var must not bypass the per-operation
   construction/handler relation while the original contract check succeeds.

   v1837: Catalog-wide coverage also requires the original ApplicationAssembly
   recognizer and operation-summary facade. A substituted verifier or invented
   route inventory cannot establish current application closure.

   v1838: Reject replacement of the original certified-plan and
   certified-operation proof-verifier functions used by the contract checker.
   Catalog coverage must not rely on permissive substituted proof checks."
  (:require [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-operation :as certified-operation]
            [gesso.model.certified-plan :as plan]))

(def report-type
  :gesso.assurance.proof.checked-authorization-assembly/selected-binding)
(def report-version 1)

(def ^:private original-issued-binding certified/issued-handler-binding?)
(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-application-assembly? application/application-assembly?)
(def ^:private original-operation-summary application/operation-summary)
(def ^:private original-checked-plan-operation? plan/checked-plan-operation?)
(def ^:private original-require-current-plan-operation!
  plan/require-current-plan-operation!)
(def ^:private original-describe-plan-operation plan/describe-plan-operation)
(def ^:private original-describe-certified-operation
  certified-operation/describe-operation)

(defn- plan-proof-boundary-current? []
  (and (identical? original-checked-plan-operation?
                   (var-get #'plan/checked-plan-operation?))
       (identical? original-require-current-plan-operation!
                   (var-get #'plan/require-current-plan-operation!))
       (identical? original-describe-plan-operation
                   (var-get #'plan/describe-plan-operation))
       (identical? original-describe-certified-operation
                   (var-get #'certified-operation/describe-operation))))

(defn- application-boundary-current? []
  (and (identical? original-application-assembly?
                   (var-get #'application/application-assembly?))
       (identical? original-operation-summary
                   (var-get #'application/operation-summary))))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- closed-map? [m required]
  (and (plain-map? m) (= required (set (keys m)))))

(defn- reject! []
  (throw (ex-info "Selected authorization assembly source binding rejected."
                  {:checked-authorization-assembly/rejection true
                   :reason :authorization-not-bound-to-original-assembly})))

(defn check-selected-binding!
  "Check one operation and route against the caller's original source maps.
   The result describes a non-authoritative relation only. This intentionally
   supports additional unrelated application operations and distinct route
   IDs, while rejecting a second valid handler issued for another executor."
  [{:keys [contract-source catalog-source operation route-id] :as selection}]
  (when-not (and (closed-map? selection
                             #{:contract-source :catalog-source :operation :route-id})
                 (qualified-keyword? operation)
                 (qualified-keyword? route-id)
                 (closed-map? contract-source
                              #{:application-assembly :semantic-route-handlers
                                :certified-operations})
                 (some? (:application-assembly contract-source))
                 (plain-map? (:semantic-route-handlers contract-source))
                 (plain-map? (:certified-operations contract-source))
                 (closed-map? catalog-source #{:catalog :physical-field-map})
                 (plain-map? (:catalog catalog-source))
                 (vector? (get-in catalog-source [:catalog :operations]))
                 (identical? original-issued-binding
                             (var-get #'certified/issued-handler-binding?)))
    (reject!))
  (let [declarations (filterv #(= operation (:id %))
                              (get-in catalog-source [:catalog :operations]))
        binding (get-in contract-source [:certified-operations operation])
        handler (get-in contract-source [:semantic-route-handlers route-id])
        original-source (:current-source binding)
        original-construction (:construction original-source)
        selected-construction (:construction (first declarations))]
    (when-not (and (= 1 (count declarations))
                   (map? selected-construction)
                   (= operation (get-in selected-construction [:subject :id]))
                   (closed-map? binding #{:executor :current-source})
                   (= selected-construction original-construction)
                   (= operation (get-in original-source
                                        [:certified-source :operation]))
                   (original-issued-binding handler (:executor binding)
                                            operation original-source))
      (reject!))
    {:type report-type
     :version report-version
     :operation operation
     :route-id route-id
     :checking :selected-original-construction-to-exact-issued-handler
     :application-assembly-closure :not-established
     :installed-handler-adoption :not-established
     :authorization-policy-provenance :not-established
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(def ^:private original-selected-binding check-selected-binding!)

;; v1792: This is an opt-in *catalog-scoped* assembly coverage check. The
;; catalog is the declared selection, NOT a proof that every business action
;; in a deployment belongs to it. The original certified-contract checker
;; establishes route-table closure for its supplied current assembly.
(def coverage-report-type
  :gesso.assurance.proof.checked-authorization-assembly/catalog-route-coverage)

(defn check-declared-route-coverage!
  "Recheck the ORIGINAL certified-contract closure ONCE, then verify every
   route of every certified operation in the selected catalog against its
   original construction and exact issued handler. Refuse both an omitted
   certified operation and an unaccounted route. Other operations may remain
   explicitly uncertified; this is not a business-policy coverage proof.

   The supplied assembly and route table must actually be installed by the
   host; this function cannot inspect a deployed router or issue authority."
  [{:keys [contract-source catalog-source] :as selection}]
  (when-not (and (closed-map? selection #{:contract-source :catalog-source})
                 (closed-map? contract-source
                              #{:application-assembly :semantic-route-handlers
                                :certified-operations})
                 (closed-map? catalog-source #{:catalog :physical-field-map})
                 (plain-map? (:catalog catalog-source))
                 (vector? (get-in catalog-source [:catalog :operations]))
                 (application-boundary-current?)
                 (plan-proof-boundary-current?)
                 (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-issued-binding
                             (var-get #'certified/issued-handler-binding?))
                 (identical? original-selected-binding
                             (var-get #'check-selected-binding!)))
    (reject!))
  (let [entries (get-in catalog-source [:catalog :operations])
        operation-ids (mapv :id entries)
        selected (set operation-ids)]
    (when-not (and (seq operation-ids)
                   (= (count operation-ids) (count selected))
                   (every? qualified-keyword? operation-ids)
                   (= selected (set (keys (:certified-operations contract-source)))))
      (reject!))
    ;; Do not accept a copied report supplied by the caller. This full check
    ;; validates the present ApplicationAssembly, all its semantic routes,
    ;; every certified plan source and the exact registered Ring handlers.
    (let [checked (try (original-contract-check contract-source)
                       (catch Exception _ (reject!)))
          certified-rows (:certified-operations checked)
          route-ids (into #{} (mapcat :route-ids (vals certified-rows)))]
      (when-not (and (= certified/report-type (:type checked))
                     (= certified/adapter-version (:version checked))
                     (= selected (set (keys certified-rows)))
                     (= route-ids (:matched-certified-route-ids checked))
                     (every? (fn [operation]
                               (seq (get-in certified-rows [operation :route-ids])))
                             operation-ids))
        (reject!))
      (doseq [operation operation-ids
              route-id (get-in certified-rows [operation :route-ids])]
        ;; Keep this relation source-relative: no per-route proof or
        ;; transaction capability is produced or cached for later requests.
        (original-selected-binding
         {:contract-source contract-source
          :catalog-source catalog-source
          :operation operation
          :route-id route-id}))
      ;; A trusted callback encountered during original contract validation
      ;; must not be allowed to replace a proof boundary before a successful
      ;; catalog report is returned. This is a point-in-time observation only.
      (when-not (and (application-boundary-current?)
                     (plan-proof-boundary-current?)
                     (identical? original-contract-check
                                 (var-get #'certified/check-certified-contracts!))
                     (identical? original-issued-binding
                                 (var-get #'certified/issued-handler-binding?))
                     (identical? original-selected-binding
                                 (var-get #'check-selected-binding!)))
        (reject!))
      {:type coverage-report-type
       :version report-version
       :checking :all-declared-catalog-operations-and-issued-routes
       :catalog-operations selected
       :matched-certified-route-ids route-ids
       :uncertified-application-operations (:uncertified-operations checked)
       :catalog-scope :selected-not-all-possible-application-policy
       :current-assembly-closure :source-relative-checked
       :installed-handler-adoption :not-established
       :authorization-policy-provenance :not-established
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))
