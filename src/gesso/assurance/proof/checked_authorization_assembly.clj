(ns gesso.assurance.proof.checked-authorization-assembly
  "v1791: Reusable operation/route-scoped ORIGINAL source-to-handler relation.

   For any selected qualified operation and route ID, relate its declared
   catalog construction to the SAME current-source construction of an issued
   certified-plan executor, and identify the EXACT issued Ring handler bound
   to that executor, operation and source. Other application operations and
   routes are allowed. This does not install or certify those routes.

   IMPORTANT: This is a conditional SOURCE-RELATIVE binding, not a checker
   for current ApplicationAssembly closure, original catalog admission,
   authentication, authorization-policy provenance, or physical deployment.
   Its caller MUST separately perform those full original checks before
   startup/adoption. A report is never a certificate or transaction authority."
  (:require [gesso.live.application-preflight.certified-contract :as certified]))

(def report-type
  :gesso.assurance.proof.checked-authorization-assembly/selected-binding)
(def report-version 1)

(def ^:private original-issued-binding certified/issued-handler-binding?)

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
