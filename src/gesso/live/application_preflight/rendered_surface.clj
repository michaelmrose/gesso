(ns gesso.live.application-preflight.rendered-surface
  "Rendered Hiccup enumeration and semantic affordance checking.

   This module owns the shared physical-surface inspection pipeline used by
   static ApplicationAssembly snapshots and actual dynamic pre-browser checks.
   It does not authenticate application assemblies, verify browser artifacts,
   serialize responses, or grant client metadata authority. Those boundaries
   remain with application-preflight and its trusted dependencies.

   The resulting analysis is evidence relative to the supplied operation
   summary and supplied rendered surfaces only. It does not establish that
   all possible Clojure render states were enumerated."
  (:require
   [gesso.live.application-preflight.affordance :as affordance]
   [gesso.live.application-preflight.rendered-request :as rendered-request]
   [gesso.live.ui :as ui]))

(defn- issue
  [kind message data]
  (merge {:kind kind :message message} data))

(defn scan-rendered-surfaces
  "Enumerate canonical affordances, physical request coordinates, and scan issues
   across supplied named Hiccup surfaces in stable sorted order. Nil means no
   static surfaces were supplied; it is not evidence of completeness.

   A failed Choreo metadata scan does not suppress the physical request or
   ambiguous-attribute findings on that surface."
  [rendered-surfaces]
  (if (nil? rendered-surfaces)
    {:affordances []
     :request-coordinates []
     :errors []}
    (reduce
     (fn [{:keys [affordances request-coordinates errors]} surface-name]
       (let [rendered
             (get rendered-surfaces surface-name)

             surface-request-coordinates
             (into []
                   (map #(assoc % :surface surface-name))
                   (rendered-request/request-coordinates rendered))
             surface-attribute-errors
             (rendered-request/attribute-conflicts rendered surface-name)]
         (try
           {:affordances
            (into affordances
                  (map #(assoc % :surface surface-name))
                  (ui/rendered-choreo-affordances rendered))
            :request-coordinates
            (into request-coordinates surface-request-coordinates)
            :errors (into errors surface-attribute-errors)}
           (catch clojure.lang.ExceptionInfo error
             {:affordances affordances
              :request-coordinates
              (into request-coordinates surface-request-coordinates)
              :errors
              (conj
               (into errors surface-attribute-errors)
               (issue
                :rendered-affordance-scan-failed
                "Application preflight could not enumerate canonical Choreo affordances from a supplied rendered surface."
                {:surface surface-name
                 :cause-type (:error/type (ex-data error))
                 :cause-kind (:error/kind (ex-data error))
                 :cause-data (dissoc (ex-data error) :error/type :error/kind)}))})
           (catch Throwable error
             {:affordances affordances
              :request-coordinates
              (into request-coordinates surface-request-coordinates)
              :errors
              (conj
               (into errors surface-attribute-errors)
               (issue
                :rendered-affordance-scan-failed
                "Application preflight failed while enumerating a supplied rendered surface."
                {:surface surface-name
                 :exception-class (str (class error))
                 :exception-message (.getMessage error)}))}))))
     {:affordances []
      :request-coordinates []
      :errors []}
     (sort-by pr-str (keys rendered-surfaces)))))


(defn analyze-rendered-surfaces
  "Derive canonical affordances and semantic request errors from *one* source
   operation summary and zero or more supplied named rendered Hiccup values.

   The result exposes the original scanner result and ordered intermediate
   errors so callers can retain existing report shapes, error ordering, and
   affordance-closure reporting without re-implementing correspondence.

   This function is pure with respect to the application assembly; it does
   not establish that the operation summary is current or authoritative.
   That must be checked by the assembly boundary before this result is used."
  [operation-summary rendered-surfaces]
  (let [scan (scan-rendered-surfaces rendered-surfaces)
        semantic-route-errors
        (affordance/semantic-route-identity-errors
         operation-summary
         (:request-coordinates scan)
         (:affordances scan))
        scan-errors
        (affordance/enrich-render-scan-errors-with-semantic-routes
         operation-summary
         (:request-coordinates scan)
         (:errors scan))
        resolution
        (affordance/resolve-rendered-affordances
         operation-summary
         (:affordances scan))
        resolution-errors (:errors resolution)]
    {:scan scan
     :affordances (vec (:affordances resolution))
     :scan-errors (vec scan-errors)
     :semantic-route-errors (vec semantic-route-errors)
     :resolution-errors (vec resolution-errors)
     :errors (vec (concat scan-errors
                          semantic-route-errors
                          resolution-errors))}))
