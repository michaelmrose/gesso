(ns gesso.live.application-preflight.affordance
  "Pure rendered Choreo affordance/physical-route correspondence checks.

   Internal component of application-preflight: consumes the current
   operation summary, enumerated rendered affordances, and physical request
   coordinates already derived by the caller. It never parses Hiccup, trusts
   client metadata as authority, or validates the application assembly itself.

   All checks retain application-preflight's issue vocabulary and precise
   surface/render-path diagnostics. Route template matching intentionally
   recognizes only the existing simple :parameter segment vocabulary; it
   does not attempt to model arbitrary Ring/Reitit routing semantics.

   Stage-one extraction: application-preflight still owns its current copies
   and callers; v730 will delegate to this module and remove that duplication."
  (:require
   [clojure.string :as str]))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- path-only
  [path]
  (first (str/split path #"[?#]" 2)))

(defn- route-template-segments
  [path]
  (str/split (path-only path) #"/" -1))

(defn- route-placeholder-segment?
  [segment]
  (and
   (str/starts-with? segment ":")
   (> (count segment) 1)))

(defn- route-template-matches?
  "Match one concrete rendered affordance path against the small physical route
   template vocabulary already used by current Biff/Reitit applications.

   A full `:name` path segment is treated as one non-empty concrete segment;
   every other segment is literal. Query/fragment suffixes on the rendered URL
   are ignored for route selection. Gesso does not infer optional segments,
   wildcards, regexes, or application-specific coercion here."
  [template concrete]
  (let [template-segments (route-template-segments template)
        concrete-segments (route-template-segments concrete)]
    (and
     (= (count template-segments) (count concrete-segments))
     (every?
      true?
      (map
       (fn [template-segment concrete-segment]
         (if (route-placeholder-segment? template-segment)
           (not (str/blank? concrete-segment))
           (= template-segment concrete-segment)))
       template-segments
       concrete-segments)))))

(defn semantic-route-matches
  [operation-summary' {:keys [method path]}]
  (vec
   (for [[operation operation-entry] operation-summary'
         route (:routes operation-entry)
         :when (and (= method (:method route))
                    (route-template-matches? (:path route) path))]
     {:operation operation
      :route-id (:route-id route)
      :method (:method route)
      :route-template (:path route)
      :required-transport (:required-transport route)})))

(defn semantic-route-identity-errors
  "Reject anonymous requests to semantic routes AND physical requests that do
   not correspond to the actual canonical Choreo affordance on their node.

   Metadata marks the node where a validated :choreo/op was constructed; it
   does not confer a blanket exemption on every HTMX or native request on that
   node. Only the exact hx-post method/path for the rendered canonical
   affordance may use that mark. Otherwise, adding hx-get/hx-put/etc. to a
   canonical button would allow a second physical action to bypass semantic
   route inspection without any associated operation declaration.

   For unmarked requests, preserve the existing route-sensitive rule: ordinary
   HTML requests are legal unless their method and URL match an assembled
   semantic operation. Invalid or forged metadata is independently rejected by
   the rendered-affordance scanner; no metadata means no exemption."
  [operation-summary' request-coordinates affordances]
  (let [canonical-by-location
        (group-by (juxt :surface :render-path) affordances)]
    (vec
     (keep
      (fn [{:keys [surface render-path method path request-source
                   choreo-operation-declared?]
            :as coordinate}]
        (let [matches (semantic-route-matches operation-summary' coordinate)]
          (if choreo-operation-declared?
            (let [expected (get canonical-by-location [surface render-path])]
              (when-not (and (= 1 (count expected))
                             (= :hx-post request-source)
                             (= method (:method (first expected)))
                             (= path (:path (first expected))))
                (issue
                 :rendered-choreo-physical-request-mismatch
                 "A node marked as a canonical Choreo affordance emits a physical request that is not its certified hx-post action. Choreo metadata cannot exempt additional or altered requests from application preflight."
                 (merge
                  {:surface surface
                   :render-path render-path
                   :method method
                   :path path
                   :request-source request-source
                   :canonical-affordances (vec expected)
                   :matching-routes matches}
                  (select-keys coordinate
                               [:form-render-path :submitter-overrides])))))
            (when (seq matches)
              (issue
               :rendered-semantic-route-without-choreo-operation
               "Rendered request targets an assembled semantic operation route but carries no canonical :choreo/op declaration. Semantic operation identity may not silently degrade to an ordinary physical request."
               (merge
                {:surface surface
                 :render-path render-path
                 :method method
                 :path path
                 :request-source request-source
                 :candidate-operations
                 (set (map :operation matches))
                 :matching-routes matches}
                (select-keys coordinate
                             [:form-render-path :submitter-overrides])))))))
      request-coordinates))))

(defn enrich-render-scan-errors-with-semantic-routes
  "Attach physical semantic-route context to rendered-affordance scanner errors.

   Malformed or forged framework metadata already fails closed in
   ui/rendered-choreo-affordances, but the scanner-local cause alone can obscure
   an important application fact: the malformed node may also target a route
   already assembled as a semantic operation.  Preserve the scanner's original
   error kind/cause while attaching every matching semantic route on that same
   rendered surface.

   This is diagnostic enrichment only.  It does not reinterpret malformed
   metadata as an anonymous affordance and therefore does not duplicate or
   weaken semantic-route-identity-errors."
  [operation-summary' request-coordinates scan-errors]
  (mapv
   (fn [scan-error]
     (let [surface (:surface scan-error)
           collisions
           (->> request-coordinates
                (filter #(= surface (:surface %)))
                (mapcat
                 (fn [{:keys [method path render-path] :as coordinate}]
                   (map
                    (fn [match]
                      (merge
                       {:surface surface
                        :render-path render-path
                        :rendered-method method
                        :rendered-path path}
                       match))
                    (semantic-route-matches operation-summary' coordinate))))
                (sort-by (juxt :render-path :rendered-path :operation :route-id))
                vec)]
       (cond-> scan-error
         (seq collisions)
         (assoc :matching-semantic-routes collisions))))
   scan-errors))

(defn resolve-rendered-affordances
  [operation-summary' affordances]
  (reduce
   (fn [{:keys [affordances errors]} affordance]
     (let [operation (:operation affordance)
           operation-entry (get operation-summary' operation)]
       (cond
         (nil? operation-entry)
         {:affordances (conj affordances affordance)
          :errors
          (conj
           errors
           (issue
            :rendered-affordance-unknown-operation
            "Rendered Choreo affordance references an operation outside the assembled route-exposed application slice."
            {:surface (:surface affordance)
             :render-path (:render-path affordance)
             :operation operation
             :available-operations (set (keys operation-summary'))}))}

         (not= (:plan-key affordance)
               (:browser-plan-key operation-entry))
         {:affordances (conj affordances affordance)
          :errors
          (conj
           errors
           (issue
            :rendered-affordance-plan-mismatch
            "Rendered Choreo affordance plan key does not match the assembled browser plan for its semantic operation."
            {:surface (:surface affordance)
             :render-path (:render-path affordance)
             :operation operation
             :affordance-plan-key (:plan-key affordance)
             :assembled-plan-key (:browser-plan-key operation-entry)}))}

         :else
         (let [declared-routes (:routes operation-entry)
               method-routes
               (filterv #(= (:method affordance) (:method %)) declared-routes)
               matching-routes
               (filterv
                #(route-template-matches? (:path %) (:path affordance))
                method-routes)]
           (cond
             (empty? method-routes)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-method-mismatch
                "Rendered Choreo affordance HTTP method is not realized by a trusted route for its semantic operation."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :declared-routes declared-routes}))}

             (empty? matching-routes)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-path-mismatch
                "Rendered Choreo affordance URL does not match a trusted route template for its semantic operation."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :path (:path affordance)
                 :route-templates (mapv :path method-routes)}))}

             (> (count matching-routes) 1)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :ambiguous-rendered-affordance-route
                "Rendered Choreo affordance matches more than one trusted route realization."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :path (:path affordance)
                 :matching-routes matching-routes}))}

             :else
             (let [route (first matching-routes)]
               {:affordances
                (conj
                 affordances
                 (assoc affordance
                        :route-id (:route-id route)
                        :route-template (:path route)
                        :required-transport (:required-transport route)))
                :errors errors}))))))
   {:affordances []
    :errors []}
   affordances))

(defn- affordances-by-operation
  [affordances]
  (reduce
   (fn [acc affordance]
     (update acc (:operation affordance) (fnil conj []) affordance))
   (sorted-map)
   affordances))

(defn enrich-operation-summary-with-affordances
  [operation-summary' affordances]
  (let [by-operation (affordances-by-operation affordances)]
    (into
     (sorted-map)
     (map
      (fn [[operation summary]]
        [operation
         (assoc summary
                :rendered-affordances
                (vec (get by-operation operation [])))])
      operation-summary'))))

