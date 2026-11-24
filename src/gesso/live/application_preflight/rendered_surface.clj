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
   [clojure.string :as str]
   [gesso.live.acquisition :as acquisition]
   [rum.server-render :as rum]
   [gesso.live.application-preflight.affordance :as affordance]
   [gesso.live.application-preflight.rendered-request :as rendered-request]
   [gesso.live.ui :as ui]))

(defn- issue
  [kind message data]
  (merge {:kind kind :message message} data))

(defn- rendered-elements
  "Retain element ancestry and Hiccup paths through transparent sequences
   and fragment nodes. This is inspection of supplied data, not HTML parsing."
  [rendered]
  (letfn [(walk [node path ancestors]
            (lazy-seq
             (when (and (sequential? node)
                        (not (and (vector? node) (= :rum/nothing (first node)))))
               (let [tag (when (vector? node) (first node))
                     element? (and (or (keyword? tag) (string? tag) (symbol? tag))
                                   (not (#{"<>" "*"} (name tag))))
                     attrs (when (and element? (map? (second node)))
                             (->> (second node)
                                  (remove (comp nil? val))
                                  (sort-by (comp pr-str key))
                                  (mapv (fn [[k v]]
                                          [(str/lower-case (rum/normalize-attr-key k)) v]))))
                     record {:path path :attrs attrs}
                     children (if element?
                                (drop (if (map? (second node)) 2 1)
                                      (map-indexed vector node))
                                (map-indexed vector node))]
                 (concat (when element? [(assoc record :ancestors ancestors)])
                         (mapcat (fn [[i child]]
                                   (walk child (conj path i)
                                         (cond-> ancestors element? (conj record))))
                                 children))))))]
    (vec (walk rendered [] []))))

(defn- attribute-values [element attribute]
  (map second (filter #(= attribute (first %)) (:attrs element))))

(defn- managed-owner-id [element]
  (when (some #(= "gesso:live-refresh" (str/trim (str %)))
              (attribute-values element "hx-trigger"))
    (some #(when-not (str/blank? (str %)) (str %))
          (attribute-values element "data-gesso-live-fragment"))))

(defn- acquisition-errors
  "Apply the portable acquisition vocabulary to actual managed markup,
   including caller-supplied ancestors and descendants outside descriptors.
   Legacy/unmanaged nodes outside an owner are not managed acquisitions."
  [rendered surface]
  (let [elements (rendered-elements rendered)]
    (vec
     (mapcat
      (fn [{:keys [ancestors path attrs] :as element}]
        (let [id (managed-owner-id element)
              owner (if id element (last (filter managed-owner-id ancestors)))
              owner-id (managed-owner-id owner)
              direct-listener? (and owner (not id)
                                    (= (:path owner) (:path (peek ancestors)))
                                    (= [owner-id] (vec (attribute-values element "data-gesso-live-invalidation"))))
              diagnostic (fn [reason at attribute value]
                           (issue :rendered-managed-acquisition-config
                                  "Rendered managed acquisition violates the restricted acquisition contract."
                                  {:surface surface :render-path (:path at)
                                   :owner-render-path (:path owner)
                                   :fragment-id owner-id :reason reason
                                   :attribute attribute :value value}))]
          (when owner
            (concat
             ;; A root must declare a supported replacement; the ancestor
             ;; chain cannot supply additional selection or history effects.
             (when id
               (let [specs (concat (attribute-values element "hx-swap")
                                   (attribute-values element "data-hx-swap"))]
                 (concat
                  (when (empty? specs)
                    [(diagnostic :unsupported-swap element "hx-swap" nil)])
                  (for [spec specs :when (not (acquisition/replacement-spec? spec))]
                    (diagnostic :unsupported-swap element "hx-swap" spec))
                  (for [at (conj ancestors element)
                        [attribute value] (:attrs at)
                        :let [reason (cond
                                       (#{"hx-select" "data-hx-select" "hx-select-oob" "data-hx-select-oob"} attribute)
                                       :unsupported-selection
                                       (and (#{"hx-push-url" "data-hx-push-url" "hx-replace-url" "data-hx-replace-url"} attribute)
                                            (not (#{false "false"} value)))
                                       :unsupported-history
                                       (and (#{"hx-ext" "data-hx-ext"} attribute)
                                            (not (acquisition/extension-declaration? value)))
                                       :unsupported-extension)]
                        :when reason]
                    (diagnostic reason at attribute value)))))
             (for [[attribute value] attrs
                   :let [reason (cond
                                  (and (not id) (#{"hx-ext" "data-hx-ext"} attribute)
                                       (not (acquisition/extension-declaration? value)))
                                  :unsupported-extension
                                  (and (acquisition/unadmitted-sse-affordance? attribute value)
                                       (not (and id (= "sse-connect" attribute)))
                                       (not (and direct-listener?
                                                 (#{"sse-swap" "data-sse-swap" "data-gesso-live-invalidation"} attribute))))
                                  :unsupported-sse-affordance)]
                   :when reason]
               (diagnostic reason element attribute value))))))
      elements))))

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
             (into (rendered-request/attribute-conflicts rendered surface-name)
                   (acquisition-errors rendered surface-name))]
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
