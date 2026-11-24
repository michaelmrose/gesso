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
                     shorthand-id (when element? (second (rum/parse-selector tag)))
                     record {:path path :tag (when element? (first (rum/parse-selector tag)))
                             :attrs (cond-> (vec attrs)
                                                shorthand-id (conj ["id" shorthand-id]))}
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

(defn- identity-values [element attribute]
  ;; Match Rum's attribute emission for identity values. False is omitted,
  ;; true is an empty attribute, and keywords render without their colon.
  (keep #(cond (false? %) nil (true? %) "" :else (rum/to-str %))
        (attribute-values element attribute)))

(defn- managed-owner-candidate? [element]
  ;; A damaged trigger must not make an explicitly declared owner disappear
  ;; from preflight. Retain legacy roots without either managed marker.
  (or (seq (attribute-values element "data-gesso-live-acquisition-policy"))
      (some #(= "gesso:live-refresh" (str/trim %))
            (concat (identity-values element "hx-trigger")
                    (identity-values element "data-hx-trigger")))))

(defn- managed-owner-id [element]
  (when (managed-owner-candidate? element)
    (some #(when-not (str/blank? %) %)
          (identity-values element "data-gesso-live-fragment"))))

(defn- nonblank-string? [value]
  (and (string? value) (not (str/blank? value))))

(defn- htmx-values [element attribute]
  (vec (concat (attribute-values element attribute)
               (attribute-values element (str "data-" attribute)))))

(def ^:private request-verbs
  ["hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete"])

(defn- extension-tokens [element]
  (mapcat #(str/split (str/replace (str %) " " "") #",")
          (htmx-values element "hx-ext")))

(defn- acquisition-correspondence-errors
  "Check the declared owner against its stable request, connection and control.
   The absent policy attribute retains the browser's automatic compatibility
   default. This is rendered evidence, not proof of network endpoint identity."
  [elements surface]
  (vec
   (mapcat
    (fn [owner]
      (when (managed-owner-candidate? owner)
        (let [id (managed-owner-id owner)
              policies (vec (identity-values owner "data-gesso-live-acquisition-policy"))
              explicit? (= ["explicit-refresh"] policies)
              direct-child? #(= (:path owner) (:path (peek (:ancestors %))))
              associated? (fn [element]
                            (or (= (:path owner) (:path element))
                                (and id (some #{id} (identity-values element "data-gesso-live-refresh")))
                                (= (:path owner)
                                   (:path (last (filter #(seq (identity-values % "data-gesso-live-fragment"))
                                                        (:ancestors element)))))))
              controls (filterv #(and (seq (attribute-values % "data-gesso-live-refresh"))
                                       (associated? %)) elements)
              control (first controls)
              listeners (filterv #(and (direct-child? %)
                                        (seq (attribute-values % "data-gesso-live-invalidation"))) elements)
              diagnostic (fn [reason related]
                           (issue :rendered-managed-acquisition-identity
                                  "Rendered managed acquisition has an invalid request, connection or Refresh control."
                                  {:surface surface :fragment-id id
                                   :render-path (:path owner) :owner-render-path (:path owner)
                                   :reason reason :related-render-paths (mapv :path related)}))]
          (cond-> []
            (nil? id)
            (conj (diagnostic :missing-owner-identity [owner]))

            (not (contains? #{[] ["automatic"] ["explicit-refresh"]} policies))
            (conj (diagnostic :invalid-acquisition-policy [owner]))

            (not (and (= ["gesso:live-refresh"]
                         (mapv str/trim (identity-values owner "hx-trigger")))
                      (empty? (attribute-values owner "data-hx-trigger"))
                      (= 1 (count (htmx-values owner "hx-get")))
                      (nonblank-string? (first (htmx-values owner "hx-get")))
                      (not-any? #(seq (htmx-values owner %)) (rest request-verbs))))
            (conj (diagnostic :invalid-acquisition-request [owner]))

            (not (and (= 1 (count (htmx-values owner "sse-connect")))
                      (nonblank-string? (first (htmx-values owner "sse-connect")))
                      (= 1 (count (htmx-values owner "hx-ext")))
                      (some #{"sse"} (extension-tokens owner))
                      (not-any? #{"ignore:sse"} (extension-tokens owner))
                      (not-any? #(some #{"ignore:sse"} (extension-tokens %)) listeners)))
            (conj (diagnostic :invalid-sse-connection (into [owner] listeners)))

            (if explicit?
              (not (and (= 1 (count controls))
                        (direct-child? control)
                        (= [id] (vec (identity-values control "data-gesso-live-refresh")))
                        (= "button" (str/lower-case (:tag control)))
                        (= ["button"] (vec (identity-values control "type")))
                        (empty? (identity-values control "disabled"))
                        (not-any? #(seq (htmx-values control %))
                                  (conj request-verbs "hx-trigger"))))
              (seq controls))
            (conj (diagnostic :invalid-refresh-control controls))))))
    elements)))

(defn- identity-errors
  "Check the canonical owner's identity and stable sibling structure relative
   to one supplied surface. Count legacy roots too: the browser's logical
   root lookup is not restricted to managed roots. Count shorthand DOM ids
   and explicit ids without pretending to resolve ambiguous HTML attributes."
  [elements surface]
  (let [matching (fn [attribute value]
                   (filterv #(some #{value} (identity-values % attribute)) elements))
        direct-child? (fn [owner child]
                        (= (:path owner) (:path (peek (:ancestors child)))))]
    (vec
     (mapcat
      (fn [owner]
        (when-let [id (managed-owner-id owner)]
          (let [roots (matching "data-gesso-live-fragment" id)
                targets (matching "id" id)
                listeners (matching "data-gesso-live-invalidation" id)
                target (first targets)
                listener (first listeners)
                diagnostic (fn [reason paths]
                             (issue :rendered-managed-acquisition-identity
                                    "Rendered managed acquisition has an invalid owner or target identity."
                                    {:surface surface :fragment-id id
                                     :render-path (:path owner)
                                     :owner-render-path (:path owner)
                                     :reason reason :related-render-paths (mapv :path paths)}))]
            (cond-> []
              (not (and (= [id] (vec (identity-values owner "data-gesso-live-fragment")))
                        (= 1 (count roots))))
              (conj (diagnostic :ambiguous-owner roots))

              (not= [(str "#" id)] (htmx-values owner "hx-target"))
              (conj (diagnostic :noncanonical-target-selector [owner]))

              (not (and (= 1 (count targets))
                        (= [id] (vec (identity-values target "id")))
                        (direct-child? owner target)))
              (conj (diagnostic :invalid-primary-target targets))

              (not (and (= 1 (count listeners))
                        (= [id] (vec (identity-values listener "data-gesso-live-invalidation")))
                        (direct-child? owner listener)
                        (not= (:path target) (:path listener))
                        (= ["none"] (htmx-values listener "hx-swap"))
                        (= 1 (count (htmx-values listener "sse-swap")))
                        (nonblank-string? (first (htmx-values listener "sse-swap")))
                        (not-any? #(seq (htmx-values listener %))
                                  ["hx-get" "hx-post" "hx-put" "hx-patch" "hx-delete"])))
              (conj (diagnostic :invalid-invalidation-listener listeners))))))
      elements))))

(defn- acquisition-errors
  "Apply the portable acquisition vocabulary to actual managed markup,
   including caller-supplied ancestors and descendants outside descriptors.
   Legacy/unmanaged nodes outside an owner are not managed acquisitions."
  [rendered surface]
  (let [elements (rendered-elements rendered)]
    (into (into (identity-errors elements surface)
                (acquisition-correspondence-errors elements surface))
     (mapcat
      (fn [{:keys [ancestors path attrs] :as element}]
        (let [id (managed-owner-id element)
              owner (if id element (last (filter managed-owner-id ancestors)))
              owner-id (managed-owner-id owner)
              direct-listener? (and owner (not id)
                                    (= (:path owner) (:path (peek ancestors)))
                                    (= [owner-id] (vec (identity-values element "data-gesso-live-invalidation"))))
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
