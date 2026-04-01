(ns gesso.components.page
  (:require
   [clojure.string :as str]
   [gesso.util :refer :all]))

(def ^:private default-layouts
  {:focused
   {:default {:areas [[:left :main :right]]
              :columns ["minmax(0,1fr)" "minmax(0,72rem)" "minmax(0,1fr)"]
              :show [:left :main :right]}
    :md      {:areas [[:left :main :right]]
              :columns ["minmax(0,1fr)" "minmax(0,72rem)" "minmax(0,1fr)"]
              :show [:left :main :right]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}

   :wide-focused
   {:default {:areas [[:left :main :right]]
              :columns ["minmax(0,1fr)" "minmax(0,88rem)" "minmax(0,1fr)"]
              :show [:left :main :right]}
    :md      {:areas [[:left :main :right]]
              :columns ["minmax(0,1fr)" "minmax(0,88rem)" "minmax(0,1fr)"]
              :show [:left :main :right]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}

   :sidebar-main
   {:default {:areas [[:left :main]]
              :columns ["minmax(14rem,20rem)" "minmax(0,1fr)"]
              :show [:left :main]}
    :md      {:areas [[:left :main]]
              :columns ["minmax(14rem,20rem)" "minmax(0,1fr)"]
              :show [:left :main]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}

   :main-rail
   {:default {:areas [[:main :right]]
              :columns ["minmax(0,1fr)" "minmax(16rem,22rem)"]
              :show [:main :right]}
    :md      {:areas [[:main :right]]
              :columns ["minmax(0,1fr)" "minmax(16rem,22rem)"]
              :show [:main :right]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}

   :three-column
   {:default {:areas [[:left :main :right]]
              :columns ["minmax(14rem,18rem)" "minmax(0,1fr)" "minmax(16rem,22rem)"]
              :show [:left :main :right]}
    :md      {:areas [[:left :main]]
              :columns ["minmax(14rem,18rem)" "minmax(0,1fr)"]
              :show [:left :main]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}

   :full
   {:default {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}
    :md      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}
    :sm      {:areas [[:main]]
              :columns ["minmax(0,1fr)"]
              :show [:main]}}})

(def ^:private fallback-layout
  {:default {:areas [[:main]]
             :columns ["minmax(0,1fr)"]
             :show [:main]}
   :md      {:areas [[:main]]
             :columns ["minmax(0,1fr)"]
             :show [:main]}
   :sm      {:areas [[:main]]
             :columns ["minmax(0,1fr)"]
             :show [:main]}})

(defn- normalize-region
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- normalize-show
  [xs]
  (some->> xs (map normalize-region) vec))

(defn- normalize-areas
  [areas]
  (cond
    (nil? areas) nil
    (string? areas) areas
    :else
    (mapv (fn [row]
            (mapv normalize-region row))
          areas)))

(defn- derive-show-from-areas
  [areas]
  (when (and areas (not (string? areas)))
    (->> areas
         flatten
         (remove nil?)
         (remove #{:.})
         distinct
         vec)))

(defn- normalize-columns
  [areas columns]
  (cond
    (string? columns) columns
    (sequential? columns) (vec columns)
    (and areas (not (string? areas)) (seq areas))
    (vec (repeat (count (first areas)) "minmax(0,1fr)"))
    :else ["minmax(0,1fr)"]))

(defn- stacked-areas
  [regions]
  (mapv vector regions))

(defn- normalize-tier-spec
  [spec]
  (let [show0    (normalize-show (:show spec))
        areas0   (normalize-areas (:areas spec))
        areas    (or areas0
                     (when (seq show0)
                       (stacked-areas show0))
                     [[:main]])
        show     (or show0
                     (derive-show-from-areas areas)
                     [:main])
        columns  (normalize-columns areas (:columns spec))]
    {:areas areas
     :columns columns
     :show show}))

(defn- normalize-layout-map
  [layout]
  (let [default-spec (normalize-tier-spec
                      (or (:default layout)
                          (:md layout)
                          (:sm layout)
                          (:default fallback-layout)))
        md-spec      (normalize-tier-spec (merge default-spec (:md layout)))
        sm-spec      (normalize-tier-spec (merge md-spec (:sm layout)))]
    {:default default-spec
     :md md-spec
     :sm sm-spec}))

(defn- region-key-from-attrs
  [attrs]
  (cond
    (:data-page-left attrs) :left
    (:data-page-main attrs) :main
    (:data-page-right attrs) :right
    :else nil))

(defn- region-key-from-node
  [node]
  (when (vector? node)
    (let [[_ maybe-attrs] node]
      (when (map? maybe-attrs)
        (region-key-from-attrs maybe-attrs)))))

(defn- present-regions
  [nodes]
  (->> nodes
       (keep region-key-from-node)
       distinct
       vec))

(defn- visible-regions
  [present-regions {:keys [keep drop]}]
  (let [present-set (set present-regions)
        keep*       (normalize-show keep)
        drop*       (set (normalize-show drop))]
    (if (seq keep*)
      (->> keep*
           (filter present-set)
           vec)
      (->> present-regions
           (remove drop*)
           vec))))

(defn- tier-from-collapse-policy
  [present-regions tier-policy]
  (let [visible (visible-regions present-regions tier-policy)
        order   (or (normalize-show (:stack-order tier-policy))
                    visible)
        ordered (->> order
                     (filter (set visible))
                     distinct
                     vec)
        ordered (if (seq ordered) ordered visible)]
    {:areas (stacked-areas ordered)
     :columns ["minmax(0,1fr)"]
     :show ordered}))

(defn- apply-collapse-policy
  [layout present-regions collapse-policy]
  (if (seq collapse-policy)
    (reduce-kv
     (fn [m bp tier-policy]
       (assoc m bp
              (merge (get m bp)
                     (tier-from-collapse-policy present-regions tier-policy))))
     layout
     collapse-policy)
    layout))

(defn- resolve-layout
  [variant layout collapse-policy present-regions]
  (let [base    (or (when variant (get default-layouts variant))
                    fallback-layout)
        merged  (merge-with merge base (or layout {}))
        layout' (apply-collapse-policy merged present-regions collapse-policy)]
    (normalize-layout-map layout')))

(defn- columns->template
  [columns]
  (cond
    (nil? columns) nil
    (string? columns) columns
    :else (str/join " " (map str columns))))

(defn- areas->template
  [areas]
  (cond
    (nil? areas) nil
    (string? areas) areas
    :else
    (str/join " "
              (map (fn [row]
                     (str "\""
                          (str/join " "
                                    (map (fn [cell]
                                           (if (= cell :.)
                                             "."
                                             (name cell)))
                                         row))
                          "\""))
                   areas))))

(defn- layout-style-vars
  [layout]
  {"--page-columns-default" (columns->template (get-in layout [:default :columns]))
   "--page-areas-default"   (areas->template (get-in layout [:default :areas]))
   "--page-columns-md"      (columns->template (get-in layout [:md :columns]))
   "--page-areas-md"        (areas->template (get-in layout [:md :areas]))
   "--page-columns-sm"      (columns->template (get-in layout [:sm :columns]))
   "--page-areas-sm"        (areas->template (get-in layout [:sm :areas]))})

(defn- region-visible?
  [layout tier region]
  (contains? (set (get-in layout [tier :show] [])) region))

(defn- apply-region-layout
  [node layout]
  (if (vector? node)
    (let [[tag maybe-attrs & children] node
          has-attrs? (map? maybe-attrs)
          attrs      (if has-attrs? maybe-attrs {})
          kids       (if has-attrs? children (cons maybe-attrs children))
          region     (region-key-from-attrs attrs)]
      (if region
        (into
         [tag
          (assoc attrs
                 :data-page-default-visible (if (region-visible? layout :default region) "true" "false")
                 :data-page-md-visible (if (region-visible? layout :md region) "true" "false")
                 :data-page-sm-visible (if (region-visible? layout :sm region) "true" "false"))]
         kids)
        node))
    node))

(defn page-left
  "Left page region."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :data-page-left true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :data-page-left true}
          attrs
          children))))

(defn page-main
  "Main page region."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :main
          {:class (class-names "section-theme min-w-0" class)
           :data-page-main true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :main
          {:class (class-names "section-theme min-w-0" class)
           :data-page-main true}
          attrs
          children))))

(defn page-right
  "Right page region."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :data-page-right true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :data-page-right true}
          attrs
          children))))

(defn page-surface
  "Continuous page surface inside a page region."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          {:class (class-names "panel-theme pad-card radius-lg shadow-sm" class)
           :data-page-surface true
           :style {:border (str "var(--border-width, 1px) solid var(--border)")
                   :background "var(--card)"
                   :color "var(--card-foreground)"}}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          {:class (class-names "panel-theme pad-card radius-lg shadow-sm" class)
           :data-page-surface true
           :style {:border (str "var(--border-width, 1px) solid var(--border)")
                   :background "var(--card)"
                   :color "var(--card-foreground)"}}
          attrs
          children))))

(defn page
  "Grid-based page layout with built-in variants and optional explicit layout.

  Standard usage:
    (page {:variant :three-column} ...)

  Advanced usage:
    (page {:layout
           {:default {:areas [[:left :main :right]]
                      :columns [\"16rem\" \"minmax(0,1fr)\" \"20rem\"]
                      :show [:left :main :right]}
            :md      {:areas [[:left :main]]
                      :columns [\"16rem\" \"minmax(0,1fr)\"]
                      :show [:left :main]}
            :sm      {:areas [[:main]]
                      :columns [\"minmax(0,1fr)\"]
                      :show [:main]}}}
      ...)

  collapse-policy remains as a convenience override for md/sm tiers:
    {:md {:keep [:left :main]
          :stack-order [:left :main]}
     :sm {:keep [:main]
          :drop [:left]
          :stack-order [:main]}}"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [variant layout collapse-policy left main right children]} props
          raw-children   (->> [(when left  (apply page-left {} (nodes left)))
                               (when main  (apply page-main {} (nodes main)))
                               (when right (apply page-right {} (nodes right)))]
                              (concat (nodes children))
                              (remove nil?))
          regions        (present-regions raw-children)
          resolved       (resolve-layout variant layout collapse-policy regions)
          children*      (map #(apply-region-layout % resolved) raw-children)
          merged-style   (merge (layout-style-vars resolved)
                                (:style attrs))
          attrs'         (cond-> (dissoc attrs :style)
                           (seq merged-style) (assoc :style merged-style))]
      (el :div
          {:class (class-names "w-full" class)
           :data-page true
           :data-page-variant (name (or variant :custom))}
          attrs'
          children*))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [variant layout collapse-policy]} props
          regions      (present-regions children)
          resolved     (resolve-layout variant layout collapse-policy regions)
          children*    (map #(apply-region-layout % resolved) children)
          merged-style (merge (layout-style-vars resolved)
                              (:style attrs))
          attrs'       (cond-> (dissoc attrs :style)
                         (seq merged-style) (assoc :style merged-style))]
      (el :div
          {:class (class-names "w-full" class)
           :data-page true
           :data-page-variant (name (or variant :custom))}
          attrs'
          children*))))
