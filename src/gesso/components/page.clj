(ns gesso.components.page
  (:require
   [clojure.set :as set]
   [gesso.util :refer :all]))

(def ^:private default-collapse-policies
  {:focused
   {:md {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}}

   :wide-focused
   {:md {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}}

   :sidebar-main
   {:md {:keep [:left :main]
         :drop [:right]
         :stack-order [:left :main :right]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}}

   :main-rail
   {:md {:keep [:main :right]
         :drop [:left]
         :stack-order [:main :right :left]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :right :left]}}

   :three-column
   {:md {:keep [:left :main]
         :drop [:right]
         :stack-order [:left :main :right]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}}

   :full
   {:md {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}
    :sm {:keep [:main]
         :drop [:left :right]
         :stack-order [:main :left :right]}}})

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

(defn- normalize-breakpoint-policy
  [present-regions {:keys [keep drop stack-order]}]
  (let [present-set (set present-regions)
        keep-set    (if (seq keep)
                      (set/intersection present-set (set keep))
                      (set/difference present-set (set drop)))
        order       (vec (concat
                          (filter present-set (or stack-order []))
                          (remove (set (or stack-order [])) present-regions)))
        order-map   (zipmap order (range 1 (inc (count order))))]
    {:visible? (fn [region] (contains? keep-set region))
     :order    (fn [region] (get order-map region 999))}))

(defn- resolved-collapse-policy
  [variant present-regions collapse-policy]
  (let [base   (get default-collapse-policies (or variant :focused)
                    (get default-collapse-policies :focused))
        merged (merge-with merge base collapse-policy)]
    {:md (normalize-breakpoint-policy present-regions (:md merged))
     :sm (normalize-breakpoint-policy present-regions (:sm merged))}))

(defn- apply-region-policy
  [node policy]
  (if (vector? node)
    (let [[tag maybe-attrs & children] node
          has-attrs? (map? maybe-attrs)
          attrs      (if has-attrs? maybe-attrs {})
          kids       (if has-attrs? children (cons maybe-attrs children))
          region     (region-key-from-attrs attrs)]
      (if region
        (let [md (:md policy)
              sm (:sm policy)
              merged-style (merge
                            (:style attrs)
                            {"--page-md-order" (str ((:order md) region))
                             "--page-sm-order" (str ((:order sm) region))})]
          (into
           [tag
            (assoc attrs
                   :style merged-style
                   :data-page-md-visible (if ((:visible? md) region) "true" "false")
                   :data-page-sm-visible (if ((:visible? sm) region) "true" "false"))]
           kids))
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
  "Grid-based page layout with named configurations and explicit left/main/right
  regions.

  Supported variants:
    :focused
    :wide-focused
    :sidebar-main
    :main-rail
    :three-column
    :full

  Optional collapse policy example:
    {:collapse-policy
     {:md {:keep [:left :main]
           :drop [:right]
           :stack-order [:left :main :right]}
      :sm {:keep [:main]
           :drop [:left :right]
           :stack-order [:main :left :right]}}}

  Notes:
    - desktop layout is driven by the page variant
    - collapse behavior is driven by CSS plus the optional collapse policy
    - users can still override layout behavior with their own CSS"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [variant collapse-policy left main right children]} props
          raw-children (->> [(when left  (apply page-left {} (nodes left)))
                             (when main  (apply page-main {} (nodes main)))
                             (when right (apply page-right {} (nodes right)))]
                            (concat (nodes children))
                            (remove nil?))
          present-regions (->> raw-children
                               (keep region-key-from-node)
                               vec)
          policy (resolved-collapse-policy variant present-regions collapse-policy)
          children* (map #(apply-region-policy % policy) raw-children)]
      (el :div
          {:class (class-names "w-full" class)
           :data-page true
           :data-page-variant (name (or variant :focused))}
          attrs
          children*))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [variant collapse-policy]} props
          present-regions (->> children
                               (keep region-key-from-node)
                               vec)
          policy (resolved-collapse-policy variant present-regions collapse-policy)
          children* (map #(apply-region-policy % policy) children)]
      (el :div
          {:class (class-names "w-full" class)
           :data-page true
           :data-page-variant (name (or variant :focused))}
          attrs
          children*))))
