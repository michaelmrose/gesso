(ns gesso.components.page
  (:require [gesso.util :refer :all]))

(defn- layout-style
  [variant]
  (case (or variant :focused)
    :focused
    {:grid-template-columns "minmax(0,1fr) minmax(0,72rem) minmax(0,1fr)"
     :grid-template-areas "\"left main right\""}

    :wide-focused
    {:grid-template-columns "minmax(0,1fr) minmax(0,88rem) minmax(0,1fr)"
     :grid-template-areas "\"left main right\""}

    :sidebar-main
    {:grid-template-columns "minmax(16rem,20rem) minmax(0,1fr)"
     :grid-template-areas "\"left main\""}

    :main-rail
    {:grid-template-columns "minmax(0,1fr) minmax(16rem,22rem)"
     :grid-template-areas "\"main right\""}

    :three-column
    {:grid-template-columns "minmax(14rem,18rem) minmax(0,1fr) minmax(16rem,22rem)"
     :grid-template-areas "\"left main right\""}

    :full
    {:grid-template-columns "minmax(0,1fr)"
     :grid-template-areas "\"main\""}

    ;; fallback
    {:grid-template-columns "minmax(0,1fr) minmax(0,72rem) minmax(0,1fr)"
     :grid-template-areas "\"left main right\""}))

(defn- page-root-class
  [class]
  (class-names
   "grid items-start gap-section pad-container w-full"
   class))

(defn page-left
  "Left page region.

  Short form:
    (page-left {:children [...]})

  Long form:
    (page-left {}
      ...)

  Intended for side content in :sidebar-main or :three-column layouts, but may
  also be used as a gutter rail in :focused variants."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :style {:grid-area "left"}
           :data-page-left true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :style {:grid-area "left"}
           :data-page-left true}
          attrs
          children))))

(defn page-main
  "Main page region.

  Short form:
    (page-main {:children [...]})

  Long form:
    (page-main {}
      ...)

  The main region is the primary content lane for the page."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :main
          {:class (class-names "section-theme min-w-0" class)
           :style {:grid-area "main"}
           :data-page-main true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :main
          {:class (class-names "section-theme min-w-0" class)
           :style {:grid-area "main"}
           :data-page-main true}
          attrs
          children))))

(defn page-right
  "Right page region.

  Short form:
    (page-right {:children [...]})

  Long form:
    (page-right {}
      ...)

  Intended for side content in :main-rail or :three-column layouts, but may
  also be used as a gutter rail in :focused variants."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :style {:grid-area "right"}
           :data-page-right true}
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :aside
          {:class (class-names "panel-theme min-w-0" class)
           :style {:grid-area "right"}
           :data-page-right true}
          attrs
          children))))

(defn page-surface
  "Continuous page surface.

  Short form:
    (page-surface {:children [...]})

  Long form:
    (page-surface {}
      ...)

  This is the \"sheet\" or continuous work surface inside a page region. It is
  visually distinct from cards and is intended to hold multiple internal
  sections as one coherent plane."
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

  Short form:
    (page {:variant :focused
           :main [...]})

    (page {:variant :three-column
           :left [...]
           :main [...]
           :right [...]})

  Long form:
    (page {:variant :focused}
      (page-main
        (page-surface ...)))

  Supported variants:
    :focused       centered main lane with flexible side gutters
    :wide-focused  wider centered main lane with flexible side gutters
    :sidebar-main  left sidebar + main content
    :main-rail     main content + right rail
    :three-column  left + main + right
    :full          single full-width main region

  Notes:
    - Page owns layout only.
    - page-surface is the optional continuous visual plane inside a region.
    - Left and right regions are optional in every variant."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [variant left main right children]} props
          root-style (layout-style variant)]
      (el :div
          {:class (page-root-class class)
           :data-page true
           :data-page-variant (name (or variant :focused))
           :style root-style}
          attrs
          [(when left
             (apply page-left {} (nodes left)))
           (when main
             (apply page-main {} (nodes main)))
           (when right
             (apply page-right {} (nodes right)))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [variant]} props
          root-style (layout-style variant)]
      (el :div
          {:class (page-root-class class)
           :data-page true
           :data-page-variant (name (or variant :focused))
           :style root-style}
          attrs
          children))))
