(ns gesso.components.icon
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [gesso.util :refer :all]))

(def ^:dynamic *icon-search-paths*
  "Classpath-relative directories searched, in order, when resolving icons by
   name. Project-local icons should typically live under resources/icons, while
   Gesso's bundled fallback set can live under resources/icons/lucide."
  ["icons" "icons/lucide"])

(defn- normalize-icon-name
  [name]
  (-> (str name)
      str/trim
      (str/replace #"\.svg$" "")))

(defn- icon-resource
  [name]
  (let [name (normalize-icon-name name)]
    (some (fn [dir]
            (io/resource (str dir "/" name ".svg")))
          *icon-search-paths*)))

(defn- xml-attrs->hiccup-attrs
  [attrs]
  (into {}
        (map (fn [[k v]]
               [(if (keyword? k) k (keyword (str k))) v]))
        attrs))

(defn- xml-node->hiccup
  [node]
  (cond
    (string? node)
    node

    (map? node)
    (let [{:keys [tag attrs content]} node
          children (->> content
                        (remove nil?)
                        (remove #(and (string? %) (str/blank? %)))
                        (map xml-node->hiccup))]
      (into [tag (xml-attrs->hiccup-attrs attrs)] children))

    :else
    node))

(def ^:private load-icon-template
  (memoize
   (fn [name]
     (let [name (normalize-icon-name name)
           res  (icon-resource name)]
       (when-not res
         (throw (ex-info (str "Icon not found: " name)
                         {:icon/name name
                          :icon/search-paths *icon-search-paths*})))
       (with-open [in (io/input-stream res)]
         (xml-node->hiccup (xml/parse in)))))))

(defn- icon-size-class
  [size]
  (case size
    :xs "icon-xs-theme"
    :sm "icon-sm-theme"
    :md "icon-md-theme"
    :lg "icon-lg-theme"
    :xl "icon-xl-theme"
    :2xl "icon-2xl-theme"
    nil))

(defn- css-size
  [v]
  (cond
    (nil? v) nil
    (number? v) (str v "px")
    :else (str v)))

(defn- computed-size-style
  [{:keys [size width height]}]
  (let [square-size (when (or (string? size) (number? size))
                      (css-size size))
        width       (or (css-size width) square-size)
        height      (or (css-size height) square-size)]
    (cond-> {}
      width (assoc :width width)
      height (assoc :height height))))

(defn- apply-icon-opts
  [svg {:keys [size width height class attrs title]}]
  (let [[tag base-attrs & children] svg
        base-attrs     (apply dissoc base-attrs [:width :height :class :aria-hidden :role :focusable])
        user-style     (:style attrs)
        base-style     (:style base-attrs)
        size-style     (computed-size-style {:size size :width width :height height})
        merged-style   (merge base-style size-style user-style)
        attrs-no-style (dissoc attrs :style)
        default-attrs  (if title
                         {:role "img"}
                         {:aria-hidden "true"
                          :focusable "false"})
        merged-attrs   (cond-> (merge-attrs
                                base-attrs
                                {:class (class-names (icon-size-class size) class)}
                                default-attrs
                                attrs-no-style)
                         (seq merged-style) (assoc :style merged-style))
        children       (if title
                         (into [[:title title]] children)
                         children)]
    (into [tag merged-attrs] children)))

(defn icon
  "Render an SVG icon by name.

  Shorthand:
    (icon \"search\")
    (icon \"search\" {:size :xs})
    (icon \"search\" {:size :lg})
    (icon \"search\" {:size \"1.5rem\"})
    (icon \"search\" {:width \"20px\" :height \"20px\"})
    (icon \"search\" {:title \"Search\"})

  Map form:
    (icon {:name \"search\"
           :size :md
           :class \"...\"
           :attrs {...}
           :title \"Search\"})

  Resolution order follows *icon-search-paths*:
  1. project/local icons (e.g. resources/icons)
  2. bundled fallback icons (e.g. resources/icons/lucide)

  Size options:
    :xs  => icon-xs-theme
    :sm  => icon-sm-theme
    :md  => icon-md-theme (default when no explicit size is provided)
    :lg  => icon-lg-theme
    :xl  => icon-xl-theme
    :2xl => icon-2xl-theme
    \"...\" / number => explicit square size applied to width and height

  Explicit sizing:
    :width and :height override any square size derived from :size

  Accessibility:
    - decorative by default (aria-hidden=\"true\")
    - if :title is provided, the icon is exposed as an image with a <title>"
  [& args]
  (let [opts (cond
               (and (= 1 (count args)) (string? (first args)))
               {:name (first args) :size :md}

               (and (= 1 (count args)) (map? (first args)))
               (merge {:size :md} (first args))

               (and (= 2 (count args))
                    (string? (first args))
                    (map? (second args)))
               (merge {:name (first args) :size :md} (second args))

               :else
               (throw (ex-info "icon expects (icon \"name\"), (icon \"name\" opts), or (icon {:name ...})"
                               {:args args})))
        {:keys [props class attrs]} (split-opts opts)
        {:keys [name size width height title]} props
        svg (load-icon-template name)]
    (apply-icon-opts svg
                     {:size size
                      :width width
                      :height height
                      :title title
                      :class class
                      :attrs attrs})))
