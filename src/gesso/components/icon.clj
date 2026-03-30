(ns gesso.components.icon
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [gesso.util :refer :all]))

(def ^:private icon-size-classes
  {:sm "icon-sm-theme"
   :md "icon-md-theme"})

(defonce ^:private lucide-cache
  (atom {}))

(defn- ->resource-name
  [icon-name]
  (-> (cond
        (keyword? icon-name) (name icon-name)
        (string? icon-name) icon-name
        :else (str icon-name))
      str/trim))

(defn- normalize-xml-key
  [k]
  (keyword (name k)))

(defn- xml-attrs->hiccup
  [attrs]
  (into {}
        (map (fn [[k v]]
               [(normalize-xml-key k) (str v)]))
        attrs))

(defn- xml-node->hiccup
  [node]
  (cond
    (string? node)
    (when-not (str/blank? node)
      node)

    (map? node)
    (let [tag      (normalize-xml-key (:tag node))
          attrs    (xml-attrs->hiccup (:attrs node))
          children (->> (:content node)
                        (map xml-node->hiccup)
                        (remove nil?))]
      (into [tag attrs] children))

    :else
    node))

(defn- parse-svg-resource
  [resource-path]
  (if-let [res (io/resource resource-path)]
    (with-open [in (io/input-stream res)]
      (xml-node->hiccup (xml/parse in)))
    (throw (ex-info (str "Icon resource not found: " resource-path)
                    {:resource-path resource-path}))))

(defn- lucide-svg
  [icon-name]
  (let [icon-name (->resource-name icon-name)]
    (or (@lucide-cache icon-name)
        (let [parsed (parse-svg-resource (str "icons/lucide/" icon-name ".svg"))]
          (swap! lucide-cache assoc icon-name parsed)
          parsed))))

(defn- normalize-svg-node
  [svg-node {:keys [class attrs size title]}]
  (let [[tag maybe-attrs & children] svg-node
        has-attrs?    (map? maybe-attrs)
        svg-attrs     (if has-attrs? maybe-attrs {})
        children      (if has-attrs? children (cons maybe-attrs children))
        {:keys [class attrs]} (split-opts {:class class :attrs attrs})
        attrs-class   (:class attrs)
        attrs         (dissoc attrs :class)
        size-class    (get icon-size-classes (or size :md))
        svg-attrs     (-> svg-attrs
                          (dissoc :width :height)
                          (merge attrs)
                          (assoc :class (class-names (:class svg-attrs)
                                                     attrs-class
                                                     size-class
                                                     class)))
        svg-attrs     (if title
                        (-> svg-attrs
                            (assoc :role "img")
                            (dissoc :aria-hidden))
                        (-> svg-attrs
                            (assoc :aria-hidden "true")
                            (dissoc :role)))
        children      (if title
                        (cons [:title title] children)
                        children)]
    (into [tag svg-attrs] children)))

(defn- wrap-non-svg-node
  [node {:keys [class attrs size title]}]
  (let [size-class (get icon-size-classes (or size :md))
        attrs-class (:class attrs)
        attrs       (dissoc attrs :class)
        attrs       (cond-> (merge attrs
                                   {:class (class-names size-class attrs-class class)})
                      (not title) (assoc :aria-hidden "true")
                      title (assoc :role "img" :aria-label title))]
    (into [:span attrs] (nodes node))))

(defn icon
  "Normalizes an icon node for consistent sizing and accessibility.

  Shorthand:
    (icon (lucide \"inbox\"))

  Map form:
    (icon {:node (lucide \"alert-triangle\")
           :size :sm
           :title \"Warning\"})

  Two-arg form:
    (icon (lucide \"search\") {:size :sm})

  Options:
    :node   required in map form
    :size   :sm | :md (defaults to :md)
    :class  extra classes
    :attrs  extra attrs for the rendered root
    :title  when present, makes the icon non-decorative"
  [& args]
  (cond
    (= 1 (count args))
    (let [arg (first args)]
      (if (map? arg)
        (let [{:keys [node] :as opts} arg]
          (if (and (vector? node) (= :svg (first node)))
            (normalize-svg-node node opts)
            (wrap-non-svg-node node opts)))
        (normalize-svg-node arg {})))

    (= 2 (count args))
    (let [[node opts] args]
      (if (and (vector? node) (= :svg (first node)))
        (normalize-svg-node node opts)
        (wrap-non-svg-node node (assoc opts :node node))))

    :else
    (throw (ex-info "icon expects either a node, an opts map, or [node opts]"
                    {:args args}))))

(defn lucide
  "Returns a vendored Lucide SVG as hiccup.

  Looks up SVGs from:
    resources/icons/lucide/<name>.svg

  Examples:
    (lucide \"inbox\")
    (lucide :search)
    (lucide \"alert-triangle\" {:size :sm})
    (lucide \"check\" {:class \"text-current\"})

  Options:
    :size   :sm | :md (defaults to :md)
    :class  extra classes
    :attrs  extra attrs for the svg root
    :title  when present, makes the icon non-decorative"
  ([icon-name]
   (lucide icon-name {}))
  ([icon-name opts]
   (normalize-svg-node (lucide-svg icon-name) opts)))
