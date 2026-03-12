(ns gesso.util
  (:require
   [clojure.string :as str]))


(defn only-map-arg?
  [args]
  (and (= 1 (count args))
       (map? (first args))))

(defn class-names
  "Join class values into a single class string.
  Accepts strings, keywords, nils, and nested sequential values.
  Splits strings and removes duplicates to prevent doubled-up classes."
  [& xs]
  (->> xs
       flatten
       (remove nil?)
       (mapcat (fn [x]
                 (cond
                   (string? x) (str/split x #"\s+")
                   (keyword? x) [(name x)]
                   (sequential? x) x
                   :else [(str x)])))
       (remove str/blank?)
       distinct
       (str/join " ")))

(defn ->value
  "Normalize an accordion item value to a string."
  [v fallback]
  (cond
    (nil? v) (str fallback)
    (keyword? v) (name v)
    (string? v) v
    :else (str v)))

(defn merge-attrs
  "Merge Hiccup attribute maps, concatenating :class instead of overwriting it."
  [& maps]
  (reduce
   (fn [acc m]
     (let [m (or m {})]
       (-> acc
           (merge (dissoc m :class))
           (assoc :class (class-names (:class acc) (:class m))))))
   {}
   maps))

(defn hiccup-element?
  "True if x looks like a hiccup element vector: [tag ...] where tag is a keyword or symbol."
  [x]
  (and (vector? x)
       (let [t (first x)]
         (or (keyword? t) (symbol? t)))))

(defn normalize-children
  "Flatten child sequences one level while preserving Hiccup element vectors as atomic."
  [children]
  (->> children
       (mapcat
        (fn [c]
          (cond
            (nil? c) []
            (hiccup-element? c) [c]
            (and (vector? c) (not (hiccup-element? c))) c
            (and (sequential? c) (not (string? c))) c
            :else [c])))
       (remove nil?)))




(defn nodes
  "Normalize a content value into a seq of nodes."
  [x]
  (cond
    (nil? x) []
    (hiccup-element? x) [x]
    (and (vector? x) (not (hiccup-element? x))) x
    (and (sequential? x) (not (string? x))) x
    :else [x]))

(defn el
  "Construct a Hiccup element with base attrs, user attrs, and children."
  [tag base-attrs attrs children]
  (let [merged-attrs (merge-attrs base-attrs attrs)
        ;; Clean up empty class strings
        final-attrs (if (str/blank? (:class merged-attrs))
                      (dissoc merged-attrs :class)
                      merged-attrs)]
    (into [tag final-attrs] (normalize-children children))))

(defn split-opts
  "Split an opts map into {:props ... :class ... :attrs ...}."
  [opts]
  {:props (dissoc opts :class :attrs)
   :class (:class opts)
   :attrs (:attrs opts)})

(defn normalize-component-args
  "Returns [opts children] for variadic component calls.
   If the first arg is a map, it is treated as opts; otherwise opts is {} and
   all args are treated as children."
  [args]
  (let [[opts & children] args
        opts (if (map? opts) opts {})]
    [opts (if (map? (first args)) children args)]))
