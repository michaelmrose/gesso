(ns gesso.util
  (:require [clojure.string :as str]))

(defn opts+children
  "Split component args into [opts children].

  Supports both:
    (component \"child\")
    (component {:class \"foo\"} \"child\")"
  [args]
  (if (map? (first args))
    [(first args) (rest args)]
    [{} args]))

(defn class-names
  "Join class values into a single class string.

  Accepts strings, keywords, nils, and nested sequential values."
  [& xs]
  (->> xs
       flatten
       (remove nil?)
       (mapcat (fn [x]
                 (cond
                   (string? x) [x]
                   (keyword? x) [(name x)]
                   (sequential? x) x
                   :else [(str x)])))
       (remove str/blank?)
       (str/join " ")))

(defn merge-attrs
  "Merge Hiccup attribute maps, concatenating :class instead of overwriting it."
  [& maps]
  (reduce
   (fn [acc m]
     (let [m (or m {})]
       (-> acc
           (merge (dissoc m :class))
           (update :class class-names (:class acc) (:class m)))))
   {}
   maps))

(defn normalize-children
  "Flatten nested child sequences and remove nils."
  [children]
  (->> children
       flatten
       (remove nil?)))

(defn element
  "Construct a Hiccup element vector with merged attrs and normalized children."
  [tag base-attrs attrs children]
  (into [tag (merge-attrs base-attrs attrs)]
        (normalize-children children)))

(defn split-opts
  "Split an opts map into [props class attrs].

  props  => opts without :class and :attrs
  class  => the value of :class
  attrs  => the value of :attrs"
  [opts]
  [(dissoc opts :class :attrs)
   (:class opts)
   (:attrs opts)])

(defn props+class+attrs
  "Return opts in map form for easier destructuring:
    {:props ...
     :class ...
     :attrs ...}"
  [opts]
  (let [[props class attrs] (split-opts opts)]
    {:props props
     :class class
     :attrs attrs}))

(defn leaf
  "Construct a simple component element with a default class merged with any user class."
  [tag default-class opts children]
  (let [{:keys [class attrs]} (props+class+attrs opts)]
    (element tag
             {:class (class-names default-class class)}
             attrs
             children)))
