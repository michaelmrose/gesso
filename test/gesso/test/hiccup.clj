(ns gesso.test.hiccup
  (:require
   [clojure.string :as str]))

(defn hiccup-element?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn element-attrs
  [node]
  (if (and (hiccup-element? node)
           (map? (second node)))
    (second node)
    {}))

(defn element-children
  [node]
  (when (hiccup-element? node)
    (let [[_ maybe-attrs & more] node]
      (if (map? maybe-attrs)
        more
        (cons maybe-attrs more)))))

(defn all-elements
  [x]
  (letfn [(walk [x]
            (lazy-seq
             (cond
               (hiccup-element? x)
               (cons x (mapcat walk (element-children x)))

               (and (sequential? x) (not (string? x)))
               (mapcat walk x)

               :else
               nil)))]
    (walk x)))

(defn find-element
  [pred hiccup]
  (first (filter pred (all-elements hiccup))))

(defn elements
  [pred hiccup]
  (filter pred (all-elements hiccup)))

(defn by-id
  [id hiccup]
  (find-element #(= id (:id (element-attrs %))) hiccup))

(defn by-name
  [name hiccup]
  (find-element #(= name (:name (element-attrs %))) hiccup))

(defn by-attr
  [k v hiccup]
  (find-element #(= v (get (element-attrs %) k)) hiccup))

(defn elements-by-tag
  [tag hiccup]
  (elements #(= tag (first %)) hiccup))

(defn classes
  [attrs]
  (->> (str/split (or (:class attrs) "") #"\s+")
       (remove str/blank?)
       set))

(defn has-class?
  [attrs class-name]
  (contains? (classes attrs) class-name))

(defn script-includes?
  [attrs s]
  (str/includes? (or (:_ attrs) "") s))

(defn no-nil-children?
  [node]
  (not-any? nil? (element-children node)))
