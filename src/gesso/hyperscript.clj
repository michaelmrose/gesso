(ns gesso.hyperscript
  (:require [clojure.string :as str]))

(def ^:private stmt-ops
  #{:on :if :for :set :let})

(def ^:private scope-ops
  #{:local :global :element})

(defn- indent
  [level]
  (apply str (repeat level "  ")))

(defn- statement-form?
  [x]
  (and (vector? x)
       (keyword? (first x))
       (contains? stmt-ops (first x))))

(defn- scope-form?
  [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))
       (contains? scope-ops (first x))))

(defn- simple-symbol-string
  [x]
  (let [s (str x)]
    (if (str/includes? s "/")
      (throw (ex-info "Namespaced symbols are not supported in hyperscript forms"
                      {:value x}))
      s)))

(defn- emit-atom
  [x]
  (cond
    (nil? x) nil
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (simple-symbol-string x)
    (true? x) "true"
    (false? x) "false"
    :else (str x)))

(defn- emit-target
  [x]
  (cond
    (scope-form? x)
    (let [[scope name] x]
      (str (name scope) " " (emit-atom name)))

    :else
    (emit-atom x)))

(declare emit-form)
(declare emit-many)

(defn- normalize-forms
  [forms]
  (mapcat
   (fn [x]
     (cond
       (nil? x) []
       (statement-form? x) [x]
       (and (sequential? x) (not (string? x))) (normalize-forms x)
       :else [x]))
   forms))

(defn- emit-block
  [head-line body level]
  (let [body-lines (emit-many body (inc level))]
    (if (str/blank? body-lines)
      (str (indent level) head-line "\n"
           (indent level) "end")
      (str (indent level) head-line "\n"
           body-lines "\n"
           (indent level) "end"))))

(defn- emit-on
  [[_ event & body] level]
  (emit-block
   (str "on " (emit-atom event))
   body
   level))

(defn- emit-if
  [[_ test then else] level]
  (let [test-str   (emit-atom test)
        then-lines (emit-many [then] (inc level))
        else-lines (when (some? else)
                     (emit-many [else] (inc level)))]
    (str (indent level) "if " test-str
         (when-not (str/blank? then-lines)
           (str "\n" then-lines))
         (when (some? else)
           (str "\n" (indent level) "else"
                (when-not (str/blank? else-lines)
                  (str "\n" else-lines))))
         "\n" (indent level) "end")))

(defn- emit-for
  [[_ binding source & body] level]
  (emit-block
   (str "for " (emit-atom binding) " in " (emit-atom source))
   body
   level))

(defn- emit-set
  [[_ target value] level]
  (str (indent level)
       "set " (emit-target target) " to " (emit-atom value)))

(defn- emit-let
  [[_ target value] level]
  (let [target (if (scope-form? target)
                 target
                 [:local target])]
    (str (indent level)
         "set " (emit-target target) " to " (emit-atom value))))

(defn- emit-form
  [form level]
  (cond
    (nil? form)
    nil

    (statement-form? form)
    (case (first form)
      :on  (emit-on form level)
      :if  (emit-if form level)
      :for (emit-for form level)
      :set (emit-set form level)
      :let (emit-let form level)
      (throw (ex-info "Unknown hyperscript form" {:form form})))

    (string? form)
    (str (indent level) form)

    :else
    (throw (ex-info "Unsupported hyperscript form" {:form form}))))

(defn- emit-many
  [forms level]
  (->> (normalize-forms forms)
       (map #(emit-form % level))
       (remove nil?)
       (remove str/blank?)
       (str/join "\n")))

(defn hs
  [& forms]
  (emit-many forms 0))
