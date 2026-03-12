(ns gesso.components.select
  (:require [gesso.util :refer :all]))

(defn- option-node
  [{:keys [value label selected disabled]}]
  [:option
   (cond-> {:value value}
     selected (assoc :selected true)
     disabled (assoc :disabled true))
   label])

(defn select
  "Native select.

  Short form:
    (select {:id \"role\"
             :name \"role\"
             :value \"staff\"
             :placeholder \"Choose a role\"
             :options [{:value \"staff\" :label \"Staff\"}
                       {:value \"manager\" :label \"Manager\"}]})

  Long form:
    (select {:attrs {:name \"role\"}}
      [:option {:value \"staff\"} \"Staff\"])"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [options id name value disabled? required? placeholder]} props
          options (or options [])
          options (map (fn [opt]
                         (if (= (:value opt) value)
                           (assoc opt :selected true)
                           opt))
                       options)]
      (el :select
          {:class (class-names "select" class)}
          (merge-attrs
           attrs
           (when id {:id id})
           (when name {:name name})
           (when (some? disabled?) {:disabled disabled?})
           (when (some? required?) {:required required?}))
          [(when placeholder
             [:option
              (cond-> {:value ""}
                (nil? value) (assoc :selected true)
                true (assoc :disabled true))
              placeholder])
           (map option-node options)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [id name disabled? required?]} props]
      (el :select
          {:class (class-names "select" class)}
          (merge-attrs
           attrs
           (when id {:id id})
           (when name {:name name})
           (when (some? disabled?) {:disabled disabled?})
           (when (some? required?) {:required required?}))
          children))))
