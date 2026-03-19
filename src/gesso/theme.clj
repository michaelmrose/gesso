(ns gesso.theme
  (:require [clojure.string :as str]))

(defn- normalize-theme [theme]
  (cond
    (nil? theme) nil
    (keyword? theme) (name theme)
    :else (str theme)))

(defn- normalize-mode [mode]
  (cond
    (nil? mode) nil
    (keyword? mode) (name mode)
    :else (-> mode str str/lower-case)))

(defn html-theme-attrs
  "Return attrs intended for the html/root theme state.

  Examples:
    (html-theme-attrs \"vercel\" :system)
    ;; => {:data-theme \"vercel\" :data-theme-mode \"system\"}

    (html-theme-attrs \"vercel\" :dark)
    ;; => {:data-theme \"vercel\" :data-theme-mode \"dark\" :class \"dark\"}"
  ([theme]
   (html-theme-attrs theme nil))
  ([theme mode]
   (let [theme (normalize-theme theme)
         mode  (normalize-mode mode)]
     (cond-> {}
       theme (assoc :data-theme theme)
       mode (assoc :data-theme-mode mode)
       (= mode "dark") (assoc :class "dark")))))

#_(defn theme
  "Return a base-html-friendly map for fixed defaults.

  Example:
    (merge (theme \"vercel\" :system) ...)"
  ([theme]
   {:base/html-attrs (html-theme-attrs theme nil)})
  ([theme mode]
   {:base/html-attrs (html-theme-attrs theme mode)}))

(defn theme-head
  "Return head elements declaring default theme/mode for gesso-theme.js.

  Example:
    (theme-head \"vercel\" :system)"
  ([theme]
   (theme-head theme nil))
  ([theme mode]
   (let [theme (normalize-theme theme)
         mode  (normalize-mode mode)]
     (remove nil?
             [[:meta {:name "gesso-theme-default"
                      :content theme}]
              (when mode
                [:meta {:name "gesso-theme-mode-default"
                        :content mode}])]))))


(defn theme
  "Returns a map compatible with Biff's base-html that sets
   both the initial HTML attributes AND the JS configuration tags."
  ([theme-name]
   (theme theme-name :system))
  ([theme-name mode]
   {:base/html-attrs (html-theme-attrs theme-name mode)
    :base/head (theme-head theme-name mode)}))
