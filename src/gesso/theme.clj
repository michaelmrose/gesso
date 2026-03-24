(ns gesso.theme
  (:require [clojure.string :as str]))

(def ^:private axis-specs
  [{:axis :color-theme
    :attr :data-color-theme
    :meta-name "gesso-color-theme-default"}

   {:axis :density
    :attr :data-density
    :meta-name "gesso-density-default"}

   {:axis :typography
    :attr :data-typography
    :meta-name "gesso-typography-default"}

   {:axis :shape
    :attr :data-shape
    :meta-name "gesso-shape-default"}])

(defn- normalize-token
  [x]
  (cond
    (nil? x) nil
    (keyword? x) (name x)
    :else (str x)))

(defn- normalize-axis-value
  [v]
  (cond
    (nil? v) nil
    (sequential? v) (->> v
                         (map normalize-token)
                         (remove str/blank?)
                         (str/join " "))
    :else (normalize-token v)))

(defn- normalize-mode
  [mode]
  (cond
    (nil? mode) nil
    (keyword? mode) (name mode)
    :else (-> mode str str/lower-case)))

(defn- normalize-theme-config
  [theme-or-config]
  (let [config (cond
                 (nil? theme-or-config) {}
                 (map? theme-or-config) theme-or-config
                 :else {:color-theme theme-or-config})]
    (reduce
     (fn [m {:keys [axis]}]
       (if-let [v (normalize-axis-value (get config axis))]
         (assoc m axis v)
         m))
     {}
     axis-specs)))

(defn html-theme-attrs
  "Return attrs intended for the html/root theme state.

  Supported forms:

    (html-theme-attrs \"vercel\" :system)
    ;; => {:data-color-theme \"vercel\" :data-theme-mode \"system\"}

    (html-theme-attrs
      {:color-theme \"vercel\"
       :density :comfortable
       :typography :ui}
      :dark)
    ;; => {:data-color-theme \"vercel\"
    ;;     :data-density \"comfortable\"
    ;;     :data-typography \"ui\"
    ;;     :data-theme-mode \"dark\"
    ;;     :class \"dark\"}

  Axis values may be a single keyword/string or a vector/seq. Sequential values
  are joined with spaces, which pairs with CSS selectors that use ~= matching.

    {:density [:compact :compactish]}
    ;; => {:data-density \"compact compactish\"}"
  ([theme-or-config]
   (html-theme-attrs theme-or-config nil))
  ([theme-or-config mode]
   (let [config (normalize-theme-config theme-or-config)
         mode   (normalize-mode mode)]
     (cond-> {}
       (:color-theme config) (assoc :data-color-theme (:color-theme config))
       (:density config) (assoc :data-density (:density config))
       (:typography config) (assoc :data-typography (:typography config))
       (:shape config) (assoc :data-shape (:shape config))
       mode (assoc :data-theme-mode mode)
       (= mode "dark") (assoc :class "dark")))))

(defn theme-head
  "Return head elements declaring default theme axes/mode for gesso-theme.js.

  Examples:
    (theme-head \"vercel\" :system)

    (theme-head {:color-theme \"vercel\"
                 :density :comfortable}
                :dark)"
  ([theme-or-config]
   (theme-head theme-or-config nil))
  ([theme-or-config mode]
   (let [config (normalize-theme-config theme-or-config)
         mode   (normalize-mode mode)]
     (remove
      nil?
      (concat
       (for [{:keys [axis meta-name]} axis-specs
             :let [value (get config axis)]
             :when value]
         [:meta {:name meta-name
                 :content value}])
       [(when mode
          [:meta {:name "gesso-theme-mode-default"
                  :content mode}])])))))

(defn theme
  "Returns a map compatible with Biff's base-html that sets both the initial
  HTML attributes AND the JS configuration tags.

  Convenience color-theme usage:
    (theme \"vercel\")
    (theme \"vercel\" :system)

  Multi-axis usage:
    (theme {:color-theme \"vercel\"
            :density :comfortable
            :typography :ui
            :shape :default})

    (theme {:color-theme \"vercel\"
            :density [:compact :compactish]}
           :dark)"
  ([theme-or-config]
   (theme theme-or-config :system))
  ([theme-or-config mode]
   {:base/html-attrs (html-theme-attrs theme-or-config mode)
    :base/head (theme-head theme-or-config mode)}))
