(ns gesso.components.toaster.attr
  (:require [gesso.util :refer :all]))

(def default-toaster-id
  "app-toaster")

(def default-position
  :bottom-right)

(def default-live
  :polite)

(def default-variant
  :default)

(def default-oob-swap
  "beforeend")

(defn- token
  [x fallback]
  (let [x (if (nil? x) fallback x)]
    (cond
      (keyword? x) (name x)
      (symbol? x)  (name x)
      (string? x)  x
      :else        (str x))))

(defn- split
  [opts]
  (let [{:keys [props class attrs]} (split-opts (or opts {}))]
    {:props (or props {})
     :class class
     :attrs attrs}))

(defn toaster-id
  [opts]
  (let [{:keys [props]} (split opts)]
    (token (:id props) default-toaster-id)))

(defn toaster-position
  [opts]
  (let [{:keys [props]} (split opts)]
    (token (:position props) default-position)))

(defn toaster-live
  [opts]
  (let [{:keys [props]} (split opts)]
    (token (:live props) default-live)))

(defn toaster-attrs
  "Return attrs for the stable toaster region.

   :attrs merges last so callers can override low-level output."
  [opts]
  (let [{:keys [class attrs]} (split opts)]
    (merge-attrs
     {:id (toaster-id opts)
      :class (class-names "toaster" class)
      :data-toaster true
      :data-position (toaster-position opts)
      :aria-live (toaster-live opts)
      :aria-atomic "false"}
     attrs)))

(defn toast-variant
  [opts]
  (let [{:keys [props]} (split opts)]
    (token (:variant props) default-variant)))

(defn toast-role
  [opts]
  (let [{:keys [props]} (split opts)]
    (token (:role props)
           (if (= "danger" (toast-variant opts))
             :alert
             :status))))

(defn toast-close?
  [opts]
  (let [{:keys [props]} (split opts)]
    (not (false? (:close? props)))))

(defn toast-duration
  [opts]
  (let [{:keys [props]} (split opts)]
    (:duration props)))

(defn- maybe-toast-id-attrs
  [id]
  (when (some? id)
    (let [id (str id)]
      {:id id
       :data-toast-id id})))

(defn- maybe-duration-attrs
  [duration]
  (when (some? duration)
    {:data-duration duration}))

(defn toast-attrs
  "Return attrs for a single toast item.

   Component options consumed here:
     :id
     :variant
     :role
     :duration
     :close?

   :attrs merges last so callers can override low-level output."
  [opts]
  (let [{:keys [props class attrs]} (split opts)
        {:keys [id duration]} props]
    (merge-attrs
     {:class (class-names "toast" class)
      :data-toast true
      :data-variant (toast-variant opts)
      :data-dismissible (if (toast-close? opts) "true" "false")
      :role (toast-role opts)}
     (maybe-toast-id-attrs id)
     (maybe-duration-attrs duration)
     attrs)))

(defn toast-body-attrs
  ([] (toast-body-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-body" class)
       :data-toast-body true}
      attrs))))

(defn toast-content-attrs
  ([] (toast-content-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-content" class)
       :data-toast-content true}
      attrs))))

(defn toast-icon-attrs
  ([] (toast-icon-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-icon" class)
       :data-toast-icon true}
      attrs))))

(defn toast-title-attrs
  ([] (toast-title-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-title" class)
       :data-toast-title true}
      attrs))))

(defn toast-description-attrs
  ([] (toast-description-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-description" class)
       :data-toast-description true}
      attrs))))

(defn toast-action-attrs
  ([] (toast-action-attrs {}))
  ([opts]
   (let [{:keys [class attrs]} (split opts)]
     (merge-attrs
      {:class (class-names "toast-action" class)
       :data-toast-action true}
      attrs))))

(defn toast-close-attrs
  ([] (toast-close-attrs {}))
  ([opts]
   (let [{:keys [props class attrs]} (split opts)
         label (or (:close-label props)
                   "Dismiss notification")]
     (merge-attrs
      {:type "button"
       :class (class-names "toast-close" class)
       :data-toast-close true
       :aria-label label}
      attrs))))

(defn render-toast-oob-attrs
  "Return attrs for the HTMX OOB wrapper that appends a toast to the toaster.

   Options:
     :target  toaster id, default app-toaster
     :swap    hx-swap-oob value, default beforeend

   :attrs merges last."
  [opts]
  (let [{:keys [props class attrs]} (split opts)
        target (token (:target props) default-toaster-id)
        swap   (token (:swap props) default-oob-swap)]
    (merge-attrs
     {:id target
      :class (class-names class)
      :hx-swap-oob swap}
     attrs)))
