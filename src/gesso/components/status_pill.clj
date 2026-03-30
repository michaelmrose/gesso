(ns gesso.components.status-pill
  (:require
   [clojure.string :as str]
   [gesso.util :refer :all]
   [gesso.components.icon :refer [icon]]))

(def ^:private status-aliases
  {:waiting :warning
   :active :info
   :claimed :info
   :complete :success
   :cancelled :muted
   :error :destructive})

(def ^:private status-styles
  {:default {:background "var(--secondary)"
             :color "var(--secondary-foreground)"}
   :muted {:background "var(--muted)"
           :color "var(--muted-foreground)"}
   :info {:background "var(--secondary)"
          :color "var(--primary)"}
   :success {:background "var(--primary)"
             :color "var(--primary-foreground)"}
   :warning {:background "var(--accent)"
             :color "var(--accent-foreground)"}
   :destructive {:background "var(--destructive)"
                 :color "var(--destructive-foreground)"}})

(defn- semantic-status
  [status]
  (get status-aliases (or status :default) (or status :default)))

(defn- status-label
  [status]
  (-> (name (or status :default))
      (str/replace "-" " ")
      str/capitalize))

(defn- leading-visual
  [{:keys [icon-name dot?]}]
  (cond
    icon-name
    (icon icon-name {:size :sm
                     :class "shrink-0"})

    dot?
    [:span {:aria-hidden "true"
            :class "shrink-0"
            :style {:display "inline-block"
                    :width "0.45rem"
                    :height "0.45rem"
                    :border-radius "9999px"
                    :background "currentColor"
                    :opacity "0.85"}}]

    :else
    nil))

(defn- pill-style
  [status]
  (merge
   {:display "inline-flex"
    :align-items "center"
    :padding-inline "0.625rem"
    :padding-block "0.25rem"
    :border-radius "9999px"
    :white-space "nowrap"}
   (get status-styles (semantic-status status) (status-styles :default))))

(defn status-pill
  "Compact inline state indicator.

  Short form:
    (status-pill {:status :waiting})
    (status-pill {:status :claimed :text \"Claimed\"})
    (status-pill {:status :success :icon \"check\"})
    (status-pill {:status :warning :dot? true})

  Long form:
    (status-pill {:status :warning}
      \"Needs attention\")

  Options:
    :status  semantic status keyword
    :text    optional explicit label; otherwise derived from :status
    :icon    optional icon name string for (icon \"...\")
    :dot?    optional decorative dot shown when :icon is not provided

  Supported core statuses:
    :default :muted :info :success :warning :destructive

  Friendly aliases:
    :waiting :active :claimed :complete :cancelled :error"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [status text icon dot?]} props
          semantic (semantic-status status)
          label (or text (status-label status))]
      (el :span
          {:class (class-names
                   "inline-flex items-center gap-2 font-body text-sm-theme leading-tight-theme weight-medium-theme"
                   class)
           :data-status-pill true
           :data-status (name semantic)
           :style (pill-style semantic)}
          attrs
          [(leading-visual {:icon-name icon :dot? dot?})
           label]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [status icon dot?]} props
          semantic (semantic-status status)]
      (el :span
          {:class (class-names
                   "inline-flex items-center gap-2 font-body text-sm-theme leading-tight-theme weight-medium-theme"
                   class)
           :data-status-pill true
           :data-status (name semantic)
           :style (pill-style semantic)}
          attrs
          [(leading-visual {:icon-name icon :dot? dot?})
           children]))))
