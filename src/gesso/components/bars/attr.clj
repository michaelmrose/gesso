(ns gesso.components.bars.attr
  (:require
   [gesso.util :refer :all]))

(defn- visibility-attrs
  [prefix visibility]
  {(keyword (str "data-bars-" prefix "-default-visible"))
   (if (get visibility :default) "true" "false")

   (keyword (str "data-bars-" prefix "-md-visible"))
   (if (get visibility :md) "true" "false")

   (keyword (str "data-bars-" prefix "-sm-visible"))
   (if (get visibility :sm) "true" "false")})

(defn bars-root-attrs
  [class {:keys [has-sidebar? sidebar-collapse-at has-hamburger-md? has-hamburger-sm?]}]
  {:class (class-names "min-w-0" class)
   :data-bars-root true
   :data-bars-open "false"
   :data-bars-has-sidebar (if has-sidebar? "true" "false")
   :data-bars-sidebar-collapse-at (name (or sidebar-collapse-at :medium))
   :data-bars-has-hamburger-md (if has-hamburger-md? "true" "false")
   :data-bars-has-hamburger-sm (if has-hamburger-sm? "true" "false")})

(defn bars-topbar-attrs
  [class]
  {:class (class-names class)
   :data-bars-topbar true})

(defn bars-brand-attrs
  [class]
  {:class (class-names "min-w-0" class)
   :data-bars-brand true})

(defn bars-segment-attrs
  [region class]
  {:class (class-names "min-w-0" class)
   :data-bars-segment (name region)})

(defn bars-toggle-attrs
  [class]
  {:class (class-names class)
   :type "button"
   :aria-label "Toggle navigation"
   :data-bars-toggle true})

(defn bars-hamburger-panel-attrs
  [class]
  {:class (class-names class)
   :data-bars-hamburger-panel true})

(defn bars-hamburger-inner-attrs
  [class]
  {:class (class-names "min-w-0" class)
   :data-bars-hamburger-inner true})

(defn bars-body-attrs
  [class]
  {:class (class-names "min-w-0" class)
   :data-bars-body true})

(defn bars-sidebar-attrs
  [class]
  {:class (class-names "min-w-0" class)
   :data-bars-sidebar true})

(defn bars-content-attrs
  [class]
  {:class (class-names "min-w-0" class)
   :data-bars-content true})

(defn bars-category-attrs
  [class visibility]
  (merge
   {:class (class-names "min-w-0" class)
    :data-bars-category true}
   (visibility-attrs "hamburger" visibility)))

(defn bars-category-label-attrs
  [class]
  {:class (class-names class)
   :data-bars-category-label true})

(defn bars-menu-attrs
  [mode class visibility]
  (merge
   {:class (class-names "min-w-0" class)
    :data-bars-menu true
    :data-bars-mode (name mode)}
   (visibility-attrs
    (if (= mode :hamburger) "hamburger" "home")
    visibility)))

(defn bars-menu-label-attrs
  [mode class]
  {:class (class-names class)
   :data-bars-menu-label true
   :data-bars-mode (name mode)})

(defn bars-menu-group-attrs
  [mode class]
  {:class (class-names "min-w-0" class)
   :data-bars-menu-group true
   :data-bars-mode (name mode)})

(defn bars-menu-heading-attrs
  [mode class]
  {:class (class-names class)
   :data-bars-menu-heading true
   :data-bars-mode (name mode)})

(defn bars-menu-items-attrs
  [mode class]
  {:class (class-names "min-w-0" class)
   :data-bars-menu-items true
   :data-bars-mode (name mode)})

(defn bars-menu-item-attrs
  [mode current? class]
  (cond-> {:class (class-names class)
           :data-bars-menu-item true
           :data-bars-mode (name mode)}
    current? (assoc :aria-current "page")))
