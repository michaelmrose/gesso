(ns gesso.components.dropdown-menu.attr
  (:require [gesso.util :refer :all]))

(defn dropdown-menu-root-attrs
  "Returns the attribute map for the dropdown root container.

   The root stores the open state and provides a positioned anchor for the
   floating content."
  [class open?]
  {:class (class-names "inline-block relative" class)
   :data-dropdown-root true
   :data-dropdown-open (if open? "true" "false")})

(defn dropdown-menu-trigger-attrs
  "Returns the attribute map for a dropdown trigger button."
  [class]
  {:class (class-names class)
   :type "button"
   :data-dropdown-trigger true
   :aria-haspopup "menu"})



(defn dropdown-menu-content-attrs
  [class open?]
  {:class (class-names class)
   :data-dropdown-content true
   :role "menu"
   :hidden (when-not open? true)
   :style {:position "absolute"
           :top "calc(100% + 0.35rem)"
           :left 0
           :min-width "14rem"
           :padding "0.35rem"
           :border-radius "0.5rem"
           :border "1px solid var(--border)"
           :background "var(--popover)"
           :color "var(--popover-foreground)"
           :box-shadow "0 18px 48px -18px rgba(0, 0, 0, 0.55), 0 10px 24px -18px rgba(0, 0, 0, 0.45)"
           :z-index 50}})

(defn dropdown-menu-label-attrs
  "Returns the attribute map for a menu label."
  [class]
  {:class (class-names class)
   :style {:padding "0.35rem 0.625rem 0.35rem 1.75rem"
           :font-size "0.75rem"
           :line-height "1.5rem"
           :color "var(--muted-foreground)"}})

(defn dropdown-menu-separator-attrs
  "Returns the attribute map for a menu separator."
  [class]
  {:class (class-names class)
   :role "separator"
   :style {:height "1px"
           :margin "0.35rem"
           :background "var(--border)"}})

(defn dropdown-menu-right-slot-attrs
  "Returns the attribute map for right-aligned secondary item content."
  [class]
  {:class (class-names class)
   :style {:margin-left "auto"
           :padding-left "1rem"
           :color "var(--muted-foreground)"
           :font-size "0.75rem"
           :line-height 1}})

(defn dropdown-menu-indicator-attrs
  "Returns the attribute map for the left gutter indicator slot."
  [class]
  {:class (class-names class)
   :style {:position "absolute"
           :left 0
           :width "1.75rem"
           :display "inline-flex"
           :align-items "center"
           :justify-content "center"}})
