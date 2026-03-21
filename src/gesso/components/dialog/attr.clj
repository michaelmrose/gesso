(ns gesso.components.dialog.attr
  (:require [gesso.util :refer :all]))

(defn dialog-root-attrs
  "Returns the attribute map for the dialog root container.

   The root tracks open state with a data attribute so trigger, overlay, and
   close controls can all coordinate through the same DOM node."
  [class open?]
  {:class (class-names "relative" class)
   :data-dialog-root true
   :data-dialog-open (if open? "true" "false")})

(defn dialog-trigger-attrs
  "Returns the attribute map for a dialog trigger button."
  [class]
  {:class (class-names "btn-primary" class)
   :type "button"
   :data-dialog-trigger true})

(defn dialog-overlay-attrs
  "Returns the attribute map for the modal overlay.

   The overlay is hidden when the dialog is closed and fills the viewport when
   open."
  [class open?]
  {:class (class-names class)
   :data-dialog-overlay true
   :hidden (when-not open? true)
   :style {:position "fixed"
           :inset 0
           :background "rgba(0, 0, 0, 0.55)"
           :z-index 40}})



(defn dialog-content-attrs
  "Returns the attribute map for the dialog content surface.

   The content is centered in the viewport and hidden when the dialog is
   closed."
  [class open?]
  {:class (class-names "shadow-sm" class)
   :data-dialog-content true
   :hidden (when-not open? true)
   :role "dialog"
   :aria-modal "true"
   :style {:position "fixed"
           :top "50%"
           :left "50%"
           :transform "translate(-50%, -50%)"
           :width "min(36rem, calc(100vw - 2rem))"
           :max-height "calc(100vh - 2rem)"
           :overflow "auto"
           :z-index 50
           :border "1px solid var(--border)"
           :border-radius "0.75rem"
           :background "var(--card)"
           :color "var(--card-foreground)"
           :padding "1.5rem"}})

(defn dialog-header-attrs
  "Returns the attribute map for the dialog header region."
  [class]
  {:class (class-names class)
   :style {:display "flex"
           :flex-direction "column"
           :gap "0.5rem"
           :margin-bottom "1rem"}})

(defn dialog-title-attrs
  "Returns the attribute map for the dialog title."
  [class]
  {:class (class-names "m-0" class)
   :style {:margin 0
           :font-size "1.25rem"
           :line-height 1.25
           :font-weight 600
           :color "var(--foreground)"}})

(defn dialog-description-attrs
  "Returns the attribute map for the dialog description."
  [class]
  {:class (class-names "m-0" class)
   :style {:margin 0
           :font-size "0.95rem"
           :line-height 1.6
           :color "var(--muted-foreground)"}})

(defn dialog-body-attrs
  "Returns the attribute map for the main dialog body."
  [class]
  {:class (class-names class)
   :style {:display "flex"
           :flex-direction "column"
           :gap "1rem"}})

(defn dialog-footer-attrs
  "Returns the attribute map for the dialog footer.

   The footer is aligned for actions and spaced away from the body."
  [class]
  {:class (class-names class)
   :style {:display "flex"
           :justify-content "flex-end"
           :gap "0.5rem"
           :margin-top "1.25rem"}})

(defn dialog-close-attrs
  "Returns the attribute map for a dialog close button."
  [class]
  {:class (class-names "btn-outline" class)
   :type "button"
   :data-dialog-close true})
