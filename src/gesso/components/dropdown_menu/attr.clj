(ns gesso.components.dropdown-menu.attr
  (:require [gesso.util :refer :all]))

(defn dropdown-menu-root-attrs
  "Returns the attribute map for the dropdown root container.

   The root stores the open state and provides a positioned anchor for the
   floating content."
  [class open?]
  {:class (class-names "relative inline-block" class)
   :data-dropdown-root true
   :data-dropdown-open (if open? "true" "false")})

(defn dropdown-menu-trigger-attrs
  [class]
  {:class (class-names "btn-outline" class)
   :type "button"
   :data-dropdown-trigger true
   :aria-haspopup "menu"})

(defn dropdown-menu-content-attrs
  "Returns the attribute map for the floating menu surface.

   The content is hidden when closed and positioned beneath the trigger."
  [class open?]
  {:class (class-names
           "absolute left-0 top-[calc(100%+0.35rem)] z-50 min-w-56 rounded-lg border p-1.5 shadow-sm
            border-[var(--border)] bg-[var(--popover)] text-[var(--popover-foreground)]
            shadow-[0_18px_48px_-18px_rgba(0,0,0,0.55),0_10px_24px_-18px_rgba(0,0,0,0.45)]"
           class)
   :data-dropdown-content true
   :role "menu"
   :hidden (when-not open? true)})

(defn dropdown-menu-item-attrs
  "Returns the attribute map for a menu item button.

   Items are laid out with a reserved left gutter and optional right slot."
  [class disabled?]
  {:class (class-names
           "relative flex min-h-8 w-full items-center rounded-sm border-none bg-transparent px-2.5 py-2 pl-7 text-left text-[0.8125rem] leading-[1.1] outline-none"
           class)
   :type "button"
   :role "menuitem"
   :data-dropdown-item true
   :data-disabled (when disabled? "true")
   :disabled (when disabled? true)
   :style {:color (if disabled?
                    "var(--muted-foreground)"
                    "var(--primary)")
           :cursor (if disabled? "default" "pointer")
           :user-select "none"
           :opacity (when disabled? 0.7)}})

(defn dropdown-menu-label-attrs
  "Returns the attribute map for a menu label."
  [class]
  {:class (class-names
           "px-2.5 py-1.5 pl-7 text-xs leading-6 text-[var(--muted-foreground)]"
           class)})

(defn dropdown-menu-separator-attrs
  "Returns the attribute map for a menu separator."
  [class]
  {:class (class-names
           "mx-1.5 my-1 h-px bg-[var(--border)]"
           class)
   :role "separator"})

(defn dropdown-menu-right-slot-attrs
  "Returns the attribute map for right-aligned secondary item content."
  [class]
  {:class (class-names
           "ml-auto pl-4 text-xs leading-none text-[var(--muted-foreground)]"
           class)})

(defn dropdown-menu-indicator-attrs
  "Returns the attribute map for the left gutter indicator slot."
  [class]
  {:class (class-names
           "absolute left-0 inline-flex w-7 items-center justify-center"
           class)})
