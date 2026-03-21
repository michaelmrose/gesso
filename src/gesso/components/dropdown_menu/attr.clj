(ns gesso.components.dropdown-menu.attr
  (:require [gesso.util :refer :all]))

(defn dropdown-menu-root-attrs
  "Returns the attribute map for the dropdown root container.

   The root provides the positioning context for the floating menu content and
   stores the current open state on a data attribute so scripts can toggle it.

   This assumes the menu content is rendered inside the same root and positioned
   relative to it."
  [class open?]
  {:class (class-names "relative inline-block" class)
   :data-dropdown-root true
   :data-dropdown-open (if open? "true" "false")})

(defn dropdown-menu-trigger-attrs
  "Returns the attribute map for a dropdown trigger button.

   The default trigger uses the outline button styling so the short form reads
   as an obvious control without extra configuration."
  [class]
  {:class (class-names "btn-outline" class)
   :type "button"
   :data-dropdown-trigger true
   :aria-haspopup "menu"})

(defn dropdown-menu-content-attrs
  "Returns the attribute map for the floating menu surface.

   The content is hidden when closed and positioned below the trigger by
   default. It uses popover tokens rather than card tokens so the surface reads
   as a floating layer in both light and dark themes.

   Positioning is intentionally simple for now: the menu opens below and
   left-aligned from the root. Near viewport edges this may still need manual
   adjustment or a later collision-handling pass."
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

   Items reserve a left gutter for optional indicators and support a highlighted
   hover and focus state through accent tokens.

   Disabled items are marked with both the disabled attribute and a data
   attribute. The current implementation still uses a small inline style map for
   disabled and default color handling; that means color behavior is not driven
   entirely by classes yet."
  [class disabled?]
  {:class (class-names
           "relative flex min-h-8 w-full items-center rounded-sm border-none bg-transparent px-2.5 py-2 pl-7 text-left text-[0.8125rem] leading-[1.1] outline-none
            hover:bg-[var(--accent)] hover:text-[var(--accent-foreground)]
            focus:bg-[var(--accent)] focus:text-[var(--accent-foreground)]"
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
  "Returns the attribute map for a menu label.

   Labels are intended for lightweight grouping and section headings inside the
   menu, not as selectable items."
  [class]
  {:class (class-names
           "px-2.5 py-1.5 pl-7 text-xs leading-6 text-[var(--muted-foreground)]"
           class)})

(defn dropdown-menu-separator-attrs
  "Returns the attribute map for a menu separator.

   Separators create a visual break between groups of items without affecting
   menu interaction."
  [class]
  {:class (class-names
           "mx-1.5 my-1 h-px bg-[var(--border)]"
           class)
   :role "separator"})

(defn dropdown-menu-right-slot-attrs
  "Returns the attribute map for right-aligned secondary item content.

   This is useful for shortcuts, state hints, or submenu affordances that
   should stay visually separated from the main item label."
  [class]
  {:class (class-names
           "ml-auto pl-4 text-xs leading-none text-[var(--muted-foreground)]"
           class)})

(defn dropdown-menu-indicator-attrs
  "Returns the attribute map for the left gutter indicator slot.

   The indicator slot reserves a stable area for checkmarks, bullets, or other
   item-state markers so item labels stay aligned whether or not an indicator is
   present."
  [class]
  {:class (class-names
           "absolute left-0 inline-flex w-7 items-center justify-center"
           class)})
