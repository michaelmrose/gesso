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

   Supports simple caller-controlled positioning with:

   - :side  :bottom | :top
   - :align :start  | :end
   - :side-offset string CSS length, default \"0.35rem\"

   The menu uses popover tokens rather than card tokens so it reads as a
   floating layer in both light and dark themes.

   Positioning is intentionally simple for now and does not attempt collision
   detection or automatic flipping.

   Possible later expansions:
   - automatic vertical flipping when there is not enough room below
   - automatic horizontal alignment when the menu would overflow right or left
   - collision padding from viewport edges
   - recomputing placement on resize or scroll
   - submenu-aware positioning"
  [class open? side align side-offset]
  (let [side        (or side :bottom)
        align       (or align :start)
        side-offset (or side-offset "0.35rem")
        position-class
        (case [side align]
          [:bottom :start] (str "top-[calc(100%+" side-offset ")] left-0")
          [:bottom :end]   (str "top-[calc(100%+" side-offset ")] right-0")
          [:top :start]    (str "bottom-[calc(100%+" side-offset ")] left-0")
          [:top :end]      (str "bottom-[calc(100%+" side-offset ")] right-0")
          (str "top-[calc(100%+" side-offset ")] left-0"))]
    {:class (class-names
             "absolute z-50 min-w-56 rounded-lg border p-1.5
              border-[var(--border)] bg-[var(--popover)] text-[var(--popover-foreground)]
              shadow-[0_24px_64px_-24px_rgba(0,0,0,0.75),0_16px_32px_-24px_rgba(0,0,0,0.65)]"
             position-class
             class)
     :data-dropdown-content true
     :data-side (name side)
     :data-align (name align)
     :role "menu"
     :hidden (when-not open? true)}))

(defn dropdown-menu-item-attrs
  "Returns the attribute map for a menu item button.

   Items reserve a left gutter for optional indicators and support a highlighted
   hover and focus state through accent tokens.

   Normal item text uses the standard foreground color so the accent color can
   be reserved for hover and focus states. Disabled items still rely on a small
   inline style map for their muted color, cursor, and opacity."
  [class disabled?]
  {:class (class-names
           "relative flex min-h-8 w-full items-center rounded-sm border-none bg-transparent px-2.5 py-2 pl-7 text-left text-[0.8125rem] leading-[1.1] outline-none
            text-[var(--foreground)] hover:bg-[var(--accent)] hover:text-[var(--accent-foreground)]
            focus:bg-[var(--accent)] focus:text-[var(--accent-foreground)]"
           class)
   :type "button"
   :role "menuitem"
   :data-dropdown-item true
   :data-disabled (when disabled? "true")
   :disabled (when disabled? true)
   :style {:color (when disabled? "var(--muted-foreground)")
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
