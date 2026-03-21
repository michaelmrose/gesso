(ns gesso.components.tabs.attr
  (:require [gesso.util :refer :all]))

(defn tabs-root-attrs
  "Returns the attribute map for the tabs root.

   The root stores the selected value on a data attribute and marks the overall
   orientation so triggers and panels can stay in sync.

   The current implementation is uncontrolled: the selected value is initialized
   from :default-value and then updated locally through scripts.

   Possible later expansions:
   - controlled tabs with an external :value
   - explicit activation mode support
   - more polished vertical styling"
  [class value orientation]
  {:class (class-names "flex flex-col" class)
   :data-tabs-root true
   :data-tabs-value (some-> value str)
   :data-orientation (name (or orientation :horizontal))})

(defn tabs-list-attrs
  "Returns the attribute map for the tab list.

   The list uses the WAI-ARIA tablist role and defaults to horizontal
   orientation."
  [class orientation]
  {:class (class-names
           "flex shrink-0 border-b border-[var(--border)]"
           (when (= orientation :vertical)
             "flex-col border-b-0 border-r")
           class)
   :role "tablist"
   :aria-orientation (name (or orientation :horizontal))
   :data-orientation (name (or orientation :horizontal))})

(defn tabs-trigger-attrs
  "Returns the attribute map for a tab trigger button.

   The visual model follows the restrained Radix style:
   neutral by default, clearer on hover, and visibly active with an underline.

   Active styling is applied later by script through data-state."
  [class value tab-id panel-id selected? orientation disabled?]
  {:class (class-names
           "relative flex flex-1 items-center justify-center bg-[var(--card)] px-5 py-3 text-sm leading-none outline-none select-none
            text-[var(--muted-foreground)] hover:text-[var(--primary)]"
           class)
   :type "button"
   :role "tab"
   :id tab-id
   :aria-controls panel-id
   :aria-selected (if selected? "true" "false")
   :tabindex (if selected? "0" "-1")
   :data-tabs-value (some-> value str)
   :data-state (if selected? "active" "inactive")
   :data-orientation (name (or orientation :horizontal))
   :data-disabled (when disabled? "true")
   :disabled (when disabled? true)
   :style {:box-shadow (when selected?
                         (if (= orientation :vertical)
                           "inset -1px 0 0 0 currentColor, -1px 0 0 0 currentColor"
                           "inset 0 -1px 0 0 currentColor, 0 1px 0 0 currentColor"))
           :color (cond
                    disabled? "var(--muted-foreground)"
                    selected? "var(--primary)"
                    :else nil)}})

(defn tabs-content-attrs
  "Returns the attribute map for a tab panel.

   Panels are linked to their trigger with aria-labelledby and hidden when
   inactive."
  [class value tab-id selected? orientation]
  {:class (class-names
           "grow rounded-b-lg bg-[var(--card)] p-5 outline-none"
           (when (= orientation :vertical)
             "rounded-b-none rounded-r-lg")
           class)
   :role "tabpanel"
   :id (str tab-id "-panel")
   :aria-labelledby tab-id
   :aria-selected (if selected? "true" "false")
   :tabindex "-1"
   :data-tabs-value (some-> value str)
   :data-state (if selected? "active" "inactive")
   :data-orientation (name (or orientation :horizontal))
   :hidden (when-not selected? true)})
