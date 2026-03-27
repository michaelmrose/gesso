(ns gesso.components.tabs.attr
  (:require [gesso.util :refer :all]))

(defn tabs-root-attrs
  "Returns the attribute map for the tabs root.

   The root stores the selected value on a data attribute and marks the overall
   orientation so triggers and panels can stay in sync.

   The current implementation is uncontrolled in the sense that the selected
   value is rendered into the DOM and then updated locally through scripts.
   A server-rendered :value may still be used as the initial selected tab."
  [class value orientation]
  {:class (class-names "flex flex-col" class)
   :data-tabs-root true
   :data-tabs-value (some-> value str)
   :data-orientation (name (or orientation :horizontal))})

(defn tabs-list-attrs
  "Returns the attribute map for the tab list.

   The list uses the WAI-ARIA tablist role and marks itself as the stable tabs
   list region for scripting, styling, and fragment targeting."
  [class orientation]
  {:class (class-names
           "flex shrink-0 border-b border-[var(--border)]"
           (when (= orientation :vertical)
             "flex-col border-b-0 border-r")
           class)
   :role "tablist"
   :aria-orientation (name (or orientation :horizontal))
   :data-tabs-list true
   :data-orientation (name (or orientation :horizontal))})

(defn tabs-trigger-attrs
  "Returns the attribute map for a tab trigger button.

   The trigger carries the stable tab value, state, orientation, and disabled
   markers used by scripts and any future styling hooks."
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
   :data-tabs-trigger true
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
   inactive. The data markers make the panel easy to target for scripting,
   fragment replacement, or future styling hooks."
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
   :data-tabs-content true
   :data-tabs-value (some-> value str)
   :data-state (if selected? "active" "inactive")
   :data-orientation (name (or orientation :horizontal))
   :hidden (when-not selected? true)})
