(ns gesso.components.tabs.scripts
  (:require [gesso.hyperscript :refer [hs]]))

(defn tabs-select-script
  "Returns the Hyperscript instructions that select the current tab trigger's
   value inside the nearest tabs root.

   This updates:
   - the root's selected value
   - trigger aria-selected, tabindex, and data-state
   - panel hidden and data-state values

   The current implementation is click-driven and uncontrolled.

   Possible later expansions:
   - keyboard navigation with arrow keys, Home, and End
   - manual vs automatic activation
   - controlled mode where the component emits intent rather than mutating local
     DOM state directly"
  []
  [[:let 'root "closest <div[data-tabs-root]/>"]
   [:let 'nextValue "me.dataset.tabsValue"]
   [:set 'root.dataset.tabsValue 'nextValue]

   [:for 'tab "<button[role='tab']/> in root"
    [:if "tab.dataset.tabsValue == nextValue"
     [[:set 'tab.dataset.state "'active'"]
      [:set 'tab.ariaSelected "'true'"]
      [:set 'tab.tabIndex "0"]
      [:set 'tab.style.color "'var(--primary)'"]
      [:if "tab.dataset.orientation == 'vertical'"
       [[:set 'tab.style.boxShadow "'inset -1px 0 0 0 currentColor, -1px 0 0 0 currentColor'"]]
       [[:set 'tab.style.boxShadow "'inset 0 -1px 0 0 currentColor, 0 1px 0 0 currentColor'"]]]]
     [[:set 'tab.dataset.state "'inactive'"]
      [:set 'tab.ariaSelected "'false'"]
      [:set 'tab.tabIndex "-1"]
      [:set 'tab.style.color "''"]
      [:set 'tab.style.boxShadow "''"]]]]

   [:for 'panel "<div[role='tabpanel']/> in root"
    [:if "panel.dataset.tabsValue == nextValue"
     [[:set 'panel.dataset.state "'active'"]
      [:set 'panel.ariaSelected "'true'"]
      [:set 'panel.hidden false]]
     [[:set 'panel.dataset.state "'inactive'"]
      [:set 'panel.ariaSelected "'false'"]
      [:set 'panel.hidden true]]]]])

(defn tabs-trigger-script
  "Builds the script attached to a tab trigger."
  []
  (hs
   [:on :click
    (tabs-select-script)]))
