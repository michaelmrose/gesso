(ns gesso.components.dropdown-menu.scripts
  (:require [gesso.hyperscript :refer [hs]]))

(defn dropdown-menu-open-script
  "Returns the Hyperscript instructions that open the nearest dropdown root."
  []
  [[:let 'root "closest <div[data-dropdown-root]/>"]
   [:let 'content "first <div[data-dropdown-content]/> in root"]
   [:set 'root.dataset.dropdownOpen "'true'"]
   [:if 'content
    [[:set 'content.hidden false]]]])

(defn dropdown-menu-close-script
  "Returns the Hyperscript instructions that close the nearest dropdown root."
  []
  [[:let 'root "closest <div[data-dropdown-root]/>"]
   [:let 'content "first <div[data-dropdown-content]/> in root"]
   [:set 'root.dataset.dropdownOpen "'false'"]
   [:if 'content
    [[:set 'content.hidden true]]]])

(defn dropdown-menu-toggle-script
  "Returns the Hyperscript instructions that toggle the nearest dropdown root."
  []
  [[:let 'root "closest <div[data-dropdown-root]/>"]
   [:if "root.dataset.dropdownOpen == 'true'"
    (dropdown-menu-close-script)
    (dropdown-menu-open-script)]])

(defn dropdown-menu-trigger-script
  "Builds the script attached to a dropdown trigger."
  []
  (hs
   [:on :click
    (dropdown-menu-toggle-script)]))

(defn dropdown-menu-item-script
  "Builds the script attached to a dropdown item.

   By default selecting an item closes the menu. When keep-open? is truthy the
   item leaves the menu open."
  [{:keys [keep-open?]}]
  (hs
   [:on :click
    (when-not keep-open?
      (dropdown-menu-close-script))]))

(defn dropdown-menu-root-script
  "Builds the script attached to the dropdown root.

   Escape closes the menu whenever it is open."
  []
  (hs
   [:on "keyup[key is 'Escape'] from document"
    [:if "my.dataset.dropdownOpen == 'true'"
     (dropdown-menu-close-script)]]))
