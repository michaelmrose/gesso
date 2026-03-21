(ns gesso.components.dialog.scripts
  (:require [gesso.hyperscript :refer [hs]]))

(defn dialog-open-script
  "Returns the Hyperscript instructions that open the nearest dialog root.

   Opening the dialog updates the root state attribute and reveals the overlay
   and content nodes."
  []
  [[:let 'root "closest <div[data-dialog-root]/>"]
   [:let 'overlay "first <div[data-dialog-overlay]/> in root"]
   [:let 'content "first <section[data-dialog-content]/> in root"]
   [:set 'root.dataset.dialogOpen "'true'"]
   [:if 'overlay
    [[:set 'overlay.hidden false]]]
   [:if 'content
    [[:set 'content.hidden false]]]])

(defn dialog-close-script
  "Returns the Hyperscript instructions that close the nearest dialog root.

   Closing the dialog updates the root state attribute and hides the overlay
   and content nodes."
  []
  [[:let 'root "closest <div[data-dialog-root]/>"]
   [:let 'overlay "first <div[data-dialog-overlay]/> in root"]
   [:let 'content "first <section[data-dialog-content]/> in root"]
   [:set 'root.dataset.dialogOpen "'false'"]
   [:if 'overlay
    [[:set 'overlay.hidden true]]]
   [:if 'content
    [[:set 'content.hidden true]]]])

(defn dialog-trigger-script
  "Builds the script attached to a dialog trigger."
  []
  (hs
   [:on :click
    (dialog-open-script)]))

(defn dialog-overlay-script
  "Builds the script attached to the overlay so clicking outside the content
   closes the dialog."
  []
  (hs
   [:on :click
    (dialog-close-script)]))

(defn dialog-close-script-attr
  "Builds the script attached to explicit close controls."
  []
  (hs
   [:on :click
    (dialog-close-script)]))

(defn dialog-root-script
  "Builds the script attached to the dialog root.

   Escape closes the dialog whenever the root is currently open."
  []
  (hs
   [:on "keyup[key is 'Escape'] from document"
    [:if "my.dataset.dialogOpen == 'true'"
     (dialog-close-script)]]))
