(ns gesso.components.bars.scripts
  (:require
   [gesso.hyperscript :refer [hs]]))

(defn bars-root-script
  "Keeps hamburger state conservative and stable.

   This first pass closes the hamburger on load and on any window resize. That
   is stricter than the final breakpoint-only requirement, but it avoids mixed
   responsive state while the component is still young."
  []
  (hs
   [:on :load
    [[:set 'me.dataset.barsOpen "'false'"]]]
   [:on "resize from window"
    [[:set 'me.dataset.barsOpen "'false'"]]]))

(defn bars-toggle-script
  "Toggle the root hamburger state."
  []
  (hs
   [:on :click
    [[:let 'root "closest <div[data-bars-root]/>"]
     [:if "root.dataset.barsOpen == 'true'"
      [[:set 'root.dataset.barsOpen "'false'"]]
      [[:set 'root.dataset.barsOpen "'true'"]]]]]))

(defn bars-close-script
  "Close the hamburger after activating an item rendered inside it."
  []
  (hs
   [:on :click
    [[:let 'root "closest <div[data-bars-root]/>"]
     [:if 'root
      [[:set 'root.dataset.barsOpen "'false'"]]
      nil]]]))
