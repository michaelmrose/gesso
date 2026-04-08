(ns gesso.components.bars.scripts
  (:require
   [gesso.hyperscript :refer [hs]]))

(defn bars-root-script
  "Keeps hamburger state stable across true tier changes.

   Important mobile fix:
   do not close on every resize. Mobile browsers often fire resize while
   scrolling because the browser chrome changes height. We only close when the
   semantic width tier actually changes.

   Tiers are:
   - default  => width >= 1025
   - md       => 769..1024
   - sm       => width <= 768"
  []
  (hs
   [:on :load
    "if window.matchMedia('(max-width: 768px)').matches
       set me.dataset.barsTier to 'sm'
     else
       if window.matchMedia('(max-width: 1024px)').matches
         set me.dataset.barsTier to 'md'
       else
         set me.dataset.barsTier to 'default'
       end
     end"
    "set me.dataset.barsOpen to 'false'"]

   [:on "resize from window"
    "set prevTier to me.dataset.barsTier"
    "if window.matchMedia('(max-width: 768px)').matches
       set me.dataset.barsTier to 'sm'
     else
       if window.matchMedia('(max-width: 1024px)').matches
         set me.dataset.barsTier to 'md'
       else
         set me.dataset.barsTier to 'default'
       end
     end"
    "if prevTier != me.dataset.barsTier
       set me.dataset.barsOpen to 'false'
     end"]))

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
