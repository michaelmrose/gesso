(ns gesso.components.accordion.scripts
  (:require
   [gesso.hyperscript :refer [hs]]))

(defn accordion-chevron-script
  "Returns the Hyperscript instructions that keep the accordion chevron in sync
   with a details element's open state.

   The script looks for the chevron inside the current item and rotates it to
   180 degrees when the item is open, restoring it to 0 degrees when closed."
  []
  [[:let 'chev "first <svg[data-accordion-chevron]/> in me"]
   [:if 'chev
    [[:if 'me.open
      [:set 'chev.style.transform "'rotate(180deg)'"]
      [:set 'chev.style.transform "'rotate(0deg)'"]]]]])

(defn accordion-single-script
  "Returns the Hyperscript needed for single-open accordion behavior.

   When collapsible? is true, opening one item closes the rest of the items in
   the same accordion root.

   When collapsible? is false, the same single-open behavior applies, but the
   last open item is prevented from closing so that one item always remains open."
  [collapsible?]
  (if collapsible?
    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]]]
    [[:let 'root "closest <div[data-accordion-root]/>"]
     [:if 'me.open
      [[:for 'd "<details/> in root"
        [:if "d != me"
         [:set 'd.open false]]]]
      [[:let 'anyOpen false]
       [:for 'd "<details/> in root"
        [:if 'd.open
         [:set 'anyOpen true]]]
       [:if "not anyOpen"
        [:set 'me.open true]]]]]))

(defn accordion-script
  "Builds the full Hyperscript string for an accordion item.

   The returned script always updates the chevron on toggle. For :single
   accordions it also applies the group behavior that closes sibling items,
   respecting the collapsible? setting.

   Selection persistence is intentionally not handled here. Accordions own local
   open/close behavior while the DOM exists; Gesso Live continuity owns restoring
   local open state across fragment replacement."
  [{:keys [type collapsible?]}]
  (let [type         (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (hs
     [:on :toggle
      (when (= type :single)
        (accordion-single-script collapsible?))
      (accordion-chevron-script)])))
