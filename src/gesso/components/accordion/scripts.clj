(ns gesso.components.accordion.scripts
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [hs merge-script-attr attach-script-to-node attach-script-to-children-by-tag]]))

(defn accordion-chevron-script []
  [[:let 'chev "first <svg[data-accordion-chevron]/> in me"]
   [:if 'chev
    [[:if 'me.open
       [:set 'chev.style.transform "'rotate(180deg)'"]
       [:set 'chev.style.transform "'rotate(0deg)'"]]]]])

(defn accordion-single-script
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
  [{:keys [type collapsible?]}]
  (let [type         (or type :multiple)
        collapsible? (if (nil? collapsible?) true collapsible?)]
    (hs
     [:on :toggle
      (when (= type :single)
        (accordion-single-script collapsible?))
      (accordion-chevron-script)])))
