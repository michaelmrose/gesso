(ns gesso.components.accordion.attr
  (:require [gesso.util :refer :all]))

(defn accordion-root-attrs
  "Returns the attribute map for the accordion root container.

   Applies the outer shell styling and marks the element so item-level scripts
   can locate the containing accordion."
  [class]
  {:class (class-names "overflow-hidden rounded-lg shadow-sm" class)
   :data-accordion-root true
   :style {:border "1px solid var(--border)"
           :background "var(--card)"}})

(defn accordion-item-attrs
  "Returns the merged attribute map for a single accordion item.

   Sets the item's stable value, initial open state, disabled treatment, and
   separator styling, then merges any caller-supplied attrs on top."
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "group overflow-hidden last:border-b-0"
            (when disabled? "opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")
    :style {:border-bottom "1px solid var(--border)"}}
   attrs))

(defn accordion-trigger-attrs
  "Returns the attribute map for an accordion trigger.

   The trigger is styled as the visible header row for each item, with padding,
   alignment, and theme-aware colors for the label and chevron."
  [class]
  {:class (class-names
           "cursor-pointer w-full list-none px-4 py-6 flex items-center justify-between gap-4 outline-none"
           class)
   :style {:background "var(--muted)"
           :color "var(--primary)"
           :font-weight 600}})

(defn accordion-content-attrs
  "Returns the attribute map for the accordion content region.

   Adds inner spacing and a top divider so the revealed body reads as a distinct
   panel beneath the header."
  [class]
  {:class (class-names
           "px-4 py-6"
           class)
   :style {:border-top "1px solid var(--border)"
           :background "var(--background)"
           :color "var(--muted-foreground)"
           :font-size "0.95rem"
           :line-height "1.7"}})

(defn accordion-chevron-node
  "Returns the chevron icon node used by accordion triggers.

   The icon is identified with a data attribute so the attached Hyperscript can
   rotate it when the corresponding item opens or closes."
  []
  [:svg {:data-accordion-chevron true
         :aria-hidden "true"
         :viewBox "0 0 20 20"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:width "1rem"
                 :height "1rem"
                 :display "block"
                 :flex-shrink 0
                 :transition "transform 200ms ease"}}
   [:path {:d "M6 8l4 4 4-4"}]])
