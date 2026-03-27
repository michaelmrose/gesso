(ns gesso.components.accordion.attr
  (:require [gesso.util :refer :all]))

(defn accordion-root-attrs
  "Returns the attribute map for the accordion root container."
  [class]
  {:class (class-names
           "overflow-hidden rounded-lg border border-border bg-card shadow-sm"
           class)
   :data-accordion-root true})

(defn accordion-item-attrs
  "Returns the merged attribute map for a single accordion item."
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "group overflow-hidden border-b border-border last:border-b-0"
            (when disabled? "opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")
    :data-disabled (when disabled? "true")}
   attrs))

(defn accordion-trigger-attrs
  "Returns the attribute map for an accordion trigger.

   Uses the shared density row utilities while keeping a clear header/body
   contrast through existing semantic color classes."
  [class]
  {:class (class-names
           "cursor-pointer w-full list-none pad-row flex items-center justify-between gap-inline bg-secondary text-primary outline-none"
           class)
   :style {:font-weight 600}})

(defn accordion-content-attrs
  "Returns the attribute map for the accordion content region.

   Uses shared panel spacing and body text utilities while keeping the body on
   the card surface with a visible divider from the header."
  [class]
  {:class (class-names
           "border-t border-border bg-card pad-panel font-body text-base-theme leading-body text-muted-foreground"
           class)})

(defn accordion-chevron-node
  "Returns the chevron icon node used by accordion triggers.

   Uses shared icon sizing utilities. The initial transform is rendered from
   open? and the toggle script keeps it in sync afterward."
  [open?]
  [:svg {:data-accordion-chevron true
         :aria-hidden "true"
         :viewBox "0 0 20 20"
         :class "icon-md-theme block shrink-0 transition-transform duration-200 ease-in-out"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "2"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:transform (if open?
                              "rotate(180deg)"
                              "rotate(0deg)")}}
   [:path {:d "M6 8l4 4 4-4"}]])
