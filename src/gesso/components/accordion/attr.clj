(ns gesso.components.accordion.attr
  (:require [gesso.util :refer :all]))

(defn accordion-root-attrs
  "Returns the attribute map for the accordion root container."
  [class]
  {:class (class-names
           "accordion accordion-root overflow-hidden rounded-lg shadow-sm"
           class)
   :data-accordion-root true
   :style {:border "1px solid var(--border)"
           :background "var(--card)"}})

(defn accordion-item-attrs
  "Returns the merged attribute map for a single accordion item."
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "accordion-item group overflow-hidden last:border-b-0"
            (when disabled? "accordion-item-disabled opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")
    :data-disabled (when disabled? "true")
    :style {:border-bottom "1px solid var(--border)"}}
   attrs))

(defn accordion-trigger-attrs
  "Returns the attribute map for an accordion trigger.

   Keeps density-aware spacing utilities, but uses an explicit token split so
   the header is visibly distinct from the body."
  [class]
  {:class (class-names
           "accordion-trigger cursor-pointer w-full list-none px-control py-control flex items-center justify-between gap-inline outline-none"
           class)
   :style {:background "var(--secondary)"
           :color "var(--primary)"
           :font-weight 600}})

(defn accordion-content-attrs
  "Returns the attribute map for the accordion content region.

   Keeps density-aware spacing and shared body text classes, but uses an
   explicit body surface so the content reads as a separate panel under the
   header."
  [class]
  {:class (class-names
           "accordion-content px-control py-control font-body text-base-theme leading-body"
           class)
   :style {:border-top "1px solid var(--border)"
           :background "var(--card)"
           :color "var(--muted-foreground)"}})

(defn accordion-chevron-node
  "Returns the chevron icon node used by accordion triggers."
  []
  [:svg {:class "accordion-chevron"
         :data-accordion-chevron true
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
