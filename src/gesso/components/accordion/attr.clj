(ns gesso.components.accordion.attr
  (:require [gesso.util :refer :all]))

(defn accordion-root-attrs
  "Returns the attribute map for the accordion root container."
  [class]
  {:class (class-names
           "overflow-hidden rounded-lg shadow-sm"
           class)
   :data-accordion-root true
   :style {:border "1px solid var(--border)"
           :background "var(--card)"}})

(defn accordion-item-attrs
  "Returns the merged attribute map for a single accordion item."
  [value open? disabled? class attrs]
  (merge-attrs
   {:class (class-names
            "group overflow-hidden last:border-b-0"
            (when disabled? "opacity-60 pointer-events-none")
            class)
    :open (when open? true)
    :data-accordion-value (->value value "item")
    :data-disabled (when disabled? "true")}
   attrs))

(defn accordion-trigger-attrs
  "Returns the attribute map for an accordion trigger.

   Uses shared density utilities for spacing, while keeping colors explicit so
   the header remains visually distinct regardless of Tailwind token utility
   availability. A hairline inset divider separates closed items."
  [class]
  {:class (class-names
           "cursor-pointer w-full list-none pad-row flex items-center justify-between gap-inline outline-none"
           class)
   :style {:background "var(--secondary)"
           :color "var(--primary)"
           :font-weight 600
           :box-shadow "inset 0 -1px 0 0 color-mix(in srgb, var(--foreground) 16%, transparent)"}})


(defn accordion-content-attrs
  "Returns the attribute map for the accordion content region.

   Uses shared panel spacing and body text utilities, while keeping body colors
   and dividers explicit so the content remains visually distinct regardless of
   Tailwind token utility availability."
  [class]
  {:class (class-names
           "pad-panel font-body text-base-theme leading-body"
           class)
   :style {:border-top "1px solid var(--border)"
           :background "var(--card)"
           :color "var(--muted-foreground)"}})

(defn accordion-chevron-node
  "Returns the chevron icon node used by accordion triggers.

   Uses shared icon sizing utilities when available. The initial transform is
   rendered from open? and the toggle script keeps it in sync afterward."
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
