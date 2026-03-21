(ns gesso.components.accordion.attr
  (:require [gesso.util :refer :all]))

(defn accordion-root-attrs
  [class]
  {:class (class-names "overflow-hidden rounded-lg shadow-sm" class)
   :data-accordion-root true
   :style {:border "1px solid var(--border)"
           :background "var(--card)"}})

(defn accordion-item-attrs
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
  [class]
  {:class (class-names
            "cursor-pointer w-full list-none px-4 py-6 flex items-center justify-between gap-4 outline-none"
            class)
   :style {:background "var(--muted)"
           :color "var(--primary)"
           :font-weight 600}})

(defn accordion-content-attrs
  [class]
  {:class (class-names
            "px-4 py-6"
            class)
   :style {:border-top "1px solid var(--border)"
           :background "var(--background)"
           :color "var(--muted-foreground)"
           :font-size "0.95rem"
           :line-height "1.7"}})

(defn accordion-chevron-node []
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
