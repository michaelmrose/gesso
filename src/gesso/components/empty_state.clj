(ns gesso.components.empty-state
  (:require
   [gesso.util :refer :all]
   [gesso.components.text :as text]))

(defn empty-state-title
  "Empty state title.

  Short form:
    (empty-state-title {:text \"No requests\"})

  Long form:
    (empty-state-title \"No requests\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/heading
       {:level 3
        :as :h3
        :class class
        :attrs attrs
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/heading
             {:level 3
              :as :h3
              :class class
              :attrs attrs}
             children))))

(defn empty-state-description
  "Empty state description.

  Short form:
    (empty-state-description {:text \"Try changing your filters.\"})

  Long form:
    (empty-state-description \"Try changing your filters.\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/text
       {:variant :muted
        :as :p
        :class class
        :attrs attrs
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/text
             {:variant :muted
              :as :p
              :class class
              :attrs attrs}
             children))))

(defn empty-state-actions
  "Empty state actions wrapper.

  Short form:
    (empty-state-actions {:children [...]})

  Long form:
    (empty-state-actions
      (button {:text \"Refresh\"}))"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "cluster-theme justify-center" class)}
          (merge-attrs attrs {:data-empty-state-actions true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "cluster-theme justify-center" class)}
          (merge-attrs attrs {:data-empty-state-actions true})
          children))))

(defn empty-state-icon
  []
  [:svg {:viewBox "0 0 24 24"
         :aria-hidden "true"
         :class "icon-md-theme"
         :fill "none"
         :stroke "currentColor"
         :stroke-width "1.75"
         :stroke-linecap "round"
         :stroke-linejoin "round"
         :style {:width "2rem"
                 :height "2rem"}}
   [:path {:d "M4 8.5h16"}]
   [:path {:d "M5.5 8.5v8.5c0 .83.67 1.5 1.5 1.5h10c.83 0 1.5-.67 1.5-1.5V8.5"}]
   [:path {:d "M9 12.5h6"}]])


(defn empty-state
  "Empty state component.

  Short form:
    (empty-state
      {:icon [:svg ...]
       :title \"No requests\"
       :description \"New requests will appear here.\"
       :action (button {:text \"Refresh\"})})

  Long form:
    (empty-state {}
      [:svg ...]
      (empty-state-title \"No requests\")
      (empty-state-description \"New requests will appear here.\")
      (empty-state-actions
        (button {:text \"Refresh\"})))

  Notes:
    - :icon is optional and should usually be decorative.
    - :action accepts a single node.
    - :actions accepts one or more nodes.
    - The component does not add outer padding so it can sit cleanly inside
      cards, panels, sections, or standalone regions."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [icon title description action actions]} props]
      (el :div
          {:class (class-names
                   "flex flex-col items-center text-center gap-field"
                   class)}
          (merge-attrs attrs {:data-empty-state true})
          [[:div {:class "flex flex-col items-center text-center gap-2"
                  :data-empty-state-main true}
            (when icon
              [:div {:data-empty-state-icon true
                     :class "flex items-center justify-center"
                     :style {:color "var(--muted-foreground)"}}
               icon])
            (when title
              (empty-state-title {:text title}))
            (when description
              (empty-state-description {:text description}))]
           (when action
             (empty-state-actions {:children [action]}))
           (when (seq actions)
             (apply empty-state-actions {} (nodes actions)))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names
                   "flex flex-col items-center text-center gap-field"
                   class)}
          (merge-attrs attrs {:data-empty-state true})
          children))))
