(ns gesso.components.card
  (:require
   [gesso.util :refer :all]
   [gesso.components.text :as text]))

(defn card-title
  "Card title subcomponent.

   Uses the shared heading system without introducing additional component-
   specific CSS hooks."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/heading
       {:level 3
        :as :h2
        :class class
        :attrs (merge-attrs attrs {:data-card-title true})
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/heading
             {:level 3
              :as :h2
              :class class
              :attrs (merge-attrs attrs {:data-card-title true})}
             children))))

(defn card-description
  "Card description subcomponent.

   Uses the shared muted text styling without introducing additional
   component-specific CSS hooks."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/muted-text
       {:as :p
        :class class
        :attrs (merge-attrs attrs {:data-card-description true})
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/muted-text
             {:as :p
              :class class
              :attrs (merge-attrs attrs {:data-card-description true})}
             children))))

(defn card-header
  "Card header subcomponent.

   Uses a simple vertical layout with the shared title gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description children]} props]
      (el :header
          {:class (class-names "flex flex-col gap-title" class)}
          (merge-attrs attrs {:data-card-header true})
          [(when title
             (card-title {:text title}))
           (when description
             (card-description {:text description}))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :header
          {:class (class-names "flex flex-col gap-title" class)}
          (merge-attrs attrs {:data-card-header true})
          children))))

(defn card-content
  "Card content subcomponent.

   Uses a simple vertical layout with the shared content gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          {:class (class-names "flex flex-col gap-content" class)}
          (merge-attrs attrs {:data-card-content true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          {:class (class-names "flex flex-col gap-content" class)}
          (merge-attrs attrs {:data-card-content true})
          children))))

(defn card-footer
  "Card footer subcomponent.

   Uses a simple vertical layout with the shared content gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :footer
          {:class (class-names "flex flex-col gap-content" class)}
          (merge-attrs attrs {:data-card-footer true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :footer
          {:class (class-names "flex flex-col gap-content" class)}
          (merge-attrs attrs {:data-card-footer true})
          children))))

(defn card
  "Long form:
    (card {:class ... :attrs ...} children...)

  Short form (map-only):
    (card {:class ... :attrs ... :header <node> :title <node> :description <node> :content <node|seq> :footer <node|seq>})

  HTMX notes:
    - Cards make good fragment boundaries for server-rendered partial updates.
    - HTMX attributes may be passed through :attrs on the root element.
    - The root includes data-card=\"true\" and subparts include stable data
      attributes for easier targeting."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [header title description content footer]} props
          header-node (or header
                          (when (or title description)
                            (card-header {}
                              (when title (card-title {} title))
                              (when description (card-description {} description)))))
          content-node (when (some? content)
                         (apply card-content {} (nodes content)))
          footer-node (when (some? footer)
                        (apply card-footer {} (nodes footer)))]
      (el :div
          {:class (class-names "card" class)}
          (merge-attrs attrs {:data-card true})
          [header-node content-node footer-node]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "card" class)}
          (merge-attrs attrs {:data-card true})
          children))))
