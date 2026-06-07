(ns gesso.components.card.core
  (:require
   [gesso.components.card.attr :as attr]
   [gesso.components.text :as text]
   [gesso.util :refer :all]))

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
        :attrs (attr/title-attrs attrs)
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/heading
             {:level 3
              :as :h2
              :class class
              :attrs (attr/title-attrs attrs)}
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
        :attrs (attr/description-attrs attrs)
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/muted-text
             {:as :p
              :class class
              :attrs (attr/description-attrs attrs)}
             children))))

(defn card-header
  "Card header subcomponent.

   Uses a simple vertical layout with the shared title gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description children]} props]
      (el :header
          {:class (attr/header-class class)}
          (attr/header-attrs attrs)
          [(when title
             (card-title {:text title}))
           (when description
             (card-description {:text description}))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :header
          {:class (attr/header-class class)}
          (attr/header-attrs attrs)
          children))))

(defn card-content
  "Card content subcomponent.

   Uses a simple vertical layout with the shared content gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :section
          {:class (attr/content-class class)}
          (attr/content-attrs attrs)
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          {:class (attr/content-class class)}
          (attr/content-attrs attrs)
          children))))

(defn card-footer
  "Card footer subcomponent.

   Uses a simple vertical layout with the shared content gap utility."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :footer
          {:class (attr/footer-class class)}
          (attr/footer-attrs attrs)
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :footer
          {:class (attr/footer-class class)}
          (attr/footer-attrs attrs)
          children))))

(defn card
  "Long form:
    (card {:class ... :attrs ... :as ...} children...)

  Short form (map-only):
    (card {:class ...
           :attrs ...
           :as :article
           :header <node>
           :title <node>
           :description <node>
           :content <node|seq>
           :footer <node|seq>})

  :as controls the root element tag and defaults to :div.

  HTMX notes:
    - Cards make good fragment boundaries for server-rendered partial updates.
    - HTMX attributes may be passed through :attrs on the root element.
    - The root includes data-card=\"true\" and subparts include stable data
      attributes for easier targeting."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [as header title description content footer]} props
          tag         (attr/root-tag as)
          header-node (or header
                          (when (or title description)
                            (card-header {}
                              (when title
                                (card-title {} title))
                              (when description
                                (card-description {} description)))))
          content-node (when (some? content)
                         (apply card-content {} (nodes content)))
          footer-node  (when (some? footer)
                         (apply card-footer {} (nodes footer)))]
      (el tag
          {:class (attr/root-class class)}
          (attr/root-attrs attrs)
          [header-node content-node footer-node]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          tag (attr/root-tag (:as props))]
      (el tag
          {:class (attr/root-class class)}
          (attr/root-attrs attrs)
          children))))
