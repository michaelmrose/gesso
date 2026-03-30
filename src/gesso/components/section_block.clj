(ns gesso.components.section-block
  (:require
   [gesso.util :refer :all]
   [gesso.components.text :as text]))

(defn section-block-title
  "Section block title.

  Short form:
    (section-block-title {:text \"Waiting requests\"})

  Long form:
    (section-block-title \"Waiting requests\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/heading
       {:level 2
        :as :h2
        :class class
        :attrs (merge-attrs attrs {:data-section-block-title true})
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/heading
             {:level 2
              :as :h2
              :class class
              :attrs (merge-attrs attrs {:data-section-block-title true})}
             children))))

(defn section-block-description
  "Section block description.

  Short form:
    (section-block-description {:text \"Requests that have not yet been claimed.\"})

  Long form:
    (section-block-description \"Requests that have not yet been claimed.\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (text/text
       {:variant :muted
        :as :p
        :class class
        :attrs (merge-attrs attrs {:data-section-block-description true})
        :text (:text props)}))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (apply text/text
             {:variant :muted
              :as :p
              :class class
              :attrs (merge-attrs attrs {:data-section-block-description true})}
             children))))

(defn section-block-actions
  "Section block actions area.

  Short form:
    (section-block-actions {:children [...]})

  Long form:
    (section-block-actions
      (button {:text \"Refresh\"}))"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "cluster-theme justify-end" class)}
          (merge-attrs attrs {:data-section-block-actions true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "cluster-theme justify-end" class)}
          (merge-attrs attrs {:data-section-block-actions true})
          children))))

(defn section-block-header
  "Section block header.

  Short form accepts :title, :description, :actions, and optional :children.
  Long form accepts explicit children."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description actions children]} props]
      (el :header
          {:class (class-names "flex flex-wrap items-start justify-between gap-inline" class)}
          (merge-attrs attrs {:data-section-block-header true})
          [[:div {:class "flex flex-col gap-field"
                  :data-section-block-heading true}
            (when title
              (section-block-title {:text title}))
            (when description
              (section-block-description {:text description}))]
           (when actions
             (apply section-block-actions {} (nodes actions)))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :header
          {:class (class-names "flex flex-wrap items-start justify-between gap-inline" class)}
          (merge-attrs attrs {:data-section-block-header true})
          children))))

(defn section-block-content
  "Section block content area.

  Short form:
    (section-block-content {:children [...]})

  Long form:
    (section-block-content
      ...)"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          {:class (class-names "panel-theme" class)}
          (merge-attrs attrs {:data-section-block-content true})
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          {:class (class-names "panel-theme" class)}
          (merge-attrs attrs {:data-section-block-content true})
          children))))

(defn section-block
  "Structured section shell for app screens and dashboard areas.

  Short form:
    (section-block
      {:title \"Waiting requests\"
       :description \"Requests that have not yet been claimed.\"
       :actions [(button {:variant :outline :text \"Refresh\"})]
       :content [...]})

  Long form:
    (section-block {}
      (section-block-header ...)
      (section-block-content ...))

  Notes:
    - Section blocks are structural, not surfaced by default.
    - Wrap in card/panel when a bordered or elevated surface is desired."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description actions content header children]} props
          header-node (or header
                          (when (or title description actions)
                            (section-block-header
                             {:title title
                              :description description
                              :actions actions})))
          content-node (when (some? content)
                         (apply section-block-content {} (nodes content)))]
      (el :section
          {:class (class-names "section-theme" class)}
          (merge-attrs attrs {:data-section-block true})
          [header-node
           content-node
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :section
          {:class (class-names "section-theme" class)}
          (merge-attrs attrs {:data-section-block true})
          children))))
