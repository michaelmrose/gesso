(ns gesso.components.dialog.core
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [merge-script-attr]]
   [gesso.components.dialog.attr :refer :all]
   [gesso.components.dialog.scripts :refer :all]))

(defn dialog-trigger
  "Renders a dialog trigger button.

   Short form:
     (dialog-trigger {:text \"Open\"})

   Long form:
     (dialog-trigger {} \"Open\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          text (:text props)
          attrs (merge-script-attr attrs (dialog-trigger-script))]
      (el :button
          (dialog-trigger-attrs class)
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)
          attrs (merge-script-attr attrs (dialog-trigger-script))]
      (el :button
          (dialog-trigger-attrs class)
          attrs
          children))))

(defn dialog-overlay
  "Renders the modal overlay.

   The overlay is intended to be a sibling of dialog-content inside dialog."
  [& args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        open? (:open? props)
        attrs (merge-script-attr attrs (dialog-overlay-script))]
    (el :div
        (dialog-overlay-attrs class open?)
        attrs
        children)))

(defn dialog-title
  "Renders the dialog title."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :h2
          (dialog-title-attrs class)
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :h2
          (dialog-title-attrs class)
          attrs
          children))))

(defn dialog-description
  "Renders the dialog description text."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :p
          (dialog-description-attrs class)
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :p
          (dialog-description-attrs class)
          attrs
          children))))

(defn dialog-header
  "Renders the dialog header region.

   Short form accepts :title and :description. Long form accepts explicit
   children."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [title description children]} props]
      (el :header
          (dialog-header-attrs class)
          attrs
          [(when title (dialog-title {} title))
           (when description (dialog-description {} description))
           (nodes children)]))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :header
          (dialog-header-attrs class)
          attrs
          children))))

(defn dialog-body
  "Renders the main body region inside dialog content."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          (dialog-body-attrs class)
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          (dialog-body-attrs class)
          attrs
          children))))

(defn dialog-footer
  "Renders the dialog footer, usually for actions."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :footer
          (dialog-footer-attrs class)
          attrs
          (nodes (:children props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :footer
          (dialog-footer-attrs class)
          attrs
          children))))

(defn dialog-close
  "Renders a dialog close button.

   Short form:
     (dialog-close {:text \"Cancel\"})

   Long form:
     (dialog-close {} \"Cancel\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          text (:text props)
          attrs (merge-script-attr attrs (dialog-close-script-attr))]
      (el :button
          (dialog-close-attrs class)
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)
          attrs (merge-script-attr attrs (dialog-close-script-attr))]
      (el :button
          (dialog-close-attrs class)
          attrs
          children))))

(defn dialog-content
  "Renders the dialog surface.

   Short form accepts :title, :description, :body, and :footer.
   Long form accepts explicit children."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [open? title description body footer children]} props]
      (el :section
          (dialog-content-attrs class open?)
          attrs
          [(when (or title description)
             (dialog-header {:title title
                             :description description}))
           (when body
             (apply dialog-body {} (nodes body)))
           (nodes children)
           (when footer
             (apply dialog-footer {} (nodes footer)))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          open? (:open? props)]
      (el :section
          (dialog-content-attrs class open?)
          attrs
          children))))

(defn dialog
  "Renders a modal dialog root.

   Short form:
     (dialog {:trigger \"Open\"
              :title \"Edit profile\"
              :description \"Make changes below.\"
              :body [...]
              :footer [...]})

   Long form:
     (dialog {}
       (dialog-trigger ...)
       (dialog-overlay)
       (dialog-content ...))

   Options:
     :default-open?   whether the dialog starts open"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [default-open? trigger title description body footer]} props
          attrs (merge-script-attr attrs (dialog-root-script))]
      (el :div
          (dialog-root-attrs class default-open?)
          attrs
          [(when trigger
             (dialog-trigger {:text trigger}))
           (dialog-overlay {:open? default-open?})
           (dialog-content {:open? default-open?
                            :title title
                            :description description
                            :body body
                            :footer footer})]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          default-open? (:default-open? props)
          attrs (merge-script-attr attrs (dialog-root-script))]
      (el :div
          (dialog-root-attrs class default-open?)
          attrs
          children))))
