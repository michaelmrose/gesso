(ns gesso.components.dialog.core
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [merge-script-attr]]
   [gesso.components.dialog.attr :refer :all]
   [gesso.components.dialog.scripts :refer :all]))

(defn- resolved-open?
  "Returns the rendered open state for the dialog.

   :open? takes precedence so a server-rendered dialog can explicitly choose its
   initial state. :default-open? remains available for the existing local
   default-open behavior."
  [props]
  (if (contains? props :open?)
    (boolean (:open? props))
    (boolean (:default-open? props))))

(defn- dialog-root-id
  "Returns the dialog root id when one is available."
  [attrs]
  (:id attrs))

(defn- dialog-content-id
  [root-id]
  (when root-id
    (str root-id "-content")))

(defn- dialog-title-id
  [root-id]
  (when root-id
    (str root-id "-title")))

(defn- dialog-description-id
  [root-id]
  (when root-id
    (str root-id "-description")))

(defn dialog-trigger
  "Renders a dialog trigger button.

   Short form:
     (dialog-trigger {:text \"Open\"})

   Long form:
     (dialog-trigger {} \"Open\")

   HTMX notes:
   - HTMX attributes may be passed through :attrs.
   - This is useful when the trigger should request dialog content from the
     server before or while opening."
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
          {:keys [title description children title-id description-id]} props]
      (el :header
          (dialog-header-attrs class)
          attrs
          [(when title
             (dialog-title {:text title
                            :attrs (merge-attrs
                                    (when title-id {:id title-id}))}))
           (when description
             (dialog-description {:text description
                                  :attrs (merge-attrs
                                          (when description-id {:id description-id}))}))
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
   Long form accepts explicit children.

   HTMX notes:
   - The content surface is a good fragment boundary for server-rendered modal
     updates.
   - A server may render dialog content directly with :open? true, and may also
     provide stable ids through the dialog root id for aria wiring."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [open? title description body footer children
                  content-id title-id description-id]} props]
      (el :section
          (dialog-content-attrs class open? content-id title-id description-id)
          attrs
          [(when (or title description)
             (dialog-header {:title title
                             :description description
                             :title-id title-id
                             :description-id description-id}))
           (when body
             (apply dialog-body {} (nodes body)))
           (nodes children)
           (when footer
             (apply dialog-footer {} (nodes footer)))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [open? content-id title-id description-id]} props]
      (el :section
          (dialog-content-attrs class open? content-id title-id description-id)
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
     :default-open?   whether the dialog starts open for local default behavior
     :open?           explicit rendered open state; takes precedence over
                      :default-open?

   HTMX notes:
   - The dialog root is a good fragment boundary for server-rendered modal
     flows.
   - For server-rendered open state, pass :open? true when returning the
     dialog.
   - HTMX attributes usually belong on the trigger, content, or form elements
     inside the dialog rather than on the scripts themselves."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [trigger title description body footer]} props
          open?      (resolved-open? props)
          root-id    (dialog-root-id attrs)
          content-id (dialog-content-id root-id)
          title-id   (when title (dialog-title-id root-id))
          desc-id    (when description (dialog-description-id root-id))
          attrs      (merge-script-attr attrs (dialog-root-script))]
      (el :div
          (dialog-root-attrs class open?)
          attrs
          [(when trigger
             (dialog-trigger {:text trigger}))
           (dialog-overlay {:open? open?})
           (dialog-content {:open? open?
                            :content-id content-id
                            :title-id title-id
                            :description-id desc-id
                            :title title
                            :description description
                            :body body
                            :footer footer})]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          open? (resolved-open? props)
          attrs (merge-script-attr attrs (dialog-root-script))]
      (el :div
          (dialog-root-attrs class open?)
          attrs
          children))))
