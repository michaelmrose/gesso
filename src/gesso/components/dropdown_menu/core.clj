(ns gesso.components.dropdown-menu.core
  (:require
   [gesso.util :refer :all]
   [gesso.hyperscript :refer [merge-script-attr]]
   [gesso.components.dropdown-menu.attr :refer :all]
   [gesso.components.dropdown-menu.scripts :refer :all]))

(defn dropdown-menu-trigger
  "Renders a dropdown trigger button.

   Short form:
     (dropdown-menu-trigger {:text \"Open\"})

   Long form:
     (dropdown-menu-trigger {} \"Open\")"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          text (:text props)
          attrs (merge-script-attr attrs (dropdown-menu-trigger-script))]
      (el :button
          (dropdown-menu-trigger-attrs class)
          attrs
          (nodes text)))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)
          attrs (merge-script-attr attrs (dropdown-menu-trigger-script))]
      (el :button
          (dropdown-menu-trigger-attrs class)
          attrs
          children))))

(defn dropdown-menu-content
  "Renders the floating dropdown surface.

   Options:
   - :open?       whether the menu starts visible
   - :side        :bottom or :top
   - :align       :start or :end
   - :side-offset CSS length string, default \"0.35rem\"

   Positioning is manual for now. The caller chooses the side and alignment."
  [& args]
  (let [[opts children] (normalize-component-args args)
        {:keys [props class attrs]} (split-opts opts)
        {:keys [open? side align side-offset]} props]
    (el :div
        (dropdown-menu-content-attrs class open? side align side-offset)
        attrs
        children)))

(defn dropdown-menu-label
  "Renders a menu label."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          (dropdown-menu-label-attrs class)
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          (dropdown-menu-label-attrs class)
          attrs
          children))))

(defn dropdown-menu-separator
  "Renders a separator between menu groups."
  [& args]
  (let [[opts children] (normalize-component-args args)
        {:keys [class attrs]} (split-opts opts)]
    (el :div
        (dropdown-menu-separator-attrs class)
        attrs
        children)))

(defn dropdown-menu-right-slot
  "Renders right-aligned secondary item content such as shortcuts or hints."
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))]
      (el :div
          (dropdown-menu-right-slot-attrs class)
          attrs
          (nodes (:text props))))
    (let [[opts children] (normalize-component-args args)
          {:keys [class attrs]} (split-opts opts)]
      (el :div
          (dropdown-menu-right-slot-attrs class)
          attrs
          children))))

(defn dropdown-menu-indicator
  "Renders the left indicator slot inside an item."
  [& args]
  (let [[opts children] (normalize-component-args args)
        {:keys [class attrs]} (split-opts opts)]
    (el :div
        (dropdown-menu-indicator-attrs class)
        attrs
        children)))

(defn dropdown-menu-item
  "Renders a menu item button.

   Short form:
     (dropdown-menu-item {:text \"Profile\"})

   Long form:
     (dropdown-menu-item {}
       [:span \"Profile\"]
       (dropdown-menu-right-slot {} \"⌘P\"))

   Options:
     :disabled?   marks the item unavailable
     :keep-open?  prevents the menu from closing when selected"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [text right-slot disabled? keep-open?]} props
          attrs (merge-script-attr attrs (dropdown-menu-item-script {:keep-open? keep-open?}))]
      (el :button
          (dropdown-menu-item-attrs class disabled?)
          attrs
          [(nodes text)
           (when right-slot
             (dropdown-menu-right-slot {} right-slot))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          {:keys [disabled? keep-open?]} props
          attrs (merge-script-attr attrs (dropdown-menu-item-script {:keep-open? keep-open?}))]
      (el :button
          (dropdown-menu-item-attrs class disabled?)
          attrs
          children))))

(defn dropdown-menu
   "Renders a dropdown menu root.

   Short form:
     (dropdown-menu
       {:trigger \"Options\"
        :items [{:text \"Profile\"}
                {:text \"Settings\"}
                {:separator? true}
                {:text \"Sign out\"}]})

   Long form:
     (dropdown-menu {}
       (dropdown-menu-trigger {:text \"Options\"})
       (dropdown-menu-content
         (dropdown-menu-item {:text \"Profile\"})
         (dropdown-menu-item {:text \"Settings\"})))

   Root options:
   - :default-open? whether the menu starts open

   Content positioning is configured on dropdown-menu-content with:
   - :side        :bottom | :top
   - :align       :start  | :end
   - :side-offset CSS length string"
  [& args]
  (if (only-map-arg? args)
    (let [{:keys [props class attrs]} (split-opts (first args))
          {:keys [default-open? trigger items]} props
          attrs (merge-script-attr attrs (dropdown-menu-root-script))]
      (el :div
          (dropdown-menu-root-attrs class default-open?)
          attrs
          [(when trigger
             (dropdown-menu-trigger {:text trigger}))
           (dropdown-menu-content {:open? default-open?}
                                  (for [item items]
                                    (cond
                                      (:separator? item)
                                      (dropdown-menu-separator)

                                      (:label? item)
                                      (dropdown-menu-label {:text (:text item)})

                                      :else
                                      (dropdown-menu-item item))))]))
    (let [[opts children] (normalize-component-args args)
          {:keys [props class attrs]} (split-opts opts)
          default-open? (:default-open? props)
          attrs (merge-script-attr attrs (dropdown-menu-root-script))]
      (el :div
          (dropdown-menu-root-attrs class default-open?)
          attrs
          children))))
