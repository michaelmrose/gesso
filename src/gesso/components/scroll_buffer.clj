(ns gesso.components.scroll-buffer
  (:require [gesso.util :refer :all]))

(def ^:private size-values
  {:xs "2rem"
   :sm "4rem"
   :md "6rem"
   :lg "10rem"
   :xl "14rem"})

(defn- size-value
  [size]
  (cond
    (keyword? size) (get size-values size "6rem")
    (string? size) size
    (number? size) (str size "rem")
    :else "6rem"))

(defn- buffer-block-size
  [{:keys [size safe-area?]}]
  (let [size (size-value (or size :md))]
    (if (false? safe-area?)
      size
      (str "max(" size ", calc(env(safe-area-inset-bottom) + " size "))"))))

(defn scroll-buffer
  "Render inert space at the end of a scrollable page or panel.

  Useful on mobile when browser chrome, keyboards, password-manager affordances,
  or fixed bottom controls can cover the final interactive element.

  Examples:
    (scroll-buffer)
    (scroll-buffer {:size :lg})
    (scroll-buffer {:size \"12rem\"})
    (scroll-buffer {:size :sm :safe-area? false})

  Options:
    :size        One of :xs :sm :md :lg :xl, a CSS length string, or a number
                 interpreted as rem.
    :safe-area?  Defaults to true. Includes env(safe-area-inset-bottom).
    :class       Extra classes.
    :attrs       Extra attrs. :style is merged with the generated style."
  ([]
   (scroll-buffer {}))
  ([opts]
   (let [{:keys [props class attrs]} (split-opts opts)
         style                       (merge
                                      {:block-size (buffer-block-size props)
                                       :flex "0 0 auto"
                                       :pointer-events "none"}
                                      (:style attrs))]
     (el :div
         {:class (class-names "scroll-buffer" class)}
         (merge-attrs
          (dissoc attrs :style)
          {:data-scroll-buffer true
           :aria-hidden "true"
           :role "presentation"
           :style style})
         []))))
