(ns gesso.components.card.attr
  (:require
   [gesso.util :refer :all]))

(defn root-tag
  [as]
  (or as :div))

(defn root-class
  [class]
  (class-names "card" class))

(defn root-attrs
  [attrs]
  (merge-attrs attrs {:data-card true}))

(defn title-attrs
  [attrs]
  (merge-attrs attrs {:data-card-title true}))

(defn description-attrs
  [attrs]
  (merge-attrs attrs {:data-card-description true}))

(defn header-class
  [class]
  (class-names "flex flex-col gap-title" class))

(defn header-attrs
  [attrs]
  (merge-attrs attrs {:data-card-header true}))

(defn content-class
  [class]
  (class-names "flex flex-col gap-content" class))

(defn content-attrs
  [attrs]
  (merge-attrs attrs {:data-card-content true}))

(defn footer-class
  [class]
  (class-names "flex flex-col gap-content" class))

(defn footer-attrs
  [attrs]
  (merge-attrs attrs {:data-card-footer true}))
