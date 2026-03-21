(ns gesso.core
  (:require
   [gesso.components.accordion.core :as accordion]
   [gesso.components.alert :as alert]
   [gesso.components.badge :as badge]
   [gesso.components.button :as button]
   [gesso.components.card :as card]
   [gesso.components.checkbox :as checkbox]
   [gesso.components.field :as field]
   [gesso.components.input :as input]
   [gesso.components.label :as label]
   [gesso.components.radio-group :as radio-group]
   [gesso.components.select :as select]
   [gesso.components.switch :as switch]
   [gesso.components.textarea :as textarea]
   [gesso.theme :as gtheme]
   [gesso.components.dialog.core :as dialog]
   ))

;; public api

;; theme

(def theme gtheme/theme)
(def html-theme-attrs gtheme/html-theme-attrs)
(def theme-head gtheme/theme-head)

(def accordion accordion/accordion)
(def accordion-item accordion/accordion-item)
(def accordion-trigger accordion/accordion-trigger)
(def accordion-content accordion/accordion-content)

(def dialog dialog/dialog)
(def dialog-trigger dialog/dialog-trigger)
(def dialog-overlay dialog/dialog-overlay)
(def dialog-content dialog/dialog-content)
(def dialog-header dialog/dialog-header)
(def dialog-title dialog/dialog-title)
(def dialog-description dialog/dialog-description)
(def dialog-body dialog/dialog-body)
(def dialog-footer dialog/dialog-footer)
(def dialog-close dialog/dialog-close)

(def alert alert/alert)
(def alert-title alert/alert-title)
(def alert-content alert/alert-content)

(def badge badge/badge)

(def button button/button)

(def card card/card)
(def card-title card/card-title)
(def card-description card/card-description)
(def card-header card/card-header)
(def card-content card/card-content)
(def card-footer card/card-footer)

(def checkbox checkbox/checkbox)

(def label label/label)

(def input input/input)
(def textarea textarea/textarea)
(def select select/select)

(def field field/field)

(def radio radio-group/radio)
(def radio-group radio-group/radio-group)

(def switch switch/switch)
