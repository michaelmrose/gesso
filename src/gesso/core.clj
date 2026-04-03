(ns gesso.core
  (:require
   [gesso.components.accordion.core :as accordion]
   [gesso.components.dropdown-menu.core :as dropdown]
   [gesso.components.tabs.core :as tabs]
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
   [gesso.components.text :as text ]
   [gesso.components.empty-state :as empty-state]
   [gesso.components.icon :as icon]
   [gesso.components.status-pill :as status-pill]
   [gesso.components.group :as group]
   [gesso.components.section-block :as section-block]
   [gesso.components.toolbar :as toolbar]
   [gesso.components.page :as page]
   [gesso.components.topbar :as topbar]
   [gesso.components.sidebar :as sidebar]
   ))

;; public api

;; page
(def page page/page)
(def page-left page/page-left)
(def page-main page/page-main)
(def page-right page/page-right)
(def page-surface page/page-surface)

;;  toolbar
(def toolbar toolbar/toolbar)
(def toolbar-start toolbar/toolbar-start)
(def toolbar-center toolbar/toolbar-center)
(def toolbar-end toolbar/toolbar-end)
(def toolbar-spacer toolbar/toolbar-spacer)

;; top and side bars
(def topbar topbar/topbar)

(def sidebar sidebar/sidebar)
(def sidebar-section sidebar/sidebar-section)
(def sidebar-overflow-items sidebar/sidebar-overflow-items)

;; section block
(def section-block section-block/section-block)
(def section-block-header section-block/section-block-header)
(def section-block-title section-block/section-block-title)
(def section-block-description section-block/section-block-description)
(def section-block-actions section-block/section-block-actions)
(def section-block-content section-block/section-block-content)

;; group

(def group group/group)
;; status-pill
(def status-pill status-pill/status-pill)
;; Icons
(def icon icon/icon)

;; empty state
(def empty-state empty-state/empty-state)
(def empty-state-title empty-state/empty-state-title)
(def empty-state-description empty-state/empty-state-description)
(def empty-state-actions empty-state/empty-state-actions)
(def empty-state-icon empty-state/empty-state-icon)

;; text

(def text text/text)
(def heading text/heading)
(def page-title text/page-title)
(def section-title text/section-title)
(def muted-text text/muted-text)
(def label-text text/label-text)

;; theme

(def theme gtheme/theme)
(def html-theme-attrs gtheme/html-theme-attrs)
(def theme-head gtheme/theme-head)

(def dropdown-menu dropdown/dropdown-menu)
(def dropdown-menu-trigger dropdown/dropdown-menu-trigger)
(def dropdown-menu-content dropdown/dropdown-menu-content)
(def dropdown-menu-item dropdown/dropdown-menu-item)
(def dropdown-menu-label dropdown/dropdown-menu-label)
(def dropdown-menu-separator dropdown/dropdown-menu-separator)
(def dropdown-menu-right-slot dropdown/dropdown-menu-right-slot)
(def dropdown-menu-indicator dropdown/dropdown-menu-indicator)

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

(def tabs tabs/tabs)
(def tabs-list tabs/tabs-list)
(def tabs-trigger tabs/tabs-trigger)
(def tabs-content tabs/tabs-content)

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
